{- | Layer 0: what @salmon-cloudrun@ reads, refuses and declares. No gcloud
is run, and no node's @up@, @check@ or @down@: these are the parser, the
refusals, the folded graph and the argv of the deploy.
-}
module Test.CloudrunSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CmdSpec (..), cmdspec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- GHC only solves a `HasField` constraint when the selector is in scope, and
-- `Dag.sameRepresentative` needs these whether or not this module says them.
import Salmon.Builtin.Extension (Extension, dynamics, evalDeps, help, notes, ref)
import Salmon.Builtin.Nodes.Binary (prepare)
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as LoadBalancing
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)

import Cloudrun

tests :: TestTree
tests =
    testGroup
        "salmon-cloudrun (Layer 0)"
        [ testGroup
            "the declaration"
            [ testCase "the documented example reads, with its defaults" exampleReads
            , testCase "the directive reads back as the declaration it was printed from" roundTrips
            , testCase "a key nobody knows is refused, not dropped" unknownKeys
            , testCase "a binding is a variable or a file, never both or neither" bindingTargets
            , testCase "a relative source file is taken from the declaration's directory" sourceFileAnchored
            , testCase "a file with something wrong in it configures to nothing" refusedAtConfig
            ]
        , testGroup
            "refusals"
            [ testCase "the examples have nothing wrong" (mapM_ (\t -> assertEqual (show t) [] (turnupProblems t)) [example, fuller])
            , testCase "each mistake is named" mistakes
            , testCase "a comma in a value is refused by the variable's name, without the value" commaNamesNoValue
            , testCase "a refused declaration is one failing node and no resource" refusedGraph
            ]
        , testGroup
            "the graph"
            [ testCase "no declaration collides with another" noConflicts
            , testCase "a service waits for its account, its grants, its secrets and the API" serviceOrder
            , testCase "the balancer's resources wait for the services and the subnet" balancerOrder
            , testCase "the declared account is under everything that calls gcloud" accountUnderAll
            , testCase "enable_apis false declares no API" noApis
            , testCase "what run down keeps says so, and nothing promises to destroy a secret" keptNodes
            , testCase "no node's text holds the bytes of a source file" noSecretBytes
            ]
        , testGroup
            "what is deployed"
            [ testCase "the deploy's argv" deployArgv
            , testCase "every service carries the turnup's label" ownerLabels
            , testCase "the balancer has a backend per routed service" balancerShape
            ]
        ]

-------------------------------------------------------------------------------

exampleJson :: String
exampleJson =
    unlines
        [ "{"
        , "  \"name\": \"acme\","
        , "  \"project\": \"acme-prod\","
        , "  \"region\": \"europe-west1\","
        , "  \"secrets\": ["
        , "    {\"name\": \"acme-db-password\", \"source_file\": \"/run/acme/db-password\"}"
        , "  ],"
        , "  \"services\": ["
        , "    {"
        , "      \"name\": \"acme-api\","
        , "      \"image\": \"europe-west1-docker.pkg.dev/acme-prod/acme/api:1.4.2\","
        , "      \"service_account\": {\"create\": \"acme-api\"},"
        , "      \"env\": {\"LOG_LEVEL\": \"info\"},"
        , "      \"bind_secrets\": ["
        , "        {\"env\": \"DB_PASSWORD\", \"secret\": \"acme-db-password\"},"
        , "        {\"file\": \"/secrets/tls/key.pem\", \"secret\": \"acme-tls-key\", \"version\": \"3\"}"
        , "      ],"
        , "      \"ingress\": \"internal-and-cloud-load-balancing\","
        , "      \"invoker\": \"no-iam-check\""
        , "    }"
        , "  ],"
        , "  \"load_balancer\": {"
        , "    \"name\": \"acme-lb\","
        , "    \"proxy_subnet\": {\"name\": \"acme-proxy\", \"range\": \"192.168.100.0/24\"},"
        , "    \"default_service\": \"acme-api\""
        , "  }"
        , "}"
        ]

example :: Turnup
example = either (error . ("the example does not read: " <>)) id (parseTurnup (LChar8.pack exampleJson))

api :: Service
api = case example.turnupServices of
    [s] -> s
    _ -> error "the example has one service"

exampleBalancer :: LoadBalancer
exampleBalancer = maybe (error "the example has a balancer") id example.turnupLoadBalancer

-- | Two services, an account that exists, routes, a certificate, an asserted account.
fuller :: Turnup
fuller =
    example
        { turnupAccount = Just "deployer@example.com"
        , turnupServices = [api, web]
        , turnupLoadBalancer =
            Just
                exampleBalancer
                    { lbNetwork = Just "acme-net"
                    , lbDefaultService = "acme-web"
                    , lbHosts = [HostRoute ["app.example.com"] "acme-web" [PathRoute ["/api/*"] "acme-api"]]
                    , lbCertificates = ["acme-cert"]
                    , lbHttp = Just HttpRedirectToHttps
                    }
        }
  where
    web =
        api
            { svcName = "acme-web"
            , svcImage = "europe-west1-docker.pkg.dev/acme-prod/acme/web:7"
            , svcAccount = ExistingAccount "web@acme-prod.iam.gserviceaccount.com"
            , svcEnv = Map.empty
            , svcSecrets = []
            , svcInvoker = InvokerAllUsers
            , svcMinInstances = Just 1
            , svcMaxInstances = Just 4
            , svcCpuAlwaysAllocated = True
            }

dagOf :: Turnup -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps . turnupOp

texts :: Dag.Dag Extension -> [(Ref, [Text])]
texts dag = [(rf, a.extension.help : a.extension.notes) | (rf, a) <- Map.toList (Dag.dagNodes dag)]

-- | The refs of the nodes whose @help@ holds these words.
named :: Text -> Dag.Dag Extension -> [Ref]
named words' dag = [rf | (rf, a) <- Map.toList (Dag.dagNodes dag), words' `Text.isInfixOf` a.extension.help]

theOne :: Text -> Dag.Dag Extension -> IO Ref
theOne words' dag = case named words' dag of
    [rf] -> pure rf
    others -> assertFailure ("expected one node saying " <> show words' <> ", found " <> show (length others))

notesOf :: Dag.Dag Extension -> Ref -> [Text]
notesOf dag rf = maybe [] (\a -> a.extension.notes) (Map.lookup rf (Dag.dagNodes dag))

-- | Everything a node waits for, however far down.
below :: Dag.Dag Extension -> Ref -> Set.Set Ref
below dag = go Set.empty . Dag.dependenciesOf dag
  where
    go seen [] = seen
    go seen (r : rest)
        | r `Set.member` seen = go seen rest
        | otherwise = go (Set.insert r seen) (Dag.dependenciesOf dag r <> rest)

-------------------------------------------------------------------------------
-- The declaration

exampleReads :: IO ()
exampleReads = do
    assertEqual "name" "acme" example.turnupName
    assertEqual "no account asserted" Nothing example.turnupAccount
    assertEqual "APIs are enabled unless said otherwise" True example.turnupEnableApis
    assertEqual "the secret" [ManagedSecret "acme-db-password" "automatic" (Just "/run/acme/db-password")] example.turnupSecrets
    assertEqual "the account" (CreateAccount "acme-api") api.svcAccount
    assertEqual
        "the bindings, a version being latest unless said"
        [ SecretBinding (EnvVar "DB_PASSWORD") "acme-db-password" "latest"
        , SecretBinding (MountedFile "/secrets/tls/key.pem") "acme-tls-key" "3"
        ]
        api.svcSecrets
    assertEqual "the grants are made unless said otherwise" True api.svcGrantSecretAccess
    assertEqual "ingress" CloudRun.InternalAndLoadBalancing api.svcIngress
    assertEqual "invoker" InvokerNoIamCheck api.svcInvoker
    assertEqual "no knob is set that the file does not set" (Nothing, Nothing, False) (api.svcCpu, api.svcMinInstances, api.svcCpuAlwaysAllocated)
    assertEqual "the balancer" (LoadBalancer "acme-lb" Nothing (Just (ProxySubnet "acme-proxy" "192.168.100.0/24")) "acme-api" [] [] Nothing) exampleBalancer
    -- what a file that says nothing about them gets
    let bare = parseTurnup "{\"name\":\"a\",\"project\":\"p\",\"region\":\"r\",\"services\":[{\"name\":\"s\",\"image\":\"i\",\"service_account\":{\"email\":\"x@y\"}}]}"
    case bare of
        Right t | [s] <- t.turnupServices -> do
            assertEqual "ingress" CloudRun.All s.svcIngress
            assertEqual "invoker" InvokerIam s.svcInvoker
            assertEqual "no balancer" Nothing t.turnupLoadBalancer
            assertEqual "no secret" [] t.turnupSecrets
        other -> assertFailure (show other)

roundTrips :: IO ()
roundTrips =
    mapM_
        (\t -> assertEqual (show t) (Just t) (decode (encode t)))
        [example, fuller, fuller{turnupLoadBalancer = Nothing, turnupEnableApis = False}]

unknownKeys :: IO ()
unknownKeys = do
    refusedNaming "secert" (replace "\"secrets\": [" "\"secert\": [")
    refusedNaming "bind_secret" (replace "\"bind_secrets\"" "\"bind_secret\"")
    refusedNaming "defaultservice" (replace "\"default_service\"" "\"defaultservice\"")
    refusedNaming "ingress" (replace "internal-and-cloud-load-balancing" "internal-and-lb")
  where
    refusedNaming word edit = case parseTurnup (LChar8.pack (edit exampleJson)) of
        Left why -> assertBool why (word `isInfixOf` why)
        Right _ -> assertFailure ("read in spite of " <> word)

replace :: Text -> Text -> String -> String
replace old new = Text.unpack . Text.replace old new . Text.pack

bindingTargets :: IO ()
bindingTargets = do
    assertBool "both" (isLeft (parseTurnup (LChar8.pack (replace "{\"env\": \"DB_PASSWORD\"," "{\"env\": \"DB_PASSWORD\", \"file\": \"/x/y\"," exampleJson))))
    assertBool "neither" (isLeft (parseTurnup (LChar8.pack (replace "{\"env\": \"DB_PASSWORD\"," "{" exampleJson))))
    assertBool "an account both made and named" (isLeft (parseTurnup (LChar8.pack (replace "{\"create\": \"acme-api\"}" "{\"create\": \"acme-api\", \"email\": \"a@b\"}" exampleJson))))

sourceFileAnchored :: IO ()
sourceFileAnchored =
    withSystemTempDirectory "salmon-cloudrun" $ \dir -> do
        createDirectory (dir </> "conf")
        let file = dir </> "conf" </> "turnup.json"
        writeFile file (replace "/run/acme/db-password" "secrets/db-password" exampleJson)
        result <- loadTurnup file
        case result of
            Right t -> assertEqual "" [Just (dir </> "conf" </> "secrets/db-password")] (map msSourceFile t.turnupSecrets)
            Left why -> assertFailure why
        -- an absolute one is the operator's word
        writeFile file exampleJson
        absolute <- loadTurnup file
        assertEqual "" (Right example) absolute

refusedAtConfig :: IO ()
refusedAtConfig =
    withSystemTempDirectory "salmon-cloudrun" $ \dir -> do
        let file = dir </> "turnup.json"
        writeFile file (replace "\"default_service\": \"acme-api\"" "\"default_service\": \"acme-apy\"" exampleJson)
        result <- loadTurnup file
        case result of
            Left why -> assertBool why ("acme-apy" `isInfixOf` why)
            Right _ -> assertFailure "a balancer routing to nothing was configured"

-------------------------------------------------------------------------------
-- Refusals

withService :: (Service -> Service) -> Turnup
withService f = example{turnupServices = [f api]}

withBalancer :: (LoadBalancer -> LoadBalancer) -> Turnup
withBalancer f = example{turnupLoadBalancer = Just (f exampleBalancer)}

mistakes :: IO ()
mistakes =
    mapM_
        ( \(what, turnup, said) -> do
            let problems = turnupProblems turnup
            assertBool (what <> ": " <> show problems) (any (said `Text.isInfixOf`) problems)
        )
        [ ("a name that is no label", example{turnupName = "Acme_Prod"}, "not usable as a label value")
        , ("no project", example{turnupProject = ""}, "no project")
        , ("no service", example{turnupServices = [], turnupLoadBalancer = Nothing}, "no service declared")
        , ("a service twice", example{turnupServices = [api, api]}, "service declared twice: acme-api")
        , ("a secret twice", example{turnupSecrets = example.turnupSecrets <> example.turnupSecrets}, "secret declared twice: acme-db-password")
        , ("a service name GCP refuses", withService (\s -> s{svcName = "Acme"}), "not a Cloud Run service name")
        , ("an account id too short", withService (\s -> s{svcAccount = CreateAccount "api"}), "not a service account id")
        , ("an address that is none", withService (\s -> s{svcAccount = ExistingAccount "acme-api"}), "not a service account's address")
        , ("a variable name", withService (\s -> s{svcEnv = Map.fromList [("LOG-LEVEL", "info")]}), "not an environment variable name: LOG-LEVEL")
        , ("a variable set plainly and from a secret", withService (\s -> s{svcEnv = Map.fromList [("DB_PASSWORD", "x")]}), "variable set twice")
        , ("a relative secret file", withService (\s -> s{svcSecrets = [SecretBinding (MountedFile "tls/key.pem") "k" "latest"]}), "not absolute")
        ,
            ( "two secrets in one directory"
            , withService (\s -> s{svcSecrets = [SecretBinding (MountedFile "/tls/key.pem") "k" "latest", SecretBinding (MountedFile "/tls/cert.pem") "c" "latest"]})
            , "two secrets mounted in one directory"
            )
        , ("a balancer routing to nothing", withBalancer (\l -> l{lbDefaultService = "acme-web"}), "undeclared service: acme-web")
        , ("a route to nothing", withBalancer (\l -> l{lbHosts = [HostRoute ["a.example.com"] "acme-api" [PathRoute ["/x/*"] "nope"]]}), "undeclared service: nope")
        , ("a balancer in front of an internal service", withService (\s -> s{svcIngress = CloudRun.Internal}), "an external balancer cannot reach it")
        , ("the balancer module's own refusals", withBalancer (\l -> l{lbHosts = [HostRoute [] "acme-api" []]}), "a host rule names no host")
        , ("a resource name too long", withBalancer (\l -> l{lbName = Text.replicate 50 "a"}), "longer than 63")
        ]

commaNamesNoValue :: IO ()
commaNamesNoValue = do
    let problems = turnupProblems (withService (\s -> s{svcEnv = Map.fromList [("ORIGINS", "https://a.example.com,https://b.example.com")]}))
    assertBool (show problems) (any ("ORIGINS" `Text.isInfixOf`) problems)
    assertBool (show problems) (not (any ("a.example.com" `Text.isInfixOf`) problems))

refusedGraph :: IO ()
refusedGraph = do
    let dag = dagOf (withBalancer (\l -> l{lbDefaultService = "acme-web"}))
    case texts dag of
        [(_, said)] -> do
            assertBool (show said) (any ("(refused)" `Text.isInfixOf`) said)
            assertBool (show said) (any ("undeclared service: acme-web" `Text.isInfixOf`) said)
        others -> assertFailure ("expected the one refusing node, found " <> show (length others))

-------------------------------------------------------------------------------
-- The graph

noConflicts :: IO ()
noConflicts =
    mapM_
        (\(what, t) -> assertEqual what 0 (length (Dag.dagConflicts (dagOf t))))
        [ ("the example", example)
        , ("two services, one balancer", fuller)
        , ("two services sharing an account and a secret", example{turnupServices = [api, api{svcName = "acme-worker"}], turnupLoadBalancer = Nothing})
        ]

serviceOrder :: IO ()
serviceOrder = do
    let dag = dagOf example
    service <- theOne "deploys CloudRun service acme-api" dag
    account <- theOne "creates service account acme-api" dag
    version <- theOne "to secret acme-db-password" dag
    secret <- theOne "creates secret acme-db-password" dag
    runApi <- theOne "enables the run.googleapis.com API" dag
    secretApi <- theOne "enables the secretmanager.googleapis.com API" dag
    iamApi <- theOne "enables the iam.googleapis.com API" dag
    let under = below dag service
        grants = named "grants roles/secretmanager.secretAccessor to serviceAccount:acme-api@acme-prod.iam.gserviceaccount.com" dag
    mapM_
        (\(what, rf) -> assertBool what (rf `Set.member` under))
        [("the account", account), ("the uploaded version", version), ("the secret", secret), ("the Cloud Run API", runApi)]
    assertEqual "a grant per bound secret" 2 (length grants)
    assertBool "the grants are made before the deploy" (all (`Set.member` under) grants)
    assertEqual
        "one on each secret"
        [["on the secret acme-db-password"], ["on the secret acme-tls-key"]]
        (Set.toList (Set.fromList [filter ("on the secret" `Text.isPrefixOf`) (notesOf dag g) | g <- grants]))
    assertBool "a grant waits for the account it names" (all ((account `Set.member`) . below dag) grants)
    assertBool "the account waits for the IAM API" (iamApi `Set.member` below dag account)
    -- the secret is declared twice, under its version and beside it: the
    -- edge to the API is the second one's
    assertBool "the secret waits for its API" (secretApi `Set.member` below dag secret)
    assertBool "the version waits for the secret" (secret `Set.member` below dag version)
    -- a secret the file does not make is bound and granted on, not created
    assertEqual "" [] (named "creates secret acme-tls-key" dag)
    -- and with the grants left to somebody else there is none
    let ungranted = dagOf (withService (\s -> s{svcGrantSecretAccess = False}))
    assertEqual "" [] (named "grants roles/secretmanager.secretAccessor" ungranted)
    -- an account that exists is not made
    assertEqual "" [] (named "creates service account" (dagOf (withService (\s -> s{svcAccount = ExistingAccount "api@acme-prod.iam.gserviceaccount.com"}))))

balancerOrder :: IO ()
balancerOrder = do
    let dag = dagOf fuller
    root <- theOne "Cloud Run turnup acme (acme-api, acme-web)" dag
    balancer <- theOne "application load balancer acme-lb" dag
    apiService <- theOne "deploys CloudRun service acme-api" dag
    webService <- theOne "deploys CloudRun service acme-web" dag
    subnet <- theOne "creates subnet acme-proxy for REGIONAL_MANAGED_PROXY" dag
    computeApi <- theOne "enables the compute.googleapis.com API" dag
    let parts = [rf | rf <- named "acme-lb" dag, rf /= balancer]
    assertBool "the balancer is made of resources" (length parts > 4)
    mapM_
        ( \part ->
            mapM_
                (\(what, rf) -> assertBool (what <> " under " <> show part) (rf `Set.member` below dag part))
                [("acme-api", apiService), ("acme-web", webService), ("the subnet", subnet), ("the compute API", computeApi)]
        )
        parts
    assertBool "the turnup is over the balancer" (balancer `Set.member` below dag root)
    -- a service the balancer does not route to is still the turnup's
    let apart = dagOf fuller{turnupLoadBalancer = fmap (\l -> l{lbHosts = []}) fuller.turnupLoadBalancer}
    root' <- theOne "Cloud Run turnup acme" apart
    lonely <- theOne "deploys CloudRun service acme-api" apart
    balancer' <- theOne "application load balancer acme-lb" apart
    assertBool "" (lonely `Set.member` below apart root')
    assertBool "" (not (lonely `Set.member` below apart balancer'))

accountUnderAll :: IO ()
accountUnderAll = do
    let dag = dagOf fuller
    account <- theOne "asserts the account gcloud acts as" dag
    assertEqual "" ["declared account: deployer@example.com"] (notesOf dag account)
    gcloud <- theOne "gcloud CLI on PATH" dag
    let exposed = [(rf, said) | (rf, said) <- texts dag, rf `notElem` [account, gcloud], not (account `Set.member` below dag rf)]
    assertEqual "nodes that could run before the account is asserted" [] (map snd exposed)
    -- and without one there is no such node
    assertEqual "" [] (named "asserts the account" (dagOf example))

noApis :: IO ()
noApis = do
    assertEqual "" 4 (length (named "enables the" (dagOf example)))
    assertEqual "" [] (named "enables the" (dagOf example{turnupEnableApis = False}))

keptNodes :: IO ()
keptNodes = do
    let dag = dagOf fuller
        kept words' = do
            rf <- theOne words' dag
            assertBool (show words' <> ": " <> show (notesOf dag rf)) (keptNote `elem` notesOf dag rf)
        notKept words' = do
            rf <- theOne words' dag
            assertBool (show words') (keptNote `notElem` notesOf dag rf)
    mapM_
        kept
        [ "creates service account acme-api"
        , "creates secret acme-db-password"
        , "creates subnet acme-proxy"
        ]
    mapM_ (\g -> assertBool "a grant" (keptNote `elem` notesOf dag g)) (named "grants roles/secretmanager.secretAccessor" dag)
    mapM_
        notKept
        [ "deploys CloudRun service acme-api"
        , "deploys CloudRun service acme-web"
        , "application load balancer acme-lb"
        , -- nothing of gcloud's own node moved, or it would no longer be the node everything else names
          "gcloud CLI on PATH"
        ]
    assertEqual
        "nodes still promising a deletion that does not happen"
        []
        [said | (_, said) <- texts dag, any ("destroys every version" `Text.isInfixOf`) said]
    root <- theOne "Cloud Run turnup acme" dag
    assertBool "the turnup says what goes" (any ("deletes the services carrying the label salmon-turnup=acme and the resources of the balancer acme-lb" `Text.isInfixOf`) (notesOf dag root))
    assertBool "and what stays" (any ("keeps the secrets" `Text.isInfixOf`) (notesOf dag root))

noSecretBytes :: IO ()
noSecretBytes =
    withSystemTempDirectory "salmon-cloudrun" $ \dir -> do
        let marker = "hunter2-not-for-any-report"
        writeFile (dir </> "db-password") marker
        writeFile (dir </> "turnup.json") (replace "/run/acme/db-password" "db-password" exampleJson)
        result <- loadTurnup (dir </> "turnup.json")
        case result of
            Left why -> assertFailure why
            Right t -> do
                assertBool "the directive" (not (marker `isInfixOf` LChar8.unpack (encode t)))
                assertEqual "nodes" [] [said | (_, said) <- texts (dagOf t), any (Text.pack marker `Text.isInfixOf`) said]

-------------------------------------------------------------------------------
-- What is deployed

deployArgv :: IO ()
deployArgv =
    assertEqual
        ""
        ( RawCommand
            "gcloud"
            [ "run"
            , "deploy"
            , "acme-api"
            , "--image"
            , "europe-west1-docker.pkg.dev/acme-prod/acme/api:1.4.2"
            , "--service-account"
            , "acme-api@acme-prod.iam.gserviceaccount.com"
            , "--ingress"
            , "internal-and-cloud-load-balancing"
            , "--set-env-vars"
            , "LOG_LEVEL=info"
            , "--set-secrets"
            , "DB_PASSWORD=acme-db-password:latest,/secrets/tls/key.pem=acme-tls-key:3"
            , "--no-invoker-iam-check"
            , "--update-labels"
            , "salmon-turnup=acme"
            , "--region"
            , "europe-west1"
            , "--project"
            , "acme-prod"
            , "--quiet"
            ]
        )
        (cmdspec (prepare CloudRun.cloudRunCommand (CloudRun.RunDeploy (cloudRunServiceOf example api))))

ownerLabels :: IO ()
ownerLabels = do
    assertEqual "" (CloudRun.OwnerLabel "salmon-turnup" "acme") (ownerLabel example)
    mapM_
        (\s -> assertEqual (show s.svcName) (Just (ownerLabel fuller)) (cloudRunServiceOf fuller s).crsOptions.croOwner)
        fuller.turnupServices
    let dag = dagOf fuller
    mapM_
        ( \name -> do
            rf <- theOne ("deploys CloudRun service " <> name) dag
            assertBool (show (notesOf dag rf)) (any ("owned through the label salmon-turnup=acme" `Text.isInfixOf`) (notesOf dag rf))
        )
        ["acme-api", "acme-web"]
    -- the knobs of the second service
    case [cloudRunServiceOf fuller s | s <- fuller.turnupServices, s.svcName == "acme-web"] of
        [web] -> do
            assertEqual "" "web@acme-prod.iam.gserviceaccount.com" web.crsServiceAccount
            assertEqual "" (True, False) (web.crsOptions.croAllowUnauthenticated, web.crsOptions.croInvokerIamCheckDisabled)
            assertEqual "" (Just 1, Just 4, True) (web.crsOptions.croMinInstances, web.crsMaxInstances, web.crsOptions.croCpuAlwaysAllocated)
        _ -> assertFailure "one acme-web"

balancerShape :: IO ()
balancerShape = do
    let lb = maybe (error "a balancer") id fuller.turnupLoadBalancer
        alb = balancerOf fuller lb
    assertEqual "the default backend" [LoadBalancing.CloudRunBackend "acme-web"] alb.albBackends
    assertEqual "one more, for the service a path names" [LoadBalancing.BackendService "acme-api" [LoadBalancing.CloudRunBackend "acme-api"] Nothing Nothing] alb.albServices
    assertEqual
        "the rules"
        [ LoadBalancing.HostRule
            ["app.example.com"]
            LoadBalancing.DefaultService
            [LoadBalancing.PathRule ["/api/*"] (LoadBalancing.NamedService "acme-api") LoadBalancing.KeepPath]
        ]
        alb.albHostRules
    assertEqual "the certificate is one that exists" [LoadBalancing.ComputeCertificate "acme-cert"] alb.albCertificates
    assertEqual "port 80" (LoadBalancing.RedirectToHttps LoadBalancing.MovedPermanently) alb.albHttp
    assertEqual "the network" (Just "acme-net") alb.albNetwork
    -- and the plain one is the balancer module's plain one
    assertEqual
        ""
        (LoadBalancing.httpLoadBalancer "acme-lb" (cloudRunServiceOf example api).crsProject (cloudRunServiceOf example api).crsRegion [LoadBalancing.CloudRunBackend "acme-api"] Nothing)
        (balancerOf example exampleBalancer)

_unused :: ()
_unused = const () (dynamics, notes, ref, help)
