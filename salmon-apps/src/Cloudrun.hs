{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A Cloud Run turnup from one declarative file: the services, what each
runs as, its plain environment, the Secret Manager secrets bound to it, and
optionally a regional external Application Load Balancer in front.

> salmon-cloudrun config --file turnup.json | salmon-cloudrun run tree
> salmon-cloudrun config --file turnup.json | salmon-cloudrun run up
> salmon-cloudrun config --file turnup.json | salmon-cloudrun run down

The file is JSON (anything that prints JSON will do for writing it):

> {
>   "name": "acme",
>   "project": "acme-prod",
>   "region": "europe-west1",
>   "secrets": [
>     {"name": "acme-db-password", "source_file": "secrets/db-password"}
>   ],
>   "services": [
>     {
>       "name": "acme-api",
>       "image": "europe-west1-docker.pkg.dev/acme-prod/acme/api:1.4.2",
>       "service_account": {"create": "acme-api"},
>       "env": {"LOG_LEVEL": "info"},
>       "bind_secrets": [
>         {"env": "DB_PASSWORD", "secret": "acme-db-password"},
>         {"file": "/secrets/tls/key.pem", "secret": "acme-tls-key", "version": "3"}
>       ],
>       "ingress": "internal-and-cloud-load-balancing",
>       "invoker": "no-iam-check"
>     }
>   ],
>   "load_balancer": {
>     "name": "acme-lb",
>     "proxy_subnet": {"name": "acme-proxy", "range": "192.168.100.0/24"},
>     "default_service": "acme-api"
>   }
> }

The directive @config@ prints is that same declaration, checked and with
every default written out; there is nothing to resolve later, so what
@run up@ reads is what the operator can read.

== What is declared

* __Services__ are deployed from an image that is already in a registry.
  Building and pushing is not this binary's ("SreBox.Gcp.CloudRunDeploy" is
  the recipe for that).
* __Service accounts__: @{"create": ID}@ makes the account in the project,
  @{"email": ADDRESS}@ names one that exists.
* __Secrets__ come in two places. The top-level @secrets@ are the ones this
  turnup /makes/: the secret, and with a @source_file@ its latest version,
  uploaded when the file's bytes differ from it. @bind_secrets@ is what a
  service /sees/, as an environment variable or as a file; the secret it
  names is one of those or one that exists already. A service's account is
  granted @roles\/secretmanager.secretAccessor@ on every secret bound to it
  (@grant_secret_access: false@ when somebody else does the granting).
* The __load balancer__ is a regional external Application Load Balancer
  ("Salmon.Builtin.Nodes.Gcp.LoadBalancing") with one serverless backend per
  service it routes to. HTTPS takes regional @compute ssl-certificates@ that
  exist already, by name.

== Secrets

No secret byte is in the file, in the directive, in a node's text or in a
command line. A @source_file@ is a path on the machine that runs @run up@,
read when the upload node runs or checks. How the file got there is not this
binary's business.

== What @run down@ removes, and what it keeps

It deletes what can be proved to be this turnup's and holds no data: the
Cloud Run services, which carry the label @salmon-turnup=NAME@ from the
deploy that made them (a service of the same name without it is neither
deployed over nor deleted, see 'CloudRun.OwnerLabel'), and the balancer's
resources, which are named after the balancer.

It keeps everything else: the secrets and their versions, the service
accounts, the IAM grants, the proxy-only subnet and the enabled APIs. None
of those carries a mark saying who made it, a secret is data, and the others
may be shared with what this file does not describe. Their nodes say so.

__Layer 0 only.__ Nothing here has been run against a real project; see
@resources\/module-notes.md@ for what was tested and for what a live run
still has to cover.
-}
module Cloudrun (
    main,
    Seed (..),
    configure,
    program,

    -- * The declaration
    Turnup (..),
    ManagedSecret (..),
    Service (..),
    ServiceIdentity (..),
    Invoker (..),
    SecretBinding (..),
    BindingTarget (..),
    LoadBalancer (..),
    ProxySubnet (..),
    HostRoute (..),
    PathRoute (..),
    HttpMode (..),
    parseTurnup,
    loadTurnup,
    turnupProblems,

    -- * What it becomes
    turnupOp,
    cloudRunServiceOf,
    balancerOf,
    accountEmail,
    ownerLabel,
    keptNote,
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, object, withObject, withText, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Object, Pair, Parser)
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (nub, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative (execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, strOption, (<**>))
import Options.Generic (ParseRecord (..))
import System.Directory (makeAbsolute)
import System.FilePath (isAbsolute, takeDirectory, (</>))

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Extension, Op, Track', check, deps, down, nodeps, notes, op, ref, up)
import qualified Salmon.Builtin.Extension as Extension
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as LoadBalancing
import qualified Salmon.Builtin.Nodes.Gcp.SecretManager as SecretManager
import qualified Salmon.Builtin.Nodes.Gcp.ServiceUsage as ServiceUsage
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (OpGraph (..), inject)
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let desc =
            fullDesc
                <> progDesc "Cloud Run services, their secrets and environment, and a load balancer in front, from one declarative file"
                <> header "salmon-cloudrun"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeedWith Serve.reportText reportPrint configure program cmd

-- | The directive is the declaration itself: there is nothing to resolve later.
program :: Track' Turnup
program = Track turnupOp

-- | What an operator types: where the declaration is.
newtype Seed = Seed {seedFile :: FilePath}

instance ParseRecord Seed where
    parseRecord = (Seed <$> strOption (long "file" <> metavar "PATH" <> help "the turnup's declaration, as JSON")) <**> helper

{- | Reads the file and refuses a declaration with something wrong in it,
before anything is printed that a @run up@ could act on.
-}
configure :: Configure IO Seed Turnup
configure = Configure $ \seed -> do
    result <- loadTurnup seed.seedFile
    case result of
        Left why -> throwIO (userError ("salmon-cloudrun: " <> why))
        Right turnup -> pure turnup

{- | The declaration in a file, or everything that is wrong with it.

A relative @source_file@ is taken from the declaration's own directory, so
the directive names the same file wherever @run up@ is started from.
-}
loadTurnup :: FilePath -> IO (Either String Turnup)
loadTurnup path = do
    bytes <- LByteString.readFile path
    home <- takeDirectory <$> makeAbsolute path
    pure $ do
        turnup <- anchored home <$> parseTurnup bytes
        case turnupProblems turnup of
            [] -> Right turnup
            problems -> Left (Text.unpack (Text.intercalate "; " problems))
  where
    anchored home t = t{turnupSecrets = map (anchor home) t.turnupSecrets}
    anchor home s = s{msSourceFile = fmap (\f -> if isAbsolute f then f else home </> f) s.msSourceFile}

-- | The declaration in some JSON, with no opinion yet on whether it holds together.
parseTurnup :: LByteString.ByteString -> Either String Turnup
parseTurnup = eitherDecode

-------------------------------------------------------------------------------
-- The declaration

data Turnup = Turnup
    { turnupName :: Text
    -- ^ what the turnup is called: the value of the label its services
    -- carry, so GCP's label rules apply
    , turnupProject :: Text
    , turnupRegion :: Text
    , turnupAccount :: Maybe Text
    -- ^ the account @gcloud@ must be acting as, asserted before anything
    -- else ('Core.declaredAccount'); 'Nothing' asserts nothing
    , turnupEnableApis :: Bool
    -- ^ whether the APIs the declaration needs are enabled by it
    , turnupSecrets :: [ManagedSecret]
    , turnupServices :: [Service]
    , turnupLoadBalancer :: Maybe LoadBalancer
    }
    deriving (Eq, Show)

-- | A secret this turnup makes.
data ManagedSecret = ManagedSecret
    { msName :: Text
    , msReplication :: Text
    -- ^ @automatic@ unless said otherwise
    , msSourceFile :: Maybe FilePath
    -- ^ a file, on the machine running @run up@, whose bytes the secret's
    -- latest version must hold. 'Nothing': the secret is made and its
    -- versions are somebody else's.
    }
    deriving (Eq, Show)

data Service = Service
    { svcName :: Text
    , svcImage :: Text
    , svcAccount :: ServiceIdentity
    , svcEnv :: Map Text Text
    , svcSecrets :: [SecretBinding]
    , svcGrantSecretAccess :: Bool
    , svcIngress :: CloudRun.IngressSetting
    , svcInvoker :: Invoker
    , svcCpu :: Maybe Text
    , svcMemory :: Maybe Text
    , svcConcurrency :: Maybe Int
    , svcTimeoutSeconds :: Maybe Int
    , svcPort :: Maybe Int
    , svcMinInstances :: Maybe Int
    , svcMaxInstances :: Maybe Int
    , svcCpuAlwaysAllocated :: Bool
    }
    deriving (Eq, Show)

-- | Who a service runs as.
data ServiceIdentity
    = -- | an account id; the account is made in the turnup's project
      CreateAccount Text
    | -- | the address of an account that exists
      ExistingAccount Text
    deriving (Eq, Show)

{- | Who may call a service. 'InvokerIam' passes nothing and leaves the
service as Cloud Run makes it (callers need @roles\/run.invoker@); the other
two are 'CloudRun.croAllowUnauthenticated' and
'CloudRun.croInvokerIamCheckDisabled', which see for which one an
organization lets through. A service behind the balancer is called by
anonymous clients, so it wants one of those two.
-}
data Invoker
    = InvokerIam
    | InvokerAllUsers
    | InvokerNoIamCheck
    deriving (Eq, Show)

-- | A secret a service sees, and where.
data SecretBinding = SecretBinding
    { sbTarget :: BindingTarget
    , sbSecret :: Text
    , sbVersion :: Text
    -- ^ @latest@ unless said otherwise
    }
    deriving (Eq, Show)

data BindingTarget
    = EnvVar Text
    | -- | an absolute path in the container
      MountedFile FilePath
    deriving (Eq, Show)

data LoadBalancer = LoadBalancer
    { lbName :: Text
    , lbNetwork :: Maybe Text
    -- ^ the VPC network; 'Nothing' is GCP's @default@
    , lbProxySubnet :: Maybe ProxySubnet
    -- ^ the proxy-only subnet such a balancer needs in its network and
    -- region. 'Nothing' when the network has one already: there can be only
    -- one per network and region.
    , lbDefaultService :: Text
    -- ^ where a request goes that no host rule claims
    , lbHosts :: [HostRoute]
    , lbCertificates :: [Text]
    -- ^ regional @compute ssl-certificates@ that exist; non-empty also
    -- serves HTTPS
    , lbHttp :: Maybe HttpMode
    -- ^ 'Nothing' is the balancer module's own default for port 80
    }
    deriving (Eq, Show)

data ProxySubnet = ProxySubnet
    { psName :: Text
    , psRange :: Text
    }
    deriving (Eq, Show)

data HostRoute = HostRoute
    { hrHosts :: [Text]
    , hrService :: Text
    , hrPaths :: [PathRoute]
    }
    deriving (Eq, Show)

data PathRoute = PathRoute
    { prPaths :: [Text]
    , prService :: Text
    }
    deriving (Eq, Show)

data HttpMode
    = HttpServe
    | HttpRedirectToHttps
    | HttpNone
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- JSON

{- | An object with these keys and no other. A misspelt key in a file like
this one is a setting silently not applied, so it is refused instead.
-}
objectOf :: String -> [Text] -> (Object -> Parser a) -> Value -> Parser a
objectOf what allowed f = withObject what $ \o ->
    case map Key.toText (KeyMap.keys o) \\ allowed of
        [] -> f o
        unknown -> fail (what <> ": unknown key " <> Text.unpack (Text.intercalate ", " unknown))

-- | The pairs that have a value: an absent setting is an absent key.
objectWith :: [Maybe Pair] -> Value
objectWith = object . catMaybes

always :: (ToJSON v) => Text -> v -> Maybe Pair
always k v = Just (Key.fromText k .= v)

whenSet :: (ToJSON v) => Text -> Maybe v -> Maybe Pair
whenSet k = fmap (Key.fromText k .=)

instance FromJSON Turnup where
    parseJSON =
        objectOf "turnup" ["name", "project", "region", "account", "enable_apis", "secrets", "services", "load_balancer"] $ \o ->
            Turnup
                <$> o .: "name"
                <*> o .: "project"
                <*> o .: "region"
                <*> o .:? "account"
                <*> o .:? "enable_apis" .!= True
                <*> o .:? "secrets" .!= []
                <*> o .: "services"
                <*> o .:? "load_balancer"

instance ToJSON Turnup where
    toJSON t =
        objectWith
            [ always "name" t.turnupName
            , always "project" t.turnupProject
            , always "region" t.turnupRegion
            , whenSet "account" t.turnupAccount
            , always "enable_apis" t.turnupEnableApis
            , always "secrets" t.turnupSecrets
            , always "services" t.turnupServices
            , whenSet "load_balancer" t.turnupLoadBalancer
            ]

instance FromJSON ManagedSecret where
    parseJSON =
        objectOf "secret" ["name", "replication", "source_file"] $ \o ->
            ManagedSecret
                <$> o .: "name"
                <*> o .:? "replication" .!= "automatic"
                <*> o .:? "source_file"

instance ToJSON ManagedSecret where
    toJSON s =
        objectWith
            [ always "name" s.msName
            , always "replication" s.msReplication
            , whenSet "source_file" s.msSourceFile
            ]

instance FromJSON Service where
    parseJSON =
        objectOf
            "service"
            [ "name"
            , "image"
            , "service_account"
            , "env"
            , "bind_secrets"
            , "grant_secret_access"
            , "ingress"
            , "invoker"
            , "cpu"
            , "memory"
            , "concurrency"
            , "timeout_seconds"
            , "port"
            , "min_instances"
            , "max_instances"
            , "cpu_always_allocated"
            ]
            $ \o ->
                Service
                    <$> o .: "name"
                    <*> o .: "image"
                    <*> o .: "service_account"
                    <*> o .:? "env" .!= Map.empty
                    <*> o .:? "bind_secrets" .!= []
                    <*> o .:? "grant_secret_access" .!= True
                    <*> (maybe (pure CloudRun.All) parseIngress =<< o .:? "ingress")
                    <*> o .:? "invoker" .!= InvokerIam
                    <*> o .:? "cpu"
                    <*> o .:? "memory"
                    <*> o .:? "concurrency"
                    <*> o .:? "timeout_seconds"
                    <*> o .:? "port"
                    <*> o .:? "min_instances"
                    <*> o .:? "max_instances"
                    <*> o .:? "cpu_always_allocated" .!= False

instance ToJSON Service where
    toJSON s =
        objectWith
            [ always "name" s.svcName
            , always "image" s.svcImage
            , always "service_account" s.svcAccount
            , always "env" s.svcEnv
            , always "bind_secrets" s.svcSecrets
            , always "grant_secret_access" s.svcGrantSecretAccess
            , always "ingress" (ingressText s.svcIngress)
            , always "invoker" s.svcInvoker
            , whenSet "cpu" s.svcCpu
            , whenSet "memory" s.svcMemory
            , whenSet "concurrency" s.svcConcurrency
            , whenSet "timeout_seconds" s.svcTimeoutSeconds
            , whenSet "port" s.svcPort
            , whenSet "min_instances" s.svcMinInstances
            , whenSet "max_instances" s.svcMaxInstances
            , always "cpu_always_allocated" s.svcCpuAlwaysAllocated
            ]

ingressText :: CloudRun.IngressSetting -> Text
ingressText = \case
    CloudRun.All -> "all"
    CloudRun.Internal -> "internal"
    CloudRun.InternalAndLoadBalancing -> "internal-and-cloud-load-balancing"

parseIngress :: Value -> Parser CloudRun.IngressSetting
parseIngress = withText "ingress" $ \case
    "all" -> pure CloudRun.All
    "internal" -> pure CloudRun.Internal
    "internal-and-cloud-load-balancing" -> pure CloudRun.InternalAndLoadBalancing
    other -> fail ("ingress: all, internal or internal-and-cloud-load-balancing, not " <> Text.unpack other)

instance FromJSON ServiceIdentity where
    parseJSON =
        objectOf "service_account" ["create", "email"] $ \o -> do
            create <- o .:? "create"
            email <- o .:? "email"
            case (create, email) of
                (Just accountId, Nothing) -> pure (CreateAccount accountId)
                (Nothing, Just address) -> pure (ExistingAccount address)
                _ -> fail "service_account: exactly one of create (an account id to make) and email (an account that exists)"

instance ToJSON ServiceIdentity where
    toJSON = \case
        CreateAccount accountId -> object ["create" .= accountId]
        ExistingAccount address -> object ["email" .= address]

instance FromJSON Invoker where
    parseJSON = withText "invoker" $ \case
        "iam" -> pure InvokerIam
        "all-users" -> pure InvokerAllUsers
        "no-iam-check" -> pure InvokerNoIamCheck
        other -> fail ("invoker: iam, all-users or no-iam-check, not " <> Text.unpack other)

instance ToJSON Invoker where
    toJSON = \case
        InvokerIam -> "iam"
        InvokerAllUsers -> "all-users"
        InvokerNoIamCheck -> "no-iam-check"

instance FromJSON SecretBinding where
    parseJSON =
        objectOf "secret binding" ["env", "file", "secret", "version"] $ \o -> do
            var <- o .:? "env"
            file <- o .:? "file"
            target <- case (var, file) of
                (Just v, Nothing) -> pure (EnvVar v)
                (Nothing, Just f) -> pure (MountedFile f)
                _ -> fail "secret binding: exactly one of env (a variable) and file (a path in the container)"
            SecretBinding target <$> o .: "secret" <*> o .:? "version" .!= "latest"

instance ToJSON SecretBinding where
    toJSON b =
        object
            [ case b.sbTarget of
                EnvVar v -> "env" .= v
                MountedFile f -> "file" .= f
            , "secret" .= b.sbSecret
            , "version" .= b.sbVersion
            ]

instance FromJSON LoadBalancer where
    parseJSON =
        objectOf "load_balancer" ["name", "network", "proxy_subnet", "default_service", "hosts", "certificates", "http"] $ \o ->
            LoadBalancer
                <$> o .: "name"
                <*> o .:? "network"
                <*> o .:? "proxy_subnet"
                <*> o .: "default_service"
                <*> o .:? "hosts" .!= []
                <*> o .:? "certificates" .!= []
                <*> o .:? "http"

instance ToJSON LoadBalancer where
    toJSON l =
        objectWith
            [ always "name" l.lbName
            , whenSet "network" l.lbNetwork
            , whenSet "proxy_subnet" l.lbProxySubnet
            , always "default_service" l.lbDefaultService
            , always "hosts" l.lbHosts
            , always "certificates" l.lbCertificates
            , whenSet "http" l.lbHttp
            ]

instance FromJSON ProxySubnet where
    parseJSON = objectOf "proxy_subnet" ["name", "range"] $ \o -> ProxySubnet <$> o .: "name" <*> o .: "range"

instance ToJSON ProxySubnet where
    toJSON s = object ["name" .= s.psName, "range" .= s.psRange]

instance FromJSON HostRoute where
    parseJSON =
        objectOf "host route" ["hosts", "service", "paths"] $ \o ->
            HostRoute <$> o .: "hosts" <*> o .: "service" <*> o .:? "paths" .!= []

instance ToJSON HostRoute where
    toJSON h = object ["hosts" .= h.hrHosts, "service" .= h.hrService, "paths" .= h.hrPaths]

instance FromJSON PathRoute where
    parseJSON = objectOf "path route" ["paths", "service"] $ \o -> PathRoute <$> o .: "paths" <*> o .: "service"

instance ToJSON PathRoute where
    toJSON p = object ["paths" .= p.prPaths, "service" .= p.prService]

instance FromJSON HttpMode where
    parseJSON = withText "http" $ \case
        "serve" -> pure HttpServe
        "redirect-to-https" -> pure HttpRedirectToHttps
        "none" -> pure HttpNone
        other -> fail ("http: serve, redirect-to-https or none, not " <> Text.unpack other)

instance ToJSON HttpMode where
    toJSON = \case
        HttpServe -> "serve"
        HttpRedirectToHttps -> "redirect-to-https"
        HttpNone -> "none"

-------------------------------------------------------------------------------
-- What is wrong with a declaration

{- | Everything wrong with a declaration that no @gcloud@ call could put
right, all of it rather than the first: names GCP would refuse, two things
under one name, a binding or a route naming nothing, two bindings landing on
one variable or in one directory, and whatever the balancer module says of
the balancer ('LoadBalancing.albProblems').

Two refusals are about this binary rather than about GCP:

* a plain environment value holding a comma. @gcloud@ reads
  @--set-env-vars@ as a comma-separated list, so the value would be cut in
  two (or refused) on the way;
* a balancer routing to a service whose ingress is @internal@, which an
  external balancer cannot reach.
-}
turnupProblems :: Turnup -> [Text]
turnupProblems t =
    concat
        [ ["the turnup's name is not usable as a label value (a lowercase letter, then lowercase letters, digits and -, 63 at most): " <> t.turnupName | not (labelLike 63 t.turnupName)]
        , ["no project" | Text.null t.turnupProject]
        , ["no region" | Text.null t.turnupRegion]
        , ["no service declared" | null t.turnupServices]
        , ["service declared twice: " <> n | n <- duplicates (map svcName t.turnupServices)]
        , ["secret declared twice: " <> n | n <- duplicates (map msName t.turnupSecrets)]
        , ["secret with no name" | any (Text.null . msName) t.turnupSecrets]
        , concatMap serviceProblems t.turnupServices
        , maybe [] balancerProblems t.turnupLoadBalancer
        ]
  where
    serviceNames = map svcName t.turnupServices

    serviceProblems :: Service -> [Text]
    serviceProblems s =
        map
            (("service " <> s.svcName <> ": ") <>)
            ( concat
                [ ["not a Cloud Run service name (a lowercase letter, then lowercase letters, digits and -, 49 at most)" | not (labelLike 49 s.svcName)]
                , ["no image" | Text.null s.svcImage]
                , case s.svcAccount of
                    CreateAccount accountId ->
                        ["not a service account id (6 to 30 of lowercase letters, digits and -, starting with a letter): " <> accountId | not (labelLike 30 accountId && Text.length accountId >= 6)]
                    ExistingAccount address ->
                        ["not a service account's address: " <> address | not ("@" `Text.isInfixOf` address)]
                , ["not an environment variable name: " <> k | k <- Map.keys s.svcEnv <> secretVars, not (variableLike k)]
                , -- the name alone: a value is not for a report
                  ["the value of " <> k <> " holds a comma, which gcloud's --set-env-vars would split on" | (k, v) <- Map.toList s.svcEnv, "," `Text.isInfixOf` v]
                , ["variable set twice (as plain environment and as a secret, or by two secrets): " <> k | k <- duplicates (Map.keys s.svcEnv <> secretVars)]
                , ["a secret file's path is not absolute: " <> Text.pack f | f <- secretFiles, not (isAbsolute f)]
                , ["file bound twice: " <> Text.pack f | f <- duplicates secretFiles]
                , [ "two secrets mounted in one directory (a secret volume is the whole directory): " <> Text.pack d
                  | d <- nub (map (takeDirectory . fst) mounts)
                  , length (nub [secret | (f, secret) <- mounts, takeDirectory f == d]) > 1
                  ]
                , ["a binding names no secret" | any (Text.null . sbSecret) s.svcSecrets]
                , ["a binding of " <> b.sbSecret <> " names no version" | b <- s.svcSecrets, Text.null b.sbVersion]
                ]
            )
      where
        secretVars = [v | SecretBinding (EnvVar v) _ _ <- s.svcSecrets]
        mounts = [(f, secret) | SecretBinding (MountedFile f) secret _ <- s.svcSecrets]
        secretFiles = map fst mounts

    balancerProblems :: LoadBalancer -> [Text]
    balancerProblems lb =
        concat
            [ ["the balancer's name is not a resource name: " <> lb.lbName | not (labelLike 63 lb.lbName)]
            , ["the balancer routes to an undeclared service: " <> n | n <- undeclared]
            , [ "the balancer routes to " <> s.svcName <> ", whose ingress is internal: an external balancer cannot reach it"
              | s <- t.turnupServices
              , s.svcName `elem` routed
              , s.svcIngress == CloudRun.Internal
              ]
            , [ "the balancer's resource for " <> n <> " would have a name longer than 63: " <> resource
              | n <- routed
              , let resource = lb.lbName <> "-" <> n <> "-backend"
              , Text.length resource > 63
              ]
            , ["the proxy subnet has no name or no range" | Just ps <- [lb.lbProxySubnet], Text.null ps.psName || Text.null ps.psRange]
            , -- the balancer module's own opinion, once the names it is built from hold
              if null undeclared then LoadBalancing.albProblems (balancerOf t lb) else []
            ]
      where
        routed = routedServices lb
        undeclared = routed \\ serviceNames

duplicates :: (Eq a) => [a] -> [a]
duplicates xs = nub (xs \\ nub xs)

-- | A lowercase letter, then lowercase letters, digits and @-@, not ending in @-@.
labelLike :: Int -> Text -> Bool
labelLike longest name = case Text.uncons name of
    Nothing -> False
    Just (c, rest) ->
        isAsciiLower c
            && Text.all (\x -> isAsciiLower x || isDigit x || x == '-') rest
            && Text.length name <= longest
            && not ("-" `Text.isSuffixOf` name)

variableLike :: Text -> Bool
variableLike name = case Text.uncons name of
    Nothing -> False
    Just (c, rest) -> letter c && Text.all (\x -> letter x || isDigit x) rest
  where
    letter x = isAsciiLower x || isAsciiUpper x || x == '_'

-- | The services a balancer sends to, the default one first, each once.
routedServices :: LoadBalancer -> [Text]
routedServices lb =
    nub (lb.lbDefaultService : concatMap (\h -> h.hrService : map prService h.hrPaths) lb.lbHosts)

-- | A declaration 'turnupOp' was handed with something wrong in it.
newtype InvalidTurnup = InvalidTurnup [Text]
    deriving (Show)

instance Exception InvalidTurnup

-------------------------------------------------------------------------------
-- What it becomes

-- | The label a turnup's services carry: @salmon-turnup=NAME@.
ownerLabel :: Turnup -> CloudRun.OwnerLabel
ownerLabel t = CloudRun.OwnerLabel "salmon-turnup" t.turnupName

-- | What the node of something @run down@ leaves in place says.
keptNote :: Text
keptNote = "kept by `run down`: nothing proves this turnup made it, and it may hold data or be shared"

-- | The address of the account a service runs as.
accountEmail :: Turnup -> ServiceIdentity -> Text
accountEmail t = \case
    CreateAccount accountId -> accountId <> "@" <> t.turnupProject <> ".iam.gserviceaccount.com"
    ExistingAccount address -> address

-- | The service as the Cloud Run node deploys it.
cloudRunServiceOf :: Turnup -> Service -> CloudRun.CloudRunService
cloudRunServiceOf t s =
    CloudRun.CloudRunService
        { CloudRun.crsName = s.svcName
        , CloudRun.crsProject = Core.Project t.turnupProject
        , CloudRun.crsRegion = Core.Region t.turnupRegion
        , CloudRun.crsImage = s.svcImage
        , CloudRun.crsEnv = s.svcEnv
        , CloudRun.crsServiceAccount = accountEmail t s.svcAccount
        , CloudRun.crsIngress = s.svcIngress
        , CloudRun.crsMaxInstances = s.svcMaxInstances
        , CloudRun.crsOptions =
            CloudRun.defaultCloudRunOptions
                { CloudRun.croSecrets = map binding s.svcSecrets
                , CloudRun.croCpu = s.svcCpu
                , CloudRun.croMemory = s.svcMemory
                , CloudRun.croConcurrency = s.svcConcurrency
                , CloudRun.croTimeoutSeconds = s.svcTimeoutSeconds
                , CloudRun.croPort = s.svcPort
                , CloudRun.croAllowUnauthenticated = s.svcInvoker == InvokerAllUsers
                , CloudRun.croInvokerIamCheckDisabled = s.svcInvoker == InvokerNoIamCheck
                , CloudRun.croMinInstances = s.svcMinInstances
                , CloudRun.croCpuAlwaysAllocated = s.svcCpuAlwaysAllocated
                , CloudRun.croOwner = Just (ownerLabel t)
                }
        }
  where
    binding :: SecretBinding -> CloudRun.SecretBinding
    binding b = case b.sbTarget of
        EnvVar v -> CloudRun.SecretEnvVar v b.sbSecret b.sbVersion
        MountedFile f -> CloudRun.SecretFile f b.sbSecret b.sbVersion

{- | The balancer as the balancer module makes it: the default backend
service sends to 'lbDefaultService', and every other service a rule names
has a backend service of its own, under the Cloud Run service's name.
-}
balancerOf :: Turnup -> LoadBalancer -> LoadBalancing.ApplicationLoadBalancer
balancerOf t lb =
    (LoadBalancing.httpLoadBalancer lb.lbName (Core.Project t.turnupProject) (Core.Region t.turnupRegion) [LoadBalancing.CloudRunBackend lb.lbDefaultService] Nothing)
        { LoadBalancing.albNetwork = lb.lbNetwork
        , LoadBalancing.albServices =
            [ LoadBalancing.BackendService n [LoadBalancing.CloudRunBackend n] Nothing Nothing
            | n <- drop 1 (routedServices lb)
            ]
        , LoadBalancing.albHostRules =
            [ LoadBalancing.HostRule
                h.hrHosts
                (target h.hrService)
                [LoadBalancing.PathRule p.prPaths (target p.prService) LoadBalancing.KeepPath | p <- h.hrPaths]
            | h <- lb.lbHosts
            ]
        , LoadBalancing.albCertificates = map LoadBalancing.ComputeCertificate lb.lbCertificates
        , LoadBalancing.albHttp = case lb.lbHttp of
            Nothing -> LoadBalancing.HttpCreatedOnce
            Just HttpServe -> LoadBalancing.ServeHttp
            Just HttpRedirectToHttps -> LoadBalancing.RedirectToHttps LoadBalancing.MovedPermanently
            Just HttpNone -> LoadBalancing.NoHttp
        }
  where
    target n
        | n == lb.lbDefaultService = LoadBalancing.DefaultService
        | otherwise = LoadBalancing.NamedService n

{- | The whole turnup under one node.

A declaration with something wrong in it becomes that one node alone, which
fails saying what: a directive can be written by hand, and half a turnup is
worse than none.
-}
turnupOp :: Turnup -> Op
turnupOp t = case turnupProblems t of
    [] -> turnupGraph t
    problems ->
        op "cloudrun-turnup" nodeps $ \actions ->
            actions
                { Extension.help = Text.unwords ["Cloud Run turnup", t.turnupName, "(refused)"]
                , notes = problems
                , ref = turnupRef t
                , up = throwIO (InvalidTurnup problems)
                , check = pure (Failure (Text.intercalate "; " problems))
                }

turnupRef :: Turnup -> Ref
turnupRef t = mkRef "cloudrun-turnup" (t.turnupProject, t.turnupRegion, t.turnupName)

turnupGraph :: Turnup -> Op
turnupGraph t =
    op "cloudrun-turnup" (deps (maybe [] (pure . balancer) t.turnupLoadBalancer <> map service t.turnupServices <> map managedSecret t.turnupSecrets)) $ \actions ->
        actions
            { Extension.help = Text.unwords ["Cloud Run turnup", t.turnupName, "(" <> Text.intercalate ", " (map svcName t.turnupServices) <> ")"]
            , notes =
                [ "`run down` deletes the services carrying the label "
                    <> CloudRun.renderOwnerLabel (ownerLabel t)
                    <> maybe "" (\lb -> " and the resources of the balancer " <> lb.lbName) t.turnupLoadBalancer
                , "`run down` keeps the secrets and their versions, the service accounts, the IAM grants, the proxy-only subnet and the enabled APIs"
                ]
            , ref = turnupRef t
            }
  where
    project = Core.Project t.turnupProject
    region = Core.Region t.turnupRegion

    -- who gcloud acts as, under everything
    foundation :: [Op]
    foundation = [Core.declaredAccount reportPrint Core.gcloud (Core.Account a) | Just a <- [t.turnupAccount]]

    needing :: Op -> [Op] -> Op
    needing o prerequisites = foldl inject o (prerequisites <> foundation)

    apis :: [Text] -> [Op]
    apis names
        | t.turnupEnableApis = [ServiceUsage.enableService reportPrint Core.gcloud project (ServiceUsage.Api n) `needing` [] | n <- names]
        | otherwise = []

    secretApi, runApi, iamApi, computeApi :: [Op]
    secretApi = apis ["secretmanager.googleapis.com"]
    runApi = apis ["run.googleapis.com"]
    iamApi = apis ["iam.googleapis.com"]
    computeApi = apis ["compute.googleapis.com"]

    -- Service accounts

    account :: ServiceIdentity -> [Op]
    account = \case
        CreateAccount accountId -> [keptNode (Iam.serviceAccount reportPrint Core.gcloud project accountId) `needing` iamApi]
        ExistingAccount _ -> []

    -- Secrets

    secretOf :: ManagedSecret -> SecretManager.Secret
    secretOf s = SecretManager.Secret s.msName project s.msReplication

    secretContainer :: ManagedSecret -> Op
    secretContainer s = keptSecret (SecretManager.secret reportPrint Core.gcloud (secretOf s)) `needing` secretApi

    -- The container is declared beside the version that already depends on
    -- it, for the edge to the API: the version's own copy of that node has
    -- none, and the two are one node once folded.
    managedSecret :: ManagedSecret -> Op
    managedSecret s = case s.msSourceFile of
        Nothing -> secretContainer s
        Just file ->
            keptSecret (SecretManager.secretVersion reportPrint Core.gcloud (SecretManager.SecretVersion (secretOf s) file))
                `needing` (secretContainer s : secretApi)

    managedNamed :: Text -> [Op]
    managedNamed name = [managedSecret s | s <- t.turnupSecrets, s.msName == name]

    grant :: Service -> Text -> Op
    grant s secretName =
        keptNode
            ( noted ["on the secret " <> secretName] $
                Iam.iamBinding
                    reportPrint
                    Core.gcloud
                    ( Iam.IamBinding
                        (Iam.ServiceAccount (accountEmail t s.svcAccount))
                        "roles/secretmanager.secretAccessor"
                        ("projects/" <> t.turnupProject <> "/secrets/" <> secretName)
                    )
            )
            `needing` (account s.svcAccount <> [secretContainer m | m <- t.turnupSecrets, m.msName == secretName] <> secretApi)

    -- Services

    service :: Service -> Op
    service s =
        CloudRun.cloudRunService reportPrint Core.gcloud (cloudRunServiceOf t s)
            `needing` concat
                [ account s.svcAccount
                , [grant s name | s.svcGrantSecretAccess, name <- bound]
                , concatMap managedNamed bound
                , runApi
                ]
      where
        bound = nub (map sbSecret s.svcSecrets)

    -- The balancer

    balancer :: LoadBalancer -> Op
    balancer lb =
        LoadBalancing.applicationLoadBalancerAfter
            (concat [proxySubnet, computeApi, map service routed, foundation])
            reportPrint
            Core.gcloud
            (balancerOf t lb)
      where
        routed = [s | s <- t.turnupServices, s.svcName `elem` routedServices lb]
        proxySubnet =
            [ keptNode
                ( Compute.subnet
                    reportPrint
                    Core.gcloud
                    Compute.Subnet
                        { Compute.subnetName = ps.psName
                        , Compute.subnetProject = project
                        , Compute.subnetRegion = region
                        , Compute.subnetNetwork = maybe "default" id lb.lbNetwork
                        , Compute.subnetRange = ps.psRange
                        , Compute.subnetPurpose = Compute.RegionalManagedProxy
                        }
                )
                `needing` computeApi
            | Just ps <- [lb.lbProxySubnet]
            ]

{- | The node as it was, except that @run down@ leaves its effect in place,
and that it says so. The one node: its dependencies are as they were.
-}
keptNode :: Op -> Op
keptNode o = noted [keptNote] o{node = fmap (\e -> e{down = pure ()}) o.node}

-- | The node as it was, saying this too. The one node.
noted :: [Text] -> Op -> Op
noted more o = o{node = fmap (\e -> e{notes = e.notes <> more}) o.node}

{- | The same for a secret, which is two nodes when it has a version (the
version's node declares the secret's own beneath it) and whose builtin note
promises a deletion that no longer happens.

This one goes through the whole of what it is given, so that the secret
declared under a version and the secret declared beside it stay one and the
same node. A node with a deletion to lose says it is kept; the others
(@gcloud@ itself) had none and are as they were, which is what keeps them
the same node as every other mention of them.
-}
keptSecret :: Op -> Op
keptSecret = fmap (fmap kept)
  where
    kept :: Extension -> Extension
    kept e
        | secretDeletionNote `elem` e.notes = e{down = pure (), notes = filter (/= secretDeletionNote) e.notes <> [keptNote]}
        | otherwise = e{down = pure ()}

-- | What "Salmon.Builtin.Nodes.Gcp.SecretManager".@secret@ says of its @down@.
secretDeletionNote :: Text
secretDeletionNote = "down destroys every version of the secret"
