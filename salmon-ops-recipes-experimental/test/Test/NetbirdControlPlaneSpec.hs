{- | Layer 0 coverage for "SreBox.NetbirdControlPlane": what is rendered
(@config.yaml@, @dashboard.env@, the two quadlets), what is refused, the
shape of the graph, and the file nodes against a scratch directory. No image
is pulled, no container is started and no NetBird server has read any of
this: the key names are compared with lists copied by hand from upstream's
sources at @v0.80.0@ ('upstreamConfigKeys', 'upstreamDashboardKeys').
-}
module Test.NetbirdControlPlaneSpec (tests) where

import Control.Exception (finally, try)
import Data.Bits ((.&.))
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (toList)
import Data.List (isSubsequenceOf, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import Test.QuickCheck hiding (Failure, Success, (.&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)
import SreBox.NetbirdControlPlane

base :: ControlPlane
base =
    controlPlaneFor
        "netbird.example.com"
        (SecretFile "/etc/salmon/netbird/relay.secret")
        (SecretFile "/etc/salmon/netbird/store.key")
        (SecretFile "/etc/salmon/netbird/cookie.key")

-- | 32 bytes in padded base64, as @openssl rand -base64 32@ prints them.
aKey :: Text
aKey = "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY="

someSecrets :: Secrets
someSecrets = Secrets{relayAuthSecret = "cmVsYXktc2VjcmV0LXZhbHVl", storeEncryptionKey = aKey, sessionCookieKey = "MDEyMzQ1Njc4OWFiY2RlZg=="}

tests :: TestTree
tests =
    testGroup
        "SreBox.NetbirdControlPlane"
        [ testGroup
            "declaration"
            [ testCase "the starting point has no problem" $ assertEqual "" [] (problems base)
            , testCase "the images are pinned to the versions that were read" $ do
                assertEqual "" "docker.io/netbirdio/netbird-server:0.80.0" serverImage
                assertEqual "" "docker.io/netbirdio/dashboard:v2.94.0" dashboardImage
            , testCase "an image that is not pinned is refused" $ do
                refused base{cpServerImage = "docker.io/netbirdio/netbird-server:latest"}
                refused base{cpDashboardImage = "docker.io/netbirdio/dashboard"}
                refused base{cpDashboardImage = "localhost:5000/netbirdio/dashboard"}
                accepted base{cpDashboardImage = "localhost:5000/netbirdio/dashboard:v2.94.0"}
                accepted base{cpServerImage = "docker.io/netbirdio/netbird-server@sha256:0123456789abcdef"}
            , testCase "what could not be written unquoted is refused" $ do
                refused base{cpDomain = ""}
                refused base{cpDomain = "netbird.example.com\" x: y"}
                refused base{cpDomain = "netbird..example.com"}
                refused base{cpConfigDir = "etc/netbird"}
                refused base{cpDataDir = "/var/lib/net bird"}
                refused base{cpBindAddress = "localhost:1"}
                refused base{cpTrustedProxies = ["10.0.0.1"]}
                refused base{cpLogLevel = "verbose"}
                refused base{cpStunPort = 0}
                refused base{cpDashboardPort = base.cpServerPort}
                refused base{cpReadyTimeout = Just 0}
            , testCase "two secrets in one file are refused" $
                refused base{cpSessionCookieKey = base.cpStoreEncryptionKey}
            , testCase "the management URL is the public base, with the port when it is not 443" $ do
                assertEqual "" "https://netbird.example.com" (managementUrl base)
                assertEqual "" "https://netbird.example.com:8443" (managementUrl base{cpPublicPort = 8443})
            ]
        , testGroup
            "config.yaml"
            [ testCase "the starting point, with the secrets shown as where they come from" $
                assertEqual
                    ""
                    ( Text.unlines
                        [ marker
                        , "server:"
                        , "  listenAddress: \":80\""
                        , "  exposedAddress: \"https://netbird.example.com:443\""
                        , "  stunPorts:"
                        , "    - 3478"
                        , "  metricsPort: 9090"
                        , "  healthcheckAddress: \":9000\""
                        , "  logLevel: \"info\""
                        , "  logFile: \"console\""
                        , "  authSecret: \"<read from /etc/salmon/netbird/relay.secret>\""
                        , "  dataDir: \"/var/lib/netbird\""
                        , "  auth:"
                        , "    issuer: \"https://netbird.example.com/oauth2\""
                        , "    signKeyRefreshEnabled: true"
                        , "    sessionCookieEncryptionKey: \"<read from /etc/salmon/netbird/cookie.key>\""
                        , "    dashboardRedirectURIs:"
                        , "      - \"https://netbird.example.com/nb-auth\""
                        , "      - \"https://netbird.example.com/nb-silent-auth\""
                        , "    cliRedirectURIs:"
                        , "      - \"http://localhost:53000/\""
                        , "  store:"
                        , "    engine: \"sqlite\""
                        , "    encryptionKey: \"<read from /etc/salmon/netbird/store.key>\""
                        ]
                    )
                    (publicConfig base)
            , testCase "the secrets go where their keys are, and nowhere else" $ do
                let fields = scalars (renderConfig base someSecrets)
                assertEqual "" (Just someSecrets.relayAuthSecret) (lookup "authSecret" fields)
                assertEqual "" (Just someSecrets.storeEncryptionKey) (lookup "encryptionKey" fields)
                assertEqual "" (Just someSecrets.sessionCookieKey) (lookup "sessionCookieEncryptionKey" fields)
            , testCase "the optional settings" $ do
                let cp = base{cpTrustedProxies = ["10.88.0.1/32", "fd00::/8"], cpDisableAnonymousMetrics = True, cpLogLevel = "debug", cpStunPort = 3479, cpPublicPort = 8443}
                    rendered = Text.lines (publicConfig cp)
                assertBool (show rendered) $
                    [ "  reverseProxy:"
                    , "    trustedHTTPProxies:"
                    , "      - \"10.88.0.1/32\""
                    , "      - \"fd00::/8\""
                    ]
                        `isSubsequenceOf` rendered
                mapM_
                    (\l -> assertBool (Text.unpack l) (l `elem` rendered))
                    [ "  disableAnonymousMetrics: true"
                    , "  logLevel: \"debug\""
                    , "    - 3479"
                    , "  exposedAddress: \"https://netbird.example.com:8443\""
                    , "    issuer: \"https://netbird.example.com:8443/oauth2\""
                    , "      - \"https://netbird.example.com:8443/nb-auth\""
                    ]
            , testCase "every key is one the combined server reads" $ do
                let cp = base{cpTrustedProxies = ["10.88.0.1/32"], cpDisableAnonymousMetrics = True}
                    unknown = [k | k <- keysOf (publicConfig cp), k `notElem` upstreamConfigKeys]
                assertEqual "" [] unknown
            , testCase "no key declares an identity provider connector" $
                assertBool "" (not (any (Text.isInfixOf "onnector") (keysOf (publicConfig base))))
            ]
        , testGroup
            "dashboard.env"
            [ testCase "the starting point" $
                assertEqual
                    ""
                    ( Text.unlines
                        [ marker
                        , "NETBIRD_MGMT_API_ENDPOINT=https://netbird.example.com"
                        , "NETBIRD_MGMT_GRPC_API_ENDPOINT=https://netbird.example.com"
                        , "AUTH_AUDIENCE=netbird-dashboard"
                        , "AUTH_CLIENT_ID=netbird-dashboard"
                        , "AUTH_CLIENT_SECRET="
                        , "AUTH_AUTHORITY=https://netbird.example.com/oauth2"
                        , "USE_AUTH0=false"
                        , "AUTH_SUPPORTED_SCOPES=openid profile email groups"
                        , "AUTH_REDIRECT_URI=/nb-auth"
                        , "AUTH_SILENT_REDIRECT_URI=/nb-silent-auth"
                        , "NGINX_SSL_PORT=443"
                        , "LETSENCRYPT_DOMAIN=none"
                        ]
                    )
                    (renderDashboardEnv base)
            , testCase "every variable is one the quickstart sets" $
                assertEqual "" [] [k | k <- envKeys (renderDashboardEnv base), k `notElem` upstreamDashboardKeys]
            ]
        , testGroup
            "quadlets"
            [ testCase "the server" $
                assertEqual
                    ""
                    ( Text.unlines
                        [ "[Unit]"
                        , "Description=NetBird combined server for netbird.example.com (salmon)"
                        , ""
                        , "[Container]"
                        , "ContainerName=netbird-server"
                        , "Image=docker.io/netbirdio/netbird-server:0.80.0"
                        , "Exec=--config /etc/netbird/config.yaml"
                        , "PublishPort=127.0.0.1:8081:80/tcp"
                        , "PublishPort=3478:3478/udp"
                        , "Volume=/etc/netbird/config.yaml:/etc/netbird/config.yaml:ro"
                        , "Volume=/var/lib/netbird:/var/lib/netbird:rw"
                        , ""
                        , "[Service]"
                        , "Restart=on-failure"
                        , ""
                        , "[Install]"
                        , "WantedBy=multi-user.target"
                        ]
                    )
                    (Quadlet.renderContainer (serverContainer base))
            , testCase "the dashboard" $
                assertEqual
                    ""
                    ( Text.unlines
                        [ "[Unit]"
                        , "Description=NetBird dashboard for netbird.example.com (salmon)"
                        , ""
                        , "[Container]"
                        , "ContainerName=netbird-dashboard"
                        , "Image=docker.io/netbirdio/dashboard:v2.94.0"
                        , "EnvironmentFile=/etc/netbird/dashboard.env"
                        , "PublishPort=127.0.0.1:8080:80/tcp"
                        , ""
                        , "[Service]"
                        , "Restart=on-failure"
                        , ""
                        , "[Install]"
                        , "WantedBy=multi-user.target"
                        ]
                    )
                    (Quadlet.renderContainer (dashboardContainer base))
            , testCase "neither is a declaration the quadlet node refuses" $ do
                assertEqual "" [] (Quadlet.containerProblems (serverContainer base))
                assertEqual "" [] (Quadlet.containerProblems (dashboardContainer base))
            , testCase "the server watches its configuration, the dashboard its environment" $ do
                assertEqual "" ["/etc/netbird/config.yaml"] (Quadlet.watchedFiles (serverContainer base))
                assertEqual "" ["/etc/netbird/dashboard.env"] (Quadlet.watchedFiles (dashboardContainer base))
            , testCase "the server's start is waited for on its published port" $
                assertEqual
                    ""
                    (Just (Quadlet.readyWhen (Quadlet.ProbeTcp "127.0.0.1" 8081) 120))
                    (serverContainer base{cpBindAddress = "0.0.0.0"}).containerReady
            , testCase "user scope starts at the user's boot target, from the user's directory" $ do
                let cp = base{cpScope = Systemd.User, cpUnitDir = "/home/op/.config/containers/systemd"}
                    rendered = Quadlet.renderContainer (serverContainer cp)
                assertBool (Text.unpack rendered) ("WantedBy=default.target" `Text.isInfixOf` rendered)
                assertEqual "" "/home/op/.config/containers/systemd/netbird-server.container" (Quadlet.quadletPath (serverContainer cp))
            ]
        , testGroup
            "reverse proxy routes"
            [ testCase "the quickstart's table, the catch-all last" $
                assertEqual
                    ""
                    [ Route ["/signalexchange.SignalExchange/", "/management.ManagementService/"] Grpc ToServer "127.0.0.1:8081"
                    , Route ["/relay", "/ws-proxy/"] WebSocket ToServer "127.0.0.1:8081"
                    , Route ["/api/", "/oauth2/"] PlainHttp ToServer "127.0.0.1:8081"
                    , Route ["/"] PlainHttp ToDashboard "127.0.0.1:8080"
                    ]
                    (proxyRoutes base)
            ]
        , testGroup
            "secrets"
            [ testCase "what the quickstart generates is accepted" $ do
                assertEqual "" Nothing (secretProblem RelayAuthSecret "cmVsYXktc2VjcmV0LXZhbHVl")
                assertEqual "" Nothing (secretProblem StoreEncryptionKey (Text.encodeUtf8 aKey))
                assertEqual "" Nothing (secretProblem SessionCookieKey (Text.encodeUtf8 aKey))
                assertEqual "raw 16 bytes" Nothing (secretProblem SessionCookieKey "0123456789abcdef")
                assertEqual "unpadded" Nothing (secretProblem SessionCookieKey "MDEyMzQ1Njc4OWFiY2RlZg")
            , testCase "what the server would refuse, or a YAML string would not hold, is refused" $ do
                assertBool "empty" (isJust (secretProblem RelayAuthSecret ""))
                assertBool "a quote" (isJust (secretProblem RelayAuthSecret "abc\"def"))
                assertBool "a backslash" (isJust (secretProblem RelayAuthSecret "abc\\def"))
                assertBool "a newline inside" (isJust (secretProblem RelayAuthSecret "abc\ndef"))
                assertBool "a store key of 16 bytes" (isJust (secretProblem StoreEncryptionKey "MDEyMzQ1Njc4OWFiY2RlZg=="))
                assertBool "a store key without its padding" (isJust (secretProblem StoreEncryptionKey (C8.takeWhile (/= '=') (Text.encodeUtf8 aKey))))
                assertBool "a cookie key of 5 bytes" (isJust (secretProblem SessionCookieKey "abcde"))
            , testCase "a written file's verdict" $ do
                assertEqual "" Success (interpretWritten "/p" (Present 0o600 True))
                assertBool "absent" (failed (interpretWritten "/p" NoFile))
                assertBool "unreadable" (failed (interpretWritten "/p" Unreadable))
                assertBool "different" (failed (interpretWritten "/p" (Present 0o600 False)))
                assertBool "readable by the group" (failed (interpretWritten "/p" (Present 0o640 True)))
            ]
        , testGroup
            "graph"
            [ testCase "two quadlets, two files, one directory, and what the quadlets bring" $
                assertEqual
                    ""
                    ( sort
                        [ "netbird-control-plane"
                        , "netbird-config-dir"
                        , "netbird-server-config"
                        , "netbird-dashboard-env"
                        , "podman-quadlet"
                        , "podman-quadlet"
                        , "file-contents"
                        , "file-contents"
                        , "podman-quadlet-image"
                        , "podman-quadlet-image"
                        , "podman-quadlet-dir"
                        , "podman-quadlet-key"
                        , "podman-quadlet-bind-dir"
                        ]
                    )
                    (sort [act.shorthand | act <- Map.elems (Dag.dagNodes (dagOf (node base)))])
            , testCase "no two nodes claim one effect site differently" $
                assertEqual "" 0 (length (Dag.dagConflicts (dagOf (node base))))
            , testCase "the configuration is written before the server's quadlet" $ do
                let dag = dagOf (node base)
                config <- one "config node" (refsOf dag "netbird-server-config")
                env <- one "env node" (refsOf dag "netbird-dashboard-env")
                let files = refsOf dag "file-contents"
                    standing r = [f | f <- files, r `elem` toList (Map.findWithDefault mempty f (Dag.dagDependencies dag))]
                assertEqual "one quadlet file stands on the configuration" 1 (length (standing config))
                assertEqual "one quadlet file stands on the environment" 1 (length (standing env))
                assertBool "they are not the same file" (standing config /= standing env)
            , testCase "what the caller's track declares comes before both files" $ do
                let delivery = op "the-delivery" nodeps (\ext -> ext{ref = mkRef "test-delivery" ("x" :: Text)})
                    dag = dagOf (controlPlane silent ignoreTrack (Track (const delivery)) base)
                d <- one "delivery node" (refsOf dag "the-delivery")
                config <- one "config node" (refsOf dag "netbird-server-config")
                env <- one "env node" (refsOf dag "netbird-dashboard-env")
                assertBool "config" (d `elem` toList (Map.findWithDefault mempty config (Dag.dagDependencies dag)))
                assertBool "env" (d `elem` toList (Map.findWithDefault mempty env (Dag.dagDependencies dag)))
            , testCase "the units are the containers' own" $ do
                let dag = dagOf (node base)
                assertEqual
                    ""
                    (sort [mkRef "systemd-unit" ("netbird-server.service" :: Text), mkRef "systemd-unit" ("netbird-dashboard.service" :: Text)])
                    (sort (refsOf dag "podman-quadlet"))
            , testCase "the public text of the whole graph names the secret files and nothing of what they hold" $ do
                let texts = publicTexts (node base)
                assertBool "the paths are public" (any (Text.isInfixOf "/etc/salmon/netbird/store.key") texts)
                assertBool "a placeholder is not a value" (not (any (Text.isInfixOf "encryptionKey: \"\"") texts))
            ]
        , testGroup
            "file nodes, in a scratch directory"
            [ testCase "the configuration: absent, written owner-only, satisfied, rotated, removed" $
                withScratch $ \dir -> do
                    cp <- scratchPlane dir someSecrets
                    let n = serverConfigNode ignoreTrack cp
                        path = serverConfigPath cp
                    before <- checkOf n
                    assertBool (show before) (failed before)
                    upOf (configDirNode cp)
                    upOf n
                    bytes <- ByteString.readFile path
                    assertEqual "" (Text.encodeUtf8 (renderConfig cp someSecrets)) bytes
                    st <- getFileStatus path
                    assertEqual "mode" 0o600 (fileMode st .&. 0o777)
                    assertEqual "" Success =<< checkOf n
                    -- a second up is the same file
                    upOf n
                    assertEqual "" bytes =<< ByteString.readFile path
                    -- a rotated secret is a difference, described by the path alone
                    ByteString.writeFile cp.cpRelayAuthSecret.secretPath "cm90YXRlZC1yZWxheS1zZWNyZXQ=\n"
                    rotated <- checkOf n
                    assertEqual "" (Failure (Text.pack path <> " is not what is declared")) rotated
                    upOf n
                    assertEqual "" Success =<< checkOf n
                    -- a file others can read is not satisfied
                    setFileMode path 0o644
                    loose <- checkOf n
                    assertBool (show loose) (failed loose)
                    upOf n
                    assertEqual "" Success =<< checkOf n
                    downOf n
                    assertEqual "" False =<< doesFileExist path
                    downOf n
            , testCase "down leaves a file this recipe did not write, and the directory" $
                withScratch $ \dir -> do
                    cp <- scratchPlane dir someSecrets
                    upOf (configDirNode cp)
                    ByteString.writeFile (serverConfigPath cp) "server:\n  listenAddress: \":443\"\n"
                    downOf (serverConfigNode ignoreTrack cp)
                    assertEqual "" True =<< doesFileExist (serverConfigPath cp)
                    downOf (configDirNode cp)
                    assertEqual "" True =<< doesDirectoryExist cp.cpConfigDir
            , testCase "a secret that cannot be used fails up and check by its path, without its bytes" $
                withScratch $ \dir -> do
                    let bad = "not a key: \"hunter2-hunter2\""
                    cp <- scratchPlane dir someSecrets{storeEncryptionKey = bad}
                    upOf (configDirNode cp)
                    let n = serverConfigNode ignoreTrack cp
                    result <- try (upOf n) :: IO (Either ControlPlaneError ())
                    case result of
                        Right () -> assertFailure "up wrote a configuration with an unusable key"
                        Left e -> do
                            assertEqual "" cp.cpStoreEncryptionKey.secretPath (case e of UnusableSecret p _ -> p; _ -> "")
                            assertBool (show e) (not ("hunter2" `Text.isInfixOf` Text.pack (show e)))
                    verdict <- checkOf n
                    assertBool (show verdict) (failed verdict)
                    assertBool (show verdict) (not ("hunter2" `Text.isInfixOf` Text.pack (show verdict)))
                    assertEqual "nothing was written" False =<< doesFileExist (serverConfigPath cp)
            , testCase "a missing secret file fails up and leaves the old configuration" $
                withScratch $ \dir -> do
                    cp <- scratchPlane dir someSecrets
                    let n = serverConfigNode ignoreTrack cp
                    upOf (configDirNode cp)
                    upOf n
                    old <- ByteString.readFile (serverConfigPath cp)
                    removeFile cp.cpSessionCookieKey.secretPath
                    result <- try (upOf n) :: IO (Either ControlPlaneError ())
                    assertEqual "" (Left (UnusableSecret cp.cpSessionCookieKey.secretPath "the session cookie key cannot be read")) result
                    assertEqual "" old =<< ByteString.readFile (serverConfigPath cp)
            , testCase "the dashboard's environment: written, satisfied, removed" $
                withScratch $ \dir -> do
                    cp <- scratchPlane dir someSecrets
                    let n = dashboardEnvNode ignoreTrack cp
                    upOf (configDirNode cp)
                    upOf n
                    assertEqual "" (Text.encodeUtf8 (renderDashboardEnv cp)) =<< ByteString.readFile (dashboardEnvPath cp)
                    assertEqual "" Success =<< checkOf n
                    downOf n
                    assertEqual "" False =<< doesFileExist (dashboardEnvPath cp)
            , testCase "a declaration with a problem writes nothing" $
                withScratch $ \dir -> do
                    cp <- scratchPlane dir someSecrets
                    let wrong = cp{cpServerImage = "docker.io/netbirdio/netbird-server:latest"}
                    result <- try (upOf (configDirNode wrong)) :: IO (Either ControlPlaneError ())
                    assertBool (show result) (case result of Left (InvalidControlPlane _) -> True; _ -> False)
                    assertEqual "" False =<< doesDirectoryExist wrong.cpConfigDir
            ]
        , testGroup
            "properties"
            [ testProperty "a generated declaration has no problem" $
                forAll genPlane (\cp -> problems cp === [])
            , testProperty "generated secrets are accepted" $
                forAll genS $ \(S s) ->
                    [secretProblem k (Text.encodeUtf8 v) | (k, v) <- kinds s] === [Nothing, Nothing, Nothing]
            , testProperty "a secret with a character a YAML string would not hold is refused, whatever its kind" $
                forAll ((,,) <$> genS <*> elements "\"\\ \n\t'\DEL\200" <*> arbitraryBoundedEnum) $ \(S s, c, kind) ->
                    isJust (secretProblem kind (C8.pack (Text.unpack s.relayAuthSecret <> [c] <> "x")))
            , testProperty "the reason a secret is refused holds nothing of it, nor a digest" $
                forAll ((,) <$> arbitraryBoundedEnum <*> (ByteString.pack <$> vectorOf 24 arbitrary)) $ \(kind, bytes) ->
                    case secretProblem kind bytes of
                        Nothing -> property True
                        Just reason -> property (not (any (`Text.isInfixOf` reason) (leaks bytes)))
            , testProperty "each secret is rendered once, at its own key, between quotes" $
                forAll ((,) <$> genPlane <*> genS) $ \(cp, S s) ->
                    let rendered = renderConfig cp s
                        fields = scalars rendered
                     in conjoin
                            [ lookup "authSecret" fields === Just s.relayAuthSecret
                            , lookup "encryptionKey" fields === Just s.storeEncryptionKey
                            , lookup "sessionCookieEncryptionKey" fields === Just s.sessionCookieKey
                            , property (all (\(_, v) -> Text.count ("\"" <> v <> "\"") rendered >= 1) (kinds s))
                            ]
            , testProperty "what the declaration decides reads back from the rendering" $
                forAll ((,) <$> genPlane <*> genS) $ \(cp, S s) ->
                    let fields = scalars (renderConfig cp s)
                     in conjoin
                            [ lookup "exposedAddress" fields === Just ("https://" <> cp.cpDomain <> ":" <> Text.pack (show cp.cpPublicPort))
                            , lookup "issuer" fields === Just (managementUrl cp <> "/oauth2")
                            , lookup "logLevel" fields === Just cp.cpLogLevel
                            , lookup "listenAddress" fields === Just ":80"
                            , property (("    - " <> Text.pack (show cp.cpStunPort)) `elem` Text.lines (renderConfig cp s))
                            ]
            , testProperty "the public rendering is the real one with each secret replaced by its file" $
                forAll ((,) <$> genPlane <*> genS) $ \(cp, S s) ->
                    let hide t = foldl (\acc (v, f) -> Text.replace ("\"" <> v <> "\"") ("\"<read from " <> Text.pack f.secretPath <> ">\"") acc) t (zip (map snd (kinds s)) (map snd (secretFiles cp)))
                     in distinct s ==> hide (renderConfig cp s) === publicConfig cp
            , testProperty "no public text of the graph holds a secret or a digest of one" $
                forAll ((,) <$> genPlane <*> genS) $ \(cp, S s) ->
                    let texts = publicTexts (node cp)
                     in conjoin [property (not (any (Text.isInfixOf l) texts)) | (_, v) <- kinds s, l <- leaks (Text.encodeUtf8 v)]
            , testProperty "every key rendered is one the combined server reads" $
                forAll genPlane $ \cp -> [k | k <- keysOf (publicConfig cp), k `notElem` upstreamConfigKeys] === []
            , testProperty "the options that default to nothing only add lines" $
                forAll genPlane $ \cp ->
                    let plain = cp{cpTrustedProxies = [], cpDisableAnonymousMetrics = False}
                     in property (Text.lines (publicConfig plain) `isSubsequenceOf` Text.lines (publicConfig cp))
            , testProperty "readiness is never rendered" $
                forAll ((,) <$> genPlane <*> genReady) $ \(cp, ready) ->
                    Quadlet.renderContainer (serverContainer cp{cpReadyTimeout = ready}) === Quadlet.renderContainer (serverContainer cp)
            , testProperty "the secret files, the settings and the STUN port are not where an effect is: the refs stay" $
                forAll ((,) <$> genPlane <*> genPlane) $ \(cp, other) ->
                    let moved =
                            cp
                                { cpLogLevel = other.cpLogLevel
                                , cpTrustedProxies = other.cpTrustedProxies
                                , cpDisableAnonymousMetrics = other.cpDisableAnonymousMetrics
                                , cpReadyTimeout = other.cpReadyTimeout
                                , cpStunPort = other.cpStunPort
                                , cpRelayAuthSecret = other.cpRelayAuthSecret
                                , cpStoreEncryptionKey = other.cpStoreEncryptionKey
                                , cpSessionCookieKey = other.cpSessionCookieKey
                                }
                     in sort (Map.keys (Dag.dagNodes (dagOf (node cp)))) === sort (Map.keys (Dag.dagNodes (dagOf (node moved))))
            , testProperty "a generated declaration folds without a conflict, into the same thirteen nodes" $
                forAll genPlane $ \cp ->
                    let dag = dagOf (node cp)
                     in (length (Dag.dagConflicts dag), Map.size (Dag.dagNodes dag)) === (0, 13)
            , testProperty "only STUN is published beyond the bind address" $
                forAll genPlane $ \cp ->
                    let published c = [Text.drop 12 l | l <- Text.lines (Quadlet.renderContainer c), "PublishPort=" `Text.isPrefixOf` l]
                        beyond = [p | p <- published (serverContainer cp) <> published (dashboardContainer cp), not ((cp.cpBindAddress <> ":") `Text.isPrefixOf` p)]
                     in beyond === [Text.pack (show cp.cpStunPort) <> ":" <> Text.pack (show cp.cpStunPort) <> "/udp"]
            , testProperty "an unpinned image is always refused" $
                forAll ((,) <$> genPlane <*> elements ["", ":latest"]) $ \(cp, suffix) ->
                    let image = Text.takeWhile (/= ':') cp.cpServerImage <> suffix
                     in property (not (null (problems cp{cpServerImage = image})))
            , testProperty "the configuration node settles: up, then check, for any declaration and secrets" $
                withNumTests 25 $
                    forAll ((,) <$> genPlane <*> genS) $ \(generated, S s) ->
                        ioProperty $
                            withScratch $ \dir -> do
                                cp <- scratchPlane dir s
                                let here = generated{cpConfigDir = cp.cpConfigDir, cpRelayAuthSecret = cp.cpRelayAuthSecret, cpStoreEncryptionKey = cp.cpStoreEncryptionKey, cpSessionCookieKey = cp.cpSessionCookieKey}
                                    n = serverConfigNode ignoreTrack here
                                upOf (configDirNode here)
                                before <- checkOf n
                                upOf n
                                after <- checkOf n
                                written <- ByteString.readFile (serverConfigPath here)
                                pure $
                                    conjoin
                                        [ property (failed before)
                                        , after === Success
                                        , written === Text.encodeUtf8 (renderConfig here s)
                                        , property (not (any (\l -> l `Text.isInfixOf` Text.pack (show before)) (concatMap (leaks . Text.encodeUtf8 . snd) (kinds s))))
                                        ]
            ]
        ]
  where
    refused cp = assertBool (show cp) (not (null (problems cp)))
    accepted cp = assertEqual "" [] (problems cp)
    node = controlPlane silent ignoreTrack ignoreTrack
    dagOf o = Dag.foldDag Dag.sameRepresentative (evalDeps o)
    refsOf :: Dag.Dag Extension -> Text -> [Ref]
    refsOf dag short = [r | (r, act) <- Map.toList (Dag.dagNodes dag), act.shorthand == short]
    publicTexts o = concat [act.extension.help : act.extension.notes | act <- Map.elems (Dag.dagNodes (dagOf o))]
    one :: String -> [a] -> IO a
    one _ [x] = pure x
    one what xs = assertFailure ("not exactly one " <> what <> ": " <> show (length xs))

failed :: CheckResult -> Bool
failed (Failure _) = True
failed _ = False

kinds :: Secrets -> [(SecretKind, Text)]
kinds s =
    [ (RelayAuthSecret, s.relayAuthSecret)
    , (StoreEncryptionKey, s.storeEncryptionKey)
    , (SessionCookieKey, s.sessionCookieKey)
    ]

distinct :: Secrets -> Bool
distinct s = length (nub (map snd (kinds s))) == 3

{- | The forms a secret could leak in: itself, and the plain digests this
repository uses elsewhere (the 12-character 'FS.hashBytes', and SHA-256 in
base64 and in hex).
-}
leaks :: ByteString.ByteString -> [Text]
leaks bytes =
    [ Text.decodeLatin1 bytes
    , FS.hashBytes bytes
    , Text.decodeLatin1 (Base64.encode (SHA256.hash bytes))
    , Text.pack (concatMap hex (ByteString.unpack (SHA256.hash bytes)))
    ]
  where
    hex w = [digits !! fromIntegral (w `div` 16), digits !! fromIntegral (w `mod` 16)]
    digits = "0123456789abcdef" :: String

-- | The @key: "value"@ lines of a rendered configuration, innermost key only.
scalars :: Text -> [(Text, Text)]
scalars rendered =
    [ (key, Text.dropEnd 1 (Text.drop 1 value))
    | l <- Text.lines rendered
    , let (key, rest) = Text.breakOn ": " (Text.strip l)
    , not (Text.null rest)
    , let value = Text.drop 2 rest
    , "\"" `Text.isPrefixOf` value
    , "\"" `Text.isSuffixOf` value
    ]

-- | Every mapping key of a rendered configuration.
keysOf :: Text -> [Text]
keysOf rendered =
    [ Text.takeWhile (/= ':') stripped
    | l <- Text.lines rendered
    , let stripped = Text.strip l
    , not ("#" `Text.isPrefixOf` stripped)
    , not ("- " `Text.isPrefixOf` stripped)
    , ":" `Text.isInfixOf` stripped
    ]

envKeys :: Text -> [Text]
envKeys rendered = [Text.takeWhile (/= '=') l | l <- Text.lines rendered, not ("#" `Text.isPrefixOf` l)]

{- | The YAML keys of @combined\/cmd\/config.go@ at @v0.80.0@ that this
recipe may render (@ServerConfig@, @AuthConfig@, @StoreConfig@,
@ReverseProxyConfig@), copied by hand from the struct tags.
-}
upstreamConfigKeys :: [Text]
upstreamConfigKeys =
    [ "server"
    , "listenAddress"
    , "exposedAddress"
    , "stunPorts"
    , "metricsPort"
    , "healthcheckAddress"
    , "logLevel"
    , "logFile"
    , "authSecret"
    , "dataDir"
    , "disableAnonymousMetrics"
    , "auth"
    , "issuer"
    , "signKeyRefreshEnabled"
    , "sessionCookieEncryptionKey"
    , "dashboardRedirectURIs"
    , "cliRedirectURIs"
    , "reverseProxy"
    , "trustedHTTPProxies"
    , "store"
    , "engine"
    , "encryptionKey"
    ]

-- | The variables of the quickstart's @render_dashboard_env@ at @v0.80.0@.
upstreamDashboardKeys :: [Text]
upstreamDashboardKeys =
    [ "NETBIRD_MGMT_API_ENDPOINT"
    , "NETBIRD_MGMT_GRPC_API_ENDPOINT"
    , "AUTH_AUDIENCE"
    , "AUTH_CLIENT_ID"
    , "AUTH_CLIENT_SECRET"
    , "AUTH_AUTHORITY"
    , "USE_AUTH0"
    , "AUTH_SUPPORTED_SCOPES"
    , "AUTH_REDIRECT_URI"
    , "AUTH_SILENT_REDIRECT_URI"
    , "NGINX_SSL_PORT"
    , "LETSENCRYPT_DOMAIN"
    ]

-------------------------------------------------------------------------------

genLabel :: Gen Text
genLabel = Text.pack <$> ((:) <$> elements ['a' .. 'z'] <*> resize 7 (listOf (elements (['a' .. 'z'] <> ['0' .. '9']))))

genPath :: Gen FilePath
genPath = do
    parts <- resize 3 (listOf1 genLabel)
    pure (concatMap (\p -> "/" <> Text.unpack p) parts)

genReady :: Gen (Maybe Int)
genReady = oneof [pure Nothing, Just <$> choose (1, 600)]

genPlane :: Gen ControlPlane
genPlane = do
    domain <- Text.intercalate "." <$> ((:) <$> genLabel <*> resize 3 (listOf1 genLabel))
    (serverPort, dashboardPort, stunPort) <- distinctPorts
    configDir <- genPath
    dataDir <- genPath
    unitDir <- genPath
    secretDir <- genPath
    tag <- genLabel
    proxies <- resize 3 (listOf (elements ["10.88.0.1/32", "172.30.0.10/32", "fd00::/8", "192.168.0.0/16"]))
    level <- elements ["panic", "fatal", "error", "warn", "info", "debug", "trace"]
    metrics <- arbitrary
    ready <- genReady
    bindAddress <- elements ["127.0.0.1", "0.0.0.0", "10.0.0.5"]
    publicPort <- elements [443, 8443]
    scope <- elements [Systemd.System, Systemd.User]
    pure
        ( controlPlaneFor
            domain
            (SecretFile (secretDir <> "/relay"))
            (SecretFile (secretDir <> "/store"))
            (SecretFile (secretDir <> "/cookie"))
        )
            { cpPublicPort = publicPort
            , cpScope = scope
            , cpUnitDir = unitDir
            , cpConfigDir = configDir
            , cpDataDir = dataDir
            , cpServerImage = "registry.example.com/netbirdio/netbird-server:" <> tag
            , cpBindAddress = bindAddress
            , cpServerPort = serverPort
            , cpDashboardPort = dashboardPort
            , cpStunPort = stunPort
            , cpTrustedProxies = proxies
            , cpLogLevel = level
            , cpDisableAnonymousMetrics = metrics
            , cpReadyTimeout = ready
            }
  where
    port = choose (1024, 65535 :: Int)
    distinctPorts = ((,,) <$> port <*> port <*> port) `suchThat` (\(a, b, c) -> length (nub [a, b, c]) == 3)

{- | 'Secrets' has no 'Show' in the library, so that none is ever printed;
a failing property here may print generated ones.
-}
newtype S = S Secrets

instance Show S where
    show (S s) = show (kinds s)

genS :: Gen S
genS = S <$> genSecrets

genSecrets :: Gen Secrets
genSecrets = do
    relay <- Text.pack <$> ((<>) <$> vectorOf 16 plainChar <*> resize 32 (listOf plainChar))
    store <- b64 32
    cookie <- oneof [b64 16, b64 24, b64 32, Text.pack <$> (elements [16, 24, 32] >>= \n -> vectorOf n plainChar)]
    pure Secrets{relayAuthSecret = relay, storeEncryptionKey = store, sessionCookieKey = cookie}
  where
    plainChar = elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "+/=_-.:@!#%")
    b64 n = Text.decodeLatin1 . Base64.encode . ByteString.pack <$> vectorOf n arbitrary

-------------------------------------------------------------------------------

upOf :: Op -> IO ()
upOf o = maybe (assertFailure "no action") (\act -> act.extension.up) (opAct o)

downOf :: Op -> IO ()
downOf o = maybe (assertFailure "no action") (\act -> act.extension.down) (opAct o)

checkOf :: Op -> IO CheckResult
checkOf o = maybe (assertFailure "no action") (\act -> act.extension.check) (opAct o)

withScratch :: (FilePath -> IO a) -> IO a
withScratch f = do
    tmp <- getTemporaryDirectory
    (path, h) <- openTempFile tmp "salmon-netbird-cp"
    hClose h
    removeFile path
    createDirectory path
    f path `finally` removeDirectoryRecursive path

{- | A declaration whose configuration directory (not yet made) and three
secret files (holding these values, with the newline a file usually ends
with) are under the scratch directory.
-}
scratchPlane :: FilePath -> Secrets -> IO ControlPlane
scratchPlane dir s = do
    let file name = dir </> name
    ByteString.writeFile (file "relay") (Text.encodeUtf8 s.relayAuthSecret <> "\n")
    ByteString.writeFile (file "store") (Text.encodeUtf8 s.storeEncryptionKey <> "\n")
    ByteString.writeFile (file "cookie") (Text.encodeUtf8 s.sessionCookieKey <> "\n")
    pure
        (controlPlaneFor "netbird.example.com" (SecretFile (file "relay")) (SecretFile (file "store")) (SecretFile (file "cookie")))
            { cpConfigDir = dir </> "etc" </> "netbird"
            }
