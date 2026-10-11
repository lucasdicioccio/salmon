{-# LANGUAGE OverloadedStrings #-}

{- | A self-hosted NetBird control plane over podman quadlets: what a
'Salmon.Builtin.Nodes.Netbird.peer' enrols against when the management
server is one's own ('managementUrl' is the URL it takes).

= What upstream's self-hosted layout is

Everything here was read in @netbirdio\/netbird@ at tag @v0.80.0@
(@infrastructure_files\/getting-started.sh@, @combined\/config.yaml.example@,
@combined\/cmd\/config.go@). At that tag the quickstart is __two__ NetBird
containers, not the management, signal, relay and coturn of older releases:

* @netbirdio\/netbird-server@, the /combined server/: management, signal,
  relay and a STUN server in one process, configured by one @config.yaml@.
  One HTTP port carries the API, both gRPC services, the relay's WebSocket
  and the identity provider; STUN is one UDP port. There is no TURN server
  and no coturn: the relay is what peers fall back to.
* @netbirdio\/dashboard@, the web UI, configured by environment variables.

The identity provider is the /embedded/ one (a Dex inside the server, with
local users), which the combined server always runs: there is no key that
switches it on, only @server.auth@ to say where it is reached. The first
user is made in the Dashboard's setup page, and so is any external identity
provider: __an external OIDC provider cannot be declared from these files__.
@server.auth@ has no connector key at this tag, the Dashboard stores
connectors in the provider's own database, and the only file route upstream
has is in the older four-container layout (an undocumented
@EmbeddedIdP.StaticConnectors@ in @management.json@, client secret inline).
So this recipe has no field for one.

= What this recipe is

'controlPlane' is two 'Quadlet.quadletContainer's and the two files they
read:

* @config.yaml@ ('serverConfigNode'), written owner-only at @up@ from the
  declaration and three secrets, and bind-mounted read-only;
* @dashboard.env@ ('dashboardEnvNode'), which holds no secret;
* a data directory bound at @\/var\/lib\/netbird@ (the stores and the
  identity provider's database), created if missing and never removed.

Both containers publish their HTTP port on 'cpBindAddress' (loopback by
default). __TLS and the routing between the two are the caller's__: a
reverse proxy in front, with the routes of 'proxyRoutes' (upstream's
"exposed ports" variant of the quickstart). STUN is published on every
address, since peers reach it directly.

= Secrets

The relay's shared secret, the store's encryption key and the session
cookie key exist only as values inside @config.yaml@: upstream has no file
or environment form for the first two. The recipe takes three paths
('SecretFile'), delivered by whatever the caller uses, reads them at @up@
and @check@ and at no other time, and writes them nowhere but that file.
Nothing of them reaches @help@, @notes@, an argv, a report or a failure
text, a digest included: the node's notes are 'publicConfig', the same
rendering with each secret replaced by the path it is read from, and a
refused secret is named by its path and by what is wrong with its /form/.
The quadlet watches the file through
"Salmon.Builtin.Nodes.Podman.Quadlet"'s keyed digest, so a rotated secret is
one restart.

A secret is refused unless it is printable ASCII with no space, quote or
backslash ('secretProblem'): what @openssl rand -base64 32@ gives, and what
goes between a YAML string's quotes unescaped.

= Not done, not run

Nothing here has been started: no image was pulled, no server has read a
rendered @config.yaml@, and no peer has enrolled against one. The reverse
proxy, a Postgres store, an initial owner, external STUN or relay servers,
and the reverse-proxy container of upstream's own Traefik variant are not
offered.
-}
module SreBox.NetbirdControlPlane (
    -- * Declaration
    ControlPlane (..),
    SecretFile (..),
    controlPlaneFor,
    serverImage,
    dashboardImage,
    problems,

    -- * Nodes
    controlPlane,
    serverContainer,
    dashboardContainer,
    serverConfigNode,
    dashboardEnvNode,
    configDirNode,

    -- * What clients and the reverse proxy need
    managementUrl,
    Route (..),
    Upstream (..),
    RouteKind (..),
    proxyRoutes,

    -- * Pure parts
    Secrets (..),
    SecretKind (..),
    secretFiles,
    secretProblem,
    publicConfig,
    renderConfig,
    renderDashboardEnv,
    serverConfigPath,
    dashboardEnvPath,
    marker,
    interpretWritten,
    Written (..),
    ControlPlaneError (..),
) where

import Control.Exception (Exception, IOException, displayException, onException, throwIO, try)
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Char8 as C8
import Data.Char (isAlphaNum, isAscii, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile, renameFile)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Posix.Types (FileMode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | A file on the target machine holding one secret, delivered by something else.
newtype SecretFile = SecretFile {secretPath :: FilePath}
    deriving (Eq, Ord, Show)

{- | One control plane on one machine. 'controlPlaneFor' is the starting
point; set the rest with record update.
-}
data ControlPlane = ControlPlane
    { cpDomain :: Text
    -- ^ the public host name of the whole thing (@netbird.example.com@): the
    -- dashboard, the API, the identity provider and the relay are all under
    -- it, behind the caller's reverse proxy
    , cpPublicPort :: Int
    -- ^ the HTTPS port of that reverse proxy; 443 unless it cannot be
    , cpScope :: Systemd.Scope
    , cpUnitDir :: FilePath
    -- ^ where the quadlet generator looks, as 'Quadlet.containerUnitDir'
    , cpConfigDir :: FilePath
    -- ^ holds @config.yaml@ and @dashboard.env@; created owner-only if
    -- missing, never removed
    , cpDataDir :: FilePath
    -- ^ bound at @\/var\/lib\/netbird@: the stores and the identity
    -- provider's database. Created if missing, never removed.
    , cpServerImage :: Text
    , cpDashboardImage :: Text
    , cpBindAddress :: Text
    -- ^ the host address both HTTP ports are published on; the reverse
    -- proxy reaches them there
    , cpServerPort :: Int
    -- ^ host port of the combined server (API, gRPC, relay, identity provider)
    , cpDashboardPort :: Int
    -- ^ host port of the dashboard
    , cpStunPort :: Int
    -- ^ UDP, published on every address and announced to peers under 'cpDomain'
    , cpRelayAuthSecret :: SecretFile
    -- ^ @server.authSecret@: any value; peers get credentials derived from it
    , cpStoreEncryptionKey :: SecretFile
    -- ^ @server.store.encryptionKey@: 32 bytes in padded standard base64.
    -- __Changing it makes the existing store unreadable.__
    , cpSessionCookieKey :: SecretFile
    -- ^ @server.auth.sessionCookieEncryptionKey@: 16, 24 or 32 bytes, as
    -- they are or in base64
    , cpTrustedProxies :: [Text]
    -- ^ @server.reverseProxy.trustedHTTPProxies@: the CIDRs the reverse
    -- proxy's requests come from /as the container sees them/, whose
    -- forwarded-for headers are then believed. Empty renders nothing.
    , cpLogLevel :: Text
    , cpDisableAnonymousMetrics :: Bool
    -- ^ @server.disableAnonymousMetrics@; 'False' renders nothing (upstream's default)
    , cpReadyTimeout :: Maybe Int
    -- ^ seconds the server has to accept a connection on its published port
    -- after a (re)start before @up@ fails ('Quadlet.containerReady').
    -- Never rendered. The default was chosen without watching a real start.
    }
    deriving (Eq, Show)

-- | The combined server, at the release its configuration keys were read in.
serverImage :: Text
serverImage = "docker.io/netbirdio/netbird-server:0.80.0"

-- | The dashboard, at its latest release when 'serverImage' was pinned.
dashboardImage :: Text
dashboardImage = "docker.io/netbirdio/dashboard:v2.94.0"

{- | A system-scope control plane for this domain and these three secrets,
with upstream's quickstart ports (dashboard 8080, server 8081, both on
loopback; STUN 3478) and the pinned images.
-}
controlPlaneFor :: Text -> SecretFile -> SecretFile -> SecretFile -> ControlPlane
controlPlaneFor domain relay store cookie =
    ControlPlane
        { cpDomain = domain
        , cpPublicPort = 443
        , cpScope = Systemd.System
        , cpUnitDir = Quadlet.systemQuadletDir
        , cpConfigDir = "/etc/netbird"
        , cpDataDir = "/var/lib/netbird"
        , cpServerImage = serverImage
        , cpDashboardImage = dashboardImage
        , cpBindAddress = "127.0.0.1"
        , cpServerPort = 8081
        , cpDashboardPort = 8080
        , cpStunPort = 3478
        , cpRelayAuthSecret = relay
        , cpStoreEncryptionKey = store
        , cpSessionCookieKey = cookie
        , cpTrustedProxies = []
        , cpLogLevel = "info"
        , cpDisableAnonymousMetrics = False
        , cpReadyTimeout = Just 120
        }

{- | Why this cannot be declared, if it cannot. Every value that is written
into @config.yaml@, @dashboard.env@ or a quadlet is held to characters that
need no quoting there, so the renderers do none.
-}
problems :: ControlPlane -> [Text]
problems cp =
    mconcat
        [ ["the domain is empty" | Text.null cp.cpDomain]
        , [ "the domain is not a host name: " <> cp.cpDomain
          | not (Text.null cp.cpDomain)
          , not (Text.all hostChar cp.cpDomain) || any Text.null (Text.splitOn "." cp.cpDomain) || "-" `Text.isPrefixOf` cp.cpDomain
          ]
        , [ "the " <> what <> " port is not a port: " <> Text.pack (show p)
          | (what, p) <- ports
          , p < 1 || p > 65535
          ]
        , ["the server and the dashboard are published on the same port" | cp.cpServerPort == cp.cpDashboardPort]
        , [ "the bind address is not an IP address: " <> cp.cpBindAddress
          | Text.null cp.cpBindAddress || not (Text.all (\c -> isHexDigit c || c == '.') cp.cpBindAddress)
          ]
        , [ "the " <> what <> " is not an absolute path: " <> Text.pack p
          | (what, p) <- paths
          , not (isAbsolute p)
          ]
        , [ "the " <> what <> " has a character a quadlet or a YAML string would misread: " <> Text.pack p
          | (what, p) <- paths
          , not (all pathChar p)
          ]
        , [ "the " <> what <> " image is not pinned to a tag or a digest: " <> image
          | (what, image) <- [("server" :: Text, cp.cpServerImage), ("dashboard", cp.cpDashboardImage)]
          , not (pinned image)
          ]
        , [ "a trusted proxy is not a CIDR: " <> c
          | c <- cp.cpTrustedProxies
          , Text.null c || not (Text.all (\ch -> isHexDigit ch || ch `elem` (".:/" :: String)) c) || not ("/" `Text.isInfixOf` c)
          ]
        , [ "the log level is not one the server knows: " <> cp.cpLogLevel
          | cp.cpLogLevel `notElem` ["panic", "fatal", "error", "warn", "info", "debug", "trace"]
          ]
        , ["the readiness timeout is not positive" | Just t <- [cp.cpReadyTimeout], t <= 0]
        , [ "two secrets are read from the same file: " <> Text.pack a.secretPath
          | (i, (_, a)) <- indexed
          , (j, (_, b)) <- indexed
          , i < j
          , a == b
          ]
        ]
  where
    indexed = zip [0 :: Int ..] (secretFiles cp)
    ports =
        [ ("public" :: Text, cp.cpPublicPort)
        , ("server", cp.cpServerPort)
        , ("dashboard", cp.cpDashboardPort)
        , ("STUN", cp.cpStunPort)
        ]
    paths =
        [ ("quadlet directory" :: Text, cp.cpUnitDir)
        , ("configuration directory", cp.cpConfigDir)
        , ("data directory", cp.cpDataDir)
        ]
            <> [(secretKindName k <> " file", f.secretPath) | (k, f) <- secretFiles cp]
    hostChar c = isAscii c && (isAlphaNum c || c == '-' || c == '.')
    pathChar c = isAscii c && (isAlphaNum c || c `elem` ("/._-+@" :: String))
    pinned image =
        let name = Text.takeWhileEnd (/= '/') image
            tag = Text.takeWhileEnd (/= ':') name
         in not (Text.null (Text.strip image))
                && Text.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("/._-:@" :: String))) image
                && ("@sha256:" `Text.isInfixOf` name || (":" `Text.isInfixOf` name && not (Text.null tag) && tag /= "latest"))

-------------------------------------------------------------------------------

-- | The three secrets of a control plane, by what the server uses them for.
data SecretKind
    = RelayAuthSecret
    | StoreEncryptionKey
    | SessionCookieKey
    deriving (Eq, Ord, Show, Enum, Bounded)

secretKindName :: SecretKind -> Text
secretKindName RelayAuthSecret = "relay secret"
secretKindName StoreEncryptionKey = "store encryption key"
secretKindName SessionCookieKey = "session cookie key"

-- | Which file each secret is read from.
secretFiles :: ControlPlane -> [(SecretKind, SecretFile)]
secretFiles cp =
    [ (RelayAuthSecret, cp.cpRelayAuthSecret)
    , (StoreEncryptionKey, cp.cpStoreEncryptionKey)
    , (SessionCookieKey, cp.cpSessionCookieKey)
    ]

{- | What goes between the quotes of the three secret values. In the node
this is what the files held, surrounding whitespace removed; in
'publicConfig' it is a description of where each comes from.
-}
data Secrets = Secrets
    { relayAuthSecret :: Text
    , storeEncryptionKey :: Text
    , sessionCookieKey :: Text
    }
    deriving (Eq)

{- | What is wrong with the /form/ of a secret, in words that say nothing of
its contents (no byte of it, no length, no digest).

Every kind: not empty, and printable ASCII with no space, quote or
backslash, so that it is a YAML double-quoted string as it stands. The store
key is what @util\/crypt.NewFieldEncrypt@ takes (padded standard base64 of
32 bytes); the cookie key what @resolveSessionCookieEncryptionKey@ takes
(16, 24 or 32 bytes, raw or in any of the four base64 forms).
-}
secretProblem :: SecretKind -> ByteString.ByteString -> Maybe Text
secretProblem kind bytes
    | ByteString.null bytes = Just "is empty"
    | not (C8.all plain bytes) = Just "has a character that is not printable ASCII, or a space, a quote or a backslash"
    | otherwise = case kind of
        RelayAuthSecret -> Nothing
        StoreEncryptionKey
            | (ByteString.length <$> Base64.decode bytes) == Right 32 -> Nothing
            | otherwise -> Just "is not 32 bytes in padded standard base64"
        SessionCookieKey
            | any keyLength (ByteString.length bytes : decodedLengths) -> Nothing
            | otherwise -> Just "is not 16, 24 or 32 bytes, raw or in base64"
  where
    plain c = c > ' ' && c < '\DEL' && c /= '"' && c /= '\\' && c /= '\''
    keyLength n = n `elem` [16, 24, 32 :: Int]
    -- padded first, so that the unpadded forms are read by the strict decoders too
    decodedLengths = [ByteString.length d | Right d <- [Base64.decode padded, Base64URL.decode padded]]
    padded = bytes <> C8.replicate ((4 - ByteString.length bytes `mod` 4) `mod` 4) '='

-------------------------------------------------------------------------------

-- | First line of both files: how @down@ knows a file is this recipe's to remove.
marker :: Text
marker = "# written by salmon (SreBox.NetbirdControlPlane); edits are overwritten"

serverConfigPath :: ControlPlane -> FilePath
serverConfigPath cp = cp.cpConfigDir </> "config.yaml"

dashboardEnvPath :: ControlPlane -> FilePath
dashboardEnvPath cp = cp.cpConfigDir </> "dashboard.env"

-- | Where the server's HTTP port is inside its container (@server.listenAddress@).
serverGuestPort :: Int
serverGuestPort = 80

-- | @https:\/\/DOMAIN@, with the port when it is not 443.
publicBase :: ControlPlane -> Text
publicBase cp
    | cp.cpPublicPort == 443 = "https://" <> cp.cpDomain
    | otherwise = "https://" <> cp.cpDomain <> ":" <> showT cp.cpPublicPort

{- | The management URL a peer is given
('Salmon.Builtin.Nodes.Netbird.peer''s, the client's @--management-url@).
-}
managementUrl :: ControlPlane -> Text
managementUrl = publicBase

{- | The combined server's @config.yaml@. The keys are those of
@combined\/cmd\/config.go@ at @v0.80.0@, laid out as the quickstart's
@render_combined_yaml@ does, with @https@ throughout since the reverse proxy
terminates TLS.
-}
renderConfig :: ControlPlane -> Secrets -> Text
renderConfig cp s =
    Text.unlines $
        mconcat
            [ [marker]
            , ["server:"]
            , ["  listenAddress: " <> quoted (":" <> showT serverGuestPort)]
            , ["  exposedAddress: " <> quoted ("https://" <> cp.cpDomain <> ":" <> showT cp.cpPublicPort)]
            , ["  stunPorts:", "    - " <> showT cp.cpStunPort]
            , ["  metricsPort: 9090"]
            , ["  healthcheckAddress: " <> quoted ":9000"]
            , ["  logLevel: " <> quoted cp.cpLogLevel]
            , ["  logFile: " <> quoted "console"]
            , ["  authSecret: " <> quoted s.relayAuthSecret]
            , ["  dataDir: " <> quoted "/var/lib/netbird"]
            , ["  disableAnonymousMetrics: true" | cp.cpDisableAnonymousMetrics]
            , ["  auth:"]
            , ["    issuer: " <> quoted (publicBase cp <> "/oauth2")]
            , ["    signKeyRefreshEnabled: true"]
            , ["    sessionCookieEncryptionKey: " <> quoted s.sessionCookieKey]
            , ["    dashboardRedirectURIs:"]
            , ["      - " <> quoted (publicBase cp <> "/nb-auth")]
            , ["      - " <> quoted (publicBase cp <> "/nb-silent-auth")]
            , ["    cliRedirectURIs:"]
            , ["      - " <> quoted "http://localhost:53000/"]
            , concat
                [ ["  reverseProxy:", "    trustedHTTPProxies:"] <> ["      - " <> quoted c | c <- cp.cpTrustedProxies]
                | not (null cp.cpTrustedProxies)
                ]
            , ["  store:"]
            , ["    engine: " <> quoted "sqlite"]
            , ["    encryptionKey: " <> quoted s.storeEncryptionKey]
            ]
  where
    quoted t = "\"" <> t <> "\""

{- | 'renderConfig' with each secret replaced by where it is read from: all
the declaration decides, and what the node shows in its @notes@.
-}
publicConfig :: ControlPlane -> Text
publicConfig cp =
    renderConfig
        cp
        Secrets
            { relayAuthSecret = from cp.cpRelayAuthSecret
            , storeEncryptionKey = from cp.cpStoreEncryptionKey
            , sessionCookieKey = from cp.cpSessionCookieKey
            }
  where
    from :: SecretFile -> Text
    from f = "<read from " <> Text.pack f.secretPath <> ">"

{- | The dashboard's environment: the quickstart's @render_dashboard_env@.
The dashboard is a public OIDC client of the embedded provider
(@netbird-dashboard@, no client secret), so nothing here is secret.
-}
renderDashboardEnv :: ControlPlane -> Text
renderDashboardEnv cp =
    Text.unlines
        [ marker
        , "NETBIRD_MGMT_API_ENDPOINT=" <> publicBase cp
        , "NETBIRD_MGMT_GRPC_API_ENDPOINT=" <> publicBase cp
        , "AUTH_AUDIENCE=netbird-dashboard"
        , "AUTH_CLIENT_ID=netbird-dashboard"
        , "AUTH_CLIENT_SECRET="
        , "AUTH_AUTHORITY=" <> publicBase cp <> "/oauth2"
        , "USE_AUTH0=false"
        , "AUTH_SUPPORTED_SCOPES=openid profile email groups"
        , "AUTH_REDIRECT_URI=/nb-auth"
        , "AUTH_SILENT_REDIRECT_URI=/nb-silent-auth"
        , "NGINX_SSL_PORT=443"
        , "LETSENCRYPT_DOMAIN=none"
        ]

-------------------------------------------------------------------------------

-- | Which of the two published ports a route goes to.
data Upstream = ToServer | ToDashboard
    deriving (Eq, Ord, Show)

-- | How the reverse proxy has to speak to the upstream.
data RouteKind
    = -- | gRPC over cleartext HTTP\/2 (h2c), long-lived
      Grpc
    | -- | HTTP with a WebSocket upgrade, long-lived
      WebSocket
    | PlainHttp
    deriving (Eq, Ord, Show)

-- | Requests whose path starts with one of these prefixes go to this upstream.
data Route = Route
    { routePrefixes :: [Text]
    , routeKind :: RouteKind
    , routeUpstream :: Upstream
    , routeHostPort :: Text
    -- ^ @ADDRESS:PORT@ of the upstream on this machine
    }
    deriving (Eq, Ord, Show)

{- | What the caller's reverse proxy has to route, in order (the last is the
catch-all): the quickstart's @print_manual_instructions@. Upstream asks for
timeouts of a day on the first two.
-}
proxyRoutes :: ControlPlane -> [Route]
proxyRoutes cp =
    [ Route ["/signalexchange.SignalExchange/", "/management.ManagementService/"] Grpc ToServer server
    , Route ["/relay", "/ws-proxy/"] WebSocket ToServer server
    , Route ["/api/", "/oauth2/"] PlainHttp ToServer server
    , Route ["/"] PlainHttp ToDashboard dashboard
    ]
  where
    server = cp.cpBindAddress <> ":" <> showT cp.cpServerPort
    dashboard = cp.cpBindAddress <> ":" <> showT cp.cpDashboardPort

-------------------------------------------------------------------------------

-- | The combined server's quadlet.
serverContainer :: ControlPlane -> Quadlet.Container
serverContainer cp =
    (Quadlet.container (Podman.ContainerName "netbird-server") cp.cpServerImage)
        { Quadlet.containerScope = cp.cpScope
        , Quadlet.containerUnitDir = cp.cpUnitDir
        , Quadlet.containerDescription = "NetBird combined server for " <> cp.cpDomain <> " (salmon)"
        , Quadlet.containerExec = ["--config", "/etc/netbird/config.yaml"]
        , Quadlet.containerPorts =
            [ Podman.PortMapping (cp.cpBindAddress <> ":" <> showT cp.cpServerPort) (showT serverGuestPort) Podman.TCPPort
            , Podman.PortMapping (showT cp.cpStunPort) (showT cp.cpStunPort) Podman.UDPPort
            ]
        , Quadlet.containerVolumes = [Podman.VolumeMount (serverConfigPath cp) "/etc/netbird/config.yaml" Podman.ReadOnly]
        , Quadlet.containerBinds =
            [ (Quadlet.bind cp.cpDataDir "/var/lib/netbird")
                { Quadlet.bindCreate = Just Quadlet.hostDir{Quadlet.hostDirMode = Just "0700"}
                }
            ]
        , Quadlet.containerWatched = [serverConfigPath cp]
        , Quadlet.containerWantedBy = Just (bootTarget cp)
        , Quadlet.containerReady =
            fmap (Quadlet.readyWhen (Quadlet.ProbeTcp probeHost cp.cpServerPort)) cp.cpReadyTimeout
        }
  where
    probeHost
        | cp.cpBindAddress == "0.0.0.0" = "127.0.0.1"
        | otherwise = cp.cpBindAddress

-- | The dashboard's quadlet.
dashboardContainer :: ControlPlane -> Quadlet.Container
dashboardContainer cp =
    (Quadlet.container (Podman.ContainerName "netbird-dashboard") cp.cpDashboardImage)
        { Quadlet.containerScope = cp.cpScope
        , Quadlet.containerUnitDir = cp.cpUnitDir
        , Quadlet.containerDescription = "NetBird dashboard for " <> cp.cpDomain <> " (salmon)"
        , Quadlet.containerEnvFile = Just (dashboardEnvPath cp)
        , Quadlet.containerPorts =
            [Podman.PortMapping (cp.cpBindAddress <> ":" <> showT cp.cpDashboardPort) "80" Podman.TCPPort]
        , Quadlet.containerWantedBy = Just (bootTarget cp)
        }

bootTarget :: ControlPlane -> Systemd.UnitTarget
bootTarget cp = case cp.cpScope of
    Systemd.System -> "multi-user.target"
    Systemd.User -> "default.target"

-------------------------------------------------------------------------------

-- | Why a node of this recipe refused to act. Public text only.
data ControlPlaneError
    = -- | the declaration's 'problems'
      InvalidControlPlane ![Text]
    | -- | a secret's file, and what is wrong with reading it or with its form
      UnusableSecret !FilePath !Text
    | -- | a path that should be a directory holds something else
      NotADirectory !FilePath
    deriving (Eq, Show)

instance Exception ControlPlaneError

-- | What is at a path this recipe writes.
data Written
    = NoFile
    | Unreadable
    | -- | its mode, and whether its bytes are the wanted ones
      Present !FileMode !Bool
    deriving (Eq, Show)

{- | The verdict on a written file. A file that differs is not described any
further: the difference may be a secret.
-}
interpretWritten :: FilePath -> Written -> CheckResult
interpretWritten path w = case w of
    NoFile -> Failure ("no file at " <> Text.pack path)
    Unreadable -> Failure (Text.pack path <> " cannot be read")
    Present mode same
        | not same -> Failure (Text.pack path <> " is not what is declared")
        | mode .&. 0o077 /= 0 -> Failure (Text.pack path <> " is readable by others than its owner")
        | otherwise -> Success

-- | The secrets as the files hold them now, or the first that cannot be used.
readSecrets :: ControlPlane -> IO (Either ControlPlaneError Secrets)
readSecrets cp = do
    relay <- readSecret RelayAuthSecret cp.cpRelayAuthSecret
    store <- readSecret StoreEncryptionKey cp.cpStoreEncryptionKey
    cookie <- readSecret SessionCookieKey cp.cpSessionCookieKey
    pure (Secrets <$> relay <*> store <*> cookie)

readSecret :: SecretKind -> SecretFile -> IO (Either ControlPlaneError Text)
readSecret kind f = do
    read_ <- try (ByteString.readFile f.secretPath) :: IO (Either IOException ByteString.ByteString)
    pure $ case read_ of
        Left _ -> Left (UnusableSecret f.secretPath ("the " <> secretKindName kind <> " cannot be read"))
        Right raw ->
            let bytes = C8.strip raw
             in case secretProblem kind bytes of
                    Just problem -> Left (UnusableSecret f.secretPath ("the " <> secretKindName kind <> " " <> problem))
                    -- ASCII, by 'secretProblem'
                    Nothing -> Right (Text.decodeLatin1 bytes)

inspectWritten :: FilePath -> ByteString.ByteString -> IO Written
inspectWritten path wanted = do
    present <- doesFileExist path
    if not present
        then pure NoFile
        else do
            read_ <- try (ByteString.readFile path) :: IO (Either IOException ByteString.ByteString)
            case read_ of
                Left _ -> pure Unreadable
                Right bytes -> do
                    st <- getFileStatus path
                    pure (Present (fileMode st) (bytes == wanted))

{- | Puts these bytes at the path, owner-only, through a temporary file in
the same directory and a rename: the server never reads half a file, and the
bytes are never in a file others can open.
-}
writeOwnerOnly :: FilePath -> ByteString.ByteString -> IO ()
writeOwnerOnly path bytes = do
    -- openBinaryTempFile creates the file 0600
    (tmp, h) <- openBinaryTempFile (takeDirectory path) ".salmon-netbird.tmp"
    ( do
            ByteString.hPut h bytes
            hClose h
            setFileMode tmp 0o600
            renameFile tmp path
        )
        `onException` (hClose h >> removeFile tmp)

-- | Removes the file if its first line is 'marker'; anything else is not ours.
removeIfOurs :: FilePath -> IO ()
removeIfOurs path = do
    present <- doesFileExist path
    when present $ do
        read_ <- try (ByteString.readFile path) :: IO (Either IOException ByteString.ByteString)
        case read_ of
            Right bytes | Text.encodeUtf8 marker `ByteString.isPrefixOf` bytes -> removeFile path
            _ -> pure ()

refuseProblems :: ControlPlane -> IO ()
refuseProblems cp = do
    let ps = problems cp
    unless (null ps) $ throwIO (InvalidControlPlane ps)

{- | 'cpConfigDir': created owner-only if missing, left as it is if there,
never removed (two files stand in it, and it may be the operator's).
-}
configDirNode :: ControlPlane -> Op
configDirNode cp =
    op "netbird-config-dir" nodeps $ \actions ->
        actions
            { help = "creates " <> Text.pack path <> " unless it is there"
            , notes = ["created owner-only; an existing directory is left as it is; down leaves it"]
            , ref = mkRef "netbird-config-dir" path
            , check = do
                isDir <- doesDirectoryExist path
                pure (if isDir then Success else Failure ("no directory at " <> Text.pack path))
            , up = do
                refuseProblems cp
                isDir <- doesDirectoryExist path
                unless isDir $ do
                    isFile <- doesFileExist path
                    when isFile $ throwIO (NotADirectory path)
                    createDirectoryIfMissing True path
                    setFileMode path 0o700
            }
  where
    path = cp.cpConfigDir

{- | The combined server's @config.yaml@, secrets included, owner-only.

The 'Track'' is where the caller says what delivers the three secret files.
@check@ reads them and compares the file byte for byte, in this process; a
secret that cannot be read or used is a 'Failure' naming its path, and @up@
then throws the same. @down@ removes the file if it starts with 'marker'.
-}
serverConfigNode :: Track' ControlPlane -> ControlPlane -> Op
serverConfigNode t cp =
    op "netbird-server-config" (deps [configDirNode cp, run t cp]) $ \actions ->
        actions
            { help = "writes " <> Text.pack path <> ", the combined server's configuration, owner-only"
            , notes = filter (/= marker) (Text.lines (publicConfig cp))
            , ref = mkRef "netbird-server-config" path
            , check = do
                secrets <- readSecrets cp
                case secrets of
                    Left e -> pure (Failure (describe e))
                    Right s -> interpretWritten path <$> inspectWritten path (Text.encodeUtf8 (renderConfig cp s))
            , up = do
                refuseProblems cp
                secrets <- readSecrets cp
                case secrets of
                    Left e -> throwIO e
                    Right s -> writeOwnerOnly path (Text.encodeUtf8 (renderConfig cp s))
            , down = removeIfOurs path
            }
  where
    path = serverConfigPath cp
    describe e = Text.pack (displayException e)

-- | The dashboard's environment file. It holds no secret and is written owner-only all the same.
dashboardEnvNode :: Track' ControlPlane -> ControlPlane -> Op
dashboardEnvNode t cp =
    op "netbird-dashboard-env" (deps [configDirNode cp, run t cp]) $ \actions ->
        actions
            { help = "writes " <> Text.pack path <> ", the dashboard's environment"
            , notes = filter (/= marker) (Text.lines (renderDashboardEnv cp))
            , ref = mkRef "netbird-dashboard-env" path
            , check = interpretWritten path <$> inspectWritten path wanted
            , up = refuseProblems cp >> writeOwnerOnly path wanted
            , down = removeIfOurs path
            }
  where
    path = dashboardEnvPath cp
    wanted = Text.encodeUtf8 (renderDashboardEnv cp)

{- | The control plane: both containers running as declared.

The 'Track'' is what the machine needs first and this recipe does not
choose: podman, and whatever delivers the three secret files. It is applied
ahead of the configuration file and of both quadlets.
-}
controlPlane ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' ControlPlane ->
    ControlPlane ->
    Op
controlPlane r systemctl t cp =
    op "netbird-control-plane" (deps [server, dashboard]) $ \actions ->
        actions
            { help = "runs a NetBird control plane for " <> cp.cpDomain
            , notes =
                [ "management URL for peers: " <> managementUrl cp
                , "server: " <> cp.cpServerImage
                , "dashboard: " <> cp.cpDashboardImage
                , "identity provider: the server's embedded one; users and external providers are added in the dashboard"
                , "TLS and routing are the reverse proxy's, in front of " <> Text.intercalate ", " upstreams
                , "STUN: udp " <> showT cp.cpStunPort
                ]
            , ref = mkRef "netbird-control-plane" cp.cpDomain
            , up = refuseProblems cp
            }
  where
    upstreams = dedup [rt.routeHostPort | rt <- proxyRoutes cp]
    dedup = foldr (\x seen -> if x `elem` seen then seen else x : seen) []
    server =
        Quadlet.quadletContainer r systemctl (Track (const (serverConfigNode t cp))) (serverContainer cp)
    dashboard =
        Quadlet.quadletContainer
            r
            systemctl
            (Track (const (dashboardEnvNode t cp)))
            (dashboardContainer cp)

showT :: Int -> Text
showT = Text.pack . show
