{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for milestone 8 of @specs\/generic-server.md@: the
HTTP surface over TCP, which is only ever TLS with a bearer token
('Salmon.Actions.Serve.Http.withHttpServerOn' with a 'Http.BindTls').

The fixture is a key and a self-signed certificate made in a temp dir by
the tree's own 'Salmon.Builtin.Nodes.Certificates' nodes — the spec's
"the @Certificates@ nodes can mint the cert; this is what they are for" —
which run as any user since they only write under the directory they are
given; @openssl@ has to be on the machine, and the group skips loudly
without it. The server binds @127.0.0.1:0@ and the test reads the port it
got. The claims: over TLS with the token, @\/status@ answers; without the
token, or with a wrong one, @401@ and nothing else; a plain-HTTP client on
the same port is refused before any route is reached; @\/events@ streams
to a client with the token; and the unix socket served by the same
'Http.Server' answers with no token at all. Plus the pure half: the option
validation the binary exits on, and the token file's own refusals.
-}
module Test.ServeTlsSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.X509.CertificateStore as X509
import GHC.Generics (Generic)
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (makeConnection)
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import qualified Network.TLS as TLS
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.Posix.Files (setFileMode)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), World)
import qualified Salmon.Actions.Serve.Events as Events
import qualified Salmon.Actions.Serve.Http as Http
import qualified Salmon.Builtin.CommandLine as CLI
import qualified Salmon.Client.Http as Client
import Salmon.Client.Model (Event (..))
import Salmon.Builtin.Extension (Track', deps, down, help, ignoreTrack, nodeps, op, ref, up)
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (contramap, silent)
import qualified Salmon.Reporter.Tagged as Tagged

import Test.Harness (capture, requireExecutable, runUp, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Serve.Http over TCP (TLS + token)"
        [ testGroup
            "option validation (pure)"
            [ testCase "nothing given is no listener" $
                assertEqual "none" (Right Nothing) (CLI.validateTcpOptions CLI.noTcp)
            , testCase "--http-tcp alone names all three missing flags" $
                case CLI.validateTcpOptions CLI.noTcp{CLI.tcpBind = Just "127.0.0.1:8443"} of
                    Left err -> forM_ ["--tls-cert", "--tls-key", "--token-file"] $ \flag ->
                        assertBool (Text.unpack err <> " names " <> Text.unpack flag) (flag `Text.isInfixOf` err)
                    Right r -> assertFailure ("accepted: " <> show r)
            , testCase "--http-tcp with a certificate names the two still missing" $
                case CLI.validateTcpOptions CLI.noTcp{CLI.tcpBind = Just "127.0.0.1:8443", CLI.tcpCert = Just "/c.pem"} of
                    Left err -> do
                        assertBool "not the one given" (not ("--tls-cert" `Text.isInfixOf` err))
                        assertBool "the key" ("--tls-key" `Text.isInfixOf` err)
                        assertBool "the token" ("--token-file" `Text.isInfixOf` err)
                    Right r -> assertFailure ("accepted: " <> show r)
            , testCase "all four is a listener" $
                assertEqual
                    "parsed"
                    (Right (Just (CLI.TcpListen "127.0.0.1" 8443 "/c.pem" "/k.pem" "/t" Http.defaultSessionPolicy)))
                    (CLI.validateTcpOptions (allFour "127.0.0.1:8443"))
            , testCase "an IPv6 address in brackets" $
                assertEqual
                    "parsed"
                    (Right (Just (CLI.TcpListen "::1" 8443 "/c.pem" "/k.pem" "/t" Http.defaultSessionPolicy)))
                    (CLI.validateTcpOptions (allFour "[::1]:8443"))
            , testCase "a port alone is refused: the host is spelled, 0.0.0.0 included" $
                assertBool "refused" (isLeft (CLI.validateTcpOptions (allFour ":8443")))
            , testCase "a bad port is refused" $ do
                assertBool "not a number" (isLeft (CLI.validateTcpOptions (allFour "127.0.0.1:https")))
                assertBool "too big" (isLeft (CLI.validateTcpOptions (allFour "127.0.0.1:70000")))
                assertBool "no colon" (isLeft (CLI.validateTcpOptions (allFour "127.0.0.1")))
            , testCase "the three files without --http-tcp do nothing on their own, and say so" $
                case CLI.validateTcpOptions CLI.noTcp{CLI.tcpTokenFile = Just "/t"} of
                    Left err -> assertBool (Text.unpack err) ("--http-tcp" `Text.isInfixOf` err)
                    Right r -> assertFailure ("accepted: " <> show r)
            , testCase "--session-lifetime/--session-idle: defaults, 0 is off, negative refused, nothing without --http-tcp" $ do
                let policyOf o = fmap (fmap (.tcpSessionPolicy)) (CLI.validateTcpOptions o)
                    base = allFour "127.0.0.1:8443"
                assertEqual "defaults" (Right (Just Http.defaultSessionPolicy)) (policyOf base)
                assertEqual "given" (Right (Just (Http.SessionPolicy (Just 60) (Just 5)))) (policyOf base{CLI.tcpSessionLifetime = Just 60, CLI.tcpSessionIdle = Just 5})
                assertEqual "0 is off" (Right (Just (Http.SessionPolicy Nothing (Just 3600)))) (policyOf base{CLI.tcpSessionLifetime = Just 0})
                case policyOf base{CLI.tcpSessionIdle = Just (-1)} of
                    Left err -> assertBool (Text.unpack err) ("--session-idle" `Text.isInfixOf` err)
                    Right r -> assertFailure ("a negative limit was accepted: " <> show r)
                case CLI.validateTcpOptions CLI.noTcp{CLI.tcpSessionLifetime = Just 60} of
                    Left err -> assertBool (Text.unpack err) ("--session-lifetime" `Text.isInfixOf` err && "--http-tcp" `Text.isInfixOf` err)
                    Right r -> assertFailure ("accepted: " <> show r)
            ]
        , testGroup
            "sessions (a clock the test moves)"
            [ testCase "the lifetime ends a session however busy; the idle limit ends one nobody uses" sessionLimits
            , testCase "an open stream holds the idle limit off but not the lifetime, which ends the stream's wait" streamPresence
            , testCase "signing in sweeps every ended session, so the store holds what is live" signInSweeps
            ]
        , testGroup
            "the token file"
            [ testCase "trimmed, and refused when readable by others or empty" $
                withTempDir $ \dir -> do
                    let path = dir </> "token"
                    ByteString.writeFile path "  s3cret\n"
                    setFileMode path 0o644
                    assertEqual "world-readable" (Left (Http.TokenFileReadable path)) =<< Http.readTokenFile path
                    setFileMode path 0o600
                    assertEqual "trimmed" (Right "s3cret") =<< Http.readTokenFile path
                    ByteString.writeFile path "\n \n"
                    assertEqual "empty" (Left (Http.TokenFileEmpty path)) =<< Http.readTokenFile path
            , testCase "sameSecret is equality" $ do
                assertBool "equal" (Http.sameSecret "abc" "abc")
                assertBool "differ" (not (Http.sameSecret "abc" "abd"))
                assertBool "prefix" (not (Http.sameSecret "abc" "abcd"))
                assertBool "empty vs not" (not (Http.sameSecret "" "a"))
                assertBool "both empty" (Http.sameSecret "" "")
            ]
        , testGroup
            "over the wire"
            [ testCase "with the token over TLS, /status answers; without or wrong, 401; plain TCP is refused; /events streams; the unix socket needs none" $
                requireExecutable "openssl" overTheWire
            , testCase "a browser: / redirects to /auth, the token posted there is a session cookie, and the cookie is as good as the token" $
                requireExecutable "openssl" signingIn
            , testCase "Salmon.Client.Http over TLS: pinned certificate and token read, command and stream; wrong token, unpinned certificate and plain http are refused" $
                requireExecutable "openssl" typedClient
            , testCase "signing out: /auth/session says whether, logout expires the cookie, revokes the session and cuts the stream it opened" $
                requireExecutable "openssl" signingOut
            , testCase "a session's lifetime: the cookie's Max-Age, the open stream cut on time, then 401 and /auth?ended" $
                requireExecutable "openssl" sessionExpiry
            ]
        ]
  where
    allFour hostPort = CLI.TcpOptions (Just hostPort) (Just "/c.pem") (Just "/k.pem") (Just "/t") Nothing Nothing
    isLeft (Left _) = True
    isLeft _ = False

-------------------------------------------------------------------------------
-- the thing served: counters per node name

newtype Spec = Spec {specNames :: [String]}
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: [String] -> Either Text Spec
parseSpec [] = Left "expected at least one node name"
parseSpec args = Right (Spec args)

spyProgram :: IORef (Map String Int) -> Track' Spec
spyProgram upsRef = Track $ \spec ->
    op "tls-root" (deps (fmap nodeOp spec.specNames)) $ \actions ->
        actions{ref = mkRef "tls-root" spec.specNames, help = "the root of " <> Text.pack (unwords spec.specNames)}
  where
    nodeOp name =
        op "tls-node" nodeps $ \actions ->
            actions
                { ref = mkRef "tls-node" name
                , help = "node " <> Text.pack name
                , up = atomicModifyIORef' upsRef (\m -> (Map.insertWith (+) name 1 m, ()))
                , down = pure ()
                }

-------------------------------------------------------------------------------
-- the fixture: a key and a self-signed certificate, by the tree's own nodes

-- | The name the certificate is issued for, and the one the client asks for.
serverName :: Text
serverName = "localhost"

data Material = Material
    { materialCert :: FilePath
    , materialKey :: FilePath
    }

{- | Mint them under the directory through 'Certs.certificateAuthority' —
a self-signed certificate naming 'serverName', made the way a recipe would.
Not 'Certs.selfSign': @openssl x509 -req -signkey@ (and @-CA@, so
'Certs.caSign' too) writes an X.509 __v1__ certificate, which the
@crypton-x509-validation@ a Haskell client verifies with rejects outright
(@LeafNotV3@) however OpenSSL-based clients take it; @req -x509@ writes v3.
-}
mintMaterial :: FilePath -> IO Material
mintMaterial dir = do
    let key = Certs.Key Certs.RSA2048 dir "server.key"
        ca = Certs.CertificateAuthority key (dir </> "server.pem") (Certs.Domain serverName) 30
    ok <- runUp (Certs.certificateAuthority silent ignoreTrack ca)
    unless ok (assertFailure "the certificate nodes did not come up")
    pure (Material ca.caCertPath (Certs.keyPath key))

-------------------------------------------------------------------------------
-- a running loop with one server on two listeners

token :: ByteString.ByteString
token = "correct-horse-battery-staple"

data Running = Running
    { runningStdin :: Handle
    , runningCert :: FilePath
    , runningWorld :: MVar (World Spec Spec)
    , runningUnixPath :: FilePath
    , runningPort :: Int
    , runningTls :: HTTP.Manager
    -- ^ trusts exactly the minted certificate
    , runningUnix :: HTTP.Manager
    }

withRunning :: (Running -> IO a) -> IO a
withRunning = withRunningWith Http.defaultSessionPolicy

withRunningWith :: Http.SessionPolicy -> (Running -> IO a) -> IO a
withRunningWith policy act =
    withTempDir $ \dir -> do
        material <- mintMaterial (dir </> "tls")
        let unixPath = dir </> "serve.http"
        (stdinR, stdinW) <- createPipe
        worldVar <- newEmptyMVar
        (own, _) <- capture
        upsRef <- newIORef Map.empty
        let binds =
                [ Http.BindUnix unixPath
                , Http.BindTls (Http.TlsBind "127.0.0.1" 0 material.materialCert material.materialKey token policy)
                ]
        Http.withHttpServerOn Events.defaultConfig binds "usage: config NAME...\n" (pure Serve.Interactive) $ \server -> do
            let base = (contramap attributed (Tagged.serveStream own), contramap attributed (Tagged.updownStream own))
                (serveR, updownR) = Http.serverReporters server base
            _ <- forkIO $ do
                w <-
                    Serve.serveObserved
                        (Http.serverObserver server)
                        []
                        Nothing
                        True
                        serveR
                        updownR
                        parseSpec
                        (Configure pure)
                        (spyProgram upsRef)
                        Nothing
                        [Serve.stdinProducer stdinR, Http.serverProducer server]
                putMVar worldVar w
            bound <- readTVarIO (Http.serverBoundTcp server)
            port <- case bound of
                [Socket.SockAddrInet p _] -> pure (fromIntegral p)
                other -> assertFailure ("not one IPv4 address bound: " <> show other)
            tlsManager <- trustingManager material.materialCert
            unixManager <- unixSocketManager unixPath
            r <- act (Running stdinW material.materialCert worldVar unixPath port tlsManager unixManager)
            _ <- try (hClose stdinW) :: IO (Either IOError ())
            ended <- timeout (10 * 1000000) (takeMVar worldVar)
            case ended of
                Nothing -> assertFailure "the loop did not end"
                Just _ -> pure r

{- | An @http-client@ manager whose TLS trusts only the minted certificate,
verifying the chain and the name as a real client would (so a server
answering with any other certificate fails the handshake); the request
names 'serverName', which resolves to the loopback the server bound.
-}
trustingManager :: FilePath -> IO HTTP.Manager
trustingManager certFile = do
    mstore <- X509.readCertificateStore certFile
    store <- maybe (assertFailure ("no certificate read from " <> certFile)) pure mstore
    let base = TLS.defaultParamsClient (Text.unpack serverName) ""
        params = base{TLS.clientShared = base.clientShared{TLS.sharedCAStore = store}}
    HTTP.newManager (HTTPS.mkManagerSettings (Connection.TLSSettings params) Nothing)

unixSocketManager :: FilePath -> IO HTTP.Manager
unixSocketManager path =
    HTTP.newManager
        HTTP.defaultManagerSettings
            { HTTP.managerRawConnection = pure $ \_ _ _ -> do
                sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
                Socket.connect sock (Socket.SockAddrUnix path)
                makeConnection (SocketBS.recv sock 4096) (SocketBS.sendAll sock) (Socket.close sock)
            }

-- | A request over TLS to the bound port, with the given @Authorization@ if any.
tlsRequest :: Running -> Maybe ByteString.ByteString -> String -> IO HTTP.Request
tlsRequest running auth route = do
    req <- HTTP.parseRequest ("https://" <> Text.unpack serverName <> ":" <> show (runningPort running) <> route)
    pure req{HTTP.requestHeaders = [(HTTP.hAuthorization, a) | Just a <- [auth]]}

bearer :: ByteString.ByteString -> Maybe ByteString.ByteString
bearer t = Just ("Bearer " <> t)

exchange :: HTTP.Manager -> HTTP.Request -> IO (Int, Value)
exchange manager req = do
    r <- timeout (10 * 1000000) (HTTP.httpLbs req manager)
    case r of
        Nothing -> assertFailure ("no answer within 10s to " <> show (HTTP.path req))
        Just resp ->
            case eitherDecode (HTTP.responseBody resp) of
                Left err -> assertFailure ("not JSON: " <> err <> ": " <> LChar8.unpack (HTTP.responseBody resp))
                Right v -> pure (HTTP.statusCode (HTTP.responseStatus resp), v)

textAt :: [Text] -> Value -> Maybe Text
textAt [] (String t) = Just t
textAt (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= textAt ks
textAt _ _ = Nothing

field :: Text -> Value -> Maybe Value
field k (Object o) = KeyMap.lookup (Key.fromText k) o
field _ _ = Nothing

-------------------------------------------------------------------------------

overTheWire :: IO ()
overTheWire =
    withRunning $ \running -> do
        -- a command over TLS with the token: handled, and answered with its reports
        post <- tlsRequest running (bearer token) "/command"
        (pcode, pv) <- exchange (runningTls running) post{HTTP.method = "POST", HTTP.requestHeaders = HTTP.requestHeaders post ++ [(HTTP.hContentType, "text/plain")], HTTP.requestBody = HTTP.RequestBodyLBS "supervise off"}
        assertEqual "sync command over TLS" 200 pcode
        assertBool ("an array of reports: " <> show pv) (case pv of Array _ -> True; _ -> False)
        up <- tlsRequest running (bearer token) "/command"
        (ucode, _) <- exchange (runningTls running) up{HTTP.method = "POST", HTTP.requestHeaders = HTTP.requestHeaders up ++ [(HTTP.hContentType, "text/plain")], HTTP.requestBody = HTTP.RequestBodyLBS "up n1 n2"}
        assertEqual "up over TLS" 200 ucode

        -- a read with the token
        (code, v) <- exchange (runningTls running) =<< tlsRequest running (bearer token) "/status"
        assertEqual "/status with the token" 200 code
        assertEqual "kind" (Just "status") (textAt ["kind"] v)
        assertEqual "the declared nodes are there" 3 (length (maybe [] arrayOf (field "nodes" v)))

        -- without the token, and with a wrong one: 401 on every route
        forM_ ["/status", "/dag", "/history", "/help/seed", "/events", "/nope"] $ \route -> do
            (c0, v0) <- exchange (runningTls running) =<< tlsRequest running Nothing route
            assertEqual ("no token on " <> route) 401 c0
            assertBool ("an error object on " <> route) (field "error" v0 /= Nothing)
            (c1, _) <- exchange (runningTls running) =<< tlsRequest running (bearer "wrong") route
            assertEqual ("wrong token on " <> route) 401 c1
            (c2, _) <- exchange (runningTls running) =<< tlsRequest running (Just ("Basic " <> token)) route
            assertEqual ("not a bearer on " <> route) 401 c2
            (c3, _) <- exchange (runningTls running) =<< tlsRequest running (bearer (token <> "x")) route
            assertEqual ("a longer token on " <> route) 401 c3
        -- and a refused command is not queued: the world is unchanged
        deny <- tlsRequest running Nothing "/command"
        (dcode, _) <- exchange (runningTls running) deny{HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS "up n3"}
        assertEqual "command without the token" 401 dcode
        (_, after) <- exchange (runningTls running) =<< tlsRequest running (bearer token) "/status"
        assertEqual "n3 was never declared" 3 (length (maybe [] arrayOf (field "nodes" after)))

        -- plain TCP on the same port: refused before any route is reached
        plain <- plainRequest (runningPort running) "GET /status HTTP/1.1\r\nHost: localhost\r\nAuthorization: Bearer correct-horse-battery-staple\r\n\r\n"
        assertBool ("plain HTTP is refused, not answered: " <> show plain) (plainRefused plain)

        -- /events with the token streams: the first frame carries an event
        ev <- tlsRequest running (bearer token) "/events?since=0"
        HTTP.withResponse ev (runningTls running) $ \resp -> do
            assertEqual "/events with the token" 200 (HTTP.statusCode (HTTP.responseStatus resp))
            assertEqual "content type" (Just "text/event-stream") (lookup HTTP.hContentType (HTTP.responseHeaders resp))
            frame <- readUntilData (HTTP.responseBody resp) ByteString.empty
            assertBool ("an event replayed: " <> Char8.unpack frame) ("data: {" `ByteString.isInfixOf` frame)

        -- the unix socket served by the same server: no token, same world
        ureq <- HTTP.parseRequest "http://salmon/status"
        (ucode', uv) <- exchange (runningUnix running) ureq
        assertEqual "/status on the unix socket without a token" 200 ucode'
        assertEqual "the same nodes" 3 (length (maybe [] arrayOf (field "nodes" uv)))
        -- and one sequence of numbers across the two listeners
        (_, tv) <- exchange (runningTls running) =<< tlsRequest running (bearer token) "/status"
        assertEqual "one counter" (field "seq" uv) (field "seq" tv)
  where
    arrayOf (Array xs) = toList xs
    arrayOf _ = []

{- | The browser's path to the UI over TCP, with redirects not followed so
each hop is visible: @/@ sends a client with no credential to @/auth@, a
wrong token posted there is refused and mints nothing, the right one is a
@303@ back to @/@ with a cookie that then stands in for the bearer header on
every route — the page, the reads, a command, the event stream — while a
cookie the listener never minted is refused like no credential at all. On
the unix socket, @/auth@ has nothing to do and sends the browser to @/@.
-}
signingIn :: IO ()
signingIn =
    withRunning $ \running -> do
        let tls = runningTls running
        (rcode, rheaders, _) <- raw tls =<< tlsRequest running Nothing "/"
        assertEqual "/ without a credential" 303 rcode
        assertEqual "redirected to the form" (Just "/auth") (lookup HTTP.hLocation rheaders)

        (fcode, fheaders, fbody) <- raw tls =<< tlsRequest running Nothing "/auth"
        assertEqual "the form" 200 fcode
        assertEqual "as HTML" (Just "text/html; charset=utf-8") (lookup HTTP.hContentType fheaders)
        assertBool "a token field" ("name=\"token\"" `ByteString.isInfixOf` LChar8.toStrict fbody)

        (wcode, wheaders, wbody) <- raw tls =<< login running "not-it"
        assertEqual "a wrong token" 401 wcode
        assertEqual "mints nothing" Nothing (lookup "Set-Cookie" wheaders)
        assertBool "and says so" ("not the token" `ByteString.isInfixOf` LChar8.toStrict wbody)

        (lcode, lheaders, _) <- raw tls =<< login running token
        assertEqual "the right token" 303 lcode
        assertEqual "back to the page" (Just "/") (lookup HTTP.hLocation lheaders)
        setCookie <- maybe (assertFailure "no cookie") pure (lookup "Set-Cookie" lheaders)
        forM_ ["HttpOnly", "Secure", "SameSite=Strict", "Path=/"] $ \attr ->
            assertBool ("cookie is " <> Char8.unpack attr <> ": " <> Char8.unpack setCookie) (attr `ByteString.isInfixOf` setCookie)
        let cookie = Char8.takeWhile (/= ';') setCookie
        assertBool ("the cookie is not the token: " <> Char8.unpack cookie) (not (token `ByteString.isInfixOf` cookie))

        let withCookie c route = do
                req <- tlsRequest running Nothing route
                pure req{HTTP.requestHeaders = [(HTTP.hCookie, c)]}
        (pcode, _, pbody) <- raw tls =<< withCookie cookie "/"
        assertEqual "the page with the cookie" 200 pcode
        assertBool "is the UI" ("ui/ui.js" `ByteString.isInfixOf` LChar8.toStrict pbody)
        (scode, _) <- exchange tls =<< withCookie ("other=1; " <> cookie) "/status"
        assertEqual "a read with the cookie among others" 200 scode
        post <- withCookie cookie "/command"
        (ccode, _) <- exchange tls post{HTTP.method = "POST", HTTP.requestHeaders = HTTP.requestHeaders post ++ [(HTTP.hContentType, "text/plain")], HTTP.requestBody = HTTP.RequestBodyLBS "supervise off"}
        assertEqual "a command with the cookie" 200 ccode
        ev <- withCookie cookie "/events?since=0"
        HTTP.withResponse ev tls $ \resp ->
            assertEqual "/events with the cookie" 200 (HTTP.statusCode (HTTP.responseStatus resp))

        forM_ ["/status", "/command"] $ \route -> do
            (bcode, _, _) <- raw tls =<< withCookie "__Host-salmon-session=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" route
            assertEqual ("a cookie never minted on " <> route) 401 bcode

        ureq <- HTTP.parseRequest "http://salmon/auth"
        (ucode, uheaders, _) <- raw (runningUnix running) ureq
        assertEqual "/auth on the unix socket" 303 ucode
        assertEqual "sends the browser to the page" (Just "/") (lookup HTTP.hLocation uheaders)
  where
    login running t = do
        req <- tlsRequest running Nothing "/auth"
        pure (HTTP.urlEncodedBody [("token", t)] req)

{- | What @salmon-tui https://...@ is made of: 'Client.newTlsClient'
pinning the minted certificate, with the token. It reads @/dag@, queues a
command, and follows @/events@ from the snapshot to the end of that command's pass
— every request carrying the header. With a wrong token every call is
'Client.Refused' 401; against the system's store the self-signed
certificate fails the handshake before any token is sent; and an
@http://@ address is refused before anything is sent at all.
-}
typedClient :: IO ()
typedClient =
    withRunning $ \running -> do
        let url = "https://" <> Text.unpack serverName <> ":" <> show (runningPort running)
        client <- Client.newTlsClient (Client.TlsTarget url token (Just (runningCert running)))
        snapshot <- Client.dag client
        since <- case field "seq" snapshot of
            Just (Number n) -> pure (Just (round n))
            other -> assertFailure ("no seq on /dag: " <> show other)
        queued <- Client.commandAsync client "up c1"
        seen <- newIORef []
        done <-
            timeout (10 * 1000000) $
                Client.events client since Client.noFilter $ \ev -> do
                    atomicModifyIORef' seen (\es -> (ev : es, ()))
                    pure (ev.eventOrigin /= Just queued.enqueuedOrigin || ev.eventKind /= "converge-stop")
        assertBool "the stream reached the end of the command's pass" (done == Just ())
        evs <- readIORef seen
        assertBool "the command's reports came over the stream" (any (\ev -> ev.eventOrigin == Just queued.enqueuedOrigin) evs)

        wrong <- Client.newTlsClient (Client.TlsTarget url "wrong" (Just (runningCert running)))
        refused <- try (Client.status wrong)
        case refused of
            Left (Client.Refused code _) -> assertEqual "a wrong token" 401 code
            other -> assertFailure ("a wrong token was not refused: " <> show (fmap (const ()) other))

        unpinned <- Client.newTlsClient (Client.TlsTarget url token Nothing)
        handshake <- try (Client.status unpinned)
        assertBool "a self-signed certificate is not trusted from the system store" (isLeftSome handshake)

        plain <- try (Client.newTlsClient (Client.TlsTarget ("http://127.0.0.1:" <> show (runningPort running)) token Nothing))
        case plain of
            Left (Client.BadTarget _) -> pure ()
            _ -> assertFailure "an http:// address was accepted"
  where
    isLeftSome :: Either SomeException a -> Bool
    isLeftSome = either (const True) (const False)

{- | The other half of 'signingIn'. @/auth/session@ is how the page knows to
offer the button: @true@ with a live cookie, @false@ without one (and on
the unix socket, where nobody signs in). @POST /auth/logout@ answers
@303@ to @/auth@ with the cookie expired, and from then on the cookie is
nothing: a read is @401@, @/@ is a redirect to the form. An @/events@
stream opened with the session before it ended does not stay open until
its next request — there is none — but ends there and then. A logout with
no cookie gets the same answer and revokes nothing, and a @GET@ of it is
refused, so a link or a prefetch cannot sign anybody out.
-}
signingOut :: IO ()
signingOut =
    withRunning $ \running -> do
        let tls = runningTls running
            withCookie c route = do
                req <- tlsRequest running Nothing route
                pure req{HTTP.requestHeaders = [(HTTP.hCookie, c) | not (ByteString.null c)]}
            sessionOf c = do
                (code, v) <- exchange tls =<< withCookie c "/auth/session"
                assertEqual "/auth/session answers" 200 code
                pure (field "session" v)
        loginReq <- tlsRequest running Nothing "/auth"
        (_, lheaders, _) <- raw tls (HTTP.urlEncodedBody [("token", token)] loginReq)
        cookie <- maybe (assertFailure "no cookie") (pure . Char8.takeWhile (/= ';')) (lookup "Set-Cookie" lheaders)
        other <- maybe (assertFailure "no cookie") (pure . Char8.takeWhile (/= ';')) . lookup "Set-Cookie" . (\(_, h, _) -> h) =<< raw tls (HTTP.urlEncodedBody [("token", token)] loginReq)

        assertEqual "signed in" (Just (Bool True)) =<< sessionOf cookie
        assertEqual "no cookie, no session" (Just (Bool False)) =<< sessionOf ""

        (gcode, _, _) <- raw tls =<< withCookie cookie "/auth/logout"
        assertEqual "a GET cannot sign out" 405 gcode
        assertEqual "and did not" (Just (Bool True)) =<< sessionOf cookie

        (acode, aheaders, _) <- raw tls . (\r -> r{HTTP.method = "POST"}) =<< tlsRequest running Nothing "/auth/logout"
        assertEqual "a logout without a cookie" 303 acode
        assertEqual "still expires one" True (maybe False ("Max-Age=0" `ByteString.isInfixOf`) (lookup "Set-Cookie" aheaders))
        assertEqual "and revokes nothing" (Just (Bool True)) =<< sessionOf cookie

        ev <- withCookie cookie "/events?since=0"
        HTTP.withResponse ev tls $ \resp -> do
            assertEqual "/events with the cookie" 200 (HTTP.statusCode (HTTP.responseStatus resp))
            _ <- readUntilData (HTTP.responseBody resp) ByteString.empty

            logout <- withCookie cookie "/auth/logout"
            (ocode, oheaders, _) <- raw tls logout{HTTP.method = "POST"}
            assertEqual "signed out" 303 ocode
            assertEqual "to the form" (Just "/auth") (lookup HTTP.hLocation oheaders)
            expired <- maybe (assertFailure "no Set-Cookie on logout") pure (lookup "Set-Cookie" oheaders)
            assertBool ("the cookie is expired: " <> Char8.unpack expired) ("__Host-salmon-session=;" `ByteString.isPrefixOf` expired && "Max-Age=0" `ByteString.isInfixOf` expired)

            ended <- timeout (5 * 1000000) (drain (HTTP.responseBody resp))
            assertEqual "the stream the session opened ends" (Just ()) ended

        assertEqual "the session is gone" (Just (Bool False)) =<< sessionOf cookie
        (scode, _, _) <- raw tls =<< withCookie cookie "/status"
        assertEqual "a read with the old cookie" 401 scode
        (rcode, rheaders, _) <- raw tls =<< withCookie cookie "/"
        assertEqual "the page with the old cookie" 303 rcode
        assertEqual "sends the browser to sign in, saying the session ended" (Just "/auth?ended") (lookup HTTP.hLocation rheaders)
        assertEqual "another browser's session is untouched" (Just (Bool True)) =<< sessionOf other

        ureq <- HTTP.parseRequest "http://salmon/auth/session"
        (ucode, uv) <- exchange (runningUnix running) ureq
        assertEqual "/auth/session on the unix socket" 200 ucode
        assertEqual "nobody signs in there" (Just (Bool False)) (field "session" uv)
  where
    drain body = do
        chunk <- HTTP.brRead body
        unless (ByteString.null chunk) (drain body)

-------------------------------------------------------------------------------
-- sessions, against a clock the test moves

-- | A clock at 0 that moves only when told, and whose waits wake when it does.
fakeClock :: IO (Http.SessionClock, Double -> IO ())
fakeClock = do
    t <- newTVarIO 0
    let clock = Http.SessionClock (readTVarIO t) (\d -> atomically (readTVar t >>= check . (>= d)))
    pure (clock, \dt -> atomically (modifyTVar' t (+ dt)))

sessionLimits :: IO ()
sessionLimits = do
    (clock, advance) <- fakeClock
    sessions <- Http.newSessionsWith (Http.SessionPolicy (Just 100) (Just 10)) clock
    busy <- Http.newSession sessions
    quiet <- Http.newSession sessions
    -- used every 5s, `busy` never idles; `quiet` is never used again
    forM_ [1 .. 3 :: Int] $ \_ -> advance 5 >> (assertBool "busy is used" =<< Http.knownSession sessions busy)
    assertBool "quiet idled out after 15s" . not =<< Http.knownSession sessions quiet
    forM_ [1 .. 16 :: Int] $ \_ -> advance 5 >> void (Http.knownSession sessions busy)
    -- 95s in: still inside its lifetime; 100s: past it, busy or not
    assertBool "busy at 95s" =<< Http.knownSession sessions busy
    advance 5
    assertBool "busy at its lifetime" . not =<< Http.knownSession sessions busy
    assertEqual "both dropped when they were looked at" 0 =<< Http.sessionCount sessions

streamPresence :: IO ()
streamPresence = do
    (clock, advance) <- fakeClock
    sessions <- Http.newSessionsWith (Http.SessionPolicy (Just 100) (Just 10)) clock
    cookie <- Http.newSession sessions
    over <- newEmptyMVar
    opened <- newEmptyMVar
    closing <- newEmptyMVar
    _ <- forkIO $ Http.withStream sessions cookie $ do
        putMVar opened ()
        Http.sessionOver sessions cookie
        putMVar over ()
        takeMVar closing
    takeMVar opened
    advance 50
    assertBool "50s of watching is not idle" =<< Http.knownSession sessions cookie
    advance 49
    r <- timeout 100000 (takeMVar over)
    assertEqual "the stream's wait is still on at 99s" Nothing r
    advance 1
    r' <- timeout (5 * 1000000) (takeMVar over)
    assertEqual "the lifetime ends the stream's wait" (Just ()) r'
    putMVar closing ()
    assertBool "and the session with it" . not =<< Http.knownSession sessions cookie
    -- the idle clock restarts when a stream closes, not before
    other <- Http.newSession sessions
    done <- newEmptyMVar
    _ <- forkIO $ Http.withStream sessions other (advance 30) >> putMVar done ()
    takeMVar done
    advance 9
    assertBool "9s after its stream closed" =<< Http.knownSession sessions other
    advance 10
    assertBool "10s after its last use" . not =<< Http.knownSession sessions other

signInSweeps :: IO ()
signInSweeps = do
    (clock, advance) <- fakeClock
    sessions <- Http.newSessionsWith (Http.SessionPolicy (Just 60) Nothing) clock
    -- a sign-in every 10s for 10 minutes, and nothing ever looked at again
    forM_ [1 .. 60 :: Int] $ \_ -> Http.newSession sessions >> advance 10
    held <- Http.sessionCount sessions
    assertBool ("at most one lifetime's worth held: " <> show held) (held <= 7)

-------------------------------------------------------------------------------

{- | The same over the wire, with a real two-second lifetime. The cookie
carries it as @Max-Age@; a stream opened with the session is cut when it
runs out, with nothing else happening; and from then on the cookie is a
@401@ on a read and, on @/@, a redirect to @/auth?ended@, whose form says
the session ended.
-}
sessionExpiry :: IO ()
sessionExpiry =
    withRunningWith (Http.SessionPolicy (Just 2) Nothing) $ \running -> do
        let tls = runningTls running
            withCookie c route = do
                req <- tlsRequest running Nothing route
                pure req{HTTP.requestHeaders = [(HTTP.hCookie, c)]}
        loginReq <- tlsRequest running Nothing "/auth"
        (_, lheaders, _) <- raw tls (HTTP.urlEncodedBody [("token", token)] loginReq)
        setCookie <- maybe (assertFailure "no cookie") pure (lookup "Set-Cookie" lheaders)
        assertBool ("Max-Age is the lifetime: " <> Char8.unpack setCookie) ("Max-Age=2" `ByteString.isInfixOf` setCookie)
        let cookie = Char8.takeWhile (/= ';') setCookie
        ev <- withCookie cookie "/events?since=0"
        ended <- timeout (10 * 1000000) $
            HTTP.withResponse ev tls $ \resp -> do
                assertEqual "/events with the cookie" 200 (HTTP.statusCode (HTTP.responseStatus resp))
                drain (HTTP.responseBody resp)
        assertEqual "the stream ends with the session" (Just ()) ended
        (scode, _, _) <- raw tls =<< withCookie cookie "/status"
        assertEqual "a read after the lifetime" 401 scode
        (rcode, rheaders, _) <- raw tls =<< withCookie cookie "/"
        assertEqual "the page after the lifetime" 303 rcode
        assertEqual "to the form, saying why" (Just "/auth?ended") (lookup HTTP.hLocation rheaders)
        (_, _, form) <- raw tls =<< tlsRequest running Nothing "/auth?ended"
        assertBool "the form says the session ended" ("session ended" `ByteString.isInfixOf` LChar8.toStrict form)
  where
    drain body = do
        chunk <- HTTP.brRead body
        unless (ByteString.null chunk) (drain body)

-- | Status, headers and body, redirects not followed.
raw :: HTTP.Manager -> HTTP.Request -> IO (Int, HTTP.ResponseHeaders, LChar8.ByteString)
raw manager req = do
    r <- timeout (10 * 1000000) (HTTP.httpLbs req{HTTP.redirectCount = 0} manager)
    case r of
        Nothing -> assertFailure ("no answer within 10s to " <> show (HTTP.path req))
        Just resp -> pure (HTTP.statusCode (HTTP.responseStatus resp), HTTP.responseHeaders resp, HTTP.responseBody resp)

-- | Read the stream until a complete @data:@ line has arrived, 10s at most.
readUntilData :: HTTP.BodyReader -> ByteString.ByteString -> IO ByteString.ByteString
readUntilData body acc
    | "\n\n" `ByteString.isInfixOf` acc && "data: " `ByteString.isInfixOf` acc = pure acc
    | otherwise = do
        chunk <- timeout (10 * 1000000) (HTTP.brRead body)
        case chunk of
            Nothing -> assertFailure ("no event within 10s; got " <> Char8.unpack acc)
            Just c | ByteString.null c -> assertFailure ("/events ended; got " <> Char8.unpack acc)
            Just c -> readUntilData body (acc <> c)

{- | Send bytes over a bare TCP connection and hand back whatever came back
before the server closed it — an exception counts as nothing.
-}
plainRequest :: Int -> ByteString.ByteString -> IO ByteString.ByteString
plainRequest port bytes = do
    r <- try $ do
        sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.connect sock (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
        SocketBS.sendAll sock bytes
        got <- timeout (10 * 1000000) (SocketBS.recv sock 4096)
        Socket.close sock
        pure (maybe ByteString.empty id got)
    pure (either (\(_ :: SomeException) -> ByteString.empty) id r)

{- | warp-tls answers a client that speaks plain HTTP on a TLS port with
@426 Upgrade Required@ and closes; a server that closed without a word
is refused too. What it must never be is an answer from a route.
-}
plainRefused :: ByteString.ByteString -> Bool
plainRefused got = ByteString.null got || "HTTP/1.1 426" `ByteString.isPrefixOf` got
