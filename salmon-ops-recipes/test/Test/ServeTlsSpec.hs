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
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
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
                    (Right (Just (CLI.TcpListen "127.0.0.1" 8443 "/c.pem" "/k.pem" "/t")))
                    (CLI.validateTcpOptions (allFour "127.0.0.1:8443"))
            , testCase "an IPv6 address in brackets" $
                assertEqual
                    "parsed"
                    (Right (Just (CLI.TcpListen "::1" 8443 "/c.pem" "/k.pem" "/t")))
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
            ]
        ]
  where
    allFour hostPort = CLI.TcpOptions (Just hostPort) (Just "/c.pem") (Just "/k.pem") (Just "/t")
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
    , runningWorld :: MVar (World Spec Spec)
    , runningUnixPath :: FilePath
    , runningPort :: Int
    , runningTls :: HTTP.Manager
    -- ^ trusts exactly the minted certificate
    , runningUnix :: HTTP.Manager
    }

withRunning :: (Running -> IO a) -> IO a
withRunning act =
    withTempDir $ \dir -> do
        material <- mintMaterial (dir </> "tls")
        let unixPath = dir </> "serve.http"
        (stdinR, stdinW) <- createPipe
        worldVar <- newEmptyMVar
        (own, _) <- capture
        upsRef <- newIORef Map.empty
        let binds =
                [ Http.BindUnix unixPath
                , Http.BindTls (Http.TlsBind "127.0.0.1" 0 material.materialCert material.materialKey token)
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
            r <- act (Running stdinW worldVar unixPath port tlsManager unixManager)
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
