{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The read surfaces and the command surface as HTTP over a unix socket:
@run serve --http PATH@.

Milestone 3 of @specs\/generic-server.md@. Four reads and one write:

  * @GET \/dag@ — the computed 'Dag' ('Serve.worldDag': the magma wired up
    with the ledger's precedence, the structure a convergence pass walks),
    one object per 'Ref' in 'Dag.dagOrder' with its edges in both directions
    and the loop's state for it. It exists from the first declaration on: a
    pass need not have run, and every node then simply reads @pending@. A
    node a retired declaration still describes is in it too, with
    @direction: down@, until its teardown is done and 'Serve.prune' drops
    it.
  * @GET \/status@, @GET \/history@ — the same objects @--json@ prints for
    the @status@ and @history@ commands ("Salmon.Reporter.Tagged"'s
    encoding, reused rather than re-described).
  * @GET \/help\/seed@ — this binary's own seed parser's @--help@ text, and
    the loop's command reference.
  * @POST \/command@ — one line of the input language, typed into the inbox
    exactly as a socket client's would be. Synchronous by default: the
    response is the JSON array of every report that line produced, which
    is what @curl@ and a CI step want. With @?async@ the line is queued and
    the response is the sequence number it was queued at, for a client
    that reads the event stream from there instead.
  * @GET \/events@ — the event stream, milestone 4, as server-sent events:
    every report the loop's reporters see, numbered, with @?since=N@ to
    replay what the ring still holds after @N@ and go on live, and
    @?stream=@\/@?origin=@ to narrow it. "Salmon.Actions.Serve.Events" is
    the record behind it; this module only writes it to a connection.
  * @GET \/@ and @GET \/ui\/*@ — the web UI (milestone 7), a handful of
    static files under @salmon-ops\/ui\/@ compiled into the binary with
    "Data.FileEmbed", so a binary is one file whatever it serves. The
    page is a client of the surfaces above and nothing more: it draws
    @\/dag@ and follows @\/events@ from that snapshot's @seq@. Nothing
    here is dynamic — a path outside the embedded set is the same @404@
    as any other unknown route, and there is no template.

= Reads never touch the inbox

Every command the loop handles first stands the tending machines down
('Serve.stopTending'), because a command is about to act. A read is not,
so it does not queue: it reads the loop's own 'Serve.World' cell through
the accessor 'Serve.serveObserved' hands over, and the tending snapshot that
cell already carries. That is what makes @\/status@ answer at once while a
node's @up@ is taking a minute in the loop, and it is the property the
loop's (R3) snapshot design was built for. The price is exactly what the
spec says: a read is at most one command old, and motion between commands
is the event stream's business, not this module's.

= One inbox, one attribution

A command is one more 'Serve.Producer' into the loop's inbox
('serverProducer'), pushed under an 'Origin' minted per request, followed
in the same transaction by that origin's 'Serve.Eof' so nothing another
producer types lands between the two. Which reports belong to the request
is the loop's knowledge — 'Serve.serveAttributed' stamps each with the
origin of the line being handled — and 'serverReporters' only collects the
ones stamped for a request still waiting, then lets the request go on the
loop's 'Serve.HungUp' for its origin, which the loop reports once every
line the origin typed has been handled. The same closing rule as
"Salmon.Actions.Serve.Socket", for the same reason.

= Its own socket, not the line protocol's

@--http@ takes a path of its own rather than sharing @--listen@'s and
telling the two apart by their first bytes. Detection itself is easy (an
HTTP request line is unmistakable); what it costs is elsewhere: the line
protocol reads its connection through a 'System.IO.Handle', which buffers
past the bytes peeked and cannot give them back, so both protocols would
have to be rewritten over raw sockets, and warp would need a
@Connection@ shim from its @Internal@ module to replay the peeked bytes.
Two paths cost one flag. The socket is bound through
'Socket.withUnixListener' all the same, so it is owner-only and refuses a
path something else is listening on — permissions are the whole access
story on that path (see the spec's security section).

= Over the network: TLS and a token, or nothing

Milestone 8. @run serve --http-tcp HOST:PORT --tls-cert FILE --tls-key
FILE --token-file FILE@ runs the same 'application' on a TCP listener
('withHttpServerOn', a 'BindTls' beside the unix 'BindUnix'), and the three
files are not optional: 'Bind' has no plaintext TCP constructor, and the
command line refuses @--http-tcp@ without all three, naming the missing
ones. A salmon server is root on the box one @up@ away, so it never listens
on a network without both. warp-tls answers a plain-HTTP client on that
port with @426 Upgrade Required@ and never reaches the application.

The token is 'requireToken', a middleware on the TCP listener only:
@Authorization: Bearer \<token\>@ on every route — the reads, the command,
the event stream, anything a later milestone adds to the application —
compared in constant time ('sameSecret') against the file's trimmed
content, or a session cookie a browser got by posting that token to
@\/auth@ (a browser cannot put a header on a navigation or an
@EventSource@), @401@ with a JSON error otherwise — @GET \/@ excepted,
which redirects to @\/auth@. @POST \/auth\/logout@ revokes the session,
cutting any event stream it had open. It is a middleware rather than
a check inside 'application' because the unix socket must stay token-free
(its permissions are its access story, and every client of it today is a
local one), and so that a route added to 'application' is covered without
its author knowing the token exists. Checking a token queues nothing: a
read is still a read. The file itself is 'readTokenFile': refused when
readable by others, or empty.

One 'Server' value serves both listeners — one event ring, one request
counter, one inbox — so sequence numbers are one sequence across them, and
an origin is 'originFor': @PATH#n@ on the unix socket, @ADDR:PORT#n@ (the
client's) over TCP, so @history@ says who typed a line from the network.
Not here: a client certificate instead of a token, a read-only token, a
plaintext option behind any flag, sessions that expire on their own.

= Sequence numbers

One counter, on 'Events'. A command @POST \/command@ queues is an
@enqueued@ event numbered from it (an @?async@ answer is that number), and
every report is numbered from it as it is published, in one transaction
with the ring and the broadcast — see "Salmon.Actions.Serve.Events" for
why that transaction, rather than the concurrent driver's reporter lock,
is the critical section. A client has one cursor: everything that happened
after its command was queued is everything numbered after the number it
was handed, and @\/dag@ and @\/status@ carry @seq@, the last number handed
out when the snapshot was read, so that @\/events?since=@ that number
resumes without a gap. The number is read __before__ the world, so an
event landing between the two reads is replayed rather than skipped.

= The stream on the wire

@\/events@ is a @text\/event-stream@ response that does not end until the
client goes or the loop does: one @id: N@ \/ @data: {...}@ per event (the
'Events.eventValue' object), a @gap@ event first when the ring no longer
reaches @?since@, and a comment line after every 'Events.configKeepAlive'
of silence, so a proxy between here and the client, or a client with a
read timeout, keeps the connection. A client hanging up is an exception
out of the write, which ends the stream and drops its subscription; the
loop ending sets 'serverStopped', on which every open stream returns, so
warp's shutdown does not wait behind a subscriber.
-}
module Salmon.Actions.Serve.Http (
    -- * Serving
    Server,
    serverName,
    serverEvents,
    serverBoundTcp,
    withHttpServer,
    withHttpServerWith,

    -- * Over the network
    Bind (..),
    TlsBind (..),
    withHttpServerOn,
    BadCredentials (..),
    requireToken,
    SessionPolicy (..),
    defaultSessionPolicy,
    SessionClock (..),
    systemSessionClock,
    Sessions,
    newSessions,
    newSessionsWith,
    newSession,
    knownSession,
    endSession,
    sessionCount,
    withStream,
    sessionOver,
    sameSecret,
    TokenError (..),
    readTokenFile,

    -- * Plugging into the loop
    serverObserver,
    serverProducer,
    serverReporters,

    -- * The read model
    WorldView (..),
    viewWorld,
    dagValue,
    application,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, withAsync)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay, retry, writeTChan, writeTVar)
import Control.Exception (Exception, bracket, bracket_, finally, fromException, throwIO)
import Control.Monad (forM_, join, unless, void, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Random as Random
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, object, withObject, (.:), (.=))
import Data.FileEmbed (embedDir, makeRelativeToProject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits (xor, (.&.), (.|.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath (takeExtension)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.TLS as TLS
import Network.Wai (Application, Request, Response)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import System.Posix.Files (fileMode, getFileStatus)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), Declaration, EpochId, Line (..), NodeState (..), Origin (..), Producer (..), World (..))
import qualified Salmon.Actions.Serve.Events as Events
import Salmon.Actions.Serve.Events (Events)
import qualified Salmon.Actions.Serve.Socket as Socket
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..))
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)
import Salmon.Op.Status (Direction (..))
import Salmon.Reporter
import Salmon.Reporter.Tagged (Tagged (..), nodeStatePairs, refValue, representativeValue)

-------------------------------------------------------------------------------

-- | One or more listeners with warp accepting on them, and the requests in flight.
data Server = Server
    { serverName :: String
    -- ^ what a request's origin is named after: the unix socket's path when
    -- there is one, else @HOST:PORT@ (see 'originFor')
    , serverBoundTcp :: TVar [Socket.SockAddr]
    -- ^ the TCP addresses actually bound, port @0@ resolved, in 'Bind' order
    , serverSeedHelp :: Text
    -- ^ the binary's own @config --help@, for @\/help\/seed@
    , serverInbox :: TVar (Maybe (TChan Line))
    -- ^ the loop's inbox, once the loop has started this server's producer
    , serverView :: TVar (Maybe (IO WorldView))
    -- ^ the read accessor, once the loop has handed it over
    , serverPending :: TVar (Map Origin Collector)
    -- ^ synchronous commands still waiting for their reports
    , serverCounter :: IORef Int
    -- ^ next request number; an origin is never reused within a run
    , serverEvents :: Events
    -- ^ the counter, the ring and the broadcast behind @\/events@
    , serverMode :: IO Serve.Mode
    -- ^ what @status@ says first: interactive, following, or replaying a
    -- cached document ("Salmon.Actions.Follow"); the loop's own accessor
    , serverStopped :: TVar Bool
    -- ^ set on the way out, so a request waiting on a loop that has ended
    -- answers with what it has rather than never
    }

-- | The reports a synchronous command has been answered with so far.
data Collector = Collector
    { collectorReports :: TVar [Tagged]
    -- ^ newest first
    , collectorDone :: TVar Bool
    }

{- | Where a server listens. A unix socket needs nothing but its path; TCP
needs everything in 'TlsBind', and there is deliberately no constructor for
TCP without it.
-}
data Bind
    = BindUnix FilePath
    | BindTls TlsBind
    deriving (Show)

{- | A TCP listener: the address to bind, the certificate and key warp-tls
serves, and the token every request on it must present. The token is the
file's content already read and trimmed ('readTokenFile'), so that a
server is refused before it binds rather than after.
-}
data TlsBind = TlsBind
    { tlsHost :: String
    -- ^ an address to bind, never a wildcard by omission: the caller spells it
    , tlsPort :: Int
    -- ^ @0@ for any free port; 'serverBoundTcp' says which
    , tlsCertFile :: FilePath
    , tlsKeyFile :: FilePath
    , tlsToken :: ByteString.ByteString
    , tlsSessions :: SessionPolicy
    -- ^ how long a browser's sign-in lasts ('defaultSessionPolicy' unless told)
    }
    deriving (Show)

{- | Bind the socket at the path (owner-only, see 'Socket.withUnixListener'),
serve HTTP on it for as long as the action runs, and take it down after.

The action normally runs the loop, with this server's producer, observer
and reporters plugged in; the server outlives the loop only for as long as
it takes the action to return, and a request still waiting at that point
is answered with the reports it collected.
-}
withHttpServer :: FilePath -> Text -> IO Serve.Mode -> (Server -> IO a) -> IO a
withHttpServer = withHttpServerWith Events.defaultConfig

-- | 'withHttpServer' with the event stream's ring size and keep-alive chosen.
withHttpServerWith :: Events.Config -> FilePath -> Text -> IO Serve.Mode -> (Server -> IO a) -> IO a
withHttpServerWith cfg path = withHttpServerOn cfg [BindUnix path]

{- | One server on every listener in the list: one 'Server' value — one
event ring, one request counter, one inbox — and the same 'application'
accepting on each, so a command typed over TCP and a read over the unix
socket see one world and one sequence of numbers. A unix bind is
'withHttpServerWith' exactly; a TCP bind is warp-tls over a socket bound
here (so port @0@ works and 'serverBoundTcp' reports what it became), with
'requireToken' in front of the application — the unix socket never asks for
a token, since its permissions are its access story, and the TCP listener
never answers without one. The certificate and key are loaded before
anything is bound, so a file that does not parse is an exception out of
this call rather than a listener thread dying quietly behind a running loop.

Binds are taken in order and released in reverse; the action runs once
every one of them is listening.
-}
withHttpServerOn :: forall a. Events.Config -> [Bind] -> Text -> IO Serve.Mode -> (Server -> IO a) -> IO a
withHttpServerOn cfg binds seedHelp mode act = do
    server <-
        Server name
            <$> newTVarIO []
            <*> pure seedHelp
            <*> newTVarIO Nothing
            <*> newTVarIO Nothing
            <*> newTVarIO Map.empty
            <*> newIORef 0
            <*> Events.newEvents cfg
            <*> pure mode
            <*> newTVarIO False
    listenOn server binds
  where
    name :: String
    name =
        case [p | BindUnix p <- binds] ++ [t.tlsHost <> ":" <> show t.tlsPort | BindTls t <- binds] of
            (n : _) -> n
            [] -> "http"

    settings = Warp.setServerName "salmon" Warp.defaultSettings

    listenOn :: Server -> [Bind] -> IO a
    listenOn server [] =
        act server `finally` atomically (writeTVar (serverStopped server) True)
    listenOn server (BindUnix path : more) =
        Socket.withUnixListener path $ \listener ->
            withAsync (Warp.runSettingsSocket settings (Socket.listenerSocket listener) (application server)) $ \_ ->
                listenOn server more
    listenOn server (BindTls tls : more) = do
        -- warp-tls loads these on its own thread, where a bad file is an
        -- error nobody waits on; load them here first so it is ours.
        _ <- either (throwIO . BadCredentials tls.tlsCertFile tls.tlsKeyFile) pure =<< TLS.credentialLoadX509 tls.tlsCertFile tls.tlsKeyFile
        withTcpListener tls.tlsHost tls.tlsPort $ \sock addr -> do
            atomically (modifyTVar' (serverBoundTcp server) (++ [addr]))
            let tlsSettings = WarpTLS.tlsSettings tls.tlsCertFile tls.tlsKeyFile
                -- warp prints what its hook is handed, and two families of
                -- exception on a TLS listener are the listener working as
                -- intended rather than anything to trace: a plain-HTTP
                -- client answered 426 and refused (warp-tls throws after),
                -- and a TLS-level error on one connection — a client that
                -- closed without a close-notify (`PostHandshake Error_EOF`,
                -- which curl does on every request) or whose handshake
                -- failed, neither of which reached the application. The
                -- startup line is meant to be the only thing on stderr.
                quietly = Warp.setOnException $ \mreq e ->
                    case (fromException e, fromException e) of
                        (Just WarpTLS.InsecureConnectionDenied, _) -> pure ()
                        (_, Just (_ :: TLS.TLSException)) -> pure ()
                        _ -> Warp.defaultOnException mreq e
            sessions <- newSessions tls.tlsSessions
            withAsync (WarpTLS.runTLSSocket tlsSettings (quietly settings) sock (requireToken tls.tlsToken sessions (application server))) $ \_ ->
                listenOn server more

-- | The certificate or key given for a TCP listener did not load.
data BadCredentials = BadCredentials FilePath FilePath String
    deriving (Show)

instance Exception BadCredentials

{- | A bound, listening TCP socket at the address, and the address it got
(the port resolved when @0@ was asked for); closed on the way out.
-}
withTcpListener :: String -> Int -> (Socket.Socket -> Socket.SockAddr -> IO a) -> IO a
withTcpListener host port act = do
    let hints = Socket.defaultHints{Socket.addrFlags = [Socket.AI_PASSIVE, Socket.AI_NUMERICSERV], Socket.addrSocketType = Socket.Stream}
    addrs <- Socket.getAddrInfo (Just hints) (Just host) (Just (show port))
    addr <- case addrs of
        (a : _) -> pure a
        [] -> throwIO (userError ("no address to bind for " <> host <> ":" <> show port))
    bracket (Socket.openSocket addr) Socket.close $ \sock -> do
        Socket.setSocketOption sock Socket.ReuseAddr 1
        Socket.bind sock (Socket.addrAddress addr)
        Socket.listen sock 16
        bound <- Socket.getSocketName sock
        act sock bound

-------------------------------------------------------------------------------
-- the token

{- | Refuse every request on this listener that is not authenticated, with
@401@ and a JSON error. Authenticated is either of two things: an
@Authorization: Bearer <token>@ header for exactly this token — what a
script or @curl@ sends — or a session cookie the listener handed out at
@\/auth@, which is what a browser sends, since nothing lets a page put a
header on a navigation or on an @EventSource@. Every route, the event
stream included: a read of the output ring is as sensitive as a command
(the spec's decision), so there is no route a network client gets for free.

Four routes are this middleware's own and are answered before the check.
@GET \/auth@ is a form asking for the token, and @POST \/auth@ with the
token as the form's @token@ field answers @303@ to @\/@ with a
@__Host-salmon-session@ cookie (@HttpOnly@, @Secure@, @SameSite=Strict@,
@Path=\/@), or @401@ and the form again. And @GET \/@ without either
credential is a @303@ to @\/auth@ rather than a @401@, since whoever asks
for the page is a browser that can do something about it.
@POST \/auth\/logout@ revokes the session the request carries, if any,
and answers @303@ to @\/auth@ with the cookie expired — the same answer
whoever asks, so it says nothing about whether the cookie was good — and
an @\/events@ stream opened with that session ends at once rather than
at its next request ('untilEnded'). @GET \/auth\/session@ answers
@{"session": true}@ when the request's cookie is a live session, which is
how the page knows to offer the button; on the unix socket, where nothing
is signed in, the application answers @false@.

The cookie is not the token: it is 32 random bytes minted per login and
known only to this listener ('Sessions'), so the token a script uses is
never stored in a browser, and a restart logs every browser out.
@SameSite=Strict@ is what keeps another site's page from posting a
command with it. The token comparison is constant-time ('sameSecret'), the
scheme name is matched without regard to case, the token itself exactly;
a session is looked up by its SHA-256, so the lookup's timing says nothing
about the cookie either.
-}
requireToken :: ByteString.ByteString -> Sessions -> Wai.Middleware
requireToken token sessions app req respond =
    case (Wai.requestMethod req, Wai.pathInfo req) of
        ("GET", ["auth"])
            | "ended" `elem` map fst (Wai.queryString req) -> respond (loginPage HTTP.status200 SessionEnded)
            | otherwise -> respond (loginPage HTTP.status200 NoNote)
        ("POST", ["auth"]) -> do
            body <- boundedBody 4096 req
            let presented = join . lookup "token" . HTTP.parseQuery =<< body
            case presented of
                Just p | sameSecret p token -> do
                    cookie <- newSession sessions
                    respond $
                        Wai.responseLBS
                            HTTP.status303
                            [ (HTTP.hLocation, "/")
                            , ("Set-Cookie", sessionCookie <> "=" <> cookie <> "; Path=/; Secure; HttpOnly; SameSite=Strict" <> maxAge)
                            , (HTTP.hCacheControl, "no-store")
                            ]
                            ""
                _ -> respond (loginPage HTTP.status401 WrongToken)
        (_, ["auth"]) -> respond (methodNotAllowed ["GET", "POST"])
        ("POST", ["auth", "logout"]) -> do
            -- whoever asks, the answer is the same: the cookie expired and
            -- the form; a session is revoked only if the request carried it
            forM_ (cookieOf req) (endSession sessions)
            respond $
                Wai.responseLBS
                    HTTP.status303
                    [ (HTTP.hLocation, "/auth")
                    , ("Set-Cookie", sessionCookie <> "=; Path=/; Secure; HttpOnly; SameSite=Strict; Max-Age=0")
                    , (HTTP.hCacheControl, "no-store")
                    ]
                    ""
        (_, ["auth", "logout"]) -> respond (methodNotAllowed ["POST"])
        ("GET", ["auth", "session"]) -> do
            signedIn <- maybe (pure False) (knownSession sessions) (cookieOf req)
            respond (json HTTP.status200 (object ["session" .= signedIn]))
        (_, ["auth", "session"]) -> respond (methodNotAllowed ["GET"])
        _ -> do
            credential <-
                case bearerOf =<< lookup HTTP.hAuthorization (Wai.requestHeaders req) of
                    Just presented -> pure (if sameSecret presented token then ByToken else NoCredential)
                    Nothing -> case cookieOf req of
                        Nothing -> pure NoCredential
                        Just cookie -> do
                            known <- knownSession sessions cookie
                            pure (if known then BySession cookie else NoCredential)
            case (credential, Wai.requestMethod req, Wai.pathInfo req) of
                (ByToken, _, _) -> app req respond
                -- a stream outlives the check that let it in, so one opened
                -- with a session ends when the session does
                (BySession cookie, _, ["events"]) -> app req (respond . untilEnded sessions cookie)
                (BySession _, _, _) -> app req respond
                -- a cookie that is no longer a session was one: say so on the form
                (NoCredential, "GET", []) ->
                    respond (Wai.responseLBS HTTP.status303 [(HTTP.hLocation, maybe "/auth" (const "/auth?ended") (cookieOf req))] "")
                _ ->
                    respond $
                        Wai.responseLBS
                            HTTP.status401
                            [(HTTP.hContentType, "application/json"), ("WWW-Authenticate", "Bearer")]
                            (encode (object ["error" .= ("a bearer token is required" :: Text)]))
  where
    -- the browser forgets the cookie when the server does, when there is a when
    maxAge :: ByteString.ByteString
    maxAge = maybe "" (\l -> "; Max-Age=" <> Char8.pack (show (ceiling l :: Int))) sessions.sessionsPolicy.sessionLifetime

    bearerOf :: ByteString.ByteString -> Maybe ByteString.ByteString
    bearerOf h =
        let (scheme, rest) = Char8.break (== ' ') h
         in if Char8.map toLower scheme == "bearer"
                then Just (Char8.dropWhileEnd isSpace (Char8.dropWhile (== ' ') rest))
                else Nothing

-- | What a request presented that let it in.
data Credential = ByToken | BySession ByteString.ByteString | NoCredential

{- | The response with its body cut short the moment the session is over
(signed out, or past its lifetime; 'sessionOver'), and held as one of the
session's open streams while it runs ('withStream'): 'Wai.responseToStream' is every response as a streaming one, and
the body races a wait on the session's membership. For an @\/events@
stream that is the difference between signing out and signing out except
in the tab still watching.
-}
untilEnded :: Sessions -> ByteString.ByteString -> Response -> Response
untilEnded sessions cookie resp =
    let (status, headers, withBody) = Wai.responseToStream resp
     in Wai.responseStream status headers $ \write flush ->
            withBody $ \body ->
                withStream sessions cookie (void (race (body write flush) (sessionOver sessions cookie)))

-- | The session cookie's name; @__Host-@ makes a browser refuse it unless @Secure@, @Path=\/@ and no @Domain@.
sessionCookie :: ByteString.ByteString
sessionCookie = "__Host-salmon-session"

-- | The value of 'sessionCookie' among the request's cookies, from every @Cookie@ header (HTTP\/2 may split them).
cookieOf :: Request -> Maybe ByteString.ByteString
cookieOf req =
    lookup sessionCookie
        [ (name, ByteString.drop 1 value)
        | (h, v) <- Wai.requestHeaders req
        , h == HTTP.hCookie
        , pair <- Char8.split ';' v
        , let (name, value) = Char8.break (== '=') (Char8.dropWhile isSpace pair)
        ]

{- | The body, if it is no longer than the limit: a login form is a few
dozen bytes, and nothing unauthenticated gets to make the server hold more.
-}
boundedBody :: Int -> Request -> IO (Maybe ByteString.ByteString)
boundedBody limit req = go 0 []
  where
    go n acc = do
        chunk <- Wai.getRequestBodyChunk req
        let n' = n + ByteString.length chunk
        if ByteString.null chunk
            then pure (Just (ByteString.concat (reverse acc)))
            else if n' > limit then pure Nothing else go n' (chunk : acc)

-- | What the form says above the button, if anything.
data FormNote = NoNote | WrongToken | SessionEnded

-- | @ui\/auth.html@, with the note in its place.
loginPage :: HTTP.Status -> FormNote -> Response
loginPage status note =
    Wai.responseLBS
        status
        [(HTTP.hContentType, "text/html; charset=utf-8"), (HTTP.hCacheControl, "no-store")]
        (LByteString.fromStrict (before <> line <> after))
  where
    page = maybe "" id (lookup "auth.html" uiFiles)
    (before, after) = ByteString.breakSubstring "<!--refused-->" page
    line = case note of
        NoNote -> ""
        WrongToken -> "<p class=\"refused\">That is not the token.</p>"
        SessionEnded -> "<p class=\"ended\">Your session ended; sign in again.</p>"

{- | How long a session lasts. Two limits, each 'Nothing' for none, because
they guard against different things. The __lifetime__ counts from sign-in
and ends a session however busy it is — it bounds how long a stolen
cookie or a tab left open over a weekend stays good, and it cuts an event
stream the session has open. The __idle__ limit counts from the session's
last use, and an open @\/events@ stream is use: the page makes almost no
requests while it is being watched, and a dashboard left open should not
expire for being looked at. So "idle" means nobody is looking, and the
lifetime is what ends a session somebody is.

What bounds the listener's memory is that an ended session is dropped at
every sign-in ('newSession'), and at the next request that presents it —
at most the sessions signed in within one lifetime are held. With both
limits off nothing ends a session but signing out or a restart.
-}
data SessionPolicy = SessionPolicy
    { sessionLifetime :: Maybe Double
    -- ^ seconds from sign-in
    , sessionIdle :: Maybe Double
    -- ^ seconds since last use, not counting while a stream is open
    }
    deriving (Show, Eq)

-- | Twelve hours from sign-in, one hour of nobody looking.
defaultSessionPolicy :: SessionPolicy
defaultSessionPolicy = SessionPolicy (Just (12 * 3600)) (Just 3600)

{- | Where sessions get their time: now, and a wait for a moment to come,
both in seconds on one monotonic scale. 'systemSessionClock' is the real
one; a test's moves when it says so.
-}
data SessionClock = SessionClock
    { clockNow :: IO Double
    , clockSleepUntil :: Double -> IO ()
    }

systemSessionClock :: SessionClock
systemSessionClock = SessionClock now sleepUntil
  where
    now = (/ 1e9) . fromIntegral <$> getMonotonicTimeNSec
    sleepUntil t = do
        n <- now
        when (n < t) $ do
            -- in slices, so a far deadline is not one huge threadDelay
            threadDelay (ceiling (min 60 (t - n) * 1e6))
            sleepUntil t

-- | One session: when it was signed in, when it was last used, how many streams it holds open.
data Session = Session
    { sessionCreated :: !Double
    , sessionLastSeen :: !Double
    , sessionStreams :: !Int
    }

{- | The sessions a TCP listener has handed out at @\/auth@, by the SHA-256
of their cookie. One lives until it is signed out ('endSession'), until
the 'SessionPolicy' ends it, or until the process ends.
-}
data Sessions = Sessions
    { sessionsPolicy :: SessionPolicy
    , sessionsClock :: SessionClock
    , sessionsVar :: TVar (Map ByteString.ByteString Session)
    }

newSessions :: SessionPolicy -> IO Sessions
newSessions policy = newSessionsWith policy systemSessionClock

newSessionsWith :: SessionPolicy -> SessionClock -> IO Sessions
newSessionsWith policy clock = Sessions policy clock <$> newTVarIO Map.empty

-- | Whether the policy still lets a session stand at this moment.
live :: SessionPolicy -> Double -> Session -> Bool
live policy now sess =
    maybe True (\l -> now - sess.sessionCreated < l) policy.sessionLifetime
        && (sess.sessionStreams > 0 || maybe True (\i -> now - sess.sessionLastSeen < i) policy.sessionIdle)

{- | Mint a session and hand back its cookie value, dropping every session
the policy has ended on the way: signing in is the one moment the set
grows, so it is the moment it is swept.
-}
newSession :: Sessions -> IO ByteString.ByteString
newSession sessions = do
    raw <- Random.getRandomBytes 32
    now <- sessions.sessionsClock.clockNow
    let cookie = Base64.encodeUnpadded raw
    atomically $
        modifyTVar' sessions.sessionsVar $
            Map.insert (SHA256.hash cookie) (Session now now 0) . Map.filter (live sessions.sessionsPolicy now)
    pure cookie

{- | Whether the cookie is a session that still stands, counting this as a
use of it. One the policy has ended is dropped here, so it is gone for
every request after this one too.
-}
knownSession :: Sessions -> ByteString.ByteString -> IO Bool
knownSession sessions cookie = do
    now <- sessions.sessionsClock.clockNow
    let key = SHA256.hash cookie
    atomically $ do
        m <- readTVar sessions.sessionsVar
        case Map.lookup key m of
            Nothing -> pure False
            Just sess
                | live sessions.sessionsPolicy now sess -> do
                    writeTVar sessions.sessionsVar (Map.insert key sess{sessionLastSeen = now} m)
                    pure True
                | otherwise -> do
                    writeTVar sessions.sessionsVar (Map.delete key m)
                    pure False

-- | Revoke a session; a cookie that was never one is nothing to revoke.
endSession :: Sessions -> ByteString.ByteString -> IO ()
endSession sessions cookie = atomically (modifyTVar' sessions.sessionsVar (Map.delete (SHA256.hash cookie)))

-- | How many sessions are held, ended or not: what a sweep is for.
sessionCount :: Sessions -> IO Int
sessionCount sessions = Map.size <$> readTVarIO sessions.sessionsVar

{- | Run the action as a stream the session holds open: the idle clock
stops while it runs, and restarts from the moment it ends.
-}
withStream :: Sessions -> ByteString.ByteString -> IO a -> IO a
withStream sessions cookie = bracket_ (adjust 1) (adjust (-1))
  where
    key = SHA256.hash cookie
    adjust n = do
        now <- sessions.sessionsClock.clockNow
        atomically $ modifyTVar' sessions.sessionsVar $ Map.adjust (\sess -> sess{sessionStreams = sess.sessionStreams + n, sessionLastSeen = now}) key

{- | Blocks until the session is over: signed out, or past its lifetime —
in which case it is dropped here, so the requests after it see as much.
The idle limit does not end a session with a stream open, so a stream
has only these two to wait for.
-}
sessionOver :: Sessions -> ByteString.ByteString -> IO ()
sessionOver sessions cookie = do
    created <- fmap (.sessionCreated) . Map.lookup key <$> readTVarIO sessions.sessionsVar
    case (created, sessions.sessionsPolicy.sessionLifetime) of
        (Nothing, _) -> pure ()
        (Just c, Just lifetime) -> do
            r <- race (atomically removed) (sessions.sessionsClock.clockSleepUntil (c + lifetime))
            either pure (const (endSession sessions cookie)) r
        (Just _, Nothing) -> atomically removed
  where
    key = SHA256.hash cookie
    removed = readTVar sessions.sessionsVar >>= STM.check . not . Map.member key

{- | Equal, in time that depends on the lengths and not on where the first
differing byte is: every byte is folded whether or not an earlier one
already differed, and the length comparison is folded in the same way
rather than short-circuiting.
-}
sameSecret :: ByteString.ByteString -> ByteString.ByteString -> Bool
sameSecret a b = (lengthBit .|. foldl' (.|.) 0 (ByteString.zipWith xor a b)) == 0
  where
    lengthBit :: Word8
    lengthBit = if ByteString.length a == ByteString.length b then 0 else 1

-- | Why a token file was not accepted.
data TokenError
    = -- | others can read it, so it is not a secret: the file's mode
      TokenFileReadable FilePath
    | -- | nothing but whitespace in it
      TokenFileEmpty FilePath
    deriving (Show, Eq)

instance Exception TokenError

{- | The token in a file: its content with surrounding whitespace removed
(so a trailing newline from @echo@ is not part of it). Refused when the
file is readable by others — a token anyone on the box can read is not one —
and when it is empty, which would make every request with an empty
@Bearer@ valid. A file that cannot be read at all throws as any read does.
-}
readTokenFile :: FilePath -> IO (Either TokenError ByteString.ByteString)
readTokenFile path = do
    st <- getFileStatus path
    if fileMode st .&. 0o004 /= 0
        then pure (Left (TokenFileReadable path))
        else do
            raw <- ByteString.readFile path
            let token = Char8.dropWhileEnd isSpace (Char8.dropWhile isSpace raw)
            pure (if ByteString.null token then Left (TokenFileEmpty path) else Right token)

{- | What to hand 'Serve.serveObserved': installs the read accessor. Reads
answer @503@ until it has been.
-}
serverObserver :: Server -> IO (World seed directive) -> IO ()
serverObserver server readWorld =
    atomically (writeTVar (serverView server) (Just (viewWorld <$> readWorld)))

{- | The producer to run beside the loop's others. It types nothing of its
own: it publishes the inbox for requests to push into and then waits to be
killed with the loop, at which point the inbox is withdrawn and a command
arriving afterwards answers @503@.
-}
serverProducer :: Server -> Producer
serverProducer server = Producer $ \inbox -> do
    atomically (writeTVar (serverInbox server) (Just inbox))
    atomically retry `finally` atomically (writeTVar (serverInbox server) Nothing)

{- | Wrap the loop's reporters: every report goes on unchanged, then onto
the event stream ('Events.eventsReporter', numbered there), and one stamped
with a waiting request's origin is collected for that request's response.
The loop's 'Serve.HungUp' for such an origin releases the request — it is
the loop saying every line typed under that origin has been handled, so the
response is complete.
-}
serverReporters ::
    Server ->
    (Reporter (Attributed Serve.Report), Reporter (Attributed (UpDown.Report Extension))) ->
    (Reporter (Attributed Serve.Report), Reporter (Attributed (UpDown.Report Extension)))
serverReporters server (serveR, updownR) = (serveR', updownR')
  where
    serveR' :: Reporter (Attributed Serve.Report)
    serveR' = ReporterM $ \a@(Attributed origin rep) -> do
        runReporter serveR a
        runReporter events (FromServe <$> a)
        forM_ origin (collect (FromServe rep))
        case rep of
            Serve.HungUp gone -> release gone
            _ -> pure ()

    updownR' :: Reporter (Attributed (UpDown.Report Extension))
    updownR' = ReporterM $ \a@(Attributed origin rep) -> do
        runReporter updownR a
        runReporter events (FromUpDown <$> a)
        forM_ origin (collect (FromUpDown rep))

    events :: Reporter (Attributed Tagged)
    events = Events.eventsReporter (serverEvents server)

    collect :: Tagged -> Origin -> IO ()
    collect tagged origin = atomically $ do
        pending <- readTVar (serverPending server)
        forM_ (Map.lookup origin pending) $ \c ->
            modifyTVar' (collectorReports c) (tagged :)

    release :: Origin -> IO ()
    release origin = atomically $ do
        pending <- readTVar (serverPending server)
        forM_ (Map.lookup origin pending) $ \c -> do
            writeTVar (collectorDone c) True
            writeTVar (serverPending server) (Map.delete origin pending)

-------------------------------------------------------------------------------
-- the read model

{- | What the reads are answered from: the parts of a 'World' they need,
computed at the moment of the read from the loop's own cell.
-}
data WorldView = WorldView
    { viewDag :: Dag Extension
    , viewConflicts :: Map Ref Serve.Collision
    , viewNodes :: Map Ref NodeState
    , viewPaths :: Map Ref [Text]
    , viewHistory :: [(EpochId, Declaration, Bool, Origin, [String])]
    , viewElided :: Int
    }

viewWorld :: World seed directive -> WorldView
viewWorld w =
    WorldView
        { viewDag = Serve.worldDag w
        , viewConflicts = w.worldConflicts
        , viewNodes = w.worldNodes
        , viewPaths = Serve.worldPaths w
        , viewHistory = Serve.historyLinesMatching (const True) w
        , viewElided = w.worldLogDropped
        }

{- | @\/dag@: the nodes in 'Dag.dagOrder', each the 'Act' projection — the
fields 'Dag.sameRepresentative' compares (shorthand, help, notes, the
rendering of dynamics) and the loop's state for the node, as @status@
lists it — plus its dependencies and dependants as refs, and, for a node
whose representative won a collision that is still standing
('Serve.Collision'), a @conflict@ with the @kept@ and @replaced@
representatives, so a client can show the pair without having caught the
pass's @conflicting@ event. Structurally what
'Salmon.Actions.Help.printDagTree' prints for the same 'Dag', with the
state added. The envelope carries the loop's 'Serve.Mode' at the moment of
the read, the same value @\/status@ opens with, so a client knows which
guarantees the nodes it is looking at are under.
-}
dagValue :: Serve.Mode -> WorldView -> Value
dagValue mode v =
    object
        [ "mode" .= mode
        , "nodes"
            .= [ nodeObject r act
               | r <- Dag.dagOrder dag
               , Just act <- [Dag.representativeOf dag r]
               ]
        ]
  where
    dag = viewDag v

    nodeObject :: Ref -> Act Extension -> Value
    nodeObject r act =
        object $
            nodeStatePairs (viewPaths v) Nothing (r, stateOf r act)
                ++ [ "notes" .= rep.repNotes
                   , "dynamics" .= rep.repDynamics
                   , "dependencies" .= fmap refValue (Dag.dependenciesOf dag r)
                   , "dependants" .= fmap refValue (Dag.dependantsOf dag r)
                   ]
                ++ [ "conflict" .= object ["kept" .= representativeValue c.conflictKept, "replaced" .= representativeValue c.conflictReplaced]
                   | Just col <- [Map.lookup r (viewConflicts v)]
                   , let c = col.collisionConflict
                   ]
      where
        rep = Dag.representative act

    -- 'Serve.prune' keeps 'worldNodes' and 'worldMagma' on the same key
    -- set, so this is always a hit; a miss would be a node the ledger
    -- describes and nothing wants, which is what the fallback says.
    stateOf :: Ref -> Act Extension -> NodeState
    stateOf r act =
        Map.findWithDefault
            (NodeState act.shorthand act.extension.help TurnDown Serve.Pending Nothing)
            r
            (viewNodes v)

-------------------------------------------------------------------------------
-- the application

-- | The body of @POST \/command@ when it is JSON.
newtype CommandBody = CommandBody String

instance FromJSON CommandBody where
    parseJSON = withObject "command" $ \o -> CommandBody <$> o .: "line"

application :: Server -> Application
application server req respond =
    case (Wai.requestMethod req, Wai.pathInfo req) of
        ("GET", ["dag"]) -> withView $ \seqNo v -> do
            mode <- serverMode server
            respond (json HTTP.status200 (withSeq seqNo (dagValue mode v)))
        ("GET", ["status"]) -> withView $ \seqNo v -> do
            mode <- serverMode server
            respond (json HTTP.status200 (withSeq seqNo (toJSON (FromServe (Serve.StatusReport mode (Map.toList (viewNodes v)) (viewPaths v))))))
        ("GET", ["history"]) -> withView $ \_ v ->
            respond (json HTTP.status200 (withElided (viewElided v) (toJSON (FromServe (Serve.HistoryReport (viewHistory v))))))
        ("GET", ["help", "seed"]) ->
            respond $
                json
                    HTTP.status200
                    ( object
                        [ "seed" .= serverSeedHelp server
                        , "commands" .= Serve.renderReport (Serve.HelpText Nothing)
                        ]
                    )
        ("POST", ["command"]) -> command >>= respond
        ("GET", ["events"]) -> events
        ("GET", []) -> respond (static "index.html")
        -- over TCP 'requireToken' answers this; anywhere else there is nothing to log into
        ("GET", ["auth"]) -> respond (Wai.responseLBS HTTP.status303 [(HTTP.hLocation, "/")] "")
        ("POST", ["auth", "logout"]) -> respond (Wai.responseLBS HTTP.status303 [(HTTP.hLocation, "/")] "")
        ("GET", ["auth", "session"]) -> respond (json HTTP.status200 (object ["session" .= False]))
        ("GET", ("ui" : rest)) -> respond (static (Text.unpack (Text.intercalate "/" rest)))
        (_, ["events"]) -> respond (methodNotAllowed ["GET"])
        (_, ["dag"]) -> respond (methodNotAllowed ["GET"])
        (_, ["status"]) -> respond (methodNotAllowed ["GET"])
        (_, ["history"]) -> respond (methodNotAllowed ["GET"])
        (_, ["help", "seed"]) -> respond (methodNotAllowed ["GET"])
        (_, ["command"]) -> respond (methodNotAllowed ["POST"])
        (_, []) -> respond (methodNotAllowed ["GET"])
        (_, "ui" : _) -> respond (methodNotAllowed ["GET"])
        _ -> respond (failure HTTP.status404 "no such resource")
  where
    -- the snapshot and the last sequence number at the time it was taken:
    -- the number first, so what lands in between is replayed, never
    -- skipped (see "Sequence numbers" above).
    withView :: (Word64 -> WorldView -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
    withView k = do
        mread <- readTVarIO (serverView server)
        case mread of
            Nothing -> respond (failure HTTP.status503 "the loop has not started")
            Just readView -> do
                seqNo <- Events.lastSequence (serverEvents server)
                readView >>= k seqNo

    withSeq :: Word64 -> Value -> Value
    withSeq n (Object o) = Object (KeyMap.insert "seq" (toJSON n) o)
    withSeq n v = object ["report" .= v, "seq" .= n]

    -- the history object as `--json` prints it, with the count `history`
    -- would print as a second object folded in as a field.
    withElided :: Int -> Value -> Value
    withElided n (Object o) = Object (KeyMap.insert "elided" (toJSON n) o)
    withElided n v = object ["report" .= v, "elided" .= n]

    command :: IO Response
    command = do
        body <- Wai.strictRequestBody req
        case commandLine req body of
            Left err -> pure (failure HTTP.status400 err)
            Right line -> do
                minbox <- readTVarIO (serverInbox server)
                case minbox of
                    Nothing -> pure (failure HTTP.status503 "the loop is not taking commands")
                    Just inbox -> do
                        n <- atomicModifyIORef' (serverCounter server) (\k -> (k + 1, k))
                        let origin = originFor server req n
                        -- numbered before it is queued, so every report
                        -- the line produces is numbered after it
                        seqNo <- Events.enqueued (serverEvents server) origin line
                        if asynchronous
                            then do
                                atomically (enqueue inbox origin line)
                                pure (json HTTP.status202 (object ["seq" .= seqNo, "origin" .= Serve.originName origin]))
                            else do
                                c <- Collector <$> newTVarIO [] <*> newTVarIO False
                                atomically $ do
                                    modifyTVar' (serverPending server) (Map.insert origin c)
                                    enqueue inbox origin line
                                reports <- atomically $ do
                                    done <- readTVar (collectorDone c)
                                    stopped <- readTVar (serverStopped server)
                                    unless (done || stopped) retry
                                    modifyTVar' (serverPending server) (Map.delete origin)
                                    reverse <$> readTVar (collectorReports c)
                                pure (json HTTP.status200 (toJSON reports))

    -- the line and its end of input in one transaction, so nothing another
    -- producer types can land between the two.
    enqueue inbox origin line = do
        writeTChan inbox (Line origin line)
        writeTChan inbox (Eof origin)

    asynchronous :: Bool
    asynchronous = any ((== "async") . fst) (Wai.queryString req)

    -- @\/events@: replay from @?since@ (a gap first if the ring no longer
    -- reaches it), then live until the client or the loop goes.
    events :: IO Wai.ResponseReceived
    events =
        case eventsQuery req of
            Left err -> respond (failure HTTP.status400 err)
            Right (since, filt) ->
                respond $ Wai.responseStream HTTP.status200 sseHeaders $ \write flush ->
                    Events.withSubscription (serverEvents server) since $ \sub -> do
                        forM_ (Events.subscriptionGap sub) (write . Events.renderGap)
                        forM_ (filter (Events.matches filt) (Events.subscriptionReplay sub)) (write . Events.renderEvent)
                        flush
                        let keepAlive = Events.configKeepAlive (Events.eventsConfig (serverEvents server))
                            live = do
                                expired <- registerDelay keepAlive
                                next <-
                                    atomically $
                                        (Just . Just <$> Events.subscriptionLive sub)
                                            `orElse` (Nothing <$ (readTVar (serverStopped server) >>= STM.check))
                                            `orElse` (Just Nothing <$ (readTVar expired >>= STM.check))
                                case next of
                                    Nothing -> pure ()
                                    Just Nothing -> write Events.keepAlive >> flush >> live
                                    Just (Just e) -> do
                                        when (Events.matches filt e) (write (Events.renderEvent e) >> flush)
                                        live
                        live

    sseHeaders :: [HTTP.Header]
    sseHeaders =
        [ (HTTP.hContentType, "text/event-stream")
        , (HTTP.hCacheControl, "no-cache")
        , ("X-Accel-Buffering", "no")
        ]

{- | The origin a request's command is typed under: @NAME#n@, where @NAME@
is the server's ('serverName', the unix socket's path) for a request on the
unix socket and the client's own address for one over TCP — @history@ then
says which network client typed a line, which "the socket" does not.
-}
originFor :: Server -> Request -> Int -> Origin
originFor server req n = Origin (Text.pack (name <> "#" <> show n))
  where
    name = case Wai.remoteHost req of
        Socket.SockAddrUnix _ -> serverName server
        addr -> show addr

-- | @?since=N@, @?stream=a,b@ (repeatable), @?origin=NAME@ (repeatable).
eventsQuery :: Request -> Either Text (Maybe Word64, Events.Filter)
eventsQuery req = do
    since <- case lookup "since" query of
        Nothing -> Right Nothing
        Just Nothing -> Left "since needs a number"
        Just (Just raw) -> case Text.decimal (Text.decodeUtf8 raw) of
            Right (n, rest) | Text.null rest -> Right (Just n)
            _ -> Left "since is not a number"
    let listed key = [Text.strip v | (k, Just raw) <- query, k == key, v <- Text.splitOn "," (Text.decodeUtf8 raw), not (Text.null (Text.strip v))]
        setOf key = case listed key of
            [] -> Nothing
            vs -> Just (Set.fromList vs)
    pure (since, Events.Filter{Events.filterStreams = setOf "stream", Events.filterOrigins = setOf "origin"})
  where
    query = Wai.queryString req

-- | One line, from a JSON @{"line": ...}@ body or a text one.
commandLine :: Request -> LByteString.ByteString -> Either Text String
commandLine req body
    | isJson = case Aeson.eitherDecode body of
        Left err -> Left ("body is not a {\"line\": ...} object: " <> Text.pack err)
        Right (CommandBody line) -> oneLine line
    | otherwise = case Text.decodeUtf8' (LByteString.toStrict body) of
        Left _ -> Left "body is not UTF-8"
        Right t -> oneLine (Text.unpack (Text.dropWhileEnd (== '\n') t))
  where
    isJson =
        case lookup HTTP.hContentType (Wai.requestHeaders req) of
            Just ct -> "application/json" `ByteString.isPrefixOf` ct
            Nothing -> False
    oneLine line
        | '\n' `elem` line = Left "one command per request"
        | otherwise = Right line

json :: HTTP.Status -> Value -> Response
json status v = Wai.responseLBS status [(HTTP.hContentType, "application/json")] (encode v)

-------------------------------------------------------------------------------
-- the web UI

{- | The files under @salmon-ops\/ui\/@, read at compile time. Relative
paths as the page references them (@ui.js@, @ui.css@), with @index.html@
the page itself.
-}
uiFiles :: [(FilePath, ByteString.ByteString)]
uiFiles = $(makeRelativeToProject "ui" >>= embedDir)

{- | One embedded file, or the same @404@ an unknown route gets — the set is
closed at compile time, so there is nothing to look up on disk.
-}
static :: FilePath -> Response
static path =
    case lookup path uiFiles of
        Nothing -> failure HTTP.status404 "no such resource"
        Just body -> Wai.responseLBS HTTP.status200 [(HTTP.hContentType, contentType path)] (LByteString.fromStrict body)

-- | By extension; the embedded set only holds these three kinds.
contentType :: FilePath -> ByteString.ByteString
contentType path =
    case takeExtension path of
        ".html" -> "text/html; charset=utf-8"
        ".js" -> "text/javascript; charset=utf-8"
        ".css" -> "text/css; charset=utf-8"
        ".svg" -> "image/svg+xml"
        _ -> "application/octet-stream"

failure :: HTTP.Status -> Text -> Response
failure status err = json status (object ["error" .= err])

methodNotAllowed :: [ByteString.ByteString] -> Response
methodNotAllowed allowed =
    Wai.responseLBS
        HTTP.status405
        [(HTTP.hContentType, "application/json"), ("Allow", Char8.intercalate ", " allowed)]
        (encode (object ["error" .= ("method not allowed" :: Text)]))
