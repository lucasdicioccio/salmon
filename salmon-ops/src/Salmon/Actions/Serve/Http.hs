{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    that reads the event stream (milestone 4) from there instead.

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
story, and there is no TCP here (see the spec's security section).

= Sequence numbers

'Sequence' is the counter an @async@ command's number is drawn from,
kept on the 'Server' ('serverSequence') so that the event stream, when it
lands, draws every report's number from the same counter under the
reporter lock the concurrent driver already serialises through. One
counter for enqueues and reports gives a client one cursor: everything
that happened after its command was queued is everything numbered after
the number it was handed.
-}
module Salmon.Actions.Serve.Http (
    -- * Serving
    Server,
    serverPath,
    withHttpServer,

    -- * Plugging into the loop
    serverObserver,
    serverProducer,
    serverReporters,

    -- * Sequence numbers
    Sequence,
    newSequence,
    nextSequence,
    serverSequence,

    -- * The read model
    WorldView (..),
    viewWorld,
    dagValue,
    application,
) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTChan, writeTVar)
import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, Request, Response)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), Declaration, EpochId, Line (..), NodeState (..), Origin (..), Producer (..), World (..))
import qualified Salmon.Actions.Serve.Socket as Socket
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..))
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)
import Salmon.Op.Status (Direction (..))
import Salmon.Reporter
import Salmon.Reporter.Tagged (Tagged (..), nodeStatePairs, refValue)

-------------------------------------------------------------------------------

{- | A per-loop counter. An @async@ command is numbered from it at enqueue;
the event stream (milestone 4) numbers every report from it too, under the
reporter lock, so the two share one order.
-}
newtype Sequence = Sequence (IORef Word64)

newSequence :: IO Sequence
newSequence = Sequence <$> newIORef 0

-- | Take the next number.
nextSequence :: Sequence -> IO Word64
nextSequence (Sequence counter) = atomicModifyIORef' counter (\n -> (n + 1, n))

-------------------------------------------------------------------------------

-- | A bound unix socket with warp accepting on it, and the requests in flight.
data Server = Server
    { serverListener :: Socket.Listener
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
    , serverSequence :: Sequence
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

serverPath :: Server -> FilePath
serverPath = Socket.listenerPath . serverListener

{- | Bind the socket at the path (owner-only, see 'Socket.withUnixListener'),
serve HTTP on it for as long as the action runs, and take it down after.

The action normally runs the loop, with this server's producer, observer
and reporters plugged in; the server outlives the loop only for as long as
it takes the action to return, and a request still waiting at that point
is answered with the reports it collected.
-}
withHttpServer :: FilePath -> Text -> IO Serve.Mode -> (Server -> IO a) -> IO a
withHttpServer path seedHelp mode act =
    Socket.withUnixListener path $ \listener -> do
        server <-
            Server listener seedHelp
                <$> newTVarIO Nothing
                <*> newTVarIO Nothing
                <*> newTVarIO Map.empty
                <*> newIORef 0
                <*> newSequence
                <*> pure mode
                <*> newTVarIO False
        let settings = Warp.setServerName "salmon" Warp.defaultSettings
        withAsync (Warp.runSettingsSocket settings (Socket.listenerSocket listener) (application server)) $ \_ ->
            act server `finally` atomically (writeTVar (serverStopped server) True)

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

{- | Wrap the loop's reporters: every report goes on unchanged, and one
stamped with a waiting request's origin is collected for that request's
response. The loop's 'Serve.HungUp' for such an origin releases the
request — it is the loop saying every line typed under that origin has
been handled, so the response is complete.
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
        forM_ origin (collect (FromServe rep))
        case rep of
            Serve.HungUp gone -> release gone
            _ -> pure ()

    updownR' :: Reporter (Attributed (UpDown.Report Extension))
    updownR' = ReporterM $ \a@(Attributed origin rep) -> do
        runReporter updownR a
        forM_ origin (collect (FromUpDown rep))

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
    , viewNodes :: Map Ref NodeState
    , viewPaths :: Map Ref [Text]
    , viewHistory :: [(EpochId, Declaration, Bool, Origin, [String])]
    , viewElided :: Int
    }

viewWorld :: World seed directive -> WorldView
viewWorld w =
    WorldView
        { viewDag = Serve.worldDag w
        , viewNodes = w.worldNodes
        , viewPaths = Serve.worldPaths w
        , viewHistory = Serve.historyLinesMatching (const True) w
        , viewElided = w.worldLogDropped
        }

{- | @\/dag@: the nodes in 'Dag.dagOrder', each the 'Act' projection — the
fields 'Dag.sameRepresentative' compares (shorthand, help, notes, the
rendering of dynamics) and the loop's state for the node, as @status@
lists it — plus its dependencies and dependants as refs. Structurally what
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
        ("GET", ["dag"]) -> withView $ \v -> do
            mode <- serverMode server
            respond (json HTTP.status200 (dagValue mode v))
        ("GET", ["status"]) -> withView $ \v -> do
            mode <- serverMode server
            respond (json HTTP.status200 (toJSON (FromServe (Serve.StatusReport mode (Map.toList (viewNodes v)) (viewPaths v)))))
        ("GET", ["history"]) -> withView $ \v ->
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
        (_, ["dag"]) -> respond (methodNotAllowed ["GET"])
        (_, ["status"]) -> respond (methodNotAllowed ["GET"])
        (_, ["history"]) -> respond (methodNotAllowed ["GET"])
        (_, ["help", "seed"]) -> respond (methodNotAllowed ["GET"])
        (_, ["command"]) -> respond (methodNotAllowed ["POST"])
        _ -> respond (failure HTTP.status404 "no such resource")
  where
    withView :: (WorldView -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
    withView k = do
        mread <- readTVarIO (serverView server)
        case mread of
            Nothing -> respond (failure HTTP.status503 "the loop has not started")
            Just readView -> readView >>= k

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
                        let origin = Origin (Text.pack (serverPath server <> "#" <> show n))
                        seqNo <- nextSequence (serverSequence server)
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

failure :: HTTP.Status -> Text -> Response
failure status err = json status (object ["error" .= err])

methodNotAllowed :: [ByteString.ByteString] -> Response
methodNotAllowed allowed =
    Wai.responseLBS
        HTTP.status405
        [(HTTP.hContentType, "application/json"), ("Allow", Char8.intercalate ", " allowed)]
        (encode (object ["error" .= ("method not allowed" :: Text)]))
