{-# LANGUAGE DeriveGeneric #-}

{- | Coverage for "Salmon.Actions.Serve.Events" and @GET \/events@ in
"Salmon.Actions.Serve.Http" (milestone 4 of @specs\/generic-server.md@).

Layer 0 on the record itself: the ring's replay and gap arithmetic, the
gap event's golden JSON, and the shape of an event object. Layer 1 over a
real unix socket and a real @http-client@ reading a @text\/event-stream@
response chunk by chunk: a client that disconnects at a seeded random point
in a pass and comes back with @?since=@ sees, end to end, exactly what a
client that never left saw; sequence numbers are strictly increasing across
the @serve@, @updown@ and @upkeep@ streams with supervision on and a node
whose check keeps failing, so that machine threads are reporting beside the
loop; a ring too small for what happened answers a resumption with a @gap@
first; an @?async@ command's number is the cursor its reports follow; the
@seq@ on @\/status@ and @\/dag@ is the cursor from which nothing after the
snapshot is missed; the filters narrow; and a client hanging up drops its
subscription.
-}
module Test.ServeEventsSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (try)
import Control.Monad (forM, forM_, when)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, eitherDecodeStrict)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (makeConnection)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.Posix.IO (FdOption (CloseOnExec), createPipe, fdToHandle, setFdOption)
import System.Posix.Types (Fd (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Text.Read (readMaybe)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), World)
import qualified Salmon.Actions.Serve.Events as Events
import qualified Salmon.Actions.Serve.Http as Http
import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Track', check, deps, down, help, nodeps, op, ref, up)
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (contramap, runReporter)
import qualified Salmon.Reporter.Tagged as Tagged

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Serve.Events"
        [ testGroup
            "the record"
            [ testCase "replay is everything after since that the ring still holds; a gap when it does not" ringArithmetic
            , testCase "golden JSON for the gap event" gapGolden
            , testCase "an event is the Tagged object plus seq, plus origin when it belongs to a command" eventShape
            ]
        , testGroup
            "GET /events"
            [ testCase "a client that reconnects mid-pass misses nothing" reconnectMissesNothing
            , testCase "sequence numbers are strictly increasing across serve, updown and upkeep" strictlyIncreasingAcrossStreams
            , testCase "a ring that no longer reaches since answers with a gap first" ringOverflowGap
            , testCase "?async then ?since= sees that command's reports" asyncThenSince
            , testCase "/status and /dag carry seq, and ?since= that seq misses nothing after" snapshotSeq
            , testCase "?stream= and ?origin= narrow the stream" filters
            , testCase "an idle stream is kept alive, and a client hanging up drops its subscription" keepAliveAndCleanup
            ]
        ]

-------------------------------------------------------------------------------
-- Layer 0: the record

anOrigin :: Serve.Origin
anOrigin = Serve.Origin "test#1"

ringArithmetic :: IO ()
ringArithmetic = do
    ev <- Events.newEvents Events.defaultConfig{Events.configRing = 3}
    forM_ [1 :: Int .. 5] $ \i -> Events.publish ev Nothing (Events.Enqueued ("line " <> show i))
    lastSeq <- Events.lastSequence ev
    assertEqual "five numbers handed out, from 1" 5 lastSeq
    let subscribe since = Events.withSubscription ev since $ \sub ->
            pure (Events.subscriptionGap sub, fmap Events.eventSeq (Events.subscriptionReplay sub))
    assertEqual "live only: nothing to replay, no gap" (Nothing, []) =<< subscribe Nothing
    assertEqual "from 0: 1 and 2 fell off, so a gap from 3" (Just 3, [3, 4, 5]) =<< subscribe (Just 0)
    assertEqual "from 1: 2 fell off, a gap" (Just 3, [3, 4, 5]) =<< subscribe (Just 1)
    assertEqual "from 2: the next event is the oldest kept, no gap" (Nothing, [3, 4, 5]) =<< subscribe (Just 2)
    assertEqual "from 4: the last one" (Nothing, [5]) =<< subscribe (Just 4)
    assertEqual "from 5: caught up" (Nothing, []) =<< subscribe (Just 5)
    assertEqual "from beyond: nothing, and no gap either" (Nothing, []) =<< subscribe (Just 9)
    -- the live feed starts exactly after the replay
    Events.withSubscription ev (Just 4) $ \sub -> do
        n <- Events.publish ev Nothing (Events.Enqueued "line 6")
        e <- atomicallyNext sub
        assertEqual "the first live event is the one published after subscribing" n (Events.eventSeq e)
    assertEqual "no subscriber left" 0 =<< Events.subscribers ev
  where
    atomicallyNext sub = do
        r <- timeout (5 * 1000000) (STM.atomically (Events.subscriptionLive sub))
        maybe (assertFailure "no live event") pure r

gapGolden :: IO ()
gapGolden = do
    let expected = "{\"kind\":\"gap\",\"from\":42,\"stream\":\"server\"}"
    expectedValue <- either (assertFailure . ("golden is not JSON: " <>)) pure (eitherDecode expected)
    assertEqual "the gap event" (expectedValue :: Value) (Events.gapValue 42)
    -- and on the wire it is one data line with no id, so a client resumes
    -- from the last real number
    let rendered = Builder.toLazyByteString (Events.renderGap 42)
    assertEqual "one data line, then the blank line" (Just ("data: ", "\n\n")) (stripAround rendered)
    assertEqual "carrying the object" (Right expectedValue) (eitherDecode (LChar8.drop 6 (LChar8.dropEnd 2 rendered)))
  where
    stripAround bs
        | LChar8.length bs > 8 = Just (LChar8.take 6 bs, LChar8.takeEnd 2 bs)
        | otherwise = Nothing

eventShape :: IO ()
eventShape = do
    let reported = Events.Event 7 (Just anOrigin) (Events.Reported (Tagged.FromServe Serve.Started))
        tending = Events.Event 8 Nothing (Events.Reported (Tagged.FromUpkeep (Upkeep.Retired 2)))
        queued = Events.Event 9 (Just anOrigin) (Events.Enqueued "up n1")
    assertEqual "a report keeps its stream and kind, and gains seq and origin"
        (Just ("serve", "started", Just 7, Just "test#1"))
        (shape (Events.eventValue reported))
    assertEqual "a tending report is on the upkeep stream with no origin"
        (Just ("upkeep", "retired", Just 8, Nothing))
        (shape (Events.eventValue tending))
    assertEqual "an enqueued command is the server's own"
        (Just ("server", "enqueued", Just 9, Just "test#1"))
        (shape (Events.eventValue queued))
    assertEqual "with the line" (Just "up n1") (textAt ["line"] (Events.eventValue queued))
    -- and a Tended report is unwrapped by the reporter
    ev <- Events.newEvents Events.defaultConfig
    runReporter (Events.eventsReporter ev) (Attributed Nothing (Tagged.FromServe (Serve.Tended (Upkeep.Holding 1))))
    Events.withSubscription ev (Just 0) $ \sub ->
        case Events.subscriptionReplay sub of
            [e] -> assertEqual "unwrapped" (Just ("upkeep", "holding", Just 1, Nothing)) (shape (Events.eventValue e))
            es -> assertFailure ("one event expected: " <> show es)
  where
    shape v = do
        stream <- textAt ["stream"] v
        kind <- textAt ["kind"] v
        pure (stream, kind, seqOf v, textAt ["origin", "name"] v)

-------------------------------------------------------------------------------
-- the thing served: counters per node name, and one node that is never satisfied

newtype Spec = Spec {specNames :: [String]}
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: [String] -> Either Text Spec
parseSpec [] = Left "expected at least one node name"
parseSpec args = Right (Spec args)

-- | The node whose @check@ always says its effect is gone, so its machine
-- keeps acting under supervision and reports from its own thread.
flakyName :: String
flakyName = "flaky"

spyProgram :: IORef (Map String Int) -> Track' Spec
spyProgram upsRef = Track $ \spec ->
    op "events-root" (deps (fmap nodeOp spec.specNames)) $ \actions ->
        actions{ref = mkRef "events-root" spec.specNames, help = "the root of " <> Text.pack (unwords spec.specNames)}
  where
    nodeOp name =
        op "events-node" nodeps $ \actions ->
            actions
                { ref = mkRef "events-node" name
                , help = "node " <> Text.pack name
                , up = bump name
                , down = pure ()
                , check = if name == flakyName then pure (UpDown.Failure "never satisfied") else actions.check
                }
    bump name = atomicModifyIORef' upsRef (\m -> (Map.insertWith (+) name 1 m, ()))

-------------------------------------------------------------------------------
-- a running loop with an HTTP server

data Running = Running
    { runningStdin :: Handle
    , runningWorld :: MVar (World Spec Spec)
    , runningServer :: Http.Server
    , runningManager :: HTTP.Manager
    , runningUps :: IORef (Map String Int)
    }

withRunning :: (Running -> IO a) -> IO a
withRunning = withRunningWith Events.defaultConfig

{- | Every file descriptor a test here opens is marked close-on-exec, and
the reason is worth spelling out: the suite runs its groups in parallel in
one process, some of them spawn processes, and a child spawned while this
test is running inherits every descriptor not so marked (nothing in the
tree passes @close_fds@). A child holding a copy of the loop's stdin pipe
keeps the loop from ever reading end of input, and one holding a copy of a
client socket keeps the server's writes succeeding after the client hung
up — each of which is a ten-second wait for something that is never going
to happen. @network@'s 'Socket.socket' sets @SOCK_NONBLOCK@ but not
@SOCK_CLOEXEC@ (its 'Socket.accept' does), and @process@'s pipe is plain.
-}
withRunningWith :: Events.Config -> (Running -> IO a) -> IO a
withRunningWith cfg act =
    withTempDir $ \dir -> do
        let path = dir </> "events.http"
        (stdinR, stdinW) <- privatePipe
        worldVar <- newEmptyMVar
        (own, _) <- capture
        upsRef <- newIORef Map.empty
        Http.withHttpServerWith cfg path "usage: config NAME...\n" (pure Serve.Interactive) $ \server -> do
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
            manager <- unixManager path
            r <- act (Running stdinW worldVar server manager upsRef)
            _ <- try (hClose stdinW) :: IO (Either IOError ())
            ended <- timeout (10 * 1000000) (takeMVar worldVar)
            when (isNothing ended) (assertFailure "the loop did not end")
            pure r

-- | A pipe neither end of which a child process may inherit; see 'withRunningWith'.
privatePipe :: IO (Handle, Handle)
privatePipe = do
    (r, w) <- createPipe
    forM_ [r, w] $ \fd -> setFdOption fd CloseOnExec True
    (,) <$> fdToHandle r <*> fdToHandle w

unixManager :: FilePath -> IO HTTP.Manager
unixManager path =
    HTTP.newManager
        HTTP.defaultManagerSettings
            { HTTP.managerRawConnection = pure $ \_ _ _ -> do
                sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
                Socket.withFdSocket sock $ \fd -> setFdOption (Fd fd) CloseOnExec True
                Socket.connect sock (Socket.SockAddrUnix path)
                makeConnection (SocketBS.recv sock 4096) (SocketBS.sendAll sock) (Socket.close sock)
            }

get :: Running -> String -> IO (Int, Value)
get running route = do
    req <- HTTP.parseRequest ("http://salmon" <> route)
    exchange running req

post :: Running -> String -> String -> IO (Int, Value)
post running route line = do
    req0 <- HTTP.parseRequest ("http://salmon" <> route)
    let req =
            req0
                { HTTP.method = "POST"
                , HTTP.requestHeaders = [(HTTP.hContentType, "text/plain")]
                , HTTP.requestBody = HTTP.RequestBodyLBS (LChar8.pack line)
                }
    exchange running req

exchange :: Running -> HTTP.Request -> IO (Int, Value)
exchange running req = do
    r <- timeout (10 * 1000000) (HTTP.httpLbs req (runningManager running))
    case r of
        Nothing -> assertFailure ("no answer within 10s to " <> show (HTTP.path req))
        Just resp ->
            case eitherDecode (HTTP.responseBody resp) of
                Left err -> assertFailure ("not JSON: " <> err <> ": " <> LChar8.unpack (HTTP.responseBody resp))
                Right v -> pure (HTTP.statusCode (HTTP.responseStatus resp), v)

-- | A synchronous command: the kinds of the reports it answered with.
sync :: Running -> String -> IO [Text]
sync running line = do
    (code, v) <- post running "/command" line
    assertEqual ("status of sync " <> line) 200 code
    pure (fmap kindOf (arrayOf v))

-- | An asynchronous command: its number and the origin it was queued under.
async :: Running -> String -> IO (Word64, Text)
async running line = do
    (code, v) <- post running "/command?async" line
    assertEqual ("status of async " <> line) 202 code
    case (seqOf v, textAt ["origin"] v) of
        (Just n, Just origin) -> pure (n, origin)
        _ -> assertFailure ("async answer has no seq/origin: " <> show v)

-------------------------------------------------------------------------------
-- an SSE client

-- | One event as the stream carried it: the @id@ line, and the @data@ object.
data Sse = Sse
    { sseId :: Maybe Word64
    , sseData :: Value
    }
    deriving (Eq, Show)

-- | An open stream: the next event (blocking, 10s at most), and how many
-- comment lines have gone by.
data Stream = Stream
    { streamNext :: IO Sse
    , streamComments :: IO Int
    }

{- | Open @\/events@ with a query string, hand the stream to the action, and
close the connection when it returns — which is how a client hangs up.
-}
withEvents :: Running -> String -> (Stream -> IO a) -> IO a
withEvents running query act = do
    req <- HTTP.parseRequest ("http://salmon/events" <> query)
    HTTP.withResponse req (runningManager running) $ \resp -> do
        assertEqual ("status of /events" <> query) 200 (HTTP.statusCode (HTTP.responseStatus resp))
        assertEqual "content type" (Just "text/event-stream") (lookup HTTP.hContentType (HTTP.responseHeaders resp))
        buf <- newIORef ByteString.empty
        pending <- newIORef []
        comments <- newIORef (0 :: Int)
        let next = do
                ps <- readIORef pending
                case ps of
                    (e : es) -> writeIORef pending es >> pure e
                    [] -> do
                        chunk <- timeout (10 * 1000000) (HTTP.brRead (HTTP.responseBody resp))
                        case chunk of
                            Nothing -> assertFailure ("no event within 10s on /events" <> query)
                            Just c | ByteString.null c -> assertFailure ("/events" <> query <> " ended")
                            Just c -> do
                                b <- readIORef buf
                                let (blocks, rest) = splitBlocks (b <> c)
                                writeIORef buf rest
                                parsed <- forM blocks parseBlock
                                let (cs, es) = (length (filter isNothing parsed), mapMaybe id parsed)
                                modifyIORef' comments (+ cs)
                                writeIORef pending es
                                next
        act (Stream next (readIORef comments))
  where
    -- complete blocks (ended by a blank line) and whatever is left
    splitBlocks :: ByteString.ByteString -> ([ByteString.ByteString], ByteString.ByteString)
    splitBlocks bs =
        case ByteString.breakSubstring "\n\n" bs of
            (block, rest)
                | ByteString.null rest -> ([], bs)
                | otherwise ->
                    let (more, left) = splitBlocks (ByteString.drop 2 rest)
                     in (block : more, left)
    -- Nothing for a comment block
    parseBlock :: ByteString.ByteString -> IO (Maybe Sse)
    parseBlock block = do
        let ls = Char8.lines block
            fieldOf name = [ByteString.drop (ByteString.length name) l | l <- ls, name `ByteString.isPrefixOf` l]
        if all (": " `ByteString.isPrefixOf`) ls
            then pure Nothing
            else case fieldOf "data: " of
                [raw] -> case eitherDecodeStrict raw of
                    Left err -> assertFailure ("event data is not JSON: " <> err <> ": " <> Char8.unpack raw)
                    Right v ->
                        pure (Just (Sse (readMaybe . Char8.unpack =<< headMay (fieldOf "id: ")) v))
                _ -> assertFailure ("not one data line: " <> Char8.unpack block)
    headMay (x : _) = Just x
    headMay [] = Nothing

-- | Read until an event satisfies the predicate; that event is included.
readUntil :: (Sse -> Bool) -> Stream -> IO [Sse]
readUntil done stream = go []
  where
    go acc = do
        e <- streamNext stream
        if done e then pure (reverse (e : acc)) else go (e : acc)

-- | Read at most @n@ events, stopping early at one satisfying the predicate.
readUpTo :: Int -> (Sse -> Bool) -> Stream -> IO [Sse]
readUpTo n done stream = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc = do
        e <- streamNext stream
        if done e then pure (reverse (e : acc)) else go (k - 1) (e : acc)

-- | The loop's @hung-up@ for an origin: the last thing it says about a command.
hungUpFrom :: Text -> Sse -> Bool
hungUpFrom origin e = kindOf (sseData e) == "hung-up" && textAt ["from"] (sseData e) == Just origin

-------------------------------------------------------------------------------
-- reading the JSON

kindOf :: Value -> Text
kindOf = maybe "<no kind>" id . textAt ["kind"]

textAt :: [Text] -> Value -> Maybe Text
textAt [] (String t) = Just t
textAt (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= textAt ks
textAt _ _ = Nothing

seqOf :: Value -> Maybe Word64
seqOf = numberAt "seq"

numberAt :: Text -> Value -> Maybe Word64
numberAt k (Object o) = case KeyMap.lookup (Key.fromText k) o of
    Just (Number n) -> Just (truncate n)
    _ -> Nothing
numberAt _ _ = Nothing

field :: Text -> Value -> Maybe Value
field k (Object o) = KeyMap.lookup (Key.fromText k) o
field _ _ = Nothing

arrayOf :: Value -> [Value]
arrayOf (Array xs) = toList xs
arrayOf _ = []

streamOf :: Sse -> Maybe Text
streamOf = textAt ["stream"] . sseData

originOf :: Sse -> Maybe Text
originOf = textAt ["origin", "name"] . sseData

-- | Every id present, and strictly increasing, and equal to the object's seq.
assertNumbered :: String -> [Sse] -> IO ()
assertNumbered label es = do
    ids <- forM es $ \e -> case sseId e of
        Nothing -> assertFailure (label <> ": an event without an id: " <> show e)
        Just n -> do
            assertEqual (label <> ": id and seq agree") (Just n) (seqOf (sseData e))
            pure n
    assertBool (label <> ": strictly increasing: " <> show ids) (and (zipWith (<) ids (drop 1 ids)))

-------------------------------------------------------------------------------
-- a seed for the random cut, printed so a failure can be replayed

-- | @SALMON_EVENTS_SEED@ if set, else the clock; printed either way.
pickSeed :: IO Word64
pickSeed = do
    env <- lookupEnv "SALMON_EVENTS_SEED"
    s <- case env >>= readMaybe of
        Just n -> pure n
        Nothing -> truncate . (* 1000) <$> getPOSIXTime
    putStrLn ("  ServeEventsSpec seed: " <> show s <> " (SALMON_EVENTS_SEED to replay)")
    pure s

-- | A step of a 64-bit LCG (Knuth's constants).
lcg :: Word64 -> Word64
lcg s = s * 6364136223846793005 + 1442695040888963407

-------------------------------------------------------------------------------
-- Layer 1

reconnectMissesNothing :: IO ()
reconnectMissesNothing = do
    seed <- pickSeed
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        -- two clients attach before anything happens; A never leaves
        withEvents running "?since=0" $ \streamA -> do
            (bs1, marker) <- withEvents running "?since=0" $ \streamB -> do
                forM_ script (async running)
                (_, marker) <- async running "history"
                -- B reads a random prefix of the pass, then hangs up
                let cut = fromIntegral (lcg seed `mod` 60)
                bs1 <- readUpTo cut (hungUpFrom marker) streamB
                pure (bs1, marker)
            let sawMarker = any (hungUpFrom marker) bs1
                lastSeen = case reverse (mapMaybe sseId bs1) of
                    (n : _) -> n
                    [] -> 0
            bs2 <-
                if sawMarker
                    then pure []
                    else withEvents running ("?since=" <> show lastSeen) (readUntil (hungUpFrom marker))
            as <- readUntil (hungUpFrom marker) streamA
            assertNumbered ("seed " <> show seed <> ", A") as
            assertBool "no gap event on the way back" (all (\e -> kindOf (sseData e) /= "gap") bs2)
            assertEqual ("seed " <> show seed <> ": B's two halves are A's stream") as (bs1 ++ bs2)
            assertBool "the pass was actually observed" (any (\e -> kindOf (sseData e) == "done") as)
  where
    script = ["up n1 n2", "up n2 n3", "down n1 n2", "up n4 n5 n6", "only n7"]

strictlyIncreasingAcrossStreams :: IO ()
strictlyIncreasingAcrossStreams =
    withRunning $ \running -> do
        _ <- sync running ("up " <> flakyName <> " n1")
        -- the loop is now idle, so the machines are tending; the flaky
        -- node's check keeps failing, so its machine keeps re-applying
        -- it from its own thread while the supervisor reports around it
        -- read until the machines have been seen at work: the supervisor's
        -- own stream, and a node report with no origin, which only a
        -- machine emits (a pass's are stamped with the command's)
        es <- withEvents running "?since=0" $ \stream ->
            let go acc seen
                    | Set.fromList ["serve", "upkeep", "machine"] `Set.isSubsetOf` seen = pure (reverse acc)
                    | otherwise = do
                        e <- streamNext stream
                        let tag = case (streamOf e, originOf e, kindOf (sseData e)) of
                                (Just "updown", Nothing, "done") -> Just "machine"
                                (st, _, _) -> st
                        go (e : acc) (maybe seen (`Set.insert` seen) tag)
             in go [] Set.empty
        assertNumbered "across streams" es
        let streams = Set.fromList (mapMaybe streamOf es)
        assertBool ("all three streams seen: " <> show streams) (Set.fromList ["serve", "updown", "upkeep"] `Set.isSubsetOf` streams)
        -- what the machines report has no origin; what the command did has
        assertBool "machine reports carry no origin" (all (isNothing . originOf) [e | e <- es, streamOf e == Just "upkeep"])
        assertBool "the command's reports carry its origin" (any (isJust . originOf) [e | e <- es, kindOf (sseData e) == "declared"])
        ups <- readIORef (runningUps running)
        assertBool "the flaky node was re-applied by its machine" (Map.findWithDefault 0 flakyName ups >= 2)

ringOverflowGap :: IO ()
ringOverflowGap =
    withRunningWith Events.defaultConfig{Events.configRing = 8} $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up n1 n2"
        _ <- sync running "up n3"
        (_, marker) <- async running "history"
        lastSeq <- Events.lastSequence (Http.serverEvents (runningServer running))
        assertBool "more happened than the ring holds" (lastSeq > 8)
        es <- withEvents running "?since=0" (readUntil (hungUpFrom marker))
        case es of
            (gap : rest) -> do
                assertEqual "the first event is the gap" "gap" (kindOf (sseData gap))
                assertEqual "with no id" Nothing (sseId gap)
                assertEqual "stream server" (Just "server") (streamOf gap)
                let from = numberAt "from" (sseData gap)
                assertEqual "from is the oldest event kept" from (sseId =<< headMay rest)
                assertEqual "which is the ring's size back from the end" (Just (lastSeq - 8 + 1)) from
                assertNumbered "after the gap" rest
                assertEqual "contiguous to the end" [lastSeq - 8 + 1 .. lastSeq] (mapMaybe sseId rest)
            [] -> assertFailure "no events"
        -- resuming from inside the ring: no gap
        es' <- withEvents running ("?since=" <> show (lastSeq - 2)) (readUntil (hungUpFrom marker))
        assertEqual "two events, no gap" [lastSeq - 1, lastSeq] (mapMaybe sseId es')
  where
    headMay (x : _) = Just x
    headMay [] = Nothing

asyncThenSince :: IO ()
asyncThenSince =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        (n, origin) <- async running "up a1 a2"
        es <- withEvents running ("?since=" <> show n) (readUntil (hungUpFrom origin))
        assertNumbered "after the enqueue" es
        assertBool "everything is numbered after the enqueue" (all (maybe False (> n)) (fmap sseId es))
        let mine = [kindOf (sseData e) | e <- es, originOf e == Just origin]
        forM_ ["declared", "converge-start", "converge-stop"] $ \k ->
            assertBool (Text.unpack k <> " is among the command's reports: " <> show mine) (k `elem` mine)
        assertBool "node reports are stamped too" ("done" `elem` mine)
        -- and the enqueued event itself is the number handed back
        withEvents running ("?since=" <> show (n - 1)) $ \stream -> do
            e <- streamNext stream
            assertEqual "the enqueue is event n" (Just n) (sseId e)
            assertEqual "kind" "enqueued" (kindOf (sseData e))
            assertEqual "line" (Just "up a1 a2") (textAt ["line"] (sseData e))
            assertEqual "origin" (Just origin) (originOf e)

snapshotSeq :: IO ()
snapshotSeq =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up s1"
        (_, st) <- get running "/status"
        (_, dag) <- get running "/dag"
        s <- maybe (assertFailure "no seq on /status") pure (seqOf st)
        assertEqual "/dag carries the same cursor, nothing having happened in between" (Just s) (seqOf dag)
        lastSeq <- Events.lastSequence (Http.serverEvents (runningServer running))
        assertEqual "the cursor is the last number handed out" lastSeq s
        -- something happens after the snapshot
        _ <- sync running "up s2"
        (_, marker) <- async running "history"
        es <- withEvents running ("?since=" <> show s) (readUntil (hungUpFrom marker))
        assertNumbered "after the snapshot" es
        assertEqual "the first event after the snapshot is the very next number" (Just (s + 1)) (sseId =<< headMay es)
        assertEqual "which is the command typed after it" (Just "up s2") (textAt ["line"] . sseData =<< headMay es)
        assertBool "and its pass is there" (any (\e -> kindOf (sseData e) == "converge-stop") es)
  where
    headMay (x : _) = Just x
    headMay [] = Nothing

filters :: IO ()
filters =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up f1"
        (_, origin) <- async running "up f2"
        _ <- sync running "status"
        (_, marker) <- async running "history"
        -- by stream
        serveOnly <- withEvents running "?since=0&stream=serve" (readUntil (hungUpFrom marker))
        assertBool "only the serve stream" (all ((== Just "serve") . streamOf) serveOnly)
        assertBool "and it is not empty" (not (null serveOnly))
        twoStreams <- withEvents running "?since=0&stream=updown,server" (readUntil (\e -> kindOf (sseData e) == "enqueued" && textAt ["line"] (sseData e) == Just "history"))
        let seen = Set.fromList (mapMaybe streamOf twoStreams)
        assertEqual "exactly the two asked for" (Set.fromList ["updown", "server"]) seen
        -- by origin: the last thing said under an origin is its converge-stop
        -- (the origin names the socket path and a `#`, so it is escaped)
        mine <- withEvents running ("?since=0&origin=" <> Char8.unpack (HTTP.urlEncode True (Text.encodeUtf8 origin))) (readUntil (\e -> kindOf (sseData e) == "converge-stop"))
        assertBool "only that origin" (all ((== Just origin) . originOf) mine)
        assertBool "the enqueue, the declaration and the pass" (all (`elem` fmap (kindOf . sseData) mine) ["enqueued", "declared", "converge-stop"])
        -- a bad cursor is refused
        req <- HTTP.parseRequest "http://salmon/events?since=soon"
        resp <- HTTP.httpLbs req (runningManager running)
        assertEqual "since must be a number" 400 (HTTP.statusCode (HTTP.responseStatus resp))

keepAliveAndCleanup :: IO ()
keepAliveAndCleanup =
    withRunningWith Events.defaultConfig{Events.configKeepAlive = 100 * 1000} $ \running -> do
        _ <- sync running "supervise off"
        let ev = Http.serverEvents (runningServer running)
        withEvents running "" $ \stream -> do
            assertEqual "one subscriber" 1 =<< Events.subscribers ev
            -- nothing happens; the stream is kept alive with comments
            threadDelay (500 * 1000)
            (_, marker) <- async running "history"
            _ <- readUntil (hungUpFrom marker) stream
            n <- streamComments stream
            assertBool ("keep-alive comments arrived while idle: " <> show n) (n >= 2)
        -- the client hung up: the next keep-alive write fails and the
        -- subscription is dropped
        let waitGone k = do
                left <- Events.subscribers ev
                if left == 0
                    then pure ()
                    else
                        if k <= (0 :: Int)
                            then assertFailure ("subscription not dropped after hang-up: " <> show left)
                            else threadDelay (100 * 1000) >> waitGone (k - 1)
        waitGone 100
