{-# LANGUAGE DeriveGeneric #-}

{- | Coverage for "Salmon.Client.Model" and "Salmon.Client.Http" (milestone
6 of @specs\/generic-server.md@, the client's half).

Layer 0 on the model: a @\/dag@ answer folded over a recorded event
sequence — @declared@, @converge-start@, @eval@, @done@, @failed@,
@blocked@, @converge-stop@, @parked@, @next-look@, @hung-up@, @gap@ —
gives the per-node view a terminal would show; folding an event twice is
folding it once; an @acted@ is unwrapped to the pass's vocabulary; a node
wanted down is dropped by its @done@; the SSE block parser reads what
"Salmon.Actions.Serve.Events" renders, comments included; and the rendered
rows and header are the text expected.

Layer 1 over a real @withHttpServer@ on a temp socket, driven through the
client itself: @dag@, then @commandAsync "up ..."@, then @events@ from the
number it answered, re-reading @\/dag@ when the model asks (the @declared@
event), sees the pass and the model converges.
-}
module Test.ClientModelSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.Posix.IO (FdOption (CloseOnExec), createPipe, fdToHandle, setFdOption)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), World)
import qualified Salmon.Actions.Serve.Events as Events
import qualified Salmon.Actions.Serve.Http as Http
import Salmon.Builtin.Extension (Track', deps, down, help, nodeps, op, ref, up)
import qualified Salmon.Client.Http as Client
import qualified Salmon.Client.Model as Model
import Salmon.Client.Model (Check (..), Model (..), Node (..), Pass (..), RefId (..))
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (contramap)
import qualified Salmon.Reporter.Tagged as Tagged

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Client"
        [ testGroup
            "the model"
            [ testCase "a /dag snapshot folded over a recorded pass gives the per-node view" recordedPass
            , testCase "folding an event twice is folding it once" replayIsIdempotent
            , testCase "an upkeep acted is unwrapped to the pass's vocabulary" actedUnwraps
            , testCase "a node wanted down is dropped by its done" downNodeDropped
            , testCase "the SSE parser reads what the server renders" sseParser
            , testCase "the rendered rows and header" rendering
            ]
        , testGroup
            "the client against a real server"
            [ testCase "dag, then commandAsync, then events from that seq: the model converges" clientConverges
            ]
        ]

-------------------------------------------------------------------------------
-- Layer 0: a recorded sequence

refOf :: Text -> RefId
refOf short = RefId short ("full-" <> short)

refValue :: Text -> Value
refValue short = object ["short" .= short, "full" .= ("full-" <> short)]

nodeValue :: Text -> Text -> [Text] -> Value
nodeValue short shorthand dependencies =
    object
        [ "ref" .= refValue short
        , "shorthand" .= shorthand
        , "help" .= ("help for " <> short)
        , "notes" .= (["a note"] :: [Text])
        , "direction" .= ("up" :: Text)
        , "convergence" .= ("pending" :: Text)
        , "status" .= Null
        , "paths" .= (["/" <> short] :: [Text])
        , "dynamics" .= ([] :: [Text])
        , "dependencies" .= fmap refValue dependencies
        , "dependants" .= ([] :: [Value])
        ]

-- | Three nodes in dependency order: the two leaves, then the root over them.
snapshot :: Value
snapshot = snapshotAt 5

snapshotAt :: Int -> Value
snapshotAt seqNo =
    object
        [ "mode" .= ("interactive" :: Text)
        , "seq" .= seqNo
        , "nodes" .= [nodeValue "n1" "node-one" [], nodeValue "n2" "node-two" [], nodeValue "root" "the-root" ["n1", "n2"]]
        ]

about :: Text -> Text -> Text -> Int -> [(Key.Key, Value)] -> Value
about stream kind short seqNo rest =
    object $
        [ "stream" .= stream
        , "kind" .= kind
        , "seq" .= seqNo
        , "ref" .= refValue short
        , "node" .= object ["shorthand" .= short, "help" .= ("" :: Text), "notes" .= ([] :: [Text])]
        ]
            ++ rest

loop :: Text -> Int -> [(Key.Key, Value)] -> Value
loop kind seqNo rest = object (["stream" .= ("serve" :: Text), "kind" .= kind, "seq" .= seqNo] ++ rest)

-- | The events the stream carries for one @up@ whose second node fails.
recorded :: [Value]
recorded =
    [ loop "declared" 6 ["epoch" .= (0 :: Int), "direction" .= ("up" :: Text), "nodes" .= (3 :: Int), "active_seeds" .= (1 :: Int)]
    , loop "converge-start" 7 ["down" .= (0 :: Int), "up" .= (3 :: Int)]
    , about "updown" "eval" "n1" 8 []
    , about "updown" "done" "n1" 9 []
    , about "updown" "eval" "n2" 10 []
    , about "updown" "failed" "n2" 11 ["error" .= ("boom" :: Text)]
    , about "updown" "blocked" "root" 12 []
    , loop "converge-stop" 13 ["ok" .= False, "remaining" .= (2 :: Int)]
    , about "upkeep" "parked" "n1" 14 []
    , about "upkeep" "next-look" "n1" 15 ["check" .= object ["verdict" .= ("success" :: Text)], "delay_us" .= (500000 :: Int)]
    , loop "hung-up" 16 ["from" .= ("x#0" :: Text)]
    , object ["stream" .= ("server" :: Text), "kind" .= ("gap" :: Text), "from" .= (40 :: Int)]
    ]

fold :: Model -> [Value] -> Model
fold = foldl' (\m v -> Model.step m (Model.eventOf v))

start :: IO Model
start = either (assertFailure . ("snapshot: " <>)) pure (Model.fromDag snapshot)

recordedPass :: IO ()
recordedPass = do
    m0 <- start
    assertEqual "the snapshot's seq" 5 m0.modelSeq
    assertEqual "the snapshot's order" [refOf "n1", refOf "n2", refOf "root"] m0.modelOrder
    assertEqual "mode" "interactive" m0.modelMode
    -- the declaration asks for a re-read, and the fold goes on past it
    let afterDeclared = fold m0 (take 1 recorded)
    assertBool "declared asks for a resync" (Model.modelResync afterDeclared /= Nothing)
    let m = fold (Model.resolve afterDeclared) (drop 1 recorded)
    assertEqual "seq is the highest seen (the gap has none)" 16 m.modelSeq
    assertEqual "the gap asks for a resync" (Just "events 40 fell off the ring") (Model.modelResync m)
    assertEqual "the pass stopped with a failure and two left" (Just (Stopped False 2)) m.modelPass
    let view r = maybe (assertFailure ("no node " <> show r)) pure (Model.lookupNode (refOf r) m)
    n1 <- view "n1"
    assertEqual "n1 converged" "converged" n1.nodeConvergence
    assertEqual "n1's last check is what next-look said" (Just (Check "success" Nothing)) n1.nodeCheck
    assertEqual "n1's last event" (Just "next-look", Just 15) (n1.nodeLastKind, n1.nodeLastSeq)
    assertEqual "n1 keeps no error" Nothing n1.nodeError
    n2 <- view "n2"
    assertEqual "n2 errored" "errored" n2.nodeConvergence
    assertEqual "n2's error" (Just "boom") n2.nodeError
    assertEqual "n2's last event" (Just "failed", Just 11) (n2.nodeLastKind, n2.nodeLastSeq)
    root <- view "root"
    assertEqual "root blocked" "blocked" root.nodeConvergence
    assertEqual "root's last event" (Just "blocked", Just 12) (root.nodeLastKind, root.nodeLastSeq)
    assertEqual "counts" (Model.Counts 1 1 3) (Model.counts m)
    assertEqual "order kept" [refOf "n1", refOf "n2", refOf "root"] (fmap nodeRef (Model.nodesInOrder m))
    assertEqual "the last event folded" (Just "gap") (Model.eventKind <$> m.modelLast)

replayIsIdempotent :: IO ()
replayIsIdempotent = do
    m0 <- start
    let once = fold m0 recorded
        twice = fold once recorded
        -- and every numbered prefix replayed onto the whole leaves it
        -- where it was: those events are at or below its seq, so they
        -- are dropped rather than regressing the pass to `converging`
        numbered = filter ((/= Nothing) . Model.eventSeq . Model.eventOf) recorded
        prefixes = [fold once (take k numbered) | k <- [0 .. length numbered]]
    assertEqual "the whole sequence twice" once twice
    forM_ (zip [0 :: Int ..] prefixes) $ \(k, m) -> assertEqual ("prefix " <> show k <> " replayed") once m
    -- a snapshot taken after a racing event drops that event's replay too
    m9 <- either (assertFailure . ("snapshot: " <>)) pure (Model.fromDag (snapshotAt 9))
    let later = fold m9 recorded
    assertEqual "events above the snapshot's seq land" (Just (Just "failed", Just 11)) ((\n -> (n.nodeLastKind, n.nodeLastSeq)) <$> Model.lookupNode (refOf "n2") later)
    assertEqual "n1's done (9) was not re-applied: the snapshot already had its say, and next-look (15) does not converge" (Just ("pending", Just "next-look")) ((\n -> (n.nodeConvergence, n.nodeLastKind)) <$> Model.lookupNode (refOf "n1") later)
    -- but the loop's part has its own stamp: a fresh snapshot read after
    -- the pass, rebased onto a model that saw it start, still takes the stop
    let started = fold m0 (take 2 recorded)
    assertEqual "converging" (Just (Converging 0 3)) started.modelPass
    m20 <- either (assertFailure . ("snapshot: " <>)) pure (Model.fromDag (snapshotAt 20))
    let rebased = Model.rebase started m20
    assertEqual "the pass carried over" (Just (Converging 0 3)) rebased.modelPass
    assertEqual "the cursor is the snapshot's" 20 rebased.modelSeq
    assertEqual "the resync is answered" Nothing (Model.modelResync rebased)
    let ended = fold rebased (drop 2 recorded)
    assertEqual "the stop (13 < 20) still lands on the loop's part" (Just (Stopped False 2)) ended.modelPass
    assertEqual "while the nodes are the snapshot's (n2's failed (11) is below its stamp)" (Just "pending") (nodeConvergence <$> Model.lookupNode (refOf "n2") ended)

actedUnwraps :: IO ()
actedUnwraps = do
    m0 <- start
    let inner = about "updown" "done" "n2" 0 []
        acted = object ["stream" .= ("upkeep" :: Text), "kind" .= ("acted" :: Text), "seq" .= (20 :: Int), "report" .= inner]
        m = fold m0 [acted]
    n2 <- maybe (assertFailure "no n2") pure (Model.lookupNode (refOf "n2") m)
    assertEqual "converged through the tending machine" "converged" n2.nodeConvergence
    assertEqual "recorded as an acted done, at the outer seq" (Just "acted done", Just 20) (n2.nodeLastKind, n2.nodeLastSeq)

downNodeDropped :: IO ()
downNodeDropped = do
    let retiring =
            object
                [ "mode" .= ("interactive" :: Text)
                , "seq" .= (1 :: Int)
                , "nodes" .= [withDirection "down" (nodeValue "n1" "node-one" []), nodeValue "n2" "node-two" []]
                ]
    m0 <- either (assertFailure . ("snapshot: " <>)) pure (Model.fromDag retiring)
    assertEqual "two nodes" 2 (Model.countTotal (Model.counts m0))
    let m = fold m0 [about "updown" "eval" "n1" 2 [], about "updown" "done" "n1" 3 []]
    assertEqual "n1 is gone once down" Nothing (Model.lookupNode (refOf "n1") m)
    assertEqual "the order follows" [refOf "n2"] m.modelOrder
    -- while a node wanted up that is done stays
    let m' = fold m0 [about "updown" "done" "n2" 4 []]
    assertEqual "n2 stays, converged" (Just "converged") (nodeConvergence <$> Model.lookupNode (refOf "n2") m')
  where
    withDirection d (Object o) = Object (KeyMap.insert "direction" (String d) o)
    withDirection _ v = v

sseParser :: IO ()
sseParser = do
    let e1 = Events.Event 7 Nothing (Events.Enqueued "up n1")
        e2 = Events.Event 8 (Just (Serve.Origin "x#0")) (Events.Reported (Tagged.FromServe Serve.Started))
        wire = LByteString.toStrict (Builder.toLazyByteString (Events.renderGap 3 <> Events.renderEvent e1 <> Events.keepAlive <> Events.renderEvent e2))
        (blocks, rest) = Client.splitBlocks (wire <> "id: 9\ndata: {\"partial")
    assertEqual "the partial block is left over" "id: 9\ndata: {\"partial" rest
    assertEqual
        "gap without id, two events with theirs, and the comment"
        [ Client.SseEvent Nothing (Events.gapValue 3)
        , Client.SseEvent (Just 7) (Events.eventValue e1)
        , Client.SseComment
        , Client.SseEvent (Just 8) (Events.eventValue e2)
        ]
        (concatMap Client.parseBlock blocks)
    -- and what the model makes of them
    let gap = Model.eventOf (Events.gapValue 3)
        queued = Model.eventOf (Events.eventValue e1)
        started = Model.eventOf (Events.eventValue e2)
    assertEqual "gap" (Nothing, "server", "gap", Nothing) (gap.eventSeq, gap.eventStream, gap.eventKind, gap.eventOrigin)
    assertEqual "enqueued" (Just 7, "server", "enqueued", Nothing) (queued.eventSeq, queued.eventStream, queued.eventKind, queued.eventOrigin)
    assertEqual "started, for a command" (Just 8, "serve", "started", Just "x#0") (started.eventSeq, started.eventStream, started.eventKind, started.eventOrigin)

rendering :: IO ()
rendering = do
    m0 <- start
    let m = fold m0 recorded
    assertEqual
        "one row per node, in order"
        [ "n1         node-one               up   converged success      next-look #15"
        , "n2         node-two               up   errored   -            failed #11: boom"
        , "root       the-root               up   blocked   -            blocked #12"
        ]
        (fmap Model.renderNodeRow (Model.nodesInOrder m))
    assertEqual
        "the header"
        "/tmp/x.http mode=interactive seq=16 converged=1 errored=1 total=3 incomplete(2 left)+failure "
        (Model.renderHeader "/tmp/x.http" m)
    assertEqual "an event line about a node" "#11 updown failed n2 n2" (Model.renderEventLine (Model.eventOf (recorded !! 5)))
    assertEqual "an event line about the loop" "#13 serve converge-stop ok=false remaining=2" (Model.renderEventLine (Model.eventOf (recorded !! 7)))

-------------------------------------------------------------------------------
-- Layer 1: the client against a real server

newtype Spec = Spec {specNames :: [String]}
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: [String] -> Either Text Spec
parseSpec [] = Left "expected at least one node name"
parseSpec args = Right (Spec args)

spyProgram :: IORef (Map String Int) -> Track' Spec
spyProgram upsRef = Track $ \spec ->
    op "client-root" (deps (fmap nodeOp spec.specNames)) $ \actions ->
        actions{ref = mkRef "client-root" spec.specNames, help = "the root of " <> Text.pack (unwords spec.specNames)}
  where
    nodeOp name =
        op "client-node" nodeps $ \actions ->
            actions
                { ref = mkRef "client-node" name
                , help = "node " <> Text.pack name
                , up = atomicModifyIORef' upsRef (\m -> (Map.insertWith (+) name 1 m, ()))
                , down = pure ()
                }

data Running = Running
    { runningStdin :: Handle
    , runningWorld :: MVar (World Spec Spec)
    , runningClient :: Client.Client
    , runningUps :: IORef (Map String Int)
    }

-- | The loop with an HTTP server on a temp socket, as 'Test.ServeEventsSpec'
-- starts it (close-on-exec on every descriptor, for the reason given there).
withRunning :: (Running -> IO a) -> IO a
withRunning act =
    withTempDir $ \dir -> do
        let path = dir </> "client.http"
        (stdinR, stdinW) <- privatePipe
        worldVar <- newEmptyMVar
        (own, _) <- capture
        upsRef <- newIORef Map.empty
        Http.withHttpServer path "usage: config NAME...\n" (pure Serve.Interactive) $ \server -> do
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
            client <- Client.newUnixClient path
            r <- act (Running stdinW worldVar client upsRef)
            _ <- try (hClose stdinW) :: IO (Either IOError ())
            ended <- timeout (10 * 1000000) (takeMVar worldVar)
            when (isNothing ended) (assertFailure "the loop did not end")
            pure r

privatePipe :: IO (Handle, Handle)
privatePipe = do
    (r, w) <- createPipe
    forM_ [r, w] $ \fd -> setFdOption fd CloseOnExec True
    (,) <$> fdToHandle r <*> fdToHandle w

clientConverges :: IO ()
clientConverges =
    withRunning $ \running -> do
        let client = runningClient running
        -- the sync command answers with the line's reports
        quiet <- Client.command client "supervise off"
        assertEqual "one report for supervise off" ["supervised"] (fmap (Model.eventKind . Model.eventOf) quiet)
        -- an empty world first
        m0 <- either (assertFailure . ("dag: " <>)) pure . Model.fromDag =<< Client.dag client
        assertEqual "nothing declared yet" 0 (Model.countTotal (Model.counts m0))
        -- queue the declaration, and read the stream from its number
        queued <- Client.commandAsync client "up n1 n2"
        assertBool "queued above the snapshot" (queued.enqueuedSeq > m0.modelSeq)
        modelRef <- newIORef m0
        seen <- newIORef []
        let converged m = Model.countTotal (Model.counts m) == 3 && Model.countConverged (Model.counts m) == 3
            onEvent e = do
                atomicModifyIORef' seen (\es -> (e : es, ()))
                m <- readIORef modelRef
                let m' = Model.step m e
                m'' <- case Model.modelResync m' of
                    -- what a terminal client does on `declared`: re-read
                    -- the snapshot, rebase, and go on folding from it
                    Just _ -> either (assertFailure . ("dag: " <>)) (pure . Model.rebase m') . Model.fromDag =<< Client.dag client
                    Nothing -> pure m'
                writeIORef modelRef m''
                -- stop once the pass has stopped and every node is converged
                pure (not (converged m'' && isStopped m''.modelPass))
        r <- timeout (20 * 1000000) (Client.events client (Just queued.enqueuedSeq) Client.noFilter onEvent)
        assertEqual "the stream was read to the pass's end" (Just ()) r
        m <- readIORef modelRef
        events <- reverse <$> readIORef seen
        let kinds = fmap Model.eventKind events
        assertBool ("the pass was seen: " <> show kinds) (all (`elem` kinds) ["declared", "converge-start", "done", "converge-stop"])
        assertBool "every event is above the enqueue number" (all (maybe False (> queued.enqueuedSeq) . Model.eventSeq) events)
        assertBool "every event carries the command's origin" (all ((== Just queued.enqueuedOrigin) . Model.eventOrigin) events)
        assertEqual "three nodes, all converged" (Model.Counts 3 0 3) (Model.counts m)
        assertBool "the model's seq moved past the enqueue" (m.modelSeq > queued.enqueuedSeq)
        assertEqual "the pass stopped clean" (Just (Stopped True 0)) m.modelPass
        -- the re-read snapshot was taken while (or after) the pass ran, so
        -- a node's view is the snapshot's or the stream's, whichever is
        -- numbered later; either way it is converged and wanted up
        forM_ (Model.nodesInOrder m) $ \n -> do
            assertEqual ("direction of " <> show n.nodeRef) "up" n.nodeDirection
            assertEqual ("convergence of " <> show n.nodeRef) "converged" n.nodeConvergence
        -- /dag's order is the Dag's first-seen order, the one `run tree` prints: the root, then what it stands on
        assertEqual "the root comes first, as printDagTree prints it" (Just "client-root") (nodeShorthand <$> headMay (Model.nodesInOrder m))
        ups <- readIORef (runningUps running)
        assertEqual "each node went up once" (Map.fromList [("n1", 1), ("n2", 1)]) ups
        -- and the reads beside it
        st <- Client.status client
        assertBool "status answers with nodes" (has "nodes" st)
        hist <- Client.history client
        assertBool "history answers with seeds" (has "seeds" hist)
        hlp <- Client.seedHelp client
        assertBool "help answers with the seed text" (has "seed" hlp)
        -- a refused request is a typed error
        bad <- try (Client.command client "status\nhistory")
        case bad of
            Left (Client.Refused 400 _) -> pure ()
            other -> assertFailure ("two lines in one body: " <> show (either show (const "answered") other))
  where
    isStopped (Just (Stopped _ _)) = True
    isStopped _ = False
    has k (Object o) = KeyMap.member k o
    has _ _ = False
    headMay [] = Nothing
    headMay (x : _) = Just x
