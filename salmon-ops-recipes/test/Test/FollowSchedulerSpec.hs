{-# LANGUAGE DeriveGeneric #-}

{- | "Salmon.Actions.Follow.Scheduler", at two layers.

The pure step ('Scheduler.observed'\/'Scheduler.poked'\/'Scheduler.injected'
and 'Scheduler.next') is table-tested directly: the ladder toward the
registry, the quiet window and @max_wait@ toward the loop, and what @fetch@
does to both.

Then the fetcher of "Salmon.Actions.Follow" is run on that step with a
clock the test moves ('FakeClock'): a @run serve@ over a temp-dir registry,
as "Test.FollowSpec" does, except that nothing here ever sleeps — the
fetcher blocks until the test advances time past its deadline, and the
test can read what deadline it is waiting for, which is what makes "polled
on the ladder, not the base" an assertion on numbers rather than on
timing.
-}
module Test.FollowSchedulerSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (TChan, TVar, atomically, check, newTChanIO, newTVarIO, orElse, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Follow as Follow
import Salmon.Actions.Follow (Document (..), Entry (..), Label)
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import Salmon.Actions.Follow.Scheduler (Action (..), Config (..), Due (..), Outcome (..), Wake (..))
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Line (..), NodeState (..), Origin (..), Producer (..), World (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension, Op, Track', deps, op, ref)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (Reporter)

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Follow.Scheduler"
        [ testGroup
            "the step"
            [ testCase "the ladder: base, then times factor per consecutive failure, up to cap, back to base on success" ladderSteps
            , testCase "jitter scales every delay within its band, and the same seed draws the same delays" jitterBand
            , testCase "the window: changes inside it restart it, and one injection is due once it is quiet" quietWindow
            , testCase "max_wait: a registry that never goes quiet is injected at max_wait" maxWait
            , testCase "poked: a round now, the ladder forgotten, the pending batch injected right after" pokedFlushes
            , testCase "poked with nothing pending afterwards flushes nothing" pokedNothing
            ]
        , testGroup
            "the fetcher on a moved clock"
            [ testCase "three writes inside the window are one injection, diffed against the last applied document, and one pass" threeWritesOnePass
            , testCase "a registry that throws is polled on the ladder, not the base" failingRegistryOnTheLadder
            , testCase "`fetch` injects the pending batch at once, and resets the ladder" fetchCommand
            ]
        , testGroup
            "the command"
            [ testCase "`fetch` parses, takes no argument, and is in the reference" fetchParses
            , testCase "`fetch` with nothing followed says so" fetchNothingFollowed
            ]
        ]

-------------------------------------------------------------------------------
-- the step

-- | Small integers as microseconds: the units are the step's own.
cfg :: Config
cfg =
    Config
        { schedBase = 2
        , schedFactor = 2
        , schedCap = 9
        , schedJitter = 0
        , schedDebounce = 5
        , schedMaxWait = 60
        }

-- | Feed outcomes at successive round times and collect each next deadline.
rounds :: Config -> Scheduler.Sched -> [(Int, Outcome)] -> [Due]
rounds c = go
  where
    go _ [] = []
    go st ((t, o) : rest) =
        let st' = Scheduler.observed c t o st
         in Scheduler.next c st' : go st' rest

ladderSteps :: IO ()
ladderSteps = do
    assertEqual "rungs" [2, 2, 4, 8, 9, 9] (fmap (Scheduler.ladder cfg) [0, 1, 2, 3, 4, 5])
    let st0 = Scheduler.start cfg (Scheduler.mkRng 1) 0
    assertEqual "after startup, one base away" (Due 2 Fetch) (Scheduler.next cfg st0)
    -- failures at their own deadlines: each next round is a rung further
    let dues = rounds cfg st0 [(2, Failed), (4, Failed), (8, Failed), (16, Failed), (25, Failed), (34, Unchanged), (36, Failed)]
    assertEqual
        "failing: base, then doubling, then capped; success resets; a fresh failure starts over at base"
        [Due 4 Fetch, Due 8 Fetch, Due 16 Fetch, Due 25 Fetch, Due 34 Fetch, Due 36 Fetch, Due 38 Fetch]
        dues
    assertEqual "a changed round is a success too" [Due 4 Fetch, Due 6 Fetch] (fmap (\d -> d{dueAction = Fetch}) (rounds cfg st0 [(2, Failed), (4, Changed)]))

jitterBand :: IO ()
jitterBand = do
    let jc = cfg{schedBase = 1000, schedJitter = 0.2}
        draws g n = if n == (0 :: Int) then [] else let (d, g') = Scheduler.jittered jc g 1000 in d : draws g' (n - 1)
        xs = draws (Scheduler.mkRng 42) 200
    assertBool "every delay within [800, 1200]" (all (\d -> d >= 800 && d <= 1200) xs)
    assertBool "and not all the same" (any (/= 1000) xs)
    assertEqual "the same seed draws the same delays" xs (draws (Scheduler.mkRng 42) 200)
    assertEqual "no jitter is the identity" (1000, Scheduler.mkRng 7) (Scheduler.jittered cfg (Scheduler.mkRng 7) 1000)

quietWindow :: IO ()
quietWindow = do
    let st0 = Scheduler.start cfg (Scheduler.mkRng 1) 0
    -- a change at 2 opens a window closing at 7; the round at 4 is sooner
    -- than that, so it is what is due; each change restarts the window;
    -- once rounds stop seeing changes the window's close is what is due
    assertEqual
        "changes at 2, 4, 6 then quiet: the injection is due at 6 + debounce"
        [Due 4 Fetch, Due 6 Fetch, Due 8 Fetch, Due 10 Fetch, Due 11 Inject]
        (rounds cfg st0 [(2, Changed), (4, Changed), (6, Changed), (8, Unchanged), (10, Unchanged)])
    -- and after the injection nothing is pending
    let st = foldl (\s (t, o) -> Scheduler.observed cfg t o s) st0 [(2, Changed), (4, Changed), (6, Changed), (8, Unchanged), (10, Unchanged)]
    assertEqual "injected: back to plain rounds" (Due 12 Fetch) (Scheduler.next cfg (Scheduler.injected 11 st))
    assertEqual "an injection due at the same instant as a round goes second" (Due 7 Fetch) (Scheduler.next cfg (Scheduler.observed cfg 2 Changed st0){Scheduler.schedNextFetch = 7})

maxWait :: IO ()
maxWait = do
    let mc = cfg{schedMaxWait = 9}
        st0 = Scheduler.start mc (Scheduler.mkRng 1) 0
    assertEqual
        "every round changes: the window never closes, max_wait (2 + 9 = 11) does"
        [Due 4 Fetch, Due 6 Fetch, Due 8 Fetch, Due 10 Fetch, Due 11 Inject]
        (rounds mc st0 [(2, Changed), (4, Changed), (6, Changed), (8, Changed), (10, Changed)])

pokedFlushes :: IO ()
pokedFlushes = do
    let pc = cfg{schedDebounce = 50}
        st0 = Scheduler.start pc (Scheduler.mkRng 1) 0
        -- three failures deep, with a change seen before they began, its
        -- window (2 + 50) still open
        st = foldl (\s (t, o) -> Scheduler.observed pc t o s) st0 [(2, Changed), (4, Failed), (6, Failed), (10, Failed)]
    assertEqual "before: the next round is up the ladder, the injection far off" (Due 18 Fetch) (Scheduler.next pc st)
    let p = Scheduler.poked 12 st
    assertEqual "poked: a round now" (Due 12 Fetch) (Scheduler.next pc p)
    assertEqual "and the failures forgotten" 0 (Scheduler.schedFailures p)
    let p' = Scheduler.observed pc 12 Unchanged p
    assertEqual "after that round: the pending batch is due now, not at its window" (Due 12 Inject) (Scheduler.next pc p')
    assertEqual "then rounds at the base" (Due 14 Fetch) (Scheduler.next pc (Scheduler.injected 12 p'))
    -- a change the poked round itself finds is flushed just the same
    let q = Scheduler.observed pc 12 Changed (Scheduler.poked 12 st0)
    assertEqual "a change found by the poked round is injected at once" (Due 12 Inject) (Scheduler.next pc q)

pokedNothing :: IO ()
pokedNothing = do
    let st0 = Scheduler.start cfg (Scheduler.mkRng 1) 0
        p = Scheduler.observed cfg 5 Unchanged (Scheduler.poked 5 st0)
    assertEqual "nothing pending: plain rounds" (Due 7 Fetch) (Scheduler.next cfg p)
    -- a later change is not flushed by a poke that is long over
    assertEqual "a later change waits its window" (Due 12 Inject) (Scheduler.next cfg (Scheduler.observed cfg 7 Changed p){Scheduler.schedNextFetch = 20})

-------------------------------------------------------------------------------
-- the served thing: "make these files exist", same as Test.FollowSpec

data Spec = Spec
    { specDir :: FilePath
    , specNames :: [String]
    }
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: FilePath -> [String] -> Either Text Spec
parseSpec root args
    | null args = Left "expected at least one file name"
    | otherwise = Right (Spec (root </> "files") args)

program :: Track' Spec
program = Track $ \spec ->
    op "follow-sched-root" (deps (fmap (fileOp spec.specDir) spec.specNames)) $ \actions ->
        actions{ref = mkRef "follow-sched-root" (spec.specDir, spec.specNames)}

fileOp :: FilePath -> String -> Op
fileOp d n = FS.filecontents (FS.FileContents (d </> n) ("contents of " <> n))

-------------------------------------------------------------------------------
-- a clock the test moves

data FakeClock = FakeClock
    { fakeNow :: TVar Int
    , fakePoke :: TVar Bool
    , fakeSleeping :: TVar (Maybe Int)
    -- ^ the deadline the fetcher is currently waiting for, if it is
    }

newFakeClock :: IO FakeClock
newFakeClock = FakeClock <$> newTVarIO 0 <*> newTVarIO False <*> newTVarIO Nothing

clockOf :: FakeClock -> Scheduler.Clock
clockOf fc =
    Scheduler.Clock
        { Scheduler.clockNow = readTVarIO fc.fakeNow
        , Scheduler.clockWaitUntil = \at -> do
            atomically (writeTVar fc.fakeSleeping (Just at))
            wake <-
                atomically $
                    (readTVar fc.fakeNow >>= \n -> check (n >= at) >> pure Elapsed)
                        `orElse` (readTVar fc.fakePoke >>= check >> writeTVar fc.fakePoke False >> pure Poked)
            atomically (writeTVar fc.fakeSleeping Nothing)
            pure wake
        }

advanceTo :: FakeClock -> Int -> IO ()
advanceTo fc t = atomically (writeTVar fc.fakeNow t)

-- | Wait for the fetcher to be asleep until exactly this deadline — which
-- asserts its schedule at the same time.
awaitSleep :: FakeClock -> Int -> IO ()
awaitSleep fc t = waitFor ("the fetcher to wait until " <> show t) ((== Just t) <$> readTVarIO fc.fakeSleeping)

-------------------------------------------------------------------------------
-- driving the loop

data Driver = Driver
    { typeLine :: String -> IO ()
    , serveReports :: IO [Serve.Report]
    , followReports :: IO [Follow.Report]
    , clock :: FakeClock
    }

-- | A registry the test can make throw, which records the fake time of
-- every call.
data Flaky = Flaky
    { flakyFailing :: IORef Bool
    , flakyCalls :: IORef [Int]
    }

newFlaky :: IO Flaky
newFlaky = Flaky <$> newIORef False <*> newIORef []

flakyRegistry :: Flaky -> FakeClock -> Follow.Registry -> Follow.Registry
flakyRegistry fl fc inner =
    inner
        { Follow.registryFetch = \lbl stamp -> do
            now <- readTVarIO fc.fakeNow
            modifyIORef' fl.flakyCalls (now :)
            failing <- readIORef fl.flakyFailing
            if failing
                then throwIO (userError "registry unreachable")
                else Follow.registryFetch inner lbl stamp
        }

withFollowing :: FilePath -> Config -> Flaky -> [Label] -> (Driver -> IO a) -> IO (World Spec Spec, [Serve.Report], [Follow.Report], a)
withFollowing root schedule fl labels body = do
    (serveReporter, readServe) <- capture
    (followReporter, readFollow) <- capture
    (nodeReporter, _) <- capture :: IO (Reporter (UpDown.Report Extension), IO [UpDown.Report Extension])
    stdinChan <- newTChanIO
    gate <- newEmptyMVar
    fc <- newFakeClock
    modeVar <- Follow.newMode
    appliedVar <- Follow.newApplied
    let follow =
            Follow.Follow
                { Follow.followRegistry = flakyRegistry fl fc (Follow.directoryRegistry (registryDir root))
                , Follow.followLabels = labels
                , Follow.followSchedule = schedule
                , Follow.followCache = Nothing
                , Follow.followRefuseOlder = False
                , Follow.followVerify = Follow.noVerifier
                }
        producers =
            [ Follow.followerWith followReporter (clockOf fc) (Scheduler.mkRng 1) modeVar appliedVar follow (putMVar gate ())
            , Follow.gated gate (chanProducer stdinChan)
            ]
        driver =
            Driver
                { typeLine = \l -> atomically (writeTChan stdinChan (Just l))
                , serveReports = readServe
                , followReports = readFollow
                , clock = fc
                }
    resultVar <- newTChanIO
    _ <- forkIO $ do
        outcome <- try (body driver)
        atomically (writeTChan stdinChan Nothing)
        atomically (writeTChan resultVar outcome)
    w <- Serve.serveFollowing [] Nothing True serveReporter nodeReporter (parseSpec root) (Configure pure) program (Just (Serve.Followed (atomically (writeTVar fc.fakePoke True)) (readIORef modeVar) (Follow.appliedDocuments appliedVar))) producers
    outcome <- atomically (readTChan resultVar)
    case outcome of
        Left (ex :: SomeException) -> throwIO ex
        Right a -> (,,,) w <$> readServe <*> readFollow <*> pure a

chanProducer :: TChan (Maybe String) -> Producer
chanProducer ch = Producer go
  where
    go inbox = do
        next <- atomically (readTChan ch)
        case next of
            Nothing -> atomically (writeTChan inbox (Eof Stdin))
            Just l -> atomically (writeTChan inbox (Line Stdin l)) >> go inbox

registryDir :: FilePath -> FilePath
registryDir root = root </> "reg"

label :: Text -> Label
label t = either (error . Text.unpack) id (Follow.mkLabel t)

publish :: FilePath -> Label -> Text -> [[String]] -> IO ()
publish root lbl did seeds = do
    createDirectoryIfMissing True (registryDir root)
    LByteString.writeFile (Follow.documentPath (registryDir root) lbl) (encode (Document did (fmap SeedWords seeds) Nothing))

waitFor :: String -> IO Bool -> IO ()
waitFor what cond = do
    ok <- timeout (10 * 1000000) go
    case ok of
        Just () -> pure ()
        Nothing -> assertFailure ("timed out waiting for " <> what)
  where
    go = do
        done <- cond
        if done then pure () else threadDelay 5000 >> go

fileExists :: FilePath -> String -> IO Bool
fileExists root n = doesFileExist (root </> "files" </> n)

convergences :: [Serve.Report] -> Int
convergences reports = length [() | Serve.ConvergeStop{} <- reports]

injections :: [Follow.Report] -> [(Int, Int)]
injections reports = [(nup, ndown) | Follow.Injected _ _ _ nup ndown <- reports]

-------------------------------------------------------------------------------

-- | Rounds every 2000, a window of 5000, so a change is seen by up to
-- three rounds before the window closes.
windowed :: Config
windowed = Config{schedBase = 2000, schedFactor = 2, schedCap = 100000, schedJitter = 0, schedDebounce = 5000, schedMaxWait = 60000}

threeWritesOnePass :: IO ()
threeWritesOnePass =
    withTempDir $ \root -> do
        fl <- newFlaky
        publish root (label "web") "web@1" [["a"]]
        (w, reports, freports, ()) <- withFollowing root windowed fl [label "web"] $ \d -> do
            let fc = d.clock
            waitFor "the startup document's file" (fileExists root "a")
            awaitSleep fc 2000
            -- three writes, each seen by its own round, each inside the window
            publish root (label "web") "web@2" [["a"], ["b"]]
            advanceTo fc 2000
            awaitSleep fc 4000
            publish root (label "web") "web@3" [["a"], ["b"], ["c"]]
            advanceTo fc 4000
            awaitSleep fc 6000
            publish root (label "web") "web@4" [["a"], ["b"], ["c"], ["d"]]
            advanceTo fc 6000
            awaitSleep fc 8000
            -- two quiet rounds; the window (6000 + 5000) closes before the next
            advanceTo fc 8000
            awaitSleep fc 10000
            advanceTo fc 10000
            awaitSleep fc 11000
            nothingYet <- not <$> fileExists root "b"
            assertBool "nothing injected while the window is open" nothingYet
            deferred <- length . filter isDeferred <$> d.followReports
            assertEqual "each round that saw a change said so" 3 deferred
            advanceTo fc 11000
            waitFor "the three files" (and <$> traverse (fileExists root) ["b", "c", "d"])
            awaitSleep fc 12000
        assertEqual "two injections in all: startup, then the one batch, diffed against web@1" [(1, 0), (3, 0)] (injections freports)
        assertEqual "the batch names the latest document" ["web@1", "web@4"] [did | Follow.Injected _ did _ _ _ <- freports]
        assertEqual "one convergence for the batch" 2 (convergences reports)
        assertBool "everything converged up" (all (\st -> st.nodeConvergence == Converged) (Map.elems w.worldNodes))
  where
    isDeferred Follow.Deferred{} = True
    isDeferred _ = False

-- | Rounds every 1000, doubling to a cap of 4000, nothing held back.
laddered :: Config
laddered = Config{schedBase = 1000, schedFactor = 2, schedCap = 4000, schedJitter = 0, schedDebounce = 0, schedMaxWait = 0}

failingRegistryOnTheLadder :: IO ()
failingRegistryOnTheLadder =
    withTempDir $ \root -> do
        fl <- newFlaky
        publish root (label "web") "web@1" [["a"]]
        (_, _, freports, ()) <- withFollowing root laddered fl [label "web"] $ \d -> do
            let fc = d.clock
            waitFor "the startup document's file" (fileExists root "a")
            awaitSleep fc 1000
            writeIORef fl.flakyFailing True
            advanceTo fc 1000
            awaitSleep fc 2000 -- first failure: base
            advanceTo fc 2000
            awaitSleep fc 4000 -- second: base * 2
            advanceTo fc 4000
            awaitSleep fc 8000 -- third: base * 4
            advanceTo fc 8000
            awaitSleep fc 12000 -- fourth: capped
            writeIORef fl.flakyFailing False
            publish root (label "web") "web@2" [["a"], ["b"]]
            advanceTo fc 12000
            waitFor "the recovered round's file" (fileExists root "b")
            awaitSleep fc 13000 -- back to the base
            calls <- reverse <$> readIORef fl.flakyCalls
            assertEqual "the registry was asked on the ladder" [0, 1000, 2000, 4000, 8000, 12000] calls
        assertEqual "the failure was reported once, not once per round" 1 (length [() | Follow.FetchFailed{} <- freports])
        assertEqual "each failed round said how long until the next" [(1, 1000), (2, 2000), (3, 4000), (4, 4000)] [(n, us) | Follow.Backoff n us <- freports]
        assertEqual "the recovered round's document was injected" [(1, 0), (1, 0)] (injections freports)

fetchCommand :: IO ()
fetchCommand =
    withTempDir $ \root -> do
        fl <- newFlaky
        publish root (label "web") "web@1" [["a"]]
        let schedule = laddered{schedCap = 100000, schedDebounce = 5000, schedMaxWait = 100000}
        (_, reports, freports, ()) <- withFollowing root schedule fl [label "web"] $ \d -> do
            let fc = d.clock
            waitFor "the startup document's file" (fileExists root "a")
            awaitSleep fc 1000
            -- a change seen, waiting for its window
            publish root (label "web") "web@2" [["a"], ["b"]]
            advanceTo fc 1000
            awaitSleep fc 2000
            nothingYet <- not <$> fileExists root "b"
            assertBool "held back by the window" nothingYet
            -- `fetch`: the pending batch goes in without the window
            d.typeLine "fetch"
            waitFor "the flushed file" (fileExists root "b")
            -- now three failures deep
            writeIORef fl.flakyFailing True
            advanceTo fc 2000
            awaitSleep fc 3000
            advanceTo fc 3000
            awaitSleep fc 5000
            advanceTo fc 5000
            awaitSleep fc 9000
            writeIORef fl.flakyFailing False
            publish root (label "web") "web@3" [["a"], ["b"], ["c"]]
            -- `fetch`: a round now rather than at 9000, its change applied
            -- at once, and the next round one base away rather than
            -- further up the ladder
            d.typeLine "fetch"
            waitFor "the fetched file" (fileExists root "c")
            awaitSleep fc 6000
            calls <- reverse <$> readIORef fl.flakyCalls
            assertEqual "the rounds `fetch` asked for ran when typed" [0, 1000, 1000, 2000, 3000, 5000, 5000] calls
        assertEqual "the loop acknowledged both" [True, True] [f | Serve.FetchRequested f <- reports]
        assertEqual "three injections: startup, flushed, fetched" [(1, 0), (1, 0), (1, 0)] (injections freports)
        assertEqual "one convergence each" 3 (convergences reports)

-------------------------------------------------------------------------------

fetchParses :: IO ()
fetchParses = do
    assertEqual "fetch" (Right Serve.Fetch) (Serve.parseServeCommand "fetch")
    assertBool "fetch takes no argument" (either (const True) (const False) (Serve.parseServeCommand "fetch now"))
    assertBool "the reference lists it" (any (Text.isInfixOf "fetch") (Serve.renderReport (Serve.HelpText Nothing)))
    assertBool "and it has a topic" (any (Text.isInfixOf "quiet window") (Serve.renderReport (Serve.HelpText (Just "fetch"))))

fetchNothingFollowed :: IO ()
fetchNothingFollowed =
    withTempDir $ \root -> do
        (serveReporter, readServe) <- capture
        (nodeReporter, _) <- capture :: IO (Reporter (UpDown.Report Extension), IO [UpDown.Report Extension])
        stdinChan <- newTChanIO
        atomically (writeTChan stdinChan (Just "fetch"))
        atomically (writeTChan stdinChan Nothing)
        _ <- Serve.serveProducers [] Nothing True serveReporter nodeReporter (parseSpec root) (Configure pure) program [chanProducer stdinChan]
        reports <- readServe
        assertEqual "nothing is being followed" [False] [f | Serve.FetchRequested f <- reports]
