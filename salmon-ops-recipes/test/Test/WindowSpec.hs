{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Layer 0 coverage for "Salmon.Op.Window": membership (midnight, weekly,
-- offsets), the next opening, parsing, and the gate holding a disruptive
-- node until an injected clock passes into the window.
module Test.WindowSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTChan)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Functor.Identity (runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.UpDown (CheckResult (..), upTreeWith)
import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Extension (..), Op, Track', deps, evalDeps, nodeps, noop, op, opAct)
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Configure (Configure (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Status (Direction (..))
import Salmon.Op.Supervision (millis)
import Salmon.Op.Track (Track (..))
import Salmon.Op.Window
import Salmon.Reporter (ReporterM (..))

import Test.Harness (capture)

-- 2026-09-26 is a Saturday.
at :: String -> UTCTime
at = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"

win :: Text -> Window
win t = either (error . show) id (parseWindow t)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.Window"
        [ testCase "daily window: start inclusive, end exclusive" $ do
            let w = win "02:00-04:00"
            assertBool "in" (inWindow w (at "2026-09-26 02:00"))
            assertBool "in" (inWindow w (at "2026-09-26 03:59"))
            assertBool "out at end" (not (inWindow w (at "2026-09-26 04:00")))
            assertBool "out before" (not (inWindow w (at "2026-09-26 01:59")))
        , testCase "a daily window crosses midnight" $ do
            let w = win "23:00-01:00"
            assertBool "late" (inWindow w (at "2026-09-26 23:30"))
            assertBool "early" (inWindow w (at "2026-09-27 00:30"))
            assertBool "out" (not (inWindow w (at "2026-09-27 01:00")))
            assertBool "out midday" (not (inWindow w (at "2026-09-27 12:00")))
        , testCase "a weekly window applies to its day only" $ do
            let w = win "Sat:02:00-04:00"
            assertBool "Sat" (inWindow w (at "2026-09-26 03:00"))
            assertBool "Sun" (not (inWindow w (at "2026-09-27 03:00")))
        , testCase "a weekly window crossing midnight ends on the next day" $ do
            let w = win "Sat:23:00-01:00"
            assertBool "Sat night" (inWindow w (at "2026-09-26 23:30"))
            assertBool "Sun early" (inWindow w (at "2026-09-27 00:30"))
            assertBool "Sat early is not" (not (inWindow w (at "2026-09-26 00:30")))
            assertBool "Mon early is not" (not (inWindow w (at "2026-09-28 00:30")))
        , testCase "a Sunday window crossing midnight ends on Monday" $ do
            let w = win "Sun:23:00-01:00"
            assertBool "Mon early" (inWindow w (at "2026-09-28 00:30"))
        , testCase "the offset moves the window (and its weekday) against UTC" $ do
            let w = win "Sun:01:00-03:00@+02:00" -- Sat 23:00-01:00 UTC
            assertBool "in" (inWindow w (at "2026-09-26 23:30"))
            assertBool "out" (not (inWindow w (at "2026-09-27 01:00")))
            let n = win "Sat:22:00-23:00@-05:00" -- Sun 03:00-04:00 UTC
            assertBool "neg" (inWindow n (at "2026-09-27 03:30"))
        , testCase "several windows: any one suffices" $
            assertBool "second" (inAnyWindow [win "02:00-03:00", win "12:00-13:00"] (at "2026-09-26 12:30"))
        , testCase "nextOpening" $ do
            let ws = [win "Sun:02:00-04:00"]
            assertEqual "from Sat" (Just (at "2026-09-27 02:00")) (nextOpening ws (at "2026-09-26 10:00"))
            assertEqual "open now" Nothing (nextOpening ws (at "2026-09-27 03:00"))
            assertEqual "just after" (Just (at "2026-10-04 02:00")) (nextOpening ws (at "2026-09-27 04:00"))
            assertEqual "with offset" (Just (at "2026-09-27 00:00")) (nextOpening [win "Sun:02:00-04:00@+02:00"] (at "2026-09-26 10:00"))
            assertEqual "none" Nothing (nextOpening [] (at "2026-09-26 10:00"))
        , testCase "parsing refuses what it cannot mean" $ do
            mapM_
                (\t -> assertBool (show t) (either (const True) (const False) (parseWindow t)))
                ["", "02:00", "02:00-02:00", "Xyz:02:00-03:00", "25:00-03:00", "02:00-03:60", "02:00-03:00@Mars", "2:0-3:0"]
            assertEqual "roundtrip" "Sun:02:00-04:00@+02:00" (renderWindow (win "Sun:02:00-04:00@+02:00"))
        , testCase "a held node is not applied, then is once the clock enters the window" gateHolds
        , testCase "holdOf: free without windows, for a plain node, and inside a window" holdOfCases
        , testGroup
            "Upkeep under windows"
            [ testCase "a due disruptive node waits, says Held once, and goes ahead when the window opens" upkeepWaitsForWindow
            , testCase "a disruptive node already in place is not held" upkeepSatisfiedNotHeld
            , testCase "Force goes through a closed window" upkeepForceGoesThrough
            ]
        , testCase "run serve: a pass holds a disruptive node, status says until when, tending applies it in the window" serveHoldsThenApplies
        ]

holdOfCases :: IO ()
holdOfCases = do
    clock <- newIORef (at "2026-09-26 10:00")
    let ws = Windows [win "02:00-04:00"] (readIORef clock) (millis 20)
        plain = maybe (error "noop has an action") (\a -> a.extension) (opAct (noop "plain"))
        restart = disruptive plain
    assertEqual "no windows" Free =<< holdOf noWindows restart
    assertEqual "not disruptive" Free =<< holdOf ws plain
    assertEqual "held" (HeldUntil (Just (at "2026-09-27 02:00"))) =<< holdOf ws restart
    writeIORef clock (at "2026-09-27 03:00")
    assertEqual "open" Free =<< holdOf ws restart

-------------------------------------------------------------------------------
-- Upkeep

type Trace = TVar [Upkeep.Report Extension]

awaitTrace :: Trace -> ([Upkeep.Report Extension] -> Bool) -> IO ()
awaitTrace trace p = atomically (readTVar trace >>= \rs -> unless (p (reverse rs)) retry)

within :: Int -> IO a -> IO a
within secs act = timeout (secs * 1000000) act >>= maybe (assertFailure "timed out" >> error "unreachable") pure

-- | One node, tended up from scratch under these windows.
tendingOne :: Windows -> Op -> (Upkeep.Supervisor Extension -> Trace -> IO a) -> IO a
tendingOne ws o body = do
    trace <- newTVarIO []
    let r = ReporterM $ \rep -> atomically (modifyTVar' trace (rep :))
        dag = Dag.foldDag Dag.sameRepresentative (evalDeps o)
    bracket
        (Upkeep.startUpkeepWith ws r Upkeep.noKept (const (Just (Upkeep.Tend TurnUp Upkeep.Unsettled))) dag)
        Upkeep.stopUpkeep
        (\sup -> body sup trace)

heldReports :: [Upkeep.Report Extension] -> [Maybe UTCTime]
heldReports rs = [next | Upkeep.Held _ next <- rs]

dones :: [Upkeep.Report Extension] -> Int
dones rs = length [() | Upkeep.Acted (UpDown.Done _) <- rs]

upkeepWaitsForWindow :: IO ()
upkeepWaitsForWindow = within 10 $ do
    applied <- newIORef (0 :: Int)
    clock <- newIORef (at "2026-09-26 10:00")
    let ws = Windows [win "02:00-04:00"] (readIORef clock) (millis 20)
        o = op "restart" nodeps $ \x ->
            disruptive x{ref = mkRef "restart" ("r" :: Text), check = pure (Failure "due"), up = modifyIORef' applied (+ 1)}
    tendingOne ws o $ \_ trace -> do
        awaitTrace trace (not . null . heldReports)
        -- several rechecks go by while held
        threadDelay 150000
        assertEqual "not applied while held" 0 =<< readIORef applied
        rs <- reverse <$> readTVarIO trace
        assertEqual "said once, with the next opening" [Just (at "2026-09-27 02:00")] (heldReports rs)
        writeIORef clock (at "2026-09-27 02:10")
        awaitTrace trace ((>= 1) . dones)
        assertEqual "applied once the window opened" 1 =<< readIORef applied

upkeepSatisfiedNotHeld :: IO ()
upkeepSatisfiedNotHeld = within 10 $ do
    applied <- newIORef (0 :: Int)
    let ws = Windows [win "02:00-04:00"] (pure (at "2026-09-26 10:00")) (millis 20)
        o = op "restart" nodeps $ \x ->
            disruptive x{ref = mkRef "restart" ("r" :: Text), check = pure Success, up = modifyIORef' applied (+ 1)}
    tendingOne ws o $ \_ trace -> do
        awaitTrace trace (\rs -> not (null [() | Upkeep.Upkeep _ Upkeep.Up <- rs]))
        rs <- reverse <$> readTVarIO trace
        assertEqual "never held" [] (heldReports rs)
        assertEqual "never applied" 0 =<< readIORef applied

upkeepForceGoesThrough :: IO ()
upkeepForceGoesThrough = within 10 $ do
    applied <- newIORef (0 :: Int)
    let ws = Windows [win "02:00-04:00"] (pure (at "2026-09-26 10:00")) (millis 20)
        r = mkRef "restart" ("r" :: Text)
        o = op "restart" nodeps $ \x ->
            disruptive x{ref = r, check = pure (Failure "due"), up = modifyIORef' applied (+ 1)}
    tendingOne ws o $ \sup trace -> do
        awaitTrace trace (not . null . heldReports)
        assertEqual "held" 0 =<< readIORef applied
        _ <- Upkeep.instruct sup r Force
        awaitTrace trace ((>= 1) . dones)
        assertEqual "forced through" 1 =<< readIORef applied

-------------------------------------------------------------------------------
-- run serve

serveHoldsThenApplies :: IO ()
serveHoldsThenApplies = within 20 $ do
    restartUps <- newIORef (0 :: Int)
    plainUps <- newIORef (0 :: Int)
    clock <- newIORef (at "2026-09-26 10:00")
    reports <- newTVarIO ([] :: [Serve.Report])
    inbox <- newEmptyMVar
    worldVar <- newEmptyMVar
    let ws = Windows [win "02:00-04:00"] (readIORef clock) (millis 20)
        restartRef = mkRef "restart" ("r" :: Text)
        plainRef = mkRef "plain" ("p" :: Text)
        program :: Track' [String]
        program = Track $ \_ ->
            op "root" (deps [restartOp, plainOp]) $ \x -> x{ref = mkRef "root" ("root" :: Text)}
        restartOp = op "restart" nodeps $ \x -> disruptive x{ref = restartRef, up = modifyIORef' restartUps (+ 1)}
        plainOp = op "plain" nodeps $ \x -> x{ref = plainRef, up = modifyIORef' plainUps (+ 1)}
        serveR = ReporterM $ \rep -> atomically (modifyTVar' reports (Serve.attributed rep :))
        silentR = ReporterM $ \_ -> pure ()
        -- lines are handed over one at a time, so the loop goes idle (and
        -- tends) in between
        producer = Serve.Producer $ \chan ->
            let go = do
                    next <- takeMVar inbox
                    case next of
                        Just l -> atomically (writeTChan chan (Serve.Line Serve.Stdin l)) >> go
                        Nothing -> atomically (writeTChan chan (Serve.Eof Serve.Stdin))
             in go
        nodeOf rf = do
            readWorld <- readMVar worldVar
            w <- readWorld
            pure (Map.lookup rf w.worldNodes)
        awaitNode rf p = do
            st <- nodeOf rf
            unless (maybe False p st) (threadDelay 10000 >> awaitNode rf p)
    done <- newEmptyMVar
    _ <- forkIO $ do
        _ <-
            Serve.serveObservedWith
                ws
                (putMVar worldVar)
                []
                Nothing
                True
                serveR
                silentR
                (Right :: [String] -> Either Text [String])
                (Configure pure)
                program
                Nothing
                [producer]
        putMVar done ()
    putMVar inbox (Just "up x")
    awaitNode restartRef (isJust . Serve.nodeHeld)
    Just st <- nodeOf restartRef
    assertEqual "held until the next opening" (Just (Just (at "2026-09-27 02:00"))) st.nodeHeld
    assertBool "not counted converged" (st.nodeConvergence /= Serve.Converged)
    assertEqual "not applied" 0 =<< readIORef restartUps
    assertEqual "a node with no opinion is applied" 1 =<< readIORef plainUps
    putMVar inbox (Just "status")
    let statusLines = do
            rs <- readTVarIO reports
            pure (concat [Serve.renderReport rep | rep@Serve.StatusReport{} <- rs])
        awaitStatus = statusLines >>= \ls -> when (null ls) (threadDelay 10000 >> awaitStatus)
    awaitStatus
    ls <- statusLines
    assertBool
        ("status says until when: " <> show ls)
        (any ("[held by maintenance window until 2026-09-27 02:00:00 UTC]" `Text.isInfixOf`) ls)
    rs <- readTVarIO reports
    assertBool "the pass said Held" (not (null [() | Serve.Tended (Upkeep.Held _ _) <- rs]))
    -- the window opens; the loop is idle, so the node's machine applies it
    writeIORef clock (at "2026-09-27 02:10")
    awaitNode restartRef (\s -> s.nodeConvergence == Serve.Converged)
    Just st' <- nodeOf restartRef
    assertEqual "no longer held" Nothing st'.nodeHeld
    assertEqual "applied once, in the window" 1 =<< readIORef restartUps
    putMVar inbox Nothing
    takeMVar done

gateHolds :: IO ()
gateHolds = do
    applied <- newIORef (0 :: Int)
    plainApplied <- newIORef (0 :: Int)
    clock <- newIORef (at "2026-09-26 10:00")
    heldLog <- newIORef ([] :: [Maybe UTCTime])
    let restart = op "restart" nodeps $ \x ->
            disruptive x{ref = mkRef "restart" ("r" :: Text), up = modifyIORef' applied (+ 1)}
        plain = op "plain" nodeps $ \x ->
            x{ref = mkRef "plain" ("p" :: Text), up = modifyIORef' plainApplied (+ 1)}
        gate = windowGateAt (readIORef clock) [win "02:00-04:00"] (\_ next -> modifyIORef' heldLog (next :))
        pass o = do
            (r, _) <- capture
            ok <- upTreeWith gate r (pure . runIdentity) o
            assertBool "pass succeeds" ok
    pass restart
    pass plain
    assertEqual "held outside the window" 0 =<< readIORef applied
    assertEqual "a node with no opinion is untouched" 1 =<< readIORef plainApplied
    assertEqual "held, told when it opens" [Just (at "2026-09-27 02:00")] =<< readIORef heldLog
    writeIORef clock (at "2026-09-27 02:30")
    pass restart
    assertEqual "applied inside the window" 1 =<< readIORef applied
