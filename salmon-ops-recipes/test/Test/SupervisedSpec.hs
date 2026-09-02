{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for "Salmon.Builtin.Nodes.Supervised", driven through
'Salmon.Actions.Serve.serveWith'.

Unlike "Test.ServeSpec" these cannot be scripted from a file: the whole point
is what the loop does /between/ commands, so the serve loop runs on its own
thread against a pipe that stays open while the test kills processes behind
its back and waits for it to notice.

Everything spawned here is @sleep@ or @true@, and every wait has a deadline,
so a broken supervisor fails an assertion rather than hanging the suite.
-}
module Test.SupervisedSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, try)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hPutStrLn, hSetBuffering)
import System.Posix.Signals (nullSignal, sigKILL, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (createPipe, proc)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), NodeState (..), World (..))
import Salmon.Builtin.Extension (Op, Track', deps, op, ref)
import qualified Salmon.Builtin.Nodes.Supervised as Sup
import Salmon.Builtin.Nodes.Supervised (Policy (..), Service (..), Status (..), Supervisor)
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)

import Test.Harness (capture)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Supervised"
        [ testCase "a declared service comes up and a second pass leaves it alone" serviceComesUp
        , testCase "a service killed behind the loop's back is restarted" killedServiceRestarts
        , testCase "retiring a service stops its process, and it stays stopped" downStopsTheProcess
        , testCase "a service that cannot stay up backs off instead of spinning" backoffPreventsSpinning
        , testCase "a crash-looping service is given up on, and reports as errored" crashLoopGivesUp
        ]

-------------------------------------------------------------------------------
-- The thing being served: "these named services are running".

newtype Spec = Spec {specNames :: [String]}
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: [String] -> Either Text Spec
parseSpec args
    | null args = Left "expected at least one service name"
    | otherwise = Right (Spec args)

{- | Three shapes of service, picked by the name the seed is declared with:
one that stays up, one that exits immediately behind a long backoff, and one
that exits immediately behind a short backoff and a low give-up threshold.
-}
serviceNamed :: String -> Service
serviceNamed name
    | "sleeper" `isPrefixOf` name =
        Sup.defaultService (Text.pack name) (proc "sleep" ["300"])
    | "slow-flapper" `isPrefixOf` name =
        (Sup.defaultService (Text.pack name) (proc "true" []))
            { service_policy =
                Sup.defaultPolicy
                    { policy_initialDelay = 5
                    , policy_stableAfter = 100
                    , policy_giveUpAfter = Nothing
                    }
            }
    | otherwise =
        (Sup.defaultService (Text.pack name) (proc "true" []))
            { service_policy =
                Sup.defaultPolicy
                    { policy_initialDelay = 0.01
                    , policy_multiplier = 1.0
                    , policy_maxDelay = 0.01
                    , policy_stableAfter = 100
                    , policy_giveUpAfter = Just 3
                    }
            }

program :: Supervisor -> Track' Spec
program sup = Track $ \spec ->
    op "supervised-root" (deps (fmap (serviceOp sup) spec.specNames)) $ \actions ->
        actions{ref = mkRef "supervised-root" spec.specNames}

serviceOp :: Supervisor -> String -> Op
serviceOp sup name = Sup.service silent sup (serviceNamed name)

-------------------------------------------------------------------------------

serviceComesUp :: IO ()
serviceComesUp = do
    ((), _, _) <- withServeLoop ["sleeper"] $ \h sup _ -> do
        alive <- waitUntil (running sup "sleeper")
        assertBool "the service came up" alive
        pid1 <- pidOf sup "sleeper"
        -- a redundant declaration must not spawn a second copy: prelim
        -- reports the live process as nothing-to-do.
        hPutStrLn h "up sleeper"
        threadDelay 300000
        pid2 <- pidOf sup "sleeper"
        assertEqual "still the same process" pid1 pid2
    pure ()

{- | The headline: nothing is typed between the kill and the restart. The
reaper notices the exit, publishes a wakeup, and the serve loop converges on
its own — the difference between supervision and restart-on-demand.
-}
killedServiceRestarts :: IO ()
killedServiceRestarts = do
    ((), _, reports) <- withServeLoop ["sleeper"] $ \_ sup _ -> do
        up <- waitUntil (running sup "sleeper")
        assertBool "the service came up" up
        pid1 <- pidOf sup "sleeper"
        case pid1 of
            Nothing -> assertBool "expected a pid" False
            Just pid -> do
                signalProcess sigKILL pid
                back <- waitUntil $ do
                    mpid <- pidOf sup "sleeper"
                    pure (maybe False (/= pid) mpid)
                assertBool "it came back under a new pid, with nothing typed" back
    assertBool
        "the loop reported being woken, rather than only converging on command"
        (not (null [n | Serve.Woken n <- reports]))

downStopsTheProcess :: IO ()
downStopsTheProcess = do
    ((), _, _) <- withServeLoop ["sleeper"] $ \h sup _ -> do
        up <- waitUntil (running sup "sleeper")
        assertBool "the service came up" up
        Just pid <- pidOf sup "sleeper"

        hPutStrLn h "down sleeper"
        gone <- waitUntil (not <$> running sup "sleeper")
        assertBool "the supervisor lets go of it" gone
        stillThere <- waitUntil (not <$> processExists pid)
        assertBool "and the process is really gone, not merely forgotten" stillThere

        -- it must also stay gone: the death its own teardown produced is not
        -- a reason to bring it back.
        threadDelay 400000
        again <- running sup "sleeper"
        assertBool "it was not resurrected after being told to stop" (not again)
    pure ()

{- | A service that exits the moment it starts is the case that turns a naive
supervisor into a fork bomb. With a five-second first delay, one attempt is
all that may happen in the window this test watches.
-}
backoffPreventsSpinning :: IO ()
backoffPreventsSpinning = do
    ((), _, _) <- withServeLoop ["slow-flapper"] $ \_ sup _ -> do
        died <- waitUntil (not <$> running sup "slow-flapper")
        assertBool "it exited immediately, as the test intends" died
        threadDelay 700000
        st <- statusOf sup "slow-flapper"
        assertEqual
            "one failure recorded: it is waiting out the delay, not respawning"
            (Just 1)
            (fmap status_failures st)
    pure ()

{- | Once it gives up, @up@ throws rather than skipping, so the node is
visibly 'Errored'. A skip would have been recorded as converged, and @status@
would show a crash-looped service as healthy.
-}
crashLoopGivesUp :: IO ()
crashLoopGivesUp = do
    (failures, w, _) <- withServeLoop ["flapper"] $ \_ sup _ -> do
        gaveUp <- waitUntil (maybe False status_gaveUp <$> statusOf sup "flapper")
        assertBool "it stopped trying" gaveUp
        st <- statusOf sup "flapper"
        pure (fmap status_failures st)
    assertEqual
        "gave up after exactly the configured number of consecutive failures"
        (Just 3)
        failures
    assertBool
        "and the node ends errored rather than converged"
        (Errored `elem` fmap nodeConvergence (Map.elems w.worldNodes))

-------------------------------------------------------------------------------

{- | Runs a serve loop on its own thread against a live pipe, declares the
named services, and hands the test the write end (to type more commands), the
supervisor (to inspect, and to interfere with), and the loop's reports so far.

Closing the pipe ends the loop; everything supervised is stopped first, so no
@sleep@ outlives the test.
-}
withServeLoop ::
    [String] ->
    (Handle -> Supervisor -> IO [Serve.Report] -> IO a) ->
    IO (a, World Spec Spec, [Serve.Report])
withServeLoop names act = do
    (readEnd, writeEnd) <- createPipe
    hSetBuffering writeEnd LineBuffering
    sup <- Sup.newSupervisor
    (serveReporter, readServeReports) <- capture
    done <- newEmptyMVar :: IO (MVar (Either SomeException (World Spec Spec)))
    -- always fill the MVar, so a serve loop that dies takes the test down
    -- with a message instead of leaving it blocked on 'takeMVar' forever.
    _ <-
        forkIO $
            try
                ( Serve.serveWith
                    (Sup.supervisorWakeups sup)
                    serveReporter
                    silent
                    parseSpec
                    (Configure pure)
                    (program sup)
                    readEnd
                )
                >>= putMVar done
    hPutStrLn writeEnd ("up " <> unwords names)
    -- stop the services and close the pipe even if an assertion throws,
    -- or a failing test leaves a `sleep` and a serve loop behind it.
    result <- act writeEnd sup readServeReports `finally` shutdown sup writeEnd
    outcome <- timeout 10000000 (takeMVar done)
    w <- case outcome of
        Nothing -> assertFailure "the serve loop did not stop when told to quit"
        Just (Left err) -> assertFailure ("the serve loop died: " <> show err)
        Just (Right w) -> pure w
    reports <- readServeReports
    pure (result, w, reports)

{- | Stop the services, then end the loop with an explicit @quit@ rather than
by closing the pipe: a spawned child inherits the pipe's write end, so
closing our copy does not necessarily produce an EOF for the loop to see.
-}
shutdown :: Supervisor -> Handle -> IO ()
shutdown sup writeEnd = do
    Sup.stopAll silent sup
    hPutStrLn writeEnd "quit"
    hClose writeEnd

-- | Polls a condition until it holds or ~5s have passed.
waitUntil :: IO Bool -> IO Bool
waitUntil cond = go (500 :: Int)
  where
    go :: Int -> IO Bool
    go 0 = cond
    go n = do
        ok <- cond
        if ok then pure True else threadDelay 10000 >> go (n - 1)

statusOf :: Supervisor -> Text -> IO (Maybe Status)
statusOf sup name = do
    sts <- Sup.supervisorStatus sup
    pure $ case [st | (_, st) <- sts, status_name st == name] of
        (st : _) -> Just st
        [] -> Nothing

running :: Supervisor -> Text -> IO Bool
running sup name = maybe False (isJust . status_pid) <$> statusOf sup name

pidOf :: Supervisor -> Text -> IO (Maybe ProcessID)
pidOf sup name = (>>= status_pid) <$> statusOf sup name

-- | @kill -0@: does a process by that id still exist?
processExists :: ProcessID -> IO Bool
processExists pid = do
    outcome <- try (signalProcess nullSignal pid)
    pure $ case outcome of
        Left (_ :: SomeException) -> False
        Right () -> True
