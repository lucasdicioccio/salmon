{-# LANGUAGE OverloadedStrings #-}

{- | Layer 1 coverage for "Salmon.Builtin.Nodes.Daemon": real subprocesses.

@Test.UpkeepSpec@ covers the state machine with a plain @IO ExitCode@ standing
in for a process, because the racing, the policy and the failure accounting
are not clearer through a real one. What is only visible through a real one is
everything this module is about: that cancelling the machine actually kills
the process, that a process ignoring @SIGTERM@ is escalated to @SIGKILL@
rather than wedging the teardown behind it, that the whole process /group/
goes rather than just the leader, and that output reaches the node's ring
while the process is still running.

Every case here spawns @\/bin\/sh@ and takes under a second. Nothing needs
root, a container, or anything on the machine but a shell.
-}
module Test.DaemonSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (try)
import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (proc)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Actions.Upkeep as Upkeep
-- imported with their field selectors: OverloadedRecordDot only solves
-- HasField for fields whose selector is in scope.
import Salmon.Builtin.Extension (Extension, Op, check, down, dynamics, evalDeps, help, managed, nodeps, notes, op, ref, up)
import qualified Salmon.Builtin.Nodes.Daemon as Daemon
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Status (Direction (..))
import Salmon.Op.Supervision (millis)
import Salmon.Reporter (ReporterM (..), silent)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Daemon"
        [ testCase "the process runs, and stops when the machine is cancelled" runsAndStops
        , testCase "a process ignoring SIGTERM is escalated to SIGKILL" escalatesToKill
        , testCase "the whole process group goes, not just the leader" killsTheGroup
        , testCase "output reaches the node while the process is still running" capturesOutput
        , testCase "a one-shot driver is told it cannot run this" upRefuses
        ]

-------------------------------------------------------------------------------

within :: Int -> IO a -> IO a
within secs act = do
    result <- timeout (secs * 1000000) act
    maybe (fail ("timed out after " <> show secs <> "s")) pure result

sh :: Text -> Daemon.Daemon
sh script = Daemon.defaultDaemon "test-daemon" (proc "/bin/sh" ["-c", Text.unpack script])

daemonOp :: Daemon.Daemon -> Op
daemonOp = Daemon.daemon silent

daemonRef :: Ref
daemonRef = mkRef "daemon" ("test-daemon" :: Text)

dagOf :: Op -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps

{- | Supervise this daemon, run the body, then stop — which cancels the
machine and so tears the process down through 'Daemon.runDaemon'\'s bracket.

The node is built here rather than taken from 'Daemon.daemon' for one reason:
the node's own output ring is not readable from a test, so this tees what
would go into it somewhere a case can block on. Everything else about the
node is what 'Daemon.daemon' would have produced, and 'upRefuses' covers that
function itself.
-}
supervising :: Daemon.Daemon -> (TVar [Text] -> IO a) -> IO a
supervising d body = do
    captured <- newTVarIO []
    let o =
            op "test-daemon" nodeps $ \x ->
                x
                    { ref = daemonRef
                    , managed = Just $ \sink ->
                        Daemon.runDaemon silent d $ \l -> do
                            sink l
                            atomically (modifyTVar' captured (l :))
                    }
    Upkeep.withUpkeep
        silent
        (const (Just (Upkeep.Tend TurnUp Upkeep.Unsettled)))
        (dagOf o)
        (const (body captured))

-- | Block until the captured output satisfies the predicate.
awaitLines :: TVar [Text] -> ([Text] -> Bool) -> IO ()
awaitLines v p = atomically (readTVar v >>= \ls -> unless (p (reverse ls)) retry)

-- | Signal 0: asks the kernel whether a pid exists, without sending anything.
alive :: ProcessID -> IO Bool
alive pid = do
    result <- try @IOError (signalProcess nullSignal pid)
    pure (either (const False) (const True) result)

-------------------------------------------------------------------------------

{- | The baseline: it really runs, and cancelling the machine really stops it.

Asserted through the process's own output rather than a pid, so it holds
without this test knowing anything about how the teardown works.
-}
runsAndStops :: IO ()
runsAndStops = within 20 $ do
    ls <- supervising (sh "while true; do echo tick; sleep 0.05; done") $ \ls -> do
        awaitLines ls (\xs -> length xs >= 2)
        pure ls
    -- the supervisor has stopped by here, so the loop is gone; if it were
    -- not, this file descriptor would still be being written to.
    before <- length <$> atomically (readTVar ls)
    -- it was writing a line every 50ms, so 400ms of silence is the process
    -- being gone rather than merely slow.
    _ <- timeout 400000 (awaitLines ls (\xs -> length xs > before))
    after <- length <$> atomically (readTVar ls)
    assertEqual "it stopped talking once the machine was cancelled" before after

{- | The case @cancel@ alone cannot handle, and the reason the escalation is
recovered from @f9d7116@ rather than left to 'System.Process.withCreateProcess':
a process that traps @SIGTERM@ and keeps going. Without the @SIGKILL@ this
teardown never returns.
-}
escalatesToKill :: IO ()
escalatesToKill = within 20 $ do
    let d = (sh "trap '' TERM; echo ignoring; while true; do sleep 0.05; done"){Daemon.daemon_stop = Daemon.Stop (Daemon.stop_signal Daemon.defaultStop) (millis 300)}
    -- the assertion is that this returns at all: `withUpkeep`'s release
    -- cancels the machine and waits for the teardown.
    ok <- timeout 10000000 $ supervising d $ \ls -> awaitLines ls (elem "ignoring")
    assertBool "the teardown escalated rather than waiting forever" (ok == Just ())

{- | A service that forks workers has to take them with it, which is why
@create_group@ is forced on and the signal goes to the group rather than to
'System.Process.terminateProcess'\'s single pid.

The shell prints its child's pid and then waits on it, so a teardown that
signalled only the leader would leave that child running.
-}
killsTheGroup :: IO ()
killsTheGroup = within 20 $ do
    pidVar <- newTVarIO Nothing
    _ <-
        supervising (sh "sleep 60 & echo child $!; wait") $ \ls -> do
            awaitLines ls (any ("child " `Text.isPrefixOf`))
            xs <- reverse <$> atomically (readTVar ls)
            atomically (writeTVar pidVar (childPid xs))
    child <- atomically (readTVar pidVar)
    case child of
        Nothing -> fail "the shell did not report its child's pid"
        Just pid -> do
            -- the group signal has been sent and waited for by the time
            -- `supervising` returned; the child's reparenting and reaping is
            -- the kernel's business and takes a moment.
            gone <- untilGone 40 pid
            assertBool "the forked child went with its parent" gone
  where
    childPid :: [Text] -> Maybe ProcessID
    childPid xs =
        case [w | l <- xs, ["child", w] <- [Text.words l]] of
            (w : _) -> case reads (Text.unpack w) :: [(Integer, String)] of
                [(n, "")] -> Just (fromInteger n)
                _ -> Nothing
            [] -> Nothing

    untilGone :: Int -> ProcessID -> IO Bool
    untilGone 0 _ = pure False
    untilGone n pid = do
        still <- alive pid
        if not still then pure True else threadDelay 25000 >> untilGone (n - 1) pid

{- | Reading the pipes has to happen /while/ the process runs, not after it
exits: a process that fills a pipe buffer otherwise blocks forever and the
node looks wedged for a reason nobody could see. This process never exits, so
nothing but concurrent draining could produce a line at all.
-}
capturesOutput :: IO ()
capturesOutput = within 20 $ do
    _ <- supervising (sh "echo one; echo two >&2; while true; do sleep 0.05; done") $ \ls ->
        awaitLines ls (\xs -> "one" `elem` xs && "two" `elem` xs)
    pure ()

{- | @run up@ has nowhere to put an action that never returns, so the node
says so rather than no-oping into a world that then believes it is up.
-}
upRefuses :: IO ()
upRefuses = within 10 $ do
    let dag = dagOf (daemonOp (sh "true"))
    case Dag.representativeOf dag daemonRef of
        Nothing -> fail "the daemon node is not in its own dag"
        Just act -> do
            assertBool "the node does declare a managed action" (isJust act.extension.managed)
            outcome <- try @Daemon.NeedsSupervisor act.extension.up
            case outcome of
                Left _ -> pure ()
                Right () -> fail "up should refuse rather than silently succeed"
