{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0: an asynchronous exception reaching a node's @up@, @check@ or
@down@ (a 'cancel' of the thread running the pass, a 'timeout' around it, a
'killThread' of an upkeep machine) ends the pass with that exception. It is
not caught and reported as the node's 'Failed' while the walk carries on,
which is what every driver did before 'UpDown.trySync'. A synchronous
failure is still the node's 'Failed', as before.

Every node that is interrupted here blocks on an 'MVar' after saying it has
started, so the interruption always lands inside it.
-}
module Test.AsyncInterruptSpec (tests) where

import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), async, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar)
import Control.Exception (AsyncException (ThreadKilled), fromException)
import Control.Monad (void)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Concurrent as Concurrent
import Salmon.Actions.UpDown (CheckResult (..), Report (..), alwaysRequired)
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.Upkeep (Standing (..), Tend (..))
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Extension (..), Op, deps, evalDeps, nodeps, op)
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Status (Direction (..))
import Salmon.Reporter (Reporter, ReporterM (..))

import Test.Harness (capture)

tests :: TestTree
tests =
    testGroup
        "asynchronous exceptions are not a node's failure"
        [ testGroup "run up (sequential)" (passCases upSeq)
        , testGroup "upDagConcurrent" (passCases upConc)
        , testGroup "run down (sequential)" (downCases downSeq)
        , testGroup "downDagConcurrent" (downCases downConc)
        , testCase "a cancel landing in a node's check ends the pass" checkCancelled
        , testCase "a timeout around the pass ends it, the node is not Failed" timeoutAround
        , testCase "a synchronous failure is still the node's Failed" syncStillFails
        , testCase "upkeep: a machine killed inside up is not reported Failed" upkeepKilled
        ]

-------------------------------------------------------------------------------

r :: Text -> Ref
r = mkRef "async-interrupt-spec"

dagOf :: Op -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps

within :: Int -> IO a -> IO a
within seconds act = do
    result <- timeout (seconds * 1000000) act
    case result of
        Just a -> pure a
        Nothing -> fail ("timed out after " <> show seconds <> "s")

type Pass = Reporter (Report Extension) -> Dag.Dag Extension -> IO Bool

upSeq, upConc, downSeq, downConc :: Pass
upSeq rep = UpDown.upDag alwaysRequired rep
upConc rep = Concurrent.upDagConcurrent alwaysRequired rep Concurrent.noMailboxes Nothing
downSeq rep = UpDown.downDag alwaysRequired rep
downConc rep = Concurrent.downDagConcurrent alwaysRequired rep Concurrent.noMailboxes Nothing

{- | Runs @pass@ over @graph@ in its own thread, waits for @started@, cancels
it, and returns how it ended, the reports, and the nodes that ran.
-}
cancelInside :: Pass -> IO () -> Dag.Dag Extension -> IO (Either String Bool, [Report Extension])
cancelInside pass started dag = do
    (rep, readBack) <- capture
    running <- async (pass rep dag)
    started
    cancel running
    ended <- waitCatch running
    reports <- readBack
    let how = case ended of
            Left e | Just AsyncCancelled <- fromException e -> Left "cancelled"
            Left e -> Left ("another exception: " <> show e)
            Right ok -> Right ok
    pure (how, reports)

-- | A node that says it has started, then waits forever.
stuck :: IO () -> IO ()
stuck entered = entered >> newEmptyMVar >>= takeMVar

passCases :: Pass -> [TestTree]
passCases pass =
    [ testCase "a cancel inside up ends the pass with the cancel" $ within 10 $ do
        entered <- newEmptyMVar
        ranRef <- newIORef []
        let rec n = atomicModifyIORef' ranRef (\xs -> (n : xs, ()))
            slow = op "slow" nodeps $ \x -> x{ref = r "slow", up = stuck (putMVar entered ())}
            after = op "after" (deps [slow]) $ \x -> x{ref = r "after", up = rec ("after" :: Text)}
        (how, reports) <- cancelInside pass (takeMVar entered) (dagOf after)
        ran <- readIORef ranRef
        assertEqual "the pass ended with the cancel" (Left "cancelled") how
        assertEqual "nothing reported Failed" [] [a.shorthand | Failed a _ <- reports]
        assertEqual "nothing reported Blocked" [] [a.shorthand | Blocked a <- reports]
        assertEqual "the dependant never ran" [] ran
    ]

downCases :: Pass -> [TestTree]
downCases pass =
    [ testCase "a cancel inside down ends the pass with the cancel" $ within 10 $ do
        entered <- newEmptyMVar
        let base = op "base" nodeps $ \x -> x{ref = r "base", down = pure ()}
            top = op "top" (deps [base]) $ \x -> x{ref = r "top", down = stuck (putMVar entered ())}
        (how, reports) <- cancelInside pass (takeMVar entered) (dagOf top)
        assertEqual "the pass ended with the cancel" (Left "cancelled") how
        assertEqual "nothing reported Failed" [] [a.shorthand | Failed a _ <- reports]
        assertEqual "nothing reported Blocked" [] [a.shorthand | Blocked a <- reports]
    ]

checkCancelled :: IO ()
checkCancelled = within 10 $ do
    entered <- newEmptyMVar
    let slow =
            op "slow" nodeps $ \x ->
                x{ref = r "slow", check = stuck (putMVar entered ()) >> pure Success}
    (how, reports) <- cancelInside upSeq (takeMVar entered) (dagOf slow)
    assertEqual "the pass ended with the cancel" (Left "cancelled") how
    -- before, the check's cancel became 'Failure' and @up@ was then run
    assertEqual "up was not evaluated" [] [a.shorthand | Eval a <- reports]

timeoutAround :: IO ()
timeoutAround = within 10 $ do
    let slow = op "slow" nodeps $ \x -> x{ref = r "slow", up = threadDelay 10000000}
    (rep, readBack) <- capture
    ended <- timeout 100000 (upSeq rep (dagOf slow))
    reports <- readBack
    assertBool "the timeout ended the pass" (not (isJust ended))
    assertEqual "nothing reported Failed" [] [a.shorthand | Failed a _ <- reports]

syncStillFails :: IO ()
syncStillFails = within 10 $ do
    let bad = op "bad" nodeps $ \x -> x{ref = r "bad", up = ioError (userError "no")}
        after = op "after" (deps [bad]) $ \x -> x{ref = r "after", up = pure ()}
    (rep, readBack) <- capture
    ok <- upSeq rep (dagOf after)
    reports <- readBack
    assertBool "the pass fails" (not ok)
    assertEqual "bad is Failed" ["bad"] [a.shorthand | Failed a _ <- reports]
    assertEqual "after is Blocked" ["after"] [a.shorthand | Blocked a <- reports]
    (repC, readBackC) <- capture
    okC <- upConc repC (dagOf after)
    reportsC <- readBackC
    assertBool "concurrently too" (not okC)
    assertEqual "bad is Failed, concurrently" ["bad"] [a.shorthand | Failed a _ <- reportsC]

{- | A machine's thread killed while its @up@ is in flight dies with the kill:
before, the machine caught it as the node's 'Failed', counted it toward its
restart budget and carried on tending.
-}
upkeepKilled :: IO ()
upkeepKilled = within 10 $ do
    entered <- newEmptyMVar
    trace <- newTVarIO []
    let rep = ReporterM $ \x -> atomically (modifyTVar' trace (x :))
        slow = op "slow" nodeps $ \x -> x{ref = r "slow", up = stuck (putMVar entered ())}
    sup <- Upkeep.startUpkeep rep Upkeep.noKept (const (Just (Tend TurnUp Unsettled))) (dagOf slow)
    takeMVar entered
    case Map.lookup (r "slow") (Upkeep.supervisorThreads sup) of
        Nothing -> assertFailure "no machine for the node"
        Just tid -> killThread tid
    void (Upkeep.stopUpkeep sup)
    reports <- reverse <$> atomically (readTVar trace)
    assertEqual "nothing reported Failed" [] [a.shorthand | Upkeep.Acted (Failed a _) <- reports]
    let escaped = [e | Upkeep.Escaped _ e <- reports]
    assertBool "the kill is what ended the machine" (not (null escaped))
    assertBool "and it was the kill" (all (\e -> fromException e == Just ThreadKilled) escaped)
