{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0/1 coverage for "Salmon.Actions.Concurrent": the same contract as
the sequential drivers, plus the things only a concurrent one has to get
right.

Three groups of case here. The first are the sequential drivers' own
guarantees restated — ordering, one application per 'Ref', failure
containment — because "same contract" is the whole claim being made. The
second is that independent nodes really do overlap, asserted by having them
block on each other rather than by timing. The third is what only appears
once nodes run at once: a cycle that no thread can ever get past, and a
mailbox delivering an operator's instruction into a running pass.
-}
module Test.ConcurrentSpec (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (elemIndex, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Actions.Concurrent as Concurrent
import Salmon.Actions.UpDown (CheckResult (..), Report (..), Requirement (..), alwaysRequired)
import Salmon.Builtin.Extension (Extension, Op, check, deps, down, dynamics, evalDeps, help, nodeps, notes, op, opAct, ref, up)
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..))
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.Ref (Ref, mkRef)

import Test.Harness (capture)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Concurrent"
        [ testCase "a predecessor is applied before its dependants" predecessorFirst
        , testCase "a shared node is applied exactly once" sharedNodeOnce
        , testCase "a failed node blocks its dependants, not its siblings" failureBlocksDependants
        , testCase "independent nodes really do run at the same time" trulyConcurrent
        , testCase "teardown waits for every dependant, concurrently" teardownWaitsForDependants
        , testCase "a cycle is reported rather than hanging" cycleDoesNotHang
        , testCase "a mailbox Force overrides a satisfied check" mailboxForces
        , testCase "a mailbox Satisfy stops a node acting" mailboxSatisfies
        , testCase "an overflowing mailbox drops the oldest and says so" mailboxOverflows
        ]

-------------------------------------------------------------------------------

dagOf :: Op -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps

runUp :: Op -> IO ([Report Extension], Bool)
runUp o = do
    (r, readBack) <- capture
    ok <- Concurrent.upDagConcurrent alwaysRequired r Concurrent.noMailboxes (dagOf o)
    (,) <$> readBack <*> pure ok

runDown :: Op -> IO ([Report Extension], Bool)
runDown o = do
    (r, readBack) <- capture
    ok <- Concurrent.downDagConcurrent alwaysRequired r Concurrent.noMailboxes (dagOf o)
    (,) <$> readBack <*> pure ok

-- | Fail the test rather than hanging forever if ordering deadlocks.
within :: Int -> IO a -> IO a
within seconds act = do
    result <- timeout (seconds * 1000000) act
    case result of
        Just a -> pure a
        Nothing -> fail ("timed out after " <> show seconds <> "s")

-------------------------------------------------------------------------------

predecessorFirst :: IO ()
predecessorFirst = within 10 $ do
    logRef <- newIORef []
    let rec name = atomicModifyIORef' logRef (\xs -> (name : xs, ()))
        shared = op "shared" nodeps $ \x -> x{ref = mkRef "leaf" ("shared" :: Text), up = rec "shared"}
        a = op "a" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("a" :: Text), up = rec "a"}
        b = op "b" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("b" :: Text), up = rec "b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" (), up = rec "root"}
    (_, ok) <- runUp root
    order <- reverse <$> readIORef logRef
    assertBool "clean" ok
    assertEqual "each node applied exactly once" (sort ["root", "a", "b", "shared"]) (sort order)
    assertEqual "the shared predecessor went first" (Just 0) (elemIndex "shared" order)
    assertEqual "and the root last" (Just 3) (elemIndex "root" order)

sharedNodeOnce :: IO ()
sharedNodeOnce = within 10 $ do
    logRef <- newIORef []
    let rec name = atomicModifyIORef' logRef (\xs -> (name : xs, ()))
        apex = op "apex" nodeps $ \x -> x{ref = mkRef "leaf" ("apex" :: Text), up = rec "apex"}
        left = op "left" (deps [apex]) $ \x -> x{ref = mkRef "mid" ("left" :: Text), up = rec "left"}
        right = op "right" (deps [apex]) $ \x -> x{ref = mkRef "mid" ("right" :: Text), up = rec "right"}
        root = op "root" (deps [left, right]) $ \x -> x{ref = mkRef "root" (), up = rec "root"}
    (reports, _) <- runUp root
    order <- readIORef logRef
    assertEqual "apex applied once" 1 (length (filter (== "apex") order))
    assertEqual "four nodes, four Evals" 4 (length [() | Eval _ <- reports])

failureBlocksDependants :: IO ()
failureBlocksDependants = within 10 $ do
    logRef <- newIORef []
    let rec name = atomicModifyIORef' logRef (\xs -> (name : xs, ()))
        a = op "a" nodeps $ \x -> x{ref = mkRef "mid" ("a" :: Text), up = rec "a" >> ioError (userError "boom")}
        b = op "b" nodeps $ \x -> x{ref = mkRef "mid" ("b" :: Text), up = rec "b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" (), up = rec "root"}
    (reports, ok) <- runUp root
    order <- readIORef logRef
    assertBool "the run reports failure" (not ok)
    assertBool "an unrelated sibling still ran" ("b" `elem` order)
    assertBool "the blocked dependant did not" ("root" `notElem` order)
    assertEqual "and is reported Blocked" ["root"] [act.shorthand | Blocked act <- reports]

{- | Two independent nodes, each of which will not finish until the /other/
has started. Under a sequential driver this deadlocks; under a concurrent one
it completes, which is the assertion. No sleeps and no timing: the overlap is
what makes it terminate at all.
-}
trulyConcurrent :: IO ()
trulyConcurrent = within 10 $ do
    aStarted <- newEmptyMVar
    bStarted <- newEmptyMVar
    let a = op "a" nodeps $ \x -> x{ref = mkRef "mid" ("a" :: Text), up = putMVar aStarted () >> readMVar bStarted}
        b = op "b" nodeps $ \x -> x{ref = mkRef "mid" ("b" :: Text), up = putMVar bStarted () >> readMVar aStarted}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" ()}
    (_, ok) <- runUp root
    assertBool "both nodes were in flight at once" ok

{- | The teardown ordering guarantee, under concurrency: the directory two
files live in must not be removed until both files are, and the two files may
go at the same time.
-}
teardownWaitsForDependants :: IO ()
teardownWaitsForDependants = within 10 $ do
    logRef <- newIORef []
    aStarted <- newEmptyMVar
    bStarted <- newEmptyMVar
    let rec name = atomicModifyIORef' logRef (\xs -> (name : xs, ()))
        shared = op "shared" nodeps $ \x -> x{ref = mkRef "leaf" ("shared" :: Text), down = rec "shared"}
        -- as above: neither finishes until both have started, so this only
        -- terminates if they overlap.
        a = op "a" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("a" :: Text), down = putMVar aStarted () >> readMVar bStarted >> rec "a"}
        b = op "b" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("b" :: Text), down = putMVar bStarted () >> readMVar aStarted >> rec "b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" (), down = rec "root"}
    (_, ok) <- runDown root
    order <- reverse <$> readIORef logRef
    assertBool "clean" ok
    assertEqual "the shared node came down last" (Just 3) (elemIndex "shared" order)
    assertEqual "and the root first" (Just 0) (elemIndex "root" order)

{- | A node on a cycle never becomes ready, so a driver that waits for
neighbours would wait forever rather than finishing and noticing. The cycle
has to be found before the walk.
-}
cycleDoesNotHang :: IO ()
cycleDoesNotHang = within 10 $ do
    let leaf name = op name nodeps $ \x -> x{ref = mkRef "cyc" (name :: Text)}
        magma =
            Map.fromList
                [ (act.extension.ref, act)
                | o <- [leaf "a", leaf "b"]
                , Just act <- [opAct o]
                ]
        looped = Set.fromList [(mkRef "cyc" ("a" :: Text), mkRef "cyc" ("b" :: Text)), (mkRef "cyc" ("b" :: Text), mkRef "cyc" ("a" :: Text))]
    (r, readBack) <- capture
    ok <- Concurrent.upDagConcurrent alwaysRequired r Concurrent.noMailboxes (Dag.fromMagma magma looped)
    reports <- readBack
    assertBool "reported as a failure rather than hanging" (not ok)
    assertEqual "both nodes named" 2 (length [() | Blocked _ <- reports])
    assertEqual "and neither evaluated" 0 (length [() | Eval _ <- reports])

-------------------------------------------------------------------------------

-- | A node whose check says it is already satisfied, so nothing runs unless
-- an operator says otherwise.
satisfied :: Text -> IO () -> Op
satisfied name action =
    op "target" nodeps $ \x ->
        x{ref = mkRef "target" name, check = pure Success, up = action, down = action}

withMailbox :: Op -> [Instruction] -> IO ([Report Extension], Bool)
withMailbox o instructions = do
    box <- Mailbox.newMailbox Mailbox.defaultCapacity
    mapM_ (Mailbox.post box) instructions
    (r, readBack) <- capture
    let dag = dagOf o
    ok <- Concurrent.upDagConcurrent alwaysRequired r (Map.fromList [(rf, box) | rf <- Dag.dagOrder dag]) dag
    (,) <$> readBack <*> pure ok

mailboxForces :: IO ()
mailboxForces = within 10 $ do
    ran <- newIORef (0 :: Int)
    let o = satisfied "forced" (atomicModifyIORef' ran (\n -> (n + 1, ())))
    (quiet, _) <- withMailbox o []
    assertEqual "with no instruction the satisfied check wins" 1 (length [() | Skip _ <- quiet])
    assertEqual "so nothing ran" 0 =<< readIORef ran

    (forced, _) <- withMailbox o [Force]
    assertEqual "Force overrides it" 1 (length [() | Eval _ <- forced])
    assertEqual "and the instruction is reported" [Force] [i | Instructed _ i <- forced]
    assertEqual "so it ran" 1 =<< readIORef ran

mailboxSatisfies :: IO ()
mailboxSatisfies = within 10 $ do
    ran <- newIORef (0 :: Int)
    let o =
            op "target" nodeps $ \x ->
                x{ref = mkRef "target" ("s" :: Text), up = atomicModifyIORef' ran (\n -> (n + 1, ()))}
    (plain, _) <- withMailbox o []
    assertEqual "a node with no check runs by default" 1 (length [() | Eval _ <- plain])
    (told, _) <- withMailbox o [Satisfy]
    assertEqual "Satisfy stops it" 1 (length [() | Skip _ <- told])
    assertEqual "so it ran only the first time" 1 =<< readIORef ran

{- | An instruction lost silently would make forcing a node unreliable in a
way nobody could see, so the eviction is reported.
-}
mailboxOverflows :: IO ()
mailboxOverflows = within 10 $ do
    box <- Mailbox.newMailbox 2
    results <- mapM (Mailbox.post box) [Recheck, Recheck, Recheck, Force]
    assertEqual "the first two fit, the next two evicted one each" [True, True, False, False] results
    assertEqual "two evictions recorded" 2 =<< Mailbox.dropped box
    held <- atomically (Mailbox.takeAll box)
    assertEqual "the newest survive, oldest first" [Recheck, Force] held
