{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Op.Contained": a node whose failure is
reported but fails nothing beyond itself, under both one-shot drivers.

The shape every case uses is a monitoring agent on a machine: the agent
(marked contained) depends on its repository, the machine's pass depends on
the agent and on a service, and the root on the machine. A failing repository
must not hold the service or the machine back, and must not fail the pass.
-}
module Test.ContainedSpec (tests) where

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Actions.Concurrent as Concurrent
import Salmon.Actions.UpDown (Report (..), alwaysRequired)
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..), Op, amendNode, contained, deps, evalDeps, nodeps, op)
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Contained (containment)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref, mkRef)

import Test.Harness (capture)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.Contained"
        [ testGroup
            "containment (pure)"
            [ testCase "a marked node is contained, and what failed only on its account" pureBelow
            , testCase "a top that is not marked is not contained" pureTop
            , testCase "a node with a dependant that came up is not contained" pureCleanDependant
            , testCase "a cycle never contains itself" pureCycle
            ]
        , driverCases "run up (sequential)" runSeq
        , driverCases "upDagConcurrent" runConc
        , testCase "teardown is unchanged: a contained node whose down failed blocks its predecessors" teardownUnchanged
        , testCase "marking a node contained leaves its representative unchanged" representativeUnchanged
        ]

-------------------------------------------------------------------------------

r :: Text -> Ref
r = mkRef "contained-spec"

pureBelow :: IO ()
pureBelow = do
    -- repo <- agent (marked) <- machine
    let dependants x
            | x == r "repo" = [r "agent"]
            | x == r "agent" = [r "machine"]
            | otherwise = []
    assertEqual
        "repo and agent"
        (Set.fromList [r "repo", r "agent"])
        (containment (== r "agent") dependants (Set.fromList [r "repo", r "agent"]))

pureTop :: IO ()
pureTop = do
    let dependants x = if x == r "repo" then [r "root"] else []
    assertEqual "nothing" Set.empty (containment (const False) dependants (Set.fromList [r "repo", r "root"]))

pureCleanDependant :: IO ()
pureCleanDependant = do
    -- repo's dependants: agent (marked, failed) and needy (came up: not in notOk)
    let dependants x = if x == r "repo" then [r "agent", r "needy"] else []
    assertEqual
        "agent only"
        (Set.fromList [r "agent"])
        (containment (== r "agent") dependants (Set.fromList [r "repo", r "agent"]))

pureCycle :: IO ()
pureCycle = do
    let dependants x
            | x == r "a" = [r "b"]
            | x == r "b" = [r "a"]
            | otherwise = []
    assertEqual "nothing" Set.empty (containment (const False) dependants (Set.fromList [r "a", r "b"]))

-------------------------------------------------------------------------------

type Runner = Op -> IO ([Report Extension], Bool)

dagOf :: Op -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps

runSeq :: Runner
runSeq o = do
    (rep, readBack) <- capture
    ok <- UpDown.upDag alwaysRequired rep (dagOf o)
    (,) <$> readBack <*> pure ok

runConc :: Runner
runConc o = do
    (rep, readBack) <- capture
    ok <- Concurrent.upDagConcurrent alwaysRequired rep Concurrent.noMailboxes Nothing (dagOf o)
    (,) <$> readBack <*> pure ok

within :: Int -> IO a -> IO a
within seconds act = do
    result <- timeout (seconds * 1000000) act
    case result of
        Just a -> pure a
        Nothing -> fail ("timed out after " <> show seconds <> "s")

-- | A node recording its name when its @up@ runs, failing after that if asked.
node :: (Text -> IO ()) -> Text -> Bool -> [Op] -> Op
node rec name failing preds =
    op name (if null preds then nodeps else deps preds) $ \x ->
        x
            { ref = r name
            , up = rec name >> (if failing then ioError (userError (show name <> " failed")) else pure ())
            }

-- | The machine: repo <- agent <- machine <- root, and svc <- machine.
-- @extra@ is added under root (for a node that explicitly needs the repo).
machine :: (Text -> IO ()) -> Bool -> (Op -> Op) -> Bool -> (Op -> [Op]) -> Op
machine rec repoFails markAgent agentFails extra =
    let repo = node rec "repo" repoFails []
        agent = markAgent (node rec "agent" agentFails [repo])
        svc = node rec "svc" False []
        mach = node rec "machine" False [agent, svc]
     in node rec "root" False (mach : extra repo)

driverCases :: String -> Runner -> TestTree
driverCases name run =
    testGroup
        name
        [ testCase "a failure below a contained node blocks nothing above it and fails nothing" $ within 10 $ do
            (logRef, rec) <- recorder
            (reports, ok) <- run (machine rec True (amendNode contained) False (const []))
            ran <- readIORef logRef
            assertBool "the pass succeeds" ok
            assertEqual "repo failed" ["repo"] [a.shorthand | Failed a _ <- reports]
            assertEqual "agent is blocked" ["agent"] [a.shorthand | Blocked a <- reports]
            assertEqual "both reported contained" (sort ["repo", "agent"]) (sort [a.shorthand | Contained a <- reports])
            assertEqual "everything else ran" (sort ["repo", "svc", "machine", "root"]) (sort ran)
        , testCase "a contained node failing itself blocks nothing" $ within 10 $ do
            (logRef, rec) <- recorder
            (reports, ok) <- run (machine rec False (amendNode contained) True (const []))
            ran <- readIORef logRef
            assertBool "the pass succeeds" ok
            assertEqual "agent reported contained" ["agent"] [a.shorthand | Contained a <- reports]
            assertEqual "everything ran" (sort ["repo", "agent", "svc", "machine", "root"]) (sort ran)
        , testCase "a node that explicitly depends inside the section is blocked, and the pass fails" $ within 10 $ do
            (logRef, rec) <- recorder
            let needy repo = [node rec "needy" False [repo]]
            (reports, ok) <- run (machine rec True (amendNode contained) False needy)
            ran <- readIORef logRef
            assertBool "the pass fails" (not ok)
            assertEqual "needy and root are blocked, agent too" (sort ["agent", "needy", "root"]) (sort [a.shorthand | Blocked a <- reports])
            assertEqual "only the agent's own failure is contained" ["agent"] [a.shorthand | Contained a <- reports]
            assertBool "machine still ran" ("machine" `elem` ran)
        , testCase "without the marker nothing changes: the failure blocks the machine and fails the pass" $ within 10 $ do
            (logRef, rec) <- recorder
            (reports, ok) <- run (machine rec True id False (const []))
            ran <- readIORef logRef
            assertBool "the pass fails" (not ok)
            assertEqual "agent, machine and root are blocked" (sort ["agent", "machine", "root"]) (sort [a.shorthand | Blocked a <- reports])
            assertEqual "no contained report" [] [a.shorthand | Contained a <- reports]
            assertEqual "only repo and svc ran" (sort ["repo", "svc"]) (sort ran)
        , testCase "a clean pass with a contained node says nothing more" $ within 10 $ do
            (_, rec) <- recorder
            (reports, ok) <- run (machine rec False (amendNode contained) False (const []))
            assertBool "the pass succeeds" ok
            assertEqual "no contained report" [] [a.shorthand | Contained a <- reports]
        ]
  where
    recorder = do
        logRef <- newIORef []
        pure (logRef, \n -> atomicModifyIORef' logRef (\xs -> (n : xs, ())))

-------------------------------------------------------------------------------

teardownUnchanged :: IO ()
teardownUnchanged = within 10 $ do
    let repo = op "repo" nodeps $ \x -> x{ref = r "repo", down = pure ()}
        agent = amendNode contained $ op "agent" (deps [repo]) $ \x -> x{ref = r "agent", down = ioError (userError "still standing")}
        root = op "root" (deps [agent]) $ \x -> x{ref = r "root", down = pure ()}
    (rep, readBack) <- capture
    ok <- UpDown.downDag alwaysRequired rep (dagOf root)
    reports <- readBack
    assertBool "the teardown fails" (not ok)
    assertEqual "repo is blocked" ["repo"] [a.shorthand | Blocked a <- reports]
    assertEqual "no contained report" [] [a.shorthand | Contained a <- reports]
    (repC, readBackC) <- capture
    okC <- Concurrent.downDagConcurrent alwaysRequired repC Concurrent.noMailboxes Nothing (dagOf root)
    reportsC <- readBackC
    assertBool "concurrently too" (not okC)
    assertEqual "repo is blocked, concurrently" ["repo"] [a.shorthand | Blocked a <- reportsC]

representativeUnchanged :: IO ()
representativeUnchanged = do
    let plain = op "agent" nodeps $ \x -> x{ref = r "agent"}
        marked = amendNode contained plain
        reps o = [Dag.representative a | Just a <- map (Dag.representativeOf (dagOf o)) (Dag.dagOrder (dagOf o))]
    assertEqual "same representative" (reps plain) (reps marked)
    assertEqual "marking twice is marking once" (length (actDynamics (amendNode contained marked))) (length (actDynamics marked))
  where
    actDynamics o = [d | a <- Dag.dagOrder (dagOf o), Just act <- [Dag.representativeOf (dagOf o) a], d <- act.extension.dynamics]
