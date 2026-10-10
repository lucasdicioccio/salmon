{-# LANGUAGE OverloadedStrings #-}

{- | A cheap guard on how the graph operations scale: a few of them over the
shapes that share nodes ("Test.GraphFixture"), at a size where an operation
that pays per node answers in well under a second and one that pays per
path never answers at all.

The bounds are deliberately loose (a minute where a few milliseconds to a
second are expected): this suite shares its process and its machine, and
the point is to tell "per node" from "per path", not to time anything. The
measurements themselves are the @graph-profiles@ benchmark and
@resources\/graph-profiles.md@.

What is __not__ guarded, because it does pay per path today:
'Salmon.Op.Dag.foldDag', and so declaring a graph in @serve@ and every
command-line entry that folds. That is why the walks here are given a 'Dag'
built by 'dagOf', and why 'dagOf' is checked against the fold first.
-}
module Test.GraphScaleSpec (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Concurrent as Concurrent
import qualified Salmon.Actions.Query as Query
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..), evalDeps)
import qualified Salmon.Op.Dag as Dag
import Salmon.Reporter (silent)

import Test.GraphFixture

tests :: TestTree
tests =
    testGroup
        "graph operations at scale"
        [ testCase "dagOf builds what foldDag builds" dagOfMatchesTheFold
        , testCase "the outline, a selector and the listed paths of a shared graph are per node" outlineIsPerNode
        , testCase "rebuilding and walking a shared graph is per node" walksArePerNode
        ]

-------------------------------------------------------------------------------

-- | The shapes whose expansion is exponentially larger than the graph.
shared :: [Shape]
shared = [Diamonds 2, Layered 8 2, RandomLayered 8 3]

{- | Two hundred diamonds: six hundred nodes and @2^200@ paths. Enough
that anything following paths cannot finish, small enough that the one
quadratic step in these walks ('Dag.stuck', see the profile) stays cheap.
-}
size :: Int
size = 601

-- | Generous: see the module header.
bound :: Int
bound = 60

-- | Fail, rather than hang the suite, if the action is still running at 'bound'.
within :: String -> IO a -> IO a
within what act = do
    outcome <- timeout (bound * 1000000) act
    case outcome of
        Just x -> pure x
        Nothing -> assertFailure (what <> ": still running after " <> show bound <> "s, which is what paying per path looks like")

-------------------------------------------------------------------------------

dagOfMatchesTheFold :: IO ()
dagOfMatchesTheFold =
    forM_ ([Chain, Fan, Tree 2, Tree 3, Layered 3 3] <> shared) $ \shape ->
        forM_ [1, 2, 7, 13, 25] $ \n -> do
            let label = show shape <> " at " <> show n
                folded = Dag.foldDag Dag.sameRepresentative (evalDeps (generate shape n 3)) :: Dag.Dag Extension
                built = dagOf (generate shape n 3)
            assertEqual (label <> ": order") (Dag.dagOrder folded) (Dag.dagOrder built)
            assertEqual (label <> ": dependencies") (Dag.dagDependencies folded) (Dag.dagDependencies built)
            assertEqual (label <> ": dependants") (fmap Set.fromList (Dag.dagDependants folded)) (fmap Set.fromList (Dag.dagDependants built))
            assertEqual (label <> ": nodes") (Map.keysSet (Dag.dagNodes folded)) (Map.keysSet (Dag.dagNodes built))

outlineIsPerNode :: IO ()
outlineIsPerNode =
    forM_ shared $ \shape -> do
        let label = show shape <> " at " <> show size
            expected = fromIntegral (counts shape size).countNodes
            cograph = evalDeps (generate shape size 3)
        nodes <- within (label <> ": outline") (evaluate (Set.size (Query.outlineRefs (Query.outline cograph))))
        assertEqual (label <> ": every node outlined") expected nodes
        selected <- within (label <> ": resolveSelectors") (evaluate (Set.size (fst (Query.resolveSelectors cograph ["**"] ["n0/n1"]))))
        assertEqual (label <> ": every node but one selected") (expected - 1) selected
        listed <- within (label <> ": outlinePaths") (evaluate (Map.foldl' (\k ps -> k + length ps) 0 (Query.outlinePaths Query.pathLimit (Query.outline cograph))))
        assertBool (label <> ": at least one path per node, at most pathLimit") (listed >= expected && listed <= expected * Query.pathLimit)

walksArePerNode :: IO ()
walksArePerNode =
    forM_ shared $ \shape -> do
        let label = show shape <> " at " <> show size
            expected = fromIntegral (counts shape size).countNodes
            dag = dagOf (generate shape size 3)
        rebuilt <- within (label <> ": fromMagma") (evaluate (Dag.fromMagma (Dag.dagNodes dag) (Dag.dagEdges dag)))
        assertEqual (label <> ": every node rebuilt") expected (Map.size (Dag.dagNodes rebuilt))
        assertEqual (label <> ": every edge rebuilt") (Dag.dagEdges dag) (Dag.dagEdges rebuilt)
        merged <- within (label <> ": mergeDag") (evaluate (Dag.mergeDag Dag.sameRepresentative rebuilt dag))
        assertEqual (label <> ": merging a graph onto itself adds nothing") (Dag.dagEdges dag) (Dag.dagEdges merged)
        cyclic <- within (label <> ": stuck") (evaluate (Set.size (Dag.stuck Dag.dependenciesOf rebuilt)))
        assertEqual (label <> ": nothing on a cycle") 0 cyclic
        up <- within (label <> ": upDag") (UpDown.upDag UpDown.alwaysRequired silent rebuilt)
        assertBool (label <> ": the sequential bring-up succeeds") up
        down <- within (label <> ": downDag") (UpDown.downDag UpDown.alwaysRequired silent rebuilt)
        assertBool (label <> ": the sequential teardown succeeds") down
        upConcurrent <- within (label <> ": upDagConcurrent") (Concurrent.upDagConcurrent UpDown.alwaysRequired silent Concurrent.noMailboxes Nothing rebuilt)
        assertBool (label <> ": the concurrent bring-up succeeds") upConcurrent
