{-# LANGUAGE OverloadedStrings #-}

{- | 'Dag.stuck', the nodes a walk can never reach: unit cases, and QuickCheck
properties comparing the one-pass version with the rounds it replaced
('Test.DagReference.stuckRounds').

The graphs are the "Test.GraphFixture" shapes (acyclic by construction), the
same with extra random edges added (which make cycles, self-loops included),
random digraphs over a fixture's nodes, folds of fixtures whose 'Ref's
collide (so the 'Dag' carries 'Dag.Conflict's), and arbitrary @waitsOn@
functions that name 'Ref's the 'Dag' does not hold, or the same one twice.

Sizes stay small, up to a few hundred nodes, so the default run is cheap.
The depth-scaling numbers are in @resources\/graph-profiles.md@ (the
@graph-profiles@ benchmark); the one large case here is a loose guard, as in
"Test.GraphScaleSpec".
-}
module Test.DagStuckSpec (tests) where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Timeout (timeout)
import Test.QuickCheck (Gen, Property, choose, counterexample, elements, forAll, listOf, vectorOf, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Builtin.Extension (Extension (..), evalDeps)
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)

import Test.DagReference (stuckRounds)
import Test.GraphFixture

tests :: TestTree
tests =
    testGroup
        "Dag.stuck"
        [ testGroup
            "cases"
            [ testCase "an acyclic graph has nothing stuck" acyclic
            , testCase "a self-loop is stuck, and so is what waits on it" selfLoop
            , testCase "a two-node cycle is stuck both ways" twoCycle
            , testCase "a tail hanging off a cycle is stuck in one direction only" cycleWithTail
            , testCase "a wait on a Ref the Dag does not hold is no wait" unknownRef
            , testCase "a deep chain and a deep chain of diamonds answer in one pass" deep
            ]
        , testGroup
            "agrees with the rounds it replaced"
            [ testProperty "on the fixture shapes (acyclic): equal, and empty" propFixture
            , testProperty "on the fixture shapes with random extra edges (cycles)" propExtraEdges
            , testProperty "on random digraphs over a fixture's nodes" propRandomDigraph
            , testProperty "on folds with colliding Refs (conflicts)" propConflicts
            , testProperty "for an arbitrary waitsOn (unknown Refs, repeats)" propArbitraryWaits
            , testProperty "every node reported stuck reaches a cycle, and only those are" propReachesCycle
            ]
        ]

-------------------------------------------------------------------------------
-- building Dags

-- | @n@ nodes (fixture nodes @0 .. n-1@) and exactly these edges, given as
-- @(dependency, dependant)@ node numbers. Nodes come from a 'Fan', so they
-- carry real representatives; its own edges are dropped.
withEdges :: Int -> [(Int, Int)] -> Dag Extension
withEdges n edges =
    Dag.fromMagma
        (Dag.dagNodes (dagOf (generate Fan n 0)))
        (Set.fromList [(nodeRef d, nodeRef r) | (d, r) <- edges, d < n, r < n])

-- | A fixture's 'Dag' with extra edges between its nodes.
plus :: Dag Extension -> [(Int, Int)] -> Dag Extension
plus dag extra =
    Dag.fromMagma
        (Dag.dagNodes dag)
        (Dag.dagEdges dag <> Set.fromList [(nodeRef d, nodeRef r) | (d, r) <- extra])

refs :: [Int] -> Set Ref
refs = Set.fromList . fmap nodeRef

directions :: [(String, Dag Extension -> Ref -> [Ref])]
directions = [("up", Dag.dependenciesOf), ("down", Dag.dependantsOf)]

-- | Both directions agree with the reference.
agrees :: Dag Extension -> Property
agrees dag =
    foldr1
        (.&&.)
        [ counterexample dir (Dag.stuck waitsOn dag === stuckRounds waitsOn dag)
        | (dir, waitsOn) <- directions
        ]

-------------------------------------------------------------------------------
-- cases

acyclic :: IO ()
acyclic = do
    let dag = dagOf (generate (Diamonds 2) 31 0)
    assertEqual "going up" Set.empty (Dag.stuck Dag.dependenciesOf dag)
    assertEqual "coming down" Set.empty (Dag.stuck Dag.dependantsOf dag)
    assertEqual "the empty Dag" Set.empty (Dag.stuck Dag.dependenciesOf (Dag.emptyDag :: Dag Extension))

-- node 1 waits on itself; node 0 depends on 1; 1 depends on 2.
selfLoop :: IO ()
selfLoop = do
    let dag = withEdges 3 [(1, 1), (1, 0), (2, 1)]
    assertEqual "up: the loop and what stands on it" (refs [0, 1]) (Dag.stuck Dag.dependenciesOf dag)
    assertEqual "down: the loop and what it stands on" (refs [1, 2]) (Dag.stuck Dag.dependantsOf dag)
    assertEqual "as the rounds" (stuckRounds Dag.dependenciesOf dag) (Dag.stuck Dag.dependenciesOf dag)

twoCycle :: IO ()
twoCycle = do
    let dag = withEdges 3 [(0, 1), (1, 0)]
    assertEqual "up" (refs [0, 1]) (Dag.stuck Dag.dependenciesOf dag)
    assertEqual "down" (refs [0, 1]) (Dag.stuck Dag.dependantsOf dag)

-- a cycle 1 <-> 2, node 0 standing on it (0 depends on 1) and node 3 below
-- it (2 depends on 3), and 4 off to the side.
cycleWithTail :: IO ()
cycleWithTail = do
    let dag = withEdges 5 [(1, 2), (2, 1), (1, 0), (3, 2)]
    assertEqual "up: what stands on the cycle waits on it" (refs [0, 1, 2]) (Dag.stuck Dag.dependenciesOf dag)
    assertEqual "down: what the cycle stands on waits on it" (refs [1, 2, 3]) (Dag.stuck Dag.dependantsOf dag)

unknownRef :: IO ()
unknownRef = do
    let dag = withEdges 2 [(1, 0)]
        waitsOn d r = nodeRef 99 : nodeRef 99 : Dag.dependenciesOf d r
    assertEqual "nothing stuck" Set.empty (Dag.stuck waitsOn dag)
    assertEqual "as the rounds" (stuckRounds waitsOn dag) (Dag.stuck waitsOn dag)

{- | Thirty thousand levels: the rounds took about 2 s at three thousand and
grew with the square of the depth, so this would take minutes; one pass takes
a fraction of a second. The bound is loose, as in "Test.GraphScaleSpec": it
tells one pass from rounds, it times nothing.
-}
deep :: IO ()
deep =
    mapM_
        ( \shape -> do
            let dag = dagOf (generate shape 30001 0)
                k = length (Dag.dagOrder dag)
                -- the bottom node now also depends on the node three above
                -- it: a cycle of four at the very bottom, under everything.
                cyclic = plus dag [(k - 4, k - 1)]
            n <- within (show shape) (evaluate (Set.size (Dag.stuck Dag.dependenciesOf dag)))
            assertEqual (show shape <> ": nothing stuck") 0 n
            up <- within (show shape <> " with a cycle, up") (evaluate (Set.size (Dag.stuck Dag.dependenciesOf cyclic)))
            assertEqual (show shape <> ": up, everything stands on the cycle") k up
            down <- within (show shape <> " with a cycle, down") (evaluate (Set.size (Dag.stuck Dag.dependantsOf cyclic)))
            assertEqual (show shape <> ": down, only the cycle itself") 4 down
        )
        [Chain, Diamonds 2]
  where
    within what act = do
        outcome <- timeout (60 * 1000000) act
        maybe (assertFailure (what <> ": still running after 60s")) pure outcome

-------------------------------------------------------------------------------
-- properties

genShape :: Gen Shape
genShape =
    elements
        [Chain, Fan, Tree 2, Tree 3, Diamonds 2, Diamonds 3, Layered 3 2, Layered 4 4, RandomLayered 5 3]

-- | A shape, a size, a seed, and the node count actually realised.
genFixture :: Gen (Shape, Int, Seed)
genFixture = (,,) <$> genShape <*> choose (1, 200) <*> (fromIntegral <$> choose (0 :: Int, 1000))

nodeCount :: Shape -> Int -> Int
nodeCount shape n = fromIntegral (counts shape n).countNodes

propFixture :: Property
propFixture = forAll genFixture $ \(shape, n, s) ->
    let dag = dagOf (generate shape n s)
     in counterexample (show (shape, n, s)) $
            agrees dag .&&. Dag.stuck Dag.dependenciesOf dag === Set.empty .&&. Dag.stuck Dag.dependantsOf dag === Set.empty

propExtraEdges :: Property
propExtraEdges = forAll genFixture $ \(shape, n, s) ->
    let k = nodeCount shape n
     in forAll (choose (1, 4)) $ \m ->
            forAll (vectorOf m ((,) <$> choose (0, k - 1) <*> choose (0, k - 1))) $ \extra ->
                counterexample (show (shape, n, s)) (agrees (plus (dagOf (generate shape n s)) extra))

propRandomDigraph :: Property
propRandomDigraph = forAll (choose (1, 40)) $ \n ->
    forAll (listOf ((,) <$> choose (0, n - 1) <*> choose (0, n - 1))) $ \edges ->
        agrees (withEdges n edges)

propConflicts :: Property
propConflicts = forAll genFixture $ \(shape, n, s) ->
    forAll (elements [1 / 10, 1 / 3, 1]) $ \fraction ->
        let opts = defaultOptions{optCollisions = fraction}
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps (generateWith opts shape (min n 60) s)) :: Dag Extension
         in counterexample (show (shape, min n 60, s, fraction, length (Dag.dagConflicts dag))) (agrees dag)

-- waitsOn from a random table: node numbers up to twice the node count, so
-- some name nodes the Dag does not hold, and repeats allowed.
propArbitraryWaits :: Property
propArbitraryWaits = forAll (choose (1, 40)) $ \n ->
    forAll (vectorOf n (listOf (choose (0, 2 * n)))) $ \table ->
        let dag = withEdges n []
            m = Map.fromList (zip (fmap nodeRef [0 ..]) (fmap (fmap nodeRef) table))
            waitsOn _ r = Map.findWithDefault [] r m
         in Dag.stuck waitsOn dag === stuckRounds waitsOn dag

{- | Stuck exactly when the node reaches a cycle along @waitsOn@: some node it
can reach (itself included) can reach itself again in one step or more.
Decided by plain reachability, sharing nothing with either version.
-}
propReachesCycle :: Property
propReachesCycle = forAll (choose (1, 30)) $ \n ->
    forAll (listOf ((,) <$> choose (0, n - 1) <*> choose (0, n - 1))) $ \edges ->
        let dag = withEdges n edges
         in foldr1
                (.&&.)
                [ counterexample dir (Dag.stuck waitsOn dag === Set.filter (reachesCycle (waitsOn dag)) (Set.fromList (Dag.dagOrder dag)))
                | (dir, waitsOn) <- directions
                ]
  where
    reachesCycle next r = any (\c -> Set.member c (reach next (next c))) (Set.toList (reach next [r]))

-- | Everything reachable from the starting points, the starting points included.
reach :: (Ref -> [Ref]) -> [Ref] -> Set Ref
reach next = go Set.empty
  where
    go seen [] = seen
    go seen (x : xs)
        | Set.member x seen = go seen xs
        | otherwise = go (Set.insert x seen) (next x <> xs)
