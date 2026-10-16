{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for the edge lists of 'Salmon.Op.Dag' and
'Salmon.Actions.Query.Outline' (the profiles' finding 3): 'Dag.record',
'Dag.mergeDag', 'Dag.foldDag', 'Dag.fromMagma', 'Dag.transposeOf' and
'Query.outline' no longer search a node's edge list on each insert or append
to it on the right, and must give exactly what they gave before, edge order
included.

"Test.EdgeListReference" keeps the appending versions verbatim. The unit
cases pin the order on the cases that matter (a fan, a node over many, a
repeated edge, a node declared twice in either order); the properties
compare the current versions with the reference on random inputs and on
the fixture graphs; the last case is the scale guard, at a size the
appending versions could not finish in the bound.
-}
module Test.EdgeListSpec (tests) where

import Control.Exception (evaluate)
import Data.List (foldl', sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.QuickCheck (Gen, Property, chooseInt, conjoin, counterexample, elements, forAll, label, listOf, oneof, suchThat, vectorOf, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

import qualified Salmon.Actions.Query as Query
import Salmon.Builtin.Extension (Extension, Op, dynamics, evalDeps, help, nodeps, notes, op, opAct, ref)
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref, mkRef)

import Test.DagReference (foldDagPerOccurrenceWith)
import Test.EdgeListReference
import Test.GraphFixture

tests :: TestTree
tests =
    testGroup
        "edge lists (Dag.record, fromMagma, transposeOf, Query.outline)"
        [ testGroup
            "unit"
            [ testCase "a fan: the root's dependencies in declaration order, each leaf's one dependant" fanOrder
            , testCase "a node many depend on: its dependants in the order they were recorded" manyDependants
            , testCase "a repeated edge and a self-edge are recorded once, where first seen" repeatedEdges
            , testCase "a node declared twice keeps its first edges first, in either order" twoOrders
            , testCase "recording onto a built Dag extends its lists where they stood" ontoABuiltDag
            , testCase "fromMagma and transposeOf on a fan and a node many depend on" fromMagmaAndTranspose
            , testCase "the outline of a fan lists its children in the order they were met" outlineOfAFan
            ]
        , testGroup
            "equal to the appending versions"
            [ localOption (QuickCheckTests 2000) $
                testProperty "record: any run of records, from empty or onto a built Dag" prop_recordMatches
            , localOption (QuickCheckTests 1000) $
                testProperty "mergeDag: any two Dags" prop_mergeDagMatches
            , localOption (QuickCheckTests 1000) $
                testProperty "fromMagma: any magma and edge set" prop_fromMagmaMatches
            , localOption (QuickCheckTests 1000) $
                testProperty "transposeOf: any dependency map, repeats included" prop_transposeOfMatches
            , localOption (QuickCheckTests 500) $
                testProperty "foldDag and outline: the fixture graphs" prop_fixturesMatch
            ]
        , testGroup
            "laws"
            [ localOption (QuickCheckTests 1000) $
                testProperty "transposeOf twice is the identity on a closed map without repeats" prop_transposeTwice
            , localOption (QuickCheckTests 1000) $
                testProperty "fromMagma is unchanged by an edge already present, and inverts dagEdges" prop_fromMagmaIdempotent
            ]
        , testCase "a fan and a node many depend on, 100 000 nodes each, within a minute per operation" scaleGuard
        ]

-------------------------------------------------------------------------------
-- Nodes.

siteRef :: Int -> Ref
siteRef = mkRef "edgelistspec" . siteName

siteName :: Int -> Text
siteName s = "site" <> Text.pack (show s)

-- | The act of a node at a site, with one of a few descriptions (so two
-- declarations of a site can differ, which is a conflict).
actAt :: Int -> Int -> Act Extension
actAt site description =
    fromJust . opAct $
        op (siteName site) nodeps $ \x ->
            x{ref = siteRef site, help = "description " <> Text.pack (show description)}

-- | Everything a 'Dag.Dag' holds that can be compared: representatives by
-- their help, both edge maps and the order as they are, conflicts by ref and
-- the two helps.
type View = (Map Ref Text, Map Ref [Ref], Map Ref [Ref], [Ref], [(Ref, Text, Text)])

view :: Dag.Dag Extension -> View
view d =
    ( fmap helpOf (Dag.dagNodes d)
    , Dag.dagDependencies d
    , Dag.dagDependants d
    , Dag.dagOrder d
    , [(c.conflictRef, helpOf c.conflictKept, helpOf c.conflictReplaced) | c <- Dag.dagConflicts d]
    )
  where
    helpOf :: Act Extension -> Text
    helpOf act = act.extension.help

-- | One 'Dag.record' call: site, description, dependencies (as sites).
type Step = (Int, Int, [Int])

runSteps ::
    ((Act Extension -> Act Extension -> Bool) -> Ref -> Act Extension -> [Ref] -> Dag.Dag Extension -> Dag.Dag Extension) ->
    Dag.Dag Extension ->
    [Step] ->
    Dag.Dag Extension
runSteps rec = foldl' (\d (s, desc, ds) -> rec Dag.sameRepresentative (siteRef s) (actAt s desc) (fmap siteRef ds) d)

-------------------------------------------------------------------------------
-- Unit cases.

fanOrder :: IO ()
fanOrder = do
    let n = 50
        root = generate Fan n 0
        dag = Dag.foldDag Dag.sameRepresentative (evalDeps root) :: Dag.Dag Extension
        old = foldDagPerOccurrenceWith recordAppending Dag.sameRepresentative (evalDeps root)
    assertEqual "the root depends on every leaf, in declaration order" (fmap nodeRef [1 .. n - 1]) (Dag.dependenciesOf dag (nodeRef 0))
    assertEqual "each leaf has the root as its one dependant" [[nodeRef 0]] (Set.toList (Set.fromList [Dag.dependantsOf dag (nodeRef i) | i <- [1 .. n - 1]]))
    assertEqual "what the appending fold gives" (view old) (view dag)
    assertEqual "mergeDag onto itself, as the appending one" (view (mergeDagAppending Dag.sameRepresentative dag dag)) (view (Dag.mergeDag Dag.sameRepresentative dag dag))

manyDependants :: IO ()
manyDependants = do
    -- sites 1..40 each depend on site 0, recorded in a shuffled order.
    let order = [17, 3, 40, 1, 22, 9, 30, 2] <> [s | s <- [1 .. 40], s `notElem` [17, 3, 40, 1, 22, 9, 30, 2]]
        steps = [(s, 0, [0]) | s <- order]
        new = runSteps Dag.record Dag.emptyDag steps
        old = runSteps recordAppending Dag.emptyDag steps
    assertEqual "site 0's dependants, in the order recorded" (fmap siteRef order) (Dag.dependantsOf new (siteRef 0))
    assertEqual "site 0 is a key of both maps, though never recorded itself" (Just [], Nothing) (Map.lookup (siteRef 0) (Dag.dagDependencies new), Dag.representativeOf new (siteRef 0) >> Just ())
    assertEqual "what the appending record gives" (view old) (view new)

repeatedEdges :: IO ()
repeatedEdges = do
    let steps = [(1, 0, [2, 3, 2, 1, 3, 2]), (1, 0, [3, 2, 4]), (2, 0, [3, 3])]
        new = runSteps Dag.record Dag.emptyDag steps
        old = runSteps recordAppending Dag.emptyDag steps
    assertEqual "each dependency once, first seen first, the self-edge included" (fmap siteRef [2, 3, 1, 4]) (Dag.dependenciesOf new (siteRef 1))
    assertEqual "a dependant once however often its edge is recorded" (fmap siteRef [1, 2]) (Dag.dependantsOf new (siteRef 3))
    assertEqual "what the appending record gives" (view old) (view new)

twoOrders :: IO ()
twoOrders = do
    let first = [(1, 0, [2, 3]), (1, 1, [4, 2])]
        second = [(1, 1, [4, 2]), (1, 0, [2, 3])]
        a = runSteps Dag.record Dag.emptyDag first
        b = runSteps Dag.record Dag.emptyDag second
    assertEqual "declared over 2, 3 then over 4, 2" (fmap siteRef [2, 3, 4]) (Dag.dependenciesOf a (siteRef 1))
    assertEqual "declared over 4, 2 then over 2, 3" (fmap siteRef [4, 2, 3]) (Dag.dependenciesOf b (siteRef 1))
    assertEqual "one conflict each way" (1, 1) (length (Dag.dagConflicts a), length (Dag.dagConflicts b))
    assertEqual "first order, as the appending record" (view (runSteps recordAppending Dag.emptyDag first)) (view a)
    assertEqual "second order, as the appending record" (view (runSteps recordAppending Dag.emptyDag second)) (view b)

ontoABuiltDag :: IO ()
ontoABuiltDag = do
    let base = runSteps recordAppending Dag.emptyDag [(1, 0, [2, 3]), (4, 0, [3]), (5, 0, [])]
        more = [(1, 0, [5, 2]), (6, 0, [3, 1]), (3, 0, [])]
    assertEqual
        "lists extended at their end; untouched entries kept"
        (view (runSteps recordAppending base more))
        (view (runSteps Dag.record base more))
    assertEqual "site 3's dependants: the old ones, then the new" (fmap siteRef [1, 4, 6]) (Dag.dependantsOf (runSteps Dag.record base more) (siteRef 3))

fromMagmaAndTranspose :: IO ()
fromMagmaAndTranspose = do
    let n = 60
        magma = Map.fromList [(siteRef s, actAt s 0) | s <- [0 .. n]]
        fan = Set.fromList [(siteRef s, siteRef 0) | s <- [1 .. n]]
        rfan = Set.fromList [(siteRef 0, siteRef s) | s <- [1 .. n]]
    assertEqual "fan: as the appending fromMagma" (view (fromMagmaAppending magma fan)) (view (Dag.fromMagma magma fan))
    assertEqual "a node many depend on: as the appending fromMagma" (view (fromMagmaAppending magma rfan)) (view (Dag.fromMagma magma rfan))
    let deps = Map.fromList ((siteRef 0, fmap siteRef ([1 .. n] <> [7, 7])) : [(siteRef s, [siteRef 0]) | s <- [1 .. n]])
    assertEqual "transposeOf, a repeated edge included, as the appending one" (transposeOfAppending deps) (Dag.transposeOf deps)

outlineOfAFan :: IO ()
outlineOfAFan = do
    let n = 50
        cograph = evalDeps (generate Fan n 0)
        new = Query.outline cograph
        old = outlineAppending cograph
    assertEqual "the root's children, in declaration order" (fmap nodeRef [1 .. n - 1]) (Query.outlineChildren new (nodeRef 0))
    assertEqual "one root" [nodeRef 0] (Query.outlineRoots new)
    assertEqual "as the appending outline" (outlineView old) (outlineViewNew new)

-------------------------------------------------------------------------------
-- Properties against the appending versions.

genStep :: Int -> Gen Step
genStep sites = do
    s <- chooseInt (0, sites - 1)
    desc <- elements [0, 0, 0, 1, 2]
    k <- chooseInt (0, 5)
    ds <- vectorOf k (chooseInt (0, sites - 1))
    pure (s, desc, ds)

genSteps :: Gen (Int, [Step])
genSteps = do
    sites <- chooseInt (1, 12)
    steps <- listOf (genStep sites)
    pure (sites, steps)

prop_recordMatches :: Property
prop_recordMatches =
    forAll genSteps $ \(sites, steps) ->
        forAll (chooseInt (0, length steps)) $ \cut ->
            let (before, after) = splitAt cut steps
                -- a base built by the reference, then the rest by each.
                base = runSteps recordAppending Dag.emptyDag before
             in label (if sites <= 3 then "few sites (many repeats)" else "more sites") $
                    conjoin
                        [ counterexample "from empty" (view (runSteps recordAppending Dag.emptyDag steps) === view (runSteps Dag.record Dag.emptyDag steps))
                        , counterexample "onto a built Dag" (view (runSteps recordAppending base after) === view (runSteps Dag.record base after))
                        ]

prop_mergeDagMatches :: Property
prop_mergeDagMatches =
    forAll genSteps $ \(sites, steps) ->
        forAll (vectorOf (length steps `div` 2 + 1) (genStep sites)) $ \others ->
            let into = runSteps recordAppending Dag.emptyDag steps
                from = runSteps recordAppending Dag.emptyDag others
             in conjoin
                    [ view (mergeDagAppending Dag.sameRepresentative into from) === view (Dag.mergeDag Dag.sameRepresentative into from)
                    , counterexample "into an empty Dag" (view (mergeDagAppending Dag.sameRepresentative Dag.emptyDag from) === view (Dag.mergeDag Dag.sameRepresentative Dag.emptyDag from))
                    ]

-- | A magma over some of the sites and edges between any of them (so some
-- edges name a site the magma lacks, which 'Dag.fromMagma' drops).
genMagmaEdges :: Gen (Map Ref (Act Extension), Set.Set (Ref, Ref))
genMagmaEdges = do
    sites <- chooseInt (1, 15)
    held <- listOf (chooseInt (0, sites - 1))
    edges <- listOf ((,) <$> chooseInt (0, sites) <*> chooseInt (0, sites))
    pure
        ( Map.fromList [(siteRef s, actAt s 0) | s <- held]
        , Set.fromList [(siteRef a, siteRef b) | (a, b) <- edges]
        )

prop_fromMagmaMatches :: Property
prop_fromMagmaMatches =
    forAll genMagmaEdges $ \(magma, edges) ->
        view (fromMagmaAppending magma edges) === view (Dag.fromMagma magma edges)

genDeps :: Gen (Map Ref [Ref])
genDeps = do
    sites <- chooseInt (1, 15)
    keys <- listOf (chooseInt (0, sites - 1))
    entries <- traverse (\k -> (,) k <$> listOf (chooseInt (0, sites))) keys
    pure (Map.fromList [(siteRef k, fmap siteRef ds) | (k, ds) <- entries])

prop_transposeOfMatches :: Property
prop_transposeOfMatches =
    forAll genDeps $ \deps -> transposeOfAppending deps === Dag.transposeOf deps

-- | Fixture graphs small enough for the per-occurrence fold.
genFixture :: Gen (Shape, Op)
genFixture = do
    shape <-
        oneof
            [ pure Chain
            , pure Fan
            , Tree <$> chooseInt (1, 4)
            , Diamonds <$> chooseInt (1, 4)
            , Layered <$> chooseInt (1, 5) <*> chooseInt (1, 3)
            , RandomLayered <$> chooseInt (1, 5) <*> chooseInt (1, 3)
            , pure (Layered 8 8)
            ]
            `suchThat` valid
    size <- chooseInt (0, 80) `suchThat` (\n -> (counts shape n).countOccurrences <= 3000)
    seed <- fromIntegral <$> chooseInt (0, maxBound)
    form <- elements [AsVertices, AsConnect, AsOverlay]
    pure (shape, generateWith defaultOptions{optEdges = form} shape size seed)
  where
    valid (Layered w k) = k <= w
    valid (RandomLayered w k) = k <= w
    valid _ = True

prop_fixturesMatch :: Property
prop_fixturesMatch =
    forAll genFixture $ \(shape, root) ->
        let cograph = evalDeps root
            -- no conflict test: the fixture graphs have no collisions here,
            -- and this compares the edges and the order, which the per-node
            -- fold gives exactly as the per-occurrence one did.
            new = Dag.foldDag (\_ _ -> True) cograph :: Dag.Dag Extension
            old = foldDagPerOccurrenceWith recordAppending (\_ _ -> True) cograph
         in label (takeWhile (/= ' ') (show shape)) $
                conjoin
                    [ counterexample "foldDag: order" (Dag.dagOrder old === Dag.dagOrder new)
                    , counterexample "foldDag: dependencies" (Dag.dagDependencies old === Dag.dagDependencies new)
                    , counterexample "foldDag: dependants" (Dag.dagDependants old === Dag.dagDependants new)
                    , counterexample "outline" (outlineView (outlineAppending cograph) === outlineViewNew (Query.outline cograph))
                    ]

-- | What an outline is read by: its nodes, its roots, each node's children
-- (order included) and the first paths with their help.
type OutlineView = ([Ref], [Ref], [(Ref, [Ref])], [([Text], Ref, Text)])

outlineView :: OutlineAppending -> OutlineView
outlineView o =
    let refs = Set.toList (appendingRefs o)
     in (refs, appendingRoots o, [(r, appendingChildren o r) | r <- refs], appendingFirstPaths o)

outlineViewNew :: Query.Outline -> OutlineView
outlineViewNew o =
    let refs = Set.toList (Query.outlineRefs o)
     in (refs, Query.outlineRoots o, [(r, Query.outlineChildren o r) | r <- refs], Query.outlineFirstPaths o)

-------------------------------------------------------------------------------
-- Laws.

{- | Transposing twice gives back a dependency map whose every target is a
key and whose lists have no repeats, up to the order inside each list (a
transposed list is in key order); exactly, when each list is in key order.
-}
prop_transposeTwice :: Property
prop_transposeTwice =
    forAll genDeps $ \deps0 ->
        let keys = Set.fromList (Map.keys deps0 <> concat (Map.elems deps0))
            closed = Map.union (fmap (Set.toList . Set.fromList) deps0) (Map.fromSet (const []) keys)
            sorted = fmap sort closed
         in conjoin
                [ counterexample "as sets" (fmap Set.fromList (Dag.transposeOf (Dag.transposeOf closed)) === fmap Set.fromList closed)
                , counterexample "exactly, lists in key order" (Dag.transposeOf (Dag.transposeOf sorted) === sorted)
                ]

prop_fromMagmaIdempotent :: Property
prop_fromMagmaIdempotent =
    forAll (genMagmaEdges `suchThat` (not . Set.null . snd)) $ \(magma, edges) ->
        forAll (elements (Set.toList edges)) $ \e ->
            let dag = Dag.fromMagma magma edges
             in conjoin
                    [ counterexample "re-inserting an edge" (view (Dag.fromMagma magma (Set.insert e edges)) === view dag)
                    , counterexample "fromMagma . dagEdges" (view (Dag.fromMagma (Dag.dagNodes dag) (Dag.dagEdges dag)) === view dag)
                    ]

-------------------------------------------------------------------------------
-- Scale.

{- | A fan of 100 000 nodes (one node over every other) and its mirror (every
node over one), through each operation this feature touched. The appending
versions were quadratic here: a fan of 10 000 took 1.3 s to fold, 2.8 s
through 'Dag.fromMagma' and 2.0 s through 'Dag.mergeDag' (the profiles'
finding 3), so 100 000 would take minutes. Like "Test.GraphScaleSpec" this
times nothing: the bound is a minute per operation, where well under a
second is expected.
-}
scaleGuard :: IO ()
scaleGuard = do
    let n = 100000
        fan = generate Fan n 0
    folded <- within "foldDag over a fan" (evaluate (Dag.foldDag Dag.sameRepresentative (evalDeps fan) :: Dag.Dag Extension))
    assertEqual "the fan's root depends on every leaf" (n - 1) (length (Dag.dependenciesOf folded (nodeRef 0)))
    rebuilt <- within "fromMagma over a fan" (evaluate (Dag.fromMagma (Dag.dagNodes folded) (Dag.dagEdges folded)))
    assertEqual "fromMagma: every edge" (n - 1) (Set.size (Dag.dagEdges rebuilt))
    merged <- within "mergeDag of a fan onto itself" (evaluate (Dag.mergeDag Dag.sameRepresentative rebuilt folded))
    assertEqual "mergeDag: nothing added" (Dag.dagEdges folded) (Dag.dagEdges merged)
    outlined <- within "outline of a fan" (evaluate (length (Query.outlineChildren (Query.outline (evalDeps fan)) (nodeRef 0))))
    assertEqual "outline: every leaf under the root" (n - 1) outlined
    let magma = Dag.dagNodes folded
        mirror = Set.fromList [(nodeRef 0, nodeRef i) | i <- [1 .. n - 1]]
    rdag <- within "fromMagma, every node over one" (evaluate (Dag.fromMagma magma mirror))
    assertEqual "every leaf stands on the root" (n - 1) (length (Dag.dependantsOf rdag (nodeRef 0)))
    transposed <- within "transposeOf, every node over one" (evaluate (Dag.transposeOf (Dag.dagDependencies rdag)))
    assertEqual "transposeOf gives the dependants" (Dag.dagDependants rdag) transposed
  where
    within :: String -> IO a -> IO a
    within what act = do
        outcome <- timeout (60 * 1000000) act
        case outcome of
            Just x -> pure x
            Nothing -> assertFailure (what <> ": still running after a minute, which is what a quadratic edge list looks like")
