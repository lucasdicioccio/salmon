{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0: 'Salmon.Op.Dag.foldDag' visits each node once, and under its
stated precondition gives what the fold that visited every occurrence gave.

The old fold is kept as "Test.DagReference".'foldDagPerOccurrence' and is the
reference throughout. What "gives the same" means is 'disagreements': the
same nodes in the same order, the same dependencies and dependants in the
same order, representatives the conflict test cannot tell apart, and the
same contested 'Ref's with the same declarations taking part. The number of
'Salmon.Op.Dag.Conflict's is deliberately /not/ the same: the reference
records one each time the representative changes, the per-node fold one per
declaration that lost.

The precondition is "Test.DagReference".'coherent'. The inputs are:

* the "Test.GraphFixture" shapes, with colliding 'Ref's, which are coherent
  by construction (a node is a function of its number);
* small random recipes ('Recipe'), where 'Ref's and descriptions are drawn
  from small pools so that nodes collide, re-declare each other with
  different predecessors, and sit on cycles of 'Ref's, filtered to the
  coherent ones;
* the family of incoherent inputs ('Hidden') on which the two folds are
  known to differ, pinned as a difference.

Everything is pure and small: the reference is exponential in the node count
on a shape that shares. The one sizeable case folds a chain of a thousand
diamonds, which only a per-node fold can finish.
-}
module Test.DagPerNodeSpec (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Monad.Identity (Identity (..))
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.QuickCheck (Gen, Property, choose, chooseInt, conjoin, counterexample, elements, forAll, frequency, label, oneof, property, shuffle, sublistOf, suchThat, vectorOf, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension, Op, check, deps, down, dynamics, evalDeps, help, nodeps, notes, op, ref, up)
import Salmon.Op.Actions (Act (..), Actions (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Graph (Graph (..))
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref, mkRef)

import Test.DagReference (coherent, foldDagPerOccurrence, occurrences)
import Test.GraphFixture
import Test.Harness (capture)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.Dag per node"
        [ testGroup
            "unit"
            [ testCase "every fixture shape folds as the reference does" shapesMatchReference
            , testCase "a colliding ref: last writer wins, the loser is reported once" collidingRef
            , testCase "a contested node reached by many paths is one conflict, not one per flip" oneConflictPerLoser
            , testCase "the representative is the last declaration in the tree, not the last one walked" lastInTreeWins
            , testCase "three declarations of one ref: the last wins, each loser is reported against it" threeWriters
            , testCase "a re-declaration with different predecessors still merges its edges" edgesStillAccumulate
            , testCase "a cycle of refs folds, and is reported stuck" cycleOfRefs
            , testCase "opting out of the conflict test records none" optOut
            , testCase "known difference: a differing node below two declarations that look alike is not seen" hiddenBelowLookAlikes
            , testCase "a thousand diamonds fold, with one conflict per colliding ref" thousandDiamonds
            ]
        , testGroup
            "properties"
            [ localOption (QuickCheckTests 500) $
                testProperty "fixture graphs: equal to the per-occurrence fold" prop_fixturesMatchReference
            , localOption (QuickCheckTests 2000) $
                testProperty "coherent recipes: equal to the per-occurrence fold" prop_coherentRecipesMatchReference
            , localOption (QuickCheckTests 2000) $
                testProperty "any recipe: a well-formed part of what the per-occurrence fold finds" prop_anyRecipeIsWellFormedPart
            , localOption (QuickCheckTests 500) $
                testProperty "incoherent recipes of the known family: the hidden conflict is lost, first writer kept" prop_hiddenConflictIsLost
            ]
        ]

-------------------------------------------------------------------------------
-- The two folds, and what agreeing means.

perNode :: Op -> Dag.Dag Extension
perNode = Dag.foldDag Dag.sameRepresentative . evalDeps

reference :: Op -> Dag.Dag Extension
reference = foldDagPerOccurrence Dag.sameRepresentative . evalDeps

isCoherent :: Op -> Bool
isCoherent = coherent Dag.sameRepresentative . evalDeps

-- | The descriptions taking part in the conflicts over each 'Ref': the kept
-- and the replaced ones, as a set.
contestants :: Dag.Dag Extension -> Map.Map Ref [Dag.Representative]
contestants dag =
    fmap (sortOn show . nub) $
        Map.fromListWith
            (<>)
            [ (c.conflictRef, [Dag.representative c.conflictKept, Dag.representative c.conflictReplaced])
            | c <- Dag.dagConflicts dag
            ]

{- | Everything on which the per-node fold (second) differs from the
reference (first), over what a coherent input promises. Empty when they
agree.
-}
disagreements :: Dag.Dag Extension -> Dag.Dag Extension -> [String]
disagreements old new =
    concat
        [ ["order: " <> show (Dag.dagOrder old) <> " /= " <> show (Dag.dagOrder new) | Dag.dagOrder old /= Dag.dagOrder new]
        , ["dependencies: " <> show (Dag.dagDependencies old) <> " /= " <> show (Dag.dagDependencies new) | Dag.dagDependencies old /= Dag.dagDependencies new]
        , ["dependants: " <> show (Dag.dagDependants old) <> " /= " <> show (Dag.dagDependants new) | Dag.dagDependants old /= Dag.dagDependants new]
        , ["nodes: " <> show (Map.keys (Dag.dagNodes old)) <> " /= " <> show (Map.keys (Dag.dagNodes new)) | Map.keys (Dag.dagNodes old) /= Map.keys (Dag.dagNodes new)]
        , [ "representative of " <> show r <> ": " <> show (Dag.representative a) <> " /= " <> show (fmap Dag.representative b)
          | (r, a) <- Map.toList (Dag.dagNodes old)
          , let b = Map.lookup r (Dag.dagNodes new)
          , fmap Dag.representative b /= Just (Dag.representative a)
          ]
        , -- conflicts, as a set: the same refs are contested, by the same
          -- declarations.
          ["contestants: " <> show (contestants old) <> " /= " <> show (contestants new) | contestants old /= contestants new]
        , wellFormedConflicts new
        ]

{- | What the per-node fold promises about its conflicts on any input: for
each contested 'Ref', one conflict per declaration that lost, each against
the final representative.
-}
wellFormedConflicts :: Dag.Dag Extension -> [String]
wellFormedConflicts new =
    concat
        [ [ "conflict on " <> show c.conflictRef <> " does not keep the final representative"
          | c <- Dag.dagConflicts new
          , fmap Dag.representative (Dag.representativeOf new c.conflictRef) /= Just (Dag.representative c.conflictKept)
          ]
        , [ "conflict on " <> show c.conflictRef <> " replaces what it keeps"
          | c <- Dag.dagConflicts new
          , Dag.representative c.conflictKept == Dag.representative c.conflictReplaced
          ]
        , [ "conflicts on " <> show r <> ": " <> show n <> " for " <> show (length reps) <> " declarations"
          | (r, reps) <- Map.toList (contestants new)
          , let n = length [() | c <- Dag.dagConflicts new, c.conflictRef == r]
          , n /= length reps - 1
          ]
        ]

-- | The invariant of a 'Dag.Dag', and that one direction is the other's transpose.
malformed :: Dag.Dag Extension -> [String]
malformed dag =
    concat
        [ ["order repeats a ref" | nub order /= order]
        , ["nodes and order disagree" | Map.keysSet (Dag.dagNodes dag) /= Set.fromList order]
        , ["dependencies and order disagree" | Map.keysSet (Dag.dagDependencies dag) /= Set.fromList order]
        , ["dependants and order disagree" | Map.keysSet (Dag.dagDependants dag) /= Set.fromList order]
        , ["dependants is not the transpose of dependencies" | forward /= backward]
        ]
  where
    order = Dag.dagOrder dag
    forward = Set.fromList [(d, r) | (r, ds) <- Map.toList (Dag.dagDependencies dag), d <- ds]
    backward = Set.fromList [(d, r) | (d, rs) <- Map.toList (Dag.dagDependants dag), r <- rs]

-------------------------------------------------------------------------------
-- Unit tests.

refOf :: Text -> Ref
refOf = mkRef "dag-per-node-spec"

-- | A node described by its name and a @help@, over the given nodes.
named :: Text -> Text -> [Op] -> Op
named name described below = op name (deps below) $ \x -> x{ref = refOf name, help = described}

helpOf :: Dag.Dag Extension -> Text -> Maybe Text
helpOf dag name = fmap (\act -> act.extension.help) (Dag.representativeOf dag (refOf name))

conflictsOn :: Dag.Dag Extension -> Text -> [(Text, Text)]
conflictsOn dag name =
    [ (c.conflictKept.extension.help, c.conflictReplaced.extension.help)
    | c <- reverse (Dag.dagConflicts dag)
    , c.conflictRef == refOf name
    ]

shapesMatchReference :: IO ()
shapesMatchReference =
    forM_ shapes $ \shape ->
        forM_ [0, 1, 2, 3, 5, 7, 12, 20] $ \size ->
            forM_ [(AsVertices, 0), (AsVertices, 1), (AsConnect, 1 / 3), (AsOverlay, 1 / 2)] $ \(form, fraction) -> do
                let root = generateWith defaultOptions{optEdges = form, optCollisions = fraction} shape size 42
                    at = show shape <> " at " <> show size <> ", " <> show form <> ", colliding " <> show fraction
                assertBool (at <> ": the fixture is coherent") (isCoherent root)
                assertEqual at [] (disagreements (reference root) (perNode root))
  where
    shapes =
        [ Chain
        , Fan
        , Tree 1
        , Tree 2
        , Tree 3
        , Diamonds 1
        , Diamonds 2
        , Diamonds 3
        , Layered 1 1
        , Layered 3 1
        , Layered 4 2
        , Layered 3 3
        , RandomLayered 4 2
        , RandomLayered 5 3
        , RandomLayered 3 3
        ]

collidingRef :: IO ()
collidingRef = do
    let root = named "root" "" [named "contested" "first" [], named "contested" "second" []]
        dag = perNode root
    assertEqual "the later declaration is the representative" (Just "second") (helpOf dag "contested")
    assertEqual "one conflict: second kept, first replaced" [("second", "first")] (conflictsOn dag "contested")
    assertEqual "nothing else is contested" 1 (length (Dag.dagConflicts dag))
    assertEqual "as the reference" [] (disagreements (reference root) dag)

{- | The count that changed. @pair@ is a node and a differing declaration of
it, side by side; it sits under a parent reached by four paths. The
reference flips the representative at every occurrence after the first:
seven conflicts for one contested node. Per node it is one.
-}
oneConflictPerLoser :: IO ()
oneConflictPerLoser = do
    let parent = named "parent" "" [named "contested" "first" [], named "contested" "second" []]
        paths = [named ("via" <> Text.pack (show i)) "" [parent] | i <- [1 .. 4 :: Int]]
        root = named "root" "" paths
    assertEqual "the reference: one per flip" 7 (length (Dag.dagConflicts (reference root)))
    assertEqual "per node: one" [("second", "first")] (conflictsOn (perNode root) "contested")
    assertEqual "and otherwise as the reference" [] (disagreements (reference root) (perNode root))

{- | The walk meets @first@ (under @parent@), then @second@, and then skips
the second occurrence of @parent@. The last declaration in the tree is the
@first@ under that skipped occurrence, and that is the one kept: the walk
order does not leak into which declaration wins.
-}
lastInTreeWins :: IO ()
lastInTreeWins = do
    let parent = named "parent" "" [named "contested" "first" []]
        root = named "root" "" [parent, named "contested" "second" [], parent]
        dag = perNode root
    assertBool "coherent" (isCoherent root)
    assertEqual "the reference keeps the last in the tree" (Just "first") (helpOf (reference root) "contested")
    assertEqual "and so does the per-node fold" (Just "first") (helpOf dag "contested")
    assertEqual "reported against the declaration that lost" [("first", "second")] (conflictsOn dag "contested")
    assertEqual "the reference reported both flips" [("second", "first"), ("first", "second")] (conflictsOn (reference root) "contested")
    assertEqual "as the reference" [] (disagreements (reference root) dag)

{- | With more than two declarations the reference reports the chain of
flips (@b@ over @a@, @c@ over @b@); per node every loser is reported against
the declaration that ended up kept, in the order the losers were first met.
-}
threeWriters :: IO ()
threeWriters = do
    let root = named "root" "" [named "contested" d [] | d <- ["a", "b", "c"]]
        dag = perNode root
    assertEqual "the last wins" (Just "c") (helpOf dag "contested")
    assertEqual "the reference: the chain of flips" [("b", "a"), ("c", "b")] (conflictsOn (reference root) "contested")
    assertEqual "per node: each loser against the winner" [("c", "a"), ("c", "b")] (conflictsOn dag "contested")
    assertEqual "the same declarations take part" [] (disagreements (reference root) dag)

-- | Same 'Ref', same description, different predecessors: both occurrences
-- are walked, because their predecessors are part of what they look like.
edgesStillAccumulate :: IO ()
edgesStillAccumulate = do
    let root = named "root" "" [named "x" "" [named "p1" "" []], named "x" "" [named "p2" "" []]]
        dag = perNode root
    assertEqual "x depends on both declarations' predecessors" (fmap refOf ["p1", "p2"]) (Dag.dependenciesOf dag (refOf "x"))
    assertEqual "no conflict" 0 (length (Dag.dagConflicts dag))
    assertEqual "as the reference" [] (disagreements (reference root) dag)

{- | An expansion is a finite tree, so a cycle can only be one of 'Ref's: @a@
over @b@ over a second declaration of @a@. Both edges are kept and both
nodes are stuck, in either direction.
-}
cycleOfRefs :: IO ()
cycleOfRefs = do
    let root = named "a" "" [named "b" "" [named "a" "" []]]
        dag = perNode root
    assertEqual "two nodes" (fmap refOf ["a", "b"]) (Dag.dagOrder dag)
    assertEqual "a stands on b" [refOf "b"] (Dag.dependenciesOf dag (refOf "a"))
    assertEqual "and b on a" [refOf "a"] (Dag.dependenciesOf dag (refOf "b"))
    assertEqual "both stuck going up" (Set.fromList (fmap refOf ["a", "b"])) (Dag.stuck Dag.dependenciesOf dag)
    assertEqual "and coming down" (Set.fromList (fmap refOf ["a", "b"])) (Dag.stuck Dag.dependantsOf dag)
    assertEqual "as the reference" [] (disagreements (reference root) dag)
    (r, readBack) <- capture
    ok <- UpDown.upDag (const (pure UpDown.Required)) r dag
    reports <- readBack
    assertBool "the walk fails" (not ok)
    assertEqual "both nodes are Blocked" 2 (length [() | UpDown.Blocked _ <- reports])

optOut :: IO ()
optOut = do
    let root = named "root" "" [named "contested" "first" [], named "contested" "second" []]
        dag = Dag.foldDag (\_ _ -> True) (evalDeps root) :: Dag.Dag Extension
        old = foldDagPerOccurrence (\_ _ -> True) (evalDeps root) :: Dag.Dag Extension
    assertEqual "no conflicts" 0 (length (Dag.dagConflicts dag))
    assertEqual "nor in the reference" 0 (length (Dag.dagConflicts old))
    assertEqual "same order" (Dag.dagOrder old) (Dag.dagOrder dag)
    assertEqual "same dependencies" (Dag.dagDependencies old) (Dag.dagDependencies dag)
    -- nothing tells the two declarations apart, so either is "the" node: the
    -- reference keeps the last occurrence, the per-node fold the first.
    assertEqual "the reference keeps the last" (Just "second") (helpOf old "contested")
    assertEqual "per node: the first, which the test called the same" (Just "first") (helpOf dag "contested")

{- | __The accepted difference.__ Two declarations of @svc@ with the same
description, each over a @file@ with the same 'Ref'; the two @file@s differ.
The two @svc@ occurrences look alike (same 'Ref', same description, same
predecessor 'Ref's), so the second is not walked and the differing @file@
below it is never seen.

The reference reports the conflict and keeps the last @file@. Per node there
is no conflict and the first @file@ is kept. This input is not 'coherent',
and that is the precondition the fold states.
-}
hiddenBelowLookAlikes :: IO ()
hiddenBelowLookAlikes = do
    let svc contents = named "svc" "a unit" [named "file" contents []]
        root = named "root" "" [named "one" "" [svc "old contents"], named "two" "" [svc "new contents"]]
        old = reference root
        new = perNode root
    assertBool "the input is not coherent" (not (isCoherent root))
    assertEqual "the reference reports the fight over the file" [("new contents", "old contents")] (conflictsOn old "file")
    assertEqual "the reference keeps the last writer" (Just "new contents") (helpOf old "file")
    assertEqual "per node: no conflict is reported" [] (conflictsOn new "file")
    assertEqual "per node: the first writer is kept" (Just "old contents") (helpOf new "file")
    -- the shape of the graph is not affected here: only the description is.
    assertEqual "same order" (Dag.dagOrder old) (Dag.dagOrder new)
    assertEqual "same dependencies" (Dag.dagDependencies old) (Dag.dagDependencies new)
    -- one level is enough to be seen again: a difference in the description
    -- of the unit itself makes the two occurrences distinct, and the file is found.
    let told = named "root" "" [named "one" "" [named "svc" "a unit (old)" [named "file" "old contents" []]], named "two" "" [named "svc" "a unit (new)" [named "file" "new contents" []]]]
    assertBool "telling the units apart makes it coherent" (isCoherent told)
    assertEqual "and both conflicts are found" [] (disagreements (reference told) (perNode told))
    assertEqual "" [("new contents", "old contents")] (conflictsOn (perNode told) "file")

{- | A chain of a thousand diamonds has @2^1000@ paths: only a fold that pays
per node returns. Compared with the fixture's own per-node build, which
shares no code with the fold.
-}
thousandDiamonds :: IO ()
thousandDiamonds = do
    let shape = Diamonds 2
        size = 3001
        opts = defaultOptions{optCollisions = 1 / 10}
        root = generateWith opts shape size 7
        built = dagOf root
    outcome <- timeout (60 * 1000000) $ do
        let dag = perNode root
        _ <- evaluate (Map.size (Dag.dagNodes dag) + length (Dag.dagConflicts dag))
        pure dag
    case outcome of
        Nothing -> assertFailure "still folding after 60s, which is what paying per path looks like"
        Just dag -> do
            assertEqual "every node" (counts shape size).countNodes (fromIntegral (Map.size (Dag.dagNodes dag)))
            assertEqual "order" (Dag.dagOrder built) (Dag.dagOrder dag)
            assertEqual "dependencies" (Dag.dagDependencies built) (Dag.dagDependencies dag)
            assertEqual "dependants" (fmap Set.fromList (Dag.dagDependants built)) (fmap Set.fromList (Dag.dagDependants dag))
            let colliders = filter (isCollider opts shape size 7) [0 .. size]
            assertEqual "the colliding refs are the contested ones" (Set.fromList (fmap nodeRef colliders)) (Set.fromList (fmap (\c -> c.conflictRef) (Dag.dagConflicts dag)))
            assertEqual "one conflict per colliding ref" (length colliders) (length (Dag.dagConflicts dag))
            forM_ colliders $ \i ->
                assertEqual
                    ("the second declaration of n" <> show i <> " is kept")
                    (Just "graph fixture node (colliding declaration)")
                    (fmap (\act -> act.extension.help) (Dag.representativeOf dag (nodeRef i)))
            assertEqual "conflicts are well formed" [] (wellFormedConflicts dag)

-------------------------------------------------------------------------------
-- Fixture graphs.

data Fixture = Fixture Shape Int Seed EdgeForm Rational
    deriving (Show)

fixtureOp :: Fixture -> Op
fixtureOp (Fixture shape size seed form fraction) =
    generateWith defaultOptions{optEdges = form, optCollisions = fraction} shape size seed

-- | Any shape, at a size whose expansion the reference can walk.
genFixture :: Gen Fixture
genFixture = do
    shape <-
        oneof
            [ pure Chain
            , pure Fan
            , Tree <$> chooseInt (1, 4)
            , Diamonds <$> chooseInt (1, 4)
            , layered Layered
            , layered RandomLayered
            ]
    size <- chooseInt (0, 60) `suchThat` (\n -> (counts shape n).countOccurrences <= 3000)
    seed <- fromIntegral <$> chooseInt (0, maxBound)
    form <- elements [AsVertices, AsConnect, AsOverlay]
    fraction <- elements [0, 0, 1 / 10, 1 / 3, 1 / 2, 1]
    pure (Fixture shape size seed form fraction)
  where
    layered mk = do
        w <- chooseInt (1, 5)
        k <- chooseInt (1, w)
        pure (mk w k)

shapeName :: Shape -> String
shapeName = takeWhile (/= ' ') . show

prop_fixturesMatchReference :: Property
prop_fixturesMatchReference =
    forAll genFixture $ \fixture@(Fixture shape _ _ _ fraction) ->
        let root = fixtureOp fixture
            old = reference root
            new = perNode root
            -- no conflict test at all: the same graph, and nothing reported.
            oldBlind = foldDagPerOccurrence (\_ _ -> True) (evalDeps root) :: Dag.Dag Extension
            newBlind = Dag.foldDag (\_ _ -> True) (evalDeps root) :: Dag.Dag Extension
         in label (shapeName shape <> (if fraction > 0 then ", colliding" else "")) $
                conjoin
                    [ counterexample "the fixture is not coherent" (isCoherent root)
                    , disagreements old new === []
                    , malformed new === []
                    , counterexample "opting out: order" (Dag.dagOrder oldBlind === Dag.dagOrder newBlind)
                    , counterexample "opting out: dependencies" (Dag.dagDependencies oldBlind === Dag.dagDependencies newBlind)
                    , counterexample "opting out: dependants" (Dag.dagDependants oldBlind === Dag.dagDependants newBlind)
                    , counterexample "opting out: conflicts" (length (Dag.dagConflicts newBlind) === 0)
                    ]

-------------------------------------------------------------------------------
-- Random recipes.

{- | One declaration of a recipe: which effect site it names, which of a few
descriptions it gives it, whether it is glue (no node at all), how its
dependencies are written, and which later declarations it depends on.
-}
data Decl = Decl
    { declSite :: Int
    , declDescription :: Int
    , declGlue :: Bool
    , declForm :: EdgeForm
    , declBelow :: [Int]
    }
    deriving (Show)

{- | Declarations numbered from 0, the root; a declaration only depends on
later ones, so the expansion is finite. Sites and descriptions come from
pools smaller than the recipe, so declarations collide on a 'Ref' with the
same or a different description, with the same or different predecessors,
below one another (a cycle of 'Ref's) or side by side.
-}
newtype Recipe = Recipe [Decl]
    deriving (Show)

recipeOp :: Recipe -> Op
recipeOp (Recipe decls) = build 0
  where
    table = Map.fromList (zip [0 :: Int ..] decls)
    build i =
        case Map.lookup i table of
            Nothing -> OpGraph nodeps Actionless
            Just d
                | d.declGlue -> OpGraph (Identity (written d.declForm (fmap build d.declBelow))) Actionless
                | otherwise ->
                    op (siteName d.declSite) (Identity (written d.declForm (fmap build d.declBelow))) $ \x ->
                        x{ref = refOf (siteName d.declSite), help = "description " <> Text.pack (show d.declDescription)}
    siteName s = "site" <> Text.pack (show s)
    written form xs =
        case form of
            AsVertices -> Vertices xs
            AsConnect -> foldr (Connect . Vertices . pure) (Vertices []) xs
            AsOverlay -> foldr (Overlay . Vertices . pure) (Vertices []) xs

genRecipe :: Gen Recipe
genRecipe = do
    n <- chooseInt (1, 9)
    sites <- chooseInt (1, max 1 (n - 1))
    fmap Recipe . sequence $
        [ do
            site <- chooseInt (0, sites - 1)
            description <- frequency [(5, pure 0), (2, pure 1), (1, pure 2)]
            glue <- frequency [(7, pure False), (1, pure True)]
            form <- elements [AsVertices, AsConnect, AsOverlay]
            wanted <- chooseInt (0, 3)
            below <- fmap (take wanted) (shuffle =<< sublistOf [i + 1 .. n - 1])
            -- now and then the same dependency written twice.
            twice <- frequency [(6, pure []), (1, pure (take 1 below))]
            pure (Decl site description glue form (below <> twice))
        | i <- [0 .. n - 1]
        ]

-- | What a recipe exercises, for the test's own report of its coverage.
recipeLabel :: Op -> String
recipeLabel root
    | not (null (Dag.dagConflicts old)) && Set.size (Dag.stuck Dag.dependenciesOf old) > 0 = "conflicts and a cycle of refs"
    | not (null (Dag.dagConflicts old)) = "conflicts"
    | Set.size (Dag.stuck Dag.dependenciesOf old) > 0 = "a cycle of refs"
    | occurrences (evalDeps root) > Map.size (Dag.dagNodes old) = "shared nodes, no conflict"
    | otherwise = "no sharing"
  where
    old = reference root

prop_coherentRecipesMatchReference :: Property
prop_coherentRecipesMatchReference =
    forAll (genRecipe `suchThat` (isCoherent . recipeOp)) $ \recipe ->
        let root = recipeOp recipe
         in label (recipeLabel root) $
                conjoin
                    [ disagreements (reference root) (perNode root) === []
                    , malformed (perNode root) === []
                    ]

{- | Without the precondition the per-node fold still returns a well-formed
'Dag.Dag' that claims nothing the reference does not: its nodes, its edges
and its contested 'Ref's are among the reference's. What it can do is miss
some, which is the known difference.
-}
prop_anyRecipeIsWellFormedPart :: Property
prop_anyRecipeIsWellFormedPart =
    forAll genRecipe $ \recipe ->
        let root = recipeOp recipe
            old = reference root
            new = perNode root
         in label (if isCoherent root then "coherent" else "incoherent") $
                conjoin
                    [ malformed new === []
                    , wellFormedConflicts new === []
                    , counterexample "a node the reference does not have" (property (Map.keysSet (Dag.dagNodes new) `Set.isSubsetOf` Map.keysSet (Dag.dagNodes old)))
                    , counterexample "an edge the reference does not have" (property (Dag.dagEdges new `Set.isSubsetOf` Dag.dagEdges old))
                    , counterexample "a contested ref the reference does not have" (property (Map.keysSet (contestants new) `Set.isSubsetOf` Map.keysSet (contestants old)))
                    ]

-------------------------------------------------------------------------------
-- The incoherent family.

{- | The counter-example, generalised: several parents, each over its own
copy of a chain of nodes that look alike from copy to copy, ending in a
@file@ whose description differs between copies (the first and the last
always differ).
-}
data Hidden = Hidden
    { hiddenDepth :: Int
    -- ^ look-alike nodes between a parent and the file: at least one.
    , hiddenContents :: [Int]
    -- ^ the file's description under each parent: at least two.
    }
    deriving (Show)

genHidden :: Gen Hidden
genHidden = do
    depth <- chooseInt (1, 5)
    parents <- chooseInt (2, 5)
    contents <- vectorOf parents (choose (0, 2)) `suchThat` (\cs -> head cs /= last cs)
    pure (Hidden depth contents)

hiddenOp :: Hidden -> Op
hiddenOp (Hidden depth contents) =
    named "root" "" [named ("parent" <> Text.pack (show i)) "" [chain depth c] | (i, c) <- zip [0 :: Int ..] contents]
  where
    chain 0 c = named "file" ("contents " <> Text.pack (show c)) []
    chain k c = named ("link" <> Text.pack (show k)) "a link" [chain (k - 1) c]

prop_hiddenConflictIsLost :: Property
prop_hiddenConflictIsLost =
    forAll genHidden $ \hidden ->
        let root = hiddenOp hidden
            old = reference root
            new = perNode root
            described c = Just ("contents " <> Text.pack (show c))
         in conjoin
                [ counterexample "the input is coherent" (not (isCoherent root))
                , counterexample "the reference reports no conflict" (not (null (conflictsOn old "file")))
                , counterexample "the reference keeps the last writer" (helpOf old "file" === described (last hidden.hiddenContents))
                , counterexample "per node: a conflict is reported" (conflictsOn new "file" === [])
                , counterexample "per node: the first writer is kept" (helpOf new "file" === described (head hidden.hiddenContents))
                , counterexample "the graph itself differs" (Dag.dagOrder old === Dag.dagOrder new)
                , counterexample "the graph itself differs" (Dag.dagDependencies old === Dag.dagDependencies new)
                , malformed new === []
                ]
