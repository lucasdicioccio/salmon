{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for 'Salmon.Op.Dag' — the collapse of an expanded graph
to a flat, 'Ref'-keyed DAG that used to be buried inside
'Salmon.Actions.UpDown.downTreeWith'.

Everything here is pure: no node's @up@\/@down@ is ever run, which is the
point of having lifted the fold out in the first place. The one exception is
the last test, which checks the conflict actually reaches a caller's
'Salmon.Reporter.Reporter' through a real teardown.
-}
module Test.DagSpec (tests) where

import qualified Data.List
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- GHC only solves a `HasField` constraint when the field selector is in
-- scope, and `Dag.sameRepresentative` needs three of them: a selective import
-- here has to name `notes` and `dynamics` even though this module never
-- mentions either.
import Salmon.Builtin.Extension (Extension, Op, check, deps, down, dynamics, evalDeps, help, nodeps, notes, op, opAct, ref, up)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Supervision (Strategy (..), Supervision (..), defaultSupervision, supervised)
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Op.Actions (Act (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.Harness (capture)

import Test.Harness (runDownCapturing)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.Dag"
        [ testCase "a shared node is one magma entry with both its dependants" sharedNodeOneEntry
        , testCase "dependants is the exact transpose of dependencies" transpose
        , testCase "edges from every occurrence accumulate" edgesAccumulate
        , testCase "a re-declared node is replaced, last writer wins" lastWriterWins
        , testCase "a differing representative is reported as a conflict" conflictReported
        , testCase "the same node reached twice is not a conflict" noSelfConflict
        , testCase "roots are the nodes nothing depends on" rootsAreUndepended
        , testCase "downTree reports a conflict to its caller" downTreeReportsConflict
        , testCase "fromMagma rebuilds what dagEdges flattened" fromMagmaRoundTrips
        , testCase "a cycle is reported Blocked, not silently skipped" cycleIsBlocked
        , testCase "a changed Supervision policy is a differing representative" changedSupervisionIsAConflict
        , testCase "an identical Supervision policy is not a differing representative" sameSupervisionIsNotAConflict
        ]

-------------------------------------------------------------------------------

refOf :: Text -> Ref
refOf = mkRef "dagspec"

leaf :: Text -> Op
leaf name = op name nodeps $ \x -> x{ref = refOf name}

-- | A diamond: @root@ over @left@ and @right@, both over one @apex@.
diamond :: (Op, Op, Op, Op)
diamond = (root, left, right, apex)
  where
    apex = leaf "apex"
    left = op "left" (deps [apex]) $ \x -> x{ref = refOf "left"}
    right = op "right" (deps [apex]) $ \x -> x{ref = refOf "right"}
    root = op "root" (deps [left, right]) $ \x -> x{ref = refOf "root"}

foldOf :: Op -> Dag.Dag Extension
foldOf = Dag.foldDag Dag.sameRepresentative . evalDeps

-------------------------------------------------------------------------------

{- | The 'Cofree' holds @apex@ twice (once under each of @left@ and @right@);
the 'Dag' holds it once, and — unlike the 'Cofree' — knows that /two/ things
stand on it, which is the fact a teardown needs and cannot get by walking
predecessors.
-}
sharedNodeOneEntry :: IO ()
sharedNodeOneEntry = do
    let (root, _, _, _) = diamond
    let dag = foldOf root
    assertEqual "one entry per ref" 4 (length (Dag.dagOrder dag))
    -- first-seen, i.e. depth-first from the root: apex is reached under
    -- @left@ before @right@ is.
    assertEqual "first-seen order" (map refOf ["root", "left", "apex", "right"]) (Dag.dagOrder dag)
    assertEqual
        "apex has both dependants"
        (map refOf ["left", "right"])
        (Dag.dependantsOf dag (refOf "apex"))
    assertEqual "apex depends on nothing" [] (Dag.dependenciesOf dag (refOf "apex"))
    assertEqual "no conflicts in a plain diamond" 0 (length (Dag.dagConflicts dag))

transpose :: IO ()
transpose = do
    let (root, _, _, _) = diamond
    let dag = foldOf root
    let forward = [(a, b) | a <- Dag.dagOrder dag, b <- Dag.dependenciesOf dag a]
        backward = [(a, b) | b <- Dag.dagOrder dag, a <- Dag.dependantsOf dag b]
    assertEqual "every dependency edge has its dependant edge" (sortP forward) (sortP backward)
    assertBool "and there are some" (not (null forward))
  where
    sortP = Data.List.sort

{- | Two nodes sharing a 'Ref' but declaring different predecessors: the
collapse @downTreeWith@ used to do internally kept only the first
occurrence's edges, so the second declaration's dependency was invisible and
could be torn down while the shared node still stood on it. Both edges are
kept now — which is also what makes folding a second graph in a merge rather
than a replacement.
-}
edgesAccumulate :: IO ()
edgesAccumulate = do
    let p1 = leaf "p1"
        p2 = leaf "p2"
        -- same shorthand, same help, same ref: indistinguishable
        -- representatives, so this is an edge merge and not a conflict.
        x1 = op "x" (deps [p1]) $ \x -> x{ref = refOf "x"}
        x2 = op "x" (deps [p2]) $ \x -> x{ref = refOf "x"}
        root = op "root" (deps [x1, x2]) $ \x -> x{ref = refOf "root"}
    let dag = foldOf root
    assertEqual
        "x depends on both declarations' predecessors"
        (map refOf ["p1", "p2"])
        (Dag.dependenciesOf dag (refOf "x"))
    assertEqual "and both know x stands on them" [refOf "x"] (Dag.dependantsOf dag (refOf "p1"))
    assertEqual "" [refOf "x"] (Dag.dependantsOf dag (refOf "p2"))
    assertEqual "merging identical representatives is not a conflict" 0 (length (Dag.dagConflicts dag))

lastWriterWins :: IO ()
lastWriterWins = do
    let dag = foldOf twoWriters
    assertEqual
        "the later declaration is the representative"
        (Just "second")
        (fmap (\act -> act.extension.help) (Dag.representativeOf dag (refOf "contested")))

conflictReported :: IO ()
conflictReported = do
    let dag = foldOf twoWriters
    case Dag.dagConflicts dag of
        [c] -> do
            assertEqual "on the contested ref" (refOf "contested") c.conflictRef
            assertEqual "kept the later one" "second" (c.conflictKept.extension.help)
            assertEqual "dropped the earlier one" "first" (c.conflictReplaced.extension.help)
        other -> assertEqual "exactly one conflict" 1 (length other)

-- | One effect site, two declarations that describe it differently.
twoWriters :: Op
twoWriters =
    op "root" (deps [first, second]) $ \x -> x{ref = refOf "root"}
  where
    first = op "contested" nodeps $ \x -> x{ref = refOf "contested", help = "first"}
    second = op "contested" nodeps $ \x -> x{ref = refOf "contested", help = "second"}

{- | The overwhelmingly common case — one node reached by several paths —
replaces a representative with an indistinguishable one on every re-encounter.
That must not be reported, or the report would be pure noise. Uses 'inject' as
well as 'deps' so the two occurrences are structurally different ('Connect'
vs 'Vertices') while the node is literally the same value.
-}
noSelfConflict :: IO ()
noSelfConflict = do
    let apex = leaf "apex"
        left = op "left" (deps [apex]) $ \x -> x{ref = refOf "left"}
        right = (op "right" nodeps $ \x -> x{ref = refOf "right"}) `inject` apex
        root = op "root" (deps [left, right]) $ \x -> x{ref = refOf "root"}
    assertEqual "" 0 (length (Dag.dagConflicts (foldOf root)))

rootsAreUndepended :: IO ()
rootsAreUndepended = do
    let (root, _, _, _) = diamond
    assertEqual "just the root" [refOf "root"] (Dag.roots (foldOf root))

-- | The teardown driver is the one caller of the fold today, so it is where
-- an operator actually learns about a contested node.
downTreeReportsConflict :: IO ()
downTreeReportsConflict = do
    reports <- runDownCapturing twoWriters
    assertEqual
        "one Conflicting, naming the contested ref"
        [refOf "contested"]
        [aref | UpDown.Conflicting aref _ _ <- reports]

{- | 'Dag.fromMagma' is 'Dag.dagEdges'' inverse, and is how a driver that
keeps nodes and precedence separately — "Salmon.Op.Ledger", where edges have
to be retractable — gets back something walkable.
-}
fromMagmaRoundTrips :: IO ()
fromMagmaRoundTrips = do
    let (root, _, _, _) = diamond
        dag = foldOf root
        rebuilt = Dag.fromMagma (Dag.dagNodes dag) (Dag.dagEdges dag)
    assertEqual "same nodes" (Map.keysSet (Dag.dagNodes dag)) (Map.keysSet (Dag.dagNodes rebuilt))
    assertEqual "same edges" (Dag.dagEdges dag) (Dag.dagEdges rebuilt)
    assertEqual
        "and the direction a teardown needs survives"
        (Set.fromList (Dag.dependantsOf dag (refOf "apex")))
        (Set.fromList (Dag.dependantsOf rebuilt (refOf "apex")))

{- | (I5): 'Data.Dynamic.Dynamic' renders as its type alone by default, which
used to make two 'Salmon.Op.Supervision.Supervision' declarations compare
equal here regardless of content — the gap that let
'Salmon.Actions.Upkeep.startUpkeep' adopt a machine whose policy had changed
underneath it. 'Dag.representative' now special-cases 'Supervision' to
compare by value, so a re-declaration that only changes the policy is a
genuine conflict, exactly like 'twoWriters' changing @help@ is.
-}
changedSupervisionIsAConflict :: IO ()
changedSupervisionIsAConflict = do
    let dag = foldOf (supervisionTwoWriters OneForOne RestForOne)
    case Dag.dagConflicts dag of
        [c] -> assertEqual "on the contested ref" (refOf "contested") c.conflictRef
        other -> assertEqual "exactly one conflict" 1 (length other)

-- | The overwhelmingly common re-declaration case — same policy, reached
-- again — must not be reported, same reasoning as 'noSelfConflict'.
sameSupervisionIsNotAConflict :: IO ()
sameSupervisionIsNotAConflict = do
    let dag = foldOf (supervisionTwoWriters OneForOne OneForOne)
    assertEqual "no conflict when the policy did not change" 0 (length (Dag.dagConflicts dag))

-- | One effect site, two declarations differing only in their
-- 'Salmon.Op.Supervision.Strategy', everything else ('help' included)
-- identical.
supervisionTwoWriters :: Strategy -> Strategy -> Op
supervisionTwoWriters s1 s2 =
    op "root" (deps [first, second]) $ \x -> x{ref = refOf "root"}
  where
    withStrategy s = supervised defaultSupervision{supStrategy = s}
    first = op "contested" nodeps $ \x -> x{ref = refOf "contested", dynamics = [withStrategy s1]}
    second = op "contested" nodeps $ \x -> x{ref = refOf "contested", dynamics = [withStrategy s2]}

{- | A 'Dag' built from a flat edge set can describe a cycle, which a 'Dag'
folded from an expanded 'Cofree' cannot — so this is a hazard that only
arrived with the ledger, where two declarations can each contribute one leg
of it. A node on a cycle never becomes ready, and the walk used to leave it
silently unapplied while still reporting success. It is 'Blocked' now.
-}
cycleIsBlocked :: IO ()
cycleIsBlocked = do
    let a = leaf "a"
        b = leaf "b"
        magma =
            Map.fromList
                [ (r, act)
                | o <- [a, b]
                , Just act <- [opAct o]
                , let r = act.extension.ref
                ]
        -- a depends on b and b depends on a: neither can ever be first.
        looped = Set.fromList [(refOf "a", refOf "b"), (refOf "b", refOf "a")]
        dag = Dag.fromMagma magma looped
    (r, readBack) <- capture
    ok <- UpDown.upDag (const (pure UpDown.Required)) r dag
    reports <- readBack
    assertBool "the walk reports failure rather than vacuous success" (not ok)
    assertEqual
        "both nodes are named"
        2
        (length [() | UpDown.Blocked _ <- reports])
    assertEqual "and nothing was evaluated" 0 (length [() | UpDown.Eval _ <- reports])
