{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for 'Salmon.Op.Ledger': who still wants which nodes,
and which edges are still the description of how to take them down.

Pure throughout — a 'Ledger' holds nothing but 'Ref's and pairs of them, so
none of this needs a graph, an 'Salmon.Builtin.Extension.Op', or an effect.
The four cases in the middle are the four ways refcounting a node instead of
keeping a set goes wrong; they are here as tests rather than as a comment
because each of them is a bug someone would otherwise reintroduce.
-}
module Test.LedgerSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Builtin.Extension (Op, deps, dynamics, evalDeps, help, nodeps, notes, op, ref)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ledger (Contribution (..))
import qualified Salmon.Op.Ledger as Ledger
import Salmon.Op.Ref (Ref, mkRef)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.Ledger"
        [ testCase "a contribution is a graph's nodes and edges, flattened" contributionFromDag
        , testCase "two declarations wanting one node do not cancel out" unionNotCancel
        , testCase "a node reached twice in one graph is one membership" oneMembership
        , testCase "retracting what was never declared is a no-op" retractAbsent
        , testCase "re-declaring the same key replaces rather than adds" redeclareReplaces
        , testCase "a retraction keeps its edges but drops its refs from desired" retractKeepsEdges
        , testCase "collect drops a retired declaration once its nodes settle" collectSettled
        , testCase "collect never drops a live declaration" collectKeepsLive
        , testCase "`only` retires every other declaration" retractOthers
        ]

-------------------------------------------------------------------------------

refOf :: Text -> Ref
refOf = mkRef "ledgerspec"

leaf :: Text -> Op
leaf name = op name nodeps $ \x -> x{ref = refOf name}

over :: Text -> [Op] -> Op
over name ps = op name (deps ps) $ \x -> x{ref = refOf name}

contributionOf :: Op -> Contribution
contributionOf = Ledger.contribution . Dag.foldDag Dag.sameRepresentative . evalDeps

-------------------------------------------------------------------------------

contributionFromDag :: IO ()
contributionFromDag = do
    let c = contributionOf (over "root" [leaf "a", leaf "b"])
    assertEqual "all three nodes" (Set.fromList (map refOf ["root", "a", "b"])) c.contribRefs
    assertEqual
        "one edge per dependency, as (dependency, dependant)"
        (Set.fromList [(refOf "a", refOf "root"), (refOf "b", refOf "root")])
        c.contribEdges
    assertBool "and it starts live" c.contribLive

{- | The hazard: under refcounting, @down g1@ takes the shared node's count to
zero via a decrement that @g2@ never agreed to. Under sets, @g2@'s set still
has it.
-}
unionNotCancel :: IO ()
unionNotCancel = do
    let shared = leaf "shared"
        l =
            Ledger.declare "g2" (contributionOf (over "r2" [shared])) $
                Ledger.declare "g1" (contributionOf (over "r1" [shared])) Ledger.emptyLedger
        l' = Ledger.retract "g1" l
    assertBool "shared is still wanted by g2" (Set.member (refOf "shared") (Ledger.desired l'))
    assertBool "but g1's own node is not" (Set.notMember (refOf "r1") (Ledger.desired l'))

-- | The hazard: a diamond double-counts its apex, so one @down@ leaves it up.
oneMembership :: IO ()
oneMembership = do
    let apex = leaf "apex"
        c = contributionOf (over "root" [over "left" [apex], over "right" [apex]])
    assertEqual "four nodes, apex once" 4 (Set.size c.contribRefs)
    let l = Ledger.retract "g" (Ledger.declare "g" c Ledger.emptyLedger)
    assertEqual "and one retraction wants nothing" Set.empty (Ledger.desired l)

-- | The hazard: a decrement of a key that was never incremented goes negative.
retractAbsent :: IO ()
retractAbsent = do
    let l = Ledger.declare "g" (contributionOf (leaf "a")) Ledger.emptyLedger
    assertEqual "retracting a stranger changes nothing" l (Ledger.retract "other" l)

-- | The hazard: a second @up@ of the same seed takes the count to 2, so the
-- @down@ that follows leaves everything stranded up.
redeclareReplaces :: IO ()
redeclareReplaces = do
    let c = contributionOf (leaf "a")
        l = Ledger.declare "g" c (Ledger.declare "g" c Ledger.emptyLedger)
    assertEqual "one entry" 1 (Map.size l)
    assertEqual "and one retraction empties it" Set.empty (Ledger.desired (Ledger.retract "g" l))

{- | The reason edges live in the ledger at all: retracting is the moment the
teardown order matters most, so a retraction must not take the edges with it.
-}
retractKeepsEdges :: IO ()
retractKeepsEdges = do
    let c = contributionOf (over "root" [leaf "a"])
        l = Ledger.retract "g" (Ledger.declare "g" c Ledger.emptyLedger)
    assertEqual "nothing is wanted up any more" Set.empty (Ledger.desired l)
    assertEqual
        "but the edge saying root comes down before a is still there"
        (Set.fromList [(refOf "a", refOf "root")])
        (Ledger.precedenceOf l)
    assertEqual "and the nodes are still known" 2 (Set.size (Ledger.knownRefs l))

collectSettled :: IO ()
collectSettled = do
    let l = Ledger.retract "g" (Ledger.declare "g" (contributionOf (leaf "a")) Ledger.emptyLedger)
    assertEqual "kept while its node is still coming down" 1 (Map.size (Ledger.collect (const True) l))
    assertEqual "collected once it has settled" 0 (Map.size (Ledger.collect (const False) l))

collectKeepsLive :: IO ()
collectKeepsLive = do
    let l = Ledger.declare "g" (contributionOf (leaf "a")) Ledger.emptyLedger
    assertEqual
        "a live declaration is what holds its nodes up, however settled they are"
        1
        (Map.size (Ledger.collect (const False) l))

retractOthers :: IO ()
retractOthers = do
    let l =
            Ledger.declare "g2" (contributionOf (leaf "b")) $
                Ledger.declare "g1" (contributionOf (leaf "a")) Ledger.emptyLedger
        l' = Ledger.retractOthers "g2" l
    assertEqual "only g2 is live" (Set.singleton "g2") (Ledger.liveKeys l')
    assertEqual "so only b is wanted" (Set.singleton (refOf "b")) (Ledger.desired l')
    assertEqual "both are still known, for the teardown" 2 (Set.size (Ledger.knownRefs l'))
