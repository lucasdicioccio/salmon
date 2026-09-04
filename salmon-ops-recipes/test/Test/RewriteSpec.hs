{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Op.Rewrite" and the one rewrite that ships
with it, 'Salmon.Builtin.Nodes.Debian.Package.batchPackages'.

Nothing here runs @apt-get@: what is under test is the /shape/ of the
computed graph — which nodes a batch swallowed, which way round the batches
are ordered, what the edges into a swallowed node were redirected onto, and
that the membership bookkeeping keeps a batch legible in terms of the nodes
an operator actually declared.
-}
module Test.RewriteSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Builtin.Extension (Extension, Op, deps, dynamics, evalDeps, help, nodeps, notes, op, ref)
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Rewrite (Phase (..))
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Reporter (silent)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.Rewrite"
        [ testCase "a run-up phase batches every package into one install node" allInstalled
        , testCase "a run-down phase batches every package into one removal node" allRemoved
        , testCase "a mixed phase splits by direction, removals first" splitByDirection
        , testCase "still-wanted wins: one live declaration protects a package" conservativePartition
        , testCase "edges into a batched node are redirected onto the batch" edgesRedirect
        , testCase "an ignored node is left out of the batch" ignoredIsLeftAlone
        , testCase "a batch stands in for its members, other nodes for themselves" membership
        , testCase "no packages means no rewrite at all" noPackagesNoOp
        ]

-------------------------------------------------------------------------------

pkg :: Text -> Op
pkg = Debian.deb . Debian.Package

pkgRef :: Text -> Ref
pkgRef name = mkRef "debian-deb" name

-- | A consumer that depends on a package, the way a recipe would.
consumer :: Text -> [Op] -> Op
consumer name ds = op name (deps ds) $ \x -> x{ref = mkRef "consumer" name}

computeWith :: Phase -> Op -> Rewrite.Rewritten Extension
computeWith phase o =
    Rewrite.rewrite [Debian.batchPackages silent] phase (Dag.foldDag Dag.sameRepresentative (evalDeps o))

-- | Every node of the computed graph, by shorthand.
shorthands :: Rewrite.Rewritten Extension -> [Text]
shorthands c = [act.shorthand | act <- Map.elems (Dag.dagNodes (Rewrite.computedDag c))]

-- | Only rewrites record membership, so these are exactly the nodes a
-- rewrite introduced.
batchRefs :: Rewrite.Rewritten Extension -> [Ref]
batchRefs = Map.keys . Rewrite.computedMembers

-- | The one batch this graph produced; fails the test rather than throwing
-- if a rewrite produced none or several.
onlyBatch :: Rewrite.Rewritten Extension -> IO Ref
onlyBatch c =
    case batchRefs c of
        [r] -> pure r
        rs -> assertFailure ("expected exactly one batch, got " <> show (length rs))

helpOf :: Rewrite.Rewritten Extension -> Ref -> Text
helpOf c r = maybe "" (\act -> act.extension.help) (Dag.representativeOf (Rewrite.computedDag c) r)

-------------------------------------------------------------------------------

root3 :: Op
root3 = op "root" (deps [pkg "curl", pkg "git", pkg "jq"]) $ \x -> x{ref = mkRef "root" ()}

allRefs :: Op -> Set Ref
allRefs = Set.fromList . Dag.dagOrder . Dag.foldDag Dag.sameRepresentative . evalDeps

allInstalled :: IO ()
allInstalled = do
    let c = computeWith (Phase (allRefs root3) Set.empty) root3
    assertEqual "three deb nodes became one" 1 (length (filter (== "debs") (shorthands c)))
    assertEqual "and no deb node survives" 0 (length (filter (== "deb") (shorthands c)))
    batch <- onlyBatch c
    assertEqual
        "which stands in for all three"
        (Set.fromList (map pkgRef ["curl", "git", "jq"]))
        (Rewrite.membersOf c batch)

allRemoved :: IO ()
allRemoved = do
    -- `run down`: nothing is desired, so the same registered phase emits a
    -- teardown batch instead of an install batch.
    let c = computeWith (Phase Set.empty Set.empty) root3
    batch <- onlyBatch c
    assertEqual
        "described as a removal, not an install"
        "removes 3 packages in one apt-get"
        (helpOf c batch)
    assertEqual
        "still standing in for all three"
        (Set.fromList (map pkgRef ["curl", "git", "jq"]))
        (Rewrite.membersOf c batch)

{- | The case nothing before the fold can express: some packages on their way
in, others on their way out, in one graph. Two batches, and a precedence edge
putting the removal first because both want the dpkg lock.
-}
splitByDirection :: IO ()
splitByDirection = do
    let desired = Set.fromList [pkgRef "curl", mkRef "root" ()]
        c = computeWith (Phase desired Set.empty) root3
    assertEqual "two batches" 2 (length (batchRefs c))
    let [(installRef, _)] = [(r, m) | (r, m) <- Map.toList (Rewrite.computedMembers c), m == Set.singleton (pkgRef "curl")]
        [(removeRef, _)] = [(r, m) | (r, m) <- Map.toList (Rewrite.computedMembers c), m == Set.fromList [pkgRef "git", pkgRef "jq"]]
    assertEqual
        "the install batch waits on the removal batch"
        [removeRef]
        (Dag.dependenciesOf (Rewrite.computedDag c) installRef)

{- | Two declarations disagree about @curl@ — one is being retracted, the
other still wants it. The ledger already answered that by union, and the
rewrite inherits the answer rather than deriving its own: @curl@ must not end
up in a removal batch, or the retraction would uninstall it out from under
the declaration still standing on it.
-}
conservativePartition :: IO ()
conservativePartition = do
    let c = computeWith (Phase (Set.singleton (pkgRef "curl")) Set.empty) root3
    let inRemoval = Set.unions [m | (_, m) <- Map.toList (Rewrite.computedMembers c), Set.notMember (pkgRef "curl") m]
    assertBool "curl is not swept into the removal batch" (Set.notMember (pkgRef "curl") inRemoval)
    assertEqual "the other two are" (Set.fromList [pkgRef "git", pkgRef "jq"]) inRemoval

{- | The old @removeSinglePackages@ blanked the per-package nodes and injected
the batch under the root, which only worked because the batch depended on
nothing. A rewrite redirects the actual edges, so a node that depended on
@deb curl@ now depends on whatever installs curl.
-}
edgesRedirect :: IO ()
edgesRedirect = do
    let one = pkg "curl"
        user = consumer "needs-curl" [one]
        root = op "root" (deps [user]) $ \x -> x{ref = mkRef "root" ()}
        c = computeWith (Phase (allRefs root) Set.empty) root
    batch <- onlyBatch c
    assertEqual
        "the consumer now waits on the batch"
        [batch]
        (Dag.dependenciesOf (Rewrite.computedDag c) (mkRef "consumer" ("needs-curl" :: Text)))
    assertEqual
        "and the batch knows who waits on it"
        [mkRef "consumer" ("needs-curl" :: Text)]
        (Dag.dependantsOf (Rewrite.computedDag c) batch)

{- | A plan's excluded refs, or a @converge --select@'s complement. Batching
one of those would run the work the operator asked to skip, under another
node's name.
-}
ignoredIsLeftAlone :: IO ()
ignoredIsLeftAlone = do
    let c = computeWith (Phase (allRefs root3) (Set.singleton (pkgRef "jq"))) root3
    assertEqual "jq survives as its own node" 1 (length (filter (== "deb") (shorthands c)))
    assertBool
        "and is in no batch"
        (all (Set.notMember (pkgRef "jq")) (Map.elems (Rewrite.computedMembers c)))
    assertEqual
        "jq stands in for itself"
        (Set.singleton (pkgRef "jq"))
        (Rewrite.membersOf c (pkgRef "jq"))

membership :: IO ()
membership = do
    let c = computeWith (Phase (allRefs root3) Set.empty) root3
    assertEqual
        "a node no rewrite touched stands in for itself"
        (Set.singleton (mkRef "root" ()))
        (Rewrite.membersOf c (mkRef "root" ()))

noPackagesNoOp :: IO ()
noPackagesNoOp = do
    let root = op "root" (deps [consumer "a" []]) $ \x -> x{ref = mkRef "root" ()}
        dag = Dag.foldDag Dag.sameRepresentative (evalDeps root)
        c = computeWith (Phase (allRefs root) Set.empty) root
    assertEqual "no batch introduced" [] (batchRefs c)
    assertEqual "same nodes as declared" (Dag.dagOrder dag) (Dag.dagOrder (Rewrite.computedDag c))
