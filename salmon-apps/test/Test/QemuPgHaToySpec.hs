{- | Layer 0: what @salmon-toy-qemu-pg-ha@ declares, read as @run serve@
reads it. No guest is booted and no node's @up@ is run (bar the writer's,
which is there to throw).

The toy under @serve@ is several declarations standing at once -- the
guests, the pair, a writer, a fault or two -- and what makes that work is
all in how their graphs overlap: a node two seeds name has to be /one/ node,
described the same way by both, or the loop reports a collision and the
second declaration re-applies what the first already did. These tests are
that overlap, written down:

* moving the primary redescribes the role node and nothing else, so the
  pass that follows the declaration boots nothing and re-provisions nothing;
* @guests@, @frozen@ and @partition@ name the machines exactly as @up@ does;
* the writer is a held action, which is the only kind that keeps writing
  while somebody else's command runs.
-}
module Test.QemuPgHaToySpec (tests) where

import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- GHC only solves a `HasField` constraint when the selector is in scope, and
-- `Dag.sameRepresentative` needs these whether or not this module says them.
import Salmon.Builtin.Extension (Extension, Op, dynamics, evalDeps, help, managed, notes, ref, up)
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)
import Salmon.Op.Track (Track (..))

import QemuPgHaToy
import qualified SreBox.PostgresPair as Pair

tests :: TestTree
tests =
    testGroup
        "salmon-toy-qemu-pg-ha (the declarations `run serve` holds)"
        [ testCase "moving the primary redescribes the role node and nothing else" moveTouchesOnlyTheRole
        , testCase "a move adds no node and retires none but the first clone's" moveKeepsTheNodes
        , testCase "guests is a part of up, described the same way" guestsIsPartOfUp
        , testCase "a fault stands on the guests without redescribing them" faultsShareTheGuests
        , testCase "each fault is its own node, keyed by what it cuts" faultsAreKeyed
        , testCase "the writer is held, and refuses a one-shot up" writerIsHeld
        , testCase "the partition is a route set and a route removed" partitionScripts
        , testCase "the writer's line carries the three numbers" writerLine
        ]

-------------------------------------------------------------------------------

host :: Host
host =
    Host
        { hostRoot = "/var/lib/salmon-toy-pg-ha"
        , hostUser = "operator"
        , hostUnitDir = "/home/operator/.config/systemd/user"
        , hostRuntimeDir = "/run/user/1000"
        , hostBoot = [(machineName m, "/boot/vmlinuz", "/boot/initrd.img") | m <- machines]
        }

upSpec :: Pair.Side -> Maybe Pair.Side -> Spec
upSpec primary clone =
    Up
        { specRoot = host.hostRoot
        , specPair = (thePair host.hostRoot){Pair.pair_primary = primary, Pair.pair_seed = clone}
        , specUser = host.hostUser
        , specUnitDir = host.hostUnitDir
        , specRuntimeDir = host.hostRuntimeDir
        , specBoot = host.hostBoot
        }

dagOf :: Spec -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps . graph
  where
    graph :: Spec -> Op
    graph = run program

nodesOf :: Spec -> Map.Map Ref (Act Extension)
nodesOf = Dag.dagNodes . dagOf

-- | The refs two declarations both name and describe differently: what `serve` would re-apply.
redescribed :: Spec -> Spec -> [Act Extension]
redescribed old new =
    [ b
    | (rf, b) <- Map.toList (nodesOf new)
    , Just a <- [Map.lookup rf (nodesOf old)]
    , not (Dag.sameRepresentative a b)
    ]

helps :: [Act Extension] -> [Text.Text]
helps = map (\a -> a.extension.help)

-------------------------------------------------------------------------------

moveTouchesOnlyTheRole :: IO ()
moveTouchesOnlyTheRole = do
    let changed = redescribed (upSpec Pair.A Nothing) (upSpec Pair.B Nothing)
    assertEqual ("redescribed: " <> show (helps changed)) 1 (length changed)
    -- and the declaration itself is free of collisions
    assertEqual "collisions inside one declaration" 0 (length (Dag.dagConflicts (dagOf (upSpec Pair.B Nothing))))

moveKeepsTheNodes :: IO ()
moveKeepsTheNodes = do
    let plainA = Map.keysSet (nodesOf (upSpec Pair.A Nothing))
        plainB = Map.keysSet (nodesOf (upSpec Pair.B Nothing))
        seeded = Map.keysSet (nodesOf (upSpec Pair.A (Just Pair.B)))
    assertEqual "the same nodes whichever side is the primary" plainA plainB
    -- `--seed` is one node more (the first clone), so retiring the
    -- declaration that carried it sends exactly that node down -- and the
    -- recipe gives it no `down`, a clone not being a thing to undo.
    assertEqual "nothing a seeded declaration lacks" Set.empty (Set.difference plainB seeded)
    assertEqual "one node only the seeded declaration has" 1 (Set.size (Set.difference seeded plainB))

guestsIsPartOfUp :: IO ()
guestsIsPartOfUp = do
    let g = nodesOf (Guests host)
        u = nodesOf (upSpec Pair.A Nothing)
        own = Map.filter (\a -> a.extension.help == "the toy's three guests, booted and answering") g
        shared = Map.difference g own
    assertEqual "one node of its own" 1 (Map.size own)
    assertBool "three guests' worth of nodes" (Map.size shared > 9)
    assertEqual "every other node is one `up` declares" Set.empty (Set.difference (Map.keysSet shared) (Map.keysSet u))
    assertEqual "and describes the same way" [] (helps (redescribed (upSpec Pair.A Nothing) (Guests host)))

faultsShareTheGuests :: IO ()
faultsShareTheGuests = do
    let g = nodesOf (Guests host)
        faults = [Frozen host MachineB, Partition host MachineA MachineB]
    mapM_
        ( \f -> do
            let extra = Map.difference (nodesOf f) g
            assertEqual ("nodes beyond the guests': " <> show (helps (Map.elems extra))) 1 (Map.size extra)
            assertEqual "redescribed guests" [] (helps (redescribed (Guests host) f))
        )
        faults

faultsAreKeyed :: IO ()
faultsAreKeyed = do
    let own s = Map.keysSet (Map.difference (nodesOf s) (nodesOf (Guests host)))
        all' =
            [ own (Frozen host MachineA)
            , own (Frozen host MachineB)
            , own (Partition host MachineA MachineB)
            , own (Partition host MachineB MachineA)
            , own (Partition host MachineA MachineBouncer)
            ]
    assertEqual "five faults, five nodes" 5 (Set.size (Set.unions all'))

writerIsHeld :: IO ()
writerIsHeld = do
    let nodes = Map.elems (nodesOf (Writer (thePair host.hostRoot)))
    case nodes of
        [w] -> do
            assertBool "it has a managed action" (maybe False (const True) w.extension.managed)
            r <- try w.extension.up
            case r of
                Left (e :: SomeException) -> assertBool (show e) ("run serve" `isInfixOf` show e)
                Right () -> assertBool "a one-shot up must not pretend to have started a writer" False
        _ -> assertBool ("expected one node, got " <> show (length nodes)) False

partitionScripts :: IO ()
partitionScripts = do
    assertEqual "cut" "ip route replace blackhole 10.98.0.3/32" (blackholeScript MachineB)
    assertBool "heal names the same route" ("ip route del blackhole 10.98.0.3/32" `isInfixOf` healScript MachineB)

writerLine :: IO ()
writerLine = do
    assertEqual
        "healthy"
        "acknowledged 25, errors 0, acknowledged rows missing 0 (last row 125)"
        (tallyLine emptyTally{tallyAcked = 25, tallyLast = 125})
    assertBool
        "an audit that could not run says so rather than saying zero"
        ("unknown" `Text.isInfixOf` tallyLine emptyTally{tallyMissing = Nothing})

_unused :: ()
_unused = const () (dynamics, notes, ref)
