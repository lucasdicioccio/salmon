{-# LANGUAGE OverloadedStrings #-}

{- | Layer 3: a real switchover between two real machines, driven by
"SreBox.PostgresPair" from a third (the test process, standing in for the
controller a deployment would run this from).

This is scenario S1 of @specs\/pg-switchover.md@, minus the client-error
assertions, which need the bouncers of phase 4. What it does claim is the
part no Layer 0 table can: that the steps in 'SreBox.PostgresPair.Step'
really do move a primary from one machine to the other, that the machine
left behind comes back as a standby of the new primary through @pg_rewind@
rather than a re-clone, and that running the whole thing again when it is
already true changes nothing.

It reuses the two rootfses and the fixture binary of
"Test.PostgresReplicationSpec" (see that module for what they need and how
to build them), plus what a switchover needs and plain replication does not:
a rewind role, and @pg_hba.conf@ lines on /both/ machines, since after the
first switchover each of them has to accept the other streaming from it.
-}
module Test.PostgresSwitchoverSpec (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import Test.Harness
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Reporter (silent)
import qualified SreBox.PostgresPair as Pair
import Test.PostgresVms

tests :: TestTree
tests =
    -- one pair of VMs, one bridge, two addresses: these cases cannot run at
    -- the same time as each other any more than the specs around them can.
    sequentialTestGroup
        "Postgres switchover (Layer 3, a primary moved between two VMs)"
        AllFinish
        [ testCase "switches the primary over, and back, without losing a row" switchesOverAndBack
        , testCase "a switchover stopped part-way is finished by the next pass" resumesAfterInterruption
        , testCase "a crashed primary is failed over only when its writes are declared expendable" failsOverFromACrash
        , testCase "a partition is waited out, not acted on" holdsThroughAPartition
        , testCase "a failover across a partition leaves two primaries, and rewinds one" splitBrainIsRewound
        , testCase "a standby that falls off the slot budget is said so, not silently re-seeded, and re-seeded once declared" theSlotBudgetBoundsTheDisk
        , testCase "a pair stopped in either order comes back with the declared primary, losing nothing" recoversFromBothStopped
        , testCase "a stranger's cluster where a member should be is refused, and nothing is deleted" refusesAStrangersCluster
        ]

{- | S8: a stranger's cluster where a member of the pair should be.

A machine is rebuilt, or a name is reused, or a directive is pointed at the
wrong address: the address answers, the cluster name matches, the port is
right, and what is there has never met the other machine. Every step in the
table would then be applied to somebody else's data, which is why the system
identifiers are compared before anything else is decided.

The direction that matters is the one with 'SreBox.PostgresPair.pair_may_discard'
set. That flag says which side's writes may go, and it presumes the two sides
are the same cluster; read as a general licence to destroy, it would let a
pass rewind a real cluster onto a stranger's. So the test declares it both
ways round and asserts the refusal survives both -- and then that neither
data directory was touched, which is the assertion a refusal is actually
about.
-}
refusesAStrangersCluster :: IO ()
refusesAStrangersCluster = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            resetRows a
            insertRow a "on-the-real-cluster"
            waitForRows b ["on-the-real-cluster"]

            -- the standby's machine is rebuilt: same address, same cluster
            -- name, same port, and a cluster that has never met A.
            resetCluster b
            psqlOrDie b "CREATE DATABASE precious;"
            strangers <- dataDirectoryIdentity b
            ours <- dataDirectoryIdentity a

            -- nothing about the declaration has changed
            let declared = pairWith a b Pair.A
            step <- Pair.decide declared
            assertBool ("expected a refusal, got " <> show step) (refuses step)
            assertBool ("the reason does not say what is wrong: " <> show step) ("cluster" `isInfixOf` show step)
            ok <- runUp (Pair.pairRole silent declared)
            assertBool "a pass across two different clusters was reported a success" (not ok)

            -- and saying whose writes may go does not change it, in either
            -- direction: declared at the real cluster or at the stranger.
            forM_ [Pair.A, Pair.B] $ \side -> do
                let p = (pairWith a b (Pair.other side)){Pair.pair_may_discard = Just side}
                flagged <- Pair.decide p
                assertBool ("expected a refusal with may_discard = " <> show side <> ", got " <> show flagged) (refuses flagged)
                acted <- runUp (Pair.pairRole silent p)
                assertBool ("a pass with may_discard = " <> show side <> " was reported a success") (not acted)

            -- neither machine was touched by any of that
            assertEqual "the stranger's data directory was replaced" strangers =<< dataDirectoryIdentity b
            assertEqual "our own data directory was replaced" ours =<< dataDirectoryIdentity a
            (_, dbs, _) <- psql b "SELECT datname FROM pg_database WHERE datname = 'precious';"
            assertBool ("the stranger's database is gone: " <> dbs) ("precious" `isInfixOf` dbs)
            assertPrimaryIs a
            waitForRows a ["on-the-real-cluster"]

{- | S7: both machines are stopped, and the one declared primary is the one
that stopped first.

The easy half is a maintenance window: the primary goes down first, so its
last record reaches the standby on the way out, and bringing the pair back up
with the roles swapped costs nothing. The interesting half is the other
order. Stop the /standby/ first, let the primary write one more thing, then
stop that too, and declare the machine that was already gone: what the pair
must not do is promote it, because the other machine holds a write it has
never seen.

There is no waiting its way out of that, either. A standby catches up by
streaming, and the machine it would stream from is stopped -- so the only way
to converge on the declaration without losing the write is to start the old
primary again, let the standby catch up from it, and then do the ordinary
switchover. The test asserts the write survives, which is the only assertion
that can tell that apart from a promotion that happened to be quick.
-}
recoversFromBothStopped :: IO ()
recoversFromBothStopped = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            resetRows a
            insertRow a "before-the-maintenance-window"
            waitForRows b ["before-the-maintenance-window"]

            -- the primary goes down first, so the standby has its last record
            stopCluster a
            stopCluster b
            let toB = pairWith a b Pair.B
            passOrExplain "bringing the pair up with B declared" toB
            assertPrimaryIs b
            assertStandbyOf a testVmAddr2

            -- and now the other order, with the declared primary the one
            -- that stopped first and so missed what came after
            stopCluster a
            insertRow b "written-after-the-standby-stopped"
            stopCluster b
            let toA = pairWith a b Pair.A
            passOrExplain "bringing the pair up with A declared" toA
            assertPrimaryIs a
            assertStandbyOf b testVmAddr

            -- declaring the machine that was behind lost nothing
            waitForRows a ["before-the-maintenance-window", "written-after-the-standby-stopped"]
            waitForRows b ["before-the-maintenance-window", "written-after-the-standby-stopped"]

{- | S6: the standby goes away and stays away, and the primary's disk does
not follow it down.

A replication slot is a promise to keep WAL until the standby has it, and an
unbounded promise is how a machine that is merely /down/ takes the machine
that is /up/ with it. @max_slot_wal_keep_size@ is the price cap on that
promise: past it the slot is invalidated, the WAL is recycled, and the
standby -- which can now never catch up -- is the only thing that was lost.

That is a good trade and a terrible surprise, so the pair has to say it out
loud. A lost slot is not a state to rewind out of: @pg_rewind@ would succeed,
change nothing, and hand back a standby that still cannot replay what is no
longer there. The only way back is a re-seed, which is a decision about
throwing a machine's data away and therefore an operator's, so the check says
'Salmon.Actions.UpDown.Unknown' with the slot named in it, and the pass does
nothing at all.
-}
theSlotBudgetBoundsTheDisk :: IO ()
theSlotBudgetBoundsTheDisk = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            resetRows a
            insertRow a "before-the-slot-budget"

            -- one switchover first, so that the slot in play is the pair's
            -- own: what the fixture set up streams with a slot of the
            -- fixture's making, and this is a test about the pair's.
            let p = pairWith a b Pair.B
            passOrExplain "the switchover" p
            assertStandbyOf a testVmAddr2
            let slot = Text.unpack (Pair.slotNameFor p Pair.A)

            -- a budget small enough to go past on purpose
            psqlOrDie b "ALTER SYSTEM SET max_slot_wal_keep_size = '32MB';"
            psqlOrDie b "ALTER SYSTEM SET max_wal_size = '64MB';"
            psqlOrDie b "SELECT pg_reload_conf();"

            stopCluster a
            churnWal b 24

            waitFor ("the slot " <> slot <> " never fell off the budget") $ do
                (_, out, _) <- psql b ("SELECT wal_status FROM pg_replication_slots WHERE slot_name = '" <> slot <> "';")
                pure ("lost" `isInfixOf` out, out)
            wal <- walMegabytes b
            assertBool
                ("the primary's WAL followed the standby down: " <> show wal <> "MB of pg_wal")
                (wal < 250)

            -- the pair says what happened, names the slot, and touches nothing
            identity <- dataDirectoryIdentity a
            step <- Pair.decide p
            assertEqual ("a lost slot is not a failure and not a success: " <> show step) Unknown (Pair.verdict step)
            assertBool ("the reason does not name the slot: " <> show step) (slot `isInfixOf` show step)
            ok <- runUp (Pair.pairRole silent p)
            assertBool "a pass over a pair with a lost slot failed" ok
            identity' <- dataDirectoryIdentity a
            assertEqual "the standby was re-seeded without anybody asking" identity identity'

            -- and the machine that is still up is still serving
            assertPrimaryIs b
            insertRow b "written-after-the-slot-was-lost"
            waitForRows b ["before-the-slot-budget", "written-after-the-slot-was-lost"]

            -- the operator now says that machine may be rebuilt: the same pass
            -- that only said so before wipes it, clones it again, swaps the
            -- lost slot for a live one, and the pair is whole (S6, continued).
            let reseed = p{Pair.pair_reseed = Just Pair.A}
            okReseed <- runUp (Pair.pairRole silent reseed)
            assertBool "the re-seeding pass failed" okReseed
            identity'' <- dataDirectoryIdentity a
            assertBool "the standby was declared rebuildable and was not rebuilt" (identity'' /= identity)
            assertStandbyOf a testVmAddr2
            after <- Pair.decide reseed
            assertEqual ("the pair is not whole after the re-seed: " <> show after) Success (Pair.verdict after)
            waitFor ("the slot " <> slot <> " is not live again") $ do
                (_, out, _) <- psql b ("SELECT wal_status FROM pg_replication_slots WHERE slot_name = '" <> slot <> "';")
                pure (any (`isInfixOf` out) ["reserved", "extended"], out)
            -- and it is a standby again in the sense that matters
            insertRow b "written-after-the-reseed"
            waitForRows a ["before-the-slot-budget", "written-after-the-slot-was-lost", "written-after-the-reseed"]

            -- put the budget back: this rootfs outlives the VM, and a 32MB
            -- cap is a trap to leave lying around for the next spec.
            psqlOrDie b "ALTER SYSTEM RESET max_slot_wal_keep_size;"
            psqlOrDie b "ALTER SYSTEM RESET max_wal_size;"
            psqlOrDie b "SELECT pg_reload_conf();"
            psqlOrDie b "DROP TABLE IF EXISTS salmon_churn;"

{- | Writes enough WAL to go past a small budget, in the cheapest way there
is: a row so that the segment is not empty (@pg_switch_wal@ does nothing to
one that is), then a switch to the next, then a checkpoint to make the
primary act on what it now may throw away.
-}
churnWal :: VmAccess -> Int -> IO ()
churnWal vm n = do
    psqlOrDie vm "CREATE TABLE IF NOT EXISTS salmon_churn (v int);"
    forM_ [1 .. n] $ \i -> do
        psqlOrDie vm ("INSERT INTO salmon_churn VALUES (" <> show (i :: Int) <> ");")
        psqlOrDie vm "SELECT pg_switch_wal();"
    psqlOrDie vm "CHECKPOINT;"
    psqlOrDie vm "CHECKPOINT;"

{- | S5: fail over while the old primary is still up and still taking
writes, then let the partition heal and watch salmon find two primaries.

This is the scenario the whole design is careful about, and the only one
where salmon knowingly destroys writes that were acknowledged to a client.
It needs a partition that hides A from /the controller/ as well as from B --
otherwise there is nothing to fail over from: a primary that can be reached
is simply stopped, and that is a switchover.

Three refusals are asserted along the way, because each is the difference
between this scenario and losing data nobody offered. While A cannot be
reached and nothing has been declared, the pass refuses. Once the partition
heals and both machines call themselves primaries, a pass without the flag
refuses again. Only 'SreBox.PostgresPair.pair_may_discard', which names the
side whose writes may go, turns either one into an action.
-}
splitBrainIsRewound :: IO ()
splitBrainIsRewound = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            resetRows a
            insertRow a "replicated-before-the-partition"
            waitForRows b ["replicated-before-the-partition"]

            psqlOrDie b "ALTER SYSTEM SET wal_receiver_timeout = '5s';"
            psqlOrDie b "SELECT pg_reload_conf();"
            partitionFrom b [testVmAddr]
            waitForNoStreaming b

            -- written on A with the standby already cut off: these are the
            -- writes the flag is about, and a client had them acknowledged.
            insertRow a "written-on-a-behind-the-partition"

            -- and now A disappears from the controller too, until the
            -- machine itself lifts the rule again.
            partitionFromEverythingFor a [controllerAddr, testVmAddr2] 120
            let declared = pairWith a b Pair.B
            waitFor "A stayed reachable through the partition" $ do
                obs <- Pair.observe declared Pair.A
                pure (unreachable obs, show obs)

            -- nothing declared: a machine that cannot be reached is not a
            -- machine that has stopped, and salmon will not guess.
            refused <- runUp (Pair.pairRole silent declared)
            assertBool "promoted without being told whose writes may go" (not refused)
            assertInRecovery b

            -- the operator accepts losing whatever A has that B does not
            let failover = declared{Pair.pair_may_discard = Just Pair.A}
            passOrExplain "the failover" failover
            assertPrimaryIs b
            insertRow b "written-on-b-after-the-failover"

            -- the partition lifts itself, and now both machines are primaries
            waitForUpTo 120 "A never came back" $ do
                obs <- Pair.observe failover Pair.A
                pure (not (unreachable obs), show obs)
            healPartition b
            twoPrimaries <- Pair.decide declared
            assertBool
                ("two primaries, nothing declared, and the step was " <> show twoPrimaries)
                (refuses twoPrimaries)

            -- with the flag, A is stopped and rewound onto B's history
            passOrExplain "the split-brain resolution" failover
            assertStandbyOf a testVmAddr2
            assertPrimaryIs b

            waitForRows a ["replicated-before-the-partition", "written-on-b-after-the-failover"]
            -- and the writes A took behind the partition are gone, which is
            -- exactly what the flag said would happen to them
            assertNoRow a "written-on-a-behind-the-partition"
            assertNoRow b "written-on-a-behind-the-partition"

unreachable :: Pair.Observed -> Bool
unreachable (Pair.Unreachable _) = True
unreachable _ = False

refuses :: Pair.Step -> Bool
refuses (Pair.Refuse _) = True
refuses _ = False

{- | S4: cut the two machines off from each other, change nothing, and let it
heal.

The declaration still says what it said, and both machines are still doing
what they were told, so there is nothing here for salmon to do -- which is
the whole assertion. A partition is the state where acting is most tempting
and least safe: the standby has stopped streaming and looks, to a check that
asks the wrong question, exactly like a standby that was never pointed here
at all.

What makes the difference is asking a standby /where it is told to stream
from/ rather than only where it /is/ streaming from. The first is a
declaration it keeps through a partition; the second is empty the moment the
connection drops.
-}
holdsThroughAPartition :: IO ()
holdsThroughAPartition = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            resetRows a
            insertRow a "before-the-partition"
            waitForRows b ["before-the-partition"]

            -- how long a standby takes to notice that its primary has gone
            -- quiet is a setting, and the default minute is longer than this
            -- test's patience.
            -- two calls, not one: psql sends a multi-statement line as one
            -- implicit transaction, and ALTER SYSTEM refuses to run in one.
            psqlOrDie b "ALTER SYSTEM SET wal_receiver_timeout = '5s';"
            psqlOrDie b "SELECT pg_reload_conf();"
            partitionFrom b [testVmAddr]
            waitForNoStreaming b

            -- the declaration has not changed: A is still the primary
            let p = pairWith a b Pair.A
            -- what the pair's own check says while the partition is up:
            -- Unknown, the one verdict that starts nothing and keeps looking.
            step <- Pair.decide p
            assertEqual
                ("a partition is not a reason to touch anything, but the step was " <> show step)
                Unknown
                (Pair.verdict step)
            ok <- runUp (Pair.pairRole silent p)
            assertBool "a pass over a partitioned pair failed" ok
            -- in particular, the standby was not stopped, rewound or re-seeded
            assertPrimaryIs a
            assertInRecovery b

            insertRow a "written-during-the-partition"
            healPartition b
            waitForRows b ["before-the-partition", "written-during-the-partition"]
            settled <- Pair.decide p
            assertEqual "the pair did not settle once the partition healed" Pair.Done settled



{- | S3: crash the primary, fail over to the standby, and let the machine
that crashed rejoin.

Two things separate this from the switchover above, and both are the point.
The old primary is not stopped, it /dies/ -- so its last checkpoint is no
longer the end of its WAL, and nothing on either machine can say what it
wrote after it. That is a state salmon is not entitled to decide about, so
the first pass here refuses and the second one is given
'SreBox.PostgresPair.pair_may_discard': the operator saying which side's
writes they accept losing, which is the only thing that makes a failover
different from a guess.

And the machine that comes back is rewound rather than re-seeded. The two
are hard to tell apart afterwards -- same rows, same system identifier, same
timeline -- so the test holds on to something only a re-clone destroys.
-}
failsOverFromACrash :: IO ()
failsOverFromACrash = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            resetRows a
            insertRow a "replicated-before-the-crash"
            waitForRows b ["replicated-before-the-crash"]

            -- with the standby down, what A writes now reaches nobody: these
            -- are the writes the flag below is about.
            stopCluster b
            insertRow a "written-while-b-was-down"
            identity <- dataDirectoryIdentity a
            crashCluster a

            -- nothing declared: a pass may start what is stopped, and may
            -- not promote over a machine whose WAL it cannot account for.
            let declared = pairWith a b Pair.B
            refused <- runUp (Pair.pairRole silent declared)
            assertBool "promoted over a crashed primary with nothing declared" (not refused)
            assertInRecovery b

            -- the operator accepts losing A's un-replicated writes
            let failover = declared{Pair.pair_may_discard = Just Pair.A}
            passOrExplain "the failover" failover
            assertPrimaryIs b
            insertRow b "written-on-b-after-the-failover"
            assertStandbyOf a testVmAddr2

            identity' <- dataDirectoryIdentity a
            assertEqual "the old primary was re-cloned rather than rewound" identity identity'

            -- what was replicated survived, on both machines
            waitForRows a ["replicated-before-the-crash", "written-on-b-after-the-failover"]
            waitForRows b ["replicated-before-the-crash", "written-on-b-after-the-failover"]
            -- and what was not is gone, which is what the flag said
            assertNoRow b "written-while-b-was-down"
            assertNoRow a "written-while-b-was-down"

{- | S2: kill the controller after each step of a switchover in turn, and
let an ordinary pass pick it up.

This is the scenario the design is /for/. 'SreBox.PostgresPair.nextStep'
reads the machines rather than a note about where a previous pass got to,
and the claim that buys -- an interrupted switchover needs no repair, only
another pass -- is a claim about states nobody writes down and so nobody
tests by accident.

The interruption is a budget: a controller allowed @k@ steps does @k@ and
throws, which is what a controller being killed looks like from the
machines' side. The direction alternates, so each @k@ lands part-way through
a switchover going the other way than the last one did.
-}
resumesAfterInterruption :: IO ()
resumesAfterInterruption = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            insertRow a "before-any-interruption"

            forM_ (zip [1 :: Int, 2, 3] (cycle [Pair.B, Pair.A])) $ \(k, side) -> do
                let p = pairWith a b side
                -- a controller that dies part-way
                outcome <- try (Pair.convergeUpTo silent k p) :: IO (Either SomeException ())
                -- ... which, for a budget short of the three steps a
                -- switchover takes, must really have stopped part-way:
                -- otherwise the pass below is being credited with finishing
                -- something that was never started.
                unless (k >= 3) $ do
                    assertBool ("a budget of " <> show k <> " steps finished a whole switchover") (isLeft outcome)
                    midA <- Pair.observe p Pair.A
                    midB <- Pair.observe p Pair.B
                    assertBool
                        ("interrupted at step " <> show k <> ", yet already settled: " <> show midA <> " / " <> show midB)
                        (Pair.nextStep p midA midB [] /= Pair.Done)
                -- and an ordinary pass afterwards, with nothing else done
                ok <- runUp (Pair.pairRole silent p)
                assertBool ("a pass after an interruption at step " <> show k <> " failed") ok
                obsA <- Pair.observe p Pair.A
                obsB <- Pair.observe p Pair.B
                assertEqual
                    ("interrupted at step " <> show k <> ", not finished: " <> show obsA <> " / " <> show obsB)
                    Pair.Done
                    (Pair.nextStep p obsA obsB [])
                insertRow (vmFor a b side) ("after-interruption-at-" <> show k)

            -- nothing written along the way was lost by any of it
            let rows = "before-any-interruption" : ["after-interruption-at-" <> show k | k <- [1 :: Int, 2, 3]]
            waitForRows a rows
            waitForRows b rows

{- | Runs the node, and on failure says what the machines looked like.

The node reports through a 'Salmon.Reporter.Reporter', which these tests
leave 'silent' -- so a pass that failed is otherwise just @False@, and the
one thing worth knowing, which of the steps refused or threw, is exactly
what was thrown away. Deciding again costs two ssh round trips and turns
that into a sentence.
-}
passOrExplain :: String -> Pair.Pair -> IO ()
passOrExplain what p = do
    ok <- runUp (Pair.pairRole silent p)
    unless ok $ do
        obsA <- Pair.observe p Pair.A
        obsB <- Pair.observe p Pair.B
        retried <- try (Pair.converge silent p) :: IO (Either SomeException ())
        fail . unlines $
            [ what <> " failed"
            , "  A: " <> show obsA
            , "  B: " <> show obsB
            , "  next step: " <> show (Pair.nextStep p obsA obsB [])
            , "  running it again said: " <> show retried
            ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

vmFor :: VmAccess -> VmAccess -> Pair.Side -> VmAccess
vmFor a _ Pair.A = a
vmFor _ b Pair.B = b

replPassword, rewindPassword :: String
replPassword = "fixture-replication-password"
rewindPassword = "fixture-rewind-password"

replPgpass, rewindPgpass :: FilePath
replPgpass = "/etc/postgresql/salmon-replication.pgpass"
rewindPgpass = "/etc/postgresql/salmon-rewind.pgpass"

switchesOverAndBack :: IO ()
switchesOverAndBack = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            insertRow a "before-any-switchover"

            -- A -> B
            switchTo a b Pair.B
            assertPrimaryIs b
            assertStandbyOf a testVmAddr2
            insertRow b "written-on-b"

            -- doing it again when it is already true must do nothing at all
            assertSettled a b Pair.B
            ok <- runUp (Pair.pairRole silent (pairWith a b Pair.B))
            assertBool "a second pass over a settled pair failed" ok
            assertPrimaryIs b

            -- B -> A
            switchTo a b Pair.A
            assertPrimaryIs a
            assertStandbyOf b testVmAddr
            insertRow a "written-on-a-again"

            -- every row, on both machines
            waitForRows b ["before-any-switchover", "written-on-b", "written-on-a-again"]
            waitForRows a ["before-any-switchover", "written-on-b", "written-on-a-again"]
  where
    switchTo :: VmAccess -> VmAccess -> Pair.Side -> IO ()
    switchTo a b side = do
        ok <- runUp (Pair.pairRole silent (pairWith a b side))
        unless ok (fail ("the switchover to " <> show side <> " failed"))

    assertSettled :: VmAccess -> VmAccess -> Pair.Side -> IO ()
    assertSettled a b side = do
        let p = pairWith a b side
        obsA <- Pair.observe p Pair.A
        obsB <- Pair.observe p Pair.B
        assertEqual
            ("the pair is not settled: " <> show obsA <> " / " <> show obsB)
            Pair.Done
            (Pair.nextStep p obsA obsB [])

pairWith :: VmAccess -> VmAccess -> Pair.Side -> Pair.Pair
pairWith a b side =
    Pair.Pair
        { Pair.pair_name = "test-pair"
        , Pair.pair_a = member testVmAddr (vmIdentityFile a)
        , Pair.pair_b = member testVmAddr2 (vmIdentityFile b)
        , Pair.pair_primary = side
        , Pair.pair_repl_role = "replicator"
        , Pair.pair_repl_passfile = replPgpass
        , Pair.pair_rewind_role = "rewinder"
        , Pair.pair_rewind_passfile = rewindPgpass
        , -- the guests are rebuilt at fixed addresses, so remembering host
          -- keys across runs would only ever be wrong
          Pair.pair_ssh_known_hosts = Just "/dev/null"
        , Pair.pair_catch_up_seconds = 60
        , -- the scenarios below route no clients; S1's client assertions are
          -- the one case that declares a bouncer.
          Pair.pair_bouncers = []
        , Pair.pair_seed = Nothing
        , Pair.pair_may_discard = Nothing
        , Pair.pair_reseed = Nothing
        }
  where
    member host identity = Pair.Member "root" host "main" 5432 (Just identity)

-------------------------------------------------------------------------------

{- | A pair as it stands before any switchover: A primary, B its standby,
and both machines ready to swap those roles.
-}
buildPair :: VmAccess -> VmAccess -> FilePath -> IO ()
buildPair a b fixtureBin = do
    -- whatever the last spec left: A may well be a standby of B, and the
    -- fixture's primary half cannot run on a read-only server.
    ensurePrimary a
    resetCluster b
    mapM_ (\(vm, path) -> scpToVm vm fixtureBin path) [(a, "/root/fixture"), (b, "/root/fixture")]
    mapM_ (\vm -> sshOrDie vm ["chmod", "+x", "/root/fixture"]) [a, b]
    runFixture a ["primary", Text.unpack testVmAddr2 <> "/32"]
    runFixture b ["standby", Text.unpack testVmAddr]
    waitForStandby b
    prepareForSwitchover a b

{- | What a switchover needs and plain streaming replication does not: a
rewind role, its password file and the replication one on both machines, and
@pg_hba.conf@ lines letting each machine be the other's primary.

The role and its grants are created on the primary only, since roles and
grants are catalog rows and reach the standby through the WAL like any other.
-}
prepareForSwitchover :: VmAccess -> VmAccess -> IO ()
prepareForSwitchover a b = do
    psqlOrDie a . unwords $
        [ "DO $$ BEGIN CREATE ROLE rewinder LOGIN PASSWORD '" <> rewindPassword <> "';"
        , "EXCEPTION WHEN duplicate_object THEN NULL; END $$;"
        , "GRANT EXECUTE ON FUNCTION pg_ls_dir(text, boolean, boolean) TO rewinder;"
        , "GRANT EXECUTE ON FUNCTION pg_stat_file(text, boolean) TO rewinder;"
        , "GRANT EXECUTE ON FUNCTION pg_read_binary_file(text) TO rewinder;"
        , "GRANT EXECUTE ON FUNCTION pg_read_binary_file(text, bigint, bigint, boolean) TO rewinder;"
        ]
    mapM_ prepareMachine [(a, testVmAddr2), (b, testVmAddr)]
  where
    prepareMachine (vm, peer) = do
        sshOrDie vm
            [ "bash"
            , "-c"
            , quoteForRemoteShell . unwords $
                [ "set -e;"
                , "version=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1);"
                , "hba=/etc/postgresql/$version/main/pg_hba.conf;"
                , line "host replication replicator " peer <> ";"
                , line "host all rewinder " peer <> ";"
                , writePgpass replPgpass "replicator" replPassword <> ";"
                , writePgpass rewindPgpass "rewinder" rewindPassword <> ";"
                , "pg_ctlcluster \"$version\" main reload"
                ]
            ]

    line prefix peer =
        let l = prefix <> Text.unpack peer <> "/32 md5"
         in "grep -qxF '" <> l <> "' \"$hba\" || echo '" <> l <> "' >> \"$hba\""

    -- .pgpass format, which is what primary_conninfo's passfile= and
    -- pg_rewind's PGPASSFILE both read: host:port:database:user:password.
    writePgpass path role pwd =
        unwords
            [ "printf '*:*:*:" <> role <> ":" <> pwd <> "\\n' > " <> path <> ";"
            , "chown postgres:postgres " <> path <> ";"
            , "chmod 0600 " <> path
            ]

-------------------------------------------------------------------------------

insertRow :: VmAccess -> String -> IO ()
insertRow vm v =
    psqlOrDie vm ("CREATE TABLE IF NOT EXISTS salmon_switchover (v text); INSERT INTO salmon_switchover VALUES ('" <> v <> "');")

{- | Starts the canaries over. The rootfses outlive the VMs, so a row from a
previous run is otherwise still there -- which matters to the one assertion
here that a row is /absent/.
-}
resetRows :: VmAccess -> IO ()
resetRows vm = psqlOrDie vm "DROP TABLE IF EXISTS salmon_switchover;"

waitForRows :: VmAccess -> [String] -> IO ()
waitForRows vm vs =
    waitFor ("expected " <> show vs) $ do
        (_, out, _) <- psql vm "SELECT v FROM salmon_switchover ORDER BY v;"
        pure (all (`isInfixOf` out) vs, out)

assertNoRow :: VmAccess -> String -> IO ()
assertNoRow vm v = do
    (_, out, _) <- psql vm "SELECT v FROM salmon_switchover ORDER BY v;"
    assertBool ("expected " <> v <> " to be gone, got: " <> out) (not (v `isInfixOf` out))

waitForNoStreaming :: VmAccess -> IO ()
waitForNoStreaming vm =
    waitFor "the standby never noticed the partition" $ do
        (_, out, _) <- psql vm "SELECT count(*) FROM pg_stat_wal_receiver;"
        pure ("0" `isInfixOf` out, out)

waitForStandby :: VmAccess -> IO ()
waitForStandby vm =
    waitFor "the standby never started streaming" $ do
        (code, out, _) <- psql vm "SELECT status FROM pg_stat_wal_receiver;"
        pure (code == ExitSuccess && "streaming" `isInfixOf` out, out)
