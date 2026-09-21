{- | Layer 3: exercises "Salmon.Builtin.Nodes.Postgres"'s WAL streaming
replication primitives (@primaryReplicationSetup@\/@standbyReplicationSetup@)
for real, against two real qemu VMs — a primary and a standby, both booted
via 'Test.Harness.withVmAt' on the shared test bridge — asserting
replication actually catches up (a row written on the primary shows up on
the standby), not just that @pg_stat_replication@ says @streaming@.

It then exercises the clone's guard, which is the part of this recipe that
deletes things. Two claims, both about a second @up@ with an unchanged
directive: a __promoted__ standby is left alone (it used to be deleted, the
guard being @standby.signal@, which promotion removes), and a __stranger's__
cluster in the standby's place is refused rather than cloned over. See
@specs\/pg-switchover.md@ (P1).

A guest's rootfs is a host directory that survives the VM, so the standby's
cluster is dropped and recreated at the start of the test rather than
assumed: the previous run deliberately left a stranger's cluster there.

This is the Layer 3 port of @salmon-ops/fixtures/PostgresReplicationFixture.hs@,
which until now was the only way to exercise this recipe at all: a hand-run
fixture against two podman containers, driven by a human reading
@psql@ output off their terminal (see that module's own haddock). Podman is
a poor fit for this recipe specifically — WAL streaming replication wants
two independently-addressable, systemd-managed hosts talking over the
network, which containers-on-one-kernel don't give you for free the way two
qemu guests do. This test drives the exact same fixture binary, just
copied onto real VMs over SSH instead of @podman cp@/@podman exec@, with
real assertions instead of a human eyeballing @psql@.

Needs the same qemu/bridge privilege requirement as "Test.QemuSmokeSpec"
(root, or the one-time capability setup in 'Test.Harness.hasVmPrivileges')
and two pre-built rootfses, one per role, each with
'Salmon.Builtin.Nodes.Debian.Debootstrap.vmEssentials' @<>@
@[Package \"postgresql\", Package \"sudo\"]@ and
'Salmon.Builtin.Nodes.Debian.Debootstrap.ensureVm9pBoot' already applied
(postgres is baked into the rootfs rather than apt-installed at test time,
same reasoning as 'Test.Harness.withVm''s own SSH-CA design: the test
bridge has no NAT\/internet route out of the guest, so nothing here can
depend on the guest reaching the network past boot). Both preconditions
skip loudly, not fail, if unmet — see 'Test.PostgresVms.requirePgVmPrereqs'.

> sudo debootstrap --include=linux-image-amd64,openssh-server,postgresql,sudo stable /var/lib/salmon-test-vms/pg-primary/root
> sudo debootstrap --include=linux-image-amd64,openssh-server,postgresql,sudo stable /var/lib/salmon-test-vms/pg-standby/root
-}
module Test.PostgresReplicationSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import Test.Harness
import Test.PostgresVms
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Postgres replication (Layer 3, real primary/standby VMs)"
        [testCase "replicates a row, keeps a promoted standby, refuses a stranger's cluster" replicatesARow]

replicatesARow :: IO ()
replicatesARow = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \primary ->
        withVmAt testVmAddr2 standbyRootfs $ \standby -> do
            -- whatever the last spec left behind: the switchover spec ends
            -- with the primary on the other machine, and this one's fixture
            -- cannot make a read-only server into a primary. Before copying
            -- anything onto these guests, not after: a cluster that cannot
            -- start crash-loops, and a crash loop on a 512MB guest starves
            -- the sshd the copy needs -- so the failure arrives as a dead
            -- scp, a long way from its cause.
            ensurePrimary primary
            resetCluster standby
            mapM_ (`installFixture` fixtureBin) [primary, standby]

            runFixture primary ["primary", Text.unpack testVmAddr2 <> "/32"]
            runFixture standby ["standby", Text.unpack testVmAddr]

            waitForStreaming primary `catch` dumpDiagAnd primary standby
            insertRow primary
            waitForRowOnStandby standby

            -- The clone guard, which is what keeps `rm -rf` off a data
            -- directory that is not a fresh one. Both halves run against the
            -- standby that was just built, in order: a promoted standby is
            -- left alone, and a stranger's cluster in its place is refused.
            promotedStandbyIsLeftAlone standby
            strangersClusterIsRefused standby
  where
    -- Promotion deletes `standby.signal`, which used to be the whole guard:
    -- the next run read the new primary as "never cloned" and deleted it.
    promotedStandbyIsLeftAlone :: VmAccess -> IO ()
    promotedStandbyIsLeftAlone standby = do
        psqlOrDie standby "SELECT pg_promote(true, 60);"
        psqlOrDie standby "INSERT INTO salmon_repl_test VALUES ('written-after-promotion');"
        runFixture standby ["standby", Text.unpack testVmAddr]
        (_, rows, _) <- psql standby "SELECT v FROM salmon_repl_test ORDER BY v;"
        assertBool
            ("a rerun after promotion lost the promoted standby's data: " <> rows)
            ("written-after-promotion" `isInfixOf` rows && "replicated-ok" `isInfixOf` rows)
        (_, recovery, _) <- psql standby "SELECT pg_is_in_recovery();"
        assertBool ("the promoted standby was put back into recovery: " <> recovery) ("f" `isInfixOf` recovery)

    -- A different cluster at the same path is somebody else's data, whatever
    -- the directive says. The clone must refuse rather than delete it.
    strangersClusterIsRefused :: VmAccess -> IO ()
    strangersClusterIsRefused standby = do
        (code, out, err) <-
            sshToVm
                standby
                [ "bash"
                , "-c"
                , quoteForRemoteShell . unwords $
                    [ -- ssh forwards the host's LANG, and pg_createcluster
                      -- refuses a locale the guest does not have.
                      "export LANG=C LC_ALL=C;"
                    , "set -e;"
                    , "version=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1);"
                    , "pg_dropcluster \"$version\" main --stop;"
                    , "pg_createcluster \"$version\" main -p 5432 -- --auth-local=peer --auth-host=md5;"
                    , "pg_ctlcluster \"$version\" main start;"
                    , "sudo -u postgres psql -c 'CREATE DATABASE precious';"
                    ]
                ]
        unless (code == ExitSuccess) (fail ("could not put a stranger's cluster in place: " <> out <> err))
        (rerun, rerunOut, rerunErr) <- sshToVm standby ["/root/fixture", "standby", Text.unpack testVmAddr]
        assertBool ("the clone did not refuse a stranger's cluster: " <> rerunOut <> rerunErr) (rerun /= ExitSuccess)
        assertBool
            ("refused, but not for the documented reason: " <> rerunOut <> rerunErr)
            ("refusing to clone over" `isInfixOf` (rerunOut <> rerunErr))
        (_, dbs, _) <- psql standby "SELECT datname FROM pg_database WHERE datname = 'precious';"
        assertBool ("the refused cluster was deleted anyway: " <> dbs) ("precious" `isInfixOf` dbs)

    dumpDiagAnd :: VmAccess -> VmAccess -> SomeException -> IO ()
    dumpDiagAnd primary standby e = do
        (_, primRepl, _) <- sshToVm primary ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell "SELECT * FROM pg_stat_replication;"]
        (_, primLog, _) <- sshToVm primary ["bash", "-c", quoteForRemoteShell "tail -n 80 /var/log/postgresql/*.log 2>&1"]
        (_, standRecv, _) <- sshToVm standby ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell "SELECT * FROM pg_stat_wal_receiver;"]
        (_, standLog, _) <- sshToVm standby ["bash", "-c", quoteForRemoteShell "tail -n 80 /var/log/postgresql/*.log 2>&1"]
        (_, standSignal, _) <-
            sshToVm
                standby
                ["bash", "-c", quoteForRemoteShell "ls -la /var/lib/postgresql/17/main/ | grep -i standby; cat /var/lib/postgresql/17/main/postgresql.auto.conf 2>&1"]
        (_, pingOut, _) <- sshToVm standby ["bash", "-c", quoteForRemoteShell ("ping -c2 " <> Text.unpack testVmAddr <> " 2>&1")]
        fail $
            "waitForStreaming diagnostics after: "
                <> show e
                <> "\n--- primary pg_stat_replication ---\n"
                <> primRepl
                <> "\n--- primary log tail ---\n"
                <> primLog
                <> "\n--- standby pg_stat_wal_receiver ---\n"
                <> standRecv
                <> "\n--- standby log tail ---\n"
                <> standLog
                <> "\n--- standby signal/auto.conf ---\n"
                <> standSignal
                <> "\n--- standby ping primary ---\n"
                <> pingOut

-- | Polls @pg_stat_replication@ on the primary until the standby shows up
-- streaming, or fails after a timeout -- same "skip/fail loudly, don't
-- hang" spirit as 'Test.Harness.waitForSsh'.
waitForStreaming :: VmAccess -> IO ()
waitForStreaming access = go (30 :: Int)
  where
    go 0 = fail "waitForStreaming: standby never reached 'streaming' state within the timeout"
    go n = do
        (code, out, _err) <- sshToVm access ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell "SELECT state FROM pg_stat_replication;"]
        if code == ExitSuccess && "streaming" `isInfixOf` out
            then pure ()
            else threadDelay 2000000 >> go (n - 1)

insertRow :: VmAccess -> IO ()
insertRow primary = do
    (code, out, err) <-
        sshToVm
            primary
            [ "sudo"
            , "-u"
            , "postgres"
            , "psql"
            , "-c"
            , quoteForRemoteShell "CREATE TABLE IF NOT EXISTS salmon_repl_test (v text); INSERT INTO salmon_repl_test VALUES ('replicated-ok');"
            ]
    unless (code == ExitSuccess) (fail ("insertRow: failed on primary: " <> out <> err))

-- | Polls the standby (read-only, since it's a streaming replica) for the
-- row 'insertRow' wrote on the primary, or fails after a timeout.
waitForRowOnStandby :: VmAccess -> IO ()
waitForRowOnStandby standby = go (30 :: Int)
  where
    go 0 = fail "waitForRowOnStandby: replicated row never showed up on the standby within the timeout"
    go n = do
        (code, out, _err) <-
            sshToVm
                standby
                ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell "SELECT v FROM salmon_repl_test WHERE v = 'replicated-ok';"]
        if code == ExitSuccess && "replicated-ok" `isInfixOf` out
            then pure ()
            else threadDelay 2000000 >> go (n - 1)
