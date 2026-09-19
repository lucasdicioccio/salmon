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
skip loudly, not fail, if unmet — see 'requirePrereqs'.

> sudo debootstrap --include=linux-image-amd64,openssh-server,postgresql,sudo stable /var/lib/salmon-test-vms/pg-primary/root
> sudo debootstrap --include=linux-image-amd64,openssh-server,postgresql,sudo stable /var/lib/salmon-test-vms/pg-standby/root
-}
module Test.PostgresReplicationSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Postgres replication (Layer 3, real primary/standby VMs)"
        [testCase "replicates a row, keeps a promoted standby, refuses a stranger's cluster" replicatesARow]

primaryRootfs, standbyRootfs :: FilePath
primaryRootfs = "/var/lib/salmon-test-vms/pg-primary/root"
standbyRootfs = "/var/lib/salmon-test-vms/pg-standby/root"

replicatesARow :: IO ()
replicatesARow = requirePrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \primary ->
        withVmAt testVmAddr2 standbyRootfs $ \standby -> do
            scpToVm primary fixtureBin "/root/fixture"
            scpToVm standby fixtureBin "/root/fixture"
            -- scp doesn't reliably carry the exec bit over without -p; set it explicitly.
            _ <- sshToVm primary ["chmod", "+x", "/root/fixture"]
            _ <- sshToVm standby ["chmod", "+x", "/root/fixture"]

            resetStandbyCluster standby

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
    -- A VM's rootfs is a directory on the host, kept between runs, so
    -- whatever the last run left behind is this run's starting state -- and
    -- the last thing this test does is put a stranger's cluster on the
    -- standby. Start from a cluster that was just created and never used,
    -- which is also the state the clone's "pristine" branch is written for.
    resetStandbyCluster :: VmAccess -> IO ()
    resetStandbyCluster standby = do
        (code, out, err) <-
            sshToVm
                standby
                [ "bash"
                , "-c"
                , quoteForRemoteShell . unwords $
                    [ "export LANG=C LC_ALL=C;"
                    , "set -e;"
                    , -- not pg_lsclusters: there may be no cluster to list.
                      "version=$(ls /usr/lib/postgresql | sort -n | tail -n1);"
                    , "if pg_lsclusters --no-header | awk '{print $2}' | grep -qx main;"
                    , "then pg_dropcluster \"$version\" main --stop; fi;"
                    , "pg_createcluster \"$version\" main -p 5432 -- --auth-local=peer --auth-host=md5;"
                    , "pg_ctlcluster \"$version\" main start"
                    ]
                ]
        unless (code == ExitSuccess) (fail ("could not reset the standby's cluster: " <> out <> err))

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

    psql :: VmAccess -> String -> IO (ExitCode, String, String)
    psql access sql = sshToVm access ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell sql]

    psqlOrDie :: VmAccess -> String -> IO ()
    psqlOrDie access sql = do
        (code, out, err) <- psql access sql
        unless (code == ExitSuccess) (fail ("psql failed: " <> sql <> "\n" <> out <> err))

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

    runFixture :: VmAccess -> [String] -> IO ()
    runFixture access args = do
        (code, out, err) <- sshToVm access (["/root/fixture"] <> args)
        if code == ExitSuccess
            then pure ()
            else do
                (_, lsOut, _) <- sshToVm access ["pg_lsclusters"]
                (_, logOut, _) <-
                    sshToVm
                        access
                        [ "bash"
                        , "-c"
                        , quoteForRemoteShell "cat /var/log/postgresql/*.log 2>&1; echo ---journal---; journalctl --no-pager -n 100 2>&1 | grep -i postgres; echo ---run---; ls -la /var/run/postgresql 2>&1"
                        ]
                assertBool
                    ( "fixture "
                        <> unwords args
                        <> " failed: "
                        <> show code
                        <> "\n"
                        <> out
                        <> err
                        <> "\n--- pg_lsclusters ---\n"
                        <> lsOut
                        <> "\n--- logs ---\n"
                        <> logOut
                    )
                    False

requirePrereqs :: IO () -> IO ()
requirePrereqs act = do
    privileged <- hasVmPrivileges
    hasQemu <- (/= Nothing) <$> findExecutable "qemu-system-x86_64"
    hasPrimary <- doesFileExist (primaryRootfs <> "/etc/issue")
    hasStandby <- doesFileExist (standbyRootfs <> "/etc/issue")
    case () of
        _
            | not privileged -> skip "needs root, or ip/qemu-system-x86_64 setcap'd (see Test.Harness.hasVmPrivileges)"
            | not hasQemu -> skip "qemu-system-x86_64 not found on PATH"
            | not hasPrimary -> skip ("no primary VM rootfs at " <> primaryRootfs <> " (see this module's haddock)")
            | not hasStandby -> skip ("no standby VM rootfs at " <> standbyRootfs <> " (see this module's haddock)")
            | otherwise -> act
  where
    skip msg = hPutStrLn stderr ("SKIPPED: " <> msg)

-- | The fixture binary isn't on PATH; resolve its build location via cabal
-- itself rather than hardcoding a dist-newstyle path that'd break on a
-- different GHC/cabal version.
resolveFixtureBinary :: IO FilePath
resolveFixtureBinary = do
    (code, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "salmon-postgres-replication-fixture"] ""
    case code of
        -- `cabal` can print extra notices before the path on stdout (e.g. as
        -- root under sudo, with no prior cabal config: "Config file path
        -- source is default config file."); the bin path is always the last
        -- non-blank line.
        ExitSuccess -> case filter (not . null) (lines out) of
            [] -> error "resolveFixtureBinary: `cabal list-bin` produced no output"
            ls -> pure (last ls)
        ExitFailure n ->
            error $
                "resolveFixtureBinary: `cabal list-bin salmon-postgres-replication-fixture` failed with exit "
                    <> show n
                    <> " -- build it first: cabal build salmon-postgres-replication-fixture\n"
                    <> err

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
