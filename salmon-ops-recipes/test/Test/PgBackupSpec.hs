{-# LANGUAGE OverloadedStrings #-}

{- | Layer 3 for @salmon-pg-backup@: a real Postgres in a real VM, dumped by
a binary that uploads itself to reach it.

This is the tier that can actually answer the question the binary exists to
answer. Layer 0 can check that the generated script says @pipefail@; only a
machine with a database on it can show that the dump is a dump, that it
arrived, and that it contains the row that was written before it was taken.

What the test drives, end to end:

1. boot a VM from the Postgres rootfs and create a database with one known row;
2. run @salmon-pg-backup --action dump --over root\@\<vm\>@ __from the host__,
   which rsyncs the binary into the guest, runs it there over ssh with a
   directive, and pulls the resulting dump back;
3. gunzip the fetched file and look for the row;
4. run the same binary with @--action schedule@ and check the guest now has
   a @\/etc\/cron.d@ entry.

= Prerequisites

Everything "Test.PostgresReplicationSpec" needs (see its header: a
debootstrapped rootfs, @qemu-system-x86_64@, and either root or the
capability grant from @salmon-qemu-host-setup-fixture@), plus a rootfs of its
own:

> sudo mkdir -p /var/lib/salmon-test-vms/pg-backup/root
> sudo rsync -aHAX --numeric-ids /var/lib/salmon-test-vms/pg-master/root/ /var/lib/salmon-test-vms/pg-backup/root/
> sudo chroot /var/lib/salmon-test-vms/pg-backup/root apt-get install -y rsync
> sudo $(cabal list-bin salmon-qemu-host-setup-fixture) "$USER" /var/lib/salmon-test-vms/pg-backup/root

(the @mkdir@ is not optional: @rsync@ creates the last component of a
destination path and no more, so without it the copy fails on the missing
parent and every later step fails on the missing rootfs.)

and the binary built first (@cabal build salmon-pg-backup@), the same way the
replication fixture must be.

= Why a rootfs of its own

__A rootfs must never back two VMs that are up at the same time.__ It is
exported over 9p in @passthrough@ mode, so the guest writes straight into the
host directory with no locking whatsoever; two kernels mounting it read and
write the same bytes, and the first casualty is the Postgres data directory.
This test was originally pointed at @pg-primary@, which
"Test.PostgresReplicationSpec" also boots, and the pair of them destroyed
that cluster's checkpoint record — @PANIC: could not locate a valid
checkpoint record@, repaired only by re-syncing the rootfs from
@pg-master@.

Serializing the VM specs (see @test/Main.hs@) makes the overlap unlikely
rather than impossible, because a VM that leaks past its own teardown is
still up when the next one starts. Separate rootfses make it harmless.

The @rsync@ requirement is this test's own: 'Self' uploads over rsync and the
fetch pulls back the same way, and the test bridge has no route to the
internet, so nothing can be installed at test time.
-}
module Test.PgBackupSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (doesFileExist, findExecutable, listDirectory)
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified Data.Text as Text
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Test.Harness (VmAccess (..), hasVmPrivileges, quoteForRemoteShell, sshToVm, testVmAddr3, withTempDir, withVmAt)

tests :: TestTree
tests =
    testGroup
        "salmon-pg-backup (Layer 3, a real database in a VM)"
        [ testCase "dumps over ssh, fetches the dump back, and installs a schedule" dumpsAndSchedules
        ]

{- | This test's __own__ rootfs — never one another VM spec boots. See the
module header for why that is not a preference.
-}
pgRootfs :: FilePath
pgRootfs = "/var/lib/salmon-test-vms/pg-backup/root"

testDatabase :: String
testDatabase = "backup_test"

-- | Written before the dump, looked for inside it afterwards.
canaryRow :: String
canaryRow = "salmon-backup-canary-42"

-------------------------------------------------------------------------------

dumpsAndSchedules :: IO ()
dumpsAndSchedules = requirePrereqs $ \binary ->
    -- An address of this spec's own, for the same reason as the rootfs: a
    -- VM still shutting down on a shared address answers for the next one,
    -- and ssh reports a connection closed by a host that is not under test.
    withVmAt testVmAddr3 pgRootfs $ \vm -> withTempDir $ \tmp -> do
        -- `withVm` waits for sshd, which says nothing about Postgres: the
        -- cluster is still replaying when the first psql lands, and answers
        -- "the database system is starting up".
        waitForPostgres vm

        -- The rootfs is exported over 9p in passthrough mode, so everything
        -- the guest writes lands in the host directory and survives the VM.
        -- Without this the second run of this test fails on CREATE DATABASE,
        -- and -- worse -- its cron assertion passes on the entry the *first*
        -- run installed, which is a test that no longer tests anything.
        resetGuest vm

        -- A database whose contents we know, so "the dump contains this" is a
        -- statement about the dump rather than about the fixture.
        run_ vm ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell ("CREATE DATABASE " <> testDatabase)]
        run_
            vm
            [ "sudo"
            , "-u"
            , "postgres"
            , "psql"
            , "-d"
            , testDatabase
            , "-tAc"
            , quoteForRemoteShell ("CREATE TABLE canary (v text); INSERT INTO canary VALUES ('" <> canaryRow <> "')")
            ]

        let identity = vm.vmIdentityFile
            common =
                [ "--database"
                , testDatabase
                , "--over"
                , "root@" <> vmAddr
                , "--ssh-identity"
                , identity
                , "--ssh-known-hosts"
                , tmp </> "known_hosts"
                , "--remote-dir"
                , "/root"
                , "--dir"
                , "/var/backups/postgresql"
                ]

        -- 1. dump, driven from here
        salmon binary (common <> ["--action", "dump", "--fetch-into", tmp </> "dumps"])

        fetched <- listDirectory (tmp </> "dumps")
        assertBool ("nothing was fetched into " <> (tmp </> "dumps")) (not (null fetched))
        let dump = tmp </> "dumps" </> head fetched
        assertBool ("fetched file is not named like a dump: " <> dump) (".sql.gz" `isInfixOf` dump)

        -- gunzip -t would only say it is a valid archive; a failed pg_dump
        -- piped into gzip produces one of those too. The row is the assertion.
        (code, out, err) <- readProcessWithExitCode "zcat" [dump] ""
        assertBool ("zcat failed on " <> dump <> ": " <> err) (code == ExitSuccess)
        assertBool
            ("the dump does not contain the canary row; first 500 bytes:\n" <> take 500 out)
            (canaryRow `isInfixOf` out)

        -- 2. install the periodic job on the same machine
        salmon binary (common <> ["--action", "schedule"])

        (cronCode, cronOut, _) <- sshToVm vm ["cat", "/etc/cron.d/salmon-pg-backup-" <> testDatabase]
        assertBool "the cron entry was not installed in the guest" (cronCode == ExitSuccess)
        assertBool
            ("the cron entry does not run the backup script: " <> cronOut)
            (("backup-" <> testDatabase <> ".sh") `isInfixOf` cronOut)
  where
    vmAddr = Text.unpack testVmAddr3

-- | A setup step that failed silently would make the real assertion fail much
-- later and much less clearly.
run_ :: VmAccess -> [String] -> IO ()
run_ vm args = do
    (code, out, err) <- sshToVm vm args
    unless (code == ExitSuccess) $
        assertBool ("setup command failed: " <> show args <> "\n" <> out <> "\n" <> err) False

{- | Undoes everything a previous run of this test left in the rootfs.

Not a nicety: a persistent rootfs turns "the cron entry is present" from an
assertion about this run into an assertion about the first run that ever
passed.
-}
resetGuest :: VmAccess -> IO ()
resetGuest vm = do
    run_ vm ["rm", "-f", "/etc/cron.d/salmon-pg-backup-" <> testDatabase]
    run_ vm ["rm", "-rf", "/var/backups/postgresql"]
    run_ vm ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell ("DROP DATABASE IF EXISTS " <> testDatabase)]

{- | Polls until the cluster accepts a connection, 30 × 2s.

Separate from the harness's own wait because they are different questions
with different answers: sshd is up within a second or two of boot, and
Postgres takes as long as its last shutdown left it needing.
-}
waitForPostgres :: VmAccess -> IO ()
waitForPostgres vm = go (30 :: Int)
  where
    go 0 = fail "postgres never accepted a connection in the VM"
    go n = do
        (code, _, _) <- sshToVm vm ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell "SELECT 1"]
        if code == ExitSuccess
            then pure ()
            else threadDelay 2000000 >> go (n - 1)

{- | Runs @salmon-pg-backup config … | salmon-pg-backup run up@, the two-phase
protocol every salmon binary speaks, and fails loudly with both streams.
-}
salmon :: FilePath -> [String] -> IO ()
salmon binary args = do
    (code, directive, err) <- runSalmon binary ("config" : args) ""
    unless (code == ExitSuccess) $
        assertBool ("config rejected " <> show args <> ":\n" <> err) False
    (upCode, upOut, upErr) <- runSalmon binary ["run", "up"] directive
    unless (upCode == ExitSuccess) $
        assertBool
            ( "run up failed for "
                <> show args
                <> "\n--- directive ---\n"
                <> directive
                <> "\n--- stdout ---\n"
                <> upOut
                <> "\n--- stderr ---\n"
                <> upErr
            )
            False

{- | Runs the binary with a __clean PATH__ rather than this process's.

Not fussiness. "Test.PostgresInitSpec" shims @apt-get@ and @dpkg-query@ onto
@PATH@ so a recipe's package nodes act on its podman sandbox instead of the
developer's machine, and @PATH@ is process-global: a subprocess spawned from
this test inherits whatever is in effect. The symptom is memorable —
salmon's own @deb@ nodes report

> E: Could not get lock /var/lib/dpkg/lock-frontend. It is held by process 1269

naming a PID that does not exist on the host, because the apt-get really ran
inside somebody else's container.
-}
runSalmon :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
runSalmon binary args input = do
    -- the real HOME: ssh reads it even when told which identity to use
    home <- getEnv "HOME"
    readCreateProcessWithExitCode
        (proc binary args){env = Just [("PATH", "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"), ("HOME", home)]}
        input

-------------------------------------------------------------------------------

{- | Every reason this test cannot run, each announced rather than failing.

Layer 3 is opt-in by having built the machine for it; a developer who has
not should see why, not a red test.
-}
requirePrereqs :: (FilePath -> IO ()) -> IO ()
requirePrereqs act = do
    privileged <- hasVmPrivileges
    qemu <- findExecutable "qemu-system-x86_64"
    rootfsOk <- doesFileExist (pgRootfs </> "etc/issue")
    -- Self uploads over rsync and the fetch pulls over rsync, and the test
    -- bridge has no internet, so it cannot be installed at test time.
    guestRsync <- anyExists [pgRootfs </> "usr/bin/rsync", pgRootfs </> "bin/rsync"]
    hostRsync <- findExecutable "rsync"
    hostSsh <- findExecutable "ssh"
    case () of
        _
            | not privileged -> skip "no VM privileges (run salmon-qemu-host-setup-fixture, or use sudo)"
            | Nothing <- qemu -> skip "qemu-system-x86_64 not on PATH"
            | not rootfsOk ->
                skip
                    ( "no rootfs at "
                        <> pgRootfs
                        <> ". It must be this spec's own (two VMs on one 9p rootfs corrupt it). Build it with:\n"
                        <> "  sudo mkdir -p "
                        <> pgRootfs
                        <> "\n  sudo rsync -aHAX --numeric-ids /var/lib/salmon-test-vms/pg-master/root/ "
                        <> pgRootfs
                        <> "/\n  sudo chroot "
                        <> pgRootfs
                        <> " apt-get install -y rsync\n"
                        <> "  sudo $(cabal list-bin salmon-qemu-host-setup-fixture) \"$USER\" "
                        <> pgRootfs
                    )
            | not guestRsync ->
                skip
                    ( "the guest rootfs has no rsync; salmon uploads itself with it and there is no route out of the test bridge. Fix with:\n"
                        <> "  sudo chroot "
                        <> pgRootfs
                        <> " apt-get install -y rsync"
                    )
            | Nothing <- hostRsync -> skip "rsync not on PATH on the host"
            | Nothing <- hostSsh -> skip "ssh not on PATH on the host"
            | otherwise -> resolveBinary >>= act

anyExists :: [FilePath] -> IO Bool
anyExists paths = or <$> traverse doesFileExist paths

skip :: String -> IO ()
skip why = putStrLn ("SKIPPED: " <> why)

{- | @cabal list-bin@, taking the last non-blank line: under sudo, cabal
prints notices before the path.
-}
resolveBinary :: IO FilePath
resolveBinary = do
    (code, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "salmon-pg-backup"] ""
    case (code, reverse (filter (not . null) (lines out))) of
        (ExitSuccess, path : _) -> pure path
        _ -> error ("could not locate salmon-pg-backup (build it first): " <> err)
