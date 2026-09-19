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
capability grant from @salmon-qemu-host-setup-fixture@), plus two of its own.

The rootfs must contain __rsync__, which the stock Postgres rootfs does not:
@Self@ uploads over rsync and this test pulls the dump back the same way, and
the test bridge has no route to the internet, so nothing can be installed at
test time. Fix on the host with

> sudo chroot /var/lib/salmon-test-vms/pg-primary/root apt-get install -y rsync

And the binary must be built first (@cabal build salmon-pg-backup@), the same
way the replication fixture must be.
-}
module Test.PgBackupSpec (tests) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (doesFileExist, findExecutable, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified Data.Text as Text
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Test.Harness (VmAccess (..), hasVmPrivileges, quoteForRemoteShell, sshToVm, testVmAddr, withTempDir, withVm)

tests :: TestTree
tests =
    testGroup
        "salmon-pg-backup (Layer 3, a real database in a VM)"
        [ testCase "dumps over ssh, fetches the dump back, and installs a schedule" dumpsAndSchedules
        ]

pgRootfs :: FilePath
pgRootfs = "/var/lib/salmon-test-vms/pg-primary/root"

testDatabase :: String
testDatabase = "backup_test"

-- | Written before the dump, looked for inside it afterwards.
canaryRow :: String
canaryRow = "salmon-backup-canary-42"

-------------------------------------------------------------------------------

dumpsAndSchedules :: IO ()
dumpsAndSchedules = requirePrereqs $ \binary ->
    withVm pgRootfs $ \vm -> withTempDir $ \tmp -> do
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
    vmAddr = Text.unpack testVmAddr

    -- a setup step that fails silently would make the real assertion fail
    -- much later and much less clearly
    run_ vm args = do
        (code, out, err) <- sshToVm vm args
        unless (code == ExitSuccess) $
            assertBool ("setup command failed: " <> show args <> "\n" <> out <> "\n" <> err) False

{- | Runs @salmon-pg-backup config … | salmon-pg-backup run up@, the two-phase
protocol every salmon binary speaks, and fails loudly with both streams.
-}
salmon :: FilePath -> [String] -> IO ()
salmon binary args = do
    (code, directive, err) <- readProcessWithExitCode binary ("config" : args) ""
    unless (code == ExitSuccess) $
        assertBool ("config rejected " <> show args <> ":\n" <> err) False
    (upCode, upOut, upErr) <- readProcessWithExitCode binary ["run", "up"] directive
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
            | not rootfsOk -> skip ("no rootfs at " <> pgRootfs <> " (see Test.PostgresReplicationSpec's header)")
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
