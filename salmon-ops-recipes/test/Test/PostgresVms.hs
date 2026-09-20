{-# LANGUAGE OverloadedStrings #-}

{- | What the Layer 3 Postgres specs share: the two VM rootfses, the
replication fixture binary, and the handful of remote commands every one of
them needs.

The reason this module exists is the state a guest leaves behind. A rootfs
is a directory on the host, so it outlives its VM and a spec starts from
whatever the last one did -- which, once a switchover is in the picture,
includes "the machine that used to be the primary is now a standby". Every
spec here therefore /normalizes/ on the way in rather than assuming, and
'ensurePrimary' and 'resetCluster' are that normalization.
-}
module Test.PostgresVms (
    primaryRootfs,
    standbyRootfs,
    requirePgVmPrereqs,
    resolveFixtureBinary,
    installFixture,
    runFixture,
    psql,
    psqlOrDie,
    sshOrDie,
    ensurePrimary,
    resetCluster,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.Harness
import Test.Tasty.HUnit (assertBool)

primaryRootfs, standbyRootfs :: FilePath
primaryRootfs = "/var/lib/salmon-test-vms/pg-primary/root"
standbyRootfs = "/var/lib/salmon-test-vms/pg-standby/root"

{- | Skips loudly rather than failing when the machine cannot run these:
qemu, the bridge privileges, and the two rootfses. See
"Test.PostgresReplicationSpec" for how to build them.
-}
requirePgVmPrereqs :: IO () -> IO ()
requirePgVmPrereqs act = do
    privileged <- hasVmPrivileges
    hasQemu <- (/= Nothing) <$> findExecutable "qemu-system-x86_64"
    hasA <- doesFileExist (primaryRootfs <> "/etc/issue")
    hasB <- doesFileExist (standbyRootfs <> "/etc/issue")
    case () of
        _
            | not privileged -> skip "needs root, or ip/qemu-system-x86_64 setcap'd (see Test.Harness.hasVmPrivileges)"
            | not hasQemu -> skip "qemu-system-x86_64 not found on PATH"
            | not hasA -> skip ("no VM rootfs at " <> primaryRootfs <> " (see Test.PostgresReplicationSpec)")
            | not hasB -> skip ("no VM rootfs at " <> standbyRootfs <> " (see Test.PostgresReplicationSpec)")
            | otherwise -> act
  where
    skip msg = hPutStrLn stderr ("SKIPPED: " <> msg)

{- | The fixture binary isn't on PATH; resolve its build location via cabal
itself rather than hardcoding a dist-newstyle path that'd break on a
different GHC\/cabal version.
-}
resolveFixtureBinary :: IO FilePath
resolveFixtureBinary = do
    (code, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "salmon-postgres-replication-fixture"] ""
    case code of
        -- `cabal` can print extra notices before the path on stdout (e.g. as
        -- root under sudo, with no prior cabal config); the bin path is
        -- always the last non-blank line.
        ExitSuccess -> case filter (not . null) (lines out) of
            [] -> error "resolveFixtureBinary: `cabal list-bin` produced no output"
            ls -> pure (last ls)
        ExitFailure n ->
            error $
                "resolveFixtureBinary: `cabal list-bin salmon-postgres-replication-fixture` failed with exit "
                    <> show n
                    <> " -- build it first: cabal build salmon-postgres-replication-fixture\n"
                    <> err

-- | Copies the fixture onto a guest and makes it executable.
installFixture :: VmAccess -> FilePath -> IO ()
installFixture vm bin = do
    scpToVm vm bin "/root/fixture"
    -- scp doesn't reliably carry the exec bit over without -p; set it explicitly.
    sshOrDie vm ["chmod", "+x", "/root/fixture"]

-- | Runs the fixture, and on failure says what the machine looked like.
runFixture :: VmAccess -> [String] -> IO ()
runFixture vm args = do
    (code, out, err) <- sshToVm vm (["/root/fixture"] <> args)
    unless (code == ExitSuccess) $ do
        (_, lsOut, _) <- sshToVm vm ["pg_lsclusters"]
        (_, logOut, _) <-
            sshToVm
                vm
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

psql :: VmAccess -> String -> IO (ExitCode, String, String)
psql vm sql = sshToVm vm ["sudo", "-u", "postgres", "psql", "-tAXc", quoteForRemoteShell sql]

psqlOrDie :: VmAccess -> String -> IO ()
psqlOrDie vm sql = do
    (code, out, err) <- psql vm sql
    unless (code == ExitSuccess) (fail ("psql failed: " <> sql <> "\n" <> out <> err))

sshOrDie :: VmAccess -> [String] -> IO ()
sshOrDie vm args = do
    (code, out, err) <- sshToVm vm args
    unless (code == ExitSuccess) (fail ("remote command failed: " <> unwords args <> "\n" <> out <> err))

{- | Makes this machine a primary, whatever it was.

A spec that ends with the primary on the other machine leaves this one a
standby, and everything a later spec does -- creating a role, writing a row
-- then fails with "cannot execute ... in a read-only transaction", which
names the symptom and not the cause. Promoting is enough: a standby that has
been promoted is an ordinary primary, and one that already was one says so
and is left alone.
-}
ensurePrimary :: VmAccess -> IO ()
ensurePrimary vm = do
    (_, out, _) <- psql vm "SELECT pg_is_in_recovery();"
    unless ("f" `isInfixOf` out) $ do
        psqlOrDie vm "SELECT pg_promote(true, 60);"
        waitOut (30 :: Int)
  where
    waitOut 0 = fail "a standby never finished promoting"
    waitOut n = do
        (_, out, _) <- psql vm "SELECT pg_is_in_recovery();"
        if "f" `isInfixOf` out then pure () else threadDelay 2000000 >> waitOut (n - 1)

{- | Drops this machine's cluster and creates an empty one.

For the standby, whose data directory is about to be replaced by a clone
anyway: starting from a cluster that was just created is also the state the
clone's own "pristine" branch is written for.
-}
resetCluster :: VmAccess -> IO ()
resetCluster vm =
    sshOrDie
        vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell . unwords $
            [ -- ssh forwards the host's LANG, and pg_createcluster refuses a
              -- locale the guest does not have.
              "export LANG=C LC_ALL=C;"
            , "set -e;"
            , -- not pg_lsclusters: there may be no cluster to list.
              "version=$(ls /usr/lib/postgresql | sort -n | tail -n1);"
            , "if pg_lsclusters --no-header | awk '{print $2}' | grep -qx main;"
            , "then pg_dropcluster \"$version\" main --stop; fi;"
            , "pg_createcluster \"$version\" main -p 5432 -- --auth-local=peer --auth-host=md5;"
            , "pg_ctlcluster \"$version\" main start"
            ]
        ]
