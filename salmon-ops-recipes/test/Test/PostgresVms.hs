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
    stopCluster,
    crashCluster,
    dataDirectoryIdentity,
    assertPrimaryIs,
    assertInRecovery,
    waitFor,
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

It removes both directories by hand rather than trusting @pg_dropcluster@
with the job, because the state this has to survive is a /half/ a cluster. A
run interrupted between "remove the data directory" and "clone into it", or
between the drop's two halves, leaves one of the two directories without the
other -- and then @pg_lsclusters@ lists nothing, so there is nothing to drop,
while @pg_createcluster@ finds a data directory to adopt and fails on the
config files a clone does not have. That is a wreck no amount of asking
politely gets rid of.
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
            , "then pg_dropcluster \"$version\" main --stop || true; fi;"
            , "rm -rf \"/etc/postgresql/$version/main\" \"/var/lib/postgresql/$version/main\";"
            , "pg_createcluster \"$version\" main -p 5432 -- --auth-local=peer --auth-host=md5;"
            , "pg_ctlcluster \"$version\" main start"
            ]
        ]

{- | Stops this machine's cluster the way an operator would, leaving a
shutdown checkpoint behind: what it stopped at is then on disk, and a
promotion elsewhere can prove it lost nothing.
-}
stopCluster :: VmAccess -> IO ()
stopCluster vm = pgCtl vm "stop"

{- | Kills this machine's cluster the way a crash would, leaving nothing
behind: whatever it wrote after its last checkpoint is in the WAL and in no
control file, which is the state a failover cannot reason about.
-}
crashCluster :: VmAccess -> IO ()
crashCluster vm = do
    sshOrDie
        vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell . unwords $
            [ "version=$(pg_lsclusters --no-header | awk '{print $1}' | head -n1);"
            , -- the unit's whole cgroup, so no backend outlives the postmaster
              "systemctl kill -s KILL postgresql@\"$version\"-main 2>/dev/null;"
            , "pkill -9 -u postgres 2>/dev/null;"
            , "true"
            ]
        ]
    waitFor "the cluster never died" $ do
        (code, out, _) <- sshToVm vm ["pg_lsclusters", "--no-header"]
        pure (code /= ExitSuccess || not ("online" `isInfixOf` out), out)

pgCtl :: VmAccess -> String -> IO ()
pgCtl vm action =
    sshOrDie
        vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell . unwords $
            [ "set -e;"
            , "version=$(pg_lsclusters --no-header | awk '{print $1}' | head -n1);"
            , "pg_ctlcluster \"$version\" main " <> action
            ]
        ]

{- | Something about this cluster's data directory that survives a rewind and
cannot survive a re-clone: the inodes of the directory and of the one file in
it that never changes.

This is how a test tells "the old primary was rewound onto the new one" from
"the old primary was thrown away and copied back", which from the outside
look alike -- same rows, same system identifier, same timeline. Only one of
them unlinks anything.
-}
dataDirectoryIdentity :: VmAccess -> IO String
dataDirectoryIdentity vm = do
    (code, out, err) <-
        sshToVm
            vm
            [ "bash"
            , "-c"
            , quoteForRemoteShell . unwords $
                [ "set -e;"
                , "version=$(pg_lsclusters --no-header | awk '{print $1}' | head -n1);"
                , "datadir=/var/lib/postgresql/$version/main;"
                , "stat -c %i \"$datadir\" \"$datadir/PG_VERSION\""
                ]
            ]
    unless (code == ExitSuccess) (fail ("could not read the data directory: " <> out <> err))
    pure (unwords (words out))

assertPrimaryIs :: VmAccess -> IO ()
assertPrimaryIs vm = do
    (_, out, _) <- psql vm "SELECT pg_is_in_recovery();"
    assertBool ("expected a primary, got: " <> out) ("f" `isInfixOf` out)

assertInRecovery :: VmAccess -> IO ()
assertInRecovery vm = do
    (_, out, _) <- psql vm "SELECT pg_is_in_recovery();"
    assertBool ("expected a standby, got: " <> out) ("t" `isInfixOf` out)

{- | Polls every two seconds for a minute, and says what it last saw rather
than only that it gave up. Every one of these waits is on a machine doing
something in its own time -- a standby connecting, a promotion finishing, a
row arriving -- and none of them is instant.
-}
waitFor :: String -> IO (Bool, String) -> IO ()
waitFor what probe = go (30 :: Int)
  where
    go :: Int -> IO ()
    go 0 = do
        (_, seen) <- probe
        fail (what <> "; last seen: " <> seen)
    go n = do
        (ok, _) <- probe
        unless ok (threadDelay 2000000 >> go (n - 1))
