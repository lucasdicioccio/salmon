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

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Reporter (silent)
import qualified SreBox.PostgresPair as Pair

tests :: TestTree
tests =
    testGroup
        "Postgres switchover (Layer 3, a primary moved between two VMs)"
        [testCase "switches the primary over, and back, without losing a row" switchesOverAndBack]

primaryRootfs, standbyRootfs :: FilePath
primaryRootfs = "/var/lib/salmon-test-vms/pg-primary/root"
standbyRootfs = "/var/lib/salmon-test-vms/pg-standby/root"

replPassword, rewindPassword :: String
replPassword = "fixture-replication-password"
rewindPassword = "fixture-rewind-password"

replPgpass, rewindPgpass :: FilePath
replPgpass = "/etc/postgresql/salmon-replication.pgpass"
rewindPgpass = "/etc/postgresql/salmon-rewind.pgpass"

switchesOverAndBack :: IO ()
switchesOverAndBack = requirePrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            -- a pair, as it stands before any switchover: A primary, B its standby
            resetCluster b
            mapM_ (\(vm, path) -> scpToVm vm fixtureBin path) [(a, "/root/fixture"), (b, "/root/fixture")]
            mapM_ (\vm -> sshOrDie vm ["chmod", "+x", "/root/fixture"]) [a, b]
            runFixture a ["primary", Text.unpack testVmAddr2 <> "/32"]
            runFixture b ["standby", Text.unpack testVmAddr]
            waitForStandby b

            prepareForSwitchover a b
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
        , Pair.pair_may_discard = Nothing
        }
  where
    member host identity = Pair.Member "root" host "main" 5432 (Just identity)

-------------------------------------------------------------------------------

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

assertPrimaryIs :: VmAccess -> IO ()
assertPrimaryIs vm = do
    (_, out, _) <- psql vm "SELECT pg_is_in_recovery();"
    assertBool ("expected a primary, got: " <> out) ("f" `isInfixOf` out)

-- | Polls: a rejoined standby takes a moment to connect to its new primary.
assertStandbyOf :: VmAccess -> Text.Text -> IO ()
assertStandbyOf vm host = go (30 :: Int)
  where
    go 0 = do
        (_, out, _) <- psql vm "SELECT pg_is_in_recovery(), coalesce((SELECT sender_host FROM pg_stat_wal_receiver LIMIT 1), 'none');"
        fail ("never became a standby of " <> Text.unpack host <> ": " <> out)
    go n = do
        (_, out, _) <- psql vm "SELECT coalesce((SELECT sender_host FROM pg_stat_wal_receiver LIMIT 1), 'none');"
        if Text.unpack host `isInfixOf` out then pure () else threadDelay 2000000 >> go (n - 1)

insertRow :: VmAccess -> String -> IO ()
insertRow vm v =
    psqlOrDie vm ("CREATE TABLE IF NOT EXISTS salmon_switchover (v text); INSERT INTO salmon_switchover VALUES ('" <> v <> "');")

waitForRows :: VmAccess -> [String] -> IO ()
waitForRows vm vs = go (30 :: Int)
  where
    go 0 = do
        (_, out, _) <- psql vm "SELECT v FROM salmon_switchover ORDER BY v;"
        fail ("expected " <> show vs <> ", got: " <> out)
    go n = do
        (_, out, _) <- psql vm "SELECT v FROM salmon_switchover ORDER BY v;"
        if all (`isInfixOf` out) vs then pure () else threadDelay 2000000 >> go (n - 1)

waitForStandby :: VmAccess -> IO ()
waitForStandby vm = go (30 :: Int)
  where
    go 0 = fail "the standby never started streaming"
    go n = do
        (code, out, _) <- psql vm "SELECT status FROM pg_stat_wal_receiver;"
        if code == ExitSuccess && "streaming" `isInfixOf` out then pure () else threadDelay 2000000 >> go (n - 1)

{- | The standby's rootfs outlives its VM, and the last run left a cluster
of some other lineage in it; start from one just created. Same reasoning as
"Test.PostgresReplicationSpec".
-}
resetCluster :: VmAccess -> IO ()
resetCluster vm =
    sshOrDie vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell . unwords $
            [ "export LANG=C LC_ALL=C;"
            , "set -e;"
            , "version=$(ls /usr/lib/postgresql | sort -n | tail -n1);"
            , "if pg_lsclusters --no-header | awk '{print $2}' | grep -qx main;"
            , "then pg_dropcluster \"$version\" main --stop; fi;"
            , "pg_createcluster \"$version\" main -p 5432 -- --auth-local=peer --auth-host=md5;"
            , "pg_ctlcluster \"$version\" main start"
            ]
        ]

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

runFixture :: VmAccess -> [String] -> IO ()
runFixture vm args = do
    (code, out, err) <- sshToVm vm (["/root/fixture"] <> args)
    unless (code == ExitSuccess) (fail ("fixture " <> unwords args <> " failed:\n" <> out <> err))

requirePrereqs :: IO () -> IO ()
requirePrereqs act = do
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

resolveFixtureBinary :: IO FilePath
resolveFixtureBinary = do
    (code, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "salmon-postgres-replication-fixture"] ""
    case code of
        ExitSuccess -> case filter (not . null) (lines out) of
            [] -> error "resolveFixtureBinary: `cabal list-bin` produced no output"
            ls -> pure (last ls)
        ExitFailure n ->
            error ("resolveFixtureBinary: cabal list-bin failed with exit " <> show n <> "\n" <> err)
