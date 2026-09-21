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
    startCluster,
    stopCluster,
    crashCluster,
    dataDirectoryIdentity,
    walMegabytes,
    assertPrimaryIs,
    assertInRecovery,
    assertStandbyOf,
    waitFor,
    waitForUpTo,
    partitionFrom,
    partitionFromEverythingFor,
    healPartition,
    controllerAddr,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Process (CmdSpec (..), cmdspec, readProcessWithExitCode)

import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Netfilter as Netfilter
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
    -- a spec is allowed to end with a cluster stopped -- S6 does, having
    -- stopped the standby to see what the primary's disk does about it --
    -- and the next one still starts from a machine, not from an error.
    started <- startCluster vm
    unless started $ do
        -- and a guest killed mid-write leaves a data directory that no
        -- longer has a valid checkpoint to start from, which this tier does
        -- routinely: every run that is interrupted stops a VM with postgres
        -- writing. A scratch fixture that cannot start is not data anybody
        -- is keeping, and the alternative is every spec after this one
        -- failing on the same corpse -- including in ways that do not look
        -- like a broken cluster at all, since a cluster crash-looping on a
        -- 512MB guest starves the sshd the next spec needs.
        hPutStrLn stderr "NOTE: the cluster would not start; recreating it (see Test.PostgresVms.ensurePrimary)"
        resetCluster vm
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

{- | Starts the cluster if it is not running, and says whether it is running
now. Unlike the rest of these helpers, it does not die on failure: its
callers have somewhere better to go than an exception.
-}
startCluster :: VmAccess -> IO Bool
startCluster vm = do
    (code, _, _) <-
        sshToVm
            vm
            [ "bash"
            , "-c"
            , quoteForRemoteShell . unwords $
                [ "version=$(pg_lsclusters --no-header | awk '{print $1}' | head -n1);"
                , "pg_ctlcluster \"$version\" main status >/dev/null 2>&1 ||"
                , "pg_ctlcluster \"$version\" main start"
                ]
            ]
    pure (code == ExitSuccess)

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

-- | How much disk the write-ahead log is taking, in megabytes.
walMegabytes :: VmAccess -> IO Int
walMegabytes vm = do
    (code, out, err) <-
        sshToVm
            vm
            [ "bash"
            , "-c"
            , quoteForRemoteShell . unwords $
                [ "set -e;"
                , "version=$(pg_lsclusters --no-header | awk '{print $1}' | head -n1);"
                , "du -sm \"/var/lib/postgresql/$version/main/pg_wal\" | awk '{print $1}'"
                ]
            ]
    unless (code == ExitSuccess) (fail ("could not measure pg_wal: " <> out <> err))
    case reads (takeWhile (/= '\n') out) of
        [(n, _)] -> pure n
        _ -> fail ("could not read a size from: " <> out)

assertPrimaryIs :: VmAccess -> IO ()
assertPrimaryIs vm = do
    (_, out, _) <- psql vm "SELECT pg_is_in_recovery();"
    assertBool ("expected a primary, got: " <> out) ("f" `isInfixOf` out)

{- | Polls: a standby takes a moment to connect to its primary, and every
spec that moves one waits for the same thing.
-}
assertStandbyOf :: VmAccess -> Text -> IO ()
assertStandbyOf vm host =
    waitFor ("never became a standby of " <> Text.unpack host) $ do
        (_, out, _) <- psql vm "SELECT coalesce((SELECT sender_host FROM pg_stat_wal_receiver LIMIT 1), 'none');"
        pure (Text.unpack host `isInfixOf` out, out)

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
waitFor = waitForUpTo 30

-- | 'waitFor' with a different number of two-second tries.
waitForUpTo :: Int -> String -> IO (Bool, String) -> IO ()
waitForUpTo tries what probe = go tries
  where
    go :: Int -> IO ()
    go 0 = do
        (_, seen) <- probe
        fail (what <> "; last seen: " <> seen)
    go n = do
        (ok, _) <- probe
        unless ok (threadDelay 2000000 >> go (n - 1))

{- | The host's address on the test bridge, which is where the controller
runs: a partition that is meant to cut a machine off from the /operator/ has
to drop this one too, and one that is only between the members must not.
-}
controllerAddr :: Text
controllerAddr = "10.99.0.1"

{- | Cuts this machine off from those addresses, which is what a partition
looks like from inside one of them.

The rules are built out of "Salmon.Builtin.Nodes.Netfilter"'s own vocabulary
and rendered by its own @nft@ command, so this says what a salmon-declared
firewall would say. It is only the /running/ of it that differs: these guests
have no salmon on them, so the argv goes over ssh instead of into an 'Op'.

Dropping by source address in @input@ breaks the connection in both
directions, since neither end gets an answer -- including, if
'controllerAddr' is among them, the ssh session that adds the rule. Hence
'healPartition' and, for that case, a caller that detaches.
-}
partitionFrom :: VmAccess -> [Text] -> IO ()
partitionFrom vm addrs = mapM_ (sshOrDie vm . map quoteForRemoteShell) (partitionCommands addrs)

-- | Removes the whole table, whatever it held: a heal is not a negotiation.
{- | Cuts this machine off from everything named, the controller included,
and heals it again after @seconds@ with nobody asking.

A partition that hides a machine from its operator cannot be lifted by that
operator: the command that would lift it has to travel the path it cut. So
the machine is handed the whole sequence -- cut, wait, heal -- and left to
run it detached, which is also what anyone sensible does before touching the
firewall of a box they can only reach over the network.
-}
partitionFromEverythingFor :: VmAccess -> [Text] -> Int -> IO ()
partitionFromEverythingFor vm addrs seconds = do
    sshOrDie vm ["bash", "-c", quoteForRemoteShell heredoc]
    sshOrDie vm ["bash", "-c", quoteForRemoteShell "setsid bash /root/partition.sh >/dev/null 2>&1 </dev/null &"]
  where
    heredoc = "cat > /root/partition.sh <<'SALMON_EOF'\n" <> script <> "SALMON_EOF\n"
    script =
        unlines $
            [unwords (map quoteForRemoteShell argv) | argv <- partitionCommands addrs]
                <> [ "sleep " <> show seconds
                   , unwords ["nft", "delete", "table", "inet", Text.unpack partitionTable.tableName]
                   ]

healPartition :: VmAccess -> IO ()
healPartition vm = do
    _ <- sshToVm vm ["nft", "delete", "table", "inet", Text.unpack partitionTable.tableName]
    pure ()

partitionCommands :: [Text] -> [[String]]
partitionCommands addrs =
    map
        nftArgv
        ( [ Netfilter.AddTable partitionTable
          , Netfilter.AddChain partitionChain
          ]
            <> [Netfilter.AddRule partitionChain (Netfilter.RawRule ["ip", "saddr", addr, "drop"]) | addr <- addrs]
        )

partitionTable :: Netfilter.Table
partitionTable = Netfilter.Table "salmon_test_partition" Netfilter.Inet

partitionChain :: Netfilter.Chain
partitionChain =
    Netfilter.baseChain
        "input"
        partitionTable
        (Netfilter.BaseChainSpec Netfilter.FilterChain Netfilter.Input 0 Netfilter.Accept)

{- | What the builtin would run, as words to send somewhere else.

Each word is quoted where it is used rather than here: ssh joins its
arguments with spaces and the remote shell splits them again, so a chain
spec's @{@, @;@ and @}@ arrive as shell syntax unless something stops them.
-}
nftArgv :: Netfilter.NftCommand -> [String]
nftArgv cmd = case cmdspec (Binary.prepare Netfilter.nftcommand cmd) of
    RawCommand bin args -> bin : args
    ShellCommand sh -> ["sh", "-c", sh]
