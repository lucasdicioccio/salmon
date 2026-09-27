{-# LANGUAGE OverloadedStrings #-}

{- | What the Patroni Layer 3 specs share: three guests booted at once on the
test bridge, each carrying @postgresql@, @patroni@, @etcd-server@ and
@haproxy@, and a way to start every scenario from the same state
(@specs\/pg-patroni.md@, "Disaster scenarios").

The rootfses are built by @salmon-patroni-rootfs@ (see @PatroniRootfs@ in
salmon-apps), because the guests cannot reach the network past boot:

> sudo $(cabal list-bin salmon-patroni-rootfs) config prereqs | sudo $(cabal list-bin salmon-patroni-rootfs) run up

As in "Test.PostgresVms", a rootfs is a host directory that outlives its VM,
so a scenario that killed a leader leaves the next one a cluster with a
history. 'withPatroniVms' therefore /normalizes/ each guest on the way in
('resetGuest') instead of assuming a blank machine: it stops the three
services and removes what they persist (etcd's data, Patroni's cluster
directory, the Postgres cluster the package created). It starts nothing:
what runs where is the scenario's declaration.
-}
module Test.PatroniVms (
    patroniRootfs,
    patroniAddrs,
    requirePatroniVmPrereqs,
    withPatroniVms,
    resetGuest,
    requiredBinaries,
    missingBinaries,
) where

import Control.Monad (forM, unless)
import Data.Text (Text)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)

import Test.Harness

-- | One rootfs per guest, numbered from 1.
patroniRootfs :: [FilePath]
patroniRootfs =
    [ "/var/lib/salmon-test-vms/patroni-" <> show n <> "/root"
    | n <- [1 .. 3 :: Int]
    ]

-- | The guests' addresses on the shared test bridge, in the order of 'patroniRootfs'.
patroniAddrs :: [Text]
patroniAddrs = [testVmAddr, testVmAddr2, testVmAddr3]

{- | Skips loudly rather than failing when the machine cannot run these:
qemu, the bridge privileges, and the three rootfses.
-}
requirePatroniVmPrereqs :: IO () -> IO ()
requirePatroniVmPrereqs act = do
    privileged <- hasVmPrivileges
    hasQemu <- (/= Nothing) <$> findExecutable "qemu-system-x86_64"
    present <- forM patroniRootfs $ \r -> (,) r <$> doesFileExist (r <> "/etc/issue")
    case () of
        _
            | not privileged -> skip "needs root, or ip/qemu-system-x86_64 setcap'd (see Test.Harness.hasVmPrivileges)"
            | not hasQemu -> skip "qemu-system-x86_64 not found on PATH"
            | ((r, _) : _) <- filter (not . snd) present ->
                skip ("no VM rootfs at " <> r <> " (build them with salmon-patroni-rootfs, see Test.PatroniVms)")
            | otherwise -> act
  where
    skip msg = hPutStrLn stderr ("SKIPPED: " <> msg)

{- | Boots the three guests (nested 'withVmAt's, so all three are torn down
however the action exits), normalizes each, and hands over their accesses in
the order of 'patroniAddrs'.
-}
withPatroniVms :: ([VmAccess] -> IO a) -> IO a
withPatroniVms act =
    withVmAt (patroniAddrs !! 0) (patroniRootfs !! 0) $ \a ->
        withVmAt (patroniAddrs !! 1) (patroniRootfs !! 1) $ \b ->
            withVmAt (patroniAddrs !! 2) (patroniRootfs !! 2) $ \c -> do
                let vms = [a, b, c]
                mapM_ resetGuest vms
                act vms

{- | Puts one guest in the state every scenario starts from: the three
services stopped and disabled from starting on their own (the Debian
packages start a cluster and an etcd on install, and a rootfs remembers), and
everything they persist removed.

Removing the Postgres cluster is what lets Patroni initialize its own: it
refuses a data directory that already holds a cluster it did not create. The
etcd data directory goes for the same reason from the other side, since a
stale member list makes a fresh three-member cluster refuse to bootstrap.
-}
resetGuest :: VmAccess -> IO ()
resetGuest vm = do
    (code, out, err) <-
        sshToVm
            vm
            [ "bash"
            , "-c"
            , quoteForRemoteShell . unwords $
                [ "set -e;"
                , "systemctl stop patroni haproxy etcd postgresql 2>/dev/null || true;"
                , "systemctl disable patroni haproxy etcd 2>/dev/null || true;"
                , "pkill -u postgres 2>/dev/null || true;"
                , "rm -rf /var/lib/etcd/* /var/lib/postgresql/*/main /etc/postgresql/*/main;"
                , "rm -f /etc/patroni/*.yml /etc/patroni.yml"
                ]
            ]
    unless (code == ExitSuccess) (fail ("could not normalize a guest: " <> out <> err))

-- | The binaries each guest must have: the "done" condition for the harness.
requiredBinaries :: [String]
requiredBinaries = ["patroni", "patronictl", "etcd", "etcdctl", "haproxy", "pg_ctlcluster", "psql"]

-- | Which of 'requiredBinaries' this guest lacks, checked the way a shell would find them.
missingBinaries :: VmAccess -> IO [String]
missingBinaries vm = do
    results <- forM requiredBinaries $ \bin -> do
        (code, _, _) <- sshToVm vm ["bash", "-c", quoteForRemoteShell ("command -v " <> bin)]
        pure (if code == ExitSuccess then [] else [bin])
    pure (concat results)
