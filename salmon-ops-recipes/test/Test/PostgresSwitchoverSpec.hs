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
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import Test.Harness
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

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
        ]

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
        , Pair.pair_may_discard = Nothing
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
