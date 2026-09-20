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
        , testCase "a crashed primary is failed over only when its writes are declared expendable" failsOverFromACrash
        ]

{- | S3: crash the primary, fail over to the standby, and let the machine
that crashed rejoin.

Two things separate this from the switchover above, and both are the point.
The old primary is not stopped, it /dies/ -- so its last checkpoint is no
longer the end of its WAL, and nothing on either machine can say what it
wrote after it. That is a state salmon is not entitled to decide about, so
the first pass here refuses and the second one is given
'SreBox.PostgresPair.pair_may_discard': the operator saying which side's
writes they accept losing, which is the only thing that makes a failover
different from a guess.

And the machine that comes back is rewound rather than re-seeded. The two
are hard to tell apart afterwards -- same rows, same system identifier, same
timeline -- so the test holds on to something only a re-clone destroys.
-}
failsOverFromACrash :: IO ()
failsOverFromACrash = requirePgVmPrereqs $ do
    fixtureBin <- resolveFixtureBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b -> do
            buildPair a b fixtureBin
            resetRows a
            insertRow a "replicated-before-the-crash"
            waitForRows b ["replicated-before-the-crash"]

            -- with the standby down, what A writes now reaches nobody: these
            -- are the writes the flag below is about.
            stopCluster b
            insertRow a "written-while-b-was-down"
            identity <- dataDirectoryIdentity a
            crashCluster a

            -- nothing declared: a pass may start what is stopped, and may
            -- not promote over a machine whose WAL it cannot account for.
            let declared = pairWith a b Pair.B
            refused <- runUp (Pair.pairRole silent declared)
            assertBool "promoted over a crashed primary with nothing declared" (not refused)
            assertInRecovery b

            -- the operator accepts losing A's un-replicated writes
            let failover = declared{Pair.pair_may_discard = Just Pair.A}
            passOrExplain "the failover" failover
            assertPrimaryIs b
            insertRow b "written-on-b-after-the-failover"
            assertStandbyOf a testVmAddr2

            identity' <- dataDirectoryIdentity a
            assertEqual "the old primary was re-cloned rather than rewound" identity identity'

            -- what was replicated survived, on both machines
            waitForRows a ["replicated-before-the-crash", "written-on-b-after-the-failover"]
            waitForRows b ["replicated-before-the-crash", "written-on-b-after-the-failover"]
            -- and what was not is gone, which is what the flag said
            assertNoRow b "written-while-b-was-down"
            assertNoRow a "written-while-b-was-down"

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

{- | Runs the node, and on failure says what the machines looked like.

The node reports through a 'Salmon.Reporter.Reporter', which these tests
leave 'silent' -- so a pass that failed is otherwise just @False@, and the
one thing worth knowing, which of the steps refused or threw, is exactly
what was thrown away. Deciding again costs two ssh round trips and turns
that into a sentence.
-}
passOrExplain :: String -> Pair.Pair -> IO ()
passOrExplain what p = do
    ok <- runUp (Pair.pairRole silent p)
    unless ok $ do
        obsA <- Pair.observe p Pair.A
        obsB <- Pair.observe p Pair.B
        retried <- try (Pair.converge silent p) :: IO (Either SomeException ())
        fail . unlines $
            [ what <> " failed"
            , "  A: " <> show obsA
            , "  B: " <> show obsB
            , "  next step: " <> show (Pair.nextStep p obsA obsB [])
            , "  running it again said: " <> show retried
            ]

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

-- | Polls: a rejoined standby takes a moment to connect to its new primary.
assertStandbyOf :: VmAccess -> Text.Text -> IO ()
assertStandbyOf vm host =
    waitFor ("never became a standby of " <> Text.unpack host) $ do
        (_, out, _) <- psql vm "SELECT coalesce((SELECT sender_host FROM pg_stat_wal_receiver LIMIT 1), 'none');"
        pure (Text.unpack host `isInfixOf` out, out)

insertRow :: VmAccess -> String -> IO ()
insertRow vm v =
    psqlOrDie vm ("CREATE TABLE IF NOT EXISTS salmon_switchover (v text); INSERT INTO salmon_switchover VALUES ('" <> v <> "');")

{- | Starts the canaries over. The rootfses outlive the VMs, so a row from a
previous run is otherwise still there -- which matters to the one assertion
here that a row is /absent/.
-}
resetRows :: VmAccess -> IO ()
resetRows vm = psqlOrDie vm "DROP TABLE IF EXISTS salmon_switchover;"

waitForRows :: VmAccess -> [String] -> IO ()
waitForRows vm vs =
    waitFor ("expected " <> show vs) $ do
        (_, out, _) <- psql vm "SELECT v FROM salmon_switchover ORDER BY v;"
        pure (all (`isInfixOf` out) vs, out)

assertNoRow :: VmAccess -> String -> IO ()
assertNoRow vm v = do
    (_, out, _) <- psql vm "SELECT v FROM salmon_switchover ORDER BY v;"
    assertBool ("expected " <> v <> " to be gone, got: " <> out) (not (v `isInfixOf` out))

waitForStandby :: VmAccess -> IO ()
waitForStandby vm =
    waitFor "the standby never started streaming" $ do
        (code, out, _) <- psql vm "SELECT status FROM pg_stat_wal_receiver;"
        pure (code == ExitSuccess && "streaming" `isInfixOf` out, out)
