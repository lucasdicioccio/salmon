{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for cluster lifecycle and streaming replication in
"Salmon.Builtin.Nodes.Postgres": the clone guard, the cluster-state and
pending-restart verdicts, and the settings a primary is given.

The clone guard is the reason this module exists. @cloneFromPrimaryScript@
runs @rm -rf@ on a data directory, and what decides whether it gets that far
is a shell script -- so the assertions here are about /order/: that the
script has learned whose cluster is in that directory, and had a chance to
refuse, before anything is deleted. See @specs\/pg-switchover.md@ (P1).
-}
module Test.PostgresClusterSpec (tests) where

import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Builtin.Nodes.Postgres as Postgres

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Postgres (clusters and replication)"
        [ testGroup "the clone guard" cloneGuardTests
        , testGroup "interpretClusterStatus" clusterStatusTests
        , testGroup "interpretPendingRestart" pendingRestartTests
        , testGroup "replicationSettings" settingsTests
        ]

-------------------------------------------------------------------------------

setup :: Postgres.StandbySetup
setup =
    Postgres.StandbySetup
        { Postgres.standby_cluster = "replica"
        , Postgres.standby_primary_host = "10.0.0.1"
        , Postgres.standby_primary_port = 5432
        , Postgres.standby_repl_user = Postgres.User "replicator"
        , Postgres.standby_repl_passfile = "/etc/postgresql/repl.pass"
        , Postgres.standby_slot = Just "replica_slot"
        }

script :: String
script = Postgres.cloneFromPrimaryScript setup

-- | Where a fragment first appears, for asserting on order.
at :: String -> Int
at needle
    | not (needle `isInfixOf` script) = error ("fragment not in the script: " <> needle)
    | otherwise = length (takeWhile (not . (needle `isPrefixOf`)) (tails script))

cloneGuardTests :: [TestTree]
cloneGuardTests =
    [ testCase "asks the primary for its system identifier" $
        assertBool script ("IDENTIFY_SYSTEM" `isInfixOf` script)
    , testCase "reads the local system identifier from pg_controldata" $
        assertBool script ("pg_controldata" `isInfixOf` script && "Database system identifier" `isInfixOf` script)
    , testCase "both identifiers are known before anything is deleted" $ do
        assertBool "primary identifier" (at "primary_sysid=$(" < at "rm -rf")
        assertBool "local identifier" (at "local_sysid=$(" < at "rm -rf")
    , testCase "a matching identifier leaves the directory alone" $
        assertBool script (at "if [ \"$local_sysid\" = \"$primary_sysid\" ]" < at "rm -rf")
    , testCase "the refusal comes before the deletion" $
        assertBool script (at "refusing to clone over" < at "rm -rf")
    , testCase "a foreign cluster is judged by its user databases" $
        assertBool script ("16384" `isInfixOf` script && at "others=" < at "rm -rf")
    , -- the guard this replaced: promotion deletes standby.signal, so a
      -- promoted standby read as "never cloned" and was wiped.
      testCase "does not depend on standby.signal" $
        assertBool script (not ("standby.signal" `isInfixOf` script))
    , testCase "stops at the first failing command" $
        assertBool script ("set -e" `isInfixOf` script)
    , -- a script is visible in `ps` and printed by every report on the way.
      -- PGPASSFILE rather than PGPASSWORD: a path, not the secret, and the
      -- same .pgpass that `primary_conninfo`'s passfile= reads once this is
      -- streaming -- so one file serves the clone and the streaming, and a
      -- pair has one secret per role rather than two spellings of it.
      testCase "reads the password from its file, and never carries it" $ do
        assertBool script ("PGPASSFILE='/etc/postgresql/repl.pass'" `isInfixOf` script)
        assertBool script (not ("PGPASSWORD" `isInfixOf` script))
        assertBool script (not ("hunter2" `isInfixOf` script))
    , testCase "streams from the declared slot" $
        assertBool script ("-S replica_slot" `isInfixOf` script)
    , testCase "no slot declared, no -S" $
        assertBool "" (not ("-S " `isInfixOf` Postgres.cloneFromPrimaryScript setup{Postgres.standby_slot = Nothing}))
    ]

-------------------------------------------------------------------------------

-- | @pg_lsclusters --no-header@: Ver Cluster Port Status Owner DataDirectory LogFile
lsclusters :: Text -> Text
lsclusters status =
    Text.unlines
        [ "15 main    5432 online postgres /var/lib/postgresql/15/main /var/log/postgresql/a.log"
        , "15 replica 5433 " <> status <> " postgres /var/lib/postgresql/15/replica /var/log/postgresql/b.log"
        ]

clusterStatusTests :: [TestTree]
clusterStatusTests =
    [ testCase "online, and online is wanted" $
        assertEqual "" Success (Postgres.interpretClusterStatus Postgres.Online "replica" (lsclusters "online"))
    , testCase "down, and online is wanted" $
        assertEqual "" (Failure "replica is down") (Postgres.interpretClusterStatus Postgres.Online "replica" (lsclusters "down"))
    , testCase "down, and down is wanted" $
        assertEqual "" Success (Postgres.interpretClusterStatus Postgres.Down "replica" (lsclusters "down"))
    , testCase "the other cluster's status is not this one's" $
        assertEqual "" Success (Postgres.interpretClusterStatus Postgres.Online "main" (lsclusters "down"))
    , -- a cluster replaying WAL has started but is not yet serving, and
      -- reading that as "already up" lets a dependant run against it.
      testCase "recovering is neither online nor down" $
        assertBool "" (isFailure (Postgres.interpretClusterStatus Postgres.Online "replica" (lsclusters "online,recovery")))
    , testCase "a cluster that is not listed at all" $
        assertEqual "" (Failure "no cluster named ghost") (Postgres.interpretClusterStatus Postgres.Online "ghost" (lsclusters "online"))
    , testCase "no clusters at all" $
        assertEqual "" (Failure "no cluster named main") (Postgres.interpretClusterStatus Postgres.Online "main" "")
    ]

pendingRestartTests :: [TestTree]
pendingRestartTests =
    [ testCase "nothing pending" $
        assertEqual "" Success (Postgres.interpretPendingRestart "\n")
    , testCase "names what is waiting" $
        assertEqual
            ""
            (Failure "settings waiting for a restart: max_wal_senders,wal_log_hints")
            (Postgres.interpretPendingRestart "max_wal_senders,wal_log_hints\n")
    ]

-------------------------------------------------------------------------------

settingsTests :: [TestTree]
settingsTests =
    [ testCase "pg_rewind is possible on a default primary" $
        assertEqual "" (Just "on") (lookup "wal_log_hints" defaults)
    , testCase "a lagging standby cannot fill the primary's disk" $
        assertEqual "" (Just "10GB") (lookup "max_slot_wal_keep_size" defaults)
    , testCase "an uncapped slot is expressible, and says so by its absence" $
        assertEqual
            ""
            Nothing
            (lookup "max_slot_wal_keep_size" (Postgres.replicationSettings Postgres.defaultReplicationTuning{Postgres.repl_max_slot_wal_keep_size = Nothing}))
    , testCase "hint logging can be turned off explicitly" $
        assertEqual
            ""
            (Just "off")
            (lookup "wal_log_hints" (Postgres.replicationSettings Postgres.defaultReplicationTuning{Postgres.repl_wal_log_hints = False}))
    , testCase "the senders and slots come from the tuning" $ do
        assertEqual "" (Just "10") (lookup "max_wal_senders" defaults)
        assertEqual "" (Just "10") (lookup "max_replication_slots" defaults)
    , testCase "replication is explicitly enabled" $
        assertEqual "" (Just "replica") (lookup "wal_level" defaults)
    ]
  where
    defaults = Postgres.replicationSettings Postgres.defaultReplicationTuning

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False
