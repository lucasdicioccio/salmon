{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "SreBox.PostgresBackup".

Two things are worth testing without a database, and they are the two things
that go wrong in practice. The first is the /script/: it is generated text
that ends up running unattended as @postgres@, so what matters is that it
parses, that values are quoted, and above all that it sets @pipefail@ — the
hand-written version of this script checks @$?@ after @pg_dump | gzip@, which
is gzip's status, so a failed dump is stored as a valid archive and reported
as a success. The second is 'interpretBackupAge', which is the whole of the
"is there actually a recent backup" verdict.
-}
module Test.PostgresBackupSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Builtin.Nodes.CronTask as Cron
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified SreBox.PostgresBackup as Backup

tests :: TestTree
tests =
    testGroup
        "SreBox.PostgresBackup"
        [ testGroup "the generated script" scriptTests
        , testGroup "interpretBackupAge" freshnessTests
        , testGroup "schedules" scheduleTests
        ]

script :: Backup.PgBackupConfig -> String
script = Text.unpack . Backup.renderBackupScript

plain :: Backup.PgBackupConfig
plain = Backup.defaultBackupConfig "newco_api" (Just "newco_api_owner")

shipped :: Backup.PgBackupConfig
shipped = plain{Backup.pgb_gcs = Just (Backup.GcsDestination "acme-backups" "newco/api")}

scriptTests :: [TestTree]
scriptTests =
    [ testCase "sets pipefail, so a failed pg_dump is not stored as a valid archive" $
        -- The regression this whole module exists around: `pg_dump | gzip`
        -- followed by a check of $? tests gzip, not pg_dump.
        assertBool (script plain) ("set -euo pipefail" `isInfixOf` script plain)
    , testCase "the default connects over the unix socket as postgres, where peer auth lives" $ do
        -- Naming 127.0.0.1 turns the same call into TCP, which pg_hba wants a
        -- password for -- the most common way a backup job on the database
        -- host fails.
        let s = script plain
        assertBool s ("sudo -u 'postgres' pg_dump" `isInfixOf` s)
        assertBool s (not ("-h " `isInfixOf` s))
    , testCase "a TCP server and role are passed when asked for" $ do
        let s = script plain{Backup.pgb_server = Just (Postgres.Server "db.example" 5433), Backup.pgb_sudoUser = Nothing}
        assertBool s ("-h 'db.example'" `isInfixOf` s)
        assertBool s ("-p 5433" `isInfixOf` s)
        assertBool s ("-U 'newco_api_owner'" `isInfixOf` s)
        assertBool s (not ("sudo" `isInfixOf` s))
    , testCase "prunes only this database's dumps, by the configured age" $ do
        let s = script plain
        assertBool s ("-name 'newco_api_*.sql.gz'" `isInfixOf` s)
        assertBool s ("-mtime +7 -delete" `isInfixOf` s)
    , testCase "a database name that would break out of quoting cannot" $ do
        let s = script plain{Backup.pgb_database = "db'; rm -rf / #"}
        assertBool s ("'db'\\''; rm -rf / #'" `isInfixOf` s)
    , testCase "no bucket means no upload" $
        assertBool (script plain) (not ("gcloud storage cp" `isInfixOf` script plain))
    , testCase "the upload names the bucket and prefix" $
        assertBool (script shipped) ("gcloud storage cp \"$BACKUP_FILE\" 'gs://acme-backups/newco/api/'" `isInfixOf` script shipped)
    , testCase "an empty prefix does not produce a doubled slash" $ do
        let s = script plain{Backup.pgb_gcs = Just (Backup.GcsDestination "acme-backups" "")}
        assertBool s ("'gs://acme-backups/'" `isInfixOf` s)
    , testCase "the upload happens before the prune" $ do
        -- Ordering is load-bearing: under `set -e` a failed upload stops the
        -- script, so a dump that never reached the bucket is still on disk.
        let s = script shipped
            Just uploadAt = substringIndex "gcloud storage cp" s
            Just pruneAt = substringIndex "-delete" s
        assertBool s (uploadAt < pruneAt)
    , testCase "credentials become environment, never arguments" $ do
        -- /proc/<pid>/cmdline is world-readable; /proc/<pid>/environ is not.
        assertBool (script plain) (not ("PGPASSFILE" `isInfixOf` script plain))
        let s = script plain{Backup.pgb_credentials = Backup.PassFile "/var/lib/postgresql/.pgpass"}
        assertBool s ("export PGPASSFILE='/var/lib/postgresql/.pgpass'" `isInfixOf` s)
        let c = script plain{Backup.pgb_credentials = Backup.ClientCertificate "/c/c.pem" "/c/k.pem" "/c/ca.pem"}
        assertBool c ("export PGSSLKEY='/c/k.pem'" `isInfixOf` c)
        assertBool c ("export PGSSLMODE=verify-ca" `isInfixOf` c)
    , testCase "a frozen timestamp is used verbatim, so another machine can name the dump" $ do
        let s = script plain{Backup.pgb_fixedTimestamp = Just "20260919_101500"}
        assertBool s ("TIMESTAMP='20260919_101500'" `isInfixOf` s)
        assertBool s (not ("date +" `isInfixOf` s))
    , testCase "rendered scripts parse as bash" $
        mapM_
            ( \cfg -> do
                (code, _, err) <- readProcessWithExitCode "bash" ["-n", "-c", script cfg] ""
                assertEqual err ExitSuccess code
            )
            [plain, shipped, plain{Backup.pgb_credentials = Backup.PassFile "/tmp/pass"}]
    ]

substringIndex :: String -> String -> Maybe Int
substringIndex needle haystack =
    case [i | (i, rest) <- zip [0 ..] (tails' haystack), needle `isPrefixOf'` rest] of
        (i : _) -> Just i
        [] -> Nothing
  where
    tails' [] = [[]]
    tails' s@(_ : rest) = s : tails' rest
    isPrefixOf' p s = take (length p) s == p

freshnessTests :: [TestTree]
freshnessTests =
    [ testCase "no dump at all is the most actionable failure there is" $
        assertBool "" (isFailure (Backup.interpretBackupAge day now Nothing))
    , testCase "a dump inside the window is satisfying" $
        assertEqual
            ""
            Success
            (Backup.interpretBackupAge day now (Just ("/b/db_1.sql.gz", addUTCTime (-3600) now)))
    , testCase "a dump exactly at the window is still satisfying" $
        assertEqual
            ""
            Success
            (Backup.interpretBackupAge day now (Just ("/b/db_1.sql.gz", addUTCTime (negate day) now)))
    , testCase "an older dump names itself and its age" $
        case Backup.interpretBackupAge day now (Just ("/b/db_1.sql.gz", addUTCTime (-3 * day) now)) of
            Failure msg -> do
                assertBool (show msg) ("/b/db_1.sql.gz" `Text.isInfixOf` msg)
                assertBool (show msg) ("72h" `Text.isInfixOf` msg)
            other -> assertBool (show other) False
    ]
  where
    day = 86400
    now = UTCTime (fromGregorian 2026 9 19) (secondsToDiffTime 0)

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

scheduleTests :: [TestTree]
scheduleTests =
    [ testCase "dailyAt puts the minute first, as crontab wants" $
        assertEqual "" ("17", "3", "*", "*", "*") (fields (Cron.dailyAt "3" "17"))
    , testCase "hourlyAt runs every hour" $
        assertEqual "" ("5", "*", "*", "*", "*") (fields (Cron.hourlyAt "5"))
    , testCase "weeklyAt pins the day of week" $
        assertEqual "" ("0", "4", "*", "*", "7") (fields (Cron.weeklyAt "7" "4" "0"))
    , testCase "the default config backs up a week's worth, daily, off the hour" $ do
        let cfg = Backup.defaultBackupConfig "db" (Just "owner")
        assertEqual "" 7 cfg.pgb_retentionDays
        assertEqual "" ("17", "3", "*", "*", "*") (fields cfg.pgb_schedule)
        -- slack over the period, so a merely late run is not a missing backup
        assertBool "" (cfg.pgb_maxAge > 86400)
        assertEqual "" Nothing cfg.pgb_server
    ]
  where
    fields :: Cron.Schedule -> (Text.Text, Text.Text, Text.Text, Text.Text, Text.Text)
    fields s = (s.minute, s.hour, s.dayOfMonth, s.month, s.dayOfWeek)
