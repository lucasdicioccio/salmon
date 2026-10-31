{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.ClickHouse.Backup".

Layer 0: the backup disk's drop-in, the quadlet with the backup directory,
the script a run is (with and without GCS), the job, the restore statements
and the readings of the target.

Layer 2 ('liveBackup'): the official image under rootless podman with the
backup drop-in and directory mounted; the rendered script run three times
(as root of podman's user namespace, standing for the system-scope job's
root: the files are the container user's), pruning to the newest two; the
database dropped and restored by the 'Backup.restore' node, row counts
compared; a second pass applying nothing; a restore over a database holding
other data refused, then done with the operator's flag; restores into an
absent and an empty database. The timer, the job under systemd and GCS are
not run here: GCS cannot be reached from the test host, so shipping and
fetching are Layer 0 only.
-}
module Test.ClickHouseBackupSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, void)
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Op.Actions (Act (..))
import Salmon.Builtin.Nodes.Binary (Command (..))
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import qualified Salmon.Builtin.Nodes.ClickHouse.Backup as Backup
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Readiness as Readiness
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)
import System.Process (CmdSpec (..), CreateProcess (..))
import Test.Harness

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.ClickHouse.Backup"
        [ testGroup "Layer 0" layer0
        , testCase "Layer 2: back up, drop and restore against the official image under podman" liveBackup
        ]

containerClient :: ClickHouse.Client
containerClient = ClickHouse.Client (ClickHouse.InContainer (Podman.ContainerName "ch")) 9000

bucket :: Storage.Bucket
bucket = Storage.Bucket "acme-ch-backups" (Core.Project "acme-prod") (Core.Region "europe-west1") True

plan :: Backup.BackupPlan
plan = Backup.backupPlan "nightly" containerClient ["shop", "we`ird"] "/var/lib/salmon/ch-backups" "/etc/salmon/ch-backup"

gcsPlan :: Backup.BackupPlan
gcsPlan =
    plan
        { Backup.planGcs = Just (Backup.GcsTarget bucket "/clickhouse/prod/" (Just "/run/secrets/gcs-backup.json"))
        , Backup.planKeepLocal = Just 3
        }

localRestore :: Backup.Restore
localRestore = Backup.restoreFrom containerClient "/var/lib/salmon/ch-backups" "nightly-20261031T020000Z.zip" "shop"

layer0 :: [TestTree]
layer0 =
    [ testCase "the drop-in declares the backups disk and allows nothing else" $ do
        let out = Text.unpack Backup.renderBackupConfig
        forM_
            [ "<backups>\n                <type>local</type>\n                <path>/var/lib/clickhouse-backups/</path>"
            , "<allowed_disk>backups</allowed_disk>"
            , "<allowed_path>/var/lib/clickhouse-backups/</allowed_path>"
            ]
            $ \line -> assertBool (line <> " in\n" <> out) (line `isInfixOf` out)
    , testCase "the quadlet adds the backup directory and the watched drop-in, and keeps the rest" $ do
        let s = ClickHouse.server (Podman.ContainerName "ch") "docker.io/clickhouse/clickhouse-server:25.8" "/etc/salmon/ch" "/var/lib/salmon/ch"
            plain = ClickHouse.serverContainer s
            c = Backup.backupServerContainer "/var/lib/salmon/ch-backups" s
            out = Text.unpack (Quadlet.renderContainer c)
        assertEqual "no problems" [] (Quadlet.containerProblems c)
        forM_
            [ "Volume=/var/lib/salmon/ch-backups:/var/lib/clickhouse-backups:rw"
            , "Volume=/etc/salmon/ch/salmon-backups.xml:/etc/clickhouse-server/config.d/salmon-backups.xml:ro"
            ]
            $ \line -> assertBool (line <> " in\n" <> out) (line `isInfixOf` out)
        assertEqual "watched" (Quadlet.watchedFiles plain <> ["/etc/salmon/ch/salmon-backups.xml"]) (Quadlet.watchedFiles c)
        assertBool "every line of the plain quadlet is still there" (all (`elem` lines out) (lines (Text.unpack (Quadlet.renderContainer plain))))
    , testCase "a backup statement names every database and the backups disk" $ do
        let (h, t) = Backup.backupSqlParts plan
        assertEqual "" "BACKUP DATABASE `shop`, DATABASE `we\\`ird` TO Disk('backups', '" h
        assertEqual "" "')" t
    , testCase "the script without GCS: the backup through the container's client, nothing shipped or pruned" $ do
        let out = Text.unpack (Backup.renderBackupScript plan)
        assertEqual "no problems" [] (Backup.planProblems plan)
        forM_
            [ "#!/bin/sh"
            , "set -eu"
            , "dir='/var/lib/salmon/ch-backups'"
            , "name='nightly-'\"$(date -u +%Y%m%dT%H%M%SZ)\"'.zip'"
            , "printf '%s%s%s\\n' 'BACKUP DATABASE `shop`, DATABASE `we\\`ird` TO Disk('\\''backups'\\'', '\\''' \"$name\" ''\\'')' | 'podman' 'exec' '-i' 'ch' 'clickhouse-client' '--port' '9000'"
            ]
            $ \line -> assertBool (line <> " in\n" <> out) (line `elem` lines out)
        assertBool out (not ("gcloud" `isInfixOf` out))
        assertBool out (not ("rm -f" `isInfixOf` out))
    , testCase "the script with GCS ships this plan's files with gcloud storage rsync, names the credential file by path, then prunes" $ do
        let out = Text.unpack (Backup.renderBackupScript gcsPlan)
            ls = lines out
        assertEqual "no problems" [] (Backup.planProblems gcsPlan)
        forM_
            [ "export CLOUDSDK_CORE_DISABLE_PROMPTS=1"
            , "export CLOUDSDK_AUTH_CREDENTIAL_FILE_OVERRIDE='/run/secrets/gcs-backup.json'"
            , "'gcloud' 'storage' 'rsync' '/var/lib/salmon/ch-backups' 'gs://acme-ch-backups/clickhouse/prod' '--recursive' '--checksums-only' '--exclude=^(?!nightly-[0-9]{8}T[0-9]{6}Z\\.zip$).*$' '--project' 'acme-prod' '--quiet'"
            , "ls -1 \"$dir\" | grep -E '^nightly-[0-9]{8}T[0-9]{6}Z\\.zip$' | sort -r | tail -n +4 | while read -r old; do rm -f -- \"$dir/$old\"; done"
            ]
            $ \line -> assertBool (line <> " in\n" <> out) (line `elem` ls)
        assertBool "nothing is deleted in the bucket" (not ("--delete-unmatched-destination-objects" `isInfixOf` out))
        let order = [i | (i, l) <- zip [0 :: Int ..] ls, any (`isPrefixOf` l) ["printf", "'gcloud'", "ls -1"]]
        assertEqual "backup, ship, prune, in that order" 3 (length order)
        assertEqual "" order (sort order)
    , testCase "the pruning and shipping pattern is this plan's files and no other" $ do
        -- the same pattern, read by grep -E, as the script does
        let names = ["nightly-20261031T020000Z.zip", "nightly-20261031T020000Z.zip.download", "hourly-20261031T020000Z.zip", "nightly-2026.zip", "xnightly-20261031T020000Z.zip"]
        (_, out, _) <- readProcessWithExitCode "grep" ["-E", Text.unpack (Backup.backupFileRegex plan)] (unlines names)
        assertEqual "" ["nightly-20261031T020000Z.zip"] (lines out)
    , testCase "a plan that cannot be rendered is refused" $ do
        let bad = plan{Backup.planName = "night ly", Backup.planDatabases = [], Backup.planKeepLocal = Just 0, Backup.planHostDir = "rel"}
        assertEqual "" 4 (length (Backup.planProblems bad))
        assertEqual "dotted prefix" 1 (length (Backup.planProblems plan{Backup.planGcs = Just (Backup.gcsTarget bucket "a/../b")}))
    , testCase "the job runs the script with sh, keeps a stamp, and its run is newer than the script" $ do
        let j = Backup.backupJob plan
        assertEqual "" "clickhouse-backup-nightly" j.jobName
        assertEqual "" ["/bin/sh", "/etc/salmon/ch-backup/clickhouse-backup-nightly.sh"] j.jobCommand
        assertEqual "" (Just "/etc/salmon/ch-backup/clickhouse-backup-nightly.stamp") j.jobStamp
        assertEqual "" [] (Job.jobProblems j)
        let c = Backup.backupCompletion j plan
        assertEqual "" ["/etc/systemd/system/clickhouse-backup-nightly.service", "/etc/salmon/ch-backup/clickhouse-backup-nightly.sh"] c.completionWritten
        assertEqual "" (Just "/etc/salmon/ch-backup/clickhouse-backup-nightly.stamp") c.completionStamp
        let tm = Job.jobTimer j [Job.OnCalendar "*-*-* 02:00:00"]
        assertBool "" ("OnCalendar=*-*-* 02:00:00" `Text.isInfixOf` Job.renderTimer tm)
    , testCase "restore statements: under its own name, under another, the marker, the drop" $ do
        assertEqual "" "RESTORE DATABASE `shop` FROM Disk('backups', 'nightly-20261031T020000Z.zip');\n" (Backup.restoreSql localRestore)
        assertEqual "" "RESTORE DATABASE `shop` AS `shop_copy` FROM Disk('backups', 'nightly-20261031T020000Z.zip');\n" (Backup.restoreSql localRestore{Backup.restoreAs = "shop_copy"})
        assertEqual "" "ALTER DATABASE `shop` MODIFY COMMENT 'salmon-restore: nightly-20261031T020000Z.zip database shop';\n" (Backup.markSql localRestore)
        assertEqual "" "DROP DATABASE `shop` SYNC;\n" (Backup.dropSql localRestore)
        assertEqual "" [] (Backup.restoreProblems localRestore)
        assertEqual "" 1 (length (Backup.restoreProblems localRestore{Backup.restoreBackup = "../etc/passwd"}))
    , testCase "the target is read: absent, this restore, empty, someone else's" $ do
        let marker = Text.unpack (Backup.restoreMarker localRestore)
        assertEqual "" (Just Backup.TargetAbsent) (Backup.interpretTarget localRestore "")
        assertEqual "" (Just Backup.TargetRestored) (Backup.interpretTarget localRestore (Text.pack (marker <> "\t3\n")))
        assertEqual "" (Just Backup.TargetEmpty) (Backup.interpretTarget localRestore "\t0\n")
        assertEqual "" (Just (Backup.TargetHolds 2)) (Backup.interpretTarget localRestore "salmon-restore: other.zip database shop\t2\n")
        assertEqual "" Nothing (Backup.interpretTarget localRestore "garbage")
        assertEqual "" Success (Backup.restoreVerdict localRestore Backup.TargetRestored)
        assertBool "" (isFailure (Backup.restoreVerdict localRestore (Backup.TargetHolds 2)))
    , testCase "a database holding tables is replaced only with the operator's flag" $ do
        assertEqual "" (Right Backup.RestoreInto) (Backup.restoreDecision localRestore Backup.TargetAbsent)
        assertEqual "" (Right Backup.NothingToRestore) (Backup.restoreDecision localRestore Backup.TargetRestored)
        assertEqual "" (Right Backup.DropThenRestore) (Backup.restoreDecision localRestore Backup.TargetEmpty)
        assertBool "refused" (either (const True) (const False) (Backup.restoreDecision localRestore (Backup.TargetHolds 1)))
        assertEqual "" (Right Backup.DropThenRestore) (Backup.restoreDecision localRestore{Backup.restoreMayDiscard = True} (Backup.TargetHolds 1))
    , testCase "a fetch from GCS is one gcloud storage cp, and the credential file is never on the argv" $ do
        let cmd = Storage.ObjectCopy (Core.Project "acme-prod") "gs://acme-ch-backups/clickhouse/prod/nightly-20261031T020000Z.zip" "/var/lib/salmon/ch-backups/nightly-20261031T020000Z.zip.download"
        assertEqual
            ""
            ["storage", "cp", "gs://acme-ch-backups/clickhouse/prod/nightly-20261031T020000Z.zip", "/var/lib/salmon/ch-backups/nightly-20261031T020000Z.zip.download", "--project", "acme-prod", "--quiet"]
            (args cmd)
        let g = Backup.GcsTarget bucket "clickhouse/prod" (Just "/run/secrets/gcs-backup.json")
        assertEqual "" [("CLOUDSDK_AUTH_CREDENTIAL_FILE_OVERRIDE", "/run/secrets/gcs-backup.json")] (Backup.gcsEnvironment g)
        let o = Backup.restore silent ignoreTrack ignoreTrack localRestore{Backup.restoreSource = Backup.FromGcs g}
        case opAct o of
            Just a -> do
                assertBool "the notes name the object" ("from: gs://acme-ch-backups/clickhouse/prod/nightly-20261031T020000Z.zip" `elem` a.extension.notes)
                assertBool "no credential path in help or notes" (not (any ("gcs-backup.json" `Text.isInfixOf`) (a.extension.help : a.extension.notes)))
            Nothing -> assertFailure "an actionless node"
    ]
  where
    args cmd = case cmdspec (prepare Storage.storageCommand cmd) of
        RawCommand _ as -> as
        ShellCommand s -> [s]

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

-------------------------------------------------------------------------------

liveBackup :: IO ()
liveBackup = requireExecutable "podman" $
    withTempDir $ \dir -> do
        let serverXml = dir </> "salmon-server.xml"
            adminXml = dir </> "salmon-admin.xml"
            backupsXml = dir </> "salmon-backups.xml"
            backups = dir </> "backups"
            scripts = dir </> "scripts"
        writeFile serverXml (Text.unpack (ClickHouse.renderServerConfig ClickHouse.defaultServerConfig))
        writeFile adminXml (Text.unpack ClickHouse.renderAdminAccess)
        writeFile backupsXml (Text.unpack Backup.renderBackupConfig)
        cname <- freshContainerName
        (reporter, readBack) <- capture
        let img = Podman.Image "clickhouse/clickhouse-server:25.8"
            opts =
                Podman.noRunOptions
                    { Podman.runVolumes =
                        [ Podman.VolumeMount serverXml "/etc/clickhouse-server/config.d/salmon-server.xml" Podman.ReadOnly
                        , Podman.VolumeMount adminXml "/etc/clickhouse-server/users.d/salmon-admin.xml" Podman.ReadOnly
                        , Podman.VolumeMount backupsXml "/etc/clickhouse-server/config.d/salmon-backups.xml" Podman.ReadOnly
                        , Podman.VolumeMount backups Backup.backupMountPoint Podman.ReadWrite
                        ]
                    }
            runOp = Podman.runContainer reporter podmanTrack Podman.dockerRegistry img cname opts
            pullOp = Podman.pullImage reporter podmanTrack Podman.dockerRegistry img
            container = Text.unpack (Podman.getContainerName cname)
            client = ClickHouse.Client (ClickHouse.InContainer cname) ClickHouse.defaultTcpPort
            p = (Backup.backupPlan "nightly" client ["shop"] backups scripts){Backup.planKeepLocal = Just 2}
            sql q = do
                (code, out, err) <- podmanExecCapture container ["clickhouse-client", "--multiquery", "--query", q]
                assertEqual ("query " <> q <> ": " <> err) ExitSuccess code
                pure (concat (lines out))
            count db = sql ("SELECT count() FROM `" <> db <> "`.orders")
            -- the job runs as root; rootless, the files are a subordinate
            -- uid's, which root of podman's user namespace stands for
            asRoot argv = readProcessWithExitCode "podman" ("unshare" : argv) ""
            runBackup = do
                (code, out, err) <- asRoot ["sh", Backup.backupScriptPath p]
                assertEqual ("the backup script: " <> out <> err) ExitSuccess code
                threadDelay 1100000
            backupFiles = do
                (_, out, _) <- asRoot ["ls", "-1", backups]
                pure (sort (lines out))
        pulled <- runUp pullOp
        assertBool "pull the image" pulled
        -- the directory is the backup node's, as in backupServerOp
        made <- runUp (Backup.backupDirectory backups)
        assertBool "the backup directory" made
        let cleanup = void (runDown runOp) >> void (asRoot ["rm", "-rf", backups])
        flip finally cleanup $ do
            started <- runUp runOp
            failures <- podmanFailures <$> readBack
            assertBool ("start the container" <> concat failures) started
            let waitOp = Readiness.waitUntil (Readiness.untilProbe "clickhouse-backup-test-ready" (ClickHouse.readyProbe client) 60)
                waitT = Track (const waitOp)
            ready <- runUp waitOp
            assertBool "the server answers within 60s" ready
            void (sql "CREATE DATABASE shop; CREATE TABLE shop.orders (id UInt64) ENGINE = MergeTree ORDER BY id; INSERT INTO shop.orders SELECT number FROM numbers(1000)")
            scripted <- runUp (Backup.backupScript p)
            assertBool "the script is written" scripted
            runBackup
            void (sql "INSERT INTO shop.orders SELECT number FROM numbers(10)")
            runBackup
            runBackup
            files <- backupFiles
            assertEqual ("two newest kept: " <> show files) 2 (length files)
            let (older, newest) = case files of
                    [a, b] -> (Text.pack a, Text.pack b)
                    _ -> ("", "")
            assertBool "named by the plan" (all ("nightly-" `isPrefixOf`) files)
            -- back up, drop, restore, compare
            void (sql "DROP DATABASE shop SYNC")
            (chr, _) <- capture
            let restoreNewest = Backup.restoreFrom client backups newest "shop"
                restoreOp x = Backup.restore chr waitT ignoreTrack x
            first <- runUpCapturing (restoreOp restoreNewest)
            assertBool ("the restore succeeds: " <> show (failed first)) (null (failed first))
            assertEqual "the rows are back" "1010" =<< count "shop"
            assertEqual "the restore reads done" Success =<< checkOf (restoreOp restoreNewest)
            second <- runUpCapturing (restoreOp restoreNewest)
            assertEqual "the second pass applies nothing" [] [help a | UpDown.Done a <- second, isRestore a]
            -- data salmon cannot prove is that restore is not discarded
            void (sql "INSERT INTO shop.orders SELECT number FROM numbers(5)")
            let restoreOlder = (Backup.restoreFrom client backups older "shop")
            refused <- runUpCapturing (restoreOp restoreOlder)
            assertBool ("a restore over another restore's data is refused: " <> show (failed refused)) (any ("restoreMayDiscard" `isInfixOf`) (failed refused))
            assertEqual "nothing changed" "1015" =<< count "shop"
            flagged <- runUpCapturing (restoreOp restoreOlder{Backup.restoreMayDiscard = True})
            assertBool ("with the operator's flag it is replaced: " <> show (failed flagged)) (null (failed flagged))
            assertEqual "the older backup's rows" "1010" =<< count "shop"
            -- into another name, absent then empty
            copy <- runUpCapturing (restoreOp restoreNewest{Backup.restoreAs = "shop_copy"})
            assertBool ("restore AS: " <> show (failed copy)) (null (failed copy))
            assertEqual "" "1010" =<< count "shop_copy"
            void (sql "CREATE DATABASE shop_empty")
            empty <- runUpCapturing (restoreOp restoreNewest{Backup.restoreAs = "shop_empty"})
            assertBool ("restore into an empty database: " <> show (failed empty)) (null (failed empty))
            assertEqual "" "1010" =<< count "shop_empty"
  where
    help :: Act' -> String
    help a = Text.unpack a.extension.help
    isRestore :: Act' -> Bool
    isRestore a = "restore ClickHouse" `Text.isPrefixOf` a.extension.help
    failed reports = [help a <> ": " <> show e | UpDown.Failed a e <- reports]
    checkOf o = case opAct o of
        Just a -> a.extension.check
        Nothing -> pure Unknown
