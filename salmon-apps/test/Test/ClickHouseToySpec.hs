{- | @salmon-toy-clickhouse@.

Layer 0: the directive and the two graphs, folded as @run up@ folds them,
nothing run: the members, the cluster and its secrets as references, the
migrations' clause, the backup job's scope, the drill's isolation, the
shipped migrations.

Layer 2, opt-in with @SALMON_TEST_CLICKHOUSE_TOY=1@: the toy end to end
under rootless podman and this user's systemd (user-scope quadlets, a
user-scope backup job and timer), on names of its own: up, a second pass
that restarts nothing, rows replicated, a backup taken by the job, the
drill restoring it, then both torn down. Everything it made is removed
afterwards whatever happened (containers, network, unit files, the data
directory through @podman unshare@). It needs @podman@, podman's user
generator, a @systemd --user@ manager and the server image already pulled,
and says so when one is missing.
-}
module Test.ClickHouseToySpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Functor.Identity (runIdentity)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Text as Text
import System.Directory (doesFileExist, findExecutable, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.UpDown as UpDown
-- GHC only solves a `HasField` constraint when the selector is in scope
import Salmon.Builtin.Extension (Extension, Op, check, down, dynamics, evalDeps, help, notes, ref, up)
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import qualified Salmon.Builtin.Nodes.ClickHouse.Backup as Backup
import qualified Salmon.Builtin.Nodes.ClickHouse.Cluster as Cluster
import qualified Salmon.Builtin.Nodes.ClickHouse.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)

import ClickHouseToy

tests :: TestTree
tests =
    testGroup
        "salmon-toy-clickhouse"
        [ testCase "the directive survives JSON" directiveRoundTrips
        , testCase "up folds to one node per effect site, three user-scope members" upGraphShape
        , testCase "the members: one Keeper ensemble, one shard of three, secrets by reference" membersShape
        , testCase "migrations are applied ON CLUSTER main" migrationsOnCluster
        , testCase "the shipped migrations load and carry the token" shippedMigrations
        , testCase "the backup job is the user's, keeps every file, ships nothing by default" backupShape
        , testCase "with a bucket, the script ships and names only the credential file's path" backupWithGcs
        , testCase "the drill is a server of its own, guarded, on no network" drillShape
        , testCase "the newest backup of this toy is the drill's default" newestBackup
        , testCase "the secrets file is two variables" secretsShape
        , testCase "Layer 2: the toy end to end under rootless podman (SALMON_TEST_CLICKHOUSE_TOY=1)" liveToy
        ]

-------------------------------------------------------------------------------
-- Layer 0

toy :: Toy
toy = exampleToy

migrationsFixture :: [Migrations.Migration]
migrationsFixture =
    [ Migrations.Migration "0001_events.sql" "/srv/m/0001_events.sql" "aa"
    , Migrations.Migration "0002_events_source.sql" "/srv/m/0002_events_source.sql" "bb"
    ]

upSpec :: Spec
upSpec = Up toy migrationsFixture ["hourly"] Nothing

drillSpec :: Spec
drillSpec = Drill toy "chtoy-20261031T020000Z.zip" Nothing False

dagOf :: Spec -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps . graph
  where
    graph :: Spec -> Op
    graph = run program

nodesOf :: Spec -> [Act Extension]
nodesOf = Map.elems . Dag.dagNodes . dagOf

helps :: Spec -> [String]
helps = fmap (\a -> Text.unpack a.extension.help) . nodesOf

shorthands :: Spec -> [String]
shorthands = fmap (\a -> Text.unpack a.shorthand) . nodesOf

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

directiveRoundTrips :: IO ()
directiveRoundTrips = do
    let gcs = GcsSpec "acme-backups" "acme-project" "clickhouse" (Just "/etc/acme/sa.json")
    forM_ [upSpec, Up toy migrationsFixture ["*-*-* 03:00:00"] (Just gcs), drillSpec, Drill toy "f.zip" (Just gcs) True] $ \s ->
        assertEqual "decode . encode" (Right s) (Aeson.eitherDecode (Aeson.encode s))

upGraphShape :: IO ()
upGraphShape = do
    assertEqual "collisions" 0 (length (Dag.dagConflicts (dagOf upSpec)))
    let hs = helps upSpec
        sh = shorthands upSpec
    assertEqual "three quadlets" 3 (count "podman-quadlet" sh)
    forM_ (memberNames toy) $ \n ->
        assertBool ("a quadlet for " <> Text.unpack n) (any (("as " <> Text.unpack n <> ".service") `isInfixOf`) hs)
    assertEqual "one network" 1 (count "podman-network" sh)
    assertEqual "one secret" 1 (count "toy-secret" sh)
    assertEqual "one quorum wait" 1 (count "wait-until" sh)
    assertEqual "a Keeper session per member" 3 (count "clickhouse-keeper-connected" sh)
    assertEqual "the cluster seen by each member" 3 (count "clickhouse-cluster-visible" sh)
    assertEqual "one ON CLUSTER database" 1 (count "clickhouse-cluster-database" sh)
    assertEqual "two migrations" 2 (count "clickhouse-migration" sh)
    assertEqual "the backup's last run" 1 (count "systemd-job-completed" sh)
    assertBool "its timer" (any ("clickhouse-backup-chtoy.timer" `isInfixOf`) hs)
    assertBool "quadlets in the user's directory" (any (("writes " <> toyQuadletDir toy <> "/chtoy-1.container") `isInfixOf`) hs)
    assertBool "units in the user's directory" (any (("writes " <> toyUnitDir toy <> "/clickhouse-backup-chtoy.service") `isInfixOf`) hs)
    -- nothing in a node's description carries a value from the secrets file
    forM_ (nodesOf upSpec) $ \a ->
        forM_ (a.extension.help : a.extension.notes) $ \t ->
            assertBool ("a secret's assignment in " <> Text.unpack t) (not ("CH_CLUSTER_SECRET=" `Text.isInfixOf` t))

membersShape :: IO ()
membersShape = do
    let s1 = memberServer toy 1
        dropIns = fmap (\d -> (d.dropInName, d.dropInContents)) s1.serverDropIns
    assertEqual "a Keeper and a cluster drop-in" ["salmon-keeper.xml", "salmon-cluster.xml"] (fmap fst dropIns)
    assertBool "member 1 does not wait for itself" (not s1.serverWaitReady)
    assertEqual "the env file" (Just (secretsFile toy)) s1.serverEnvFile
    assertEqual "the network" (Just (networkName toy)) s1.serverNetwork
    assertEqual "one shard of the three" [Cluster.shardOf (memberNames toy)] (toyCluster toy).clusterShards
    let keeper = maybe "" id (lookup "salmon-keeper.xml" dropIns)
        cluster = maybe "" id (lookup "salmon-cluster.xml" dropIns)
    forM_ (memberNames toy) $ \n ->
        assertBool ("raft lists " <> Text.unpack n) (("<hostname>" <> n <> "</hostname>") `Text.isInfixOf` keeper)
    assertBool "the secret by reference" ("<secret from_env=\"CH_CLUSTER_SECRET\" />" `Text.isInfixOf` cluster)
    assertBool "the interserver password by reference" ("<password from_env=\"CH_INTERSERVER_PASSWORD\" />" `Text.isInfixOf` cluster)
    let q = ClickHouse.serverContainer (memberServer toy 2)
    assertBool "the drop-ins are watched" (any ("salmon-cluster.xml" `isInfixOf`) q.containerWatched)
    assertEqual "the env file is podman's to read" (Just (secretsFile toy)) (q :: Quadlet.Container).containerEnvFile

migrationsOnCluster :: IO ()
migrationsOnCluster = do
    let ms = toyMigrations migrationsFixture
    assertEqual "the clause" (Just (Cluster.onClusterClause "main")) ms.migrationsOnCluster
    assertEqual "the ledger's database" "toy" ms.migrationsDatabase
    assertEqual
        "the token replaced"
        "CREATE TABLE IF NOT EXISTS toy.t  ON CLUSTER `main` (x UInt8) ENGINE = ReplicatedMergeTree ORDER BY x"
        (Migrations.renderMigration ms.migrationsOnCluster "CREATE TABLE IF NOT EXISTS toy.t {{on_cluster}} (x UInt8) ENGINE = ReplicatedMergeTree ORDER BY x")

-- | Run from the package directory, as @cabal test@ does.
shippedMigrations :: IO ()
shippedMigrations = do
    ms <- Migrations.loadMigrationsDir "clickhouse-toy/migrations"
    assertEqual "two, in order" ["0001_events.sql", "0002_events_source.sql"] (fmap (.migrationName) ms)
    forM_ ms $ \m -> do
        body <- readFile m.migrationPath
        assertBool (m.migrationPath <> " says {{on_cluster}}") ("{{on_cluster}}" `isInfixOf` body)
        assertBool (m.migrationPath <> " tolerates a rerun") ("IF NOT EXISTS" `isInfixOf` body)

backupShape :: IO ()
backupShape = do
    let p = toyBackupPlan toy Nothing
        j = toyBackupJob toy (Backup.backupJob p)
    assertEqual "no problems" [] (Backup.planProblems p)
    assertEqual "member 1's client" (ClickHouse.serverClient (memberServer toy 1)) p.planClient
    assertEqual "every local file kept" Nothing p.planKeepLocal
    assertEqual "no bucket" Nothing p.planGcs
    assertEqual "user scope" Systemd.User j.jobScope
    assertEqual "the user's unit directory" (toyUnitDir toy) j.jobUnitDir
    assertEqual "named after the toy" "clickhouse-backup-chtoy" j.jobName
    let script = Backup.renderBackupScript p
    assertBool "no gcloud" (not ("gcloud" `Text.isInfixOf` script))
    assertBool ("through member 1: " <> Text.unpack script) ("'podman' 'exec' '-i' 'chtoy-1'" `Text.isInfixOf` script)
    assertEqual "the timer" (Job.jobTimer j [Job.OnCalendar "hourly"]).timerScope Systemd.User

backupWithGcs :: IO ()
backupWithGcs = do
    let p = toyBackupPlan toy (Just (GcsSpec "acme-backups" "acme-project" "ch" (Just "/etc/acme/sa.json")))
        script = Backup.renderBackupScript p
    assertBool "ships with rsync" ("'storage' 'rsync'" `Text.isInfixOf` script)
    assertBool "to the bucket and prefix" ("gs://acme-backups/ch" `Text.isInfixOf` script)
    assertBool "the credential file by path" ("CLOUDSDK_AUTH_CREDENTIAL_FILE_OVERRIDE='/etc/acme/sa.json'" `Text.isInfixOf` script)
    assertBool "never deletes in the bucket" (not ("--delete-unmatched-destination-objects" `Text.isInfixOf` script))

drillShape :: IO ()
drillShape = do
    assertEqual "collisions" 0 (length (Dag.dagConflicts (dagOf drillSpec)))
    let s = drillServer toy
        r = toyRestore toy "chtoy-20261031T020000Z.zip" Nothing False
        sh = shorthands drillSpec
    assertEqual "its own name" (drillName toy) (Podman.getContainerName s.serverName)
    assertBool "no network" (isNothing s.serverNetwork)
    assertBool "no env file: it shares no secret" (isNothing s.serverEnvFile)
    assertBool "embeds its own Keeper" (any (\d -> d.dropInName == "salmon-keeper.xml" && "<hostname>localhost</hostname>" `Text.isInfixOf` d.dropInContents) s.serverDropIns)
    assertEqual "one quadlet" 1 (count "podman-quadlet" sh)
    assertEqual "one restore" 1 (count "clickhouse-restore" sh)
    assertEqual "no migration, no backup, no member" 0 (count "clickhouse-migration" sh + count "systemd-job-completed" sh + count "podman-network" sh)
    assertEqual "into toy" "toy" r.restoreAs
    assertBool "refuses a toy that is not this restore" (not r.restoreMayDiscard)
    assertEqual "from the shared directory" (backupsDir toy) r.restoreHostDir
    assertEqual "through the drill server" (ClickHouse.serverClient s) r.restoreClient
    assertEqual "with --may-discard" True (toyRestore toy "f.zip" Nothing True).restoreMayDiscard
    assertBool "a drill from GCS fetches" $ case (toyRestore toy "f.zip" (Just (GcsSpec "b" "p" "" Nothing)) False).restoreSource of
        Backup.FromGcs _ -> True
        Backup.LocalBackup -> False

newestBackup :: IO ()
newestBackup = do
    let names = ["chtoy-20261031T010000Z.zip", "chtoy-20261031T020000Z.zip", "other-20261231T000000Z.zip", "chtoy-20261031T030000Z.zip.download", "notes.txt"]
    assertEqual "the newest of this toy's" (Just "chtoy-20261031T020000Z.zip") (latestBackup toy names)
    assertEqual "none" Nothing (latestBackup toy ["other-1.zip"])

secretsShape :: IO ()
secretsShape =
    assertEqual "two lines" "CH_CLUSTER_SECRET=s\nCH_INTERSERVER_PASSWORD=p\n" (renderSecrets "s" "p")

-------------------------------------------------------------------------------
-- Layer 2

liveToy :: IO ()
liveToy = do
    wanted <- lookupEnv "SALMON_TEST_CLICKHOUSE_TOY"
    if wanted /= Just "1"
        then hPutStrLn stderr "SKIPPED: set SALMON_TEST_CLICKHOUSE_TOY=1 to run the ClickHouse toy under rootless podman"
        else do
            missing <- prerequisites
            case missing of
                Just why -> hPutStrLn stderr ("SKIPPED: " <> why <> "; the ClickHouse toy was not run")
                Nothing -> runLive

prerequisites :: IO (Maybe String)
prerequisites = do
    podman <- findExecutable "podman"
    generator <- or <$> traverse doesFileExist ["/usr/lib/systemd/user-generators/podman-user-generator", "/usr/libexec/podman/quadlet"]
    (manager, _, _) <- readProcessWithExitCode "systemctl" ["--user", "show-environment"] ""
    (image, _, _) <- readProcessWithExitCode "podman" ["image", "exists", Text.unpack exampleToy.toyImage] ""
    pure (verdict podman generator manager image)
  where
    verdict podman generator manager image
        | isNothing podman = Just "`podman` not found on PATH"
        | not generator = Just "podman's quadlet user generator is not installed"
        | manager /= ExitSuccess = Just "no `systemd --user` manager is answering"
        | image /= ExitSuccess = Just (Text.unpack exampleToy.toyImage <> " is not on this machine (pull it first)")
        | otherwise = Nothing

runLive :: IO ()
runLive = do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "salmon-chtoy"
    -- names of its own: a container, network or unit of another run is never touched
    stamp <- (\s -> show ((floor (s * 1000) :: Integer) `mod` 1000000)) <$> getPOSIXTime
    let base = exampleToy{toyRoot = root, toyPrefix = Text.pack ("chtt" <> stamp)}
    home <- maybe "" id <$> lookupEnv "HOME"
    let t = base{toyQuadletDir = home </> ".config/containers/systemd", toyUnitDir = home </> ".config/systemd/user"}
    ms <- Migrations.loadMigrationsDir "clickhouse-toy/migrations"
    let up' = Up t ms ["hourly"] Nothing
    flip finally (cleanup t) $ do
        ok <- runUp up'
        assertBool "up" ok
        started <- traverse startedAt (memberNames t)
        again <- runUp up'
        assertBool "second up" again
        started' <- traverse startedAt (memberNames t)
        assertEqual "a second pass restarts no member" started started'
        -- rows written on member 1 reach member 3
        void (sql t (memberName t 1) "INSERT INTO toy.events (at, kind, value) SELECT now(), 'demo', number FROM numbers(1000)")
        replicated <- poll 30 ((== "1000") <$> sql t (memberName t 3) "SELECT count() FROM toy.events")
        assertBool "1000 rows on member 3" replicated
        ledger <- sql t (memberName t 1) "SELECT name FROM toy.salmon_migrations ORDER BY name FORMAT TSV"
        assertEqual "the ledger" "0001_events.sql\n0002_events_source.sql" ledger
        -- a backup now, as the timer would take it
        (code, _, err) <- readProcessWithExitCode "systemctl" ["--user", "start", "clickhouse-backup-" <> Text.unpack t.toyPrefix <> ".service"] ""
        unless (code == ExitSuccess) $ assertFailure ("the backup job: " <> err)
        (_, listing, _) <- readProcessWithExitCode "podman" ["unshare", "ls", "-1", backupsDir t] ""
        file <- maybe (assertFailure ("no backup in " <> listing) >> pure "") pure (latestBackup t (Text.lines (Text.pack listing)))
        assertEqual "two backups: the pass's and this one" 2 (length (filter (".zip" `isSuffixOf'`) (lines listing)))
        let drill = Drill t file Nothing False
        drilled <- runUp drill
        assertBool "drill" drilled
        restored <- sql t (drillName t) "SELECT count(), sum(value) FROM toy.events FORMAT TSV"
        assertEqual "the rows, restored" "1000\t499500" restored
        marker <- sql t (drillName t) "SELECT comment FROM system.databases WHERE name = 'toy'"
        assertEqual "the restore's marker" ("salmon-restore: " <> Text.unpack file <> " database toy") marker
        drilledAgain <- runUp drill
        assertBool "second drill" drilledAgain
        downDrill <- runDown drill
        assertBool "drill down" downDrill
        downUp <- runDown up'
        assertBool "up down" downUp
        (_, left, _) <- readProcessWithExitCode "podman" ["ps", "-a", "--format", "{{.Names}}"] ""
        assertEqual "no container left" [] (filter ((Text.unpack t.toyPrefix <> "-") `isPrefixOf`) (lines left))
  where
    isSuffixOf' s x = reverse s `isPrefixOf` reverse x

memberName :: Toy -> Int -> Text.Text
memberName t i = memberNames t !! (i - 1)

runUp :: Spec -> IO Bool
runUp s = UpDown.upTree silent (pure . runIdentity) (run program s)

runDown :: Spec -> IO Bool
runDown s = UpDown.downTree silent (pure . runIdentity) (run program s)

startedAt :: Text.Text -> IO String
startedAt n = do
    (_, out, _) <- readProcessWithExitCode "podman" ["inspect", "--format", "{{.State.StartedAt}}", Text.unpack n] ""
    pure out

sql :: Toy -> Text.Text -> String -> IO String
sql _ n q = do
    (_, out, _) <- readProcessWithExitCode "podman" ["exec", Text.unpack n, "clickhouse-client", "--query", q] ""
    pure (Text.unpack (Text.strip (Text.pack out)))

poll :: Int -> IO Bool -> IO Bool
poll seconds act = go (seconds * 2)
  where
    go n = do
        ok <- act
        if ok || n <= 0 then pure ok else threadDelay 500000 >> go (n - 1)

{- | Whatever the run left: the containers, the network, the unit files (a
failed @up@ may leave them), the failed-unit records, the data.
-}
cleanup :: Toy -> IO ()
cleanup t = do
    let names = memberNames t <> [drillName t]
        prefix = Text.unpack t.toyPrefix
        quadlets = [t.toyQuadletDir </> Text.unpack n <> ".container" | n <- names]
        units = [t.toyUnitDir </> "clickhouse-backup-" <> prefix <> ext | ext <- [".service", ".timer"]]
    void (readProcessWithExitCode "systemctl" ["--user", "stop", "clickhouse-backup-" <> prefix <> ".timer"] "")
    forM_ names $ \n -> void (readProcessWithExitCode "systemctl" ["--user", "stop", Text.unpack n <> ".service"] "")
    forM_ names $ \n -> void (readProcessWithExitCode "podman" ["rm", "-f", "-t", "1", Text.unpack n] "")
    void (readProcessWithExitCode "podman" ["network", "rm", "-f", Text.unpack (networkName t)] "")
    forM_ (quadlets <> units) $ \f -> do
        there <- doesFileExist f
        when there (removeFile f)
    void (readProcessWithExitCode "systemctl" ["--user", "daemon-reload"] "")
    void (readProcessWithExitCode "systemctl" (["--user", "reset-failed"] <> [Text.unpack n <> ".service" | n <- names] <> ["clickhouse-backup-" <> prefix <> ".service"]) "")
    void (readProcessWithExitCode "podman" ["unshare", "rm", "-rf", t.toyRoot] "")
