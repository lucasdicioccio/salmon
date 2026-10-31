{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.ClickHouse.Migrations".

Layer 0: planning (filtering, ordering, checksums, refused names), the SQL,
the cluster clause and its token, the ledger's reading, and the graph's
order. Layer 2 ('liveMigrations'): the official image under podman, a
directory of migrations applied, a second pass applying nothing, a new file
applied alone, and an edited applied file refused without being run.
-}
module Test.ClickHouseMigrationsSpec (tests) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.ByteString as ByteString
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import Salmon.Builtin.Nodes.ClickHouse.Migrations (Migration (..), Migrations (..))
import qualified Salmon.Builtin.Nodes.ClickHouse.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Readiness as Readiness
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)
import Test.Harness

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.ClickHouse.Migrations"
        [ testGroup "Layer 0" layer0
        , testCase "Layer 2: migrations applied, recorded and refused when edited, against clickhouse-server under podman" liveMigrations
        ]

client :: ClickHouse.Client
client = ClickHouse.Client (ClickHouse.InContainer (Podman.ContainerName "ch")) 9000

layer0 :: [TestTree]
layer0 =
    [ testCase "only .sql files, ordered by name, each with the SHA-256 of its bytes" $ do
        let planned =
                Migrations.planMigrations
                    [ ("/m/0002_b.sql", "B")
                    , ("/m/README", "x")
                    , ("/m/0001_a.sql", "A")
                    , ("/m/0010_c.sql", "C")
                    ]
        case planned of
            Left ps -> fail (show ps)
            Right ms -> do
                assertEqual "order" ["0001_a.sql", "0002_b.sql", "0010_c.sql"] (fmap migrationName ms)
                assertEqual "paths" ["/m/0001_a.sql", "/m/0002_b.sql", "/m/0010_c.sql"] (fmap migrationPath ms)
                -- printf A | sha256sum
                assertEqual "checksum" ["559aead08264d5795d3909718cdd05abd49572e84fe55590eef31a88a08fdffd"] (fmap migrationChecksum (take 1 ms))
    , testCase "a name with a backslash or a control character, and a duplicate name, are refused" $ do
        let bad = Migrations.planMigrations [("/m/a\\b.sql", ""), ("/m/x\ty.sql", ""), ("/m/c.sql", ""), ("/n/c.sql", "")]
        assertEqual "" (Left 3) (either (Left . length) (Right . length) bad)
    , testCase "the ledger table renders no cluster clause by default" $ do
        let sql = Migrations.ledgerTableSql (Migrations.singleServer "default" [])
        assertBool (Text.unpack sql) ("CREATE TABLE IF NOT EXISTS `default`.`salmon_migrations`\n(" `Text.isPrefixOf` sql)
        assertBool (Text.unpack sql) ("ENGINE = MergeTree\nORDER BY name;" `Text.isInfixOf` sql)
        assertBool (Text.unpack sql) (not ("CLUSTER" `Text.isInfixOf` sql))
    , testCase "the cluster clause goes on the ledger and replaces the token in files" $ do
        let clause = " ON CLUSTER `main`"
            ms = (Migrations.singleServer "ops" []){migrationsOnCluster = Just clause}
        assertBool "" ("`ops`.`salmon_migrations` ON CLUSTER `main`\n" `Text.isInfixOf` Migrations.ledgerTableSql ms)
        assertEqual "with" "CREATE TABLE t ON CLUSTER `main` (x UInt8);" (Migrations.renderMigration (Just clause) "CREATE TABLE t{{on_cluster}} (x UInt8);")
        assertEqual "without" "CREATE TABLE t (x UInt8);" (Migrations.renderMigration Nothing "CREATE TABLE t{{on_cluster}} (x UInt8);")
        assertEqual "untouched otherwise" "SELECT '{cluster}';" (Migrations.renderMigration Nothing "SELECT '{cluster}';")
    , testCase "the record is quoted" $
        assertEqual
            ""
            "INSERT INTO `ops`.`salmon_migrations` (name, checksum) VALUES ('it\\'s.sql', 'abc');\n"
            (Migrations.recordSql "ops" (Migration "it's.sql" "/m/it's.sql" "abc"))
    , testCase "the ledger reads: applied, pending, and a changed checksum named loudly" $ do
        let a = Migration "0001_a.sql" "/m/0001_a.sql" "aaa"
            b = Migration "0002_b.sql" "/m/0002_b.sql" "bbb"
            rows = Migrations.parseLedger "0001_a.sql\taaa\ngarbage\n0099_other.sql\tzzz\n"
        assertEqual "rows" [("0001_a.sql", "aaa"), ("0099_other.sql", "zzz")] rows
        assertEqual "all applied" Success (Migrations.interpretLedger [a] rows)
        assertEqual "one pending" (Failure "pending migrations: 0002_b.sql") (Migrations.interpretLedger [a, b] rows)
        assertEqual "an empty plan" Success (Migrations.interpretLedger [] [])
        case Migrations.interpretLedger [a {migrationChecksum = "new"}, b] rows of
            Failure t -> do
                assertBool (Text.unpack t) ("0001_a.sql was applied with checksum aaa but the file now has new" `Text.isInfixOf` t)
                assertBool (Text.unpack t) ("never re-applies" `Text.isInfixOf` t)
            other -> fail (show other)
        assertEqual "status" (Migrations.Mismatch ["aaa"]) (Migrations.status rows a{migrationChecksum = "new"})
    , testCase "a missing ledger or database reads as nothing applied" $ do
        assertBool "" (Migrations.missingLedger "Code: 60. DB::Exception: Table default.salmon_migrations does not exist. (UNKNOWN_TABLE)")
        assertBool "" (not (Migrations.missingLedger "Code: 210. Connection refused (NETWORK_ERROR)"))
    , testCase "each migration waits for the previous one, the first for the ledger, the ledger for its prerequisites" $ do
        let a = Migration "0001_a.sql" "/m/0001_a.sql" "aaa"
            b = Migration "0002_b.sql" "/m/0002_b.sql" "bbb"
            dbOp = ClickHouse.database silent ignoreTrack client "ops"
            o = Migrations.migrations silent ignoreTrack client [dbOp] (Migrations.singleServer "ops" [a, b])
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps o)
            key = ClickHouse.clientKey client
            ledgerRef = mkRef "clickhouse-migrations-ledger" (key, "ops" :: Text.Text)
            refA = mkRef "clickhouse-migration" (key, "ops" :: Text.Text, "0001_a.sql" :: Text.Text)
            refB = mkRef "clickhouse-migration" (key, "ops" :: Text.Text, "0002_b.sql" :: Text.Text)
            dbRef = mkRef "clickhouse-db" (key, "ops" :: Text.Text)
        assertBool "b after a" (refA `elem` Dag.dependenciesOf dag refB)
        assertBool "a after the ledger" (ledgerRef `elem` Dag.dependenciesOf dag refA)
        assertBool "the ledger after the database" (dbRef `elem` Dag.dependenciesOf dag ledgerRef)
        case Dag.representativeOf dag refA of
            Nothing -> fail "no node for 0001_a.sql"
            Just act -> assertBool (show act.extension.notes) ("checksum: sha256:aaa" `elem` act.extension.notes)
    ]

-------------------------------------------------------------------------------

{- | Two files applied in order (the second uses the first's table), the
checks then 'Success', a second pass applying nothing, a third file added
and applied alone, then the first file edited: its check a 'Failure' naming
both checksums, and @up@ refusing it without running it.
-}
liveMigrations :: IO ()
liveMigrations = requireExecutable "podman" $
    withTempDir $ \dir -> do
        let serverXml = dir </> "salmon-server.xml"
            adminXml = dir </> "salmon-admin.xml"
            mdir = dir </> "migrations"
        writeFile serverXml (Text.unpack (ClickHouse.renderServerConfig ClickHouse.defaultServerConfig))
        writeFile adminXml (Text.unpack ClickHouse.renderAdminAccess)
        createDirectory mdir
        ByteString.writeFile (mdir </> "0001_events.sql") "CREATE TABLE IF NOT EXISTS ops.events{{on_cluster}} (id UInt64, kind String) ENGINE = MergeTree ORDER BY id;\n"
        ByteString.writeFile (mdir </> "0002_seed.sql") "-- a comment, then two statements\nINSERT INTO ops.events VALUES (1, 'a');\nINSERT INTO ops.events VALUES (2, 'b')\n"
        ByteString.writeFile (mdir </> "notes.txt") "not a migration"
        cname <- freshContainerName
        (reporter, readBack) <- capture
        let img = Podman.Image "clickhouse/clickhouse-server:25.8"
            opts =
                Podman.noRunOptions
                    { Podman.runVolumes =
                        [ Podman.VolumeMount serverXml "/etc/clickhouse-server/config.d/salmon-server.xml" Podman.ReadOnly
                        , Podman.VolumeMount adminXml "/etc/clickhouse-server/users.d/salmon-admin.xml" Podman.ReadOnly
                        ]
                    }
            runOp = Podman.runContainer reporter podmanTrack Podman.dockerRegistry img cname opts
            pullOp = Podman.pullImage reporter podmanTrack Podman.dockerRegistry img
            container = Text.unpack (Podman.getContainerName cname)
        pulled <- runUp pullOp
        assertBool "pull the image" pulled
        bracket (runUp runOp) (\_ -> void (runDown runOp)) $ \started -> do
            failures <- podmanFailures <$> readBack
            assertBool ("start the container" <> concat failures) started
            let c = ClickHouse.Client (ClickHouse.InContainer cname) ClickHouse.defaultTcpPort
                waitOp = Readiness.waitUntil (Readiness.untilProbe "clickhouse-migrations-test-ready" (ClickHouse.readyProbe c) 60)
                waitT = Track (const waitOp)
            ready <- runUp waitOp
            assertBool "the server answers within 60s" ready
            let dbOp = ClickHouse.database silent waitT c "ops"
                graphOf ms = Migrations.migrations silent waitT c [dbOp] (Migrations.singleServer "ops" ms)
            planned <- Migrations.loadMigrationsDir mdir
            assertEqual "planned" ["0001_events.sql", "0002_seed.sql"] (fmap migrationName planned)
            assertEqual "pending before" (Failure "pending migrations: 0001_events.sql, 0002_seed.sql") =<< Migrations.checkMigrations c (Migrations.singleServer "ops" planned)
            first <- runUpCapturing (graphOf planned)
            assertEqual ("first pass: " <> show (failed first)) [] (failed first)
            assertEqual "applied in order" 2 (length (migrationsDone first))
            assertBool (show (migrationsDone first)) (all ("0001_events.sql" `isInfixOf`) (take 1 (migrationsDone first)))
            assertEqual "all applied" Success =<< Migrations.checkMigrations c (Migrations.singleServer "ops" planned)
            assertEqual "rows" "1\ta\n2\tb\n" =<< query container "SELECT id, kind FROM ops.events ORDER BY id FORMAT TSV"
            second <- runUpCapturing (graphOf planned)
            assertEqual ("second pass: " <> show (failed second)) [] (failed second)
            assertEqual "the second pass applies nothing" [] (migrationsDone second)
            -- a new file: only it runs
            ByteString.writeFile (mdir </> "0003_more.sql") "INSERT INTO ops.events VALUES (3, 'c');\n"
            planned3 <- Migrations.loadMigrationsDir mdir
            third <- runUpCapturing (graphOf planned3)
            assertEqual ("third pass: " <> show (failed third)) [] (failed third)
            assertEqual "only the new one" 1 (length (migrationsDone third))
            assertEqual "rows after" "3\n" =<< query container "SELECT count() FROM ops.events FORMAT TSV"
            assertEqual "ledger" "0001_events.sql\n0002_seed.sql\n0003_more.sql\n" =<< query container "SELECT name FROM ops.salmon_migrations ORDER BY name FORMAT TSV"
            -- an applied file edited: a loud failure, nothing run
            ByteString.writeFile (mdir </> "0002_seed.sql") "INSERT INTO ops.events VALUES (99, 'edited');\n"
            edited <- Migrations.loadMigrationsDir mdir
            verdict <- Migrations.checkMigrations c (Migrations.singleServer "ops" edited)
            assertBool ("an edited migration is a Failure: " <> show verdict) $ case verdict of
                Failure t -> "0002_seed.sql was applied with checksum" `Text.isInfixOf` t
                _ -> False
            fourth <- runUpCapturing (graphOf edited)
            assertBool ("up refuses the edited migration: " <> show (failed fourth)) (any ("MigrationChecksumMismatch" `isInfixOf`) (failed fourth))
            assertEqual "nothing ran" "3\n" =<< query container "SELECT count() FROM ops.events FORMAT TSV"
  where
    query container sql = do
        (_, out, _) <- podmanExecCapture container ["clickhouse-client", "--query", sql]
        pure out

migrationsDone :: [UpDown.Report Extension] -> [String]
migrationsDone reports = [Text.unpack a.extension.help | UpDown.Done a <- reports, "apply ClickHouse migration " `Text.isPrefixOf` a.extension.help]

failed :: [UpDown.Report Extension] -> [String]
failed reports = [Text.unpack a.extension.help <> ": " <> show e | UpDown.Failed a e <- reports]
