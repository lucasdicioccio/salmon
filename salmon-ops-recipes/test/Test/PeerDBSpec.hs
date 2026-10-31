{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.PeerDB", "Salmon.Builtin.Nodes.Postgres.Logical"
and 'ClickHouse.sourceReadGrant': CDC from PostgreSQL into ClickHouse.

Layer 0: the SQL the Postgres nodes send, the bodies sent to PeerDB's flow
API (and that the password is in the peer's body only), and the readings of
what PostgreSQL and PeerDB answer, on answers copied from PeerDB
@stable-v0.37.12@.

Layer 2 ('sandboxTests', opt-in with @SALMON_TEST_PEERDB=1@): PeerDB's
services as upstream's compose file runs them at @stable-v0.37.12@ (catalog,
Temporal, the object store, @flow-api@ and both workers; not the UI nor the
pgwire server), a PostgreSQL 17 source and a ClickHouse 25.8 target, all on
one podman network, started by plain @podman run@ here. The nodes run
against them: the Postgres ones as the @postgres@ OS user through a @sudo@
shim into the source container.
-}
module Test.PeerDBSpec (tests, sandboxTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket_, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.PeerDB as PeerDB
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Postgres.Logical as Logical
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.OpGraph (OpGraph (..), inject)

import Salmon.Reporter (silent)
import Test.Harness

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.PeerDB and Postgres.Logical (Layer 0)"
        [ testGroup "Postgres.Logical" logicalTests
        , testGroup "PeerDB" peerdbTests
        , testCase "ClickHouse: READ ON S3, and its check" $ do
            assertEqual "" "GRANT READ ON S3 TO `peer`;\n" (ClickHouse.sourceReadGrantSql "s3" "peer")
            assertEqual "a source name is upper-cased" "GRANT READ ON URL TO `peer`;\n" (ClickHouse.sourceReadGrantSql "url" "peer")
            assertEqual "an odd source name is not markup" "GRANT READ ON S3DROP TO `peer`;\n" (ClickHouse.sourceReadGrantSql "s3; DROP" "peer")
            assertBool "" ("access_object = 'S3'" `Text.isInfixOf` ClickHouse.sourceReadGrantedSql "s3" "peer")
        ]

pub :: Logical.Publication
pub = Logical.Publication "cdc_pub" "app" [Logical.publicTable "orders", Logical.QualifiedTable "Sales" "line \"items\""]

logicalTests :: [TestTree]
logicalTests =
    [ testCase "wal_level: set, reloaded; the check reads the files, then the running value" $ do
        assertEqual "" "ALTER SYSTEM SET wal_level = 'logical';\nSELECT pg_reload_conf();\n" Logical.walLevelSql
        assertEqual "" Success (Logical.interpretWalLevel "logical\n")
        assertEqual "" (Failure "wal_level is replica, logical decoding needs logical") (Logical.interpretWalLevel "replica")
        assertEqual "" Unknown (Logical.interpretWalLevel "")
    , testCase "the wal_level node is on alterSystemSet's effect site" $ do
        let mine = nodeOf (Logical.logicalWalLevel silent ignoreTrack 5432)
            theirs = nodeOf (Postgres.alterSystemSet silent ignoreTrack 5432 "wal_level" "replica")
        assertEqual "" theirs.ref mine.ref
    , testCase "REPLICATION: the statement and the verdicts" $ do
        assertEqual "" "ALTER ROLE \"cdc\" WITH REPLICATION;\n" (Logical.replicationRoleSql "cdc")
        assertEqual "" Success (Logical.interpretReplicationRole "cdc" "t\n")
        assertEqual "" (Failure "role cdc may not replicate") (Logical.interpretReplicationRole "cdc" "f")
        assertEqual "" (Failure "role cdc does not exist") (Logical.interpretReplicationRole "cdc" "")
    , testCase "a publication: refusal of an unmarked one, create via gexec, set table, marker, one transaction" $ do
        let sql = Logical.publicationSql pub
            ls = Text.lines sql
        assertEqual "first and last" (["BEGIN;"], ["COMMIT;"]) (take 1 ls, drop (length ls - 1) ls)
        forM_
            [ "NOT LIKE 'salmon-publication:%'"
            , "RAISE EXCEPTION"
            , "SELECT 'CREATE PUBLICATION \"cdc_pub\" FOR TABLE \"public\".\"orders\", \"Sales\".\"line \"\"items\"\"\"' WHERE NOT EXISTS (SELECT 1 FROM pg_publication WHERE pubname = 'cdc_pub') \\gexec"
            , "ALTER PUBLICATION \"cdc_pub\" SET TABLE \"public\".\"orders\", \"Sales\".\"line \"\"items\"\"\";"
            , "COMMENT ON PUBLICATION \"cdc_pub\" IS 'salmon-publication: app';"
            ]
            $ \frag -> assertBool (Text.unpack frag <> " in\n" <> Text.unpack sql) (frag `Text.isInfixOf` sql)
    , testCase "a publication's verdicts" $ do
        let marker = "present:salmon-publication: app"
            tables = "Sales.line \"items\"\npublic.orders\n"
        assertEqual "" Success (Logical.interpretPublication pub marker tables)
        assertBool "absent" (isFailure (Logical.interpretPublication pub "" ""))
        assertEqual
            "unmarked"
            (Failure "publication cdc_pub exists but salmon did not make it")
            (Logical.interpretPublication pub "present:" tables)
        assertEqual
            "other tables"
            (Failure "publication cdc_pub publishes public.orders, declared Sales.line \"items\", public.orders")
            (Logical.interpretPublication pub marker "public.orders")
    , testCase "a logical slot: refused when the name is taken by another kind, created when missing" $ do
        let sql = Logical.logicalSlotSql "app" "cdc_slot"
        assertBool (Text.unpack sql) ("pg_create_logical_replication_slot('cdc_slot', 'pgoutput') WHERE NOT EXISTS" `Text.isInfixOf` sql)
        assertBool (Text.unpack sql) ("slot_type = 'logical' AND plugin = 'pgoutput' AND database = 'app'" `Text.isInfixOf` sql)
        assertEqual "" Success (Logical.interpretSlot "app" "cdc_slot" "logical|pgoutput|app\n")
        assertEqual "" (Failure "replication slot cdc_slot does not exist") (Logical.interpretSlot "app" "cdc_slot" "")
        assertBool "physical" (isFailure (Logical.interpretSlot "app" "cdc_slot" "physical||"))
    , testCase "the slot node is on the physical slot node's effect site" $
        assertEqual "" (nodeOf (Postgres.replicationSlot silent ignoreTrack 5432 "s")).ref (nodeOf (Logical.logicalSlot silent ignoreTrack 5432 "app" "s")).ref
    , testCase "nothing here has a down" $
        forM_
            [ Logical.logicalWalLevel silent ignoreTrack 5432
            , Logical.replicationRole silent ignoreTrack 5432 "cdc"
            , Logical.publication silent ignoreTrack 5432 pub
            , Logical.logicalSlot silent ignoreTrack 5432 "app" "s"
            ]
            $ \o -> do
                ok <- runDown o
                assertBool "down does nothing and succeeds" ok
    ]

srcPeer :: PeerDB.Peer
srcPeer = PeerDB.Peer "pg_source" (PeerDB.PostgresSource (PeerDB.PostgresPeer "db.internal" 5432 "cdc" "app" False))

dstPeer :: PeerDB.Peer
dstPeer = PeerDB.Peer "ch_target" (PeerDB.ClickHouseTarget (PeerDB.ClickHousePeer "ch.internal" 9000 "peer" "analytics" True))

theMirror :: PeerDB.Mirror
theMirror =
    (PeerDB.mirror "orders_to_ch" "pg_source" "ch_target" [PeerDB.TableMapping "public.orders" "orders"])
        { PeerDB.mirrorPublication = Just "cdc_pub"
        , PeerDB.mirrorIdleTimeoutSeconds = Just 5
        }

-- | What @GET /v1/peers/info/pg_source@ answered at stable-v0.37.12, with this test's names.
peerInfoAnswer :: Text -> LChar8.ByteString
peerInfoAnswer host =
    "{\"peer\":{\"name\":\"pg_source\",\"type\":\"POSTGRES\",\"postgresConfig\":{\"host\":\""
        <> LChar8.pack (Text.unpack host)
        <> "\",\"port\":5432,\"user\":\"cdc\",\"password\":\"********\",\"database\":\"app\",\"tlsHost\":\"\",\"requireTls\":false,\"authType\":\"POSTGRES_PASSWORD\",\"skipCertVerification\":false}},\"version\":\"PostgreSQL 17.11\"}"

-- | What @POST /v1/mirrors/status@ answered, trimmed, with this test's names.
mirrorStatusAnswer :: LChar8.ByteString -> LChar8.ByteString -> LChar8.ByteString
mirrorStatusAnswer tables state =
    "{\"flowJobName\":\"orders_to_ch\",\"cdcStatus\":{\"config\":{\"flowJobName\":\"orders_to_ch\",\"tableMappings\":["
        <> tables
        <> "],\"publicationName\":\"cdc_pub\",\"sourceName\":\"pg_source\",\"destinationName\":\"ch_target\"},\"sourceType\":\"POSTGRES\",\"destinationType\":\"CLICKHOUSE\",\"rowsSynced\":\"0\"},\"currentFlowState\":\""
        <> state
        <> "\",\"createdAt\":\"2026-10-31T07:13:44.731934Z\"}"

ordersMapping :: LChar8.ByteString
ordersMapping = "{\"sourceTableIdentifier\":\"public.orders\",\"destinationTableIdentifier\":\"orders\",\"engine\":\"CH_ENGINE_REPLACING_MERGE_TREE\"}"

peerdbTests :: [TestTree]
peerdbTests =
    [ testCase "a peer's body: the type, the config under its field, allow_update; the password only there" $ do
        let body = Aeson.encode (PeerDB.createPeerBody dstPeer (PeerDB.Password "s3cr3t"))
            s = LChar8.unpack body
        forM_ ["\"type\":\"CLICKHOUSE\"", "\"clickhouse_config\":{", "\"disable_tls\":true", "\"allow_update\":true", "\"password\":\"s3cr3t\""] $ \frag ->
            assertBool (frag <> " in " <> s) (frag `isInfixOf` s)
        let o = nodeOf (PeerDB.peer silent ignoreTrack (PeerDB.Api "http://127.0.0.1:8113") (FS.PreExisting "/run/secrets/ch-peer") dstPeer)
        assertBool "not in help" (not ("s3cr3t" `Text.isInfixOf` o.help))
        assertBool "the file, in help" ("/run/secrets/ch-peer" `Text.isInfixOf` o.help)
    , testCase "a peer read back: same is Success, a moved host is a Failure naming it" $ do
        assertEqual "" (Just Success) (PeerDB.interpretPeerInfo srcPeer <$> Aeson.decode (peerInfoAnswer "db.internal"))
        assertEqual
            ""
            (Just (Failure "PeerDB peer pg_source differs: host is elsewhere, declared db.internal"))
            (PeerDB.interpretPeerInfo srcPeer <$> Aeson.decode (peerInfoAnswer "elsewhere"))
        assertBool "a ClickHouse declaration read on a Postgres peer" $
            maybe False isFailure (PeerDB.interpretPeerInfo dstPeer{PeerDB.peerName = "pg_source"} <$> Aeson.decode (peerInfoAnswer "db.internal"))
    , testCase "a mirror's body: names, mappings, publication, idle timeout; no slot when none is named" $ do
        let s = LChar8.unpack (Aeson.encode (PeerDB.createMirrorBody theMirror))
        forM_
            [ "\"flow_job_name\":\"orders_to_ch\""
            , "\"source_table_identifier\":\"public.orders\""
            , "\"destination_table_identifier\":\"orders\""
            , "\"publication_name\":\"cdc_pub\""
            , "\"idle_timeout_seconds\":5"
            , "\"do_initial_snapshot\":true"
            ]
            $ \frag -> assertBool (frag <> " in " <> s) (frag `isInfixOf` s)
        assertBool s (not ("replication_slot_name" `isInfixOf` s))
    , testCase "a mirror's status read against the declaration" $ do
        let verdict tables st = PeerDB.interpretMirrorStatus theMirror <$> Aeson.decode (mirrorStatusAnswer tables st)
        assertEqual "running" (Just Success) (verdict ordersMapping "STATUS_RUNNING")
        assertEqual "paused by an operator" (Just Success) (verdict ordersMapping "STATUS_PAUSED")
        assertEqual "snapshotting" (Just Unknown) (verdict ordersMapping "STATUS_SNAPSHOT")
        assertEqual "failed" (Just (Failure "PeerDB mirror orders_to_ch is STATUS_FAILED")) (verdict ordersMapping "STATUS_FAILED")
        assertEqual
            "other tables"
            (Just (Failure "PeerDB mirror orders_to_ch differs: tables are none"))
            (verdict "" "STATUS_RUNNING")
    , testCase "a declared slot with an initial snapshot is refused; without one, or with PeerDB's own slot, it is not" $ do
        assertEqual "" [] (PeerDB.mirrorProblems theMirror)
        assertEqual "" [] (PeerDB.mirrorProblems theMirror{PeerDB.mirrorSlot = Just "s", PeerDB.mirrorInitialSnapshot = False})
        assertEqual "" 1 (length (PeerDB.mirrorProblems theMirror{PeerDB.mirrorSlot = Just "s"}))
        assertEqual "" 1 (length (PeerDB.mirrorProblems theMirror{PeerDB.mirrorTables = []}))
    , testCase "names in a path are escaped" $
        assertEqual "" "a%2Fb%20c" (PeerDB.pathSegment "a/b c")
    , testCase "an error's message is kept, the password cut out" $
        assertEqual
            ""
            ": failed for pw <redacted>"
            (PeerDB.errorMessage (PeerDB.redact ["hunter2"] "{\"code\":9,\"message\":\"failed for pw hunter2\"}"))
    ]

nodeOf :: Op -> Extension
nodeOf o = case o.node of
    Actions a -> a.extension
    Actionless -> error "an op without actions"

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

-------------------------------------------------------------------------------
-- Layer 2

sandboxTests :: TestTree
sandboxTests =
    testGroup
        "Salmon.Builtin.Nodes.PeerDB (Layer 2, PeerDB stable-v0.37.12, PostgreSQL 17 and ClickHouse 25.8 under podman)"
        [ testCase "prerequisites, peers and a mirror converge, rows stream, a second pass applies nothing" sandbox
        ]

peerdbTag :: String
peerdbTag = "stable-v0.37.12"

sandbox :: IO ()
sandbox = requireExecutable "podman" $ do
    enabled <- lookupEnv "SALMON_TEST_PEERDB"
    if enabled /= Just "1"
        then hPutStrLn stderr "SKIPPED: SALMON_TEST_PEERDB is not 1; this Layer 2 test pulls and runs PeerDB's services (several GB)"
        else withTempDir $ \dir -> do
            Podman.ContainerName stem <- freshContainerName
            apiPort <- freeTcpPort
            let net = Text.unpack stem
                name s = net <> "-" <> s
            bracket_ (pure ()) (teardown net name) $ do
                stack dir net name apiPort
                converge dir net name apiPort

{- | Upstream's compose file, by hand: names are prefixed, the object store's
endpoint is its container, the API is published on a loopback port.
-}
stack :: FilePath -> String -> (String -> String) -> Text -> IO ()
stack dir net name apiPort = do
    let pgConf = dir </> "catalog.conf"
        dynconf = dir </> "dynamicconfig"
    writeFile pgConf "listen_addresses = '*'\nwal_level = logical\nmax_wal_senders = 4\nmax_replication_slots = 4\nmax_connections = 2048\n"
    void (readProcessWithExitCode "mkdir" ["-p", dynconf] "")
    writeFile (dynconf </> "development-sql.yaml") "limit.maxIDLength:\n  - value: 255\n    constraints: {}\nsystem.forceSearchAttributesCacheRefreshOnRead:\n  - value: true\n    constraints: {}\n"
    podman ["network", "create", net]
    let catalog = ["-e", "PEERDB_CATALOG_HOST=" <> name "catalog", "-e", "PEERDB_CATALOG_PORT=5432", "-e", "PEERDB_CATALOG_USER=postgres", "-e", "PEERDB_CATALOG_PASSWORD=postgres", "-e", "PEERDB_CATALOG_DATABASE=postgres"]
        s3 =
            [ "-e"
            , "PEERDB_CLICKHOUSE_AWS_CREDENTIALS_AWS_ACCESS_KEY_ID=_peerdb_minioadmin"
            , "-e"
            , "PEERDB_CLICKHOUSE_AWS_CREDENTIALS_AWS_SECRET_ACCESS_KEY=_peerdb_minioadmin"
            , "-e"
            , "PEERDB_CLICKHOUSE_AWS_CREDENTIALS_AWS_REGION=us-east-1"
            , "-e"
            , "PEERDB_CLICKHOUSE_AWS_CREDENTIALS_AWS_ENDPOINT_URL_S3=http://" <> name "minio" <> ":9000"
            , "-e"
            , "PEERDB_CLICKHOUSE_AWS_S3_BUCKET_NAME=peerdbbucket"
            ]
        flow = ["-e", "TEMPORAL_HOST_PORT=" <> name "temporal" <> ":7233", "-e", "PEERDB_TEMPORAL_NAMESPACE=default"]
        detached n args = podman (["run", "-d", "--name", name n, "--network", net] <> args)
    detached "catalog" ["-e", "POSTGRES_PASSWORD=postgres", "-v", pgConf <> ":/etc/postgresql.conf:ro", "docker.io/library/postgres:17-alpine", "-c", "config_file=/etc/postgresql.conf"]
    waitUntil "the catalog" 60 (execOk (name "catalog") ["pg_isready", "-U", "postgres"])
    detached
        "minio"
        [ "-e"
        , "MINIO_ROOT_USER=_peerdb_minioadmin"
        , "-e"
        , "MINIO_ROOT_PASSWORD=_peerdb_minioadmin"
        , "--entrypoint"
        , "/bin/sh"
        , "docker.io/pgsty/silo:RELEASE.2026-09-16T00-00-00Z"
        , "-c"
        , "silo server /data & sleep 3; mc alias set m http://localhost:9000 _peerdb_minioadmin _peerdb_minioadmin; mc mb m/peerdbbucket; wait"
        ]
    detached
        "temporal"
        [ "-e"
        , "DB=postgres12"
        , "-e"
        , "DB_PORT=5432"
        , "-e"
        , "POSTGRES_USER=postgres"
        , "-e"
        , "POSTGRES_PWD=postgres"
        , "-e"
        , "POSTGRES_SEEDS=" <> name "catalog"
        , "-e"
        , "DYNAMIC_CONFIG_FILE_PATH=config/dynamicconfig/development-sql.yaml"
        , "-v"
        , dynconf <> ":/etc/temporal/config/dynamicconfig:ro"
        , "docker.io/temporalio/auto-setup:1.29"
        ]
    podman (["run", "--rm", "--network", net, "--entrypoint", "/home/peerdb/peer-flow"] <> catalog <> ["ghcr.io/peerdb-io/flow-api:" <> peerdbTag, "migrate"])
    -- what upstream's admin-tools container does at start
    waitUntil "Temporal and its MirrorName attribute" 180 $
        ok
            "podman"
            [ "run"
            , "--rm"
            , "--network"
            , net
            , "-e"
            , "TEMPORAL_ADDRESS=" <> name "temporal" <> ":7233"
            , "--entrypoint"
            , "/bin/sh"
            , "docker.io/temporalio/admin-tools:1.25.2-tctl-1.18.1-cli-1.1.1"
            , "-c"
            , "temporal operator search-attribute list | grep -qw MirrorName || temporal operator search-attribute create --name MirrorName --type Text --namespace default"
            ]
    detached "flow-api" (["-p", "127.0.0.1:" <> Text.unpack apiPort <> ":8113"] <> catalog <> flow <> s3 <> ["ghcr.io/peerdb-io/flow-api:" <> peerdbTag])
    detached "flow-worker" (catalog <> flow <> s3 <> ["ghcr.io/peerdb-io/flow-worker:" <> peerdbTag])
    detached "flow-snapshot-worker" (catalog <> flow <> s3 <> ["ghcr.io/peerdb-io/flow-snapshot-worker:" <> peerdbTag])
    -- the source: PostgreSQL's own default wal_level (replica), so the node has something to do
    detached "source" ["-e", "POSTGRES_PASSWORD=unused-admin", "docker.io/library/postgres:17-alpine"]
    let adminXml = dir </> "salmon-admin.xml"
    writeFile adminXml (Text.unpack ClickHouse.renderAdminAccess)
    detached "ch" ["-v", adminXml <> ":/etc/clickhouse-server/users.d/salmon-admin.xml:ro", "docker.io/clickhouse/clickhouse-server:25.8"]
    waitUntil "the source" 60 (execOk (name "source") ["pg_isready", "-U", "postgres"])
    waitUntil "ClickHouse" 60 (execOk (name "ch") ["clickhouse-client", "--query", "SELECT 1"])
    waitUntil "the flow API" 120 (ok "curl" ["-sf", "http://127.0.0.1:" <> Text.unpack apiPort <> "/v1/version"])

converge :: FilePath -> String -> (String -> String) -> Text -> IO ()
converge dir net name apiPort = do
    let src = name "source"
        ch = name "ch"
        pgPass = dir </> "cdc.pass"
        chPass = dir </> "peer.pass"
        api = PeerDB.Api ("http://127.0.0.1:" <> apiPort)
        chClient = ClickHouse.Client (ClickHouse.InContainer (Podman.ContainerName (Text.pack ch))) ClickHouse.defaultTcpPort
    writeFile pgPass "cdc-password"
    writeFile chPass "peer-password"
    let walLevel = Logical.logicalWalLevel silent ignoreTrack 5432
    withSudoShim src $ do
        -- wal_level first, alone: it needs the restart that pg_ctlcluster
        -- would do on Debian, done here by podman
        assertEqual "wal_level starts at replica" (Failure "wal_level is replica, logical decoding needs logical") =<< (nodeOf walLevel).check
        okWal <- runUp walLevel
        assertBool "wal_level written" okWal
        assertEqual "the wal_level check reads the files" Success =<< (nodeOf walLevel).check
    podman ["restart", src]
    waitUntil "the source after its restart" 60 (execOk src ["pg_isready", "-U", "postgres"])
    psqlOut src "SHOW wal_level" >>= assertEqual "running wal_level" "logical"
    -- peers name their hosts by address, taken after the restart (which
    -- changes it): podman's resolver has been seen to time out for a
    -- container a few seconds old, and PeerDB refuses a peer it cannot reach
    srcIp <- addressOf net src
    chIp <- addressOf net ch
    psql src "CREATE TABLE orders (id int PRIMARY KEY, item text); INSERT INTO orders VALUES (1, 'apple'), (2, 'pear')"
    let pgUser = Postgres.userPassFile silent ignoreTrack ignoreTrack 5432 (FS.PreExisting pgPass) (Postgres.User "cdc")
        role = Logical.replicationRole silent ignoreTrack 5432 "cdc" `inject` pgUser
        publication = Logical.publication silent ignoreTrack 5432 (Logical.Publication "cdc_pub" "postgres" [Logical.publicTable "orders"])
        slot = Logical.logicalSlot silent ignoreTrack 5432 "postgres" "cdc_slot" `inject` walLevel
        chDb = ClickHouse.database silent ignoreTrack chClient "analytics"
        chUser = ClickHouse.userPassFile silent ignoreTrack chClient (FS.PreExisting chPass) "peer"
        chGrant = (ClickHouse.grant silent ignoreTrack chClient (ClickHouse.Grant ["SELECT", "INSERT", "CREATE TABLE", "ALTER", "DROP TABLE", "TRUNCATE", "OPTIMIZE"] "analytics" "peer") `inject` chUser) `inject` chDb
        chS3 = ClickHouse.sourceReadGrant silent ignoreTrack chClient "S3" "peer" `inject` chUser
        source = PeerDB.Peer "pg_source" (PeerDB.PostgresSource (PeerDB.PostgresPeer srcIp 5432 "cdc" "postgres" False))
        target = PeerDB.Peer "ch_target" (PeerDB.ClickHouseTarget (PeerDB.ClickHousePeer chIp 9000 "peer" "analytics" True))
        srcPeerOp = (PeerDB.peer silent ignoreTrack api (FS.PreExisting pgPass) source `inject` role) `inject` walLevel
        dstPeerOp = (PeerDB.peer silent ignoreTrack api (FS.PreExisting chPass) target `inject` chGrant) `inject` chS3
        -- PeerDB's own slot, and the snapshot it exports with it
        snapshotting =
            (PeerDB.mirror "orders_snapshot" "pg_source" "ch_target" [PeerDB.TableMapping "public.orders" "orders"])
                { PeerDB.mirrorPublication = Just "cdc_pub"
                , PeerDB.mirrorIdleTimeoutSeconds = Just 3
                }
        -- the declared slot: no snapshot (PeerDB refuses one on an existing slot)
        declared =
            (PeerDB.mirror "orders_from_slot" "pg_source" "ch_target" [PeerDB.TableMapping "public.orders" "orders_cdc"])
                { PeerDB.mirrorPublication = Just "cdc_pub"
                , PeerDB.mirrorSlot = Just "cdc_slot"
                , PeerDB.mirrorInitialSnapshot = False
                , PeerDB.mirrorIdleTimeoutSeconds = Just 3
                }
        withPeers m = ((PeerDB.mirrorOp silent ignoreTrack api m `inject` srcPeerOp) `inject` dstPeerOp) `inject` publication
        snapshotOp = withPeers snapshotting
        mirrorOp = withPeers declared `inject` slot
        graph = op "peerdb-test" (deps [snapshotOp, mirrorOp]) id
    withSudoShim src $ do
        okRole <- runUp role
        assertBool "the CDC role, with REPLICATION" okRole
    -- what the declaration does not hold: the CDC role reads the table, and
    -- may create PeerDB's metadata schema in the database
    psql src "GRANT SELECT ON orders TO cdc; GRANT CREATE ON DATABASE postgres TO cdc"
    withSudoShim src $ do
        first <- runUpCapturing graph
        assertBool ("first pass: " <> show (failures first)) (null (failures first))
        forM_ [("wal_level", walLevel), ("role", role), ("publication", publication), ("slot", slot), ("ch database", chDb), ("ch user", chUser), ("ch grant", chGrant), ("ch s3", chS3), ("source peer", srcPeerOp), ("target peer", dstPeerOp)] $ \(what, o) ->
            assertEqual what Success =<< (nodeOf o).check
        forM_ [("orders_snapshot", snapshotOp), ("orders_from_slot", mirrorOp)] $ \(mname, o) ->
            waitForCheck ("mirror " <> mname) 180 (nodeOf o).check $ do
                (_, out, _) <-
                    readProcessWithExitCode
                        "curl"
                        ["-s", "-X", "POST", "-d", "{\"flow_job_name\":\"" <> mname <> "\",\"exclude_batches\":true}", "http://127.0.0.1:" <> Text.unpack apiPort <> "/v1/mirrors/status"]
                        ""
                pure ("PeerDB says: " <> take 3000 out)
    -- the initial snapshot on one, nothing on the other; then a change streamed to both
    waitUntil "the snapshot in ClickHouse" 120 ((== "1:apple,2:pear") <$> chRows ch "orders")
    psql src "INSERT INTO orders VALUES (3, 'plum'); UPDATE orders SET item = 'quince' WHERE id = 2"
    waitUntil "the change through PeerDB's slot" 120 ((== "1:apple,2:quince,3:plum") <$> chRows ch "orders")
    waitUntil "the change through the declared slot" 120 ((== "2:quince,3:plum") <$> chRows ch "orders_cdc")
    psqlOut src "SELECT active FROM pg_replication_slots WHERE slot_name = 'cdc_slot'" >>= assertEqual "PeerDB reads the declared slot" "t"
    psqlOut src "SELECT string_agg(slot_name, ',' ORDER BY slot_name) FROM pg_replication_slots" >>= assertEqual "and PeerDB made one for the other mirror" "cdc_slot,peerflow_slot_orders_snapshot"
    -- a declared slot with a snapshot is refused before PeerDB is asked
    withSudoShim src $ do
        let bad = declared{PeerDB.mirrorName = "orders_bad", PeerDB.mirrorInitialSnapshot = True}
            badOp = PeerDB.mirrorOp silent ignoreTrack api bad
        refusedBad <- runUp badOp
        assertBool "a declared slot with a snapshot is refused" (not refusedBad)
        assertEqual "and PeerDB was not asked" (Failure "PeerDB has no mirror orders_bad") =<< (nodeOf badOp).check
    withSudoShim src $ do
        second <- runUpCapturing graph
        -- what runs again is what has no check: the root and the two
        -- pre-existing password files' placeholders
        reapplied <- sequence [(\r -> (Text.unpack a.shorthand, r)) <$> a.extension.check | UpDown.Done a <- second]
        assertEqual "the second pass applies nothing that has a check" [] [x | x@(_, r) <- reapplied, r /= Immaterial]
        -- a mirror declared with another table: a Failure, and up refuses
        let other = declared{PeerDB.mirrorTables = [PeerDB.TableMapping "public.orders" "orders", PeerDB.TableMapping "public.other" "other"]}
            otherOp = PeerDB.mirrorOp silent ignoreTrack api other
        otherCheck <- (nodeOf otherOp).check
        assertBool ("another table set is a Failure: " <> show otherCheck) (isFailure otherCheck)
        refused <- runUp otherOp
        assertBool "up refuses to change a mirror" (not refused)
        -- a publication salmon did not make is left alone
        psql src "CREATE PUBLICATION by_hand FOR TABLE orders"
        let byHand = Logical.publication silent ignoreTrack 5432 (Logical.Publication "by_hand" "postgres" [Logical.publicTable "orders"])
        assertEqual "unmarked" (Failure "publication by_hand exists but salmon did not make it") =<< (nodeOf byHand).check
        refusedPub <- runUp byHand
        assertBool "up refuses an unmarked publication" (not refusedPub)
        -- down takes nothing away
        downOk <- runDown graph
        assertBool "down succeeds" downOk
    assertEqual "the mirrors still run" [Success, Success] =<< mapM (\o -> (nodeOf o).check) [snapshotOp, mirrorOp]
    psqlOut src "SELECT count(*) FROM pg_publication WHERE pubname IN ('cdc_pub', 'by_hand')" >>= assertEqual "both publications remain" "2"
    psqlOut src "SELECT obj_description(oid, 'pg_publication') IS NULL FROM pg_publication WHERE pubname = 'by_hand'" >>= assertEqual "the hand-made one is unmarked still" "t"
  where
    failures :: [UpDown.Report Extension] -> [String]
    failures reports = [Text.unpack a.extension.help <> ": " <> show e | UpDown.Failed a e <- reports]

-------------------------------------------------------------------------------

teardown :: String -> (String -> String) -> IO ()
teardown net name = do
    forM_ ["flow-snapshot-worker", "flow-worker", "flow-api", "temporal", "minio", "catalog", "source", "ch"] $ \n ->
        void (readProcessWithExitCode "podman" ["rm", "-f", "-t", "2", name n] "")
    void (readProcessWithExitCode "podman" ["network", "rm", net] "")

podman :: [String] -> IO ()
podman args = do
    (code, out, err) <- readProcessWithExitCode "podman" args ""
    unless (code == ExitSuccess) $ assertFailure ("podman " <> unwords (take 6 args) <> " failed:\n" <> out <> err)

ok :: String -> [String] -> IO Bool
ok exe args = do
    r <- try (readProcessWithExitCode exe args "") :: IO (Either SomeException (ExitCode, String, String))
    pure (either (const False) (\(c, _, _) -> c == ExitSuccess) r)

execOk :: String -> [String] -> IO Bool
execOk cid args = ok "podman" (["exec", cid] <> args)

waitUntil :: String -> Int -> IO Bool -> IO ()
waitUntil what seconds probe = go seconds
  where
    go n = do
        done <- probe
        unless done $ do
            when (n <= 0) $ assertFailure (what <> " not ready in " <> show seconds <> "s")
            threadDelay 1000000
            go (n - 1)

psql :: String -> String -> IO ()
psql cid sql = podman ["exec", "-u", "postgres", cid, "psql", "-v", "ON_ERROR_STOP=1", "-c", sql]

-- | A container's address on a network.
addressOf :: String -> String -> IO Text
addressOf net cid = do
    (code, out, err) <- readProcessWithExitCode "podman" ["inspect", "-f", "{{(index .NetworkSettings.Networks \"" <> net <> "\").IPAddress}}", cid] ""
    unless (code == ExitSuccess && not (null (words out))) $ assertFailure ("no address for " <> cid <> ": " <> err)
    pure (Text.strip (Text.pack out))

-- | @id:item@ rows of a mirrored table, deduplicated (@FINAL@), deleted rows left out.
chRows :: String -> String -> IO String
chRows cid table = do
    (_, out, _) <-
        readProcessWithExitCode
            "podman"
            ["exec", cid, "clickhouse-client", "--query", "SELECT arrayStringConcat(groupArray(concat(toString(id), ':', item)), ',') FROM (SELECT id, item FROM analytics." <> table <> " FINAL WHERE _peerdb_is_deleted = 0 ORDER BY id)"]
            ""
    pure (concat (lines out))

psqlOut :: String -> String -> IO String
psqlOut cid sql = do
    (_, out, _) <- readProcessWithExitCode "podman" ["exec", "-u", "postgres", cid, "psql", "-tAc", sql] ""
    pure (concat (lines out))

{- | @sudo -u USER CMD ...@ as @podman exec -i -u USER CONTAINER CMD ...@: the
Postgres nodes run @sudo -u postgres psql@, and the official image has no
@sudo@.
-}
withSudoShim :: String -> IO a -> IO a
withSudoShim cid act =
    withTempDir $ \dir -> do
        writeExecutable (dir </> "sudo") $
            unlines
                [ "#!/bin/bash"
                , "u=root"
                , "if [ \"$1\" = \"-u\" ]; then u=\"$2\"; shift 2; fi"
                , "exec podman exec -i -u \"$u\" " <> cid <> " \"$@\""
                ]
        withPrependedPath dir act

-- | Waits for a check to read 'Success', and says what it read last when it does not.
waitForCheck :: String -> Int -> IO CheckResult -> IO String -> IO ()
waitForCheck what seconds check diagnose = go seconds
  where
    go n = do
        r <- check
        unless (r == Success) $ do
            when (n <= 0) $ do
                d <- diagnose
                assertFailure (what <> " not Success in " <> show seconds <> "s, last read " <> show r <> "\n" <> d)
            threadDelay 1000000
            go (n - 1)
