{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.PgBackRest" at two layers.

Layer 0 ('tests'): the configuration, the command lines, what Patroni is
handed, and the verdicts drawn from @pgbackrest info --output=json@.

Layer 2 ('sandboxTests'): the same rendered words run for real in a disposable
Debian container holding a Postgres and a pgBackRest. It shows what Layer 0
cannot: that the archive they describe restores to a point in time, and that a
second data directory built from it comes up as a streaming standby. The
filesystem nodes write on the machine salmon runs on, so the container is
driven with the module's pure output rather than through its 'Op's; and there
is no Patroni in it, so "Patroni builds a replica from the archive" is /not/
what this shows -- it shows the command Patroni is handed doing that job.
-}
module Test.PgBackRestSpec (tests, sandboxTests) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Builtin.Nodes.CronTask as Cron
import qualified Salmon.Builtin.Nodes.Patroni as Patroni
import Salmon.Builtin.Nodes.PgBackRest
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Test.Harness (podmanExec_, requireExecutable, withContainer)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.PgBackRest"
        [ testCase "the configuration names the repository, the retention and the cluster" $ do
            has "[global]\nrepo1-type=posix\nrepo1-path=/srv/archive\nrepo1-retention-full=2\nlog-path=/var/log/pgbackrest\n"
            has "[demo]\npg1-path=/var/lib/postgresql/16/main\npg1-port=5432\n"
        , testCase "an S3 repository is rendered without any key" $ do
            let out = Text.unpack (renderConf s3cfg)
            assertBool out ("repo1-type=s3\nrepo1-path=/pgbackrest\nrepo1-s3-bucket=wal\nrepo1-s3-endpoint=s3.example.net\nrepo1-s3-region=eu\nrepo1-s3-uri-style=path\n" `isInfixOf` out)
            assertBool out (not ("key" `isInfixOf` out))
        , testCase "extra options are passed through, in their section" $ do
            let out = Text.unpack (renderConf cfg{pbr_global = [("compress-type", "zst")], pbr_stanza_options = [("pg1-socket-path", "/run/pg")]})
            assertBool out ("compress-type=zst\n\n[demo]" `isInfixOf` out)
            assertBool out ("pg1-port=5432\npg1-socket-path=/run/pg\n" `isInfixOf` out)
        , testCase "every command names the configuration, the include directory and the stanza" $ do
            assertEqual "" ["--config=/etc/pgbackrest/pgbackrest.conf", "--stanza=demo", "stanza-create"] (stanzaCreateArgs cfg)
            assertEqual
                ""
                ["--config=/etc/pgbackrest/pgbackrest.conf", "--config-include-path=/etc/pgbackrest/conf.d", "--stanza=demo", "--type=incr", "backup"]
                (backupArgs s3cfg Incremental)
        , testCase "archive and restore commands leave %p and %f to Postgres" $ do
            assertEqual "" "/usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=demo archive-push %p" (archiveCommand cfg)
            assertEqual "" "/usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=demo archive-get %f \"%p\"" (restoreCommand cfg)
            assertEqual "" [("archive_mode", "on"), ("archive_command", archiveCommand cfg)] (archiveParameters cfg)
        , testCase "a restore never carries --delta or --force, whatever the target" $
            mapM_
                (\t -> assertBool (show t) (not (any (`elem` ["--delta", "--force"]) (restoreArgs cfg t))))
                [Latest, Immediate, AtTime "2026-10-02 14:03:00+00", AtLsn "0/3000000", AtXid "740", AtName "before-migration"]
        , testCase "a point-in-time restore names its target and promotes" $ do
            assertEqual "" (commonArgs cfg <> ["restore"]) (restoreArgs cfg Latest)
            assertEqual
                ""
                (commonArgs cfg <> ["--type=time", "--target=2026-10-02 14:03:00+00", "--target-action=promote", "restore"])
                (restoreArgs cfg (AtTime "2026-10-02 14:03:00+00"))
            assertEqual "" (commonArgs cfg <> ["--type=immediate", "--target-action=promote", "restore"]) (restoreArgs cfg Immediate)
        , testCase "a command line quotes what a shell would split" $ do
            assertEqual "" "'2026-10-02 14:03:00+00'" (shellWord "2026-10-02 14:03:00+00")
            assertEqual "" "'it'\\''s'" (shellWord "it's")
            assertEqual "" "--stanza=demo" (shellWord "--stanza=demo")
            assertBool "" ("'--target=2026-10-02 14:03:00+00'" `isInfixOf` Text.unpack (commandLine cfg (restoreArgs cfg (AtTime "2026-10-02 14:03:00+00"))))
        , testCase "sudo is used only when a user is declared" $ do
            assertBool "" (not ("sudo" `isInfixOf` show (process cfg (infoArgs cfg))))
            assertBool "" ("\"sudo\" [\"-u\",\"postgres\",\"/usr/bin/pgbackrest\"" `isInfixOf` show (process cfg{pbr_run_as = Just "postgres"} (infoArgs cfg)))
        , testCase "Patroni is handed a delta restore first and a base backup as the fallback" $ do
            let yml = Text.unpack (Patroni.renderPatroni (patroniCfg (Just (patroniArchive cfg))))
                hasY s = assertBool (s <> " in:\n" <> yml) (s `isInfixOf` yml)
            hasY "  create_replica_methods:\n    - \"pgbackrest\"\n    - \"basebackup\"\n"
            hasY "  pgbackrest:\n    command: \"/usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=demo --delta restore\"\n    keep_data: true\n    no_params: true\n"
            hasY "  recovery_conf:\n    restore_command: \"/usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=demo archive-get %f \\\"%p\\\"\"\n"
            hasY "  parameters:\n    archive_mode: \"on\"\n    archive_command: \"/usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=demo archive-push %p\"\n"
            assertBool "no custom bootstrap unless asked" (not ("method:" `isInfixOf` yml))
        , testCase "without an archive, patroni.yml says nothing about one" $ do
            let yml = Text.unpack (Patroni.renderPatroni (patroniCfg Nothing))
            assertBool yml (not ("create_replica_methods" `isInfixOf` yml))
            assertBool yml (not ("archive" `isInfixOf` yml))
        , testCase "a recovering cluster bootstraps from the archive, to the target" $ do
            let yml = Text.unpack (Patroni.renderPatroni (patroniCfg (Just (patroniArchiveRecovering cfg (AtName "before-migration")))))
            assertBool yml ("  method: \"pgbackrest\"\n  pgbackrest:\n    command: \"/usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=demo --type=name --target=before-migration --target-action=promote restore\"\n    keep_existing_recovery_conf: true\n    no_params: true\n" `isInfixOf` yml)
        , testCase "an archive never puts a credential or a primary in patroni.yml" $ do
            let yml = Text.unpack (Patroni.renderPatroni (patroniCfg (Just (patroniArchive s3cfg))))
            mapM_ (\w -> assertBool w (not (w `isInfixOf` yml))) ["password", "s3-key", "primary_conninfo"]
        , testCase "info: a stanza with a backup satisfies both checks" $ do
            assertEqual "" Success (interpretStanza "demo" infoOk)
            assertEqual "" Success (interpretHasBackup "demo" infoOk)
            assertEqual
                ""
                (Right [StanzaInfo "demo" 0 "ok" [BackupInfo "20261002-140000F" "full" (Just 1790949615)]])
                (parseInfo infoOk)
        , testCase "info: a stanza without a backup exists, and has no backup" $ do
            assertEqual "" Success (interpretStanza "demo" infoNoBackup)
            assertBool "" (isFailure (interpretHasBackup "demo" infoNoBackup))
        , testCase "info: a missing stanza fails both, naming pgBackRest's reason" $ do
            case interpretStanza "demo" infoMissing of
                Failure why -> assertBool (Text.unpack why) ("missing stanza path" `isInfixOf` Text.unpack why)
                other -> assertFailure (show other)
            assertBool "" (isFailure (interpretHasBackup "demo" infoMissing))
        , testCase "info: another stanza's backups do not count" $ do
            assertBool "" (isFailure (interpretStanza "other" infoOk))
            assertBool "" (isFailure (interpretHasBackup "other" infoOk))
            assertBool "" (isFailure (interpretStanza "demo" "[]"))
        , testCase "info: garbage is a Failure, not an exception" $
            assertBool "" (isFailure (interpretStanza "demo" "nope"))
        , testCase "the scheduled command gates itself, and exits 0 when it is not this member's turn" $ do
            let s = BackupSchedule "nightly" Incremental (Cron.dailyAt "3" "17") "/usr/local/bin/pgbackrest-nightly.sh" (Just "http://127.0.0.1:8008/primary")
            assertEqual
                ""
                "#!/bin/bash\nset -euo pipefail\ncurl -sf -o /dev/null http://127.0.0.1:8008/primary || exit 0\nexec /usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=demo --type=incr backup\n"
                (backupScript cfg s)
            assertBool "" (not ("curl" `isInfixOf` Text.unpack (backupScript cfg s{bs_gate_url = Nothing})))
        ]
  where
    rendered = Text.unpack (renderConf cfg)
    has s = assertBool (s <> " in:\n" <> rendered) (s `isInfixOf` rendered)

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

cfg :: PgBackRestConfig
cfg =
    PgBackRestConfig
        { pbr_stanza = "demo"
        , pbr_pg_path = "/var/lib/postgresql/16/main"
        , pbr_pg_port = 5432
        , pbr_repo = PosixRepo "/srv/archive"
        , pbr_retention_full = 2
        , pbr_global = []
        , pbr_stanza_options = []
        , pbr_config_file = "/etc/pgbackrest/pgbackrest.conf"
        , pbr_secrets_file = Nothing
        , pbr_log_path = "/var/log/pgbackrest"
        , pbr_user = "postgres"
        , pbr_run_as = Nothing
        , pbr_binary = "/usr/bin/pgbackrest"
        }

s3cfg :: PgBackRestConfig
s3cfg =
    cfg
        { pbr_repo = S3Repo "wal" "s3.example.net" "eu" "/pgbackrest" True
        , pbr_secrets_file = Just "/etc/pgbackrest/conf.d/secrets.conf"
        }

patroniCfg :: Maybe Patroni.Archive -> Patroni.PatroniConfig
patroniCfg archive =
    Patroni.PatroniConfig
        { Patroni.pat_scope = "demo"
        , Patroni.pat_namespace = "/service/"
        , Patroni.pat_name = "n2"
        , Patroni.pat_etcd_hosts = ["10.0.0.1:2379"]
        , Patroni.pat_etcd_tls = Nothing
        , Patroni.pat_rest_listen = "0.0.0.0:8008"
        , Patroni.pat_rest_connect_address = "10.0.0.2:8008"
        , Patroni.pat_pg_version = "16"
        , Patroni.pat_pg_cluster = "main"
        , Patroni.pat_pg_listen = "0.0.0.0:5432"
        , Patroni.pat_pg_connect_address = "10.0.0.2:5432"
        , Patroni.pat_pg_hba = []
        , Patroni.pat_bootstrap_parameters = Patroni.defaultBootstrapParameters
        , Patroni.pat_config_dir = "/etc/patroni"
        , Patroni.pat_secrets_file = "/etc/patroni/secrets.yml"
        , Patroni.pat_user = "postgres"
        , Patroni.pat_binary = "/usr/bin/patroni"
        , Patroni.pat_ready_timeout_seconds = 60
        , Patroni.pat_archive = archive
        }

-- The shapes below are pgBackRest 2.x's @info --output=json@, cut down to what is read plus a few fields that are not.
infoOk, infoNoBackup, infoMissing :: ByteString
infoOk =
    "[{\"archive\":[{\"database\":{\"id\":1},\"id\":\"16-1\",\"max\":\"000000010000000000000005\",\"min\":\"000000010000000000000002\"}],\"backup\":[{\"label\":\"20261002-140000F\",\"type\":\"full\",\"timestamp\":{\"start\":1790949600,\"stop\":1790949615}}],\"name\":\"demo\",\"status\":{\"code\":0,\"lock\":{\"backup\":{\"held\":false}},\"message\":\"ok\"}}]"
infoNoBackup =
    "[{\"archive\":[],\"backup\":[],\"name\":\"demo\",\"status\":{\"code\":2,\"message\":\"no valid backups\"}}]"
infoMissing =
    "[{\"archive\":[],\"backup\":[],\"name\":\"demo\",\"status\":{\"code\":1,\"message\":\"missing stanza path\"}}]"

-------------------------------------------------------------------------------

sandboxTests :: TestTree
sandboxTests =
    testGroup
        "Salmon.Builtin.Nodes.PgBackRest (Layer 2, a real archive in a podman sandbox)"
        [ testCase "archives, restores to a point in time, and builds a standby from the archive" archiveRoundTrip
        , testCase "Patroni builds its second member from the archive" patroniReplicaFromArchive
        ]

{- | Two Patroni members and one etcd in one container, no systemd: each
member is started on the @patroni.yml@ 'Patroni.renderPatroni' renders with
'patroniArchive' declared. The second member has nothing but its
configuration, and must come up as a replica that Patroni made with
pgBackRest -- which its own log is asked about, since a base backup from the
leader would produce the same replica and prove nothing.
-}
patroniReplicaFromArchive :: IO ()
patroniReplicaFromArchive = requireExecutable "podman" $
    withContainer (Podman.Image "debian:trixie") (Podman.PortMapping "15440" "5432" Podman.TCPPort) $ \cid -> do
        podmanExec_ cid ["apt-get", "update", "-qq"]
        podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq postgresql pgbackrest patroni python3-etcd etcd-server curl"]
        version <- head . lines <$> sh cid "root" "ls /usr/lib/postgresql | sort -n | tail -n1"
        -- Debian's own cluster would hold port 5432 and is nobody's here
        sh_ cid "root" ("pg_dropcluster --stop " <> version <> " main || true")
        let archiveOf name port =
                cfg
                    { pbr_pg_path = "/var/lib/postgresql/" <> version <> "/" <> name
                    , pbr_pg_port = port
                    , pbr_repo = PosixRepo "/var/lib/pgbackrest-test"
                    , pbr_log_path = "/var/log/pgbackrest-test"
                    , pbr_config_file = "/etc/pgbackrest-test/" <> name <> ".conf"
                    }
            member name port rest =
                (patroniCfg (Just (patroniArchive (archiveOf name port))))
                    { Patroni.pat_name = Text.pack name
                    , Patroni.pat_etcd_hosts = ["127.0.0.1:2379"]
                    , Patroni.pat_rest_listen = "127.0.0.1:" <> Text.pack (show rest)
                    , Patroni.pat_rest_connect_address = "127.0.0.1:" <> Text.pack (show rest)
                    , Patroni.pat_pg_version = Text.pack version
                    , Patroni.pat_pg_cluster = Text.pack name
                    , Patroni.pat_pg_listen = "127.0.0.1:" <> Text.pack (show port)
                    , Patroni.pat_pg_connect_address = "127.0.0.1:" <> Text.pack (show port)
                    , Patroni.pat_pg_hba = ["local all all peer", "host all all 127.0.0.1/32 scram-sha-256", "host replication replicator 127.0.0.1/32 scram-sha-256"]
                    , Patroni.pat_config_dir = "/etc/patroni-" <> name
                    , Patroni.pat_secrets_file = "/etc/patroni-" <> name <> "/secrets.yml"
                    }
            lay name port rest = do
                let m = member name port rest
                sh_ cid "root" ("mkdir -p " <> Patroni.pat_config_dir m <> " " <> Patroni.pgConfigDir m <> " && touch " <> Patroni.pgConfigDir m <> "/postgresql.conf && chown -R postgres " <> Patroni.pgConfigDir m)
                write cid (Patroni.patroniFile m) (Text.unpack (Patroni.renderPatroni m))
                -- the pre-provisioned fragment: the one place a credential is
                write cid (Patroni.pat_secrets_file m) secretsFragment
                write cid (pbr_config_file (archiveOf name port)) (Text.unpack (renderConf (archiveOf name port)))
            start name =
                sh_ cid "postgres" ("nohup patroni /etc/patroni-" <> name <> " > /tmp/patroni-" <> name <> ".log 2>&1 &")
            verdict rest = do
                (_, health, _) <- argv cid "postgres" ["curl", "-s", "-o", "/dev/null", "-w", "%{http_code}", "http://127.0.0.1:" <> show rest <> "/health"]
                (_, body, _) <- argv cid "postgres" ["curl", "-s", "http://127.0.0.1:" <> show rest <> "/patroni"]
                pure (Patroni.interpretMember "demo" (if all (`elem` ['0' .. '9']) health && not (null health) then read health else 0) (C8.pack body), body)
            waitMember name rest = go (90 :: Int)
              where
                go 0 = do
                    (v, body) <- verdict rest
                    logs <- sh cid "root" ("tail -n 60 /tmp/patroni-" <> name <> ".log")
                    assertFailure ("member " <> name <> " never became healthy: " <> show v <> "\n" <> body <> "\n" <> logs)
                go n = do
                    (v, _) <- verdict rest
                    unless (v == Success) (threadDelay 1000000 >> go (n - 1))
            a = archiveOf "a" 5432
            pgbackrest args = do
                (code, out, err) <- argv cid "postgres" (pbr_binary a : fmap Text.unpack args)
                assertEqual (show args <> " failed:\n" <> out <> "\n" <> err) ExitSuccess code

        sh_ cid "root" "mkdir -p /etc/pgbackrest-test /var/lib/pgbackrest-test /var/log/pgbackrest-test /var/lib/etcd-test && chown postgres /var/lib/pgbackrest-test /var/log/pgbackrest-test /var/lib/etcd-test"
        sh_ cid "postgres" "nohup etcd --data-dir /var/lib/etcd-test > /tmp/etcd.log 2>&1 &"
        lay "a" 5432 8008
        lay "b" 5433 8009

        -- the first member: Patroni initialises it, and it archives from its first segment
        start "a"
        waitMember "a" 8008
        waitFor cid 5432 "SHOW archive_mode" "on"
        pgbackrest (stanzaCreateArgs a)
        pgbackrest (backupArgs a Full)
        _ <- psql cid 5432 "CREATE TABLE canary (v text); INSERT INTO canary VALUES ('archived')"

        -- the second: nothing but configuration
        start "b"
        waitMember "b" 8009
        logB <- sh cid "root" "cat /tmp/patroni-b.log"
        assertBool ("Patroni did not make the replica with pgbackrest:\n" <> logB) ("replica has been created using pgbackrest" `isInfixOf` logB)
        waitFor cid 5433 "SELECT pg_is_in_recovery()" "t"
        waitFor cid 5433 "SELECT string_agg(v, ',') FROM canary" "archived"
        waitFor cid 5432 "SELECT count(*) FROM pg_stat_replication WHERE state = 'streaming'" "1"
  where
    secretsFragment =
        unlines
            [ "postgresql:"
            , "  authentication:"
            , "    superuser:"
            , "      username: postgres"
            , "      password: not-a-secret-1"
            , "    replication:"
            , "      username: replicator"
            , "      password: not-a-secret-2"
            ]

archiveRoundTrip :: IO ()
archiveRoundTrip = requireExecutable "podman" $
    withContainer (Podman.Image "debian:trixie") (Podman.PortMapping "15439" "5432" Podman.TCPPort) $ \cid -> do
        podmanExec_ cid ["apt-get", "update", "-qq"]
        podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq postgresql pgbackrest"]
        version <- head . lines <$> sh cid "root" "ls /usr/lib/postgresql | sort -n | tail -n1"
        let c =
                cfg
                    { pbr_pg_path = "/var/lib/postgresql/" <> version <> "/main"
                    , pbr_repo = PosixRepo "/var/lib/pgbackrest-test"
                    , pbr_log_path = "/var/log/pgbackrest-test"
                    , pbr_config_file = "/etc/pgbackrest-test/pgbackrest.conf"
                    }
            -- the same archive seen from a second data directory: what a second member's configuration is
            c2 = c{pbr_pg_path = "/var/lib/postgresql/" <> version <> "/standby", pbr_pg_port = 5433, pbr_config_file = "/etc/pgbackrest-test/standby.conf"}
            pgbackrest = argv cid "postgres" . (pbr_binary c :) . fmap Text.unpack
            ctl cluster verb = sh_ cid "postgres" ("pg_ctlcluster " <> version <> " " <> cluster <> " " <> verb)
            info = do
                (code, out, err) <- pgbackrest (infoArgs c)
                assertEqual ("info failed: " <> err) ExitSuccess code
                pure (C8.pack out)
            run_ what args = do
                (code, out, err) <- pgbackrest args
                assertEqual (what <> " failed:\n" <> out <> "\n" <> err) ExitSuccess code

        -- what `configuration` would lay down, and what `archiveParameters` asks of Postgres
        sh_ cid "root" "mkdir -p /etc/pgbackrest-test /var/lib/pgbackrest-test /var/log/pgbackrest-test && chown postgres /var/lib/pgbackrest-test /var/log/pgbackrest-test"
        write cid (pbr_config_file c) (Text.unpack (renderConf c))
        write cid (pbr_config_file c2) (Text.unpack (renderConf c2))
        write
            cid
            ("/etc/postgresql/" <> version <> "/main/conf.d/archive.conf")
            (unlines [Text.unpack k <> " = '" <> Text.unpack v <> "'" | (k, v) <- archiveParameters c])
        sh_ cid "postgres" ("pg_ctlcluster " <> version <> " main restart || pg_ctlcluster " <> version <> " main start")
        waitFor cid 5432 "SELECT 1" "1"

        -- the stanza and the first backup, with the checks read before and after
        before <- info
        assertBool ("before stanza-create: " <> show before) (isFailure (interpretStanza "demo" before))
        run_ "stanza-create" (stanzaCreateArgs c)
        created <- info
        assertEqual (show created) Success (interpretStanza "demo" created)
        assertBool (show created) (isFailure (interpretHasBackup "demo" created))
        run_ "backup" (backupArgs c Full)
        backedUp <- info
        assertEqual (show backedUp) Success (interpretHasBackup "demo" backedUp)

        -- one row before the target, one after; both archived
        _ <- psql cid 5432 "CREATE TABLE canary (v text); INSERT INTO canary VALUES ('before')"
        threadDelay 1500000
        target <- psql cid 5432 "SELECT now()"
        threadDelay 1500000
        _ <- psql cid 5432 "INSERT INTO canary VALUES ('after')"
        segment <- psql cid 5432 "SELECT pg_walfile_name(pg_current_wal_lsn())"
        _ <- psql cid 5432 "SELECT pg_switch_wal()"
        waitFor cid 5432 ("SELECT last_archived_wal >= '" <> segment <> "' FROM pg_stat_archiver") "t"

        -- the guard: a data directory that holds a cluster is refused
        ctl "main" "stop"
        (refused, _, refusedErr) <- pgbackrest (restoreArgs c (AtTime (Text.pack target)))
        assertBool ("a restore over a live data directory was not refused: " <> refusedErr) (refused /= ExitSuccess)

        -- the disaster, and the recovery to the target
        sh_ cid "postgres" ("find " <> pbr_pg_path c <> " -mindepth 1 -delete")
        run_ "restore" (restoreArgs c (AtTime (Text.pack target)))
        ctl "main" "start"
        waitFor cid 5432 "SELECT pg_is_in_recovery()" "f"
        rows <- psql cid 5432 "SELECT string_agg(v, ',') FROM canary"
        assertEqual "the cluster is not at the target" "before" rows

        -- a second data directory made by the command Patroni is handed, started as a standby of the first
        run_ "backup after recovery" (backupArgs c Incremental)
        sh_ cid "root" ("pg_createcluster " <> version <> " standby -p 5433")
        (code, out, err) <- argv cid "postgres" ["bash", "-c", Text.unpack (replicaCreateCommand c2)]
        assertEqual ("replica creation failed:\n" <> out <> "\n" <> err) ExitSuccess code
        -- what Patroni writes after the method succeeds
        sh_ cid "postgres" ("touch " <> pbr_pg_path c2 <> "/standby.signal")
        write
            cid
            ("/etc/postgresql/" <> version <> "/standby/conf.d/standby.conf")
            (unlines ["primary_conninfo = 'host=/var/run/postgresql port=5432 user=postgres'", "restore_command = '" <> Text.unpack (restoreCommand c2) <> "'"])
        ctl "standby" "start"
        waitFor cid 5433 "SELECT pg_is_in_recovery()" "t"
        _ <- psql cid 5432 "INSERT INTO canary VALUES ('later')"
        waitFor cid 5433 "SELECT string_agg(v, ',') FROM canary" "before,later"
        waitFor cid 5432 "SELECT count(*) FROM pg_stat_replication WHERE state = 'streaming'" "1"

-- | Runs an argv in the container as a user.
argv :: String -> String -> [String] -> IO (ExitCode, String, String)
argv cid user args = readProcessWithExitCode "podman" (["exec", "-i", "--user", user, cid] <> args) ""

sh :: String -> String -> String -> IO String
sh cid user script = do
    (code, out, err) <- argv cid user ["bash", "-c", script]
    unless (code == ExitSuccess) $ assertFailure (script <> " failed:\n" <> out <> "\n" <> err)
    pure out

sh_ :: String -> String -> String -> IO ()
sh_ cid user script = () <$ sh cid user script

write :: String -> FilePath -> String -> IO ()
write cid path contents = do
    (code, _, err) <- readProcessWithExitCode "podman" ["exec", "-i", cid, "bash", "-c", "cat > " <> path] contents
    unless (code == ExitSuccess) $ assertFailure ("writing " <> path <> " failed: " <> err)

psql :: String -> Int -> String -> IO String
psql cid port q = do
    (code, out, err) <- argv cid "postgres" ["psql", "-p", show port, "-tAc", q]
    unless (code == ExitSuccess) $ assertFailure (q <> " failed:\n" <> err)
    pure (concat (take 1 (reverse (filter (not . null) (lines out)))))

-- | Polls a query until it answers what is expected, 60 x 1s; a server that is still coming up is not an error yet.
waitFor :: String -> Int -> String -> String -> IO ()
waitFor cid port q expected = go (60 :: Int) ""
  where
    go 0 lastSeen = assertFailure (q <> " on port " <> show port <> " never answered " <> expected <> "; last: " <> lastSeen)
    go n _ = do
        (code, out, err) <- argv cid "postgres" ["psql", "-p", show port, "-tAc", q]
        let got = concat (take 1 (lines out))
        if code == ExitSuccess && got == expected
            then pure ()
            else threadDelay 1000000 >> go (n - 1) (got <> err)
