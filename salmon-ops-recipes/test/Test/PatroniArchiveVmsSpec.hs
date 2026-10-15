{-# LANGUAGE OverloadedStrings #-}

{- | T7 of @specs\/pg-patroni.md@ on the three-VM harness ("Test.PatroniVms"):
a Patroni member that has nothing but its configuration is made, by Patroni,
from the pgBackRest archive rather than by a base backup from the leader.

The guests are driven with the pure output of "Salmon.Builtin.Nodes.Patroni"
and "Salmon.Builtin.Nodes.PgBackRest" ('Patroni.renderPatroni',
'renderConf', 'patroniArchive', the argument lists), written over ssh, as
"Test.PgBackRestSpec" does in a container; their 'Op's are not run here. The
one thing the module's code does run is 'Patroni.checkMember', from the host,
against each member's REST address on the bridge.

What is placed where:

* guest 2: one etcd, plain HTTP. The @Etcd@ builtin speaks TLS only and has
  its own Layer 3 spec; T7's "the join phase runs" half is not this test.
* guest 1: the first member. Patroni initialises it, it archives from its
  first segment, and the stanza and a full backup are taken on it.
* guest 3: the member under test, configuration only.

__The repository is copied, not shared.__ The spec wants a repository every
member reads (a shared mount or S3); the guests have neither, so the
repository on guest 1 is copied to the same path on guest 3 (tar through the
host) after the backup, which is what a shared mount would show guest 3 at
that moment. Rows written after the copy reach guest 3 by streaming.

'tests' is Layer 0: the configurations the scenario lays down, and the
properties that make it mean anything (the restore lands in the data
directory Patroni starts, pgBackRest is the first method, nothing secret or
primary in public text). 'vmTests' is Layer 3.
-}
module Test.PatroniArchiveVmsSpec (
    tests,
    vmTests,
    archiveFor,
    memberFor,
    etcdRunArgv,
    patroniRunArgv,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (onException)
import Control.Monad (unless)
import Data.List (isInfixOf, nub)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (addUTCTime, getCurrentTime)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import Test.QuickCheck (Gen, chooseInt, elements, forAll)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.LinuxBridge (Cidr (..))
import qualified Salmon.Builtin.Nodes.Patroni as Patroni
import Salmon.Builtin.Nodes.PgBackRest
import Test.Harness
import Test.PatroniVms

-------------------------------------------------------------------------------
-- What the scenario lays down (pure)

-- | The archive as every member sees it: the same repository path, the cluster @main@ of a version.
archiveFor :: Text -> PgBackRestConfig
archiveFor version =
    PgBackRestConfig
        { pbr_stanza = "t7"
        , pbr_pg_path = "/var/lib/postgresql/" <> Text.unpack version <> "/main"
        , pbr_pg_port = 5432
        , pbr_repo = PosixRepo repoPath
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

-- | Debian's own path, which 'resetGuest' empties.
repoPath :: FilePath
repoPath = "/var/lib/pgbackrest"

{- | One member: a name, the guest's own address, the etcd's address. The
@pg_hba@ admits the bridge's /24, replication included, with passwords.
-}
memberFor :: Text -> Text -> Text -> Text -> Patroni.PatroniConfig
memberFor version name addr etcdAddr =
    Patroni.PatroniConfig
        { Patroni.pat_scope = "t7"
        , Patroni.pat_namespace = "/service/"
        , Patroni.pat_name = name
        , Patroni.pat_etcd_hosts = [etcdAddr <> ":2379"]
        , Patroni.pat_etcd_tls = Nothing
        , Patroni.pat_rest_listen = "0.0.0.0:8008"
        , Patroni.pat_rest_connect_address = addr <> ":8008"
        , Patroni.pat_pg_version = version
        , Patroni.pat_pg_cluster = "main"
        , Patroni.pat_pg_listen = "0.0.0.0:5432"
        , Patroni.pat_pg_connect_address = addr <> ":5432"
        , Patroni.pat_pg_hba =
            [ "local all all peer"
            , "host all all " <> bridgeNet <> " scram-sha-256"
            , "host replication replicator " <> bridgeNet <> " scram-sha-256"
            ]
        , Patroni.pat_bootstrap_parameters = Patroni.defaultBootstrapParameters
        , Patroni.pat_config_dir = "/etc/patroni"
        , Patroni.pat_secrets_file = "/etc/patroni/secrets.yml"
        , Patroni.pat_user = "postgres"
        , Patroni.pat_binary = "/usr/bin/patroni"
        , Patroni.pat_ready_timeout_seconds = 120
        , Patroni.pat_archive = Just (patroniArchive (archiveFor version))
        }
  where
    bridgeNet = testBridgeCidr.cidrAddr `withLastOctet` "0" <> "/24"
    withLastOctet a o = Text.intercalate "." (take 3 (Text.splitOn "." a) <> [o])

{- | Where etcd keeps its data on the guest: @\/dev\/shm@, gone with the
guest. Not under @\/var\/lib\/etcd@, because these guests' root filesystem is
the host's directory over 9p, where bbolt's @mmap@ of its database fails
with @invalid argument@ and etcd panics at start; and not on @\/run@, a tmpfs
of a tenth of 512 MB, smaller than the 64 MB WAL segment etcd preallocates.
-}
etcdDataDir :: FilePath
etcdDataDir = "/dev/shm/salmon-t7-etcd"

{- | etcd as a transient unit (it dies with the guest), plain HTTP, one
member, advertising the guest's address. 'etcdDataDir' must exist first,
owned by @etcd@.
-}
etcdRunArgv :: Text -> [Text]
etcdRunArgv addr =
    [ "systemd-run"
    , "--unit=salmon-t7-etcd"
    , "-p"
    , "User=etcd"
    , "/usr/bin/etcd"
    , "--name=t7"
    , "--data-dir=" <> Text.pack etcdDataDir
    , "--listen-peer-urls=http://127.0.0.1:2380"
    , "--initial-advertise-peer-urls=http://127.0.0.1:2380"
    , "--initial-cluster=t7=http://127.0.0.1:2380"
    , "--listen-client-urls=http://0.0.0.0:2379"
    , "--advertise-client-urls=http://" <> addr <> ":2379"
    ]

{- | Patroni as a transient unit, on the same words as 'Patroni.unitConfig'
(binary, configuration directory, user). @KillMode=process@ as there:
stopping Patroni must not take Postgres with it.
-}
patroniRunArgv :: Patroni.PatroniConfig -> [Text]
patroniRunArgv c =
    [ "systemd-run"
    , patroniUnit
    , "-p"
    , "User=" <> c.pat_user
    , "-p"
    , "KillMode=process"
    , Text.pack c.pat_binary
    , Text.pack c.pat_config_dir
    ]

patroniUnit :: Text
patroniUnit = "--unit=salmon-t7-patroni"

-- | The pre-provisioned fragment. Test-only values; the one place a credential is.
secretsFragment :: String
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

-------------------------------------------------------------------------------
-- Layer 0

genVersion :: Gen Text
genVersion = Text.pack . show <$> chooseInt (13, 19)

genMember :: Gen Patroni.PatroniConfig
genMember = do
    v <- genVersion
    (name, addr) <- elements (zip ["n1", "n2", "n3"] patroniAddrs)
    etcd <- elements patroniAddrs
    pure (memberFor v name addr etcd)

memberVersion :: Patroni.PatroniConfig -> Text
memberVersion = Patroni.pat_pg_version

tests :: TestTree
tests =
    testGroup
        "Patroni T7 on three VMs: what is laid down"
        [ testProperty "the restore lands in the data directory Patroni starts" $
            forAll genMember $ \m ->
                pbr_pg_path (archiveFor (memberVersion m)) == Patroni.pgDataDir m
        , testProperty "pgBackRest is the first replica method, a base backup the fallback" $
            forAll genMember $ \m ->
                let yml = Text.unpack (Patroni.renderPatroni m)
                 in "  create_replica_methods:\n    - \"pgbackrest\"\n    - \"basebackup\"\n" `isInfixOf` yml
                        && Text.unpack (replicaCreateCommand (archiveFor (memberVersion m))) `isInfixOf` yml
        , testProperty "no credential, no primary in patroni.yml or pgbackrest.conf" $
            forAll genMember $ \m ->
                let public = Text.unpack (Patroni.renderPatroni m <> renderConf (archiveFor (memberVersion m)))
                 in not (any (`isInfixOf` public) ["password", "not-a-secret", "primary_conninfo"])
        , testProperty "every member is configured identically but for its name and addresses" $
            forAll genVersion $ \v ->
                let ms = [memberFor v n a (patroniAddrs !! 1) | (n, a) <- zip ["n1", "n2", "n3"] patroniAddrs]
                    strip m =
                        let own = Text.takeWhile (/= ':') (Patroni.pat_rest_connect_address m)
                         in foldr
                                (\(from, to) -> Text.replace from to)
                                (Patroni.renderPatroni m)
                                [(Patroni.pat_name m, "N"), (own <> ":8008", "A:8008"), (own <> ":5432", "A:5432")]
                 in length (nub (fmap strip ms)) == 1
                        && length (nub (fmap Patroni.renderPatroni ms)) == 3
        , testProperty "Patroni runs on its configuration directory, as its user, and keeps Postgres on a stop" $
            forAll genMember $ \m ->
                let argv = patroniRunArgv m
                 in drop (length argv - 2) argv == [Text.pack (Patroni.pat_binary m), Text.pack (Patroni.pat_config_dir m)]
                        && ("User=" <> Patroni.pat_user m) `elem` argv
                        && "KillMode=process" `elem` argv
        , testCase "the guests' addresses are their own on the bridge" $ do
            assertEqual "three distinct" 3 (length (nub patroniAddrs))
            mapM_
                (\a -> assertBool (Text.unpack a) (a `notElem` [testVmAddr, testVmAddr2, testVmAddr3, testVmAddr4]))
                patroniAddrs
            mapM_ (\a -> assertBool (Text.unpack a) ("10.99.0." `Text.isPrefixOf` a)) patroniAddrs
        , testCase "pg_hba admits the bridge, replication included" $ do
            let m = memberFor "17" "n3" (patroniAddrs !! 2) (patroniAddrs !! 1)
            assertBool (show m.pat_pg_hba) ("host replication replicator 10.99.0.0/24 scram-sha-256" `elem` Patroni.pat_pg_hba m)
        , testCase "etcd advertises the guest it runs on, and is reached there" $ do
            let etcd = patroniAddrs !! 1
                m = memberFor "17" "n1" (patroniAddrs !! 0) etcd
            assertBool "" (("--advertise-client-urls=http://" <> etcd <> ":2379") `elem` etcdRunArgv etcd)
            assertEqual "" [etcd <> ":2379"] (Patroni.pat_etcd_hosts m)
        , testCase "the archive's commands name the configuration and the stanza" $ do
            let a = archiveFor "17"
            assertEqual "" "/usr/bin/pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=t7 --delta restore" (replicaCreateCommand a)
            assertEqual "" "/var/lib/postgresql/17/main" (pbr_pg_path a)
            assertBool "" ("repo1-path=/var/lib/pgbackrest\n" `isInfixOf` Text.unpack (renderConf a))
        ]

-------------------------------------------------------------------------------
-- Layer 3

vmTests :: TestTree
vmTests =
    testGroup
        "Patroni T7 (Layer 3, three guests)"
        [ testCase "a member with only its configuration is made by Patroni from the pgBackRest archive" replicaFromArchive
        ]

replicaFromArchive :: IO ()
replicaFromArchive = requirePatroniVmPrereqs $
    withPatroniVms $ \vms -> case vms of
        [vm1, vm2, vm3] -> scenario vm1 vm2 vm3
        _ -> assertFailure ("expected three guests, got " <> show (length vms))

scenario :: VmAccess -> VmAccess -> VmAccess -> IO ()
scenario vm1 vm2 vm3 = do
    version <- Text.pack . takeWhile (/= '\n') <$> guest vm1 "ls /usr/lib/postgresql | sort -n | tail -n1"
    let [addr1, addr2, addr3] = patroniAddrs
        m1 = memberFor version "n1" addr1 addr2
        m3 = memberFor version "n3" addr3 addr2
        a = archiveFor version
        asPostgres args = "runuser -u postgres -- " <> Text.unpack (commandLine a args)
    lay vm1 m1 a
    lay vm3 m3 a

    -- the DCS
    guest_ vm2 ("install -d -o etcd -g etcd -m 0700 " <> etcdDataDir <> " && " <> unwords (Text.unpack <$> etcdRunArgv addr2))
    waitGuest vm2 "etcd" "etcdctl --endpoints=http://127.0.0.1:2379 endpoint health >/dev/null 2>&1 && echo ok" "ok"

    -- the first member: Patroni initialises it, and it archives from its first segment
    guest_ vm1 (unwords (Text.unpack <$> patroniRunArgv m1))
    waitMember vm1 m1
    waitGuest vm1 "archive_mode" (psqlOn "SHOW archive_mode") "on"
    guest_ vm1 (asPostgres (stanzaCreateArgs a))
    guest_ vm1 (asPostgres (backupArgs a Full))
    _ <- guest vm1 (psqlOn "CREATE TABLE canary (v text); INSERT INTO canary VALUES ('archived')")

    -- what a shared mount would show guest 3 now
    pipeBetween vm1 ("tar -C " <> repoPath <> " -cf - .") vm3 ("tar -C " <> repoPath <> " -xpf -")

    -- the member under test: nothing but configuration
    guest_ vm3 (unwords (Text.unpack <$> patroniRunArgv m3))
    waitMember vm3 m3
    logs <- guest vm3 "journalctl -u salmon-t7-patroni --no-pager"
    assertBool
        ("Patroni did not make the replica with pgbackrest:\n" <> logs)
        ("replica has been created using pgbackrest" `isInfixOf` logs)
    waitGuest vm3 "in recovery" (psqlOn "SELECT pg_is_in_recovery()") "t"
    waitGuest vm3 "the row written before" (psqlOn "SELECT string_agg(v, ',') FROM canary") "archived"
    waitGuest vm1 "a streaming standby" (psqlOn "SELECT count(*) FROM pg_stat_replication WHERE state = 'streaming'") "1"
    _ <- guest vm1 (psqlOn "INSERT INTO canary VALUES ('streamed')")
    waitGuest vm3 "a row written after" (psqlOn "SELECT string_agg(v, ',' ORDER BY v) FROM canary") "archived,streamed"

{- | Lays a member's files on its guest: the directories, @patroni.yml@, the
credentials fragment, the @postgresql.conf@ Patroni renames (what
'Patroni.pgConfSeed' does), and the archive's configuration.
-}
lay :: VmAccess -> Patroni.PatroniConfig -> PgBackRestConfig -> IO ()
lay vm m a = do
    guest_ vm . unwords $
        [ "mkdir -p"
        , m.pat_config_dir
        , Patroni.pgConfigDir m
        , takeDirectory a.pbr_config_file
        , repoPath
        , a.pbr_log_path
        , "&& touch " <> Patroni.pgConfigDir m <> "/postgresql.conf"
        , "&& chown -R postgres:postgres"
        , m.pat_config_dir
        , "/etc/postgresql"
        , repoPath
        , a.pbr_log_path
        ]
    writeOn vm (Patroni.patroniFile m) (Text.unpack (Patroni.renderPatroni m))
    writeOn vm m.pat_secrets_file secretsFragment
    writeOn vm a.pbr_config_file (Text.unpack (renderConf a))
    guest_ vm ("chown postgres:postgres " <> Patroni.patroniFile m <> " " <> m.pat_secrets_file <> " && chmod 0600 " <> m.pat_secrets_file)

-- | Polls the module's own check, from the host, until the member is healthy.
waitMember :: VmAccess -> Patroni.PatroniConfig -> IO ()
waitMember vm m = do
    -- a replica restores and replays before it is healthy
    deadline <- addUTCTime 300 <$> getCurrentTime
    let go = do
            v <- Patroni.checkMember m
            now <- getCurrentTime
            case () of
                _
                    | v == Success -> pure ()
                    | now > deadline -> do
                        logs <- guest vm "journalctl -u salmon-t7-patroni --no-pager | tail -n 80"
                        assertFailure ("member " <> Text.unpack m.pat_name <> " never became healthy: " <> show v <> "\n" <> logs)
                    | otherwise -> threadDelay 2000000 >> go
    go

psqlOn :: String -> String
psqlOn q = "runuser -u postgres -- psql -X -p 5432 -tAc " <> quoteForRemoteShell q

-- | Runs a script on a guest as root; a non-zero exit fails the test.
guest :: VmAccess -> String -> IO String
guest vm script = do
    (code, out, err) <- sshToVm vm ["bash", "-c", quoteForRemoteShell script]
    unless (code == ExitSuccess) $ assertFailure (script <> " failed:\n" <> out <> "\n" <> err)
    pure out

guest_ :: VmAccess -> String -> IO ()
guest_ vm script = () <$ guest vm script

{- | Polls a script until its first line of output is what is expected, for
two minutes of wall clock (an ssh round trip to a loaded guest is itself
seconds, so a count of tries says little).
-}
waitGuest :: VmAccess -> String -> String -> String -> IO ()
waitGuest vm what script expected = do
    deadline <- addUTCTime 120 <$> getCurrentTime
    let go lastSeen = do
            now <- getCurrentTime
            if now > deadline
                then do
                    -- whatever ran on this guest says why, more than the last answer does
                    (_, units, _) <- sshToVm vm ["bash", "-c", quoteForRemoteShell "journalctl -u 'salmon-t7-*' --no-pager | tail -n 60"]
                    assertFailure (what <> ": " <> script <> " never answered " <> expected <> "; last: " <> lastSeen <> "\n" <> units)
                else do
                    (code, out, err) <- sshToVm vm ["bash", "-c", quoteForRemoteShell script]
                    let got = takeWhile (/= '\n') out
                    if code == ExitSuccess && got == expected
                        then pure ()
                        else threadDelay 1000000 >> go (got <> err)
    go ""

writeOn :: VmAccess -> FilePath -> String -> IO ()
writeOn vm path contents =
    withSystemTempDirectory "salmon-t7" $ \dir -> do
        let local = dir </> "f"
        writeFile local contents
        scpToVm vm local path

{- | @src@'s stdout into @dst@'s stdin, through the host: two ssh processes and
one pipe, neither inheriting the test's stdin.
-}
pipeBetween :: VmAccess -> String -> VmAccess -> String -> IO ()
pipeBetween src srcScript dst dstScript = do
    (Just srcIn, Just out, _, ph1) <-
        createProcess (proc "ssh" (sshArgv src ["bash", "-c", quoteForRemoteShell srcScript])){std_in = CreatePipe, std_out = CreatePipe}
    hClose srcIn
    -- createProcess closes a handle it is given with UseHandle, in this process
    (_, _, _, ph2) <-
        createProcess (proc "ssh" (sshArgv dst ["bash", "-c", quoteForRemoteShell dstScript])){std_in = UseHandle out}
            `onException` (hClose out >> terminateProcess ph1)
    c1 <- waitForProcess ph1
    c2 <- waitForProcess ph2
    unless (c1 == ExitSuccess && c2 == ExitSuccess) $
        assertFailure ("copying " <> srcScript <> " -> " <> dstScript <> " failed: " <> show (c1, c2))
