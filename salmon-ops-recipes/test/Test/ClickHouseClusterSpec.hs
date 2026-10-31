{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.ClickHouse.Cluster".

Layer 0: the Keeper and cluster drop-ins, their refusals, a server's quadlet
with and without them, the SQL, and the readings of what the server
answers.

Layer 2, under rootless podman, several containers on one fresh network,
each started with the arguments its quadlet would give it (binds, env file,
network; not the data directory):

* 'liveCluster': three servers of the official image, each embedding a
  Keeper member, in one cluster of two shards (two replicas, then one), with
  a shared secret and interserver credentials from an env file. The quorum,
  the Keeper sessions, @system.clusters@, a @Replicated@ database on every
  member and an @ON CLUSTER@ one, twice (the second pass applies nothing),
  then a replicated table's rows reaching the other replica.
* 'liveStandaloneKeeper': three members of the official Keeper image: a lone
  member answers nothing (no client port before a quorum), then all three do.

The quadlets themselves are Layer 0 only here: they are system-scope units.
-}
module Test.ClickHouseClusterSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM, forM_, void)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import Salmon.Builtin.Nodes.ClickHouse.Cluster (Cluster (..), ClusterConfig (..), Distribution (..), Keeper (..), KeeperNode (..), KeeperPeer (..), Replica (..), Shard (..))
import qualified Salmon.Builtin.Nodes.ClickHouse.Cluster as Cluster
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Readiness as Readiness
import Salmon.Op.Actions (Act (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)
import Test.Harness

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.ClickHouse.Cluster"
        [ testGroup "Layer 0" layer0
        , testCase "Layer 2: three servers with embedded Keeper, two shards, replicated DDL, under podman" liveCluster
        , testCase "Layer 2: a standalone Keeper ensemble of three under podman" liveStandaloneKeeper
        ]

peers3 :: [KeeperPeer]
peers3 = Cluster.keeperPeersFor ["ch1", "ch2", "ch3"]

mainCluster :: Cluster
mainCluster =
    (Cluster.cluster "main" [Cluster.shardOf ["ch1", "ch2"], Cluster.shardOf ["ch3"]])
        { clusterSecretEnv = Just "CH_CLUSTER_SECRET"
        }

has :: Text -> [Text] -> IO ()
has out needles = forM_ needles $ \n -> assertBool (Text.unpack n <> " in\n" <> Text.unpack out) (n `Text.isInfixOf` out)

layer0 :: [TestTree]
layer0 =
    [ testCase "a Keeper member renders its id, port and every peer, the Raft list replacing the packaged one" $
        assertEqual
            ""
            ( Text.unlines
                [ "<clickhouse>"
                , "    <keeper_server>"
                , "        <tcp_port>9181</tcp_port>"
                , "        <server_id>2</server_id>"
                , "        <raft_configuration replace=\"replace\">"
                , "            <server>"
                , "                <id>1</id>"
                , "                <hostname>ch1</hostname>"
                , "                <port>9234</port>"
                , "            </server>"
                , "            <server>"
                , "                <id>2</id>"
                , "                <hostname>ch2</hostname>"
                , "                <port>9234</port>"
                , "            </server>"
                , "            <server>"
                , "                <id>3</id>"
                , "                <hostname>ch3</hostname>"
                , "                <port>9234</port>"
                , "            </server>"
                , "        </raft_configuration>"
                , "    </keeper_server>"
                , "</clickhouse>"
                ]
            )
            (Cluster.renderKeeperConfig (Cluster.keeperMember peers3 2))
    , testCase "optional Keeper parts render when set, escaped" $ do
        let k =
                (Cluster.keeperMember peers3 1)
                    { keeperListenHosts = ["0.0.0.0"]
                    , keeperLogStoragePath = Just "/var/lib/clickhouse/coordination/log"
                    , keeperCoordinationSettings = [("raft_logs_level", "a<b")]
                    }
        has
            (Cluster.renderKeeperConfig k)
            [ "    <listen_host>0.0.0.0</listen_host>"
            , "<log_storage_path>/var/lib/clickhouse/coordination/log</log_storage_path>"
            , "        <coordination_settings>\n            <raft_logs_level>a&lt;b</raft_logs_level>\n        </coordination_settings>"
            ]
    , testCase "a Keeper member that cannot be is refused" $ do
        assertEqual "fine" [] (Cluster.keeperProblems (Cluster.keeperMember peers3 3))
        assertEqual "not a peer" 1 (length (Cluster.keeperProblems (Cluster.keeperMember peers3 4)))
        assertEqual "no peers" 1 (length (Cluster.keeperProblems (Cluster.keeperMember [] 1)))
        let dup = [KeeperPeer 1 "a" 9234, KeeperPeer 1 "b" 9234, KeeperPeer 0 "a" 9234]
        -- id 1 twice, id 0 not positive, a:9234 twice
        assertEqual "duplicates" 3 (length (Cluster.keeperProblems (Cluster.keeperMember dup 1)))
        assertEqual "setting name" 1 (length (Cluster.keeperProblems (Cluster.keeperMember peers3 1){keeperCoordinationSettings = [("a b", "1")]}))
    , testCase "the default cluster drop-in is the root element and nothing else" $
        assertEqual "" "<clickhouse>\n</clickhouse>\n" (Cluster.renderClusterConfig Cluster.defaultClusterConfig)
    , testCase "a member's cluster drop-in: topology, secret by name, macros, Keeper, interserver" $ do
        let cfg =
                (Cluster.memberConfig mainCluster (Cluster.keeperNodesFor peers3) "01" "ch2" "ch2")
                    { clusterInterserverCredentials = Just (Cluster.InterserverCredentials "salmon-interserver" "CH_INTERSERVER_PASSWORD")
                    }
            out = Cluster.renderClusterConfig cfg
        assertEqual "no problems" [] (Cluster.clusterConfigProblems cfg)
        has
            out
            [ "    <interserver_http_host>ch2</interserver_http_host>"
            , "    <interserver_http_credentials>\n        <user>salmon-interserver</user>\n        <password from_env=\"CH_INTERSERVER_PASSWORD\" />\n    </interserver_http_credentials>"
            , "    <remote_servers>\n        <main>\n            <secret from_env=\"CH_CLUSTER_SECRET\" />\n            <shard>\n                <internal_replication>true</internal_replication>\n                <replica>\n                    <host>ch1</host>\n                    <port>9000</port>\n                </replica>\n                <replica>\n                    <host>ch2</host>"
            , "            <shard>\n                <internal_replication>true</internal_replication>\n                <replica>\n                    <host>ch3</host>"
            , "    <macros>\n        <cluster>main</cluster>\n        <shard>01</shard>\n        <replica>ch2</replica>\n    </macros>"
            , "    <zookeeper replace=\"replace\">\n        <node>\n            <host>ch1</host>\n            <port>9181</port>\n        </node>"
            ]
        assertBool "no root unless asked" (not ("<root>" `Text.isInfixOf` out))
    , testCase "a cluster drop-in that cannot be is refused" $ do
        let bad =
                Cluster.defaultClusterConfig
                    { clusterRemoteServers =
                        [ Cluster.cluster "9main" [Cluster.shardOf ["a"]]
                        , (Cluster.cluster "x" [Cluster.shardOf ["a", "a"], Shard [] Nothing True]){clusterSecretEnv = Just "lower"}
                        , Cluster.cluster "x" [Cluster.shardOf ["b c"]]
                        , Cluster.cluster "empty" []
                        ]
                    , clusterMacros = [("<shard>", "1")]
                    , clusterKeeperRoot = Just "relative"
                    }
        -- 9main; x twice; empty has no shard; x has an empty shard; a twice
        -- in x; "b c"; the macro; the env name; the root
        assertEqual (show (Cluster.clusterConfigProblems bad)) 9 (length (Cluster.clusterConfigProblems bad))
    , testCase "a server without drop-ins, env file or network is the quadlet it was" $ do
        let s = ClickHouse.server (Podman.ContainerName "ch") "docker.io/clickhouse/clickhouse-server:25.8" "/etc/salmon/ch" "/var/lib/salmon/ch"
            out = Quadlet.renderContainer (ClickHouse.serverContainer s)
        assertEqual
            ""
            ( Text.unlines
                [ "[Unit]"
                , "Description=ClickHouse ch (salmon)"
                , ""
                , "[Container]"
                , "ContainerName=ch"
                , "Image=docker.io/clickhouse/clickhouse-server:25.8"
                , "Volume=/var/lib/salmon/ch:/var/lib/clickhouse:rw"
                , "Volume=/etc/salmon/ch/salmon-server.xml:/etc/clickhouse-server/config.d/salmon-server.xml:ro"
                , "Volume=/etc/salmon/ch/salmon-admin.xml:/etc/clickhouse-server/users.d/salmon-admin.xml:ro"
                , ""
                , "[Service]"
                , "Restart=on-failure"
                , ""
                , "[Install]"
                , "WantedBy=multi-user.target"
                ]
            )
            out
    , testCase "a cluster member's quadlet binds and watches its drop-ins, reads the env file, joins the network" $ do
        let s =
                (ClickHouse.server (Podman.ContainerName "ch2") "docker.io/clickhouse/clickhouse-server:25.8" "/etc/salmon/ch2" "/var/lib/salmon/ch2")
                    { ClickHouse.serverDropIns =
                        [ Cluster.keeperDropIn (Cluster.keeperMember peers3 2)
                        , Cluster.clusterDropIn (Cluster.memberConfig mainCluster (Cluster.keeperNodesFor peers3) "01" "ch2" "ch2")
                        ]
                    , ClickHouse.serverEnvFile = Just "/etc/salmon/ch2.env"
                    , ClickHouse.serverNetwork = Just "chnet"
                    }
            c = ClickHouse.serverContainer s
            out = Quadlet.renderContainer c
        assertEqual "no problems" [] (Quadlet.containerProblems c)
        has
            out
            [ "EnvironmentFile=/etc/salmon/ch2.env"
            , "Network=chnet"
            , "Volume=/etc/salmon/ch2/salmon-keeper.xml:/etc/clickhouse-server/config.d/salmon-keeper.xml:ro"
            , "Volume=/etc/salmon/ch2/salmon-cluster.xml:/etc/clickhouse-server/config.d/salmon-cluster.xml:ro"
            ]
        assertEqual
            ""
            ["/etc/salmon/ch2.env", "/etc/salmon/ch2/salmon-server.xml", "/etc/salmon/ch2/salmon-admin.xml", "/etc/salmon/ch2/salmon-keeper.xml", "/etc/salmon/ch2/salmon-cluster.xml"]
            (Quadlet.watchedFiles c)
    , testCase "a drop-in's name must be a plain .xml file salmon does not already write" $ do
        let named n = ClickHouse.DropIn n "<clickhouse/>" []
        assertEqual "" [] (ClickHouse.dropInNameProblems (named "salmon-keeper.xml"))
        forM_ ["../evil.xml", "x.conf", "salmon-server.xml", ".xml", "a b.xml"] $ \n ->
            assertBool n (not (null (ClickHouse.dropInNameProblems (named n))))
    , testCase "a standalone Keeper listens on every address, binds its drop-in into keeper_config.d, waits for nothing" $ do
        let ks = Cluster.keeperServer (Podman.ContainerName "k1") "docker.io/clickhouse/clickhouse-keeper:25.8" "/etc/salmon/k1" "/var/lib/salmon/k1" (Cluster.keeperMember peers3 1)
            c = Cluster.keeperContainer ks
            out = Quadlet.renderContainer c
        assertEqual "no problems" [] (Quadlet.containerProblems c)
        has out ["Volume=/etc/salmon/k1/salmon-keeper.xml:/etc/clickhouse-keeper/keeper_config.d/salmon-keeper.xml:ro", "Volume=/var/lib/salmon/k1:/var/lib/clickhouse:rw"]
        has (Cluster.renderKeeperConfig ks.keeperConfig) ["<listen_host>0.0.0.0</listen_host>"]
        assertEqual "" Nothing (Quadlet.containerReady c)
    , testCase "a member embedding Keeper carries both drop-ins and does not wait for itself" $ do
        let s0 = ClickHouse.server (Podman.ContainerName "ch1") "docker.io/clickhouse/clickhouse-server:25.8" "/etc/salmon/ch1" "/var/lib/salmon/ch1"
            s = Cluster.embeddedKeeperMember (Cluster.keeperMember peers3 1) (Cluster.memberConfig mainCluster (Cluster.keeperNodesFor peers3) "01" "ch1" "ch1") s0
        assertEqual "" ["salmon-keeper.xml", "salmon-cluster.xml"] (fmap (.dropInName) s.serverDropIns)
        assertEqual "" Nothing (Quadlet.containerReady (ClickHouse.serverContainer s))
        assertBool "a plain server still waits" (Quadlet.containerReady (ClickHouse.serverContainer s0) /= Nothing)
    , testCase "Keeper probes: ruok inside the container, a session through keeper-client" $ do
        let via = ClickHouse.InContainer (Podman.ContainerName "ch1")
        assertEqual
            ""
            (Readiness.ProbeCommand "podman" ["exec", "ch1", "bash", "-c", "exec 3<>/dev/tcp/127.0.0.1/9181 && printf ruok >&3 && r=$(timeout 5 cat <&3) && [ \"$r\" = imok ]"])
            (Cluster.keeperAliveProbe via 9181)
        assertEqual
            ""
            (Readiness.ProbeCommand "podman" ["exec", "ch1", "clickhouse-keeper-client", "--host", "127.0.0.1", "--port", "9181", "--query", "ls '/'"])
            (Cluster.keeperSessionProbe via 9181)
    , testCase "system.clusters rows are read against the declared topology" $ do
        assertEqual "" Success (Cluster.interpretClusterRows mainCluster "1\t1\tch1\t9000\n1\t2\tch2\t9000\n2\t1\tch3\t9000\n")
        assertEqual "" (Failure "the server does not list cluster main") (Cluster.interpretClusterRows mainCluster "")
        assertEqual
            ""
            (Failure "the server lists cluster main as 1.1=ch1:9000 1.2=ch2:9000, declared 1.1=ch1:9000 1.2=ch2:9000 2.1=ch3:9000")
            (Cluster.interpretClusterRows mainCluster "1\t1\tch1\t9000\n1\t2\tch2\t9000\n")
        assertEqual "" Unknown (Cluster.interpretClusterRows mainCluster "garbage")
    , testCase "the database statements" $ do
        assertEqual
            ""
            "CREATE DATABASE IF NOT EXISTS `events` ENGINE = Replicated('/clickhouse/databases/events', '{shard}', '{replica}');\n"
            (Cluster.clusterDatabaseSql "events" (Cluster.replicatedEngine "events"))
        assertEqual "" "CREATE DATABASE IF NOT EXISTS `events` ON CLUSTER `main`;\n" (Cluster.clusterDatabaseSql "events" (OnCluster "main"))
        assertEqual
            ""
            "SELECT (SELECT count() FROM clusterAllReplicas('main', system.databases) WHERE name = 'events') = (SELECT count() FROM system.clusters WHERE cluster = 'main')"
            (Cluster.onClusterPresentSql "events" "main")
    , testCase "the engine read back" $ do
        assertEqual "" Success (Cluster.interpretEngine "e" "Replicated\n")
        assertEqual "" (Failure "database e does not exist") (Cluster.interpretEngine "e" "")
        assertEqual "" (Failure "database e exists with engine Atomic, not Replicated") (Cluster.interpretEngine "e" "Atomic\n")
    , testCase "a replicated database shares its ref with a plain one of the same name on the same server" $ do
        let c = ClickHouse.Client (ClickHouse.InContainer (Podman.ContainerName "ch1")) 9000
            refOf o = (.extension.ref) <$> opAct o
        assertEqual
            ""
            (refOf (ClickHouse.database silent ignoreTrack c "events"))
            (refOf (Cluster.clusterDatabase silent ignoreTrack c "events" (Cluster.replicatedEngine "events")))
    ]

-------------------------------------------------------------------------------

-- | The arguments of @podman run@ its quadlet would give a container: binds (not the data directory), env file, network.
runArgs :: Quadlet.Container -> [String]
runArgs c =
    mconcat
        [ ["--network", Text.unpack n] | Just n <- [c.containerNetwork]
        ]
        <> mconcat [["--network-alias", Text.unpack (Podman.getContainerName c.containerName)] | Just _ <- [c.containerNetwork]]
        <> mconcat [["--env-file", f] | Just f <- [c.containerEnvFile]]
        <> mconcat [["-v", Text.unpack (Quadlet.renderBind b)] | b <- c.containerBinds, b.bindGuestPath /= "/var/lib/clickhouse"]

podman :: [String] -> IO (ExitCode, String, String)
podman args = readProcessWithExitCode "podman" args ""

startContainer :: Quadlet.Container -> IO ()
startContainer c = do
    (code, _, err) <- podman (["run", "-d", "--name", Text.unpack (Podman.getContainerName c.containerName)] <> runArgs c <> [Text.unpack c.containerImage])
    case code of
        ExitSuccess -> pure ()
        _ -> assertFailure ("podman run " <> Text.unpack (Podman.getContainerName c.containerName) <> ": " <> err)

withNetwork :: String -> IO a -> IO a
withNetwork net act = do
    void (podman ["network", "create", net])
    act `finally` void (podman ["network", "rm", "-f", net])

withContainers :: [Quadlet.Container] -> IO a -> IO a
withContainers cs act =
    (mapM_ startContainer cs >> act)
        `finally` forM_ cs (\c -> podman ["rm", "-f", "-t", "1", Text.unpack (Podman.getContainerName c.containerName)])

pullImage :: String -> IO ()
pullImage img = do
    (code, _, err) <- podman ["pull", "--quiet", img]
    assertBool ("pull " <> img <> ": " <> err) (code == ExitSuccess)

{- | Three servers, each a Keeper member, in cluster @main@: shard 1 is the
first two, shard 2 the third. Members find each other by container name on
the network.
-}
liveCluster :: IO ()
liveCluster = requireExecutable "podman" $
    withTempDir $ \dir -> do
        base <- Text.unpack . Podman.getContainerName <$> freshContainerName
        let image = "docker.io/clickhouse/clickhouse-server:25.8"
            net = base <> "-net"
            names = [Text.pack (base <> "-ch" <> show i) | i <- [1 :: Int, 2, 3]]
            peers = Cluster.keeperPeersFor names
            topo =
                (Cluster.cluster "main" [Cluster.shardOf (take 2 names), Cluster.shardOf (drop 2 names)])
                    { clusterSecretEnv = Just "CH_CLUSTER_SECRET"
                    }
            envFile = dir </> "cluster.env"
            member i name =
                Cluster.embeddedKeeperMember
                    (Cluster.keeperMember peers i)
                    (Cluster.memberConfig topo (Cluster.keeperNodesFor peers) (if i <= 2 then "01" else "02") name name)
                        { clusterInterserverCredentials = Just (Cluster.InterserverCredentials "salmon-interserver" "CH_INTERSERVER_PASSWORD")
                        }
                    (ClickHouse.server (Podman.ContainerName name) (Text.pack image) (dir </> ("cfg" <> show i)) (dir </> ("data" <> show i)))
                        { ClickHouse.serverEnvFile = Just envFile
                        , ClickHouse.serverNetwork = Just (Text.pack net)
                        }
            servers = zipWith member [1 ..] names
        pullImage image
        writeFile envFile "CH_CLUSTER_SECRET=cluster-secret-not-in-any-report\nCH_INTERSERVER_PASSWORD=interserver-password-not-in-any-report\n"
        -- the files serverOp writes: the two drop-ins it always had, and the new ones through dropInFile
        forM_ servers $ \s -> do
            void (readProcessWithExitCode "mkdir" ["-p", s.serverConfigDir] "")
            Text.writeFile (ClickHouse.serverConfigPath s) (ClickHouse.renderServerConfig s.serverConfig)
            Text.writeFile (ClickHouse.adminAccessPath s) ClickHouse.renderAdminAccess
            forM_ s.serverDropIns $ \d -> do
                ok <- runUp (ClickHouse.dropInFile (ClickHouse.dropInPath s d) d)
                assertBool ("write " <> d.dropInName) ok
        withNetwork net $ withContainers (fmap ClickHouse.serverContainer servers) $ do
            let clients = fmap ClickHouse.serverClient servers
                vias = [(c.clientVia, Cluster.defaultKeeperPort) | c <- clients]
                -- no member waits for itself: the graph waits once, for the quorum
                quorum = Cluster.keeperQuorum "main" vias 120
                quorumT = Track (const quorum)
                perMember c =
                    let connected = Cluster.keeperConnected c 60 `inject` quorum
                        visible = Cluster.clusterVisible c topo 60 `inject` quorum
                     in Cluster.clusterDatabase silent quorumT c "events" (Cluster.replicatedEngine "events") `inject` connected `inject` visible
                first = case clients of
                    c : _ -> c
                    [] -> error "no client"
                onCluster =
                    Cluster.clusterDatabase silent quorumT first "audit" (OnCluster "main")
                        `inject` (Cluster.keeperConnected first 60 `inject` quorum)
                        `inject` (Cluster.clusterVisible first topo 60 `inject` quorum)
                graph = op "clickhouse-cluster-test" (deps (onCluster : fmap perMember clients)) id
            pass1 <- runUpCapturing graph
            assertEqual "first pass succeeds" [] (failed pass1)
            forM_ clients $ \c -> do
                assertEqual ("keeper " <> show c) Success =<< Cluster.checkKeeperConnected c
                assertEqual ("cluster " <> show c) Success =<< Cluster.checkClusterVisible c topo
                assertEqual ("events " <> show c) Success =<< Cluster.checkClusterDatabase c "events" (Cluster.replicatedEngine "events")
            assertEqual "audit on every replica" Success =<< Cluster.checkClusterDatabase first "audit" (OnCluster "main")
            pass2 <- runUpCapturing graph
            assertEqual "the second pass applies nothing" [] [help a | UpDown.Done a <- pass2, isApplied a]
            -- a replicated table made once reaches every member (the
            -- database's DDL log), and its rows the other replica of the
            -- shard (interserver fetch, with the credentials)
            let container i = Text.unpack (names !! i)
            (c1, _, e1) <- podmanExecCapture (container 0) ["clickhouse-client", "--multiquery", "--query", "CREATE TABLE IF NOT EXISTS events.t (x UInt64) ENGINE = ReplicatedMergeTree ORDER BY x; INSERT INTO events.t VALUES (1), (2), (3)"]
            assertEqual ("create and insert: " <> e1) ExitSuccess c1
            rows <- forM [1, 2] $ \i -> do
                let ask = do
                        (_, out, _) <- podmanExecCapture (container i) ["clickhouse-client", "--query", "SELECT count() FROM events.t"]
                        pure out
                _ <- Readiness.pollUntil Readiness.systemClock 30000 500 ((== "3") . filter (/= '\n') <$> ask)
                ask
            assertEqual "the other replica of shard 1 has the rows; shard 2 has the table, not the rows" ["3\n", "0\n"] rows
            -- the secrets are where podman put them, and in nothing the server lists
            (_, clusters, _) <- podmanExecCapture (container 0) ["clickhouse-client", "--query", "SELECT * FROM system.clusters FORMAT TSV"]
            assertBool clusters (not ("cluster-secret" `isInfixOf` clusters))
            forM_ [a | UpDown.Done a <- pass1 <> pass2] $ \a ->
                assertBool "no secret in help or notes" (not (any ("not-in-any-report" `Text.isInfixOf`) (a.extension.help : a.extension.notes)))
  where
    help :: Act' -> String
    help a = Text.unpack a.extension.help
    isApplied :: Act' -> Bool
    isApplied a = any (`Text.isInfixOf` a.extension.help) ["ClickHouse database", "replicated ClickHouse", "lists cluster", "Keeper session"]
    failed reports = [help a <> ": " <> show e | UpDown.Failed a e <- reports]

-- | Three Keeper members of the official Keeper image: each answers @imok@ alone, and together they give sessions.
liveStandaloneKeeper :: IO ()
liveStandaloneKeeper = requireExecutable "podman" $
    withTempDir $ \dir -> do
        base <- Text.unpack . Podman.getContainerName <$> freshContainerName
        let image = "docker.io/clickhouse/clickhouse-keeper:25.8"
            net = base <> "-net"
            names = [Text.pack (base <> "-k" <> show i) | i <- [1 :: Int, 2, 3]]
            peers = Cluster.keeperPeersFor names
            member i name =
                (Cluster.keeperServer (Podman.ContainerName name) (Text.pack image) (dir </> ("k" <> show i)) (dir </> ("kdata" <> show i)) (Cluster.keeperMember peers i))
                    { Cluster.keeperNetwork = Just (Text.pack net)
                    }
            members = zipWith member [1 ..] names
        pullImage image
        forM_ members $ \ks -> do
            ok <- runUp (ClickHouse.dropInFile (Cluster.keeperConfigPath ks) (Cluster.keeperDropIn ks.keeperConfig))
            assertBool "write the drop-in" ok
        withNetwork net $ do
            -- the first member alone: alive, no quorum
            let containers = fmap Cluster.keeperContainer members
                vias = [(ClickHouse.InContainer ks.keeperName, ks.keeperConfig.keeperPort) | ks <- members]
                alive (via, port) = Readiness.waitUntil (Readiness.untilProbe ("keeper-alive-" <> Text.pack (show via)) (Cluster.keeperAliveProbe via port) 60)
            case (containers, vias) of
                (c1 : rest, v1 : _) -> withContainers [c1] $ do
                    -- a lone member of three opens no client port: why
                    -- nothing waits for a member alone
                    lonely <- Readiness.pollUntil Readiness.systemClock 15000 1000 (Readiness.runProbe (uncurry Cluster.keeperAliveProbe v1))
                    assertBool "a lone member of three does not answer ruok" (not lonely)
                    withContainers rest $ do
                        quorum <- runUpCapturing (Cluster.keeperQuorum "standalone" vias 120)
                        assertEqual "the ensemble gives sessions through every member" [] [Text.unpack a.extension.help <> ": " <> show e | UpDown.Failed a e <- quorum]
                        forM_ vias $ \v -> do
                            ok <- runUp (alive v)
                            assertBool ("imok from " <> show v) ok
                _ -> assertFailure "three members"
