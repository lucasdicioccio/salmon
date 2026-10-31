{-# LANGUAGE LambdaCase #-}

{- | ClickHouse as a replicated and sharded cluster, on top of
"Salmon.Builtin.Nodes.ClickHouse":

* a __Keeper ensemble__: the @keeper_server@ drop-in of one member
  ('renderKeeperConfig'), embedded in a server ('keeperDropIn' into
  'ClickHouse.serverDropIns') or standalone in its own container
  ('keeperServerOp'), and the wait for the ensemble ('keeperQuorum'). A member
  of several answers nothing, not even a server's @SELECT 1@ when embedded,
  before there is a quorum, so no member waits for itself: the graph waits
  once, after all of them ('embeddedKeeperMember');
* a __cluster drop-in__ for a server ('renderClusterConfig'): @remote_servers@
  (shards × replicas), @macros@, the @zookeeper@ section pointing at Keeper,
  the interserver host and credentials;
* __checks__ that read back what the server made of it: @system.clusters@
  ('clusterVisible') and a Keeper session ('keeperConnected');
* a __database on the cluster__ ('clusterDatabase'): the @Replicated@ engine,
  declared on every member, or @ON CLUSTER@, declared once.

Secrets. The cluster's shared secret and the interserver password are never
in a file salmon renders: the drop-in says @\<secret from_env=\"VAR\"\/\>@ and
the value is in the server's pre-provisioned 'ClickHouse.serverEnvFile',
which podman reads at start (and which the quadlet watches through its keyed
digest, so a rotated file is a restart and no digest of it is public).
Nothing here reads that file; nothing secret is in @help@, @notes@ or argv.

Nothing here names a leader or decides a member is dead. Keeper elects its
own leader; 'keeperQuorum' asks every member whether it has a session
(which needs a quorum) and waits, and the cluster topology is what the
operator declares. Nothing drops, detaches or removes a replica.
-}
module Salmon.Builtin.Nodes.ClickHouse.Cluster where

import Control.Exception (Exception, throwIO)
import Data.Char (isAlphaNum, isAscii, isDigit, isUpper)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.FilePath ((</>))

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinaryStdin)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.ClickHouse (Client (..), DatabaseName, DropIn (..), Via (..), adminQuery, adminRun, AdminBatch (..), clientKey, quoteIdent, quoteLiteral, xmlEscape)
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Readiness as Readiness
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = CHClusterDatabase !DatabaseName !Binary.Report
    deriving (Show)

type ClusterName = Text

-------------------------------------------------------------------------------
-- Keeper

-- | Keeper's client port.
defaultKeeperPort :: Int
defaultKeeperPort = 9181

-- | Keeper's Raft port, between members.
defaultRaftPort :: Int
defaultRaftPort = 9234

-- | One member of the ensemble, as every member's @raft_configuration@ lists it.
data KeeperPeer = KeeperPeer
    { peerId :: Int
    -- ^ the member's @server_id@: positive, unique in the ensemble, never reused
    , peerHost :: Text
    -- ^ how the other members reach it (a container name on a podman network)
    , peerRaftPort :: Int
    }
    deriving (Eq, Ord, Show)

-- | One member's @keeper_server@ section.
data Keeper = Keeper
    { keeperServerId :: Int
    -- ^ this member's id, one of 'keeperPeers'
    , keeperPeers :: [KeeperPeer]
    -- ^ every member, this one included, the same list on each
    , keeperPort :: Int
    -- ^ the client port (@tcp_port@)
    , keeperListenHosts :: [Text]
    -- ^ @listen_host@, one element each. Empty renders none: an embedded
    -- member uses the server's (the official server image listens on every
    -- address); a standalone one listens on loopback only unless given one.
    , keeperLogStoragePath :: Maybe Text
    -- ^ 'Nothing' renders nothing: the packaged default under the data directory
    , keeperSnapshotStoragePath :: Maybe Text
    , keeperCoordinationSettings :: [(Text, Text)]
    -- ^ @coordination_settings@, as @(element, value)@; empty renders none
    }
    deriving (Eq, Ord, Show)

-- | The member with this id of this ensemble, on the default client port.
keeperMember :: [KeeperPeer] -> Int -> Keeper
keeperMember peers myId =
    Keeper
        { keeperServerId = myId
        , keeperPeers = peers
        , keeperPort = defaultKeeperPort
        , keeperListenHosts = []
        , keeperLogStoragePath = Nothing
        , keeperSnapshotStoragePath = Nothing
        , keeperCoordinationSettings = []
        }

-- | Peers numbered from 1 in list order, on the default Raft port.
keeperPeersFor :: [Text] -> [KeeperPeer]
keeperPeersFor hosts = [KeeperPeer i h defaultRaftPort | (i, h) <- zip [1 ..] hosts]

{- | The drop-in. @raft_configuration@ carries @replace=\"replace\"@: the
standalone image ships one (a single member on @localhost@) and a merge
would add to it rather than say what the ensemble is.
-}
renderKeeperConfig :: Keeper -> Text
renderKeeperConfig k =
    Text.unlines $
        mconcat
            [ ["<clickhouse>"]
            , [element 1 "listen_host" h | h <- k.keeperListenHosts]
            , ["    <keeper_server>"]
            , [element 2 "tcp_port" (showT k.keeperPort)]
            , [element 2 "server_id" (showT k.keeperServerId)]
            , [element 2 "log_storage_path" p | Just p <- [k.keeperLogStoragePath]]
            , [element 2 "snapshot_storage_path" p | Just p <- [k.keeperSnapshotStoragePath]]
            , section 2 "coordination_settings" [element 3 n v | (n, v) <- k.keeperCoordinationSettings]
            , ["        <raft_configuration replace=\"replace\">"]
            , concat
                [ [ "            <server>"
                  , element 4 "id" (showT p.peerId)
                  , element 4 "hostname" p.peerHost
                  , element 4 "port" (showT p.peerRaftPort)
                  , "            </server>"
                  ]
                | p <- k.keeperPeers
                ]
            , ["        </raft_configuration>"]
            , ["    </keeper_server>"]
            , ["</clickhouse>"]
            ]

{- | What makes a member's configuration wrong before any server sees it: no
peers, an id that is not positive or not unique, this member's id not
among the peers, two peers at one address, a setting that is not an
element name.
-}
keeperProblems :: Keeper -> [Text]
keeperProblems k =
    mconcat
        [ ["the ensemble has no member" | null k.keeperPeers]
        , ["server id " <> showT i <> " is not positive" | i <- ids, i < 1]
        , ["server id " <> showT i <> " is declared twice" | i <- nub ids, length (filter (== i) ids) > 1]
        , ["this member's id " <> showT k.keeperServerId <> " is not among the peers" | not (null k.keeperPeers), k.keeperServerId `notElem` ids]
        , ["two members at " <> h <> ":" <> showT p | (h, p) <- nub addrs, length (filter (== (h, p)) addrs) > 1]
        , ["peer host " <> h <> " is empty or holds a space" | h <- fmap (.peerHost) k.keeperPeers, badHost h]
        , ["coordination setting " <> n <> " is not a plain XML element name" | (n, _) <- k.keeperCoordinationSettings, not (plainName n)]
        ]
  where
    ids = fmap (.peerId) k.keeperPeers
    addrs = [(p.peerHost, p.peerRaftPort) | p <- k.keeperPeers]

-- | 'renderKeeperConfig' as a server drop-in (@salmon-keeper.xml@): Keeper embedded in that server.
keeperDropIn :: Keeper -> DropIn
keeperDropIn k = DropIn "salmon-keeper.xml" (renderKeeperConfig k) (keeperProblems k)

{- | A server that embeds this Keeper member and carries this cluster
drop-in, and whose @up@ does not wait for it to answer
('ClickHouse.serverWaitReady'): it answers nothing before the ensemble has
a quorum. Wait with 'keeperQuorum' after every member, then
'keeperConnected' and 'clusterVisible' on each.
-}
embeddedKeeperMember :: Keeper -> ClusterConfig -> ClickHouse.Server -> ClickHouse.Server
embeddedKeeperMember k cfg s =
    s
        { ClickHouse.serverDropIns = ClickHouse.serverDropIns s <> [keeperDropIn k, clusterDropIn cfg]
        , ClickHouse.serverWaitReady = False
        }

{- | A server that carries this cluster drop-in and reaches a Keeper
elsewhere (standalone members): it answers without a quorum, so its @up@
still waits for it.
-}
clusterMember :: ClusterConfig -> ClickHouse.Server -> ClickHouse.Server
clusterMember cfg s = s{ClickHouse.serverDropIns = ClickHouse.serverDropIns s <> [clusterDropIn cfg]}

{- | Whether the member on this client port answers Keeper's @ruok@ with
@imok@. Asked inside the container (bash's @\/dev\/tcp@, the images have no
@nc@) or on the host. A member of an ensemble of several opens its client
port only once it has joined a quorum (seen on 25.8), so this is not "the
process runs": a lone member of three never answers it.
-}
keeperAliveProbe :: Via -> Int -> Readiness.Probe
keeperAliveProbe via port = inside via "bash" ["-c", script]
  where
    script = "exec 3<>/dev/tcp/127.0.0.1/" <> showT port <> " && printf ruok >&3 && r=$(timeout 5 cat <&3) && [ \"$r\" = imok ]"

{- | Whether the member on this client port gives a client a session:
@clickhouse-keeper-client -q \"ls '/'\"@ exits 0. A session needs a quorum,
so this is "the ensemble answers through this member".
-}
keeperSessionProbe :: Via -> Int -> Readiness.Probe
keeperSessionProbe via port = inside via "clickhouse-keeper-client" ["--host", "127.0.0.1", "--port", showT port, "--query", "ls '/'"]

inside :: Via -> FilePath -> [Text] -> Readiness.Probe
inside LocalClient cmd args = Readiness.ProbeCommand cmd args
inside (InContainer name) cmd args = Readiness.podmanExec (Podman.getContainerName name) cmd args

{- | Waits until every listed member gives a session ('keeperSessionProbe'),
for up to this many seconds. Put it after all the members' nodes and before
whatever needs Keeper (a replicated database). It asks every member and
prefers none: which one leads is Keeper's business. @check@ asks once (a
no is 'Unknown', as 'Readiness.waitUntil'); @down@ does nothing.
-}
keeperQuorum :: Text -> [(Via, Int)] -> Int -> Op
keeperQuorum key members seconds =
    Readiness.waitUntil
        (Readiness.untilIO ("clickhouse-keeper-quorum:" <> key) asking (and <$> traverse (Readiness.runProbe . uncurry keeperSessionProbe) members) seconds)
  where
    asking = "every Keeper member of " <> key <> " gives a session: " <> Text.intercalate ", " (fmap (Readiness.describeProbe . uncurry keeperSessionProbe) members)

-------------------------------------------------------------------------------
-- Standalone Keeper

-- | A Keeper member in its own container (the official @clickhouse-keeper@ image), as a system-scope quadlet.
data KeeperServer = KeeperServer
    { keeperName :: Podman.ContainerName
    , keeperImage :: Text
    -- ^ e.g. @docker.io\/clickhouse\/clickhouse-keeper:25.8@
    , keeperConfigDir :: FilePath
    -- ^ a host directory holding the drop-in, bound read-only into the
    -- image's @keeper_config.d@ (the directory its @keeper_config.xml@ merges)
    , keeperDataDir :: FilePath
    -- ^ mounted at @\/var\/lib\/clickhouse@ (the Raft log and snapshots);
    -- created if missing, never removed
    , keeperConfig :: Keeper
    , keeperNetwork :: Maybe Text
    , keeperPorts :: [Podman.PortMapping]
    }
    deriving (Eq, Show)

-- | Listening on every address (the peers and the servers reach it over the network).
keeperServer :: Podman.ContainerName -> Text -> FilePath -> FilePath -> Keeper -> KeeperServer
keeperServer name image configDir dataDir k =
    KeeperServer
        { keeperName = name
        , keeperImage = image
        , keeperConfigDir = configDir
        , keeperDataDir = dataDir
        , keeperConfig = k{keeperListenHosts = if null k.keeperListenHosts then ["0.0.0.0"] else k.keeperListenHosts}
        , keeperNetwork = Nothing
        , keeperPorts = []
        }

keeperConfigPath :: KeeperServer -> FilePath
keeperConfigPath ks = ks.keeperConfigDir </> "salmon-keeper.xml"

{- | The quadlet: the data directory and the drop-in (watched). Its @up@
does not wait for the member to answer: a member opens its client port only
once it has joined a quorum, so a graph starting the members one after
another would wait on the first for members started after it. Wait once,
after all of them, with 'keeperQuorum'.
-}
keeperContainer :: KeeperServer -> Quadlet.Container
keeperContainer ks =
    (Quadlet.container ks.keeperName ks.keeperImage)
        { Quadlet.containerDescription = "ClickHouse Keeper " <> Podman.getContainerName ks.keeperName <> " (salmon)"
        , Quadlet.containerPorts = ks.keeperPorts
        , Quadlet.containerBinds =
            [ Quadlet.bind ks.keeperDataDir "/var/lib/clickhouse"
            , (Quadlet.bind (keeperConfigPath ks) "/etc/clickhouse-keeper/keeper_config.d/salmon-keeper.xml"){Quadlet.bindMode = Podman.ReadOnly, Quadlet.bindCreate = Nothing}
            ]
        , Quadlet.containerWatched = [keeperConfigPath ks]
        , Quadlet.containerNetwork = ks.keeperNetwork
        }

-- | The drop-in written (refused with 'keeperProblems'), then the quadlet.
keeperServerOp :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> KeeperServer -> Op
keeperServerOp r systemctl ks =
    Quadlet.quadletContainer r systemctl (Track (const file)) (keeperContainer ks)
  where
    file = ClickHouse.dropInFile (keeperConfigPath ks) (keeperDropIn ks.keeperConfig)

-------------------------------------------------------------------------------
-- The cluster drop-in

data Replica = Replica
    { replicaHost :: Text
    , replicaPort :: Int
    -- ^ the native protocol port
    }
    deriving (Eq, Ord, Show)

data Shard = Shard
    { shardReplicas :: [Replica]
    , shardWeight :: Maybe Int
    -- ^ 'Nothing' renders nothing (weight 1)
    , shardInternalReplication :: Bool
    -- ^ @True@ for replicated tables: a @Distributed@ insert goes to one
    -- replica and the table replicates it
    }
    deriving (Eq, Ord, Show)

-- | A shard of these hosts on the default port, with internal replication.
shardOf :: [Text] -> Shard
shardOf hosts = Shard [Replica h ClickHouse.defaultTcpPort | h <- hosts] Nothing True

-- | One entry of @remote_servers@.
data Cluster = Cluster
    { clusterName :: ClusterName
    , clusterShards :: [Shard]
    , clusterSecretEnv :: Maybe Text
    -- ^ the environment variable holding the cluster's shared secret
    -- (@\<secret from_env=\"VAR\"\/\>@). With it, a query a member sends to
    -- another (a @Distributed@ read, @clusterAllReplicas@) runs there as the
    -- user who sent it, without a password; without it, as @default@ with no
    -- password, which 'ClickHouse.renderAdminAccess' only lets in from
    -- loopback. 'Nothing' renders nothing.
    }
    deriving (Eq, Ord, Show)

cluster :: ClusterName -> [Shard] -> Cluster
cluster name shards = Cluster name shards Nothing

-- | A Keeper member as the servers' @zookeeper@ section lists it.
data KeeperNode = KeeperNode {keeperNodeHost :: Text, keeperNodePort :: Int}
    deriving (Eq, Ord, Show)

-- | The client side of an ensemble: every peer's host on the default client port.
keeperNodesFor :: [KeeperPeer] -> [KeeperNode]
keeperNodesFor peers = [KeeperNode p.peerHost defaultKeeperPort | p <- peers]

{- | @interserver_http_credentials@: what replicas present to each other to
fetch parts. The user is a name, the password the environment variable that
holds it (@from_env@).
-}
data InterserverCredentials = InterserverCredentials
    { interserverUser :: Text
    , interserverPasswordEnv :: Text
    }
    deriving (Eq, Ord, Show)

-- | One server's cluster drop-in. Every part is optional and renders nothing when empty.
data ClusterConfig = ClusterConfig
    { clusterRemoteServers :: [Cluster]
    , clusterMacros :: [(Text, Text)]
    -- ^ @macros@, as @(name, value)@: @shard@ and @replica@ ('memberMacros')
    , clusterKeeper :: [KeeperNode]
    -- ^ the @zookeeper@ section (rendered @replace@: the list is the ensemble)
    , clusterKeeperRoot :: Maybe Text
    -- ^ a chroot in Keeper for this server's paths; 'Nothing' renders nothing
    , clusterInterserverHost :: Maybe Text
    -- ^ the name the other replicas fetch parts from (@interserver_http_host@);
    -- the default is the machine's hostname, which in a container is its id
    , clusterInterserverCredentials :: Maybe InterserverCredentials
    }
    deriving (Eq, Ord, Show)

-- | Renders the root element and nothing else.
defaultClusterConfig :: ClusterConfig
defaultClusterConfig = ClusterConfig [] [] [] Nothing Nothing Nothing

{- | The @shard@ and @replica@ macros (with @cluster@ when given), what a
@Replicated@ database and a @ReplicatedMergeTree@ path refer to.
-}
memberMacros :: Maybe ClusterName -> Text -> Text -> [(Text, Text)]
memberMacros name shard replica = [("cluster", n) | Just n <- [name]] <> [("shard", shard), ("replica", replica)]

{- | A member of one cluster: its topology, its macros, Keeper, and the
interserver host. The usual shape, for a server declared in that topology.
-}
memberConfig :: Cluster -> [KeeperNode] -> Text -> Text -> Text -> ClusterConfig
memberConfig c keeper shard replica host =
    defaultClusterConfig
        { clusterRemoteServers = [c]
        , clusterMacros = memberMacros (Just c.clusterName) shard replica
        , clusterKeeper = keeper
        , clusterInterserverHost = Just host
        }

renderClusterConfig :: ClusterConfig -> Text
renderClusterConfig cfg =
    Text.unlines $
        mconcat
            [ ["<clickhouse>"]
            , [element 1 "interserver_http_host" h | Just h <- [cfg.clusterInterserverHost]]
            , case cfg.clusterInterserverCredentials of
                Nothing -> []
                Just ic ->
                    [ "    <interserver_http_credentials>"
                    , element 2 "user" ic.interserverUser
                    , "        <password from_env=\"" <> xmlEscape ic.interserverPasswordEnv <> "\" />"
                    , "    </interserver_http_credentials>"
                    ]
            , section 1 "remote_servers" (concatMap renderCluster cfg.clusterRemoteServers)
            , section 1 "macros" [element 2 n v | (n, v) <- cfg.clusterMacros]
            , if null cfg.clusterKeeper
                then []
                else
                    ["    <zookeeper replace=\"replace\">"]
                        <> concat
                            [ ["        <node>", element 3 "host" n.keeperNodeHost, element 3 "port" (showT n.keeperNodePort), "        </node>"]
                            | n <- cfg.clusterKeeper
                            ]
                        <> [element 2 "root" r | Just r <- [cfg.clusterKeeperRoot]]
                        <> ["    </zookeeper>"]
            , ["</clickhouse>"]
            ]
  where
    renderCluster :: Cluster -> [Text]
    renderCluster c =
        ["        <" <> c.clusterName <> ">"]
            <> ["            <secret from_env=\"" <> xmlEscape v <> "\" />" | Just v <- [c.clusterSecretEnv]]
            <> concatMap renderShard c.clusterShards
            <> ["        </" <> c.clusterName <> ">"]
    renderShard :: Shard -> [Text]
    renderShard s =
        ["            <shard>"]
            <> [element 4 "weight" (showT w) | Just w <- [s.shardWeight]]
            <> [element 4 "internal_replication" (if s.shardInternalReplication then "true" else "false")]
            <> concat
                [ ["                <replica>", element 5 "host" r.replicaHost, element 5 "port" (showT r.replicaPort), "                </replica>"]
                | r <- s.shardReplicas
                ]
            <> ["            </shard>"]

{- | What makes a cluster drop-in wrong: a cluster or macro name that is not an
element name, a cluster declared twice or without shards, a shard without
replicas, a replica twice in one cluster, an environment variable name that
is not one, a Keeper root that is not an absolute path.
-}
clusterConfigProblems :: ClusterConfig -> [Text]
clusterConfigProblems cfg =
    mconcat
        [ ["cluster " <> n <> " is not a plain XML element name" | n <- names, not (plainName n)]
        , ["cluster " <> n <> " is declared twice" | n <- nub names, length (filter (== n) names) > 1]
        , ["cluster " <> c.clusterName <> " has no shard" | c <- cfg.clusterRemoteServers, null c.clusterShards]
        , ["cluster " <> c.clusterName <> " has a shard without replicas" | c <- cfg.clusterRemoteServers, any (null . (.shardReplicas)) c.clusterShards]
        , [ "cluster " <> c.clusterName <> " lists " <> r.replicaHost <> ":" <> showT r.replicaPort <> " twice"
          | c <- cfg.clusterRemoteServers
          , let rs = concatMap (.shardReplicas) c.clusterShards
          , r <- nub rs
          , length (filter (== r) rs) > 1
          ]
        , ["replica host " <> h <> " is empty or holds a space" | c <- cfg.clusterRemoteServers, s <- c.clusterShards, Replica h _ <- s.shardReplicas, badHost h]
        , ["macro " <> n <> " is not a plain XML element name" | (n, _) <- cfg.clusterMacros, not (plainName n)]
        , ["environment variable " <> v <> " is not a plain name" | v <- envs, not (envName v)]
        , ["Keeper root " <> r <> " is not an absolute path" | Just r <- [cfg.clusterKeeperRoot], not ("/" `Text.isPrefixOf` r)]
        , ["Keeper host " <> n.keeperNodeHost <> " is empty or holds a space" | n <- cfg.clusterKeeper, badHost n.keeperNodeHost]
        ]
  where
    names = fmap (.clusterName) cfg.clusterRemoteServers
    envs = [v | c <- cfg.clusterRemoteServers, Just v <- [c.clusterSecretEnv]] <> [ic.interserverPasswordEnv | Just ic <- [cfg.clusterInterserverCredentials]]

-- | 'renderClusterConfig' as a server drop-in (@salmon-cluster.xml@).
clusterDropIn :: ClusterConfig -> DropIn
clusterDropIn cfg = DropIn "salmon-cluster.xml" (renderClusterConfig cfg) (clusterConfigProblems cfg)

-------------------------------------------------------------------------------
-- Reading back

-- | The rows the server should list in @system.clusters@: shard and replica numbers from 1, in declaration order, host and port.
expectedClusterRows :: Cluster -> [(Int, Int, Text, Int)]
expectedClusterRows c =
    [ (si, ri, r.replicaHost, r.replicaPort)
    | (si, s) <- zip [1 ..] c.clusterShards
    , (ri, r) <- zip [1 ..] s.shardReplicas
    ]

clusterRowsSql :: ClusterName -> Text
clusterRowsSql name =
    "SELECT shard_num, replica_num, host_name, port FROM system.clusters WHERE cluster = "
        <> quoteLiteral name
        <> " ORDER BY shard_num, replica_num FORMAT TSV"

{- | 'Success' when the rows are the declared ones, else a 'Failure' naming
the first difference; output that does not parse is 'Unknown'.
-}
interpretClusterRows :: Cluster -> Text -> CheckResult
interpretClusterRows c out =
    case traverse parseRow (filter (not . Text.null . Text.strip) (Text.lines out)) of
        Nothing -> Unknown
        Just rows
            | rows == expected -> Success
            | null rows -> Failure ("the server does not list cluster " <> c.clusterName)
            | otherwise ->
                Failure $
                    "the server lists cluster "
                        <> c.clusterName
                        <> " as "
                        <> describe rows
                        <> ", declared "
                        <> describe expected
  where
    expected = expectedClusterRows c
    parseRow l = case Text.splitOn "\t" l of
        [s, r, h, p] -> (,,,) <$> readInt s <*> readInt r <*> pure h <*> readInt p
        _ -> Nothing
    describe rows = Text.intercalate " " [showT s <> "." <> showT r <> "=" <> h <> ":" <> showT p | (s, r, h, p) <- rows]

checkClusterVisible :: Client -> Cluster -> IO CheckResult
checkClusterVisible client c = either (const Unknown) (interpretClusterRows c) <$> adminQuery client (clusterRowsSql c.clusterName)

{- | The server lists the cluster as declared in @system.clusters@. A server
reloads @remote_servers@ without a restart, and the quadlet restarts on a
changed drop-in, so @up@ only waits for it, for up to this many seconds,
and throws if it never holds. The check names the difference. @down@ does
nothing.
-}
clusterVisible :: Client -> Cluster -> Int -> Op
clusterVisible client c seconds =
    op "clickhouse-cluster-visible" nodeps $ \actions ->
        actions
            { ref = mkRef "clickhouse-cluster-visible" (clientKey client, c.clusterName)
            , help = "ClickHouse server " <> clientKey client <> " lists cluster " <> c.clusterName <> " as declared"
            , notes = ["server: " <> clientKey client, "declared: " <> Text.intercalate " " [showT s <> "." <> showT r <> "=" <> h <> ":" <> showT p | (s, r, h, p) <- expectedClusterRows c]]
            , check = checkClusterVisible client c
            , up = waitFor ("cluster " <> c.clusterName <> " on " <> clientKey client) seconds (checkClusterVisible client c)
            }

keeperSessionSql :: Text
keeperSessionSql = "SELECT count() > 0 FROM system.zookeeper WHERE path = '/'"

{- | Whether the server has a Keeper session: it answers @SELECT 1@ (else
'Unknown') and reads @system.zookeeper@ at @\/@ (else a 'Failure' with the
server's error).
-}
checkKeeperConnected :: Client -> IO CheckResult
checkKeeperConnected client = do
    alive <- adminQuery client "SELECT 1"
    case alive of
        Left _ -> pure Unknown
        Right _ -> do
            r <- adminQuery client keeperSessionSql
            pure $ case r of
                Right out | Text.strip out == "1" -> Success
                Right out -> Failure ("unexpected answer from system.zookeeper: " <> Text.strip out)
                Left err -> Failure ("no Keeper session: " <> firstLine err)

{- | The server reaches Keeper: @up@ waits for 'checkKeeperConnected' for up
to this many seconds, and throws if it never holds. @down@ does nothing.
-}
keeperConnected :: Client -> Int -> Op
keeperConnected client seconds =
    op "clickhouse-keeper-connected" nodeps $ \actions ->
        actions
            { ref = mkRef "clickhouse-keeper-connected" (clientKey client)
            , help = "ClickHouse server " <> clientKey client <> " has a Keeper session"
            , notes = ["server: " <> clientKey client]
            , check = checkKeeperConnected client
            , up = waitFor ("a Keeper session on " <> clientKey client) seconds (checkKeeperConnected client)
            }

-- | Thrown by a wait that never saw 'Success'; the text is the last verdict.
data NotAsDeclared = NotAsDeclared !Text !Int !Text
    deriving (Show)

instance Exception NotAsDeclared

waitFor :: Text -> Int -> IO CheckResult -> IO ()
waitFor what seconds ask = do
    ok <- Readiness.pollUntil Readiness.systemClock (seconds * 1000) 500 ((== Success) <$> ask)
    if ok
        then pure ()
        else do
            last' <- ask
            case last' of
                Success -> pure ()
                Failure why -> throwIO (NotAsDeclared what seconds why)
                other -> throwIO (NotAsDeclared what seconds (showT other))

-------------------------------------------------------------------------------
-- Databases on the cluster

-- | How a database's DDL reaches every replica.
data Distribution
    = {- | @ENGINE = Replicated(path, shard, replica)@: the database keeps its
      DDL log in Keeper at this path and every member that has it replays it.
      Declare it on every member (each with its own client); the shard and
      replica names are usually the macros.
      -}
      ReplicatedEngine
        { replicatedPath :: Text
        , replicatedShard :: Text
        , replicatedReplica :: Text
        }
    | {- | @CREATE DATABASE ... ON CLUSTER@ (engine left to the server):
      once, through any member; the distributed DDL queue runs it on each.
      -}
      OnCluster ClusterName
    deriving (Eq, Ord, Show)

-- | Keeper path @\/clickhouse\/databases\/NAME@, the @{shard}@ and @{replica}@ macros.
replicatedEngine :: DatabaseName -> Distribution
replicatedEngine name = ReplicatedEngine ("/clickhouse/databases/" <> name) "{shard}" "{replica}"

clusterDatabaseSql :: DatabaseName -> Distribution -> Text
clusterDatabaseSql name = \case
    ReplicatedEngine path shard replica ->
        "CREATE DATABASE IF NOT EXISTS "
            <> quoteIdent name
            <> " ENGINE = Replicated("
            <> Text.intercalate ", " (fmap quoteLiteral [path, shard, replica])
            <> ");\n"
    OnCluster c -> "CREATE DATABASE IF NOT EXISTS " <> quoteIdent name <> onClusterClause c <> ";\n"

-- | @ ON CLUSTER `name`@, for DDL a later node sends to a whole cluster.
onClusterClause :: ClusterName -> Text
onClusterClause c = " ON CLUSTER " <> quoteIdent c

databaseEngineSql :: DatabaseName -> Text
databaseEngineSql name = "SELECT engine FROM system.databases WHERE name = " <> quoteLiteral name

-- | Every replica of the cluster has the database: the count across @clusterAllReplicas@ is the replica count.
onClusterPresentSql :: DatabaseName -> ClusterName -> Text
onClusterPresentSql name c =
    "SELECT (SELECT count() FROM clusterAllReplicas("
        <> quoteLiteral c
        <> ", system.databases) WHERE name = "
        <> quoteLiteral name
        <> ") = (SELECT count() FROM system.clusters WHERE cluster = "
        <> quoteLiteral c
        <> ")"

-- | The engine read back: @Replicated@ is 'Success', none a 'Failure', another engine a 'Failure' naming it.
interpretEngine :: DatabaseName -> Text -> CheckResult
interpretEngine name out = case Text.strip out of
    "Replicated" -> Success
    "" -> Failure ("database " <> name <> " does not exist")
    other -> Failure ("database " <> name <> " exists with engine " <> other <> ", not Replicated")

checkClusterDatabase :: Client -> DatabaseName -> Distribution -> IO CheckResult
checkClusterDatabase client name = \case
    ReplicatedEngine{} -> either (const Unknown) (interpretEngine name) <$> adminQuery client (databaseEngineSql name)
    OnCluster c -> either (const Unknown) (ClickHouse.interpretCount ("database " <> name <> " is missing on some replica of " <> c)) <$> adminQuery client (onClusterPresentSql name c)

-- | A database whose engine is not the declared one; nothing converts or drops it.
data WrongEngine = WrongEngine !DatabaseName !Text
    deriving (Show)

instance Exception WrongEngine

{- | A database whose DDL reaches every replica ('Distribution'), created if
missing. The server must reach Keeper first (inject 'keeperConnected', and
for @ON CLUSTER@ 'clusterVisible'). @down@ drops nothing.

For 'ReplicatedEngine', @up@ reads the engine back after the create and
throws when a database of that name already existed with another engine:
nothing here converts or drops one. The 'Ref' is the one
'ClickHouse.database' uses, so declaring both for one name on one server is
a conflict, not two nodes.

For 'OnCluster', @up@ fails when a member does not answer within the
server's @distributed_ddl_task_timeout@ (the task stays queued, and the
member runs it when it is back); the check is 'Unknown' while a replica does
not answer.
-}
clusterDatabase :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> DatabaseName -> Distribution -> Op
clusterDatabase r clientT client name dist =
    withBinaryStdin clientT (adminRun client) AdminBatch (Text.encodeUtf8 (clusterDatabaseSql name dist)) $ \create ->
        op "clickhouse-cluster-database" nodeps $ \actions ->
            actions
                { ref = case dist of
                    ReplicatedEngine{} -> mkRef "clickhouse-db" (clientKey client, name)
                    OnCluster c -> mkRef "clickhouse-cluster-db" (clientKey client, c, name)
                , help = case dist of
                    ReplicatedEngine{} -> "create replicated ClickHouse database " <> name
                    OnCluster c -> "create ClickHouse database " <> name <> " on cluster " <> c
                , notes = ["server: " <> clientKey client, "statement: " <> Text.strip (clusterDatabaseSql name dist)]
                , check = checkClusterDatabase client name dist
                , up = do
                    create (contramap (CHClusterDatabase name) r)
                    case dist of
                        OnCluster _ -> pure ()
                        ReplicatedEngine{} -> do
                            engine <- adminQuery client (databaseEngineSql name)
                            case engine of
                                Right out | Text.strip out /= "Replicated" -> throwIO (WrongEngine name (Text.strip out))
                                _ -> pure ()
                }

-------------------------------------------------------------------------------

element :: Int -> Text -> Text -> Text
element depth k v = Text.replicate (4 * depth) " " <> "<" <> k <> ">" <> xmlEscape v <> "</" <> k <> ">"

section :: Int -> Text -> [Text] -> [Text]
section _ _ [] = []
section depth name inner = [pad <> "<" <> name <> ">"] <> inner <> [pad <> "</" <> name <> ">"]
  where
    pad = Text.replicate (4 * depth) " "

plainName :: Text -> Bool
plainName k = case Text.uncons k of
    Just (c, rest) -> isAscii c && (c == '_' || isAlphaNum c) && not (isDigit c) && Text.all (\x -> isAscii x && (x == '_' || isAlphaNum x)) rest
    Nothing -> False

envName :: Text -> Bool
envName v = case Text.uncons v of
    Just (c, rest) -> (c == '_' || (isAscii c && isUpper c)) && Text.all (\x -> x == '_' || (isAscii x && (isUpper x || isDigit x))) rest
    Nothing -> False

badHost :: Text -> Bool
badHost h = Text.null h || Text.any (\c -> c == ' ' || c == '\t' || c == '\n' || c == '<' || c == '>' || c == '&') h

readInt :: Text -> Maybe Int
readInt t = case reads (Text.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing

firstLine :: Text -> Text
firstLine t = case Text.lines t of
    (l : _) -> l
    [] -> ""

showT :: (Show a) => a -> Text
showT = Text.pack . show
