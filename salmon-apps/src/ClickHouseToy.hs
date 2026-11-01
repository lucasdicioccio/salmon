{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A small ClickHouse cluster on this machine, under rootless podman: a
Keeper ensemble, a replicated cluster, schema migrations applied @ON
CLUSTER@, a scheduled backup, and a restore drill into a server of its own.

It is the ClickHouse counterpart of @salmon-toy-qemu-pg-ha@ and
@salmon-gcp-toy@: the nodes of "Salmon.Builtin.Nodes.ClickHouse",
@.Cluster@, @.Migrations@ and @.Backup@ wired end to end, in a form a person
can run and read.

> t=$(cabal list-bin salmon-toy-clickhouse)
> $t config up --root $HOME/chtoy --migrations salmon-apps/clickhouse-toy/migrations | $t run up
> podman exec -i chtoy-1 clickhouse-client --query "INSERT INTO toy.events (at, kind, value) SELECT now(), 'demo', number FROM numbers(1000)"
> systemctl --user start clickhouse-backup-chtoy.service      # a backup now, as the timer would
> $t config drill --root $HOME/chtoy | $t run up              # the newest backup, restored elsewhere
> podman exec -i chtoy-drill clickhouse-client --query "SELECT count() FROM toy.events"
> $t config drill --root $HOME/chtoy | $t run down
> $t config up --root $HOME/chtoy --migrations ... | $t run down

= What @up@ declares

Three servers, @PREFIX-1@ to @PREFIX-3@, on one podman network, each
embedding a member of a three-member Keeper ensemble, and cluster @main@ of
one shard of three replicas (so any one member holds all the data, which is
what makes a backup of one member a backup of the cluster). The graph waits
for the quorum once, after all three, then for each member's Keeper session
and its view of the cluster; then database @toy@ is created @ON CLUSTER@,
the migrations are applied @ON CLUSTER@ through member 1 (the ledger is
member 1's), and a backup of @toy@ is scheduled on member 1: a user-scope
systemd timer over the job, whose first run is taken by the pass that
schedules it.

= Why podman and not qemu

The feature asked for qemu guests, or podman if guests proved too heavy.
This is podman, rootless, with every container a __user-scope quadlet__ (so
the nodes are the same ones a deployment uses, only the scope and the unit
directory amended): three servers start in seconds, need no capability
granted, and leave nothing behind a @run down@
and an @rm -rf@ of the root cannot remove. qemu guests on a shared machine
are slow to boot, need the capabilities @salmon-toy-qemu-pg-ha@ asks for,
and a killed run leaks them.

= The restore drill

@drill@ is a fourth server, @PREFIX-drill@, on no network and in no
cluster of the three: its own one-member Keeper and a one-member cluster
@drill@ on @localhost@, binding the same backup directory. It restores
@toy@ from one backup file (by default the newest in the directory, found by
@config@), guarded as "Salmon.Builtin.Nodes.ClickHouse.Backup".@restore@ is:
a second pass does nothing, and a @toy@ there that is not this restore is
refused unless @--may-discard@ is given. Restoring next to the cluster, not
into it, is the point: a drill reads the backup without touching what it is
a backup of, and the replicated tables it recreates register in the drill's
own Keeper, not the cluster's.

= Secrets

The cluster's shared secret and the interserver password are in
@ROOT\/secrets\/cluster.env@, which podman reads at each start
('ClickHouse.serverEnvFile'); no rendered file, @help@, @notes@ or argv holds
them. The toy plays the "somebody else" who provisions that file
('inventSecrets', after @salmon-toy-qemu-pg-ha@'s): random values, written
once, mode @0600@, never rewritten. A deployment hands the recipe a file it
did not make. GCS shipping is off unless @--gcs-bucket@ is given, and its
credentials are gcloud's own or a pre-provisioned key file whose /path/ is
passed.

= Rootless leftovers

The data and backup directories belong to the containers' @clickhouse@
user, a subordinate uid of yours: remove them with @podman unshare rm -rf
ROOT@. For the same reason the backup plan keeps every local file (pruning
would be @rm@ by you of files you do not own).
-}
module ClickHouseToy (
    main,
    Seed (..),
    Spec (..),
    Toy (..),
    GcsSpec (..),
    program,
    configure,
    exampleToy,
    memberNames,
    memberServer,
    drillName,
    drillServer,
    networkName,
    toyDatabase,
    toyCluster,
    toyMigrations,
    toyBackupPlan,
    toyBackupJob,
    toyRestore,
    secretsFile,
    backupsDir,
    renderSecrets,
    latestBackup,
) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as ByteString
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Options.Applicative (command, execParser, fullDesc, header, help, helper, info, long, many, optional, progDesc, strOption, subparser, switch, value, (<**>))
import Options.Generic (ParseRecord (..))
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory, makeAbsolute, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension hiding (help)
import qualified Salmon.Builtin.Extension as Extension
import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import qualified Salmon.Builtin.Nodes.ClickHouse.Backup as Backup
import qualified Salmon.Builtin.Nodes.ClickHouse.Cluster as Cluster
import qualified Salmon.Builtin.Nodes.ClickHouse.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

main :: IO ()
main = do
    let desc =
            fullDesc
                <> progDesc "A ClickHouse cluster under rootless podman: Keeper, replicas, migrations ON CLUSTER, scheduled backups and a restore drill"
                <> header "salmon-toy-clickhouse"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd

-------------------------------------------------------------------------------
-- Seed

data Seed
    = SeedUp
        { seedRoot :: FilePath
        , seedPrefix :: Text
        , seedImage :: Text
        , seedMigrations :: FilePath
        , seedSchedules :: [Text]
        , seedGcs :: Maybe GcsSpec
        }
    | SeedDrill
        { seedRoot :: FilePath
        , seedPrefix :: Text
        , seedImage :: Text
        , seedBackup :: Maybe Text
        , seedGcs :: Maybe GcsSpec
        , seedMayDiscard :: Bool
        }

instance ParseRecord Seed where
    parseRecord = combo <**> helper
      where
        combo =
            subparser $
                mconcat
                    [ command
                        "up"
                        ( info
                            (SeedUp <$> rootOpt <*> prefixOpt <*> imageOpt <*> migrationsOpt <*> schedulesOpt <*> gcsOpt)
                            (progDesc "the network, three servers with a Keeper ensemble, cluster main, database toy, the migrations ON CLUSTER, and a scheduled backup")
                        )
                    , command
                        "drill"
                        ( info
                            (SeedDrill <$> rootOpt <*> prefixOpt <*> imageOpt <*> backupOpt <*> gcsOpt <*> discardOpt)
                            (progDesc "a server of its own, and database toy restored into it from one backup file")
                        )
                    ]
        rootOpt = strOption (long "root" <> help "the directory holding the toy's configuration, data, secrets and backups")
        prefixOpt = strOption (long "prefix" <> help "names the containers, the network and the backup job" <> value "chtoy")
        imageOpt = strOption (long "image" <> help "the server image" <> value "docker.io/clickhouse/clickhouse-server:25.8")
        migrationsOpt = strOption (long "migrations" <> help "a directory of .sql migrations, applied in file-name order; {{on_cluster}} becomes ON CLUSTER `main`")
        schedulesOpt = defaultSchedules <$> many (strOption (long "schedule" <> help "a systemd OnCalendar expression for the backup (repeatable; default: hourly)"))
        defaultSchedules [] = ["hourly"]
        defaultSchedules xs = xs
        backupOpt = optional (strOption (long "backup" <> help "the backup file to restore (default: the newest of this toy's in ROOT/backups)"))
        discardOpt = switch (long "may-discard" <> help "replace a toy database on the drill server that is not this restore")
        gcsOpt =
            optional
                ( GcsSpec
                    <$> strOption (long "gcs-bucket" <> help "ship backups to (or, for drill, fetch from) this bucket; off when not given")
                    <*> strOption (long "gcs-project" <> help "the bucket's project")
                    <*> strOption (long "gcs-prefix" <> help "a prefix in the bucket" <> value "")
                    <*> optional (strOption (long "gcs-credentials" <> help "a pre-provisioned credential file gcloud reads (its path only is passed)"))
                )

-------------------------------------------------------------------------------
-- Spec

-- | Where the bucket is; the toy never creates or deletes it.
data GcsSpec = GcsSpec
    { gcsSpecBucket :: Text
    , gcsSpecProject :: Text
    , gcsSpecPrefix :: Text
    , gcsSpecCredentials :: Maybe FilePath
    }
    deriving (Eq, Show, Generic)

instance FromJSON GcsSpec
instance ToJSON GcsSpec

-- | The facts every seed resolves: names, the image, and this user's unit directories.
data Toy = Toy
    { toyRoot :: FilePath
    -- ^ absolute
    , toyPrefix :: Text
    , toyImage :: Text
    , toyQuadletDir :: FilePath
    -- ^ @~\/.config\/containers\/systemd@, where the user generator looks
    , toyUnitDir :: FilePath
    -- ^ @~\/.config\/systemd\/user@, for the backup job and its timer
    }
    deriving (Eq, Show, Generic)

instance FromJSON Toy
instance ToJSON Toy

data Spec
    = Up
        { specToy :: Toy
        , specMigrations :: [Migrations.Migration]
        , specSchedules :: [Text]
        , specGcs :: Maybe GcsSpec
        }
    | Drill
        { specToy :: Toy
        , specBackup :: Text
        , specGcs :: Maybe GcsSpec
        , specMayDiscard :: Bool
        }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> case seed of
    SeedUp root prefix image dir schedules gcs -> do
        toy <- toyFacts root prefix image
        -- absolute: @up@ reads each file again, maybe from another directory
        ms <- Migrations.loadMigrationsDir =<< makeAbsolute dir
        pure (Up toy ms schedules gcs)
    SeedDrill root prefix image backup gcs discard -> do
        toy <- toyFacts root prefix image
        file <- case backup of
            Just f -> pure f
            Nothing -> do
                found <- listBackups toy
                maybe (fail ("no backup of " <> Text.unpack prefix <> " in " <> backupsDir toy <> ": run `up` first, or pass --backup")) pure (latestBackup toy found)
        pure (Drill toy file gcs discard)
  where
    toyFacts root prefix image = do
        absRoot <- makeAbsolute root
        quadlets <- getXdgDirectory XdgConfig ("containers" </> "systemd")
        units <- getXdgDirectory XdgConfig ("systemd" </> "user")
        pure (Toy absRoot prefix image quadlets units)

{- | The backup directory's listing. It belongs to the containers' user (a
subordinate uid), so it is read through @podman unshare@; a directory that
is not there yet lists nothing.
-}
listBackups :: Toy -> IO [Text]
listBackups toy = do
    (code, out, _) <- readProcessWithExitCode "podman" ["unshare", "ls", "-1", backupsDir toy] ""
    pure $ case code of
        ExitSuccess -> Text.lines (Text.pack out)
        _ -> []

-- | The newest of this toy's backup files among these names (their names sort by time).
latestBackup :: Toy -> [Text] -> Maybe Text
latestBackup toy names =
    case reverse (sort [n | n <- names, (toyPrefix toy <> "-") `Text.isPrefixOf` n, ".zip" `Text.isSuffixOf` n]) of
        (n : _) -> Just n
        [] -> Nothing

program :: Track' Spec
program = Track $ \spec -> case spec of
    Up toy ms schedules gcs -> upOp toy ms schedules gcs
    Drill toy file gcs discard -> drillOp toy file gcs discard

-------------------------------------------------------------------------------
-- Names and paths

-- | The values the Layer 0 tests use.
exampleToy :: Toy
exampleToy = Toy "/home/operator/chtoy" "chtoy" "docker.io/clickhouse/clickhouse-server:25.8" "/home/operator/.config/containers/systemd" "/home/operator/.config/systemd/user"

memberNames :: Toy -> [Text]
memberNames toy = [toyPrefix toy <> "-" <> Text.pack (show i) | i <- [1 :: Int .. 3]]

drillName :: Toy -> Text
drillName toy = toyPrefix toy <> "-drill"

networkName :: Toy -> Text
networkName toy = toyPrefix toy <> "-net"

toyDatabase :: ClickHouse.DatabaseName
toyDatabase = "toy"

clusterName :: Cluster.ClusterName
clusterName = "main"

secretsFile :: Toy -> FilePath
secretsFile toy = toyRoot toy </> "secrets" </> "cluster.env"

backupsDir :: Toy -> FilePath
backupsDir toy = toyRoot toy </> "backups"

backupJobDir :: Toy -> FilePath
backupJobDir toy = toyRoot toy </> "backup-job"

serverDir :: Toy -> Text -> FilePath
serverDir toy name = toyRoot toy </> Text.unpack name

-- | The environment variables 'secretsFile' defines.
secretEnv, interserverEnv :: Text
secretEnv = "CH_CLUSTER_SECRET"
interserverEnv = "CH_INTERSERVER_PASSWORD"

-------------------------------------------------------------------------------
-- The cluster

-- | One shard of the three members, the shared secret from the env file.
toyCluster :: Toy -> Cluster.Cluster
toyCluster toy = (Cluster.cluster clusterName [Cluster.shardOf (memberNames toy)]){Cluster.clusterSecretEnv = Just secretEnv}

keeperPeers :: Toy -> [Cluster.KeeperPeer]
keeperPeers = Cluster.keeperPeersFor . memberNames

-- | Member @i@ (1 to 3): an embedded Keeper member, cluster @main@, on the toy's network, reading the env file.
memberServer :: Toy -> Int -> ClickHouse.Server
memberServer toy i =
    Cluster.embeddedKeeperMember (Cluster.keeperMember peers i) cfg base
  where
    name = memberNames toy !! (i - 1)
    peers = keeperPeers toy
    cfg =
        (Cluster.memberConfig (toyCluster toy) (Cluster.keeperNodesFor peers) "01" name name)
            { Cluster.clusterInterserverCredentials = Just (Cluster.InterserverCredentials "salmon-interserver" interserverEnv)
            }
    base =
        (ClickHouse.server (Podman.ContainerName name) (toyImage toy) (serverDir toy name </> "config") (serverDir toy name </> "data"))
            { ClickHouse.serverEnvFile = Just (secretsFile toy)
            , ClickHouse.serverNetwork = Just (networkName toy)
            , ClickHouse.serverReadyTimeout = 120
            }

memberClient :: Toy -> Int -> ClickHouse.Client
memberClient toy i = ClickHouse.serverClient (memberServer toy i)

-- | The user's quadlet directory and scope, and no start at boot: a toy.
userScope :: Toy -> Quadlet.Container -> Quadlet.Container
userScope toy c =
    c
        { Quadlet.containerScope = Systemd.User
        , Quadlet.containerUnitDir = toyQuadletDir toy
        , Quadlet.containerWantedBy = Nothing
        , Quadlet.containerStartTimeout = Just 300
        }

upOp :: Toy -> [Migrations.Migration] -> [Text] -> Maybe GcsSpec -> Op
upOp toy ms schedules gcs =
    op "clickhouse-toy" (deps [backup]) $ \actions ->
        actions
            { Extension.help = "the ClickHouse toy " <> toyPrefix toy <> ": three members, cluster main, database toy, migrations, backups"
            , notes = ["root: " <> Text.pack (toyRoot toy), "members: " <> Text.intercalate ", " (memberNames toy)]
            , ref = mkRef "clickhouse-toy" (toyPrefix toy)
            }
  where
    secrets = inventSecrets toy
    net = Podman.network reportPrint ignoreTrack (Podman.NetworkName (networkName toy))
    -- the env file and the network are read when each quadlet is written
    -- and started, so they are prerequisites of its drop-ins, not injected
    servers = fmap serverNode [1 .. 3]
    serverNode :: Int -> Op
    serverNode 1 = Backup.backupServerOpIn (userScope toy) [secrets, net] reportPrint ignoreTrack (backupsDir toy) (memberServer toy 1)
    serverNode i = ClickHouse.serverOpIn (userScope toy) [secrets, net] reportPrint ignoreTrack (memberServer toy i)
    quorum =
        foldl
            inject
            (Cluster.keeperQuorum (toyPrefix toy) [(ClickHouse.InContainer (Podman.ContainerName n), Cluster.defaultKeeperPort) | n <- memberNames toy] 180)
            servers
    ready =
        concat
            [ [ Cluster.keeperConnected (memberClient toy i) 120 `inject` quorum
              , Cluster.clusterVisible (memberClient toy i) (toyCluster toy) 120 `inject` quorum
              ]
            | i <- [1 .. 3]
            ]
    database =
        foldl inject (Cluster.clusterDatabase reportPrint ignoreTrack (memberClient toy 1) toyDatabase (Cluster.OnCluster clusterName)) ready
    migrations =
        Migrations.migrations
            reportPrint
            ignoreTrack
            (memberClient toy 1)
            [database]
            (toyMigrations ms)
    backup =
        Backup.scheduledBackup
            reportPrint
            ignoreTrack
            (Track (const migrations))
            (toyBackupJob toy)
            (toyBackupPlan toy gcs)
            (fmap Job.OnCalendar schedules)

-- | The ledger in @toy@, the clause @ON CLUSTER `main`@ ('Cluster.onClusterClause').
toyMigrations :: [Migrations.Migration] -> Migrations.Migrations
toyMigrations ms =
    Migrations.Migrations
        { Migrations.migrationsDatabase = toyDatabase
        , Migrations.migrationsOnCluster = Just (Cluster.onClusterClause clusterName)
        , Migrations.migrationsFiles = ms
        }

-- | Database @toy@ of member 1, every local file kept (see the module header), shipped when a bucket is given.
toyBackupPlan :: Toy -> Maybe GcsSpec -> Backup.BackupPlan
toyBackupPlan toy gcs =
    (Backup.backupPlan (toyPrefix toy) (memberClient toy 1) [toyDatabase] (backupsDir toy) (backupJobDir toy))
        { Backup.planGcs = fmap gcsTarget gcs
        , Backup.planTimeout = Just 600
        }

-- | The job in this user's systemd.
toyBackupJob :: Toy -> Job.Job -> Job.Job
toyBackupJob toy j = j{Job.jobScope = Systemd.User, Job.jobUnitDir = toyUnitDir toy}

gcsTarget :: GcsSpec -> Backup.GcsTarget
gcsTarget g =
    Backup.GcsTarget
        { Backup.gcsBucket =
            Storage.Bucket
                { Storage.bucketName = gcsSpecBucket g
                , Storage.bucketProject = Core.Project (gcsSpecProject g)
                , -- the toy neither creates nor describes the bucket: no command it runs reads these
                  Storage.bucketLocation = Core.Region ""
                , Storage.bucketUniformBucketLevelAccess = True
                }
        , Backup.gcsPrefix = gcsSpecPrefix g
        , Backup.gcsCredentialFile = gcsSpecCredentials g
        }

-------------------------------------------------------------------------------
-- The drill

{- | The drill's server: no network, its own one-member Keeper and a
one-member cluster @drill@ on @localhost@ (shard @01@ as the cluster's, so
the replicated tables' paths read the same), the backup directory bound as
member 1's is.
-}
drillServer :: Toy -> ClickHouse.Server
drillServer toy =
    Cluster.embeddedKeeperMember (Cluster.keeperMember peers 1) cfg base
  where
    name = drillName toy
    peers = Cluster.keeperPeersFor ["localhost"]
    cfg = Cluster.memberConfig (Cluster.cluster "drill" [Cluster.shardOf ["localhost"]]) (Cluster.keeperNodesFor peers) "01" name "localhost"
    base =
        (ClickHouse.server (Podman.ContainerName name) (toyImage toy) (serverDir toy name </> "config") (serverDir toy name </> "data"))
            { ClickHouse.serverReadyTimeout = 120
            }

toyRestore :: Toy -> Text -> Maybe GcsSpec -> Bool -> Backup.Restore
toyRestore toy file gcs discard =
    (Backup.restoreFrom (ClickHouse.serverClient (drillServer toy)) (backupsDir toy) file toyDatabase)
        { Backup.restoreSource = maybe Backup.LocalBackup (Backup.FromGcs . gcsTarget) gcs
        , Backup.restoreMayDiscard = discard
        }

drillOp :: Toy -> Text -> Maybe GcsSpec -> Bool -> Op
drillOp toy file gcs discard =
    restoring `inject` connected
  where
    s = drillServer toy
    client = ClickHouse.serverClient s
    srv = Backup.backupServerOpIn (userScope toy) [] reportPrint ignoreTrack (backupsDir toy) s
    quorum = Cluster.keeperQuorum (drillName toy) [(ClickHouse.InContainer (Podman.ContainerName (drillName toy)), Cluster.defaultKeeperPort)] 120 `inject` srv
    connected = Cluster.keeperConnected client 120 `inject` quorum
    restoring = Backup.restore reportPrint ignoreTrack ignoreTrack (toyRestore toy file gcs discard)

-------------------------------------------------------------------------------
-- Secrets

{- | The toy as the somebody else who provisions secrets: 'secretsFile' with
random values, written once (mode @0600@ before anything is in it) and never
rewritten, since a new secret is a restart of every member. Its check is
that the file exists; nothing reads it but podman. This is the only node
that knows the values, and it shows none of them.
-}
inventSecrets :: Toy -> Op
inventSecrets toy =
    op "toy-secret" nodeps $ \actions ->
        actions
            { Extension.help = "the cluster secret and interserver password in " <> Text.pack path
            , notes = ["random, written once by the toy; a deployment provisions this file itself", "variables: " <> secretEnv <> ", " <> interserverEnv]
            , ref = mkRef "toy-secret" path
            , check = do
                there <- doesFileExist path
                pure (if there then Success else Failure ("no " <> Text.pack path))
            , up = do
                there <- doesFileExist path
                unless there $ do
                    createDirectoryIfMissing True (takeDirectory path)
                    a <- randomHex 24
                    b <- randomHex 24
                    let partial = path <> ".partial"
                    Text.writeFile partial ""
                    setFileMode partial 0o600
                    Text.writeFile partial (renderSecrets a b)
                    renameFile partial path
            }
  where
    path = secretsFile toy

-- | The env file's contents.
renderSecrets :: Text -> Text -> Text
renderSecrets secret password =
    Text.unlines [secretEnv <> "=" <> secret, interserverEnv <> "=" <> password]

randomHex :: Int -> IO Text
randomHex n = do
    bytes <- withBinaryFile "/dev/urandom" ReadMode (\h -> ByteString.hGet h n)
    unless (ByteString.length bytes == n) $ throwIO (userError "short read from /dev/urandom")
    pure (Text.pack (concatMap (printf "%02x") (ByteString.unpack bytes)))
