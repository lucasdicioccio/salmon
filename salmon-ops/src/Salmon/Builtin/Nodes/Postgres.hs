{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Salmon.Builtin.Nodes.Postgres where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import System.FilePath
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), justInstall, untrackedExec, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem (File, withFile)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------
-- todo: collapse PsqlAdmin commands as a single constructor
data Report
    = PGStartLocalCluster !Binary.Report
    | PGCreateDatabase !Database !Binary.Report
    | PGCreateUser !User !Binary.Report
    | PGSetUserPass !User !Binary.Report
    | PGCreateGroup !Group !Binary.Report
    | PGGrant !AccessRight !Binary.Report
    | PGGroupMembership !Group !Role !Binary.Report
    | PGDatabaseOwnership !Database !Role !Binary.Report
    | PGScript !FilePath !Binary.Report
    | PGAdminScript !FilePath !Binary.Report
    | PGChmod !FilePath !Binary.Report
    | PGCreateReplicationUser !User !Binary.Report
    | PGClusterOp !PgCtl !Binary.Report
    | PGAlterSystem !Text !Text !Binary.Report
    | PGReloadConf !Binary.Report
    | PGReplicationSlot !Text !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
type Host = Text
type Port = Int

-- | todo: distinguish server (to which we connect to) and cluster (with version)
data Server
    = Server
    { serverHost :: Host
    , serverPort :: Port
    }
    deriving (Generic)

instance ToJSON Server
instance FromJSON Server

localServer :: Server
localServer = Server "127.0.0.1" 5432

type Version = Int

{- | Which Debian package's default postgres major version ends up installed
varies by release (e.g. 13 on bullseye, 15 on bookworm) and shifts over
time, so we can't bake a version number in here — instead the started
command detects, on the target machine at 'up' time, whichever cluster
"postgresql-common" actually created (via @pg_lsclusters@) and starts that
one. If several versions are installed, the highest one wins.
-}
pgLocalCluster :: Reporter Report -> Track' (Binary "postgres") -> Track' (Binary "pg_ctlcluster") -> Server -> Op
pgLocalCluster r pg pgctl server =
    withBinary pgctl pgctlRun (Start server.serverPort) $ \start ->
        op "pg-server" (deps [justInstall pg]) $ \actions ->
            actions
                { notes = ["default server"]
                , help = Text.unwords ["start pg cluster"]
                , up = start r'
                }
  where
    r' = contramap PGStartLocalCluster r

type ClusterName = Text

mainCluster :: ClusterName
mainCluster = "main"

data PgCtl
    = Start Port
    | CreateCluster !ClusterName !Port
    | StartCluster !ClusterName
    | StopCluster !ClusterName
    | RestartCluster !ClusterName
    | PromoteCluster !ClusterName
    | EnsureHbaLine !ClusterName !Text
    | CloneFromPrimary !StandbySetup
    deriving (Show)

pgctlRun :: Command "pg_ctlcluster" PgCtl
pgctlRun = Command go
  where
    go (Start _port) = proc "bash" ["-c", detectVersionAndStartMainCluster]
    go (CreateCluster name port) = proc "bash" ["-c", createClusterScript name port]
    go (StartCluster name) = proc "bash" ["-c", clusterCtlScript name "start"]
    go (StopCluster name) = proc "bash" ["-c", clusterCtlScript name "stop"]
    go (RestartCluster name) = proc "bash" ["-c", clusterCtlScript name "restart"]
    go (PromoteCluster name) = proc "bash" ["-c", clusterCtlScript name "promote"]
    go (EnsureHbaLine name line) = proc "bash" ["-c", ensureHbaLineScript name line]
    go (CloneFromPrimary setup) = proc "bash" ["-c", cloneFromPrimaryScript setup]

{- | @pg_lsclusters@'s header-less output is one line per cluster:
@Ver Cluster Port Status Owner DataDirectory LogFile@; sort numerically and
take the highest version so a freshly-provisioned box with a single cluster
just works, and a box with several installed versions picks the newest.
-}
detectVersionAndStartMainCluster :: String
detectVersionAndStartMainCluster =
    "set -e; version=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1); pg_ctlcluster \"$version\" main start"

-- | Shared preamble: detects the (single) installed major version, same way as 'detectVersionAndStartMainCluster'.
detectVersion :: String
detectVersion = "version=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1)"

-- | Idempotent: only creates the named cluster if @pg_lsclusters@ doesn't already list it.
createClusterScript :: ClusterName -> Port -> String
createClusterScript name port =
    unlines
        [ "set -e"
        , detectVersion
        , "pg_lsclusters --no-header | awk '{print $2}' | grep -qx " <> shellQuote name <> " || pg_createcluster \"$version\" " <> Text.unpack name <> " -p " <> show port <> " -- --auth-local=peer --auth-host=md5"
        ]

clusterCtlScript :: ClusterName -> String -> String
clusterCtlScript name action =
    unlines
        [ "set -e"
        , detectVersion
        , "pg_ctlcluster \"$version\" " <> Text.unpack name <> " " <> action
        ]

-- | Appends a @pg_hba.conf@ line for the named cluster (skipping if already present) and reloads it.
ensureHbaLineScript :: ClusterName -> Text -> String
ensureHbaLineScript name line =
    unlines
        [ "set -e"
        , detectVersion
        , "hba=/etc/postgresql/$version/" <> Text.unpack name <> "/pg_hba.conf"
        , "grep -qxF " <> shellQuote line <> " \"$hba\" || echo " <> shellQuote line <> " >> \"$hba\""
        , "pg_ctlcluster \"$version\" " <> Text.unpack name <> " reload"
        ]

{- | Clones the named cluster's data directory from a running primary via
@pg_basebackup -R@ (which writes both @standby.signal@ and
@primary_conninfo@, so the cluster comes up in streaming-standby mode as
soon as it's started) and starts it. Guarded by @standby.signal@'s presence
so a second 'up' is a no-op instead of re-cloning (and destroying) an
already-running standby.
-}
cloneFromPrimaryScript :: StandbySetup -> String
cloneFromPrimaryScript setup =
    unlines
        [ "set -e"
        , detectVersion
        , "datadir=/var/lib/postgresql/$version/" <> Text.unpack setup.standby_cluster
        , "if [ ! -e \"$datadir/standby.signal\" ]; then"
        , "  pg_ctlcluster \"$version\" " <> Text.unpack setup.standby_cluster <> " stop || true"
        , "  rm -rf \"$datadir\""
        , "  PGPASSWORD="
            <> shellQuote setup.standby_repl_password.revealPassword
            <> " pg_basebackup -h "
            <> Text.unpack setup.standby_primary_host
            <> " -p "
            <> show setup.standby_primary_port
            <> " -U "
            <> Text.unpack setup.standby_repl_user.userRole
            <> " -D \"$datadir\" -Fp -Xs -R"
            <> slotArg
        , "  chown -R postgres:postgres \"$datadir\""
        , "  pg_ctlcluster \"$version\" " <> Text.unpack setup.standby_cluster <> " start"
        , "fi"
        ]
  where
    -- the slot (if any) is expected to already exist on the primary, created
    -- independently via 'replicationSlot'/'primaryReplicationSetup'
    slotArg = maybe "" (\slot -> " -S " <> Text.unpack slot) setup.standby_slot

shellQuote :: Text -> String
shellQuote t = "'" <> Text.unpack (Text.replace "'" "'\\''" t) <> "'"

type DatabaseName = Text

newtype Database = Database {getDatabase :: DatabaseName}
    deriving (Show, ToJSON, FromJSON)

database :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Port -> Database -> Op
database r server psql port db =
    withBinary psql (psqlAdminRun_Sudo port) (CreateDB db.getDatabase) $ \up ->
        op "pg-database" (deps [run server localServer]) $ \actions ->
            actions
                { ref = mkRef "pg-db" db.getDatabase
                , up = up r'
                , help = Text.unwords ["create db", db.getDatabase]
                }
  where
    r' = contramap (PGCreateDatabase db) r

type RoleName = Text

newtype Password = Password {revealPassword :: Text}

readPassword :: FilePath -> IO Password
readPassword path = Password <$> Text.readFile path

instance Show Password where
    show _ = "<password>"

newtype User = User {userRole :: RoleName}
    deriving (Show, ToJSON, FromJSON)

user :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Port -> User -> Password -> Op
user r server psql port user pwd =
    withBinary psql (psqlAdminRun_Sudo port) (CreateUser user.userRole pwd) $ \up ->
        op "pg-user" (deps [run server localServer]) $ \actions ->
            actions
                { ref = mkRef "pg-user" user.userRole
                , help = Text.unwords ["create user", user.userRole]
                , up = up r'
                }
  where
    r' = contramap (PGCreateUser user) r

userPassFile :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Port -> File "passfile" -> User -> Op
userPassFile r server psql port genpass user =
    withFile genpass $ \passfile ->
        op "pg-user" (deps [runningServer, justInstall psql]) $ \actions ->
            actions
                { ref = mkRef "pg-user" user.userRole
                , help = Text.unwords ["set user password for", user.userRole, "from file at", Text.pack passfile]
                , up = do
                    up =<< fmap Password (Text.readFile passfile)
                }
  where
    r' = contramap (PGSetUserPass user) r
    runningServer = run server localServer
    up pass = untrackedExec (psqlAdminRun_Sudo port) (CreateUser user.userRole pass) "" r'

data Group = Group {groupRole :: RoleName}
    deriving (Show, Generic)
instance ToJSON Group
instance FromJSON Group

group :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Port -> Group -> Op
group r server psql port group =
    withBinary psql (psqlAdminRun_Sudo port) (CreateGroup group.groupRole) $ \up ->
        op "pg-group" (deps [run server localServer]) $ \actions ->
            actions
                { ref = mkRef "pg-group" group.groupRole
                , up = up r'
                , help = Text.unwords ["creates group", group.groupRole]
                }
  where
    r' = contramap (PGCreateGroup group) r

data Role
    = UserRole User
    | GroupRole Group
    deriving (Show, Generic)
instance ToJSON Role
instance FromJSON Role

roleName :: Role -> RoleName
roleName (UserRole u) = u.userRole
roleName (GroupRole g) = g.groupRole

data PGRight
    = CREATE
    | CONNECT
    deriving (Show, Generic)
instance ToJSON PGRight
instance FromJSON PGRight

data AccessRight
    = AccessRight
    { access_database :: Database
    , access_role :: Role
    , access_rights :: [PGRight]
    }
    deriving (Show, Generic)
instance ToJSON AccessRight
instance FromJSON AccessRight

grant :: Reporter Report -> Track' (Binary "psql") -> Port -> Track' Role -> AccessRight -> Op
grant r psql port role acl =
    withBinary psql (psqlAdminRun_Sudo port) (Grant acl) $ \up ->
        op "pg-grant" (deps [dbrole]) $ \actions ->
            actions
                { ref = mkRef "pg-grant" (roleName acl.access_role)
                , up = up r'
                , help = Text.unwords ["grant", roleName acl.access_role]
                }
  where
    r' = contramap (PGGrant acl) r
    dbrole = run role acl.access_role

databaseOnwership ::
    Reporter Report ->
    Track' Server ->
    Track' (Binary "psql") ->
    Port ->
    Track' Database ->
    Database ->
    Track' Role ->
    Role ->
    Op
databaseOnwership r server psql port mkdb db role u =
    withBinary psql (psqlAdminRun_Sudo port) (DatabaseOwnership db.getDatabase (roleName u)) $ \up ->
        op "pg-member" (deps [run mkdb db, dbuser]) $ \actions ->
            actions
                { ref = mkRef "pg-ownership" (db.getDatabase, roleName u)
                , up = up r'
                , help = Text.unwords ["grant", db.getDatabase, "ownership to", roleName u]
                }
  where
    r' = contramap (PGDatabaseOwnership db u) r
    dbuser = run role u

groupMember :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Port -> Group -> Track' Role -> Role -> Op
groupMember r server psql port g role u =
    withBinary psql (psqlAdminRun_Sudo port) (GroupMembership g.groupRole (roleName u)) $ \up ->
        op "pg-member" (deps [dbgroup, dbuser]) $ \actions ->
            actions
                { ref = mkRef "pg-member" (g.groupRole, roleName u)
                , up = up r'
                , help = Text.unwords ["add", roleName u, "to", g.groupRole]
                }
  where
    r' = contramap (PGGroupMembership g u) r
    dbgroup = group r server psql port g
    dbuser = run role u

adminScript ::
    Reporter Report ->
    Track' (Binary "psql") ->
    Port ->
    Track' DatabaseName ->
    DatabaseName ->
    File "psql-script" ->
    Op
adminScript r psql port mkdb dbname file =
    withFile file $ \path ->
        let accessiblePath = adminDir </> path
         in withBinary psql (psqlAdminRun_Sudo port) (ChmodAdminScript accessiblePath) $ \chmod ->
                withBinary psql (psqlAdminRun_Sudo port) (AdminScript dbname accessiblePath) $ \up ->
                    op "pg-admin-script" (deps [run mkdb dbname, FS.fileCopy path accessiblePath `inject` enclosingdir]) $ \actions ->
                        actions
                            { ref = mkRef "pg-admin-script" path
                            , up = chmod (r1 accessiblePath) >> up (r2 accessiblePath)
                            , help = Text.unwords ["runs pg script", Text.pack path]
                            }
  where
    adminDir :: FilePath
    adminDir = "/opt/salmon/postgres/migrations/admin"
    enclosingdir :: Op
    enclosingdir = FS.dir (FS.Directory adminDir)

    r1 path = contramap (PGChmod path) r
    r2 path = contramap (PGAdminScript path) r

{- | commands to bootstrap PG roles and dbs as admin
expected to run as user "postgres" in Debian to handle the nopassword initial state
-}
data PsqlAdmin
    = CreateDB DatabaseName
    | CreateUser RoleName Password
    | CreateReplicationUser RoleName Password
    | CreateGroup RoleName
    | Grant AccessRight
    | GroupMembership RoleName RoleName
    | AdminScript DatabaseName FilePath
    | DatabaseOwnership DatabaseName RoleName
    | ChmodAdminScript FilePath
    | AlterSystemSet Text Text
    | ReloadConf
    | EnsurePhysicalReplicationSlot Text

{- | Every case connects to the locally-running cluster on 'port' explicitly
(via @-p@) rather than relying on @psql@'s default (which only ever reaches
whichever cluster happens to be on the default port, i.e. "main" — see
'Postgres.CreateDB' below and the "Conventions for node authors" note in
CLAUDE.md for why this matters once more than one named cluster exists on a
box).

todo: workaround chmod and sudo hack with some calling preference
- we'll need to request more than a Track' (Binary "psql") but some more complex logic
with sudo, the user, and the right binary
-}
psqlAdminRun_Sudo :: Port -> Command "psql" PsqlAdmin
psqlAdminRun_Sudo port = Command go
  where
    portArgs :: [String]
    portArgs = ["-p", show port]

    go (ChmodAdminScript path) =
        proc "chmod" ["a+r", path]
    go (AdminScript name path) =
        proc "sudo" (["-u", "postgres", "psql"] <> portArgs <> ["-f", path, Text.unpack name])
    -- CREATE DATABASE can't run inside a transaction/DO block (a hard Postgres
    -- restriction), so unlike the role-creation commands below, idempotency
    -- has to be a shell-level check-then-create rather than a SQL one.
    go (CreateDB name) =
        proc
            "sudo"
            [ "-u"
            , "postgres"
            , "bash"
            , "-c"
            , mconcat
                [ "psql -p "
                , show port
                , " -tAc \"SELECT 1 FROM pg_database WHERE datname = '"
                , Text.unpack name
                , "'\" | grep -q 1 || psql -p "
                , show port
                , " -c 'CREATE DATABASE "
                , Text.unpack name
                , "'"
                ]
            ]
    -- CREATE ROLE has no IF NOT EXISTS form, but (unlike CREATE DATABASE) it's
    -- fine inside a DO block, so we guard it with an explicit existence check.
    go (CreateUser name pass) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> [ "-c"
                   , mconcat
                        [ "DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '"
                        , Text.unpack name
                        , "') THEN CREATE ROLE "
                        , Text.unpack name
                        , " WITH LOGIN PASSWORD "
                        , quotePass pass
                        , "; END IF; END $$;"
                        ]
                   ]
            )
    go (CreateReplicationUser name pass) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> [ "-c"
                   , mconcat
                        [ "DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '"
                        , Text.unpack name
                        , "') THEN CREATE ROLE "
                        , Text.unpack name
                        , " WITH REPLICATION LOGIN PASSWORD "
                        , quotePass pass
                        , "; END IF; END $$;"
                        ]
                   ]
            )
    go (AlterSystemSet param val) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> ["-c", unwords ["ALTER SYSTEM SET", Text.unpack param, "=", "'" <> Text.unpack val <> "'"]]
            )
    go ReloadConf =
        proc "sudo" (["-u", "postgres", "psql"] <> portArgs <> ["-c", "SELECT pg_reload_conf();"])
    go (EnsurePhysicalReplicationSlot slot) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> [ "-c"
                   , mconcat
                        [ "DO $$ BEGIN IF NOT EXISTS (SELECT 1 FROM pg_replication_slots WHERE slot_name = '"
                        , Text.unpack slot
                        , "') THEN PERFORM pg_create_physical_replication_slot('"
                        , Text.unpack slot
                        , "'); END IF; END $$;"
                        ]
                   ]
            )
    go (CreateGroup name) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> [ "-c"
                   , mconcat
                        [ "DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '"
                        , Text.unpack name
                        , "') THEN CREATE ROLE "
                        , Text.unpack name
                        , "; END IF; END $$;"
                        ]
                   ]
            )
    go (GroupMembership g u) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> [ "-c"
                   , unwords
                        [ "ALTER GROUP"
                        , Text.unpack g
                        , "ADD USER"
                        , Text.unpack u
                        ]
                   ]
            )
    go (DatabaseOwnership d u) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> [ "-c"
                   , unwords
                        [ "ALTER DATABASE"
                        , Text.unpack d
                        , "OWNER TO"
                        , Text.unpack u
                        ]
                   ]
            )
    go (Grant acl) =
        proc
            "sudo"
            ( ["-u", "postgres", "psql"]
                <> portArgs
                <> [ "-c"
                   , unwords
                        [ "GRANT"
                        , Text.unpack $ commaList $ fmap renderRight acl.access_rights
                        , "ON DATABASE"
                        , Text.unpack acl.access_database.getDatabase
                        , "TO"
                        , Text.unpack (roleName acl.access_role)
                        ]
                   ]
            )

    quotePass :: Password -> String
    quotePass pwd = "'" <> Text.unpack pwd.revealPassword <> "'"

    renderRight :: PGRight -> Text
    renderRight CREATE = "CREATE"
    renderRight CONNECT = "CONNECT"

    commaList :: [Text] -> Text
    commaList = Text.intercalate ","

-------------------------------------------------------------------------------

data ConnString pass = ConnString
    { connstring_server :: Server
    , connstring_user :: User
    , connstring_user_pass :: pass
    , connstring_db :: Database
    }
    deriving (Generic, Functor)
instance (ToJSON a) => ToJSON (ConnString a)
instance (FromJSON a) => FromJSON (ConnString a)

type UnknownPassword = ()

connstring :: ConnString Password -> Text
connstring (ConnString server user pass db) =
    mconcat
        [ "postgresql://"
        , user.userRole
        , ":"
        , pass.revealPassword
        , "@"
        , server.serverHost
        , ":"
        , Text.pack $ show server.serverPort
        , "/"
        , db.getDatabase
        ]

withPassword :: ConnString a -> Password -> ConnString Password
withPassword c pass = const pass <$> c

userScriptInMemoryPass ::
    Reporter Report ->
    Track' (Binary "psql") ->
    Track' (ConnString Password) ->
    ConnString Password ->
    File "psql-script" ->
    Op
userScriptInMemoryPass r psql mksetup c@(ConnString server user pass db) file =
    withFile file $ \path ->
        withBinary psql (psqlUserRun c) (UserScript path) $ \up ->
            op "pg-script" (deps [run mksetup c]) $ \actions ->
                actions
                    { ref = mkRef "pg-script" path
                    , up = up (r' path)
                    , help = Text.unwords ["runs pg script", Text.pack path]
                    }
  where
    r' path = contramap (PGScript path) r

userScript ::
    Reporter Report ->
    Track' (Binary "psql") ->
    Track' (ConnString FilePath) ->
    ConnString FilePath ->
    File "psql-script" ->
    Op
userScript r psql mksetup c@(ConnString server user passFile db) file =
    withFile file $ \path ->
        op "pg-script" (deps [run mksetup c, justInstall psql]) $ \actions ->
            actions
                { ref = mkRef "pg-script" path
                , up = do
                    up path =<< fmap Password (Text.readFile passFile)
                , help = Text.unwords ["runs pg script", Text.pack path]
                }
  where
    r' path = contramap (PGScript path) r
    up path pass = untrackedExec (psqlUserRun $ c `withPassword` pass) (UserScript path) "" (r' path)

data PsqlUser
    = UserScript FilePath

psqlUserRun :: ConnString Password -> Command "psql" PsqlUser
psqlUserRun c = Command go
  where
    go (UserScript path) =
        proc
            "psql"
            [ Text.unpack $ connstring c
            , "-f"
            , path
            ]

-------------------------------------------------------------------------------
-- Cluster lifecycle (named, non-"main" clusters)

-- | @pg_createcluster@s a new, empty cluster under Debian's cluster management
-- (idempotent: a no-op if a cluster by that name already exists).
createCluster :: Reporter Report -> Track' (Binary "postgres") -> Track' (Binary "pg_ctlcluster") -> ClusterName -> Port -> Op
createCluster r pg pgctl name port =
    withBinary pgctl pgctlRun cmd $ \run ->
        op "pg-create-cluster" (deps [justInstall pg]) $ \actions ->
            actions
                { ref = mkRef "pg-create-cluster" name
                , help = Text.unwords ["creates pg cluster", name, "on port", Text.pack (show port)]
                , up = run r'
                }
  where
    cmd = CreateCluster name port
    r' = contramap (PGClusterOp cmd) r

clusterCtl :: Reporter Report -> Track' (Binary "pg_ctlcluster") -> ClusterName -> PgCtl -> Text -> Op
clusterCtl r pgctl name cmd label =
    withBinary pgctl pgctlRun cmd $ \run ->
        op "pg-cluster-ctl" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-cluster-ctl" (name, label)
                , help = Text.unwords [label, "pg cluster", name]
                , up = run r'
                }
  where
    r' = contramap (PGClusterOp cmd) r

startCluster, stopCluster, restartCluster, promoteCluster :: Reporter Report -> Track' (Binary "pg_ctlcluster") -> ClusterName -> Op
startCluster r pgctl name = clusterCtl r pgctl name (StartCluster name) "start"
stopCluster r pgctl name = clusterCtl r pgctl name (StopCluster name) "stop"
restartCluster r pgctl name = clusterCtl r pgctl name (RestartCluster name) "restart"
promoteCluster r pgctl name = clusterCtl r pgctl name (PromoteCluster name) "promote"

-------------------------------------------------------------------------------
-- Physical (WAL streaming) replication

{- | Settings a primary needs to accept streaming replicas. Debian's default
@postgresql.conf@ already ships @wal_level = replica@ on modern versions,
but we set it explicitly since a misconfigured value there is a silent
failure mode (replication just won't start).
-}
data ReplicationTuning
    = ReplicationTuning
    { repl_max_wal_senders :: Int
    , repl_max_replication_slots :: Int
    }
    deriving (Show)

defaultReplicationTuning :: ReplicationTuning
defaultReplicationTuning = ReplicationTuning 10 10

-- | A host or CIDR allowed to authenticate as the replication role, e.g. the standby's address.
type AllowedCidr = Text

type ReplicationSlotName = Text

-- | Everything needed to clone an empty (or freshly created) cluster off a running primary and start it as a streaming standby.
data StandbySetup
    = StandbySetup
    { standby_cluster :: ClusterName
    , standby_primary_host :: Host
    , standby_primary_port :: Port
    , standby_repl_user :: User
    , standby_repl_password :: Password
    , standby_slot :: Maybe ReplicationSlotName
    }
    deriving (Show)

-- | A login role carrying the @REPLICATION@ attribute, for a standby's @pg_basebackup@\/streaming connection.
replicationUser :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Port -> User -> Password -> Op
replicationUser r server psql port u pwd =
    withBinary psql (psqlAdminRun_Sudo port) (CreateReplicationUser u.userRole pwd) $ \up ->
        op "pg-replication-user" (deps [run server localServer]) $ \actions ->
            actions
                { ref = mkRef "pg-replication-user" u.userRole
                , help = Text.unwords ["create replication user", u.userRole]
                , up = up r'
                }
  where
    r' = contramap (PGCreateReplicationUser u) r

alterSystemSet :: Reporter Report -> Track' (Binary "psql") -> Port -> Text -> Text -> Op
alterSystemSet r psql port param val =
    withBinary psql (psqlAdminRun_Sudo port) (AlterSystemSet param val) $ \up ->
        op "pg-alter-system" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-alter-system" param
                , help = Text.unwords ["ALTER SYSTEM SET", param, "=", val]
                , up = up r'
                }
  where
    r' = contramap (PGAlterSystem param val) r

reloadConf :: Reporter Report -> Track' (Binary "psql") -> Port -> Op
reloadConf r psql port =
    withBinary psql (psqlAdminRun_Sudo port) ReloadConf $ \up ->
        op "pg-reload-conf" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-reload-conf" ("reload" :: Text)
                , up = up r'
                }
  where
    r' = contramap PGReloadConf r

-- | Ensures a physical replication slot exists on the primary (idempotent: skips if already present).
replicationSlot :: Reporter Report -> Track' (Binary "psql") -> Port -> ReplicationSlotName -> Op
replicationSlot r psql port slot =
    withBinary psql (psqlAdminRun_Sudo port) (EnsurePhysicalReplicationSlot slot) $ \up ->
        op "pg-replication-slot" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-replication-slot" slot
                , help = Text.unwords ["ensure replication slot", slot]
                , up = up r'
                }
  where
    r' = contramap (PGReplicationSlot slot) r

allowReplicationFrom :: Reporter Report -> Track' (Binary "pg_ctlcluster") -> ClusterName -> RoleName -> AllowedCidr -> Op
allowReplicationFrom r pgctl name replRole cidr =
    withBinary pgctl pgctlRun cmd $ \run ->
        op "pg-hba-replication" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-hba-replication" (name, replRole, cidr)
                , help = Text.unwords ["allow replication from", cidr, "as", replRole, "on", name]
                , up = run r'
                }
  where
    line = Text.unwords ["host", "replication", replRole, cidr, "md5"]
    cmd = EnsureHbaLine name line
    r' = contramap (PGClusterOp cmd) r

{- | Turns an already-running, named cluster into a replication-capable
primary: WAL/replication-slot tuning (restart-required, so this restarts the
cluster), a @pg_hba.conf@ entry authorizing the standby, and the physical
replication slot the standby will stream from. Does /not/ create the
replication role itself — do that once via 'replicationUser' (it's shared
infrastructure, not per-standby).
-}
primaryReplicationSetup ::
    Reporter Report ->
    Track' (Binary "psql") ->
    Track' (Binary "pg_ctlcluster") ->
    Port ->
    ClusterName ->
    ReplicationTuning ->
    RoleName ->
    AllowedCidr ->
    ReplicationSlotName ->
    Op
primaryReplicationSetup r psql pgctl port name tuning replRole cidr slot =
    op "pg-primary-replication-setup" (deps [replicationSlot r psql port slot, allowReplicationFrom r pgctl name replRole cidr, restartOp]) id
  where
    restartOp = restartCluster r pgctl name `inject` applySettings
    applySettings = op "pg-primary-wal-settings" (deps $ fmap (uncurry (alterSystemSet r psql port)) settings) id
    settings =
        [ ("wal_level", "replica")
        , ("max_wal_senders", tshow tuning.repl_max_wal_senders)
        , ("max_replication_slots", tshow tuning.repl_max_replication_slots)
        , ("listen_addresses", "*")
        ]
    tshow = Text.pack . show

-- | Clones 'StandbySetup's cluster off its primary via @pg_basebackup -R@ and starts it as a streaming standby.
standbyReplicationSetup :: Reporter Report -> Track' (Binary "pg_ctlcluster") -> StandbySetup -> Op
standbyReplicationSetup r pgctl setup =
    withBinary pgctl pgctlRun cmd $ \run ->
        op "pg-standby-setup" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-standby-setup" setup.standby_cluster
                , help = Text.unwords ["clone", setup.standby_cluster, "from", setup.standby_primary_host, "as a streaming standby"]
                , up = run r'
                }
  where
    cmd = CloneFromPrimary setup
    r' = contramap (PGClusterOp cmd) r
