{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Salmon.Builtin.Nodes.Postgres where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Actions.UpDown (CheckResult (..))

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), justInstall, untrackedExec, withBinary, withBinaryStdin)
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
    | PGTemplate !DatabaseName !Binary.Report
    | PGCloneDatabase !Clone !Binary.Report
    | PGDropClone !DatabaseName !Binary.Report
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
    deriving (Eq, Ord, Show, Generic)

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
    -- `pg_ctlcluster start` exits 2 on a cluster that is already running, so
    -- starting unconditionally failed every pass after the first (and every
    -- pass on a box where apt's own service had started it).
    "set -e; version=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1); pg_ctlcluster \"$version\" main status >/dev/null || pg_ctlcluster \"$version\" main start"

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
                { -- keyed by port as well as name, for the reason given at
                  -- 'alterSystemSet': two clusters on one box each hold their
                  -- own "appdb". 'cloneDatabase' uses the same key on
                  -- purpose, since a clone is a database at the same site.
                  ref = mkRef "pg-db" (port, db.getDatabase)
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
                , up = if null acl.access_rights then pure () else up r'
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
                { -- keyed by port as well as parameter: a box running two
                  -- clusters has two genuinely different settings of the same
                  -- name, and keying on the name alone deduped them into one
                  -- node, silently dropping whichever was declared second.
                  ref = mkRef "pg-alter-system" (port, param)
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
                { -- same reasoning as 'alterSystemSet': one reload per
                  -- cluster, not one reload for the whole machine.
                  ref = mkRef "pg-reload-conf" port
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

{- | Ensures an arbitrary line is present in a cluster's @pg_hba.conf@, then
reloads it.

'allowReplicationFrom' and 'allowClientCertFrom' are the two lines this repo
has an opinion about; this is the escape hatch for the rest of
@pg_hba.conf@'s vocabulary, which is large and changes between major
versions. The line is matched verbatim (@grep -qxF@), so a line differing
only in whitespace is a /second/ line rather than an update of the first --
which is also why a caller changing its mind leaves the old line behind.
-}
hbaLine :: Reporter Report -> Track' (Binary "pg_ctlcluster") -> ClusterName -> Text -> Op
hbaLine r pgctl name line =
    withBinary pgctl pgctlRun cmd $ \run ->
        op "pg-hba-line" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-hba-line" (name, line)
                , help = Text.unwords ["ensure pg_hba line on", name <> ":", line]
                , notes = ["appended verbatim; changing it leaves the previous line in place"]
                , up = run r'
                }
  where
    cmd = EnsureHbaLine name line
    r' = contramap (PGClusterOp cmd) r

{- | Authenticates a role by __client certificate only__, over TLS:
@hostssl \<db\> \<role\> \<cidr\> cert clientcert=verify-full@.

Two properties make this the interesting @pg_hba@ line rather than just
another one. @hostssl@ refuses a plaintext connection outright, so there is
no password path left to get wrong, and @clientcert=verify-full@ requires the
certificate's @CN@ to __equal the role name__ -- which turns "who may connect
as this role" into "who holds a certificate this cluster's CA issued for that
name", with no secret on the client that is not also a key.

The cluster must already be serving TLS and trusting the right CA for this to
be usable at all; that is 'serverTls'. A @hostssl@ line on a cluster with
@ssl = off@ is accepted by @pg_hba.conf@ and matches nothing.
-}
allowClientCertFrom ::
    Reporter Report ->
    Track' (Binary "pg_ctlcluster") ->
    ClusterName ->
    DatabaseName ->
    RoleName ->
    AllowedCidr ->
    Op
allowClientCertFrom r pgctl name db role cidr =
    hbaLine r pgctl name (Text.unwords ["hostssl", db, role, cidr, "cert", "clientcert=verify-full"])

{- | Where a cluster's TLS material lives. Paths are on the /database/ host,
and the key must be readable by the @postgres@ user and by nobody else --
see "Salmon.Builtin.Nodes.Filesystem".@ownedFile@, which exists for this.
-}
data ServerTls
    = ServerTls
    { tls_certFile :: FilePath
    , tls_keyFile :: FilePath
    , tls_caFile :: FilePath
    -- ^ the CA whose certificates this cluster will accept from clients.
    }
    deriving (Eq, Ord, Show)

{- | Turns TLS on for a cluster and points it at its certificate, key and
client CA.

All four settings are @sighup@-able, so this reloads rather than restarting:
a cluster serving traffic picks up a renewed certificate without dropping a
connection. (That also means a __broken__ certificate is not noticed until
something tries to connect, since the reload itself succeeds.)
-}
serverTls :: Reporter Report -> Track' (Binary "psql") -> Port -> ServerTls -> Op
serverTls r psql port tls =
    op "pg-server-tls" (deps [reload]) $ \actions ->
        actions
            { ref = mkRef "pg-server-tls" (port, tls.tls_certFile)
            , help = Text.unwords ["serves TLS on port", Text.pack (show port)]
            }
  where
    reload = foldl inject (reloadConf r psql port) settings
    settings =
        [ alterSystemSet r psql port "ssl" "on"
        , alterSystemSet r psql port "ssl_cert_file" (Text.pack tls.tls_certFile)
        , alterSystemSet r psql port "ssl_key_file" (Text.pack tls.tls_keyFile)
        , alterSystemSet r psql port "ssl_ca_file" (Text.pack tls.tls_caFile)
        ]

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

-------------------------------------------------------------------------------
-- Template databases, and databases cloned from them

{- $templates
@CREATE DATABASE c TEMPLATE t@ copies @t@ at the file level, which is how a
database that took a whole migration history to build is handed out in a
second. What makes that safe to automate is almost entirely about who else
is touching @t@:

* Nothing may be connected to @t@ while it is copied, or the copy fails with
  "source database is being accessed by other users". A template is therefore
  /locked/ once built: @ALLOW_CONNECTIONS false@, and any session still
  attached is terminated.
* @IS_TEMPLATE true@ lets a role with only @CREATEDB@ clone it, and makes
  @DROP DATABASE@ refuse, so every teardown has to flip it back first.

Both the template and its clones are databases salmon /drops/ -- to rebuild a
template, and to take a clone down -- so each carries a marker in its
database comment ('templateMarker', 'cloneMarker') and every statement that
would drop or adopt one refuses a database without it. That is what stops a
template or clone named after an existing database from replacing it: the
name is the caller's to choose, and nothing else about a database says who
made it.

The statements are fed to @psql@ on stdin rather than with @-c@, because
@CREATE DATABASE@ cannot run inside a transaction or a @DO@ block, and
@\\gexec@ is the one conditional form it tolerates. @DROP DATABASE ... WITH
(FORCE)@ needs Postgres 13 or later.
-}

-- | The comment prefix on a database salmon built as a template.
templateMarker :: Text
templateMarker = "salmon-template:"

-- | The comment prefix on a database salmon cloned from a template.
cloneMarker :: Text
cloneMarker = "salmon-clone:"

-- | A double-quoted SQL identifier.
quoteIdent :: Text -> Text
quoteIdent t = "\"" <> Text.replace "\"" "\"\"" t <> "\""

-- | A single-quoted SQL string literal (with @standard_conforming_strings@, the default since 9.1).
quoteLiteral :: Text -> Text
quoteLiteral t = "'" <> Text.replace "'" "''" t <> "'"

{- | Dollar-quotes a @DO@ body with a tag that does not occur in it.

A bare @$$@ is broken out of by a database name containing @$$@, and
'quoteLiteral' does nothing about that because inside a dollar-quoted body
nothing is a literal yet.
-}
dollarQuote :: Text -> Text
dollarQuote body = tag <> body <> tag
  where
    -- the search terminates: a body of length n contains fewer than n tags
    tag = case [t | n <- [0 :: Int ..], let t = "$salmon" <> Text.pack (show n) <> "$", not (t `Text.isInfixOf` body)] of
        (t : _) -> t
        [] -> error "unreachable: infinitely many candidate tags"

-- | A batch of SQL, fed to @psql@ on stdin as the @postgres@ OS user.
data PsqlBatch = PsqlBatch

{- | @ON_ERROR_STOP@ is not optional: @psql@ reading a script carries on past
a failed statement and exits @0@, so without it a refused drop would be
followed by the @CREATE@ it was guarding, and the node would report success.
-}
psqlBatchRun_Sudo :: Port -> Command "psql" PsqlBatch
psqlBatchRun_Sudo port = Command go
  where
    go PsqlBatch =
        proc "sudo" ["-u", "postgres", "psql", "-p", show port, "-X", "-q", "-v", "ON_ERROR_STOP=1", "-d", "postgres"]

-- | Aborts the batch unless @name@ is absent or its comment starts with @marker@.
refuseUnmarked :: Text -> Text -> DatabaseName -> Text
refuseUnmarked marker verb name =
    "DO " <> dollarQuote body <> ";\n"
  where
    body =
        Text.unwords
            [ "BEGIN IF EXISTS (SELECT FROM pg_database WHERE datname =" <> quoteLiteral name
            , "AND coalesce(left(shobj_description(oid, 'pg_database'), " <> Text.pack (show (Text.length marker)) <> "), '') <>" <> quoteLiteral marker <> ")"
            , "THEN RAISE EXCEPTION '%'," <> quoteLiteral ("refusing to " <> verb <> " database " <> name <> ": salmon did not create it") <> ";"
            , "END IF; END"
            ]

-- | Drops a template salmon built, if it is there.
dropTemplateSql :: DatabaseName -> Text
dropTemplateSql name =
    refuseUnmarked templateMarker "drop" name
        <> Text.unlines
            [ "SELECT format('ALTER DATABASE %I IS_TEMPLATE false', datname) FROM pg_database WHERE datname = " <> quoteLiteral name <> " \\gexec"
            , "DROP DATABASE IF EXISTS " <> quoteIdent name <> " WITH (FORCE);"
            ]

{- | Starts a template build from nothing: whatever was there before is
dropped, and the fresh database is marked as a build in progress, so that a
build which dies half-way is recognisably salmon's to replace next time.
-}
prepareTemplateSql :: DatabaseName -> Text
prepareTemplateSql name =
    refuseUnmarked templateMarker "replace" name
        <> Text.unlines
            [ "SELECT format('ALTER DATABASE %I IS_TEMPLATE false', datname) FROM pg_database WHERE datname = " <> quoteLiteral name <> " \\gexec"
            , "DROP DATABASE IF EXISTS " <> quoteIdent name <> " WITH (FORCE);"
            , "CREATE DATABASE " <> quoteIdent name <> ";"
            , "COMMENT ON DATABASE " <> quoteIdent name <> " IS " <> quoteLiteral (templateMarker <> "building") <> ";"
            ]

{- | Finishes a build: stamps the inputs it was built from, locks it, and
evicts whatever is still connected -- a session left on the template is the
thing that makes the next clone fail.
-}
lockTemplateSql :: DatabaseName -> Text -> Text
lockTemplateSql name fingerprint =
    Text.unlines
        [ "COMMENT ON DATABASE " <> quoteIdent name <> " IS " <> quoteLiteral (templateMarker <> fingerprint) <> ";"
        , "ALTER DATABASE " <> quoteIdent name <> " WITH IS_TEMPLATE true ALLOW_CONNECTIONS false;"
        , "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = " <> quoteLiteral name <> " AND pid <> pg_backend_pid();"
        ]

-- | One row, @datistemplate|datallowconn|comment@, or none.
inspectTemplateSql :: DatabaseName -> Text
inspectTemplateSql name =
    "SELECT datistemplate, datallowconn, coalesce(shobj_description(oid, 'pg_database'), '') FROM pg_database WHERE datname = " <> quoteLiteral name

-- | Is @name@ a finished, locked template built from @fingerprint@.
checkTemplate :: Port -> DatabaseName -> Text -> IO CheckResult
checkTemplate port name fingerprint =
    either (const Unknown) (interpretTemplateRow name fingerprint) <$> psqlQuery_Sudo port (inspectTemplateSql name)

{- | The verdict 'checkTemplate' draws, split out so it is testable without a
cluster.

Everything short of "locked, and stamped with these inputs" is a 'Failure',
and every one of them means the same thing to the node -- rebuild -- but the
reasons are kept apart because they are different stories for an operator:
a template built from older migrations is routine, a half-built one means a
build died, and an unlocked one means somebody has been connected to it.
-}
interpretTemplateRow :: DatabaseName -> Text -> Text -> CheckResult
interpretTemplateRow name fingerprint out =
    case Text.lines (Text.strip out) of
        [] -> Failure ("template " <> name <> " does not exist")
        (row : _) -> case Text.splitOn "|" row of
            (istemplate : allowconn : rest) -> verdict istemplate allowconn (Text.intercalate "|" rest)
            _ -> Unknown
  where
    verdict istemplate allowconn comment
        | not (templateMarker `Text.isPrefixOf` comment) =
            Failure (name <> " exists and salmon did not build it as a template")
        | comment == templateMarker <> "building" =
            Failure ("template " <> name <> " is half-built: a previous build did not finish")
        | comment /= templateMarker <> fingerprint =
            Failure ("template " <> name <> " was built from different inputs")
        | istemplate /= "t" || allowconn /= "f" =
            Failure ("template " <> name <> " is not locked")
        | otherwise = Success

-- | A database copied from a template once, on creation.
data Clone
    = Clone
    { clone_database :: DatabaseName
    , clone_template :: DatabaseName
    , clone_owner :: Maybe RoleName
    -- ^ must already exist; 'Nothing' leaves it owned by @postgres@. The
    -- objects /inside/ keep whichever owners they had in the template.
    }
    deriving (Eq, Show, Generic)

instance ToJSON Clone
instance FromJSON Clone

cloneDatabaseSql :: Clone -> Text
cloneDatabaseSql c =
    refuseUnmarked cloneMarker "adopt" c.clone_database
        <> Text.unlines
            [ "SELECT " <> quoteLiteral create <> " WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = " <> quoteLiteral c.clone_database <> ") \\gexec"
            , "COMMENT ON DATABASE " <> quoteIdent c.clone_database <> " IS " <> quoteLiteral (cloneMarker <> c.clone_template) <> ";"
            ]
  where
    create =
        Text.unwords $
            ["CREATE DATABASE", quoteIdent c.clone_database, "TEMPLATE", quoteIdent c.clone_template]
                <> maybe [] (\o -> ["OWNER", quoteIdent o]) c.clone_owner

dropCloneSql :: DatabaseName -> Text
dropCloneSql name =
    refuseUnmarked cloneMarker "drop" name
        <> Text.unlines ["DROP DATABASE IF EXISTS " <> quoteIdent name <> " WITH (FORCE);"]

-- | One row, @row:\<comment\>@, or none; the prefix tells an uncommented database from a missing one.
inspectCloneSql :: DatabaseName -> Text
inspectCloneSql name =
    "SELECT 'row:' || coalesce(shobj_description(oid, 'pg_database'), '') FROM pg_database WHERE datname = " <> quoteLiteral name

checkClone :: Port -> DatabaseName -> IO CheckResult
checkClone port name =
    either (const Unknown) (interpretCloneRow name) <$> psqlQuery_Sudo port (inspectCloneSql name)

{- | A clone that exists is satisfied whichever template it came from: a
clone is somebody's data from the moment it is made, and re-declaring it
from a newer template is not a reason to throw that away.
-}
interpretCloneRow :: DatabaseName -> Text -> CheckResult
interpretCloneRow name out =
    case Text.lines (Text.strip out) of
        [] -> Failure ("database " <> name <> " does not exist")
        (row : _)
            | (("row:" <> cloneMarker) `Text.isPrefixOf` row) -> Success
            | otherwise -> Failure (name <> " exists and salmon did not clone it")

{- | What taking a clone down does to its data.

The choice belongs to the declaration rather than to the clone, because the
same database changes hands during its life. A clone backing a pull
request's environment should survive that environment being torn down and
redeployed while the PR is open (somebody's test data is in it), and should
go once the PR is merged. That is one database declared 'Retain' and later
'Discard' -- see 'retainedClone' and 'disposableClone'.
-}
data Retention
    = -- | @down@ leaves the database in place.
      Retain
    | -- | @down@ drops the database, data included.
      Discard
    deriving (Eq, Show, Generic)

instance ToJSON Retention
instance FromJSON Retention

{- | A database copied from a template.

The copy is taken __once__: a template rebuilt later does not reach an
existing clone. To pick up a newer template, take the clone down with
'Discard' and bring it up again.

With 'Discard', @down@ refuses a database salmon did not clone, same as @up@
refuses to adopt one. The 'Retention' is in the node's @notes@, so under
@run serve@ re-declaring a clone with the other one is seen as a change to
it, and a graph holding both is reported as a conflict.
-}
cloneDatabase :: Reporter Report -> Track' (Binary "psql") -> Port -> Track' DatabaseName -> Retention -> Clone -> Op
cloneDatabase r psql port mktemplate retention c =
    withBinaryStdin psql (psqlBatchRun_Sudo port) PsqlBatch (Text.encodeUtf8 (cloneDatabaseSql c)) $ \create ->
        withBinaryStdin psql (psqlBatchRun_Sudo port) PsqlBatch (Text.encodeUtf8 (dropCloneSql c.clone_database)) $ \dropIt ->
            op "pg-clone" (deps [run mktemplate c.clone_template]) $ \actions ->
                actions
                    { ref = mkRef "pg-db" (port, c.clone_database)
                    , help = Text.unwords ["clone", c.clone_database, "from template", c.clone_template]
                    , notes = ["copied once: rebuilding the template does not refresh it", retentionNote]
                    , check = checkClone port c.clone_database
                    , up = create (contramap (PGCloneDatabase c) r)
                    , down = case retention of
                        Retain -> pure ()
                        Discard -> dropIt (contramap (PGDropClone c.clone_database) r)
                    }
  where
    retentionNote = case retention of
        Retain -> "down keeps the database"
        Discard -> "down drops the database"

-- | A clone whose data outlives a teardown: an open PR's environment.
retainedClone :: Reporter Report -> Track' (Binary "psql") -> Port -> Track' DatabaseName -> Clone -> Op
retainedClone r psql port mktemplate = cloneDatabase r psql port mktemplate Retain

-- | A clone that goes with its teardown: a test fixture, or a PR's environment once merged.
disposableClone :: Reporter Report -> Track' (Binary "psql") -> Port -> Track' DatabaseName -> Clone -> Op
disposableClone r psql port mktemplate = cloneDatabase r psql port mktemplate Discard

{- | Runs one query as the @postgres@ OS user, unaligned and tuples-only.

@Left@ when it could not be asked at all (cluster down, no @psql@), which the
checks above read as 'Unknown' rather than as the effect being absent.
-}
psqlQuery_Sudo :: Port -> Text -> IO (Either Text Text)
psqlQuery_Sudo port sql = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (proc "sudo" ["-u", "postgres", "psql", "-p", show port, "-X", "-tA", "-F", "|", "-d", "postgres", "-c", Text.unpack sql])
            ""
    pure $ case code of
        ExitSuccess -> Right (Text.decodeUtf8With TextError.lenientDecode out)
        ExitFailure _ -> Left (Text.decodeUtf8With TextError.lenientDecode err)
