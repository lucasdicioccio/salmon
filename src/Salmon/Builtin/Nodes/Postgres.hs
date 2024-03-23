{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Salmon.Builtin.Nodes.Postgres where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), justInstall, untrackedExec, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem (File, withFile)
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
    | PGScript !FilePath !Binary.Report
    | PGAdminScript !FilePath !Binary.Report
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

hardcodedVersion :: Version
hardcodedVersion = 12

pgLocalCluster :: Reporter Report -> Track' (Binary "postgres") -> Track' (Binary "pg_ctlcluster") -> Server -> Op
pgLocalCluster r pg pgctl server =
    withBinary pgctl pgctlRun (Start hardcodedVersion server.serverPort) $ \start ->
        op "pg-server" (deps [justInstall pg]) $ \actions ->
            actions
                { notes = ["default server"]
                , help = Text.unwords ["start pg cluster"]
                , up = start r'
                }
  where
    r' = contramap PGStartLocalCluster r

data PgCtl
    = Start Version Port

pgctlRun :: Command "pg_ctlcluster" PgCtl
pgctlRun = Command go
  where
    go (Start ver port) = proc "pg_ctlcluster" [show ver, "main", "start"]

type DatabaseName = Text

newtype Database = Database {getDatabase :: DatabaseName}
    deriving (Show, ToJSON, FromJSON)

database :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Database -> Op
database r server psql db =
    withBinary psql psqlAdminRun_Sudo (CreateDB db.getDatabase) $ \up ->
        op "pg-database" (deps [run server localServer]) $ \actions ->
            actions
                { ref = dotRef $ "pg-db:" <> db.getDatabase
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

user :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> User -> Password -> Op
user r server psql user pwd =
    withBinary psql psqlAdminRun_Sudo (CreateUser user.userRole pwd) $ \up ->
        op "pg-user" (deps [run server localServer]) $ \actions ->
            actions
                { ref = dotRef $ "pg-user:" <> user.userRole
                , help = Text.unwords ["create user", user.userRole]
                , up = up r'
                }
  where
    r' = contramap (PGCreateUser user) r

userPassFile :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> File "passfile" -> User -> Op
userPassFile r server psql genpass user =
    withFile genpass $ \passfile ->
        op "pg-user" (deps [runningServer, justInstall psql]) $ \actions ->
            actions
                { ref = dotRef $ "pg-user:" <> user.userRole
                , help = Text.unwords ["set user password for", user.userRole, "from file at", Text.pack passfile]
                , up = do
                    up =<< fmap Password (Text.readFile passfile)
                }
  where
    r' = contramap (PGSetUserPass user) r
    runningServer = run server localServer
    up pass = untrackedExec psqlAdminRun_Sudo (CreateUser user.userRole pass) "" r'

data Group = Group {groupRole :: RoleName}
    deriving (Show, Generic)
instance ToJSON Group
instance FromJSON Group

group :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Group -> Op
group r server psql group =
    withBinary psql psqlAdminRun_Sudo (CreateGroup group.groupRole) $ \up ->
        op "pg-group" (deps [run server localServer]) $ \actions ->
            actions
                { ref = dotRef $ "pg-group:" <> group.groupRole
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

grant :: Reporter Report -> Track' (Binary "psql") -> Track' Role -> AccessRight -> Op
grant r psql role acl =
    withBinary psql psqlAdminRun_Sudo (Grant acl) $ \up ->
        op "pg-grant" (deps [dbrole]) $ \actions ->
            actions
                { ref = dotRef $ "pg-grant:" <> roleName acl.access_role
                , up = up r'
                , help = Text.unwords ["grant", roleName acl.access_role]
                }
  where
    r' = contramap (PGGrant acl) r
    dbrole = run role acl.access_role

groupMember :: Reporter Report -> Track' Server -> Track' (Binary "psql") -> Group -> Track' Role -> Role -> Op
groupMember r server psql g role u =
    withBinary psql psqlAdminRun_Sudo (GroupMembership g.groupRole (roleName u)) $ \up ->
        op "pg-member" (deps [dbgroup, dbuser]) $ \actions ->
            actions
                { ref = dotRef $ "pg-member:" <> g.groupRole <> (roleName u)
                , up = up r'
                , help = Text.unwords ["add", roleName u, "to", g.groupRole]
                }
  where
    r' = contramap (PGGroupMembership g u) r
    dbgroup = group r server psql g
    dbuser = run role u

adminScript :: Reporter Report -> Track' (Binary "psql") -> File "psql-script" -> Op
adminScript r psql file =
    withFile file $ \path ->
        withBinary psql psqlAdminRun_Sudo (AdminScript path) $ \up ->
            op "pg-adming-script" nodeps $ \actions ->
                actions
                    { ref = dotRef $ "pg-admin-script:" <> Text.pack path
                    , up = up (r' path)
                    , help = Text.unwords ["runs pg script", Text.pack path]
                    }
  where
    r' path = contramap (PGAdminScript path) r

{- | commands to bootstrap PG roles and dbs as admin
expected to run as user "postgres" in Debian to handle the nopassword initial state
-}
data PsqlAdmin
    = CreateDB DatabaseName
    | CreateUser RoleName Password
    | CreateGroup RoleName
    | Grant AccessRight
    | GroupMembership RoleName RoleName
    | AdminScript FilePath

{- | todo: workaround sudo hack with some calling preference
- we'll need to request more than a Track' (Binary "psql") but some more complex logic
with sudo, the user, and the right binary
-}
psqlAdminRun_Sudo :: Command "psql" PsqlAdmin
psqlAdminRun_Sudo = Command go
  where
    go (AdminScript path) =
        proc "sudo" ["-u", "postgres", "psql", "-f", path]
    go (CreateDB name) =
        proc "sudo" ["-u", "postgres", "psql", "-c", unwords ["CREATE DATABASE", Text.unpack name]]
    go (CreateUser name pass) =
        proc
            "sudo"
            [ "-u"
            , "postgres"
            , "psql"
            , "-c"
            , unwords
                [ "CREATE ROLE"
                , Text.unpack name
                , "WITH"
                , "LOGIN"
                , "PASSWORD"
                , quotePass pass
                ]
            ]
    go (CreateGroup name) =
        proc
            "sudo"
            [ "-u"
            , "postgres"
            , "psql"
            , "-c"
            , unwords
                [ "CREATE ROLE"
                , Text.unpack name
                ]
            ]
    go (GroupMembership g u) =
        proc
            "sudo"
            [ "-u"
            , "postgres"
            , "psql"
            , "-c"
            , unwords
                [ "ALTER GROUP"
                , Text.unpack g
                , "ADD USER"
                , Text.unpack u
                ]
            ]
    go (Grant acl) =
        proc
            "sudo"
            [ "-u"
            , "postgres"
            , "psql"
            , "-c"
            , unwords
                [ "GRANT"
                , Text.unpack $ commaList $ fmap renderRight acl.access_rights
                , "ON DATABASE"
                , Text.unpack acl.access_database.getDatabase
                , "TO"
                , Text.unpack (roleName acl.access_role)
                ]
            ]

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
                    { ref = dotRef $ "pg-script:" <> Text.pack path
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
                { ref = dotRef $ "pg-script:" <> Text.pack path
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
