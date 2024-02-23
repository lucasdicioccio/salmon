
module Salmon.Builtin.Nodes.Postgres where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Process.ListLike (CreateProcess(..), proc)

import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Filesystem (File, withFile)

type Host = Text
type Port = Int

-- | todo: distinguish server (to which we connect to) and cluster (with version)
data Server
  = Server
  { serverHost :: Host
  , serverPort :: Port
  }

localServer :: Server
localServer = Server "127.0.0.1" 5432

type Version = Int

hardcodedVersion :: Version
hardcodedVersion = 12

pgLocalCluster :: Track' (Binary "postgres") -> Track' (Binary "pg_ctlcluster") -> Server -> Op
pgLocalCluster pg pgctl server =
  withBinary pgctl pgctlRun (Start hardcodedVersion server.serverPort) $ \start ->
  op "pg-server" (deps [justInstall pg]) $ \actions -> actions {
    notes = ["default server"]
  , up = start
  }

data PgCtl
  = Start Version Port

pgctlRun :: Command "pg_ctlcluster" PgCtl
pgctlRun = Command go
  where
    go (Start ver port) = proc "pg_ctlcluster" [show ver,"main","start"]

type DatabaseName = Text

newtype Database = Database { getDatabase :: DatabaseName }

database :: Track' Server -> Track' (Binary "psql") -> Database -> Op
database server psql db =
  withBinary psql psqlAdminRun_Sudo (CreateDB db.getDatabase) $ \up ->
  op "pg-database" (deps [ run server localServer ]) $ \actions -> actions {
    ref = dotRef $ "pg-db:" <> db.getDatabase
  , up = up
  }

type RoleName = Text

newtype Password = Password { revealPassword :: Text }

instance Show Password where
  show _ = "<password>"

data User = User { userRole :: RoleName }

user :: Track' Server -> Track' (Binary "psql") -> User -> Password -> Op
user server psql user pwd =
  withBinary psql psqlAdminRun_Sudo (CreateUser user.userRole pwd) $ \up ->
  op "pg-user" (deps [ run server localServer ]) $ \actions -> actions {
    ref = dotRef $ "pg-user:" <> user.userRole
  , up = up
  }

data Group = Group { groupRole :: RoleName }

group :: Track' Server -> Track' (Binary "psql") -> Group -> Op
group server psql group =
  withBinary psql psqlAdminRun_Sudo (CreateGroup group.groupRole) $ \up ->
  op "pg-group" (deps [ run server localServer ]) $ \actions -> actions {
    ref = dotRef $ "pg-group:" <> group.groupRole
  , up = up
  }

data Role
  = UserRole User
  | GroupRole Group

roleName :: Role -> RoleName
roleName (UserRole u) = u.userRole
roleName (GroupRole g) = g.groupRole

data Right
  = CREATE
  | CONNECT

data AccessRight
  = AccessRight 
  { access_database :: Database
  , access_role :: Role
  , access_rights :: [Right]
  }

grant :: Track' (Binary "psql") -> AccessRight -> Op
grant psql acl =
  withBinary psql psqlAdminRun_Sudo (Grant acl) $ \up ->
  op "pg-grant" nodeps $ \actions -> actions {
    ref = dotRef $ "pg-group:" <> roleName acl.access_role
  , up = up
  }

adminScript :: Track' (Binary "psql") -> File "psql-script" -> Op
adminScript psql file =
  withFile file $ \path ->
  withBinary psql psqlAdminRun_Sudo (AdminScript path) $ \up ->
  op "pg-adming-script" nodeps $ \actions -> actions {
    ref = dotRef $ "pg-admin-script:" <> Text.pack path
  , up = up
  , help = "runs " <> Text.pack path
  }

-- | commands to bootstrap PG roles and dbs as admin
-- expected to run as user "postgres" in Debian to handle the nopassword initial state
data PsqlAdmin
  = CreateDB DatabaseName
  | CreateUser RoleName Password
  | CreateGroup RoleName
  | Grant AccessRight
  | AdminScript FilePath

-- | todo: workaround sudo hack with some calling preference
-- - we'll need to request more than a Track' (Binary "psql") but some more complex logic
-- with sudo, the user, and the right binary
psqlAdminRun_Sudo :: Command "psql" PsqlAdmin
psqlAdminRun_Sudo = Command go
  where
    go (AdminScript path) =
      proc "sudo" [ "-u", "postgres", "psql", "-f", path ]
    go (CreateDB name) =
      proc "sudo" [ "-u", "postgres", "psql",  "-c", unwords [ "CREATE DATABASE" , Text.unpack name ] ]
    go (CreateUser name pass) =
      proc "sudo" [ "-u", "postgres", "psql",  "-c"
                  , unwords
                    [ "CREATE ROLE" 
                    , Text.unpack name
                    , "WITH"
                    , "LOGIN"
                    , "PASSWORD", quotePass pass
                    ] 
                  ]
    go (CreateGroup name) =
      proc "sudo" [ "-u", "postgres", "psql",  "-c"
                  , unwords
                    [ "CREATE ROLE" 
                    , Text.unpack name
                    ] 
                  ]
    go (Grant acl) =
      proc "sudo" [ "-u", "postgres", "psql",  "-c"
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

    renderRight :: Right -> Text
    renderRight CREATE = "CREATE"
    renderRight CONNECT = "CONNECT"

    commaList :: [Text] -> Text
    commaList = Text.intercalate ","


-------------------------------------------------------------------------------

data ConnString pass = ConnString {
    connstring_server :: Server
  , connstring_user :: User
  , connstring_user_pass :: pass
  , connstring_db :: Database
  } deriving (Functor)

withPassword :: Password -> ConnString a -> ConnString Password
withPassword pass c = const pass <$> c

userScript
  :: Track' (Binary "psql")
  -> Track' (ConnString Password)
  -> ConnString Password
  -> File "psql-script"
  -> Op
userScript psql mksetup c@(ConnString server user pass db) file =
  withFile file $ \path ->
  withBinary psql (psqlUserRun c) (UserScript path) $ \up ->
  op "pg-script" (deps [run mksetup c]) $ \actions -> actions {
    ref = dotRef $ "pg-script:" <> Text.pack path
  , up = up
  , help = "runs " <> Text.pack path
  }

data PsqlUser
  = UserScript FilePath

psqlUserRun :: ConnString Password -> Command "psql" PsqlUser
psqlUserRun (ConnString server user pass db) = Command go
  where
    connstring :: String
    connstring =
      mconcat
        ["postgresql://"
        , Text.unpack user.userRole
        , ":"
        , Text.unpack $ pass.revealPassword
        , "@"
        , Text.unpack server.serverHost
        , ":"
        , show server.serverPort
        , "/"
        , Text.unpack db.getDatabase
        ]

    go (UserScript path) =
      proc "psql"
        [ connstring
        , "-f"
        , path
        ]
