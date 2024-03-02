
module SreBox.PostgresInit where

import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS

data InitSetup
  = InitSetup
  { init_setup_database :: Postgres.Database
  , init_setup_users :: [(Postgres.User, FS.File "passfile", [Postgres.Right])]
  }

-- | generate a db with a given sets of users, all having some rights
setupPG :: InitSetup -> Op
setupPG setup =
    op "pg-setup" (deps [acls `inject` basics]) id
  where
    basics = op "pg-basics" (deps [users `inject` db]) id
    cluster = Track $ Postgres.pgLocalCluster Debian.postgres Debian.pg_ctlcluster
    db = Postgres.database cluster Debian.psql setup.init_setup_database

    users = op "pg-users" (deps $ fmap user setup.init_setup_users) id

    user (u, pass, _) =
      Postgres.userPassFile cluster Debian.psql pass u

    acls = op "grants" (deps $ fmap acl setup.init_setup_users) id
    acl (u, _, rights) =
      Postgres.grant
        Debian.psql
        (Postgres.AccessRight setup.init_setup_database (Postgres.UserRole u) rights)

-- | typical setup with a single connection account
setupSingleUserPG :: Postgres.ConnString FilePath -> Op
setupSingleUserPG connstring =
    setupPG setup
  where
    setup = InitSetup d1 [(u1,FS.PreExisting pass1,[Postgres.CONNECT, Postgres.CREATE])]
  
    cluster = Track $ Postgres.pgLocalCluster Debian.postgres Debian.pg_ctlcluster
    d1 = connstring.connstring_db
    u1 = connstring.connstring_user
    pass1 = connstring.connstring_user_pass

-- | typical setup with a -rw and an -ro connection account
setupTwoUsersPG :: Postgres.ConnString FilePath -> (Postgres.User, FilePath) -> Op
setupTwoUsersPG connstring (u2,pass2) =
    setupPG setup
  where
    setup = InitSetup d1
              [ (u1,FS.PreExisting pass1,[Postgres.CONNECT, Postgres.CREATE])
              , (u2,FS.PreExisting pass2,[Postgres.CONNECT])
              ]
  
    cluster = Track $ Postgres.pgLocalCluster Debian.postgres Debian.pg_ctlcluster
    d1 = connstring.connstring_db
    u1 = connstring.connstring_user
    pass1 = connstring.connstring_user_pass
