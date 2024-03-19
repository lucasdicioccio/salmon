{-# LANGUAGE DeriveGeneric #-}
module SreBox.PostgresInit where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text as Text

import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.CommandLine as CLI
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified SreBox.PostgresMigrations as PGMigrate

-- | A PG setup definition where:
-- * groups and users have rights
-- * users have passwords
-- * users may be in groups (no subgroups)
data InitSetup pass
  = InitSetup
  { init_setup_database :: Postgres.Database
  , init_setup_users :: [(Postgres.User, pass, [Postgres.PGRight], [Postgres.Group])]
  , init_setup_groups :: [(Postgres.Group, [Postgres.PGRight])]
  } deriving (Generic)
instance FromJSON a => FromJSON (InitSetup a)
instance ToJSON a => ToJSON (InitSetup a)

setupWithPreExistingPasswords :: InitSetup FilePath -> InitSetup (FS.File "passfile")
setupWithPreExistingPasswords setup =
  InitSetup
    setup.init_setup_database
    [(u,FS.PreExisting pass,rs,gs) | (u,pass,rs,gs) <- setup.init_setup_users]
    setup.init_setup_groups

-- | generate a db with a given sets of users, all having some rights
setupPG :: InitSetup (FS.File "passfile") -> Op
setupPG setup =
    op "pg-setup" (deps [memberships, groupAcls, userAcls `inject` basics]) id
  where
    basics = op "pg-basics" (deps [users `inject` groups `inject` db]) id
    cluster = Track $ Postgres.pgLocalCluster Debian.postgres Debian.pg_ctlcluster
    db = Postgres.database cluster Debian.psql setup.init_setup_database

    groups = op "pg-groups" (deps $ fmap group setup.init_setup_groups) id

    group (g, _) =
      Postgres.group cluster Debian.psql g

    users = op "pg-users" (deps $ fmap user setup.init_setup_users) id

    user (u, pass, _, _) =
      Postgres.userPassFile cluster Debian.psql pass u

    -- we force all users to be created before any group membership
    trackuserFromMembershipByCreatingAllUsers = Track $ const users
    trackuserFromAclsByCreatingAllUsers = Track $ const users
    trackgroupsFromAclsByCreatingAllGroups = Track $ const groups

    userAcls = op "pg-user-grants" (deps $ fmap acl setup.init_setup_users) id
      where
        acl (u, _, rights,_) =
          Postgres.grant
            Debian.psql
            trackuserFromAclsByCreatingAllUsers
            (Postgres.AccessRight setup.init_setup_database (Postgres.UserRole u) rights)

    groupAcls = op "pg-group-grants" (deps $ fmap acl setup.init_setup_groups) id
      where
        acl (g, rights) =
          Postgres.grant
            Debian.psql
            trackgroupsFromAclsByCreatingAllGroups
            (Postgres.AccessRight setup.init_setup_database (Postgres.GroupRole g) rights)

    memberships = op "pg-user-memberships" (deps $ [membership u g | (u,_,_,gs) <- setup.init_setup_users, g <- gs]) id
      where
        membership u g =
          Postgres.groupMember
            cluster
            Debian.psql
            g
            trackuserFromMembershipByCreatingAllUsers
            (Postgres.UserRole u)

-- | typical setup with a single connection account
setupSingleUserPG :: Postgres.ConnString FilePath -> Op
setupSingleUserPG connstring =
    setupPG setup
  where
    setup = InitSetup d1 users []
    users = [(u1,FS.PreExisting pass1,[Postgres.CONNECT, Postgres.CREATE],[])]
  
    cluster = Track $ Postgres.pgLocalCluster Debian.postgres Debian.pg_ctlcluster
    d1 = connstring.connstring_db
    u1 = connstring.connstring_user
    pass1 = connstring.connstring_user_pass

remoteSetupPG
  :: (FromJSON directive, ToJSON directive)
  => Track' directive
  -> Self.Remote
  -> Self.SelfPath
  -> (InitSetup FilePath -> directive)
  -> InitSetup FilePath
  -> Op
remoteSetupPG simulate selfRemote selfpath toSpec cfg =
    op "init-remotely" (deps [remoteInit `inject` uploadSecrets]) id
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    remoteSetup :: InitSetup FilePath
    remoteSetup = InitSetup cfg.init_setup_database remoteusers cfg.init_setup_groups

    remoteusers = [ (u,remotePassFilePath u,rs,gs) | (u,_,rs,gs) <- cfg.init_setup_users ]
    remotePassFilePath u = Text.unpack $ "tmp/pg-init-pass-" <> Postgres.userRole u

    remoteInit :: Op
    remoteInit =
       let s = Self.uploadSelf "tmp" selfRemote selfpath
       in
       opGraph $ s `bindTracked` \x -> Self.callSelfAsSudo Ssh.preExistingRemoteMachine x simulate CLI.Up (toSpec remoteSetup)

    uploadSecrets :: Op
    uploadSecrets = op "pg-init-secrets" (deps [uploaduserSecret u p | (u,p,_,_) <- cfg.init_setup_users]) id

    uploaduserSecret :: Postgres.User -> FilePath -> Op
    uploaduserSecret u pass =
      Rsync.sendFile
        Debian.rsync
        (FS.Generated
          PGMigrate.pgPassword
          pass)
        (rsyncRemote)
        (remotePassFilePath u)
