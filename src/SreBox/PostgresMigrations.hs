{-# LANGUAGE DeriveGeneric #-}

module SreBox.PostgresMigrations where

import Control.Monad.Identity
import Control.Comonad.Cofree (Cofree(..))
import Data.ByteString.Char8 as C8
import Data.Dynamic (toDyn)
import qualified Data.ByteString.Base16 as Base16
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Maybe (catMaybes)
import Data.List (stripPrefix)
import System.FilePath ((</>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics

import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Actions.Dot as Dot
import Salmon.Op.OpGraph
import Salmon.Op.Graph
import Salmon.Op.Eval
import Salmon.Op.Ref
import Salmon.Op.G
import Salmon.Op.Track
import Salmon.Builtin.Extension
import Salmon.Builtin.Migrations
import Salmon.Builtin.Nodes.Debian.OS as Debian
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Postgres
import Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Helpers (collapse)

data MigrationFile
  = MigrationFile
  { path :: FilePath
  } deriving (Show, Generic)

instance FromJSON MigrationFile
instance ToJSON MigrationFile

defaultMigrationReader :: MigrationReader MigrationFile
defaultMigrationReader =
    MigrationReader
      (pure . MigrationFile)
      (parsePredecessors)
      id
  where
    parsePredecessors :: C8.ByteString -> [FilePath]
    parsePredecessors txt =
      catMaybes $ fmap parsePredecessorLine $ C8.lines txt

    parsePredecessorLine :: C8.ByteString -> Maybe FilePath
    parsePredecessorLine line =
      C8.unpack <$> C8.stripPrefix prefix line

    prefix :: C8.ByteString
    prefix = "-- migrate.after: "

migrateG
  :: Track' (Binary "psql")
  -> Track' (ConnString FilePath)
  -> ConnString FilePath
  -> G MigrationFile
  -> Op
migrateG psql mksetup connstring g = migrate psql mksetup connstring (coerce g)

-- | A helper to turn a migration graph into an Op.
migrate
  :: Track' (Binary "psql")
  -> Track' (ConnString FilePath)
  -> ConnString FilePath
  -> Cofree Graph MigrationFile
  -> Op
migrate psql mksetup connstring (m :< x) =
  let 
      current, pred :: Op
      current = userScript psql mksetup connstring (PreExisting m.path) 
      pred = evalPred "" x
  in
  current `inject` pred
  where
    gorec = migrate psql mksetup connstring
    setref lineage actions =
      actions { ref = dotRef $ Text.pack $ m.path  <> " " <> lineage }

    -- the string in eval pred accumulates left/right branches choices to disambiguate noop nodes by ref
    evalPred :: String -> Graph (Cofree Graph MigrationFile) -> Op
    evalPred l (Vertices []) = realNoop
    evalPred l (Vertices zs) =
      op "migrate:deps" (deps $ fmap gorec zs) (setref l)
    evalPred l (Overlay g1 g2) =
      op "migrate:deps" (deps [evalPred ('l':l) g1, evalPred ('r':l) g2]) (setref l)
    evalPred l (Connect g1 g2) =
      op "migrate:deps" (deps [evalPred ('l':l) g2 `inject` evalPred ('r':l) g1]) (setref l)

-------------------------------------------------------------------------------

data MigrationPlan a =
  MigrationPlan
  { migration_connstring :: ConnString a
  , migration_file :: G MigrationFile
  } deriving (Generic)
instance ToJSON a => ToJSON (MigrationPlan a)
instance FromJSON a => FromJSON (MigrationPlan a)

migratePlan :: MigrationPlan FilePath -> Op
migratePlan plan =
  migrateG Debian.psql ignoreTrack plan.migration_connstring plan.migration_file

-------------------------------------------------------------------------------

data RemoteMigrateConfig t
  = RemoteMigrateConfig
  { cfg_migrations :: t (G MigrationFile)
  , cfg_user :: User
  , cfg_database :: Database
  , cfg_password :: FilePath
  }

data RemoteMigrateSetup
  = RemoteMigrateSetup
  { setup_migration :: G MigrationFile
  , setup_user :: User
  , setup_database :: Database
  , setup_tmp_secret_path :: FilePath
  }
  deriving (Generic)
instance FromJSON RemoteMigrateSetup
instance ToJSON RemoteMigrateSetup

remoteMigrateSetup
  :: (FromJSON directive, ToJSON directive)
  => Password
  -> Track' directive
  -> Self.Remote
  -> Self.SelfPath
  -> (RemoteMigrateSetup -> directive)
  -> RemoteMigrateConfig Identity
  -> Op
remoteMigrateSetup pass1 simulate selfRemote selfpath toSpec cfg =
    op "migrate-remotely" (deps [remoteApply `inject` uploadsecret `inject` uploadmigrations]) id
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    migrations = runIdentity cfg.cfg_migrations

    uploadmigrations =
      collapse "." uploadmigrationFile (getCofreeGraph migrations)

    dbname = getDatabase cfg.cfg_database

    remoteMigrationPath :: MigrationFile -> FilePath
    remoteMigrationPath m = "tmp/migration-" <> Text.unpack dbname <> (C8.unpack $ Base16.encode (C8.pack m.path))

    uploadmigrationFile :: MigrationFile -> Op
    uploadmigrationFile m =
      Rsync.sendFile
        Debian.rsync
        (FS.PreExisting m.path)
        (rsyncRemote)
        (remoteMigrationPath m)

    uploadsecret :: Op
    uploadsecret =
      Rsync.sendFile
        Debian.rsync
        (FS.Generated pgPassword cfg.cfg_password)
        (rsyncRemote)
        (remotePgSecretPath)

    remotePgSecretPath :: FilePath
    remotePgSecretPath = "tmp/connstring"

    remoteMigrationPlan :: G MigrationFile
    remoteMigrationPlan =
         (G $ fmap (MigrationFile . remoteMigrationPath) $ getCofreeGraph migrations)

    remoteApply :: Op
    remoteApply =
       let s = Self.uploadSelf "tmp" selfRemote selfpath
       in
       opGraph $ s `bindTracked` \x -> Self.callSelfAsSudo Ssh.preExistingRemoteMachine x simulate CLI.Up (toSpec $ RemoteMigrateSetup remoteMigrationPlan cfg.cfg_user cfg.cfg_database remotePgSecretPath)

remoteMigrateOpaqueSetup
  :: (FromJSON directive, ToJSON directive)
  => Track' directive
  -> Self.Remote
  -> Self.SelfPath
  -> (RemoteMigrateSetup -> directive)
  -> RemoteMigrateConfig TrackedIO
  -> Op
remoteMigrateOpaqueSetup simulate selfRemote selfpath toSpec cfg =
    op "migrate-remotely" (deps [opaquemigration `inject` uploadsecret]) id
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    opaquemigration =
      using (unwrapTIO cfg.cfg_migrations) $ \ioMigrations ->
      op "opaque-migrate" nodeps $ \actions -> actions {
        up = do
          migrations <- ioMigrations
          let uploads = collapse "." uploadmigrationFile (getCofreeGraph migrations)
          let apply = remoteApply migrations
          UpDown.upTree (pure . runIdentity) (apply `inject` uploads)
      , dynamics = [toDyn $ Dot.OpaqueNode "migration"]
      }

    dbname = getDatabase cfg.cfg_database

    remoteMigrationPath :: MigrationFile -> FilePath
    remoteMigrationPath m = "tmp/migration-" <> Text.unpack dbname <> (C8.unpack $ Base16.encode (C8.pack m.path))

    uploadmigrationFile :: MigrationFile -> Op
    uploadmigrationFile m =
      Rsync.sendFile
        Debian.rsync
        (FS.PreExisting m.path)
        (rsyncRemote)
        (remoteMigrationPath m)

    uploadsecret :: Op
    uploadsecret =
      Rsync.sendFile
        Debian.rsync
        (FS.Generated pgPassword cfg.cfg_password)
        (rsyncRemote)
        (remotePgSecretPath)

    remotePgSecretPath :: FilePath
    remotePgSecretPath = Text.unpack $ "tmp/connstring-" <> dbname

    remoteMigrationPlan :: G MigrationFile -> G MigrationFile
    remoteMigrationPlan migrations =
         (G $ fmap (MigrationFile . remoteMigrationPath) $ getCofreeGraph migrations)

    remoteApply :: G MigrationFile -> Op
    remoteApply migrations =
       let s = Self.uploadSelf "tmp" selfRemote selfpath
       in
       opGraph $ s `bindTracked` \x -> Self.callSelfAsSudo Ssh.preExistingRemoteMachine x simulate CLI.Up (toSpec $ RemoteMigrateSetup (remoteMigrationPlan migrations) cfg.cfg_user cfg.cfg_database remotePgSecretPath)


applyMigration
  :: Track' (Binary "psql")
  -> Track' (ConnString FilePath)
  -> RemoteMigrateSetup
  -> Op
applyMigration psql mkConnstring setup =
    migrateG psql mkConnstring connstring setup.setup_migration
  where
    connstring =
      ConnString localServer setup.setup_user setup.setup_tmp_secret_path setup.setup_database

pgPassword :: Track' FilePath
pgPassword = Track $ \path ->
  Secrets.sharedSecretFile
    Debian.openssl
    (Secrets.Secret Secrets.Hex 48 path)

pgConnstringFile :: ConnString FilePath -> Track' FilePath
pgConnstringFile conn =
  Track $ \cstringpath ->
  op "pg-connstring" (deps [run pgPassword passFile]) $ \actions -> actions {
    ref = dotRef $ "connstring: " <> Text.pack cstringpath
  , help = "store connection string at " <> Text.pack cstringpath
  , up = do
     pass <- readPassword passFile
     let contents = connstring (withPassword conn pass)
     Text.writeFile cstringpath contents
  }
  where
    passFile :: FilePath
    passFile = conn.connstring_user_pass
