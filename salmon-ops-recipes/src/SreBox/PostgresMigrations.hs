{-# LANGUAGE DeriveGeneric #-}

module SreBox.PostgresMigrations where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 as C8
import Data.Coerce (coerce)
import Data.Dynamic (toDyn)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import System.FilePath ((</>))

import qualified Salmon.Actions.Dot as Dot
import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import Salmon.Builtin.Helpers (collapse)
import Salmon.Builtin.Migrations
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import Salmon.Builtin.Nodes.Debian.OS as Debian
import Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Nodes.Postgres (ConnString (..), Database (..), DatabaseName, Password, User, adminScript, connstring, localServer, readPassword, userScript, withPassword)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Eval
import Salmon.Op.G
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------
data Report
    = ApplyMigration !FilePath !Postgres.Report
    | UploadFile !Rsync.Report
    | UploadSecretFile !Rsync.Report
    | UploadSelf !Self.Report
    | CallSelf !Self.Report
    | CreatePassword !Secrets.Report
    | Continuation !(UpDown.Report Extension)
    deriving (Show)

-------------------------------------------------------------------------------

data MigrationFile
    = MigrationFile
    { path :: FilePath
    }
    deriving (Show, Generic)

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

data MigrateStyle
    = MigrateUserScript (Track' (ConnString FilePath)) (ConnString FilePath)
    | MigrateAdminScript (Track' DatabaseName) DatabaseName

migrateG ::
    Reporter Report ->
    Track' (Binary "psql") ->
    MigrateStyle ->
    G MigrationFile ->
    Op
migrateG r psql style g =
    migrate r psql style (coerce g)

-- | A helper to turn a migration graph into an Op.
migrate ::
    Reporter Report ->
    Track' (Binary "psql") ->
    MigrateStyle ->
    Cofree Graph MigrationFile ->
    Op
migrate r psql style (m :< x) =
    let
        current :: Op
        current =
            case style of
                MigrateUserScript mksetup connstring ->
                    userScript r' psql mksetup connstring (PreExisting m.path)
                MigrateAdminScript mksetup dbname ->
                    adminScript r' psql mksetup dbname (PreExisting m.path)

        pred :: Op
        pred = evalPred "" x
     in
        current `inject` pred
  where
    r' = contramap (ApplyMigration m.path) r
    gorec = migrate r psql style
    setref lineage actions =
        actions{ref = dotRef $ Text.pack $ m.path <> " " <> lineage}

    -- the string in eval pred accumulates left/right branches choices to disambiguate noop nodes by ref
    evalPred :: String -> Graph (Cofree Graph MigrationFile) -> Op
    evalPred l (Vertices []) = realNoop
    evalPred l (Vertices zs) =
        op "migrate:deps" (deps $ fmap gorec zs) (setref l)
    evalPred l (Overlay g1 g2) =
        op "migrate:deps" (deps [evalPred ('l' : l) g1, evalPred ('r' : l) g2]) (setref l)
    evalPred l (Connect g1 g2) =
        op "migrate:deps" (deps [evalPred ('l' : l) g2 `inject` evalPred ('r' : l) g1]) (setref l)

-------------------------------------------------------------------------------

data MigrationPlan a
    = MigrationPlan
    { migration_connstring :: ConnString a
    , migration_file :: G MigrationFile
    }
    deriving (Generic)
instance (ToJSON a) => ToJSON (MigrationPlan a)
instance (FromJSON a) => FromJSON (MigrationPlan a)

-------------------------------------------------------------------------------

data RemoteMigrateConfig t
    = RemoteMigrateConfig
    { cfg_migrations :: t (G MigrationFile)
    , cfg_user :: User
    , cfg_database :: Database
    , cfg_password :: FilePath
    }

remoteMigrateSetup ::
    (FromJSON directive, ToJSON directive) =>
    Reporter Report ->
    Password ->
    Track' directive ->
    Self.Remote ->
    Self.SelfPath ->
    (MigrationSetup -> directive) ->
    RemoteMigrateConfig Identity ->
    Op
remoteMigrateSetup r pass1 simulate selfRemote selfpath toSpec cfg =
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
            (contramap UploadFile r)
            Debian.rsync
            (FS.PreExisting m.path)
            (rsyncRemote)
            (remoteMigrationPath m)

    uploadsecret :: Op
    uploadsecret =
        Rsync.sendFile
            (contramap UploadSecretFile r)
            Debian.rsync
            (FS.Generated (pgPassword r) cfg.cfg_password)
            (rsyncRemote)
            (remotePgSecretPath)

    remotePgSecretPath :: FilePath
    remotePgSecretPath = "tmp/connstring"

    remoteMigrationPlan :: G MigrationFile
    remoteMigrationPlan =
        (G $ fmap (MigrationFile . remoteMigrationPath) $ getCofreeGraph migrations)

    remoteApply :: Op
    remoteApply =
        let s = Self.uploadSelf (contramap UploadSelf r) "tmp" selfRemote selfpath
         in opGraph $ s `bindTracked` \x -> Self.callSelfAsSudo (contramap CallSelf r) Ssh.preExistingRemoteMachine x simulate CLI.Up (toSpec $ MigrationSetup remoteMigrationPlan cfg.cfg_user cfg.cfg_database remotePgSecretPath)

remoteMigrateOpaqueSetup ::
    (FromJSON directive, ToJSON directive) =>
    Reporter Report ->
    Track' directive ->
    Self.Remote ->
    Self.SelfPath ->
    (MigrationSetup -> directive) ->
    RemoteMigrateConfig TrackedIO ->
    Op
remoteMigrateOpaqueSetup r simulate selfRemote selfpath toSpec cfg =
    op "migrate-remotely" (deps [opaquemigration `inject` uploadsecret]) id
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    opaquemigration =
        using (unwrapTIO cfg.cfg_migrations) $ \ioMigrations ->
            op "opaque-migrate" nodeps $ \actions ->
                actions
                    { up = do
                        migrations <- ioMigrations
                        let uploads = collapse "." uploadmigrationFile (getCofreeGraph migrations)
                        let apply = remoteApply migrations
                        UpDown.upTree (contramap Continuation r) (pure . runIdentity) (apply `inject` uploads)
                    , dynamics = [toDyn $ Dot.OpaqueNode "migration"]
                    }

    dbname = getDatabase cfg.cfg_database

    remoteMigrationPath :: MigrationFile -> FilePath
    remoteMigrationPath m = "tmp/migration-" <> Text.unpack dbname <> (C8.unpack $ Base16.encode (C8.pack m.path))

    uploadmigrationFile :: MigrationFile -> Op
    uploadmigrationFile m =
        Rsync.sendFile
            (contramap UploadFile r)
            Debian.rsync
            (FS.PreExisting m.path)
            (rsyncRemote)
            (remoteMigrationPath m)

    uploadsecret :: Op
    uploadsecret =
        Rsync.sendFile
            (contramap UploadSecretFile r)
            Debian.rsync
            (FS.Generated (pgPassword r) cfg.cfg_password)
            (rsyncRemote)
            (remotePgSecretPath)

    remotePgSecretPath :: FilePath
    remotePgSecretPath = Text.unpack $ "tmp/connstring-" <> dbname

    remoteMigrationPlan :: G MigrationFile -> G MigrationFile
    remoteMigrationPlan migrations =
        (G $ fmap (MigrationFile . remoteMigrationPath) $ getCofreeGraph migrations)

    remoteApply :: G MigrationFile -> Op
    remoteApply migrations =
        let s = Self.uploadSelf (contramap UploadSelf r) "tmp" selfRemote selfpath
         in opGraph $ s `bindTracked` \x -> Self.callSelfAsSudo (contramap CallSelf r) Ssh.preExistingRemoteMachine x simulate CLI.Up (toSpec $ MigrationSetup (remoteMigrationPlan migrations) cfg.cfg_user cfg.cfg_database remotePgSecretPath)

data MigrationSetup
    = MigrationSetup
    { setup_migration :: G MigrationFile
    , setup_user :: User
    , setup_database :: Database
    , setup_tmp_secret_path :: FilePath
    }
    deriving (Generic)
instance FromJSON MigrationSetup
instance ToJSON MigrationSetup

applyUserScriptMigration ::
    Reporter Report ->
    Track' (Binary "psql") ->
    Track' (ConnString FilePath) ->
    MigrationSetup ->
    Op
applyUserScriptMigration r psql mkConnstring setup =
    migrateG r psql style setup.setup_migration
  where
    style = MigrateUserScript mkConnstring connstring
    connstring =
        ConnString localServer setup.setup_user setup.setup_tmp_secret_path setup.setup_database

applyAdminScriptMigration ::
    Reporter Report ->
    Track' (Binary "psql") ->
    Track' DatabaseName ->
    MigrationSetup ->
    Op
applyAdminScriptMigration r psql mkdb setup =
    migrateG r psql style setup.setup_migration
  where
    style = MigrateAdminScript mkdb (setup.setup_database.getDatabase)

pgPassword :: Reporter Report -> Track' FilePath
pgPassword r = Track $ \path ->
    Secrets.sharedSecretFile
        (contramap CreatePassword r)
        Debian.openssl
        (Secrets.Secret Secrets.Hex 48 path)

pgConnstringFile :: Reporter Report -> ConnString FilePath -> Track' FilePath
pgConnstringFile r conn =
    Track $ \cstringpath ->
        op "pg-connstring" (deps [run (pgPassword r) passFile]) $ \actions ->
            actions
                { ref = dotRef $ "connstring: " <> Text.pack cstringpath
                , help = "store connection string at " <> Text.pack cstringpath
                , up = do
                    pass <- readPassword passFile
                    let contents = connstring (withPassword conn pass)
                    Text.writeFile cstringpath contents
                }
  where
    passFile :: FilePath
    passFile = conn.connstring_user_pass
