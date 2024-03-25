{-# LANGUAGE DeriveGeneric #-}

module SreBox.Postgrest where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import System.Directory
import System.FilePath

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Continuation as Continuation
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.G (G (..))
import Salmon.Op.OpGraph (OpGraph (..), inject)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.Track (Track (..), Tracked (..), bindTracked, opGraph, using, (>*<))
import Salmon.Reporter

import qualified Salmon.Builtin.Nodes.Git as Git
import SreBox.CabalBuilding (cabalBinUpload, postgrest)
import qualified SreBox.CabalBuilding as CabalBuilding
import SreBox.Environment
import qualified SreBox.PostgresInit as PGInit
import qualified SreBox.PostgresMigrations as PGMigrate

-------------------------------------------------------------------------------
data Report
    = Build !CabalBuilding.Report
    | Upload !CabalBuilding.Report
    | CallSelf !Self.Report
    | UploadSelf !Self.Report
    | UploadFile !Rsync.Report
    | GetSources !Git.Report
    | InitializeDB !PGInit.Report
    | Migration !PGMigrate.Report
    | SetupSystemd !Systemd.Report
    | GenerateJWTSecret !Secrets.Report
    deriving (Show)

-------------------------------------------------------------------------------

type ServiceName = Text

type PortNumber = Int

data PostgrestConfig
    = PostgrestConfig
    { postgrest_cfg_serviceName :: ServiceName
    , postgrest_cfg_portnum :: PortNumber
    , postgrest_cfg_anonRole :: Postgres.Group
    , postgrest_cfg_connectionString :: Postgres.ConnString FilePath
    , postgrest_cfg_configPath :: FilePath
    -- TODO: schemas
    }

data PostgrestSetup
    = PostgrestSetup
    { postgrest_setup_serviceName :: ServiceName
    , postgrest_setup_localBinPath :: FilePath
    , postgrest_setup_localConfigPath :: FilePath
    , postgrest_setup_localConnstringPath :: FilePath
    }
    deriving (Generic)
instance FromJSON PostgrestSetup
instance ToJSON PostgrestSetup

setupPostgrest ::
    (FromJSON directive, ToJSON directive) =>
    Reporter Report ->
    Track' Ssh.Remote ->
    Track' directive ->
    Self.Remote ->
    Self.SelfPath ->
    (PostgrestSetup -> directive) ->
    PostgrestConfig ->
    Op
setupPostgrest r mkRemote simulate selfRemote selfpath toSpec cfg =
    using (cabalBinUpload (contramap Upload r) (postgrest (contramap Build r)) rsyncRemote) $ \remotepath ->
        let
            setup = PostgrestSetup cfg.postgrest_cfg_serviceName remotepath remoteConfig remoteConnstring
         in
            opGraph (continueRemotely setup) `inject` configUploads
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    configUploads = op "uploads-postgrest-configs" (deps [uploadConfig, uploadConnStringFile]) $ \actions ->
        actions
            { notes =
                [ "for service: " <> cfg.postgrest_cfg_serviceName
                ]
            , ref = dotRef $ "postgrest:uploads" <> cfg.postgrest_cfg_serviceName
            }

    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup

    recurse setup selfref =
        Self.callSelfAsSudo (contramap CallSelf r) mkRemote selfref simulate CLI.Up (toSpec setup)

    -- upload self
    self = Self.uploadSelf (contramap UploadSelf r) "tmp" selfRemote selfpath

    -- upload config file
    remoteConfig = Text.unpack $ "tmp/postgrest-" <> cfg.postgrest_cfg_serviceName <> ".config"
    remoteConnstring = Text.unpack $ "tmp/postgrest-" <> cfg.postgrest_cfg_serviceName <> ".connstring"

    upload gen localpath distpath =
        Rsync.sendFile (contramap UploadFile r) Debian.rsync (FS.Generated gen localpath) rsyncRemote distpath

    uploadConfig =
        upload makeConfig cfg.postgrest_cfg_configPath remoteConfig
      where
        -- make config using remote filepaths as contents
        makeConfig =
            Track $ \path ->
                let connectionString = connstringPath cfg.postgrest_cfg_serviceName
                    jwtSigningKey = jwtSigningKeyPath cfg.postgrest_cfg_serviceName
                 in FS.filecontents $
                        (FS.FileContents path (renderConfig cfg jwtSigningKey connectionString))

    uploadConnStringFile =
        upload makeConnstring connstringFilePath remoteConnstring
      where
        makeConnstring = PGMigrate.pgConnstringFile (contramap Migration r) (cfg.postgrest_cfg_connectionString)

    connstringFilePath = Text.unpack $ mconcat ["./configs/posgtrest/postgrest-", cfg.postgrest_cfg_serviceName, ".connstring"]

renderConfig :: PostgrestConfig -> FilePath -> FilePath -> Text
renderConfig cfg secretFilePath connstringFilePath =
    Text.unlines
        [ kv_text "db-anon-role" $ Postgres.groupRole cfg.postgrest_cfg_anonRole
        , kv_text "db-channel" "pgrst"
        , kv_bool "db-channel-enabled" True
        , kv_bool "db-config" True
        , kv_text "db-extra-search-path" "public"
        , kv_int "db-pool" 10
        , kv_bool "db-prepared-statements" True
        , kv_text "db-schemas" "public"
        , kv_text "db-tx-end" "commit"
        , kv_text "db-uri" (Text.pack $ '@' : connstringFilePath)
        , kv_text "jwt-role-claim-key" ".role"
        , kv_text "jwt-secret" (Text.pack $ '@' : secretFilePath)
        , kv_bool "jwt-secret-is-base64" True
        , kv_text "log-level" "error"
        , kv_text "openapi-mode" "follow-privileges"
        , kv_text "openapi-server-proxy-uri" ""
        , kv_text "server-host" "!4"
        , kv_int "server-port" cfg.postgrest_cfg_portnum
        , kv_bool "server-timing-enabled" False
        ]
  where
    kv_text k v = Text.unwords [k, "=", "\"" <> v <> "\""]
    kv_bool k v = Text.unwords [k, "=", if v then "true" else "false"]
    kv_int k v = Text.unwords [k, "=", Text.pack $ show v]

systemdPostgrest :: Reporter Report -> PostgrestSetup -> Op
systemdPostgrest r setup =
    Systemd.systemdService (contramap SetupSystemd r) Debian.systemctl trackConfig config
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \cfg ->
        let
            execPath = Systemd.start_path $ Systemd.service_execStart $ Systemd.config_service $ cfg
            copybin = FS.fileCopy (postgrest_setup_localBinPath setup) execPath
            copyConfig = FS.fileCopy (postgrest_setup_localConfigPath setup) cpath
            copySecret = FS.fileCopy (postgrest_setup_localConnstringPath setup) spath
         in
            op "setup-systemd-for-postgrest" (deps [copybin, copyConfig, generateJwtSecret, copySecret]) $ \actions ->
                actions
                    { ref = dotRef $ "postgrest:systemd" <> setup.postgrest_setup_serviceName
                    }

    config :: Systemd.Config
    config = Systemd.Config tgt unit service install

    tgt :: Systemd.UnitTarget
    tgt = "salmon-postgrest-" <> setup.postgrest_setup_serviceName <> ".service"

    unit :: Systemd.Unit
    unit = Systemd.Unit (mconcat ["Postgrest from Salmon (", setup.postgrest_setup_serviceName, ")"]) "network-online.target"

    service :: Systemd.Service
    service = Systemd.Service Systemd.Simple "root" "root" "007" start Systemd.OnFailure Systemd.Process "/opt/rundir/postgrest"

    start :: Systemd.Start
    start =
        Systemd.Start
            "/opt/rundir/postgrest/bin/postgrest"
            [ Text.pack cpath
            ]

    cpath :: FilePath
    cpath = configPath setup.postgrest_setup_serviceName

    spath :: FilePath
    spath = connstringPath setup.postgrest_setup_serviceName

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"

    generateJwtSecret :: Op
    generateJwtSecret =
        Secrets.sharedSecretFile
            (contramap GenerateJWTSecret r)
            Debian.openssl
            (Secrets.Secret Secrets.Base64 32 (jwtSigningKeyPath setup.postgrest_setup_serviceName))

-------------------------------------------------------------------------------

configPath :: ServiceName -> FilePath
configPath name =
    "/opt/rundir/postgrest" </> Text.unpack name </> "config.txt"

connstringPath :: ServiceName -> FilePath
connstringPath name =
    "/opt/rundir/postgrest" </> Text.unpack name </> "connstring.connstring"

jwtSigningKeyPath :: ServiceName -> FilePath
jwtSigningKeyPath name =
    "/opt/rundir/postgrest" </> Text.unpack name </> "jwt-shared-secret"

-------------------------------------------------------------------------------

data PostgrestMigratedApiConfig
    = PostgrestMigratedApiConfig
    { pma_serviceName :: Text
    , pma_apiPort :: Int
    , pma_repo :: Git.Repo
    , pma_migrationTip :: FilePath
    , pma_migrationsPrefix :: FilePath
    , pma_migrate_connstring :: Postgres.ConnString FilePath
    , pma_setrole_connstring :: Postgres.ConnString FilePath
    , pma_fallback_role :: Postgres.Group
    }

postgrestMigratedApi ::
    (FromJSON directive, ToJSON directive) =>
    Reporter Report ->
    Track' directive ->
    (PGInit.InitSetup FilePath -> directive) ->
    (PGMigrate.MigrationSetup -> directive) ->
    (PostgrestSetup -> directive) ->
    Self.SelfPath ->
    Self.Remote ->
    PostgrestMigratedApiConfig ->
    Op
postgrestMigratedApi r simulate toSpec0 toSpec1 toSpec2 selfpath remoteSelf cfg =
    op "postgrest-api" (deps [prest `inject` migrateDBWithUser `inject` initDB]) $ \actions ->
        actions
            { ref = dotRef $ "prest-api" <> cfg.pma_serviceName
            }
  where
    inputMigrations :: IO (G PGMigrate.MigrationFile)
    inputMigrations =
        let reader =
                Migrations.addFilePrefix (Git.clonedir cfg.pma_repo </> cfg.pma_migrationsPrefix) $
                    PGMigrate.defaultMigrationReader
         in G . fmap node <$> Migrations.loadMigrations reader cfg.pma_migrationTip

    exposedDatabase = cfg.pma_migrate_connstring.connstring_db
    migrateUser = cfg.pma_migrate_connstring.connstring_user
    setroleUser = cfg.pma_setrole_connstring.connstring_user
    anonRoleGroup = cfg.pma_fallback_role

    cloneSource = Track $ const $ Git.repo (contramap GetSources r) Debian.git cfg.pma_repo

    initDB =
        PGInit.remoteSetupPG
            (contramap InitializeDB r)
            simulate
            remoteSelf
            selfpath
            toSpec0
            ( PGInit.InitSetup
                exposedDatabase
                [ (migrateUser, cfg.pma_migrate_connstring.connstring_user_pass, [Postgres.CONNECT, Postgres.CREATE], [])
                , (setroleUser, cfg.pma_setrole_connstring.connstring_user_pass, [Postgres.CONNECT], [anonRoleGroup])
                ]
                [ (anonRoleGroup, [])
                ]
            )

    migrateDBWithUser =
        PGMigrate.remoteMigrateOpaqueSetup
            (contramap Migration r)
            simulate
            remoteSelf
            selfpath
            toSpec1
            ( PGMigrate.RemoteMigrateConfig
                (TrackedIO $ Tracked cloneSource inputMigrations)
                migrateUser
                exposedDatabase
                cfg.pma_migrate_connstring.connstring_user_pass
            )

    prest = op "postgrest" (deps [setupPrest]) $ \actions ->
        actions
            { ref = dotRef $ "prest-service" <> cfg.pma_serviceName
            }

    setupPrest =
        setupPostgrest
            r
            Ssh.preExistingRemoteMachine
            simulate
            remoteSelf
            selfpath
            toSpec2
            prestConfig

    serviceName = "postgrest-api-" <> cfg.pma_serviceName
    prestConfigPath = Text.unpack $ mconcat ["./configs/posgtrest/postgrest-", cfg.pma_serviceName, ".config"]
    prestConfig =
        PostgrestConfig
            serviceName
            cfg.pma_apiPort
            anonRoleGroup
            cfg.pma_setrole_connstring
            prestConfigPath
