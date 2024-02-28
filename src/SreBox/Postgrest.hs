{-# LANGUAGE DeriveGeneric #-}

module SreBox.Postgrest where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import qualified Data.Text as Text
import System.Directory
import System.FilePath

import Salmon.Builtin.Extension
import Salmon.Op.Ref (dotRef)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track (Track(..), (>*<), using, opGraph, bindTracked)
import qualified Salmon.Builtin.CommandLine as CLI
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Continuation as Continuation
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Postgres as Postgres

import SreBox.CabalBuilding
import SreBox.Environment

-------------------------------------------------------------------------------

type ServiceName = Text

type PortNumber = Int

data PostgrestConfig
  = PostgrestConfig
  { postgrest_cfg_serviceName :: ServiceName
  , postgrest_cfg_portnum :: PortNumber
  , postgrest_cfg_anonRole :: Postgres.User
  , postgrest_cfg_connectionString :: Postgres.ConnString Postgres.Password
  , postgrest_cfg_configPath :: FilePath
  -- TODO: anon-role, schemas
  }

data PostgrestSetup
  = PostgrestSetup
  { postgrest_setup_serviceName :: ServiceName
  , postgrest_setup_localBinPath :: FilePath
  , postgrest_setup_localConfigPath :: FilePath
  }
  deriving (Generic)
instance FromJSON PostgrestSetup
instance ToJSON PostgrestSetup

setupPostgrest
  :: (FromJSON directive, ToJSON directive)
  => Track' Ssh.Remote
  -> Self.Remote
  -> Self.SelfPath
  -> (PostgrestSetup -> directive)
  -> PostgrestConfig
  -> Op
setupPostgrest mkRemote selfRemote selfpath toSpec cfg =
  using (cabalBinUpload postgrest rsyncRemote) $ \remotepath ->
    let
      setup = PostgrestSetup cfg.postgrest_cfg_serviceName remotepath remoteConfig
    in
    opGraph (continueRemotely setup) `inject` configUploads
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    configUploads = op "uploads-postgrest-configs" (deps [uploadConfig]) $ \actions -> actions {
      notes = [ "for service: " <> cfg.postgrest_cfg_serviceName
              ]
    , ref = dotRef $ "postgrest:uploads" <> cfg.postgrest_cfg_serviceName
    }

    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup

    recurse setup selfref =
      Self.callSelfAsSudo mkRemote selfref CLI.Up (toSpec setup)

    -- upload self
    self = Self.uploadSelf "tmp" selfRemote selfpath

    -- upload config file
    remoteConfig  = Text.unpack $ "tmp/postgrest-" <> cfg.postgrest_cfg_serviceName <> ".config"

    upload gen localpath distpath =
      Rsync.sendFile Debian.rsync (FS.Generated gen localpath) rsyncRemote distpath

    uploadConfig =
      upload makeConcfig cfg.postgrest_cfg_configPath remoteConfig
      where
        makeConcfig =
          Track $ \path -> FS.filecontents (FS.FileContents path (renderConfig cfg))

renderConfig :: PostgrestConfig -> Text
renderConfig cfg =
  Text.unlines
  [ kv_text "db-anon-role" $ Postgres.userRole cfg.postgrest_cfg_anonRole
  , kv_text "db-channel" "pgrst"
  , kv_bool "db-channel-enabled" True
  , kv_bool "db-config" True
  , kv_text "db-extra-search-path" "public"
  , kv_int "db-pool" 10
  , kv_bool "db-prepared-statements" True
  , kv_text "db-schemas" "public"
  , kv_text "db-tx-end" "commit"
  , kv_text "db-uri" (Postgres.connstring cfg.postgrest_cfg_connectionString)
  , kv_text "jwt-role-claim-key" ".role"
  -- todo: , kv_text "# jwt-secret" "@filepath"
  , kv_bool "jwt-secret-is-base64" False
  , kv_text "log-level" "error"
  , kv_text "openapi-mode" "follow-privileges"
  , kv_text "openapi-server-proxy-uri" ""
  , kv_text "server-host" "!4"
  , kv_int "server-port" cfg.postgrest_cfg_portnum
  , kv_bool "server-timing-enabled" False
  ]

  where
    kv_text k v = Text.unwords [ k , "=", "\"" <> v <> "\"" ]
    kv_bool k v = Text.unwords [ k , "=", if v then "true" else "false" ]
    kv_int k v = Text.unwords [ k , "=", Text.pack $ show v ]

systemdPostgrest :: PostgrestSetup -> Op
systemdPostgrest setup =
    Systemd.systemdService Debian.systemctl trackConfig config
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \cfg ->
      let
          execPath = Systemd.start_path $ Systemd.service_execStart $ Systemd.config_service $ cfg
          copybin = FS.fileCopy (postgrest_setup_localBinPath setup) execPath
          copyConfig = FS.fileCopy (postgrest_setup_localConfigPath setup) configPath
      in
      op "setup-systemd-for-postgrest" (deps [copybin, copyConfig]) $ \actions -> actions {
        ref = dotRef $ "postgrest:systemd" <> setup.postgrest_setup_serviceName
      }

    config :: Systemd.Config
    config = Systemd.Config tgt unit service install

    tgt :: Systemd.UnitTarget
    tgt = "salmon-postgrest-" <> setup.postgrest_setup_serviceName <> ".service"

    configPath :: FilePath
    configPath = "/opt/rundir/postgrest" </> Text.unpack setup.postgrest_setup_serviceName </> "config.txt"

    unit :: Systemd.Unit
    unit = Systemd.Unit (mconcat ["Postgrest from Salmon (", setup.postgrest_setup_serviceName, ")"]) "network-online.target"

    service :: Systemd.Service
    service = Systemd.Service Systemd.Simple "root" "root" "007" start Systemd.OnFailure Systemd.Process "/opt/rundir/postgrest"

    start :: Systemd.Start
    start =
      Systemd.Start "/opt/rundir/postgrest/bin/postgrest"
        [ Text.pack configPath
        ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"
