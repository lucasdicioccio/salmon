{-# LANGUAGE DeriveGeneric #-}

module SreBox.KitchenSinkMultiSites where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.FilePath ((</>), takeDirectory)

import Salmon.Op.Track
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (dotRef)
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.CommandLine as CLI
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Systemd as Systemd

import SreBox.CabalBuilding

import KitchenSink.Engine.MultiSiteConfig
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.Config

data KitchenSinkConfig
  = KitchenSinkConfig {
    ks_cfg_fallback_stanza :: StanzaConfig
  , ks_cfg_services_stanzas :: [StanzaConfig]
  }

ks_cfg_stanzas :: KitchenSinkConfig -> [StanzaConfig]
ks_cfg_stanzas c = c.ks_cfg_fallback_stanza : c.ks_cfg_services_stanzas

data StanzaConfig
  = StanzaConfig {
    stanza_cfg_certSpec :: (Certs.Domain, Text)
  , stanza_cfg_title :: Text
  , stanza_cfg_keyPath :: FilePath
  , stanza_cfg_pemPath :: FilePath
  -- todo: generalize source to presence/absence of api-proxy-configs, non-git configs
  , stanza_cfg_repo :: Git.Repo
  , stanza_cfg_sourceSubdir :: FilePath
  , stanza_cfg_dhallSubdir :: FilePath
  , stanza_cfg_ks_proxy_config :: ApiProxyConfig
  }

data KitchenSinkSetup
  = KitchenSinkSetup
  { ks_setup_localBinPath :: FilePath
  , ks_setup_localUploadRoot :: FilePath
  , ks_setup_fallback_stanza :: StanzaSetup
  , ks_setup_services_stanzas :: [StanzaSetup]
  }
  deriving (Generic)
instance FromJSON KitchenSinkSetup
instance ToJSON KitchenSinkSetup

data StanzaSetup
  = StanzaSetup {
    stanza_setup_domain :: Text
  , stanza_setup_title :: Text
  , stanza_setup_dir :: FilePath
  , stanza_setup_keyPath :: FilePath
  , stanza_setup_pemPath :: FilePath
  , stanza_setup_sourceDir :: FilePath
  , stanza_setup_subdir :: FilePath
  , stanza_setup_dhall_subdir :: FilePath
  , stanza_setup_ks_proxy_config :: ApiProxyConfig
  }
  deriving (Generic)
instance FromJSON StanzaSetup
instance ToJSON StanzaSetup

configToSetup :: StanzaConfig -> StanzaSetup
configToSetup cfg =
  StanzaSetup
    dom
    title
    remoteStanzaDir
    keypath
    pempath
    sourcedir
    subdir
    dhalldir
    proxy
  where
    dom = Certs.getDomain . fst $ cfg.stanza_cfg_certSpec
    title = cfg.stanza_cfg_title
    remoteStanzaDir = "tmp/ks" </> Text.unpack dom
    sourcedir  = remoteStanzaDir </> "repo"
    pempath  = remoteStanzaDir </> "ks.pem"
    keypath  = remoteStanzaDir </> "ks.key"
    subdir = cfg.stanza_cfg_sourceSubdir
    dhalldir = cfg.stanza_cfg_dhallSubdir
    proxy = cfg.stanza_cfg_ks_proxy_config


ks_setup_stanzas :: KitchenSinkSetup -> [StanzaSetup]
ks_setup_stanzas c = c.ks_setup_fallback_stanza : c.ks_setup_services_stanzas


setupKS
  :: (FromJSON directive, ToJSON directive)
  => Track' Ssh.Remote
  -> Track' (Certs.Domain, Text)
  -> Tracked' FilePath
  -> Self.Remote
  -> Self.SelfPath
  -> KitchenSinkConfig
  -> (KitchenSinkSetup -> directive)
  -> Op
setupKS mkRemote mkCerts cloneKitchenSink selfRemote selfpath cfg toSpec =
  using (cabalBinUpload cloneKitchenSink rsyncRemote) $ \remotepath ->
    let
      setup = KitchenSinkSetup remotepath uploadRoot fallback services
    in
    op "remote-ks-setup" (depSequence setup) id
  where
    fallback = configToSetup cfg.ks_cfg_fallback_stanza
    services = configToSetup <$> cfg.ks_cfg_services_stanzas
    uploadRoot = "tmp/ks-multisites"
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote
    finalize setup = opGraph (continueRemotely setup)
    depSequence setup = deps [ finalize setup `inject` uploads ]
    -- upload sites
    uploads = op "uploads-ks" (deps stanzaUploads) id
    stanzaUploads = stanzaUpload mkCerts rsyncRemote <$> ks_cfg_stanzas cfg
    -- upload self
    self = Self.uploadSelf "tmp" selfRemote selfpath
    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup
    recurse setup selfref =
      Self.callSelfAsSudo mkRemote selfref CLI.Up (toSpec setup)

stanzaUpload
  :: Track' (Certs.Domain, Text)
  -> Rsync.Remote
  -> StanzaConfig
  -> Op
stanzaUpload mkCerts rsyncRemote cfg =
  using (Git.repodir cloneSite cfg.stanza_cfg_repo "") $ \repoDir ->
    op "uploads-ks-stanza" (deps [uploadCert, uploadKey, uploadSources repoDir]) setRef `inject` mkRemoteDir
  where
    setup = configToSetup cfg
    setRef actions = actions { ref = dotRef $ "uploads-ks-stanza" <> setup.stanza_setup_domain }
    sshRemote = (\(Rsync.Remote a b) -> Ssh.Remote a b) rsyncRemote
    mkRemoteDir = Ssh.call Debian.ssh ignoreTrack sshRemote "mkdir" ["-p", Text.pack setup.stanza_setup_dir] ""

    cloneSite = Track $ Git.repo Debian.git
    -- upload sources
    uploadSources repoDir =
      Rsync.sendDir Debian.rsync ignoreTrack (FS.Directory (FS.directoryPath repoDir <> "/")) rsyncRemote setup.stanza_setup_sourceDir

    -- upload certificate and key
    upload gen localpath distpath =
      Rsync.sendFile Debian.rsync (FS.Generated gen localpath) rsyncRemote distpath

    uploadCert = upload siteCert cfg.stanza_cfg_pemPath setup.stanza_setup_pemPath
    uploadKey = upload siteCert cfg.stanza_cfg_keyPath setup.stanza_setup_keyPath

    siteCert = Track $ const $ run mkCerts cfg.stanza_cfg_certSpec

ksRunDir :: FilePath
ksRunDir = "/opt/rundir/ks-multisite"

ksConfigPath :: FilePath
ksConfigPath = ksRunDir </> "kitchen-sink.config.json"

stanzaPath :: FilePath -> StanzaSetup -> FilePath
stanzaPath sub ss = ksRunDir </> "services" </> Text.unpack ss.stanza_setup_domain </> sub

siteSrcDir,siteSrcPath,keyPath,pemPath,siteExecRoot,siteDhallRoot :: StanzaSetup -> FilePath
pemPath = stanzaPath "cert.pem"
keyPath = stanzaPath "cert.key"
siteSrcDir = stanzaPath "src"
siteExecRoot = siteSrcDir
siteSrcPath ss = siteSrcDir ss </> ss.stanza_setup_subdir
siteDhallRoot ss = siteSrcDir ss </> ss.stanza_setup_dhall_subdir

systemdKitchenSink :: KitchenSinkSetup -> Op
systemdKitchenSink setup =
    Systemd.systemdService Debian.systemctl trackConfig systemdConfig
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \cfg ->
      let
          execPath = Systemd.start_path $ Systemd.service_execStart $ Systemd.config_service $ cfg
          copybin = FS.fileCopy setup.ks_setup_localBinPath execPath
      in
      op "setup-systemd-for-ks" (deps [copybin, localSetup]) id

    localSetup :: Op
    localSetup =
      op "ks-setup" (deps [sysDeps, ksconfigFile, prepareStanzas]) id

    prepareStanzas :: Op
    prepareStanzas =
      op "ks-setup-stanzas" (deps $ prepareStanza <$> ks_setup_stanzas setup) id

    prepareStanza :: StanzaSetup -> Op
    prepareStanza ss =
      let
          copypem = FS.fileCopy ss.stanza_setup_pemPath (pemPath ss)
          copykey = FS.fileCopy ss.stanza_setup_keyPath (keyPath ss)
          movesrc = FS.moveDirectory ss.stanza_setup_sourceDir (siteSrcDir ss)
      in
      op "ks-setup-stanza" (deps [movesrc, copypem, copykey]) $ \actions  -> actions {
        ref = dotRef $ "prepare-ks-stanza" <> ss.stanza_setup_domain
      }

    sysDeps :: Op
    sysDeps =
      op "system" (deps [Debian.deb (Debian.Package "graphviz")]) id

    ksconfigFile :: Op
    ksconfigFile = FS.filecontents $ FS.FileContents ksConfigPath (Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode $ ksConfigContents setup)

    systemdConfig :: Systemd.Config
    systemdConfig = Systemd.Config tgt unit service install

    tgt :: Systemd.UnitTarget
    tgt = "salmon-ks-multisite.service"

    unit :: Systemd.Unit
    unit = Systemd.Unit "Kitchen-Sink from Salmon" "network-online.target"

    service :: Systemd.Service
    service = Systemd.Service Systemd.Simple "root" "root" "007" start Systemd.OnFailure Systemd.Process ksRunDir

    start :: Systemd.Start
    start =
      Systemd.Start "/opt/rundir/ks/bin/kitchen-sink"
        [ "multisite"
        , "--configFile", Text.pack ksConfigPath
        , "--httpPort", "80"
        , "--httpsPort", "443"
        , "--tlsCertFile", Text.pack (pemPath setup.ks_setup_fallback_stanza)
        , "--tlsKeyFile", Text.pack (keyPath setup.ks_setup_fallback_stanza)
        ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"

ksConfigContents :: KitchenSinkSetup -> MultiSiteConfig
ksConfigContents setup =
    MultiSiteConfig
      (stanza <$> setup.ks_setup_services_stanzas)
      (FallbackSite $ stanza setup.ks_setup_fallback_stanza)
  where
    domain :: StanzaSetup -> Text
    domain ss = ss.stanza_setup_domain

    url :: StanzaSetup -> Text
    url ss = "https://" <> domain ss

    tls :: StanzaSetup -> TLSStanza
    tls ss = TLSStanza Nothing (CertificateFileSource (CertificateFiles (pemPath ss) (keyPath ss)))

    site :: StanzaSetup -> SourceStanza
    site ss =
      KitchenSinkDirectorySource
      $ KitchenSinkDirectorySourceStanza
          (siteSrcPath ss)
          (siteInfo ss)
          (Just $ siteDhallRoot ss)
          (Just $ siteExecRoot ss)

    siteInfo :: StanzaSetup -> SiteInfo
    siteInfo ss =
      SiteInfo ss.stanza_setup_title (url ss) Nothing Nothing

    proxy :: StanzaSetup -> ApiProxyConfig
    proxy ss = ss.stanza_setup_ks_proxy_config

    stanza :: StanzaSetup -> SiteStanza
    stanza ss =
      SiteStanza
        (domain ss)
        []
        [tls ss]
        (site ss)
        (proxy ss)
