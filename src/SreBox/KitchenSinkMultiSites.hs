{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SreBox.KitchenSinkMultiSites where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe,catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.FilePath ((</>), takeDirectory)
import Data.Foldable (toList)

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
    ks_cfg_fallback_stanza :: Maybe StanzaConfig
  , ks_cfg_services_stanzas :: [StanzaConfig]
  }

ks_cfg_stanzas :: KitchenSinkConfig -> [StanzaConfig]
ks_cfg_stanzas c = toList c.ks_cfg_fallback_stanza <> c.ks_cfg_services_stanzas

ks_cfg_site_stanzas :: KitchenSinkConfig -> [GitSiteStanzaConfig]
ks_cfg_site_stanzas c = toList (f =<< c.ks_cfg_fallback_stanza) <> (catMaybes $ fmap f c.ks_cfg_services_stanzas)

  where
    f :: StanzaConfig -> Maybe GitSiteStanzaConfig
    f (StanzaConfig_Site cfg) = Just cfg
    f (StanzaConfig_Proxy _) = Nothing

data StanzaConfig
  = StanzaConfig_Site GitSiteStanzaConfig
  | StanzaConfig_Proxy ProxyStanzaConfig

viewPemPath :: StanzaConfig -> FilePath
viewPemPath (StanzaConfig_Site cfg) = cfg.stanza_cfg_pemPath
viewPemPath (StanzaConfig_Proxy cfg) = cfg.stanza_cfg_pemPath

viewKeyPath :: StanzaConfig -> FilePath
viewKeyPath (StanzaConfig_Site cfg) = cfg.stanza_cfg_keyPath
viewKeyPath (StanzaConfig_Proxy cfg) = cfg.stanza_cfg_keyPath

viewCertSpec :: StanzaConfig -> (Certs.Domain,Text)
viewCertSpec (StanzaConfig_Site cfg) = cfg.stanza_cfg_certSpec
viewCertSpec (StanzaConfig_Proxy cfg) = cfg.stanza_cfg_certSpec

data ProxyStanzaConfig
  = ProxyStanzaConfig {
    stanza_cfg_certSpec :: (Certs.Domain, Text)
  , stanza_cfg_keyPath :: FilePath
  , stanza_cfg_pemPath :: FilePath
  , stanza_cfg_ks_proxy_config :: ApiProxyConfig
  }

data GitSiteStanzaConfig
  = GitSiteStanzaConfig {
    stanza_cfg_certSpec :: (Certs.Domain, Text)
  , stanza_cfg_title :: Text
  , stanza_cfg_keyPath :: FilePath
  , stanza_cfg_pemPath :: FilePath
  , stanza_cfg_repo :: Git.Repo
  , stanza_cfg_sourceSubdir :: FilePath
  , stanza_cfg_dhallSubdir :: FilePath
  , stanza_cfg_ks_proxy_config :: ApiProxyConfig
  , stanza_cfg_ks_linked_sites :: [LinkedSite]
  }

data KitchenSinkSetup
  = KitchenSinkSetup
  { ks_setup_localBinPath :: FilePath
  , ks_setup_localUploadRoot :: FilePath
  , ks_setup_fallback_stanza :: Maybe StanzaSetup
  , ks_setup_services_stanzas :: [StanzaSetup]
  }
  deriving (Generic)
instance FromJSON KitchenSinkSetup
instance ToJSON KitchenSinkSetup

data StanzaSetup
  = StanzaSetup {
    stanza_setup_domain :: Text
  , stanza_setup_dir :: FilePath
  , stanza_setup_keyPath :: FilePath
  , stanza_setup_pemPath :: FilePath
  , stanza_setup_ks_proxy_config :: ApiProxyConfig
  , stanza_setup_site :: Maybe SiteSetup
  }
  deriving (Generic)
instance FromJSON StanzaSetup
instance ToJSON StanzaSetup

data SiteSetup 
  = SiteSetup {
    site_setup_title :: Text
  , site_setup_sourceDir :: FilePath
  , site_setup_subdir :: FilePath
  , site_setup_dhall_subdir :: FilePath
  , site_setup_ks_linked_sites :: [LinkedSite]
  }
  deriving (Generic)
instance FromJSON SiteSetup
instance ToJSON SiteSetup

configToSetup :: StanzaConfig -> StanzaSetup
configToSetup s =
  case s of
    StanzaConfig_Site cfg -> fst (gitConfigToSetup cfg)
    StanzaConfig_Proxy cfg -> proxyConfigToSetup cfg

proxyConfigToSetup :: ProxyStanzaConfig -> StanzaSetup
proxyConfigToSetup cfg =
  StanzaSetup
    dom
    remoteStanzaDir
    keypath
    pempath
    proxy
    Nothing
  where
    dom = Certs.getDomain . fst $ cfg.stanza_cfg_certSpec
    remoteStanzaDir = "tmp/ks" </> Text.unpack dom
    pempath  = remoteStanzaDir </> "ks.pem"
    keypath  = remoteStanzaDir </> "ks.key"
    proxy = cfg.stanza_cfg_ks_proxy_config
    linkedSites = []


gitConfigToSetup :: GitSiteStanzaConfig -> (StanzaSetup, SiteSetup)
gitConfigToSetup cfg =
   (ss, siteSetup)
  where
    ss = StanzaSetup
        dom
        remoteStanzaDir
        keypath
        pempath
        proxy
        (Just siteSetup)

    siteSetup = SiteSetup
        title
        sourcedir
        subdir
        dhalldir
        linkedSites
    dom = Certs.getDomain . fst $ cfg.stanza_cfg_certSpec
    title = cfg.stanza_cfg_title
    remoteStanzaDir = "tmp/ks" </> Text.unpack dom
    sourcedir  = remoteStanzaDir </> "repo"
    pempath  = remoteStanzaDir </> "ks.pem"
    keypath  = remoteStanzaDir </> "ks.key"
    subdir = cfg.stanza_cfg_sourceSubdir
    dhalldir = cfg.stanza_cfg_dhallSubdir
    proxy = cfg.stanza_cfg_ks_proxy_config
    linkedSites = cfg.stanza_cfg_ks_linked_sites

ks_setup_stanzas :: KitchenSinkSetup -> [StanzaSetup]
ks_setup_stanzas c = toList c.ks_setup_fallback_stanza <> c.ks_setup_services_stanzas

setupKS
  :: (FromJSON directive, ToJSON directive)
  => Track' Ssh.Remote
  -> Track' (Certs.Domain, Text)
  -> Tracked' FilePath
  -> Track' directive
  -> Self.Remote
  -> Self.SelfPath
  -> KitchenSinkConfig
  -> (KitchenSinkSetup -> directive)
  -> Op
setupKS mkRemote mkCerts cloneKitchenSink simulate selfRemote selfpath cfg toSpec =
  using (cabalBinUpload cloneKitchenSink rsyncRemote) $ \remotepath ->
    let
      setup = KitchenSinkSetup remotepath uploadRoot fallback services
    in
    op "remote-ks-setup" (depSequence setup) id
  where
    fallback = configToSetup <$> cfg.ks_cfg_fallback_stanza
    services = configToSetup <$> cfg.ks_cfg_services_stanzas
    uploadRoot = "tmp/ks-multisites"
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote
    finalize setup = opGraph (continueRemotely setup)
    depSequence setup = deps [ finalize setup `inject` uploads ]
    -- upload sites
    uploads = op "uploads-ks" (deps stanzaUploads) id
    stanzaUploads =  srcUploads <> certUploads
    srcUploads = stanzaUploadSources rsyncRemote <$> ks_cfg_site_stanzas cfg
    certUploads = stanzaUploadCerts mkCerts rsyncRemote <$> ks_cfg_stanzas cfg
    -- upload self
    self = Self.uploadSelf "tmp" selfRemote selfpath
    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup
    recurse setup selfref =
      Self.callSelfAsSudo mkRemote selfref simulate CLI.Up (toSpec setup)

stanzaUploadCerts
  :: Track' (Certs.Domain, Text)
  -> Rsync.Remote
  -> StanzaConfig
  -> Op
stanzaUploadCerts mkCerts rsyncRemote cfg =
    op "uploads-ks-stanza-certs" (deps [uploadCert, uploadKey]) setRef `inject` mkRemoteDir
  where
    setup = configToSetup cfg
    setRef actions = actions { ref = dotRef $ "uploads-ks-stanza" <> setup.stanza_setup_domain }
    sshRemote = (\(Rsync.Remote a b) -> Ssh.Remote a b) rsyncRemote
    mkRemoteDir = Ssh.call Debian.ssh ignoreTrack sshRemote "mkdir" ["-p", Text.pack setup.stanza_setup_dir] ""

    -- upload certificate and key
    upload gen localpath distpath =
      Rsync.sendFile Debian.rsync (FS.Generated gen localpath) rsyncRemote distpath

    uploadCert = upload siteCert (viewPemPath cfg) setup.stanza_setup_pemPath
    uploadKey = upload siteCert (viewKeyPath cfg) setup.stanza_setup_keyPath
    siteCert = Track $ const $ run mkCerts (viewCertSpec cfg)

stanzaUploadSources
  :: Rsync.Remote
  -> GitSiteStanzaConfig
  -> Op
stanzaUploadSources rsyncRemote cfg =
  using (Git.repodir cloneSite cfg.stanza_cfg_repo "") $ \repoDir ->
    op "uploads-ks-stanza-src" (deps [uploadSources repoDir]) setRef `inject` mkRemoteDir
  where
    (setup,site) = gitConfigToSetup cfg
    setRef actions = actions { ref = dotRef $ "uploads-ks-stanza" <> setup.stanza_setup_domain }
    sshRemote = (\(Rsync.Remote a b) -> Ssh.Remote a b) rsyncRemote
    mkRemoteDir = Ssh.call Debian.ssh ignoreTrack sshRemote "mkdir" ["-p", Text.pack setup.stanza_setup_dir] ""

    cloneSite = Track $ Git.repo Debian.git
    -- upload sources
    uploadSources repoDir =
      Rsync.sendDir Debian.rsync ignoreTrack (FS.Directory (FS.directoryPath repoDir <> "/")) rsyncRemote site.site_setup_sourceDir

ksRunDir :: FilePath
ksRunDir = "/opt/rundir/ks-multisite"

lastResortPemPath :: FilePath
lastResortPemPath = ksRunDir </> "last-resort-cert/cert.pem"

lastResortKey :: Certs.Key
lastResortKey = Certs.Key Certs.RSA2048 (ksRunDir </> "last-resort-cert") "key.pem"

lastResortKeyPath :: FilePath
lastResortKeyPath = Certs.keyPath lastResortKey

ksConfigPath :: FilePath
ksConfigPath = ksRunDir </> "kitchen-sink.config.json"

stanzaPath :: FilePath -> StanzaSetup -> FilePath
stanzaPath sub ss = ksRunDir </> "services" </> Text.unpack ss.stanza_setup_domain </> sub

siteSrcDir,keyPath,pemPath,siteExecRoot :: StanzaSetup -> FilePath
pemPath = stanzaPath "cert.pem"
keyPath = stanzaPath "cert.key"
siteSrcDir = stanzaPath "src"
siteExecRoot = siteSrcDir

siteSrcPath,siteDhallRoot :: StanzaSetup -> SiteSetup -> FilePath
siteSrcPath ss site = siteSrcDir ss </> site.site_setup_subdir
siteDhallRoot ss site = siteSrcDir ss </> site.site_setup_dhall_subdir

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
      op "ks-setup" (deps [lastResortCert, sysDeps, ksconfigFile, prepareStanzas]) id

    prepareStanzas :: Op
    prepareStanzas =
      op "ks-setup-stanzas" (deps $ prepareStanza <$> ks_setup_stanzas setup) id

    prepareStanza :: StanzaSetup -> Op
    prepareStanza ss =
      let
          copypem = FS.fileCopy ss.stanza_setup_pemPath (pemPath ss)
          copykey = FS.fileCopy ss.stanza_setup_keyPath (keyPath ss)
          movesrc =
            maybe
              realNoop
              (\site -> FS.moveDirectory site.site_setup_sourceDir (siteSrcDir ss))
              ss.stanza_setup_site

      in
      op "ks-setup-stanza" (deps [movesrc, copypem, copykey]) $ \actions  -> actions {
        ref = dotRef $ "prepare-ks-stanza" <> ss.stanza_setup_domain
      }

    lastResortCert :: Op
    lastResortCert =
      case setup.ks_setup_fallback_stanza of
        Just _ -> realNoop
        Nothing -> Certs.selfSign Debian.openssl (Certs.SelfSigned lastResortPemPath (Certs.SigningRequest (Certs.Domain "kitchen-sink.local") lastResortKey "tmp/ks-multisites-self-cert/csr" "cert.csr"))

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
        , "--tlsCertFile", Text.pack (fromMaybe lastResortPemPath $ pemPath <$> setup.ks_setup_fallback_stanza)
        , "--tlsKeyFile", Text.pack (fromMaybe lastResortKeyPath $ keyPath <$> setup.ks_setup_fallback_stanza)
        ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"

ksConfigContents :: KitchenSinkSetup -> MultiSiteConfig
ksConfigContents setup =
    MultiSiteConfig
      (stanza <$> setup.ks_setup_services_stanzas)
      (maybe FallbackWithOminousError (FallbackSite . stanza) setup.ks_setup_fallback_stanza)
  where
    domain :: StanzaSetup -> Text
    domain ss = ss.stanza_setup_domain

    url :: StanzaSetup -> Text
    url ss = "https://" <> domain ss

    tls :: StanzaSetup -> TLSStanza
    tls ss = TLSStanza Nothing (CertificateFileSource (CertificateFiles (pemPath ss) (keyPath ss)))

    site :: StanzaSetup -> SourceStanza
    site ss = case ss.stanza_setup_site of
      Nothing -> NoFiles
      Just site -> 
          KitchenSinkDirectorySource
          $ KitchenSinkDirectorySourceStanza
              (siteSrcPath ss site)
              (siteInfo ss site)
              (Just $ siteDhallRoot ss site)
              (Just $ siteExecRoot ss)

    linkedSites :: SiteSetup -> [LinkedSite]
    linkedSites ss = ss.site_setup_ks_linked_sites

    siteInfo :: StanzaSetup -> SiteSetup -> SiteInfo
    siteInfo ss site =
      SiteInfo site.site_setup_title (url ss) Nothing (Just $ linkedSites site)

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
