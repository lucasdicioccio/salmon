{-# LANGUAGE DeriveGeneric #-}

module SreBox.MicroDNS where

import qualified Crypto.Hash.SHA256 as HMAC256
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import Data.CaseInsensitive (CI)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.X509 as Crypton
import Data.X509.CertificateStore as Crypton
import Data.X509.Validation as Crypton
import GHC.Generics (Generic)
import Network.Connection as Crypton
import Network.HTTP.Client (Manager, Request, httpNoBody)
import Network.HTTP.Client.TLS as Tls
import Network.TLS as Tls
import Network.TLS.Extra as Tls
import qualified Data.Text as Text
import System.Directory
import System.FilePath

import Salmon.Builtin.Extension
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

import SreBox.CabalBuilding
import SreBox.Environment

-------------------------------------------------------------------------------

type DNSName = Text
type PortNumber = Int

data MicroDNSConfig
  = MicroDNSConfig
  { microdns_cfg_domainName :: DNSName
  , microdns_cfg_apex :: DNSName
  , microdns_cfg_portnum :: PortNumber
  , microdns_cfg_postTxt :: DNSName -> Text -> IO ()
  , microdns_cfg_key :: Certs.Key
  , microdns_cfg_pemPath :: FilePath
  , microdns_cfg_secretPath :: FilePath
  , microdns_cfg_selfCsr :: Certs.SigningRequest
  , microdns_cfg_zonefileContents :: Text
  }

data MicroDNSSetup
  = MicroDNSSetup
  { microdns_setup_localBinPath :: FilePath
  , microdns_setup_apex :: DNSName
  , microdns_setup_portnum :: PortNumber
  , microdns_setup_localPemPath :: FilePath
  , microdns_setup_localKeyPath :: FilePath
  , microdns_setup_localSecretPath :: FilePath
  , microdns_setup_zoneFileContents :: Text
  }
  deriving (Generic)
instance FromJSON MicroDNSSetup
instance ToJSON MicroDNSSetup

setupDNS
  :: (FromJSON directive, ToJSON directive)
  => Track' Ssh.Remote
  -> Self.Remote
  -> Self.SelfPath
  -> (MicroDNSSetup -> directive)
  -> MicroDNSConfig
  -> Op
setupDNS mkRemote selfRemote selfpath toSpec cfg =
  using (cabalBinUpload microDNS rsyncRemote) $ \remotepath ->
    let
      setup = MicroDNSSetup remotepath cfg.microdns_cfg_apex cfg.microdns_cfg_portnum remotePem remoteKey remoteSecret cfg.microdns_cfg_zonefileContents
    in
    opGraph (continueRemotely setup) `inject` configUploads
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    configUploads = op "uploads-microdns-configs" (deps [uploadCert, uploadKey, uploadSecret]) id

    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup

    recurse setup selfref =
      Self.callSelfAsSudo mkRemote selfref CLI.Up (toSpec setup)

    -- upload self
    self = Self.uploadSelf "tmp" selfRemote selfpath

    -- upload certificate and key
    remotePem  = "tmp/microdns.pem"
    remoteKey  = "tmp/microdns.key"
    remoteSecret  = "tmp/microdns.shared-secret"

    upload gen localpath distpath =
      Rsync.sendFile Debian.rsync (FS.Generated gen localpath) rsyncRemote distpath

    uploadCert =
      upload (selfSignedCert cfg) cfg.microdns_cfg_pemPath remotePem

    uploadKey =
      upload (selfSigningKey cfg) (Certs.keyPath cfg.microdns_cfg_key) remoteKey

    uploadSecret =
      upload sharedSecret cfg.microdns_cfg_secretPath remoteSecret
      where
        sharedSecret =
          Track $ dnsSecretFile

dnsZoneFile :: FilePath -> Text -> Op
dnsZoneFile path contents =
    FS.filecontents (FS.FileContents path contents)

dnsSecretFile :: FilePath -> Op
dnsSecretFile path =
  Secrets.sharedSecretFile
    Debian.openssl
    (Secrets.Secret Secrets.Base64 16 path)

systemdMicroDNS :: MicroDNSSetup -> Op
systemdMicroDNS arg =
    Systemd.systemdService Debian.systemctl trackConfig config
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \cfg ->
      let
          execPath = Systemd.start_path $ Systemd.service_execStart $ Systemd.config_service $ cfg
          copybin = FS.fileCopy (microdns_setup_localBinPath arg) execPath
          copypem = FS.fileCopy (microdns_setup_localPemPath arg) pemPath
          copykey = FS.fileCopy (microdns_setup_localKeyPath arg) keyPath
          copySecret = FS.fileCopy (microdns_setup_localSecretPath arg) hmacSecretFile
      in
      op "setup-systemd-for-microdns" (deps [copybin, copypem, copykey, copySecret, localDnsSetup]) id

    localDnsSetup :: Op
    localDnsSetup =
      op "dns-setup" (deps [localDNSZoneFile]) id
      where
        localDNSZoneFile = dnsZoneFile zoneFile arg.microdns_setup_zoneFileContents

    config :: Systemd.Config
    config = Systemd.Config tgt unit service install

    tgt :: Systemd.UnitTarget
    tgt = "salmon-microdns.service"

    hmacSecretFile,zoneFile,keyPath,pemPath :: FilePath
    hmacSecretFile = "/opt/rundir/microdns/microdns.secret"
    zoneFile = "/opt/rundir/microdns/microdns.zone"
    pemPath = "/opt/rundir/microdns/cert.pem"
    keyPath = "/opt/rundir/microdns/cert.key"

    unit :: Systemd.Unit
    unit = Systemd.Unit "MicroDNS from Salmon" "network-online.target"

    service :: Systemd.Service
    service = Systemd.Service Systemd.Simple "root" "root" "007" start Systemd.OnFailure Systemd.Process "/opt/rundir/microdns"

    start :: Systemd.Start
    start =
      Systemd.Start "/opt/rundir/microdns/bin/microdns"
        [ "tls"
        , "--webPort", Text.pack (show arg.microdns_setup_portnum)
        , "--dnsPort", "53"
        , "--dnsApex", arg.microdns_setup_apex
        , "--webHmacSecretFile", Text.pack hmacSecretFile
        , "--zoneFile", Text.pack zoneFile
        , "--certFile", Text.pack pemPath
        , "--keyFile", Text.pack keyPath
        ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"

makeTlsManagerForSelfSigned :: DNSName -> FilePath -> IO (Maybe Manager)
makeTlsManagerForSelfSigned hostname dir = do
  certStore <- Crypton.readCertificateStore dir
  case certStore of
    Nothing -> pure Nothing
    Just store -> do
      let base = Tls.defaultParamsClient tlshostname ""
      let tlsSetts = setStore store base
      Just <$> Tls.newTlsManagerWith (Tls.mkManagerSettings (Crypton.TLSSettings tlsSetts) Nothing)
  where
    tlshostname :: Tls.HostName
    tlshostname = Text.unpack hostname

    setStore
      :: Crypton.CertificateStore
      -> Tls.ClientParams
      -> Tls.ClientParams
    setStore store base
      = base {
        clientShared =
          (base.clientShared) {
            sharedCAStore = store
          }
      , clientSupported =
          (base.clientSupported) {
            supportedCiphers = Tls.ciphersuite_default
          }
      , clientHooks =
          (base.clientHooks) {
            onServerCertificate = Crypton.validate Crypton.HashSHA256 Crypton.defaultHooks relaxedChecks
          }
      }
    relaxedChecks :: Crypton.ValidationChecks
    relaxedChecks = Crypton.defaultChecks { checkLeafV3 = False }

sharedToken :: FilePath -> Text -> FilePath -> Op
sharedToken secret_path hashedpart token_path =
  op "microdns-token" (deps [prepareSecret, enclosingdir]) $ \actions -> actions {
    help = "store token"
  , up = do
      sharedsecret <- ByteString.readFile secret_path
      ByteString.writeFile token_path $ hmacHashedPart sharedsecret hashedpart
  }
  where
    enclosingdir :: Op
    enclosingdir = FS.dir (FS.Directory $ takeDirectory token_path)
    prepareSecret :: Op
    prepareSecret = dnsSecretFile secret_path

hmacHashedPart :: ByteString -> Text -> ByteString
hmacHashedPart sharedsecret txtrecord =
  Base16.encode $ HMAC256.hmac sharedsecret (Text.encodeUtf8 txtrecord)

hmacHeader :: ByteString -> Text -> (CI ByteString, ByteString)
hmacHeader s t = ("x-microdns-hmac", hmacHashedPart s t)

selfSignedCert :: MicroDNSConfig -> Track' FilePath
selfSignedCert cfg =
  Track $ \p -> Certs.selfSign Debian.openssl (Certs.SelfSigned p cfg.microdns_cfg_selfCsr)

selfSigningKey :: MicroDNSConfig -> Track' FilePath
selfSigningKey cfg =
  Track $ const $ Certs.tlsKey Debian.openssl cfg.microdns_cfg_key
