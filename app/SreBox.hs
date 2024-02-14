{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

-- to-re-export
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Configure (Configure(..))
import Salmon.Op.Track (Track(..), (>*<), Tracked(..), using, opGraph, bindTracked)
import Data.Functor.Contravariant ((>$<))
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString as ByteString
import Options.Generic
import Options.Applicative

import Control.Monad (void,forM_)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Acme.NotAJoke.LetsEncrypt (letsencryptv2)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, parseUrlThrow, method, requestHeaders )
import Acme.NotAJoke.Api.Certificate (storeCert)
import Acme.NotAJoke.Dancer (DanceStep(..), showProof, showToken, showKeyAuth)
import System.FilePath ((</>), takeFileName)
import Network.HTTP.Client (Manager, Request, httpNoBody)
import Network.HTTP.Client.TLS as Tls
import Network.TLS as Tls
import Network.TLS.Extra as Tls
import Network.TLS (Shared(..),ClientParams(..))
import Data.X509.CertificateStore as Crypton
import Data.X509 as Crypton
import Data.X509.Validation as Crypton
import Network.Connection as Crypton
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as HMAC256
import qualified Data.Text.Encoding as Text

import qualified Salmon.Builtin.Nodes.Continuation as Continuation
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Acme as Acme
import qualified Salmon.Builtin.Nodes.Demo as Demo
import qualified Salmon.Builtin.Nodes.Cabal as Cabal
import qualified Salmon.Builtin.Nodes.Keys as Keys
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified Salmon.Builtin.Nodes.Bash as Bash
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Web as Web
import Salmon.Op.Ref (dotRef)
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.CommandLine as CLI

boxSelf = Self.Remote "salmon" "box.dicioccio.fr"
boxRsync = Rsync.Remote "salmon" "box.dicioccio.fr"

setupKS :: Track' (Certs.Domain, Text) -> Self.Remote -> Self.SelfPath -> Op
setupKS mkCert remote selfpath =
  using (Git.repodir cloneSite blogRepo "") $ \blogSrcDir ->
  using (cabalBinUpload kitchenSink boxRsync) $ \remotepath ->
    let
      setup = KitchenSinkBlogSetup remotepath remotePem remoteKey (remoteBlogDir </> "blog") "site-src"
    in
    op "remote-ks-setup" (depSequence blogSrcDir setup) $ \actions -> actions {
      up = LByteString.putStr $ encode $ RunningLocalKitchenSinkBlog setup
    }
  where
    cloneSite = Track $ Git.repo Debian.git
    blogRepo = Git.Repo "./git-repos/" "blog" (Git.Remote "git@github.com:lucasdicioccio/blog.git") (Git.Branch "main")
    depSequence blogSrcDir setup = deps [opGraph (continueRemotely setup) `inject` uploads blogSrcDir]
    uploads blogSrcDir = op "uploads-ks" (deps [uploadCert, uploadKey, uploadSources blogSrcDir]) id

    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup

    recurse setup selfref =
      Self.callSelfAsSudo selfref CLI.Up (RunningLocalKitchenSinkBlog setup)

    -- upload self
    self = Self.uploadSelf "tmp" remote selfpath

    -- upload sources
    uploadSources blogSrcDir =
      Rsync.sendDir Debian.rsync ignoreTrack blogSrcDir boxRsync remoteBlogDir

    remoteBlogDir  = "tmp/ks-blog-src"

    -- upload certificate and key
    upload gen localpath distpath =
      Rsync.sendFile Debian.rsync (FS.Generated gen localpath) boxRsync distpath

    uploadCert = upload siteCert "./acme/certs/acme-production-dicioccio.fr.pem" remotePem
    uploadKey = upload siteCert "./acme/keys/dicioccio.fr.rsa2048.key" remoteKey

    siteCert = Track $ const $ run mkCert (Certs.Domain "dicioccio.fr", "apex-challenge")

    remotePem  = "tmp/ks.pem"
    remoteKey  = "tmp/ks.key"


-------------------------------------------------------------------------------

setupDNS :: Self.Remote -> Self.SelfPath -> Text -> Op
setupDNS remote selfpath domainName =
  using (cabalBinUpload microDNS boxRsync) $ \remotepath ->
    let
      setup = MicroDNSSetup remotepath remotePem remoteKey remoteSecret
    in
    op "remote-dns-setup" (depSequence setup) $ \actions -> actions {
      up = LByteString.putStr $ encode $ RunningLocalDNS setup
    }
  where
    depSequence setup = deps [opGraph (continueRemotely setup) `inject` uploads]
    uploads = op "uploads-dns" (deps [uploadCert, uploadKey, uploadSecret]) id

    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup

    recurse setup selfref =
      Self.callSelfAsSudo selfref CLI.Up (RunningLocalDNS setup)

    -- upload self
    self = Self.uploadSelf "tmp" remote selfpath

    -- upload certificate and key
    remotePem  = "tmp/microdns.pem"
    remoteKey  = "tmp/microdns.key"
    remoteSecret  = "tmp/microdns.shared-secret"

    upload gen localpath distpath =
      Rsync.sendFile Debian.rsync (FS.Generated gen localpath) boxRsync distpath

    uploadCert =
      upload selfSignedCert pemPath remotePem
      where
        selfSignedCert =
          Track $ \p -> Certs.selfSign Debian.openssl (Certs.SelfSigned p csr)

    uploadKey =
      upload selfSigningKey keyPath remoteKey
      where
        selfSigningKey =
          Track $ const $ Certs.tlsKey Debian.openssl key

    uploadSecret =
      upload sharedSecret secretPath remoteSecret
      where
        sharedSecret =
          Track $ dnsSecretFile


    domain = Certs.Domain domainName
    tlsDir x = "./certs" </> Text.unpack domainName </> x
    secretsDir x = "./secrets" </> Text.unpack domainName </> x
    key = Certs.Key Certs.RSA4096 (tlsDir "microdns/keys") "signing-key.rsa4096.key"
    csr = Certs.SigningRequest domain key csrPath "cert.csr"
    pemPath = tlsDir "microdns/self-signed/cert.pem"
    csrPath = tlsDir "microdns/self-signed/csr"
    keyPath = Certs.keyPath key
    secretPath = secretsDir "microdns/shared-secret/secret.b64"

dnsZoneFile :: FilePath -> Op
dnsZoneFile path =
    FS.filecontents (FS.FileContents path contents)
  where
    contents =
      Text.unlines
        [ "caa example.dyn.dicioccio.fr. \"issue\" \"letsencrypt\""
        , "A dyn.dicioccio.fr. 163.172.53.34"
        , "A localhost.dyn.dicioccio.fr. 127.0.0.1"
        , "TXT dyn.dicioccio.fr. \"microdns\""
        , "TXT dyn.dicioccio.fr. \"salmon\""
        ]

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
          copybin = FS.fileCopy (microdns_localBinPath arg) execPath
          copypem = FS.fileCopy (microdns_localPemPath arg) pemPath
          copykey = FS.fileCopy (microdns_localKeyPath arg) keyPath
          copySecret = FS.fileCopy (microdns_localSecretPath arg) hmacSecretFile
      in
      op "setup-systemd-for-microdns" (deps [copybin, copypem, copykey, copySecret, localDnsSetup]) id

    localDnsSetup :: Op
    localDnsSetup =
      op "dns-setup" (deps [localDNSZoneFile]) id
      where
        localDNSZoneFile = dnsZoneFile zoneFile

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

    dnsApex :: Text
    dnsApex = "dyn.dicioccio.fr."

    start :: Systemd.Start
    start =
      Systemd.Start "/opt/rundir/microdns/bin/microdns"
        [ "tls"
        , "--webPort", "65432"
        , "--dnsPort", "53"
        , "--dnsApex", dnsApex
        , "--webHmacSecretFile", Text.pack hmacSecretFile
        , "--zoneFile", Text.pack zoneFile
        , "--certFile", Text.pack pemPath
        , "--keyFile", Text.pack keyPath
        ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"

-------------------------------------------------------------------------------
systemdKitchenSinkBlog :: KitchenSinkBlogSetup -> Op
systemdKitchenSinkBlog arg =
    Systemd.systemdService Debian.systemctl trackConfig config
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \cfg ->
      let
          execPath = Systemd.start_path $ Systemd.service_execStart $ Systemd.config_service $ cfg
          copybin = FS.fileCopy arg.ks_blog_localBinPath execPath
          copypem = FS.fileCopy arg.ks_blog_localPemPath pemPath
          copykey = FS.fileCopy arg.ks_blog_localKeyPath keyPath
          movesrc = FS.moveDirectory arg.ks_blog_localSrcDir blogSrcDir
      in
      op "setup-systemd-for-ks" (deps [movesrc, copybin, copypem, copykey, localSetup]) id

    localSetup :: Op
    localSetup =
      op "ks-setup" (deps [sysDeps]) id

    sysDeps :: Op
    sysDeps =
      op "system" (deps [Debian.deb (Debian.Package "graphviz")]) id

    config :: Systemd.Config
    config = Systemd.Config tgt unit service install

    tgt :: Systemd.UnitTarget
    tgt = "salmon-ks.service"

    blogSrcDir,keyPath,pemPath :: FilePath
    pemPath = "/opt/rundir/ks/cert.pem"
    keyPath = "/opt/rundir/ks/cert.key"
    blogSrcDir = "/opt/rundir/ks/site"
    blogSrcPath = blogSrcDir </> arg.ks_blog_subdir

    unit :: Systemd.Unit
    unit = Systemd.Unit "Kitchen-Sink from Salmon" "network-online.target"

    service :: Systemd.Service
    service = Systemd.Service Systemd.Simple "root" "root" "007" start Systemd.OnFailure Systemd.Process blogSrcDir

    start :: Systemd.Start
    start =
      Systemd.Start "/opt/rundir/ks/bin/kitchen-sink"
        [ "serve"
        , "--servMode", "SERVE"
        , "--httpPort", "80"
        , "--httpsPort", "443"
        , "--tlsCertFile", Text.pack pemPath
        , "--tlsKeyFile", Text.pack keyPath
        , "--outDir", "./out"
        , "--srcDir", Text.pack blogSrcPath
        ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"

-------------------------------------------------------------------------------


cabalBinUpload :: Tracked' FilePath -> Rsync.Remote -> Tracked' FilePath
cabalBinUpload mkbin remote =
    mkbin `bindTracked` go
  where
    go localpath =
       Tracked (Track $ const $ upload localpath) (remotePath localpath)
    upload local = Rsync.sendFile Debian.rsync (FS.PreExisting local) remote distpath
    distpath = "tmp/"
    remotePath local = distpath  </> takeFileName local

microDNS = cabalRepoBuild
  "microdns"
  "microdns"
  "microdns"
  "https://github.com/lucasdicioccio/microdns.git"
  "main"
  ""

kitchenSink = cabalRepoBuild
  "ks"
  "exe:kitchen-sink"
  "kitchen-sink"
  "https://github.com/kitchensink-tech/kitchensink.git"
  "main"
  "hs"

-- builds a cabal repository
cabalRepoBuild dirname target binname remote branch subdir = 
    Tracked (Track $ const op) binpath
  where
    op = FS.withFile (Git.repofile mkrepo repo subdir) $ \repopath ->
           Cabal.install cabal (Cabal.Cabal repopath target) bindir
    bindir = "/opt/builds/bin"
    binpath = bindir </> Text.unpack binname
    repo = Git.Repo "./git-repos/" dirname (Git.Remote remote) (Git.Branch branch)
    git = Debian.git
    cabal = (Track $ \_ -> noop "preinstalled")
    mkrepo = Track $ Git.repo git

domains :: [(Certs.Domain,Text)]
domains =
  [ (Certs.Domain "dicioccio.fr", "apex-challenge")
  , (Certs.Domain "phasein.dyn.dicioccio.fr", "_acme-challenge.phasein")
  , (Certs.Domain "e-webhook.dyn.dicioccio.fr", "_acme-challenge.e-webhook")
  , (Certs.Domain "localhost.dyn.dicioccio.fr", "_acme-challenge.localhost")
  ]

data DNSRunning = DNSRunning

acmeSign :: Track' DNSRunning -> (Certs.Domain, Text) -> Op
acmeSign mkDNS (domain, txtrecord) =
    op "acme-sign" (deps [ Acme.acmeChallenge_dns01 chall challenger ]) $ \actions -> actions {
      ref = dotRef $ "acme-sign:" <> Certs.getDomain domain
    }
  where
    chall :: Track' Acme.Challenger
    chall = adapt >$< f1 >*< f2 >*< mkDNS

    adapt c = (Acme.challengerRequest c, (Acme.challengerAccount c, DNSRunning))
    f1 :: Track' Certs.SigningRequest
    f1 = Track $ Certs.signingRequest Debian.openssl
    f2 :: Track' Acme.Account
    f2 = Track $ Acme.acmeAccount

    challenger = Acme.Challenger acc csr pemdir pemname runAcmeDance
    csr = Certs.SigningRequest domain domainKey csrpath "cert.csr"
    acc = Acme.Account letsencryptv2 accountKey mail
    mail = Acme.Email "certmaster+salmon@dicioccio.fr"
    accountKey = Keys.JWKKeyPair Keys.RSA2048 "./jwk-keys" "production-key"
    pemdir = "./acme/certs"
    pemname = mconcat [ "acme", "-production-", Certs.getDomain domain, ".pem"]
    csrpath =  pemdir <> "/" <> Text.unpack (Certs.getDomain domain) <> ".csr"
    domainKey = Certs.Key Certs.RSA4096 "./acme/keys" (Certs.getDomain domain <> ".rsa2048.key")

    runAcmeDance :: Continuation.Continue a (FilePath -> DanceStep -> IO ())
    runAcmeDance = Continuation.Continue ignoreTrack handle
      where
        --TODO: extract
        selfSignDir = "./certs/box.dicioccio.fr/microdns/self-signed/cert.pem"
        secretPath = "./secrets/box.dicioccio.fr/microdns/shared-secret/secret.b64"

        handle :: FilePath -> DanceStep -> IO ()
        handle pemPath step =
          case step of
            Done _ cert -> do
              print ("storing cert at", pemPath)
              storeCert pemPath cert
            Validation (tok,keyAuth,sha) -> do
              print ("token (http01) is" :: Text, showToken tok)
              print ("key authorization (http01) is" :: Text, showKeyAuth keyAuth)
              print ("sha256 (dns01) is" :: Text, showProof sha)
              print ("press enter to continue" :: Text)

              sharedsecret <- ByteString.readFile $ secretPath
              tlsManager <- makeTlsManagerForSelfSigned "box.dicioccio.fr" selfSignDir
              baseReq <- parseUrlThrow $ "https://box.dicioccio.fr:65432/register/txt" </> Text.unpack txtrecord </> Text.unpack (showProof sha)
              let hmac = Base16.encode $ HMAC256.hmac sharedsecret (Text.encodeUtf8 txtrecord)
              let req = baseReq { method = "POST", requestHeaders = [("x-microdns-hmac", hmac)] }
              forM_ tlsManager (httpNoBody $ req)
              threadDelay 1000000
              pure ()
            _ -> pure ()

makeTlsManagerForSelfSigned :: Tls.HostName -> FilePath -> IO (Maybe Manager)
makeTlsManagerForSelfSigned hostname dir = do
  certStore <- Crypton.readCertificateStore dir
  case certStore of
    Nothing -> pure Nothing
    Just store -> do
      let base = Tls.defaultParamsClient hostname ""
      let tlsSetts = setStore store base
      Just <$> Tls.newTlsManagerWith (Tls.mkManagerSettings (Crypton.TLSSettings tlsSetts) Nothing)
  where
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

sreBox :: Self.SelfPath -> Text -> Op
sreBox selfpath selfCertDomain =
    op "sre-box" (deps (ks : domainCerts)) id
  where 
    dns = Track $ const $ setupDNS boxSelf selfpath selfCertDomain
    domainCerts = fmap (\d -> acmeSign dns d) domains
    cert = Track $ acmeSign dns
    ks = setupKS cert boxSelf selfpath

-------------------------------------------------------------------------------

data MicroDNSSetup
  = MicroDNSSetup
  { microdns_localBinPath :: FilePath
  , microdns_localPemPath :: FilePath
  , microdns_localKeyPath :: FilePath
  , microdns_localSecretPath :: FilePath
  }
  deriving (Generic)
instance FromJSON MicroDNSSetup
instance ToJSON MicroDNSSetup

data KitchenSinkBlogSetup
  = KitchenSinkBlogSetup
  { ks_blog_localBinPath :: FilePath
  , ks_blog_localPemPath :: FilePath
  , ks_blog_localKeyPath :: FilePath
  , ks_blog_localSrcDir :: FilePath
  , ks_blog_subdir :: FilePath
  }
  deriving (Generic)
instance FromJSON KitchenSinkBlogSetup
instance ToJSON KitchenSinkBlogSetup

data Spec
  = SreBox Text
  | RunningLocalDNS MicroDNSSetup
  | RunningLocalKitchenSinkBlog KitchenSinkBlogSetup
  deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

program :: Self.SelfPath -> Manager -> Track' Spec
program selfpath httpManager =
    Track $ \spec -> optimizedDeps $ op "program" (deps $ specOp spec) id
  where
   specOp :: Spec -> [Op]
   specOp (SreBox domainName) =  [sreBox selfpath domainName]
   specOp (RunningLocalDNS arg) =  [systemdMicroDNS arg]
   specOp (RunningLocalKitchenSinkBlog arg) =  [systemdKitchenSinkBlog arg]
  
   optimizedDeps :: Op -> Op
   optimizedDeps base =
     let pkgs = Debian.installAllDebsAtOnce base
     in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
  = SreBoxSeed { boxDomain :: Text }

instance ParseRecord Seed where
  parseRecord =
      combo <**> helper
    where
      combo =
        subparser $ mconcat
          [ command "sre-box" (info sreBox (progDesc "configures SRE box"))
          ]
      sreBox = SreBoxSeed <$> strArgument (Options.Applicative.help "domain")

configure :: Configure' Seed Spec
configure = Configure $ pure . go 
  where
    go :: Seed -> Spec
    go (SreBoxSeed d) = SreBox d

-------------------------------------------------------------------------------
main :: IO ()
main = do
  let desc = fullDesc <> progDesc "Personal configurations." <> header "for box.dicioccio.fr"
  let opts = info parseRecord desc
  cmd <- execParser opts
  manager <- newManager defaultManagerSettings
  selfpath <- Self.readSelfPath_linux
  CLI.execCommandOrSeed configure (program selfpath manager) cmd
