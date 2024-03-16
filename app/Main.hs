{-# LANGUAGE OverloadedStrings #-}
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
import Options.Generic
import Options.Applicative

import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Acme.NotAJoke.LetsEncrypt (staging_letsencryptv2)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, parseUrlThrow)
import Acme.NotAJoke.Api.Certificate (storeCert)
import Acme.NotAJoke.Dancer (DanceStep(..), showProof, showToken, showKeyAuth)
import System.FilePath ((</>), takeFileName)

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
import qualified Salmon.Builtin.Nodes.Bash as Bash
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Web as Web
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.CommandLine as CLI

filesystemExample hello =
    op "fs-example" (deps [run mkFileContents configObj]) id
  where
    mkFileContents :: Track' (FS.FileContents Text, FS.Directory)
    mkFileContents = Track FS.filecontents >*< Track FS.dir

    configObj = (contents,directory)
    contents = FS.FileContents "./example-dir/file1" (hello <> "\n")
    directory = FS.Directory "./example-dir"

bashHelloExample =
    op "bash-hello" (deps [go]) id
  where
    go = Bash.run Debian.bash script
    script = FS.Generated mkBashScript "./example-script/hello.sh"
    mkBashScript :: Track' FilePath
    mkBashScript = Track $ \path -> FS.filecontents $ FS.FileContents path body
    body :: Text
    body = "echo hello >> hello-world.txt"

rsyncCopyExample =
    op "rsync-exampe" (deps [uploadExample]) id
  where
    uploadExample = Rsync.sendFile Debian.rsync payload remote remotedir
    payload = FS.Generated mkHelloWorld "./example-rsync/rsync-salmon-demo.txt"
    mkHelloWorld :: Track' FilePath
    mkHelloWorld = Track $ \path -> FS.filecontents $ FS.FileContents path body
    body :: Text
    body = "hello from a demo RSync copy started from a Salmon operation"
    remote = cheddarRsync
    remotedir = "tmp/"

cheddarSelf = Self.Remote "salmon" "cheddar.local"
cheddarRsync = Rsync.Remote "salmon" "cheddar.local"
cheddarSSH = Ssh.Remote "salmon" "cheddar.local"

mkRemote :: Track' Ssh.Remote
mkRemote = Track $ \r -> placeholder "remote" ("a remote at" <> Ssh.remoteHost r)

tlsCertsExample =
    op "tls-certs" certsinfo id
  where
    domain = Certs.Domain "localhost.example.com"
    key = Certs.Key Certs.RSA4096 "./tls/keys" "signing-key.rsa2048.key"
    csr = Certs.SigningRequest domain key "./tls/certs/localhost.example.com" "cert.csr"
    ssReq = Certs.SelfSigned "./tls/certs/localhost.example.com/cert.pem" csr
    openssl = Debian.openssl
    certsinfo = deps
      [ Certs.tlsKey openssl key
      , Certs.signingRequest openssl csr
      , Certs.selfSign openssl ssReq
      ]

jwkKeysExample =
    op "jwk-keys" keys id
  where
    keys = deps
      [ Keys.jwkKey (Keys.JWKKeyPair Keys.RSA2048 "./jwk-keys" "staging-key")
      , Keys.jwkKey (Keys.JWKKeyPair Keys.RSA4096 "./jwk-keys" "prod-key")
      ]

sshKeysExample =
    op "ssh-keys" keys id
  where
    ssh = Debian.sshClient
    keys = deps
      [ Keys.signKey ssh caKey (Keys.KeyIdentifier "key0") key0
      , Keys.signKey ssh caKey (Keys.KeyIdentifier "key1") key1
      ]
    caKey = Keys.SSHCertificateAuthority $ Keys.SSHKeyPair Keys.ED25519 "./ssh-keys/ca" "certificate-auth"
    key0 = Keys.SSHKeyPair Keys.ED25519 "./ssh-keys/keys" "node1"
    key1 = Keys.SSHKeyPair Keys.ED25519 "./ssh-keys/keys" "node2"

gitRepoExample =
    Git.repo git (Git.Repo "./git-repos/" "ks" remote branch)
  where
    remote = Git.Remote "git@github.com:kitchensink-tech/kitchensink.git"
    branch = Git.Branch "main"
    git = Debian.git

packagesExample =
    op "packages" (deps [pkg1, pkg2, pkg3]) id
  where
   pkg1 = Debian.deb (Debian.Package "vlc")
   pkg2 = Debian.deb (Debian.Package "gimp")
   pkg3 = Debian.deb (Debian.Package "vim")

acmeExample =
    op "acme" (deps [ Acme.acmeChallenge_dns01 chall challenger ]) id
  where
    chall :: Track' Acme.Challenger
    chall = adapt >$< f1 >*< f2

    adapt c = (Acme.challengerRequest c, Acme.challengerAccount c)
    f1 :: Track' Certs.SigningRequest
    f1 = Track $ Certs.signingRequest Debian.openssl
    f2 :: Track' Acme.Account
    f2 = Track $ Acme.acmeAccount

    challenger = Acme.Challenger acc csr pempath runAcmeDance
    csr = Certs.SigningRequest domain domainKey csrpath "cert.csr"
    acc = Acme.Account staging_letsencryptv2 accountKey mail
    mail = Acme.Email "salmon+001@dicioccio.fr"
    accountKey = Keys.JWKKeyPair Keys.RSA2048 "./jwk-keys" "staging-key"
    domain :: Certs.Domain
    domain = Certs.Domain "salmon.dyn.dicioccio.fr"
    pemdir = "./acme/certs"
    pemname = mconcat [ "acme", "staging", Certs.getDomain domain, ".pem"]
    csrpath =  pemdir <> "/" <> Text.unpack (Certs.getDomain domain) <> ".csr"
    domainKey = Certs.Key Certs.RSA4096 "./acme/keys" (Certs.getDomain domain <> ".rsa2048.key")
    pempath = pemdir </> Text.unpack pemname

    runAcmeDance :: Continuation.Continue a (FilePath -> DanceStep -> IO ())
    runAcmeDance = Continuation.Continue (Track $ const dnsTodo) handle
      where
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
              -- void getLine

              threadDelay 100000000
              pure ()
            _ -> pure ()

    dnsTodo = placeholder "DNS" "a dns server on which we can add known records"

remoteDnsSetup trackSelfDirective selfpath =
  using (cabalBinUpload microDNS cheddarRsync) $ \remotepath ->
    let
      setup = MicroDNSSetup remotepath remotePem remoteKey
    in
    op "remote-dns-setup" (depSequence setup) $ \actions -> actions {
      up = LByteString.putStr $ encode $ RunningLocalDNS setup
    }
  where
    depSequence setup = deps [opGraph (continueRemotely setup) `inject` uploadCert `inject` uploadKey]

    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup

    recurse setup selfref =
      Self.callSelfAsSudo mkRemote selfref trackSelfDirective CLI.Up (RunningLocalDNS setup)

    -- upload self
    self = Self.uploadSelf "tmp" cheddarSelf selfpath

    -- upload certificate and key
    remotePem  = "tmp/microdns.pem"
    remoteKey  = "tmp/microdns.key"

    upload gen localpath distpath =
      Rsync.sendFile Debian.rsync (FS.Generated gen localpath) cheddarRsync distpath

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

    domain = Certs.Domain "cheddar.local"
    key = Certs.Key Certs.RSA4096 "./certs/cheddar.local/microdns/keys" "signing-key.rsa4096.key"
    csr = Certs.SigningRequest domain key csrPath "cert.csr"
    pemPath = "./certs/cheddar.local/microdns/self-signed/cert.pem"
    csrPath = "./certs/cheddar.local/microdns/self-signed/csr"
    keyPath = Certs.keyPath key

dnsZoneFile :: FilePath -> Op
dnsZoneFile path =
    FS.filecontents (FS.FileContents path contents)
  where
    contents =
      Text.unlines
        [ "CAA example.dyn.dicioccio.fr. \"issue\" \"letsencrypt\""
        , "A dyn.dicioccio.fr. 163.172.53.34"
        , "A localhost.dyn.dicioccio.fr. 127.0.0.1"
        , "TXT dyn.dicioccio.fr. \"microdns\""
        , "TXT dyn.dicioccio.fr. \"salmon\""
        , "TXT dyn.dicioccio.fr. \"fish\""
        ]

dnsSecretFile :: FilePath -> Op
dnsSecretFile path =
    FS.filecontents (FS.FileContents path contents)
  where
    contents :: Text
    contents = "unsafe-1234!@#@!3eqel2129ce,Eeqe@x"

httpPostExample manager =
    op "http" (deps $ catMaybes [ get1 ]) id
  where
    get1 :: Maybe Op
    get1 = Web.call ignoreTrack <$> call1

    call1 :: Maybe Web.Call
    call1 = Web.Call manager <$> parseUrlThrow "http://dicioccio.fr/index.html"

distantCallExample selfpath trackSelfDirective = 
    self `bindTracked` recurse
  where
    self = Self.uploadSelf "tmp" cheddarSelf selfpath
    recurse s = Self.callSelf s trackSelfDirective CLI.Up directive
    directive = DistantCall "hello world"

systemdMicroDNSExample :: MicroDNSSetup -> Op
systemdMicroDNSExample arg =
    Systemd.systemdService Debian.systemctl trackConfig config
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \cfg ->
      let
          execPath = Systemd.start_path $ Systemd.service_execStart $ Systemd.config_service $ cfg
          copybin = FS.fileCopy (localBinPath arg) execPath
          copypem = FS.fileCopy (localPemPath arg) pemPath
          copykey = FS.fileCopy (localKeyPath arg) keyPath
      in
      op "setup-systemd-for-microdns" (deps [copybin, copypem, copykey, localDnsSetup]) id

    localDnsSetup :: Op
    localDnsSetup =
      op "dns-setup" (deps [localDNSZoneFile, localDNSSecretFile]) id
      where
        localDNSZoneFile = dnsZoneFile zoneFile
        localDNSSecretFile = dnsSecretFile hmacSecretFile

    config :: Systemd.Config
    config = Systemd.Config tgt unit service install

    tgt :: Systemd.UnitTarget
    tgt = "salmon-demo.service"

    hmacSecretFile,zoneFile,keyPath,pemPath :: FilePath
    hmacSecretFile = "/opt/rundir/microdns/microdns.secret"
    zoneFile = "/opt/rundir/microdns/microdns.zone"
    pemPath = "/opt/rundir/microdns/cert.pem"
    keyPath = "/opt/rundir/microdns/cert.key"

    unit :: Systemd.Unit
    unit = Systemd.Unit "a demo Salmon" "network-online.target"

    service :: Systemd.Service
    service = Systemd.Service Systemd.Simple "root" "root" "007" start Systemd.OnFailure Systemd.Process "/opt/rundir/microdns"

    domain :: Certs.Domain
    domain = Certs.Domain "cheddar.local"

    dnsApex :: Text
    dnsApex = "dyn.dicioccio.fr."

    start :: Systemd.Start
    start =
      Systemd.Start "/opt/rundir/microdns/bin/microdns"
        [ "tls"
        , "--webPort", "3355"
        , "--dnsPort", "53"
        , "--dnsApex", dnsApex
        , "--webHmacSecretFile", Text.pack hmacSecretFile
        , "--zoneFile", Text.pack zoneFile
        , "--certFile", Text.pack pemPath
        , "--keyFile", Text.pack keyPath
        ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"

demoOps trackSelfDirective selfpath httpManager n =
  [ Demo.collatz [x | x <- [1 .. n], odd x]
  , sshKeysExample
  , jwkKeysExample
  , tlsCertsExample
  , gitRepoExample
  , packagesExample
  , filesystemExample "hello demo"
  , bashHelloExample
  , rsyncCopyExample
  -- , acmeExample
  , httpPostExample httpManager
  , opGraph (distantCallExample selfpath trackSelfDirective)
  ]

demoDistantCallOps w =
  [ filesystemExample w
  ]

-------------------------------------------------------------------------------
buildOps _ =
  fmap opGraph
  [ microDNS
  , kitchensink
  ]

cabalBinUpload :: Tracked' FilePath -> Rsync.Remote -> Tracked' FilePath
cabalBinUpload mkbin remote =
    mkbin `bindTracked` go
  where
    go localpath =
       Tracked (Track $ const $ upload localpath) (remotePath localpath)
    upload local = Rsync.sendFile Debian.rsync (FS.PreExisting local) remote distpath
    distpath = "tmp/"
    remotePath local = distpath  </> takeFileName local

kitchensink = cabalRepoBuild
  "ks"
  "exe:kitchen-sink"
  "kitchen-sink"
  "https://github.com/kitchensink-tech/kitchensink.git"
  "main"
  "hs"

microDNS = cabalRepoBuild
  "microdns"
  "microdns"
  "microdns"
  "https://github.com/lucasdicioccio/microdns.git"
  "main"
  ""

-- builds a cabal repository
cabalRepoBuild dirname target binname remote branch subdir = 
    Tracked (Track $ const op) binpath
  where
    op = FS.withFile (Git.repofile mkrepo repo subdir) $ \repopath ->
           Cabal.install cabal [] (Cabal.Cabal repopath target) bindir
    bindir = "/opt/builds/bin"
    binpath = bindir </> Text.unpack binname
    repo = Git.Repo "./git-repos/" dirname (Git.Remote remote) (Git.Branch branch)
    git = Debian.git
    cabal = (Track $ \_ -> noop "preinstalled")
    mkrepo = Track $ Git.repo git

-------------------------------------------------------------------------------
data MicroDNSSetup
  = MicroDNSSetup
  { localBinPath :: FilePath
  , localPemPath :: FilePath
  , localKeyPath :: FilePath
  }
  deriving (Generic)
instance FromJSON MicroDNSSetup
instance ToJSON MicroDNSSetup

data Spec
  = DemoSpec { specCollatz :: Int }
  | BuildSpec { specBuildName :: Text }
  | DistantCall { distantCallWord :: Text }
  | RunningLocalDNS MicroDNSSetup
  | CheddarMicroDNS
  deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

program :: Self.SelfPath -> Manager -> Track' Spec
program selfpath httpManager =
   go
  where
   go = Track $ \spec -> optimizedDeps $ op "program" (deps $ specOp spec) id

   specOp :: Spec -> [Op]
   specOp (DemoSpec k) =  demoOps go selfpath httpManager k
   specOp (BuildSpec k) = buildOps k
   specOp (DistantCall k) =  demoDistantCallOps k
   specOp (CheddarMicroDNS) =  [remoteDnsSetup go selfpath]
   specOp (RunningLocalDNS arg) =  [systemdMicroDNSExample arg]
  
   optimizedDeps :: Op -> Op
   optimizedDeps base =
     let pkgs = Debian.installAllDebsAtOnce base
     in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
  = DemoSeed { seedCollatzNum :: Int }
  | BuildSeed { seedBuildName :: Text }
  | CheddarSeed

instance ParseRecord Seed where
  parseRecord =
      combo <**> helper
    where
      combo =
        subparser $ mconcat
          [ command "demo" (info demo (progDesc "runs demo"))
          , command "build" (info builds (progDesc "runs builds"))
          , command "cheddar" (info cheddar (progDesc "runs cheddar"))
          ]
      demo = DemoSeed <$> option auto (long "n-collatz")
      builds = BuildSeed <$> strArgument (Options.Applicative.help "build-name")
      cheddar = pure CheddarSeed

configure :: Configure IO Seed Spec
configure = Configure $ pure . go 
  where
    go :: Seed -> Spec
    go (DemoSeed k) = DemoSpec k
    go (BuildSeed k) = BuildSpec k
    go CheddarSeed = CheddarMicroDNS

-------------------------------------------------------------------------------
main :: IO ()
main = do
  let desc = fullDesc <> progDesc "A Salmon program." <> header "demonstration examples"
  let opts = info parseRecord desc
  cmd <- execParser opts
  manager <- newManager defaultManagerSettings
  selfpath <- Self.readSelfPath_linux
  CLI.execCommandOrSeed configure (program selfpath manager) cmd
