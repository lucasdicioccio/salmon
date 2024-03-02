{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Main where

-- to-re-export
import GHC.TypeLits (Symbol)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.OpGraph (inject, node)
import Salmon.Op.Configure (Configure(..))
import Salmon.Op.Track (Track(..), (>*<), Tracked(..), using, opGraph, bindTracked)
import Data.Functor.Contravariant ((>$<))
import Data.Foldable (toList)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as Base16
import Options.Generic
import Options.Applicative

import Control.Monad (void,forM_)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Acme.NotAJoke.LetsEncrypt (staging_letsencryptv2, letsencryptv2)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, parseUrlThrow, method, requestHeaders )
import Acme.NotAJoke.Api.Certificate (storeCert)
import Acme.NotAJoke.Dancer (DanceStep(..), showProof, showToken, showKeyAuth)
import System.FilePath ((</>), takeFileName)
import Network.HTTP.Client (Manager, Request, httpNoBody)
import Network.HTTP.Client.TLS as Tls
import Network.TLS as Tls
import Network.TLS.Extra as Tls
import Network.TLS (Shared(..),ClientParams(..))
import qualified Data.Text.Encoding as Text
import qualified KitchenSink.Engine.Config as KS
import qualified KitchenSink.Engine.SiteConfig as KS

import Salmon.Op.G (G(..))
import qualified Salmon.Builtin.Nodes.Continuation as Continuation
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Acme as Acme
import qualified Salmon.Builtin.Nodes.Demo as Demo
import qualified Salmon.Builtin.Nodes.Cabal as Cabal
import qualified Salmon.Builtin.Nodes.Keys as Keys
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.CronTask as CronTask
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified Salmon.Builtin.Nodes.User as User
import qualified Salmon.Builtin.Nodes.Web as Web
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Web as Web
import Salmon.Builtin.Extension
import Salmon.Builtin.Helpers (collapse)
import Salmon.Builtin.Migrations as Migrations
import qualified Salmon.Builtin.CommandLine as CLI

import SreBox.Environment
import SreBox.CabalBuilding as CabalBuilding
import SreBox.CertSigning
import SreBox.MicroDNS
import qualified SreBox.DNSRegistration as DNSRegistration
import qualified SreBox.KitchenSinkBlog as KSBlog
import qualified SreBox.KitchenSinkMultiSites as KSMulti
import qualified SreBox.PostgresInit as PostgresInit
import qualified SreBox.PostgresMigrations as PGMigrate
import qualified SreBox.Postgrest as Postgrest
import qualified SreBox.Initialize as Initialize

data CertPrefs
  = CertPrefs
  { cert_key :: Certs.Key
  , cert_csr :: Certs.SigningRequest
  , cert_pem :: FilePath
  }

data AccountPrefs
  = AccountPrefs
  { account_email :: Acme.Email
  , account_key :: Keys.JWKKeyPair
  }

data TaskPrefs
  = TaskPrefs
  { task_script :: FilePath
  , task_data_path :: FilePath -> FilePath
  }

domains :: [(Certs.Domain,Text)]
domains =
  [ (Certs.Domain "dicioccio.fr", "apex-challenge")
  , (Certs.Domain "e-webhook.dyn.dicioccio.fr", "_acme-challenge.e-webhook")
  , (Certs.Domain "localhost.dyn.dicioccio.fr", "_acme-challenge.localhost")
  ]

acmeCertPrefs :: Environment -> Certs.Domain -> CertPrefs
acmeCertPrefs env domain =
  CertPrefs
    key
    csr
    pempath
  where
    certdir = "./acme/certs"
    keydir = "./acme/keys"

    csrpath = certdir <> "/" <> Text.unpack (Certs.getDomain domain) <> ".csr"
    csr = Certs.SigningRequest domain key csrpath "cert.csr"

    pempath = certdir </> Text.unpack pemname
    pemname = mconcat [ "acme", envInfix, Certs.getDomain domain, ".pem"]

    key = Certs.Key Certs.RSA4096 keydir (Certs.getDomain domain <> ".rsa2048.key")

    envInfix = case env of
           Production -> "-production-"
           Staging -> "-staging-"

microDNSCertPrefs :: Certs.Domain -> CertPrefs
microDNSCertPrefs dom =
  CertPrefs
    key
    csr
    pempath
  where
    tlsDir x = "./certs" </> Text.unpack (Certs.getDomain dom) </> x
    pempath = tlsDir "microdns/self-signed/cert.pem"
    csrPath = tlsDir "microdns/self-signed/csr"
    csr = Certs.SigningRequest dom key csrPath "cert.csr"
    key = Certs.Key Certs.RSA4096 (tlsDir "microdns/keys") "signing-key.rsa4096.key"


acmeAccountPrefs :: Environment -> AccountPrefs
acmeAccountPrefs env =
  AccountPrefs
    email
    key
  where
    email = Acme.Email "certmaster+salmon@dicioccio.fr"
    keydir = "./jwk-keys"
    key = Keys.JWKKeyPair Keys.RSA2048 "./jwk-keys" (envPrefix <> "key")

    envPrefix = case env of
           Production -> "production-"
           Staging -> "staging-"

type TaskName = String

taskPrefs :: Environment -> TaskName -> TaskPrefs
taskPrefs env name =
  TaskPrefs
    script
    dataitem
  where
    dir = "/opt/tasks" </> envInfix </> name
    script = dir </> "task.sh"
    dataitem sub = dir </> "data" </> sub
    envInfix = case env of
           Production -> "production"
           Staging -> "staging"

data SecretPrefs (a :: Symbol)
  = SecretPrefs
  { secret_path :: FilePath
  }

microDNSSecretPrefs :: Certs.Domain -> SecretPrefs "microdns-shared-secret"
microDNSSecretPrefs dom =
  SecretPrefs
    path
  where
    secretsDir :: FilePath -> FilePath
    secretsDir x = "./secrets" </> Text.unpack (Certs.getDomain dom) </> x
    path :: FilePath
    path = secretsDir "microdns/shared-secret/secret.b64"

microDNSTokensPrefs :: Certs.Domain -> DNSName -> SecretPrefs "microdns-token"
microDNSTokensPrefs dom sub =
  SecretPrefs
    path
  where
    secretsDir :: FilePath -> FilePath
    secretsDir x = "./tokens" </> Text.unpack (Certs.getDomain dom) </> x
    path :: FilePath
    path = secretsDir ("microdns" </> Text.unpack sub)

acmeConfig :: MicroDNSConfig -> Environment -> AcmeConfig
acmeConfig dns env =
    AcmeConfig account pemPath csr dns
  where
    le = case env of
           Production -> letsencryptv2
           Staging -> staging_letsencryptv2
    account = Acme.Account le accountKey email
    email = (acmeAccountPrefs env).account_email
    accountKey = (acmeAccountPrefs env).account_key
    prefs domain = acmeCertPrefs env domain
    pemPath domain = (prefs domain).cert_pem
    csr domain = (prefs domain).cert_csr

dnsConfig :: Text -> MicroDNSConfig
dnsConfig selfCertDomain =
    MicroDNSConfig
      selfCertDomain
      dnsApex
      portnum
      postValidation
      certPrefs.cert_key
      certPrefs.cert_pem
      secretPrefs.secret_path
      certPrefs.cert_csr
      zonefile
  where
    secretPrefs = microDNSSecretPrefs (Certs.Domain selfCertDomain)
    certPrefs = microDNSCertPrefs (Certs.Domain selfCertDomain)
    portnum = 65432
    postTxtRecordURL txtrecord val = mconcat ["https://", Text.unpack selfCertDomain, ":", show portnum, "/register/txt/", Text.unpack txtrecord, "/", Text.unpack val]
    postValidation txtrecord sha = do
      sharedsecret <- ByteString.readFile $ secretPrefs.secret_path
      tlsManager <- makeTlsManagerForSelfSigned selfCertDomain certPrefs.cert_pem
      baseReq <- parseUrlThrow $ postTxtRecordURL txtrecord sha
      let req = baseReq { method = "POST", requestHeaders = [hmacHeader sharedsecret txtrecord] }
      forM_ tlsManager (httpNoBody $ req)
    dnsApex = "dyn.dicioccio.fr."
    zonefile =
      Text.unlines
        [ "CAA example.dyn.dicioccio.fr. \"issue\" \"letsencrypt\""
        , "A dyn.dicioccio.fr. 163.172.53.34"
        , "A localhost.dyn.dicioccio.fr. 127.0.0.1"
        , "A kitchensink.dyn.dicioccio.fr. 163.172.53.34"
        , "TXT dyn.dicioccio.fr. \"microdns\""
        , "TXT dyn.dicioccio.fr. \"salmon\""
        ]

ksBlogConfig :: KSBlog.KitchenSinkBlogConfig
ksBlogConfig =
    KSBlog.KitchenSinkBlogConfig
      (Git.Repo "./git-repos/" "blog" (Git.Remote "git@github.com:lucasdicioccio/blog.git") (Git.Branch "main"))
      "site-src"
      (domain, acmeChallengeDnsleaf)
      (prefs.cert_pem)
      (Certs.keyPath prefs.cert_key)
  where
    domain = Certs.Domain "dicioccio.fr"
    acmeChallengeDnsleaf = "apex-challenge"
    prefs = acmeCertPrefs Production domain

kitchenSinkBlog :: KSMulti.StanzaConfig
kitchenSinkBlog =
  ksBlogStanza
    (Certs.Domain "kitchensink.dyn.dicioccio.fr")
    "_acme-challenge.kitchensink"
    "The Kitchen Sink Blog Generator"
    (Git.Repo "./git-repos/" "kitchensink" (Git.Remote "git@github.com:kitchensink-tech/kitchensink.git") (Git.Branch "main"))
    "website-src"
    ""
    (KS.SlashApiProxyList
      [ KS.SlashApiProxyDirective
          KS.UseHTTPS
          "/api/github-proxy"
          (KS.RewritePrefixHost "/repos/kitchensink-tech/kitchensink" "api.github.com")
          "api.github.com"
          443
      ]
    )
    [ KS.LinkedSite "https://dicioccio.fr" "kitchen-sink" "Lucas' blog"
    , KS.LinkedSite "https://en.wikipedia.org/" "website" "The English Wikipedia"
    ]

chouetteBlog :: KSMulti.StanzaConfig
chouetteBlog =
  ksBlogStanza
    (Certs.Domain "chouette.dyn.dicioccio.fr")
    "_acme-challenge.chouette"
    "Des idees sur la trace de la Chouette d'or."
    (Git.Repo "./git-repos/" "blog" (Git.Remote "git@github.com:lucasdicioccio/blog.git") (Git.Branch "main"))
    "chouette-src"
    ""
    KS.NoProxying
    [ KS.LinkedSite "https://dicioccio.fr" "kitchen-sink" "Lucas' blog"
    , KS.LinkedSite "https://en.wikipedia.org/" "website" "The English Wikipedia"
    , KS.LinkedSite "https://fr.wikipedia.org/" "website" "Wikipedia en Francais"
    ]

dicioccioDotFr :: KSMulti.StanzaConfig
dicioccioDotFr =
  ksBlogStanza
    (Certs.Domain "dicioccio.fr")
    "apex-challenge"
    ("Lucas DiCioccio's Blog")
    (Git.Repo "./git-repos/" "blog" (Git.Remote "git@github.com:lucasdicioccio/blog.git") (Git.Branch "main"))
    "site-src"
    ""
    KS.NoProxying
    [ KS.LinkedSite "https://kitchensink-tech.github.io" "kitchen-sink" "KitchenSink website"
    , KS.LinkedSite "https://en.wikipedia.org/" "website" "The English Wikipedia"
    , KS.LinkedSite "https://github.com/" "website" "GitHub"
    ]

ksBlogStanza
  :: Certs.Domain
  -> DNSName
  -> Text
  -> Git.Repo
  -> FilePath
  -> FilePath
  -> KS.ApiProxyConfig
  -> [KS.LinkedSite]
  -> KSMulti.StanzaConfig
ksBlogStanza domain txt title repo subdir dhalldir proxy linkessites =
  let prefs = acmeCertPrefs Production domain in
  KSMulti.StanzaConfig_Site
    $ KSMulti.GitSiteStanzaConfig
        (domain, txt)
        title
        (Certs.keyPath prefs.cert_key)
        prefs.cert_pem
        repo
        subdir
        dhalldir
        proxy
        linkessites

ksPureProxyStanza
  :: Certs.Domain
  -> DNSName
  -> KS.ApiProxyConfig
  -> KSMulti.StanzaConfig
ksPureProxyStanza domain txt proxy =
  let prefs = acmeCertPrefs Production domain in
  KSMulti.StanzaConfig_Proxy
    $ KSMulti.ProxyStanzaConfig
        (domain, txt)
        (Certs.keyPath prefs.cert_key)
        prefs.cert_pem
        proxy

phaseInOnCheddar :: KSMulti.StanzaConfig
phaseInOnCheddar =
  ksPureProxyStanza
    (Certs.Domain "phasein.dyn.dicioccio.fr")
    "_acme-challenge.phasein"
    (KS.SlashApiProxyList
      [ KS.SlashApiProxyDirective
          KS.UsePlainText
          ""
          KS.NoRewrite
          "localhost"
          7776
      ]
    )

eWebhook :: KSMulti.StanzaConfig
eWebhook =
  ksPureProxyStanza
    (Certs.Domain "e-webhook.dyn.dicioccio.fr")
    "_acme-challenge.e-webhook"
    (KS.SlashApiProxyList
      [ KS.SlashApiProxyDirective
          KS.UsePlainText
          "/private"
          KS.DropPrefix
          "lucasdicioccio-ThinkPad-T490.home"
          8083
      , KS.SlashApiProxyDirective
          KS.UsePlainText
          "/webhook"
          KS.DropPrefix
          "lucasdicioccio-ThinkPad-T490.home"
          8087
      , KS.SlashApiProxyDirective
          KS.UsePlainText
          "/pipeskouillou"
          KS.DropPrefix
          "127.0.0.1"
          2001
      ]
    )

pipeskouillouiApi :: Track' Spec -> Self.SelfPath -> Op
pipeskouillouiApi simulate selfpath =
    Postgrest.postgrestMigratedApi 
      simulate
      MigratePostgres
      PostgrestService
      selfpath
      (cheddarSelf Production)
      cfg
  where
    remote = Git.Remote "git@github.com:lucasdicioccio/blog.git"
    branch = Git.Branch "main"
    repo = Git.Repo "./git-repos/" "blog" remote branch
    d1 = Postgres.Database "salmon_pipeskouillou"
    u1 = Postgres.User "salmon_pipeskouillou_rw"
    pass1 = Postgres.Password "qwwxeqe12341c2nkjndscjhb2131!"
    connstring = Postgres.ConnString Postgres.localServer u1 pass1 d1
    cfg = Postgrest.PostgrestMigratedApiConfig
            "pipeskouillou"
            2001
            repo
            "dbs/pipeskouillou/tip.sql"
            ""
            connstring

boxSelf :: Environment -> Self.Remote
boxSelf _ = Self.Remote "salmon" "box.dicioccio.fr"

cheddarSelf :: Environment -> Self.Remote
cheddarSelf _ = Self.Remote "salmon" "cheddar.local"

cheddarRsync :: Environment -> Rsync.Remote
cheddarRsync _ = Rsync.Remote "salmon" "cheddar.local"

sreBox :: Environment -> Self.SelfPath -> Track' Spec -> Text -> Op
sreBox env selfpath simulate selfCertDomain =
    op "sre-box" (deps (ksmulti : domainCerts)) id
  where
    dns = dnsConfig selfCertDomain
    acme = acmeConfig dns env

    mkDNS = Track $ setupDNS Ssh.preExistingRemoteMachine simulate (boxSelf env) selfpath AuthoritativeDNS
    mkCert = Track $ acmeSign acme mkDNS

    domainCerts :: [Op]
    domainCerts = [acmeSign acme mkDNS domain | domain <- domains]

    cloneKS :: Tracked' FilePath
    cloneKS = case env of Production -> kitchenSink ; Staging -> kitchenSink_dev

    ksmulti :: Op
    ksmulti = KSMulti.setupKS Ssh.preExistingRemoteMachine mkCert cloneKS simulate (boxSelf env) selfpath ksMultiConfig KitchenSinkService

    ksMultiConfig :: KSMulti.KitchenSinkConfig
    ksMultiConfig = KSMulti.KitchenSinkConfig (Just dicioccioDotFr) [dicioccioDotFr, kitchenSinkBlog ]

cheddarBox :: Environment -> Self.SelfPath -> Track' Spec -> Text -> Op
cheddarBox env selfpath simulate selfCertDomain =
    op "cheddar-box" (deps [ ksmulti, registrations, pipeskouillouiApi simulate selfpath ] ) id
  where
    dns = dnsConfig selfCertDomain
    acme = acmeConfig dns env

    mkDNS = Track $ setupDNS Ssh.preExistingRemoteMachine simulate (boxSelf env) selfpath AuthoritativeDNS
    mkCert = Track $ acmeSign acme mkDNS

    cloneKS :: Tracked' FilePath
    cloneKS = case env of Production -> kitchenSink ; Staging -> kitchenSink_dev

    ksmulti :: Op
    ksmulti =
      KSMulti.setupKS
        Ssh.preExistingRemoteMachine
        mkCert
        cloneKS
        simulate
        (cheddarSelf env)
        selfpath
        ksMultiConfig
        KitchenSinkService

    ksMultiConfig :: KSMulti.KitchenSinkConfig
    ksMultiConfig =
      KSMulti.KitchenSinkConfig
        (Just eWebhook)
        [ phaseInOnCheddar
        , eWebhook
        , chouetteBlog
        ]

    registrations :: Op
    registrations =
      op "registration-tasks" (deps [registration name | name <- machineNames]) id

    machineNames :: [DNSName]
    machineNames = ["cheddar","chouette","e-webhook","phasein"]

    registrationConfig machineName =
      let prefs = taskPrefs env "auto-register-dns" in
      let tokenPrefs = microDNSTokensPrefs (Certs.Domain selfCertDomain) machineName in
      DNSRegistration.RegisteredMachineConfig
        machineName
        tokenPrefs.secret_path
        (prefs.task_script)
        (prefs.task_data_path $ Text.unpack machineName <> ".microdns-token")
        (prefs.task_data_path "microdns.self-signed-cert.pem")

    registration :: DNSName -> Op
    registration machineName =
      DNSRegistration.setupRegistration
        Ssh.preExistingRemoteMachine
        simulate
        selfpath
        (cheddarSelf env)
        dns
        (registrationConfig machineName)
        RegisterMachine

-------------------------------------------------------------------------------
laptop :: Track' Spec -> Self.SelfPath -> Op
laptop simulate selfpath =
    op "laptop" (deps packages) id
  where
    packages = fmap (Debian.deb . Debian.Package) [ "tmux", "git", "curl", "watch", "tree", "jq", "vim" ]

    selfCertDomain = "box.dicioccio.fr"
    dns = dnsConfig selfCertDomain
    machineName = "laptop"
    env = Production
    registration =
      DNSRegistration.setupRegistration
        Ssh.preExistingRemoteMachine
        simulate
        selfpath
        (cheddarSelf env)
        dns
        registrationConfig
        RegisterMachine
    registrationConfig =
      let prefs = taskPrefs env "auto-register-dns" in
      let tokenPrefs = microDNSTokensPrefs (Certs.Domain selfCertDomain) machineName in
      DNSRegistration.RegisteredMachineConfig
        machineName
        tokenPrefs.secret_path
        (prefs.task_script)
        (prefs.task_data_path $ Text.unpack machineName <> ".microdns-token")
        (prefs.task_data_path "microdns.self-signed-cert.pem")

-------------------------------------------------------------------------------
localDev :: Track' Spec -> Self.SelfPath -> Op
localDev simulate selfpath = noop "local-dev"

-------------------------------------------------------------------------------
data MachineSpec
  = Cheddar
  | Box
  | Laptop
  deriving (Generic)
instance FromJSON MachineSpec
instance ToJSON MachineSpec

data InfectTarget a
  = InfectTarget
  { infectUser :: Text
  , infectHost :: Text
  , infectPulicKey :: a
  } deriving (Generic)
instance FromJSON a => FromJSON (InfectTarget a)
instance ToJSON a => ToJSON (InfectTarget a)

data Spec
  = Batch [Spec]
  -- dev for working out new recipes
  | LocalDev
  -- configure machines
  | Initialize
  -- todo: | Infect InfectTarget
  | Machine MachineSpec Text
  | MigratePostgres (PGMigrate.RemoteMigrateSetup)
  -- services running
  | RegisterMachine DNSRegistration.RegisteredMachineSetup
  | AuthoritativeDNS MicroDNSSetup
  | SingleKintchensinkBlog KSBlog.KitchenSinkBlogSetup
  | KitchenSinkService KSMulti.KitchenSinkSetup
  | PostgrestService Postgrest.PostgrestSetup
  deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

program :: Self.SelfPath -> Manager -> Track' Spec
program selfpath httpManager =
   go 0
  where
   go n = Track $ \spec ->
     optimizedDeps $ op "program" (deps $ specOp (n+1) spec) $ \actions -> actions {
       notes = [Text.pack $ "at depth " <> show n]
     , ref = dotRef $ Text.pack $ "program:" <> show n
     }

   specOp :: Int -> Spec -> [Op]
   specOp k (LocalDev) = [localDev (go k) selfpath]
   -- meta
   specOp k (Batch xs) = concatMap (specOp k) xs
   -- machine
   specOp k (Initialize) = [Initialize.initialize]
   specOp k (Machine Box domainName) = [sreBox Production selfpath (go k) domainName]
   specOp k (Machine Cheddar domainName) = [cheddarBox Production selfpath (go k) domainName]
   specOp k (Machine Laptop domainName) = [laptop (go k) selfpath]
   -- actions
   specOp k (MigratePostgres arg) = [PGMigrate.applyMigration Debian.psql (Track PostgresInit.setupSingleUserPG) arg]
   -- services
   specOp k (RegisterMachine arg) = [DNSRegistration.registerMachine arg]
   specOp k (AuthoritativeDNS arg) = [systemdMicroDNS arg]
   specOp k (SingleKintchensinkBlog arg) = [KSBlog.systemdKitchenSinkBlog arg]
   specOp k (KitchenSinkService arg) = [KSMulti.systemdKitchenSink arg]
   specOp k (PostgrestService arg) = [Postgrest.systemdPostgrest arg]

   optimizedDeps :: Op -> Op
   optimizedDeps base =
     let pkgs = Debian.installAllDebsAtOnce base
     in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
  = SreBoxSeed { boxDomain :: Text }
  | CheddarBoxSeed { boxDomain :: Text }
  | LaptopSeed
  | LocalDevSeed

instance ParseRecord Seed where
  parseRecord =
      combo <**> helper
    where
      combo =
        subparser $ mconcat
          [ command "sre-box" (info sreBox (progDesc "configures SRE box"))
          , command "cheddar" (info cheddarBox (progDesc "configures cheddar"))
          , command "laptop" (info laptop (progDesc "configures my local laptop"))
          , command "localdev" (info localdev (progDesc "configures local stuff (for dev)"))
          ]
      sreBox = SreBoxSeed <$> strArgument (Options.Applicative.help "domain")
      cheddarBox = CheddarBoxSeed <$> strArgument (Options.Applicative.help "domain")
      localdev = pure LocalDevSeed
      laptop = pure LaptopSeed

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (LaptopSeed) = pure $ Machine Laptop "box.dicioccio.fr"
    go (SreBoxSeed d) = pure $ Machine Box d
    go (CheddarBoxSeed d) = pure $ Machine Cheddar d
    go (LocalDevSeed) = pure $ LocalDev 

-------------------------------------------------------------------------------
main :: IO ()
main = do
  let desc = fullDesc <> progDesc "Personal configurations." <> header "for dicioccio.fr"
  let opts = info parseRecord desc
  cmd <- execParser opts
  manager <- newManager defaultManagerSettings
  selfpath <- Self.readSelfPath_linux
  CLI.execCommandOrSeed configure (program selfpath manager) cmd
