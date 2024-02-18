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
import qualified Data.ByteString as ByteString
import Options.Generic
import Options.Applicative

import Control.Monad (void,forM_)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Acme.NotAJoke.LetsEncrypt (staging_letsencryptv2, letsencryptv2)
import Acme.NotAJoke.Api.Validation (ValidationProof)
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
import qualified KitchenSink.Engine.Config as KS

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
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.CommandLine as CLI

import SreBox.Environment
import SreBox.CabalBuilding
import SreBox.CertSigning
import SreBox.MicroDNS
import qualified SreBox.KitchenSinkBlog as KSBlog
import qualified SreBox.KitchenSinkMultiSites as KSMulti

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
    MicroDNSConfig selfCertDomain dnsApex portnum postValidation dnsAdminKey dnsPemPath secretPath dnsCsr zonefile
  where
    portnum = 65432
    tlsDir x = "./certs" </> Text.unpack selfCertDomain </> x
    secretsDir x = "./secrets" </> Text.unpack selfCertDomain </> x
    dnsPemPath = tlsDir "microdns/self-signed/cert.pem"
    dnsCsrPath = tlsDir "microdns/self-signed/csr"
    secretPath = secretsDir "microdns/shared-secret/secret.b64"
    dnsAdminKey = Certs.Key Certs.RSA4096 (tlsDir "microdns/keys") "signing-key.rsa4096.key"
    dnsCsr = Certs.SigningRequest (Certs.Domain selfCertDomain) dnsAdminKey dnsCsrPath "cert.csr"
    postTxtRecordURL txtrecord val = mconcat ["https://", Text.unpack selfCertDomain, ":", show portnum, "/register/txt/", Text.unpack txtrecord, "/", Text.unpack val]
    postValidation txtrecord sha = do
      sharedsecret <- ByteString.readFile $ secretPath
      tlsManager <- makeTlsManagerForSelfSigned selfCertDomain dnsPemPath
      baseReq <- parseUrlThrow $ postTxtRecordURL txtrecord sha
      let hmac = Base16.encode $ HMAC256.hmac sharedsecret (Text.encodeUtf8 txtrecord)
      let req = baseReq { method = "POST", requestHeaders = [("x-microdns-hmac", hmac)] }
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

ksMultiConfig :: KSMulti.KitchenSinkConfig
ksMultiConfig =
    KSMulti.KitchenSinkConfig
      fallback
      sites
  where
    sites = [dicioccioDotFr, kitchenSinkBlog]
    fallback :: KSMulti.StanzaConfig
    fallback = dicioccioDotFr

kitchenSinkBlog :: KSMulti.StanzaConfig
kitchenSinkBlog =
  ksBlogStanza
    (Certs.Domain "kitchensink.dyn.dicioccio.fr")
    "The Kitchen Sink Blog Generator"
    "_acme-challenge.kitchensink"
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
     ])

dicioccioDotFr :: KSMulti.StanzaConfig
dicioccioDotFr =
  ksBlogStanza
    (Certs.Domain "dicioccio.fr")
    ("Lucas DiCioccio's Blog")
    "apex-challenge"
    (Git.Repo "./git-repos/" "blog" (Git.Remote "git@github.com:lucasdicioccio/blog.git") (Git.Branch "main"))
    "site-src"
    ""
    KS.NoProxying

ksBlogStanza
  :: Certs.Domain
  -> DNSName
  -> Text
  -> Git.Repo
  -> FilePath
  -> FilePath
  -> KS.ApiProxyConfig
  -> KSMulti.StanzaConfig
ksBlogStanza domain title txt repo subdir dhalldir proxy =
  let prefs = acmeCertPrefs Production domain in
  KSMulti.StanzaConfig 
    (domain, txt)
    title
    (Certs.keyPath prefs.cert_key)
    prefs.cert_pem
    repo
    subdir
    dhalldir
    proxy

boxSelf :: Environment -> Self.Remote
boxSelf _ = Self.Remote "salmon" "box.dicioccio.fr"

sreBox :: Environment -> Self.SelfPath -> Text -> Op
sreBox env selfpath selfCertDomain =
    op "sre-box" (deps (ksmulti : domainCerts)) id
  where 
    dns = dnsConfig selfCertDomain
    acme = acmeConfig dns env

    mkDNS = Track $ setupDNS Ssh.preExistingRemoteMachine (boxSelf env) selfpath RunningLocalDNS
    mkCert = Track $ acmeSign acme mkDNS

    domainCerts :: [Op]
    domainCerts = [acmeSign acme mkDNS domain | domain <- domains]

    cloneKS :: Tracked' FilePath
    cloneKS = case env of Production -> kitchenSink ; Staging -> kitchenSink_dev

    ksmulti :: Op
    ksmulti = KSMulti.setupKS Ssh.preExistingRemoteMachine mkCert cloneKS (boxSelf env) selfpath ksMultiConfig RunningLocalKitchenSink

-------------------------------------------------------------------------------
data Spec
  = SreBox Text
  | RunningLocalDNS MicroDNSSetup
  | RunningLocalKitchenSinkBlog KSBlog.KitchenSinkBlogSetup
  | RunningLocalKitchenSink KSMulti.KitchenSinkSetup
  deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

program :: Self.SelfPath -> Manager -> Track' Spec
program selfpath httpManager =
    Track $ \spec -> optimizedDeps $ op "program" (deps $ specOp spec) id
  where
   specOp :: Spec -> [Op]
   specOp (SreBox domainName) =  [sreBox Production selfpath domainName]
   specOp (RunningLocalDNS arg) =  [systemdMicroDNS arg]
   specOp (RunningLocalKitchenSinkBlog arg) =  [KSBlog.systemdKitchenSinkBlog arg]
   specOp (RunningLocalKitchenSink arg) =  [KSMulti.systemdKitchenSink arg]
  
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
  let desc = fullDesc <> progDesc "Personal configurations." <> header "for dicioccio.fr"
  let opts = info parseRecord desc
  cmd <- execParser opts
  manager <- newManager defaultManagerSettings
  selfpath <- Self.readSelfPath_linux
  CLI.execCommandOrSeed configure (program selfpath manager) cmd
