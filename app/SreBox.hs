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

import SreBox.Environment
import SreBox.CabalBuilding
import SreBox.CertSigning
import SreBox.MicroDNS
import SreBox.KitchenSinkBlog

boxSelf = Self.Remote "salmon" "box.dicioccio.fr"
boxRsync = Rsync.Remote "salmon" "box.dicioccio.fr"

domains :: [(Certs.Domain,Text)]
domains =
  [ (Certs.Domain "dicioccio.fr", "apex-challenge")
  , (Certs.Domain "phasein.dyn.dicioccio.fr", "_acme-challenge.phasein")
  , (Certs.Domain "e-webhook.dyn.dicioccio.fr", "_acme-challenge.e-webhook")
  , (Certs.Domain "localhost.dyn.dicioccio.fr", "_acme-challenge.localhost")
  ]

acmeConfig :: MicroDNSConfig -> Environment -> AcmeConfig
acmeConfig dns env =
    AcmeConfig account certdir pemName csr dns
  where
    le = case env of
           Production -> letsencryptv2
           Staging -> staging_letsencryptv2
    envPrefix = case env of
           Production -> "production-"
           Staging -> "staging-"
    envInfix = case env of
           Production -> "-production-"
           Staging -> "-staging-"

    certdir = "./acme/certs"
    account = Acme.Account le accountKey email
    email = Acme.Email "certmaster+salmon@dicioccio.fr"
    accountKey = Keys.JWKKeyPair Keys.RSA2048 "./jwk-keys" (envPrefix <> "key")
    domainKey domain = Certs.Key Certs.RSA4096 "./acme/keys" (Certs.getDomain domain <> ".rsa2048.key")
    csrpath domain = certdir <> "/" <> Text.unpack (Certs.getDomain domain) <> ".csr"
    pemName domain = mconcat [ "acme", envInfix, Certs.getDomain domain, ".pem"]
    csr domain = Certs.SigningRequest domain (domainKey domain) (csrpath domain) "cert.csr"

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
    postTxtRecordURL txtrecord val = mconcat ["https://", Text.unpack selfCertDomain, ":", show portnum, "/register/txt", Text.unpack txtrecord, "/", Text.unpack val]
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
        [ "caa example.dyn.dicioccio.fr. \"issue\" \"letsencrypt\""
        , "A dyn.dicioccio.fr. 163.172.53.34"
        , "A localhost.dyn.dicioccio.fr. 127.0.0.1"
        , "TXT dyn.dicioccio.fr. \"microdns\""
        , "TXT dyn.dicioccio.fr. \"salmon\""
        ]

ksConfig :: KitchenSinkBlogConfig
ksConfig =
    KitchenSinkBlogConfig
      (Git.Repo "./git-repos/" "blog" (Git.Remote "git@github.com:lucasdicioccio/blog.git") (Git.Branch "main"))
      "site-src"
      (Certs.Domain "dicioccio.fr", "apex-challenge")
      "./acme/certs/acme-production-dicioccio.fr.pem"
      "./acme/keys/dicioccio.fr.rsa2048.key"

sreBox :: Self.SelfPath -> Text -> Op
sreBox selfpath selfCertDomain =
    op "sre-box" (deps (ks : domainCerts)) id
  where 
    dns = dnsConfig selfCertDomain
    acme = acmeConfig dns Production

    mkDNS = Track $ setupDNS Ssh.preExistingRemoteMachine boxSelf selfpath RunningLocalDNS
    mkCert = Track $ acmeSign acme mkDNS

    domainCerts :: [Op]
    domainCerts = [acmeSign acme mkDNS domain | domain <- domains]

    ks :: Op
    ks = setupKS Ssh.preExistingRemoteMachine mkCert boxSelf selfpath ksConfig RunningLocalKitchenSinkBlog

-------------------------------------------------------------------------------
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
  let desc = fullDesc <> progDesc "Personal configurations." <> header "for dicioccio.fr"
  let opts = info parseRecord desc
  cmd <- execParser opts
  manager <- newManager defaultManagerSettings
  selfpath <- Self.readSelfPath_linux
  CLI.execCommandOrSeed configure (program selfpath manager) cmd
