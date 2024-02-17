{-# LANGUAGE DeriveGeneric #-}

module SreBox.CertSigning where

import Acme.NotAJoke.Api.Certificate (storeCert)
import Acme.NotAJoke.Api.Validation (ValidationProof)
import Acme.NotAJoke.Dancer (DanceStep(..), showProof)
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
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
import Data.Functor.Contravariant ((>$<))
import System.FilePath ((</>))

import Salmon.Builtin.Extension
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.Track (Track(..), (>*<), using, opGraph, bindTracked)
import qualified Salmon.Builtin.CommandLine as CLI
import qualified Salmon.Builtin.Nodes.Acme as Acme
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
import SreBox.MicroDNS

data AcmeConfig
  = AcmeConfig
  { acme_cfg_account :: Acme.Account
  , acme_cfg_pemPath :: Certs.Domain -> FilePath
  , acme_cfg_csr     :: Certs.Domain -> Certs.SigningRequest
  , acme_cfg_dns     :: MicroDNSConfig
  }

acmeSign :: AcmeConfig -> Track' MicroDNSConfig -> (Certs.Domain, Text) -> Op
acmeSign cfg mkDNS (domain, txtrecord) =
    op "acme-sign" (deps [ Acme.acmeChallenge_dns01 chall challenger ]) $ \actions -> actions {
      ref = dotRef $ "acme-sign:" <> Certs.getDomain domain
    }
  where
    chall :: Track' Acme.Challenger
    chall = adapt >$< f1 >*< f2 >*< mkDNS

    adapt c = (Acme.challengerRequest c, (Acme.challengerAccount c, cfg.acme_cfg_dns))
    f1 :: Track' Certs.SigningRequest
    f1 = Track $ Certs.signingRequest Debian.openssl
    f2 :: Track' Acme.Account
    f2 = Track $ Acme.acmeAccount

    challenger = Acme.Challenger cfg.acme_cfg_account csr pempath runAcmeDance
    csr = cfg.acme_cfg_csr domain
    pempath = cfg.acme_cfg_pemPath domain

    runAcmeDance :: Continuation.Continue a (FilePath -> DanceStep -> IO ())
    runAcmeDance = Continuation.Continue ignoreTrack handle
      where
        handle :: FilePath -> DanceStep -> IO ()
        handle pemPath step =
          case step of
            Done _ cert -> do
              storeCert pemPath cert
            Validation (tok,keyAuth,sha) -> do
              cfg.acme_cfg_dns.microdns_cfg_postTxt txtrecord (showProof sha)
              threadDelay 1000000
              pure ()
            _ -> pure ()
