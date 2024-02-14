module Salmon.Builtin.Nodes.Acme where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))

import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Nodes.Continuation (Continue, withContinuation)
import Salmon.Builtin.Nodes.Keys (JWKKeyPair, jwkfilepath, jwkKey)
import qualified Salmon.Builtin.Nodes.Certificates as Cert
import Acme.NotAJoke.Api.Directory (fetchDirectory, directory, Directory(..))
import Acme.NotAJoke.Api.Account (postCreateAccount, createAccount, fetchAccount)
import Acme.NotAJoke.Api.Order (OrderIdentifier(..), OrderType(..), createOrder)
import Acme.NotAJoke.Api.Nonce (getNonce)
import Acme.NotAJoke.Api.CSR (CSR(..))
import Acme.NotAJoke.KeyManagement (loadJWKFile)
import Acme.NotAJoke.CertManagement (loadDER)
import Acme.NotAJoke.Api.Endpoint (BaseUrl)
import Acme.NotAJoke.Dancer (runAcmeDance_dns01, AcmeDancer(..), ghciDance, DanceStep)
import Data.Maybe

newtype Email = Email { getEmail :: Text }
  deriving (Show, Ord, Eq)

data Account = Account
  { accountBaseUrl   :: BaseUrl
  , accountKey   :: JWKKeyPair
  , accountEmail :: Email
  }
  deriving (Show)

acmeAccount :: Account -> Op
acmeAccount acc =
  op "acme-account" (deps [jwkKey acc.accountKey]) $ \actions -> actions {
      help = "creates an account against an ACME provider"
    , ref = dotRef $ "acme-account " <> getEmail acc.accountEmail
    , up = up
  }

  where
    contacts = [ "mailto:" <> getEmail acc.accountEmail ]
    up = void $ do
       jwk <- fromJust <$> loadJWKFile (jwkfilepath acc.accountKey)
       leDir <- fetchDirectory (directory acc.accountBaseUrl)
       nonce <- fromJust <$> getNonce leDir.newNonce
       postCreateAccount jwk leDir.newAccount nonce (createAccount contacts)

data Challenger
  = Challenger
  { challengerAccount :: Account
  , challengerRequest :: Cert.SigningRequest
  , challengerPEMDir :: FilePath
  , challengerPEMName :: Text
  , challengerContinuation :: Continue "dance" (FilePath -> DanceStep -> IO ())
  }

acmeChallenge_dns01 :: Track' Challenger -> Challenger -> Op
acmeChallenge_dns01 t chall =
  withContinuation chall.challengerContinuation $ \stepdance ->
    op "acme-challenge" (deps [run t chall, enclosingdir]) $ \actions -> actions {
        help = "sign a certificate with an ACME challenge"
      , ref = dotRef $ "acme:challenge:" <> Text.pack pemPath
      , prelim = skipIfFileExists pemPath
      , up = up stepdance
      }
  where
    acc :: Account
    acc = chall.challengerAccount

    enclosingdir :: Op
    enclosingdir = FS.dir (FS.Directory chall.challengerPEMDir)

    contacts = [ "mailto:" <> getEmail acc.accountEmail ]
    pemPath = chall.challengerPEMDir </> Text.unpack chall.challengerPEMName

    up stepdance = do
       jwk <- fromJust <$> loadJWKFile (jwkfilepath acc.accountKey)
       csr <- CSR <$> loadDER (Cert.derPath chall.challengerRequest)
       let domain = Cert.getDomain chall.challengerRequest.certDomain
       let order = createOrder (Nothing, Nothing) [ OrderIdentifier DNSOrder domain ]
       runAcmeDance_dns01
         $ AcmeDancer
             acc.accountBaseUrl
             jwk
             (fetchAccount contacts)
             csr
             order
             (stepdance pemPath)
