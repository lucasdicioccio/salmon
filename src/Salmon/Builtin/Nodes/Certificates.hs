module Salmon.Builtin.Nodes.Certificates where

import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Filesystem

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

newtype Domain = Domain { getDomain :: Text }
  deriving (Show, Ord, Eq)

data KeyType
  = RSA4096
  deriving (Show, Ord, Eq)

data Key
  = Key
  { keyType :: KeyType
  , keyDir  :: FilePath
  , keyName :: Text
  }
  deriving (Show, Ord, Eq)

data SigningRequest
  = SigningRequest
  { certDomain :: Domain
  , certKey :: Key
  , certCSRDir :: FilePath
  , certCSRName :: Text
  }
  deriving (Show, Ord, Eq)

csrPath :: SigningRequest -> FilePath
csrPath req = req.certCSRDir </> Text.unpack req.certCSRName

derPath :: SigningRequest -> FilePath
derPath req = csrPath req <> ".der"

data SelfSigned
  = SelfSigned
  { selfSignedPEMPath :: FilePath
  , selfSignedRequest :: SigningRequest
  }
  deriving (Show, Ord, Eq)

tlsKey :: Track' (Binary "openssl") -> Key -> Op
tlsKey bin key =
  using bin openssl (GenTLSKey key.keyType path) $ \up -> do
  op "certificate-key" (deps [enclosingdir]) $ \actions -> actions {
      help = "generate a certificate-key"
    , notes =
      [ "does not delete keys on down"
      ]
    , ref = dotRef $ "openssl:" <> Text.pack path
    , up = up
    }
  where
    path :: FilePath
    path = keyPath key

    enclosingdir :: Op
    enclosingdir = dir (Directory key.keyDir)

keyPath :: Key -> FilePath
keyPath key = key.keyDir </> Text.unpack key.keyName

signingRequest :: Track' (Binary "openssl") -> SigningRequest -> Op
signingRequest bin req =
  using bin openssl (GenCSR kpath csrpath dom) $ \makeCSR ->
  using bin openssl (ConvertCSR2DER csrpath derpath) $ \convert ->

  op "certificate-csr" (deps [enclosingdir, tlsKey bin req.certKey]) $ \actions -> actions {
      help = "generate a certificate signing request"
    , ref = dotRef $ "openssl:csr:" <> Text.pack csrpath
    , up = void $ makeCSR >> convert
    }
  where

    kpath :: FilePath
    kpath = keyPath req.certKey

    csrpath :: FilePath
    csrpath = csrPath req

    derpath :: FilePath
    derpath = derPath req

    enclosingdir :: Op
    enclosingdir = dir (Directory csrdir)

    csrdir :: FilePath
    csrdir = req.certCSRDir

    dom :: Domain
    dom = req.certDomain

selfSign :: Track' (Binary "openssl") -> SelfSigned -> Op
selfSign bin selfsigned =
  using bin openssl (SignCSR csr key pempath) $ \up ->
  op "certificate-self-sign" (deps [signingRequest bin selfsigned.selfSignedRequest]) $ \actions -> actions {
      help = "self sign a certificate"
    , ref = dotRef $ "openssl:selfsign:" <> Text.pack pempath
    , up = up
    }
  where

    key :: FilePath
    key = keyPath selfsigned.selfSignedRequest.certKey

    csr :: FilePath
    csr = csrPath selfsigned.selfSignedRequest

    pempath :: FilePath
    pempath = selfsigned.selfSignedPEMPath

data OpenSSL
  = GenCSR FilePath FilePath Domain
  | ConvertCSR2DER FilePath FilePath
  | SignCSR FilePath FilePath FilePath
  | GenTLSKey KeyType FilePath

openssl :: Command "openssl" OpenSSL
openssl = Command $ \cmd ->
  case cmd of
    (GenTLSKey kt filepath) ->
      case kt of
        RSA4096 -> proc "openssl" ["genrsa", "-out", filepath, "4096"]

    (GenCSR keyPath csrPath dom) ->
      proc "openssl"
        [ "req"
        , "-new"
        , "-key", keyPath
        , "-out", csrPath
        , "-subj", Text.unpack $ "/CN=" <> getDomain dom
        ]

    (ConvertCSR2DER csrPath derPath) ->
      proc "openssl"
        [ "req"
        , "-in", csrPath
        , "-outform", "DER"
        , "-out", derPath
        ]

    (SignCSR csrPath keyPath pemPath) ->
      proc "openssl"
        [ "x509"
        , "-req"
        , "-in", csrPath
        , "-signkey", keyPath
        , "-out", pemPath
        ]
