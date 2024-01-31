module Salmon.Builtin.Nodes.Certificates where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
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

data SelfSigned
  = SelfSigned
  { selfSignedPEMPath :: FilePath
  , selfSignedRequest :: SigningRequest
  }
  deriving (Show, Ord, Eq)

tlsKey :: Key -> Op
tlsKey key =
  op "certificate-key" (deps [enclosingdir]) $ \actions -> actions {
      help = "generate a certificate-key"
    , notes =
      [ "does not delete keys on down"
      ]
    , ref = dotRef $ "openssl:" <> Text.pack path
    , up = void $ readCreateProcessWithExitCode (createGenTLSKeyProcess key.keyType path) ""
    }
  where
    path :: FilePath
    path = keyPath key

    enclosingdir :: Op
    enclosingdir = dir (Directory key.keyDir)

keyPath :: Key -> FilePath
keyPath key = key.keyDir </> Text.unpack key.keyName

signingRequest :: SigningRequest -> Op
signingRequest req =
  op "certificate-csr" (deps [enclosingdir, tlsKey req.certKey]) $ \actions -> actions {
      help = "generate a certificate signing request"
    , ref = dotRef $ "openssl:csr:" <> Text.pack csrpath
    , up = void $ do
             readCreateProcessWithExitCode genCSR ""
             readCreateProcessWithExitCode convertDER ""
    }
  where
    genCSR :: CreateProcess
    genCSR = createGenCSR kpath csrpath dom

    convertDER :: CreateProcess
    convertDER = convertCSRtoDER csrpath derpath

    kpath :: FilePath
    kpath = keyPath req.certKey

    csrpath :: FilePath
    csrpath = csrPath req

    enclosingdir :: Op
    enclosingdir = dir (Directory csrdir)

    csrdir :: FilePath
    csrdir = req.certCSRDir

    derpath :: FilePath
    derpath = csrpath <> ".der"

    dom :: Domain
    dom = req.certDomain

selfSign :: SelfSigned -> Op
selfSign selfsigned =
  op "certificate-self-sign" (deps [signingRequest selfsigned.selfSignedRequest]) $ \actions -> actions {
      help = "self sign a certificate"
    , ref = dotRef $ "openssl:selfsign:" <> Text.pack pempath
    , up = void $ readCreateProcessWithExitCode genCSR ""
    }
  where
    genCSR :: CreateProcess
    genCSR = signCSRProcess csr key pempath

    key :: FilePath
    key = keyPath selfsigned.selfSignedRequest.certKey

    csr :: FilePath
    csr = csrPath selfsigned.selfSignedRequest

    pempath :: FilePath
    pempath = selfsigned.selfSignedPEMPath

createGenCSR :: FilePath -> FilePath -> Domain -> CreateProcess
createGenCSR keyPath csrPath dom =
  proc "openssl"
    [ "req"
    , "-new"
    , "-key", keyPath
    , "-out", csrPath
    , "-subj", Text.unpack $ "/CN=" <> getDomain dom
    ]

convertCSRtoDER :: FilePath -> FilePath -> CreateProcess
convertCSRtoDER csrPath derPath =
  proc "openssl"
    [ "req"
    , "-in", csrPath
    , "-outform", "DER"
    , "-out", derPath
    ]

createGenTLSKeyProcess :: KeyType -> FilePath -> CreateProcess
createGenTLSKeyProcess kt filepath =
  case kt of
    RSA4096 -> proc "openssl" ["genrsa", "-out", filepath, "4096"]

signCSRProcess :: FilePath -> FilePath -> FilePath -> CreateProcess
signCSRProcess csrPath keyPath pemPath =
  proc "openssl"
    [ "x509"
    , "-req"
    , "-in", csrPath
    , "-signkey", keyPath
    , "-out", pemPath
    ]
