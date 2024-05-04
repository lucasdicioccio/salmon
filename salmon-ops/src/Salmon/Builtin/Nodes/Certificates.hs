module Salmon.Builtin.Nodes.Certificates where

import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------
data Report
    = RunOpenSSLCommand !OpenSSLCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

newtype Domain = Domain {getDomain :: Text}
    deriving (Show, Ord, Eq)

data KeyType
    = RSA2048
    | RSA4096
    deriving (Show, Ord, Eq)

data Key
    = Key
    { keyType :: KeyType
    , keyDir :: FilePath
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

tlsKey :: Reporter Report -> Track' (Binary "openssl") -> Key -> Op
tlsKey r bin key =
    withBinary bin openssl cmd $ \up -> do
        op "certificate-key" (deps [enclosingdir]) $ \actions ->
            actions
                { help = "generate a certificate-key"
                , notes =
                    [ "does not delete keys on down"
                    ]
                , ref = dotRef $ "openssl:" <> Text.pack path
                , prelim = skipIfFileExists path
                , up = up r'
                }
  where
    cmd = GenTLSKey key.keyType path
    r' = contramap (RunOpenSSLCommand cmd) r
    path :: FilePath
    path = keyPath key

    enclosingdir :: Op
    enclosingdir = dir (Directory key.keyDir)

keyPath :: Key -> FilePath
keyPath key = key.keyDir </> Text.unpack key.keyName

signingRequest :: Reporter Report -> Track' (Binary "openssl") -> SigningRequest -> Op
signingRequest r bin req =
    withCommand (GenCSR kpath csrpath dom) $ \makeCSR ->
        withCommand (ConvertCSR2DER csrpath derpath) $ \convert ->
            op "certificate-csr" (deps [enclosingdir, tlsKey r bin req.certKey]) $ \actions ->
                actions
                    { help = "generate a certificate signing request"
                    , ref = dotRef $ "openssl:csr:" <> Text.pack csrpath
                    , up = void $ makeCSR >> convert
                    }
  where
    r' cmd = contramap (RunOpenSSLCommand cmd) r
    withCommand cmd f =
        let
            g :: (Reporter Binary.Report -> IO ()) -> Op
            g callbin = f (callbin (r' cmd))
         in
            withBinary bin openssl cmd g

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

selfSign :: Reporter Report -> Track' (Binary "openssl") -> SelfSigned -> Op
selfSign r bin selfsigned =
    withBinary bin openssl cmd $ \up ->
        op "certificate-self-sign" (deps [signingRequest r bin selfsigned.selfSignedRequest]) $ \actions ->
            actions
                { help = "self sign a certificate"
                , ref = dotRef $ "openssl:selfsign:" <> Text.pack pempath
                , prelim = skipIfFileExists pempath
                , up = up r'
                }
  where
    cmd = SignCSR csr key pempath
    r' = contramap (RunOpenSSLCommand cmd) r
    key :: FilePath
    key = keyPath selfsigned.selfSignedRequest.certKey

    csr :: FilePath
    csr = csrPath selfsigned.selfSignedRequest

    pempath :: FilePath
    pempath = selfsigned.selfSignedPEMPath

data OpenSSLCommand
    = GenCSR FilePath FilePath Domain
    | ConvertCSR2DER FilePath FilePath
    | SignCSR FilePath FilePath FilePath
    | GenTLSKey KeyType FilePath
    deriving (Show)

openssl :: Command "openssl" OpenSSLCommand
openssl = Command $ \cmd ->
    case cmd of
        (GenTLSKey kt filepath) ->
            case kt of
                RSA2048 -> proc "openssl" ["genrsa", "-out", filepath, "2048"]
                RSA4096 -> proc "openssl" ["genrsa", "-out", filepath, "4096"]
        (GenCSR keyPath csrPath dom) ->
            proc
                "openssl"
                [ "req"
                , "-new"
                , "-key"
                , keyPath
                , "-out"
                , csrPath
                , "-subj"
                , Text.unpack $ "/CN=" <> getDomain dom
                ]
        (ConvertCSR2DER csrPath derPath) ->
            proc
                "openssl"
                [ "req"
                , "-in"
                , csrPath
                , "-outform"
                , "DER"
                , "-out"
                , derPath
                ]
        (SignCSR csrPath keyPath pemPath) ->
            proc
                "openssl"
                [ "x509"
                , "-req"
                , "-in"
                , csrPath
                , "-signkey"
                , keyPath
                , "-out"
                , pemPath
                ]
