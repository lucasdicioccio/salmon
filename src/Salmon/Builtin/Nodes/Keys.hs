module Salmon.Builtin.Nodes.Keys where

import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Binary

import qualified Crypto.JOSE.JWK as JWK
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (encode)
import Control.Monad (void)
import Data.Text (Text)
import Data.Functor.Contravariant (contramap)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

data KeyType
  = RSA2048
  | RSA4096
  | ED25519
  deriving (Eq, Ord, Show)

data SSHKeyPair = SSHKeyPair { sshKeyType :: KeyType , sshKeyDir :: FilePath, sshKeyName :: Text }
  deriving (Eq, Ord, Show)

privateKeyPath :: SSHKeyPair -> FilePath
privateKeyPath key = key.sshKeyDir </> Text.unpack key.sshKeyName

publicKeyPath :: SSHKeyPair -> FilePath
publicKeyPath key = key.sshKeyDir </> Text.unpack key.sshKeyName <> ".pub"

publicCAKeyPath :: SSHKeyPair -> FilePath
publicCAKeyPath key = key.sshKeyDir </> Text.unpack key.sshKeyName <> "-cert.pub"

-------------------------------------------------------------------------------

sshKey :: Track' (Binary "ssh-keygen") -> SSHKeyPair -> Op
sshKey bin key =
  withBinary bin sshkeygen (Keygen (key.sshKeyType, filepath)) $ \up ->
  op "ssh-key" (deps [enclosingdir]) $ \actions -> actions {
      help = "generate an ssh-key"
    , notes =
      [ "keeps keys around"
      ]
    , ref = dotRef $ "ssh:" <> Text.pack sshdir <> key.sshKeyName
    , prelim = skipIfFileExists filepath
    , up = up
    }
  where

    filename :: FilePath
    filename = Text.unpack key.sshKeyName

    sshdir :: FilePath
    sshdir = key.sshKeyDir

    filepath :: FilePath
    filepath = sshdir </> filename

    enclosingdir :: Op
    enclosingdir = dir (Directory sshdir)

newtype Keygen = Keygen (KeyType, FilePath)

sshkeygen :: Command "ssh-keygen" Keygen
sshkeygen = Command $ \(Keygen (kt, filepath)) ->
  case kt of
    RSA2048 -> proc "ssh-keygen" ["-t", "rsa", "-b", "2048", "-N", "", "-f", filepath]
    RSA4096 -> proc "ssh-keygen" ["-t", "rsa", "-b", "4096", "-N", "", "-f", filepath]
    ED25519 -> proc "ssh-keygen" ["-t", "ed25519", "-N", "", "-f", filepath]

newtype KeyIdentifier = KeyIdentifier { getIdentifier :: Text }
  deriving (Eq, Ord, Show)

data SSHCertificateAuthority = SSHCertificateAuthority { sshcaKey :: SSHKeyPair }
  deriving (Eq, Ord, Show)


-------------------------------------------------------------------------------

signKey
  :: Track' (Binary "ssh-keygen")
  -> SSHCertificateAuthority
  -> KeyIdentifier
  -> SSHKeyPair
  -> Op
signKey bin ca kid keyToSign =
  withBinary bin sshsign (SignKey (ca, kid,(privateKeyPath keyToSign))) $ \up ->
  op "ssh-ca-sign" (deps preds) $ \actions -> actions {
      help = "sign a SSH-key"
    , ref = dotRef $ "ssh-ca-sign:" <> Text.pack (show ca) <> kid.getIdentifier
    , prelim = skipIfFileExists (publicCAKeyPath keyToSign)
    , up = up
    }

  where
    preds =
      [ sshKey bin keyToSign
      , sshKey bin ca.sshcaKey
      ]

newtype SignKey = SignKey (SSHCertificateAuthority, KeyIdentifier, FilePath)

sshsign :: Command "ssh-keygen" SignKey
sshsign = Command $ \(SignKey(ca,kid,certifiedPath)) ->
  proc "ssh-keygen" ["-s", privateKeyPath ca.sshcaKey, "-I", Text.unpack kid.getIdentifier, certifiedPath ]


data JWKKeyPair = JWKKeyPair { jwkKeyType :: KeyType , jwkKeyDir :: FilePath, jwkKeyName :: Text }
  deriving (Eq, Ord, Show)

jwkfilepath :: JWKKeyPair -> FilePath
jwkfilepath key =
  key.jwkKeyDir </> Text.unpack key.jwkKeyName


jwkKey :: JWKKeyPair -> Op
jwkKey key =
  op "jwk-key" (deps [enclosingdir]) $ \actions -> actions {
      help = "generate a jwk-key"
    , notes = [ "keeps keys around" ]
    , ref = dotRef $ "jwk:" <> Text.pack jwkdir <> key.jwkKeyName
    , prelim = skipIfFileExists (jwkfilepath key)
    , up = up
    }
  where
    up :: IO ()
    up =  jwk >>= LBS.writeFile (jwkfilepath key) . encode

    jwk :: IO JWK.JWK
    jwk = case key.jwkKeyType of
             RSA2048 -> JWK.genJWK (JWK.RSAGenParam (2048 `div` 8))
             RSA4096 -> JWK.genJWK (JWK.RSAGenParam (4096 `div` 8))
             ED25519 -> JWK.genJWK (JWK.OKPGenParam JWK.Ed25519)

    filename :: FilePath
    filename = Text.unpack key.jwkKeyName

    jwkdir :: FilePath
    jwkdir = key.jwkKeyDir

    enclosingdir :: Op
    enclosingdir = dir (Directory jwkdir)


