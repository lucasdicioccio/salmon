module Salmon.Builtin.Nodes.Keys where

import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Binary

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

-------------------------------------------------------------------------------

sshKey :: Track' (Binary "ssh-keygen") -> SSHKeyPair -> Op
sshKey bin key =
  op "ssh-key" (deps [install, enclosingdir]) $ \actions -> actions {
      help = "generate an ssh-key"
    , notes =
      [ "keeps keys around"
      ]
    , ref = dotRef $ "ssh:" <> Text.pack sshdir <> key.sshKeyName
    , up = up
    }
  where
    (exe, up) = exec sshkeygen (Keygen (key.sshKeyType, filepath))
    install = run bin exe

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
  op "ssh-ca-sign" (deps preds) $ \actions -> actions {
      help = "sign a SSH-key"
    , ref = dotRef $ "ssh-ca-sign:" <> Text.pack (show ca) <> kid.getIdentifier
    , up = up
    }

  where
    (exe, up) = exec sshsign (SignKey (ca, kid,(privateKeyPath keyToSign)))

    preds =
      [ run bin exe
      , sshKey bin keyToSign
      , sshKey bin ca.sshcaKey
      ]

newtype SignKey = SignKey (SSHCertificateAuthority, KeyIdentifier, FilePath)

sshsign :: Command "ssh-keygen" SignKey
sshsign = Command $ \(SignKey(ca,kid,certifiedPath)) ->
  proc "ssh-keygen" ["-s", privateKeyPath ca.sshcaKey, "-I", Text.unpack kid.getIdentifier, certifiedPath ]
