module Salmon.Builtin.Nodes.Keys where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem

import Control.Monad (void)
import Data.Text (Text)
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

sshKey :: SSHKeyPair -> Op
sshKey key =
  op "ssh-key" (deps [enclosingdir]) $ \actions -> actions {
      help = "generate an ssh-key"
    , notes =
      [ "keeps keys around"
      ]
    , ref = dotRef $ "ssh:" <> Text.pack sshdir <> key.sshKeyName
    , up = void $ readCreateProcessWithExitCode (createKeygenProcess key.sshKeyType filepath) ""
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

createKeygenProcess :: KeyType -> FilePath -> CreateProcess
createKeygenProcess kt filepath =
  case kt of
    RSA2048 -> proc "ssh-keygen" ["-t", "rsa", "-b", "2048", "-N", "", "-f", filepath]
    RSA4096 -> proc "ssh-keygen" ["-t", "rsa", "-b", "4096", "-N", "", "-f", filepath]
    ED25519 -> proc "ssh-keygen" ["-t", "ed25519", "-N", "", "-f", filepath]

newtype KeyIdentifier = KeyIdentifier { getIdentifier :: Text }
  deriving (Eq, Ord, Show)

data SSHCertificateAuthority = SSHCertificateAuthority { sshcaKey :: SSHKeyPair }
  deriving (Eq, Ord, Show)

signKey :: SSHCertificateAuthority -> KeyIdentifier -> SSHKeyPair -> Op
signKey ca kid keyToSign =
  op "ssh-ca-sign" (deps [sshKey keyToSign, sshKey ca.sshcaKey]) $ \actions -> actions {
      help = "sign a SSH-key"
    , ref = dotRef $ "ssh-ca-sign:" <> Text.pack (show ca) <> kid.getIdentifier
    , up = void $ readCreateProcessWithExitCode (createSignKey ca kid (privateKeyPath keyToSign)) ""
    }

createSignKey :: SSHCertificateAuthority -> KeyIdentifier -> FilePath -> CreateProcess
createSignKey ca kid certifiedPath =
  proc "ssh-keygen" ["-s", privateKeyPath ca.sshcaKey, "-I", Text.unpack kid.getIdentifier, certifiedPath ]
