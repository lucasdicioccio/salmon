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

data SSHKeyType
  = RSA2048
  | RSA4096
  | ED25519
  deriving (Eq, Ord, Show)

data SSHKeyPair = SSHKeyPair { sshKeyType :: SSHKeyType , sshKeyDir :: FilePath, sshKeyName :: Text }
  deriving (Eq, Ord, Show)

sshKey :: SSHKeyPair -> Op
sshKey key =
  op "ssh-key" (deps [enclosingdir]) $ \actions -> actions {
      help = "generate an ssh-key"
    , notes =
      [ "keeps keys around"
      ]
    , ref = dotRef $ "ssh:" <> Text.pack sshdir <> key.sshKeyName
    , up = void $ readCreateProcessWithExitCode (createSSHProcess key.sshKeyType filepath) ""
    }
  where
    filename :: FilePath
    filename = Text.unpack key.sshKeyName

    filepath :: FilePath
    filepath = sshdir </> filename

    enclosingdir :: Op
    enclosingdir = dir (Directory sshdir)

    sshdir :: FilePath
    sshdir = key.sshKeyDir


createSSHProcess :: SSHKeyType -> FilePath -> CreateProcess
createSSHProcess kt filepath =
  case kt of
    RSA2048 -> proc "ssh-keygen" ["-t", "rsa", "-b", "2048", "-N", "", "-f", filepath]
    RSA4096 -> proc "ssh-keygen" ["-t", "rsa", "-b", "4096", "-N", "", "-f", filepath]
    ED25519 -> proc "ssh-keygen" ["-t", "ed25519", "-N", "", "-f", filepath]
