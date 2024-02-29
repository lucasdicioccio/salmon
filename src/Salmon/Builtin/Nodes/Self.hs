-- | todo: pass rsync in
{-# LANGUAGE DeriveGeneric #-}
module Salmon.Builtin.Nodes.Self where

import Data.Aeson (ToJSON, FromJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic (toDyn)
import GHC.Generics (Generic)
import Data.Text (Text)
import System.FilePath ((</>), takeFileName)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LByteString

import Salmon.Op.Track (Track(..), (>*<), Tracked(..), using, opGraph, bindTracked)
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Extension
import Salmon.Op.Ref
import qualified Salmon.Builtin.CommandLine as CLI

import System.Posix.Files (readSymbolicLink)

newtype SelfPath = SelfPath { getSelfPath :: FilePath }
  deriving (Show, Ord, Eq)

readSelfPath_linux :: IO SelfPath
readSelfPath_linux = SelfPath <$> readSymbolicLink "/proc/self/exe"

data Remote = Remote { remoteUser :: Text , remoteHost :: Text }
  deriving (Show, Ord, Eq)

data RemoteSelf = RemoteSelf { selfRemote :: Remote, selfRemotePath :: FilePath }

uploadSelf :: FilePath -> Remote -> SelfPath -> Tracked' RemoteSelf
uploadSelf remotedir remote path =
    Tracked (Track $ \self -> op "copy-oneself" (deps [copy]) id) (RemoteSelf remote selfpathOnRemote)
  where
    selfpathOnRemote = remotedir  </> takeFileName (getSelfPath path)
    rsyncRemote = Rsync.Remote remote.remoteUser remote.remoteHost
    copy = Rsync.sendFile Debian.rsync (FS.PreExisting $ getSelfPath path) rsyncRemote selfpathOnRemote

data RemoteCall a
  = RemoteCall
  { remoteCall_command :: CLI.BaseCommand
  , remoteCall_directive :: a
  } deriving (Show,Eq,Generic)
instance ToJSON a => ToJSON (RemoteCall a)
instance FromJSON a => FromJSON (RemoteCall a)

-- | A record for dynamic remote-op.
data RemoteOp = RemoteOp Op

callSelf
  :: forall directive. (ToJSON directive, FromJSON directive)
  => RemoteSelf
  -> Track' directive
  -> CLI.BaseCommand
  -> directive
  -> Tracked' (RemoteCall directive)
callSelf self simulate base directive =
    Tracked (Track $ \_ -> op "self-call" (deps [callOverSSH]) modActions) rc
  where
    modActions actions =
      actions {
        ref = dotRef $ "call-self:" <> Text.decodeUtf8 (LByteString.toStrict $ encode directive)
      , dynamics = [toDyn $ RemoteOp $ run simulate directive ]
      }
    rc = RemoteCall base directive
    sshRemote = Ssh.Remote self.selfRemote.remoteUser self.selfRemote.remoteHost
    cmdArgs = ["run", CLI.argForBaseCommand base]
    cmdStdin = toStrict $ encode directive
    callOverSSH = Ssh.call Debian.ssh ignoreTrack sshRemote self.selfRemotePath cmdArgs cmdStdin

callSelfAsSudo
  :: forall directive. (ToJSON directive, FromJSON directive)
  => Track' Ssh.Remote
  -> RemoteSelf
  -> Track' directive
  -> CLI.BaseCommand
  -> directive
  -> Tracked' (RemoteCall directive)
callSelfAsSudo mkRemote self simulate base directive =
    Tracked (Track $ \_ -> op "self-call" (deps [callOverSSH]) modActions) rc
  where
    modActions actions =
      actions {
        ref = dotRef $ "call-self-sudo:" <> Text.decodeUtf8 (LByteString.toStrict $ encode directive)
      , dynamics = [toDyn $ RemoteOp $ run simulate directive ]
      }
    rc = RemoteCall base directive
    sshRemote = Ssh.Remote self.selfRemote.remoteUser self.selfRemote.remoteHost
    cmdArgs = [Text.pack self.selfRemotePath, "run", CLI.argForBaseCommand base]
    cmdStdin = toStrict $ encode directive
    callOverSSH = Ssh.call Debian.ssh mkRemote sshRemote "sudo" cmdArgs cmdStdin
