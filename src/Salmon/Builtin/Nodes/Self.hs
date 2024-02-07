-- | todo: pass rsync in
-- | todo: add an ssh-call
{-# LANGUAGE DeriveGeneric #-}
module Salmon.Builtin.Nodes.Self where

import Data.Aeson (ToJSON, FromJSON, encode)
import Data.ByteString.Lazy (toStrict)
import GHC.Generics (Generic)
import Data.Text (Text)
import System.FilePath ((</>), takeFileName)

import Salmon.Op.Track (Track(..), (>*<), Tracked(..), using, opGraph, bindTracked)
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.CommandLine as CLI

import System.Posix.Files (readSymbolicLink)

newtype SelfPath = SelfPath { getSelfPath :: FilePath }
  deriving (Show, Ord, Eq)

readSelfPath_linux :: IO SelfPath
readSelfPath_linux = SelfPath <$> readSymbolicLink "/proc/self/exe"

data Remote = Remote { remoteUser :: Text , remoteHost :: Text }
  deriving (Show, Ord, Eq)

data Self = Self { selfRemote :: Remote, selfRemotePath :: FilePath }

uploadSelf :: FilePath -> Remote -> SelfPath -> Tracked' Self
uploadSelf remotedir remote path =
    Tracked (Track $ \self -> op "copy-oneself" (deps [copy]) id) (Self remote selfpathOnRemote)
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

callSelf
  :: forall directive. (ToJSON directive, FromJSON directive)
  => Self
  -> CLI.BaseCommand
  -> directive
  -> Tracked' (RemoteCall directive)
callSelf self base directive =
    Tracked (Track $ \_ -> op "call-oneself" (deps [callOverSSH]) id) rc
  where
    rc = RemoteCall base directive
    sshRemote = Ssh.Remote self.selfRemote.remoteUser self.selfRemote.remoteHost
    cmdArgs = ["run", CLI.argForBaseCommand base]
    cmdStdin = toStrict $ encode directive
    callOverSSH = Ssh.call Debian.ssh ignoreTrack sshRemote self.selfRemotePath cmdArgs cmdStdin
