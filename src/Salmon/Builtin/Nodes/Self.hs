-- | todo: pass rsync in
-- | todo: add an ssh-call
module Salmon.Builtin.Nodes.Self where

import Data.Text (Text)
import System.FilePath ((</>), takeFileName)

import Salmon.Op.Track (Track(..), (>*<), Tracked(..), using, opGraph, bindTracked)
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Extension

data Remote = Remote { remoteUser :: Text , remoteHost :: Text }
  deriving (Show, Ord, Eq)

data Self = Self { selfRemote :: Remote, selfRemotePath :: FilePath }

uploadSelf :: FilePath -> Remote -> FilePath -> Tracked' Self
uploadSelf remotedir remote path =
    Tracked (Track $ \self -> op "copy-oneself" (deps [copy]) id) (Self remote selfpathOnRemote)
  where
    selfpathOnRemote = remotedir  </> takeFileName path
    rsyncRemote = Rsync.Remote remote.remoteUser remote.remoteHost
    copy = Rsync.sendFile Debian.rsync (FS.PreExisting path) rsyncRemote remotedir
