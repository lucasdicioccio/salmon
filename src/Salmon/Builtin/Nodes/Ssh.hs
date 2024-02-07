module Salmon.Builtin.Nodes.Ssh where

import Salmon.Op.Ref
import Salmon.Op.Track (run)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Binary

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

data Remote = Remote { remoteUser :: Text , remoteHost :: Text }
  deriving (Show, Ord, Eq)

call :: Track' (Binary "ssh") -> Track' Remote -> Remote -> FilePath -> Op
call ssh tRemote remote remotepath =
  withBinary ssh sshRun (Call remotepath remote) $ \up -> 
    op "ssh:call" (deps [run tRemote remote]) $ \actions -> actions {
        help = "calls " <>  Text.pack remotepath <> " on " <> remote.remoteHost
      , ref = dotRef $ "ssh-run" <> Text.pack (show (remotepath, remote))
      , up = up
      }

data Run = Call FilePath Remote

sshRun :: Command "ssh" Run
sshRun = Command $ \(Call path rem) ->
  proc "ssh"
    [ Text.unpack (loginAtHost rem)
    , path
    ]

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser,"@",rem.remoteHost]

