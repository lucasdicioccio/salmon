module Salmon.Builtin.Nodes.Ssh where

import Salmon.Op.Ref
import Salmon.Op.Track (Track(..),run)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Binary

import Control.Monad (void)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

data Remote = Remote { remoteUser :: Text , remoteHost :: Text }
  deriving (Show, Ord, Eq)

call :: Track' (Binary "ssh") -> Track' Remote -> Remote -> FilePath -> [Text] -> ByteString -> Op
call ssh tRemote remote remotepath args stdin =
  withBinaryStdin ssh sshRun (Call remotepath remote args) stdin $ \up -> 
    op "ssh:call" (deps [run tRemote remote]) $ \actions -> actions {
        help = "calls " <>  Text.pack remotepath <> " on " <> remote.remoteHost
      , ref = dotRef $ "ssh-run" <> Text.pack (show (remotepath, remote, args, stdin))
      , up = up
      }

data Run = Call FilePath Remote [Text]

sshRun :: Command "ssh" Run
sshRun = Command $ \(Call path rem args) ->
  proc "ssh"
    (  [ Text.unpack (loginAtHost rem)
       , path
       ]
     <> map Text.unpack args
    )

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser,"@",rem.remoteHost]

preExistingRemoteMachine :: Track' Remote
preExistingRemoteMachine = Track $ \r -> placeholder "remote" ("a remote at" <> r.remoteHost)

