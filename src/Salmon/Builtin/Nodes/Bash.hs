module Salmon.Builtin.Nodes.Bash where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Binary

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

run :: Track' (Binary "bash") -> File "script" -> Op
run bash script =
  withFile script $ \filepath ->
  withBinary bash bashrun (Run filepath) $ \up -> 
    op "bash-run" nodeps $ \actions -> actions {
        help = "runs a bash command"
      , ref = dotRef $ "bash-run:" <> Text.pack filepath
      , up = up
      }

data Run = Run FilePath

bashrun :: Command "bash" Run
bashrun = Command $ \(Run path) ->
  proc "bash"
    [ path
    ]