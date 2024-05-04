module Salmon.Builtin.Nodes.Bash where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Reporter

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------

data Report
    = RunBash !BashCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
run :: Reporter Report -> Track' (Binary "bash") -> File "script" -> Op
run r bash script =
    withFile script $ \filepath ->
        let cmd = BashCommand filepath
         in withBinary bash bashrun cmd $ \up ->
                op "bash-run" nodeps $ \actions ->
                    actions
                        { help = "runs a bash command"
                        , ref = dotRef $ "bash-run:" <> Text.pack filepath
                        , up = up (r' cmd)
                        }
  where
    r' cmd = contramap (RunBash cmd) r

data BashCommand = BashCommand FilePath
    deriving (Show)

bashrun :: Command "bash" BashCommand
bashrun = Command $ \(BashCommand path) ->
    proc
        "bash"
        [ path
        ]
