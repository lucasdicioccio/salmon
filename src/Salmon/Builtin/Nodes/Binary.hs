module Salmon.Builtin.Nodes.Binary where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (Symbol)

import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

data Binary (wellKnownName :: Symbol) = Binary

data Command (wellKnownName :: Symbol) arg =
  Command
  { prepare :: arg -> CreateProcess 
  }

exec :: Command x a -> a -> (Binary x, IO())
exec binary arg =
  let io = void $ readCreateProcessWithExitCode (prepare binary arg) ""
  in (Binary, io)
