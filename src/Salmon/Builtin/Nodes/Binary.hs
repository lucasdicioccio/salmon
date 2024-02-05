module Salmon.Builtin.Nodes.Binary
  ( Binary
  , Command(..)
  , withBinary
  , untrackedExec
  ) where

import Salmon.Op.Track
import Salmon.Op.OpGraph
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (Symbol)

import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

-- | A proxy type to pass binaries around.
--
-- This proxy cannot be constructed directly.
data Binary (wellKnownName :: Symbol) = Binary

-- | A command declares using a command.
data Command (wellKnownName :: Symbol) arg =
  Command
  { prepare :: arg -> CreateProcess 
  }

-- | Captures the property that, to use a binary one needs to inherit the
-- dependencies from the binary provider.
withBinary :: Track' (Binary x) -> Command x arg -> arg -> (IO () -> Op) -> Op
withBinary t cmd arg consumeIO =
  let mk a = (untrackedExec cmd a, Binary)
  in
  tracking t mk arg consumeIO

untrackedExec :: Command x a -> a -> IO ()
untrackedExec binary arg =
  void $ readCreateProcessWithExitCode (prepare binary arg) ""
