module Salmon.Builtin.Nodes.Binary
  ( Binary
  , justInstall
  , Command(..)
  , withBinary
  , withBinaryStdin
  , untrackedExec
  ) where

import Salmon.Op.Track
import Salmon.Op.OpGraph
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem

import Control.Monad (void)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import GHC.TypeLits (Symbol)

import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

-- | A proxy type to pass binaries around.
--
-- This proxy cannot be constructed directly.
data Binary (wellKnownName :: Symbol) = Binary

justInstall :: Track' (Binary sym) -> Op
justInstall t = run t Binary

-- | A command declares using a command.
data Command (wellKnownName :: Symbol) arg =
  Command
  { prepare :: arg -> CreateProcess 
  }

-- | Captures the property that, to use a binary one needs to inherit the
-- dependencies from the binary provider.
withBinary :: Track' (Binary x) -> Command x arg -> arg -> (IO () -> Op) -> Op
withBinary t cmd arg consumeIO =
  let mk a = (untrackedExec cmd a "", Binary)
  in
  tracking t mk arg consumeIO

withBinaryStdin :: Track' (Binary x) -> Command x arg -> arg -> ByteString -> (IO () -> Op) -> Op
withBinaryStdin t cmd arg stdin consumeIO =
  let mk a = (untrackedExec cmd a stdin, Binary)
  in
  tracking t mk arg consumeIO

untrackedExec :: Command x a -> a -> ByteString -> IO ()
untrackedExec binary arg dat =
  void $ readCreateProcessWithExitCode (prepare binary arg) dat
