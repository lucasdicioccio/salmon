module Salmon.Builtin.Nodes.Binary (
    Binary,
    justInstall,
    Command (..),
    withBinary,
    withBinaryStdin,
    untrackedExec,
    CommandIO (..),
    withBinaryIO,
    untrackedExecIO,
    Report (..),
) where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode)
import GHC.TypeLits (Symbol)

import GHC.IO.Handle (Handle)
import System.Process (ProcessHandle, createProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------

data Report
    = CommandStart !CreateProcess
    | CommandStopped !CreateProcess !ExitCode !ByteString !ByteString
    | Referred !(Maybe Act') !Report
    deriving (Show)

-------------------------------------------------------------------------------

{- | A proxy type to pass binaries around.

This proxy cannot be constructed directly.
-}
data Binary (wellKnownName :: Symbol) = Binary

justInstall :: Track' (Binary sym) -> Op
justInstall t = run t Binary

-- | A command declares using a command.
data Command (wellKnownName :: Symbol) arg
    = Command
    { prepare :: arg -> CreateProcess
    }

{- | Captures the property that, to use a binary one needs to inherit the
dependencies from the binary provider.
-}
withBinary :: Reporter Report -> Track' (Binary x) -> Command x arg -> arg -> (IO () -> Op) -> Op
withBinary r t cmd arg consumeIO =
    withBinaryStdin r t cmd arg "" consumeIO

withBinaryStdin :: Reporter Report -> Track' (Binary x) -> Command x arg -> arg -> ByteString -> (IO () -> Op) -> Op
withBinaryStdin r t cmd arg stdin consumeIO =
    -- we use laziness here so that the Ref we add as Referral is the Ref from the enclosed Op (which has a circular dep itself)
    let mk a = (untrackedExec r' cmd a stdin, Binary)
        r' = contramap (Referred (opAct ret)) r
        ret = tracking t mk arg consumeIO
     in ret

untrackedExec :: Reporter Report -> Command x a -> a -> ByteString -> IO ()
untrackedExec r binary arg dat = do
    let p = prepare binary arg
    runReporter r (CommandStart p)
    (code, out, err) <- readCreateProcessWithExitCode p dat
    runReporter r (CommandStopped p code out err)

type RunningCommand = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

{- | A more general Command where more side-effects are allowed to generate the command and more information is returned.
we recommend using Command until this is no longer practical
intended use case is to redirect inputs/outputs but the mechanism could be abused to significantly alter the command being run based on runtime info (i.e., best avoided)
arg and ioarg allow to split a deterministic arg, which can be directly tracked, and an ioarg that will exist only when executing up/down effects
-}
data CommandIO (wellKnownName :: Symbol) arg ioarg
    = CommandIO
    { prepareIO :: arg -> ioarg -> IO CreateProcess
    }

withBinaryIO :: Track' (Binary x) -> CommandIO x arg ioarg -> arg -> ((ioarg -> IO RunningCommand) -> Op) -> Op
withBinaryIO t cmd arg consumeIO =
    let mk a = (untrackedExecIO cmd a, Binary)
     in tracking t mk arg consumeIO

untrackedExecIO :: CommandIO x a ioarg -> a -> (ioarg -> IO RunningCommand)
untrackedExecIO binary arg = \ioarg -> do
    p <- prepareIO binary arg ioarg
    createProcess p
