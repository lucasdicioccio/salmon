{-# LANGUAGE PatternSynonyms #-}

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
    pattern CommandSuccess,
    isCommandSuccessful,
    CommandFailed (..),
    CommandFailedSimple (..),
    checkExitCode,
) where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import GHC.TypeLits (Symbol)

import GHC.IO.Handle (Handle)
import System.Process (ProcessHandle, createProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------

data Report
    = CommandStart !CreateProcess
    | CommandStopped !CreateProcess !ExitCode !ByteString !ByteString
    | Requested !(Maybe Act') !Report
    deriving (Show)

pattern CommandSuccess out err <-
    CommandStopped _ ExitSuccess out err

isCommandSuccessful :: Report -> Bool
isCommandSuccessful r = case r of
    (CommandStart _) -> False
    (CommandStopped _ ExitSuccess _ _) -> True
    (CommandStopped _ _ _ _) -> False
    (Requested _ child) -> isCommandSuccessful child

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
withBinary :: Track' (Binary x) -> Command x arg -> arg -> ((Reporter Report -> IO ()) -> Op) -> Op
withBinary t cmd arg consumeIO =
    withBinaryStdin t cmd arg "" consumeIO

withBinaryStdin :: Track' (Binary x) -> Command x arg -> arg -> ByteString -> ((Reporter Report -> IO ()) -> Op) -> Op
withBinaryStdin t cmd arg stdin consumeIO =
    -- we use laziness here so that the Ref we add as Referral is the Ref from the enclosed Op (which has a circular dep itself)
    let mk a = (untrackedExec cmd a stdin, Binary)
        -- wrap consumer by capturing the reporter being passed around
        fconsume :: (Reporter Report -> IO ()) -> Op
        fconsume f =
            let
                g :: Reporter Report -> IO ()
                g r = f (contramap (Requested (opAct ret)) r)
             in
                consumeIO g
        ret = tracking t mk arg fconsume
     in ret

{- | Runs the command and, unlike a naive shell-out, does not swallow a
non-zero exit: after reporting 'CommandStopped' (so the failure is still
visible in the 'Report' stream either way), it throws 'CommandFailed'. This
is what lets "Salmon.Actions.UpDown".'Salmon.Actions.UpDown.upTree' actually
notice a failing command instead of blindly running every dependent as if it
had succeeded.
-}
untrackedExec :: Command x a -> a -> ByteString -> (Reporter Report -> IO ())
untrackedExec binary arg dat = \r -> do
    let p = prepare binary arg
    runReporter r (CommandStart p)
    (code, out, err) <- readCreateProcessWithExitCode p dat
    runReporter r (CommandStopped p code out err)
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> throwIO (CommandFailed p n out err)

-- | Thrown by 'untrackedExec' (and so, transitively, by every node built on 'withBinary') on a non-zero exit.
data CommandFailed
    = CommandFailed
    { commandFailed_process :: CreateProcess
    , commandFailed_exitCode :: Int
    , commandFailed_stdout :: ByteString
    , commandFailed_stderr :: ByteString
    }

instance Show CommandFailed where
    show e =
        mconcat
            [ "command failed (exit "
            , show e.commandFailed_exitCode
            , "): "
            , show (cmdspec e.commandFailed_process)
            , "\nstdout:\n"
            , C8.unpack e.commandFailed_stdout
            , "\nstderr:\n"
            , C8.unpack e.commandFailed_stderr
            ]

instance Exception CommandFailed

{- | A minimal variant of 'CommandFailed' for call sites that only have a
human-readable label for what ran, not the full 'CreateProcess' (e.g. those
built on 'withBinaryIO', which hands back a raw 'ProcessHandle' rather than
a checked result — see "Salmon.Builtin.Nodes.WireGuard" for an example).
-}
data CommandFailedSimple = CommandFailedSimple String Int

instance Show CommandFailedSimple where
    show (CommandFailedSimple label n) = mconcat ["command failed (exit ", show n, "): ", label]

instance Exception CommandFailedSimple

checkExitCode :: String -> ExitCode -> IO ()
checkExitCode _ ExitSuccess = pure ()
checkExitCode label (ExitFailure n) = throwIO (CommandFailedSimple label n)

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
