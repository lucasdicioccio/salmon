{-# LANGUAGE PatternSynonyms #-}

module Salmon.Builtin.Nodes.Binary (
    Binary,
    justInstall,
    Command (..),
    withBinary,
    withBinaryStdin,
    untrackedExec,
    untrackedExecOutput,

    -- * Where a command's output goes
    Sink (..),
    Routing (..),
    captured,
    streamed,
    streamedKeeping,
    discarded,
    appendedTo,
    defaultTailBytes,
    withBinaryWith,
    withBinaryStdinWith,
    untrackedExecWith,
    runRouted,
    tailOf,
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
import Salmon.Builtin.NodeLog (Channel (..))
import qualified Salmon.Builtin.NodeLog as NodeLog
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Actions (Act (..))
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Concurrent.Async (concurrently)
import Control.Exception (Exception, IOException, throwIO, try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import GHC.IO.Exception (ExitCode (..))
import GHC.TypeLits (Symbol)

import GHC.IO.Handle (Handle)
import System.IO (IOMode (..), hClose, hIsEOF)
import qualified System.IO as IO
import System.Process (ProcessHandle, StdStream (..), createProcess, waitForProcess, withCreateProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------

data Report
    = CommandStart !CreateProcess
    | CommandStopped !CreateProcess !ExitCode !ByteString !ByteString
    | Requested !(Maybe Act') !Report
    | -- | One line a command wrote while running, under a 'Stream' sink
      -- (see 'Routing'); never emitted by a command whose output is
      -- 'captured', which is every command that does not ask otherwise.
      CommandOutput !CreateProcess !Channel !ByteString
    deriving (Show)

pattern CommandSuccess out err <-
    CommandStopped _ ExitSuccess out err

isCommandSuccessful :: Report -> Bool
isCommandSuccessful r = case r of
    (CommandStart _) -> False
    (CommandStopped _ ExitSuccess _ _) -> True
    (CommandStopped _ _ _ _) -> False
    (CommandOutput _ _ _) -> False
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
withBinary = withBinaryWith captured

withBinaryStdin :: Track' (Binary x) -> Command x arg -> arg -> ByteString -> ((Reporter Report -> IO ()) -> Op) -> Op
withBinaryStdin = withBinaryStdinWith captured

-- | 'withBinary' with a say in where the command's output goes; see 'Routing'.
withBinaryWith :: Routing -> Track' (Binary x) -> Command x arg -> arg -> ((Reporter Report -> IO ()) -> Op) -> Op
withBinaryWith routing t cmd arg consumeIO =
    withBinaryStdinWith routing t cmd arg "" consumeIO

{- | 'withBinaryStdin' with a say in where the command's output goes.

Under a 'Stream' sink, each line the command writes is a
"Salmon.Builtin.NodeLog" line about the node the command is run by (the
enclosed 'Op'), emitted while the command is still running: that is what a
one-shot @run up@ prints, what @--json@ reports and what @run serve@ puts on
@\/events@. Those lines are /not/ also handed to the caller's 'Reporter', which
still gets 'CommandStart' and a 'CommandStopped' carrying the tail that was
kept: one line has one destination, and a recipe's reporter that prints
would otherwise print every line a second time.
-}
withBinaryStdinWith :: Routing -> Track' (Binary x) -> Command x arg -> arg -> ByteString -> ((Reporter Report -> IO ()) -> Op) -> Op
withBinaryStdinWith routing t cmd arg stdin consumeIO =
    -- we use laziness here so that the Ref we add as Referral is the Ref from the enclosed Op (which has a circular dep itself)
    let mk a = (untrackedExecWith routing cmd a stdin, Binary)
        -- wrap consumer by capturing the reporter being passed around
        fconsume :: (Reporter Report -> IO ()) -> Op
        fconsume f =
            let
                g :: Reporter Report -> IO ()
                g r = f (nodeLogTap (opAct ret) (contramap (Requested (opAct ret)) r))
             in
                consumeIO g
        ret = tracking t mk arg fconsume
     in ret

{- | Sends the lines of a streamed command to "Salmon.Builtin.NodeLog" under
the node's 'Ref' and everything else to the given reporter. With no node to
name, everything goes to the reporter.
-}
nodeLogTap :: Maybe Act' -> Reporter Report -> Reporter Report
nodeLogTap mact r = ReporterM $ \rep ->
    case (mact, rep) of
        (Just act, CommandOutput _ channel line) ->
            NodeLog.emit (NodeLog.Line act.extension.ref channel (decodeLine line))
        _ -> runReporter r rep

decodeLine :: ByteString -> Text
decodeLine = Text.dropWhileEnd (== '\r') . Text.decodeUtf8With Text.lenientDecode

{- | Runs the command and, unlike a naive shell-out, does not swallow a
non-zero exit: after reporting 'CommandStopped' (so the failure is still
visible in the 'Report' stream either way), it throws 'CommandFailed'. This
is what lets "Salmon.Actions.UpDown".'Salmon.Actions.UpDown.upTree' actually
notice a failing command instead of blindly running every dependent as if it
had succeeded.
-}
untrackedExec :: Command x a -> a -> ByteString -> (Reporter Report -> IO ())
untrackedExec = untrackedExecWith captured

{- | 'untrackedExec' with a say in where the command's output goes. A
'Stream'ed line is reported as 'CommandOutput' when it is read; what
'CommandStopped' and 'CommandFailed' carry is what the 'Routing' kept.
-}
untrackedExecWith :: Routing -> Command x a -> a -> ByteString -> (Reporter Report -> IO ())
untrackedExecWith routing binary arg dat = \r -> do
    let p = prepare binary arg
    runReporter r (CommandStart p)
    (code, out, err) <- runRouted routing (\channel line -> runReporter r (CommandOutput p channel line)) p dat
    runReporter r (CommandStopped p code out err)
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> throwIO (CommandFailed p n out err)

{- | 'untrackedExec' for a caller that wants the command's standard output
back — a @git rev-parse@, a @dig +short@ — under the same rule about exit
codes: non-zero throws 'CommandFailed', so what is handed back is always
the output of a command that succeeded.
-}
untrackedExecOutput :: Command x a -> a -> ByteString -> Reporter Report -> IO ByteString
untrackedExecOutput binary arg dat r = do
    let p = prepare binary arg
    runReporter r (CommandStart p)
    (code, out, err) <- readCreateProcessWithExitCode p dat
    runReporter r (CommandStopped p code out err)
    case code of
        ExitSuccess -> pure out
        ExitFailure n -> throwIO (CommandFailed p n out err)

-------------------------------------------------------------------------------

{- | Where one of a command's two output streams goes.

The choice is the node author's, per command, and there is no global switch,
because the right answer depends on what the command prints: a build wants
to be followed while it runs, and a CLI that prints a token, or a @psql@
that echoes the statement it failed on, must never be.
-}
data Sink
    = {- | Held in memory, whole, until the command exits, and handed to the
      final report. What every command did before there was a choice, and
      still the default.
      -}
      Capture
    | {- | Reported line by line as the command writes it, /and/ kept for the
      final report — but only the last so many bytes of it (see 'tailOf'),
      so that a build printing megabytes does not sit in memory until it
      ends. Not for a command whose output can hold a secret: a streamed
      line is public the moment it is read.
      -}
      Stream !Int
    | -- | Sent to @\/dev\/null@. The final report carries nothing for it.
      Discard
    | {- | Appended to this file, which is created if missing. The final
      report carries nothing for it; the file is the caller's to name,
      rotate and protect.
      -}
      AppendTo !FilePath
    deriving (Eq, Ord, Show)

-- | A 'Sink' for each of the two streams.
data Routing = Routing
    { stdoutTo :: !Sink
    , stderrTo :: !Sink
    }
    deriving (Eq, Ord, Show)

-- | Both streams 'Capture'd: the default, and what 'withBinary' and 'untrackedExec' do.
captured :: Routing
captured = Routing Capture Capture

-- | Both streams 'Stream'ed, keeping 'defaultTailBytes' of each.
streamed :: Routing
streamed = streamedKeeping defaultTailBytes

-- | Both streams 'Stream'ed, keeping this many bytes of each for the final report.
streamedKeeping :: Int -> Routing
streamedKeeping n = Routing (Stream n) (Stream n)

-- | Both streams 'Discard'ed.
discarded :: Routing
discarded = Routing Discard Discard

-- | Both streams appended to one file, in the order the command wrote them.
appendedTo :: FilePath -> Routing
appendedTo path = Routing (AppendTo path) (AppendTo path)

-- | 64 KiB: what 'streamed' keeps of each stream.
defaultTailBytes :: Int
defaultTailBytes = 64 * 1024

{- | Run a process with its output routed, feeding it @dat@ on standard
input, and hand back its exit code with what each 'Sink' kept. The callback
is called once per line of a 'Stream'ed stream, from the thread reading that
stream, as the line is read; the two streams are read concurrently, so it
may be called from two threads at once.

'captured' is exactly @readCreateProcessWithExitCode@, as before. Anything
else is run here: both pipes are drained /while/ the process runs (a process
that fills a pipe nobody reads blocks forever), a process that does not read
its standard input is not an error, and an exception — the node's thread
being cancelled — terminates the process on the way out
('withCreateProcess'). As with the captured form, whatever the caller's
'CreateProcess' said about its three standard streams is overridden.
-}
runRouted :: Routing -> (Channel -> ByteString -> IO ()) -> CreateProcess -> ByteString -> IO (ExitCode, ByteString, ByteString)
runRouted routing onLine p dat
    | routing == captured = readCreateProcessWithExitCode p dat
    | otherwise =
        withStreams routing $ \outStream errStream ->
            withCreateProcess p{std_in = CreatePipe, std_out = outStream, std_err = errStream} $ \mIn mOut mErr ph -> do
                (out, (err, ())) <-
                    concurrently
                        (drain routing.stdoutTo Stdout mOut)
                        (concurrently (drain routing.stderrTo Stderr mErr) (feed mIn))
                code <- waitForProcess ph
                pure (code, out, err)
  where
    -- a process that exits, or closes its standard input, without reading
    -- all of it is not a failure of the command: its exit code is
    feed :: Maybe Handle -> IO ()
    feed Nothing = pure ()
    feed (Just h) = do
        _ <- try (ByteString.hPut h dat >> hClose h) :: IO (Either IOException ())
        _ <- try (hClose h) :: IO (Either IOException ())
        pure ()

    drain :: Sink -> Channel -> Maybe Handle -> IO ByteString
    drain _ _ Nothing = pure ""
    drain sink channel (Just h) = case sink of
        Stream keep -> streamLines keep channel h
        _ -> ByteString.hGetContents h

    streamLines :: Int -> Channel -> Handle -> IO ByteString
    streamLines keep channel h = go emptyTail
      where
        go acc = do
            eof <- hIsEOF h
            if eof
                then pure (renderTail acc)
                else do
                    line <- C8.hGetLine h
                    onLine channel line
                    go (pushTail keep line acc)

-- | The 'StdStream' each sink asks of the process, with whatever file it
-- needs open for as long as the process runs.
withStreams :: Routing -> (StdStream -> StdStream -> IO a) -> IO a
withStreams routing k =
    case (routing.stdoutTo, routing.stderrTo) of
        -- one file, one handle: two handles on one path would be refused by
        -- the runtime's own file locking, and would interleave badly anyway
        (AppendTo a, AppendTo b)
            | a == b -> IO.withFile a AppendMode $ \h -> k (UseHandle h) (UseHandle h)
        (o, e) -> withStream o $ \outStream -> withStream e $ \errStream -> k outStream errStream
  where
    withStream :: Sink -> (StdStream -> IO a) -> IO a
    withStream sink f = case sink of
        Capture -> f CreatePipe
        Stream _ -> f CreatePipe
        Discard -> IO.withFile "/dev/null" WriteMode (f . UseHandle)
        AppendTo path -> IO.withFile path AppendMode (f . UseHandle)

-- | The last lines of a stream, and how many bytes they are.
data Tail = Tail !Int !(Seq ByteString)

emptyTail :: Tail
emptyTail = Tail 0 Seq.empty

{- | Add a line, then forget lines from the front until what is kept fits in
@keep@ bytes (each line counted with its newline). The newest line is never
forgotten whole: one longer than @keep@ is cut down to its last bytes.
-}
pushTail :: Int -> ByteString -> Tail -> Tail
pushTail keep line (Tail size ls) = trim (Tail (size + cost line) (ls |> line))
  where
    cost l = ByteString.length l + 1
    trim t@(Tail n kept)
        | n <= keep = t
        | otherwise = case Seq.viewl kept of
            EmptyL -> t
            l :< rest
                | Seq.null rest ->
                    let cut = ByteString.drop (ByteString.length l - max 0 (keep - 1)) l
                     in Tail (cost cut) (Seq.singleton cut)
                | otherwise -> trim (Tail (n - cost l) rest)

renderTail :: Tail -> ByteString
renderTail (Tail _ ls) = C8.unlines (toList ls)

{- | What a 'Stream' sink keeping @keep@ bytes hands to the final report for
a stream made of these lines: the longest run of whole trailing lines that
fits, each ended by a newline. Pure, and what 'runRouted' does line by line.
-}
tailOf :: Int -> [ByteString] -> ByteString
tailOf keep = renderTail . foldl (flip (pushTail keep)) emptyTail

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
