{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A process salmon owns and keeps running.

Every other builtin here is a one-shot idempotent action: @up@ runs to
completion and returns, and what it left behind — a file, a database role, a
route — stays put on its own. A long-running process does not. Nothing keeps
it alive but something watching it, which is why
"Salmon.Builtin.Nodes.Systemd" hands the whole problem to systemd rather
than solving it.

This is for where there is no systemd to hand it to: a container, a test
harness, or the supervisor of @specs\/salmon-as-init.md@. It fills in
'Salmon.Builtin.Extension.managed', so the node's own thread is in scope for
the process's entire lifetime and the handle never has to escape — which is
what makes ownership possible at all, and what @up :: IO ()@ (which ran and
returned into nothing) could not offer.

= Three things ownership buys over polling a @check@

Supervising an /unowned/ effect — a systemd unit, a container, a service on
another host — is a @check@ and a re-@up@, and "Salmon.Actions.Upkeep"
already does it. For a process salmon started itself that is a poor answer:

* __the exit status.__ A @check@ answers alive-or-dead; waiting on the
  process answers @ExitFailure 137@, which is the difference between
  restarting a service and respecting its decision to stop;
* __timeliness.__ The upkeep delay backs /off/ on success, to a minute. A
  service that dies a second after a successful check would stay dead for
  that minute;
* __identity.__ A pidfile plus @kill -0@ cannot survive pid reuse, and cannot
  tell a live process from a zombie. Here the pid is a local variable on the
  owning thread's stack, which is why no pid table appears anywhere in this
  design.

= Stopping is an escalation, not a @cancel@

Teardown is cancelling the node's thread, and the bracket in 'runDaemon' is
what does the killing — but a @cancel@ on its own is not a stop.
'System.Process.withCreateProcess' sends @SIGTERM@ and waits, and a service
that ignores @SIGTERM@ then wedges the teardown behind it. So: signal the
process __group__, wait 'stop_grace', then @SIGKILL@ and wait again. The
group matters as much as the escalation — a service that forks workers has to
take them with it, which is why 'runDaemon' forces @create_group@ on
regardless of what the caller's 'CreateProcess' said.

This is recovered rather than invented: it is what the removed
@Salmon.Builtin.Nodes.Supervised@ did, at @f9d7116@.

= Under a one-shot driver, this node fails on purpose

@run up@\/@run down@ call 'Salmon.Builtin.Extension.up', and a synchronous
one-pass driver has nowhere to put an action that never returns. So 'daemon'
throws 'NeedsSupervisor' from @up@ rather than no-oping: a node that cannot
be brought up by this driver should say so loudly, per CLAUDE.md's "failure
must not be swallowed". @run serve@ (which routes managed nodes to
"Salmon.Actions.Upkeep" and never calls their @up@) is the driver that can
hold one.

@down@ is the other way round and it is not an inconsistency: under @serve@
the process died when the machine holding it was cancelled, and under
@run down@ this process never held one — so there is genuinely nothing here
to stop, and @pure ()@ is the true answer rather than a swallowed failure.
The gap it leaves is a process left behind by a @serve@ that has since
exited, which nothing in v1 can recover: recovering it needs the pidfile
convention @specs\/per-node-state-machines.md@ lists under non-goals.
-}
module Salmon.Builtin.Nodes.Daemon (
    -- * The node
    Daemon (..),
    defaultDaemon,
    daemon,
    daemonRef,

    -- * How it stops
    Stop (..),
    defaultStop,

    -- * The action, for building your own node on
    runDaemon,

    -- * Observing
    Report (..),
    NeedsSupervisor (..),
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (Exception, SomeException, bracket, throwIO, try)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (ExitCode (..))
import System.IO (Handle, hIsEOF)
import qualified System.IO as IO
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcessGroup)
import System.Process (CreateProcess (..), Pid, ProcessHandle, StdStream (..), createProcess, getPid, getProcessExitCode, waitForProcess)

import Salmon.Builtin.Extension
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Supervision (Micros (..), millis, seconds)
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | How to stop a process that will not stop on its own.
data Stop = Stop
    { stop_signal :: !Signal
    -- ^ sent to the process /group/ first, politely.
    , stop_grace :: !Micros
    -- ^ how long to let it go on its own before @SIGKILL@.
    }

-- | @SIGTERM@, then five seconds, then @SIGKILL@.
defaultStop :: Stop
defaultStop = Stop sigTERM (seconds 5)

data Daemon = Daemon
    { daemon_name :: !Text
    -- ^ names it in reports, and is its 'Ref' — so it is the identity of the
    -- /effect site/, not of the command line. Two declarations giving one
    -- name two different commands are one node, and the magma's
    -- last-writer-wins picks between them; see @Salmon.Op.Dag@.
    , daemon_process :: !CreateProcess
    -- ^ @create_group@ is forced on when this is spawned, whatever it says.
    , daemon_stop :: !Stop
    , daemon_capture :: !Bool
    -- ^ pipe stdout and stderr into the node's own bounded ring, a line at a
    -- time. Worth having on: it is what an operator reads when the node has
    -- failed, and it is what tells a watchdog that a slow node is making
    -- progress rather than wedged. Turn it off for a process whose output
    -- should go where it would have gone anyway (a container's stdout, say),
    -- since capturing it here means it no longer reaches the parent's.
    }

-- | 'daemon_capture' on, 'defaultStop'.
defaultDaemon :: Text -> CreateProcess -> Daemon
defaultDaemon name cp = Daemon name cp defaultStop True

daemonRef :: Daemon -> Ref
daemonRef d = mkRef "daemon" d.daemon_name

data Report
    = Spawned !Text !(Maybe Pid)
    | -- | one line the process wrote (only with 'daemon_capture')
      Wrote !Text !Text
    | Exited !Text !ExitCode
    | -- | asked it to stop
      Signalling !Text !Signal
    | -- | it did not go within 'stop_grace'
      Killing !Text
    | Reaped !Text
    deriving (Show)

{- | Thrown by 'daemon''s @up@: this node cannot be brought up by a driver
that cannot hold a running action.
-}
newtype NeedsSupervisor = NeedsSupervisor Text

instance Show NeedsSupervisor where
    show (NeedsSupervisor name) =
        mconcat
            [ Text.unpack name
            , " is a process salmon owns, so it can only be brought up by a driver that can"
            , " hold it running (`run serve`). A one-shot `run up` has nowhere to put it."
            ]

instance Exception NeedsSupervisor

-------------------------------------------------------------------------------

daemon :: Reporter Report -> Daemon -> Op
daemon r d =
    op "daemon" nodeps $ \actions ->
        actions
            { help = "keeps " <> d.daemon_name <> " running"
            , ref = daemonRef d
            , managed = Just (runDaemon r d)
            , -- see the module header: loud rather than a silent no-op.
              up = throwIO (NeedsSupervisor d.daemon_name)
            , -- ...and, equally deliberately, not loud. There is nothing for
              -- a one-shot teardown to stop.
              down = pure ()
            }

{- | Spawn the process and block until it exits, tearing it down through the
bracket if this thread is cancelled.

Exposed because a node that wants more than 'daemon' offers — a @check@ of
its own, dependencies, a richer 'Ref' key — should build its own 'Op' around
this rather than reimplement the escalation:

@
op "webserver" (deps [config]) $ \\actions ->
    actions
        { ref = mkRef "webserver" name
        , managed = Just (runDaemon reportPrint d)
        , check = probeHttp url
        , up = throwIO (NeedsSupervisor name)
        }
@
-}
runDaemon :: Reporter Report -> Daemon -> Output -> IO ExitCode
runDaemon r d out =
    bracket spawn teardown wait
  where
    name = d.daemon_name

    cp :: CreateProcess
    cp =
        d.daemon_process
            { -- so the whole group goes: a service that forks workers must
              -- take them with it.
              create_group = True
            , std_out = if d.daemon_capture then CreatePipe else std_out d.daemon_process
            , std_err = if d.daemon_capture then CreatePipe else std_err d.daemon_process
            }

    spawn :: IO (Maybe Handle, Maybe Handle, ProcessHandle, Maybe Pid)
    spawn = do
        (_, mout, merr, ph) <- createProcess cp
        pid <- getPid ph
        runReporter r (Spawned name pid)
        pure (mout, merr, ph, pid)

    {- | Reading the pipes has to happen /while/ waiting, not after, or a
    process that fills a pipe buffer blocks forever and the node looks
    wedged for a reason nobody could see. 'withAsync' also means the readers
    go when the bracket does. -}
    wait :: (Maybe Handle, Maybe Handle, ProcessHandle, Maybe Pid) -> IO ExitCode
    wait (mout, merr, ph, _) =
        drain mout $
            drain merr $ do
                code <- waitForProcess ph
                runReporter r (Exited name code)
                pure code

    drain :: Maybe Handle -> IO a -> IO a
    drain Nothing k = k
    drain (Just h) k = withAsync (pump h) (const k)

    pump :: Handle -> IO ()
    pump h = do
        -- a process writing invalid UTF-8, or a handle closed under us, must
        -- not take the node down with it.
        _ <- try @SomeException go
        pure ()
      where
        go = do
            IO.hSetBuffering h IO.LineBuffering
            loop
        loop = do
            eof <- hIsEOF h
            if eof
                then pure ()
                else do
                    line <- Text.pack <$> IO.hGetLine h
                    out line
                    runReporter r (Wrote name line)
                    loop

    {- | @SIGTERM@ the group, wait, then @SIGKILL@ it and wait again.

    Signalling the group by pid rather than closing the handle is what makes
    the escalation possible at all: 'System.Process.terminateProcess' signals
    only the leader, and 'System.Process.withCreateProcess'\'s own cleanup
    waits indefinitely for a process that has decided to ignore it. -}
    teardown :: (Maybe Handle, Maybe Handle, ProcessHandle, Maybe Pid) -> IO ()
    teardown (_, _, ph, mpid) = do
        alive <- getProcessExitCode ph
        case (alive, mpid) of
            -- it exited on its own; nothing to stop, and `wait` has the code.
            (Just _, _) -> pure ()
            (Nothing, Nothing) -> pure ()
            (Nothing, Just pid) -> do
                runReporter r (Signalling name d.daemon_stop.stop_signal)
                signal d.daemon_stop.stop_signal pid
                gone <- waitGone ph d.daemon_stop.stop_grace
                if gone
                    then runReporter r (Reaped name)
                    else do
                        runReporter r (Killing name)
                        signal sigKILL pid
                        void (waitGone ph d.daemon_stop.stop_grace)
                        runReporter r (Reaped name)

    -- signalling a group that has already gone is not an error worth
    -- propagating out of a teardown.
    signal :: Signal -> Pid -> IO ()
    signal sig pid = void (try @SomeException (signalProcessGroup sig pid))

{- | Poll for the process to be gone, up to a deadline.

Polling rather than a second 'waitForProcess': this runs from a @bracket@
release while an async exception is in flight, and the thread that /was/
waiting on the handle has just been interrupted. Polling asks nothing of
whether two waits on one handle compose.
-}
waitGone :: ProcessHandle -> Micros -> IO Bool
waitGone ph grace = do
    deadline <- (+ toNanos grace) <$> getMonotonicTimeNSec
    go deadline
  where
    toNanos (Micros n) = fromIntegral n * 1000
    go deadline = do
        code <- getProcessExitCode ph
        case code of
            Just _ -> pure True
            Nothing -> do
                now <- getMonotonicTimeNSec
                if now >= deadline
                    then pure False
                    else threadDelay (unMicros (millis 20)) >> go deadline
