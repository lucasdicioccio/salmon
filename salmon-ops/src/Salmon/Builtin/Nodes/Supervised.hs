{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Keeping a process running, rather than making a state of the world so.

Every other builtin here is a one-shot idempotent action: @up@ runs to
completion and returns, and what it left behind (a file, a database role, a
route) stays put on its own. A long-running process does not: nothing keeps
it alive but something watching it, which is why
"Salmon.Builtin.Nodes.Systemd" delegates the whole problem to systemd rather
than solving it.

This module solves it in-process, for where there is no systemd to delegate
to — a container, a test harness, or the PID-2 supervisor of
@specs\/salmon-as-init.md@. The shape is deliberately not a new execution
model, just the existing one pointed at a different question:

  * 'prelim' answers @Skippable@ while the process is alive (nothing to do)
    and @Required@ once it is not, so "restart it" is an ordinary bring-up;
  * @up@ spawns and records the pid;
  * @down@ signals the process group and waits for it to go;
  * backoff needs no new control flow either — @Skippable@ /already/ means
    "not now", so a service inside its backoff window reports itself as
    nothing-to-do until the window passes.

== Where the state lives

The one thing that cannot follow that pattern is /where the pid is kept/.
'Salmon.Builtin.Extension.up' is an @IO ()@ closed over when the graph is
built, and "Salmon.Actions.Serve" re-evaluates the whole graph on every
declaration — so a cell created during graph construction is a different cell
on the next pass, and the running process would be forgotten. The pid must
live in something that outlives graph evaluation: a 'Supervisor', created
once by the application and closed over by its @Track'@.

> main = do
>     sup <- newSupervisor
>     cmd <- getRecord "my-salmon"
>     execCommandOrSeed reportPrint configure (program sup) cmd
>
> program :: Supervisor -> Track' Spec
> program sup = Track $ \spec ->
>     op "site" (deps [service reportPrint sup (webserver spec)]) $ \actions -> ...

This is the same "take the resource as a parameter" convention the rest of
the repo uses for anything a recipe must not invent for itself; an
application with no services to supervise never makes one.

== Reaping, and why it polls

A 'Supervisor' runs one thread that calls 'getProcessExitCode' over the
processes it knows about, which both notices a death and reaps the zombie.
Polling rather than @SIGCHLD@ (or a @waitForProcess@ thread per child) is
deliberate: it needs no signal handler, it does not assume the threaded RTS,
and — the reason that actually matters — it never calls @waitpid(-1)@, which
would race with the "Salmon.Builtin.Nodes.Binary" commands every other node
runs and could steal their exit statuses. That race is the whole argument for
the two-process split in @specs\/salmon-as-init.md@; there is no reason to
import it into a library that can sidestep it.

The same thread publishes two kinds of wakeup on 'supervisorWakeups', which
"Salmon.Actions.Serve" selects on alongside its command input: a service
/died/, and a service's backoff window /expired/. The second is not optional.
A service inside its backoff answers @Skippable@, which a convergence pass
records as converged — so without a wakeup when the window passes, nothing
would ever come back to restart it and the backoff would be a permanent stop.

An exit the supervisor /asked for/ is not published: 'serviceDown' has
already waited for it, and waking the loop over it would put a node that is
still wanted up straight back into a restart — a teardown that resurrects
what it just stopped.

Build an application that drives 'Salmon.Actions.Serve.serveWith' with
@-threaded@. The reaper here needs nothing special, but that loop waits on
its input handle and on wakeups at the same time, and without the threaded
runtime a blocking read stalls every other Haskell thread, this one included.

Leaving a @serve@ loop does not stop what it supervised: children are spawned
in their own process group and outlive the parent, matching @serve@'s
existing promise to leave the machine as the last convergence left it. Call
'stopAll' to do otherwise.
-}
module Salmon.Builtin.Nodes.Supervised (
    -- * The supervisor
    Supervisor,
    newSupervisor,
    supervisorWakeups,
    supervisorStatus,
    stopAll,

    -- * Declaring a service
    Service (..),
    defaultService,
    serviceRef,
    service,

    -- * Restart policy
    Policy (..),
    defaultPolicy,
    neverGiveUp,

    -- * Observing
    Status (..),
    Report (..),
    ServiceGaveUp (..),
) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (forever, unless, void)
import Data.Dynamic (toDyn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (ExitCode (..))
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcessGroup)
import System.Process (CmdSpec (..), CreateProcess (..), Pid, ProcessHandle, createProcess, getPid, getProcessExitCode)

import Salmon.Actions.UpDown (Requirement (..))
import Salmon.Builtin.Extension
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Reporter

-------------------------------------------------------------------------------

{- | How hard to try, and when to stop trying.

Two fields are worth reading twice. 'policy_stableAfter' is what stops a
service that crashes once a day from eventually being treated as a crash
loop: having run that long resets the escalation, so only /consecutive quick/
failures count. 'policy_giveUpAfter' is right for a service and wrong for
anything the machine cannot come back without — see 'neverGiveUp'.
-}
data Policy = Policy
    { policy_initialDelay :: !Double
    -- ^ seconds to wait before the first restart
    , policy_multiplier :: !Double
    -- ^ how much the delay grows after each consecutive failure
    , policy_maxDelay :: !Double
    -- ^ ceiling on that growth, in seconds
    , policy_stableAfter :: !Double
    -- ^ having run this long counts as working: the delay and the failure
    -- count both go back to their initial values
    , policy_giveUpAfter :: !(Maybe Int)
    -- ^ stop restarting after this many consecutive failures; 'Nothing'
    -- never gives up
    }
    deriving (Show, Eq)

defaultPolicy :: Policy
defaultPolicy =
    Policy
        { policy_initialDelay = 0.1
        , policy_multiplier = 2.0
        , policy_maxDelay = 30
        , policy_stableAfter = 10
        , policy_giveUpAfter = Just 10
        }

{- | Backs off, but never latches off. Right for a process the system has no
way back without — in @specs\/salmon-as-init.md@ terms, the supervisor slot
rather than a service.
-}
neverGiveUp :: Policy
neverGiveUp = defaultPolicy{policy_giveUpAfter = Nothing}

-------------------------------------------------------------------------------

-- | What to run, and how to stop it.
data Service = Service
    { service_name :: !Text
    -- ^ names the service in reports, and is half of its 'Ref'
    , service_process :: CreateProcess
    -- ^ @create_group@ is forced on when spawning, so 'down' can signal the
    -- whole group and a service that forks children takes them with it
    , service_stopSignal :: !Signal
    , service_stopGrace :: !Double
    -- ^ seconds to wait after 'service_stopSignal' before @SIGKILL@
    , service_policy :: !Policy
    }

defaultService :: Text -> CreateProcess -> Service
defaultService name cp =
    Service
        { service_name = name
        , service_process = cp
        , service_stopSignal = sigTERM
        , service_stopGrace = 5
        , service_policy = defaultPolicy
        }

{- | The identity a service converges under. Includes the command and not
just the name, so that editing what a service runs is a different node: the
old one goes down and the new one comes up, rather than the change being
skipped because something of that name happens to be alive.
-}
serviceRef :: Service -> Ref
serviceRef svc =
    mkRef "supervised-service" (svc.service_name, renderCmdSpec (cmdspec svc.service_process))

renderCmdSpec :: CmdSpec -> Text
renderCmdSpec (ShellCommand s) = Text.pack s
renderCmdSpec (RawCommand p args) = Text.unwords (fmap Text.pack (p : args))

-------------------------------------------------------------------------------

data Report
    = Spawning !Text
    | Spawned !Text !(Maybe Pid)
    | SpawnFailed !Text !Text
    | -- | name, status, whether we asked for the exit
      Exited !Text !ExitCode !Bool
    | -- | name, seconds until the next attempt, consecutive failures
      BackingOff !Text !Double !Int
    | -- | name, consecutive failures. No further restarts until it is
      -- declared down and up again.
      GaveUp !Text !Int
    | Stopping !Text !Signal
    | -- | the grace period elapsed, so it was killed
      Killed !Text
    | Stopped !Text
    deriving (Show)

{- | Thrown by @up@ for a service whose 'policy_giveUpAfter' has tripped.

Throwing rather than skipping is the point: a skip would be recorded as
converged, and @status@ would show a crash-looped service as healthy. Failing
leaves it visibly 'Salmon.Actions.Serve.Errored' while still not spawning
anything, which is what "gave up" should look like from the outside.
-}
data ServiceGaveUp = ServiceGaveUp !Text !Int
    deriving (Show)

instance Exception ServiceGaveUp

-------------------------------------------------------------------------------

-- | A live process, and when it started.
data Running = Running
    { run_handle :: ProcessHandle
    , run_pid :: !(Maybe Pid)
    , run_started :: !Word64
    -- ^ monotonic nanoseconds
    , run_stopping :: !Bool
    -- ^ 'down' asked for this exit, so it does not count as a failure
    }

-- | Everything the supervisor remembers about one service, keyed by 'Ref'.
data ServiceState = ServiceState
    { svc_service :: Service
    , svc_running :: !(Maybe Running)
    , svc_failures :: !Int
    , svc_delay :: !Double
    -- ^ the delay to apply after the /next/ consecutive failure
    , svc_nextEligible :: !Word64
    -- ^ monotonic nanoseconds before which 'prelim' answers @Skippable@
    , svc_announced :: !Bool
    -- ^ whether the reaper has already published the wakeup for this
    -- backoff window expiring, so it publishes it exactly once
    , svc_gaveUp :: !Bool
    , svc_lastExit :: !(Maybe ExitCode)
    }

-- | A 'ServiceState' with the un-'Show'able parts resolved, for reporting.
data Status = Status
    { status_name :: !Text
    , status_pid :: !(Maybe Pid)
    , status_failures :: !Int
    , status_gaveUp :: !Bool
    , status_lastExit :: !(Maybe ExitCode)
    }
    deriving (Show, Eq)

initialState :: Service -> ServiceState
initialState svc =
    ServiceState
        { svc_service = svc
        , svc_running = Nothing
        , svc_failures = 0
        , svc_delay = svc.service_policy.policy_initialDelay
        , svc_nextEligible = 0
        , svc_announced = True
        , svc_gaveUp = False
        , svc_lastExit = Nothing
        }

-------------------------------------------------------------------------------

{- | Owns the running processes and the per-service restart bookkeeping.
Create one per application, not one per graph evaluation — see the module
header for why that distinction is the whole point.
-}
data Supervisor = Supervisor
    { sup_states :: MVar (Map Ref ServiceState)
    , sup_wakeups :: TVar (Set Ref)
    , sup_reaper :: ThreadId
    }

-- | How often the reaper looks for exits and expired backoff windows.
reapInterval :: Int
reapInterval = 50000 -- microseconds

newSupervisor :: IO Supervisor
newSupervisor = do
    states <- newMVar Map.empty
    wakeups <- newTVarIO Set.empty
    reaper <- forkIO $ forever $ do
        threadDelay reapInterval
        tick states wakeups
    pure (Supervisor states wakeups reaper)

{- | Blocks until at least one supervised service wants attention — it died,
or its backoff window has passed — and answers which. Draining rather than
queueing means a burst is one wakeup rather than several redundant
convergences.

Pass to 'Salmon.Actions.Serve.serveWith' to have the loop re-converge on its
own when a service goes.
-}
supervisorWakeups :: Supervisor -> STM (Set Ref)
supervisorWakeups sup = do
    refs <- readTVar sup.sup_wakeups
    if Set.null refs
        then retry
        else do
            writeTVar sup.sup_wakeups Set.empty
            pure refs

supervisorStatus :: Supervisor -> IO [(Ref, Status)]
supervisorStatus sup = do
    states <- readMVar sup.sup_states
    pure [(rf, toStatus st) | (rf, st) <- Map.toList states]
  where
    -- annotated because a record-dot binding without a signature
    -- generalizes over 'HasField' and would need FlexibleContexts.
    toStatus :: ServiceState -> Status
    toStatus st =
        Status
            { status_name = st.svc_service.service_name
            , status_pid = st.svc_running >>= run_pid
            , status_failures = st.svc_failures
            , status_gaveUp = st.svc_gaveUp
            , status_lastExit = st.svc_lastExit
            }

-------------------------------------------------------------------------------

{- | One node: "this process is running". Bring it up to spawn, tear it down
to stop it, and leave it alone in between — a convergence pass over a healthy
service does nothing at all.
-}
service :: Reporter Report -> Supervisor -> Service -> Op
service r sup svc =
    op "supervised-service" nodeps $ \actions ->
        actions
            { help = "keeps " <> svc.service_name <> " running"
            , notes = ["command: " <> renderCmdSpec (cmdspec svc.service_process)]
            , ref = serviceRef svc
            , prelim = servicePrelim sup (serviceRef svc)
            , up = serviceUp r sup svc
            , down = serviceDown r sup svc
            , dynamics = [toDyn svc]
            }

{- | @Skippable@ here covers two genuinely different "nothing to do now"s:
the process is already running, or it is inside its backoff window. The
second is why the reaper has to publish a wakeup when that window expires —
a skip is recorded as converged, so nothing else would ever come back for it.

A service that has been given up on deliberately answers @Required@ instead,
so that @up@ runs and throws; see 'ServiceGaveUp'.
-}
servicePrelim :: Supervisor -> Ref -> IO Requirement
servicePrelim sup rf = do
    now <- getMonotonicTimeNSec
    states <- readMVar sup.sup_states
    pure $ case Map.lookup rf states of
        Nothing -> Required
        Just st
            | isJust st.svc_running -> Skippable
            | st.svc_gaveUp -> Required
            | now < st.svc_nextEligible -> Skippable
            | otherwise -> Required

serviceUp :: Reporter Report -> Supervisor -> Service -> IO ()
serviceUp r sup svc = do
    outcome <- modifyMVar sup.sup_states $ \states -> do
        let st = Map.findWithDefault (initialState svc) rf states
        case () of
            _
                | st.svc_gaveUp -> pure (states, GaveUpAlready st.svc_failures)
                -- 'prelim' normally catches this; a caller that skips prelim,
                -- or a race with the reaper, must not spawn a second copy.
                | isJust st.svc_running -> pure (states, AlreadyUp)
                | otherwise -> do
                    spawned <- try (createProcess (svc.service_process{create_group = True}))
                    case spawned of
                        Left (e :: SomeException) ->
                            pure (states, CouldNotSpawn (Text.pack (show e)))
                        Right (_, _, _, ph) -> do
                            now <- getMonotonicTimeNSec
                            pid <- getPid ph
                            let running = Running ph pid now False
                            pure (Map.insert rf st{svc_running = Just running} states, DidSpawn pid)
    case outcome of
        AlreadyUp -> pure ()
        DidSpawn pid -> runReporter r (Spawned svc.service_name pid)
        GaveUpAlready n -> do
            runReporter r (GaveUp svc.service_name n)
            throwIO (ServiceGaveUp svc.service_name n)
        CouldNotSpawn err -> do
            runReporter r (SpawnFailed svc.service_name err)
            throwIO (ServiceGaveUp svc.service_name 0)
  where
    rf = serviceRef svc

data SpawnOutcome
    = AlreadyUp
    | DidSpawn !(Maybe Pid)
    | GaveUpAlready !Int
    | CouldNotSpawn !Text

{- | Signals the group, gives it 'service_stopGrace' to go, then @SIGKILL@s
it. Also clears the failure count and the give-up latch: an explicit teardown
is where a crash-looping service is forgiven, so declaring it up again starts
from a clean slate rather than being refused.
-}
serviceDown :: Reporter Report -> Supervisor -> Service -> IO ()
serviceDown r sup svc = do
    -- flag it stopping before signalling, so whichever of us notices the
    -- exit does not count it as a failure or schedule a restart.
    mrunning <- modifyMVar sup.sup_states $ \states ->
        case Map.lookup rf states >>= svc_running of
            Nothing -> pure (states, Nothing)
            Just running ->
                pure
                    ( Map.adjust (\st -> st{svc_running = Just running{run_stopping = True}}) rf states
                    , Just running
                    )
    case mrunning of
        Nothing -> pure ()
        Just running -> do
            runReporter r (Stopping svc.service_name svc.service_stopSignal)
            signalGroup svc.service_stopSignal running
            gone <- waitGone running svc.service_stopGrace
            unless gone $ do
                runReporter r (Killed svc.service_name)
                signalGroup sigKILL running
                void (waitGone running svc.service_stopGrace)
            runReporter r (Stopped svc.service_name)
    -- running or not, forget the failure history.
    modifyMVar_ sup.sup_states $ \states ->
        pure $ Map.alter (Just . forget) rf states
  where
    rf = serviceRef svc
    forget mst = (initialState svc){svc_lastExit = mst >>= svc_lastExit}

{- | Signals the process /group/, so a service that forked children takes
them with it. A group that has already gone is the normal race with the
reaper rather than an error, so the failure is swallowed.
-}
signalGroup :: Signal -> Running -> IO ()
signalGroup sig running =
    case running.run_pid of
        Nothing -> pure ()
        Just pid -> void (try (signalProcessGroup sig pid) :: IO (Either SomeException ()))

-- | Polls for the process to exit, up to a deadline in seconds.
waitGone :: Running -> Double -> IO Bool
waitGone running grace = do
    deadline <- (+ nanos grace) <$> getMonotonicTimeNSec
    go deadline
  where
    go deadline = do
        code <- getProcessExitCode running.run_handle
        case code of
            Just _ -> pure True
            Nothing -> do
                now <- getMonotonicTimeNSec
                if now >= deadline
                    then pure False
                    else threadDelay reapInterval >> go deadline

-------------------------------------------------------------------------------

{- | One pass of the reaper: notice exits, then notice backoff windows that
have passed. Both publish a wakeup, and both have to: the first says "it
died", the second says "you may try again now", and a supervisor that only
published the first would stop restarting anything the moment a backoff was
scheduled.
-}
tick :: MVar (Map Ref ServiceState) -> TVar (Set Ref) -> IO ()
tick states wakeups = do
    now <- getMonotonicTimeNSec
    woken <- modifyMVar states $ \m -> do
        reaped <- traverse (checkExit now) (Map.toList m)
        let (m', due) = foldr (eligible now) (Map.empty, Set.empty) reaped
        pure (m', due)
    unless (Set.null woken) $
        atomically (modifyTVar' wakeups (Set.union woken))
  where
    -- an exit we asked for is deliberately /not/ published: 'serviceDown'
    -- already waited for it, and waking the loop over it would put a node
    -- that is still wanted up straight back into a restart — turning
    -- 'stopAll', or any teardown of a node another seed still wants,
    -- into a resurrection.
    checkExit now (rf, st) =
        case st.svc_running of
            Nothing -> pure (rf, st, False)
            Just running -> do
                code <- getProcessExitCode running.run_handle
                case code of
                    Nothing -> pure (rf, st, False)
                    Just c -> pure (rf, onExit now running c st, not running.run_stopping)

    -- a service that is not running, has not been given up on, and whose
    -- window has passed is announced exactly once.
    eligible now (rf, st, publish) (m, due)
        | publish = (Map.insert rf st m, Set.insert rf due)
        | not st.svc_announced && not st.svc_gaveUp && now >= st.svc_nextEligible =
            (Map.insert rf st{svc_announced = True} m, Set.insert rf due)
        | otherwise = (Map.insert rf st m, due)

{- | The restart policy, as a pure state transition.

An exit we asked for resets everything. Otherwise a process that stayed up
for 'policy_stableAfter' is treated as having worked — it is a service that
crashed, not a service that cannot start — so both the delay and the failure
count reset; only consecutive quick failures escalate, and only they can trip
'policy_giveUpAfter'.

Note this decides /when/ a restart becomes eligible but never performs one:
it records 'svc_nextEligible' and stops. The restart itself is an ordinary
bring-up gated by 'servicePrelim', which is what keeps backoff out of the
control flow entirely.
-}
onExit :: Word64 -> Running -> ExitCode -> ServiceState -> ServiceState
onExit now running code st
    | running.run_stopping =
        st{svc_running = Nothing, svc_lastExit = Just code}
    | otherwise =
        st
            { svc_running = Nothing
            , svc_lastExit = Just code
            , svc_failures = failures
            , svc_delay = nextDelay
            , svc_nextEligible = now + nanos thisDelay
            , svc_announced = False
            , svc_gaveUp = gaveUp
            }
  where
    policy = st.svc_service.service_policy
    ranFor = fromIntegral (now - running.run_started) / 1.0e9 :: Double
    stable = ranFor >= policy.policy_stableAfter

    failures = if stable then 1 else st.svc_failures + 1
    thisDelay = if stable then policy.policy_initialDelay else st.svc_delay
    nextDelay =
        if stable
            then policy.policy_initialDelay * policy.policy_multiplier
            else min policy.policy_maxDelay (st.svc_delay * policy.policy_multiplier)
    gaveUp = maybe False (failures >=) policy.policy_giveUpAfter

nanos :: Double -> Word64
nanos s = round (max 0 s * 1.0e9)

-------------------------------------------------------------------------------

{- | Tear every supervised service down. Intended for an application shutting
down deliberately; a @serve@ loop that simply ends leaves its services
running.
-}
stopAll :: Reporter Report -> Supervisor -> IO ()
stopAll r sup = do
    states <- readMVar sup.sup_states
    mapM_ (serviceDown r sup . svc_service) (Map.elems states)
