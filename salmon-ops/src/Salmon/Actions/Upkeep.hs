{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The continuous driver: a node is not applied once, it is /tended/.

"Salmon.Actions.UpDown" and "Salmon.Actions.Concurrent" both make one pass —
one attempt per node, then the walk returns @IO Bool@ and everything it
started is over. This one does not return. Each node runs a small state
machine that keeps asking whether its effect is still in place and puts it
back when it is not, which is the whole of "keep this running" and the reason
@Salmon.Builtin.Nodes.Supervised@ was deleted rather than ported: supervision
is not a kind of node, it is what every node gets.

= Two machines, three states each

@
'UpkeepState'   = 'WaitUp'   | 'Upping'  | 'Up'
'DownkeepState' = 'WaitDown' | 'Downing' | 'Down'
@

A node wanted up runs the first, a node wanted down runs the second, and
which neighbours it waits on is the only difference in their ordering:
dependencies going up, dependants coming down, exactly as in the one-shot
drivers. 'Salmon.Op.Status.waitStability' does the blocking, so there is
still no scheduler and no ready-queue.

The asymmetry between them is not an oversight: __'Down' is terminal and
'Up' is not.__ A node's 'Salmon.Builtin.Extension.check' answers "does my
effect need creating", which is a question about being up; nothing in the
model answers "is it still gone". So a downkeep machine reaching 'Down' has
finished and exits, while an upkeep machine reaching 'Up' has only started.

= The steady state is a check on an adaptive delay

@
'Up': wait 'Delay'; then 'Salmon.Actions.UpDown.runCheck':
    the effect is there   -> stay 'Up', 'relaxed'   (double, capped at 60s)
    the effect is gone    -> go 'Upping', 'attentive' (halve, floored at 500ms)
@

Backing off while healthy and tightening while not is what makes this cost
nothing in the common case and react quickly in the uncommon one. It is also
why a check is allowed to be expensive: the adaptive value is a delay
/between/ checks rather than a period, so a slow check reduces its own
frequency and the load is self-limiting.

Two refinements this module makes to that rule, both places where the
one-shot reading does not survive contact with a loop:

* __'Salmon.Actions.UpDown.Unknown' does not restart anything.__
  'Salmon.Actions.UpDown.requirement' maps it to
  'Salmon.Actions.UpDown.Required', which is right for one pass over an
  idempotent action and wrong here: a node with no @check@ of its own answers
  'Salmon.Actions.UpDown.Unknown' forever, and "run @up@ again" would spin it
  at the delay floor for as long as @serve@ is up. "I could not look" is not
  evidence the effect went away. Such a node therefore settles into a 60s
  no-op poll, which is what a node that says nothing about itself has earned.
* __A failing @up@ backs off rather than tightening.__ The spec's rule
  adapts the delay on what the /check/ said; it says nothing about how often
  to retry an @up@ that keeps throwing. Tightening there would hammer
  @apt-get@ every 500ms, so 'Upping' 'relaxed's on each failure and the
  retry cadence decays to the cap.

= Instructions finally mean something

'Salmon.Op.Mailbox.Recheck', 'Salmon.Op.Mailbox.Pause' and
'Salmon.Op.Mailbox.Resume' are read and ignored by the one-shot concurrent
driver, because there is nothing continuous for them to modify. Here
'Salmon.Op.Mailbox.Recheck' collapses the delay to its floor and looks now,
'Salmon.Op.Mailbox.Pause' stops tending the node without touching its effect,
and 'Salmon.Op.Mailbox.Resume' starts again. 'Salmon.Op.Mailbox.Force' and
'Salmon.Op.Mailbox.Satisfy' keep their meanings. Every wait in the machine —
the neighbour wait and the delay both — is a choice against the mailbox, so
an instruction is never queued behind a 60s nap.

= Failure is waited out, not contained

This is the sharpest difference from the one-shot drivers and the reason
'Salmon.Op.Status.waitStability' deliberately cannot see whether a neighbour
succeeded. A one-shot pass reports 'Salmon.Actions.UpDown.Blocked' for a node
whose dependency failed, because the pass is about to end and the node will
not get another chance. Here it gets nothing but chances: the dependency's
own machine is still retrying, so the dependant simply keeps waiting and
proceeds the moment the dependency recovers, with nobody re-declaring
anything. That is the same fact — "do not act against an unmet
precondition" — with the driver's own answer to what to do about it.

A cycle is the one case with no answer, and it is found before the walk for
the same reason "Salmon.Actions.Concurrent" finds it there: a thread waiting
on a node in a cycle never wakes.

= A node leaving 'Up' can take its dependants with it

By default it does not: putting a node back is a statement about that node,
and the nodes standing on it that have already reached 'Up' are not
disturbed. A node whose author says
'Salmon.Op.Supervision.RestForOne' is the exception — its dependants go back
to 'WaitUp' and are brought up again on top of whatever it turns into, which
is Erlang's strategy of the same name read along dependency edges, and the
only thing in this design that changes what a /correct/ graph does.

Three things keep that affordable:

* __it is opt-in on the node that goes away__, so a graph naming no strategy
  behaves exactly as it did before, and a machine with no such dependency
  subscribes to no statuses at all — the cost is zero rather than small;
* __a dependency that has not been ready yet cannot demote anybody.__
  Otherwise a supervisor starting over a graph a pass has just converged
  would send every opted-in node back to 'WaitUp' before its dependencies'
  machines had settled, undoing 'Standing' wholesale;
* __a node is demoted at most once per its own
  'Salmon.Op.Supervision.supStableAfter'__, so a flapping dependency cannot
  rebuild the cone behind it on every flap. A rate limit rather than a
  settling delay, deliberately: a settling delay would swallow the case the
  feature is for, since a rewritten config file is back within milliseconds.

= The watchdog

'Salmon.Op.Supervision.supWatchdog' is a node author saying how long their
node may go without doing anything observable. A single scanning thread
compares 'Salmon.Op.Status.statusLastActive' against it and reports
'Wedged' — once per episode, with 'Unwedged' when the node moves again. It
only ever /reports/: killing a wedged @up@ would need a bracket around it
that an @up :: IO ()@ does not have, which is exactly what a node owning its
process ('Salmon.Builtin.Extension.managed') supplies and no other node can.
If no node in the dag declares a watchdog the thread is never started.
-}
module Salmon.Actions.Upkeep (
    -- * The machines
    UpkeepState (..),
    DownkeepState (..),

    -- * The adaptive delay
    Delay,
    initialDelay,
    delayFloor,
    delayCap,
    delayMicros,
    attentive,
    relaxed,

    -- * What a node is tended as
    Tend (..),
    Standing (..),

    -- * Running
    Supervisor,
    startUpkeep,
    stopUpkeep,
    withUpkeep,
    supervisorStatuses,
    supervisorMailboxes,
    supervisorTending,
    instruct,

    -- * Machines that outlive a supervisor
    Kept,
    noKept,
    keptHeld,
    releaseKept,

    -- * Reporting
    Report (..),
) where

import Control.Concurrent.Async (Async, async, cancel, poll, waitCatch, waitCatchSTM, withAsync)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay, retry, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forM_, unless)
import Data.Dynamic (Dynamic)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Records (HasField, getField)
import System.Exit (ExitCode (..))

import Salmon.Actions.UpDown (CheckResult (..), runCheck)
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..), Mailbox)
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.Ref (Ref)
import Salmon.Op.Status (Direction (..), Stability (..), Status (..), newStatus, note, settle, touch, unsettle, waitStability, wedged)
import Salmon.Op.Supervision (Micros (..), Restart (..), Strategy (..), Supervision (..), millis, seconds, supervisionOf, toNanos)
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | Where a node wanted up currently is.
data UpkeepState
    = -- | a dependency is not up yet, or is failing
      WaitUp
    | -- | running @up@
      Upping
    | -- | the effect is in place; checking on it periodically
      Up
    deriving (Show, Eq, Ord)

{- | Where a node wanted down currently is. 'Down' is terminal — see the
module header on why there is no polling counterpart to 'Up'.
-}
data DownkeepState
    = -- | something still standing on this node has not come down
      WaitDown
    | -- | running @down@
      Downing
    | Down
    deriving (Show, Eq, Ord)

{- | What a node is to be tended as: which way, and whether it is already
there.

@
'Tend' 'TurnUp' 'Unsettled'  -- bring it up, then keep it up
'Tend' 'TurnUp' 'Settled'    -- it is up; just keep it that way
@
-}
data Tend = Tend
    { tendDirection :: !Direction
    , tendStanding :: !Standing
    }
    deriving (Show, Eq)

{- | Whether the caller already knows the node to be where it wants to be.

This exists because a supervisor is usually started /after/ something else
has just done the work — a convergence pass, or an earlier supervisor — and
a node whose check cannot confirm its own effect would otherwise have that
work done again immediately. Most nodes in this repository have no @check@
at all and answer 'Salmon.Actions.UpDown.Unknown', so 'Consult'ing them on
the way in means re-running every @up@ in the graph after every pass. The
caller knows better and says so.

'Settled' is a claim about the past, not a promise about the future: the
node's machine still looks, on the ordinary adaptive delay, and still puts
the node back if the effect has gone. What 'Settled' skips is the first
@up@, not the watching.
-}
data Standing
    = -- | not known to be there: wait for the neighbours, then act
      Unsettled
    | -- | already there; start in 'Up' (or 'Down') and only look
      Settled
    deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

{- | How long to wait before looking again. Doubles while things are fine and
halves while they are not, between a floor and a cap.
-}
newtype Delay = Delay Micros
    deriving (Show, Eq, Ord)

-- | Half a second: as often as this ever looks.
delayFloor :: Micros
delayFloor = millis 500

-- | A minute: as rarely as this ever looks.
delayCap :: Micros
delayCap = seconds 60

{- | Where a machine starts. At the floor, because a node that has just been
brought up is the one most likely to fall straight back over.
-}
initialDelay :: Delay
initialDelay = Delay delayFloor

delayMicros :: Delay -> Micros
delayMicros (Delay m) = m

-- | Look sooner: halve, no lower than 'delayFloor'.
attentive :: Delay -> Delay
attentive (Delay (Micros m)) = Delay (Micros (max (unMicros delayFloor) (m `div` 2)))

-- | Look later: double, no higher than 'delayCap'.
relaxed :: Delay -> Delay
relaxed (Delay (Micros m)) = Delay (Micros (min (unMicros delayCap) (m * 2)))

-------------------------------------------------------------------------------

{- | Everything this driver has to say. 'Acted' carries the one-shot drivers'
own vocabulary unchanged, so a caller already listening to
'Salmon.Actions.UpDown.Report' — @serve@'s convergence bookkeeping, for
instance — keeps working by looking at nothing else.
-}
data Report ext
    = -- | what the node did, in the one-shot drivers' words
      Acted !(UpDown.Report ext)
    | -- | a node wanted up changed state
      Upkeep !(Act ext) !UpkeepState
    | -- | a node wanted down changed state
      Downkeep !(Act ext) !DownkeepState
    | -- | resting in 'Up': this is what the check said, and this is how long
      -- until the next one
      NextLook !(Act ext) !CheckResult !Micros
    | -- | silent for longer than its author said it ever should be
      Wedged !(Act ext) !Micros
    | -- | ...and moving again
      Unwedged !(Act ext)
    | -- | a dependency that declared 'Salmon.Op.Supervision.RestForOne' left
      -- 'Up', so this node went back to 'WaitUp' to be brought up again on
      -- top of whatever that dependency becomes
      Demoted !(Act ext) !Ref
    | -- | told to stop tending this node; its effect is left exactly as it is
      Paused !(Act ext)
    | Resumed !(Act ext)
    | -- | this many consecutive failures was the author's limit
      -- ('Salmon.Op.Supervision.supGiveUpAfter'), so the node is parked
      -- until an operator forces or rechecks it
      GaveUp !(Act ext) !Int
    | -- | a machine left running by a previous supervisor was taken over
      -- rather than restarted, so the effect it holds never stopped
      Adopted !(Act ext)
    | -- | ...and one that was not taken over: cancelled, which tears the
      -- effect it held down through the action's own bracket
      Released !(Act ext)
    | -- | this node declared more than one 'Supervision'; the first is in
      -- force and the rest are not. See "Salmon.Op.Supervision".
      Policy !(Act ext) !Supervision ![Supervision]
    | -- | in the dag, but not this supervisor's business: settled out of the
      -- way so that its neighbours are not held up
      Untended !(Act ext)
    | -- | a node's own machine threw, which is a bug in this module rather
      -- than a failure of the node
      Escaped !(Act ext) !SomeException
    | -- | machines started: wanted up, wanted down
      Supervising !Int !Int
    | -- | machines stopped
      Retired !Int
    | -- | ...and machines left running, holding effects up, for the next
      -- supervisor to adopt. See 'Kept'.
      Holding !Int
    deriving (Show)


-------------------------------------------------------------------------------

-- | One node's machine, its observable state, and the way to talk to it.
data Machine ext = Machine
    { machineAct :: !(Act ext)
    , machineDirection :: !Direction
    , machineStatus :: !(TVar Status)
    , machineMailbox :: !Mailbox
    , machineWatchdog :: !(Maybe Micros)
    , machineHolds :: !Bool
    -- ^ whether this machine holds a running
    -- 'Salmon.Builtin.Extension.managed' action, and so is 'Kept' rather
    -- than wound down when its supervisor stops.
    , machineUnder :: !(TVar Under)
    -- ^ the supervisor this machine is running under. Held here, and not
    -- only inside the machine's own closure, so that a supervisor adopting
    -- the machine can hand it its own. See 'Under'.
    , machineThread :: !(Async ())
    }

{- | Everything about a machine that belongs to its /supervisor/ rather than
to its node: who it waits on, which of those can send it back to 'WaitUp',
where the failures everyone reads are recorded, and when to stop.

Behind a 'TVar' for one case, and it is the case 'Kept' created. A machine
holding a 'Salmon.Builtin.Extension.managed' action outlives the supervisor
that started it, and an adopted machine still looking at that supervisor's
state would be looking at things nobody maintains any more: it could never
see a dependency leave 'Up', its own failures would be recorded where no
dependant reads them, and — the one that bites hardest — the halt flag it
watches is permanently set, so the moment such a machine took a path that
heeds it (which, before 'Salmon.Op.Supervision.RestForOne', it never did) it
would quietly exit and orphan the process it holds. So 'startUpkeep' writes
its own state into every machine it adopts, and every wait reads that afresh
rather than closing over it.
-}
data Under = Under
    { underStatuses :: !(Map Ref (TVar Status))
    -- ^ every node this supervisor is tending. A neighbour that is not in
    -- here is not waited on at all: nothing is going to move it, so waiting
    -- for it to move would be waiting forever.
    , underFailed :: !(TVar (Set Ref))
    -- ^ which nodes are currently failing. Not in 'Status' for the reason
    -- 'Salmon.Op.Status.waitStability' gives: the two drivers answer
    -- "proceed past a failure?" differently.
    , underHalt :: !(TVar Bool)
    -- ^ set when this supervisor is stopping. See 'Heed' for who is allowed
    -- to hear it, and why a machine holding an effect is not.
    , underDependencies :: ![Ref]
    -- ^ waited on by a node going up.
    , underDependants :: ![Ref]
    -- ^ waited on by a node coming down.
    , underDemoters :: ![Ref]
    -- ^ the dependencies that declared 'Salmon.Op.Supervision.RestForOne':
    -- the ones whose leaving 'Up' sends this node back to 'WaitUp'. Empty
    -- for every node until somebody opts one in, and that emptiness is the
    -- whole of why the feature costs nothing.
    }

{- | Where a node in 'Up' last saw each of its demoting dependencies: which
machine it was watching, and the 'Salmon.Op.Status.statusEpoch' that machine
was settled at.

A dependency that is /absent/ is disarmed — it has not been seen settled up
since this node started watching, and so cannot send it anywhere. That is
what a dependency starts out as when it has not come up yet, and what one
becomes again when a departure of its is deliberately not acted on.

The 'TVar' is remembered alongside the number because the two are only
comparable together. An adopted machine's dependency is a /different
machine/ for the same node (a fresh 'Salmon.Op.Status.Status', counting from
zero), and comparing this node's memory of the old one against the new one's
epoch would read as a departure on every command @serve@ is handed —
restarting every service, which is the thing 'Kept' exists to prevent. A
dependency whose machine has been replaced is therefore re-armed, not acted
on. See 'crossing'.
-}
type Armed = Map Ref (TVar Status, Word64)

{- | A running set of node machines.

Only /tended/ nodes are in here. A node in the dag that this supervisor was
not asked to tend has no machine and no 'Status', and is not waited on by
anybody: nothing is going to move it, so waiting for it to move would be
waiting forever. That is the same call the one-shot drivers' 'UpDown.Gate'
makes when it answers 'UpDown.Skippable'.
-}
data Supervisor ext = Supervisor
    { supMachines :: !(Map Ref (Machine ext))
    , supHalt :: !(TVar Bool)
    , supWatch :: !(Maybe (Async ()))
    , supSay :: !(Report ext -> IO ())
    }

{- | Machines a stopped supervisor left running, for the next one to take
over.

A machine that holds a running process cannot be treated the way a one-shot
machine is. Stopping a supervisor stops /tending/ — and since @serve@ stands
its machines down before every command it is handed, a supervisor that wound
its processes down with it would kill every service on every @status@. So a
holding machine survives its supervisor, and the next 'startUpkeep' either
__adopts__ it (the effect it holds never stopped) or __releases__ it
(cancelled, which tears that effect down through the action's own bracket).

The choice between the two is what @specs\/per-node-state-machines.md@'s
§"@Ref@ is location-addressed" calls the one case where swapping a machine is
right: a node is adopted only if it is still wanted 'TurnUp' /and/ its
representative has not changed. A @managed@ node whose command line changed
but whose ref key did not is the same node with a different action, and the
process running is the old one's.
-}
newtype Kept ext = Kept (Map Ref (Machine ext))

-- | Nothing running: what a first supervisor is given.
noKept :: Kept ext
noKept = Kept Map.empty

-- | What each kept machine is holding up, for a caller deciding what to
-- release.
keptHeld :: Kept ext -> Map Ref (Act ext)
keptHeld (Kept ms) = fmap machineAct ms

{- | Cancel every kept machine whose 'Ref' the predicate rejects, and return
what is left.

Cancelling is the teardown: the machine's thread is inside a 'withAsync' over
the node's action, so the async exception unwinds through whatever bracket
that action is built from — for "Salmon.Builtin.Nodes.Daemon" that is
@SIGTERM@ to the process group, a grace period, then @SIGKILL@. 'cancel'
waits, so when this returns the effects really are down.

That waiting is the point of exposing this at all: a caller tearing a node
down has to be able to do it __before__ anything else in the graph moves. A
daemon's dependencies — its config file, its working directory — must not be
removed while it is still running, and nothing but ordering prevents that.
-}
releaseKept ::
    Reporter (Report ext) ->
    (Ref -> Bool) ->
    Kept ext ->
    IO (Kept ext)
releaseKept report keep (Kept ms) = do
    let (kept, going) = Map.partitionWithKey (\aref _ -> keep aref) ms
    forM_ (Map.elems going) $ \m -> do
        cancel (machineThread m)
        runReporter report (Released (machineAct m))
    pure (Kept kept)

-- | The live state of every node being tended.
supervisorStatuses :: Supervisor ext -> Map Ref (TVar Status)
supervisorStatuses = fmap machineStatus . supMachines

-- | The mailbox of every node being tended.
supervisorMailboxes :: Supervisor ext -> Map Ref Mailbox
supervisorMailboxes = fmap machineMailbox . supMachines

-- | Which nodes are being tended, and which way each.
supervisorTending :: Supervisor ext -> Map Ref Direction
supervisorTending = fmap machineDirection . supMachines

{- | Tell one node something. 'False' if the node has no machine here, or if
its mailbox was full and an older instruction had to be evicted to make room
(which the node reports when it reads it).
-}
instruct :: Supervisor ext -> Ref -> Instruction -> IO Bool
instruct sup aref instruction =
    case Map.lookup aref (supMachines sup) of
        Nothing -> pure False
        Just m -> Mailbox.post (machineMailbox m) instruction

-------------------------------------------------------------------------------

{- | Start tending every node the second argument names a direction for.

A node it returns 'Nothing' for is reported 'Untended' and left entirely
alone — no machine, no status, and nothing waits on it. A node on a cycle is
reported 'Salmon.Actions.UpDown.Blocked' and likewise never started, because
here it would wait forever rather than be noticed at the end of a pass.

Returns as soon as the machines are running. They run until 'stopUpkeep'.
-}
startUpkeep ::
    forall ext.
    ( HasField "up" ext (IO ())
    , HasField "down" ext (IO ())
    , HasField "managed" ext (Maybe ((Text -> IO ()) -> IO ExitCode))
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    , HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Reporter (Report ext) ->
    -- | machines a previous supervisor left running; 'noKept' for a first one
    Kept ext ->
    -- | which nodes to tend, how
    (Ref -> Maybe Tend) ->
    Dag ext ->
    IO (Supervisor ext)
startUpkeep report (Kept prior) tend dag = do
    -- reports are serialised for the same reason the concurrent one-shot
    -- driver serialises them: the caller's reporter is not assumed
    -- thread-safe, and interleaved multi-line reports are unreadable.
    lock <- newMVar ()
    let say rep = withMVar lock (\() -> runReporter report rep)

    halt <- newTVarIO False
    failed <- newTVarIO (Set.empty :: Set Ref)

    forM_ untended $ \act -> say (Untended act)
    forM_ blocked $ \act -> say (Acted (UpDown.Blocked act))

    -- machines left running by the previous supervisor that this one is
    -- taking over rather than restarting. Everything else it left is
    -- released below, which tears down what it was holding.
    adopted <- fmap (Map.fromList . concat) $ forM tended $ \(aref, act, t) ->
        case Map.lookup aref prior of
            Just m
                | t.tendDirection == TurnUp
                , Dag.sameRepresentative (machineAct m) act -> do
                    alive <- poll (machineThread m)
                    case alive of
                        -- its thread finished while nobody was watching, so
                        -- there is nothing to take over.
                        Just _ -> pure []
                        Nothing -> do
                            say (Adopted act)
                            pure [(aref, m)]
            _ -> pure []
    Kept leftovers <- releaseKept report (`Map.member` adopted) (Kept prior)
    -- 'releaseKept' cancelled everything not adopted, so this is empty; it
    -- is bound rather than ignored so that a future change to that function
    -- cannot silently strand a process here.
    unless (Map.null leftovers) $
        forM_ (Map.elems leftovers) (say . Released . machineAct)

    let starting = [entry | entry@(aref, _, _) <- tended, not (Map.member aref adopted)]

    fresh <-
        Map.fromList
            <$> forM starting (\(aref, _, t) -> (,) aref <$> newStatus t.tendDirection)
    let statuses = fmap machineStatus adopted <> fresh

    let under aref =
            let ds = Dag.dependenciesOf dag aref
             in Under
                    { underStatuses = statuses
                    , underFailed = failed
                    , underHalt = halt
                    , underDependencies = ds
                    , underDependants = Dag.dependantsOf dag aref
                    , -- authored on the dependency, read by the dependant:
                      -- only the node that goes away knows whether its going
                      -- away matters to whatever is standing on it.
                      underDemoters =
                        [ d
                        | d <- ds
                        , Map.member d statuses
                        , Map.lookup d strategies == Just RestForOne
                        ]
                    }

    -- an adopted machine came from a supervisor whose maps are now nobody's:
    -- hand it this one's, or it would watch 'TVar's that never change again
    -- and record its failures where no dependant reads them.
    forM_ (Map.toList adopted) $ \(aref, m) ->
        atomically (writeTVar (machineUnder m) (under aref))

    machines <- forM starting $ \(aref, act, t) -> do
        let (policy, ignored) = supervisionOf act.extension
        unless (null ignored) $ say (Policy act policy ignored)
        box <- Mailbox.newMailbox Mailbox.defaultCapacity
        drops <- newTVarIO 0
        under' <- newTVarIO (under aref)
        let status = statuses Map.! aref
        let holds = isJust (getField @"managed" act.extension)
        let ctx =
                Ctx
                    { ctxSay = say
                    , ctxUnder = under'
                    , ctxRef = aref
                    , ctxAct = act
                    , ctxStatus = status
                    , ctxBox = box
                    , ctxDrops = drops
                    , ctxPolicy = policy
                    }
        -- A 'Settled' claim is about an effect that persists on its own, and
        -- a managed effect does not persist without a machine holding it. So
        -- a managed node that was not adopted starts from scratch whatever
        -- the caller believes about it: there is no process, so it is not up.
        let t' = if holds then t{tendStanding = Unsettled} else t
        thread <- async (machine t' ctx)
        pure
            ( aref
            , Machine
                { machineAct = act
                , machineDirection = t.tendDirection
                , machineStatus = status
                , machineMailbox = box
                , machineWatchdog = supWatchdog policy
                , machineHolds = holds
                , machineUnder = under'
                , machineThread = thread
                }
            )

    let table = adopted <> Map.fromList machines
    let ups = length [() | m <- Map.elems table, machineDirection m == TurnUp]
    say (Supervising ups (Map.size table - ups))

    watch <- startWatchdog say halt table

    pure
        Supervisor
            { supMachines = table
            , supHalt = halt
            , supWatch = watch
            , supSay = say
            }
  where
    -- a node on a cycle never becomes ready. One relation is enough to find
    -- one: the dependants relation is the dependencies relation reversed, so
    -- a cycle in either is a cycle in both.
    stuckRefs :: Set Ref
    stuckRefs = Dag.stuck Dag.dependenciesOf dag

    classified :: [(Ref, Act ext, Maybe Tend, Bool)]
    classified =
        [ (aref, act, tend aref, Set.member aref stuckRefs)
        | aref <- Dag.dagOrder dag
        , Just act <- [Dag.representativeOf dag aref]
        ]

    tended :: [(Ref, Act ext, Tend)]
    tended = [(aref, act, t) | (aref, act, Just t, False) <- classified]

    blocked :: [Act ext]
    blocked = [act | (_, act, Just _, True) <- classified]

    untended :: [Act ext]
    untended = [act | (_, act, Nothing, _) <- classified]

    -- what each tended node's own author said about the nodes standing on
    -- it; every dependant reads its dependencies' entries out of here.
    strategies :: Map Ref Strategy
    strategies =
        Map.fromList
            [ (aref, supStrategy (fst (supervisionOf act.extension)))
            | (aref, act, _) <- tended
            ]

{- | Ask every machine to stop, wait for it, and report how many stopped.

__Nothing is torn down.__ Stopping a supervisor stops /tending/ these nodes;
it does not run anybody's @down@. A node whose @up@ is in flight is waited
for rather than interrupted, because interrupting an @up@ halfway is how a
half-applied effect happens; a node that is merely napping stops at once.
-}
stopUpkeep :: Supervisor ext -> IO (Kept ext)
stopUpkeep sup = do
    atomically (writeTVar (supHalt sup) True)
    traverse_ waitCatch (supWatch sup)
    let (holding, oneShots) = Map.partition machineHolds (supMachines sup)
    forM_ (Map.elems oneShots) $ \m -> do
        outcome <- waitCatch (machineThread m)
        case outcome of
            Right () -> pure ()
            -- a machine is not supposed to be able to throw: `up` and
            -- `down` are caught inside it. If one does, that is this
            -- module's bug and not the node's, so it is reported as such.
            Left e -> supSay sup (Escaped (machineAct m) e)
    -- a holding machine ignores the halt flag by construction, so these are
    -- all still running — except one whose node never got past 'WaitUp' (it
    -- was holding nothing yet, so it heeded the halt like any other) or
    -- whose action gave up on its own. Those have nothing to hand over.
    kept <- flip Map.traverseMaybeWithKey holding $ \_ m -> do
        alive <- poll (machineThread m)
        pure (if isJust alive then Nothing else Just m)
    supSay sup (Retired (Map.size oneShots))
    unless (Map.null kept) $ supSay sup (Holding (Map.size kept))
    pure (Kept kept)

-- | 'startUpkeep' and 'stopUpkeep' as a bracket.
withUpkeep ::
    ( HasField "up" ext (IO ())
    , HasField "down" ext (IO ())
    , HasField "managed" ext (Maybe ((Text -> IO ()) -> IO ExitCode))
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    , HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Reporter (Report ext) ->
    (Ref -> Maybe Tend) ->
    Dag ext ->
    (Supervisor ext -> IO a) ->
    IO a
withUpkeep report tend dag body =
    bracket (startUpkeep report noKept tend dag) release body
  where
    -- a bracket owns everything it started, holding machines included: the
    -- caller has nowhere to put a 'Kept'.
    release sup = do
        kept <- stopUpkeep sup
        _ <- releaseKept report (const False) kept
        pure ()

-------------------------------------------------------------------------------

-- | What one machine needs to do its job.
data Ctx ext = Ctx
    { ctxSay :: !(Report ext -> IO ())
    , ctxUnder :: !(TVar Under)
    -- ^ everything about this machine's surroundings, re-read on every wait
    -- rather than captured: an adopted machine's surroundings change under
    -- it. See 'Under'.
    , ctxRef :: !Ref
    , ctxAct :: !(Act ext)
    , ctxStatus :: !(TVar Status)
    , ctxBox :: !Mailbox
    , ctxDrops :: !(TVar Int)
    -- ^ evictions already reported, so a repeated read reports the
    -- difference rather than the running total.
    , ctxPolicy :: !Supervision
    }

{- | What 'upping' is to do about the node's own check before acting.

Two entries into 'Upping' and they want opposite things. Arriving from
'WaitUp' the check has not been asked yet and is the whole point: it is what
makes a re-declared graph cost nothing. Arriving from 'Up' — or from a
'Salmon.Op.Mailbox.Force' — it has just been asked and the answer is the
/reason/ we are here, so asking again would be both wasteful and wrong: a
'Salmon.Actions.UpDown.Completed' node that 'Salmon.Op.Supervision.Always'
says to run again would be talked out of it by its own check.
-}
data Intent
    = -- | ask the check; skip if it says the effect is already there
      Consult
    | -- | act, and this is why
      Regardless !CheckResult
    deriving (Show)

-- | Why a wait ended.
data Wake
    = -- | the delay expired, or the neighbours are ready
      Elapsed
    | -- | somebody said something; oldest first, never empty
      Told ![Instruction]
    | -- | the held action stopped, with its exit status (or the exception it
      -- threw instead of exiting). Only a machine holding a
      -- 'Salmon.Builtin.Extension.managed' action can see this.
      Ended !(Either SomeException ExitCode)
    | -- | a dependency that declared 'Salmon.Op.Supervision.RestForOne' has
      -- stopped being up. Only a machine in 'Up' with such a dependency can
      -- see this.
      Demote !Ref
    | -- | ...and one that had, is settled up again — this is its machine and
      -- the 'Salmon.Op.Status.statusEpoch' it is settled at, and it may
      -- demote this node next time it moves. See 'crossing' on why the two
      -- are a pair.
      Rearm !Ref !(TVar Status) !Word64
    | -- | the supervisor is stopping
      Halt

{- | Whether a wait is allowed to end because the supervisor is stopping.

A machine holding a running effect answers 'IgnoreHalt': stopping a
supervisor stops /tending/, and a machine that let go of its own process
every time a command was typed would kill every service on every @status@.
Such a machine is kept (see 'Kept') and is only ever taken by an outright
'cancel', which is also what tears its effect down.
-}
data Heed
    = HeedHalt
    | IgnoreHalt
    deriving (Show, Eq)

-------------------------------------------------------------------------------

{- | What the restart policy has to remember between attempts.

Two fields, and neither is derivable from the node's 'Status': that carries
what the node is doing now, while this carries how it has been getting on.
-}
data Tally = Tally
    { tallyFailures :: !Int
    -- ^ /consecutive/ failures, which is the only count a give-up limit can
    -- sensibly read.
    , tallyUpSince :: !(Maybe Word64)
    -- ^ monotonic nanoseconds at the moment the node last reached 'Up'.
    , tallyDemotedAt :: !(Maybe Word64)
    -- ^ monotonic nanoseconds at the moment a dependency last sent this node
    -- back to 'WaitUp'. What rate-limits 'Salmon.Op.Supervision.RestForOne';
    -- see 'tooSoon'.
    }

freshTally :: Tally
freshTally = Tally 0 Nothing Nothing

{- | Count a failure — first forgetting the ones before it, if the node had
been up long enough to count as working.

'Salmon.Op.Supervision.supStableAfter' is what makes a give-up limit usable
at all: without it, a service that falls over once a day reaches any finite
limit eventually and latches off, having never actually been in a crash
loop.
-}
countFailure :: Supervision -> Word64 -> Tally -> Tally
countFailure sup now t =
    case t.tallyUpSince of
        Just since
            | now >= since
            , now - since >= toNanos sup.supStableAfter ->
                t{tallyFailures = 1, tallyUpSince = Nothing}
        _ -> t{tallyFailures = t.tallyFailures + 1, tallyUpSince = Nothing}

-- | Has this node used up the author's patience?
exhausted :: Supervision -> Tally -> Bool
exhausted sup t = maybe False (\n -> t.tallyFailures >= n) sup.supGiveUpAfter

{- | Was this node sent back by a dependency so recently that doing it again
would be following a flap rather than a change?

Never having been demoted is never too soon: an isolated departure is
honoured whenever it comes. That is what keeps this a __rate limit rather
than a settling delay__ — a settling delay would swallow the very case
'Salmon.Op.Supervision.RestForOne' exists for, since the config file a
service stands on is rewritten in milliseconds and is back long before any
window could expire. What is dropped is the /second/ demotion inside the
node's own 'Salmon.Op.Supervision.supStableAfter', which is what a flap looks
like and a change does not.
-}
tooSoon :: Supervision -> Word64 -> Tally -> Bool
tooSoon sup now t =
    case t.tallyDemotedAt of
        Just at | now >= at -> now - at < toNanos sup.supStableAfter
        _ -> False

{- | How long to wait before the n-th consecutive retry: the floor doubled
@n-1@ times, capped.

Derived from the failure count rather than carried alongside it, so it resets
exactly when 'countFailure' resets the count, and cannot drift out of step
with it.
-}
backoff :: Int -> Delay
backoff n = iterate relaxed initialDelay !! min 24 (max 0 (n - 1))

{- | What a restart policy makes of an exit status.

The systemd reading, and the reason owning a process is worth the trouble:
'Salmon.Op.Supervision.OnFailure' is only expressible if something can tell
@exit 0@ from @exit 137@, which no @check@ can.
-}
restartsOnExit :: Restart -> ExitCode -> Bool
restartsOnExit Never _ = False
restartsOnExit OnFailure ExitSuccess = False
restartsOnExit OnFailure (ExitFailure _) = True
restartsOnExit Always _ = True

machine ::
    ( HasField "up" ext (IO ())
    , HasField "down" ext (IO ())
    , HasField "managed" ext (Maybe ((Text -> IO ()) -> IO ExitCode))
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Tend ->
    Ctx ext ->
    IO ()
machine (Tend TurnUp standing) ctx = upkeep standing ctx
machine (Tend TurnDown standing) ctx = downkeep standing ctx

-------------------------------------------------------------------------------

{- | The lifecycle of a node wanted up: @WaitUp -> Upping -> Up@, and back to
'Upping' whenever the node stops being up and its policy says to put it back.

Two shapes of node run through here and the difference is confined to one
step. A node with only @up@ /does/ something and returns, and 'Up' is a
periodic check on what it left behind. A node with a
'Salmon.Builtin.Extension.managed' action /is/ its effect for as long as the
action runs, so 'Up' additionally races the action itself: the exit it
eventually yields is the reason the node stopped being up, and is what the
restart policy reads instead of a 'CheckResult'.
-}
upkeep ::
    forall ext.
    ( HasField "up" ext (IO ())
    , HasField "managed" ext (Maybe ((Text -> IO ()) -> IO ExitCode))
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Standing ->
    Ctx ext ->
    IO ()
upkeep standing ctx =
    case standing of
        Unsettled -> waitUp freshTally []
        -- Already up: settle so dependants may go, and start watching. The
        -- verdict is 'Skipped' because that is exactly what it is — nobody
        -- looked, somebody said — and the first 'look' replaces it.
        --
        -- 'startUpkeep' never hands 'Settled' to a node with a managed
        -- action, because a 'Settled' claim is about an effect that persists
        -- on its own and a managed effect does not persist without the
        -- machine holding it.
        Settled -> do
            markOk ctx
            settle status Skipped
            say (Upkeep act Up)
            entering Skipped (relaxed initialDelay) freshTally
  where
    act = ctxAct ctx
    say = ctxSay ctx
    status = ctxStatus ctx
    policy = ctxPolicy ctx

    -- | The action, if this node owns one.
    holding :: Maybe ((Text -> IO ()) -> IO ExitCode)
    holding = getField @"managed" act.extension

    {- | Nothing to do until the dependencies are up. Instructions that arrive
    meanwhile are held rather than lost: a 'Force' typed at a node whose
    dependency is still coming up means "when you get there, act", not "act
    now against an unmet precondition".

    Carries the 'Tally' rather than starting a fresh one, because this is
    where a demoted node comes back to and the moment it was demoted is what
    stops a flapping dependency demoting it again immediately. -}
    waitUp :: Tally -> [Instruction] -> IO ()
    waitUp tally pending = do
        say (Upkeep act WaitUp)
        loop pending
      where
        loop held = do
            w <- standby ctx TurnUp
            told <- announce ctx w
            case w of
                Halt -> pure ()
                Ended _ -> pure () -- nothing is running yet; unreachable
                -- 'standby' does not watch for these; a node that is not up
                -- has nothing to be demoted from.
                Demote _ -> loop held
                Rearm{} -> loop held
                Told _ -> paused ctx told (loop (held <> told)) (loop (held <> told))
                Elapsed -> attempt (held <> told) Consult tally

    -- | Decide whether to act, then act in whichever way this node acts.
    attempt :: [Instruction] -> Intent -> Tally -> IO ()
    attempt told intent tally
        | Just Satisfy <- override told = satisfy
        | otherwise = do
            say (Upkeep act Upping)
            unsettle status TurnUp
            decided <- case (intent, told `has` Force) of
                (_, True) -> pure (Right (Failure "forced"))
                (Regardless why, _) -> pure (Right why)
                (Consult, _) -> do
                    verdict <- runCheck act
                    pure (if satisfiedBy verdict then Left verdict else Right verdict)
            case decided of
                Left verdict -> do
                    say (Acted (UpDown.Skip act))
                    reached verdict tally
                Right _ -> case holding of
                    Nothing -> oneShot tally
                    Just action -> hold action tally

    -- | An @up@ that returns, leaving something behind that persists.
    oneShot :: Tally -> IO ()
    oneShot tally = do
        say (Acted (UpDown.Eval act))
        note status "up"
        outcome <- try @SomeException act.extension.up
        case outcome of
            Right () -> do
                say (Acted (UpDown.Done act))
                reached Success tally
            Left e -> do
                say (Acted (UpDown.Failed act e))
                note status (Text.pack (show e))
                failed (Failure (Text.pack (show e))) tally

    {- | An action that /is/ the effect. 'withAsync' rather than 'async' is
    the whole of the teardown story: cancelling this machine's thread
    cancels the action, and whatever bracket the action is built from does
    the killing — see "Salmon.Builtin.Nodes.Daemon". The scope of the
    'withAsync' is one run of the effect; a restart leaves it and comes
    back through 'attempt'. -}
    hold :: ((Text -> IO ()) -> IO ExitCode) -> Tally -> IO ()
    hold action tally = do
        say (Acted (UpDown.Eval act))
        note status "spawn"
        -- 'watch' hands back what to do /once the action is no longer held/,
        -- and that continuation is run outside the 'withAsync' on purpose:
        -- leaving the block is what cancels the action, so a restart is
        -- guaranteed to have torn the old effect down before the new
        -- attempt spawns.
        next <- withAsync (action (note status)) $ \running -> do
            -- Up as soon as it is running: for a node whose action is the
            -- effect, "the action is running" is the whole of being up. The
            -- report is 'Done' for the same reason, which is what lets
            -- @serve@ record such a node as converged at all.
            now <- getMonotonicTimeNSec
            markOk ctx
            settle status Success
            say (Acted (UpDown.Done act))
            say (Upkeep act Up)
            armed <- arming ctx
            watch running Success (relaxed initialDelay) tally{tallyUpSince = Just now} armed
        next

    {- | 'Up' with an action in hand: the nap, the mailbox, the action's own
    exit and any demoting dependency, raced. The check still runs on the
    adaptive delay, so a managed node that also supplies a @check@ gets both;
    one that does not pays a @pure Unknown@ per delay, which is the price of
    not being able to tell "no check" from "a check that could not tell". -}
    watch :: Async ExitCode -> CheckResult -> Delay -> Tally -> Armed -> IO (IO ())
    watch running verdict d tally armed = do
        say (NextLook act verdict (delayMicros d))
        w <- naptimeHolding ctx running armed (delayMicros d)
        told <- announce ctx w
        case w of
            -- 'naptimeHolding' answers 'IgnoreHalt', so this cannot happen:
            -- a machine holding a running effect is kept rather than wound
            -- down, and only a 'cancel' takes it.
            Halt -> watch running verdict d tally armed
            Ended outcome -> pure (afterExit outcome tally)
            Elapsed -> peek (relaxed d)
            Rearm dep var e -> watch running verdict d tally (Map.insert dep (var, e) armed)
            Demote dep -> do
                sending <- demote dep tally
                case sending of
                    -- handed back rather than run, like a restart and for
                    -- the same reason: it runs outside the 'withAsync', so
                    -- the process this node holds is torn down before it
                    -- goes back to waiting.
                    Just go -> pure go
                    Nothing -> watch running verdict d tally (Map.delete dep armed)
            Told _
                -- pausing a node that owns a process must not kill the
                -- process: that is the whole difference between 'Pause' and
                -- a teardown. So this parks while still holding.
                | Just Pause <- tending told -> do
                    say (Paused act)
                    heldPause
                    say (Resumed act)
                    watch running verdict d tally armed
                -- forcing a node that is already running its own effect
                -- means restart it: hand back the next attempt, which runs
                -- after the 'withAsync' has cancelled this one.
                | Just Force <- override told -> pure (attempt told (Regardless (Failure "forced")) tally)
                | Just Satisfy <- override told -> pure satisfy
                | otherwise -> peek (soonIf told d)
      where
        {- | Look while still holding. A check that says the effect is gone
        even though the action is still running is a health probe failing —
        the process is up and not working — and restarting is what the
        policy is for. -}
        peek :: Delay -> IO (IO ())
        peek d' = do
            v <- runCheck act
            touch status
            if restarts policy v
                then pure (attempt [] (Regardless v) tally)
                else watch running v d' tally armed

        -- | Block for a 'Resume'. Ignores the halt flag for the same reason
        -- the nap does.
        heldPause :: IO ()
        heldPause = do
            told <- listenHolding ctx
            _ <- announce ctx (Told told)
            case tending told of
                Just Resume -> pure ()
                _ -> heldPause

    {- | The action stopped. Consult the check /before/ the policy: a process
    that exits 0 because it daemonised is still up, and the check is the only
    thing that can say so. That one ordering handles the double-fork case for
    free — the one shape a process handle cannot speak to at all, since a
    handle to a process that has exited says nothing about the daemon it left
    behind. -}
    afterExit :: Either SomeException ExitCode -> Tally -> IO ()
    afterExit outcome tally = do
        case outcome of
            Left e -> do
                say (Acted (UpDown.Failed act e))
                note status (Text.pack (show e))
            Right code -> note status ("exited " <> Text.pack (show code))
        verdict <- runCheck act
        if satisfiedBy verdict
            then do
                -- it forked, or something else is holding the effect up. The
                -- node is now an unowned effect and is polled like one.
                settle status verdict
                entering verdict (relaxed initialDelay) tally
            else
                if wantsBack
                    then failed (why verdict) tally
                    else do
                        -- it stopped and the policy says leave it. Settled,
                        -- and 'statusCheck' says which kind of stopped.
                        let final = case outcome of
                                Right ExitSuccess -> Completed
                                _ -> why verdict
                        case final of
                            Completed -> markOk ctx
                            _ -> markFailed ctx
                        settle status final
                        say (NextLook act final (delayMicros (relaxed initialDelay)))
                        entering final (relaxed initialDelay) tally
      where
        wantsBack = case outcome of
            -- the action threw rather than exiting, so there is no code for
            -- the policy to read; anything but 'Never' tries again.
            Left _ -> policy.supRestart /= Never
            Right code -> restartsOnExit policy.supRestart code
        why verdict = case outcome of
            Left e -> Failure (Text.pack (show e))
            Right ExitSuccess -> case verdict of
                Failure _ -> verdict
                _ -> Failure "exited"
            Right code -> Failure (Text.pack ("exited " <> show code))

    -- | The effect is in place. Keep an eye on it.
    reached :: CheckResult -> Tally -> IO ()
    reached verdict tally = do
        now <- getMonotonicTimeNSec
        markOk ctx
        settle status verdict
        say (Upkeep act Up)
        entering verdict (relaxed initialDelay) tally{tallyUpSince = Just now}

    {- | Enter 'Up'.

    The demote watch starts /disarmed/ for every dependency that is not ready
    at this instant, and each arms itself the first time it is seen ready.
    Without that, a supervisor starting over a graph a pass has just
    converged would demote every opted-in node before its dependencies'
    machines had settled — undoing 'Standing' wholesale and re-running every
    @up@ in the cone, which under @serve@ is once per command typed. -}
    entering :: CheckResult -> Delay -> Tally -> IO ()
    entering verdict d tally = do
        armed <- arming ctx
        resting verdict d tally armed

    {- | 'Up' without an action to hold: sleep, look, and adapt — back off
    while the effect is there, tighten and go back to 'Upping' when it is
    not. -}
    resting :: CheckResult -> Delay -> Tally -> Armed -> IO ()
    resting verdict d tally armed = do
        say (NextLook act verdict (delayMicros d))
        w <- napWatching ctx armed (delayMicros d)
        told <- announce ctx w
        case w of
            Halt -> pure ()
            Ended _ -> pure ()
            Rearm dep var e -> resting verdict d tally (Map.insert dep (var, e) armed)
            Demote dep -> do
                sending <- demote dep tally
                case sending of
                    Just go -> go
                    Nothing -> resting verdict d tally (Map.delete dep armed)
            -- 'Pause' is read before 'Force'/'Satisfy', so a flush holding
            -- both contradictory things does the lesser: stop tending, and
            -- let the operator say what they meant.
            Told _ ->
                paused ctx told (resting verdict d tally armed) $
                    case override told of
                        Just Force -> attempt told (Regardless (Failure "forced")) tally
                        Just Satisfy -> satisfy
                        _ -> look (soonIf told d) tally armed
            Elapsed -> look d tally armed

    {- | A demoting dependency has moved. Either this node is going back to
    'WaitUp', or it was sent back too recently for a second departure to be a
    change rather than a flap.

    A departure that is not acted on leaves the dependency /disarmed/ rather
    than armed where it was — so that this node is not woken by the same
    departure again, and so that what it eventually re-arms at is where the
    dependency ended up rather than where it was before it moved. Both
    callers do that; only whether they run the result or hand it back
    differs. -}
    demote :: Ref -> Tally -> IO (Maybe (IO ()))
    demote dep tally = do
        now <- getMonotonicTimeNSec
        pure $
            if tooSoon policy now tally
                then Nothing
                else Just (demoting dep now tally)

    {- | Going back to 'WaitUp', to be brought up again on top of whatever the
    dependency that sent this node back becomes.

    No 'markFailed': being demoted is not failing, and 'Transient' is already
    enough to hold this node's own dependants. That is also what carries the
    cascade — a dependant of /this/ node that opted in sees exactly what this
    node just saw. -}
    demoting :: Ref -> Word64 -> Tally -> IO ()
    demoting dep now tally = do
        say (Demoted act dep)
        unsettle status TurnUp
        waitUp tally{tallyDemotedAt = Just now} []

    {- | Look, and either carry on resting or go back to 'Upping'. The policy
    is consulted before 'satisfiedBy' rather than after, which is the only
    way 'Salmon.Op.Supervision.Always' can act on a 'Completed' node — that
    verdict /is/ satisfied, and the whole of what @Always@ means is "run it
    again anyway". -}
    look :: Delay -> Tally -> Armed -> IO ()
    look d tally armed = do
        verdict <- runCheck act
        touch status
        if restarts policy verdict
            then attempt [] (Regardless verdict) tally
            else do
                -- either still up, or stopped being up with a policy that
                -- says leave it: settled either way, but 'statusCheck'
                -- carries which, and so does the report.
                --
                -- A node that has actually stopped being up counts as
                -- failing, so a dependant still in 'WaitUp' holds off rather
                -- than being brought up on top of it. Only an outright
                -- 'Failure' qualifies: marking 'Unknown' would strand the
                -- dependants of every node that has no check at all.
                case verdict of
                    Failure _ -> markFailed ctx
                    _ -> markOk ctx
                settle status verdict
                resting verdict (relaxed d) tally armed

    {- | The node did not get up, or stopped being up and is wanted back.
    Counts the failure, and either backs off and tries again or latches off. -}
    failed :: CheckResult -> Tally -> IO ()
    failed why tally = do
        now <- getMonotonicTimeNSec
        let tally' = countFailure policy now tally
        markFailed ctx
        settle status why
        if exhausted policy tally'
            then gaveUp why tally'
            else retryUp why (backoff tally'.tallyFailures) tally'

    -- | Wait out the backoff, then have another go.
    retryUp :: CheckResult -> Delay -> Tally -> IO ()
    retryUp why d tally = do
        say (NextLook act why (delayMicros d))
        w <- naptime ctx (delayMicros d)
        told <- announce ctx w
        case w of
            Halt -> pure ()
            Ended _ -> pure ()
            Demote _ -> retryUp why d tally
            Rearm{} -> retryUp why d tally
            Told _ ->
                paused ctx told (retryUp why d tally) $
                    case override told of
                        Just Satisfy -> satisfy
                        _ -> attempt told Consult tally
            Elapsed -> attempt told Consult tally

    {- | This many consecutive failures was the node author's limit, so stop
    trying and stay out of the way.

    Parked rather than exited, for two reasons: the node's dependants have to
    keep seeing it settled-and-failing, and an operator has to be able to
    change their mind. 'Force' or 'Recheck' starts it over with a clean
    tally. -}
    gaveUp :: CheckResult -> Tally -> IO ()
    gaveUp why tally = do
        say (GaveUp act tally.tallyFailures)
        loop
      where
        loop = do
            w <- atomically (halting HeedHalt ctx (listen ctx retry))
            told <- announce ctx w
            case w of
                Halt -> pure ()
                Ended _ -> pure ()
                Demote _ -> loop
                Rearm{} -> loop
                Elapsed -> loop
                Told _
                    | told `has` Force -> attempt told (Regardless why) freshTally
                    | told `has` Recheck -> attempt told Consult freshTally
                    | Just Satisfy <- override told -> satisfy
                    | otherwise -> loop

    {- | An operator said "treat this as done". Settled without acting, and
    still tended: the instruction satisfies this attempt, it does not stop
    the node being looked after. 'Salmon.Op.Mailbox.Pause' is the one that
    does that. -}
    satisfy :: IO ()
    satisfy = do
        say (Acted (UpDown.Skip act))
        markOk ctx
        settle status Skipped
        say (Upkeep act Up)
        entering Skipped (Delay delayCap) freshTally

{- | @WaitDown -> Downing -> Down@. 'Down' is terminal: nothing in the model
answers "is it still gone", so there is nothing to poll for.
-}
downkeep ::
    forall ext.
    ( HasField "down" ext (IO ())
    , HasField "ref" ext Ref
    ) =>
    Standing ->
    Ctx ext ->
    IO ()
downkeep standing ctx =
    case standing of
        Unsettled -> waitDown
        -- already down. 'Down' is terminal, so this machine is done before
        -- it starts; it settles only so that its dependencies may go too.
        Settled -> finished Skipped
  where
    act = ctxAct ctx
    say = ctxSay ctx
    status = ctxStatus ctx

    waitDown :: IO ()
    waitDown = do
        say (Downkeep act WaitDown)
        loop
      where
        loop = do
            w <- standby ctx TurnDown
            told <- announce ctx w
            case w of
                Halt -> pure ()
                Ended _ -> pure ()
                -- a node coming down is not up, so nothing can demote it.
                Demote _ -> loop
                Rearm{} -> loop
                Told _ ->
                    paused ctx told loop $
                        case override told of
                            Just Satisfy -> finished Skipped
                            _ -> loop
                Elapsed -> downing initialDelay

    downing :: Delay -> IO ()
    downing d = do
        say (Downkeep act Downing)
        unsettle status TurnDown
        say (Acted (UpDown.Eval act))
        note status "down"
        outcome <- try @SomeException act.extension.down
        case outcome of
            Right () -> do
                say (Acted (UpDown.Done act))
                finished Success
            Left e -> do
                let why = Failure (Text.pack (show e))
                say (Acted (UpDown.Failed act e))
                note status (Text.pack (show e))
                markFailed ctx
                settle status why
                retryDown why (relaxed d)

    retryDown :: CheckResult -> Delay -> IO ()
    retryDown why d = do
        say (NextLook act why (delayMicros d))
        w <- naptime ctx (delayMicros d)
        told <- announce ctx w
        case w of
            Halt -> pure ()
            Ended _ -> pure ()
            Demote _ -> retryDown why d
            Rearm{} -> retryDown why d
            Told _ ->
                paused ctx told (retryDown why d) $
                    case override told of
                        Just Satisfy -> finished Skipped
                        _ -> downing (soonIf told d)
            Elapsed -> downing d

    -- | Off the machine. The node's dependencies may now go down too.
    finished :: CheckResult -> IO ()
    finished verdict = do
        markOk ctx
        settle status verdict
        say (Downkeep act Down)

-------------------------------------------------------------------------------

{- | Block until the neighbours in the given direction have settled /and/
none of them is currently failing — or until an instruction arrives, or the
supervisor stops.

Waiting out a neighbour's failure rather than reporting
'Salmon.Actions.UpDown.Blocked' is the sharpest difference between this
driver and the one-shot ones; see the module header. Under nobody is
tending are not waited on at all.
-}
standby :: Ctx ext -> Direction -> IO Wake
standby ctx dir =
    atomically $
        halting HeedHalt ctx $
            listen ctx $ do
                u <- readTVar (ctxUnder ctx)
                let neighbours = case dir of
                        TurnUp -> underDependencies u
                        TurnDown -> underDependants u
                let watched = [n | n <- neighbours, Map.member n (underStatuses u)]
                waitStability dir Stable (mapMaybe (`Map.lookup` underStatuses u) watched)
                broken <- readTVar (underFailed u)
                if any (`Set.member` broken) watched then retry else pure Elapsed

{- | Sleep, unless an instruction arrives or the supervisor stops — so an
instruction is never queued behind a 60s nap.
-}
naptime :: Ctx ext -> Micros -> IO Wake
naptime ctx d = do
    timer <- registerDelay (unMicros d)
    atomically $
        halting HeedHalt ctx $
            listen ctx $ do
                over <- readTVar timer
                if over then pure Elapsed else retry

{- | 'naptime' for a node in 'Up': the nap and the mailbox as before, plus
any dependency that opted into demoting this node.
-}
napWatching :: Ctx ext -> Armed -> Micros -> IO Wake
napWatching ctx armed d = do
    timer <- registerDelay (unMicros d)
    atomically $
        halting HeedHalt ctx $
            crossing ctx armed $
                listen ctx $ do
                    over <- readTVar timer
                    if over then pure Elapsed else retry

{- | 'napWatching' for a machine holding a running action: plus the action's
own exit, and no 'Halt'.

Five things raced in one transaction, which is the shape §"Ordering is STM"
promised and the reason nothing here needs a scheduler: the exit wins as soon
as it happens, rather than being noticed at the end of a delay that may be a
minute long.
-}
naptimeHolding :: Ctx ext -> Async ExitCode -> Armed -> Micros -> IO Wake
naptimeHolding ctx running armed d = do
    timer <- registerDelay (unMicros d)
    atomically $
        halting IgnoreHalt ctx $
            crossing ctx armed $
                ended running $
                    listen ctx $ do
                        over <- readTVar timer
                        if over then pure Elapsed else retry

-- | Block until somebody says something. For a holding machine, which has no
-- other reason to stop waiting.
listenHolding :: Ctx ext -> IO [Instruction]
listenHolding ctx = do
    w <- atomically (listen ctx retry)
    case w of
        Told told -> pure told
        _ -> listenHolding ctx

{- | 'Halt' wins over everything, for a machine that is allowed to hear it: a
stopping supervisor is not negotiable. A machine holding a running effect is
not allowed to hear it — see 'Heed'.
-}
halting :: Heed -> Ctx ext -> STM Wake -> STM Wake
halting IgnoreHalt _ k = k
halting HeedHalt ctx k = do
    u <- readTVar (ctxUnder ctx)
    stop <- readTVar (underHalt u)
    if stop then pure Halt else k

-- | The held action stopping pre-empts the nap, though not an instruction
-- already waiting.
ended :: Async ExitCode -> STM Wake -> STM Wake
ended running k = k `orElse` (Ended <$> waitCatchSTM running)

{- | Wake when a dependency that declared 'Salmon.Op.Supervision.RestForOne'
crosses the line between ready and not.

__Skipped entirely for a node with no such dependency__, which is every node
until somebody opts one in. That is not an optimisation but the reason this
feature is affordable at all: the alternative — every node in a supervised
graph holding a live subscription to all of its dependencies' statuses — is
the thundering herd @specs\/per-node-state-machines.md@ warned about, and
here it simply does not exist.

The @quiet@ set is what turns level-triggered STM into edge detection. A
dependency that has already been handed over is not looked at again until it
is ready, at which point it comes back as 'Rearm'; without that, a node that
declined a demotion would be re-woken by the same unready dependency
immediately, forever. It is also how a node that has just entered 'Up' avoids
demoting itself over a dependency that has not come up yet — see 'disarmed'.
-}
crossing :: Ctx ext -> Armed -> STM Wake -> STM Wake
crossing ctx armed k = do
    u <- readTVar (ctxUnder ctx)
    case underDemoters u of
        [] -> k
        demoters -> k `orElse` edge u demoters
  where
    edge u demoters = do
        broken <- readTVar (underFailed u)
        crossings <- traverse (look u broken) demoters
        case catMaybes crossings of
            [] -> retry
            (w : _) -> pure w

    look u broken dep =
        -- a neighbour nobody is tending is never going to move, so it is
        -- never going to leave 'Up' either.
        case Map.lookup dep (underStatuses u) of
            Nothing -> pure Nothing
            Just var -> do
                now <- readyNow var broken dep
                pure $ case Map.lookup dep armed of
                    -- armed against a different machine: this node's
                    -- supervisor was replaced under it, so there is nothing
                    -- to compare and it re-arms rather than reacting.
                    Just (v, _) | v /= var -> Rearm dep var <$> now
                    -- armed, and exactly where it was left: nothing happened.
                    Just (_, was) | now == Just was -> Nothing
                    -- armed, and either moved since or currently failing.
                    Just _ -> Just (Demote dep)
                    -- not armed, and settled up: arm it where it is now.
                    Nothing -> Rearm dep var <$> now

{- | Where a neighbour is, if it is settled up and not currently failing —
the condition 'standby' blocks on, asked about one node, and answered with
the 'Salmon.Op.Status.statusEpoch' that says /which/ time it is settled.

That number rather than a 'Bool' is what makes a departure impossible to
miss. A dependency that fell over and recovered between two of this node's
waits is 'Stable' at both of them, and STM keeps no queue of what happened in
between — the epoch is the only thing left that remembers.
-}
readyNow :: TVar Status -> Set Ref -> Ref -> STM (Maybe Word64)
readyNow var broken dep = do
    st <- readTVar var
    pure $
        if st.statusStability == Stable
            && st.statusDirection == TurnUp
            && not (Set.member dep broken)
            then Just st.statusEpoch
            else Nothing

{- | Which of this node's demoting dependencies are ready at this instant,
and where each of them is — the ones that are not are left out, and so cannot
demote this node until they have been seen up at least once.

Taken afresh on every entry into 'Up' rather than remembered, because the two
places that matter are exactly the ones where this machine has not been
watching: a supervisor that has just started, and a node that has just been
put back.
-}
arming :: Ctx ext -> IO Armed
arming ctx =
    atomically $ do
        u <- readTVar (ctxUnder ctx)
        case underDemoters u of
            [] -> pure Map.empty
            demoters -> do
                broken <- readTVar (underFailed u)
                entries <- traverse (entry u broken) demoters
                pure (Map.fromList (catMaybes entries))
  where
    entry u broken dep =
        case Map.lookup dep (underStatuses u) of
            Nothing -> pure Nothing
            Just var -> fmap (\e -> (dep, (var, e))) <$> readyNow var broken dep

-- | Anything pending in the mailbox pre-empts whatever else this wait was for.
listen :: Ctx ext -> STM Wake -> STM Wake
listen ctx k = do
    told <- Mailbox.takeAll (ctxBox ctx)
    if null told then k else pure (Told told)

{- | Report what was said (and what was dropped to make room for it), and
hand it back. @[]@ for any wake that was not an instruction.
-}
announce :: Ctx ext -> Wake -> IO [Instruction]
announce _ Halt = pure []
announce _ Elapsed = pure []
announce _ (Ended _) = pure []
announce _ (Demote _) = pure []
announce _ (Rearm _ _ _) = pure []
announce ctx (Told told) = do
    total <- Mailbox.dropped (ctxBox ctx)
    fresh <- atomically $ do
        seen <- readTVar (ctxDrops ctx)
        writeTVar (ctxDrops ctx) total
        pure (total - seen)
    unless (fresh == 0) $ ctxSay ctx (Acted (UpDown.DroppedInstructions (ctxAct ctx) fresh))
    forM_ told $ \i -> ctxSay ctx (Acted (UpDown.Instructed (ctxAct ctx) i))
    pure told

{- | If the last thing said was 'Pause', stop tending until a 'Resume'
arrives (or until the supervisor stops) and then take @onResume@; otherwise
take @onwards@ immediately.

Pausing leaves the node's effect and its 'Status' exactly as they are: a
paused node still looks settled to its neighbours, which is the point —
pausing is about whether /we/ keep tending it, not about whether it is up.
Everything else in the mailbox still applies; only 'Pause' and 'Resume' are
read here.
-}
paused :: Ctx ext -> [Instruction] -> IO () -> IO () -> IO ()
paused ctx told onResume onwards =
    case tending told of
        Just Pause -> do
            ctxSay ctx (Paused (ctxAct ctx))
            hold
        _ -> onwards
  where
    hold = do
        w <- atomically (halting HeedHalt ctx (listen ctx retry))
        case w of
            Halt -> pure ()
            Ended _ -> pure ()
            Demote _ -> hold
            Rearm{} -> hold
            Elapsed -> hold
            Told ts -> do
                _ <- announce ctx (Told ts)
                case tending ts of
                    Just Resume -> do
                        ctxSay ctx (Resumed (ctxAct ctx))
                        onResume
                    _ -> hold

-- | The last 'Pause'/'Resume' said, if either was.
tending :: [Instruction] -> Maybe Instruction
tending told = case [t | t <- told, t == Pause || t == Resume] of
    [] -> Nothing
    xs -> Just (last xs)

-- | The last 'Force' or 'Satisfy' said, if either was: later supersedes
-- earlier, these being statements of current intent.
override :: [Instruction] -> Maybe Instruction
override told = case [t | t <- told, t == Force || t == Satisfy] of
    [] -> Nothing
    xs -> Just (last xs)

has :: [Instruction] -> Instruction -> Bool
has told i = i `elem` told

-- | 'Recheck' means "look now": the delay collapses to its floor.
soonIf :: [Instruction] -> Delay -> Delay
soonIf told d = if told `has` Recheck then initialDelay else d

-- | 'Success', 'Skipped' and 'Completed' all mean "the effect is in place";
-- see 'Salmon.Op.Supervision.Restart' on why 'Unknown' is in neither camp.
satisfiedBy :: CheckResult -> Bool
satisfiedBy Success = True
satisfiedBy Skipped = True
satisfiedBy Completed = True
satisfiedBy (Failure _) = False
satisfiedBy Unknown = False

-- | Does this policy put the node back, given what the check said?
restarts :: Supervision -> CheckResult -> Bool
restarts sup verdict =
    case (supRestart sup, verdict) of
        (Never, _) -> False
        (_, Failure _) -> True
        (Always, Completed) -> True
        _ -> False

markFailed :: Ctx ext -> IO ()
markFailed ctx = onFailures ctx (Set.insert (ctxRef ctx))

markOk :: Ctx ext -> IO ()
markOk ctx = onFailures ctx (Set.delete (ctxRef ctx))

-- | Through 'ctxUnder' rather than a captured 'TVar', so that an adopted
-- machine records what it is doing where its /current/ supervisor's
-- dependants read it.
onFailures :: Ctx ext -> (Set Ref -> Set Ref) -> IO ()
onFailures ctx f =
    atomically $ do
        u <- readTVar (ctxUnder ctx)
        modifyTVar' (underFailed u) f

-------------------------------------------------------------------------------

{- | One thread watching every node that declared a watchdog. Not started at
all when none did, which is the common case.

It scans rather than being woken, because "nothing has happened for N
seconds" is exactly the event no node can report about itself. A node is
reported 'Wedged' once per episode and 'Unwedged' when it moves again.

Reporting is all it does. Killing a wedged @up@ needs the
teardown-through-a-bracket that owning the process buys, which is the next
milestone; until then the operator is the one who decides.
-}
startWatchdog ::
    (Report ext -> IO ()) ->
    TVar Bool ->
    Map Ref (Machine ext) ->
    IO (Maybe (Async ()))
startWatchdog say halt machines
    | null watched = pure Nothing
    | otherwise = Just <$> async (loop Set.empty)
  where
    watched =
        [ (aref, m, w)
        | (aref, m) <- Map.toList machines
        , Just w <- [machineWatchdog m]
        ]

    -- often enough to notice promptly, rarely enough to cost nothing: half
    -- the shortest declared watchdog, clamped to [250ms, 5s].
    tick :: Micros
    tick =
        Micros
            . max (unMicros (millis 250))
            . min (unMicros (seconds 5))
            . (`div` 2)
            . minimum
            $ [unMicros w | (_, _, w) <- watched]

    loop :: Set Ref -> IO ()
    loop reported = do
        timer <- registerDelay (unMicros tick)
        stop <- atomically $ do
            halted <- readTVar halt
            if halted
                then pure True
                else do
                    over <- readTVar timer
                    if over then pure False else retry
        unless stop $ do
            now <- getMonotonicTimeNSec
            loop =<< sweep now reported

    sweep :: Word64 -> Set Ref -> IO (Set Ref)
    sweep now = go watched
      where
        go [] acc = pure acc
        go ((aref, m, w) : rest) acc = do
            st <- readTVarIO (machineStatus m)
            let bad = wedged now (Just (toNanos w)) st
            let was = Set.member aref acc
            acc' <- case (bad, was) of
                (True, False) -> do
                    say (Wedged (machineAct m) (silentFor now st))
                    pure (Set.insert aref acc)
                (False, True) -> do
                    say (Unwedged (machineAct m))
                    pure (Set.delete aref acc)
                _ -> pure acc
            go rest acc'

    silentFor :: Word64 -> Status -> Micros
    silentFor now st = Micros (fromIntegral ((now - statusLastActive st) `div` 1000))
