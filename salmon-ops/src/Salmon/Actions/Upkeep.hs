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

= The watchdog

'Salmon.Op.Supervision.supWatchdog' is a node author saying how long their
node may go without doing anything observable. A single scanning thread
compares 'Salmon.Op.Status.statusLastActive' against it and reports
'Wedged' — once per episode, with 'Unwedged' when the node moves again. It
only ever /reports/: killing a wedged @up@ needs the teardown-through-a-bracket
that owning the process buys, which is the next milestone. If no node in the
dag declares a watchdog the thread is never started.
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

    -- * Reporting
    Report (..),
) where

import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, registerDelay, retry, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forM_, unless)
import Data.Dynamic (Dynamic)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Records (HasField)

import Salmon.Actions.UpDown (CheckResult (..), runCheck)
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..), Mailbox)
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.Ref (Ref)
import Salmon.Op.Status (Direction (..), Stability (..), Status (..), newStatus, note, settle, touch, unsettle, waitStability, wedged)
import Salmon.Op.Supervision (Micros (..), Restart (..), Supervision (..), millis, seconds, supervisionOf, toNanos)
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
    | -- | told to stop tending this node; its effect is left exactly as it is
      Paused !(Act ext)
    | Resumed !(Act ext)
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
    deriving (Show)


-------------------------------------------------------------------------------

-- | One node's machine, its observable state, and the way to talk to it.
data Machine ext = Machine
    { machineAct :: !(Act ext)
    , machineDirection :: !Direction
    , machineStatus :: !(TVar Status)
    , machineMailbox :: !Mailbox
    , machineWatchdog :: !(Maybe Micros)
    , machineThread :: !(Async ())
    }

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
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Reporter (Report ext) ->
    -- | which nodes to tend, how
    (Ref -> Maybe Tend) ->
    Dag ext ->
    IO (Supervisor ext)
startUpkeep report tend dag = do
    -- reports are serialised for the same reason the concurrent one-shot
    -- driver serialises them: the caller's reporter is not assumed
    -- thread-safe, and interleaved multi-line reports are unreadable.
    lock <- newMVar ()
    let say rep = withMVar lock (\() -> runReporter report rep)

    halt <- newTVarIO False
    failed <- newTVarIO (Set.empty :: Set Ref)

    forM_ untended $ \act -> say (Untended act)
    forM_ blocked $ \act -> say (Acted (UpDown.Blocked act))

    statuses <-
        Map.fromList
            <$> forM tended (\(aref, _, t) -> (,) aref <$> newStatus t.tendDirection)

    machines <- forM tended $ \(aref, act, t) -> do
        let (policy, ignored) = supervisionOf act.extension
        unless (null ignored) $ say (Policy act policy ignored)
        box <- Mailbox.newMailbox Mailbox.defaultCapacity
        drops <- newTVarIO 0
        let status = statuses Map.! aref
        let ctx =
                Ctx
                    { ctxSay = say
                    , ctxHalt = halt
                    , ctxFailed = failed
                    , ctxStatuses = statuses
                    , ctxDag = dag
                    , ctxRef = aref
                    , ctxAct = act
                    , ctxStatus = status
                    , ctxBox = box
                    , ctxDrops = drops
                    , ctxPolicy = policy
                    }
        thread <- async (machine t ctx)
        pure
            ( aref
            , Machine
                { machineAct = act
                , machineDirection = t.tendDirection
                , machineStatus = status
                , machineMailbox = box
                , machineWatchdog = supWatchdog policy
                , machineThread = thread
                }
            )

    let table = Map.fromList machines
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

{- | Ask every machine to stop, wait for it, and report how many stopped.

__Nothing is torn down.__ Stopping a supervisor stops /tending/ these nodes;
it does not run anybody's @down@. A node whose @up@ is in flight is waited
for rather than interrupted, because interrupting an @up@ halfway is how a
half-applied effect happens; a node that is merely napping stops at once.
-}
stopUpkeep :: Supervisor ext -> IO ()
stopUpkeep sup = do
    atomically (writeTVar (supHalt sup) True)
    traverse_ waitCatch (supWatch sup)
    forM_ (Map.elems (supMachines sup)) $ \m -> do
        outcome <- waitCatch (machineThread m)
        case outcome of
            Right () -> pure ()
            -- a machine is not supposed to be able to throw: `up` and
            -- `down` are caught inside it. If one does, that is this
            -- module's bug and not the node's, so it is reported as such.
            Left e -> supSay sup (Escaped (machineAct m) e)
    supSay sup (Retired (Map.size (supMachines sup)))

-- | 'startUpkeep' and 'stopUpkeep' as a bracket.
withUpkeep ::
    ( HasField "up" ext (IO ())
    , HasField "down" ext (IO ())
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Reporter (Report ext) ->
    (Ref -> Maybe Tend) ->
    Dag ext ->
    (Supervisor ext -> IO a) ->
    IO a
withUpkeep report tend dag =
    bracket (startUpkeep report tend dag) stopUpkeep

-------------------------------------------------------------------------------

-- | What one machine needs to do its job.
data Ctx ext = Ctx
    { ctxSay :: !(Report ext -> IO ())
    , ctxHalt :: !(TVar Bool)
    , ctxFailed :: !(TVar (Set Ref))
    , ctxStatuses :: !(Map Ref (TVar Status))
    , ctxDag :: !(Dag ext)
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
    | -- | the supervisor is stopping
      Halt
    deriving (Show)

machine ::
    ( HasField "up" ext (IO ())
    , HasField "down" ext (IO ())
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Tend ->
    Ctx ext ->
    IO ()
machine (Tend TurnUp standing) ctx = upkeep standing ctx
machine (Tend TurnDown standing) ctx = downkeep standing ctx

-------------------------------------------------------------------------------

{- | @WaitUp -> Upping -> Up@, and back to 'Upping' whenever the check says
the effect has gone and the policy says to put it back.
-}
upkeep ::
    forall ext.
    ( HasField "up" ext (IO ())
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Standing ->
    Ctx ext ->
    IO ()
upkeep standing ctx =
    case standing of
        Unsettled -> waitUp []
        -- already up: settle so dependants may go, and start watching. The
        -- verdict is 'Skipped' because that is exactly what it is — nobody
        -- looked, somebody said — and the first 'look' replaces it.
        Settled -> do
            markOk ctx
            settle status Skipped
            say (Upkeep act Up)
            resting Skipped initialDelay
  where
    act = ctxAct ctx
    say = ctxSay ctx
    status = ctxStatus ctx

    -- | Nothing to do until the dependencies are up. Instructions that
    -- arrive meanwhile are held rather than lost: a 'Force' typed at a node
    -- whose dependency is still coming up means "when you get there, act",
    -- not "act now against an unmet precondition".
    waitUp :: [Instruction] -> IO ()
    waitUp pending = do
        say (Upkeep act WaitUp)
        loop pending
      where
        loop held = do
            w <- standby ctx TurnUp (Dag.dependenciesOf (ctxDag ctx) (ctxRef ctx))
            told <- announce ctx w
            case w of
                Halt -> pure ()
                Told _ -> paused ctx told (loop (held <> told)) (loop (held <> told))
                Elapsed -> upping (held <> told) Consult initialDelay

    -- | Run @up@, unless the check (or an instruction) says not to.
    upping :: [Instruction] -> Intent -> Delay -> IO ()
    upping told intent d
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
                    reached verdict d
                Right _ -> do
                    say (Acted (UpDown.Eval act))
                    note status "up"
                    outcome <- try @SomeException act.extension.up
                    case outcome of
                        Right () -> do
                            say (Acted (UpDown.Done act))
                            reached Success d
                        Left e -> do
                            let why = Failure (Text.pack (show e))
                            say (Acted (UpDown.Failed act e))
                            note status (Text.pack (show e))
                            markFailed ctx
                            -- back off rather than tighten: this is a
                            -- failing action, not a vanished effect, and
                            -- retrying `apt-get` twice a second helps
                            -- nobody. See the module header.
                            settle status why
                            retryUp why (relaxed d)

    -- | Wait out the backoff, then have another go at @up@.
    retryUp :: CheckResult -> Delay -> IO ()
    retryUp why d = do
        say (NextLook act why (delayMicros d))
        w <- naptime ctx (delayMicros d)
        told <- announce ctx w
        case w of
            Halt -> pure ()
            Told _ -> paused ctx told (retryUp why d) (upping told Consult (soonIf told d))
            Elapsed -> upping told Consult d

    -- | The effect is in place. Keep an eye on it.
    reached :: CheckResult -> Delay -> IO ()
    reached verdict d = do
        markOk ctx
        settle status verdict
        say (Upkeep act Up)
        resting verdict (relaxed d)

    {- | 'Up': the steady state. Sleep, look, and adapt — back off while the
    effect is there, tighten and go back to 'Upping' when it is not. -}
    resting :: CheckResult -> Delay -> IO ()
    resting verdict d = do
        say (NextLook act verdict (delayMicros d))
        w <- naptime ctx (delayMicros d)
        told <- announce ctx w
        case w of
            Halt -> pure ()
            -- 'Pause' is read before 'Force'/'Satisfy', so a flush holding
            -- both contradictory things does the lesser: stop tending, and
            -- let the operator say what they meant.
            Told _ ->
                paused ctx told (resting verdict d) $
                    case override told of
                        Just Force -> upping told (Regardless (Failure "forced")) initialDelay
                        Just Satisfy -> satisfy
                        _ -> look (soonIf told d)
            Elapsed -> look d

    {- | Look, and either carry on resting or go back to 'Upping'. The
    policy is consulted before 'satisfiedBy' rather than after, which is
    the only way 'Salmon.Op.Supervision.Always' can act on a
    'Salmon.Actions.UpDown.Completed' node — that verdict /is/ satisfied,
    and the whole of what @Always@ means is "run it again anyway". -}
    look :: Delay -> IO ()
    look d = do
        verdict <- runCheck act
        touch status
        if restarts (ctxPolicy ctx) verdict
            then upping [] (Regardless verdict) (attentive d)
            else do
                -- either still up, or stopped being up with a policy that
                -- says leave it: settled either way, but 'statusCheck'
                -- carries which, and so does the report.
                --
                -- A node that has actually stopped being up counts as
                -- failing, so a dependant still in 'WaitUp' holds off rather
                -- than being brought up on top of it. Only an outright
                -- 'Salmon.Actions.UpDown.Failure' qualifies: marking
                -- 'Salmon.Actions.UpDown.Unknown' would strand the
                -- dependants of every node that has no check at all.
                case verdict of
                    Failure _ -> markFailed ctx
                    _ -> markOk ctx
                settle status verdict
                resting verdict (relaxed d)

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
        resting Skipped (Delay delayCap)

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
            w <- standby ctx TurnDown (Dag.dependantsOf (ctxDag ctx) (ctxRef ctx))
            told <- announce ctx w
            case w of
                Halt -> pure ()
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
driver and the one-shot ones; see the module header. Neighbours nobody is
tending are not waited on at all.
-}
standby :: Ctx ext -> Direction -> [Ref] -> IO Wake
standby ctx dir neighbours =
    atomically $
        halting ctx $
            listen ctx $ do
                waitStability dir Stable vars
                broken <- readTVar (ctxFailed ctx)
                if any (`Set.member` broken) watched then retry else pure Elapsed
  where
    watched = [n | n <- neighbours, Map.member n (ctxStatuses ctx)]
    vars = mapMaybe (`Map.lookup` ctxStatuses ctx) watched

{- | Sleep, unless an instruction arrives or the supervisor stops — so an
instruction is never queued behind a 60s nap.
-}
naptime :: Ctx ext -> Micros -> IO Wake
naptime ctx d = do
    timer <- registerDelay (unMicros d)
    atomically $
        halting ctx $
            listen ctx $ do
                over <- readTVar timer
                if over then pure Elapsed else retry

-- | 'Halt' wins over everything: a stopping supervisor is not negotiable.
halting :: Ctx ext -> STM Wake -> STM Wake
halting ctx k = do
    stop <- readTVar (ctxHalt ctx)
    if stop then pure Halt else k

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
        w <- atomically (halting ctx (listen ctx retry))
        case w of
            Halt -> pure ()
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
markFailed ctx = atomically (modifyTVar' (ctxFailed ctx) (Set.insert (ctxRef ctx)))

markOk :: Ctx ext -> IO ()
markOk ctx = atomically (modifyTVar' (ctxFailed ctx) (Set.delete (ctxRef ctx)))

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
