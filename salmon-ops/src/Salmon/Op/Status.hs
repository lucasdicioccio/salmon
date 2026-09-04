{-# LANGUAGE ScopedTypeVariables #-}

{- | What a node's own thread publishes about itself, and how its neighbours
wait on it.

Until now a node had no state of its own: a traversal held the ordering in
counters on its own stack, and a node was whatever the traversal had most
recently done to it. Giving each node a 'TVar' 'Status' moves that
information to where the node is, which buys three things at once — ordering
becomes a blocking read rather than a counter, several nodes can be in flight
without a scheduler, and something outside the traversal (an operator, a
status command, a supervisor) can ask a node how it is doing while it is
doing it.

= Ordering is a blocking read

'waitStability' is the whole of dependency ordering:

* a node going up waits on its /dependencies/ being 'Stable' in 'TurnUp';
* a node coming down waits on its /dependants/ being 'Stable' in 'TurnDown'.

That single inversion replaces the counting the synchronous
'Salmon.Actions.UpDown.walk' does in either direction, and it is only
expressible because "Salmon.Op.Dag" kept both adjacency directions. STM
'retry' means no polling, no wakeup channel and no scheduler: a node blocks
until a neighbour's state actually changes.

= Settled, and separately, making progress

'Stability' is deliberately two-valued, because it is what 'waitStability'
blocks on and a richer value would wake dependants on every twitch. But two
values cannot tell a node that has been 'Transient' for four seconds because
it is building from one that has been 'Transient' for four seconds because it
is wedged — which is exactly the distinction a restart decision needs.

So progress is a monotonic timestamp rather than a third state: anything
observable a node does bumps 'statusLastActive' (a transition, a check
returning, a line of output arriving), and "wedged" is a derived predicate.
'wedged' takes the watchdog as an argument rather than assuming one, because
a @cabal build@ is legitimately silent for minutes and a web server's startup
is not — only the node's author knows which, and a node that declares no
watchdog is never considered wedged. Silence is evidence only once somebody
has said what silence would mean.

= Output is a bounded ring

'statusOutput' keeps the last few lines a node produced. A ring rather than a
buffer, because a chatty node would otherwise be quietly accumulated into the
heap. It pays for itself three times: it is what an operator wants to see when
a node has failed, it is what feeds 'statusLastActive', and it lets a crash
report carry the lines that preceded the crash rather than just an exit code.
-}
module Salmon.Op.Status (
    -- * Which way a node is wanted
    Direction (..),
    opposite,

    -- * Whether it has got there
    Stability (..),
    Status (..),
    newStatus,
    readStatus,

    -- * Publishing
    settle,
    unsettle,
    touch,
    note,

    -- * Waiting
    waitStability,
    settled,

    -- * Progress
    wedged,

    -- * The output ring
    Ring,
    emptyRing,
    ringSize,
    pushRing,
    ringLines,
) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Salmon.Actions.UpDown (CheckResult (..))

-- | Which way a node is currently wanted.
data Direction
    = TurnUp
    | TurnDown
    deriving (Show, Eq, Ord)

opposite :: Direction -> Direction
opposite TurnUp = TurnDown
opposite TurnDown = TurnUp

{- | Whether a node has finished moving. Two-valued on purpose: this is what
'waitStability' blocks on, so a value that changed often would wake every
dependant every time it did.
-}
data Stability
    = Stable
    | Transient
    deriving (Show, Eq, Ord)

data Status = Status
    { statusCheck :: !CheckResult
    -- ^ the node's own last word on its effect.
    , statusDirection :: !Direction
    , statusStability :: !Stability
    , statusLastActive :: !Word64
    -- ^ monotonic nanoseconds at the node's last observable activity. Only
    -- ever compared against a later reading of the same clock.
    , statusOutput :: !Ring
    }
    deriving (Show)

{- | A node's status before its machine starts, and deliberately 'Transient'.

Initialising 'Stable' would let every dependant proceed before the node had
done anything at all — one line, and otherwise the kind of thing that only
shows up as a heisenbug on a wide graph.
-}
newStatus :: Direction -> IO (TVar Status)
newStatus dir = do
    now <- getMonotonicTimeNSec
    newTVarIO (Status Unknown dir Transient now emptyRing)

readStatus :: TVar Status -> IO Status
readStatus = readTVarIO

-------------------------------------------------------------------------------

-- | The node has finished moving, with this as its last word.
settle :: TVar Status -> CheckResult -> IO ()
settle var result = do
    now <- getMonotonicTimeNSec
    atomically $
        modifyTVar' var $ \st ->
            st{statusCheck = result, statusStability = Stable, statusLastActive = now}

{- | The node is moving again — and, if the direction changed, moving the
other way, which resets what it has to say about itself.
-}
unsettle :: TVar Status -> Direction -> IO ()
unsettle var dir = do
    now <- getMonotonicTimeNSec
    atomically $
        modifyTVar' var $ \st ->
            st
                { statusDirection = dir
                , statusStability = Transient
                , statusLastActive = now
                , statusCheck = if statusDirection st == dir then statusCheck st else Unknown
                }

-- | Record activity without changing anything else: the node is still doing
-- whatever it was doing, and is not wedged.
touch :: TVar Status -> IO ()
touch var = do
    now <- getMonotonicTimeNSec
    atomically $ modifyTVar' var $ \st -> st{statusLastActive = now}

-- | A line of output (or of the node's own narration): into the ring, and
-- counts as activity.
note :: TVar Status -> Text -> IO ()
note var line = do
    now <- getMonotonicTimeNSec
    atomically $
        modifyTVar' var $ \st ->
            st{statusOutput = pushRing line st.statusOutput, statusLastActive = now}

-------------------------------------------------------------------------------

{- | Block until every one of these nodes has settled in the given direction.

The whole of dependency ordering. Pass a node's dependencies when it is going
up and its dependants when it is coming down; an empty list never blocks,
which is what makes a leaf start immediately.

Reads direction and stability only — never 'statusCheck' — so a node that
settled having failed is indistinguishable here from one that settled having
succeeded. That is deliberate: whether a dependant should proceed past a
failure is a policy the driver applies, not a property of the neighbour, and
the two drivers answer it differently (a one-shot pass reports
'Salmon.Actions.UpDown.Blocked' and moves on; a supervisor waits, because the
neighbour may yet be repaired).
-}
waitStability :: Direction -> Stability -> [TVar Status] -> STM ()
waitStability dir stab vars = do
    sts <- traverse readTVar vars
    if all ok sts then pure () else retry
  where
    ok :: Status -> Bool
    ok st = st.statusStability == stab && st.statusDirection == dir

-- | 'waitStability' for the common case: settled, in this direction.
settled :: Direction -> [TVar Status] -> STM ()
settled dir = waitStability dir Stable

{- | Has this node been silent for longer than its author said silence should
ever last? 'Nothing' for a watchdog means the node never declares itself
wedged, which is the default and the right one.

Three conditions, and the middle one is easy to leave out and wrong to. A
node is wedged if it has not settled, /has said something at least once/, and
has said nothing since. Without the middle condition a node sitting in
'Salmon.Actions.Upkeep.WaitUp' behind a slow dependency trips its own
watchdog, having never run at all: it is not silent, it has not started. The
node actually worth reporting there is the dependency, which /is/ doing
something and will trip its own.

A node's own machine notes its transitions into the ring precisely so this
has something to read.
-}
wedged :: Word64 -> Maybe Word64 -> Status -> Bool
wedged _ Nothing _ = False
wedged now (Just watchdogNs) st =
    st.statusStability == Transient
        && ringSize st.statusOutput > 0
        && now - st.statusLastActive > watchdogNs

-------------------------------------------------------------------------------

{- | The last few lines a node produced, newest first, dropping the oldest
once full.
-}
data Ring = Ring
    { ringCap :: !Int
    , ringHeld :: !Int
    , ringRev :: ![Text]
    }
    deriving (Show)

-- | A few hundred lines: enough to explain a failure, small enough that a
-- chatty node costs nothing. Anything wanting real logs should be shipping
-- them somewhere, which is a node of its own.
emptyRing :: Ring
emptyRing = Ring 256 0 []

ringSize :: Ring -> Int
ringSize = ringHeld

pushRing :: Text -> Ring -> Ring
pushRing line ring
    | ring.ringHeld < ring.ringCap = ring{ringHeld = ring.ringHeld + 1, ringRev = line : ring.ringRev}
    | otherwise = ring{ringRev = line : dropLast ring.ringRev}
  where
    dropLast xs = take (length xs - 1) xs

-- | Oldest first, the way one would read them.
ringLines :: Ring -> [Text]
ringLines = reverse . ringRev
