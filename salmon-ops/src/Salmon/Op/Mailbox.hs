{- | One bounded mailbox per node: the things a node must be told, as opposed
to the things it can work out by looking at its neighbours.

"Salmon.Op.Status" covers everything derivable from the graph — a node pulls
its neighbours' state with 'Salmon.Op.Status.waitStability' and needs nobody
to tell it anything. Instructions are the complement: statements an operator
makes that are not a property of any neighbour at all.

@
Force    -- run @up@ even though @check@ says it need not; the operator knows
            something the check does not
Satisfy  -- treat as satisfied without acting
Recheck  -- collapse the adaptive delay to its floor and look now
Pause    -- stop tending this node, without tearing its effect down
Resume   -- start again
@

= Why a mailbox rather than replacing the node

The alternative is to swap the node's definition (and its running machine) for
a decorated one. That cannot express a /transient/ instruction without killing
and restarting the machine, which for a node that owns a process means killing
a healthy process in order to set a flag.

Swapping keeps exactly one narrow job, and it is not this one: when the fold
replaces a node's representative under last-writer-wins, the machine started
from the old representative has to go. That is a fold-time event. Everything
an operator wants to /say/ comes through here.

There is a third channel, and keeping the three apart is the point: a
'Salmon.Actions.Query.Plan' is part of a declaration, so its exclusions are
applied as the graph is folded — the node enters with its check pre-answered
and no running machine is disturbed. Declaration-time forcing is decoration;
run-time forcing is a mailbox.

= Bounded, dropping the oldest, and saying so

Bounded because a control plane can outrun a node that is busy doing something
slow, and an unbounded mailbox turns a wedged node into a memory leak.

Dropping the /oldest/ because these are statements about current intent — if
one has to go, the stale one is the one to lose. A drop is reported rather
than silent, or forcing a node becomes unreliable in a way nobody can see.

Provisional on purpose: whether drop-oldest is right, or whether a coalescing
mailbox (at most one pending instruction of each kind) would be better,
depends on how instructions actually get used, and there is no way to know
that before something is driving them.
-}
module Salmon.Op.Mailbox (
    Instruction (..),
    Mailbox,
    newMailbox,
    defaultCapacity,
    post,
    tryTake,
    takeAll,
    dropped,
) where

import Control.Concurrent.STM (STM, TBQueue, TVar, atomically, flushTBQueue, isFullTBQueue, modifyTVar', newTBQueueIO, newTVarIO, readTBQueue, readTVarIO, tryReadTBQueue, writeTBQueue)
import Numeric.Natural (Natural)

data Instruction
    = -- | act even though 'Salmon.Actions.UpDown.CheckResult' says otherwise
      Force
    | -- | treat as satisfied without acting. @specs\/per-node-state-machines.md@
      -- calls this @Skip@; renamed to keep it out of
      -- 'Salmon.Actions.UpDown.Report''s way, whose 'Salmon.Actions.UpDown.Skip'
      -- is what a node reports when it takes this instruction.
      Satisfy
    | -- | look now rather than at the end of the current delay
      Recheck
    | -- | stop tending this node, leaving its effect alone
      Pause
    | Resume
    deriving (Show, Eq, Ord)

data Mailbox = Mailbox
    { mailboxQueue :: !(TBQueue Instruction)
    , mailboxDropped :: !(TVar Int)
    }

-- | Small: a node with a dozen pending instructions has a control plane
-- problem, not a queueing one.
defaultCapacity :: Natural
defaultCapacity = 8

newMailbox :: Natural -> IO Mailbox
newMailbox cap = Mailbox <$> newTBQueueIO cap <*> newTVarIO 0

{- | Deliver an instruction, evicting the oldest if the mailbox is full.
Returns 'False' iff something was evicted to make room, which the caller is
expected to report.
-}
post :: Mailbox -> Instruction -> IO Bool
post box instruction = atomically $ do
    full <- isFullTBQueue box.mailboxQueue
    if full
        then do
            _ <- readTBQueue box.mailboxQueue
            modifyTVar' box.mailboxDropped (+ 1)
            writeTBQueue box.mailboxQueue instruction
            pure False
        else do
            writeTBQueue box.mailboxQueue instruction
            pure True

-- | The next instruction, if there is one. Never blocks: a node reads its
-- mailbox as one branch of a choice, not as its reason to wait.
tryTake :: Mailbox -> STM (Maybe Instruction)
tryTake = tryReadTBQueue . mailboxQueue

-- | Everything pending, oldest first. What a one-shot pass wants: it acts
-- once, so it needs the operator's whole say before it decides.
takeAll :: Mailbox -> STM [Instruction]
takeAll = flushTBQueue . mailboxQueue

-- | How many instructions this mailbox has evicted, ever.
dropped :: Mailbox -> IO Int
dropped = readTVarIO . mailboxDropped
