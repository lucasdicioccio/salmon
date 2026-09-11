{- | A single global knob capping how many nodes' @check@\/@up@\/@down@ run
at once across one traversal (R6 in
@specs/per-node-state-machines-remaining.md@).

"Salmon.Actions.Concurrent" runs one thread per node and lets 'STM' order
them: a node's thread blocks on its neighbours settling and then, once
unblocked, runs its own work immediately. That is deliberately unbounded —
the only thing standing between two nodes and running at once is an edge or a
collection (see the module's own header) — which is fine for two nodes
genuinely fighting over one resource (an edge fixes that) and is not what
this module is for. What it does not help with is unbounded /width/: a wide
DAG (many independent leaves — a large batch of files, say) spawns one
thread per leaf, and every one of those threads reaches its own 'IO' action
at once, which is a problem of machine capacity (CPU, file descriptors, an
outbound connection limit) rather than of any particular pair of nodes
sharing a resource.

A 'ConcurrencyLimit' is a cap on that width, orthogonal to the DAG's edges:
it says nothing about /order/ (edges and 'Salmon.Op.Status.waitStability'
still own that entirely) and everything about how many nodes may be
/inside their own action/ at the same moment. Optional throughout — a caller
that passes 'Nothing' pays nothing, not even a semaphore allocation, so
nothing about existing callers changes until one opts in.
-}
module Salmon.Op.Concurrency (
    ConcurrencyLimit,
    newConcurrencyLimit,
    withConcurrencyLimit,
) where

import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)

-- | A cap on how many actions gated by 'withConcurrencyLimit' may run at
-- once, shared across everyone holding this value — so one limit passed to
-- both a teardown pass and a bring-up pass bounds the two of them together,
-- not each separately.
newtype ConcurrencyLimit = ConcurrencyLimit QSem

{- | @n@ must be positive: a limit of zero would mean "run nothing", which
is not what a concurrency cap is for (that is what excluding every node from
the pass, or not running the pass at all, already says) and would instead
deadlock every gated action against a semaphore that can never be signalled.
Throws rather than silently building a limit nothing can ever pass.
-}
newConcurrencyLimit :: Int -> IO ConcurrencyLimit
newConcurrencyLimit n
    | n <= 0 = error ("Salmon.Op.Concurrency.newConcurrencyLimit: limit must be positive, got " <> show n)
    | otherwise = ConcurrencyLimit <$> newQSem n

{- | Run an 'IO' action, holding one slot of the limit for its duration.
'Nothing' means unbounded, matching the driver's behaviour before this
module existed.

Held only around the action itself, never around anything that waits on a
neighbour: "Salmon.Actions.Concurrent" already guarantees a node acquires no
slot until every node it depends on has settled and released its own, so
two nodes never hold a slot each while waiting on one another through this
mechanism — the only thing a wait can be for is a free slot, not another
node's turn.
-}
withConcurrencyLimit :: Maybe ConcurrencyLimit -> IO a -> IO a
withConcurrencyLimit Nothing act = act
withConcurrencyLimit (Just (ConcurrencyLimit sem)) act =
    bracket_ (waitQSem sem) (signalQSem sem) act
