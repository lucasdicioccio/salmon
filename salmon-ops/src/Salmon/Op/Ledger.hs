{-# LANGUAGE ScopedTypeVariables #-}

{- | What each declaration still wants: a set of nodes and a set of edges per
declaration, and nothing else.

This is the second half of @specs\/per-node-state-machines.md@'s "four
structures" — "Salmon.Op.Dag" holds what a node /is/, and this holds who
still wants it. Together they replace keeping a per-declaration graph around,
which is what "Salmon.Actions.Serve" used to do and what made its storage
grow with the shape of what had been declared rather than with how much was
declared.

= Why a set and not a count

Refcounting each node is the tempting implementation and it is wrong in four
separate ways that a set gets structurally:

* a node reached by several paths within one graph double-counts — here it is
  a 'Data.Set.Set', so one membership;
* a @down@ of something never up drives a count negative — here a retraction
  of an absent key is a no-op;
* re-declaring the same seed takes its count to 2, so one @down@ strands it
  up — here the same key replaces, rather than adds;
* two declarations wanting one node must not cancel each other out — here
  they union, and the node leaves when the last set does.

The one thing counting buys is @O(1)@ lookup per node, which is not worth
having: declarations are rare (a human, or a control plane, types them),
while node state changes are the hot path and never touch the ledger at all.
So 'desired' is recomputed when a declaration changes, and memoised only if
it ever shows up in a profile.

= Why edges, and why retraction retires rather than deletes

Both are the same answer: edges have to be retractable, and a retracted
declaration's edges are needed /after/ it is retracted.

Needed after, because retracting is exactly when an edge matters most. Given
@A@ depends on @B@, both wanted down, that edge is the whole of what says
@A@ comes down before @B@ does; delete the contribution outright and the
teardown order goes with it. Hence 'contribLive': 'retract' clears the flag,
which takes the contribution out of 'desired' while leaving its edges in
'precedenceOf', and 'collect' drops it only once none of its nodes is still
standing.

Retractable, because a stale edge is not inert. In the one-shot traversals a
leftover edge would at worst re-walk something; in the supervised model the
spec builds on this, a stale @A → B@ where @B@ is no longer wanted leaves
@A@ waiting on a node that has settled and will never move again — a silent
deadlock, with no report and no failure, which is strictly worse than the
'Salmon.Actions.UpDown.Blocked' a traversal would have produced.
-}
module Salmon.Op.Ledger (
    -- * The structure
    Edge,
    Contribution (..),
    Ledger,
    emptyLedger,
    contribution,

    -- * Folding declarations in
    declare,
    retract,
    retractAll,
    retractOthers,

    -- * Reading it back
    desired,
    precedenceOf,
    knownRefs,
    liveKeys,
    isLive,
    liveCount,

    -- * Collection
    collect,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Salmon.Op.Dag (Dag, dagEdges)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)

-- | A precedence edge, @(dependency, dependant)@ — the dependency comes up
-- first and goes down last.
type Edge = (Ref, Ref)

{- | What one declaration asks for: which nodes, and in which order relative
to each other. Both flat sets, so this is bounded by the declaration's node
count and not by the shape or depth of the graph it came from.
-}
data Contribution = Contribution
    { contribRefs :: !(Set Ref)
    , contribEdges :: !(Set Edge)
    , contribLive :: !Bool
    -- ^ 'False' from 'retract' until 'collect' — the declaration no longer
    -- wants these nodes up, but its edges still say how to take them down.
    }
    deriving (Show, Eq)

{- | Keyed by whatever identifies a declaration. "Salmon.Actions.Serve" uses
the encoded directive, so that two spellings of the same desired state are
one declaration.
-}
type Ledger key = Map key Contribution

emptyLedger :: Ledger key
emptyLedger = Map.empty

-- | Read a declaration's contribution off the graph it evaluated to.
contribution :: Dag ext -> Contribution
contribution dag =
    Contribution
        { contribRefs = Map.keysSet (Dag.dagNodes dag)
        , contribEdges = dagEdges dag
        , contribLive = True
        }

-------------------------------------------------------------------------------

{- | Declare (or re-declare) one key. Replaces rather than accumulates: the
same key declared twice is one declaration, which is what makes a single
@down@ afterwards enough to retract it.
-}
declare :: (Ord key) => key -> Contribution -> Ledger key -> Ledger key
declare = Map.insert

{- | Retire a key: it stops contributing to 'desired' immediately, keeps
contributing to 'precedenceOf', and is dropped by 'collect' once its nodes
have settled. Retracting a key that was never declared is a no-op.
-}
retract :: (Ord key) => key -> Ledger key -> Ledger key
retract = Map.adjust (\c -> c{contribLive = False})

-- | @clear@: retire every declaration.
retractAll :: Ledger key -> Ledger key
retractAll = fmap (\c -> c{contribLive = False})

-- | @only@: retire every declaration but this one.
retractOthers :: (Ord key) => key -> Ledger key -> Ledger key
retractOthers k = Map.mapWithKey (\k' c -> if k' == k then c else c{contribLive = False})

-------------------------------------------------------------------------------

-- | Every node some /live/ declaration still asks for: what should be up.
desired :: Ledger key -> Set Ref
desired = Set.unions . fmap contribRefs . filter contribLive . Map.elems

{- | Every edge any declaration contributed, retiring ones included — see the
module header for why liveness is not consulted here.
-}
precedenceOf :: Ledger key -> Set Edge
precedenceOf = Set.unions . fmap contribEdges . Map.elems

-- | Every node any retained declaration mentions, whether or not it is still
-- wanted up. The nodes this ledger can still say something about.
knownRefs :: Ledger key -> Set Ref
knownRefs = Set.unions . fmap contribRefs . Map.elems

liveKeys :: (Ord key) => Ledger key -> Set key
liveKeys = Map.keysSet . Map.filter contribLive

isLive :: (Ord key) => key -> Ledger key -> Bool
isLive k = maybe False contribLive . Map.lookup k

liveCount :: Ledger key -> Int
liveCount = length . filter contribLive . Map.elems

{- | Drop the retired declarations that have nothing left to say. The
predicate answers "is this node still standing?" — for a convergence loop,
"still to be turned down". A live declaration is never collected, however
settled its nodes are: it is what keeps them up.
-}
collect :: (Ref -> Bool) -> Ledger key -> Ledger key
collect standing = Map.filter needed
  where
    needed c = contribLive c || any standing (contribRefs c)
