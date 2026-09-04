{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The collapse of an expanded 'Cofree' 'Graph' into a flat, 'Ref'-keyed DAG:
one representative per node, both adjacency directions, and a record of the
representatives that were replaced on the way.

This used to live inside "Salmon.Actions.UpDown".'Salmon.Actions.UpDown.downTreeWith',
which needed it for one reason only: teardown ordering ("a node is free once
its /last/ dependant is done") cannot be read off a structure that only knows
each node's predecessors. Lifting it out is what
@specs\/per-node-state-machines.md@ calls the /magma/ plus /precedence/, and
it is a prerequisite for everything else there, because the whole model in
that document is derived from folding declared graphs into state rather than
from walking one graph.

Three properties matter and none of them hold for the 'Cofree' itself:

* __It is keyed by 'Ref'.__ So folding a /second/ graph into an existing 'Dag'
  is a merge rather than a replacement ('mergeDag'), which is what lets a
  long-running driver take new declarations without rebuilding everything.
* __It carries dependants as well as dependencies.__ 'dagDependants' is
  maintained as edges are recorded, not recovered by a separate counting pass.
* __It is pure.__ 'expand' is the only effectful step; everything here is a
  fold over its result, so it can be unit-tested without running a node.

= Representatives, and what happens when two collide

A 'Ref' is /location-addressed/: 'Salmon.Op.Ref.mkRef' hashes a kind tag plus
an author-chosen identity key, and that key is deliberately not the node's
behaviour (@filecontents@ keys on the path alone). So an equal 'Ref' means
"the same effect site", not an equal node, and two declarations writing
different bytes to the same path collapse to one entry here.

__Last writer wins, and the loser is recorded.__ Last rather than first
because re-declaring a node is how an operator changes it, and first-wins
would make the newer declaration silently inert. Recorded because this fold is
the first thing in salmon that /can/ notice: @upTree@ dedupes by 'Ref' too,
but per-traversal and discarded at the end, so two declarations fighting over
one file is invisible today.

Deciding whether a replacement is a genuine conflict needs an equality, and
'Salmon.Builtin.Extension.Extension' has none — @up :: IO ()@ is not 'Eq'. So
'foldDag' takes the test as an argument, and 'sameRepresentative' is the
best one available: 'Representative', i.e. the fields that /are/ comparable.
That is a heuristic — it misses a node whose action changed behind an
identical description, and 'Dynamic' only renders its type — but it is
strictly more than the zero available today.

Note this needs no @instance Semigroup Extension@: choosing a representative
is not combining two.
-}
module Salmon.Op.Dag (
    -- * The structure
    Dag (..),
    emptyDag,
    dagOrder,
    dependenciesOf,
    dependantsOf,
    dagEdges,
    representativeOf,
    roots,
    leaves,

    -- * Building one
    foldDag,
    mergeDag,
    fromMagma,
    record,

    -- * Colliding representatives
    Conflict (..),
    Representative (..),
    representative,
    sameRepresentative,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Dynamic (Dynamic)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Records (HasField (..))

import Salmon.Op.Actions
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref

-------------------------------------------------------------------------------

{- | Every node seen so far, keyed by 'Ref', with both adjacency directions.

Invariant: every 'Ref' in 'dagOrder' is a key of 'dagNodes',
'dagDependencies' and 'dagDependants' (possibly mapping to an empty edge
list), and the three maps have exactly the keys 'dagOrder' lists.
-}
data Dag ext = Dag
    { dagNodes :: !(Map Ref (Act ext))
    -- ^ The magma: the current representative of each node — its
    -- 'Salmon.Op.Actions.shorthand' and its extension, never an
    -- @Op@. Deliberately not an @Op@: @Op@'s @predecessors@ field retains the
    -- whole expanded closure, so a map of them would retain every graph ever
    -- folded and bound nothing at all. All structure lives in the two
    -- adjacency maps.
    , dagDependencies :: !(Map Ref [Ref])
    -- ^ What each node depends on: its /effective/ predecessors, i.e. the
    -- nearest 'Ref'-carrying nodes below it, descending through 'Actionless'
    -- glue. Deduplicated, in first-seen order.
    , dagDependants :: !(Map Ref [Ref])
    -- ^ The transpose: what depends on each node. This is the direction a
    -- teardown needs and the one the 'Cofree' cannot answer.
    , dagOrderRev :: ![Ref]
    -- ^ 'dagOrder' reversed, which is how it is accumulated. Use 'dagOrder'.
    , dagConflicts :: ![Conflict ext]
    -- ^ Representatives replaced by a /differing/ one, newest first. Empty
    -- for the overwhelmingly common case of one node reached by several
    -- paths, which replaces a representative with an indistinguishable one.
    }

-- | A representative that lost to last-writer-wins, and the one that beat it.
data Conflict ext = Conflict
    { conflictRef :: !Ref
    , conflictKept :: !(Act ext)
    -- ^ The representative that replaced 'conflictReplaced'. Note a /later/
    -- write may have replaced this one in turn; @'dagNodes' dag '!'
    -- 'conflictRef'@ is the final answer.
    , conflictReplaced :: !(Act ext)
    }

emptyDag :: Dag ext
emptyDag = Dag Map.empty Map.empty Map.empty [] []

{- | Nodes in the order they were first seen — root-first, for a fold of a
single rooted graph. Only ever a tie-break: it is what makes a traversal that
is otherwise order-independent (which nodes are ready at the same time)
deterministic, and it is the key order of all three maps.
-}
dagOrder :: Dag ext -> [Ref]
dagOrder = reverse . dagOrderRev

-- | Total: a 'Ref' the 'Dag' has never seen simply depends on nothing.
dependenciesOf :: Dag ext -> Ref -> [Ref]
dependenciesOf dag r = Map.findWithDefault [] r (dagDependencies dag)

-- | Total, as 'dependenciesOf'. An empty list means the node is a teardown
-- starting point: nothing is standing on it.
dependantsOf :: Dag ext -> Ref -> [Ref]
dependantsOf dag r = Map.findWithDefault [] r (dagDependants dag)

{- | Every edge as a flat @(dependency, dependant)@ set — the shape
"Salmon.Op.Ledger" keeps per declaration, where it has to be unionable and
retractable rather than walkable.
-}
dagEdges :: Dag ext -> Set (Ref, Ref)
dagEdges dag =
    Set.fromList
        [ (d, r)
        | (r, ds) <- Map.toList (dagDependencies dag)
        , d <- ds
        ]

representativeOf :: Dag ext -> Ref -> Maybe (Act ext)
representativeOf dag r = Map.lookup r (dagNodes dag)

-- | The nodes nothing depends on, in first-seen order — where a teardown
-- starts, and (for a fold of a single rooted graph) normally just the root.
roots :: Dag ext -> [Ref]
roots dag = [r | r <- dagOrder dag, null (dependantsOf dag r)]

-- | The nodes that depend on nothing, in first-seen order — where a bring-up
-- starts. The mirror of 'roots', and the reason both adjacency directions are
-- kept rather than one being recovered on demand.
leaves :: Dag ext -> [Ref]
leaves dag = [r | r <- dagOrder dag, null (dependenciesOf dag r)]

-------------------------------------------------------------------------------

{- | Collapse an expanded graph.

The walk visits every occurrence of a node, so the edge sets are the union
over all of them and the representative is the last one seen. It does not
prune already-seen 'Ref's the way the collapse inside @downTreeWith@ used to:
that pruning dropped the edges of every occurrence after the first, which is
exactly what a merge must not do. The cost is a full walk of the expanded
'Cofree' — the same cost @upTree@'s @postOrderM@ already pays, and 'expand'
before it.

The first argument decides whether replacing a representative is worth
reporting: it answers "are these two the same node?", so 'True' records no
'Conflict'. Pass 'sameRepresentative' unless you have something better; pass
@\\_ _ -> True@ to opt out of conflict detection entirely.
-}
foldDag ::
    forall m ext.
    (HasField "ref" ext Ref) =>
    (Act ext -> Act ext -> Bool) ->
    Cofree Graph (OpGraph m (Actions ext)) ->
    Dag ext
foldDag same = go emptyDag
  where
    go ::
        Dag ext ->
        Cofree Graph (OpGraph m (Actions ext)) ->
        Dag ext
    go dag (x :< g) =
        case x.node of
            Actionless -> foldl' go dag (effPreds g)
            Actions act ->
                let aref = getField @"ref" act.extension
                    preds = effPreds g
                    predRefs = nubOrd [pr | p <- preds, Just pr <- [refOf p]]
                 in foldl' go (record same aref act predRefs dag) preds

    -- The effective predecessors of a node: the nearest 'Ref'-carrying
    -- subtrees below it, descending through 'Actionless' nodes, which are
    -- structural glue with no identity and nothing to run.
    effPreds ::
        Graph (Cofree Graph (OpGraph m (Actions ext))) ->
        [Cofree Graph (OpGraph m (Actions ext))]
    effPreds g = concatMap pick (toList g)
      where
        pick c@(x :< g') =
            case x.node of
                Actions _ -> [c]
                Actionless -> effPreds g'

    refOf :: Cofree Graph (OpGraph m (Actions ext)) -> Maybe Ref
    refOf (x :< _) =
        case x.node of
            Actions act -> Just (getField @"ref" act.extension)
            Actionless -> Nothing

{- | Add one node and its outgoing dependency edges, last-writer-wins.

Idempotent in the edges (they are a set) but not in the representative, which
is the point: this is where a re-declaration takes over a node.
-}
record ::
    (Act ext -> Act ext -> Bool) ->
    Ref ->
    Act ext ->
    [Ref] ->
    Dag ext ->
    Dag ext
record same aref act predRefs dag =
    Dag
        { dagNodes = Map.insert aref act (dagNodes dag)
        , dagDependencies = foldl' addDependency deps0 predRefs
        , dagDependants = foldl' addDependant dependants0 predRefs
        , dagOrderRev = order'
        , dagConflicts = conflicts'
        }
  where
    previous = Map.lookup aref (dagNodes dag)

    conflicts'
        | Just old <- previous, not (same old act) =
            Conflict aref act old : dagConflicts dag
        | otherwise = dagConflicts dag

    order'
        | Map.member aref (dagNodes dag) = dagOrderRev dag
        | otherwise = aref : dagOrderRev dag

    -- every node is a key of both maps, even with no edges either way.
    deps0 = Map.insertWith (\_ old -> old) aref [] (dagDependencies dag)
    dependants0 = Map.insertWith (\_ old -> old) aref [] (dagDependants dag)

    addDependency m d = Map.insertWith snoc aref [d] (Map.insertWith (\_ old -> old) d [] m)
    addDependant m d = Map.insertWith snoc d [aref] m

    -- append-if-absent, keeping first-seen order.
    snoc [new] old
        | new `elem` old = old
        | otherwise = old <> [new]
    snoc new old = old <> filter (`notElem` old) new

{- | Rebuild a 'Dag' from a magma and a flat edge set — the inverse of
'dagEdges', and how a driver that keeps nodes and precedence separately (as
"Salmon.Op.Ledger" does, because edges have to be retractable there) gets
back something it can walk.

Restricted to the magma: an edge naming a node the magma no longer holds is
dropped rather than resurrecting a node with no representative. 'dagOrder' is
the magma's key order, which is arbitrary but stable — the walk this feeds is
order-independent apart from tie-breaks.
-}
fromMagma :: Map Ref (Act ext) -> Set (Ref, Ref) -> Dag ext
fromMagma magma edges = foldl' add emptyDag (Map.keys magma)
  where
    -- \_ _ -> True: these representatives are already the survivors of
    -- whatever fold produced the magma, so there is no collision left to
    -- report here.
    add dag r = record (\_ _ -> True) r (magma Map.! r) (deps r) dag

    deps :: Ref -> [Ref]
    deps r = Map.findWithDefault [] r incoming

    incoming :: Map Ref [Ref]
    incoming =
        Map.fromListWith
            (flip (<>))
            [ (dependant, [dependency])
            | (dependency, dependant) <- Set.toList edges
            , Map.member dependency magma
            , Map.member dependant magma
            ]

{- | Fold the right 'Dag' into the left one: representatives from the right
win, edges and order accumulate. This is how a second declaration joins a
running world.
-}
mergeDag :: (Act ext -> Act ext -> Bool) -> Dag ext -> Dag ext -> Dag ext
mergeDag same into from = foldl' step into (dagOrder from)
  where
    step dag r =
        case representativeOf from r of
            Nothing -> dag
            Just act -> record same r act (dependenciesOf from r) dag

-------------------------------------------------------------------------------

{- | The part of a node two representatives can actually be compared on.

Everything an 'Salmon.Builtin.Extension.Extension' is /for/ — @up@, @check@,
@down@ — is a function and therefore outside any equality, so this is the
whole of the available evidence. 'Data.Dynamic.Dynamic' renders as its type
alone, so two 'Supervision' dynamics with different restart policies compare
equal here.
-}
data Representative = Representative
    { repShorthand :: !ShortHand
    , repHelp :: !Text
    , repNotes :: ![Text]
    , repDynamics :: ![String]
    }
    deriving (Show, Eq)

representative ::
    ( HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Act ext ->
    Representative
representative act =
    Representative
        { repShorthand = act.shorthand
        , repHelp = getField @"help" act.extension
        , repNotes = getField @"notes" act.extension
        , repDynamics = fmap show (getField @"dynamics" act.extension)
        }

-- | The default conflict test for 'foldDag': equality on 'Representative'.
sameRepresentative ::
    ( HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Act ext ->
    Act ext ->
    Bool
sameRepresentative a b = representative a == representative b

-------------------------------------------------------------------------------

-- | order-preserving dedup.
nubOrd :: (Ord b) => [b] -> [b]
nubOrd = go Set.empty
  where
    go _ [] = []
    go s (y : ys)
        | Set.member y s = go s ys
        | otherwise = y : go (Set.insert y s) ys
