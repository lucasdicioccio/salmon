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
    stuck,

    -- * Building one
    foldDag,
    mergeDag,
    fromMagma,
    record,
    collapseInto,
    addEdge,
    transposeOf,

    -- * Colliding representatives
    Conflict (..),
    Representative (..),
    representative,
    sameRepresentative,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Dynamic (Dynamic, fromDynamic)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Records (HasField (..))

import Salmon.Op.Actions
import Salmon.Op.Graph
import Salmon.Op.Guard (Guard)
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Op.Supervision (Supervision)

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
    -- 'foldDag' records one per declaration that lost, each against the
    -- final representative; 'record' (and so 'mergeDag') one per
    -- replacement.
    }

-- | A representative that lost to last-writer-wins, and the one that beat it.
data Conflict ext = Conflict
    { conflictRef :: !Ref
    , conflictKept :: !(Act ext)
    -- ^ The representative that replaced 'conflictReplaced'. Note a /later/
    -- write may have replaced this one in turn (a merge; within one
    -- 'foldDag' it is always the final one); @'dagNodes' dag '!'
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

{- | The nodes that can never become ready: everything on a cycle, and
everything behind one.

@waitsOn@ is what a node waits for in the direction of interest —
'dependenciesOf' going up, 'dependantsOf' coming down. Kahn's algorithm, and
what is left over when it runs out of ready nodes is the answer.

Worth having because a 'Dag' assembled from a flat edge set /can/ describe a
cycle, unlike one folded from an expanded 'Control.Comonad.Cofree.Cofree':
two declarations can each contribute one leg of it. A driver that waits for
neighbours would wait forever on such a node, so it needs to know up front
which nodes those are rather than discovering it by hanging.
-}
stuck :: (Dag ext -> Ref -> [Ref]) -> Dag ext -> Set Ref
stuck waitsOn dag = go (Set.fromList (dagOrder dag))
  where
    go remaining =
        let ready =
                Set.filter
                    (\r -> all (`Set.notMember` remaining) (waitsOn dag r))
                    remaining
         in if Set.null ready
                then remaining
                else go (remaining `Set.difference` ready)

-------------------------------------------------------------------------------

{- | Collapse an expanded graph, visiting each node once.

= What is walked

An /occurrence/ is one cell of the expansion that carries a node. The
expansion is a tree, so a node reached by several paths has one occurrence
per path, and a chain of diamonds has exponentially many. This fold does not
walk them all. Of an occurrence it looks at three things, its /top/:

* its 'Ref';
* its act, as far as the conflict test (the first argument) can tell;
* the 'Ref's of its effective predecessors (the nearest nodes below it,
  through any 'Actionless' glue), deduplicated, in order.

An occurrence whose top has been walked already is __not walked again, and
neither is anything below it__. So the cost is one visit per distinct top
(one per node, in a graph where a node is declared one way) plus one look at
each of its predecessors, instead of one visit per path. It does not prune by
'Ref' alone, the way the collapse inside @downTreeWith@ used to: that dropped
the edges of a re-declaration with other predecessors, which is exactly what
a merge must not do.

= The precondition: coherent declarations

Skipping is only right if a second occurrence with the same top has the same
things below it. That is the precondition, and nothing here checks it (doing
so is the per-path walk this fold exists to avoid):

__Coherent__: whenever two occurrences have the same top, they have as many
effective predecessors as each other, and those have the same tops, one for
one in order. By induction, same top means same tree all the way down, up to
the conflict test.

A graph built by calling the same functions with the same arguments is
coherent, however many paths reach a node. Two declarations of one 'Ref' that
differ in their description or in their predecessors have different tops, are
both walked, and are still merged (edges) and reported (conflict) as before.

The conflict test is assumed to be an equivalence. 'sameRepresentative' is.

= What is the same as walking every occurrence, on a coherent expansion

* 'dagNodes' has the same keys, and each representative is one the conflict
  test cannot tell from the last occurrence in the tree (last writer wins,
  in the order of the tree, not in the order of this walk).
* 'dagDependencies' and 'dagDependants' are equal, order included, and so is
  'dagOrder'.
* The same 'Ref's are contested, by the same declarations.

= What is not the same

* __Conflicts are one per declaration that lost__, not one per change of
  representative. Walking every occurrence used to record a 'Conflict' each
  time the representative flipped, so a node and a differing declaration of
  it under a parent reached by @k@ paths gave @2k - 1@ of them. Now each
  contested 'Ref' with @n@ distinguishable declarations gives @n - 1@: the
  final representative as 'conflictKept', against each of the others as
  'conflictReplaced'. With two declarations that is the last flip the old
  walk recorded; with three or more, the old chain of flips (@b@ over @a@,
  @c@ over @b@) reads @c@ over @a@, @c@ over @b@.
* __On an expansion that is not coherent, what differs below two occurrences
  with the same top is not seen.__ The standard example: a unit declared
  twice with the same description, each over a file with the same path, the
  two files differing in content. The second unit is not walked, so the
  second file is never met: no 'Conflict' is recorded, and the first file is
  the representative where the last used to be. Nodes and edges that only
  exist below such an occurrence are missed in the same way. A difference in
  the description of the upper node (or in its predecessor 'Ref's) is enough
  to have both walked.
* With a conflict test that cannot tell two declarations apart (@\\_ _ ->
  True@ in particular), which of them is the representative is unspecified:
  that is what the test said.

The first argument decides whether two acts are the same node: 'True' records
no 'Conflict'. Pass 'sameRepresentative' unless you have something better;
pass @\\_ _ -> True@ to opt out of conflict detection entirely, at the price
of occurrences being told apart by 'Ref' and predecessors alone.

The expansion is only forced where it is walked, so over a lazily built one
(@Identity@, as 'Salmon.Builtin.Extension.evalDeps' builds) the skipped
occurrences are never built at all. An expansion built in a strict monad has
been paid for per path before this is called.
-}
foldDag ::
    forall m ext.
    (HasField "ref" ext Ref) =>
    (Act ext -> Act ext -> Bool) ->
    Cofree Graph (OpGraph m (Actions ext)) ->
    Dag ext
foldDag same root
    | Map.null contested = folded
    | otherwise =
        folded
            { dagNodes = Map.foldlWithKey' (\nodes r (winner, _) -> Map.insert r winner nodes) (dagNodes folded) verdicts
            , dagConflicts =
                reverse
                    [ Conflict r winner loser
                    | r <- dagOrder folded
                    , Just (winner, losers) <- [Map.lookup r verdicts]
                    , loser <- losers
                    ]
            }
  where
    walked :: Walk ext
    walked = forward (Walk (building emptyDag) Map.empty) root

    folded :: Dag ext
    folded = built (walkDag walked)

    variants :: Map Ref [Act ext]
    variants = Map.map (fmap fst) (walkMet walked)

    -- the refs declared in more than one distinguishable way.
    contested :: Map Ref [Act ext]
    contested = Map.filter (\declared -> length declared > 1) variants

    -- for each contested ref, the declaration that is last in the tree and
    -- the ones it beat, in first-met order.
    verdicts :: Map Ref (Act ext, [Act ext])
    verdicts = Map.intersectionWith verdict contested (lastWriters same variants contested root)

    verdict :: [Act ext] -> Int -> (Act ext, [Act ext])
    verdict declared winner =
        ( declared !! winner
        , [v | (i, v) <- zip [0 ..] declared, i /= winner]
        )

    -- Depth first, predecessors in order, as the walk of every occurrence
    -- was: on a coherent expansion a skipped occurrence would have recorded
    -- nothing new, so what is recorded, and in which order, is unchanged.
    forward :: Walk ext -> Cofree Graph (OpGraph m (Actions ext)) -> Walk ext
    forward w (x :< g) =
        case x.node of
            Actionless -> foldl' forward w (effPreds g)
            Actions act ->
                let aref = getField @"ref" act.extension
                    preds = effPreds g
                    predRefs = nubOrd [pr | p <- preds, Just pr <- [refOf p]]
                 in case meet same act predRefs (Map.findWithDefault [] aref (walkMet w)) of
                        Nothing -> w
                        Just met ->
                            foldl'
                                forward
                                Walk
                                    { -- only for the edges and the order: the
                                      -- representative and the conflicts of
                                      -- a contested ref are settled at the end.
                                      walkDag = recordInto (\_ _ -> True) aref act predRefs (walkDag w)
                                    , walkMet = Map.insert aref met (walkMet w)
                                    }
                                preds

data Walk ext = Walk
    { walkDag :: !(Building ext)
    , walkMet :: !(Map Ref [(Act ext, [[Ref]])])
    -- ^ The tops walked, by 'Ref': the declarations of it the conflict test
    -- tells apart, in first-met order, each with the predecessor lists it
    -- was met with. One declaration with one list, almost always.
    }

-- | A top among those met for its 'Ref': 'Nothing' if it was walked already,
-- otherwise the tops with it added.
meet ::
    forall ext.
    (Act ext -> Act ext -> Bool) ->
    Act ext ->
    [Ref] ->
    [(Act ext, [[Ref]])] ->
    Maybe [(Act ext, [[Ref]])]
meet same act predRefs = go
  where
    go :: [(Act ext, [[Ref]])] -> Maybe [(Act ext, [[Ref]])]
    go [] = Just [(act, [predRefs])]
    go ((v, walked) : rest)
        | same v act = if predRefs `elem` walked then Nothing else Just ((v, predRefs : walked) : rest)
        | otherwise = fmap ((v, walked) :) (go rest)

-- | What an occurrence is recognised by: its 'Ref', which of the
-- distinguishable declarations of that 'Ref' it is (by position, first met
-- first), and the 'Ref's it depends on.
type Top = (Ref, Int, [Ref])

-- | Which known declaration an act is, as far as the test can tell.
variantOf :: forall ext. (Act ext -> Act ext -> Bool) -> [Act ext] -> Act ext -> Maybe Int
variantOf same known act = go 0 known
  where
    go :: Int -> [Act ext] -> Maybe Int
    go _ [] = Nothing
    go i (v : vs)
        | same v act = Just i
        | otherwise = go (i + 1) vs

{- | For each contested 'Ref', which of its declarations is the last
occurrence in the tree: the one walking every occurrence would have ended on.

The forward walk cannot say: it skips occurrences, and the last one in the
tree may sit below a skipped one. So this walks the tree backwards (a node's
predecessors last to first, then the node), where the first occurrence met
of a 'Ref' is its last in the tree, and skips an occurrence whose top was
already walked for the same reason the forward walk does: on a coherent
expansion everything below it has been met. It stops as soon as every
contested 'Ref' is settled, and is not run at all when nothing is contested.
-}
lastWriters ::
    forall m ext a.
    (HasField "ref" ext Ref) =>
    (Act ext -> Act ext -> Bool) ->
    Map Ref [Act ext] ->
    Map Ref a ->
    Cofree Graph (OpGraph m (Actions ext)) ->
    Map Ref Int
lastWriters same variants contested root = snd (backward (Set.empty, Map.empty) root)
  where
    backward :: (Set Top, Map Ref Int) -> Cofree Graph (OpGraph m (Actions ext)) -> (Set Top, Map Ref Int)
    backward acc@(tops, found) (x :< g)
        | Map.size found == Map.size contested = acc
        | otherwise =
            case x.node of
                Actionless -> foldl' backward acc (reverse (effPreds g))
                Actions act ->
                    let aref = getField @"ref" act.extension
                        preds = effPreds g
                        predRefs = nubOrd [pr | p <- preds, Just pr <- [refOf p]]
                        -- always found: the forward walk classified this
                        -- occurrence, or one with the same top.
                        variant = fromMaybe 0 (variantOf same (Map.findWithDefault [] aref variants) act)
                        top = (aref, variant, predRefs)
                     in if Set.member top tops
                            then acc
                            else
                                let (tops', found') = foldl' backward (tops, found) (reverse preds)
                                    found''
                                        | Map.member aref contested = Map.insertWith (\_ earlier -> earlier) aref variant found'
                                        | otherwise = found'
                                 in (Set.insert top tops', found'')

-- | The effective predecessors of a node: the nearest 'Ref'-carrying
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

refOf :: (HasField "ref" ext Ref) => Cofree Graph (OpGraph m (Actions ext)) -> Maybe Ref
refOf (x :< _) =
    case x.node of
        Actions act -> Just (getField @"ref" act.extension)
        Actionless -> Nothing

{- | Add one node and its outgoing dependency edges, last-writer-wins.

Idempotent in the edges (they are a set) but not in the representative, which
is the point: this is where a re-declaration takes over a node.

One call costs the edge lists it touches (the node's own and each
dependency's dependants), once each. The functions here that record many
nodes in a row ('foldDag', 'mergeDag', 'fromMagma') do not go through this
one: they keep a 'Building' across the whole run, so a node gaining many
edges over many calls pays for each edge once rather than for its whole list
every time.
-}
record ::
    (Act ext -> Act ext -> Bool) ->
    Ref ->
    Act ext ->
    [Ref] ->
    Dag ext ->
    Dag ext
record same aref act predRefs dag = built (recordInto same aref act predRefs (building dag))

{- | A 'Dag' being built by a run of 'record's.

'dagDependencies' and 'dagDependants' are lists in first-seen order without
repeats. Adding an edge to such a list means asking whether it is there and
putting it at the end, both a walk of the list, so a node gaining @k@ edges
one at a time costs @k^2@ (the profiles' finding 3: a fan of 10 000 took
1.3 s to fold). While building, an edge list that has been touched is kept
as an 'Edges' instead, which answers both in @log k@; lists nobody touched
stay where they are in the 'Dag', so building on a large 'Dag' costs what is
touched, not the 'Dag'. 'built' turns the touched ones back into lists,
which are the same lists, in the same order, that appending one at a time
gives.

Invariant: a key of either overlay is a key of the matching map of the
finished 'Dag' (it takes precedence over the 'Dag''s own entry, if any).
-}
data Building ext = Building
    { buildingDag :: !(Dag ext)
    -- ^ nodes, order and conflicts are current; of the two edge maps, only
    -- the entries absent from the overlays below are.
    , buildingDependencies :: !(Map Ref Edges)
    , buildingDependants :: !(Map Ref Edges)
    }

-- | One edge list while building: its members, and the list itself reversed.
data Edges = Edges !(Set Ref) ![Ref]

noEdges :: Edges
noEdges = Edges Set.empty []

edgesFromList :: [Ref] -> Edges
edgesFromList xs = Edges (Set.fromList xs) (reverse xs)

edgesToList :: Edges -> [Ref]
edgesToList (Edges _ rev) = reverse rev

-- | Add at the end unless already present: what @old <> [new]@ after an
-- @elem@ test did.
snocEdge :: Ref -> Edges -> Edges
snocEdge r e@(Edges members rev)
    | Set.member r members = e
    | otherwise = Edges (Set.insert r members) (r : rev)

building :: Dag ext -> Building ext
building dag = Building dag Map.empty Map.empty

built :: Building ext -> Dag ext
built b =
    (buildingDag b)
        { dagDependencies = settle (buildingDependencies b) (dagDependencies (buildingDag b))
        , dagDependants = settle (buildingDependants b) (dagDependants (buildingDag b))
        }
  where
    settle overlay base
        | Map.null overlay = base
        | otherwise = Map.union (Map.map edgesToList overlay) base

-- | The edge list at a key, as an 'Edges', from the overlay or the 'Dag'.
edgesAt :: Map Ref Edges -> Map Ref [Ref] -> Ref -> Edges
edgesAt overlay base r =
    case Map.lookup r overlay of
        Just e -> e
        Nothing -> maybe noEdges edgesFromList (Map.lookup r base)

-- | Make @r@ a key, with no edges unless it has some.
ensureKey :: Map Ref [Ref] -> Ref -> Map Ref Edges -> Map Ref Edges
ensureKey base r overlay
    | Map.member r overlay || Map.member r base = overlay
    | otherwise = Map.insert r noEdges overlay

-- | 'record', on a 'Building'.
recordInto ::
    (Act ext -> Act ext -> Bool) ->
    Ref ->
    Act ext ->
    [Ref] ->
    Building ext ->
    Building ext
recordInto same aref act predRefs (Building dag depsOverlay dependantsOverlay) =
    Building
        { buildingDag =
            dag
                { dagNodes = Map.insert aref act (dagNodes dag)
                , dagOrderRev = order'
                , dagConflicts = conflicts'
                }
        , buildingDependencies = deps'
        , buildingDependants = dependants'
        }
  where
    baseDeps = dagDependencies dag
    baseDependants = dagDependants dag

    previous = Map.lookup aref (dagNodes dag)

    conflicts'
        | Just old <- previous, not (same old act) =
            Conflict aref act old : dagConflicts dag
        | otherwise = dagConflicts dag

    order'
        | Map.member aref (dagNodes dag) = dagOrderRev dag
        | otherwise = aref : dagOrderRev dag

    -- every node is a key of both maps, even with no edges either way, and
    -- so is every dependency, before it is recorded itself. The node's own
    -- list is read once and extended in order; a dependency equal to the
    -- node (a self-edge) lands in that same list, as it did.
    deps' =
        let withKeys = foldl' (flip (ensureKey baseDeps)) depsOverlay predRefs
            own = foldl' (flip snocEdge) (edgesAt withKeys baseDeps aref) predRefs
         in Map.insert aref own withKeys

    dependants' =
        foldl'
            (\m d -> Map.insert d (snocEdge aref (edgesAt m baseDependants d)) m)
            (ensureKey baseDependants aref dependantsOverlay)
            predRefs

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
fromMagma magma edges = built (Map.foldlWithKey' add (building emptyDag) magma)
  where
    -- \_ _ -> True: these representatives are already the survivors of
    -- whatever fold produced the magma, so there is no collision left to
    -- report here.
    add b r act = recordInto (\_ _ -> True) r act (deps r) b

    deps :: Ref -> [Ref]
    deps r = Map.findWithDefault [] r incoming

    -- each list in edge-set order: built by consing (what 'Map.fromListWith'
    -- does with '(<>)' on singletons) and reversed once, rather than by
    -- appending on the right, which costs a node's whole list per edge.
    incoming :: Map Ref [Ref]
    incoming =
        Map.map reverse . Map.fromListWith (<>) $
            [ (dependant, [dependency])
            | (dependency, dependant) <- Set.toList edges
            , Map.member dependency magma
            , Map.member dependant magma
            ]

{- | Replace a set of nodes with one node that stands in for all of them,
redirecting every edge that touched a member onto the replacement.

This is what a collection rewrite needs and the only structural edit this
module offers: "twenty @apt-get install@ nodes become one @apt-get install@
node, and whatever depended on any of them now depends on that one". Edges
purely between members collapse to self-edges and are dropped, which is the
whole reason this cannot be done by editing the magma alone.

The replacement takes the position of the first member in 'dagOrder', so a
collection lands where its members were rather than at the end. If no member
is present the 'Dag' is returned unchanged — a rewrite that finds nothing to
do is a no-op, not an empty node.
-}
collapseInto :: Ref -> Act ext -> Set Ref -> Dag ext -> Dag ext
collapseInto into act members dag
    | Set.null present = dag
    | otherwise =
        Dag
            { dagNodes = Map.insert into act survivors
            , dagDependencies = renamedDeps
            , dagDependants = transposeOf renamedDeps
            , dagOrderRev = reverse order'
            , dagConflicts = dagConflicts dag
            }
  where
    present = Set.intersection members (Map.keysSet (dagNodes dag))
    survivors = Map.withoutKeys (dagNodes dag) present

    rename r = if Set.member r present then into else r

    renamedDeps = Map.map (nubOrd . fmap rename) keptDeps

    -- every surviving node's dependencies, with members renamed and the
    -- resulting self-edges dropped; the replacement inherits the union of its
    -- members' own dependencies.
    keptDeps :: Map Ref [Ref]
    keptDeps =
        Map.insert into inherited $
            Map.mapMaybeWithKey
                ( \r ds ->
                    if Set.member r present
                        then Nothing
                        else Just [d | d <- ds, rename d /= r]
                )
                (dagDependencies dag)

    inherited =
        [ d
        | m <- Set.toList present
        , d <- Map.findWithDefault [] m (dagDependencies dag)
        , not (Set.member d present)
        ]

    order' =
        case break (`Set.member` present) (dagOrder dag) of
            (before, []) -> before <> [into]
            (before, _ : after) -> before <> [into] <> filter (not . (`Set.member` present)) after

-- | Add one precedence edge, @(dependency, dependant)@. Both ends must
-- already be nodes; an edge to a node the magma does not hold is ignored,
-- matching 'fromMagma'.
addEdge :: (Ref, Ref) -> Dag ext -> Dag ext
addEdge (dependency, dependant) dag
    | not (Map.member dependency (dagNodes dag)) = dag
    | not (Map.member dependant (dagNodes dag)) = dag
    | otherwise =
        let deps' = Map.adjust (\ds -> nubOrd (ds <> [dependency])) dependant (dagDependencies dag)
         in dag{dagDependencies = deps', dagDependants = transposeOf deps'}

-- | Invert a dependency map into a dependant map. Left-biased 'Map.union' so
-- the computed entry wins; the right-hand map only supplies the empty list
-- for nodes nothing depends on, which have to stay keys.
--
-- Each dependant list is in key order of the dependants (with a repeat for a
-- dependency listed twice): consed and reversed once, not appended on the
-- right, which cost a node's whole list per edge.
transposeOf :: Map Ref [Ref] -> Map Ref [Ref]
transposeOf deps =
    Map.union
        (Map.map reverse (Map.fromListWith (<>) [(d, [r]) | (r, ds) <- Map.toList deps, d <- ds]))
        (Map.map (const []) deps)

{- | Fold the right 'Dag' into the left one: representatives from the right
win, edges and order accumulate. This is how a second declaration joins a
running world.
-}
mergeDag :: (Act ext -> Act ext -> Bool) -> Dag ext -> Dag ext -> Dag ext
mergeDag same into from = built (foldl' step (building into) (dagOrder from))
  where
    step b r =
        case representativeOf from r of
            Nothing -> b
            Just act -> recordInto same r act (dependenciesOf from r) b

-------------------------------------------------------------------------------

{- | The part of a node two representatives can actually be compared on.

Everything an 'Salmon.Builtin.Extension.Extension' is /for/ — @up@, @check@,
@down@ — is a function and therefore outside any equality, so this is the
whole of the available evidence. 'Data.Dynamic.Dynamic' renders as its type
alone by default, which would make two 'Salmon.Op.Supervision.Supervision'
dynamics with different restart policies compare equal here — 'showDynamic'
special-cases 'Salmon.Op.Supervision.Supervision' to render its 'Show'
instance instead, precisely so that this comparison (and so adoption, see
"Salmon.Actions.Upkeep"'s @startUpkeep@) can see a changed policy. Every
other 'Dynamic' payload still renders as its type name alone.
-}
data Representative = Representative
    { repShorthand :: !ShortHand
    , repHelp :: !Text
    , repNotes :: ![Text]
    , repDynamics :: ![String]
    }
    deriving (Show, Eq)

{- | Render one 'Dynamic' for 'Representative' comparison: by value where a
type's value matters to identity ('Salmon.Op.Supervision.Supervision', so a
changed policy is a changed representative — see (I5) in
@specs\/per-node-state-machines-remaining.md@), by type name otherwise
(the 'Dynamic' default, e.g. 'Salmon.Builtin.Nodes.Debian.Package.Package'
dynamics, whose identity 'foldDag' does not need to track this way).
-}
showDynamic :: Dynamic -> String
showDynamic d =
    case (fromDynamic d :: Maybe Supervision, fromDynamic d :: Maybe Guard) of
        (Just s, _) -> show s
        -- likewise by value: a guarded node whose hold set changed is a
        -- different node to tend. See "Salmon.Op.Guard".
        (_, Just g) -> show g
        _ -> show d

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
        , repDynamics = fmap showDynamic (getField @"dynamics" act.extension)
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
