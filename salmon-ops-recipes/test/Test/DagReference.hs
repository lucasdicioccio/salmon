{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The fold "Salmon.Op.Dag" had before it went per node, kept as the
reference the per-node 'Salmon.Op.Dag.foldDag' is tested against, and the
precondition under which the two agree, as a predicate.

Nothing outside the tests and the @graph-profiles@ benchmark uses this
module. 'foldDagPerOccurrence' visits every occurrence of every node, so on a
shape that shares nodes it is exponential in the node count: keep its inputs
small.
-}
module Test.DagReference (
    foldDagPerOccurrence,
    foldDagPerOccurrenceWith,
    coherent,
    occurrences,
    stuckRounds,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (HasField (..))

import Salmon.Op.Actions
import Salmon.Op.Dag (Dag, dagOrder, emptyDag, record)
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref

{- | The per-occurrence fold, as it was: every occurrence is 'record'ed, so
the edge sets are the union over all of them, the representative is the last
occurrence in the tree, and a 'Salmon.Op.Dag.Conflict' is recorded every time
the representative changes to a differing one (so one contested node reached
by @k@ paths gives up to @2k - 1@ of them).
-}
foldDagPerOccurrence ::
    forall m ext.
    (HasField "ref" ext Ref) =>
    (Act ext -> Act ext -> Bool) ->
    Cofree Graph (OpGraph m (Actions ext)) ->
    Dag ext
foldDagPerOccurrence = foldDagPerOccurrenceWith record

{- | 'foldDagPerOccurrence' with the 'record' to use given: the tests of
"Test.EdgeListSpec" pass the appending one of "Test.EdgeListReference", to
compare the fold with what it was before edge lists stopped appending.
-}
foldDagPerOccurrenceWith ::
    forall m ext.
    (HasField "ref" ext Ref) =>
    ((Act ext -> Act ext -> Bool) -> Ref -> Act ext -> [Ref] -> Dag ext -> Dag ext) ->
    (Act ext -> Act ext -> Bool) ->
    Cofree Graph (OpGraph m (Actions ext)) ->
    Dag ext
foldDagPerOccurrenceWith record' same = go emptyDag
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
                 in foldl' go (record' same aref act predRefs dag) preds

-- | The nearest 'Ref'-carrying subtrees below a node, through any glue.
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

-- | One occurrence of a node: what it is, the refs it depends on, and the
-- occurrences below it.
data Occurrence ext = Occurrence
    { occRef :: Ref
    , occAct :: Act ext
    , occPredRefs :: [Ref]
    , occBelow :: [Occurrence ext]
    }

-- | The occurrences at the top of an expansion: the root, or (a root that is
-- glue) the nearest nodes below it.
tops ::
    (HasField "ref" ext Ref) =>
    Cofree Graph (OpGraph m (Actions ext)) ->
    [Occurrence ext]
tops c@(x :< g) =
    case x.node of
        Actionless -> concatMap tops (effPreds g)
        Actions act ->
            let preds = effPreds g
             in [ Occurrence
                    { occRef = getField @"ref" act.extension
                    , occAct = act
                    , occPredRefs = nubOrd [pr | p <- preds, Just pr <- [refOf p]]
                    , occBelow = concatMap tops preds
                    }
                | Just _ <- [refOf c]
                ]

everyOccurrence :: [Occurrence ext] -> [Occurrence ext]
everyOccurrence os = concat [o : everyOccurrence o.occBelow | o <- os]

-- | How many occurrences of nodes an expansion holds: what
-- 'foldDagPerOccurrence' pays for.
occurrences :: (HasField "ref" ext Ref) => Cofree Graph (OpGraph m (Actions ext)) -> Int
occurrences = length . everyOccurrence . tops

{- | The precondition of the per-node 'Salmon.Op.Dag.foldDag', decided by
looking at every occurrence (which is exactly what the fold no longer does).

Two occurrences /look alike/ when they have the same 'Ref', acts the test
calls the same, and the same list of predecessor 'Ref's. An expansion is
__coherent__ when any two occurrences that look alike have as many
predecessors as each other and those look alike too, one for one in order:
that is, when looking alike at the top means being the same tree all the way
down.

The test must be an equivalence for this to mean anything;
'Salmon.Op.Dag.sameRepresentative' is.
-}
coherent ::
    forall m ext.
    (HasField "ref" ext Ref) =>
    (Act ext -> Act ext -> Bool) ->
    Cofree Graph (OpGraph m (Actions ext)) ->
    Bool
coherent same root = all agree (Map.elems groups)
  where
    -- alike in ref and predecessor refs; the acts are told apart below,
    -- since they can only be compared pairwise.
    groups :: Map.Map (Ref, [Ref]) [Occurrence ext]
    groups =
        Map.fromListWith
            (flip (<>))
            [((o.occRef, o.occPredRefs), [o]) | o <- everyOccurrence (tops root)]

    agree :: [Occurrence ext] -> Bool
    agree [] = True
    agree (o : rest) =
        let (alike, others) = span' (\p -> same o.occAct p.occAct) rest
         in all (sameBelow o) alike && agree others

    span' p xs = (filter p xs, filter (not . p) xs)

    sameBelow :: Occurrence ext -> Occurrence ext -> Bool
    sameBelow a b =
        length a.occBelow == length b.occBelow
            && and (zipWith lookAlike a.occBelow b.occBelow)

    lookAlike :: Occurrence ext -> Occurrence ext -> Bool
    lookAlike a b =
        a.occRef == b.occRef
            && a.occPredRefs == b.occPredRefs
            && same a.occAct b.occAct

{- | 'Salmon.Op.Dag.stuck' as it was before it went one pass, kept as the
reference the one-pass version is tested against ("Test.DagStuckSpec") and
compared with in the @graph-profiles@ benchmark.

Kahn's algorithm by rounds: each round recomputes the ready set over every
node still remaining, so a graph @d@ levels deep costs @d@ rounds of up to
every node each, quadratic on a chain.
-}
stuckRounds :: (Dag ext -> Ref -> [Ref]) -> Dag ext -> Set.Set Ref
stuckRounds waitsOn dag = go (Set.fromList (dagOrder dag))
  where
    go remaining =
        let ready =
                Set.filter
                    (\r -> all (`Set.notMember` remaining) (waitsOn dag r))
                    remaining
         in if Set.null ready
                then remaining
                else go (remaining `Set.difference` ready)

-- | order-preserving dedup.
nubOrd :: (Ord b) => [b] -> [b]
nubOrd = go Set.empty
  where
    go _ [] = []
    go s (y : ys)
        | Set.member y s = go s ys
        | otherwise = y : go (Set.insert y s) ys
