{-# LANGUAGE FlexibleContexts #-}

{- | A /contained/ node: one whose failure is reported but fails nothing
beyond itself.

The one-shot drivers block every dependant of a node that failed, and a pass
with any failed or blocked node is a failed pass. A node marked 'Contained'
(a marker on @dynamics@, the channel "Salmon.Op.Window"'s @Disruptive@ and
"Salmon.Op.PassPhase.Tag" also use) changes that for itself only, and only
going up:

* when it fails, or is 'Salmon.Actions.UpDown.Blocked' by a failure below
  it, its dependants are __not__ blocked: they run as if it had succeeded;
* a failure that every upward path stops at a contained node (the contained
  node itself, and whatever below it failed only on its account) does not
  count against the pass: 'Salmon.Actions.UpDown.upDag' and
  'Salmon.Actions.Concurrent.upDagConcurrent' return 'True' if that is all
  that went wrong, and report each such node once more as
  'Salmon.Actions.UpDown.Contained' after the walk.

The use is a section of a graph nothing above it needs, such as a
monitoring agent's repository, package and service on a machine whose
services must come up whether or not the agent does: mark the top of that
section (the node the rest of the graph depends on) contained. Something that
really does need the section depends on a node /inside/ it instead, and is
blocked by its failure as usual: that edge is the explicit dependency.

Teardown is unchanged. A contained node whose @down@ failed is still
standing, so its predecessors are blocked, as for any node.

The marker is left out of 'Salmon.Op.Dag.Representative' (it is pass policy,
like a pass phase, not node identity), so marking a node contained renders
nothing different and restarts nothing under @run serve@. A consequence: two
declarations of one 'Salmon.Op.Ref.Ref' that differ only in being contained
are not reported 'Salmon.Actions.UpDown.Conflicting', and the last writer's
choice stands.
-}
module Salmon.Op.Contained (
    Contained (..),
    isContained,
    isContainedDynamic,
    containment,
) where

import Data.Dynamic (Dynamic, fromDynamic)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))

import Salmon.Op.Ref (Ref)

-- | Marker: this node's failure stops at it. See the module header.
data Contained = Contained
    deriving (Show, Eq)

isContainedDynamic :: Dynamic -> Bool
isContainedDynamic d = isJust (fromDynamic d :: Maybe Contained)

isContained :: (HasField "dynamics" ext [Dynamic]) => ext -> Bool
isContained ext = any isContainedDynamic (getField @"dynamics" ext)

{- | Which of the nodes that did not come up are contained: the least set
holding every marked node among them, and every other one whose dependants
(at least one) are all in it.

@dependants@ is the direction a failure travels ('Salmon.Op.Dag.dependantsOf'
going up). A node with no dependants is a top and is never contained unless
marked; a node with a dependant that came up cleanly is not contained either
(only a contained failure lets a dependant run). The least fixed point is what
keeps a cycle, whose nodes are each other's dependants, from containing
itself: nodes on a cycle are never contained unless marked.

Returns the contained subset of @notOk@. The pass succeeded iff that is all of
@notOk@. Linear in the edges among @notOk@.
-}
containment :: (Ref -> Bool) -> (Ref -> [Ref]) -> Set Ref -> Set Ref
containment marked dependants notOk = go seeds (Set.fromList seeds) counts0
  where
    seeds = [r | r <- Set.toList notOk, marked r]

    -- every unmarked failed node, with how many of its dependants are not
    -- (yet) known to be contained. A dependant counts once however many
    -- edges name it.
    counts0 :: Map.Map Ref Int
    counts0 =
        Map.fromList
            [ (r, Set.size ds)
            | r <- Set.toList notOk
            , not (marked r)
            , let ds = Set.fromList (dependants r)
            , not (Set.null ds)
            ]

    -- the dependencies of a contained node, among the unmarked failed ones:
    -- its failed predecessors, found by inverting @dependants@ once.
    dependenciesIn :: Map.Map Ref [Ref]
    dependenciesIn =
        Map.fromListWith
            (++)
            [ (d, [r])
            | r <- Map.keys counts0
            , d <- Set.toList (Set.fromList (dependants r))
            ]

    go :: [Ref] -> Set Ref -> Map.Map Ref Int -> Set Ref
    go [] acc _ = acc
    go (x : xs) acc counts =
        let preds = Map.findWithDefault [] x dependenciesIn
            step (q, a, cs) p =
                let k = Map.findWithDefault 0 p cs - 1
                    cs' = Map.insert p k cs
                 in if k == 0 && Set.notMember p a
                        then (p : q, Set.insert p a, cs')
                        else (q, a, cs')
            (xs', acc', counts') = foldl step (xs, acc, counts) preds
         in go xs' acc' counts'
