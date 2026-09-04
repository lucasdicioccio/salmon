{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Rewrites: what a graph turns into just before it is walked, and the one
place cross-declaration knowledge is allowed to live.

= Why this is a phase and not a recipe

A recipe author supplies a @'Salmon.Op.Track.Track' directive@, i.e.
@directive -> Op@ — a function of __one directive in isolation__. It cannot
see the other live declarations, it cannot see which way each of their nodes
is wanted, and it cannot see what was declared before. So "batch this package
with the other packages that are also currently wanted up" is not awkward to
write in a recipe; it is inexpressible there, because nothing in
@directive -> Op@ has the second argument.

Widening the recipe to @Ledger -> directive -> Op@ is the tempting fix and is
wrong three times over: expansion stops being deterministic in the directive
(so @run up@ is no longer reproducible), 'Salmon.Actions.Query.planDirectiveDigest'
stops identifying a graph (it pins a plan to the directive's digest), and
retraction becomes uncomputable (a declaration's contribution would depend on
what order declarations arrived in).

So recipes stay a pure function of their own directive, and cross-declaration
knowledge lives here, after the fold — which is simply where that information
first exists. @'Salmon.Builtin.Extension.dynamics' :: [Dynamic]@ is the
channel: a node says /"I am a Package"/ without knowing what will be done
about it, and a phase collects the set and acts on it.

= What a phase may assume

A 'Rewrite' sees the whole folded 'Dag' — every declaration's nodes, merged —
plus a 'Phase' saying which of them are wanted up and which this traversal
will not touch at all. Two rules follow and both matter:

* __Partition conservatively.__ A node in 'phaseDesired' is still wanted by
  some live declaration; only a node absent from it is going away. Sweeping a
  still-wanted node into a teardown batch would let one retraction pull
  something out from under a declaration still standing on it, which is the
  one failure here that retrying does not recover.
* __Leave 'phaseIgnored' alone.__ Those nodes are excluded by a plan or a
  @converge --select@, and collecting one into a batch would quietly execute
  what the operator asked to skip.

A phase that introduces a node records what that node stands in for, in
'computedMembers'. That is what lets a driver's gate and its convergence
recording keep speaking in terms of the nodes the operator declared: the
ledger is /declared intent/ and keeps per-package nodes, while a collection
is an /execution-plan detail/ that exists only here. They never disagree
because they answer different questions.
-}
module Salmon.Op.Rewrite (
    Phase (..),
    wholeGraph,
    Rewritten (..),
    Rewrite,
    rewrite,
    membersOf,
    collectDynamic,
    introduce,
) where

import Data.Dynamic (Dynamic, Typeable, fromDynamic)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))

import Salmon.Op.Actions (Act (..))
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)

{- | What the traversal about to happen knows that the graph does not.

'phaseDesired' is the ledger's @desired@ for a @serve@ convergence, every
node for a @run up@, and nothing at all for a @run down@ — which is exactly
what makes one direction-aware rewrite do the right thing in all three.
-}
data Phase = Phase
    { phaseDesired :: !(Set Ref)
    , phaseIgnored :: !(Set Ref)
    -- ^ Nodes this traversal will not touch: a plan's excluded refs, or the
    -- complement of a @converge --select@. A rewrite must not collect them.
    }
    deriving (Show, Eq)

-- | The 'Phase' for a one-shot traversal of a whole graph: everything in it
-- is wanted, nothing is excluded. @run down@ passes @'Phase' mempty mempty@.
wholeGraph :: Dag ext -> Phase
wholeGraph dag = Phase (Set.fromList (Dag.dagOrder dag)) Set.empty

-- | A folded graph plus whatever the rewrites did to it.
data Rewritten ext = Rewritten
    { computedDag :: !(Dag ext)
    -- ^ What will actually execute.
    , computedMembers :: !(Map Ref (Set Ref))
    -- ^ For each node a rewrite introduced, the declared nodes it stands in
    -- for. A node absent from this map stands in for itself; use 'membersOf'
    -- rather than reading it directly.
    }

type Rewrite ext = Phase -> Rewritten ext -> Rewritten ext

-- | Run the registered phases in order over a freshly folded 'Dag'.
rewrite :: [Rewrite ext] -> Phase -> Dag ext -> Rewritten ext
rewrite phases phase dag = foldl' (\r f -> f phase r) (Rewritten dag Map.empty) phases

{- | The declared nodes a computed node stands in for — itself, when it stands
in for nothing, which is every node in a graph no rewrite touched.

A driver uses this twice: to decide whether a node is worth touching (it is,
if any member is), and to record what happened (it happened to every member).
-}
membersOf :: Rewritten ext -> Ref -> Set Ref
membersOf r aref = Map.findWithDefault (Set.singleton aref) aref (computedMembers r)

{- | Every node carrying a 'Dynamic' of the given type, with the values it
carries. This is the input side of a rewrite: the magma read across every
declaration at once, which is what a recipe could not do.
-}
collectDynamic ::
    forall a ext.
    (Typeable a, HasField "dynamics" ext [Dynamic]) =>
    Rewritten ext ->
    [(Ref, [a])]
collectDynamic r =
    [ (aref, vals)
    | (aref, act) <- Map.toList (Dag.dagNodes (computedDag r))
    , let vals = mapMaybe fromDynamic (getField @"dynamics" act.extension)
    , not (null vals)
    ]

{- | Collapse a set of declared nodes into the given node, which then stands
in for them, recording the membership so the drivers can still speak in
declared terms.

The replacement's own 'Salmon.Builtin.Extension.ref' is its identity here, so
it has to be one no declared node uses — two batches in one graph need two
refs, or the second silently replaces the first.

Members already standing in for something else are flattened through, so
collections compose: collecting a collection names the original nodes, not
the intermediate.
-}
introduce ::
    (HasField "ref" ext Ref) =>
    Act ext ->
    Set Ref ->
    Rewritten ext ->
    Rewritten ext
introduce act members r
    | Set.null members = r
    | otherwise =
        Rewritten
            { computedDag = Dag.collapseInto into act members (computedDag r)
            , computedMembers =
                Map.insert into flattened $
                    Map.withoutKeys (computedMembers r) members
            }
  where
    -- taken from the node rather than passed alongside it: the drivers look
    -- a collection's members up by the ref the node carries, so the two
    -- diverging would silently gate the collection out of every pass.
    into = getField @"ref" act.extension
    flattened = Set.unions (fmap (membersOf r) (Set.toList members))
