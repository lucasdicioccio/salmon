{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A two-phase pass: every node tagged 'BuildPhase' (builds and checks)
must have succeeded before any node tagged 'DeployPhase' (pushes and
deploys) starts, and a pass can be asked to run the build phase alone
(@run up --phase build@, a release's dry run).

= The tag

A node opts in by carrying a 'PassPhase' in its
'Salmon.Builtin.Extension.dynamics' (@amendNode inBuildPhase o@). An
untagged node is in neither phase and is ordered by its own edges only,
exactly as before this module existed: nothing changes for a graph with no
tag in it.

The tag is scheduling, not effect, so "Salmon.Op.Dag"'s @representative@
leaves it out (like a card template): tagging a node that is already
running under @serve@ does not make it a changed node, and two declarations
that disagree only about a node's phase do not conflict (the newest one's
tag wins, as every other field does).

= The barrier

'barrier' is a "Salmon.Op.Rewrite" phase. Which nodes are in which phase
is cross-declaration knowledge (a deploy declared by one directive waits on
a build declared by another), so it lives after the fold, and it reads the
tags off the folded graph. It adds an edge from every build node to every
deploy node, among the nodes the pass wants up ('phaseDesired'): a failed
build node then blocks every deploy node through the drivers' ordinary
failure containment ('Salmon.Actions.UpDown.walk', the concurrent driver),
with no new report and no new driver.

Restricting to 'phaseDesired' is the rewrite rules' conservative
partition: a build node on its way down under @serve@ must not be made a
dependency of a deploy node that stays, and a @run down@ (nothing desired)
is left as declared. The edges are @builds × deploys@; a release has tens of
each, so they are added directly rather than through a synthetic barrier
node, which would need a 'Salmon.Op.Ref.Ref' and an action of its own.

A build node that depends on a deploy node (directly or through untagged
nodes) contradicts the barrier: the added edge closes a cycle and the
drivers report every node on it 'Salmon.Actions.UpDown.Blocked', which
fails the pass visibly. 'buildSelection' names such nodes before anything
runs.

A node a rewrite collapsed into a batch carries the batch node's dynamics,
which have no tag: register 'barrier' after the rewrites that collect
nodes ("Salmon.Builtin.CommandLine" puts it last), and do not tag a node
that a rewrite batches.

= Running the build phase alone

'buildSelection' is what @run up --phase build@ runs: the build nodes and
everything they depend on (a build cannot run without its context), and
nothing else. It refuses when there is no build node (a dry run that tests
nothing must not pass) and when a deploy node is among the build nodes'
dependencies (the dry run would deploy).
-}
module Salmon.Op.PassPhase (
    PassPhase (..),
    parsePassPhase,
    renderPassPhase,

    -- * The tag
    passPhaseOf,
    isPassPhase,
    setPassPhase,
    phaseNodes,

    -- * The barrier
    barrier,
    addBarrierEdges,

    -- * Running the build phase alone
    SelectionError (..),
    buildSelection,
) where

import Data.Dynamic (Dynamic)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))

import Salmon.Op.Actions (Act (..))
import Salmon.Op.Dag (Dag (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)
import Salmon.Op.PassPhase.Tag
import Salmon.Op.Rewrite (Phase (..), Rewrite, Rewritten (..))

-- | The nodes of a graph tagged with this phase, in first-seen order.
phaseNodes ::
    (HasField "dynamics" ext [Dynamic]) =>
    PassPhase ->
    Dag ext ->
    [Ref]
phaseNodes p dag =
    [ aref
    | aref <- Dag.dagOrder dag
    , Just act <- [Dag.representativeOf dag aref]
    , passPhaseOf (getField @"dynamics" act.extension) == Just p
    ]

{- | The "Salmon.Op.Rewrite" phase: every desired deploy node depends on
every desired build node. The identity when either side is empty, so a
graph without tags is returned as it came.
-}
barrier :: (HasField "dynamics" ext [Dynamic]) => Rewrite ext
barrier phase r =
    r{computedDag = addBarrierEdges (phaseDesired phase) (computedDag r)}

-- | 'barrier' on a bare 'Dag', among the given nodes.
addBarrierEdges ::
    (HasField "dynamics" ext [Dynamic]) =>
    Set Ref ->
    Dag ext ->
    Dag ext
addBarrierEdges desired dag
    | null builds || null deploys = dag
    | otherwise =
        dag
            { dagDependencies = deps'
            , dagDependants = Dag.transposeOf deps'
            }
  where
    wanted = filter (`Set.member` desired)
    builds = wanted (phaseNodes BuildPhase dag)
    deploys = wanted (phaseNodes DeployPhase dag)
    -- one transpose for all the edges, not one per edge as 'Dag.addEdge' does
    deps' = foldl' (\m d -> Map.adjust (appendNew builds) d m) (dagDependencies dag) deploys
    appendNew new ds =
        let seen = Set.fromList ds
         in ds <> filter (`Set.notMember` seen) new

-- | Why the build phase cannot run alone.
data SelectionError
    = -- | no node is tagged 'BuildPhase'
      NoBuildNode
    | -- | these deploy nodes are dependencies of build nodes
      DeployUnderBuild ![Ref]
    deriving (Show, Eq)

{- | The nodes @run up --phase build@ runs: every build node and the
transitive closure of its dependencies.
-}
buildSelection ::
    (HasField "dynamics" ext [Dynamic]) =>
    Dag ext ->
    Either SelectionError (Set Ref)
buildSelection dag
    | null builds = Left NoBuildNode
    | not (null misplaced) = Left (DeployUnderBuild misplaced)
    | otherwise = Right closure
  where
    builds = phaseNodes BuildPhase dag
    closure = go Set.empty builds
    go seen [] = seen
    go seen (x : xs)
        | Set.member x seen = go seen xs
        | otherwise = go (Set.insert x seen) (Dag.dependenciesOf dag x <> xs)
    misplaced = filter (`Set.member` closure) (phaseNodes DeployPhase dag)
