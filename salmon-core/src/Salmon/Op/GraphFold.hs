{-# LANGUAGE ScopedTypeVariables #-}

{- | Generic traversal primitives over 'Cofree' 'Graph' trees.

'Graph' already derives 'Foldable'/'Functor'/'Traversable', which gives
pre-order traversal for free (see 'Data.Foldable.toList'). 'foldWithContext'
covers the shape pre-order alone doesn't: a fold that carries context down
from ancestor to descendant while remembering which 'Graph' constructor
connected them. Generic over any node type and knows nothing about ops,
actions, or refs — that stays in the caller.

This module used to also offer a post-order visitor
(@postOrderM@), written for "Salmon.Actions.UpDown".'Salmon.Actions.UpDown.upTree'
to propagate "a predecessor failed, so skip me too" down a subtree. Milestone
4 of @specs\/per-node-state-machines.md@ moved both drivers onto
"Salmon.Op.Dag", which answers that question from the collapsed magma
instead of a tree walk, and nothing else in this repository ever called it —
dropped rather than kept speculative; recover it from history if a caller
needs it again.
-}
module Salmon.Op.GraphFold (
    Shape (..),
    Branch (..),
    foldWithContext,
) where

import Control.Comonad.Cofree (Cofree (..))

import Salmon.Op.Graph

-- | Which 'Graph' constructor a node's own predecessors are wrapped in.
data Shape = SVertices | SOverlay | SConnect
    deriving (Show, Eq, Ord)

-- | Which arm of its parent's 'Graph' constructor a child was reached
-- through.
data Branch
    = FromVertices
    | FromOverlayL
    | FromOverlayR
    | FromConnectL
    | FromConnectR
    deriving (Show, Eq, Ord)

{- | Walk a 'Cofree' 'Graph', calling @onNode ctx shape x@ at every node
(told the inherited context and the 'Shape' of its own predecessor graph),
and folding results with '(<>)'. @nextCtx@ computes the context handed down
to a child, told which 'Branch' connects the current node to that child —
this is how e.g. "skip through nodes with no real payload" is implemented by
callers: return the unchanged @ctx@ instead of a new one.
-}
foldWithContext ::
    forall a ctx r.
    (Monoid r) =>
    ctx ->
    (ctx -> Shape -> a -> r) ->
    (ctx -> Branch -> a -> ctx) ->
    Cofree Graph a ->
    r
foldWithContext ctx0 onNode nextCtx = go ctx0
  where
    go :: ctx -> Cofree Graph a -> r
    go ctx (x :< gr) = onNode ctx (shapeOf gr) x <> descend ctx x gr

    shapeOf :: Graph b -> Shape
    shapeOf (Vertices _) = SVertices
    shapeOf (Overlay _ _) = SOverlay
    shapeOf (Connect _ _) = SConnect

    descend :: ctx -> a -> Graph (Cofree Graph a) -> r
    descend ctx x (Vertices cs) = foldMap (go (nextCtx ctx FromVertices x)) cs
    descend ctx x (Overlay c1 c2) =
        foldMap (go (nextCtx ctx FromOverlayL x)) c1
            <> foldMap (go (nextCtx ctx FromOverlayR x)) c2
    descend ctx x (Connect c1 c2) =
        foldMap (go (nextCtx ctx FromConnectL x)) c1
            <> foldMap (go (nextCtx ctx FromConnectR x)) c2
