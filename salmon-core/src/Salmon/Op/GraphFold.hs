{-# LANGUAGE ScopedTypeVariables #-}

{- | Generic traversal primitives over 'Cofree' 'Graph' trees.

'Graph' already derives 'Foldable'/'Functor'/'Traversable', which gives
pre-order traversal for free (see 'Data.Foldable.toList'). These primitives
cover the two shapes that pre-order alone doesn't: a post-order visitor
(children before the node itself), and a fold that carries context down from
ancestor to descendant while remembering which 'Graph' constructor connected
them. Both are generic over any node type and know nothing about ops,
actions, or refs — that stays in the callers.
-}
module Salmon.Op.GraphFold (
    postOrderM,
    Shape (..),
    Branch (..),
    foldWithContext,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Foldable (toList)

import Salmon.Op.Graph

{- | Visit every node of a 'Cofree' 'Graph' in post-order: all of a node's
immediate Graph-children are visited (recursively, in the same left-to-right
order 'Graph's derived 'Foldable' instance would produce) before the node
itself. Performs no deduplication — a node reachable via two paths is
visited once per path. Callers that need dedup do it themselves, keyed off
whatever identity they extract from the node.

@visit@ is told whether /any/ of the current node's immediate children
reported 'True' (e.g. "this child, or something it itself depended on,
failed"), and returns its own such flag for the node just visited — letting
a caller like "Salmon.Actions.UpDown".'Salmon.Actions.UpDown.upTree'
propagate "a predecessor failed, so skip me too" down through the whole
subtree without needing its own separate graph walk.
-}
postOrderM :: (Monad m) => (Bool -> a -> m Bool) -> Cofree Graph a -> m Bool
postOrderM visit = go
  where
    go (x :< g) = do
        childFailed <- or <$> mapM go (toList g)
        visit childFailed x

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
