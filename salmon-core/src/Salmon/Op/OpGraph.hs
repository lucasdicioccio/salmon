{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds #-}

module Salmon.Op.OpGraph where

import Data.Functor.Classes
import Data.Kind (Type)
import Salmon.Op.Graph

-------------------------------------------------------------------------------

{- | An OpGraph is a complicated object.

- An OpGraph feels like a Comonad as it is centered on a node and has a recipe
  to find neighbors.
- An OpGraph feels like a program chunk because the recipe to find neighbors
  actually perfoms an effect.
- An OpGraph feels like a graph because the node on which is centered is
  somehow connected to a full graph of OpGraphs.

In Salmon, we want to define operations like "create a file" or "turn a server
up" as nodes.  It is really powerful to be able to see both operations
uniformly. However, to turn a server up, one needs many more steps than for
creating a file.
-}
data OpGraph (meval :: Type -> Type) node = OpGraph
    { predecessors :: meval (Graph (OpGraph meval node))
    , node :: node
    }
    deriving (Functor, Foldable, Traversable)

instance (Show a) => Show (OpGraph m a) where
    show gr = show gr.node

-------------------------------------------------------------------------------

-- | Injects a dependency so that op1 `inject` op2 is adding op2 as Connect-ed predecessor to op1
inject :: (Applicative m) => OpGraph m a -> OpGraph m a -> OpGraph m a
inject x y =
    x
        { predecessors = Connect <$> pure (Vertices [y]) <*> predecessors x
        }

-- | Injects a dependency so that op1 `inject` op2 is adding op2 as Overlay-ed predecessor to op1
overlaid :: (Applicative m) => OpGraph m a -> OpGraph m a -> OpGraph m a
overlaid x y =
    x
        { predecessors =
            Overlay <$> pure (Vertices [y]) <*> predecessors x
        }
