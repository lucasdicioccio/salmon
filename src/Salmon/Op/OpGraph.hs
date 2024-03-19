{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds #-}

module Salmon.Op.OpGraph where

import Data.Functor.Classes
import Salmon.Op.Graph

data OpGraph (meval :: * -> *) node = OpGraph
    { predecessors :: meval (Graph (OpGraph meval node))
    , node :: node
    }
    deriving (Functor, Foldable, Traversable)

instance (Show a) => Show (OpGraph m a) where
    show gr = show gr.node

-- combinators

-- | injects a dependency so that op1 `inject` op2 is adding op2 as Connect-ed predecessor to op1
inject :: (Applicative m) => OpGraph m a -> OpGraph m a -> OpGraph m a
inject x y = x{predecessors = Connect <$> pure (Vertices [y]) <*> predecessors x}

-- | injects a dependency so that op1 `inject` op2 is adding op2 as Connect-ed predecessor to op1
overlaid :: (Applicative m) => OpGraph m a -> OpGraph m a -> OpGraph m a
overlaid x y = x{predecessors = Overlay <$> pure (Vertices [y]) <*> predecessors x}
