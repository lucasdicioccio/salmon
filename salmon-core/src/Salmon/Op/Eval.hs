{-# LANGUAGE ApplicativeDo #-}

module Salmon.Op.Eval where

import Control.Comonad.Cofree (Cofree, unfoldM)

import Salmon.Op.Graph
import Salmon.Op.OpGraph

-- | Expands all predecessors.
expand :: (Monad m) => OpGraph m node -> m (Cofree Graph (OpGraph m node))
expand = unfoldM expandOne

-- | Expands a single node.
expandOne :: (Applicative m) => OpGraph m node -> m (OpGraph m node, (Graph (OpGraph m node)))
expandOne op = do
    preds <- op.predecessors
    pure (op, preds)
