module Salmon.FoldBranch where

import Control.Comonad.Cofree (Cofree (..))

{- | Maps the branch of a Cofree comonad by accumulating a function
from tree to leaves.

Unlike a typical fold, at each branch of the the Cofree, the accumulator
forks.

This function is useful to turn nodes into paths from the Cofree seed to each
node.
-}
foldBranch ::
    forall t item accum.
    (Functor t) =>
    (accum -> item -> accum) ->
    accum ->
    Cofree t item ->
    Cofree t accum
foldBranch f pfx (x :< xs) =
    path :< subtree
  where
    path :: accum
    path = f pfx x

    subtree :: t (Cofree t accum)
    subtree = fmap (foldBranch f path) xs
