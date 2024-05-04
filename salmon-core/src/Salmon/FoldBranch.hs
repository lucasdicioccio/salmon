module Salmon.FoldBranch where

import Control.Comonad.Cofree (Cofree (..))

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
