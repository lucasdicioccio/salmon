{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Salmon.Actions.Check where

import GHC.Records
import Data.Foldable (traverse_, toList)
import Control.Comonad.Cofree (Cofree)

import Salmon.FoldBranch
import Salmon.Op.Graph
import Salmon.Op.Eval
import Salmon.Op.Actions

checkTree
  :: ( Monad m
     , HasField "check" ext (IO ())
     )
  => (forall a. m a -> IO a)
  -> OpGraph m (Actions ext)
  -> IO ()
checkTree nat graph = do
    checkCograph =<< nat (expand graph)

checkCograph
  :: ( Monad m
     , HasField "check" ext (IO ())
     )
  => Cofree Graph (OpGraph m (Actions ext))
  -> IO ()
checkCograph gr1 = do
    traverse_ go $ toList gr1
  where
    go = gonode . node
    gonode x =
      case x of
        Actionless -> pure ()
        (Actions act) -> (extension act).check
