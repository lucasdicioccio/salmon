{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Salmon.Actions.Notify where

import GHC.Records
import Data.Foldable (traverse_, toList)
import Control.Comonad.Cofree (Cofree)

import Salmon.FoldBranch
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Eval
import Salmon.Op.Actions

notifyTree
  :: ( Monad m
     , HasField "notify" ext (IO ())
     )
  => (forall a. m a -> IO a)
  -> OpGraph m (Actions ext)
  -> IO ()
notifyTree nat graph = do
    notifyCograph =<< nat (expand graph)

notifyCograph
  :: ( Monad m
     , HasField "notify" ext (IO ())
     )
  => Cofree Graph (OpGraph m (Actions ext))
  -> IO ()
notifyCograph gr1 = do
    let as = toList gr1 
    traverse_ go as
  where
    go = gonode . node
    gonode x =
      case x of
        Actionless -> pure ()
        (Actions act) -> (extension act).notify

