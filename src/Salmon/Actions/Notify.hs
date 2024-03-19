{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Salmon.Actions.Notify where

import Control.Comonad.Cofree (Cofree)
import Data.Foldable (toList, traverse_)
import GHC.Records

import Salmon.FoldBranch
import Salmon.Op.Actions
import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.OpGraph

notifyTree ::
    ( Monad m
    , HasField "notify" ext (IO ())
    ) =>
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO ()
notifyTree nat graph = do
    notifyCograph =<< nat (expand graph)

notifyCograph ::
    ( Monad m
    , HasField "notify" ext (IO ())
    ) =>
    Cofree Graph (OpGraph m (Actions ext)) ->
    IO ()
notifyCograph gr1 = do
    let as = toList gr1
    traverse_ go as
  where
    go = gonode . node
    gonode x =
        case x of
            Actionless -> pure ()
            (Actions act) -> (extension act).notify
