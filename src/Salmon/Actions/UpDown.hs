{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Salmon.Actions.UpDown where

import GHC.Records
import Data.Foldable (traverse_, toList)

import Salmon.FoldBranch
import Salmon.Op.Graph
import Salmon.Op.Eval
import Salmon.Op.Actions

upTree
  :: ( Monad m
     , HasField "up" ext (IO ())
     )
  => (forall a. m a -> IO a)
  -> OpGraph m (Actions ext)
  -> IO ()
upTree nat graph = do
    gr1 <- nat $ expand graph
    let as = toList gr1 
    traverse_ up $ reverse as
  where
    up = upnode . node
    upnode x =
      case x of
        Actionless -> pure ()
        (Actions act) -> (extension act).up

downTree
  :: ( Monad m
     , HasField "down" ext (IO ())
     )
  => (forall a. m a -> IO a)
  -> OpGraph m (Actions ext)
  -> IO ()
downTree nat graph = do
    gr1 <- nat $ expand graph
    let as = toList gr1 
    traverse_ down as
  where
    down = downnode . node
    downnode x =
      case x of
        Actionless -> pure ()
        (Actions act) -> (extension act).down
