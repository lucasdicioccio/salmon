{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Salmon.Actions.Help where

import Control.Comonad.Cofree (Cofree)
import Data.Foldable (toList, traverse_)
import GHC.Records

import Salmon.FoldBranch
import Salmon.Op.Actions
import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.OpGraph

import Data.Text (Text)
import qualified Data.Text.IO as Text

printTree :: (Monad m) => (forall a. m a -> IO a) -> OpGraph m (Actions ext) -> IO ()
printTree nat graph = do
    printCograph =<< nat (expand graph)

printCograph ::
    (Monad m) =>
    Cofree Graph (OpGraph m (Actions ext)) ->
    IO ()
printCograph gr1 = do
    traverse_ Text.putStrLn $ dirtree gr1
  where
    dirtree = foldBranch mkPath ""
    mkPath pfx x =
        case node x of
            Actionless -> pfx
            Actions y -> pfx <> "/" <> shorthand y

printHelpTree ::
    ( Monad m
    , HasField "help" ext Text
    ) =>
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO ()
printHelpTree nat graph = do
    printHelpCograph =<< nat (expand graph)

printHelpCograph ::
    ( Monad m
    , HasField "help" ext Text
    ) =>
    Cofree Graph (OpGraph m (Actions ext)) ->
    IO ()
printHelpCograph gr1 = do
    let as = toList $ dirtree gr1
    let bs = toList $ helptree gr1
    traverse_ Text.putStrLn $ zipWith (\a b -> a <> " " <> b) as bs
  where
    dirtree = foldBranch mkPath ""
    helptree = fmap (helpnode . node)
    helpnode x =
        case x of
            Actionless -> ""
            (Actions act) -> (extension act).help
    mkPath pfx x =
        case node x of
            Actionless -> pfx
            Actions y -> pfx <> "/" <> shorthand y
