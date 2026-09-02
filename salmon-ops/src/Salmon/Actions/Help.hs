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
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

{- | Per-node path, as the segments (one 'shorthand' per non-'Actionless'
ancestor, root-to-node) that 'run tree'/'run dag' already render joined by
@\/@. Shared by 'printCograph', "Salmon.Actions.Dot", and
"Salmon.Actions.Query" so there is exactly one definition of "what a node's
path is" across all of them.
-}
nodeSegments ::
    (Functor t) =>
    Cofree t (Actions ext) ->
    Cofree t [Text]
nodeSegments = foldBranch step []
  where
    step pfx x =
        case x of
            Actionless -> pfx
            Actions y -> pfx <> [shorthand y]

pathText :: [Text] -> Text
pathText = ("" <>) . Text.concat . map ("/" <>)

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
    dirtree = fmap pathText . nodeSegments . fmap node

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
    dirtree = fmap pathText . nodeSegments . fmap node
    helptree = fmap (helpnode . node)
    helpnode x =
        case x of
            Actionless -> ""
            (Actions act) -> (extension act).help
