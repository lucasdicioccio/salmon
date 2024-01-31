{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salmon.Actions.Dot
  ( printDigraph
  , printCograph
  ) where

import GHC.Records
import Data.Foldable (traverse_, toList)
import Control.Comonad.Cofree (Cofree)

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Salmon.FoldBranch
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Eval
import Salmon.Op.Actions
import Salmon.Op.Ref

dotNode :: Node -> Text
dotNode (Just (ref,name)) = mconcat [ unRef ref, "[label=\"", dotEscape name, "\"];" ]
dotNode _ = ""

dotEdge :: Edge -> Text
dotEdge (ref1,ref2) = mconcat [ unRef ref1, "->", unRef ref2 ]

dotEscape :: Text -> Text
dotEscape = id

type Node = Maybe (Ref, ShortHand)

sameNode :: Node -> Node -> Bool
sameNode n1 n2 = Maybe.fromMaybe False $ do
  (l,_) <- n1
  (r,_) <- n2
  pure $ l == r

type Edge = (Ref, Ref)

sameEdge :: Edge -> Edge -> Bool
sameEdge e1 e2 = e1 == e2

printDigraph
  :: forall m ext.
     ( Monad m
     , HasField "ref" ext Ref
     )
  => (forall a. m a -> IO a)
  -> OpGraph m (Actions ext)
  -> IO ()
printDigraph nat graph = do
    printCograph =<< nat (expand graph)

printCograph
  :: forall m ext.
     ( Monad m
     , HasField "ref" ext Ref
     )
  => Cofree Graph (OpGraph m (Actions ext))
  -> IO ()
printCograph gr1 = do
    let nodes = fmap dotNode $ List.nubBy sameNode $ toList $ dirNodes gr1
    let edges = fmap dotEdge $ List.nubBy sameEdge $ Maybe.catMaybes $ toList $ dirEdges gr1 
    putStrLn "digraph {"
    putStrLn "rankdir=LR;"
    traverse_ Text.putStrLn $ nodes
    traverse_ Text.putStrLn $ edges
    putStrLn "}"
  where
    dirNodes :: Cofree Graph (OpGraph m (Actions ext)) -> Cofree Graph Node
    dirNodes = fmap (\x -> mkNode x.node)

    mkNode :: Actions ext -> Node
    mkNode x =
      case x of
        Actionless ->
          Nothing
        (Actions act) ->
          Just ((extension act).ref, act.shorthand)

    -- implementation is akin to a zip storing the previous parent
    dirEdges :: Cofree Graph (OpGraph m (Actions ext)) -> Cofree Graph (Maybe Edge)
    dirEdges = fmap snd <$> foldBranch mkEdges (Nothing, Nothing)

    mkEdges :: (Maybe Ref, Maybe Edge) -> OpGraph m (Actions ext) -> (Maybe Ref, Maybe Edge)
    mkEdges ante@(pred,_) x =
      case node x of
        Actionless -> ante
        Actions act ->
          let ref1 = (extension act).ref in
          case pred of
            Just ref0 -> (Just ref1, Just (ref0, ref1))
            Nothing -> (Just ref1, Nothing)
