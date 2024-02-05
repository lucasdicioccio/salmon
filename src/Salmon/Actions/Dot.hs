{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salmon.Actions.Dot
  ( printDigraph
  , printCograph
  , PlaceHolder(..)
  , DotGraphExt
  ) where

import GHC.Records
import Data.Foldable (traverse_, toList)
import Control.Comonad.Cofree (Cofree(..), hoistCofree)

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Data.Dynamic (Dynamic, fromDynamic)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Salmon.FoldBranch
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Eval
import Salmon.Op.Actions
import Salmon.Op.Ref

type Node ext = Maybe (Ref, ShortHand, ext)

type RawEdge = (Ref, Ref)

data Edge = Edge { connectType :: ConnectType, rawEdge :: RawEdge }
data ConnectType = C | O

-- | Slightly over-constrained constraint for graph extentions we want to represent.
type DotGraphExt ext = (HasField "dynamics" ext [Dynamic], HasField "ref" ext Ref)

data PlaceHolder = PlaceHolder Text

hasPlaceholder
  :: DotGraphExt ext
  => ext
  -> Bool
hasPlaceholder e =
  not $ null $ Maybe.catMaybes $ map (fromDynamic @PlaceHolder) e.dynamics

dotNode :: DotGraphExt ext => Node ext -> Text
dotNode Nothing = ""
dotNode (Just (ref,name,ext))
  | hasPlaceholder ext = mconcat [ unRef ref, "[color=grey;label=\"", dotEscape name, "\"];" ]
  | otherwise = mconcat [ unRef ref, "[label=\"", dotEscape name, "\"];" ]

dotEdge :: Edge -> Text
dotEdge e =
  let (ref1,ref2) = e.rawEdge in
  case e.connectType of
    O -> mconcat [ unRef ref1, "->", unRef ref2 ]
    C -> mconcat [ unRef ref1, "->", unRef ref2, "[color=red]" ]

dotEscape :: Text -> Text
dotEscape = id

sameNode :: Node a -> Node a -> Bool
sameNode n1 n2 = Maybe.fromMaybe False $ do
  (l,_,_) <- n1
  (r,_,_) <- n2
  pure $ l == r

sameEdge :: Edge -> Edge -> Bool
sameEdge e1 e2 = e1.rawEdge == e2.rawEdge

-------------------------------------------------------------------------------

evalEdges
  :: forall m ext. (DotGraphExt ext)
  => Cofree Graph (OpGraph m (Actions ext))
  -> [Edge]
evalEdges = go Nothing . fmap node
  where
    go :: Maybe (ConnectType, ext) -> Cofree Graph (Actions ext)-> [Edge]
    go prev (a :< (Vertices d))    =
         let set1 = [ Edge ct (l.ref,r.ref) | r <- toList a, (ct, l) <- toList prev ]
             next = case a of Actionless -> prev ; Actions (Act _ ext) -> Just (O, ext)
             set2 = concatMap (go next) d
         in
         set1 <> set2
    go prev (a :< (Overlay d1 d2)) =
         let set1 = [ Edge ct (l.ref,r.ref) | r <- toList a, (ct, l) <- toList prev ]
             next = case a of Actionless -> prev ; Actions (Act _ ext) -> Just (O, ext)
             set2 = concatMap (go next) d1
             set3 = concatMap (go next) d2
         in
         set1 <> set2 <> set3
    go prev (a :< (Connect d1 d2)) =
         let set1 = [ Edge ct (l.ref,r.ref) | r <- toList a, (ct, l) <- toList prev ]
             next k = case a of Actionless -> prev ; Actions (Act _ ext) -> Just (k, ext)
             set2 = concatMap (go $ next C) d1
             set3 = concatMap (go $ next O) d2
         in
         set1 <> set2 <> set3

-------------------------------------------------------------------------------

evalNodes
  :: (DotGraphExt ext)
  => Cofree Graph (OpGraph m (Actions ext))
  -> Cofree Graph (Node ext)
evalNodes = fmap (\x -> mkNode x.node)

mkNode
  :: (DotGraphExt ext)
  => Actions ext
  -> Node ext
mkNode x =
  case x of
    Actionless ->
      Nothing
    (Actions act) ->
      Just (act.extension.ref, act.shorthand, act.extension)

-------------------------------------------------------------------------------

printDigraph
  :: forall m ext.
     ( Monad m
     , DotGraphExt ext
     )
  => (forall a. m a -> IO a)
  -> OpGraph m (Actions ext)
  -> IO ()
printDigraph nat graph = do
    printCograph =<< nat (expand graph)

printCograph
  :: forall m ext.
     ( Monad m
     , DotGraphExt ext
     )
  => Cofree Graph (OpGraph m (Actions ext))
  -> IO ()
printCograph gr1 = do
    let nodes = fmap dotNode $ List.nubBy sameNode $ toList $ evalNodes gr1
    let edges = fmap dotEdge $ List.nubBy sameEdge $ evalEdges gr1 
    putStrLn "digraph {"
    putStrLn "rankdir=LR;"
    traverse_ Text.putStrLn $ nodes
    traverse_ Text.putStrLn $ edges
    putStrLn "}"
