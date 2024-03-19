{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salmon.Actions.Dot (
    printDigraph,
    printCograph,
    PlaceHolder (..),
    OpaqueNode (..),
    DotGraphExt,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Foldable (toList, traverse_)
import GHC.Records

import Data.Dynamic (Dynamic, fromDynamic)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Salmon.FoldBranch
import Salmon.Op.Actions
import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref

type Node ext = Maybe (Ref, ShortHand, ext)

type RawEdge = (Ref, Ref)

data Edge = Edge {connection :: ConnectType, rawEdge :: RawEdge}
data PrevConnectType = CL | CR | OV
data CurConnectType = V | O | C
type ConnectType = (PrevConnectType, CurConnectType)

-- | Slightly over-constrained constraint for graph extentions we want to represent.
type DotGraphExt ext = (HasField "dynamics" ext [Dynamic], HasField "ref" ext Ref)

data PlaceHolder = PlaceHolder Text

data OpaqueNode = OpaqueNode Text

hasPlaceholder ::
    (DotGraphExt ext) =>
    ext ->
    Bool
hasPlaceholder e =
    not $ null $ Maybe.catMaybes $ map (fromDynamic @PlaceHolder) e.dynamics

hasOpaqueNode ::
    (DotGraphExt ext) =>
    ext ->
    Bool
hasOpaqueNode e =
    not $ null $ Maybe.catMaybes $ map (fromDynamic @OpaqueNode) e.dynamics

dotNode :: (DotGraphExt ext) => Node ext -> Text
dotNode Nothing = ""
dotNode (Just (ref, name, ext))
    | hasOpaqueNode ext = mconcat [unRef ref, "[color=darkgreen;shape=egg;label=\"", dotEscape name, "\"];"]
    | hasPlaceholder ext = mconcat [unRef ref, "[color=grey;label=\"", dotEscape name, "\"];"]
    | otherwise = mconcat [unRef ref, "[label=\"", dotEscape name, "\"];"]

dotEdge :: Edge -> Text
dotEdge e =
    let (ref1, ref2) = e.rawEdge
     in case e.connection of
            (OV, _) -> mconcat [unRef ref1, "->", unRef ref2]
            (CL, _) -> mconcat [unRef ref1, "->", unRef ref2, "[color=red]"]
            (CR, C) -> mconcat [unRef ref1, "->", unRef ref2, "[color=orange]"]
            (CR, O) -> mconcat [unRef ref1, "->", unRef ref2, "[color=gray]"]
            (CR, V) -> mconcat [unRef ref1, "->", unRef ref2]

dotEscape :: Text -> Text
dotEscape = id

sameNode :: Node a -> Node a -> Bool
sameNode n1 n2 = Maybe.fromMaybe False $ do
    (l, _, _) <- n1
    (r, _, _) <- n2
    pure $ l == r

sameEdge :: Edge -> Edge -> Bool
sameEdge e1 e2 = e1.rawEdge == e2.rawEdge

-------------------------------------------------------------------------------

evalEdges ::
    forall m ext.
    (DotGraphExt ext) =>
    Cofree Graph (OpGraph m (Actions ext)) ->
    [Edge]
evalEdges = go Nothing . fmap node
  where
    go :: Maybe (PrevConnectType, ext) -> Cofree Graph (Actions ext) -> [Edge]
    go prev (a :< (Vertices d)) =
        let set1 = [Edge (ct, V) (l.ref, r.ref) | r <- toList a, (ct, l) <- toList prev]
            next = case a of Actionless -> prev; Actions (Act _ ext) -> Just (OV, ext)
            set2 = concatMap (go next) d
         in set1 <> set2
    go prev (a :< (Overlay d1 d2)) =
        let set1 = [Edge (ct, O) (l.ref, r.ref) | r <- toList a, (ct, l) <- toList prev]
            next = case a of Actionless -> prev; Actions (Act _ ext) -> Just (OV, ext)
            set2 = concatMap (go next) d1
            set3 = concatMap (go next) d2
         in set1 <> set2 <> set3
    go prev (a :< (Connect d1 d2)) =
        let set1 = [Edge (ct, C) (l.ref, r.ref) | r <- toList a, (ct, l) <- toList prev]
            next k = case a of Actionless -> prev; Actions (Act _ ext) -> Just (k, ext)
            set2 = concatMap (go $ next CL) d1
            set3 = concatMap (go $ next CR) d2
         in set1 <> set2 <> set3

-------------------------------------------------------------------------------

evalNodes ::
    (DotGraphExt ext) =>
    Cofree Graph (OpGraph m (Actions ext)) ->
    Cofree Graph (Node ext)
evalNodes = fmap (\x -> mkNode x.node)

mkNode ::
    (DotGraphExt ext) =>
    Actions ext ->
    Node ext
mkNode x =
    case x of
        Actionless ->
            Nothing
        (Actions act) ->
            Just (act.extension.ref, act.shorthand, act.extension)

-------------------------------------------------------------------------------

printDigraph ::
    forall m ext.
    ( Monad m
    , DotGraphExt ext
    ) =>
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO ()
printDigraph nat graph = do
    printCograph =<< nat (expand graph)

printCograph ::
    forall m ext.
    ( Monad m
    , DotGraphExt ext
    ) =>
    Cofree Graph (OpGraph m (Actions ext)) ->
    IO ()
printCograph gr1 = do
    let nodes = fmap dotNode $ List.nubBy sameNode $ toList $ evalNodes gr1
    let edges = fmap dotEdge $ List.nubBy sameEdge $ evalEdges gr1
    putStrLn "digraph {"
    putStrLn "rankdir=LR;"
    traverse_ Text.putStrLn $ nodes
    traverse_ Text.putStrLn $ edges
    putStrLn "}"
