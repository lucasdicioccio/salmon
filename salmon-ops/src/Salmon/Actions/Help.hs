{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Salmon.Actions.Help where

import Control.Comonad.Cofree (Cofree)
import Data.Foldable (toList, traverse_)
import qualified Data.Maybe as Maybe
import GHC.Records

import Salmon.FoldBranch
import Salmon.Op.Actions
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref (unRef)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

{- | Per-node path, as the segments (one 'shorthand' per non-'Actionless'
ancestor, root-to-node) that 'query' renders joined by @\/@ ((R4): @run
tree@\/@run dag@ moved to the computed 'Salmon.Op.Dag.Dag' and print
'printDagTree'\/'Salmon.Actions.Dot.printDagCograph' instead — this is
declared-graph-only now). Shared by 'printCograph', "Salmon.Actions.Dot", and
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

{- | (R4) 'printHelpCograph' for a folded, and possibly rewritten, 'Dag'
rather than the declared @Cofree Graph@ — what @run tree@ prints once any
"Salmon.Op.Rewrite" phases are registered, so a batched node is shown once,
under the shorthand\/help the rewrite gave it, rather than as however many
per-package nodes it replaced.

There is no path to print: a 'Dag' is 'Ref'-keyed, not tree-shaped, so a
node reached from several declarations no longer has several positions to
list it at — it is one line, same as it is one node in the traversal that
actually runs. Each line is followed by its dependencies, indented, so the
ordering a rewrite's edges impose (e.g. removals before installs) is still
visible without a hierarchy to draw it in.
-}
printDagTree ::
    (HasField "help" ext Text) =>
    Dag ext ->
    IO ()
printDagTree dag = traverse_ Text.putStrLn (dagLines dag)

dagLines :: (HasField "help" ext Text) => Dag ext -> [Text]
dagLines dag =
    [ line
    | aref <- Dag.dagOrder dag
    , Just act <- [Dag.representativeOf dag aref]
    , line <- nodeLine aref act : depLines aref
    ]
  where
    nodeLine aref act =
        act.shorthand <> " (" <> unRef aref <> ") " <> (extension act).help
    depLines aref =
        [ "  <- " <> Maybe.maybe (unRef dref) (.shorthand) (Dag.representativeOf dag dref)
        | dref <- Dag.dependenciesOf dag aref
        ]
