{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Salmon.Actions.UpDown where

import Control.Comonad.Cofree (Cofree (..))
import Data.Foldable (toList, traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records
import System.Directory (doesDirectoryExist, doesFileExist)

import Salmon.FoldBranch
import Salmon.Op.Actions
import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Reporter

-------------------------------------------------------------------------------
data Report ext
    = Redundant !(Act ext)
    | Skip !(Act ext)
    | Eval !(Act ext)
    deriving (Show)

-------------------------------------------------------------------------------

data Requirement
    = Required
    | Skippable
    deriving (Show, Ord, Eq)

instance Semigroup Requirement where
    Skippable <> Skippable = Skippable
    _ <> _ = Required

skipIfDirectoryIsMissing :: FilePath -> IO Requirement
skipIfDirectoryIsMissing path = do
    exists <- doesDirectoryExist path
    if not exists
        then pure Skippable
        else pure Required

skipIfFileExists :: FilePath -> IO Requirement
skipIfFileExists path = do
    exists <- doesFileExist path
    if exists
        then pure Skippable
        else pure Required

upTree ::
    forall a m ext.
    ( Monad m
    , HasField "up" ext (IO ())
    , HasField "prelim" ext (IO Requirement)
    , HasField "ref" ext Ref
    ) =>
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO ()
upTree r nat graph = do
    s <- newIORef (Set.empty)
    runC s =<< nat (expand graph)
  where
    runC :: IORef (Set Ref) -> Cofree Graph (OpGraph m (Actions ext)) -> IO ()
    runC s (x :< Vertices gs) = traverse_ (runC s) gs >> upnode s x
    runC s (x :< Overlay g1 g2) = runG s g1 >> runG s g2 >> upnode s x
    runC s (x :< Connect g1 g2) = runG s g1 >> runG s g2 >> upnode s x

    runG :: IORef (Set Ref) -> Graph (Cofree Graph (OpGraph m (Actions ext))) -> IO ()
    runG s (Vertices gs) = traverse_ (runC s) gs
    runG s (Overlay g1 g2) = traverse_ (runC s) g1 >> traverse_ (runC s) g2
    runG s (Connect g1 g2) = traverse_ (runC s) g1 >> traverse_ (runC s) g2

    upnode :: IORef (Set Ref) -> (OpGraph m (Actions ext)) -> IO ()
    upnode s x =
        case x.node of
            Actionless -> pure ()
            (Actions act) -> do
                got <- atomicModifyIORef' s (\set -> let aref = act.extension.ref in (Set.insert aref set, Set.member aref set))
                if got
                    then do
                        runReporter r (Redundant act)
                    else do
                        st <- act.extension.prelim
                        case st of
                            Skippable -> do
                                runReporter r (Skip act)
                            Required -> do
                                runReporter r (Eval act)
                                act.extension.up

downTree ::
    ( Monad m
    , HasField "down" ext (IO ())
    ) =>
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO ()
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
