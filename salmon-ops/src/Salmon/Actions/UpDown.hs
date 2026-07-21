{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Salmon.Actions.UpDown where

import Control.Comonad.Cofree (Cofree (..))
import Control.Exception (SomeException, try)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Records
import System.Directory (doesDirectoryExist, doesFileExist)

import Salmon.FoldBranch
import Salmon.Op.Actions
import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.GraphFold (postOrderM)
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Reporter

-------------------------------------------------------------------------------

{- | 'Failed' and 'Blocked' are new relative to the early history of this
module: 'upTree' used to run every node's 'up' unconditionally and never
looked at whether it actually succeeded (a failing subprocess only ever
showed up, if at all, buried in a 'Salmon.Builtin.Nodes.Binary.Report').
Now a thrown exception from 'up' — which "Salmon.Builtin.Nodes.Binary".'Salmon.Builtin.Nodes.Binary.untrackedExec'
raises on a non-zero exit, so this isn't opt-in per node — is caught,
reported as 'Failed', and every node that (transitively) depends on it gets
'Blocked' instead of being evaluated against an unmet precondition. Nodes
outside that failed subtree are untouched: one broken branch doesn't halt
the whole traversal.
-}
data Report ext
    = Redundant !(Act ext)
    | Skip !(Act ext)
    | Eval !(Act ext)
    | Failed !(Act ext) !SomeException
    | Blocked !(Act ext)
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

{- | Returns 'True' iff every node actually ran (or was legitimately
'Skip'ped via 'prelim') — i.e. 'False' means at least one node threw and
something downstream of it was 'Blocked'. Callers that only care about
side effects (the historical behaviour) can ignore the result; callers that
want a process exit code to reflect reality (e.g. a CLI) now can.
-}
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
    IO Bool
upTree r nat graph = do
    s <- newIORef (Map.empty :: Map Ref Bool)
    not <$> (postOrderM (upnode s) =<< nat (expand graph))
  where
    -- Bool in: did a predecessor of this node fail? Bool out: did this node (or a predecessor) fail?
    upnode :: IORef (Map Ref Bool) -> Bool -> (OpGraph m (Actions ext)) -> IO Bool
    upnode s predecessorFailed x =
        case x.node of
            Actionless -> pure False
            (Actions act) -> do
                let aref = act.extension.ref
                already <- atomicModifyIORef' s (\m -> (m, Map.lookup aref m))
                case already of
                    Just failed -> do
                        runReporter r (Redundant act)
                        pure failed
                    Nothing
                        | predecessorFailed -> do
                            runReporter r (Blocked act)
                            recordOutcome s aref True
                        | otherwise -> do
                            st <- act.extension.prelim
                            case st of
                                Skippable -> do
                                    runReporter r (Skip act)
                                    recordOutcome s aref False
                                Required -> do
                                    runReporter r (Eval act)
                                    result <- try @SomeException act.extension.up
                                    case result of
                                        Left e -> do
                                            runReporter r (Failed act e)
                                            recordOutcome s aref True
                                        Right () -> recordOutcome s aref False

-- | Shared by 'upTree' and 'downTree': remembers a node's outcome (by 'Ref') so a
-- second encounter (dedup) or a descendant can look it up without re-running anything.
recordOutcome :: IORef (Map Ref Bool) -> Ref -> Bool -> IO Bool
recordOutcome s aref failed = do
    atomicModifyIORef' s (\m -> (Map.insert aref failed m, ()))
    pure failed

{- | Tears a graph down, dedupe-by-'Ref' and failure-contained the same way
'upTree' is (see its haddock): a node reachable via multiple paths is only
torn down once (reported 'Redundant' afterwards), and if 'down' throws,
that's caught and reported 'Failed'. Teardown walks top-to-bottom — the node
itself before whatever it depended on, the reverse of 'upTree's order, since
it's generally not safe to remove a dependency while something built on top
of it is still standing — so a failure here /blocks descending further into
that same node's own predecessors/ (the opposite propagation direction from
'upTree's "a failed predecessor blocks its dependents"), reported 'Blocked'.
Nodes reached via an unrelated path are untouched. Returns 'True' iff
everything was actually torn down (no 'Failed'/'Blocked').
-}
downTree ::
    forall a m ext.
    ( Monad m
    , HasField "down" ext (IO ())
    , HasField "ref" ext Ref
    ) =>
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO Bool
downTree r nat graph = do
    s <- newIORef (Map.empty :: Map Ref Bool)
    gr1 <- nat (expand graph)
    not <$> go s False gr1
  where
    -- Bool in: did the parent (the node we're a predecessor of) fail/get blocked?
    -- Bool out: did this node, or anything in its own predecessor subtree, fail?
    go :: IORef (Map Ref Bool) -> Bool -> Cofree Graph (OpGraph m (Actions ext)) -> IO Bool
    go s blockedByParent (x :< g) = do
        selfFailed <- downnode s blockedByParent x
        childFailed <- or <$> mapM (go s selfFailed) (toList g)
        pure (selfFailed || childFailed)

    downnode :: IORef (Map Ref Bool) -> Bool -> OpGraph m (Actions ext) -> IO Bool
    downnode s blockedByParent x =
        case x.node of
            Actionless -> pure False
            (Actions act) -> do
                let aref = act.extension.ref
                already <- atomicModifyIORef' s (\m -> (m, Map.lookup aref m))
                case already of
                    Just failed -> do
                        runReporter r (Redundant act)
                        pure failed
                    Nothing
                        | blockedByParent -> do
                            runReporter r (Blocked act)
                            recordOutcome s aref True
                        | otherwise -> do
                            runReporter r (Eval act)
                            result <- try @SomeException act.extension.down
                            case result of
                                Left e -> do
                                    runReporter r (Failed act e)
                                    recordOutcome s aref True
                                Right () -> recordOutcome s aref False
