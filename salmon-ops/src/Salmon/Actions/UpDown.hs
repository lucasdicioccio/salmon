{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Salmon.Actions.UpDown where

import Control.Comonad.Cofree (Cofree (..))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
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
    | Done !(Act ext)
    | Failed !(Act ext) !SomeException
    | Blocked !(Act ext)
    deriving (Show)

-------------------------------------------------------------------------------

{- | What a node's own 'Salmon.Builtin.Extension.check' answers about the
effect that node is responsible for.

This is the merge of what used to be two fields: @prelim :: IO Requirement@,
implemented by 22 nodes and consulted by 'upTreeWith', and @check :: IO ()@,
implemented by none and consulted only by a module with no callers. See
@specs/per-node-state-machines.md@ — the per-node state machines that spec
describes are driven by exactly this answer, so it has to say more than
"should I act".

Four of the five constructors describe the /effect/. 'Skipped' is the odd one
out: it describes a decision someone made /about/ the node, and
'Salmon.Actions.Query.forceSkip' is what produces it.
-}
data CheckResult
    = -- | the effect is in place
      Success
    | -- | treat as satisfied without looking; see 'Salmon.Actions.Query.forceSkip'
      Skipped
    | -- | the effect ran to completion and stopped on purpose — a job rather
      -- than a service. Converged, but not running.
      Completed
    | -- | the effect is not in place, with a reason. Note this is the
      -- ordinary answer on a first run and not an error report: "the file
      -- isn't there yet" and "the file is there but wrong" are the same
      -- answer to the only question 'upTreeWith' asks, which is whether to
      -- run 'up'.
      Failure !Text
    | -- | the check could not tell — including because the node has no check
      -- of its own. Acts like 'Failure' when deciding whether to run 'up',
      -- and is kept distinct so that a supervisor can tell "I looked and it
      -- is gone" from "I could not look".
      Unknown
    deriving (Show, Eq)

{- | Least-satisfied wins, mirroring 'Requirement''s "'Required' wins": if
either half of a merged node still needs doing, the merged node does. Only
reachable through @instance Semigroup Salmon.Builtin.Extension.Extension@,
which nothing on the execution path uses.
-}
instance Semigroup CheckResult where
    Failure a <> Failure b = Failure (a <> "; " <> b)
    Failure a <> _ = Failure a
    _ <> Failure b = Failure b
    Unknown <> _ = Unknown
    _ <> Unknown = Unknown
    Completed <> _ = Completed
    _ <> Completed = Completed
    Skipped <> b = b
    Success <> b = b

data Requirement
    = Required
    | Skippable
    deriving (Show, Ord, Eq)

instance Semigroup Requirement where
    Skippable <> Skippable = Skippable
    _ <> _ = Required

{- | What 'upTreeWith' does with a 'CheckResult'.

'Failure' and 'Unknown' both mean 'Required'. Erring that way is safe because
'Salmon.Builtin.Extension.up' is required to be idempotent regardless (see
CLAUDE.md), and it is the direction that keeps a node with a broken check
converging rather than stalling.
-}
requirement :: CheckResult -> Requirement
requirement Success = Skippable
requirement Skipped = Skippable
requirement Completed = Skippable
requirement (Failure _) = Required
requirement Unknown = Required

skipIfDirectoryIsMissing :: FilePath -> IO CheckResult
skipIfDirectoryIsMissing path = do
    exists <- doesDirectoryExist path
    if not exists
        then pure Success
        else pure (Failure $ "still present: " <> Text.pack path)

skipIfFileExists :: FilePath -> IO CheckResult
skipIfFileExists path = do
    exists <- doesFileExist path
    if exists
        then pure Success
        else pure (Failure $ "missing: " <> Text.pack path)

{- | An extra, caller-supplied precondition, consulted per node /before/ the
node's own opinion about itself is asked for.

Where a node's own 'Salmon.Builtin.Extension.check' answers "is my effect
already in place on this machine", a 'Gate' answers the orthogonal question
"does this traversal want to touch this node at all" — which only the caller
knows. It exists for
"Salmon.Actions.Serve".'Salmon.Actions.Serve.serve', which walks a graph
that is the union of several seeds' graphs and must leave alone the nodes
that belong to some /other/ seed, or that it has already converged.

A 'Gate' returning 'Skippable' short-circuits: for 'upTreeWith' the node's
own 'check' is not even consulted, and either way the node is reported
'Skip'ped. Returning 'Required' means "this traversal does want this node",
and the usual per-node logic proceeds unchanged.
-}
type Gate ext = Act ext -> IO Requirement

-- | The 'Gate' that wants every node: what plain 'upTree'/'downTree' use.
alwaysRequired :: Gate ext
alwaysRequired = const (pure Required)

{- | Returns 'True' iff every node actually ran (or was legitimately
'Skip'ped via 'check') — i.e. 'False' means at least one node threw and
something downstream of it was 'Blocked'. Callers that only care about
side effects (the historical behaviour) can ignore the result; callers that
want a process exit code to reflect reality (e.g. a CLI) now can.
-}
upTree ::
    forall a m ext.
    ( Monad m
    , HasField "up" ext (IO ())
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO Bool
upTree = upTreeWith alwaysRequired

-- | 'upTree', but only touching the nodes a caller-supplied 'Gate' asks for.
upTreeWith ::
    forall a m ext.
    ( Monad m
    , HasField "up" ext (IO ())
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO Bool
upTreeWith gate r nat graph = do
    s <- newIORef (Map.empty :: Map Ref Bool)
    not <$> (postOrderM (upnode s) =<< nat (expand graph))
  where
    -- Bool in: did a predecessor of this node fail? Bool out: did this node (or a predecessor) fail?
    upnode :: IORef (Map Ref Bool) -> Bool -> (OpGraph m (Actions ext)) -> IO Bool
    upnode s predecessorFailed x =
        case x.node of
            -- nothing to run, but the node still has to pass the verdict of
            -- its predecessors along: an 'Actionless' node that answered
            -- 'False' unconditionally would hide a failure underneath it
            -- from everything above it (including this function's own
            -- result, when the graph is rooted on one).
            Actionless -> pure predecessorFailed
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
                            wanted <- gate act
                            st <- case wanted of
                                Skippable -> pure Skippable
                                Required -> requirement <$> runCheck act
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
                                        Right () -> do
                                            runReporter r (Done act)
                                            recordOutcome s aref False

{- | Runs a node's own 'Salmon.Builtin.Extension.check', containing a thrower
as a 'Failure' rather than letting it escape.

This is the one behaviour change in merging @prelim@ into @check@: @prelim@
was evaluated outside the 'try' that wraps 'Salmon.Builtin.Extension.up', so
a @prelim@ that threw took the whole traversal down with it instead of
failing the one node. A check that throws now just means the node's effect
could not be confirmed, and 'requirement' turns that into "run 'up'".
-}
runCheck ::
    (HasField "check" ext (IO CheckResult)) =>
    Act ext ->
    IO CheckResult
runCheck act = do
    result <- try @SomeException act.extension.check
    pure $ case result of
        Left e -> Failure (Text.pack (show e))
        Right x -> x

-- | Shared by 'upTree' and 'downTree': remembers a node's outcome (by 'Ref') so a
-- second encounter (dedup) or a descendant can look it up without re-running anything.
recordOutcome :: IORef (Map Ref Bool) -> Ref -> Bool -> IO Bool
recordOutcome s aref failed = do
    atomicModifyIORef' s (\m -> (Map.insert aref failed m, ()))
    pure failed

{- | Tears a graph down in reverse-dependency (topological) order: a node is
torn down only after /every/ node that depends on it already has been. This
matters precisely for a predecessor shared by several dependents — e.g. a
directory two files sit in: the naive "walk the tree top-down, dedupe by
'Ref'" would tear that directory down at the /first/ dependent it was reached
through, while the other dependents were still standing on top of it (a
directory-not-empty failure, in the filesystem case). So this does not walk
the 'Cofree' structurally; it first collapses it to a 'Ref'-level DAG
(deduping shared nodes, and skipping through 'Actionless' nodes, which carry
no identity) and then tears nodes down as they become free — a node is
processed once all its dependents are done, and only then are its own
predecessors released.

Failure is contained the mirror image of 'upTree's: a node whose 'down' threw
is reported 'Failed', and because it is therefore /still standing/, every one
of its predecessors is 'Blocked' — it would be unsafe to pull a dependency
out from under a node that is still up. A predecessor is likewise blocked if
/any/ of its dependents was blocked, so one failure contains a whole
still-standing sub-DAG rather than a single tree branch. A 'Gate' 'Skip'
(caller says "leave this node alone") is /not/ a failure and does not block
predecessors. Returns 'True' iff everything wanted was actually torn down (no
'Failed'/'Blocked'). Unlike the old structural walk, a shared node is visited
exactly once, so 'downTree' never emits 'Redundant'.
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
downTree = downTreeWith alwaysRequired

{- | 'downTree', but only tearing down the nodes a caller-supplied 'Gate'
asks for. Note that, unlike 'upTreeWith', this is the /only/ way a node gets
'Skip'ped on the way down: a node's own 'prelim' is never consulted for
teardown (it is written to answer "does my effect still need creating",
which is not the question a teardown needs answered).
-}
downTreeWith ::
    forall a m ext.
    ( Monad m
    , HasField "down" ext (IO ())
    , HasField "ref" ext Ref
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO Bool
downTreeWith gate r nat graph = do
    cofree <- nat (expand graph)

    -- 1. Collapse the Cofree to a Ref-level DAG: each node's payload, its
    -- direct predecessor Refs (Actionless nodes skipped), and the order Refs
    -- were first seen (for a stable, top-down-ish teardown order).
    payloadRef <- newIORef (Map.empty :: Map Ref (Act ext))
    depsRef <- newIORef (Map.empty :: Map Ref [Ref])
    orderRef <- newIORef ([] :: [Ref])
    seenRef <- newIORef (Set.empty :: Set Ref)
    let
        goNode :: Cofree Graph (OpGraph m (Actions ext)) -> IO ()
        goNode (x :< g) =
            case x.node of
                Actionless -> mapM_ goNode (effPreds g)
                Actions act -> do
                    let aref = act.extension.ref
                    let preds = effPreds g
                    let predRefs = nubOrd [pr | p <- preds, Just pr <- [refOf p]]
                    seen <- Set.member aref <$> readIORef seenRef
                    unless seen $ do
                        modifyIORef' seenRef (Set.insert aref)
                        modifyIORef' payloadRef (Map.insert aref act)
                        modifyIORef' depsRef (Map.insert aref predRefs)
                        modifyIORef' orderRef (aref :)
                        mapM_ goNode preds
    goNode cofree

    payload <- readIORef payloadRef
    depsMap <- readIORef depsRef
    order <- reverse <$> readIORef orderRef

    -- 2. Count each node's dependents; a node with none is a starting point.
    let depCount0 :: Map Ref Int
        depCount0 =
            Map.fromListWith
                (+)
                ([(aref, 0) | aref <- order] <> [(d, 1) | aref <- order, d <- depsMap Map.! aref])

    -- 3. Tear down: a node becomes ready once its last dependent has released
    -- it; then, unless it's blocked, run its 'down' and release its own
    -- predecessors (blocking them if this node is still standing).
    countRef <- newIORef depCount0
    blockedRef <- newIORef (Set.empty :: Set Ref)
    failRef <- newIORef False
    let
        -- returns True iff the node is still standing (Failed/Blocked), so its
        -- predecessors must not be pulled out from under it.
        processNode :: Ref -> IO Bool
        processNode aref = do
            let act = payload Map.! aref
            blocked <- Set.member aref <$> readIORef blockedRef
            if blocked
                then do
                    runReporter r (Blocked act)
                    writeIORef failRef True
                    pure True
                else do
                    wanted <- gate act
                    case wanted of
                        Skippable -> do
                            runReporter r (Skip act)
                            pure False
                        Required -> do
                            runReporter r (Eval act)
                            result <- try @SomeException act.extension.down
                            case result of
                                Left e -> do
                                    runReporter r (Failed act e)
                                    writeIORef failRef True
                                    pure True
                                Right () -> do
                                    runReporter r (Done act)
                                    pure False

        processReady :: Ref -> IO ()
        processReady aref = do
            standing <- processNode aref
            forM_ (depsMap Map.! aref) $ \d -> do
                when standing $ modifyIORef' blockedRef (Set.insert d)
                n <- atomicModifyIORef' countRef $ \m ->
                    let k = (m Map.! d) - 1 in (Map.insert d k m, k)
                when (n == 0) $ processReady d

    mapM_ processReady [aref | aref <- order, depCount0 Map.! aref == 0]
    not <$> readIORef failRef
  where
    -- The effective predecessors of a node: the nearest 'Ref'-carrying
    -- subtrees below it, descending through 'Actionless' nodes (which are
    -- structural glue with no teardown of their own).
    effPreds ::
        Graph (Cofree Graph (OpGraph m (Actions ext))) ->
        [Cofree Graph (OpGraph m (Actions ext))]
    effPreds g = concatMap pick (toList g)
      where
        pick c@(x :< g') =
            case x.node of
                Actions _ -> [c]
                Actionless -> effPreds g'

    refOf :: Cofree Graph (OpGraph m (Actions ext)) -> Maybe Ref
    refOf (x :< _) =
        case x.node of
            Actions act -> Just act.extension.ref
            Actionless -> Nothing

    -- order-preserving dedup.
    nubOrd :: (Ord b) => [b] -> [b]
    nubOrd = go Set.empty
      where
        go _ [] = []
        go s (y : ys)
            | Set.member y s = go s ys
            | otherwise = y : go (Set.insert y s) ys
