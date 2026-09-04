{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Salmon.Actions.UpDown where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.Dynamic (Dynamic)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Records
import System.Directory (doesDirectoryExist, doesFileExist)

import Salmon.FoldBranch
import Salmon.Op.Actions
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Eval
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
    = Skip !(Act ext)
    | Eval !(Act ext)
    | Done !(Act ext)
    | Failed !(Act ext) !SomeException
    | Blocked !(Act ext)
    | -- | Two nodes in this graph share one 'Ref' — the same effect site,
      -- reached from two declarations that describe it differently. The
      -- second 'Act' is the representative that lost to last-writer-wins and
      -- was /not/ run; the first is the one that replaced it. See
      -- "Salmon.Op.Dag" for why the comparison is a heuristic, and why
      -- last-wins.
      --
      Conflicting !Ref !(Act ext) !(Act ext)
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
    , -- the fields 'Salmon.Op.Dag.sameRepresentative' compares two colliding
      -- representatives on; see 'Conflicting'.
      HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
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
    , -- the fields 'Salmon.Op.Dag.sameRepresentative' compares two colliding
      -- representatives on; see 'Conflicting'.
      HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO Bool
upTreeWith gate r nat graph = upDag gate r =<< expandDag r nat graph

{- | 'upTreeWith' once the graph has already been collapsed: one pass over a
'Dag.Dag' in dependency order, one attempt per node, 'Salmon.Builtin.Extension.check'
then 'Salmon.Builtin.Extension.up'.

The exact mirror of 'downDag', which is the point — the two drivers now share
the collapse, the ordering machinery and the failure containment, and differ
only in which adjacency direction they follow and which action they run. A
long-running driver that keeps a magma and a "Salmon.Op.Ledger" rather than
graphs gets here through 'Dag.fromMagma'.

Termination is structural in the finite 'Dag.Dag' this walks: one attempt per
node, no retries, no waiting. That is what keeps @run up@ a command that
returns rather than a supervisor.
-}
upDag ::
    forall ext.
    ( HasField "up" ext (IO ())
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    Dag ext ->
    IO Bool
upDag gate r dag =
    walk r dag Dag.dependenciesOf Dag.dependantsOf (Dag.leaves dag) apply
  where
    -- returns True iff this node failed, so its dependants must not be run
    -- against an unmet precondition.
    apply :: Act ext -> IO Bool
    apply act = do
        wanted <- gate act
        st <- case wanted of
            Skippable -> pure Skippable
            Required -> requirement <$> runCheck act
        case st of
            Skippable -> do
                runReporter r (Skip act)
                pure False
            Required -> do
                runReporter r (Eval act)
                result <- try @SomeException act.extension.up
                case result of
                    Left e -> do
                        runReporter r (Failed act e)
                        pure True
                    Right () -> do
                        runReporter r (Done act)
                        pure False

{- | The ordering, containment and completeness machinery both drivers share,
parameterised by which way round they read the 'Dag.Dag'.

@ready@ is the direction a node waits on (dependencies for a bring-up,
dependants for a teardown) and @release@ its opposite; @start@ is the nodes
waiting on nothing. A node is applied once everything it waits on is done;
if @apply@ (or a prior 'Blocked') says it is not safe to proceed past this
node, everything it would have released is 'Blocked' instead — one failure
contains a whole sub-DAG rather than a single branch, in whichever direction
that sub-DAG lies.

The final sweep is what a walk over a 'Dag.Dag' needs and a walk over a
'Cofree' did not: an edge set can describe a cycle, and a node on one never
reaches a count of zero. Reporting those 'Blocked' turns "silently did
nothing and claimed success" into a visible failure.
-}
walk ::
    forall ext.
    (HasField "ref" ext Ref) =>
    Reporter (Report ext) ->
    Dag ext ->
    (Dag ext -> Ref -> [Ref]) ->
    (Dag ext -> Ref -> [Ref]) ->
    [Ref] ->
    (Act ext -> IO Bool) ->
    IO Bool
walk r dag ready release start apply = do
    let order = Dag.dagOrder dag
    countRef <- newIORef (Map.fromList [(aref, length (ready dag aref)) | aref <- order])
    blockedRef <- newIORef (Set.empty :: Set Ref)
    doneRef <- newIORef (Set.empty :: Set Ref)
    failRef <- newIORef False
    let
        processNode :: Act ext -> IO Bool
        processNode act = do
            blocked <- Set.member act.extension.ref <$> readIORef blockedRef
            if blocked
                then do
                    runReporter r (Blocked act)
                    writeIORef failRef True
                    pure True
                else apply act >>= \stop -> do
                    when stop (writeIORef failRef True)
                    pure stop

        processReady :: Ref -> IO ()
        processReady aref = do
            modifyIORef' doneRef (Set.insert aref)
            stop <- maybe (pure False) processNode (Dag.representativeOf dag aref)
            forM_ (release dag aref) $ \d -> do
                when stop $ modifyIORef' blockedRef (Set.insert d)
                n <- atomicModifyIORef' countRef $ \m ->
                    let k = Map.findWithDefault 0 d m - 1 in (Map.insert d k m, k)
                when (n == 0) $ processReady d

    mapM_ processReady start

    -- anything a cycle kept from ever becoming ready.
    reached <- readIORef doneRef
    forM_ [aref | aref <- order, Set.notMember aref reached] $ \aref -> do
        forM_ (Dag.representativeOf dag aref) $ \act -> runReporter r (Blocked act)
        writeIORef failRef True

    not <$> readIORef failRef

{- | Everything both drivers do before either of them walks anything: expand
the effectful @predecessors@ recipes, collapse the result to a 'Dag.Dag', and
report any 'Ref' collision the collapse had to resolve.

Exposed rather than inlined because this is the seam a
"Salmon.Op.Rewrite" phase goes in: a caller that has rewrites registered folds
here, rewrites, and hands 'upDag'\/'downDag' the computed graph instead of
the declared one.
-}
expandDag ::
    forall a m ext.
    ( Monad m
    , HasField "ref" ext Ref
    , HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO (Dag ext)
expandDag r nat graph = do
    cofree <- nat (expand graph)
    let dag = Dag.foldDag Dag.sameRepresentative cofree
    reportConflicts r dag
    pure dag

-- | Emit one 'Conflicting' per representative that lost to last-writer-wins,
-- oldest first. Both drivers do this before touching anything.
reportConflicts :: Reporter (Report ext) -> Dag ext -> IO ()
reportConflicts r dag =
    forM_ (reverse (Dag.dagConflicts dag)) $ \c ->
        runReporter r (Conflicting c.conflictRef c.conflictKept c.conflictReplaced)

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

{- | Tears a graph down in reverse-dependency (topological) order: a node is
torn down only after /every/ node that depends on it already has been. This
matters precisely for a predecessor shared by several dependents — e.g. a
directory two files sit in: the naive "walk the tree top-down, dedupe by
'Ref'" would tear that directory down at the /first/ dependent it was reached
through, while the other dependents were still standing on top of it (a
directory-not-empty failure, in the filesystem case). So this does not walk
the 'Cofree' structurally; it first collapses it with
"Salmon.Op.Dag".'Salmon.Op.Dag.foldDag' into a 'Ref'-level DAG that knows
each node's /dependants/ as well as its dependencies, and then tears nodes
down as they become free — a node is processed once all its dependants are
done, and only then are its own predecessors released. A 'Ref' collision
inside the graph is reported 'Conflicting' before anything runs.

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
    , -- the fields 'Salmon.Op.Dag.sameRepresentative' compares two colliding
      -- representatives on; see 'Conflicting'.
      HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO Bool
downTree = downTreeWith alwaysRequired

{- | 'downTree', but only tearing down the nodes a caller-supplied 'Gate'
asks for. Note that, unlike 'upTreeWith', this is the /only/ way a node gets
'Skip'ped on the way down: a node's own 'Salmon.Builtin.Extension.check' is
never consulted for teardown (it is written to answer "does my effect still
need creating", which is not the question a teardown needs answered).

The @help@\/@notes@\/@dynamics@ constraints are
'Salmon.Op.Dag.sameRepresentative''s, not this function's. GHC only solves a
'HasField' constraint when the field selector is in scope, so a caller that
imports 'Salmon.Builtin.Extension' selectively has to name those three
fields even though it never mentions them.
-}
downTreeWith ::
    forall a m ext.
    ( Monad m
    , HasField "down" ext (IO ())
    , HasField "ref" ext Ref
    , -- the fields 'Salmon.Op.Dag.sameRepresentative' compares two colliding
      -- representatives on; see 'Conflicting'.
      HasField "help" ext Text
    , HasField "notes" ext [Text]
    , HasField "dynamics" ext [Dynamic]
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    (forall a. m a -> IO a) ->
    OpGraph m (Actions ext) ->
    IO Bool
downTreeWith gate r nat graph = downDag gate r =<< expandDag r nat graph

{- | 'downTreeWith' once the graph has already been collapsed — the exact
mirror of 'upDag': the same 'walk', read the other way round, running
'Salmon.Builtin.Extension.down' instead of
'Salmon.Builtin.Extension.up'.

A node's own 'Salmon.Builtin.Extension.check' is never consulted here (it
answers "does my effect still need creating", which is not the question a
teardown asks), so a 'Gate' is the only thing that skips a node on the way
down.

Split out from 'downTreeWith' because a long-running driver does not keep
graphs: it keeps a magma and a "Salmon.Op.Ledger" of who still wants what,
and rebuilds something walkable with 'Dag.fromMagma'. Expanding a 'Cofree' is
one way to get here, not the only one.
-}
downDag ::
    forall ext.
    ( HasField "down" ext (IO ())
    , HasField "ref" ext Ref
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    Dag ext ->
    IO Bool
downDag gate r dag =
    walk r dag Dag.dependantsOf Dag.dependenciesOf (Dag.roots dag) apply
  where
    -- returns True iff the node is still standing, so its predecessors must
    -- not be pulled out from under it.
    apply :: Act ext -> IO Bool
    apply act = do
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
                        pure True
                    Right () -> do
                        runReporter r (Done act)
                        pure False
