{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The concurrent driver: one thread per node, ordering by STM rather than
by counters on a traversal's stack.

Same contract as "Salmon.Actions.UpDown"'s synchronous drivers — same
'UpDown.Report' stream, same @'IO' 'Bool'@, same failure containment — and
the same one pass, one attempt per node. What differs is that independent
subtrees no longer wait for each other, and that each node has state of its
own while it runs, which is what everything after this is built on.

= How ordering works here

Each node gets a @'TVar' 'Status'@ and a thread. The thread blocks on
'waitStability' over its neighbours in the direction it waits — dependencies
going up, dependants coming down — and 'retry' does the scheduling. No
counters, no ready-queue, no wakeup channel: a node wakes exactly when a
neighbour's state changes and not otherwise. The teardown ordering that
'UpDown.walk' spends a countdown map on is the same code with the two
adjacency directions swapped.

= Failure is still a property of the pass

'waitStability' reads direction and stability only, so a node that settled
having failed looks exactly like one that settled having succeeded. That is
deliberate — whether to proceed past a failure is the /driver's/ policy, not
the neighbour's property — and this driver answers it the way the synchronous
one does: a node whose neighbour did not succeed reports 'UpDown.Blocked',
settles, and contains the failure to that sub-DAG. A supervisor would answer
it differently (wait, because the neighbour may yet be repaired), which is
why it is not baked into 'Status'.

= Two things concurrency forces that the sequential drivers never had to face

* __Reports are serialised.__ Every 'runReporter' call goes through one
  'MVar', so a multi-line report cannot interleave with another node's. The
  reporter belongs to the caller and cannot be assumed thread-safe.
* __A cycle has to be found before the walk, not after.__ The sequential
  drivers discover unreachable nodes by finishing and noticing what they
  never touched. A thread waiting on a node in a cycle simply never wakes, so
  'Dag.stuck' is consulted up front and those nodes are reported 'Blocked'
  without being spawned.

= Instructions

A node consults its mailbox once, immediately before deciding what to do —
which is all a single-pass driver can honour. 'Force' makes it act regardless
of what 'check' or the 'UpDown.Gate' says; 'Satisfy' makes it treat the node
as already done. The last of those two in the mailbox wins, since a later
instruction supersedes an earlier statement of intent. 'Recheck', 'Pause' and
'Resume' are meaningful only to a driver that tends a node continuously; here
they are read, reported and otherwise ignored.
-}
module Salmon.Actions.Concurrent (
    upDagConcurrent,
    downDagConcurrent,
    noMailboxes,
) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Records (HasField)

import Salmon.Actions.UpDown (CheckResult (..), Gate, Report (..), Requirement (..), requirement, runCheck)
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Dag (Dag)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..), Mailbox)
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.Ref (Ref)
import Salmon.Op.Status (Direction (..), Stability (..), Status (..), newStatus, note, waitStability)
import Salmon.Reporter

-- | No node can be instructed: what a driver with no control plane in front
-- of it passes.
noMailboxes :: Map Ref Mailbox
noMailboxes = Map.empty

{- | 'Salmon.Actions.UpDown.upDag', concurrently. Every node runs as soon as
the nodes it depends on have settled, rather than as soon as the traversal
gets to it.
-}
upDagConcurrent ::
    forall ext.
    ( HasField "up" ext (IO ())
    , HasField "check" ext (IO CheckResult)
    , HasField "ref" ext Ref
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    Map Ref Mailbox ->
    Dag ext ->
    IO Bool
upDagConcurrent gate r boxes dag =
    walkConcurrent TurnUp r boxes dag Dag.dependenciesOf apply
  where
    apply :: Say ext -> TVar Status -> Act ext -> [Instruction] -> IO CheckResult
    apply say status act instructions = do
        wanted <- decide
        case wanted of
            Skippable -> do
                say (Skip act)
                pure Skipped
            Required -> do
                say (Eval act)
                note status "eval"
                result <- try @SomeException act.extension.up
                case result of
                    Left e -> do
                        say (Failed act e)
                        note status (Text.pack (show e))
                        pure (Failure (Text.pack (show e)))
                    Right () -> do
                        say (Done act)
                        note status "done"
                        pure Success
      where
        decide =
            case override instructions of
                Just Force -> pure Required
                Just Satisfy -> pure Skippable
                _ -> do
                    asked <- gate act
                    case asked of
                        Skippable -> pure Skippable
                        Required -> requirement <$> runCheck act

{- | 'Salmon.Actions.UpDown.downDag', concurrently. A node comes down as soon
as everything standing on it has, which is the same STM wait with the two
adjacency directions swapped.

A node's own 'Salmon.Builtin.Extension.check' is not consulted, as in the
sequential teardown: it answers "does my effect still need creating", which
is not the question. 'Force' and 'Satisfy' still apply, since they are
statements about whether to act at all.
-}
downDagConcurrent ::
    forall ext.
    ( HasField "down" ext (IO ())
    , HasField "ref" ext Ref
    ) =>
    Gate ext ->
    Reporter (Report ext) ->
    Map Ref Mailbox ->
    Dag ext ->
    IO Bool
downDagConcurrent gate r boxes dag =
    walkConcurrent TurnDown r boxes dag Dag.dependantsOf apply
  where
    apply :: Say ext -> TVar Status -> Act ext -> [Instruction] -> IO CheckResult
    apply say status act instructions = do
        wanted <- decide
        case wanted of
            Skippable -> do
                say (Skip act)
                pure Skipped
            Required -> do
                say (Eval act)
                note status "eval"
                result <- try @SomeException act.extension.down
                case result of
                    Left e -> do
                        say (Failed act e)
                        note status (Text.pack (show e))
                        pure (Failure (Text.pack (show e)))
                    Right () -> do
                        say (Done act)
                        note status "done"
                        pure Success
      where
        decide =
            case override instructions of
                Just Force -> pure Required
                Just Satisfy -> pure Skippable
                _ -> gate act

-------------------------------------------------------------------------------

-- | A serialised 'runReporter': see the module header on why.
type Say ext = Report ext -> IO ()

-- | The last 'Force' or 'Satisfy' in the mailbox, if either is there. Later
-- supersedes earlier: these are statements of current intent.
override :: [Instruction] -> Maybe Instruction
override = go Nothing
  where
    go acc [] = acc
    go acc (i : is)
        | i `elem` [Force, Satisfy] = go (Just i) is
        | otherwise = go acc is

{- | The ordering, containment and completeness machinery both concurrent
drivers share, parameterised by which adjacency direction a node waits on.

@apply@ returns what the node has to say about itself afterwards, in the
vocabulary of the direction it is going: 'Success' means the node reached the
state this pass wanted (the effect is up, or the effect is gone), 'Failure'
that it did not, 'Skipped' that nobody asked it to try.
-}
walkConcurrent ::
    forall ext.
    (HasField "ref" ext Ref) =>
    Direction ->
    Reporter (Report ext) ->
    Map Ref Mailbox ->
    Dag ext ->
    (Dag ext -> Ref -> [Ref]) ->
    (Say ext -> TVar Status -> Act ext -> [Instruction] -> IO CheckResult) ->
    IO Bool
walkConcurrent dir r boxes dag waitsOn apply = do
    let order = Dag.dagOrder dag
    let stuckRefs = Dag.stuck waitsOn dag

    statuses <- Map.fromList <$> traverse (\aref -> (,) aref <$> newStatus dir) order
    -- nodes that did not reach what this pass wanted. Read by a node's
    -- neighbours once they have settled, which is why it is a TVar and not
    -- an IORef.
    failedVar <- newTVarIO (Set.empty :: Set Ref)

    reportLock <- newMVar ()
    let say :: Say ext
        say rep = withMVar reportLock (\() -> runReporter r rep)

    -- A node on a cycle never becomes ready, so it is never spawned; nothing
    -- live waits on it, since everything that does is on or behind the same
    -- cycle and therefore also here.
    forM_ [aref | aref <- order, Set.member aref stuckRefs] $ \aref -> do
        forM_ (Dag.representativeOf dag aref) $ \act -> say (Blocked act)
        atomically (modifyTVar' failedVar (Set.insert aref))

    forConcurrently_ [aref | aref <- order, Set.notMember aref stuckRefs] $ \aref ->
        forM_ (Dag.representativeOf dag aref) $ \act -> do
            let status = statuses Map.! aref
            let neighbours = waitsOn dag aref
            atomically (waitStability dir Stable [statuses Map.! n | n <- neighbours])
            blocked <- anyFailed failedVar neighbours
            outcome <-
                if blocked
                    then do
                        say (Blocked act)
                        note status "blocked"
                        pure (Failure "a neighbour did not settle as wanted")
                    else do
                        instructions <- takeInstructions say act aref
                        -- a node's own thread throwing would leave its
                        -- neighbours waiting forever, so nothing is allowed
                        -- to escape here even though `apply` catches already.
                        escaped <- try @SomeException (apply say status act instructions)
                        case escaped of
                            Right result -> pure result
                            Left e -> do
                                say (Failed act e)
                                pure (Failure (Text.pack (show e)))
            -- recording the outcome and settling has to be one transaction:
            -- a neighbour that saw `Stable` before the failure was recorded
            -- would proceed against a node that had in fact failed.
            atomically $ do
                unless (succeeded outcome) $ modifyTVar' failedVar (Set.insert aref)
                modifyTVar' status $ \st ->
                    st{statusStability = Stable, statusCheck = outcome}

    failed <- readTVarIO failedVar
    pure (Set.null failed)
  where
    succeeded :: CheckResult -> Bool
    succeeded (Failure _) = False
    succeeded Unknown = False
    succeeded _ = True

    anyFailed :: TVar (Set Ref) -> [Ref] -> IO Bool
    anyFailed var neighbours = atomically $ do
        f <- readTVar var
        pure (any (`Set.member` f) neighbours)

    takeInstructions :: Say ext -> Act ext -> Ref -> IO [Instruction]
    takeInstructions say act aref =
        case Map.lookup aref boxes of
            Nothing -> pure []
            Just box -> do
                before <- Mailbox.dropped box
                instructions <- atomically (Mailbox.takeAll box)
                unless (before == 0) $ say (DroppedInstructions act before)
                forM_ instructions $ \i -> say (Instructed act i)
                pure instructions
