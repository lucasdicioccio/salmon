{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0/1 coverage for "Salmon.Actions.Upkeep": the continuous driver.

The one-shot drivers can be asserted on by running them to completion and
reading the report list. Nothing here completes, so every case instead
'startUpkeep's, waits on the report stream for the state it is looking for,
pokes the world, waits again, and stops. The waiting is STM on a 'TVar' of
reports rather than @threadDelay@, so a case that passes does so as fast as
the machines run and a case that fails fails by timing out rather than by
flaking.

Four groups. First, that a node is tended at all: satisfied nodes are left
alone, unsatisfied ones are brought up, and the ordering guarantees the
one-shot drivers have still hold. Second, the part that only exists here —
the effect going away brings the node back, the restart policy decides
whether it does, and a check that cannot tell decides nothing. Third, the
control surface: instructions that only mean something to a continuous
driver, and the watchdog. Fourth, the two groups at the end, for the two
things a node can be beyond an @up@ that returns: one that owns the process
it stands for, and one whose going away takes its dependants with it.
-}
module Test.UpkeepSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Control.Monad (unless, void)
import Data.Dynamic (toDyn)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import System.Timeout (timeout)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.Upkeep (DownkeepState (..), Report (..), Standing (..), Supervisor, Tend (..), UpkeepState (..))
import qualified Salmon.Actions.Upkeep as Upkeep
-- imported with their field selectors: OverloadedRecordDot only solves
-- HasField for fields whose selector is in scope, and 'Upkeep' asks for
-- several this module never mentions by name.
import Salmon.Builtin.Extension (Extension, Op, check, deps, down, dynamics, evalDeps, help, managed, nodeps, notes, op, opAct, ref, up)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Status (Direction (..))
import Salmon.Op.Supervision (Restart (..), Strategy (..), Supervision (..), defaultSupervision, millis, seconds, supervised)
import Salmon.Reporter (ReporterM (..))
import System.Directory (doesDirectoryExist, removeDirectory)
import System.FilePath ((</>))
import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Upkeep"
        [ testCase "the adaptive delay clamps at both ends" delayClamps
        , testCase "a satisfied node is not upped, and rests" satisfiedRests
        , testCase "an unsatisfied node is upped, then rests" unsatisfiedIsUpped
        , testCase "a check that cannot tell is not evidence to act on" unknownDoesNotSpin
        , testCase "the effect going away brings the node back" vanishedComesBack
        , testCase "Restart Never leaves a fallen-over node alone" neverLeavesItAlone
        , testCase "Restart Always acts on a Completed node" alwaysActsOnCompleted
        , testCase "a dependant waits for its dependency" dependantWaits
        , testCase "a failing dependency is waited out, not Blocked" failureIsWaitedOut
        , testCase "an untended neighbour is not waited on" untendedNotWaited
        , testCase "a cycle is reported rather than hanging" cycleIsReported
        , testCase "teardown waits for every dependant" teardownOrdering
        , testCase "Pause stops tending; Resume starts again" pauseAndResume
        , testCase "a silent node past its watchdog is reported" watchdogFires
        , testCase "two supervision policies on one node are reported" policyConflict
        , testCase "stopping waits for an up in flight rather than cutting it" stopWaitsForUp
        , testCase "a node already standing is watched, not re-upped" settledIsNotReUpped
        , testCase "a node with no check parks instead of polling" immaterialParks
        , testCase "a parked node still hears a dependency go away" parkedNodeIsStillDemotable
        , testCase "a node waiting on a slow dependency is not itself wedged" watchdogSkipsWaiters
        , testGroup
            "a node that owns its effect"
            [ testCase "is Up for as long as its action runs" managedIsUpWhileRunning
            , testCase "exiting cleanly is Completed, not a restart" managedCleanExitRests
            , testCase "exiting non-zero is restarted" managedFailureRestarts
            , testCase "Restart Never respects even a crash" managedNeverStaysDown
            , testCase "Restart Always restarts a clean exit too" managedAlwaysRestarts
            , testCase "a check that says the effect is there survives a 0 exit" managedDoubleFork
            , testCase "giving up latches off until forced" managedGivesUp
            , testCase "having run a while resets the failure count" managedStableResets
            , testCase "cancelling tears the action down through its bracket" managedCancelTearsDown
            , testCase "is never told it is already standing" managedIgnoresSettled
            , testCase "Force restarts it rather than skipping it" managedForceRestarts
            ]
        , testGroup
            "a node that takes its dependants with it"
            [ testCase "a dependant already up is sent back, and comes back" restForOneDemotes
            , testCase "the default strategy leaves the dependant alone" oneForOneLeavesItAlone
            , testCase "a demoted dependant waits rather than acting" demotedWaitsForItsDependency
            , testCase "it cascades along the dependants that opted in" demotionCascades
            , testCase "a node with no dependants demotes nobody" noDependantsCostsNothing
            , testCase "a second departure in quick succession is dropped" flapIsRateLimited
            , testCase "a dependency coming up for the first time demotes nobody" settledStartIsNotDemoted
            , testCase "a dependant that owns a process is torn down and respawned" managedDependantIsRestarted
            , testCase "a torn-down process is put back whatever its own check says" managedDemotionOutranksItsOwnCheck
            , testCase "a node that lost nothing still asks its check" oneShotDemotionAsksItsCheck
            ]
        , testGroup
            "a node that reapplies instead of asking (supReapply)"
            [ testCase "it re-runs up on the loop rather than parking" reapplyRunsAgain
            , testCase "a successful reapply never re-enters Upping" reapplyStaysInUp
            , testCase "reapplying a RestForOne node does not demote its dependants" reapplyDoesNotDemoteDependants
            , testCase "a throwing reapply is a real failure, backed off and given up on" failingReapplyGivesUp
            , testCase "a node holding an action ignores supReapply and parks" managedIgnoresSupReapply
            , testCase "Filesystem.dir puts itself back, unsupervised by anybody else" dirSelfHeals
            ]
        ]

-------------------------------------------------------------------------------
-- driving a supervisor

{- | Reports, newest first, in a 'TVar' so a case can block on them in STM
rather than sleeping.
-}
type Trace = TVar [Report Extension]

-- | Fail the test rather than hanging if a machine never gets where it should.
within :: Int -> IO a -> IO a
within secs act = do
    result <- timeout (secs * 1000000) act
    maybe (fail ("timed out after " <> show secs <> "s")) pure result

{- | Block until the reports so far (oldest first) satisfy the predicate.
Combined with 'within', this is the whole of how these cases synchronise:
never "wait 200ms and hope", always "wait until the machine says so".
-}
await :: Trace -> ([Report Extension] -> Bool) -> IO ()
await trace p =
    atomically $ do
        rs <- readTVar trace
        unless (p (reverse rs)) retry

seen :: Trace -> IO [Report Extension]
seen trace = reverse <$> atomically (readTVar trace)

dagOf :: Op -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps

-- | Start a supervisor over this graph, run the body, stop it.
supervising ::
    Dag.Dag Extension ->
    (Ref -> Maybe Tend) ->
    (Supervisor Extension -> Trace -> IO a) ->
    IO a
supervising dag tend body = do
    trace <- newTVarIO []
    let r = ReporterM $ \rep -> atomically (modifyTVar' trace (rep :))
    -- a bracket, so a failing assertion does not leave machines running
    -- into the next case.
    Upkeep.withUpkeep r tend dag (\sup -> body sup trace)

{- | Everything brought up from scratch — the shape a supervisor takes when
nothing has run yet. 'Settled' is what @serve@ passes for a node a pass has
already dealt with; 'restingUp' below covers that.
-}
allUp :: Ref -> Maybe Tend
allUp = const (Just (Tend TurnUp Unsettled))

-- | Everything taken down from scratch.
allDown :: Ref -> Maybe Tend
allDown = const (Just (Tend TurnDown Unsettled))

-- | Everything already up: watched, not applied.
restingUp :: Ref -> Maybe Tend
restingUp = const (Just (Tend TurnUp Settled))

-------------------------------------------------------------------------------
-- report predicates

evals :: [Report Extension] -> [Text]
evals rs = [act.shorthand | Acted (UpDown.Eval act) <- rs]

skips :: [Report Extension] -> [Text]
skips rs = [act.shorthand | Acted (UpDown.Skip act) <- rs]

blockeds :: [Report Extension] -> [Text]
blockeds rs = [act.shorthand | Acted (UpDown.Blocked act) <- rs]

reached :: UpkeepState -> [Report Extension] -> [Text]
reached want rs = [act.shorthand | Upkeep act st <- rs, st == want]

reachedDown :: DownkeepState -> [Report Extension] -> [Text]
reachedDown want rs = [act.shorthand | Downkeep act st <- rs, st == want]

{- | How many times any machine has come round its 'Up' loop and settled
down to wait again. 'Parked' counts alongside 'NextLook' because it is the
same event said about a node with nothing to poll for: the machine finished
a turn and is waiting on its mailbox rather than on a timer. A case that
counted only 'NextLook' would hang forever on a node with no @check@, which
is most of them. 'Reapplying' counts too, for the same reason on a node
that declared 'Salmon.Op.Supervision.supReapply'.
-}
looks :: [Report Extension] -> Int
looks rs = length [() | r <- rs, waiting r]

waiting :: Report Extension -> Bool
waiting NextLook{} = True
waiting Parked{} = True
waiting Reapplying{} = True
waiting _ = False

-- | Which node was sent back to 'WaitUp', and by which dependency.
demotions :: [Report Extension] -> [(Text, Ref)]
demotions rs = [(act.shorthand, dep) | Demoted act dep <- rs]

-- | How many times this one node has said what it is waiting on next: the
-- way a case waits for one machine to have been round its loop again.
looksAt :: Text -> [Report Extension] -> Int
looksAt name rs = length (filter (== name) (waiters rs))

-- | Which node said it was settling down to wait, in order. See 'looks'.
waiters :: [Report Extension] -> [Text]
waiters rs =
    [ a.shorthand
    | r <- rs
    , a <- case r of
        NextLook a' _ _ -> [a']
        Parked a' -> [a']
        Reapplying a' _ -> [a']
        _ -> []
    ]

-- | How many times this one node ran its @up@ (or spawned its action).
evalsOf :: Text -> [Report Extension] -> Int
evalsOf name rs = length (filter (== name) (evals rs))

reachedBy :: Text -> UpkeepState -> [Report Extension] -> Int
reachedBy name want rs = length (filter (== name) (reached want rs))

-------------------------------------------------------------------------------
-- building nodes

counter :: IO (IORef Int, IO ())
counter = do
    v <- newIORef 0
    pure (v, atomicModifyIORef' v (\n -> (n + 1, ())))

node :: Text -> (Extension -> Extension) -> Op
node name f = op name nodeps (\x -> f x{ref = mkRef "upkeep" name})

nodeOn :: Text -> [Op] -> (Extension -> Extension) -> Op
nodeOn name preds f = op name (deps preds) (\x -> f x{ref = mkRef "upkeep" name})

refOf :: Text -> Ref
refOf = mkRef "upkeep"

-------------------------------------------------------------------------------

delayClamps :: IO ()
delayClamps = do
    let floored = iterate Upkeep.attentive Upkeep.initialDelay !! 10
    let capped = iterate Upkeep.relaxed Upkeep.initialDelay !! 20
    assertEqual "halving stops at the floor" Upkeep.delayFloor (Upkeep.delayMicros floored)
    assertEqual "doubling stops at the cap" Upkeep.delayCap (Upkeep.delayMicros capped)
    assertBool
        "one relaxation is a real increase"
        (Upkeep.delayMicros (Upkeep.relaxed Upkeep.initialDelay) > Upkeep.delayFloor)

-- | The common case, and the one that has to cost nothing: a node whose
-- effect is already in place is not touched, and its machine settles into
-- watching it.
satisfiedRests :: IO ()
satisfiedRests = within 10 $ do
    (ran, bump) <- counter
    let o = node "sat" $ \x -> x{check = pure Success, up = bump}
    rs <- supervising (dagOf o) allUp $ \_ trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        seen trace
    assertEqual "nothing ran" 0 =<< readIORef ran
    assertEqual "reported as skipped" ["sat"] (skips rs)
    assertEqual "and never evaluated" [] (evals rs)

unsatisfiedIsUpped :: IO ()
unsatisfiedIsUpped = within 10 $ do
    (ran, bump) <- counter
    let o = node "unsat" $ \x -> x{check = pure (Failure "not yet"), up = bump}
    rs <- supervising (dagOf o) allUp $ \_ trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        seen trace
    assertEqual "ran once" 1 =<< readIORef ran
    assertEqual "evaluated" ["unsat"] (evals rs)
    assertEqual "passed through Upping on the way" ["unsat"] (reached Upping rs)

{- | The refinement this module makes to the spec's rule: 'Unknown' is not
evidence the effect went away, so it must not restart anything. Were it
treated the way the one-shot drivers treat it — as
'Salmon.Actions.UpDown.Required' — this node would re-run @up@ at the delay
floor for as long as the process lived.

The check here answers 'Unknown' explicitly. That used to be the same thing
as having no check at all; it is not any more (see 'immaterialParks'), and
the two rules are worth pinning separately: this one is about a check that
ran and could not tell, which is a node that keeps being asked.
-}
unknownDoesNotSpin :: IO ()
unknownDoesNotSpin = within 10 $ do
    (ran, bump) <- counter
    let o = node "quiet" $ \x -> x{check = pure Unknown, up = bump}
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "upped once on the way in" 1 =<< readIORef ran
        -- Recheck collapses the delay and looks now, so this does not wait
        -- out a real nap to prove the second look happened.
        void (Upkeep.instruct sup (refOf "quiet") Recheck)
        await trace (\rs -> looks rs >= 2)
        assertEqual "and looking again did not re-up it" 1 =<< readIORef ran
        rs <- seen trace
        assertEqual "a node with a check is polled, not parked" 0 (length [() | Parked{} <- rs])

{- | The other half of the same story, and the one that covers most of this
repository. A node with no @check@ answers 'Immaterial' — "asking costs what
applying costs" — and there is then nothing for a timer to be for, so the
machine parks on its mailbox instead of waking to be told the same thing at
the delay cap forever.

It takes exactly one look to get there, and that is not an oversight: what
a machine knows on the way in is that its @up@ ran, not what its check would
say about it. It announces one 'NextLook', asks once, is told 'Immaterial',
and never asks again — which is the difference between one wasted check per
supervisor and one per minute forever.

Parked is not unwatched: the operator still gets through, which is what the
'Recheck' here shows. That it is answered with another 'Parked' rather than
a 'NextLook' is the point — the node looked, learned nothing again, and went
straight back to waiting.
-}
immaterialParks :: IO ()
immaterialParks = within 10 $ do
    (ran, bump) <- counter
    -- no `check` at all, so `runCheck` answers Immaterial.
    let o = node "cheap" $ \x -> x{up = bump}
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "upped once on the way in" 1 =<< readIORef ran
        await trace (\rs -> not (null [() | Parked{} <- rs]))
        rs0 <- seen trace
        assertEqual "one look, and then it knew" 1 (length [() | NextLook{} <- rs0])
        void (Upkeep.instruct sup (refOf "cheap") Recheck)
        await trace (\rs -> looks rs >= 3)
        rs <- seen trace
        assertEqual "and no further look was ever announced" 1 (length [() | NextLook{} <- rs])
        assertEqual "being woken did not re-up it" 1 =<< readIORef ran

{- | Parking must not cost the node the one thing supervision is for. A
'Salmon.Op.Supervision.RestForOne' dependency going away is an event, not a
timer, so a parked dependant still hears it and is brought up again on top
of whatever the dependency turns into.
-}
parkedNodeIsStillDemotable :: IO ()
parkedNodeIsStillDemotable = within 20 $ do
    (cfg, _, there) <- breakable "cfg" [] restForOne
    -- no check, so it parks the moment it is up
    (svc, svcRan) <- counted "svc" [cfg] id
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        await trace (\rs -> not (null [() | Parked a <- rs, a.shorthand == "svc"]))
        assertEqual "up once so far" 1 =<< readIORef svcRan
        breakIt sup there "cfg"
        await trace (\rs -> reachedBy "svc" Up rs >= 2)
        rs <- seen trace
        assertEqual "the parked node was sent back" [("svc", refOf "cfg")] (demotions rs)
        assertEqual "and brought up again on the new config" 2 =<< readIORef svcRan

{- | The point of the whole module: a node that was up and is not any more
gets put back, with nobody re-declaring anything.
-}
vanishedComesBack :: IO ()
vanishedComesBack = within 10 $ do
    there <- newIORef False
    (ran, bump) <- counter
    let o =
            node "svc" $
                \x ->
                    x
                        { check = do
                            ok <- readIORef there
                            pure (if ok then Success else Failure "gone")
                        , up = bump >> writeIORef there True
                        }
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "brought up once" 1 =<< readIORef ran
        -- the effect disappears behind salmon's back
        writeIORef there False
        void (Upkeep.instruct sup (refOf "svc") Recheck)
        await trace (\rs -> length (evals rs) >= 2)
        assertEqual "and was put back" 2 =<< readIORef ran

neverLeavesItAlone :: IO ()
neverLeavesItAlone = within 10 $ do
    there <- newIORef False
    (ran, bump) <- counter
    let o =
            node "once" $
                \x ->
                    x
                        { check = do
                            ok <- readIORef there
                            pure (if ok then Success else Failure "gone")
                        , up = bump >> writeIORef there True
                        , dynamics = [supervised defaultSupervision{supRestart = Never}]
                        }
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        writeIORef there False
        void (Upkeep.instruct sup (refOf "once") Recheck)
        await trace (\rs -> looks rs >= 2)
        assertEqual "the policy said don't, so it didn't" 1 =<< readIORef ran

{- | 'Completed' is a satisfied verdict — a job that ran and stopped on
purpose — so 'Always' acting on it only works if the policy is consulted
before satisfaction is, which is what this pins down.
-}
alwaysActsOnCompleted :: IO ()
alwaysActsOnCompleted = within 10 $ do
    (ran, bump) <- counter
    let o =
            node "job" $
                \x ->
                    x
                        { check = pure Completed
                        , up = bump
                        , dynamics = [supervised defaultSupervision{supRestart = Always}]
                        }
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "a Completed check skipped it on the way in" ["job"] . skips =<< seen trace
        void (Upkeep.instruct sup (refOf "job") Recheck)
        await trace (\rs -> not (null (evals rs)))
        assertEqual "but Always ran it anyway" 1 =<< readIORef ran

dependantWaits :: IO ()
dependantWaits = within 10 $ do
    gate <- newEmptyMVar
    (ran, bump) <- counter
    let dep = node "dep" $ \x -> x{up = takeMVar gate}
        top = nodeOn "top" [dep] $ \x -> x{up = bump}
    supervising (dagOf top) allUp $ \_ trace -> do
        await trace (\rs -> "dep" `elem` evals rs)
        assertEqual "the dependant has not run" 0 =<< readIORef ran
        putMVar gate ()
        await trace (\rs -> "top" `elem` evals rs)
        assertEqual "and now it has" 1 =<< readIORef ran

{- | The sharpest difference from the one-shot drivers. There, a node whose
dependency failed is reported 'Salmon.Actions.UpDown.Blocked' and the pass
ends. Here the dependency's own machine is still retrying, so the dependant
waits and proceeds the moment the dependency recovers — no 'Blocked', and
nobody re-declares anything.
-}
failureIsWaitedOut :: IO ()
failureIsWaitedOut = within 20 $ do
    attempts <- newIORef (0 :: Int)
    (ran, bump) <- counter
    let dep = node "flaky" $ \x ->
            x
                { up = do
                    n <- atomicModifyIORef' attempts (\k -> (k + 1, k))
                    unless (n > 0) (ioError (userError "first time always fails"))
                }
        top = nodeOn "onflaky" [dep] $ \x -> x{up = bump}
    supervising (dagOf top) allUp $ \sup trace -> do
        await trace (\rs -> length [() | Acted (UpDown.Failed _ _) <- rs] >= 1)
        assertEqual "the dependant is held off" 0 =<< readIORef ran
        rs <- seen trace
        assertEqual "and is not reported Blocked" [] (blockeds rs)
        -- skip the backoff rather than sleeping through it
        void (Upkeep.instruct sup (refOf "flaky") Recheck)
        await trace (\rs -> "onflaky" `elem` evals rs)
        assertEqual "it proceeds once the dependency recovers" 1 =<< readIORef ran

{- | A node nobody is tending will never move, so waiting for it to move
would be waiting forever. It is reported and stepped over — the same call the
one-shot drivers' gate makes when it answers 'UpDown.Skippable'.
-}
untendedNotWaited :: IO ()
untendedNotWaited = within 10 $ do
    (ran, bump) <- counter
    let dep = node "other" $ \x -> x{up = ioError (userError "must never run")}
        top = nodeOn "mine" [dep] $ \x -> x{up = bump}
    let mine = refOf "mine"
    rs <- supervising (dagOf top) (\rf -> if rf == mine then Just (Tend TurnUp Unsettled) else Nothing) $ \_ trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        seen trace
    assertEqual "the tended node ran" 1 =<< readIORef ran
    assertEqual "the other one is named as untended" ["other"] [act.shorthand | Untended act <- rs]
    assertEqual "and never ran" ["mine"] (evals rs)

cycleIsReported :: IO ()
cycleIsReported = within 10 $ do
    let leaf name = node name id
        magma =
            Map.fromList
                [ (act.extension.ref, act)
                | o <- [leaf "cyc-a", leaf "cyc-b"]
                , Just act <- [opAct o]
                ]
        looped =
            Set.fromList
                [ (refOf "cyc-a", refOf "cyc-b")
                , (refOf "cyc-b", refOf "cyc-a")
                ]
    rs <- supervising (Dag.fromMagma magma looped) allUp $ \sup trace -> do
        await trace (\rs -> not (null [() | Supervising{} <- rs]))
        assertEqual "no machine was started" 0 (Map.size (Upkeep.supervisorTending sup))
        seen trace
    assertEqual "both nodes reported Blocked" 2 (length (blockeds rs))
    assertEqual "and neither evaluated" [] (evals rs)

{- | Teardown is the same wait with the adjacency direction swapped: the
directory goes only after the file in it. 'Down' is terminal, so both
machines exit on their own.
-}
teardownOrdering :: IO ()
teardownOrdering = within 10 $ do
    logRef <- newIORef []
    let rec name = atomicModifyIORef' logRef (\xs -> (name : xs, ()))
        dir = node "dir" $ \x -> x{down = rec ("dir" :: Text)}
        file = nodeOn "file" [dir] $ \x -> x{down = rec "file"}
    rs <- supervising (dagOf file) allDown $ \_ trace -> do
        await trace (\rs -> length (reachedDown Down rs) >= 2)
        seen trace
    order <- reverse <$> readIORef logRef
    assertEqual "the dependant came down first" ["file", "dir"] order
    assertEqual "both machines finished" 2 (length (reachedDown Down rs))

{- | 'Pause' and 'Resume' are the two instructions that mean nothing to a
one-shot driver. Pausing a node still waiting on its dependency proves the
pause is real: the dependency settles while it is paused, and the node stays
put until told to carry on.
-}
pauseAndResume :: IO ()
pauseAndResume = within 10 $ do
    gate <- newEmptyMVar
    (ran, bump) <- counter
    let dep = node "gate" $ \x -> x{up = takeMVar gate}
        top = nodeOn "held" [dep] $ \x -> x{up = bump}
    supervising (dagOf top) allUp $ \sup trace -> do
        await trace (\rs -> "gate" `elem` evals rs)
        void (Upkeep.instruct sup (refOf "held") Pause)
        await trace (\rs -> not (null [() | Paused _ <- rs]))
        -- the dependency now settles; a tended node would proceed here
        putMVar gate ()
        await trace (\rs -> "gate" `elem` [act.shorthand | Acted (UpDown.Done act) <- rs])
        assertEqual "the paused node stayed put" 0 =<< readIORef ran
        void (Upkeep.instruct sup (refOf "held") Resume)
        await trace (\rs -> "held" `elem` evals rs)
        assertEqual "and moved once resumed" 1 =<< readIORef ran

{- | The watchdog is a node author saying what their node's silence would
mean. It only reports — there is nothing here that could safely kill an @up@
halfway through — but reporting is what an operator needs, and it is what
tells a slow node from a stuck one.
-}
watchdogFires :: IO ()
watchdogFires = within 10 $ do
    gate <- newEmptyMVar
    let o =
            node "wedges" $
                \x ->
                    x
                        { up = takeMVar gate
                        , dynamics = [supervised defaultSupervision{supWatchdog = Just (millis 300)}]
                        }
    supervising (dagOf o) allUp $ \_ trace -> do
        await trace (\rs -> not (null [() | Wedged{} <- rs]))
        putMVar gate ()
        await trace (\rs -> not (null [() | Unwedged{} <- rs]))
        rs <- seen trace
        assertEqual "named once while stuck" ["wedges"] [act.shorthand | Wedged act _ <- rs]
        assertEqual "and once when it moved again" ["wedges"] [act.shorthand | Unwedged act <- rs]

{- | @dynamics@ is untyped, so nothing stops an author stating two
contradictory policies. One is taken and the rest are reported, exactly as a
conflicting magma representative is.
-}
policyConflict :: IO ()
policyConflict = within 10 $ do
    let first = defaultSupervision{supRestart = Never}
        second = defaultSupervision{supRestart = Always, supWatchdog = Just (millis 500)}
    let o =
            node "twominds" $
                \x -> x{check = pure Success, dynamics = [toDyn first, toDyn second]}
    rs <- supervising (dagOf o) allUp $ \_ trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        seen trace
    assertEqual
        "the first is in force and the second is named"
        [(first, [second])]
        [(inForce, ignored) | Policy _ inForce ignored <- rs]

{- | Stopping a supervisor stops tending; it does not interrupt work in
flight. Cutting an @up@ halfway is how a half-applied effect happens, so
'stopUpkeep' waits it out.
-}
stopWaitsForUp :: IO ()
stopWaitsForUp = within 10 $ do
    finished <- newIORef False
    let o =
            node "slow" $
                \x -> x{up = threadPause >> writeIORef finished True}
    supervising (dagOf o) allUp $ \_ trace ->
        await trace (\rs -> "slow" `elem` evals rs)
    assertBool "the in-flight up ran to completion" =<< readIORef finished
  where
    -- long enough that a stop which cut the thread would win the race
    threadPause = threadDelay 400000

{- | The one thing 'Standing' exists for. Almost no node in this repository
implements @check@, so almost every node answers 'Unknown' — and a
supervisor started after a convergence pass would run every one of their
@up@s a second time if it took that answer at face value. It is told what
the pass achieved instead.

The node still gets watched: its check is consulted on the ordinary delay,
and 'vanishedComesBack' is the case that shows it acting on the answer.
-}
settledIsNotReUpped :: IO ()
settledIsNotReUpped = within 10 $ do
    (ran, bump) <- counter
    -- no check, so nothing about the node itself can confirm its effect
    let o = node "already" $ \x -> x{up = bump}
    supervising (dagOf o) restingUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "it was not applied" 0 =<< readIORef ran
        rs <- seen trace
        assertEqual "and never evaluated" [] (evals rs)
        -- but it is genuinely being watched
        void (Upkeep.instruct sup (refOf "already") Recheck)
        await trace (\rs -> looks rs >= 2)
        assertEqual "looking still does not re-up a checkless node" 0 =<< readIORef ran

{- | The false positive the "has it said anything at all" clause in
'Salmon.Op.Status.wedged' exists to avoid. Both nodes here declare a short
watchdog and both are 'Transient' for well past it — but only one of them is
doing anything. The other is in 'WaitUp' behind it, and reporting /that/ as
wedged would point at the wrong node.
-}
watchdogSkipsWaiters :: IO ()
watchdogSkipsWaiters = within 10 $ do
    gate <- newEmptyMVar
    let policy = supervised defaultSupervision{supWatchdog = Just (millis 300)}
        dep = node "slowdep" $ \x -> x{up = takeMVar gate, dynamics = [policy]}
        top = nodeOn "waiter" [dep] $ \x -> x{dynamics = [policy]}
    supervising (dagOf top) allUp $ \_ trace -> do
        await trace (\rs -> not (null [() | Wedged{} <- rs]))
        rs <- seen trace
        assertEqual
            "only the node actually doing something is named"
            ["slowdep"]
            [act.shorthand | Wedged act _ <- rs]
        putMVar gate ()
        await trace (\rs -> "waiter" `elem` evals rs)

-------------------------------------------------------------------------------
-- nodes that own their effect

{- | These use a plain 'IO' 'ExitCode' as the "process", which is all the
machine ever sees of one — the state machine's job is the racing, the
policy and the accounting, and none of that is easier to see through a real
subprocess. @Test.DaemonSpec@ covers the part that /is/ about processes:
signals, groups, and pipes.
-}
exits :: Int -> ExitCode
exits 0 = ExitSuccess
exits n = ExitFailure n

{- | A managed node: counts its spawns and hands each one the action to run.

The count is a 'TVar' rather than an 'IORef' so a case can /wait/ for it. The
machine reports @Eval@ before it starts the action, so any assertion made off
the report stream alone races the thread that does the spawning.
-}
holder :: Text -> TVar Int -> IO ExitCode -> (Extension -> Extension) -> Op
holder name = holderOn name []

holderOn :: Text -> [Op] -> TVar Int -> IO ExitCode -> (Extension -> Extension) -> Op
holderOn name preds spawns action f =
    nodeOn name preds $ \x ->
        f
            x
                { managed = Just $ \_out -> do
                    atomically (modifyTVar' spawns (+ 1))
                    action
                }

spawnCounter :: IO (TVar Int)
spawnCounter = newTVarIO 0

-- | Block until the action has been started at least this many times.
awaitSpawns :: TVar Int -> Int -> IO ()
awaitSpawns v n = atomically (readTVar v >>= \k -> unless (k >= n) retry)

spawnsSoFar :: TVar Int -> IO Int
spawnsSoFar = readTVarIO

verdicts :: [Report Extension] -> [CheckResult]
verdicts rs = [v | NextLook _ v _ <- rs]

managedIsUpWhileRunning :: IO ()
managedIsUpWhileRunning = within 10 $ do
    gate <- newEmptyMVar
    spawns <- spawnCounter
    let o = holder "svc" spawns (takeMVar gate >> pure ExitSuccess) id
    supervising (dagOf o) allUp $ \_ trace -> do
        -- Up as soon as the action is running: for a node whose action is
        -- the effect, that is the whole of being up.
        await trace (\rs -> not (null (reached Up rs)))
        awaitSpawns spawns 1
        assertEqual "spawned once" 1 =<< spawnsSoFar spawns
        rs <- seen trace
        assertEqual "and reported Done, which is what lets serve converge it" ["svc"] [act.shorthand | Acted (UpDown.Done act) <- rs]
        putMVar gate ()

managedCleanExitRests :: IO ()
managedCleanExitRests = within 10 $ do
    spawns <- spawnCounter
    let o = holder "job" spawns (pure ExitSuccess) id
    supervising (dagOf o) allUp $ \_ trace -> do
        await trace (\rs -> Completed `elem` verdicts rs)
        assertEqual "ran once and was left alone" 1 =<< spawnsSoFar spawns

managedFailureRestarts :: IO ()
managedFailureRestarts = within 20 $ do
    spawns <- spawnCounter
    let o = holder "flapper" spawns (pure (exits 3)) id
    supervising (dagOf o) allUp $ \_ _ -> do
        awaitSpawns spawns 2
        n <- spawnsSoFar spawns
        assertBool "put back after a non-zero exit" (n >= 2)

managedNeverStaysDown :: IO ()
managedNeverStaysDown = within 10 $ do
    spawns <- spawnCounter
    let o = holder "once" spawns (pure (exits 1)) $ \x ->
            x{dynamics = [supervised defaultSupervision{supRestart = Never}]}
    supervising (dagOf o) allUp $ \sup trace -> do
        -- it has stopped and settled; nothing put it back
        await trace (\rs -> any isFailure (verdicts rs))
        assertEqual "ran once" 1 =<< spawnsSoFar spawns
        -- and a look does not change its mind either
        void (Upkeep.instruct sup (refOf "once") Recheck)
        await trace (\rs -> length (verdicts rs) >= 2)
        assertEqual "still once" 1 =<< spawnsSoFar spawns
  where
    isFailure (Failure _) = True
    isFailure _ = False

managedAlwaysRestarts :: IO ()
managedAlwaysRestarts = within 20 $ do
    spawns <- spawnCounter
    let o = holder "reloader" spawns (pure ExitSuccess) $ \x ->
            x{dynamics = [supervised defaultSupervision{supRestart = Always}]}
    supervising (dagOf o) allUp $ \_ _ -> do
        awaitSpawns spawns 2
        n <- spawnsSoFar spawns
        assertBool "a clean exit is not the end of it under Always" (n >= 2)

{- | The one shape a process handle cannot speak to: a daemon that exits 0
having forked. Consulting the check before the policy handles it for free,
and this is what pins that ordering.
-}
managedDoubleFork :: IO ()
managedDoubleFork = within 10 $ do
    forked <- newIORef False
    spawns <- spawnCounter
    let o = holder "forker" spawns (writeIORef forked True >> pure ExitSuccess) $ \x ->
            x
                { -- as a real double-forking daemon looks: nothing there
                  -- until it has run, and there afterwards even though the
                  -- process salmon spawned has exited.
                  check = do
                    up' <- readIORef forked
                    pure (if up' then Success else Failure "not yet")
                , dynamics = [supervised defaultSupervision{supRestart = Always}]
                }
    supervising (dagOf o) allUp $ \sup trace -> do
        awaitSpawns spawns 1
        await trace (\rs -> Success `elem` verdicts rs)
        -- `Always` would otherwise restart even a clean exit. The check
        -- saying the effect is there anyway is what stops it, and is the
        -- only thing that could.
        void (Upkeep.instruct sup (refOf "forker") Recheck)
        await trace (\rs -> length (verdicts rs) >= 2)
        assertEqual "the check outranks the policy" 1 =<< spawnsSoFar spawns

managedGivesUp :: IO ()
managedGivesUp = within 20 $ do
    spawns <- spawnCounter
    let o = holder "hopeless" spawns (pure (exits 1)) $ \x ->
            x{dynamics = [supervised defaultSupervision{supGiveUpAfter = Just 2}]}
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null [n | GaveUp _ n <- rs]))
        assertEqual "tried exactly as often as it was told to" 2 =<< spawnsSoFar spawns
        rs <- seen trace
        assertEqual "and says how many times" [2] [n | GaveUp _ n <- rs]
        -- parked, not gone: an operator can change their mind
        void (Upkeep.instruct sup (refOf "hopeless") Force)
        awaitSpawns spawns 3
        n <- spawnsSoFar spawns
        assertBool "forcing starts it over" (n >= 3)

{- | Without 'supStableAfter' a give-up limit latches off any long-lived node
eventually: a service that falls over once a day reaches any finite count in
that many days, having never been in a crash loop. So only /consecutive
quick/ failures count.
-}
managedStableResets :: IO ()
managedStableResets = within 30 $ do
    spawns <- spawnCounter
    let o = holder "daily" spawns (threadDelay 200000 >> pure (exits 1)) $ \x ->
            x
                { dynamics =
                    [ supervised
                        defaultSupervision
                            { supGiveUpAfter = Just 2
                            , supStableAfter = millis 100
                            }
                    ]
                }
    supervising (dagOf o) allUp $ \_ trace -> do
        awaitSpawns spawns 3
        rs <- seen trace
        assertEqual "having run past supStableAfter, it never accumulates" [] [n | GaveUp _ n <- rs]

{- | Teardown is cancelling the machine, and whatever bracket the action is
built from is what does the killing. This is the property
"Salmon.Builtin.Nodes.Daemon" relies on entirely.
-}
managedCancelTearsDown :: IO ()
managedCancelTearsDown = within 10 $ do
    torn <- newIORef False
    blocker <- newEmptyMVar
    spawns <- spawnCounter
    let o =
            holder "held" spawns
                ( bracket
                    (pure ())
                    (\() -> writeIORef torn True)
                    (\() -> takeMVar blocker >> pure ExitSuccess)
                )
                id
    -- `supervising` stops the supervisor on the way out, and a machine
    -- holding an effect is only ever taken by a cancel.
    supervising (dagOf o) allUp $ \_ _ ->
        awaitSpawns spawns 1
    assertBool "the action's own bracket ran" =<< readIORef torn

{- | A 'Settled' claim is about an effect that persists on its own. A managed
effect does not persist without the machine holding it, so a caller believing
otherwise (@serve@ does, for any node a pass marked converged) must not stop
it being spawned.
-}
managedIgnoresSettled :: IO ()
managedIgnoresSettled = within 10 $ do
    gate <- newEmptyMVar
    spawns <- spawnCounter
    let o = holder "wrongly-settled" spawns (takeMVar gate >> pure ExitSuccess) id
    supervising (dagOf o) restingUp $ \_ _ -> do
        awaitSpawns spawns 1
        assertEqual "spawned anyway" 1 =<< spawnsSoFar spawns
        putMVar gate ()

managedForceRestarts :: IO ()
managedForceRestarts = within 10 $ do
    torn <- newIORef (0 :: Int)
    spawns <- spawnCounter
    blocker <- newEmptyMVar
    let o =
            holder "restartable" spawns
                ( bracket
                    (pure ())
                    (\() -> atomicModifyIORef' torn (\n -> (n + 1, ())))
                    (\() -> takeMVar blocker >> pure ExitSuccess)
                )
                id
    supervising (dagOf o) allUp $ \sup _ -> do
        awaitSpawns spawns 1
        void (Upkeep.instruct sup (refOf "restartable") Force)
        awaitSpawns spawns 2
        assertEqual "the old one was torn down before the new one spawned" 1 =<< readIORef torn
        assertEqual "and there is a new one" 2 =<< spawnsSoFar spawns

-------------------------------------------------------------------------------
-- a node that takes its dependants with it

{- | Milestone 9's cases. All eight share one shape: a dependency is made to
leave 'Up' at a moment the case controls (its effect is removed behind
salmon's back, then it is 'Recheck'ed), and what the /dependant/ does about
it is the assertion.

Nothing here sleeps to decide anything. The two negative cases are the
exception and say so: proving something does not happen needs a bounded wait
for it, and a 'timeout' returning 'Nothing' is that wait made explicit.
-}
restForOne :: Extension -> Extension
restForOne x = x{dynamics = [supervised defaultSupervision{supStrategy = RestForOne}]}

-- | Opts a node into 'Salmon.Op.Supervision.supReapply': re-run @up@ on the
-- loop instead of parking. See the "supReapply" test group.
reapplying :: Extension -> Extension
reapplying x = x{dynamics = [supervised defaultSupervision{supReapply = True}]}

-- | Both at once: the shape a config-file-that-happens-to-be-cheap would
-- declare, and the case that pins 'supReapply' must not fire 'RestForOne'
-- on a success — only a genuine departure may.
reapplyingRestForOne :: Extension -> Extension
reapplyingRestForOne x = x{dynamics = [supervised defaultSupervision{supReapply = True, supStrategy = RestForOne}]}

-- | Like 'reapplying', but gives up after exactly one failure — deterministic
-- without needing to wait out a real backoff or a real 'supStableAfter'.
reapplyingGivesUpFast :: Extension -> Extension
reapplyingGivesUpFast x = x{dynamics = [supervised defaultSupervision{supReapply = True, supGiveUpAfter = Just 1}]}

{- | A node whose effect can be taken away behind salmon's back. Hands back
the count of its @up@s and the flag that says whether its effect is there.
-}
breakable :: Text -> [Op] -> (Extension -> Extension) -> IO (Op, IORef Int, IORef Bool)
breakable name preds f = do
    there <- newIORef False
    (ran, bump) <- counter
    let o =
            nodeOn name preds $ \x ->
                f
                    x
                        { check = do
                            ok <- readIORef there
                            pure (if ok then Success else Failure "gone")
                        , up = bump >> writeIORef there True
                        }
    pure (o, ran, there)

{- | A node that only counts. Deliberately without a @check@, so that being
sent back to 'WaitUp' really does re-run its @up@ — which is what makes a
demotion visible at all, and is the case a repository whose nodes mostly have
no check actually has.
-}
counted :: Text -> [Op] -> (Extension -> Extension) -> IO (Op, IORef Int)
counted name preds f = do
    (ran, bump) <- counter
    pure (nodeOn name preds (\x -> f x{up = bump}), ran)

-- | Take the effect away and tell the node to look now.
breakIt :: Supervisor Extension -> IORef Bool -> Text -> IO ()
breakIt sup there name = do
    writeIORef there False
    void (Upkeep.instruct sup (refOf name) Recheck)

{- | The payoff of the whole milestone: a service standing on a configuration
file that has just been rewritten is brought up again on the new one, rather
than left running against content it has never seen.
-}
restForOneDemotes :: IO ()
restForOneDemotes = within 20 $ do
    (cfg, cfgRan, there) <- breakable "cfg" [] restForOne
    (svc, svcRan) <- counted "svc" [cfg] id
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        assertEqual "the service came up on the original config" 1 =<< readIORef svcRan
        breakIt sup there "cfg"
        await trace (\rs -> reachedBy "svc" Up rs >= 2)
        rs <- seen trace
        assertEqual "it was sent back by its config" [("svc", refOf "cfg")] (demotions rs)
        assertEqual "and brought up again on the new one" 2 =<< readIORef svcRan
        assertEqual "which had itself been rewritten" 2 =<< readIORef cfgRan

{- | ...and the property that makes the feature safe to have landed at all:
until a node says otherwise, its dependants are not anybody's business. This
is today's behaviour, asserted so that it stays that way.
-}
oneForOneLeavesItAlone :: IO ()
oneForOneLeavesItAlone = within 20 $ do
    -- no strategy declared, so 'OneForOne'
    (cfg, _, there) <- breakable "cfg" [] id
    (svc, svcRan) <- counted "svc" [cfg] id
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        breakIt sup there "cfg"
        await trace (\rs -> reachedBy "cfg" Up rs >= 2)
        -- one full turn of svc's own loop after the config came back, so
        -- that "it did not react" is a statement about a machine that has
        -- since run rather than one that has not got there yet.
        void (Upkeep.instruct sup (refOf "svc") Recheck)
        await trace (\rs -> looksAt "svc" rs >= 2)
        rs <- seen trace
        assertEqual "nobody was sent back" [] (demotions rs)
        assertEqual "and the service was never touched" 1 =<< readIORef svcRan

{- | Being demoted is going back to 'WaitUp', not going back to 'Upping'.
The distinction is the whole point: a node brought up again immediately would
be brought up against the very dependency that is currently missing.
-}
demotedWaitsForItsDependency :: IO ()
demotedWaitsForItsDependency = within 20 $ do
    gate <- newEmptyMVar
    there <- newIORef False
    (cfgRan, cfgBump) <- counter
    let cfg =
            node "cfg" $ \x ->
                restForOne
                    x
                        { check = do
                            ok <- readIORef there
                            pure (if ok then Success else Failure "gone")
                        , up = do
                            n <- readIORef cfgRan
                            cfgBump
                            -- the repair is held open, so the dependency
                            -- stays visibly in flight
                            unless (n == 0) (takeMVar gate)
                            writeIORef there True
                        }
    (svc, svcRan) <- counted "svc" [cfg] id
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        breakIt sup there "cfg"
        await trace (\rs -> reachedBy "svc" WaitUp rs >= 2)
        assertEqual "waiting, not acting" 1 =<< readIORef svcRan
        putMVar gate ()
        await trace (\rs -> evalsOf "svc" rs >= 2)
        assertEqual "and only once the dependency was back" 2 =<< readIORef svcRan

{- | A demoted node is itself no longer up, which is all a dependant of /it/
that opted in needs to see. Nothing propagates the cascade; it falls out.
-}
demotionCascades :: IO ()
demotionCascades = within 20 $ do
    (cfg, _, there) <- breakable "cfg" [] restForOne
    (mid, midRan) <- counted "mid" [cfg] restForOne
    (leaf, leafRan) <- counted "leaf" [mid] id
    supervising (dagOf leaf) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "leaf" Up rs >= 1)
        breakIt sup there "cfg"
        await trace (\rs -> reachedBy "leaf" Up rs >= 2)
        rs <- seen trace
        assertEqual
            "each was sent back by the one in front of it, in that order"
            [("mid", refOf "cfg"), ("leaf", refOf "mid")]
            (demotions rs)
        assertEqual "mid came up again" 2 =<< readIORef midRan
        assertEqual "and so did leaf" 2 =<< readIORef leafRan

-- | The overwhelmingly common shape, and it must cost nothing.
noDependantsCostsNothing :: IO ()
noDependantsCostsNothing = within 20 $ do
    (solo, ran, there) <- breakable "solo" [] restForOne
    supervising (dagOf solo) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "solo" Up rs >= 1)
        breakIt sup there "solo"
        await trace (\rs -> reachedBy "solo" Up rs >= 2)
        rs <- seen trace
        assertEqual "it demoted nobody, itself included" [] (demotions rs)
        assertEqual "it just put its own effect back" 2 =<< readIORef ran

{- | The hazard this milestone had to be designed against: a dependency that
flaps would otherwise rebuild the whole cone behind it on every flap.

'Salmon.Op.Supervision.supStableAfter' bounds it to once per interval, and
the default of ten seconds is well beyond what this case takes — so the
second departure is dropped rather than delayed.
-}
flapIsRateLimited :: IO ()
flapIsRateLimited = within 30 $ do
    (cfg, _, there) <- breakable "cfg" [] restForOne
    (svc, svcRan) <- counted "svc" [cfg] id
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        breakIt sup there "cfg"
        await trace (\rs -> reachedBy "svc" Up rs >= 2)
        assertEqual "the first departure was acted on" 2 =<< readIORef svcRan
        breakIt sup there "cfg"
        await trace (\rs -> reachedBy "cfg" Up rs >= 3)
        -- proving a thing does not happen: wait for it, and expect not to
        -- get it. Half a second is many turns of both machines.
        again <- timeout 500000 (await trace (\rs -> evalsOf "svc" rs >= 3))
        assertEqual "the second was dropped rather than acted on" Nothing again
        rs <- seen trace
        assertEqual "one demotion, not two" 1 (length (demotions rs))
        assertEqual "and one extra bring-up, not two" 2 =<< readIORef svcRan

{- | The rule that keeps this from undoing 'Standing': a dependency that has
not been seen up yet cannot send anybody back.

Without it, @serve@ — which stands its machines up again after every command
it is handed — would re-run every @up@ in an opted-in cone each time an
operator typed anything, which is exactly the regression 'Standing' exists to
prevent.
-}
settledStartIsNotDemoted :: IO ()
settledStartIsNotDemoted = within 20 $ do
    (cfg, cfgRan, _) <- breakable "cfg" [] restForOne
    (svc, svcRan) <- counted "svc" [cfg] id
    -- the shape a supervisor starts in over a graph a pass has half done:
    -- the dependant is known to be up, the dependency is not.
    let tend aref
            | aref == refOf "svc" = Just (Tend TurnUp Settled)
            | otherwise = Just (Tend TurnUp Unsettled)
    supervising (dagOf svc) tend $ \sup trace -> do
        await trace (\rs -> reachedBy "cfg" Up rs >= 1)
        void (Upkeep.instruct sup (refOf "svc") Recheck)
        await trace (\rs -> looksAt "svc" rs >= 2)
        rs <- seen trace
        assertEqual "coming up for the first time demoted nobody" [] (demotions rs)
        assertEqual "so the standing claim held" 0 =<< readIORef svcRan
        assertEqual "while the dependency did its own work" 1 =<< readIORef cfgRan

{- | The case the feature is really for: the thing standing on the config is
a process salmon owns. Being sent back has to tear it down — through the
action's own bracket, outside the 'Control.Concurrent.Async.withAsync' —
before anything spawns again.
-}
managedDependantIsRestarted :: IO ()
managedDependantIsRestarted = within 20 $ do
    (cfg, _, there) <- breakable "cfg" [] restForOne
    spawns <- spawnCounter
    -- held by this thread, so blocking on it is a wait rather than a
    -- deadlock the runtime is entitled to notice
    gate <- newEmptyMVar
    let svc = holderOn "svc" [cfg] spawns (takeMVar gate) id
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        awaitSpawns spawns 1
        breakIt sup there "cfg"
        awaitSpawns spawns 2
        rs <- seen trace
        assertEqual "the process was sent back by its config" [("svc", refOf "cfg")] (demotions rs)

{- | (I1). A node that owns its process is torn down on the way out of
'Salmon.Actions.Upkeep.watch', so by the time it comes back round its effect
is certainly gone — whatever its own @check@ says.

The check here is one somebody would plausibly write and which is wrong in
the way health checks are wrong: it answers "this ran at some point", not
"it is running now". Before the fix that answer was believed, the node
reported @Skip@ and settled into 'Up' holding nothing, and the process was
gone for good.
-}
managedDemotionOutranksItsOwnCheck :: IO ()
managedDemotionOutranksItsOwnCheck = within 20 $ do
    (cfg, _, there) <- breakable "cfg" [] restForOne
    spawns <- spawnCounter
    gate <- newEmptyMVar
    ranOnce <- newIORef False
    let svc = holderOn "svc" [cfg] spawns (takeMVar gate) $ \x ->
            x
                { check = do
                    stale <- readIORef ranOnce
                    pure (if stale then Success else Failure "not started yet")
                }
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        awaitSpawns spawns 1
        -- from here its check lies: it says the effect is in place, and only
        -- this machine knows it has just cancelled the thing providing it.
        writeIORef ranOnce True
        breakIt sup there "cfg"
        awaitSpawns spawns 2
        rs <- seen trace
        assertEqual "it was sent back" [("svc", refOf "cfg")] (demotions rs)
        assertBool "and put back rather than talked out of it" (null (skips rs))

{- | ...and the other half of the same decision, which is what keeps it a
narrow fix rather than "a demotion always re-applies".

A node whose effect persists on its own has not lost anything by being
demoted — nothing was torn down — so its check is still the authority on
whether the demotion means any work at all. This one says yes, it is fine,
and is believed.
-}
oneShotDemotionAsksItsCheck :: IO ()
oneShotDemotionAsksItsCheck = within 20 $ do
    (cfg, _, there) <- breakable "cfg" [] restForOne
    (ran, bump) <- counter
    let svc = nodeOn "svc" [cfg] $ \x -> x{check = pure Success, up = bump}
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        breakIt sup there "cfg"
        await trace (\rs -> not (null (demotions rs)))
        await trace (\rs -> reachedBy "svc" Up rs >= 2)
        assertEqual "its check said there was nothing to do, and was right" 0 =<< readIORef ran
        rs <- seen trace
        assertBool "so it reported a skip rather than acting" (not (null (skips rs)))

--------------------------------------------------------------------------------
-- a node that reapplies instead of asking (supReapply)

{- | The whole point: a node with 'reapplying' and no @check@ re-runs @up@
on the adaptive delay rather than parking on its mailbox forever. 'Recheck'
collapses the delay exactly as it does for a checked node, which for this
node means "reapply now" rather than "look now".
-}
reapplyRunsAgain :: IO ()
reapplyRunsAgain = within 10 $ do
    (ran, bump) <- counter
    let o = node "cheap-dir" $ \x -> reapplying x{up = bump}
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "upped once on the way in" 1 =<< readIORef ran
        await trace (\rs -> not (null [() | Reapplying{} <- rs]))
        rs0 <- seen trace
        assertEqual "never parked, since it opted out of that" 0 (length [() | Parked{} <- rs0])
        void (Upkeep.instruct sup (refOf "cheap-dir") Recheck)
        await trace (\rs -> evalsOf "cheap-dir" rs >= 2)
        assertEqual "reapplied rather than merely looked at" 2 =<< readIORef ran

{- | A successful reapply is not a restart: it must not go back through
'Salmon.Actions.Upkeep.WaitUp' \/ 'Upping', or a node reapplying once a
minute would announce itself exactly like one flapping. 'Upping' is reported
only for this machine's original arrival at 'Up'.
-}
reapplyStaysInUp :: IO ()
reapplyStaysInUp = within 10 $ do
    (ran, bump) <- counter
    let o = node "steady" $ \x -> reapplying x{up = bump}
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        void (Upkeep.instruct sup (refOf "steady") Recheck)
        void (Upkeep.instruct sup (refOf "steady") Recheck)
        await trace (\rs -> evalsOf "steady" rs >= 3)
        rs <- seen trace
        assertEqual "Upping was only ever the original arrival" 1 (reachedBy "steady" Upping rs)
        assertEqual "and Up was only ever entered once" 1 (reachedBy "steady" Up rs)

{- | The interaction 'Salmon.Op.Supervision.supReapply' has to get right
with 'RestForOne': re-applying is not the dependency "going away and coming
back" from a dependant's point of view, so a dependant that opted in must
not be sent back merely because the dependency reapplied successfully —
only a genuine departure (a real 'Salmon.Actions.UpDown.Failure', or an
operator's 'Force') may do that.
-}
reapplyDoesNotDemoteDependants :: IO ()
reapplyDoesNotDemoteDependants = within 10 $ do
    (depRan, depBump) <- counter
    let dep = node "cheap-dir" $ \x -> reapplyingRestForOne x{up = depBump}
    (svc, svcRan) <- counted "svc" [dep] id
    supervising (dagOf svc) allUp $ \sup trace -> do
        await trace (\rs -> reachedBy "svc" Up rs >= 1)
        assertEqual "svc came up once" 1 =<< readIORef svcRan
        -- several successful reapplies, forced rather than waited for
        mapM_
            (const (void (Upkeep.instruct sup (refOf "cheap-dir") Recheck)))
            [1 :: Int .. 3]
        await trace (\rs -> evalsOf "cheap-dir" rs >= 4)
        rs <- seen trace
        assertEqual "reapplied several times" [] (demotions rs)
        assertEqual "svc was never sent back" 1 =<< readIORef svcRan
        assertBool "reapplying happened at all" (depRanAtLeast rs)
  where
    depRanAtLeast rs = evalsOf "cheap-dir" rs >= 4

{- | A reapply that throws is a genuine failure, not a shrug: it goes
through the same 'Salmon.Actions.Upkeep.failed' machinery a one-shot @up@
failure does, with the same backoff and the same
'Salmon.Op.Supervision.supGiveUpAfter'. A tight give-up limit makes this
deterministic — one throwing reapply is enough to exhaust it.
-}
failingReapplyGivesUp :: IO ()
failingReapplyGivesUp = within 10 $ do
    calls <- newIORef (0 :: Int)
    let o =
            node "flaky-dir" $ \x ->
                reapplyingGivesUpFast
                    x
                        { up = do
                            n <- atomicModifyIORef' calls (\k -> (k + 1, k))
                            unless (n == 0) (ioError (userError "boom"))
                        }
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "the first up succeeded" 1 =<< readIORef calls
        void (Upkeep.instruct sup (refOf "flaky-dir") Recheck)
        await trace (\rs -> not (null [() | GaveUp{} <- rs]))
        rs <- seen trace
        assertBool "the failing reapply was reported as a failure" (not (null [() | Acted (UpDown.Failed{}) <- rs]))

{- | 'Salmon.Op.Supervision.supReapply' is read only by a node with no
action to hold: a managed node's @up@ throws by convention
("Salmon.Builtin.Nodes.Daemon"), so re-running it on a schedule would
crash-loop a service that is otherwise fine. Declaring both must therefore
still park — never announce 'Reapplying', never spawn a second time.
-}
managedIgnoresSupReapply :: IO ()
managedIgnoresSupReapply = within 10 $ do
    gate <- newEmptyMVar
    spawns <- spawnCounter
    let o = holder "svc" spawns (takeMVar gate >> pure ExitSuccess) reapplying
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        awaitSpawns spawns 1
        void (Upkeep.instruct sup (refOf "svc") Recheck)
        await trace (\rs -> not (null [() | Parked{} <- rs]))
        rs <- seen trace
        assertEqual "never announced as reapplying" 0 (length [() | Reapplying{} <- rs])
        assertEqual "and never spawned a second time" 1 =<< spawnsSoFar spawns

{- | (R9), end to end: the real 'Salmon.Builtin.Nodes.Filesystem.dir'
builtin, not a stand-in, under a real supervisor and a real filesystem. It
declares 'Salmon.Op.Supervision.supReapply' itself (see its haddock), so
this is the payoff the whole field exists for — a directory removed behind
salmon's back comes back with nobody re-declaring anything, the same
guarantee 'vanishedComesBack' pins for a checked node.
-}
dirSelfHeals :: IO ()
dirSelfHeals = within 10 $ withTempDir $ \tmp -> do
    let path = tmp </> "managed"
        theRef = mkRef "directory" path
        o = FS.dir (FS.Directory path)
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertBool "created on the way in" =<< doesDirectoryExist path
        removeDirectory path
        assertBool "really gone" . not =<< doesDirectoryExist path
        void (Upkeep.instruct sup theRef Recheck)
        await trace (\rs -> evalsOf "directory" rs >= 2)
        assertBool "put back without anybody re-declaring it" =<< doesDirectoryExist path
        rs <- seen trace
        assertEqual "nothing ever demoted, since this node has no dependants" [] (demotions rs)
