{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0/1 coverage for "Salmon.Actions.Upkeep": the continuous driver.

The one-shot drivers can be asserted on by running them to completion and
reading the report list. Nothing here completes, so every case instead
'startUpkeep's, waits on the report stream for the state it is looking for,
pokes the world, waits again, and stops. The waiting is STM on a 'TVar' of
reports rather than @threadDelay@, so a case that passes does so as fast as
the machines run and a case that fails fails by timing out rather than by
flaking.

Three groups. First, that a node is tended at all: satisfied nodes are left
alone, unsatisfied ones are brought up, and the ordering guarantees the
one-shot drivers have still hold. Second, the part that only exists here —
the effect going away brings the node back, the restart policy decides
whether it does, and a check that cannot tell decides nothing. Third, the
control surface: instructions that only mean something to a continuous
driver, and the watchdog.
-}
module Test.UpkeepSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry)
import Control.Monad (unless, void)
import Data.Dynamic (toDyn)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.Upkeep (DownkeepState (..), Report (..), Standing (..), Supervisor, Tend (..), UpkeepState (..))
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Extension, Op, check, deps, down, dynamics, evalDeps, help, nodeps, notes, op, opAct, ref, up)
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Status (Direction (..))
import Salmon.Op.Supervision (Restart (..), Supervision (..), millis, supervised)
import Salmon.Reporter (ReporterM (..))

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Upkeep"
        [ testCase "the adaptive delay clamps at both ends" delayClamps
        , testCase "a satisfied node is not upped, and rests" satisfiedRests
        , testCase "an unsatisfied node is upped, then rests" unsatisfiedIsUpped
        , testCase "a node with no check is upped once and never again" unknownDoesNotSpin
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
        , testCase "a node waiting on a slow dependency is not itself wedged" watchdogSkipsWaiters
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

looks :: [Report Extension] -> Int
looks rs = length [() | NextLook{} <- rs]

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

{- | The refinement this module makes to the spec's rule, and the one that
matters most in a repository where almost no node has a check: 'Unknown' is
not evidence the effect went away, so it must not restart anything. Were it
treated the way the one-shot drivers treat it — as
'Salmon.Actions.UpDown.Required' — this node would re-run @up@ at the delay
floor for as long as the process lived.
-}
unknownDoesNotSpin :: IO ()
unknownDoesNotSpin = within 10 $ do
    (ran, bump) <- counter
    -- no `check` at all, so `runCheck` answers Unknown.
    let o = node "quiet" $ \x -> x{up = bump}
    supervising (dagOf o) allUp $ \sup trace -> do
        await trace (\rs -> not (null (reached Up rs)))
        assertEqual "upped once on the way in" 1 =<< readIORef ran
        -- Recheck collapses the delay and looks now, so this does not wait
        -- out a real nap to prove the second look happened.
        void (Upkeep.instruct sup (refOf "quiet") Recheck)
        await trace (\rs -> looks rs >= 2)
        assertEqual "and looking again did not re-up it" 1 =<< readIORef ran

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
                        , dynamics = [supervised (Supervision Never Nothing)]
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
                        , dynamics = [supervised (Supervision Always Nothing)]
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
                        , dynamics = [supervised (Supervision OnFailure (Just (millis 300)))]
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
    let first = Supervision Never Nothing
        second = Supervision Always (Just (millis 500))
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
        assertEqual "looking still does not re-up an Unknown node" 0 =<< readIORef ran

{- | The false positive the "has it said anything at all" clause in
'Salmon.Op.Status.wedged' exists to avoid. Both nodes here declare a short
watchdog and both are 'Transient' for well past it — but only one of them is
doing anything. The other is in 'WaitUp' behind it, and reporting /that/ as
wedged would point at the wrong node.
-}
watchdogSkipsWaiters :: IO ()
watchdogSkipsWaiters = within 10 $ do
    gate <- newEmptyMVar
    let policy = supervised (Supervision OnFailure (Just (millis 300)))
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
