{-# LANGUAGE OverloadedStrings #-}

{- | What a tended node costs to keep, and that keeping it cheaply changed
nothing about how it is tended.

Finding 6 of @resources\/graph-profiles.md@: a supervisor over 100 000 nodes
did not fit in a 4 GB heap. A heap profile by closure type put about 95% of
what it retained in @STACK@ objects, 32 KiB a machine: each machine's thread
overflowed its first stack chunk (1 KiB) while registering a nap timeout
(the insertion into the runtime's timer queue runs on the calling thread and
deepens with the number of timeouts outstanding), was moved onto a chunk of
@+RTS -kc@ size, and stayed there. 'Salmon.Actions.Upkeep' now registers on
a short-lived thread of its own.

The retention cases measure that directly: they clone each machine's stack
once it has settled and compare its size with a thread that has done nothing
at all, so they do not depend on what else this process is doing (the suite
runs its groups in parallel, which rules out reading the heap's residency).
The rest checks what the change must not alter: every fixture shape is
started, settled and stopped as before, a nap still ends, and, as a
property, a random graph under a random script of instructions converges
with its dependency order intact.
-}
module Test.UpkeepRetentionSpec (tests) where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newEmptyMVar, takeMVar, yield)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Control.Monad (forM, forM_, unless, void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Conc (ThreadStatus (..), threadStatus)
import GHC.Exts.Heap.Closures (GenStackFrame (..), GenStgStackClosure (..), StgStackClosure)
import GHC.Exts.Stack.Decode (decodeStack)
import GHC.Stack.CloneStack (cloneThreadStack)
import System.Timeout (timeout)
import Test.QuickCheck (Gen, Property, choose, counterexample, elements, forAll, ioProperty, listOf, (.&&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.Upkeep (Report (..), Standing (..), Supervisor, Tend (..), UpkeepState (..))
import qualified Salmon.Actions.Upkeep as Upkeep
-- the field selectors 'Upkeep' asks for through HasField must be in scope.
import Salmon.Builtin.Extension (Extension (..))
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Mailbox (Instruction (..))
import Salmon.Op.Ref (Ref)
import Salmon.Op.Status (Direction (..), Stability (..), Status (..))
import Salmon.Reporter (ReporterM (..))

import Test.GraphFixture

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Upkeep: what a tended node retains"
        [ testCase "a settled machine keeps the stack it started with" (stacksStaySmall Settled)
        , testCase "a machine that brought its node up keeps the stack it started with" (stacksStaySmall Unsettled)
        , testCase "every shape starts, settles and stops" everyShapeStartsSettlesStops
        , testCase "a nap still ends on its own" napsEnd
        , testProperty "a random graph under a random script converges in dependency order" prop_scriptedConvergence
        ]

-------------------------------------------------------------------------------
-- driving a supervisor

type Trace = TVar [Report Extension]

within :: Int -> IO a -> IO a
within secs act = do
    result <- timeout (secs * 1000000) act
    maybe (fail ("timed out after " <> show secs <> "s")) pure result

-- | Start a supervisor over this graph with a trace of its reports (newest
-- first), run the body, and stop it whatever the body did.
supervising ::
    Dag.Dag Extension ->
    (Ref -> Maybe Tend) ->
    (Supervisor Extension -> Trace -> IO a) ->
    IO a
supervising dag tend body = do
    trace <- newTVarIO []
    let r = ReporterM $ \rep -> atomically (modifyTVar' trace (rep :))
    Upkeep.withUpkeep r tend dag (\sup -> body sup trace)

-- | Block until the reports so far (oldest first) satisfy the predicate.
await :: Trace -> ([Report Extension] -> Bool) -> IO ()
await trace p =
    atomically $ do
        rs <- readTVar trace
        unless (p (reverse rs)) retry

seen :: Trace -> IO [Report Extension]
seen trace = reverse <$> readTVarIO trace

-- | Every machine settled, the way a dependant would read it.
awaitStable :: Supervisor Extension -> IO ()
awaitStable sup =
    forM_ (Map.elems (Upkeep.supervisorStatuses sup)) $ \var ->
        atomically $ do
            st <- readTVar var
            unless (st.statusStability == Stable && st.statusDirection == TurnUp) retry

-- | Which nodes have said they are waiting for their next look (or parked).
resting :: [Report Extension] -> Set.Set Ref
resting rs = Set.fromList [act.extension.ref | r <- rs, act <- restingAct r]
  where
    restingAct (NextLook a _ _) = [a]
    restingAct (Parked a) = [a]
    restingAct _ = []

upAt :: [Report Extension] -> [Ref]
upAt rs = [act.extension.ref | Upkeep act Up <- rs]

evaluated :: [Report Extension] -> [Ref]
evaluated rs = [act.extension.ref | Acted (UpDown.Eval act) <- rs]

retired :: [Report Extension] -> [Int]
retired rs = [n | Retired n <- rs]

-- | A fixture graph, folded by the fixture's own per-node build.
fixture :: CheckResult -> Shape -> Int -> Seed -> Dag.Dag Extension
fixture verdict shape n s = dagOf (generateWith defaultOptions{optCheck = verdict} shape n s)

-------------------------------------------------------------------------------
-- stacks

-- | Words in every chunk of a thread's stack.
stackWords :: ThreadId -> IO Int
stackWords tid = chunkWords <$> (decodeStack =<< cloneThreadStack tid)
  where
    chunkWords :: StgStackClosure -> Int
    chunkWords st =
        fromIntegral (ssc_stack_size st)
            + sum [chunkWords c | UnderflowFrame{nextChunk = c} <- ssc_stack st]

-- | What a thread that has done nothing but block holds: the runtime's first
-- chunk, whatever @+RTS -ki@ made it.
freshStackWords :: IO Int
freshStackWords = do
    never <- newEmptyMVar :: IO (MVar ())
    tid <- forkIO (takeMVar never)
    awaitBlocked [tid]
    w <- stackWords tid
    killThread tid
    pure w

-- | Until every one of these threads is blocked (or gone).
awaitBlocked :: [ThreadId] -> IO ()
awaitBlocked tids = do
    statuses <- mapM threadStatus tids
    unless (all parked statuses) (yield >> awaitBlocked tids)
  where
    parked (ThreadBlocked _) = True
    parked ThreadFinished = True
    parked ThreadDied = True
    parked ThreadRunning = False

{- | Three thousand machines of a binary tree, every one of them napping on a
timer (their check says 'Success', so they poll rather than park). Before
the change every one of them held a 32 KiB chunk from about a thousand nodes
up. One machine in a hundred is allowed a bigger stack than a fresh thread:
the first to force something every machine shares pays for evaluating it.
-}
stacksStaySmall :: Standing -> IO ()
stacksStaySmall standing = within 60 $ do
    let n = 3000
        dag = fixture Success (Tree 2) n 1
    baseline <- freshStackWords
    supervising dag (const (Just (Tend TurnUp standing))) $ \sup trace -> do
        await trace (\rs -> Set.size (resting rs) == n)
        let threads = Map.elems (Upkeep.supervisorThreads sup)
        assertEqual "one machine a node" n (length threads)
        awaitBlocked threads
        sizes <- forM threads stackWords
        let grown = length (filter (> baseline) sizes)
        assertBool
            ( "machines holding more stack than a fresh thread ("
                <> show baseline
                <> " words): "
                <> show grown
                <> " of "
                <> show n
                <> ", largest "
                <> show (maximum sizes)
                <> " words"
            )
            (grown <= n `div` 100)

-------------------------------------------------------------------------------
-- behaviour that must not have moved

shapes :: [Shape]
shapes = [Chain, Fan, Tree 2, Diamonds 2, Layered 8 2, RandomLayered 8 3, Layered 4 4]

{- | Start, settle, stop, on every shape, both standings, both kinds of
check: every node comes up, every machine settles, and stopping retires them
all and keeps none.
-}
everyShapeStartsSettlesStops :: IO ()
everyShapeStartsSettlesStops = within 120 $
    forM_ shapes $ \shape ->
        forM_ [Settled, Unsettled] $ \standing ->
            forM_ [Success, Immaterial] $ \verdict -> do
                (dag, touches) <- instrumented (fixture verdict shape 300 7)
                let n = length (Dag.dagOrder dag)
                    what = show shape <> " " <> show standing <> " " <> show verdict
                trace <- newTVarIO []
                let r = ReporterM $ \rep -> atomically (modifyTVar' trace (rep :))
                sup <- Upkeep.startUpkeep r Upkeep.noKept (const (Just (Tend TurnUp standing))) dag
                await trace (\rs -> Set.size (Set.fromList (upAt rs)) == n)
                awaitStable sup
                kept <- Upkeep.stopUpkeep sup
                rs <- seen trace
                assertEqual (what <> ": nothing kept") 0 (Map.size (Upkeep.keptHeld kept))
                assertEqual (what <> ": every machine retired") [n] (retired rs)
                case (standing, verdict) of
                    (Settled, _) -> assertEqual (what <> ": nothing applied") [] (evaluated rs)
                    (Unsettled, Success) -> assertEqual (what <> ": nothing applied") [] (evaluated rs)
                    (Unsettled, _) ->
                        assertEqual (what <> ": every node applied once") (Set.fromList (Dag.dagOrder dag)) (Set.fromList (evaluated rs))
                case standing of
                    Settled -> pure ()
                    Unsettled -> do
                        first <- firstTouches touches
                        assertEqual (what <> ": nodes acted on before a dependency") [] (outOfOrder Set.empty dag first)

{- | Wrap every node's @check@ and @up@ so that entering either is logged,
in the order it happened.

The reports cannot tell that order: a dependency settles its status and only
then says it is up, so a dependant can say so in between. The log is causal
instead. A dependant consults its check (or runs its @up@) only once its
dependency has settled, which is after the dependency's own first @check@ or
@up@ was entered.
-}
instrumented :: Dag.Dag Extension -> IO (Dag.Dag Extension, TVar [Ref])
instrumented dag = do
    touches <- newTVarIO []
    let touch aref = atomically (modifyTVar' touches (aref :))
        wrap aref act =
            let ext = act.extension
             in act{extension = ext{check = touch aref >> ext.check, up = touch aref >> ext.up}}
    pure (dag{Dag.dagNodes = Map.mapWithKey wrap (Dag.dagNodes dag)}, touches)

-- | Where each node first appears in the log.
firstTouches :: TVar [Ref] -> IO (Map.Map Ref Int)
firstTouches touches = do
    logged <- reverse <$> readTVarIO touches
    pure (Map.fromListWith min (zip logged [0 ..]))

{- | Pairs of a node and a dependency it was acted on before. A node never
acted on constrains nothing. A dependency told to 'Satisfy' is left out: it
may settle without acting and be acted on later (forced, say), after its
dependants, and both are right.
-}
outOfOrder :: Set.Set Ref -> Dag.Dag Extension -> Map.Map Ref Int -> [(Ref, Ref)]
outOfOrder satisfied dag first =
    [ (aref, d)
    | aref <- Dag.dagOrder dag
    , Just mine <- [Map.lookup aref first]
    , d <- Dag.dependenciesOf dag aref
    , not (Set.member d satisfied)
    , maybe True (>= mine) (Map.lookup d first)
    ]

{- | The timer is the one thing the change moved: a node polling on a check
must still be woken by it, with nobody telling it anything. The first nap
after settling is a second ('Upkeep.relaxed' of the floor).
-}
napsEnd :: IO ()
napsEnd = within 30 $ do
    let dag = fixture Success Chain 5 3
        n = length (Dag.dagOrder dag)
    supervising dag (const (Just (Tend TurnUp Settled))) $ \_ trace -> do
        await trace $ \rs ->
            let looks = Map.fromListWith (+) [(act.extension.ref, 1 :: Int) | NextLook act _ _ <- rs]
             in Map.size looks == n && all (>= 2) (Map.elems looks)

-------------------------------------------------------------------------------
-- the property

data Script = Script
    { scriptShape :: Shape
    , scriptSize :: Int
    , scriptSeed :: Seed
    , scriptVerdict :: CheckResult
    , scriptStanding :: Standing
    , scriptSteps :: [(Int, Instruction)]
    }
    deriving (Show)

genScript :: Gen Script
genScript =
    Script
        <$> elements shapes
        <*> choose (1, 40)
        <*> (fromIntegral <$> choose (0 :: Int, 1000000))
        <*> elements [Success, Immaterial]
        <*> elements [Settled, Unsettled]
        <*> listOf ((,) <$> choose (0, 1000) <*> elements [Recheck, Force, Satisfy, Pause, Resume])

{- | Whatever is said to whichever machines while they start, once every one
of them is told to 'Resume' they all come up and settle; from scratch they
come up in dependency order; a node whose check is satisfied is applied only
if it was forced; and stopping retires every machine and keeps none.

Instructions go out as the machines start, so they land in every state a
machine passes through on the way: waiting on dependencies, applying,
napping, parked. Nothing here compares against the code before the change
(there is one implementation); it pins the behaviour that change had to
keep.
-}
prop_scriptedConvergence :: Property
prop_scriptedConvergence = forAll genScript $ \script -> ioProperty $ do
    let dag = fixture script.scriptVerdict script.scriptShape script.scriptSize script.scriptSeed
        order = Dag.dagOrder dag
        n = length order
        pick i = order !! (i `mod` n)
        told = [(pick i, ins) | (i, ins) <- script.scriptSteps]
        forced = Set.fromList [aref | (aref, Force) <- told]
    (dag', touches) <- instrumented dag
    trace <- newTVarIO []
    let r = ReporterM $ \rep -> atomically (modifyTVar' trace (rep :))
    outcome <- timeout (20 * 1000000) $ do
        sup <- Upkeep.startUpkeep r Upkeep.noKept (const (Just (Tend TurnUp script.scriptStanding))) dag'
        forM_ told $ \(aref, ins) -> void (Upkeep.instruct sup aref ins)
        forM_ order $ \aref -> void (Upkeep.instruct sup aref Resume)
        await trace (\rs -> Set.fromList (upAt rs) == Set.fromList order)
        awaitStable sup
        kept <- Upkeep.stopUpkeep sup
        rs <- seen trace
        pure (Map.size (Upkeep.keptHeld kept), rs)
    case outcome of
        Nothing -> do
            rs <- seen trace
            pure $
                counterexample
                    ("did not converge; nodes up: " <> show (Set.size (Set.fromList (upAt rs))) <> " of " <> show n)
                    False
        Just (keptCount, rs) -> do
            first <- firstTouches touches
            let misordered = case script.scriptStanding of
                    Settled -> []
                    Unsettled -> outOfOrder (Set.fromList [aref | (aref, Satisfy) <- told]) dag first
                unforced = Set.fromList (evaluated rs) `Set.difference` forced
            pure $
                counterexample ("kept " <> show keptCount) (keptCount == 0)
                    .&&. counterexample ("retired " <> show (retired rs)) (retired rs == [n])
                    .&&. counterexample ("up before a dependency: " <> show misordered) (null misordered)
                    .&&. counterexample
                        ("applied without being forced: " <> show (Set.toList unforced))
                        (script.scriptVerdict /= Success || Set.null unforced)
