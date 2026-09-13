{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for "Salmon.Actions.Serve": drive the @run serve@ loop
with a scripted stdin over a throwaway temp dir, and assert on both the real
filesystem effects and the 'World' it hands back.

The seed here is deliberately trivial (a list of file names, configured
straight through to the directive) — what is under test is the convergence
bookkeeping, not the recipe: that re-declaring a converged seed does nothing,
that retiring one tears down exactly the nodes no other seed still wants
(nodes unify by 'Ref' across seeds), and that a node whose @up@ threw is left
non-converged and picked up again by the next pass.
-}
module Test.ServeSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LByteString
import Data.Dynamic (toDyn)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadMode), hClose, hPutStr, hPutStrLn, hSetBuffering, withFile)
import System.IO.Temp (withSystemTempFile)
import System.Process (createPipe, proc)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Direction (..), NodeState (..), World (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Extension, Op, Track', check, deps, down, dynamics, managed, nodeps, op, opAct, ref, up)
import qualified Salmon.Builtin.Nodes.Daemon as Daemon
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import qualified Salmon.Op.Ledger as Ledger
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Rewrite (Phase (..), Rewrite)
import qualified Salmon.Op.Status as MachineStatus
import Salmon.Op.Supervision (Strategy (..), Supervision (..), defaultSupervision, supervised)
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (ReporterM (..), silent)

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Serve"
        [ testCase "a declared seed converges and stays converged" declaredSeedConverges
        , testCase "re-declaring a converged seed is a no-op" reDeclareIsNoop
        , testCase "retiring a seed tears its nodes down" retireTearsDown
        , testCase "retiring a multi-file bundle removes its shared directory cleanly" retireMultiFileBundle
        , testCase "`only` retires the previous seed but keeps shared nodes" onlySupersedes
        , testCase "a node whose up threw is retried by the next pass" failedNodeIsRetried
        , testCase "a retired declaration survives a failed down" retiredContributionSurvives
        , testCase "re-declaring a seed does not accumulate graphs" reDeclareDoesNotAccumulate
        , testCase "history outlives the graph it declared" historyOutlivesTheGraph
        , testCase "the history log is capped and says how much it dropped" historyLogIsCapped
        , testCase "unparseable input does not disturb the world" badInputIsInert
        , testCase "`load` runs a file of declarations as if typed" loadRunsAScript
        , testCase "`up-directive` declares straight from a directive file" upDirectiveDeclares
        , testCase "`status --exclude **` hides every node" statusExcludeAllHidesEverything
        , testCase "`history --exclude **` hides every epoch" historyExcludeAllHidesEverything
        , testCase "`converge --select` restricted to nothing leaves a failed node untouched" convergeSelectRestricts
        , testCase "`help` prints the command reference and touches nothing" helpPrintsReference
        , testCase "`help TOPIC` prints a longer, topic-specific block" helpTopicIsLonger
        , testCase "`help` with an unrecognised topic falls back to the full reference" helpUnknownTopicFallsBack
        , testCase "a registered rewrite batches across seeds and converges its members" rewriteBatchesAcrossSeeds
        , testCase "a rewrite's batch splits by direction when a seed is retired" rewriteSplitsOnRetire
        , testCase "an idle loop tends its nodes and puts a vanished effect back" idleLoopTends
        , testCase "`supervise off` leaves a vanished effect alone" superviseOffLeavesItAlone
        , testCase "a node that owns a process keeps it across commands, and loses it on clear" ownedProcessSurvivesCommands
        , testCase "an adopted process still follows the config it stands on" adoptedDaemonFollowsItsConfig
        , testCase "status shows a failing node's check and its last output" statusShowsAFailingNodesOutput
        , testCase "`force` re-applies a node its own check still calls satisfied" forceOverridesASatisfiedCheck
        , testCase "`pause` stops a node coming back, `resume` lets it" pauseThenResume
        , testCase "(I6) a re-declaration with changed content is applied by the pass itself, not just the tending loop" reDeclareWithChangedContentIsAppliedByThePass
        , testCase "`autoconverge off` records a declaration without converging it" autoConvergeOffDefersConvergence
        , testCase "`autoconverge off` also keeps the idle tending loop from applying a deferred declaration" autoConvergeOffAlsoStopsIdleTending
        , testCase "`autoconverge off` keeps a checkless node's `up` from ever running" autoConvergeOffKeepsAChecklessNodeFromRunning
        ]

-------------------------------------------------------------------------------
-- The thing being served: "make these files exist in this directory".

data Spec = Spec
    { specDir :: FilePath
    , specNames :: [String]
    }
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

-- | @up a b@ in the serve script means "these file names".
parseSpec :: FilePath -> [String] -> Either Text Spec
parseSpec root args
    | null args = Left "expected at least one file name"
    | otherwise = Right (Spec (root </> "files") args)

program :: Track' Spec
program = Track $ \spec ->
    op "serve-spec-root" (deps (fmap (fileOp spec.specDir) spec.specNames)) $ \actions ->
        actions{ref = mkRef "serve-spec-root" (spec.specDir, spec.specNames)}

-- | Note both the file and (via 'FS.filecontents') its enclosing directory
-- are real nodes with a real teardown, and the directory node is shared by
-- every seed here — that shared node is the unification this spec leans on.
fileOp :: FilePath -> String -> Op
fileOp d n = FS.filecontents (FS.FileContents (d </> n) ("contents of " <> n))

-------------------------------------------------------------------------------

declaredSeedConverges :: IO ()
declaredSeedConverges =
    withTempDir $ \root -> do
        (w, reports, _) <- runServe program root ["up a"]
        assertFileExists root "a" True
        assertAllConverged TurnUp w
        assertEqual "one converge, nothing left over" [True] (convergeOutcomes reports)

-- | `autoconverge off` lets a declaration record itself (visible to
-- `status`) without touching the filesystem, until a later `converge`
-- catches it up.
autoConvergeOffDefersConvergence :: IO ()
autoConvergeOffDefersConvergence =
    withTempDir $ \root -> do
        (w1, reports1, _) <- runServe program root ["autoconverge off", "up a"]
        assertFileExists root "a" False
        assertEqual "the declaration itself does not converge" [] (convergeOutcomes reports1)
        assertBool
            "declared node is recorded but still pending"
            (all (\st -> st.nodeConvergence == Pending) (Map.elems w1.worldNodes))
        (w2, reports2, _) <- runServe program root ["autoconverge off", "up a", "converge"]
        assertFileExists root "a" True
        assertEqual "the explicit converge runs exactly once" [True] (convergeOutcomes reports2)
        assertAllConverged TurnUp w2

reDeclareIsNoop :: IO ()
reDeclareIsNoop =
    withTempDir $ \root -> do
        (w, reports, _) <- runServe program root ["up a", "up a"]
        assertFileExists root "a" True
        assertAllConverged TurnUp w
        -- the first declaration has work to do, the second finds everything
        -- already converged and evaluates nothing at all.
        assertEqual
            "second declaration has nothing pending"
            [(0, 3), (0, 0)]
            (convergeStarts reports)
        assertEqual "history keeps both declarations" 2 (length w.worldLog)
        assertEqual "but they are one live declaration" 1 (Ledger.liveCount w.worldLedger)
        -- the superseded epoch is no longer the newest for its key and none
        -- of its nodes is on its way down, so its graph is collected rather
        -- than piling up.
        assertEqual "and only the live graph is retained" 1 (length w.worldEpochs)

retireTearsDown :: IO ()
retireTearsDown =
    withTempDir $ \root -> do
        (w, _, _) <- runServe program root ["up a", "down a"]
        assertFileExists root "a" False
        assertDirExists root False
        assertWorldSettled w

{- | Two files in one bundle share the enclosing-directory node. Tearing the
bundle down must remove both files before that directory, or @removeDirectory@
throws "directory not empty" — the shared-predecessor ordering fixed in
'Salmon.Actions.UpDown.downTree'. With one convergence and no retry, a wrong
order would leave the directory 'Errored'.
-}
retireMultiFileBundle :: IO ()
retireMultiFileBundle =
    withTempDir $ \root -> do
        (w, _, nodeReports) <- runServe program root ["up a b c", "down a b c"]
        assertFileExists root "a" False
        assertFileExists root "b" False
        assertFileExists root "c" False
        assertDirExists root False
        assertWorldSettled w
        assertEqual "nothing failed while tearing down" [] [() | UpDown.Failed{} <- nodeReports]

onlySupersedes :: IO ()
onlySupersedes =
    withTempDir $ \root -> do
        (w, _, _) <- runServe program root ["up a", "only b"]
        assertFileExists root "a" False
        assertFileExists root "b" True
        -- the enclosing directory is one node shared by both seeds' graphs:
        -- it must survive the teardown of the seed that is going away.
        assertDirExists root True
        assertEqual "one live declaration" 1 (Ledger.liveCount w.worldLedger)
        assertBool
            "every node converged, whichever way it is wanted"
            (all (\st -> st.nodeConvergence == Converged) (Map.elems w.worldNodes))
        -- the retired seed's graph has nothing left to describe: the node it
        -- alone held is down, and the shared directory belongs to `b` now.
        assertEqual "only the surviving seed's graph is retained" 1 (length w.worldEpochs)

failedNodeIsRetried :: IO ()
failedNodeIsRetried =
    withTempDir $ \root -> do
        attempts <- newIORef (0 :: Int)
        (w1, _, _) <- runServe (flaky attempts) root ["up a"]
        assertEqual "attempted once" 1 =<< readIORef attempts
        assertEqual
            "a node that threw is left non-converged"
            [Errored]
            (convergences w1)

        attempts2 <- newIORef (0 :: Int)
        (w2, reports, _) <- runServe (flaky attempts2) root ["up a", "converge"]
        assertEqual "retried by the explicit converge" 2 =<< readIORef attempts2
        assertEqual "and then it stuck" [Converged] (convergences w2)
        assertEqual "first pass failed, second one did not" [False, True] (convergeOutcomes reports)
  where
    convergences :: World Spec Spec -> [Convergence]
    convergences w = fmap nodeConvergence (Map.elems w.worldNodes)

{- | A contribution is collected once nothing could still walk it — so the
one thing that must never happen is collecting the description a teardown has
not finished with. This is what pins 'Salmon.Actions.Serve.resettle''s
ordering: prune before retune and the @down a@ declaration's own nodes still
look up-and-converged, so its contribution would be dropped and the retry
would have nothing to walk.

Note what is /not/ retained: the graph. A retired declaration's epoch goes
immediately, and what survives is its 'Salmon.Op.Ledger.Contribution' (nodes
and edges) plus those nodes' representatives in the magma — which is what
'Salmon.Actions.Serve.downDag' rebuilds the teardown from.
-}
retiredContributionSurvives :: IO ()
retiredContributionSurvives =
    withTempDir $ \root -> do
        attempts <- newIORef (0 :: Int)
        (w1, _, _) <- runServe (flakyDown attempts) root ["up a", "down a"]
        assertEqual "the teardown was attempted once" 1 =<< readIORef attempts
        assertEqual "and left the node non-converged" [Errored] (convergences w1)
        assertEqual "the retired declaration's graph is gone" 0 (length w1.worldEpochs)
        assertBool
            "but its contribution is still held, so the retry has something to walk"
            (not (null (Map.elems w1.worldLedger)))
        assertBool
            "and the node still has a representative to run down"
            (not (Map.null w1.worldMagma))

        attempts2 <- newIORef (0 :: Int)
        (w2, _, _) <- runServe (flakyDown attempts2) root ["up a", "down a", "converge"]
        assertEqual "retried by the explicit converge" 2 =<< readIORef attempts2
        assertWorldSettled w2
  where
    convergences :: World Spec Spec -> [Convergence]
    convergences w = fmap nodeConvergence (Map.elems w.worldNodes)

reDeclareDoesNotAccumulate :: IO ()
reDeclareDoesNotAccumulate =
    withTempDir $ \root -> do
        (w, _, _) <- runServe program root (replicate 5 "up a")
        assertEqual "every declaration is logged" 5 (length w.worldLog)
        assertEqual "but they describe one live graph between them" 1 (length w.worldEpochs)

historyOutlivesTheGraph :: IO ()
historyOutlivesTheGraph =
    withTempDir $ \root -> do
        (w, reports, _) <- runServe program root ["up a", "down a", "history"]
        assertWorldSettled w
        assertEqual
            "both declarations are still listed after their graphs are gone"
            [2]
            [length xs | Serve.HistoryReport xs <- reports]

historyLogIsCapped :: IO ()
historyLogIsCapped =
    withTempDir $ \root -> do
        let overflow = 5
        let script = replicate (Serve.worldLogLimit + overflow) "up a" <> ["history"]
        (w, reports, _) <- runServe program root script
        assertEqual "the log stops at the limit" Serve.worldLogLimit (length w.worldLog)
        assertEqual
            "and history says how much it is not showing"
            [overflow]
            [n | Serve.HistoryElided n <- reports]

badInputIsInert :: IO ()
badInputIsInert =
    withTempDir $ \root -> do
        (w, reports, _) <- runServe program root ["nonsense", "up", "up a"]
        assertFileExists root "a" True
        assertAllConverged TurnUp w
        assertEqual "only the well-formed declaration made an epoch" 1 (length w.worldLog)
        assertEqual "one unknown command, one unusable seed" (1, 1) (badCounts reports)
  where
    badCounts reports =
        ( length [() | Serve.BadCommand _ <- reports]
        , length [() | Serve.BadSeed _ <- reports]
        )

loadRunsAScript :: IO ()
loadRunsAScript =
    withTempDir $ \root -> do
        let scriptPath = root </> "commands.txt"
        writeFile scriptPath (unlines ["up a"])
        (w, reports, _) <- runServe program root ["load " <> scriptPath]
        assertFileExists root "a" True
        assertAllConverged TurnUp w
        assertEqual "loaded exactly one line" [1] [n | Serve.LoadDone _ n <- reports]

upDirectiveDeclares :: IO ()
upDirectiveDeclares =
    withTempDir $ \root -> do
        let directivePath = root </> "directive.json"
        LByteString.writeFile directivePath (encode (Spec (root </> "files") ["a"]))
        (w, _, _) <- runServe program root ["up-directive " <> directivePath]
        assertFileExists root "a" True
        assertAllConverged TurnUp w
        assertEqual "one epoch, declared from a directive file" [Nothing] (fmap Serve.epochSeed w.worldEpochs)
        assertEqual
            "history records the file, not seed args"
            [["<directive-file>", directivePath]]
            (fmap Serve.logTokens w.worldLog)

statusExcludeAllHidesEverything :: IO ()
statusExcludeAllHidesEverything =
    withTempDir $ \root -> do
        (_, reports, _) <- runServe program root ["up a", "status --exclude **"]
        -- `up a`'s own auto-converge never emits a StatusReport, so the only
        -- one here is the explicit `status` call's.
        assertEqual "every node excluded" [0] [length xs | Serve.StatusReport xs <- reports]

historyExcludeAllHidesEverything :: IO ()
historyExcludeAllHidesEverything =
    withTempDir $ \root -> do
        (_, reports, _) <- runServe program root ["up a", "history --exclude **"]
        assertEqual "every epoch excluded" [[]] [xs | Serve.HistoryReport xs <- reports]

convergeSelectRestricts :: IO ()
convergeSelectRestricts =
    withTempDir $ \root -> do
        attempts <- newIORef (0 :: Int)
        (w, reports, _) <-
            runServe (flaky attempts) root ["up a", "converge --select nope-does-not-match", "converge"]
        assertEqual "attempted twice: the initial failure, then the unrestricted retry" 2 =<< readIORef attempts
        assertEqual "eventually converged" [Converged] (convergences w)
        assertEqual
            -- the restricted pass reports True ("nothing it attempted
            -- failed") even though the excluded node is still pending —
            -- that's why `ConvergeStop`'s remaining-node count matters too.
            "three passes: declare's auto-converge (fails), the restricted no-op, the unrestricted retry"
            [False, True, True]
            (convergeOutcomes reports)
        assertEqual
            "the restricted pass leaves the node pending rather than wrongly marking it converged"
            [(0, 1), (0, 1), (0, 1)]
            [(ndown, nup) | Serve.ConvergeStart ndown nup <- reports]
  where
    convergences :: World Spec Spec -> [Convergence]
    convergences w = fmap nodeConvergence (Map.elems w.worldNodes)

helpPrintsReference :: IO ()
helpPrintsReference =
    withTempDir $ \root -> do
        (w, reports, _) <- runServe program root ["help"]
        assertEqual "help declares nothing" 0 (length w.worldLog)
        assertEqual "exactly one HelpText report, no topic" [Nothing] [t | Serve.HelpText t <- reports]

helpTopicIsLonger :: IO ()
helpTopicIsLonger =
    withTempDir $ \root -> do
        (_, reports, _) <- runServe program root ["help converge", "help"]
        let [topicLines, fullLines] = [Serve.renderReport rep | rep@Serve.HelpText{} <- reports]
        assertBool "a topic's own text is shorter than the full reference" (length topicLines < length fullLines)
        assertBool "a topic's text mentions its own command" (any (Text.isInfixOf "converge") topicLines)
        assertBool "a topic's text does not repeat unrelated commands" (not (any (Text.isInfixOf "up-directive") topicLines))

helpUnknownTopicFallsBack :: IO ()
helpUnknownTopicFallsBack =
    withTempDir $ \root -> do
        (_, reports, _) <- runServe program root ["help there-is-no-such-topic", "help"]
        let [unknownLines, fullLines] = [Serve.renderReport rep | rep@Serve.HelpText{} <- reports]
        assertEqual "an unrecognised topic renders exactly like no topic at all" fullLines unknownLines

-------------------------------------------------------------------------------

{- | A program with exactly one node, which throws the first time its @up@ is
run and succeeds afterwards.
-}
flaky :: IORef Int -> Track' Spec
flaky attempts = Track $ \spec ->
    op "flaky" nodeps $ \actions ->
        actions
            { ref = mkRef "flaky" spec.specNames
            , up = do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k))
                when (n == 0) $ throwIO (userError "flaky node failing on purpose")
            }

-- | Like 'flaky', but never recovers: every attempt throws. Used to pin
-- (R3) — a node whose failure is genuinely the /tending/ loop's doing, not
-- the declaring pass's, needs one that is still broken when the loop gets
-- to it.
flakyForever :: IORef Int -> Track' Spec
flakyForever attempts = Track $ \spec ->
    op "flaky-forever" nodeps $ \actions ->
        actions
            { ref = mkRef "flaky-forever" spec.specNames
            , up = do
                atomicModifyIORef' attempts (\k -> (k + 1, ()))
                throwIO (userError "flaky-forever node failing on purpose")
            }

{- | The mirror of 'flaky': one node whose @down@ throws the first time, so
the world is left holding a teardown it has not finished.
-}
flakyDown :: IORef Int -> Track' Spec
flakyDown attempts = Track $ \spec ->
    op "flaky-down" nodeps $ \actions ->
        actions
            { ref = mkRef "flaky-down" spec.specNames
            , down = do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k))
                when (n == 0) $ throwIO (userError "flaky node refusing to go down on purpose")
            }

-- | Run the serve loop over a scripted stdin, capturing both report streams.
runServe ::
    Track' Spec ->
    FilePath ->
    [String] ->
    IO (World Spec Spec, [Serve.Report], [UpDown.Report Extension])
runServe = runServeWith []

-- | 'runServe' with "Salmon.Op.Rewrite" phases registered.
runServeWith ::
    [Rewrite Extension] ->
    Track' Spec ->
    FilePath ->
    [String] ->
    IO (World Spec Spec, [Serve.Report], [UpDown.Report Extension])
runServeWith rewrites prog root script = do
    (serveReporter, readServeReports) <- capture
    (nodeReporter, readNodeReports) <- capture
    w <-
        withScript script $
            Serve.serveWith rewrites Nothing True serveReporter nodeReporter (parseSpec root) (Configure pure) prog
    (,,) w <$> readServeReports <*> readNodeReports

withScript :: [String] -> (Handle -> IO a) -> IO a
withScript ls act =
    withSystemTempFile "salmon-serve-script" $ \path h -> do
        hPutStr h (unlines ls)
        hClose h
        withFile path ReadMode act

-- | (nodes to turn down, nodes to turn up) at the start of each convergence.
convergeStarts :: [Serve.Report] -> [(Int, Int)]
convergeStarts reports = [(ndown, nup) | Serve.ConvergeStart ndown nup <- reports]

-- | Whether each convergence applied everything it attempted cleanly.
convergeOutcomes :: [Serve.Report] -> [Bool]
convergeOutcomes reports = [ok | Serve.ConvergeStop ok _ <- reports]

assertAllConverged :: Direction -> World seed directive -> IO ()
assertAllConverged dir w = do
    assertBool "expected at least one node" (not (Map.null w.worldNodes))
    mapM_ (uncurry check) (Map.toList w.worldNodes)
  where
    check :: Ref -> NodeState -> IO ()
    check _ st = do
        assertEqual (Text.unpack st.nodeShorthand <> ": direction") dir st.nodeDirection
        assertEqual (Text.unpack st.nodeShorthand <> ": convergence") Converged st.nodeConvergence

{- | A world whose seeds have all been retired and converged keeps nothing:
the nodes are off the machine, and every structure that described them has
nothing left to say. @history@ is what still remembers they existed.
-}
assertWorldSettled :: World seed directive -> IO ()
assertWorldSettled w = do
    assertEqual "no node left to manage" 0 (Map.size w.worldNodes)
    assertEqual "no graph left to walk" 0 (length w.worldEpochs)
    assertEqual "no contribution left in the ledger" 0 (Map.size w.worldLedger)
    assertEqual "no representative left in the magma" 0 (Map.size w.worldMagma)

assertFileExists :: FilePath -> String -> Bool -> IO ()
assertFileExists root name expected = do
    found <- doesFileExist (root </> "files" </> name)
    assertEqual (name <> " exists") expected found

assertDirExists :: FilePath -> Bool -> IO ()
assertDirExists root expected = do
    found <- doesDirectoryExist (root </> "files")
    assertEqual "enclosing directory exists" expected found

-------------------------------------------------------------------------------
-- A rewrite, without needing apt on the machine running the tests.
--
-- The same shape as 'Salmon.Builtin.Nodes.Debian.Package.batchPackages' —
-- collect every node carrying a 'Widget' into one node per direction, keyed
-- on 'phaseDesired' — but the batch just appends to an 'IORef' instead of
-- shelling out. What is under test is 'Salmon.Actions.Serve''s side of it:
-- that a node no declaration ever mentioned is still gated correctly (via
-- its members) and that its outcome is recorded against the nodes an
-- operator actually declared.

newtype Widget = Widget Text
    deriving (Eq, Ord, Show)

-- | A seed whose file names each also declare a 'Widget'.
widgetProgram :: Track' Spec
widgetProgram = Track $ \spec ->
    op "widget-root" (deps (fmap widget spec.specNames)) $ \actions ->
        actions{ref = mkRef "widget-root" (spec.specDir, spec.specNames)}
  where
    widget n =
        op "widget" nodeps $ \actions ->
            actions
                { ref = mkRef "widget" (Text.pack n)
                , dynamics = [toDyn (Widget (Text.pack n))]
                }

batchWidgets :: IORef [(Text, [Widget])] -> Rewrite Extension
batchWidgets ranRef phase computed =
    batch "install" (filter (isDesired . fst) declared) $
        batch "remove" (filter (not . isDesired . fst) declared) computed
  where
    declared = Rewrite.collectDynamic computed
    isDesired rf = Set.member rf phase.phaseDesired

    batch what members c
        | null members = c
        | otherwise =
            case opAct (batchOp what (concatMap snd members)) of
                Nothing -> c
                Just act -> Rewrite.introduce act (Set.fromList (fmap fst members)) c

    batchOp what ws =
        op "widget-batch" nodeps $ \actions ->
            actions
                { ref = mkRef "widget-batch" (what :: Text, [w | Widget w <- ws])
                , up = record what ws
                , down = record what ws
                }

    record what ws = atomicModifyIORef' ranRef (\xs -> ((what, ws) : xs, ()))

{- | Two seeds, each declaring its own widgets, both live. A rewrite running
after the fold sees all of them at once — which is exactly what an
@Op -> Op@ applied inside the 'Track'' could not do, since it only ever had
one directive.
-}
rewriteBatchesAcrossSeeds :: IO ()
rewriteBatchesAcrossSeeds =
    withTempDir $ \root -> do
        ran <- newIORef []
        (w, _, _) <- runServeWith [batchWidgets ran] widgetProgram root ["up a", "up b"]
        batches <- reverse <$> readIORef ran
        assertEqual
            "the second convergence batched both seeds' widgets in one node"
            [Widget "a", Widget "b"]
            (Data.List.sort (concat [ws | ("install", ws) <- batches, length ws == 2]))
        assertBool
            "every declared widget node is converged, though none of them ran itself"
            (all (\st -> st.nodeConvergence == Converged) (Map.elems w.worldNodes))

{- | Retiring one of the two seeds is the case that has no pre-fold
expression at all: one widget is on its way out while the other is staying,
so the rewrite must emit two batches rather than one, and must not sweep the
surviving widget into the removal.
-}
rewriteSplitsOnRetire :: IO ()
rewriteSplitsOnRetire =
    withTempDir $ \root -> do
        ran <- newIORef []
        (w, _, _) <- runServeWith [batchWidgets ran] widgetProgram root ["up a", "up b", "down a"]
        batches <- reverse <$> readIORef ran
        assertEqual
            "a came out on its own"
            [[Widget "a"]]
            [ws | ("remove", ws) <- batches]
        assertBool
            "and b was never in a removal batch"
            (all (\(_, ws) -> Widget "b" `notElem` ws) [b | b@("remove", _) <- batches])
        assertAllConverged TurnUp w

-------------------------------------------------------------------------------
-- supervision between commands

{- | The two cases below drive the loop over a real pipe rather than a
scripted file, because idleness is the whole point: 'Salmon.Actions.Serve'
tends its nodes only while nothing is waiting in its input, and every line of
a piped script is already queued by the time the first pass finishes. So
these write one command, wait for what the machines say, and only then write
the next.

The waiting is on the report streams, never on a clock: a case that passes
does so as soon as the machines get there.
-}
data Session = Session
    { sessionIn :: !Handle
    , sessionServe :: !(TVar [Serve.Report])
    , sessionNodes :: !(TVar [UpDown.Report Extension])
    }

-- | Run the loop on its own thread over a pipe the body writes into.
withSession :: Track' Spec -> FilePath -> (Session -> IO a) -> IO (a, World Spec Spec)
withSession prog root body = do
    serveTrace <- newTVarIO []
    nodeTrace <- newTVarIO []
    let serveReporter = ReporterM (\rep -> atomically (modifyTVar' serveTrace (rep :)))
    let nodeReporter = ReporterM (\rep -> atomically (modifyTVar' nodeTrace (rep :)))
    (readEnd, writeEnd) <- createPipe
    hSetBuffering writeEnd LineBuffering
    done <- newEmptyMVar
    _ <-
        forkIO $ do
            w <- Serve.serveWith [] Nothing True serveReporter nodeReporter (parseSpec root) (Configure pure) prog readEnd
            putMVar done w
    let session = Session writeEnd serveTrace nodeTrace
    result <- body session
    hPutStrLn writeEnd "quit"
    w <- expect "the loop to exit" (takeMVar done)
    hClose writeEnd
    pure (result, w)

-- | Block until the reports so far (oldest first) satisfy the predicate.
awaitOn :: TVar [a] -> ([a] -> Bool) -> IO ()
awaitOn trace p =
    expect "the reports to say so" $
        atomically $ do
            rs <- readTVar trace
            unless (p (reverse rs)) retry

expect :: String -> IO a -> IO a
expect what act = do
    result <- timeout 20000000 act
    maybe (fail ("timed out waiting for " <> what)) pure result

-- | Whether supervision has started over at least one node.
tending :: [Serve.Report] -> Bool
tending rs = not (null [() | Serve.Tended (Upkeep.Supervising nup _) <- rs, nup > 0])

dones :: [UpDown.Report Extension] -> Int
dones rs = length [() | UpDown.Done _ <- rs]

{- | A node whose effect something else can remove. Its @check@ is the only
thing in the model that can notice, which is exactly the case @check@ was
merged into existence for.
-}
watched :: IORef Bool -> IORef Int -> Track' Spec
watched there attempts = Track (watchedOp there attempts id)

watchedOp :: IORef Bool -> IORef Int -> (Extension -> Extension) -> Spec -> Op
watchedOp there attempts f spec =
    op "watched" nodeps $ \actions ->
        f
            actions
                { ref = mkRef "watched" spec.specNames
                , check = do
                    ok <- readIORef there
                    pure (if ok then Success else Failure "gone")
                , up = do
                    atomicModifyIORef' attempts (\k -> (k + 1, ()))
                    writeIORefTrue there
                }
  where
    writeIORefTrue v = atomicModifyIORef' v (const (True, ()))

{- | A stub node with no @check@ at all — the default 'Immaterial' verdict
almost every builtin in this repository actually has, per
"Salmon.Actions.Serve"'s own note that this is the common case a fix here
has to hold for. Unlike 'watched', nothing here can ever say "already
done"; the only way to tell whether the loop left it alone is to count how
many times @up@ itself ran.
-}
neverRuns :: IORef Int -> Track' Spec
neverRuns attempts = Track $ \spec ->
    op "never-runs" nodeps $ \actions ->
        actions
            { ref = mkRef "never-runs" spec.specNames
            , up = atomicModifyIORef' attempts (\k -> (k + 1, ()))
            }

idleLoopTends :: IO ()
idleLoopTends =
    withTempDir $ \root -> do
        there <- newIORef False
        attempts <- newIORef (0 :: Int)
        (_, w) <- withSession (watched there attempts) root $ \session -> do
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe tending
            assertEqual "the pass brought it up once" 1 =<< readIORef attempts
            -- something else removes the effect; nothing tells the loop
            atomicModifyIORef' there (const (False, ()))
            awaitOn session.sessionNodes (\rs -> dones rs >= 2)
            assertEqual "its own machine put it back" 2 =<< readIORef attempts
        assertEqual
            "and the world still says converged"
            [Converged]
            (fmap nodeConvergence (Map.elems w.worldNodes))

{- | The bug this pinned: 'commitEpoch' skipping its own auto-@converge@ is
not enough on its own, because the idle tending loop ('tendOf') used to
treat any not-yet-'Converged' node as work to do regardless of
@autoconverge@ — so a script that declared, then merely went idle for a
moment before its next command, got the deferred node applied anyway by the
tending loop rather than by the pass. This drives a real idle gap (a
'threadDelay', not a queued script) so the loop actually gets the chance to
tend before asserting it did not.
-}
autoConvergeOffAlsoStopsIdleTending :: IO ()
autoConvergeOffAlsoStopsIdleTending =
    withTempDir $ \root -> do
        there <- newIORef False
        attempts <- newIORef (0 :: Int)
        (_, w) <- withSession (watched there attempts) root $ \session -> do
            hPutStrLn session.sessionIn "autoconverge off"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.AutoConverged False <- rs]))
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.Declared{} <- rs]))
            -- a real idle gap: enough time for the loop to have started
            -- tending and applied the node, if it were going to.
            threadDelay 300000
            hPutStrLn session.sessionIn "status"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.StatusReport _ <- rs]))
            assertEqual "the idle loop must not apply a deferred declaration" 0 =<< readIORef attempts
        assertEqual "the node is still pending, not silently converged" [Pending] (fmap nodeConvergence (Map.elems w.worldNodes))

{- | The same property as 'autoConvergeOffAlsoStopsIdleTending', pinned
directly on the node's own 'up' rather than through 'watched''s @check@
detour: 'neverRuns' has no @check@ at all (the ordinary 'Immaterial'
default, not a hand-written "gone" verdict), so there is nothing here that
can claim the effect is already in place — the only way this test could
pass wrongly is if @up@ genuinely never ran, which is the whole point.
-}
autoConvergeOffKeepsAChecklessNodeFromRunning :: IO ()
autoConvergeOffKeepsAChecklessNodeFromRunning =
    withTempDir $ \root -> do
        attempts <- newIORef (0 :: Int)
        (_, w) <- withSession (neverRuns attempts) root $ \session -> do
            hPutStrLn session.sessionIn "autoconverge off"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.AutoConverged False <- rs]))
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.Declared{} <- rs]))
            -- a real idle gap: enough time for the loop to have started
            -- tending and applied the node, if it were going to.
            threadDelay 300000
            hPutStrLn session.sessionIn "status"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.StatusReport _ <- rs]))
            assertEqual "up must never have run" 0 =<< readIORef attempts
            -- the deferred work is still there, waiting for an explicit
            -- `converge` — this isn't "up never runs at all", only "not
            -- before I say so".
            hPutStrLn session.sessionIn "converge"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.ConvergeStop{} <- rs]))
            assertEqual "the explicit converge finally runs it, exactly once" 1 =<< readIORef attempts
        assertEqual "and the world now agrees it converged" [Converged] (fmap nodeConvergence (Map.elems w.worldNodes))

superviseOffLeavesItAlone :: IO ()
superviseOffLeavesItAlone =
    withTempDir $ \root -> do
        there <- newIORef False
        attempts <- newIORef (0 :: Int)
        _ <- withSession (watched there attempts) root $ \session -> do
            hPutStrLn session.sessionIn "supervise off"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.Supervised False <- rs]))
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe (\rs -> length [() | Serve.ConvergeStop _ _ <- rs] >= 1)
            atomicModifyIORef' there (const (False, ()))
            -- there is nothing to wait for, which is the assertion: ask the
            -- loop to do something else and check nothing happened in the
            -- meantime.
            hPutStrLn session.sessionIn "status"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.StatusReport _ <- rs]))
            assertEqual "nothing put it back" 1 =<< readIORef attempts
            assertBool "and supervision never started" . not . tending
                =<< atomically (reverse <$> readTVar session.sessionServe)
        pure ()

{- | The property the whole 'Salmon.Actions.Upkeep.Kept' machinery exists
for, and the one that cannot be seen anywhere smaller.

@serve@ stands its machines down before every command it is handed, @status@
included. A machine that holds a running process cannot be stood down the way
a one-shot machine is, or typing @status@ would restart every service on the
box — so it survives, and the next supervisor adopts it. And when the node
stops being wanted, it has to be let go /before/ the down pass starts
removing what it stood on.

Both halves are asserted the same way: whether the process is still writing.
-}
ownedProcessSurvivesCommands :: IO ()
ownedProcessSurvivesCommands =
    withTempDir $ \root -> do
        let ticks = root </> "ticks"
        (_, w) <- withSession (ticker ticks) root $ \session -> do
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe (\rs -> length [() | Serve.ConvergeStop _ _ <- rs] >= 1)
            -- it is running
            n0 <- awaitTicks ticks 2
            -- a read-only command: the machines stand down, but not this one
            hPutStrLn session.sessionIn "status"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.StatusReport _ <- rs]))
            n1 <- awaitTicks ticks (n0 + 2)
            assertBool "it kept running across the command" (n1 > n0)
            -- ...and now nothing wants it
            hPutStrLn session.sessionIn "clear"
            awaitOn session.sessionServe (\rs -> length [() | Serve.ConvergeStop _ _ <- rs] >= 2)
            n2 <- countTicks ticks
            threadDelay 400000
            n3 <- countTicks ticks
            assertEqual "and stopped once nothing wanted it" n2 n3
        assertEqual
            "the world records it as having been dealt with"
            []
            [st.nodeConvergence | st <- Map.elems w.worldNodes, st.nodeConvergence /= Converged]

{- | Milestone 9 under @serve@, which is the one place its neighbourhood
refresh can be seen at all.

A machine holding a process is /adopted/ by every new supervisor rather than
restarted (see 'Salmon.Actions.Upkeep.Kept'), and every supervisor is new: the
loop stands its machines down before each command it is handed. So an adopted
machine that went on watching the maps of the supervisor that started it
would stop noticing its config change after the very first command — which is
to say, immediately and silently.

Both halves are asserted: that the command did not restart it (the spawn
count is unchanged across a @status@), and that it still followed its config
afterwards.
-}
adoptedDaemonFollowsItsConfig :: IO ()
adoptedDaemonFollowsItsConfig =
    withTempDir $ \root -> do
        there <- newIORef False
        attempts <- newIORef (0 :: Int)
        spawns <- newTVarIO (0 :: Int)
        let ticks = root </> "ticks"
        _ <- withSession (tickerOnConfig ticks there attempts spawns) root $ \session -> do
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe (\rs -> length [() | Serve.ConvergeStop _ _ <- rs] >= 1)
            awaitSpawns spawns 1
            -- a read-only command, after which a different supervisor is
            -- tending the same still-running process
            hPutStrLn session.sessionIn "status"
            awaitOn session.sessionServe (\rs -> supervisings rs >= 2)
            assertEqual "the command adopted it rather than restarting it" 1 =<< readTVarIO spawns
            -- now the config underneath it is taken away
            atomicModifyIORef' there (const (False, ()))
            awaitSpawns spawns 2
            assertEqual "the config node had put its own effect back first" 2 =<< readIORef attempts
        pure ()

-- | How many times supervision has (re)started over at least one node.
supervisings :: [Serve.Report] -> Int
supervisings rs = length [() | Serve.Tended (Upkeep.Supervising _ _) <- rs]

awaitSpawns :: TVar Int -> Int -> IO ()
awaitSpawns v n =
    expect ("the process to have been started " <> show n <> " time(s)") $
        atomically (readTVar v >>= \k -> unless (k >= n) retry)

{- | A process standing on a configuration node that declares
'Salmon.Op.Supervision.RestForOne', which is the shape the whole strategy
exists for: the config is rewritten, so what reads it has to be bounced.
-}
tickerOnConfig :: FilePath -> IORef Bool -> IORef Int -> TVar Int -> Track' Spec
tickerOnConfig path there attempts spawns = Track $ \spec ->
    let cfg = watchedOp there attempts restForOne spec
     in op "ticker" (deps [cfg]) $ \actions ->
            actions
                { ref = Daemon.daemonRef (tickerDaemon path)
                , managed = Just $ \out -> do
                    atomically (modifyTVar' spawns (+ 1))
                    Daemon.runDaemon silent (tickerDaemon path) out
                , up = throwIO (userError "the ticker cannot be brought up by a one-shot pass")
                }
  where
    restForOne x = x{dynamics = [supervised defaultSupervision{supStrategy = RestForOne}]}

{- | One node, which owns a process that writes a line every 50ms.

@up@ throwing is 'Salmon.Builtin.Nodes.Daemon.daemon''s own convention and is
what makes the test meaningful: if @serve@ ever routed this node through a
convergence pass instead of to a machine, the pass would fail loudly rather
than quietly do nothing.
-}
ticker :: FilePath -> Track' Spec
ticker path = Track (const (Daemon.daemon silent (tickerDaemon path)))

tickerDaemon :: FilePath -> Daemon.Daemon
tickerDaemon path =
    Daemon.defaultDaemon
        "ticker"
        (proc "/bin/sh" ["-c", "while true; do echo tick >> " <> path <> "; sleep 0.05; done"])

countTicks :: FilePath -> IO Int
countTicks path = do
    there <- doesFileExist path
    if not there
        then pure 0
        else do
            contents <- try (readFile path) :: IO (Either IOException String)
            pure (either (const 0) (length . lines) contents)

-- | Block until the file has at least this many lines, and say how many.
awaitTicks :: FilePath -> Int -> IO Int
awaitTicks path n = expect ("the process to write " <> show n <> " line(s)") go
  where
    go = do
        k <- countTicks path
        if k >= n then pure k else threadDelay 25000 >> go

-- | Block until an attempt counter has reached at least this many — the way
-- a case confirms the /idle tending loop/, not just the declaring pass, has
-- had a go at a node.
awaitAttempts :: IORef Int -> Int -> IO ()
awaitAttempts ref n = expect ("at least " <> show n <> " attempt(s)") go
  where
    go = do
        k <- readIORef ref
        if k >= n then pure () else threadDelay 25000 >> go

{- | (R3): a node's own last word about itself is visible after its machine
stands down, not lost the moment 'Salmon.Actions.Serve.stopTending' drops
the 'Salmon.Actions.Upkeep.Supervisor' holding its
'Salmon.Op.Status.Status'. The node here never recovers, so the idle tending
loop — not the declaring pass — is what produces the failing status this
pins: @up a@ fails synchronously ('Errored'), the idle loop picks it up
because it is not yet 'Converged', and every retry settles a fresh
'Salmon.Actions.UpDown.Failure' (with its own narration already in the
output ring) into the 'Salmon.Op.Status.Status' that @status@ then reads.
-}
statusShowsAFailingNodesOutput :: IO ()
statusShowsAFailingNodesOutput =
    withTempDir $ \root -> do
        attempts <- newIORef (0 :: Int)
        _ <- withSession (flakyForever attempts) root $ \session -> do
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe (\rs -> length [() | Serve.ConvergeStop _ _ <- rs] >= 1)
            -- one retry beyond the declaring pass's own attempt, so the
            -- failing status this pins is genuinely the tending machine's.
            awaitAttempts attempts 2
            hPutStrLn session.sessionIn "status"
            awaitOn session.sessionServe (\rs -> any isFailingSnapshot (flakyStates rs))
            rs <- atomically (reverse <$> readTVar session.sessionServe)
            case flakyStates rs of
                (st : _) -> case st.nodeStatus of
                    Just ms -> do
                        assertBool "remembered as a failure" (isFailure (MachineStatus.statusCheck ms))
                        assertBool
                            "and its last output was captured"
                            (not (null (MachineStatus.ringLines (MachineStatus.statusOutput ms))))
                    Nothing -> assertFailure "expected a status snapshot for the failing node"
                [] -> assertFailure "expected the flaky node in a status report"
        pure ()
  where
    flakyStates :: [Serve.Report] -> [NodeState]
    flakyStates rs =
        [st | Serve.StatusReport xs <- rs, (_, st) <- xs, st.nodeShorthand == "flaky-forever"]

    isFailingSnapshot :: NodeState -> Bool
    isFailingSnapshot st = maybe False (isFailure . MachineStatus.statusCheck) st.nodeStatus

    isFailure :: CheckResult -> Bool
    isFailure (Failure _) = True
    isFailure _ = False

{- | (R2). @force@ posts straight into a node's mailbox once its next
machine exists, and the FSM already treats that as "run @up@ regardless of
what the check says" (see @Test.UpkeepSpec@'s "Force restarts it rather than
skipping it"). What that leaves untested is the command-language plumbing
that gets an instruction there at all: parse the command, resolve the
selection against the world, queue it, and deliver it the moment tending
next starts.

The node here never goes unsatisfied (@there@ stays 'True' throughout), so a
second @up@ can only be explained by @force@ itself — not by the ordinary
"the effect went away" path 'idleLoopTends' already covers.
-}
forceOverridesASatisfiedCheck :: IO ()
forceOverridesASatisfiedCheck =
    withTempDir $ \root -> do
        there <- newIORef False
        attempts <- newIORef (0 :: Int)
        _ <- withSession (watched there attempts) root $ \session -> do
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe tending
            assertEqual "the pass brought it up once" 1 =<< readIORef attempts
            hPutStrLn session.sessionIn "force"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.Instructed _ n <- rs, n > 0]))
            awaitAttempts attempts 2
            assertBool "the check never had a reason to fail" =<< readIORef there
        pure ()

{- | (R2). @pause@ stops a node's machine reacting to its effect going away;
@resume@ lets it react again. Both travel the same queue-then-deliver path
'forceOverridesASatisfiedCheck' pins, and are worth their own case because an
instruction whose whole effect is "do nothing" is otherwise invisible: this
is the one place a wrong delivery (skipped, or delivered to the wrong
machine) would show up as a spurious @up@ instead of a missing one.
-}
pauseThenResume :: IO ()
pauseThenResume =
    withTempDir $ \root -> do
        there <- newIORef False
        attempts <- newIORef (0 :: Int)
        _ <- withSession (watched there attempts) root $ \session -> do
            hPutStrLn session.sessionIn "up a"
            awaitOn session.sessionServe tending
            assertEqual "the pass brought it up once" 1 =<< readIORef attempts
            hPutStrLn session.sessionIn "pause"
            awaitOn session.sessionServe (\rs -> not (null [() | Serve.Tended (Upkeep.Paused _) <- rs]))
            atomicModifyIORef' there (const (False, ()))
            threadDelay 300000
            assertEqual "paused, so nothing put it back" 1 =<< readIORef attempts
            hPutStrLn session.sessionIn "resume"
            awaitAttempts attempts 2
        pure ()

-------------------------------------------------------------------------------
-- (I6): a maintained Ref whose content changed goes 'Serve.Stale', not
-- silently 'Serve.Converged'.

-- | Unlike 'Spec' above, content is independent of the declared name — the
-- whole point here is to redeclare the *same* path with *different*
-- content, which 'Spec'\/'program' cannot express (its content is
-- deterministic from the file name).
data GreetingSpec = GreetingSpec
    { greetingPath :: FilePath
    , greetingText :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON GreetingSpec
instance FromJSON GreetingSpec

parseGreetingSpec :: [String] -> Either Text GreetingSpec
parseGreetingSpec [path, txt] = Right (GreetingSpec path (Text.pack txt))
parseGreetingSpec _ = Left "expected: <path> <text>"

greetingProgram :: Track' GreetingSpec
greetingProgram = Track $ \spec -> FS.filecontents (FS.FileContents spec.greetingPath spec.greetingText)

runGreetingServe :: [String] -> IO (World GreetingSpec GreetingSpec, [Serve.Report], [UpDown.Report Extension])
runGreetingServe script = do
    (serveReporter, readServeReports) <- capture
    (nodeReporter, readNodeReports) <- capture
    w <-
        withScript script $
            Serve.serveWith [] Nothing True serveReporter nodeReporter parseGreetingSpec (Configure pure) greetingProgram
    (,,) w <$> readServeReports <*> readNodeReports

assertFileContentIs :: FilePath -> String -> IO ()
assertFileContentIs path expected = do
    got <- Prelude.readFile path
    assertEqual (path <> ": content") expected got

{- | The scenario @salmon-ops-serve-fixture@'s own haddock uses to demonstrate
(I6) — re-declaring a config file with new content reports @converging (0
down, 0 up)@ and (before this) relied entirely on the tending machine's own
next look to apply it. A piped script is deliberately never supervised (see
"Salmon.Actions.Serve"'s own module haddock: idle-only tending is what keeps
@serve < script@ a deterministic sequence of passes), so this is also the
sharpest possible demonstration of the bug: under the old behaviour, this
exact test would leave the file saying "hello" forever, since nothing here
ever gives a tending machine a chance to run.

'FS.filecontents' backs onto 'Text.Text', which has a
'Salmon.Builtin.Nodes.Filesystem.EncodeFileContents' 'contentFingerprint'
(see (I6) in @specs\/per-node-state-machines-remaining.md@), so the second
declaration's 'notes' differ from the first's and 'Serve.record' marks the
'Ref' 'Serve.Stale' rather than leaving it 'Serve.Converged' — which is what
lets the second 'Serve.ConvergeStart' actually have a node to apply, instead
of the @(0, 0)@ a fully-converged, untouched graph would report.
-}
reDeclareWithChangedContentIsAppliedByThePass :: IO ()
reDeclareWithChangedContentIsAppliedByThePass =
    withTempDir $ \root -> do
        let path = root </> "daemon.conf"
        (w, reports, _) <-
            runGreetingServe ["up " <> path <> " hello", "only " <> path <> " goodbye"]
        assertFileContentIs path "goodbye"
        assertEqual
            "the first declaration converges the file and its enclosing directory; the second, only the changed file"
            [(0, 2), (0, 1)]
            (convergeStarts reports)
        assertAllConverged TurnUp w
