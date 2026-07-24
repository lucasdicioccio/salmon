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

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode), hClose, hPutStr, withFile)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Direction (..), NodeState (..), World (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension, Op, Track', deps, nodeps, op, ref, up)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Track (Track (..))

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
        , testCase "unparseable input does not disturb the world" badInputIsInert
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
        assertEqual "history keeps both declarations" 2 (length w.worldHistory)
        assertEqual "but they are one active seed" 1 (Map.size w.worldActive)

retireTearsDown :: IO ()
retireTearsDown =
    withTempDir $ \root -> do
        (w, _, _) <- runServe program root ["up a", "down a"]
        assertFileExists root "a" False
        assertDirExists root False
        assertAllConverged TurnDown w

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
        assertAllConverged TurnDown w
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
        assertEqual "one active seed" 1 (Map.size w.worldActive)
        assertBool
            "every node converged, whichever way it is wanted"
            (all (\st -> st.nodeConvergence == Converged) (Map.elems w.worldNodes))

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

badInputIsInert :: IO ()
badInputIsInert =
    withTempDir $ \root -> do
        (w, reports, _) <- runServe program root ["nonsense", "up", "up a"]
        assertFileExists root "a" True
        assertAllConverged TurnUp w
        assertEqual "only the well-formed declaration made an epoch" 1 (length w.worldHistory)
        assertEqual "one unknown command, one unusable seed" (1, 1) (badCounts reports)
  where
    badCounts reports =
        ( length [() | Serve.BadCommand _ <- reports]
        , length [() | Serve.BadSeed _ <- reports]
        )

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

-- | Run the serve loop over a scripted stdin, capturing both report streams.
runServe ::
    Track' Spec ->
    FilePath ->
    [String] ->
    IO (World Spec Spec, [Serve.Report], [UpDown.Report Extension])
runServe prog root script = do
    (serveReporter, readServeReports) <- capture
    (nodeReporter, readNodeReports) <- capture
    w <-
        withScript script $
            Serve.serve serveReporter nodeReporter (parseSpec root) (Configure pure) prog
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

assertFileExists :: FilePath -> String -> Bool -> IO ()
assertFileExists root name expected = do
    found <- doesFileExist (root </> "files" </> name)
    assertEqual (name <> " exists") expected found

assertDirExists :: FilePath -> Bool -> IO ()
assertDirExists root expected = do
    found <- doesDirectoryExist (root </> "files")
    assertEqual "enclosing directory exists" expected found
