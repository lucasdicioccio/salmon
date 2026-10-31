{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Op.PassPhase": the two-phase barrier (every
build node before any deploy node) and @run up --phase build@.

Nothing here runs a real build or push: the nodes record their own name
when their @up@ runs, and one of them can be made to throw. What is under
test is the shape of the computed graph, the order and containment the
sequential driver gives it, and which nodes the build phase selects.
-}
module Test.PassPhaseSpec (tests) where

import Control.Exception (throwIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (elemIndex)
import qualified Data.Set as Set
import Data.Text (Text)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, info)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.CommandLine (RunCommand (..), buildPhaseGate, runCommandParser)
import Salmon.Builtin.Extension (Extension (..), Op, amendNode, deps, evalDeps, inBuildPhase, inDeployPhase, op)
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.PassPhase (PassPhase (..), SelectionError (..))
import qualified Salmon.Op.PassPhase as PassPhase
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Rewrite (Phase (..))
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Reporter (silent)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.PassPhase"
        [ testCase "a graph without tags is left exactly as folded" untaggedUnchanged
        , testCase "every deploy node depends on every build node" barrierEdges
        , testCase "every build runs before any deploy, across branches" buildsFirst
        , testCase "one failed build blocks every deploy, not the other builds" failedBuildBlocksDeploys
        , testCase "the barrier only joins nodes the pass wants up" onlyDesired
        , testCase "a build under a deploy is a cycle the driver reports" buildUnderDeployBlocks
        , testCase "the tag is left out of the representative" tagNotInRepresentative
        , testCase "the build phase selects builds and their dependencies only" selectionClosure
        , testCase "the build phase refuses a graph with no build node" selectionNoBuild
        , testCase "the build phase refuses a deploy under a build" selectionDeployUnderBuild
        , testCase "run up --phase build runs builds and skips deploys" phaseBuildRuns
        , testCase "run up parses --phase build, rejects --phase deploy, defaults to everything" phaseFlag
        ]

-------------------------------------------------------------------------------

type Recorder = Text -> IO ()

node :: Recorder -> Text -> [Op] -> Op
node rec name ds = op name (deps ds) $ \x -> x{ref = mkRef "node" name, up = rec name}

failing :: Text -> [Op] -> Op
failing name ds = op name (deps ds) $ \x -> x{ref = mkRef "node" name, up = throwIO (userError "boom")}

build, deploy :: Op -> Op
build = amendNode inBuildPhase
deploy = amendNode inDeployPhase

nodeRef :: Text -> Ref
nodeRef = mkRef "node"

{- | A release: a shared build context, two image builds, two pushes each
depending on its own image only (as declared), and a root. Nothing declared
orders push-a after build-b: that is what the barrier adds.
-}
release :: Recorder -> (Text -> [Op] -> Op) -> Op
release rec mkBuildB =
    let ctx = node rec "ctx" []
        buildA = build (node rec "build-a" [ctx])
        buildB = build (mkBuildB "build-b" [ctx])
        pushA = deploy (node rec "push-a" [buildA])
        pushB = deploy (node rec "push-b" [buildB])
     in node rec "root" [pushA, pushB]

folded :: Op -> Dag.Dag Extension
folded = Dag.foldDag Dag.sameRepresentative . evalDeps

-- | What @run up@ computes: everything desired, the barrier last.
computedUp :: Op -> Dag.Dag Extension
computedUp o =
    let dag = folded o
     in Rewrite.computedDag (Rewrite.rewrite [PassPhase.barrier] (Rewrite.wholeGraph dag) dag)

runRecording :: (Recorder -> Op) -> (Dag.Dag Extension -> UpDown.Gate Extension) -> IO (Bool, [Text])
runRecording mk gateOf = do
    logRef <- newIORef []
    let dag = computedUp (mk (\n -> modifyIORef' logRef (n :)))
    ok <- UpDown.upDag (gateOf dag) silent dag
    order <- reverse <$> readIORef logRef
    pure (ok, order)

noop :: Recorder
noop _ = pure ()

before :: Text -> Text -> [Text] -> IO ()
before a b order =
    case (elemIndex a order, elemIndex b order) of
        (Just i, Just j) -> assertBool (show a <> " before " <> show b <> " in " <> show order) (i < j)
        _ -> assertFailure ("both of " <> show (a, b) <> " should have run: " <> show order)

-------------------------------------------------------------------------------

untaggedUnchanged :: IO ()
untaggedUnchanged = do
    let ctx = node noop "ctx" []
        o = node noop "root" [node noop "a" [ctx], node noop "b" [ctx]]
        dag = folded o
    assertEqual "same edges" (Dag.dagEdges dag) (Dag.dagEdges (computedUp o))

barrierEdges :: IO ()
barrierEdges = do
    let dag = computedUp (release noop (node noop))
        depsOf n = Set.fromList (Dag.dependenciesOf dag (nodeRef n))
    assertEqual "push-a waits on both builds" (Set.fromList (map nodeRef ["build-a", "build-b"])) (depsOf "push-a")
    assertEqual "push-b waits on both builds" (Set.fromList (map nodeRef ["build-a", "build-b"])) (depsOf "push-b")
    assertEqual "builds are untouched" (Set.singleton (nodeRef "ctx")) (depsOf "build-a")
    assertBool
        "and the dependants map agrees"
        (nodeRef "push-a" `elem` Dag.dependantsOf dag (nodeRef "build-b"))

buildsFirst :: IO ()
buildsFirst = do
    (ok, order) <- runRecording (\rec -> release rec (node rec)) (const UpDown.alwaysRequired)
    assertBool "the pass succeeds" ok
    sequence_ [before b d order | b <- ["build-a", "build-b"], d <- ["push-a", "push-b"]]

failedBuildBlocksDeploys :: IO ()
failedBuildBlocksDeploys = do
    (ok, order) <- runRecording (\rec -> release rec failing) (const UpDown.alwaysRequired)
    assertBool "the pass fails" (not ok)
    assertBool "the other build still ran" ("build-a" `elem` order)
    assertBool ("no push ran: " <> show order) (not (any (`elem` order) ["push-a", "push-b"]))

onlyDesired :: IO ()
onlyDesired = do
    let o = release noop (node noop)
        dag = folded o
        -- push-b's declaration is staying, build-a's is going away (serve)
        desired = Set.fromList (map nodeRef ["ctx", "build-b", "push-b", "root"])
        computed = Rewrite.computedDag (Rewrite.rewrite [PassPhase.barrier] (Phase desired Set.empty) dag)
    assertEqual
        "push-b does not wait on the build being retracted"
        [nodeRef "build-b"]
        (Dag.dependenciesOf computed (nodeRef "push-b"))
    assertEqual
        "run down (nothing desired) is left as declared"
        (Dag.dagEdges dag)
        (Dag.dagEdges (Rewrite.computedDag (Rewrite.rewrite [PassPhase.barrier] (Phase Set.empty Set.empty) dag)))

buildUnderDeployBlocks :: IO ()
buildUnderDeployBlocks = do
    let mk rec =
            let pushA = deploy (node rec "push-a" [])
                buildB = build (node rec "build-b" [pushA])
             in node rec "root" [buildB]
    (ok, order) <- runRecording mk (const UpDown.alwaysRequired)
    assertBool "the pass fails" (not ok)
    assertBool ("neither runs: " <> show order) (not (any (`elem` order) ["push-a", "build-b"]))

tagNotInRepresentative :: IO ()
tagNotInRepresentative = do
    let plain = node noop "x" []
        tagged = build plain
        actOf o = Dag.representativeOf (folded o) (nodeRef "x")
    assertBool "the tag is there" (any PassPhase.isPassPhase (concatMap (\a -> dynamics a.extension) (actOf tagged)))
    assertEqual
        "but the representative is the same"
        (fmap Dag.representative (actOf plain))
        (fmap Dag.representative (actOf tagged))

selectionClosure :: IO ()
selectionClosure =
    assertEqual
        "the builds and their context"
        (Right (Set.fromList (map nodeRef ["ctx", "build-a", "build-b"])))
        (PassPhase.buildSelection (computedUp (release noop (node noop))))

selectionNoBuild :: IO ()
selectionNoBuild =
    assertEqual
        "refused"
        (Left NoBuildNode)
        (PassPhase.buildSelection (computedUp (deploy (node noop "push" [node noop "ctx" []]))))

selectionDeployUnderBuild :: IO ()
selectionDeployUnderBuild = do
    let pushA = deploy (node noop "push-a" [])
        o = node noop "root" [build (node noop "build-b" [node noop "glue" [pushA]])]
    assertEqual "refused, naming the deploy node" (Left (DeployUnderBuild [nodeRef "push-a"])) (PassPhase.buildSelection (folded o))
    assertBool "and the CLI gate refuses too" (either (const True) (const False) (buildPhaseGate (computedUp o)))

phaseBuildRuns :: IO ()
phaseBuildRuns = do
    let gateOf dag = either (const (error "selection refused")) snd (buildPhaseGate dag)
    (ok, order) <- runRecording (\rec -> release rec (node rec)) gateOf
    assertBool "the pass succeeds" ok
    assertEqual "only the builds and their context ran" (Set.fromList ["ctx", "build-a", "build-b"]) (Set.fromList order)
    (ok', order') <- runRecording (\rec -> release rec failing) gateOf
    assertBool "a failed build fails the build phase" (not ok')
    assertBool "and still deploys nothing" (not (any (`elem` order') ["push-a", "push-b", "root"]))

phaseFlag :: IO ()
phaseFlag = do
    let parse args = case execParserPure defaultPrefs (info runCommandParser mempty) args of
            Success c -> Just c
            _ -> Nothing
        phaseOf (Just (RunUp _ _ _ _ _ p)) = Just p
        phaseOf _ = Nothing
    assertEqual "--phase build" (Just (Just BuildPhase)) (phaseOf (parse ["up", "--phase", "build"]))
    assertEqual "no flag: everything" (Just Nothing) (phaseOf (parse ["up"]))
    assertEqual "--phase deploy is refused" Nothing (parse ["up", "--phase", "deploy"])
    assertEqual "an unknown phase is refused" Nothing (parse ["up", "--phase", "push"])
