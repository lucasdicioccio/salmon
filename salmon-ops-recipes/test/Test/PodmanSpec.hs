-- | Layer 2: real system-service testing, dogfooded through the project's
-- own Podman nodes ("Salmon.Builtin.Nodes.Podman") instead of hand-rolled
-- shell-outs. Container lifecycle itself (bring-up, id recovery, teardown)
-- lives in "Test.Harness"."withContainer" so other Layer 2 tests can reuse
-- it (see "Test.PostgresInitSpec" for a heavier recipe sandboxed this way).
--
-- The Podman builtins currently only expose 'up' (pull/build/run) and have
-- no 'down' — there is nothing to reuse for teardown, so "withContainer"
-- manages cleanup itself via raw @podman rm -f@. That gap is itself a
-- finding: any recipe that provisions containers via these nodes has no
-- accompanying teardown story yet.
module Test.PodmanSpec (tests) where

import Control.Exception (finally)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Control.Monad (void)
import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Salmon.Builtin.Extension (Extension (..), deps, op)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Podman as Podman
import System.Directory (createDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Podman (Layer 2, dogfooded)"
        [ testCase "pullImage + runContainer bring up a real, reachable container" pullAndRun
        , testCase "runContainer: a second run up skips a running container, and a stopped one is replaced" runTwiceThenStopped
        , testCase "buildImageWith builds one stage of a multi-stage file against a context that is not the file's directory" buildContextAndTarget
        , testCase "imageHasId passes the id it was given and blocks its dependant once the tag is rebuilt" imageIdGate
        ]

{- | A Containerfile in @deploy\/@ whose @COPY@ sources are relative to the
directory /above/ it, and which has two stages. Built with the default
options this cannot work (the marker is not in @deploy\/@), so a passing
build is the context being honoured; the image holding the first stage's
file and not the second's is the target being honoured.
-}
buildContextAndTarget :: IO ()
buildContextAndTarget = requireExecutable "podman" $
    withTempDir $ \root -> do
        -- the temp directory's name is already unique to this run
        let tag = Text.pack ("localhost/salmon-test-build-" <> map toLower (takeFileName root) <> ":first")
            containerfile = root </> "deploy" </> "Containerfile"
        createDirectory (root </> "deploy")
        writeFile (root </> "marker.txt") "from the context root\n"
        writeFile containerfile $
            unlines
                [ "FROM docker.io/library/alpine:latest AS first"
                , "COPY marker.txt /first.txt"
                , "FROM docker.io/library/alpine:latest AS second"
                , "COPY marker.txt /second.txt"
                ]
        (reporter, _) <- capture
        let opts = Podman.defaultBuildOptions{Podman.buildContext = Just root, Podman.buildTarget = Just "first"}
            built = Podman.buildImageWith reporter podmanTrack opts (FS.PreExisting containerfile) tag
            imageExists = (\(code, _, _) -> code) <$> readProcessWithExitCode "podman" ["image", "exists", Text.unpack tag] ""
        ( do
            ok <- runUp built
            assertBool "the build succeeds" ok
            (code, out, _) <- readProcessWithExitCode "podman" ["run", "--rm", Text.unpack tag, "ls", "/"] ""
            assertEqual "the image runs" ExitSuccess code
            assertBool "the first stage's file, copied from the context root, is there" ("first.txt" `elem` lines out)
            assertBool "the second stage was not built into this image" ("second.txt" `notElem` lines out)
            )
            `finally` void (runDown built)
        gone <- imageExists
        assertBool "down removed the image" (gone /= ExitSuccess)

pullAndRun :: IO ()
pullAndRun = requireExecutable "podman" $ do
    -- Not a fixed host port: it used to be 18080, and two runs of the suite
    -- on one host (or a container a killed run left up) then made this fail
    -- at `podman run` with "address already in use".
    hostPort <- freeTcpPort
    withContainer (Podman.Image "alpine:latest") (Podman.PortMapping hostPort "80" Podman.TCPPort) $ \cid -> do
        (code, out, _) <- readProcessWithExitCode "podman" ["inspect", "--format", "{{.State.Running}}", cid] ""
        assertBool "podman reports the container as Running" (code == ExitSuccess && "true" `isInfixOf` out)

{- | The acceptance the customer stated: @run up@ twice, the second pass
skips @podman run@ and succeeds; then @podman stop@, and @run up@ brings the
container back (the stopped leftover is removed and run again).
-}
runTwiceThenStopped :: IO ()
runTwiceThenStopped = requireExecutable "podman" $ do
    cname <- freshContainerName
    (reporter, readBack) <- capture
    let reg = Podman.dockerRegistry
        img = Podman.Image "alpine:latest"
        name = Text.unpack (Podman.getContainerName cname)
        pullOp = Podman.pullImage reporter podmanTrack reg img
        runOp = Podman.runContainer reporter podmanTrack reg img cname Podman.noRunOptions
        runs = length . filter isRunStopped <$> readBack
        removals = length . filter isRemoveStopped <$> readBack
        running = do
            (code, out, _) <- readProcessWithExitCode "podman" ["container", "inspect", "--format", "{{.State.Status}}", name] ""
            pure (code == ExitSuccess && "running" `isInfixOf` out)
    ( do
        pulled <- runUp pullOp
        assertBool "the image is pulled" pulled
        first <- runUp runOp
        assertBool "the first run up succeeds" first
        assertEqual "the first run up ran podman run" 1 =<< runs
        assertBool "the container runs" =<< running
        second <- runUp runOp
        assertBool "the second run up succeeds" second
        assertEqual "the second run up did not run podman run" 1 =<< runs
        (stopCode, _, _) <- readProcessWithExitCode "podman" ["stop", "-t", "0", name] ""
        assertEqual "podman stop" ExitSuccess stopCode
        assertBool "the container is stopped" . not =<< running
        third <- runUp runOp
        assertBool "run up after a stop succeeds" third
        assertEqual "the stopped leftover was removed" 1 =<< removals
        assertEqual "and podman run ran again" 2 =<< runs
        assertBool "the container runs again" =<< running
        )
        `finally` void (runDown runOp)
    (code, _, _) <- readProcessWithExitCode "podman" ["container", "exists", name] ""
    assertBool "down removed the container" (code /= ExitSuccess)
  where
    isRunStopped r = case r of
        Podman.RunContainer _ _ _ _ b -> stopped b
        _ -> False
    isRemoveStopped r = case r of
        Podman.RemoveContainer _ b -> stopped b
        _ -> False
    stopped b = case b of
        Binary.CommandStopped{} -> True
        Binary.Requested _ inner -> stopped inner
        _ -> False

{- | The dry-run-then-release use: build a tag, read its id with
'Podman.probeImageId', declare 'Podman.imageHasId' with it under a
stand-in for the push. The gate passes and the dependant runs; after the tag
is rebuilt from other content the same declaration fails and the dependant
does not run. Nothing here reaches a registry.
-}
imageIdGate :: IO ()
imageIdGate = requireExecutable "podman" $
    withTempDir $ \root -> do
        let tag = Text.pack ("localhost/salmon-test-imageid-" <> map toLower (takeFileName root) <> ":1")
            containerfile = root </> "Containerfile"
            writeBuild content =
                writeFile containerfile $
                    unlines ["FROM docker.io/library/alpine:latest", "RUN echo " <> content <> " > /stamp"]
        (reporter, _) <- capture
        let built = Podman.buildImage reporter podmanTrack (FS.PreExisting containerfile) tag
        ( do
            writeBuild "first"
            assertBool "the first build succeeds" =<< runUp built
            st <- Podman.probeImageId tag
            recorded <- case st of
                Podman.ImagePresent i -> pure i
                other -> assertFailure ("no id read for the built tag: " <> show other) >> fail "unreachable"
            ran <- newIORef (0 :: Int)
            let gate = Podman.imageHasId tag recorded
                released = op "test-release" (deps [gate]) $ \actions -> actions{up = modifyIORef ran (+ 1)}
            assertBool "the gate passes on the id it was given" =<< runUp released
            assertEqual "and its dependant ran" 1 =<< readIORef ran
            writeBuild "second"
            assertBool "the rebuild succeeds" =<< runUp built
            moved <- Podman.probeImageId tag
            assertBool "the rebuild gave the tag another id" (moved /= Podman.ImagePresent recorded)
            ok <- runUp released
            assertBool "the gate fails once the tag moved" (not ok)
            assertEqual "and its dependant did not run" 1 =<< readIORef ran
            )
            `finally` void (runDown built)
        gone <- Podman.probeImageId tag
        assertEqual "down removed the image, and the probe says so" Podman.ImageAbsent gone
