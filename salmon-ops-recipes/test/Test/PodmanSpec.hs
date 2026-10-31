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

import Control.Concurrent (threadDelay)
import Control.Exception (finally, try)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Control.Monad (void)
import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Salmon.Builtin.Extension (Extension (..), Op, deps, op, opAct)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Op.Actions (Act (..))
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
        , testCase "pushImageId pushes the id it was given to a local registry, whatever the tag became, and checks the digest" pushImageIdToRegistry
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

{- | 'Podman.pushImageId' against a real registry: a @registry:2@ container
on a loopback port, plain HTTP (so @pushTlsVerify = False@). Build a tag,
read its id, push the id; rebuild the tag from other content and push the
/old/ id again with the digest the first push reported as expected: the
registry still serves the first image, which is the point (the tag moving
does not change what an id push ships). A push expecting another digest
fails with 'Podman.PushedDigestMismatch', and an id no longer in the local
store is not pushed. Nothing reaches a registry other than the container.
-}
pushImageIdToRegistry :: IO ()
pushImageIdToRegistry = requireExecutable "podman" $
    withTempDir $ \root -> do
        port <- freeTcpPort
        recorded <- newIORef Nothing
        let suffix = map toLower (takeFileName root)
            tag = Text.pack ("localhost/salmon-test-pushid-" <> suffix <> ":1")
            dest = "localhost:" <> port <> "/salmon-test/pushid-" <> Text.pack suffix <> ":1"
            containerfile = root </> "Containerfile"
            writeBuild content =
                writeFile containerfile $
                    unlines ["FROM docker.io/library/alpine:latest", "RUN echo " <> content <> " > /stamp"]
            podman_ args = void (readProcessWithExitCode "podman" args "")
        withContainer (Podman.Image "registry:2") (Podman.PortMapping ("127.0.0.1:" <> port) "5000" Podman.TCPPort) $ \cid -> do
            awaitRegistry cid (40 :: Int)
            (reporter, readBack) <- capture
            let built = Podman.buildImage reporter podmanTrack (FS.PreExisting containerfile) tag
                opts = Podman.defaultPushOptions{Podman.pushTlsVerify = False}
                pushed = do
                    reports <- readBack
                    pure [d | Podman.PushedImageDigest _ d <- reports]
                -- What the registry serves under the destination, as a local
                -- id. The pulled name is taken off with `untag`, not `rmi`:
                -- once the build tag has moved, that name is the only one the
                -- first image has, and `rmi` would delete the image itself.
                served = do
                    (code, _, err) <- readProcessWithExitCode "podman" ["pull", "-q", "--tls-verify=false", Text.unpack dest] ""
                    assertEqual ("pulling the destination back: " <> err) ExitSuccess code
                    st <- Podman.probeImageId dest
                    podman_ ["untag", Text.unpack dest, Text.unpack dest]
                    pure st
            ( do
                writeBuild "first"
                assertBool "the first build succeeds" =<< runUp built
                first <- Podman.probeImageId tag
                firstId <- case first of
                    Podman.ImagePresent i -> writeIORef recorded (Just i) >> pure i
                    other -> assertFailure ("no id read for the built tag: " <> show other) >> fail "unreachable"
                ok <- runUp (Podman.pushImageId reporter podmanTrack opts firstId dest)
                failures <- podmanFailures <$> readBack
                assertBool ("the push by id succeeds" <> concatMap ("\n  " <>) failures) ok
                digest <-
                    pushed >>= \ds -> case ds of
                        [d] -> pure d
                        other -> assertFailure ("one digest reported, not " <> show other) >> fail "unreachable"
                assertEqual "the registry serves the image pushed" (Podman.ImagePresent firstId) =<< served

                writeBuild "second"
                assertBool "the rebuild succeeds" =<< runUp built
                moved <- Podman.probeImageId tag
                assertBool "the rebuild gave the tag another id" (moved /= Podman.ImagePresent firstId)
                again <- runUp (Podman.pushImageId reporter podmanTrack opts{Podman.pushExpectedDigest = Just digest} firstId dest)
                assertBool "the old id, pushed again expecting the first digest, succeeds" again
                assertEqual "the registry still serves the first image, not the tag's new one" (Podman.ImagePresent firstId) =<< served

                let wrong = either (error . Text.unpack) id (Podman.mkManifestDigest ("sha256:" <> Text.replicate 64 "0"))
                refused <- try (upOf (Podman.pushImageId reporter podmanTrack opts{Podman.pushExpectedDigest = Just wrong} firstId dest))
                assertEqual "a push expecting another digest fails, naming both" (Left (Podman.PushedDigestMismatch dest wrong digest)) refused
                assertEqual "only the pushes that passed reported their digest, the same one" [digest, digest] =<< pushed

                podman_ ["rmi", "-f", Text.unpack (Podman.getImageId firstId)]
                gone <- runUp (Podman.pushImageId reporter podmanTrack opts firstId dest)
                assertBool "an id no longer in the local store is not pushed" (not gone)
                )
                `finally` do
                    void (runDown built)
                    readIORef recorded >>= mapM_ (\i -> podman_ ["rmi", "-f", "--ignore", Text.unpack (Podman.getImageId i)])
        gone <- Podman.probeImageId tag
        assertEqual "down removed the image" Podman.ImageAbsent gone
  where
    upOf :: Op -> IO ()
    upOf o = maybe (fail "not a node") (\act -> act.extension.up) (opAct o)
    awaitRegistry cid n = do
        (code, _, _) <- podmanExecCapture cid ["wget", "-q", "-O", "/dev/null", "http://127.0.0.1:5000/v2/"]
        case code of
            ExitSuccess -> pure ()
            _
                | n <= 0 -> assertFailure "the registry container never answered on /v2/"
                | otherwise -> threadDelay 250000 >> awaitRegistry cid (n - 1)
