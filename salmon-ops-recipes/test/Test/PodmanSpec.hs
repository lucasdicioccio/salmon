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
import Control.Monad (void)
import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Podman as Podman
import System.Directory (createDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Podman (Layer 2, dogfooded)"
        [ testCase "pullImage + runContainer bring up a real, reachable container" pullAndRun
        , testCase "buildImageWith builds one stage of a multi-stage file against a context that is not the file's directory" buildContextAndTarget
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
pullAndRun = requireExecutable "podman" $
    withContainer (Podman.Image "alpine:latest") (Podman.PortMapping "18080" "80" Podman.TCPPort) $ \cid -> do
        (code, out, _) <- readProcessWithExitCode "podman" ["inspect", "--format", "{{.State.Running}}", cid] ""
        assertBool "podman reports the container as Running" (code == ExitSuccess && "true" `isInfixOf` out)
