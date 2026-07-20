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

import Data.List (isInfixOf)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Harness
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Podman (Layer 2, dogfooded)"
        [ testCase "pullImage + runContainer bring up a real, reachable container" pullAndRun
        ]

pullAndRun :: IO ()
pullAndRun = requireExecutable "podman" $
    withContainer (Podman.Image "alpine:latest") (Podman.PortMapping "18080" "80" Podman.TCPPort) $ \cid -> do
        (code, out, _) <- readProcessWithExitCode "podman" ["inspect", "--format", "{{.State.Running}}", cid] ""
        assertBool "podman reports the container as Running" (code == ExitSuccess && "true" `isInfixOf` out)
