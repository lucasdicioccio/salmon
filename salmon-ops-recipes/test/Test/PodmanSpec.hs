-- | Layer 2: real system-service testing, dogfooded through the project's
-- own Podman nodes ("Salmon.Builtin.Nodes.Podman") instead of hand-rolled
-- shell-outs.
--
-- The Podman builtins currently only expose 'up' (pull/build/run) and have
-- no 'down' — there is nothing to reuse for teardown, so this test manages
-- container lifecycle/cleanup itself via 'bracket' + raw @podman rm -f@.
-- That gap is itself a finding: any recipe that provisions containers via
-- these nodes has no accompanying teardown story yet.
--
-- This module is the intended substrate for future Layer 2 tests of
-- heavier recipes (Postgres, systemd, Debian packages): bring up a
-- throwaway container via 'withPodmanContainer', then exercise the recipe
-- against whatever the container exposes, instead of requiring root/apt on
-- the test-running host itself.
module Test.PodmanSpec (tests) where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as C8
import Data.List (find, isInfixOf)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Builtin.Extension (Track', ignoreTrack)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Podman (Layer 2, dogfooded)"
        [ testCase "pullImage + runContainer bring up a real, reachable container" pullAndRun
        ]

podmanTrack :: Track' (Binary.Binary "podman")
podmanTrack = ignoreTrack -- assumes podman is already installed on this host/CI image

pullAndRun :: IO ()
pullAndRun = requireExecutable "podman" $ do
    (reporter, readReports) <- Test.Harness.capture
    let img = Podman.Image "alpine:latest"
        reg = Podman.dockerRegistry
        pm = Podman.PortMapping "18080" "80" Podman.TCPPort

        pullOp = Podman.pullImage reporter podmanTrack reg img
        runOp = Podman.runContainer reporter podmanTrack reg img pm

    -- exercise the ops exactly as production code would: through upTree
    runUp pullOp
    runUp runOp

    reports <- readReports
    let cmdReports = [b | Podman.RunContainer _ _ _ b <- reports]
    assertBool "podman run reported at least one command" (not (null cmdReports))
    assertBool "podman run succeeded" (any Binary.isCommandSuccessful cmdReports)

    containerId <- case find isCommandStopped (map unwrapRequested cmdReports) of
        Just (Binary.CommandStopped _ ExitSuccess out _) -> pure (C8.unpack (C8.takeWhile (/= '\n') out))
        _ -> error "could not recover the container id from podman run's stdout"

    bracket (pure containerId) cleanupContainer $ \cid -> do
        (code, out, _) <- readProcessWithExitCode "podman" ["inspect", "--format", "{{.State.Running}}", cid] ""
        assertBool "podman reports the container as Running" (code == ExitSuccess && "true" `isInfixOf` out)
  where
    -- Binary.Report tags every event with `Requested (Just act)`, so a bare
    -- CommandStopped pattern never matches directly; unwrap it first, same
    -- as `Binary.isCommandSuccessful` does internally.
    unwrapRequested (Binary.Requested _ r) = unwrapRequested r
    unwrapRequested r = r

    isCommandStopped (Binary.CommandStopped _ _ _ _) = True
    isCommandStopped _ = False

    cleanupContainer cid = do
        _ <- readProcessWithExitCode "podman" ["rm", "-f", cid] ""
        pure ()
