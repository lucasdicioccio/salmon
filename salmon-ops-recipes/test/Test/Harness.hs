-- | Generic plumbing to run 'Op' graphs for real (no mocking) and observe
-- what happened, on top of the existing 'Salmon.Actions.UpDown' machinery.
--
-- The rest of the test suite is organized in tiers by IO cost/blast-radius:
--
--   * Layer 0 (structural): 'evalDeps' on an 'Op', no side effects at all.
--   * Layer 1 (sandboxed IO): real 'up'\/'down' against a throwaway temp dir
--     or ephemeral resource, via 'runUpCapturing'\/'runDown'.
--   * Layer 2 (system services): real IO against a service that only exists
--     inside a disposable podman container, dogfooding "Podman.pullImage"\/
--     "Podman.runContainer" as the sandbox provisioner (see "Test.PodmanSpec").
module Test.Harness (
    -- * capturing UpDown traversal reports
    capture,
    runUpCapturing,
    runUp,
    runDown,

    -- * scratch filesystem
    withTempDir,

    -- * skipping tests when a precondition isn't met
    requireExecutable,

    -- * podman-backed sandboxes (Layer 2)
    podmanTrack,
    withContainer,
    podmanExec_,
    podmanExecCapture,

    -- * redirecting a recipe's system binaries into a container via PATH shims
    withShimmedPath,
) where

import Control.Exception (bracket, bracket_)
import Control.Monad.Identity (Identity, runIdentity)
import Data.IORef
import qualified Data.Text as Text
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.UpDown (downTree, upTree)
import Salmon.Builtin.Extension (Extension (..), Op, Track', ignoreTrack)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Reporter (Reporter, ReporterM (..))
import System.CPUTime (getCPUTime)
import System.Directory (findExecutable, getPermissions, setOwnerExecutable, setPermissions)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

-- | Build a reporter that accumulates every emitted value, in order, plus a
-- way to read them back out. Good enough for single-threaded test runs.
capture :: IO (Reporter a, IO [a])
capture = do
    ref <- newIORef []
    let r = ReporterM $ \x -> atomicModifyIORef' ref (\xs -> (x : xs, ()))
    pure (r, reverse <$> readIORef ref)

-- | 'Op'-graph traversal is 'Identity'-effectful in this codebase; both
-- entry points below hardcode that natural transformation.
nat :: Identity a -> IO a
nat = pure . runIdentity

-- | Run 'upTree' and return the full traversal trace (Eval\/Skip\/Redundant
-- per node), so idempotency\/dedup can be asserted on directly instead of
-- only inferring it from side effects.
runUpCapturing :: Op -> IO [UpDown.Report Extension]
runUpCapturing o = do
    (r, readBack) <- capture
    upTree r nat o
    readBack

-- | Run 'upTree' when you only care about the side effects, not the trace.
runUp :: Op -> IO ()
runUp o = do
    (r, _) <- capture
    upTree r nat o

runDown :: Op -> IO ()
runDown = downTree nat

-- | A fresh, auto-cleaned-up temp directory for filesystem-touching nodes.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "salmon-ops-recipes-test"

-- | Layer-2 tests need a real binary on PATH (podman, postgres, ...). Rather
-- than failing the suite on a machine that doesn't have it, skip loudly:
-- print a note and report the test as passing-vacuously.
requireExecutable :: String -> IO () -> IO ()
requireExecutable name act = do
    found <- findExecutable name
    case found of
        Just _ -> act
        Nothing ->
            hPutStrLn stderr $
                "SKIPPED: `" <> name <> "` not found on PATH; this Layer 2 test needs it installed to run for real"

-------------------------------------------------------------------------------
-- Podman-backed sandboxes.
--
-- Dogfoods "Podman.pullImage"\/"Podman.runContainer"\/"Podman.runContainer"'s
-- 'down' (i.e. runs them for real through 'runUp'\/'runDown', exactly like
-- production code would) as the sandbox provisioner and cleaner-upper —
-- there is no hand-rolled @podman rm -f@ shell-out here at all, since the
-- Podman nodes now carry a real teardown of their own. We pick the
-- container's name ourselves (so it's known up front and 'down' has a
-- stable target), rather than needing to recover an id from `podman run`'s
-- stdout.

-- | Assumes podman is already installed on the host\/CI image running the
-- test (checked by 'requireExecutable' at the call site).
podmanTrack :: Track' (Binary.Binary "podman")
podmanTrack = ignoreTrack

-- | Pull an image and run it under a fresh, unique name (dogfooding the
-- project's own Podman ops both ways), and guarantee cleanup via the
-- production 'down' action afterwards, however the action exits (including
-- on exception). The pulled image itself is left in the local cache — only
-- the container is torn down — since removing shared image cache on every
-- test run would be needlessly destructive and slow subsequent runs down.
withContainer :: Podman.Image -> Podman.PortMapping -> (String -> IO a) -> IO a
withContainer img pm act =
    bracket bringUp cleanup (act . fst)
  where
    cleanup :: (String, Op) -> IO ()
    cleanup (_, runOp) = runDown runOp

    bringUp :: IO (String, Op)
    bringUp = do
        cname <- freshContainerName
        (reporter, _) <- capture
        let reg = Podman.dockerRegistry
            pullOp = Podman.pullImage reporter podmanTrack reg img
            runOp = Podman.runContainer reporter podmanTrack reg img cname pm
        runUp pullOp
        runUp runOp
        pure (Text.unpack (Podman.getContainerName cname), runOp)

-- | CPU time at picosecond resolution is more than enough entropy to keep
-- concurrent\/successive test containers from colliding on a name.
freshContainerName :: IO Podman.ContainerName
freshContainerName = do
    t <- getCPUTime
    pure (Podman.ContainerName (Text.pack ("salmon-ops-recipes-test-" <> show t)))

-- | Run a command inside an already-running container, discarding its output.
-- Used for sandbox setup steps (installing prerequisites) that are not
-- themselves the thing under test.
podmanExec_ :: String -> [String] -> IO ()
podmanExec_ containerId args = do
    (code, out, err) <- readProcessWithExitCode "podman" (["exec", "-i", containerId] <> args) ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure n ->
            error $
                "podmanExec_ " <> show args <> " failed with exit " <> show n <> "\nstdout: " <> out <> "\nstderr: " <> err

-- | Like 'podmanExec_', but for postcondition checks: hands back the full
-- (exit code, stdout, stderr) instead of throwing on failure.
podmanExecCapture :: String -> [String] -> IO (ExitCode, String, String)
podmanExecCapture containerId args =
    readProcessWithExitCode "podman" (["exec", "-i", containerId] <> args) ""

-------------------------------------------------------------------------------
-- Redirecting a recipe's real system binaries (apt-get, sudo, pg_ctlcluster,
-- ...) into a podman container.
--
-- Recipes call these binaries directly by name via 'System.Process.proc',
-- with no indirection to hook into — so the only way to run their *real*
-- logic against a sandbox instead of the host is to put lookalike wrapper
-- scripts earlier on PATH that forward the invocation into the container via
-- @podman exec@. This tests the recipe's actual command construction and
-- graph wiring for real, unmodified, while keeping the destructive parts
-- (apt installs, service starts) confined to the disposable container.

-- | Create shims for the given command names that all forward into
-- @containerId@, prepend them to PATH for the duration of the action, and
-- restore the original PATH afterwards.
withShimmedPath :: String -> [String] -> IO a -> IO a
withShimmedPath containerId commands act =
    withSystemTempDirectory "salmon-ops-recipes-test-shims" $ \dir -> do
        mapM_ (writeShim dir) commands
        withPrependedPath dir act
  where
    writeShim :: FilePath -> String -> IO ()
    writeShim dir cmd = do
        let path = dir </> cmd
        writeFile path $
            unlines
                [ -- Absolute shebang on purpose: `#!/usr/bin/env bash` would
                  -- have `env` resolve "bash" via the (now shim-prepended)
                  -- PATH, which — if "bash" is itself one of the shimmed
                  -- commands — finds this very script and recurses into
                  -- itself forever instead of running real bash.
                  "#!/bin/bash"
                , "exec podman exec -i " <> containerId <> " " <> cmd <> " \"$@\""
                ]
        perms <- getPermissions path
        setPermissions path (setOwnerExecutable True perms)

withPrependedPath :: FilePath -> IO a -> IO a
withPrependedPath dir act = do
    original <- lookupEnv "PATH"
    bracket_
        (setEnv "PATH" (dir <> maybe "" (":" <>) original))
        (maybe (unsetEnv "PATH") (setEnv "PATH") original)
        act
