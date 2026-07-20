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
) where

import Control.Monad.Identity (Identity, runIdentity)
import Data.IORef
import Salmon.Actions.UpDown (downTree, upTree)
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..), Op)
import Salmon.Reporter (Reporter, ReporterM (..))
import System.Directory (findExecutable)
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)

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
