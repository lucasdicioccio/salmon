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
--   * Layer 3 (whole-machine): real IO against a qemu VM booted from a
--     caller-prepared "Salmon.Builtin.Nodes.Debian.Debootstrap" rootfs,
--     dogfooding "Salmon.Builtin.Nodes.LinuxBridge"\/"Salmon.Builtin.Nodes.Qemu"
--     as the sandbox provisioner, for recipes Layer 2's containers can't
--     exercise well (real systemd-as-PID-1, real network interfaces). See
--     @specs/qemu-test-vms.md@ for the design.
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

    -- * qemu-backed sandboxes (Layer 3)
    testBridge,
    testBridgeCidr,
    testVmAddr,
    ensureTestBridge,
    withVm,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import Control.Monad (unless, void)
import Control.Monad.Identity (Identity, runIdentity)
import Data.IORef
import qualified Data.Text as Text
import Numeric (showHex)
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.UpDown (downTree, upTree)
import Salmon.Builtin.Extension (Extension (..), Op, Track', ignoreTrack)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.LinuxBridge as LinuxBridge
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Qemu as Qemu
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
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
    _ <- upTree r nat o
    readBack

{- | Run 'upTree' when you only care about the side effects, not the trace.
Returns whether everything actually succeeded (see 'UpDown.upTree') — most
callers that don't check it explicitly still get a real postcondition
assertion elsewhere in the test, but the result is there for callers that
want to assert on it directly instead.
-}
runUp :: Op -> IO Bool
runUp o = do
    (r, _) <- capture
    upTree r nat o

runDown :: Op -> IO Bool
runDown o = do
    (r, _) <- capture
    downTree r nat o

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
    cleanup (_, runOp) = void (runDown runOp)

    bringUp :: IO (String, Op)
    bringUp = do
        cname <- freshContainerName
        (reporter, _) <- capture
        let reg = Podman.dockerRegistry
            opts = Podman.noRunOptions{Podman.runPorts = [pm]}
            pullOp = Podman.pullImage reporter podmanTrack reg img
            runOp = Podman.runContainer reporter podmanTrack reg img cname opts
        pullOk <- runUp pullOp
        unless pullOk (fail "withContainer: pulling the sandbox image failed")
        runOk <- runUp runOp
        unless runOk (fail "withContainer: starting the sandbox container failed")
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

-------------------------------------------------------------------------------
-- qemu-backed sandboxes (Layer 3).
--
-- Mirrors the podman section above in spirit: dogfoods
-- "Salmon.Builtin.Nodes.LinuxBridge"'s and "Salmon.Builtin.Nodes.Qemu"'s own
-- up\/down through 'runUp'\/'runDown' as the sandbox provisioner, real IO, no
-- mocking. Unlike podman, this needs real host privilege (@CAP_NET_ADMIN@
-- for the bridge\/tap devices, plus whatever qemu itself needs) that is
-- assumed already available to whoever runs this tier — a documented
-- prerequisite, same stance @specs/qemu-test-vms.md@'s privilege open
-- question leans towards, rather than this harness trying to sudo on its
-- own behalf.
--
-- Caveat carried over from the spec: the guest-networking scheme here
-- (static IP via the kernel @ip=@ cmdline parameter, assumed @eth0@ naming)
-- is a first cut, not yet checked against a real boot — @specs/qemu-test-vms.md@'s
-- phased plan puts "hand-validate a boot" before wrapping things in a node,
-- and that hand-validation hasn't happened yet. Expect to revisit the exact
-- cmdline\/interface-naming details here once a real VM has actually booted.

ipTrack :: Track' (Binary.Binary "ip")
ipTrack = ignoreTrack

qemuBinTrack :: Track' (Binary.Binary "qemu-system-x86_64")
qemuBinTrack = ignoreTrack

systemctlTrack :: Track' (Binary.Binary "systemctl")
systemctlTrack = ignoreTrack

-- | One shared bridge, left standing across test runs rather than torn down
-- per test — matches @specs/qemu-test-vms.md@'s leaning on bridge lifecycle
-- scope. Only each VM's own tap is created\/destroyed per test.
testBridge :: LinuxBridge.Bridge
testBridge = LinuxBridge.Bridge "salmontest0"

testBridgeCidr :: LinuxBridge.Cidr
testBridgeCidr = LinuxBridge.Cidr "10.99.0.1" 24

{- | Fixed guest address — v1 assumes a single VM under test at a time (see
@specs/qemu-test-vms.md@'s phased plan: proving the tier end to end comes
before anything like a real address pool).
-}
testVmAddr :: Text.Text
testVmAddr = "10.99.0.2"

-- | Ensures the shared test bridge (and its address) exist. Idempotent via
-- the production 'LinuxBridge.bridgeAddr' op's own @prelim@ — safe to call
-- before every test.
ensureTestBridge :: IO ()
ensureTestBridge = do
    (reporter, _) <- capture
    ok <- runUp (LinuxBridge.bridgeAddr reporter ipTrack testBridge testBridgeCidr)
    unless ok (fail "ensureTestBridge: failed to bring up the shared test bridge")

-- | CPU time at picosecond resolution, truncated to fit Linux's 15-character
-- interface name limit — same entropy source as 'freshContainerName' above,
-- just shorter (an interface name, unlike a container name, can't be long).
freshTapName :: IO LinuxBridge.DevName
freshTapName = do
    t <- getCPUTime
    pure (Text.pack ("vmtap" <> take 6 (reverse (show t))))

-- | A locally-administered MAC in qemu's own default OUI (@52:54:00@), with
-- a CPU-time-derived low byte for uniqueness across concurrent\/successive VMs.
freshMac :: IO Text.Text
freshMac = do
    t <- getCPUTime
    let byte = fromInteger (t `mod` 256) :: Int
        hex = showHex byte ""
    pure (Text.pack ("52:54:00:12:34:" <> (if length hex < 2 then '0' : hex else hex)))

{- | Boots a VM from an already-prepared 'Salmon.Builtin.Nodes.Debian.Debootstrap.RootTree'
directory (built and populated by the caller — this harness does not run
debootstrap itself, see @specs/qemu-test-vms.md@) — waits for SSH to answer,
runs the action, and guarantees teardown afterwards via the production
'Qemu.setup' down action, however the action exits (including on
exception), same bracket-based shape as 'withContainer'.

@rootfs@ must already have @root\/.ssh\/authorized_keys@ populated with a
key the caller holds the private half of (this harness only ever connects
as @root@ over key-based SSH; it does not generate or transport any secret,
matching this project's key-exchange-agnostic recipe convention).
-}
withVm :: FilePath -> (Ssh.Remote -> IO a) -> IO a
withVm rootfs act =
    withSystemTempDirectory "salmon-ops-recipes-test-vm" $ \tmpdir ->
        bracket (bringUp tmpdir) cleanup (act . fst)
  where
    cleanup :: (Ssh.Remote, Op) -> IO ()
    cleanup (_, vmOp) = void (runDown vmOp)

    bringUp :: FilePath -> IO (Ssh.Remote, Op)
    bringUp tmpdir = do
        ensureTestBridge
        tapName <- freshTapName
        mac <- freshMac
        (kernel, initrd) <- Qemu.resolveKernelInitrd rootfs
        (reporter, _) <- capture
        (reporterTap, _) <- capture
        let cfg =
                Qemu.VmConfig
                    { Qemu.vm_name = Text.pack ("salmon-test-vm-" <> takeWhile (/= '/') (reverse tmpdir))
                    , Qemu.vm_memory_mb = 512
                    , Qemu.vm_smp = 1
                    , Qemu.vm_rootfs = rootfs
                    , Qemu.vm_kernel = kernel
                    , Qemu.vm_initrd = initrd
                    , Qemu.vm_extra_kernel_args =
                        [ "ip=" <> testVmAddr <> "::" <> testBridgeCidr.cidrAddr <> ":255.255.255.0::eth0:off"
                        ]
                    , Qemu.vm_tap = LinuxBridge.Tap tapName testBridge Nothing
                    , Qemu.vm_mac = mac
                    , Qemu.vm_monitor_socket = tmpdir </> "monitor.sock"
                    , Qemu.vm_enable_kvm = False
                    , Qemu.vm_user = "root"
                    , Qemu.vm_group = "root"
                    , Qemu.vm_working_dir = tmpdir
                    }
            vmOp = Qemu.setup reporter reporterTap systemctlTrack qemuBinTrack ipTrack cfg
        ok <- runUp vmOp
        unless ok (fail "withVm: starting the sandbox VM failed")
        let remote = Ssh.Remote "root" testVmAddr
        waitForSsh remote
        pure (remote, vmOp)

-- | Polls SSH every two seconds (a VM takes real seconds to boot, unlike a
-- podman container being "up") for up to two minutes, then fails loudly
-- rather than hanging the test suite indefinitely — same "skip\/fail loudly,
-- don't hang" spirit as 'requireExecutable'.
waitForSsh :: Ssh.Remote -> IO ()
waitForSsh remote = go (60 :: Int)
  where
    go 0 = fail ("withVm: " <> show remote <> " never answered SSH within the timeout")
    go n = do
        (code, _, _) <-
            readProcessWithExitCode
                "ssh"
                [ "-o"
                , "BatchMode=yes"
                , "-o"
                , "StrictHostKeyChecking=no"
                , "-o"
                , "UserKnownHostsFile=/dev/null"
                , "-o"
                , "ConnectTimeout=2"
                , Text.unpack (Ssh.remoteUser remote) <> "@" <> Text.unpack (Ssh.remoteHost remote)
                , "true"
                ]
                ""
        case code of
            ExitSuccess -> pure ()
            _ -> threadDelay 2000000 >> go (n - 1)
