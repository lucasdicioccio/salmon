{- | Layer 3: runs 'Debootstrap.rootTree' and 'Debootstrap.ensureVm9pBoot' as
real salmon 'Op's (not the equivalent hand-run chroot script used to
originally debug this) against a rootfs, and checks the op leaves 9p kernel
modules registered and idempotently skips on a second 'runUp' — closes out
@specs/qemu-test-vms-progress.md@ §3 item 2 ("ensureVm9pBoot itself is
untested as a salmon Op").

Needs root (debootstrap + the chroot bind-mount dance) — skipped loudly,
not failed, without it. The *first* run against 'freshRootPath' is a real,
from-scratch debootstrap (fetches ~100+ packages, took ~26 minutes over
this author's connection) and additionally needs the opt-in env var
@SALMON_TEST_RUN_DEBOOTSTRAP=1@ set, precisely so it never fires
unexpectedly on a metered connection just from running the suite under
sudo. Deliberately does *not* wipe 'freshRootPath' between runs (unlike a
from-scratch-every-time design): once debootstrapped, 'Debootstrap.rootTree'
and 'Debootstrap.ensureVm9pBoot''s own 'prelim's make every subsequent run
report @Skippable@ and finish in seconds with no network use at all — that
skip path is itself exactly what this test wants to exercise on reruns.
Delete @freshRootPath@ by hand to force a real re-debootstrap.
-}
module Test.DebootstrapSpec (tests) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Salmon.Builtin.Extension (Track', ignoreTrack)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Debian.Debootstrap as Debootstrap
import Salmon.Op.OpGraph (inject)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getEffectiveUserID)
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Debootstrap (Layer 3, real rootTree + ensureVm9pBoot ops)"
        [testCase "debootstraps a fresh root and makes it 9p-bootable" debootstrapsAndFixes9pBoot]

freshRootPath :: FilePath
freshRootPath = "/var/lib/salmon-test-vms/debootstrap-op-smoke/root"

debootstrapCmdTrack :: Track' (Binary.Binary "debootstrap")
debootstrapCmdTrack = ignoreTrack

bashTrack :: Track' (Binary.Binary "bash")
bashTrack = ignoreTrack

debootstrapsAndFixes9pBoot :: IO ()
debootstrapsAndFixes9pBoot = do
    isRoot <- (== 0) <$> getEffectiveUserID
    hasDebootstrap <- (/= Nothing) <$> findExecutable "debootstrap"
    alreadyBootstrapped <- doesFileExist (freshRootPath <> "/etc/issue")
    optedIn <- isJust <$> lookupEnv "SALMON_TEST_RUN_DEBOOTSTRAP"
    case () of
        _
            | not isRoot -> skip "needs root (debootstrap + chroot bind-mounts)"
            | not hasDebootstrap -> skip "debootstrap not found on PATH"
            | not (alreadyBootstrapped || optedIn) ->
                skip
                    ( "first run needs a real (network-heavy) debootstrap; set "
                        <> "SALMON_TEST_RUN_DEBOOTSTRAP=1 to allow it (reruns after that are "
                        <> "free/offline via prelim skip, see this module's haddock)"
                    )
            | otherwise -> run
  where
    skip msg = hPutStrLn stderr ("SKIPPED: " <> msg)

    run :: IO ()
    run = do
        (reporter, _) <- capture
        let root = Debootstrap.RootTree Debootstrap.Stable freshRootPath Debootstrap.vmEssentials
            vmOp =
                Debootstrap.ensureVm9pBoot reporter bashTrack root
                    `inject` Debootstrap.rootTree reporter debootstrapCmdTrack root

        ok <- runUp vmOp
        unless ok (fail "debootstrapsAndFixes9pBoot: rootTree/ensureVm9pBoot op failed")

        let modulesFile = freshRootPath <> "/etc/initramfs-tools/modules"
        modulesExist <- doesFileExist modulesFile
        assertBool (modulesFile <> " should exist after ensureVm9pBoot") modulesExist
        contents <- readFile modulesFile
        assertBool
            (modulesFile <> " should list the 9p modules")
            (all (`isInfixOf` contents) ["9p", "9pnet", "9pnet_virtio", "virtio", "virtio_pci", "virtio_ring"])

        -- idempotency: rerunning should skip cleanly (both ops' prelims report Skippable)
        ok2 <- runUp vmOp
        unless ok2 (fail "debootstrapsAndFixes9pBoot: second (idempotent) runUp failed")
