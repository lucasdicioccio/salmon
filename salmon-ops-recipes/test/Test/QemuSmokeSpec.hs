{- | Layer 3 smoke test: boots a real qemu VM via 'Test.Harness.withVm' and
proves SSH into it actually works end to end (bridge/tap up, kernel boots,
9p root mounts, network configures, sshd answers) — see
@specs/qemu-test-vms-progress.md@ for the design/validation history this
closes out (step 4 of its "exact next steps").

Needs root (bridge/tap + a systemd unit, matching this whole VM tier's
documented privilege assumption — see "Salmon.Builtin.Nodes.Qemu"'s
haddock) and a pre-built VM rootfs at 'smokeRootfs', with
'Salmon.Builtin.Nodes.Debian.Debootstrap.vmEssentials' and
'Salmon.Builtin.Nodes.Debian.Debootstrap.ensureVm9pBoot' already applied
(this test does not run debootstrap itself, same stance as
'Test.Harness.withVm') — no SSH key pre-provisioning needed, 'withVm'
generates and trusts its own per-boot CA-signed key:

> sudo debootstrap --include=linux-image-amd64,openssh-server stable /var/lib/salmon-test-vms/smoke/root

Both preconditions are checked and skipped loudly (not failed) if unmet,
same "skip/fail loudly, don't hang" spirit as 'Test.Harness.requireExecutable'.
-}
module Test.QemuSmokeSpec (tests) where

import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getEffectiveUserID)
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Qemu (Layer 3, real VM boot via withVm)"
        [testCase "boots the smoke rootfs and answers SSH" bootsAndAnswersSsh]

smokeRootfs :: FilePath
smokeRootfs = "/var/lib/salmon-test-vms/smoke/root"

bootsAndAnswersSsh :: IO ()
bootsAndAnswersSsh = do
    isRoot <- (== 0) <$> getEffectiveUserID
    hasQemu <- (/= Nothing) <$> findExecutable "qemu-system-x86_64"
    hasRootfs <- doesFileExist (smokeRootfs <> "/etc/issue")
    if not isRoot
        then skip "needs root (bridge/tap + systemd unit)"
        else
            if not hasQemu
                then skip "qemu-system-x86_64 not found on PATH"
                else
                    if not hasRootfs
                        then skip ("no VM rootfs at " <> smokeRootfs <> " (run debootstrap by hand first, see this module's haddock)")
                        else
                            withVm smokeRootfs $ \access -> do
                                (code, out, _err) <- sshToVm access ["echo smoke-ok"]
                                assertBool ("ssh into VM failed: " <> show code <> " / " <> out) (code == ExitSuccess)
                                assertBool ("unexpected ssh output: " <> out) ("smoke-ok" `elem` lines out)
  where
    skip msg = hPutStrLn stderr ("SKIPPED: " <> msg)
