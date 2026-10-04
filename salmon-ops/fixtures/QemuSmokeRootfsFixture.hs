{-# LANGUAGE DataKinds #-}

{- | The smoke rootfs of the Layer 3 qemu test tier, built by ops.

"Test.QemuSmokeSpec" boots @\/var\/lib\/salmon-test-vms\/smoke\/root@. That
tree was first made by a hand-typed @debootstrap@ and then fixed by hand in
a chroot so that its initrd could mount a 9p root (see
@specs\/qemu-test-vms-progress.md@ §2). This is the same tree as three
nodes, the shape of @salmon-toy-qemu-pg-ha prereqs@ and of
@salmon-patroni-rootfs@:

  * 'Debootstrap.rootTree' with 'Debootstrap.vmEssentials' (a kernel and
    sshd, nothing else: the smoke test only asks that the guest answers ssh);
  * 'Debootstrap.ensureVm9pBoot', the node the hand-applied fix became;
  * the @etc\/ssh@ subtree handed to the unprivileged user who runs the
    tests, since "Test.Harness".'Test.Harness.ensureVmSshAccess' writes a
    per-boot CA and an sshd drop-in there, on the host, before qemu starts.
    Recursive, as @salmon-qemu-host-setup-fixture@ does it: the drop-in goes
    into @sshd_config.d@, which debootstrap leaves to root.

It needs root (debootstrap, and the bind mounts around @update-initramfs@):

> sudo $(cabal list-bin salmon-qemu-smoke-rootfs-fixture) "$USER"
> sudo $(cabal list-bin salmon-qemu-smoke-rootfs-fixture) "$USER" /some/other/root

Idempotent, and that cuts both ways: both Debootstrap nodes have a 'check'
(@etc\/issue@ exists; the modules file names @9pnet_virtio@), so a tree
that is already there is left as it is, however it was made. To /rebuild/
the smoke rootfs, move the old tree aside first. The binary says so on
stderr when it finds one.
-}
module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.Identity (runIdentity)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Salmon.Actions.UpDown (upTree)
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Debian.Debootstrap as Debootstrap
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.User as User
import Salmon.Op.OpGraph (inject)
import Salmon.Reporter (reportPrint)

-- | Where "Test.QemuSmokeSpec" looks for it.
defaultSmokeRootfs :: FilePath
defaultSmokeRootfs = "/var/lib/salmon-test-vms/smoke/root"

-- | The tree, able to boot over 9p, with @etc\/ssh@ handed over.
smokeRootfs :: User.Owner -> FilePath -> Op
smokeRootfs owner rootfs =
    handOver `inject` bootable
  where
    tree = Debootstrap.RootTree Debootstrap.Stable rootfs Debootstrap.vmEssentials
    bootable =
        Debootstrap.ensureVm9pBoot reportPrint bashTrack tree
            `inject` Debootstrap.rootTree reportPrint debootstrapTrack tree
    handOver = User.chown reportPrint Debian.chown True owner (rootfs </> "etc/ssh")

bashTrack :: Track' (Binary.Binary "bash")
bashTrack = ignoreTrack

debootstrapTrack :: Track' (Binary.Binary "debootstrap")
debootstrapTrack = ignoreTrack

main :: IO ()
main = do
    args <- getArgs
    case args of
        [user] -> build user defaultSmokeRootfs
        [user, rootfs] -> build user rootfs
        _ -> die "usage: salmon-qemu-smoke-rootfs-fixture <unprivileged-user> [rootfs-path]"
  where
    build user rootfs = do
        existing <- doesFileExist (rootfs </> "etc/issue")
        when existing $
            hPutStrLn stderr $
                "note: a tree already exists at "
                    <> rootfs
                    <> " and is left as it is; move it aside first to rebuild it from nothing"
        let owner = User.Owner (User.User (Text.pack user)) (User.Group (Text.pack user))
        ok <- upTree reportPrint (pure . runIdentity) (smokeRootfs owner rootfs)
        unless ok exitFailure
