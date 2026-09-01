{- | Runs a qemu VM as a systemd unit — see @specs/qemu-test-vms.md@ (§2) for
the design this implements.

A VM's disk is (v1) a plain "Salmon.Builtin.Nodes.Debian.Debootstrap" chroot
directory, exported to the guest via qemu's @virtfs@ 9p passthrough rather
than a loop-mounted disk image (no @mkfs@\/loop-device step, and the guest's
files stay plain files on the host, trivially inspectable — see §3 of the
same spec for the tradeoffs). Booting skips a bootloader entirely: the
kernel\/initrd already unpacked into the chroot's own @\/boot@ by
'Salmon.Builtin.Nodes.Debian.Debootstrap.vmEssentials' are handed to qemu
directly via @-kernel@\/@-initrd@.

Two details below only became certain after actually booting one of these
(hand-validated 2026-08-20, see @specs/qemu-test-vms-progress.md@):

* The 9p @fsdev@ uses @security_model=passthrough@, not the more obvious
  @mapped@: qemu (and this whole tier) already runs as root on the host
  (see 'Qemu.setup's haddock below), so there's no need for @mapped@'s
  host-uid remapping — and @mapped@ actively breaks booting here, because
  it doesn't round-trip Debian's @\/bin -> usr\/bin@-style symlinks
  faithfully, which @run-init@ then sees as a symlink loop
  (@\/sbin\/init: Too many symbolic links encountered@).
* The 9p mount tag (and the kernel's @root=@) is @vroot@, not
  @\/dev\/root@: Debian's stock @initramfs-tools@ @\/scripts\/local@ only
  skips its udev block-device wait for a @ROOT@ that neither starts with
  @\/dev@ nor contains @=@ (see @local_device_setup@) — anything else, 9p
  mount tags included, it waits on forever since a 9p mount never produces
  a udev block device. A tag with no @\/dev@ prefix takes that fast path
  and hands the tag straight to @mount -t 9p@, which resolves it fine.

The kernel command line also always carries @net.ifnames=0 biosdevname=0@
(see 'kernelCmdline'): Debian's default predictable-naming udev rules
rename the single virtio-net device to something like @ens4@, not @eth0@,
which breaks a caller-supplied @ip=...:eth0:off@ kernel arg silently (VM
boots, network never comes up). Forcing classic naming keeps the "single
NIC, always @eth0@" assumption this whole tier's networking (fixed
@ip=@\/'Salmon.Builtin.Nodes.Debian.Debootstrap.ensureVm9pBoot') already
makes actually true.

Lifecycle (start\/stop) is delegated entirely to
"Salmon.Builtin.Nodes.Systemd" — a VM is just another systemd unit from the
host's point of view, exactly like 'Salmon.Builtin.Nodes.Nginx.setup' or
'Salmon.Builtin.Nodes.PgBouncer.setup' delegate to it, so 'up'\/'down' reuse
that module's process-supervision instead of this module inventing its own.

Caveat carried over from the spec, not yet addressed: stopping the unit
sends the guest a bare SIGTERM (qemu's default: quits immediately), not a
graceful ACPI shutdown via the qemu monitor socket. Acceptable for v1's
disposable-test-VM use case (nothing in the guest is precious), called out
explicitly rather than silently accepted — see @specs/qemu-test-vms.md@ §2
for the graceful-shutdown follow-up this would need.
-}
module Salmon.Builtin.Nodes.Qemu where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import qualified Salmon.Builtin.Nodes.LinuxBridge as LinuxBridge
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Reporter

import Control.Exception (throwIO)
import Control.Monad (filterM)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

-------------------------------------------------------------------------------

type VmName = Text

-- | Absolute path to a qemu monitor unix socket (for out-of-band control; see the module haddock).
type MonitorSocket = FilePath

data VmConfig
    = VmConfig
    { vm_name :: VmName
    , vm_memory_mb :: Int
    , vm_smp :: Int
    , vm_rootfs :: FilePath
    -- ^ a 'Salmon.Builtin.Nodes.Debian.Debootstrap.RootTree' path, exported via 9p
    , vm_kernel :: FilePath
    , vm_initrd :: FilePath
    -- ^ resolve both with 'resolveKernelInitrd' before constructing a 'VmConfig' —
    -- see its haddock for why this can't happen inside the 'Op' itself
    , vm_extra_kernel_args :: [Text]
    , vm_tap :: LinuxBridge.Tap
    , vm_mac :: Text
    , vm_monitor_socket :: MonitorSocket
    , vm_enable_kvm :: Bool
    , vm_user :: Systemd.User
    , vm_group :: Systemd.Group
    , vm_working_dir :: FilePath
    , vm_systemd_scope :: Systemd.Scope
    , vm_unit_dir :: FilePath
    -- ^ @\/etc\/systemd\/system@ for 'Systemd.System' scope, or a
    -- caller-resolved @~\/.config\/systemd\/user@ for 'Systemd.User' scope
    -- (needs no root at all — see "Test.Harness".'Test.Harness.withVmAt',
    -- the only 'Systemd.User'-scope caller so far) — same "resolve before
    -- constructing" rule as 'resolveKernelInitrd' above.
    }

{- | Finds the single @vmlinuz-*@\/@initrd.img-*@ pair a
'Salmon.Builtin.Nodes.Debian.Debootstrap.vmEssentials'-equipped chroot's
@\/boot@ was left with, so a caller building a 'VmConfig' doesn't have to
know the exact kernel version string. This has to be an ordinary 'IO'
function run at seed\/'Salmon.Op.Configure.Configure'-generation time, not
something resolved inside the 'Op' itself: 'Salmon.Op.OpGraph.OpGraph'
values in this codebase are always built from already-concrete data — an
'Op' has no mechanism to hand a discovered path to a sibling node's
arguments mid-traversal, so this resolution has to happen before a
'VmConfig' is constructed at all.

Throws if @\/boot@ doesn't contain exactly one of each, which is what a
fresh, single-kernel debootstrap chroot should always have; ambiguity (an
upgraded/held-over old kernel) is a caller-visible error rather than a
silent "pick one."
-}
resolveKernelInitrd :: FilePath -> IO (FilePath, FilePath)
resolveKernelInitrd rootfs = do
    (,) <$> theOne "vmlinuz-" <*> theOne "initrd.img-"
  where
    bootDir = rootfs </> "boot"
    theOne prefix = do
        entries <- listDirectory bootDir
        let matches = filter (prefix `isPrefixOf`) entries
        files <- filterM (doesFileExist . (bootDir </>)) matches
        case files of
            [f] -> pure (bootDir </> f)
            [] -> throwIO (userError ("resolveKernelInitrd: no " <> prefix <> "* under " <> bootDir))
            fs -> throwIO (userError ("resolveKernelInitrd: ambiguous " <> prefix <> "* under " <> bootDir <> ": " <> show fs))

-------------------------------------------------------------------------------

{- | Installs qemu, brings up the VM's tap device (see
"Salmon.Builtin.Nodes.LinuxBridge"), and runs the VM as a systemd unit.
-}
setup :: Reporter Systemd.Report -> Reporter LinuxBridge.Report -> Track' (Binary "systemctl") -> Track' (Binary "qemu-system-x86_64") -> Track' (Binary "ip") -> VmConfig -> Op
setup r rTap systemctl qemuBin ip cfg =
    Systemd.systemdService r systemctl trackConfig systemdCfg
        `inject` LinuxBridge.tap rTap ip cfg.vm_tap
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \_ -> op "qemu-setup" (deps [justInstall qemuBin]) id

    systemdCfg :: Systemd.Config
    systemdCfg = Systemd.Config cfg.vm_systemd_scope cfg.vm_unit_dir unitName unit svc install

    unitName :: Systemd.UnitTarget
    unitName = "salmon-vm-" <> cfg.vm_name <> ".service"

    -- | @network-online.target@/@multi-user.target@ only exist in the
    -- system manager — a 'Systemd.User'-scope unit orders against and is
    -- wanted by the user session's own @default.target@ instead.
    unit :: Systemd.Unit
    unit = Systemd.Unit ("Salmon-managed qemu VM: " <> cfg.vm_name) afterTarget

    afterTarget :: Systemd.UnitTarget
    afterTarget = case cfg.vm_systemd_scope of
        Systemd.System -> "network-online.target"
        Systemd.User -> "default.target"

    svc :: Systemd.Service
    svc = Systemd.Service Systemd.Simple cfg.vm_user cfg.vm_group "0022" start Systemd.OnFailure Systemd.Process cfg.vm_working_dir

    start :: Systemd.Start
    start = Systemd.Start "/usr/bin/qemu-system-x86_64" (qemuArgs cfg)

    install :: Systemd.Install
    install = Systemd.Install wantedByTarget

    wantedByTarget :: Systemd.UnitTarget
    wantedByTarget = case cfg.vm_systemd_scope of
        Systemd.System -> "multi-user.target"
        Systemd.User -> "default.target"

-- | The command-line qemu is started with — see @specs/qemu-test-vms.md@ §2 for the design.
qemuArgs :: VmConfig -> [Text]
qemuArgs cfg =
    mconcat
        [
            [ "-name"
            , cfg.vm_name
            , "-m"
            , Text.pack (show cfg.vm_memory_mb)
            , "-smp"
            , Text.pack (show cfg.vm_smp)
            ]
        ,
            [ "-fsdev"
            , "local,id=root,path=" <> Text.pack cfg.vm_rootfs <> ",security_model=passthrough"
            , "-device"
            , "virtio-9p-pci,fsdev=root,mount_tag=vroot"
            ]
        ,
            [ "-kernel"
            , Text.pack cfg.vm_kernel
            , "-initrd"
            , Text.pack cfg.vm_initrd
            , "-append"
            , kernelCmdline cfg
            ]
        ,
            [ "-netdev"
            , "tap,id=net0,ifname=" <> cfg.vm_tap.tapName <> ",script=no,downscript=no"
            , "-device"
            , "virtio-net-pci,netdev=net0,mac=" <> cfg.vm_mac
            ]
        ,
            [ "-monitor"
            , "unix:" <> Text.pack cfg.vm_monitor_socket <> ",server,nowait"
            , "-nographic"
            , "-serial"
            , "mon:stdio"
            ]
        , if cfg.vm_enable_kvm then ["-enable-kvm", "-cpu", "host"] else []
        ]

kernelCmdline :: VmConfig -> Text
kernelCmdline cfg =
    Text.unwords $
        mconcat
            [
                [ "root=vroot"
                , "rootfstype=9p"
                , "rootflags=trans=virtio"
                , "rw"
                , "console=ttyS0"
                , "net.ifnames=0"
                , "biosdevname=0"
                ]
            , cfg.vm_extra_kernel_args
            ]
