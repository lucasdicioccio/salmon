module Salmon.Builtin.Nodes.Debian.Debootstrap where

import Salmon.Actions.UpDown (Requirement (..), skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Builtin.Nodes.Debian.Package (Package (..))

-------------------------------------------------------------------------------
data Report
    = RunDebootstrap !DebootstrapCommand !Binary.Report
    | RunEnsureVm9pBoot !Vm9pBootCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
data Suite
    = Stable
    | OldStable
    | Unstable
    | Testing
    deriving (Show)

type Includes =
    [Package]

{- | Packages a 'RootTree' needs beyond a bare chroot for it to be bootable
as a qemu guest and reachable once up: a kernel (so there's a
@\/boot\/vmlinuz-*@\/@initrd.img-*@ pair to hand qemu's @-kernel@\/@-initrd@
directly, skipping a bootloader entirely) and an SSH server (so a test
harness can reach in the same way "Salmon.Builtin.Nodes.Podman"-backed
tests @podman exec@ into a container). Debian's default debootstrap variant
already pulls in @systemd-sysv@ (so PID 1 reaches multi-user target) unless
@--variant=minbase@ was requested elsewhere — this list only adds what's
never on by default.

Merge into a caller's own 'Includes' with @<>@, e.g.:

> rootTree r boot (RootTree Stable "/var/lib/salmon-test-vms/foo/root" (vmEssentials <> [Package "curl"]))

A 'RootTree' meant to actually boot as a "Salmon.Builtin.Nodes.Qemu" guest
also needs 'ensureVm9pBoot' injected after 'rootTree' — this package list
alone gets you a kernel and sshd, not an initramfs that can find its own
root filesystem (see that function's haddock for why).
-}
vmEssentials :: Includes
vmEssentials =
    [ Package "linux-image-amd64"
    , Package "openssh-server"
    ]

data RootTree
    = RootTree
    { suite :: Suite
    , path :: FilePath
    , includes :: Includes
    }
    deriving (Show)

rootTree ::
    Reporter Report ->
    Track' (Binary "debootstrap") ->
    RootTree ->
    Op
rootTree r boot root =
    withBinary boot debootstrapCommand cmd $ \up ->
        op "debootstrap" (deps [rootdir]) $ \actions ->
            actions
                { help = Text.unwords ["debootstraps", Text.pack (show root.suite), "at", Text.pack root.path]
                , ref = mkRef "debootstrap" root.path
                , prelim = skipIfFileExists etcIssues
                , up = up r'
                }
  where
    r' = contramap (RunDebootstrap cmd) r
    cmd = MakeRoot root.includes root.suite root.path
    rootdir :: Op
    rootdir = dir (Directory root.path)
    etcIssues :: FilePath
    etcIssues = root.path </> "etc/issue"

data DebootstrapCommand
    = MakeRoot Includes Suite FilePath
    deriving (Show)

debootstrapCommand :: Command "debootstrap" DebootstrapCommand
debootstrapCommand = Command $ \cmd -> case cmd of
    (MakeRoot [] suite rootdir) ->
        proc
            "debootstrap"
            [ suiteName suite
            , rootdir
            ]
    (MakeRoot packages suite rootdir) ->
        proc
            "debootstrap"
            [ includearg packages
            , suiteName suite
            , rootdir
            ]
  where
    includearg xs =
        Text.unpack $
            "--include=" <> Text.intercalate "," (fmap pkgName xs)
    suiteName n =
        case n of
            Stable -> "stable"
            OldStable -> "oldstable"
            Unstable -> "unstable"
            Testing -> "testing"

-------------------------------------------------------------------------------

{- | Makes a 'vmEssentials'-equipped 'RootTree' actually able to boot as a
"Salmon.Builtin.Nodes.Qemu" guest: appends the 9p kernel modules
(@9p@\/@9pnet@\/@9pnet_virtio@\/@virtio@\/@virtio_pci@\/@virtio_ring@) to
@\/etc\/initramfs-tools\/modules@ and regenerates the initrd via a chroot
(hand-validated 2026-08-20, see @specs/qemu-test-vms-progress.md@).

Without this, the stock debootstrap initrd never even attempts a 9p mount
of its own root — @NET_9P@\/@NET_9P_VIRTIO@\/@9P_FS@ are modules, not
built into Debian's stock kernel, and nothing loads them, so
@initramfs-tools@'s @local_device_setup@ waits forever for a block device
that a 9p mount tag will never produce, then panics with @\/dev\/root does
not exist@.

Needs root (bind-mounts @\/proc@,@\/sys@,@\/dev@ into the chroot and
unmounts them after) — same privileged-execution assumption the rest of
this VM tier already carries. Idempotent: 'prelim' skips once
@\/etc\/initramfs-tools\/modules@ already mentions @9pnet_virtio@, so
rerunning after modules are already merged in only exits early rather than
running @update-initramfs@ (and its bind-mount dance) again.

Caller is expected to 'Salmon.Op.OpGraph.inject' this after the same
'RootTree''s 'rootTree', e.g.:

> ensureVm9pBoot r bash root \`inject\` rootTree r boot root
-}
ensureVm9pBoot :: Reporter Report -> Track' (Binary "bash") -> RootTree -> Op
ensureVm9pBoot r bash root =
    withBinary bash vm9pBootCommand cmd $ \up ->
        op "debootstrap-9p-boot" nodeps $ \actions ->
            actions
                { help = Text.unwords ["ensures", Text.pack root.path, "can boot its root filesystem over 9p"]
                , ref = mkRef "debootstrap-9p-boot" root.path
                , prelim = skipIf9pModulesConfigured root.path
                , up = up r'
                }
  where
    r' = contramap (RunEnsureVm9pBoot cmd) r
    cmd = EnsureVm9pBoot root.path

skipIf9pModulesConfigured :: FilePath -> IO Requirement
skipIf9pModulesConfigured rootdir = do
    let modulesFile = rootdir </> "etc/initramfs-tools/modules"
    exists <- doesFileExist modulesFile
    if not exists
        then pure Required
        else do
            contents <- readFile modulesFile
            pure $ if "9pnet_virtio" `isInfixOf` contents then Skippable else Required

newtype Vm9pBootCommand = EnsureVm9pBoot FilePath
    deriving (Show)

vm9pBootCommand :: Command "bash" Vm9pBootCommand
vm9pBootCommand = Command $ \(EnsureVm9pBoot rootdir) -> proc "bash" ["-c", ensureVm9pBootScript rootdir]

ensureVm9pBootScript :: FilePath -> String
ensureVm9pBootScript rootdir =
    unlines
        [ "set -e"
        , "root=" <> shellQuote rootdir
        , "modules=\"$root/etc/initramfs-tools/modules\""
        , "for m in 9p 9pnet 9pnet_virtio virtio virtio_pci virtio_ring; do"
        , "  grep -qxF \"$m\" \"$modules\" || echo \"$m\" >> \"$modules\""
        , "done"
        , "for d in proc sys dev; do mount --bind \"/$d\" \"$root/$d\"; done"
        , "chroot " <> shellQuote rootdir <> " update-initramfs -u -k all"
        , "for d in dev sys proc; do umount \"$root/$d\"; done"
        ]

shellQuote :: FilePath -> String
shellQuote p = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) p <> "'"
