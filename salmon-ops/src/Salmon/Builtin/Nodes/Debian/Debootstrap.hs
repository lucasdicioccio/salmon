module Salmon.Builtin.Nodes.Debian.Debootstrap where

import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Builtin.Nodes.Debian.Package (Package (..))

-------------------------------------------------------------------------------
data Report
    = RunDebootstrap !DebootstrapCommand !Binary.Report
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
