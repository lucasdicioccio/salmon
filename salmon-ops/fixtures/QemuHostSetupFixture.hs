{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | One-time, privileged host setup for the Layer 3 qemu test tier (see
@specs/qemu-test-vms.md@\/@specs/qemu-test-vms-progress.md@): grants
"Salmon.Builtin.Nodes.Capabilities" to @ip@\/@qemu-system-x86_64@ and hands
ownership of each rootfs's @etc\/ssh@ subtree to an unprivileged user, so
that routine test runs (@cabal test salmon-ops-recipes@,
"Test.Harness".'Test.Harness.hasVmPrivileges') no longer need to run as
root at all — only this one-off setup does.

Meant to be run once per machine, as root (or under @sudo@), and again
after any @apt upgrade@ of @iproute2@\/@qemu-system-x86@ (package upgrades
replace the binary, wiping its capabilities — see
"Salmon.Builtin.Nodes.Capabilities".'Salmon.Builtin.Nodes.Capabilities.grantCapabilities'
haddock) or after debootstrapping a new rootfs:

> cabal build salmon-qemu-host-setup-fixture
> sudo dist-newstyle/build/*/*/salmon-ops-0.1.0.0/x/salmon-qemu-host-setup-fixture/build/salmon-qemu-host-setup-fixture/salmon-qemu-host-setup-fixture \
>     lucas /var/lib/salmon-test-vms/smoke/root /var/lib/salmon-test-vms/pg-primary/root /var/lib/salmon-test-vms/pg-standby/root

Idempotent (same conventions as every other node in this codebase): safe to
rerun, and every step it didn't need to redo is reported 'Skip'/no-op.
-}
module Main (main) where

import Control.Monad (unless)
import Control.Monad.Identity (runIdentity)
import Data.List (foldl')
import qualified Data.Text as Text
import System.Directory (canonicalizePath, findExecutable)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))

import Salmon.Actions.UpDown (upTree)
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Capabilities as Capabilities
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.User as User
import Salmon.Op.OpGraph (overlaid)
import Salmon.Reporter (reportPrint)

-- | The capabilities each binary needs — see 'Salmon.Builtin.Nodes.Capabilities.grantCapabilities'.
ipCapabilities, qemuCapabilities :: [Capabilities.Capability]
ipCapabilities = ["cap_net_admin"]
qemuCapabilities = ["cap_dac_override", "cap_chown", "cap_fowner"]

-- | Grants a resolved binary path its needed capabilities, or dies loudly
-- if the binary isn't found — same "fail loudly, don't hang" spirit as
-- "Test.Harness".'Test.Harness.requireExecutable'. Canonicalizes past any
-- symlink first (e.g. Debian's usrmerge makes @\/usr\/sbin\/ip@ a symlink to
-- @\/bin\/ip@) — @setcap@ refuses to operate on a symlink at all
-- ("Invalid file for capability operation"), it needs the real inode.
capabilityOp :: String -> [Capabilities.Capability] -> IO Op
capabilityOp exe caps = do
    mPath <- findExecutable exe
    case mPath of
        Nothing -> die (exe <> " not found on PATH")
        Just linkedPath -> do
            path <- canonicalizePath linkedPath
            pure (Capabilities.grantCapabilities reportPrint Debian.setcap path caps)

-- | Hands ownership of one rootfs's @etc\/ssh@ subtree to the unprivileged
-- test user — see "Test.Harness".'Test.Harness.ensureVmSshAccess', which
-- writes a fresh per-boot SSH CA there directly on the host filesystem.
sshDirOwnershipOp :: User.Owner -> FilePath -> Op
sshDirOwnershipOp owner rootfs =
    User.chown reportPrint Debian.chown True owner (rootfs </> "etc/ssh")

main :: IO ()
main = do
    args <- getArgs
    case args of
        (user : rootfsPaths) -> do
            ipOp <- capabilityOp "ip" ipCapabilities
            qemuOp <- capabilityOp "qemu-system-x86_64" qemuCapabilities
            let owner = User.Owner (User.User (Text.pack user)) (User.Group (Text.pack user))
                sshOps = map (sshDirOwnershipOp owner) rootfsPaths
                allOps = foldl' overlaid ipOp (qemuOp : sshOps)
            ok <- upTree reportPrint (pure . runIdentity) allOps
            unless ok exitFailure
        _ -> die "usage: salmon-qemu-host-setup-fixture <unprivileged-user> <rootfs-path>..."
