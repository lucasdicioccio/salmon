{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The root filesystems of the three-VM Patroni harness
(@specs\/pg-patroni.md@, "Disaster scenarios").

The guests have no network past boot, so every package a scenario needs is
baked in here: @postgresql@, @patroni@, @etcd-server@ and @haproxy@ on all
three machines (a scenario decides which member runs which). It is the
same shape as @salmon-toy-qemu-pg-ha prereqs@ -- 'Debootstrap.rootTree' plus
'Debootstrap.ensureVm9pBoot' -- and is the only part that needs root:

> t=$(cabal list-bin salmon-patroni-rootfs)
> sudo $t config prereqs | sudo $t run up

The rootfses land where "Test.PatroniVms" looks for them
(@\/var\/lib\/salmon-test-vms\/patroni-{1,2,3}\/root@). The test harness
writes each guest's SSH trust into its rootfs at boot, so the only other
thing provisioned here is that each guest's @\/etc\/ssh@ belongs to whoever
runs the tests.
-}
module PatroniRootfs (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative (command, execParser, fullDesc, header, helper, info, long, progDesc, strOption, subparser, value, (<**>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Posix.User (getEffectiveUserName)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Debian.Debootstrap as Debootstrap
import Salmon.Builtin.Nodes.Debian.Package (Package (..))
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

main :: IO ()
main = do
    let desc =
            fullDesc
                <> progDesc "Root filesystems for the three-VM Patroni test harness"
                <> header "salmon-patroni-rootfs"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd

newtype Seed = SeedPrereqs {seedRoot :: FilePath}

instance ParseRecord Seed where
    parseRecord = combo <**> helper
      where
        combo =
            subparser $
                command "prereqs" (info (SeedPrereqs <$> rootOpt) (progDesc "the three root filesystems -- needs root"))
        rootOpt = strOption (long "root" <> Opt.help "where the guests' root filesystems live" <> value defaultRoot)

data Spec = Prereqs
    { specRoot :: FilePath
    , specOwner :: Text
    -- ^ who runs the tests afterwards, and so must own each guest's @\/etc\/ssh@
    }
    deriving (Generic)

instance FromJSON Spec
instance ToJSON Spec

-- | Where "Test.PatroniVms" expects to find them.
defaultRoot :: FilePath
defaultRoot = "/var/lib/salmon-test-vms"

-- | One rootfs per guest. Named by number, not role: which member leads is Patroni's business.
guests :: [Int]
guests = [1, 2, 3]

rootfsOf :: FilePath -> Int -> FilePath
rootfsOf root n = root </> ("patroni-" <> show n) </> "root"

-- | What every guest carries; the guests cannot install anything after boot.
patroniPackages :: Debootstrap.Includes
patroniPackages =
    Debootstrap.vmEssentials
        <> map Package ["postgresql", "sudo", "patroni", "etcd-server", "etcd-client", "haproxy"]

configure :: Configure IO Seed Spec
configure = Configure $ \(SeedPrereqs root) -> Prereqs root <$> unprivilegedUser

-- | The user who typed @sudo@, since real root has nobody to hand the rootfs to.
unprivilegedUser :: IO Text
unprivilegedUser = do
    sudoUser <- lookupEnv "SUDO_USER"
    case sudoUser of
        Just u | not (null u) -> pure (Text.pack u)
        _ -> do
            me <- getEffectiveUserName
            if me == "root"
                then fail "run `prereqs` with sudo, or pass SUDO_USER: somebody unprivileged has to own /etc/ssh afterwards"
                else pure (Text.pack me)

program :: Track' Spec
program = Track $ \(Prereqs root owner) ->
    op "patroni-prereqs" (deps (map (rootfsFor root owner) guests)) $ \actions ->
        actions
            { help = "root filesystems for the Patroni harness's guests"
            , notes = ["owned afterwards by " <> owner]
            , ref = mkRef "patroni-prereqs" root
            }

rootfsFor :: FilePath -> Text -> Int -> Op
rootfsFor root owner n =
    handOver `inject` bootable
  where
    tree = Debootstrap.RootTree Debootstrap.Stable (rootfsOf root n) patroniPackages
    bootable =
        Debootstrap.ensureVm9pBoot reportPrint bashTrack tree
            `inject` Debootstrap.rootTree reportPrint debootstrapTrack tree
    handOver = FS.ownedFile (FS.FileOwnership (rootfsOf root n </> "etc/ssh") (Just owner) Nothing 0o755)

bashTrack :: Track' (Binary.Binary "bash")
bashTrack = ignoreTrack

debootstrapTrack :: Track' (Binary.Binary "debootstrap")
debootstrapTrack = ignoreTrack
