module Salmon.Builtin.Nodes.Debian.Package where

import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Reporter

import Data.Dynamic (toDyn)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getEnvironment)
import System.Process.ListLike (CreateProcess, env, proc)

data Package = Package {pkgName :: Text}
    deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

-- | Which apt-get invocation a 'Report' is for (including the full package set, e.g. to see why an install failed with "too many arguments").
data AptCommand
    = AptInstall !(NEList.NonEmpty Package)
    | AptRemove !(NEList.NonEmpty Package)
    deriving (Show)

data Report
    = RunAptGet !AptCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

deb :: Package -> Op
deb = debWith silent

-- | Like 'deb', but takes a 'Reporter' to observe the apt-get invocation (command, exit code, stdout/stderr).
debWith :: Reporter Report -> Package -> Op
debWith r pkg =
    op "deb" nodeps $ \actions ->
        actions
            { help = "installs " <> pkg.pkgName
            , ref = mkRef "debian-deb" pkg.pkgName
            , up = upAction
            , down = downAction
            , dynamics = [toDyn pkg]
            }
  where
    pkgs :: NEList.NonEmpty Package
    pkgs = NEList.singleton pkg

    upAction :: IO ()
    upAction = do
        baseEnv <- getEnvironment
        Binary.untrackedExec (aptInstallCommand baseEnv) pkgs "" (contramap (RunAptGet (AptInstall pkgs)) r)
    downAction :: IO ()
    downAction =
        Binary.untrackedExec aptUninstallCommand pkgs "" (contramap (RunAptGet (AptRemove pkgs)) r)

debs :: NEList.NonEmpty Package -> Op
debs = debsWith silent

-- | Like 'debs', but takes a 'Reporter' to observe the apt-get invocation (command, exit code, stdout/stderr).
debsWith :: Reporter Report -> NEList.NonEmpty Package -> Op
debsWith r pkgs =
    op "debs" nodeps $ \actions ->
        actions
            { help = "installs " <> Text.pack (show (length pkgset)) <> " packages"
            , notes = pkgName <$> toList pkgset
            , ref = mkRef "debian-deb-set" (pkgName <$> Set.toList pkgset)
            , up = upAction
            , down = downAction
            }
  where
    pkgset :: Set.Set Package
    pkgset = Set.fromList $ NEList.toList pkgs
    -- dedup before ever building the apt-get argv, not just for display (`help`/`notes`/`ref` above) —
    -- otherwise many predecessors depending on the same package (e.g. one per migration file) each
    -- contribute their own copy of it to the same apt-get invocation.
    dedupedPkgs :: NEList.NonEmpty Package
    dedupedPkgs = NEList.fromList $ Set.toList pkgset
    upAction :: IO ()
    upAction = do
        baseEnv <- getEnvironment
        Binary.untrackedExec (aptInstallCommand baseEnv) dedupedPkgs "" (contramap (RunAptGet (AptInstall dedupedPkgs)) r)
    downAction :: IO ()
    downAction =
        Binary.untrackedExec aptUninstallCommand dedupedPkgs "" (contramap (RunAptGet (AptRemove dedupedPkgs)) r)

-- | Returns a new Op collecting.
installAllDebsAtOnce :: Op -> Op
installAllDebsAtOnce = installAllDebsAtOnceWith silent

-- | Like 'installAllDebsAtOnce', but takes a 'Reporter' to observe the batched apt-get invocation.
installAllDebsAtOnceWith :: Reporter Report -> Op -> Op
installAllDebsAtOnceWith r =
    collectPackagesAsSet
  where
    collectPackagesAsSet :: Op -> Op
    collectPackagesAsSet root =
        case NEList.nonEmpty (concatMap snd $ collectDynamics root) of
            Just pkgs -> debsWith r pkgs
            Nothing -> realNoop

removeSinglePackages :: Op -> Op
removeSinglePackages root
    | null (packages root) = root{predecessors = fmap (fmap removeSinglePackages) root.predecessors}
    | otherwise = realNoop{predecessors = fmap (fmap removeSinglePackages) root.predecessors}
  where
    packages :: Op -> [Package]
    packages root = getDynamics root

aptInstallCommand :: [(String, String)] -> Binary.Command "apt-get" (NEList.NonEmpty Package)
aptInstallCommand baseEnv = Binary.Command $ \pkgs -> aptInstallProcess pkgs baseEnv

aptInstallProcess :: NEList.NonEmpty Package -> [(String, String)] -> CreateProcess
aptInstallProcess pkgs baseEnv =
    (proc "apt-get" args){env = Just (("DEBIAN_FRONTEND", "noninteractive") : baseEnv)}
  where
    args :: [String]
    args = ["install", "-y", "-q"] <> [Text.unpack pkg.pkgName | pkg <- toList pkgs]

aptUninstallCommand :: Binary.Command "apt-get" (NEList.NonEmpty Package)
aptUninstallCommand = Binary.Command aptUninstallProcess

aptUninstallProcess :: NEList.NonEmpty Package -> CreateProcess
aptUninstallProcess pkgs =
    proc "apt-get" args
  where
    args :: [String]
    args = ["remove", "-q"] <> [Text.unpack pkg.pkgName | pkg <- toList pkgs]
