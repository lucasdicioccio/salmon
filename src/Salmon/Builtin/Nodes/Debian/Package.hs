
module Salmon.Builtin.Nodes.Debian.Package where

import Salmon.Builtin.Extension
import Salmon.Op.Ref
import Salmon.Op.OpGraph

import Control.Monad (void)
import Data.Dynamic (toDyn)
import Data.Text (Text)
import Data.Foldable (toList)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NEList
import System.Environment (getEnvironment)
import System.Process.ListLike (CreateProcess, proc, env)
import System.Process.ByteString (readCreateProcessWithExitCode)

data Package = Package { pkgName :: Text }
  deriving (Eq, Ord, Show)

deb :: Package -> Op
deb pkg =
  op "deb" nodeps $ \actions -> actions {
      help = "installs " <> pkg.pkgName
    , ref = dotRef $ "debian:deb:" <> pkg.pkgName
    , up = upAction
    , down = downAction
    , dynamics = [toDyn pkg]
    }

  where
    pkgs :: NEList.NonEmpty Package
    pkgs = NEList.singleton pkg

    upAction :: IO ()
    upAction = void $ do
      baseEnv <- getEnvironment
      readCreateProcessWithExitCode (aptInstallProcess pkgs baseEnv) ""
    downAction :: IO ()
    downAction = void $
      readCreateProcessWithExitCode (aptUninstallProcess pkgs ) ""

debs :: NEList.NonEmpty Package -> Op
debs pkgs =
  op "debs" nodeps $ \actions -> actions {
      help = "installs " <> Text.pack (show (length pkgs)) <> " packages"
    , notes = pkgName <$> toList pkgs
    , ref = dotRef $ "debian:deb:set:" <> foldMap pkgName pkgs
    , up = upAction
    , down = downAction
    }

  where
    upAction :: IO ()
    upAction = void $ do
      baseEnv <- getEnvironment
      readCreateProcessWithExitCode (aptInstallProcess pkgs baseEnv) ""
    downAction :: IO ()
    downAction = void $
      readCreateProcessWithExitCode (aptUninstallProcess pkgs ) ""

-- | Returns a new Op collecting.
installAllDebsAtOnce :: Op -> Op
installAllDebsAtOnce root =
  case NEList.nonEmpty (concatMap snd $ collectDynamics root) of
    Just pkgs -> debs pkgs
    Nothing -> realNoop

aptInstallProcess :: NEList.NonEmpty Package -> [(String,String)] -> CreateProcess
aptInstallProcess pkgs baseEnv =
  (proc "apt-get" args) { env = Just (("DEBIAN_FRONTEND", "noninteractive"):baseEnv) }
  where
    args :: [String]
    args = [ "install", "-y", "-q" ] <> [ Text.unpack pkg.pkgName | pkg <- toList pkgs ]

aptUninstallProcess :: NEList.NonEmpty Package -> CreateProcess
aptUninstallProcess pkgs =
  proc "apt-get" args
  where
    args :: [String]
    args = [ "remove", "-q" ] <> [ Text.unpack pkg.pkgName | pkg <- toList pkgs ]
