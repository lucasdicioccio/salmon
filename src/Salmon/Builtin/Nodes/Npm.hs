module Salmon.Builtin.Nodes.Npm where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (Symbol)

import System.FilePath (takeDirectory, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, cwd, proc)

-------------------------------------------------------------------------------
data Report
    = NpmInstall !Npm !Binary.Report
    deriving (Show)

isInstallSuccess :: Report -> Bool
isInstallSuccess r = case r of
    (NpmInstall _ cmd) ->
        Binary.isCommandSuccessful cmd

-------------------------------------------------------------------------------
data Npm = Npm {spagoDir :: FilePath}
    deriving (Eq, Ord, Show)

data NpmRun
    = Install Npm

install :: Reporter Report -> Track' (Binary "npm") -> Npm -> Op
install r spago s =
    withBinary spago npmRun (Install s) $ \up ->
        op "npm-install" nodeps $ \actions ->
            actions
                { help = "npm installs a project"
                , ref = dotRef $ "npm:install:" <> (Text.pack (show s))
                , up = up r'
                }
  where
    r' = contramap (NpmInstall s) r

npmRun :: Command "npm" NpmRun
npmRun = Command $ go
  where
    go (Install c) = (proc "npm" ["install"]){cwd = Just c.spagoDir}

