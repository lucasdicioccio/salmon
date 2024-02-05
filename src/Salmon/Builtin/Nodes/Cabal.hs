-- todo:
-- --cabal-file configuration
-- --optimization and profile modes
module Salmon.Builtin.Nodes.Cabal where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Track
import Salmon.Builtin.Nodes.Binary

import GHC.TypeLits (Symbol)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc, cwd)
import System.Process.ByteString (readCreateProcessWithExitCode)

data Cabal = Cabal { cabalDir :: FilePath, cabalTarget :: Text }
  deriving (Eq, Ord, Show)

data CabalRun
  = Build Cabal
  | Install Cabal FilePath

build :: Track' (Binary "cabal") -> Cabal -> Op
build cabal c =
  withBinary cabal cabalRun (Build c) $ \up ->
    op "cabal-build" nodeps $ \actions -> actions {
        help = "cabal builds a target"
      , ref = dotRef $ "cabal:build:" <> (Text.pack (show c))
      , up = up
      }

install :: Track' (Binary "cabal") -> Cabal -> FilePath -> Op
install cabal c installdir =
  withBinary cabal cabalRun (Install c installdir) $ \up ->
    op "cabal-install" previous $ \actions -> actions {
        help = "cabal builds a target"
      , ref = dotRef $ "cabal:install:" <> (Text.pack (show c))
      , up = up
      }
  where
    previous = deps [dir (Directory installdir)]

cabalRun :: Command "cabal" CabalRun
cabalRun = Command $ go
  where
    go (Build c) = (proc "cabal" ["build", Text.unpack c.cabalTarget]) { cwd = Just c.cabalDir }
    go (Install c dir) = (proc "cabal" ["install", "--installdir=" <> dir, Text.unpack c.cabalTarget]) { cwd = Just c.cabalDir }

data Instructions (s :: Symbol)
  = Instructions
  { installed_name :: Text
  , instructions_binary :: Track' (Binary "cabal")
  , instructions_cabal :: Cabal
  , instructions_installdir :: FilePath
  }

data Installed (s :: Symbol)
  = Installed
  { installation_path :: FilePath
  }

installed :: Instructions a -> Tracked' (Installed a)
installed instr =
    Tracked (Track $ const op) obj
  where
    op =
        install
          instr.instructions_binary
          instr.instructions_cabal
          instr.instructions_installdir
    installPath = instr.instructions_installdir </> Text.unpack instr.installed_name
    obj = Installed installPath
