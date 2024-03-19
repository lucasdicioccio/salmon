-- todo:
-- --cabal-file configuration
-- --optimization and profile modes
module Salmon.Builtin.Nodes.Cabal where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (Symbol)

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, cwd, proc)

data Cabal = Cabal {cabalDir :: FilePath, cabalTarget :: Text}
    deriving (Eq, Ord, Show)

type CabalFlags = [Flag]

data Flag
    = AllowNewer

data CabalRun
    = Build CabalFlags Cabal
    | Install CabalFlags Cabal FilePath

build :: Track' (Binary "cabal") -> CabalFlags -> Cabal -> Op
build cabal flags c =
    withBinary cabal cabalRun (Build flags c) $ \up ->
        op "cabal-build" nodeps $ \actions ->
            actions
                { help = "cabal builds a target"
                , ref = dotRef $ "cabal:build:" <> (Text.pack (show c))
                , up = up
                }

install :: Track' (Binary "cabal") -> CabalFlags -> Cabal -> FilePath -> Op
install cabal flags c installdir =
    withBinary cabal cabalRun (Install flags c installdir) $ \up ->
        op "cabal-install" previous $ \actions ->
            actions
                { help = "cabal builds a target"
                , ref = dotRef $ "cabal:install:" <> (Text.pack (show c))
                , up = up
                }
  where
    previous = deps [dir (Directory installdir)]

cabalRun :: Command "cabal" CabalRun
cabalRun = Command $ go
  where
    go (Build flags c) = (proc "cabal" (["build", Text.unpack c.cabalTarget] <> (extraArgs flags))){cwd = Just c.cabalDir}
    go (Install flags c dir) = (proc "cabal" (["install", "--install-method=copy", "--overwrite-policy=always", "--installdir=" <> dir, Text.unpack c.cabalTarget] <> (extraArgs flags))){cwd = Just c.cabalDir}

    extraArgs :: CabalFlags -> [String]
    extraArgs xs = fmap extraArg xs

    extraArg :: Flag -> String
    extraArg AllowNewer = "--allow-newer=true"

data Instructions (s :: Symbol)
    = Instructions
    { installed_name :: Text
    , instructions_binary :: Track' (Binary "cabal")
    , instructions_cabal :: Cabal
    , instructions_cabal_flags :: CabalFlags
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
            instr.instructions_cabal_flags
            instr.instructions_cabal
            instr.instructions_installdir
    installPath = instr.instructions_installdir </> Text.unpack instr.installed_name
    obj = Installed installPath
