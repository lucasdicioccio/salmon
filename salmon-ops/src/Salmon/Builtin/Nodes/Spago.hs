module Salmon.Builtin.Nodes.Spago where

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
    = SpagoBuild !Spago !Binary.Report
    | SpagoBundle !Spago !FilePath !Binary.Report
    deriving (Show)

isBuildSuccess :: Report -> Bool
isBuildSuccess r = case r of
    (SpagoBuild _ cmd) ->
        Binary.isCommandSuccessful cmd
    otherwise -> False

-------------------------------------------------------------------------------
data Spago = Spago {spagoDir :: FilePath}
    deriving (Eq, Ord, Show)

type MainModule = Text

data SpagoRun
    = Build Spago
    | BundleApp Spago MainModule FilePath

build :: Reporter Report -> Track' (Binary "spago") -> Spago -> Op
build r spago s =
    withBinary spago spagoRun (Build s) $ \up ->
        op "spago-build" nodeps $ \actions ->
            actions
                { help = "spago builds a target"
                , ref = dotRef $ "spago:build:" <> (Text.pack (show s))
                , up = up r'
                }
  where
    r' = contramap (SpagoBuild s) r

bundleApp :: Reporter Report -> Track' (Binary "spago") -> Spago -> MainModule -> FilePath -> Op
bundleApp r spago s m path =
    withBinary spago spagoRun (BundleApp s m path) $ \up ->
        op "spago-build" (deps [enclosingdir]) $ \actions ->
            actions
                { help = "spago bundles a target"
                , ref = dotRef $ "spago:build:" <> (Text.pack path)
                , up = up r'
                }
  where
    r' = contramap (SpagoBundle s path) r
    enclosingdir = dir (Directory dirpath)
    dirpath = takeDirectory path

spagoRun :: Command "spago" SpagoRun
spagoRun = Command $ go
  where
    go (Build c) = (proc "spago" ["build"]){cwd = Just c.spagoDir}
    go (BundleApp c m t) = (proc "spago" ["bundle-app", "-m", Text.unpack m, "-t", t]){cwd = Just c.spagoDir}
