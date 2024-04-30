module Salmon.Builtin.Nodes.Upx where

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
    = UpxPack !FilePath !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
data UpxRun
    = Pack FilePath

pack :: Reporter Report -> Track' (Binary "upx") -> FilePath -> Op
pack r upx path =
    withBinary upx upxRun (Pack path) $ \up ->
        op "upx-pack" nodeps $ \actions ->
            actions
                { help = "upx packs a binary"
                , ref = dotRef $ "upx:pack:" <> (Text.pack path)
                , up = up r'
                }
  where
    r' = contramap (UpxPack path) r

upxRun :: Command "upx" UpxRun
upxRun = Command $ go
  where
    go (Pack path) = proc "upx" [path]
