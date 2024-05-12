{-# LANGUAGE ExistentialQuantification #-}

module Salmon.Builtin.Nodes.Tar where

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
    = TarCreate !FilePath !Binary.Report
    | TarExtract !FilePath !FilePath !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

data Compression
    = Uncompressed
    | Gzip
    | Bzip2
    | Xz
    deriving (Eq, Ord, Show)

data Tar = Tar {tarPath :: FilePath, tarCompression :: Compression}
    deriving (Eq, Ord, Show)

data Tarfile = forall a. Tarfile (File a)

tarfilePath :: Tarfile -> FilePath
tarfilePath (Tarfile file) = getFilePath file

data FileList = FileList [Tarfile]

filePaths :: FileList -> [FilePath]
filePaths (FileList files) = fmap tarfilePath files

-------------------------------------------------------------------------------
create :: Reporter Report -> Track' (Binary "tar") -> Tar -> FilePath -> FileList -> Op
create r tar t dirpath files =
    withBinary tar tarRun (Create t dirpath files) $ \up ->
        op "tar-create" (deps [enclosingdir]) $ \actions ->
            actions
                { help = "creates a tar archive"
                , ref = dotRef $ "tar:create:" <> (Text.pack t.tarPath)
                , up = up r'
                }
  where
    r' = contramap (TarCreate t.tarPath) r
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory t.tarPath)

-------------------------------------------------------------------------------
extract :: Reporter Report -> Track' (Binary "tar") -> Tar -> FilePath -> Op
extract r tar t dirpath =
    withBinary tar tarRun (Extract t dirpath) $ \up ->
        op "tar-extract" (deps [extractiondir]) $ \actions ->
            actions
                { help = "extracts a tar archive"
                , ref = dotRef $ "tar:extract:" <> (Text.pack t.tarPath)
                , up = up r'
                }
  where
    r' = contramap (TarExtract t.tarPath dirpath) r
    extractiondir :: Op
    extractiondir = dir (Directory dirpath)

-------------------------------------------------------------------------------
data TarRun
    = Create Tar FilePath FileList
    | Extract Tar FilePath

-------------------------------------------------------------------------------
tarRun :: Command "tar" TarRun
tarRun = Command $ go
  where
    compressionFlags Uncompressed = []
    compressionFlags Gzip = ["--gzip"]
    compressionFlags Bzip2 = ["--bzip2"]
    compressionFlags Xz = ["--xz"]

    go (Create t dir files) =
        let args = compressionFlags t.tarCompression ++ ["--create", "--file", t.tarPath, "--directory", dir] ++ filePaths files
         in proc "tar" args
    go (Extract t dir) =
        let args = compressionFlags t.tarCompression ++ ["--extract", "-f", t.tarPath, "--directory", dir]
         in proc "tar" args
