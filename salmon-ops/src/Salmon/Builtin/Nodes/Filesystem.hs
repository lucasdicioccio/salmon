{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Salmon.Builtin.Nodes.Filesystem where

import Salmon.Builtin.Extension
import Salmon.Op.Ref

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBytestring
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.TypeLits (Symbol)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import System.Directory
import System.FilePath

newtype Directory = Directory {directoryPath :: FilePath}
    deriving (Eq, Ord, Show)

dir :: Directory -> Op
dir directory =
    op "directory" nodeps $ \actions ->
        actions
            { help = Text.pack $ "ensures " <> path <> "exists, including subdirs"
            , notes =
                [ "create dir recursively"
                , "does not delete contents of the directory"
                ]
            , ref = dotRef $ Text.pack $ "directory:" <> path
            , up = createDirectoryIfMissing True path
            , down = removeDirectory path
            }
  where
    path :: FilePath
    path = directory.directoryPath

-------------------------------------------------------------------------------

{- | Some file contents that get set once.

Default behaviour is to delete the file on down action
-}
data FileContents a = FileContents {filePath :: FilePath, contents :: a}
    deriving (Eq, Ord, Show, Functor)

filecontents :: (EncodeFileContents a) => FileContents a -> Op
filecontents fcontents =
    op "file-contents" (deps [enclosingdir]) $ \actions ->
        actions
            { help = Text.pack $ "writes " <> path <> "with some contents"
            , notes =
                [ "depends on the enclosing directory"
                ]
            , ref = dotRef $ Text.pack $ "file:" <> path
            , up = ByteString.writeFile path =<< encodeFileContents fcontents.contents
            , down = removeFile path
            }
  where
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory path)

    path :: FilePath
    path = fcontents.filePath

{- | Utility class to write various file contents.
The Text instance encodes contents in UTF8.
-}
class EncodeFileContents a where
    encodeFileContents :: a -> IO ByteString.ByteString

instance EncodeFileContents Text.Text where
    encodeFileContents = pure . Text.encodeUtf8

instance EncodeFileContents ByteString.ByteString where
    encodeFileContents = pure . id

instance EncodeFileContents String where
    encodeFileContents = pure . C8.pack

instance EncodeFileContents Aeson.Value where
    encodeFileContents = pure . LBytestring.toStrict . Aeson.encode

instance (EncodeFileContents a) => EncodeFileContents (IO a) where
    encodeFileContents ioX = ioX >>= encodeFileContents

-------------------------------------------------------------------------------

fileCopy :: FilePath -> FilePath -> Op
fileCopy src tgt =
    op "file-copy" (deps [enclosingdir]) $ \actions ->
        actions
            { help = Text.pack $ "copies " <> src <> tgt
            , ref = dotRef $ Text.pack $ "file-copy:" <> src <> " " <> tgt
            , up = copyFile src tgt
            , down = removeFile tgt
            }
  where
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory tgt)

-------------------------------------------------------------------------------
moveDirectory :: FilePath -> FilePath -> Op
moveDirectory src tgt =
    op "move-dir" (deps [enclosingdir]) $ \actions ->
        actions
            { help = Text.pack $ "moves " <> src <> " " <> tgt
            , ref = dotRef $ Text.pack $ "move-dir:" <> src <> " " <> tgt
            , up = renameDirectory src tgt
            }
  where
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory tgt)

-------------------------------------------------------------------------------
replaceDirectory :: FilePath -> FilePath -> FilePath -> Op
replaceDirectory src tgt trash =
    op "replace-dir" (deps [delete3 `inject` move2 `inject` move1]) $ \actions ->
        actions
            { help = Text.pack $ "replace " <> src <> " " <> tgt
            , ref = dotRef $ Text.pack $ "replace-dir:" <> src <> " " <> tgt
            }
  where
    move1 :: Op
    move1 = moveDirectory tgt trash
    move2 :: Op
    move2 = moveDirectory src tgt
    delete3 :: Op
    delete3 = destroyDirectory trash

-------------------------------------------------------------------------------
destroyDirectory :: FilePath -> Op
destroyDirectory trash =
    op "delete-dir" nodeps $ \actions ->
        actions
            { help = Text.pack $ "recursively trashes" <> trash
            , ref = dotRef $ Text.pack $ "delete-dir:" <> trash
            , up = removeDirectoryRecursive trash
            }

-------------------------------------------------------------------------------

data File (sym :: Symbol)
    = PreExisting FilePath
    | Generated (Track' FilePath) FilePath

getFilePath :: File a -> FilePath
getFilePath (PreExisting path) = path
getFilePath (Generated _ path) = path

fileOp :: File a -> Op
fileOp (PreExisting path) = placeholder "pre-existing-file" (Text.pack path)
fileOp (Generated t path) = run t path

withFile :: File a -> (FilePath -> Op) -> Op
withFile file@(PreExisting path) f = f path `inject` fileOp file
withFile (Generated mkp path) f = tracking mkp (\x -> (x, x)) path f
