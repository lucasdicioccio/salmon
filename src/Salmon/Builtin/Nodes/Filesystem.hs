{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Salmon.Builtin.Nodes.Filesystem where

import Salmon.Builtin.Extension
import Salmon.Op.Ref

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import System.Directory
import System.FilePath
import Salmon.Actions.Dot

newtype Directory = Directory { directoryPath :: FilePath }
  deriving (Eq, Ord, Show)

dir :: Directory -> Op
dir directory =
  op "directory" nodeps $ \actions -> actions {
      help = Text.pack $ "ensures " <> path <> "exists, including subdirs"
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

-- | Some file contents that get set once.
-- 
-- Default behaviour is to delete the file on down action
data FileContents a = FileContents { filePath :: FilePath, contents :: a }
  deriving (Eq, Ord, Show, Functor)

filecontents :: EncodeFileContents a => FileContents a -> Op
filecontents fcontents =
  op "file-contents" (deps [enclosingdir]) $ \actions -> actions {
      help = Text.pack $ "writes " <> path <> "with some contents"
    , notes =
      [ "depends on the enclosing directory"
      ]
    , ref = dotRef $ Text.pack $ "file:" <> path
    , up = ByteString.writeFile path $ encodeFileContents fcontents.contents
    , down = removeFile path
    }
  where
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory path)

    path :: FilePath
    path = fcontents.filePath

-- | Utility class to write various file contents.
-- The Text instance encodes contents in UTF8.
class EncodeFileContents a where
   encodeFileContents :: a -> ByteString.ByteString

instance EncodeFileContents Text.Text where
   encodeFileContents = Text.encodeUtf8

instance EncodeFileContents ByteString.ByteString where
   encodeFileContents = id

instance EncodeFileContents String where
   encodeFileContents = C8.pack
