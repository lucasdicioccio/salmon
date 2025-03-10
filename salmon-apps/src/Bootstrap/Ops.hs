{-# LANGUAGE OverloadedRecordDot #-}

module Bootstrap.Ops where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (takeFileName)

import Salmon.Builtin.Extension (Op)
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Self as Self
import Salmon.Op.OpGraph (inject)

import Salmon.Reporter

bootstrapUbuntu :: Self.SelfPath -> Podman.TagName -> Op
bootstrapUbuntu self tagname =
    podImage `inject` copyBinaryToLocalDir
  where
    podImage :: Op
    podImage =
        Podman.buildImage
            reportPrint
            Debian.podman
            containerFile
            tagname

    currentBinaryName :: FilePath
    currentBinaryName = takeFileName self.getSelfPath

    containerFile :: FS.File "containerfile"
    containerFile =
        FS.generateFileContents
            (containerFileContents currentBinaryName)
            "Containerfile.bootstrap.ubuntu"

    copyBinaryToLocalDir :: Op
    copyBinaryToLocalDir =
        FS.fileCopy self.getSelfPath currentBinaryName

containerFileContents :: FilePath -> Text
containerFileContents selfPath =
    Text.unlines
        [ "FROM docker.io/ubuntu:latest"
        , "RUN apt-get update"
        , Text.unwords ["COPY", Text.pack selfPath, "/sbin"]
        ]
