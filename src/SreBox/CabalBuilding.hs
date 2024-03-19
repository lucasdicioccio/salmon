module SreBox.CabalBuilding where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (takeFileName, (</>))

import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Cabal as Cabal
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track

cabalBinUpload :: Tracked' FilePath -> Rsync.Remote -> Tracked' FilePath
cabalBinUpload mkbin remote =
    mkbin `bindTracked` go
  where
    go localpath =
        Tracked (Track $ const $ upload localpath) (remotePath localpath)
    upload local = Rsync.sendFile Debian.rsync (FS.PreExisting local) remote distpath
    distpath = "tmp/"
    remotePath local = distpath </> takeFileName local

microDNS :: BinaryDir -> Tracked' FilePath
microDNS bindir =
    cabalRepoBuild
        "microdns"
        bindir
        realNoop
        "microdns"
        "microdns"
        (Git.Remote "https://github.com/lucasdicioccio/microdns.git")
        "main"
        ""
        []

kitchenSink :: BinaryDir -> Tracked' FilePath
kitchenSink bindir =
    cabalRepoBuild
        "kitchensink"
        bindir
        realNoop
        "exe:kitchen-sink"
        "kitchen-sink"
        (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
        "main"
        "hs"
        []

kitchenSink_dev :: Tracked' FilePath
kitchenSink_dev =
    cabalRepoBuild
        "kitchensink-dev"
        optBuildsBindir
        realNoop
        "exe:kitchen-sink"
        "kitchen-sink"
        (Git.Remote "/home/lucasdicioccio/code/opensource/kitchen-sink")
        "main"
        "hs"
        []

postgrest :: Tracked' FilePath
postgrest =
    cabalRepoBuild
        "postgrest"
        optBuildsBindir
        (Debian.deb $ Debian.Package "libpq-dev")
        "exe:postgrest"
        "postgrest"
        (Git.Remote "https://github.com/PostgREST/postgrest.git")
        "main"
        ""
        []

type CloneDir = Text
type BinaryDir = FilePath
type CabalTarget = Text
type CabalBinaryName = Text -- may vary from target when exe: or lib:  are prepended
type BranchName = Text
type SubDir = FilePath -- subdir where we can cabal build

optBuildsBindir :: BinaryDir
optBuildsBindir = "/opt/builds/bin"

-- builds a cabal repository
cabalRepoBuild ::
    CloneDir ->
    BinaryDir ->
    Op ->
    CabalTarget ->
    CabalBinaryName ->
    Git.Remote ->
    BranchName ->
    SubDir ->
    Cabal.CabalFlags ->
    Tracked' FilePath
cabalRepoBuild dirname bindir sysdeps target binname remote branch subdir flags =
    Tracked (Track $ const $ op `inject` sysdeps) binpath
  where
    op = FS.withFile (Git.repofile mkrepo repo subdir) $ \repopath ->
        Cabal.install cabal flags (Cabal.Cabal repopath target) bindir
    binpath = bindir </> Text.unpack binname
    repo = Git.Repo "./git-repos/" dirname remote (Git.Branch branch)
    git = Debian.git
    cabal = (Track $ \_ -> noop "preinstalled")
    mkrepo = Track $ Git.repo git
