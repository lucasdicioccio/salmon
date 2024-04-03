module SreBox.CabalBuilding where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (takeFileName, (</>))

import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Cabal as Cabal
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------
data Report
    = Upload !FilePath !Rsync.Report
    | CallCabal !CabalTarget !Cabal.Report
    | CallGit !CabalTarget !Git.Report
    deriving (Show)

isBuildSuccess :: Report -> Bool
isBuildSuccess r = case r of
    (CallCabal _ (Cabal.CabalBuild _ cmd)) ->
        Binary.isCommandSuccessful cmd
    (CallCabal _ (Cabal.CabalInstall _ cmd)) ->
        Binary.isCommandSuccessful cmd
    otherwise -> False

-------------------------------------------------------------------------------

cabalBinUpload :: Reporter Report -> Tracked' FilePath -> Rsync.Remote -> Tracked' FilePath
cabalBinUpload r mkbin remote =
    mkbin `bindTracked` go
  where
    go localpath =
        Tracked (Track $ const $ upload localpath) (remotePath localpath)
    upload local = Rsync.sendFile (r' local) Debian.rsync (FS.PreExisting local) remote distpath
    r' local = contramap (Upload local) r
    distpath = "tmp/"
    remotePath local = distpath </> takeFileName local

microDNS :: Reporter Report -> BinaryDir -> Tracked' FilePath
microDNS r bindir =
    cabalRepoBuild
        r
        "microdns"
        bindir
        realNoop
        "microdns"
        "microdns"
        (Git.Remote "https://github.com/lucasdicioccio/microdns.git")
        "main"
        ""
        []

kitchenSink :: Reporter Report -> BinaryDir -> Tracked' FilePath
kitchenSink r bindir =
    cabalRepoBuild
        r
        "kitchensink"
        bindir
        realNoop
        "exe:kitchen-sink"
        "kitchen-sink"
        (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
        "main"
        "hs"
        []

kitchenSink_dev :: Reporter Report -> Tracked' FilePath
kitchenSink_dev r =
    cabalRepoBuild
        r
        "kitchensink-dev"
        optBuildsBindir
        realNoop
        "exe:kitchen-sink"
        "kitchen-sink"
        (Git.Remote "/home/lucasdicioccio/code/opensource/kitchen-sink")
        "main"
        "hs"
        []

postgrest :: Reporter Report -> Tracked' FilePath
postgrest r =
    cabalRepoBuild
        r
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

-- builds a binary in a cabal repository
cabalRepoBuild ::
    Reporter Report ->
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
cabalRepoBuild r dirname bindir sysdeps target binname remote branch subdir flags =
    Tracked (Track $ const $ op `inject` sysdeps) binpath
  where
    op = FS.withFile (Git.repofile mkrepo repo subdir) $ \repopath ->
        Cabal.install (contramap (CallCabal target) r) cabal flags (Cabal.Cabal repopath target) bindir
    binpath = bindir </> Text.unpack binname
    repo = Git.Repo "./git-repos/" dirname remote (Git.Branch branch)
    git = Debian.git
    cabal = (Track $ \_ -> noop "preinstalled")
    mkrepo = Track $ Git.repo (contramap (CallGit target) r) git

-- builds a library in a cabal repository
cabalRepoOnlyBuild ::
    Reporter Report ->
    CloneDir ->
    BinaryDir ->
    Op ->
    CabalTarget ->
    Git.Remote ->
    BranchName ->
    SubDir ->
    Cabal.CabalFlags ->
    Op
cabalRepoOnlyBuild r dirname bindir sysdeps target remote branch subdir flags =
    op `inject` sysdeps
  where
    op = FS.withFile (Git.repofile mkrepo repo subdir) $ \repopath ->
        Cabal.build (contramap (CallCabal target) r) cabal flags (Cabal.Cabal repopath target)
    repo = Git.Repo "./git-repos/" dirname remote (Git.Branch branch)
    git = Debian.git
    cabal = (Track $ \_ -> noop "preinstalled")
    mkrepo = Track $ Git.repo (contramap (CallGit target) r) git
