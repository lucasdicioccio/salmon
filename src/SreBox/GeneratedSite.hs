module SreBox.GeneratedSite where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))
import System.Process.ListLike (cwd, proc)

import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Git as Git
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import qualified SreBox.CabalBuilding as CabalBuilding

-------------------------------------------------------------------------------
data Report
    = CallGit !SiteName !Git.Report
    deriving (Show)

-------------------------------------------------------------------------------
type SiteName = Text

-------------------------------------------------------------------------------

data GenSite
    = GenSite
    { genSiteBuildDir :: FilePath
    , genSiteName :: SiteName
    , genSiteSourceRepo :: Git.Repo
    , genSiteSourceRepoSubDir :: FilePath
    , genSiteRepo :: Git.Repo
    , genSiteAuthor :: Git.Author
    , genSiteCommitMessage :: Git.CommitMessage
    , genSiteTagName :: Git.TagName
    , genSiteBuild :: Track' FilePath
    }

generateSite :: Reporter Report -> GenSite -> Op
generateSite r genSite =
    op "github-site" (deps [pushTheBlog]) $ \actions ->
        actions
            { help = mconcat ["builds a KitchenSink-generated GitHub site for ", genSite.genSiteName]
            , ref = dotRef $ "gh-site:" <> genSite.genSiteName
            }
  where
    r' = contramap (CallGit genSite.genSiteName) r

    blogrepo :: Git.Repo
    blogrepo = genSite.genSiteRepo

    remoteToCloneFrom, remoteToPushTo :: Git.Remote
    remoteToCloneFrom = blogrepo.repoRemote
    remoteToPushTo = blogrepo.repoRemote

    pushTheBlog :: Op
    pushTheBlog =
        Git.push r' Debian.git mkrepo blogrepo remoteToPushTo (Git.RemoteName "localclone") changes

    salmonAuthor :: Git.Author
    salmonAuthor = genSite.genSiteAuthor

    commitSomeChange :: Op
    commitSomeChange =
        Git.commit r' Debian.git mkrepo ignoreTrack blogrepo salmonAuthor genSite.genSiteCommitMessage makeChanges

    makeChanges :: Op
    makeChanges =
        Git.addfiles r' Debian.git mkrepo blogrepo [wholecontent]

    wholecontent :: FS.File "change"
    wholecontent = FS.Generated genSite.genSiteBuild (Git.clonedir blogrepo)

    changes :: Op
    changes = op "changes-to-push" (deps [tagTheBlog `inject` commitSomeChange]) $ \actions ->
        actions
            { ref = dotRef $ "gh-site:changes:" <> genSite.genSiteName
            }

    tagTheBlog :: Op
    tagTheBlog =
        Git.tag r' Debian.git mkrepo blogrepo genSite.genSiteTagName Nothing

    mkrepo :: Track' Git.Repo
    mkrepo = Track $ \r -> Git.repoFull r' Debian.git r

-------------------------------------------------------------------------------

data Report_KS
    = GenerateSite !Report
    | BuildKS !CabalBuilding.Report
    | ProduceSite !SiteName !Binary.Report
    | FetchSources !SiteName !Git.Report
    deriving (Show)

data GenKitchenSinkSite
    = GenKitchenSinkSite
    { genKitchenSinkSiteBuildDir :: FilePath
    , genKitchenSinkSiteName :: Text
    , genKitchenSinkSiteSourceRepo :: Git.Repo
    , genKitchenSinkSiteSourceRepoSubDir :: FilePath
    , genKitchenSinkSiteRepo :: Git.Repo
    , genKitchenSinkSiteAuthor :: Git.Author
    , genKitchenSinkSiteCommitMessage :: Git.CommitMessage
    , genKitchenSinkSiteTagName :: Git.TagName
    }

generateKitchenSinkSite :: Reporter Report_KS -> GenKitchenSinkSite -> Op
generateKitchenSinkSite r base =
    generateSite (contramap GenerateSite r) $
        GenSite
            base.genKitchenSinkSiteBuildDir
            base.genKitchenSinkSiteName
            base.genKitchenSinkSiteSourceRepo
            base.genKitchenSinkSiteSourceRepoSubDir
            base.genKitchenSinkSiteRepo
            base.genKitchenSinkSiteAuthor
            base.genKitchenSinkSiteCommitMessage
            base.genKitchenSinkSiteTagName
            (Track build)
  where
    blogrepo :: Git.Repo
    blogrepo = base.genKitchenSinkSiteRepo

    localKSPath :: FilePath
    localKSPath = base.genKitchenSinkSiteBuildDir </> "ci/bin/ks"

    callks :: Binary.Command "ks" (FilePath, FilePath, FilePath, FilePath)
    callks =
        Binary.Command $ \(path, builddir, srcdir, outdir) -> (proc path ["produce", "--outDir", outdir, "--srcDir", srcdir]){cwd = Just builddir}

    build :: FilePath -> Op
    build outdir =
        using (CabalBuilding.kitchenSink (contramap BuildKS r) localKSPath) $ \kspath ->
            using (Git.repodir mkrepo base.genKitchenSinkSiteSourceRepo "") $ \srcdir ->
                let ksargs = (kspath, srcdir.directoryPath, srcdir.directoryPath </> "site-src", outdir)
                 in Binary.withBinary ignoreTrack callks ksargs $ \buildBlog ->
                        op "gen-ks-site" nodeps $ \actions ->
                            actions
                                { up = buildBlog (contramap (ProduceSite base.genKitchenSinkSiteName) r)
                                , ref = dotRef $ "gen-ks-site:build:" <> (Text.pack $ show ksargs)
                                }

    mkrepo :: Track' Git.Repo
    mkrepo = Track $ \repo -> Git.repoFull (contramap (FetchSources base.genKitchenSinkSiteName) r) Debian.git repo
