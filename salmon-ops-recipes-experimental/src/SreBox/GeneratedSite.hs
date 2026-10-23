{-# LANGUAGE ScopedTypeVariables #-}

module SreBox.GeneratedSite where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, cwd, proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
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
            , ref = mkRef "gh-site" genSite.genSiteName
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
            { ref = mkRef "gh-site-changes" genSite.genSiteName
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
        using2
            (CabalBuilding.kitchenSink (contramap BuildKS r) localKSPath)
            (Git.repodir mkrepo base.genKitchenSinkSiteSourceRepo "")
            $ \(kspath, srcdir) ->
                let ksargs = (kspath, srcdir.directoryPath, srcdir.directoryPath </> "site-src", outdir)
                 in Binary.withBinary ignoreTrack callks ksargs $ \buildBlog ->
                        op "gen-ks-site" nodeps $ \actions ->
                            actions
                                { up = buildBlog (contramap (ProduceSite base.genKitchenSinkSiteName) r)
                                , ref = mkRef "gen-ks-site-build" ksargs
                                }

    mkrepo :: Track' Git.Repo
    mkrepo = Track $ \repo -> Git.repoFull (contramap (FetchSources base.genKitchenSinkSiteName) r) Debian.git repo

-------------------------------------------------------------------------------
-- Auto-publish: re-produce a site when its source moves, push only a change.

{- $autoPublish
'generateSite' is a one-shot pipeline: every node of it runs on every
@run up@ and none has a @check@, so under @run serve@ each one parks after
its first pass and nothing ever notices the source repository moving.

'autoPublishSite' folds the produce-and-publish steps into one node, so that
a single @check@ can stand for all of them:

* @check@ asks the source remote for its branch head (@git ls-remote@) and
  compares it with the source revision this node last published, kept in a
  stamp file under 'autoPublishStateDir' (in neither repository). Equal is
  'Success'; different, or no stamp yet, is 'Failure'; a remote that cannot
  be asked is 'Unknown', which restarts nothing under @serve@. That check is
  the freshness trigger: @serve@'s upkeep asks it on its doubling delay (60s
  cap) and re-runs @up@ when it fails.
* @up@ pulls both clones (fast-forward only), runs the producer from the
  source clone into the output clone, stages everything (@git add -A@, so a
  page that is gone is removed), commits only when @git status --porcelain@
  shows a change, and pushes only when the output's local head is not the
  remote's. The branch push is not forced; only the moved tag is. The stamp
  is written last, so a failed push is retried on the next pass.

Nothing here discards the output repository's history: a remote that moved
under it makes the pull or the push fail, which is reported and retried.
-}

data Report_Publish
    = PublishFetch !SiteName !Git.Report
    | PublishGit !SiteName !Binary.Report
    | PublishProduce !SiteName !Binary.Report
    | PublishOutcome !SiteName !PublishOutcome
    deriving (Show)

data PublishOutcome
    = -- | the producer wrote what is already committed
      OutputUnchanged !Text
    | -- | a commit was made for this source revision
      OutputCommitted !Text
    | -- | the output branch was pushed at this head
      OutputPushed !Text
    | -- | the output remote already had this head
      OutputAlreadyPushed !Text
    deriving (Show, Eq)

data AutoPublish
    = AutoPublish
    { autoPublishName :: SiteName
    , autoPublishStateDir :: FilePath
    -- ^ holds the stamp of the last published source revision
    , autoPublishGit :: Track' (Binary "git")
    , autoPublishSourceRepo :: Git.Repo
    , autoPublishOutputRepo :: Git.Repo
    , autoPublishAuthor :: Git.Author
    , autoPublishCommitMessage :: Git.CommitMessage
    , autoPublishTagName :: Maybe Git.TagName
    , autoPublishPrereqs :: [Op]
    -- ^ what the producer needs in place first (e.g. its binary built)
    , autoPublishProduce :: FilePath -> FilePath -> CreateProcess
    -- ^ given the source clone and the output clone, writes the site
    }

-- | Where 'autoPublishSite' records the source revision it last published.
autoPublishStampFile :: AutoPublish -> FilePath
autoPublishStampFile ap = ap.autoPublishStateDir </> (Text.unpack ap.autoPublishName <> ".published-source")

autoPublishSite :: Reporter Report_Publish -> AutoPublish -> Op
autoPublishSite r ap =
    op "auto-publish-site" (deps prereqs) $ \actions ->
        actions
            { help = mconcat ["re-produces ", ap.autoPublishName, " when its source moves and pushes only a changed output"]
            , notes =
                [ "source clone: " <> Text.pack srcdir
                , "output clone: " <> Text.pack outdir
                , "stamp: " <> Text.pack stamp
                ]
            , ref = mkRef "auto-publish-site" (ap.autoPublishName, outdir)
            , check = checkFresh
            , up = publish
            }
  where
    name = ap.autoPublishName
    rgit = contramap (PublishGit name) r
    outcome = runReporter r . PublishOutcome name

    srcrepo, outrepo :: Git.Repo
    srcrepo = ap.autoPublishSourceRepo
    outrepo = ap.autoPublishOutputRepo

    srcdir, outdir, stamp :: FilePath
    srcdir = Git.clonedir srcrepo
    outdir = Git.clonedir outrepo
    stamp = autoPublishStampFile ap

    prereqs :: [Op]
    prereqs =
        [ Binary.justInstall ap.autoPublishGit
        , FS.dir (FS.Directory ap.autoPublishStateDir)
        , Git.repoFull (contramap (PublishFetch name) r) ap.autoPublishGit srcrepo
        , Git.repoFull (contramap (PublishFetch name) r) ap.autoPublishGit outrepo
        ]
            <> ap.autoPublishPrereqs

    git :: PublishGitCommand -> IO ()
    git cmd = Binary.untrackedExec publishGit cmd "" rgit

    gitOut :: PublishGitCommand -> IO ByteString.ByteString
    gitOut cmd = Binary.untrackedExecOutput publishGit cmd "" rgit

    remoteHead :: Git.Repo -> IO (Maybe Text)
    remoteHead repo =
        parseLsRemote repo.repoBranch <$> gitOut (LsRemote repo.repoRemote repo.repoBranch)

    checkFresh :: IO CheckResult
    checkFresh = do
        published <- readStamp stamp
        upstream <- try (remoteHead srcrepo)
        pure $ case upstream of
            Left (_ :: SomeException) -> Unknown
            Right h -> freshness published h

    publish :: IO ()
    publish = do
        git (PullBranch srcdir srcrepo.repoRemote srcrepo.repoBranch)
        git (PullBranch outdir outrepo.repoRemote outrepo.repoBranch)
        rev <- revText <$> gitOut (RevParseHead srcdir)
        Binary.untrackedExec (Binary.Command (uncurry ap.autoPublishProduce)) (srcdir, outdir) "" (contramap (PublishProduce name) r)
        git (AddAll outdir)
        status <- gitOut (StatusPorcelain outdir)
        if outputChanged status
            then do
                git (CommitAll outdir ap.autoPublishAuthor (publishCommitMessage ap.autoPublishCommitMessage rev))
                traverse_ (git . ForceTag outdir) ap.autoPublishTagName
                outcome (OutputCommitted rev)
            else outcome (OutputUnchanged rev)
        local <- revText <$> gitOut (RevParseHead outdir)
        upstream <- remoteHead outrepo
        if needsPush local upstream
            then do
                git (PushBranch outdir outrepo.repoRemote outrepo.repoBranch)
                traverse_ (git . PushTag outdir outrepo.repoRemote) ap.autoPublishTagName
                outcome (OutputPushed local)
            else outcome (OutputAlreadyPushed local)
        writeFile stamp (Text.unpack rev <> "\n")

{- | 'autoPublishSite' for a kitchen-sink site: builds kitchen-sink as
'generateKitchenSinkSite' does, and runs @produce@ from the source clone's
@site-src@ into the output clone.
-}
autoPublishKitchenSinkSite :: Reporter Report_KS -> Reporter Report_Publish -> GenKitchenSinkSite -> Op
autoPublishKitchenSinkSite rks r base =
    autoPublishSite r $
        AutoPublish
            { autoPublishName = base.genKitchenSinkSiteName
            , autoPublishStateDir = base.genKitchenSinkSiteBuildDir </> "auto-publish"
            , autoPublishGit = Debian.git
            , autoPublishSourceRepo = base.genKitchenSinkSiteSourceRepo
            , autoPublishOutputRepo = base.genKitchenSinkSiteRepo
            , autoPublishAuthor = base.genKitchenSinkSiteAuthor
            , autoPublishCommitMessage = base.genKitchenSinkSiteCommitMessage
            , autoPublishTagName = Just base.genKitchenSinkSiteTagName
            , autoPublishPrereqs = [run ksTrack kspath]
            , autoPublishProduce = \src out ->
                (proc kspath ["produce", "--outDir", out, "--srcDir", src </> "site-src"]){cwd = Just src}
            }
  where
    ks :: Tracked' FilePath
    ks = CabalBuilding.kitchenSink (contramap BuildKS rks) (base.genKitchenSinkSiteBuildDir </> "ci/bin/ks")
    ksTrack = ks.track
    kspath = ks.obj

-------------------------------------------------------------------------------
-- The decisions, pure.

-- | The head @git ls-remote REMOTE refs/heads/BRANCH@ printed for that branch.
parseLsRemote :: Git.Branch -> ByteString.ByteString -> Maybe Text
parseLsRemote branch out =
    case [sha | [sha, ref'] <- fmap Text.words (Text.lines (Text.decodeUtf8Lenient out)), ref' == wanted] of
        (sha : _) -> Just sha
        [] -> Nothing
  where
    wanted = "refs/heads/" <> branch.getBranch

-- | The last published source revision, against the source remote's head.
freshness :: Maybe Text -> Maybe Text -> CheckResult
freshness Nothing _ = Failure "never published"
freshness (Just _) Nothing = Unknown
freshness (Just published) (Just upstream)
    | published == upstream = Success
    | otherwise = Failure (mconcat ["source moved from ", published, " to ", upstream])

-- | Whether @git status --porcelain@ shows anything to commit.
outputChanged :: ByteString.ByteString -> Bool
outputChanged = not . Text.null . Text.strip . Text.decodeUtf8Lenient

-- | Whether the local head is somewhere the remote branch is not.
needsPush :: Text -> Maybe Text -> Bool
needsPush local upstream = Just local /= upstream

-- | The declared message, with the source revision it was produced from.
publishCommitMessage :: Git.CommitMessage -> Text -> Text
publishCommitMessage msg rev =
    Text.unlines [Text.stripEnd (Git.commitMessage msg), "", "Salmon-Source-Revision: " <> rev]

revText :: ByteString.ByteString -> Text
revText = Text.strip . Text.decodeUtf8Lenient

readStamp :: FilePath -> IO (Maybe Text)
readStamp path = do
    present <- doesFileExist path
    if present
        then do
            t <- Text.strip . Text.decodeUtf8Lenient <$> ByteString.readFile path
            pure (if Text.null t then Nothing else Just t)
        else pure Nothing

-------------------------------------------------------------------------------

data PublishGitCommand
    = PullBranch FilePath Git.Remote Git.Branch
    | RevParseHead FilePath
    | LsRemote Git.Remote Git.Branch
    | AddAll FilePath
    | StatusPorcelain FilePath
    | CommitAll FilePath Git.Author Text
    | ForceTag FilePath Git.TagName
    | PushBranch FilePath Git.Remote Git.Branch
    | PushTag FilePath Git.Remote Git.TagName

publishGit :: Binary.Command "git" PublishGitCommand
publishGit = Binary.Command $ \cmd -> case cmd of
    PullBranch dir remote branch -> inDir dir ["pull", "--ff-only", Text.unpack remote.getRemote, Text.unpack branch.getBranch]
    RevParseHead dir -> inDir dir ["rev-parse", "HEAD"]
    LsRemote remote branch -> proc "git" ["ls-remote", Text.unpack remote.getRemote, "refs/heads/" <> Text.unpack branch.getBranch]
    AddAll dir -> inDir dir ["add", "-A", "."]
    StatusPorcelain dir -> inDir dir ["status", "--porcelain"]
    CommitAll dir author msg -> inDir dir ["commit", "--author", Text.unpack author.getAuthor, "-m", Text.unpack msg]
    ForceTag dir tag -> inDir dir ["tag", "-f", Text.unpack tag.getTagName]
    PushBranch dir remote branch -> inDir dir ["push", Text.unpack remote.getRemote, "HEAD:refs/heads/" <> Text.unpack branch.getBranch]
    PushTag dir remote tag -> inDir dir ["push", "-f", Text.unpack remote.getRemote, "refs/tags/" <> Text.unpack tag.getTagName]
  where
    inDir dir args = (proc "git" args){cwd = Just dir}
