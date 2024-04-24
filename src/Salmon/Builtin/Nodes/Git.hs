module Salmon.Builtin.Nodes.Git where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath (makeRelative, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = CloneRepo !Repo !Binary.Report
    | PullRepo !Repo !Binary.Report
    | AddFileChanges !Repo !([FilePath]) !Binary.Report
    | ApplyTag !TagName !Repo !Binary.Report
    | SkippedApplyingTag !String !TagName !Repo
    | ApplyCommit !Headline !Repo !Binary.Report
    | PushRepo !Repo !Remote !Binary.Report
    | DefineRemote !Repo !RemoteName !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
newtype Remote = Remote {getRemote :: Text}
    deriving (Eq, Ord, Show)

newtype Branch = Branch {getBranch :: Text}
    deriving (Eq, Ord, Show)

data Repo = Repo {repoClonedir :: FilePath, repoLocalName :: Text, repoRemote :: Remote, repoBranch :: Branch}
    deriving (Eq, Ord, Show)

clonedir :: Repo -> FilePath
clonedir r = r.repoClonedir </> Text.unpack r.repoLocalName

-- | Clones a repository.
repo :: Reporter Report -> Track' (Binary "git") -> Repo -> Op
repo r git repository =
    withBinary git gitDLcommand (Clone remote branch (clonedir repository)) $ \clone ->
        withBinary git gitDLcommand (Pull remote branch (clonedir repository)) $ \pull ->
            op "git-repo" (deps [enclosingdir]) $ \actions ->
                actions
                    { help = "clones and force sync a repo"
                    , ref = dotRef $ "repo:" <> Text.pack (clonedir repository)
                    , up = clone r1' >> pull r2'
                    }
  where
    r1' = contramap (CloneRepo repository) r
    r2' = contramap (PullRepo repository) r
    cloneparentdir :: FilePath
    cloneparentdir = repository.repoClonedir

    remote :: Remote
    remote = repository.repoRemote

    branch :: Branch
    branch = repository.repoBranch

    enclosingdir :: Op
    enclosingdir = dir (Directory cloneparentdir)

data GitDownloadCommand
    = Clone Remote Branch FilePath
    | Pull Remote Branch FilePath

gitDLcommand :: Command "git" GitDownloadCommand
gitDLcommand = Command $ \cmd -> case cmd of
    (Clone repo branch localdir) ->
        proc
            "git"
            [ "clone"
            , "-b"
            , Text.unpack branch.getBranch
            , "--depth"
            , "1"
            , Text.unpack repo.getRemote
            , localdir
            ]
    (Pull repo branch dir) ->
        ( proc
            "git"
            [ "pull"
            , Text.unpack repo.getRemote
            , Text.unpack branch.getBranch
            ]
        )
            { cwd = Just dir
            }

-------------------------------------------------------------------------------

addfiles ::
    Reporter Report ->
    Track' (Binary "git") ->
    Track' Repo ->
    Repo ->
    [File "change"] ->
    Op
addfiles _ _ _ _ [] = noop "git-add-nothing"
addfiles r git mkrepo repository files =
    withBinary git gitModCommand (AddFiles (clonedir repository) paths) $ \up ->
        op "git-add" (deps filechanges) $ \actions ->
            actions
                { help = Text.unwords ["add", Text.pack (show (length files)), "in repo", repository.repoLocalName]
                , notes = fmap Text.pack paths
                , ref = dotRef $ "git-add:" <> Text.pack (concat paths)
                , up = up (contramap (AddFileChanges repository paths) r)
                }
  where
    filechanges :: [Op]
    filechanges = fmap (\x -> fileOp x `inject` run mkrepo repository) files
    paths :: [FilePath]
    paths = fmap (\x -> makeRelative (clonedir repository) (getFilePath x)) files

commit ::
    Reporter Report ->
    Track' (Binary "git") ->
    Track' Repo ->
    Track' Author ->
    Repo ->
    Author ->
    CommitMessage ->
    Op ->
    Op
commit r git mkrepo mkauthor repository author msg modrepo =
    withBinary git gitModCommand (Commit (clonedir repository) author msg) $ \up ->
        op "git-commit" (deps [run mkauthor author, repochange]) $ \actions ->
            actions
                { help = Text.unwords ["commits", headline.getHeadline, "on repo", repository.repoLocalName]
                , ref = dotRef $ "commit:" <> headline.getHeadline <> repository.repoLocalName
                , up = up (contramap (ApplyCommit headline repository) r)
                }
  where
    headline :: Headline
    headline = msg.commitHeadline

    repochange :: Op
    repochange = modrepo `inject` run mkrepo repository

tag ::
    Reporter Report ->
    Track' (Binary "git") ->
    Track' Repo ->
    Repo ->
    TagName ->
    Maybe Message ->
    Op
tag r git mkrepo repository name msg =
    withBinary git gitModCommand (VerifyClean (clonedir repository)) $ \checkClean ->
        withBinary git gitModCommand (Tag (clonedir repository) name msg) $ \f ->
            op "git-tag" (deps [run mkrepo repository]) $ \actions ->
                actions
                    { help = Text.unwords ["apply git tag", name.getTagName, "on repo", repository.repoLocalName]
                    , ref = dotRef $ "tag:" <> name.getTagName <> repository.repoLocalName
                    , up = do
                        checkClean (reportBoth r' (applyTagOnCleanRepository (f r')))
                    }
  where
    r' = contramap (ApplyTag name repository) r

    reportSkip txt = runReporter r (SkippedApplyingTag (ByteString.unpack txt) name repository)

    applyTagOnCleanRepository :: IO () -> Reporter Binary.Report
    applyTagOnCleanRepository applyTag = ReporterM go
      where
        go br = case br of
            Binary.Requested _ brr -> go brr
            Binary.CommandSuccess out _ ->
                if ByteString.null out
                    then applyTag
                    else reportSkip out
            Binary.CommandStopped _ _ _ err ->
                reportSkip err
            otherwise -> pure ()

remote ::
    Reporter Report ->
    Track' (Binary "git") ->
    Track' Repo ->
    Repo ->
    RemoteName ->
    Remote ->
    Op
remote r git mkrepo repository name remote =
    withBinary git gitModCommand (AddRemote (clonedir repository) name remote) $ \up ->
        op "git-add-remote" (deps [run mkrepo repository]) $ \actions ->
            actions
                { help = Text.unwords ["add remote", name.getRemoteName, "on repo", repository.repoLocalName]
                , ref = dotRef $ "remote:" <> name.getRemoteName <> repository.repoLocalName
                , up = up (contramap (DefineRemote repository name) r)
                }

newtype Headline = Headline {getHeadline :: Text}
    deriving (Eq, Ord, Show)

type Message = Text

data CommitMessage
    = CommitMessage
    { commitHeadline :: !Headline
    , commitBody :: !Message
    }
    deriving (Eq, Ord, Show)

commitMessage :: CommitMessage -> Text
commitMessage (CommitMessage h b) = Text.unlines [h.getHeadline, b]

newtype TagName = TagName {getTagName :: Text}
    deriving (Eq, Ord, Show)

newtype RemoteName = RemoteName {getRemoteName :: Text}
    deriving (Eq, Ord, Show)

newtype Author = Author {getAuthor :: Text}
    deriving (Eq, Ord, Show)

data GitModifyCommand
    = AddFiles FilePath [FilePath]
    | Commit FilePath Author CommitMessage
    | Tag FilePath TagName (Maybe Message)
    | AddRemote FilePath RemoteName Remote
    | VerifyClean FilePath

gitModCommand :: Command "git" GitModifyCommand
gitModCommand = Command $ \cmd -> case cmd of
    (AddFiles dir paths) ->
        ( proc
            "git"
            ("add" : paths)
        )
            { cwd = Just dir
            }
    (AddRemote dir name spec) ->
        ( proc
            "git"
            [ "remote"
            , "add"
            , Text.unpack name.getRemoteName
            , Text.unpack spec.getRemote
            ]
        )
            { cwd = Just dir
            }
    (Commit dir author msg) ->
        ( proc
            "git"
            [ "commit"
            , "--author"
            , Text.unpack author.getAuthor
            , "-a"
            , "-m"
            , Text.unpack (commitMessage msg)
            ]
        )
            { cwd = Just dir
            }
    (Tag dir name Nothing) ->
        ( proc
            "git"
            [ "tag"
            , "-f"
            , Text.unpack name.getTagName
            ]
        )
            { cwd = Just dir
            }
    (Tag dir name (Just msg)) ->
        ( proc
            "git"
            [ "tag"
            , "-f"
            , Text.unpack name.getTagName
            , "-m"
            , Text.unpack msg
            ]
        )
            { cwd = Just dir
            }
    (VerifyClean dir) ->
        ( proc
            "git"
            [ "status"
            , "--untracked-files=no"
            , "--porcelain"
            ]
        )
            { cwd = Just dir
            }

-------------------------------------------------------------------------------

push ::
    Reporter Report ->
    Track' (Binary "git") ->
    Track' Repo ->
    -- | the local branch to push is taken from this repo
    Repo ->
    -- | the remote to push to
    Remote ->
    -- | name given to the remote we push to (e.g., "origin")
    RemoteName ->
    Op ->
    Op
push r git mkrepo repository remoteSpec remotename modrepo =
    withBinary git gitULCommand (Push (clonedir repository) remoteSpec branch) $ \up ->
        op "git-push" (deps [referenceRemote, repochange]) $ \actions ->
            actions
                { help = Text.unwords ["pushes", branch.getBranch, "of repo", repository.repoLocalName, "to", remoteSpec.getRemote]
                , ref = dotRef $ "push:" <> repository.repoLocalName <> remoteSpec.getRemote <> branch.getBranch
                , up = up (contramap (PushRepo repository remoteSpec) r)
                }
  where
    repochange :: Op
    repochange = modrepo `inject` run mkrepo repository
    branch :: Branch
    branch = repository.repoBranch
    referenceRemote = remote r git mkrepo repository remotename remoteSpec

data GitULCommand
    = Push FilePath Remote Branch

gitULCommand :: Command "git" GitULCommand
gitULCommand = Command $ \cmd -> case cmd of
    (Push dir remote branch) ->
        ( proc
            "git"
            [ "push"
            , "-f"
            , "--tags"
            , Text.unpack remote.getRemote
            , Text.unpack branch.getBranch
            ]
        )
            { cwd = Just dir
            }

-------------------------------------------------------------------------------

-- | Provides a file from an existing repository.
repofile :: Track' Repo -> Repo -> FilePath -> File a
repofile t r sub =
    let
        path = clonedir r </> sub
     in
        Generated mkPath path
  where
    mkPath :: Track' FilePath
    mkPath = Track $ \_ -> run t r

-- | Provides a Directory from an existing repository.
repodir :: Track' Repo -> Repo -> FilePath -> Tracked' Directory
repodir t r sub =
    let
        path = clonedir r </> sub
     in
        Tracked mkPath (Directory path)
  where
    mkPath :: Track' a
    mkPath = Track $ \_ -> run t r
