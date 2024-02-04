module Salmon.Builtin.Nodes.Git where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Track
import Salmon.Builtin.Nodes.Binary

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

newtype Remote = Remote { getRemote :: Text }
  deriving (Eq, Ord, Show)

newtype Branch = Branch { getBranch :: Text }
  deriving (Eq, Ord, Show)

data Repo = Repo { repoClonedir :: FilePath , repoLocalName :: Text, repoRemote :: Remote, repoBranch :: Branch }
  deriving (Eq, Ord, Show)

repo :: Track' (Binary "git") -> Repo -> Op
repo git r =
  using git gitclone (Clone remote branch clonedir) $ \up -> 
    op "git-repo" (deps [enclosingdir]) $ \actions -> actions {
        help = "clones and force sync a repo"
      , ref = dotRef $ "repo:" <> Text.pack clonedir
      , up = up
      }
  where

    clonedir :: FilePath
    clonedir = r.repoClonedir </> Text.unpack r.repoLocalName

    cloneparentdir :: FilePath
    cloneparentdir = r.repoClonedir

    remote :: Remote
    remote = r.repoRemote

    branch :: Branch
    branch = r.repoBranch

    enclosingdir :: Op
    enclosingdir = dir (Directory cloneparentdir)

data Clone = Clone Remote Branch FilePath

gitclone :: Command "git" Clone
gitclone = Command $ \(Clone repo branch localdir) ->
  proc "git"
    [ "clone"
    , "-b"
    , Text.unpack branch.getBranch
    , "--depth"
    , "1"
    , Text.unpack repo.getRemote
    , localdir
    ]
