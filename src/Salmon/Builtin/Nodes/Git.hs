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
import System.Process.ListLike (CreateProcess(..), proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

newtype Remote = Remote { getRemote :: Text }
  deriving (Eq, Ord, Show)

newtype Branch = Branch { getBranch :: Text }
  deriving (Eq, Ord, Show)

data Repo = Repo { repoClonedir :: FilePath , repoLocalName :: Text, repoRemote :: Remote, repoBranch :: Branch }
  deriving (Eq, Ord, Show)

clonedir :: Repo -> FilePath
clonedir r = r.repoClonedir </> Text.unpack r.repoLocalName

-- | Clones a repository.
repo :: Track' (Binary "git") -> Repo -> Op
repo git r =
  withBinary git gitcommand (Clone remote branch (clonedir r)) $ \clone -> 
  withBinary git gitcommand (Pull remote branch (clonedir r)) $ \pull -> 
    op "git-repo" (deps [enclosingdir]) $ \actions -> actions {
        help = "clones and force sync a repo"
      , ref = dotRef $ "repo:" <> Text.pack (clonedir r)
      , up = clone >> pull
      }
  where

    cloneparentdir :: FilePath
    cloneparentdir = r.repoClonedir

    remote :: Remote
    remote = r.repoRemote

    branch :: Branch
    branch = r.repoBranch

    enclosingdir :: Op
    enclosingdir = dir (Directory cloneparentdir)

data GitCommand
  = Clone Remote Branch FilePath
  | Pull Remote Branch FilePath

gitcommand :: Command "git" GitCommand
gitcommand = Command $ \cmd -> case cmd of
  (Clone repo branch localdir) ->
    proc "git"
      [ "clone"
      , "-b"
      , Text.unpack branch.getBranch
      , "--depth"
      , "1"
      , Text.unpack repo.getRemote
      , localdir
      ]
  (Pull repo branch dir) ->
    (proc "git"
      [ "pull"
      , Text.unpack repo.getRemote
      , Text.unpack branch.getBranch
      ]) { cwd = Just dir }

-------------------------------------------------------------------------------

-- | Provides a file from an existing repository.
repofile :: Track' Repo -> Repo -> FilePath -> File a
repofile t r sub =
  let
    path = clonedir r </> sub
  in Generated mkPath path

  where
    mkPath :: Track' FilePath
    mkPath = Track $ \_ -> run t r

-- | Provides a Directory from an existing repository.
repodir :: Track' Repo -> Repo -> FilePath -> Tracked' Directory
repodir t r sub =
  let
    path = clonedir r </> sub
  in Tracked mkPath (Directory path)

  where
    mkPath :: Track' a
    mkPath = Track $ \_ -> run t r
