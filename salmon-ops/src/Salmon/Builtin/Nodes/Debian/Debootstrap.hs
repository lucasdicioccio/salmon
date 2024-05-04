module Salmon.Builtin.Nodes.Debian.Debootstrap where

import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Builtin.Nodes.Debian.Package (Package (..))

-------------------------------------------------------------------------------
data Report
    = RunDebootstrap !DebootstrapCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
data Suite
    = Stable
    | OldStable
    | Unstable
    | Testing
    deriving (Show)

type Includes =
    [Package]

data RootTree
    = RootTree
    { suite :: Suite
    , path :: FilePath
    , includes :: Includes
    }
    deriving (Show)

rootTree ::
    Reporter Report ->
    Track' (Binary "debootstrap") ->
    RootTree ->
    Op
rootTree r boot root =
    withBinary boot debootstrapCommand cmd $ \up ->
        op "debootstrap" (deps [rootdir]) $ \actions ->
            actions
                { help = Text.unwords ["debootstraps", Text.pack (show root.suite), "at", Text.pack root.path]
                , ref = dotRef $ "debootstrap:" <> Text.pack root.path
                , prelim = skipIfFileExists etcIssues
                , up = up r'
                }
  where
    r' = contramap (RunDebootstrap cmd) r
    cmd = MakeRoot root.includes root.suite root.path
    rootdir :: Op
    rootdir = dir (Directory root.path)
    etcIssues :: FilePath
    etcIssues = root.path </> "etc/issue"

data DebootstrapCommand
    = MakeRoot Includes Suite FilePath
    deriving (Show)

debootstrapCommand :: Command "debootstrap" DebootstrapCommand
debootstrapCommand = Command $ \cmd -> case cmd of
    (MakeRoot [] suite rootdir) ->
        proc
            "debootstrap"
            [ suiteName suite
            , rootdir
            ]
    (MakeRoot packages suite rootdir) ->
        proc
            "debootstrap"
            [ includearg packages
            , suiteName suite
            , rootdir
            ]
  where
    includearg xs =
        Text.unpack $
            "--include=" <> Text.intercalate "," (fmap pkgName xs)
    suiteName n =
        case n of
            Stable -> "stable"
            OldStable -> "oldstable"
            Unstable -> "unstable"
            Testing -> "testing"
