{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-docs-sync@: keep the generated website (@docs/@, what GitHub Pages
serves) in step with the files it is generated from.

The repository's own docs build, @website/scripts/build-site.sh@, is a
manual step; this is that step as a salmon node, so it can be run once
(@run up@), from cron, or tended by @run serve@.

__check__ fingerprints the inputs — every /tracked/ file under @resources/@,
@specs/@, @website/src/@ and @website/scripts/@, plus @README.md@ — and
compares the result with the stamp stored in @docs/.docs-sync-stamp@. The
stamp is committed with @docs/@, so "what was the site built from" travels
with the site. Gitignored files (the mirrored @docs-*.cmark@ pages, which
@sync-repo-docs.sh@ rewrites with a fresh date on every run) are not inputs:
they are derived, and counting them would make the site stale after every
build. Because @git ls-files@ names the inputs, an edit that is not yet
committed still counts (the contents are read from disk): the node
regenerates against what the working tree says.

__up__ runs @build-site.sh@, writes the stamp, @git add docs@, commits with a
fixed headline (@docs: regenerate site@) when something is staged, and
pushes. It touches nothing outside @docs/@.

= Trigger: on demand or tended

The check is a few file reads and one @git ls-files@, so the node is cheap to
ask but the build behind @up@ is not, which rules out @supReapply@ (that is
for an @up@ as cheap as its check). Two fitting triggers:

* @run up@ from a cron entry or a post-merge hook: the simplest, one shot
  per event, nothing resident.
* @run serve@: the check runs on the tending ladder, so a change to the
  working tree is noticed and the site regenerated and pushed without anybody
  typing @converge@. Costs a resident process.

Push uses the repository's ambient authentication; this binary never handles
a credential. @--no-push@ commits only.
-}
module DocsSync (
    main,
    Seed (..),
    Spec (..),
    configure,
    program,
    docsSync,
    inputFingerprint,
    stampPath,
) where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Options.Applicative (execParser, fullDesc, header, helper, info, long, metavar, progDesc, showDefault, strOption, switch, value, (<**>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Control.Exception (Exception, throwIO)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem (hashBytes)
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Regenerate, commit and push docs/ when its sources changed" <> header "salmon-docs-sync"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd

-------------------------------------------------------------------------------

data Seed = Seed
    { seedRepo :: FilePath
    , seedRemote :: Text
    , seedNoPush :: Bool
    }
    deriving (Eq, Show, Generic)

instance FromJSON Seed
instance ToJSON Seed

instance ParseRecord Seed where
    parseRecord =
        build <**> helper
      where
        build =
            Seed
                <$> strOption (long "repo" <> metavar "DIR" <> value "." <> showDefault <> Opt.help "root of the salmon checkout")
                <*> strOption (long "remote" <> metavar "REMOTE" <> value "origin" <> showDefault <> Opt.help "git remote to push to")
                <*> switch (long "no-push" <> Opt.help "commit but do not push")

data Spec = Spec
    { specRepo :: FilePath
    , specRemote :: Text
    , specPush :: Bool
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \s -> pure (Spec s.seedRepo s.seedRemote (not s.seedNoPush))

program :: Track' Spec
program = Track docsSync

-------------------------------------------------------------------------------

data DocsSyncError = StepFailed String Int String
    deriving (Show)

instance Exception DocsSyncError

stampPath :: FilePath -> FilePath
stampPath repo = repo </> "docs" </> ".docs-sync-stamp"

-- | The tracked files the site is generated from.
inputRoots :: [String]
inputRoots = ["resources", "specs", "README.md", "website/src", "website/scripts"]

step :: FilePath -> String -> [String] -> IO String
step repo cmd args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc cmd args){cwd = Just repo} ""
    case code of
        ExitSuccess -> pure out
        ExitFailure n -> throwIO (StepFailed (unwords (cmd : args)) n err)

-- | A fingerprint of every tracked input, by path and content.
inputFingerprint :: FilePath -> IO Text
inputFingerprint repo = do
    listing <- step repo "git" (["ls-files", "-z", "--"] <> inputRoots)
    let paths = filter (not . null) (splitNul listing)
    contents <- mapM (\p -> readIfThere (repo </> p) >>= \c -> pure (C8.pack p <> "\0" <> c <> "\0")) paths
    pure (hashBytes (ByteString.concat contents))
  where
    splitNul s = case break (== '\0') s of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitNul rest
    -- a tracked file deleted from the working tree is an input that changed
    readIfThere f = do
        there <- doesFileExist f
        if there then ByteString.readFile f else pure "<missing>"

readStamp :: FilePath -> IO (Maybe Text)
readStamp repo = do
    let f = stampPath repo
    there <- doesFileExist f
    if there then Just . Text.strip <$> Text.readFile f else pure Nothing

docsSync :: Spec -> Op
docsSync spec =
    op "docs-sync" nodeps $ \actions ->
        actions
            { help = "regenerates docs/ when its sources changed, then commits and pushes it"
            , notes =
                [ "inputs: tracked files under " <> Text.unwords (map Text.pack inputRoots)
                , "stamp: docs/.docs-sync-stamp"
                , if spec.specPush then "pushes to " <> spec.specRemote else "does not push"
                ]
            , ref = mkRef "docs-sync" (Text.pack repo)
            , check = do
                now <- inputFingerprint repo
                stamp <- readStamp repo
                pure $
                    if stamp == Just now
                        then Success
                        else Failure ("docs/ was built from " <> maybe "nothing recorded" id stamp <> ", sources are " <> now)
            , up = do
                _ <- step repo "bash" ["website/scripts/build-site.sh"]
                fp <- inputFingerprint repo
                Text.writeFile (stampPath repo) (fp <> "\n")
                _ <- step repo "git" ["add", "--all", "docs"]
                (staged, _, _) <- readCreateProcessWithExitCode (proc "git" ["diff", "--cached", "--quiet", "--", "docs"]){cwd = Just repo} ""
                when (staged /= ExitSuccess) $ do
                    _ <- step repo "git" ["commit", "-m", "docs: regenerate site (sources " <> Text.unpack fp <> ")", "--", "docs"]
                    pure ()
                when spec.specPush $ do
                    _ <- step repo "git" ["push", Text.unpack spec.specRemote, "HEAD"]
                    pure ()
            }
  where
    repo = spec.specRepo
