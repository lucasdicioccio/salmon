{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A tiny end-to-end binary for playing with @run serve@
('Salmon.Actions.Serve') by hand, against nothing more dangerous than files
in a directory you name.

A seed is "a named bundle of files under a base directory". The 'Track''
turns it into real 'Salmon.Builtin.Nodes.Filesystem' nodes (a directory plus
one file per name, each with deterministic content), so declaring a seed up
actually creates those files and retiring it actually deletes them — you can
watch the convergence happen in another terminal with @ls@/@watch@.

Because it goes through 'CLI.execCommandOrSeed' it is a full salmon binary,
so the same seed works one-shot too:

> salmon-ops-serve-fixture config --dir /tmp/play --name web --file index.html | salmon-ops-serve-fixture run up

But the point is the loop. Try (typing, or piping in, one line per command):

> salmon-ops-serve-fixture run serve
> up --dir /tmp/play --name web --file index.html --file style.css
> up --dir /tmp/play --name api --file openapi.json
> status
> down --dir /tmp/play --name web --file index.html --file style.css
> only --dir /tmp/play --name api --file openapi.json --file CHANGELOG
> history
> quit

Notes to notice while playing:

  * the base directory itself is one node shared by every seed (they unify by
    'Salmon.Op.Ref.Ref'), so it survives until the last seed under it is gone;
  * re-declaring an unchanged seed converges to a no-op (nothing is re-run);
  * a seed is identified by the /files it asks for/, so @only@ with a
    different --file set supersedes rather than adds;
  * @status@ shows each node's wanted direction and whether it has converged.
-}
module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative (execParser, fullDesc, header, help, info, long, many, metavar, progDesc, strOption, value)
import Options.Generic (ParseRecord (..))
import System.FilePath ((</>))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Op, Track', deps, op, ref)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

-------------------------------------------------------------------------------

-- | What a human types after @config@ / @up@ / @down@ / @only@.
data Seed = Seed
    { seedDir :: FilePath
    , seedName :: Text
    , seedFiles :: [String]
    }
    deriving (Generic, Show)

instance ParseRecord Seed where
    parseRecord =
        Seed
            <$> strOption
                (long "dir" <> metavar "DIR" <> value "/tmp/salmon-serve-fixture" <> help "base directory to converge files under")
            <*> fmap Text.pack (strOption (long "name" <> metavar "NAME" <> help "name of this bundle (its own subdirectory)"))
            <*> many (strOption (long "file" <> metavar "FILE" <> help "a file to keep in the bundle (repeatable)"))

-- | The hermetic directive: same shape as the seed here, but that's a
-- coincidence of how trivial this fixture is — the point of the type is that
-- it is 'FromJSON'\/'ToJSON', so it is what identifies a seed in the loop.
data Spec = Spec
    { specRoot :: FilePath
    , specFiles :: [FilePath]
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \seed ->
    let root = seed.seedDir </> Text.unpack seed.seedName
     in pure $ Spec root [root </> f | f <- seed.seedFiles]

program :: Track' Spec
program = Track $ \spec ->
    op "serve-fixture-bundle" (deps (fmap fileOp spec.specFiles)) $ \actions ->
        actions{ref = mkRef "serve-fixture-bundle" (spec.specRoot, spec.specFiles)}
  where
    fileOp :: FilePath -> Op
    fileOp path =
        FS.filecontents (FS.FileContents path (Text.pack ("managed by salmon-ops-serve-fixture: " <> path <> "\n")))

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "play with `run serve` against files in a directory" <> header "salmon-ops-serve-fixture"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd
