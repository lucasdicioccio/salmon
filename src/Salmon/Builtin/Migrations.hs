{- | Object to help reading migration files and turning a series of files into
a Migration.

The idea is that
-}
module Salmon.Builtin.Migrations where

import Control.Comonad.Cofree (Cofree (..))
import Data.ByteString as ByteString
import System.FilePath ((</>))

import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.OpGraph

{- | Migrations are sequenced operations that are stored in separate files.

Hence, evaluating the Migration steps requires opening multiple files and is non-deterministic.
-}
type Migration a = OpGraph IO a

{- | Collections of functions to read Migration steps.

The idea of these functions is that they should be independent.
Indeed, normalizePath could be contramapped on register or mapped on
parsePredecessors. However the idea is to build a MigrationReader piecewise.
Further, we expect that the loadMigrations step is done once in a Seeding
stage. Thus, we expect that Migrations out of the reader are JSON-ifiable
data items. Which are then turned into some operation-bearing in an
Run stage.
Hence, some hints are provided about expectations.
-}
data MigrationReader a
    = MigrationReader
    { register :: FilePath -> IO a
    -- ^ function to turn a filepath in the migration
    -- We expect that most-often this function will be pure, merely recording the
    -- file path. IO is provided as a convenience.
    , parsePredecessors :: ByteString -> [FilePath]
    -- ^ locate predecessor files in the contents of the file
    -- We expect that this parsing is superficial, for instance reading only
    -- inside comments of SQL scripts rather than parsing a whole SQL AST.
    , normalizePath :: FilePath -> FilePath
    -- ^ function to turn a filepath found in the migration into a file the
    -- reader can actually open.
    }

{- | Add a prefix like a source-directory where migrations refer to each-other
using local-paths that may differ from the rundir where Salmon binaries run.
-}
addFilePrefix :: FilePath -> MigrationReader a -> MigrationReader a
addFilePrefix pfx reader =
    reader{normalizePath = \p -> pfx </> reader.normalizePath p}

-- TODO: ioref to avoid double reading
-- TODO: detect cycles
readMigrationFile ::
    forall a.
    MigrationReader a ->
    FilePath ->
    IO (Migration a)
readMigrationFile reader src =
    OpGraph readPredecessors <$> (reader.register path)
  where
    path :: FilePath
    path = reader.normalizePath src

    readPredecessors :: IO (Graph (Migration a))
    readPredecessors = do
        contents <- ByteString.readFile (reader.normalizePath src)
        Vertices
            <$> traverse (readMigrationFile reader) (reader.parsePredecessors contents)

-- | Load migrations steps for a given starting file.
loadMigrations ::
    MigrationReader a ->
    FilePath ->
    IO (Cofree Graph (Migration a))
loadMigrations r path = expand =<< readMigrationFile r path
