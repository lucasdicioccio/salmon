{- | pgvector: the @vector@ type and the @hnsw@ / @ivfflat@ access methods.

The simplest of the Postgres extensions: files from a package, then
@CREATE EXTENSION vector@ in each database. No @shared_preload_libraries@
entry, no restart.

Two decisions the node makes so a caller does not have to:

* __The package name comes from a declared major version__
  (@postgresql-\<major\>-pgvector@), not from the running cluster. 'deb'
  takes a static name, and a declared major keeps the graph hermetic (and
  lets 'Salmon.Builtin.Nodes.Debian.Package.batchPackages' see it). A wrong
  declaration is loud: @CREATE EXTENSION@ then cannot find its control file.
* __Where the package comes from is the caller's choice.__ The package is
  in the PostgreSQL project's own repository (and, on some releases, the
  distribution's). 'pgvSource' is a @'Track'' ()@: pass
  @'Salmon.Builtin.Nodes.Debian.AptRepository.viaRepository' ('Salmon.Builtin.Nodes.Debian.AptRepository.pgdg' key fingerprint)@
  to take on the external repository, or
  'Salmon.Builtin.Extension.ignoreTrack' to require that the package is
  installable already.

Out of scope: vector columns and indexes (schema: migrations), and the
per-query knobs (@hnsw.ef_search@, @ivfflat.probes@).
-}
module Salmon.Builtin.Nodes.PgVector (
    PgVector (..),
    pgvector,
    pgvectorPackage,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import Salmon.Builtin.Nodes.Debian.Package (Package (..))
import qualified Salmon.Builtin.Nodes.Debian.Package as Package
import Salmon.Builtin.Nodes.Postgres (DatabaseName, PgExtension (..), Port)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

data PgVector = PgVector
    { pgvMajor :: Int
    -- ^ the cluster's major version; 13 is the floor
    , pgvPort :: Port
    , pgvDatabases :: [DatabaseName]
    , pgvUpgrade :: Bool
    -- ^ also @ALTER EXTENSION vector UPDATE@ when the package is newer (see 'extUpgrade')
    }
    deriving (Eq, Show)

-- | @postgresql-16-pgvector@ for major 16.
pgvectorPackage :: Int -> Package
pgvectorPackage major = Package ("postgresql-" <> Text.pack (show major) <> "-pgvector")

{- | Install the package (after its source, if the caller supplied one), then
make @vector@ usable in each declared database.

@down@ drops the extension from each database without @CASCADE@, so a
database with vector columns refuses, and leaves the package installed.
-}
pgvector ::
    Reporter Postgres.Report ->
    Reporter Package.Report ->
    Track' (Binary "psql") ->
    Track' () ->
    Track' DatabaseName ->
    PgVector ->
    Op
pgvector r pkgReporter psql source mkdb cfg =
    op "pgvector" (deps exts) $ \actions ->
        actions
            { help = "pgvector for postgresql " <> Text.pack (show cfg.pgvMajor) <> " in " <> Text.pack (show (length cfg.pgvDatabases)) <> " database(s)"
            , ref = mkRef "pgvector" (cfg.pgvPort, cfg.pgvDatabases)
            }
  where
    package :: Op
    package = Package.debWith pkgReporter (pgvectorPackage cfg.pgvMajor) `inject` run source ()

    exts :: [Op]
    exts =
        [ Postgres.extension r psql cfg.pgvPort mkdb (spec db) `inject` package
        | db <- cfg.pgvDatabases
        ]

    spec :: DatabaseName -> PgExtension
    spec db =
        PgExtension
            { extName = "vector"
            , extDatabase = db
            , extMinServerVersion = Just 130000
            , extUpgrade = cfg.pgvUpgrade
            }
