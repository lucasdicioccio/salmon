{-# LANGUAGE DeriveGeneric #-}

module Migrator.Spec where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified SreBox.PostgresMigrations as PGMigrate

data Spec
    = Migrate
    { migrateAsSuperUser :: PGMigrate.MigrationSetup
    , migrateAsOwner :: PGMigrate.MigrationSetup
    -- todo: some PGInit.InitSetup
    }
    | BuildTemplate
    { templateAsSuperUser :: PGMigrate.MigrationSetup
    , templateAsOwner :: PGMigrate.MigrationSetup
    , templateFingerprint :: Text
    -- ^ resolved by @configure@, on the machine holding the migration files
    }
    | -- | Copies a preview environment's database from an already-built
      -- template (see 'BuildTemplate'). Does not migrate anything itself --
      -- @salmon-migrator template@ must have already locked
      -- 'cloneTemplateName' on this cluster. The template is referenced by
      -- name only ('Salmon.Builtin.Extension.ignoreTrack'): this command
      -- does not know or care how the template got there, only that it did.
      Clone
    { cloneRetention :: Postgres.Retention
    , cloneSpec :: Postgres.Clone
    }
    deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec
