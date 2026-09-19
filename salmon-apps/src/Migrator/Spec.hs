{-# LANGUAGE DeriveGeneric #-}

module Migrator.Spec where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

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
    deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec
