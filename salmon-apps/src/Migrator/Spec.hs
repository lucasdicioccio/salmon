{-# LANGUAGE DeriveGeneric #-}

module Migrator.Spec where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import qualified SreBox.PostgresMigrations as PGMigrate

data Spec
    = Migrate
    { migrateAsSuperUser :: PGMigrate.MigrationSetup
    , migrateAsOwner :: PGMigrate.MigrationSetup
    -- todo: some PGInit.InitSetup
    }
    deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec
