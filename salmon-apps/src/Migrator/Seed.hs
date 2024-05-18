module Migrator.Seed where

import Data.Text (Text)
import Options.Applicative (command, help, helper, info, progDesc, strArgument, subparser, (<**>))
import Options.Generic (ParseRecord (..))

data Seed
    = Seed
    { migrateRoot_superuser :: FilePath
    , migrateTip_superuser :: FilePath
    , migrateRoot :: FilePath
    , migrateTip :: FilePath
    , migrateDatabase :: Text
    , migrateUser :: Text
    , migratePassFile :: FilePath
    }

instance ParseRecord Seed where
    parseRecord =
        combo <**> helper
      where
        combo =
            subparser $
                mconcat
                    [ command "migrate" (info build (progDesc "migrates sql file"))
                    ]
        build =
            Seed
                <$> strArgument (Options.Applicative.help "root of migration files [superuser]")
                <*> strArgument (Options.Applicative.help "tip of migration files [superuser]")
                <*> strArgument (Options.Applicative.help "root of migration files [db-owner]")
                <*> strArgument (Options.Applicative.help "tip of migration files [db-owner]")
                <*> strArgument (Options.Applicative.help "dbname")
                <*> strArgument (Options.Applicative.help "username [db-owner]")
                <*> strArgument (Options.Applicative.help "passfile")
