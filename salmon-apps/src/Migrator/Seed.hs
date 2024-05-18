module Migrator.Seed where

import Data.Text (Text)
import Options.Applicative (command, commandGroup, fullDesc, header, help, helper, info, long, progDesc, strOption, subparser, (<**>))

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
                    [ commandGroup "pg"
                    , command
                        "migrate"
                        (info build (header "Migrate" <> fullDesc <> progDesc description))
                    ]
        description :: String
        description =
            unlines
                [ "Migrates PostgreSQL files on a Debian-like."
                , ""
                , "Assumes two sets of migrations: admin and user."
                , "Admin migrations run first as the `postgres` system user."
                , "User migrations run with a user-name and a password (in a password file)."
                ]
        build =
            Seed
                <$> strOption
                    (long "admin-root" <> Options.Applicative.help "root of migration files [superuser]")
                <*> strOption
                    (long "admin-tip" <> Options.Applicative.help "tip of migration files [superuser]")
                <*> strOption
                    (long "user-root" <> Options.Applicative.help "root of migration files [db-owner]")
                <*> strOption
                    (long "user-tip" <> Options.Applicative.help "tip of migration files [db-owner]")
                <*> strOption
                    (long "db" <> Options.Applicative.help "dbname")
                <*> strOption
                    (long "db-user" <> Options.Applicative.help "username [db-owner]")
                <*> strOption
                    (long "db-passfile" <> Options.Applicative.help "passfile")
