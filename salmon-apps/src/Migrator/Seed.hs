module Migrator.Seed where

import Data.Text (Text)
import Options.Applicative (command, commandGroup, fullDesc, header, help, helper, info, long, many, progDesc, strOption, subparser, value, (<**>))

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
    , migrateExtraUsers :: [Text]
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
                    (long "superuser-root" <> Options.Applicative.help "root of migration files [database superuser]" <> value "migrations/superuser")
                <*> strOption
                    (long "superuser-tip" <> Options.Applicative.help "tip of migration files [database superuser]" <> value "tip.sql")
                <*> strOption
                    (long "owner-root" <> Options.Applicative.help "root of migration files [database owner]" <> value "migrations/owner")
                <*> strOption
                    (long "owner-tip" <> Options.Applicative.help "tip of migration files [database owner]" <> value "tip.sql")
                <*> strOption
                    (long "db" <> Options.Applicative.help "dbname")
                <*> strOption
                    (long "db-owner" <> Options.Applicative.help "username [database owner]")
                <*> strOption
                    (long "db-passfile" <> Options.Applicative.help "passfile")
                <*> ( many $
                        strOption
                            (long "db-extra-user" <> Options.Applicative.help "username [database user]")
                    )
