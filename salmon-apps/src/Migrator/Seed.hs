module Migrator.Seed where

import Data.Text (Text)
import Options.Applicative (command, commandGroup, fullDesc, header, help, helper, info, long, many, progDesc, strOption, subparser, value, (<**>))

import Options.Generic (ParseRecord (..))

-- | What the migrations are applied to.
data Mode
    = -- | a live database, moved forward in place
      InPlace
    | -- | a template database, rebuilt from nothing whenever they change
      AsTemplate

data Seed
    = Seed
    { migrateMode :: Mode
    , migrateRoot_superuser :: FilePath
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
                        (info (build InPlace) (header "Migrate" <> fullDesc <> progDesc description))
                    , command
                        "template"
                        (info (build AsTemplate) (header "Template" <> fullDesc <> progDesc templateDescription))
                    ]
        templateDescription :: String
        templateDescription =
            unlines
                [ "Builds a template database from the same migrations `migrate` applies."
                , ""
                , "The database named by --db is created from nothing, migrated, then locked"
                , "(IS_TEMPLATE, no connections), so `CREATE DATABASE x TEMPLATE <db>` copies it."
                , "It is skipped while the migration files are unchanged, and dropped and"
                , "rebuilt when any of them change -- never migrated in place."
                , ""
                , "Roles are cluster-wide: the owner and extra users are those of this cluster,"
                , "and objects inside a clone keep the owners they have here."
                , "Refuses to replace a database salmon did not build as a template."
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
        build mode =
            Seed mode
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
