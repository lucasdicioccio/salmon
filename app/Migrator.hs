{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative (command, execParser, fullDesc, header, help, helper, info, progDesc, strArgument, subparser, (<**>))
import Options.Generic (Generic, ParseRecord (..))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Op, Track', deps, notes, op, realNoop, ref)
import qualified Salmon.Builtin.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified SreBox.PostgresInit as PGInit
import qualified SreBox.PostgresMigrations as PGMigrate

import Salmon.Op.Configure (Configure (..))
import Salmon.Op.G (G (..))
import Salmon.Op.OpGraph (inject, node)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.Track (Track (..), opGraph)
import Salmon.Reporter

import System.FilePath ((</>))

-------------------------------------------------------------------------------
loadMigrations :: FilePath -> FilePath -> IO (G PGMigrate.MigrationFile)
loadMigrations root tip =
    let reader =
            Migrations.addFilePrefix root $
                PGMigrate.defaultMigrationReader
     in G . fmap node <$> Migrations.loadMigrations reader tip

prepare ::
    FilePath ->
    FilePath ->
    Text ->
    Text ->
    FilePath ->
    IO PGMigrate.MigrationSetup
prepare root tip dbname migrateusername passfile =
    PGMigrate.MigrationSetup
        <$> loadMigrations root tip
        <*> pure (Postgres.User migrateusername)
        <*> pure (Postgres.Database dbname)
        <*> pure passfile

migrateSuperUser :: PGMigrate.MigrationSetup -> Op
migrateSuperUser arg =
    runMigration
  where
    runMigration :: Op
    runMigration =
        PGMigrate.applyAdminScriptMigration
            reportPrint
            Debian.psql
            (Track $ PGInit.setupNakedPG reportPrint)
            arg

migrate :: PGMigrate.MigrationSetup -> Op
migrate arg =
    runMigration
  where
    initPg = Track (\connstring -> initdb connstring `inject` passfile connstring)

    initdb :: Postgres.ConnString FilePath -> Op
    initdb = PGInit.setupSingleUserPG reportPrint

    passfile :: Postgres.ConnString FilePath -> Op
    passfile conn =
        Secrets.sharedSecretFile
            reportPrint
            Debian.openssl
            (Secrets.Secret Secrets.Hex 48 conn.connstring_user_pass)

    runMigration :: Op
    runMigration =
        PGMigrate.applyUserScriptMigration
            reportPrint
            Debian.psql
            initPg
            arg

-------------------------------------------------------------------------------
data Spec
    = Migrate {migrateAsSuperUser :: PGMigrate.MigrationSetup, migrateAsOwner :: PGMigrate.MigrationSetup}
    deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

program :: Track' Spec
program =
    go 0
  where
    go n = Track $ \spec ->
        optimizedDeps $ op "program" (deps $ specOp (n + 1) spec) $ \actions ->
            actions
                { notes = [Text.pack $ "at depth " <> show n]
                , ref = dotRef $ Text.pack $ "program:" <> show n
                }

    specOp :: Int -> Spec -> [Op]
    -- meta
    specOp k (Migrate setup1 setup2) = [migrate setup2 `inject` migrateSuperUser setup1]

    optimizedDeps :: Op -> Op
    optimizedDeps base =
        let pkgs = Debian.installAllDebsAtOnce base
         in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
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

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (Seed root1 tip1 root2 tip2 dbname username passfile) =
        Migrate
            <$> prepare root1 tip1 dbname username passfile
            <*> prepare root2 tip2 dbname username passfile

-------------------------------------------------------------------------------
main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Standalone db migration tool" <> header "for Postgres"
    let opts = info parseRecord desc
    cmd <- execParser opts
    CLI.execCommandOrSeed reportPrint configure program cmd
