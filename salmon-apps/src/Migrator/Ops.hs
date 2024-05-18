{-# LANGUAGE OverloadedRecordDot #-}

module Migrator.Ops where

import Data.Text (Text)

import Salmon.Builtin.Extension (Op)
import qualified Salmon.Builtin.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified SreBox.PostgresInit as PGInit
import qualified SreBox.PostgresMigrations as PGMigrate

import Salmon.Op.G (G (..))
import Salmon.Op.OpGraph (inject, node)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter

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
