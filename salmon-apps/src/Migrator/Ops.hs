{-# LANGUAGE OverloadedRecordDot #-}

module Migrator.Ops where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension (Op, Track')
import qualified Salmon.Builtin.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified SreBox.PostgresInit as PGInit
import qualified SreBox.PostgresMigrations as PGMigrate
import System.FilePath (takeDirectory, (</>))

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
    [Text] ->
    FilePath ->
    IO PGMigrate.MigrationSetup
prepare root tip dbname migrateusername extrausernames passfile =
    PGMigrate.MigrationSetup
        <$> loadMigrations root tip
        <*> pure (Postgres.User migrateusername)
        <*> pure (Postgres.Database dbname)
        <*> pure passfile
        <*> pure [(Postgres.User u, passfileForUser u) | u <- extrausernames]
  where
    passfileForUser :: Text -> FilePath
    passfileForUser u = takeDirectory passfile </> (Text.unpack $ u <> ".pass")

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
    initdb ownerConnstring =
        PGInit.setupMultiUserPG
            reportPrint
            ownerConnstring
            [(u, p, [], []) | (u, p) <- users]
            []

    users :: [(Postgres.User, FS.File "passfile")]
    users = [(u, FS.Generated extrapassfile p) | (u, p) <- arg.setup_extra_users]

    passfile :: Postgres.ConnString FilePath -> Op
    passfile conn =
        Secrets.sharedSecretFile
            reportPrint
            Debian.openssl
            (Secrets.Secret Secrets.Hex 48 conn.connstring_user_pass)

    extrapassfile :: Track' FilePath
    extrapassfile = Track $ \path ->
        Secrets.sharedSecretFile
            reportPrint
            Debian.openssl
            (Secrets.Secret Secrets.Hex 48 path)

    runMigration :: Op
    runMigration =
        PGMigrate.applyUserScriptMigration
            reportPrint
            Debian.psql
            initPg
            arg
