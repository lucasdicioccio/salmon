{-# LANGUAGE OverloadedRecordDot #-}

module Migrator.Ops where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Salmon.Builtin.Extension (Op, Track')
import qualified Salmon.Builtin.Migrations as Migrations
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified SreBox.PostgresInit as PGInit
import qualified SreBox.PostgresMigrations as PGMigrate
import qualified SreBox.PostgresTemplate as PGTemplate
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

{- | The migrations of both sets, built into a template database.

Same two migration graphs, same order, same users as 'migrate' and
'migrateSuperUser' -- only walked from inside the template node, behind its
check, so that a template already built from these files is not touched at
all (see "SreBox.PostgresTemplate" for why it cannot be an ordinary
dependency).
-}
buildTemplate :: Text -> PGMigrate.MigrationSetup -> PGMigrate.MigrationSetup -> Op
buildTemplate fp superuser owner =
    PGTemplate.template
        reportPrint
        (Track $ Postgres.pgLocalCluster reportPrint Debian.postgres Debian.pg_ctlcluster)
        Debian.psql
        Postgres.localServer
        (PGTemplate.Template owner.setup_database.getDatabase fp)
        (migrate owner `inject` migrateSuperUser superuser)

{- | What a template is built from: every migration file's path and contents,
each set tagged so moving a file from one to the other counts as a change,
plus the database and roles, which end up baked into the template's
ownership and grants.
-}
fingerprintInputs :: PGMigrate.MigrationSetup -> PGMigrate.MigrationSetup -> IO Text
fingerprintInputs superuser owner = do
    superuserFiles <- PGTemplate.fileFingerprintParts (paths superuser)
    ownerFiles <- PGTemplate.fileFingerprintParts (paths owner)
    pure $
        PGTemplate.fingerprint $
            ["superuser"]
                <> superuserFiles
                <> ["owner"]
                <> ownerFiles
                <> ["database", Text.encodeUtf8 owner.setup_database.getDatabase, "owner", Text.encodeUtf8 owner.setup_user.userRole]
                <> ["user:" <> Text.encodeUtf8 u.userRole | (u, _) <- owner.setup_extra_users]
  where
    paths :: PGMigrate.MigrationSetup -> [FilePath]
    paths setup = [m.path | m <- toList setup.setup_migration]
