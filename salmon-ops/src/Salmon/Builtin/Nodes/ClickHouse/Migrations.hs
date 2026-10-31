{-# LANGUAGE DeriveGeneric #-}

{- | ClickHouse schema migrations: an ordered set of @.sql@ files applied once
each, and recorded in a ledger table, @salmon_migrations@ (name, checksum,
applied_at), in a database the caller names.

Planning happens where the seed is turned into a directive
('loadMigrationsDir', or the pure 'planMigrations'): the files are listed,
ordered by name, and each one's SHA-256 is computed and carried in the
'Migration' (and so in the directive and the node's @notes@: a changed file is
a changed node). Applying happens at @up@: one node per file, each waiting for
the previous one, the first waiting for the ledger table.

* A migration recorded with the planned checksum is done: its check reads
  'Success' and @up@ does nothing.
* A migration not recorded is pending: its check reads 'Failure', @up@ sends
  the file to @clickhouse-client --multiquery@ on its standard input (as
  'ClickHouse.adminRun' does), then records it.
* A migration recorded with another checksum is a loud failure: the check
  reads 'Failure' naming both checksums, and @up@ throws
  'MigrationChecksumMismatch' and sends nothing. Salmon never re-applies an
  applied migration; an edited one is the operator's to reconcile (write a
  new migration instead).
* A file whose content no longer matches the planned checksum (edited
  between @config@ and @run up@) makes @up@ throw 'MigrationFileChanged'.

ClickHouse has no transactional DDL: the file runs, then the ledger row is
written, in two client calls. A file that fails part-way is not recorded and
runs again in full on the next pass, as does one whose recording failed, so
write statements that tolerate a second run (@IF NOT EXISTS@).

The cluster-aware option is 'migrationsOnCluster': a clause such as
@ ON CLUSTER \`main\`@ supplied by the caller (what the cluster module's
@onClusterClause@ renders). It is added to the ledger table's @CREATE TABLE@,
and it replaces the token @{{on_cluster}}@ wherever a migration file has it,
so a file can say @CREATE TABLE IF NOT EXISTS db.t{{on_cluster}} (...)@.
With 'Nothing' the token is removed and nothing else changes. The checksum is
the file's as written, before the token is replaced. With a cluster, the
ledger table is a plain @MergeTree@ on every member and rows are written on
the server salmon talks to only: the ledger is that server's.

Nothing here drops anything: @down@ of every node is nothing.
-}
module Salmon.Builtin.Nodes.ClickHouse.Migrations where

import Control.Exception (Exception, throwIO)
import qualified Crypto.Hash as Hash
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as ByteString
import Data.Char (isControl)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinaryStdinAtUp)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.ClickHouse (AdminBatch (..), Client, DatabaseName, adminQuery, adminRun, clientKey, decode, interpretCount, quoteIdent, quoteLiteral)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = CHMigrationLedger !DatabaseName !Binary.Report
    | CHApplyMigration !Text !Binary.Report
    | CHRecordMigration !Text !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
-- Planning

-- | One migration file, as planned: its name (the file name), path and checksum.
data Migration
    = Migration
    { migrationName :: Text
    , migrationPath :: FilePath
    , migrationChecksum :: Text
    -- ^ hex SHA-256 of the file's bytes
    }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Migration
instance ToJSON Migration

-- | What a set of migrations is applied to, and how.
data Migrations
    = Migrations
    { migrationsDatabase :: DatabaseName
    -- ^ where the ledger table lives; it must exist (@default@ always does)
    , migrationsOnCluster :: Maybe Text
    -- ^ e.g. @Just " ON CLUSTER \`main\`"@; 'Nothing' for a single server
    , migrationsFiles :: [Migration]
    -- ^ in the order they apply ('planMigrations' gives it)
    }
    deriving (Eq, Show, Generic)

instance FromJSON Migrations
instance ToJSON Migrations

-- | A single server, the ledger in this database.
singleServer :: DatabaseName -> [Migration] -> Migrations
singleServer db ms = Migrations db Nothing ms

-- | The ledger table's name.
ledgerTable :: Text
ledgerTable = "salmon_migrations"

checksum :: ByteString.ByteString -> Text
checksum bytes = Text.pack (show (Hash.hashWith Hash.SHA256 bytes))

{- | Orders files (a path and its content) into migrations: the @.sql@ ones
only, by file name. A name with a control character or a backslash is
refused (it would not survive the ledger's TSV), as are two files with one
name.
-}
planMigrations :: [(FilePath, ByteString.ByteString)] -> Either [Text] [Migration]
planMigrations files =
    case problems of
        [] -> Right ms
        _ -> Left problems
  where
    ms =
        sortOn migrationName
            [ Migration (Text.pack (takeFileName path)) path (checksum bytes)
            | (path, bytes) <- files
            , takeExtension path == ".sql"
            ]
    names = fmap migrationName ms
    problems =
        [ "migration file name " <> n <> " has a control character or a backslash"
        | n <- names
        , Text.any (\c -> isControl c || c == '\\') n
        ]
            <> [ "two migration files are named " <> a
               | (a, b) <- zip names (drop 1 names)
               , a == b
               ]

data InvalidMigrations = InvalidMigrations !FilePath ![Text]
    deriving (Show)

instance Exception InvalidMigrations

{- | Reads a directory's @.sql@ files and plans them ('planMigrations'),
throwing 'InvalidMigrations' on a problem. For the seed stage: the files
are read here, to compute the checksums.
-}
loadMigrationsDir :: FilePath -> IO [Migration]
loadMigrationsDir dir = do
    entries <- listDirectory dir
    files <- filterFiles [dir </> e | e <- entries, takeExtension e == ".sql"]
    contents <- traverse (\p -> (,) p <$> ByteString.readFile p) files
    either (throwIO . InvalidMigrations dir) pure (planMigrations contents)
  where
    filterFiles ps = fmap concat (traverse (\p -> (\ok -> [p | ok]) <$> doesFileExist p) ps)

-------------------------------------------------------------------------------
-- SQL

qualifiedLedger :: DatabaseName -> Text
qualifiedLedger db = quoteIdent db <> "." <> quoteIdent ledgerTable

ledgerTableSql :: Migrations -> Text
ledgerTableSql ms =
    Text.unlines
        [ "CREATE TABLE IF NOT EXISTS " <> qualifiedLedger ms.migrationsDatabase <> maybe "" id ms.migrationsOnCluster
        , "("
        , "    name String,"
        , "    checksum String,"
        , "    applied_at DateTime DEFAULT now()"
        , ")"
        , "ENGINE = MergeTree"
        , "ORDER BY name;"
        ]

ledgerExistsSql :: DatabaseName -> Text
ledgerExistsSql db =
    "SELECT count() FROM system.tables WHERE database = " <> quoteLiteral db <> " AND name = " <> quoteLiteral ledgerTable

-- | Every recorded row, as TSV @name\\tchecksum@.
ledgerRowsSql :: DatabaseName -> Text
ledgerRowsSql db = "SELECT name, checksum FROM " <> qualifiedLedger db <> " ORDER BY name FORMAT TSV"

recordSql :: DatabaseName -> Migration -> Text
recordSql db m =
    "INSERT INTO "
        <> qualifiedLedger db
        <> " (name, checksum) VALUES ("
        <> quoteLiteral m.migrationName
        <> ", "
        <> quoteLiteral m.migrationChecksum
        <> ");\n"

-- | The token a migration file uses for the cluster clause.
onClusterToken :: Text
onClusterToken = "{{on_cluster}}"

-- | The statements sent for a file: its content with 'onClusterToken' replaced.
renderMigration :: Maybe Text -> ByteString.ByteString -> ByteString.ByteString
renderMigration clause bytes =
    Text.encodeUtf8 (Text.replace onClusterToken (maybe "" id clause) (decode bytes))

-------------------------------------------------------------------------------
-- Reading the ledger

-- | The ledger's rows; a line that is not two fields is skipped.
parseLedger :: Text -> [(Text, Text)]
parseLedger out =
    [ (n, Text.strip c)
    | line <- Text.lines out
    , (n, rest) <- [Text.breakOn "\t" line]
    , not (Text.null rest)
    , let c = Text.drop 1 rest
    ]

-- | What the ledger says of one migration.
data Status
    = Applied
    | Pending
    | -- | recorded with these checksums, none of them the planned one
      Mismatch ![Text]
    deriving (Eq, Show)

status :: [(Text, Text)] -> Migration -> Status
status rows m =
    case [c | (n, c) <- rows, n == m.migrationName] of
        [] -> Pending
        cs
            | all (== m.migrationChecksum) cs -> Applied
            | otherwise -> Mismatch (filter (/= m.migrationChecksum) cs)

mismatchText :: Migration -> [Text] -> Text
mismatchText m cs =
    "migration "
        <> m.migrationName
        <> " was applied with checksum "
        <> Text.intercalate ", " cs
        <> " but the file now has "
        <> m.migrationChecksum
        <> ": salmon never re-applies a migration, write a new one"

{- | The verdict on these migrations, from the ledger's rows: 'Success' when
every one is applied with its checksum; a 'Failure' naming every mismatch
first, else the pending ones. Rows for migrations not in the list are
ignored.
-}
interpretLedger :: [Migration] -> [(Text, Text)] -> CheckResult
interpretLedger ms rows =
    case (mismatches, pending) of
        ([], []) -> Success
        ([], _) -> Failure ("pending migrations: " <> Text.intercalate ", " pending)
        _ -> Failure (Text.intercalate "; " mismatches)
  where
    statuses = [(m, status rows m) | m <- ms]
    mismatches = [mismatchText m cs | (m, Mismatch cs) <- statuses]
    pending = [m.migrationName | (m, Pending) <- statuses]

{- | A missing ledger (or database) reads as no rows; any other error
'Nothing', which the checks read as 'Unknown'.
-}
readLedger :: Client -> DatabaseName -> IO (Maybe [(Text, Text)])
readLedger c db = do
    r <- adminQuery c (ledgerRowsSql db)
    pure $ case r of
        Right out -> Just (parseLedger out)
        Left err
            | missingLedger err -> Just []
            | otherwise -> Nothing

missingLedger :: Text -> Bool
missingLedger err = any (`Text.isInfixOf` err) ["UNKNOWN_TABLE", "UNKNOWN_DATABASE"]

checkMigrations :: Client -> Migrations -> IO CheckResult
checkMigrations c ms = maybe Unknown (interpretLedger ms.migrationsFiles) <$> readLedger c ms.migrationsDatabase

-------------------------------------------------------------------------------
-- Nodes

data MigrationChecksumMismatch = MigrationChecksumMismatch !Text
    deriving (Show)

instance Exception MigrationChecksumMismatch

data MigrationFileChanged = MigrationFileChanged !FilePath !Text !Text
    deriving (Show)

instance Exception MigrationFileChanged

data LedgerUnreadable = LedgerUnreadable !DatabaseName
    deriving (Show)

instance Exception LedgerUnreadable

{- | The migrations, applied in order. The @[Op]@ are what the ledger table
waits for (the ledger's database, when it is not @default@; the server's
readiness comes through the client's @Track'@). The node returned has no
effect of its own; its check is 'checkMigrations'.
-}
migrations :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> [Op] -> Migrations -> Op
migrations r clientT c prerequisites ms =
    op "clickhouse-migrations" (deps [chain]) $ \actions ->
        actions
            { ref = mkRef "clickhouse-migrations" (clientKey c, ms.migrationsDatabase)
            , help = Text.unwords ["apply", Text.pack (show (length ms.migrationsFiles)), "ClickHouse migrations recorded in", qualifiedLedger ms.migrationsDatabase]
            , notes = ["server: " <> clientKey c] <> clusterNote ms
            , check = checkMigrations c ms
            }
  where
    ledger = foldl inject (ledgerOp r clientT c ms) prerequisites
    chain = foldl (\prev m -> migrationOp r clientT c ms m `inject` prev) ledger ms.migrationsFiles

clusterNote :: Migrations -> [Text]
clusterNote ms = ["on cluster:" <> clause | Just clause <- [ms.migrationsOnCluster]]

-- | The ledger table, created if missing; checked on @system.tables@.
ledgerOp :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> Migrations -> Op
ledgerOp r clientT c ms =
    withBinaryStdinAtUp clientT (adminRun c) AdminBatch $ \run ->
        op "clickhouse-migrations-ledger" nodeps $ \actions ->
            actions
                { ref = mkRef "clickhouse-migrations-ledger" (clientKey c, db)
                , help = "create the ClickHouse migrations ledger " <> qualifiedLedger db
                , notes = ["server: " <> clientKey c] <> clusterNote ms
                , check = either (const Unknown) (interpretCount ("ledger " <> qualifiedLedger db <> " does not exist")) <$> adminQuery c (ledgerExistsSql db)
                , up = run (Text.encodeUtf8 (ledgerTableSql ms)) (contramap (CHMigrationLedger db) r)
                }
  where
    db = ms.migrationsDatabase

{- | One migration file. @up@ reads the ledger first: applied, nothing;
recorded with another checksum, 'MigrationChecksumMismatch'; pending, the
file (checked against its planned checksum) is sent, then recorded.
-}
migrationOp :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> Migrations -> Migration -> Op
migrationOp r clientT c ms m =
    withBinaryStdinAtUp clientT (adminRun c) AdminBatch $ \run ->
        op "clickhouse-migration" nodeps $ \actions ->
            actions
                { ref = mkRef "clickhouse-migration" (clientKey c, db, m.migrationName)
                , help = Text.unwords ["apply ClickHouse migration", m.migrationName, "from", Text.pack m.migrationPath]
                , notes = ["server: " <> clientKey c, "ledger: " <> qualifiedLedger db, "checksum: sha256:" <> m.migrationChecksum] <> clusterNote ms
                , check = maybe Unknown (\rows -> interpretLedger [m] rows) <$> readLedger c db
                , up = do
                    rows <- maybe (throwIO (LedgerUnreadable db)) pure =<< readLedger c db
                    case status rows m of
                        Applied -> pure ()
                        Mismatch cs -> throwIO (MigrationChecksumMismatch (mismatchText m cs))
                        Pending -> do
                            bytes <- ByteString.readFile m.migrationPath
                            let found = checksum bytes
                            if found /= m.migrationChecksum
                                then throwIO (MigrationFileChanged m.migrationPath m.migrationChecksum found)
                                else do
                                    run (renderMigration ms.migrationsOnCluster bytes) (contramap (CHApplyMigration m.migrationName) r)
                                    run (Text.encodeUtf8 (recordSql db m)) (contramap (CHRecordMigration m.migrationName) r)
                }
  where
    db = ms.migrationsDatabase
