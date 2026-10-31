{- | What a PostgreSQL cluster needs before something can read its changes
by logical decoding (a CDC tool such as PeerDB, see
"Salmon.Builtin.Nodes.PeerDB"): @wal_level = logical@, a role allowed to
replicate, a publication naming the tables, and possibly a logical
replication slot.

Every node here talks to the cluster as the @postgres@ OS user, as the rest
of "Salmon.Builtin.Nodes.Postgres" does, and has a real check.

Nothing here drops or lowers anything: @down@ is nothing for all four. A
publication or a slot that disappears under a running CDC mirror breaks it,
and PostgreSQL refuses to start with @wal_level@ below @logical@ while a
logical slot exists. Taking them away is an operator's decision.

None of these nodes names a primary: they are declared against one cluster
on one port, and on a standby they fail as PostgreSQL makes them fail.
-}
module Salmon.Builtin.Nodes.Postgres.Logical where

import Control.Exception (Exception, throwIO)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinaryStdin)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Postgres (
    DatabaseName,
    Port,
    PsqlBatch (..),
    ReplicationSlotName,
    RoleName,
    dollarQuote,
    psqlBatchIn_Sudo,
    psqlBatchRun_Sudo,
    psqlQueryIn_Sudo,
    psqlQuery_Sudo,
    quoteIdent,
    quoteLiteral,
 )
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = LRWalLevel !Binary.Report
    | LRReplicationRole !RoleName !Binary.Report
    | LRPublication !PublicationName !Binary.Report
    | LRSlot !ReplicationSlotName !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
-- wal_level

{- | @ALTER SYSTEM SET wal_level = logical@, then a reload, which applies
nothing (the setting needs a restart) but makes the server report the
setting as @pending_restart@: follow this node with
"Salmon.Builtin.Nodes.Postgres".'Salmon.Builtin.Nodes.Postgres.restartClusterIfPending'
(@restart \`inject\` logicalWalLevel ...@), which is the node that restarts.

The check reads what the configuration files say (the last entry of
@pg_file_settings@), and the running value when no file sets it: a level
written and waiting for its restart is this node done, the restart node's
business.

The 'Ref' is 'Salmon.Builtin.Nodes.Postgres.alterSystemSet''s for
@wal_level@ on this port, on purpose: it is the same effect site. A
declaration that also uses
'Salmon.Builtin.Nodes.Postgres.primaryReplicationSetup' (which sets
@replica@) has two nodes on one site, which the fold reports as
@Conflicting@ (last writer wins); @logical@ includes everything @replica@
gives a streaming standby, so drop the other one.

@down@ is nothing.
-}
logicalWalLevel :: Reporter Report -> Track' (Binary "psql") -> Port -> Op
logicalWalLevel r psql port =
    withBinaryStdin psql (psqlBatchRun_Sudo port) PsqlBatch (Text.encodeUtf8 walLevelSql) $ \apply ->
        op "pg-wal-level-logical" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-alter-system" (port, "wal_level" :: Text)
                , help = "ALTER SYSTEM SET wal_level = logical"
                , notes = ["port " <> Text.pack (show port), "takes effect at the next restart of the cluster", "down leaves wal_level as it is"]
                , check = either (const Unknown) interpretWalLevel <$> psqlQuery_Sudo port walLevelInFilesSql
                , up = apply (contramap LRWalLevel r)
                }

walLevelSql :: Text
walLevelSql = "ALTER SYSTEM SET wal_level = 'logical';\nSELECT pg_reload_conf();\n"

-- | What the files say @wal_level@ is, or what the server runs with when no file sets it.
walLevelInFilesSql :: Text
walLevelInFilesSql =
    "SELECT coalesce((SELECT setting FROM pg_file_settings WHERE name = 'wal_level' ORDER BY seqno DESC LIMIT 1), current_setting('wal_level'))"

interpretWalLevel :: Text -> CheckResult
interpretWalLevel out = case Text.strip out of
    "logical" -> Success
    "" -> Unknown
    other -> Failure ("wal_level is " <> other <> ", logical decoding needs logical")

-------------------------------------------------------------------------------
-- The REPLICATION attribute

{- | @ALTER ROLE name WITH REPLICATION@ on a role declared elsewhere (a
'Salmon.Builtin.Nodes.Postgres.userPassFile', say: @inject@ it). Checked on
@pg_roles.rolreplication@. @down@ takes nothing away: another declaration
(a standby's replication user) may count on it.
-}
replicationRole :: Reporter Report -> Track' (Binary "psql") -> Port -> RoleName -> Op
replicationRole r psql port name =
    withBinaryStdin psql (psqlBatchRun_Sudo port) PsqlBatch (Text.encodeUtf8 (replicationRoleSql name)) $ \apply ->
        op "pg-role-replication" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-role-replication" (port, name)
                , help = "give role " <> name <> " the REPLICATION attribute"
                , notes = ["port " <> Text.pack (show port)]
                , check = either (const Unknown) (interpretReplicationRole name) <$> psqlQuery_Sudo port (replicationRoleCheckSql name)
                , up = apply (contramap (LRReplicationRole name) r)
                }

replicationRoleSql :: RoleName -> Text
replicationRoleSql name = "ALTER ROLE " <> quoteIdent name <> " WITH REPLICATION;\n"

replicationRoleCheckSql :: RoleName -> Text
replicationRoleCheckSql name = "SELECT rolreplication FROM pg_roles WHERE rolname = " <> quoteLiteral name

interpretReplicationRole :: RoleName -> Text -> CheckResult
interpretReplicationRole name out = case Text.strip out of
    "t" -> Success
    "f" -> Failure ("role " <> name <> " may not replicate")
    "" -> Failure ("role " <> name <> " does not exist")
    _ -> Unknown

-------------------------------------------------------------------------------
-- Publications

type PublicationName = Text

-- | A table, by schema and name.
data QualifiedTable = QualifiedTable {tableSchema :: Text, tableName :: Text}
    deriving (Eq, Ord, Show)

-- | @public.name@.
publicTable :: Text -> QualifiedTable
publicTable = QualifiedTable "public"

renderQualified :: QualifiedTable -> Text
renderQualified t = t.tableSchema <> "." <> t.tableName

quoteQualified :: QualifiedTable -> Text
quoteQualified t = quoteIdent t.tableSchema <> "." <> quoteIdent t.tableName

-- | A publication of exactly these tables (at least one), in one database.
data Publication
    = Publication
    { pubName :: PublicationName
    , pubDatabase :: DatabaseName
    , pubTables :: [QualifiedTable]
    }
    deriving (Eq, Show)

-- | The comment prefix on a publication salmon made.
publicationMarker :: Text
publicationMarker = "salmon-publication:"

{- | A publication of the declared tables, created if missing and set to
exactly them if present (@ALTER PUBLICATION ... SET TABLE@: a table taken
out of the declaration stops being published; no row anywhere is touched).

Salmon only touches a publication it made: one it creates carries a
'publicationMarker' comment, and @up@ refuses (and changes nothing) when a
publication of that name exists without it. The whole batch is one
transaction.

Creating a publication of a table needs to own it; the @postgres@ OS user
does, which is the reason to declare the publication here rather than let a
CDC tool's less privileged role make its own.

The check: absent or unmarked is a 'Failure' (@up@ then creates or refuses),
a different set of tables is a 'Failure' naming them. @down@ is nothing.
-}
publication :: Reporter Report -> Track' (Binary "psql") -> Port -> Publication -> Op
publication r psql port pub =
    withBinaryStdin psql (psqlBatchIn_Sudo port pub.pubDatabase) PsqlBatch (Text.encodeUtf8 (publicationSql pub)) $ \apply ->
        op "pg-publication" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-publication" (port, pub.pubDatabase, pub.pubName)
                , help = "publication " <> pub.pubName <> " in " <> pub.pubDatabase
                , notes = ["port " <> Text.pack (show port), "tables: " <> Text.intercalate ", " (fmap renderQualified (sort pub.pubTables))]
                , check = checkPublication port pub
                , up =
                    if null pub.pubTables
                        then throwIO (LogicalRefused ("publication " <> pub.pubName <> " declares no table"))
                        else apply (contramap (LRPublication pub.pubName) r)
                }

{- | The batch 'publication' sends: the refusal of an unmarked publication,
a @CREATE@ when there is none (through @\\gexec@), the @SET TABLE@ and the
marker, in a transaction.
-}
publicationSql :: Publication -> Text
publicationSql pub =
    Text.unlines
        [ "BEGIN;"
        , "DO "
            <> dollarQuote
                ( "BEGIN IF EXISTS (SELECT 1 FROM pg_publication p WHERE p.pubname = "
                    <> quoteLiteral pub.pubName
                    <> " AND coalesce(obj_description(p.oid, 'pg_publication'), '') NOT LIKE "
                    <> quoteLiteral (publicationMarker <> "%")
                    <> ") THEN RAISE EXCEPTION '%', "
                    <> quoteLiteral ("refusing to change publication " <> pub.pubName <> ": salmon did not make it (no " <> publicationMarker <> " comment)")
                    <> "; END IF; END"
                )
            <> ";"
        , "SELECT "
            <> quoteLiteral ("CREATE PUBLICATION " <> quoteIdent pub.pubName <> " FOR TABLE " <> tables)
            <> " WHERE NOT EXISTS (SELECT 1 FROM pg_publication WHERE pubname = "
            <> quoteLiteral pub.pubName
            <> ") \\gexec"
        , "ALTER PUBLICATION " <> quoteIdent pub.pubName <> " SET TABLE " <> tables <> ";"
        , "COMMENT ON PUBLICATION " <> quoteIdent pub.pubName <> " IS " <> quoteLiteral (publicationMarker <> " " <> pub.pubDatabase) <> ";"
        , "COMMIT;"
        ]
  where
    tables = Text.intercalate ", " (fmap quoteQualified pub.pubTables)

-- | One row, @present:COMMENT@, when the publication exists.
publicationMarkerSql :: Publication -> Text
publicationMarkerSql pub =
    "SELECT 'present:' || coalesce(obj_description(oid, 'pg_publication'), '') FROM pg_publication WHERE pubname = "
        <> quoteLiteral pub.pubName

-- | The published tables, @schema.table@, one per line.
publicationTablesSql :: Publication -> Text
publicationTablesSql pub =
    "SELECT schemaname || '.' || tablename FROM pg_publication_tables WHERE pubname = "
        <> quoteLiteral pub.pubName
        <> " ORDER BY 1"

checkPublication :: Port -> Publication -> IO CheckResult
checkPublication port pub = do
    marker <- psqlQueryIn_Sudo port pub.pubDatabase (publicationMarkerSql pub)
    tables <- psqlQueryIn_Sudo port pub.pubDatabase (publicationTablesSql pub)
    pure $ case (marker, tables) of
        (Right m, Right t) -> interpretPublication pub m t
        _ -> Unknown

interpretPublication :: Publication -> Text -> Text -> CheckResult
interpretPublication pub markerOut tablesOut =
    case Text.stripPrefix "present:" (Text.strip markerOut) of
        Nothing -> Failure ("publication " <> pub.pubName <> " does not exist in " <> pub.pubDatabase)
        Just comment
            | not (publicationMarker `Text.isPrefixOf` comment) -> Failure ("publication " <> pub.pubName <> " exists but salmon did not make it")
            | held /= declared ->
                Failure
                    ( "publication "
                        <> pub.pubName
                        <> " publishes "
                        <> list held
                        <> ", declared "
                        <> list declared
                    )
            | otherwise -> Success
  where
    held = sort (filter (not . Text.null) (fmap Text.strip (Text.lines tablesOut)))
    declared = sort (fmap renderQualified pub.pubTables)
    list [] = "nothing"
    list xs = Text.intercalate ", " xs

-------------------------------------------------------------------------------
-- Logical replication slots

-- | The output plugin of the slots made here: the built-in one, which PeerDB and native subscriptions read.
slotPlugin :: Text
slotPlugin = "pgoutput"

{- | A logical replication slot (plugin @pgoutput@) in one database, created
if missing. A slot of that name that exists with another type, plugin or
database is refused, not replaced.

A slot keeps every WAL segment its consumer has not confirmed, for as long
as the consumer is away: a slot nobody reads fills the disk. Cap it with
@max_slot_wal_keep_size@ (past the cap PostgreSQL invalidates the slot and
the consumer has to start over). PeerDB makes its own slot for a mirror
that names none, which is the simpler choice; declare one here when it
should exist before the mirror, or outlive it.

The 'Ref' is the physical slot node's
('Salmon.Builtin.Nodes.Postgres.replicationSlot'), the slot's name: one
cluster has one slot of a name.

@down@ is nothing: dropping a slot under a mirror loses its place in the
stream.
-}
logicalSlot :: Reporter Report -> Track' (Binary "psql") -> Port -> DatabaseName -> ReplicationSlotName -> Op
logicalSlot r psql port db slot =
    withBinaryStdin psql (psqlBatchIn_Sudo port db) PsqlBatch (Text.encodeUtf8 (logicalSlotSql db slot)) $ \apply ->
        op "pg-logical-slot" nodeps $ \actions ->
            actions
                { ref = mkRef "pg-replication-slot" slot
                , help = "logical replication slot " <> slot <> " in " <> db
                , notes = ["port " <> Text.pack (show port), "plugin " <> slotPlugin, "down leaves the slot: an unread slot keeps WAL"]
                , check = either (const Unknown) (interpretSlot db slot) <$> psqlQueryIn_Sudo port db (slotCheckSql slot)
                , up = apply (contramap (LRSlot slot) r)
                }

logicalSlotSql :: DatabaseName -> ReplicationSlotName -> Text
logicalSlotSql db slot =
    Text.unlines
        [ "DO "
            <> dollarQuote
                ( "BEGIN IF EXISTS (SELECT 1 FROM pg_replication_slots WHERE slot_name = "
                    <> quoteLiteral slot
                    <> " AND NOT (slot_type = 'logical' AND plugin = "
                    <> quoteLiteral slotPlugin
                    <> " AND database = "
                    <> quoteLiteral db
                    <> ")) THEN RAISE EXCEPTION '%', "
                    <> quoteLiteral ("refusing slot " <> slot <> ": a slot of that name exists that is not a " <> slotPlugin <> " slot of " <> db)
                    <> "; END IF; END"
                )
            <> ";"
        , "SELECT pg_create_logical_replication_slot("
            <> quoteLiteral slot
            <> ", "
            <> quoteLiteral slotPlugin
            <> ") WHERE NOT EXISTS (SELECT 1 FROM pg_replication_slots WHERE slot_name = "
            <> quoteLiteral slot
            <> ");"
        ]

-- | One row, @type|plugin|database@, when the slot exists.
slotCheckSql :: ReplicationSlotName -> Text
slotCheckSql slot =
    "SELECT slot_type || '|' || coalesce(plugin, '') || '|' || coalesce(database, '') FROM pg_replication_slots WHERE slot_name = "
        <> quoteLiteral slot

interpretSlot :: DatabaseName -> ReplicationSlotName -> Text -> CheckResult
interpretSlot db slot out = case Text.strip out of
    "" -> Failure ("replication slot " <> slot <> " does not exist")
    row
        | row == "logical|" <> slotPlugin <> "|" <> db -> Success
        | otherwise -> Failure ("replication slot " <> slot <> " is " <> row <> ", declared logical|" <> slotPlugin <> "|" <> db)

-------------------------------------------------------------------------------

newtype LogicalRefused = LogicalRefused Text
    deriving (Show)

instance Exception LogicalRefused
