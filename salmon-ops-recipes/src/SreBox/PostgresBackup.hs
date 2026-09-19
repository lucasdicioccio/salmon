{-# LANGUAGE OverloadedStrings #-}

{- | Periodic @pg_dump@ backups: the script, the schedule that runs it, and a
node that notices when no recent dump exists.

The shape is the one everybody writes by hand -- dump, compress, ship, prune
-- expressed so that the three questions salmon can answer about it are
answerable: is the job installed, is the script the one we meant, and __is
there actually a recent backup__. The last is the one a hand-rolled cron
entry never answers: a job that has been failing silently for three weeks
looks exactly like one that has been working, right up until a restore.

= The bug in the obvious version

The canonical script says:

> pg_dump … | gzip > "$BACKUP_FILE"
> if [ $? -eq 0 ]; then echo "Backup completed"; else echo "Backup failed!" >&2; exit 1; fi

@$?@ there is __gzip's__ exit status, not @pg_dump@'s. A dump that fails
half-way -- lost connection, permission denied on one table, disk full on the
server -- still leaves a perfectly valid gzip file and still reports success,
because @gzip@ compressed whatever it was given and exited @0@. The generated
script sets @pipefail@, which is what makes the failure of any stage of the
pipeline the failure of the whole thing.

= Retention is local only

'pgb_retentionDays' prunes the /local/ directory. When 'pgb_gcs' is set the
copies in the bucket are not pruned by this recipe at all: object lifecycle
is a property of the bucket, and expressing it here would mean this node
silently deleting objects a bucket policy says to keep. Set a lifecycle rule
on the bucket instead.
-}
module SreBox.PostgresBackup (
    Report (..),
    GcsDestination (..),
    PgBackupConfig (..),
    defaultBackupConfig,

    -- * The recipe
    postgresBackup,
    scheduledBackup,
    recentBackup,

    -- * The script
    renderBackupScript,
    backupFilePattern,

    -- * Freshness
    checkBackupFreshness,
    interpretBackupAge,
) where

import Control.Exception (SomeException, try)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, getModificationTime, listDirectory)
import System.FilePath ((</>))

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Bash as Bash
import Salmon.Builtin.Nodes.Binary (Binary, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.CronTask as Cron
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunBackup !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | A bucket (and optional prefix) each dump is copied to after it is written.
data GcsDestination = GcsDestination
    { gcs_bucket :: Text
    -- ^ bare bucket name, no @gs:\/\/@
    , gcs_prefix :: Text
    -- ^ may be empty; no leading or trailing slash
    }
    deriving (Eq, Show)

data PgBackupConfig = PgBackupConfig
    { pgb_database :: Postgres.DatabaseName
    , pgb_role :: Postgres.RoleName
    , pgb_server :: Postgres.Server
    , pgb_dir :: FilePath
    -- ^ where dumps are written
    , pgb_scriptPath :: FilePath
    , pgb_osUser :: Text
    -- ^ the account cron runs the job as, and whose credentials @pg_dump@ uses
    , pgb_passFile :: Maybe FilePath
    -- ^ a @.pgpass@ this recipe does __not__ create: how the password gets
    -- onto the box is the caller's business (see "SreBox.PostgresTls" for the
    -- certificate alternative, which needs no password at all).
    , pgb_retentionDays :: Int
    , pgb_schedule :: Cron.Schedule
    , pgb_gcs :: Maybe GcsDestination
    , pgb_maxAge :: NominalDiffTime
    -- ^ how old the newest dump may be before 'recentBackup' calls it stale.
    -- Give this slack over 'pgb_schedule': equal values make every check that
    -- lands just before the next run report a failure.
    }

{- | Daily at 03:17, keeping a week, with a freshness window of 26 hours.

The odd minute is deliberate (see 'Cron.dailyAt'), and the window is the
period plus two hours rather than exactly a day, so a run that is merely late
is not reported as a missing backup.
-}
defaultBackupConfig :: Postgres.DatabaseName -> Postgres.RoleName -> PgBackupConfig
defaultBackupConfig db role =
    PgBackupConfig
        { pgb_database = db
        , pgb_role = role
        , pgb_server = Postgres.localServer
        , pgb_dir = "/data/backups/postgresql"
        , pgb_scriptPath = "/opt/salmon/postgres/backup-" <> Text.unpack db <> ".sh"
        , pgb_osUser = "postgres"
        , pgb_passFile = Nothing
        , pgb_retentionDays = 7
        , pgb_schedule = Cron.dailyAt "3" "17"
        , pgb_gcs = Nothing
        , pgb_maxAge = 26 * 3600
        }

-------------------------------------------------------------------------------

{- | The whole thing: the script, the cron entry that runs it, and a node
asserting a recent dump exists.

Bringing this up therefore /takes a backup immediately/ if there is not
already a recent one — which is what you want the first time (waiting until
03:17 to discover the credentials are wrong is not a plan) and costs nothing
on later passes, since the freshness check skips the node.
-}
postgresBackup ::
    Reporter Report ->
    Track' (Binary "bash") ->
    PgBackupConfig ->
    Op
postgresBackup r bash cfg =
    op "pg-backup" (deps [scheduledBackup cfg, recentBackup r bash cfg]) $ \actions ->
        actions
            { help = Text.unwords ["backs up", cfg.pgb_database, "to", Text.pack cfg.pgb_dir]
            , ref = mkRef "pg-backup" (cfg.pgb_database, cfg.pgb_dir)
            }

-- | The script and the crontab entry, without taking a backup now.
scheduledBackup :: PgBackupConfig -> Op
scheduledBackup cfg =
    Cron.crontask (Track $ const scriptOp) task
  where
    task :: Cron.CronTask
    task =
        Cron.CronTask
            { Cron.name = "pg-backup-" <> cfg.pgb_database
            , Cron.user = cfg.pgb_osUser
            , Cron.schedule = cfg.pgb_schedule
            , Cron.command = "/bin/bash"
            , Cron.commandArgs = [Text.pack cfg.pgb_scriptPath]
            }

    scriptOp :: Op
    scriptOp = backupScript cfg

-- | The generated script, and the directory it writes into.
backupScript :: PgBackupConfig -> Op
backupScript cfg =
    FS.filecontents (FS.FileContents cfg.pgb_scriptPath (renderBackupScript cfg))
        `inject` FS.dir (FS.Directory cfg.pgb_dir)

{- | "There is a dump newer than 'pgb_maxAge'", with @up@ being "take one".

The node that makes this recipe worth more than a crontab line: its @check@
is a statement about the /backups/, not about the job, so under @run serve@
it is the thing that notices three weeks of silent failure. Under a one-shot
@run up@ it takes a dump only when one is missing.
-}
recentBackup :: Reporter Report -> Track' (Binary "bash") -> PgBackupConfig -> Op
recentBackup r bash cfg =
    withBinary bash Bash.bashrun (Bash.BashCommand cfg.pgb_scriptPath) $ \runScript ->
        op "pg-backup-recent" (deps [backupScript cfg]) $ \actions ->
            actions
                { help = Text.unwords ["ensures a recent backup of", cfg.pgb_database]
                , notes = ["up takes a backup; check reports on the newest one on disk"]
                , ref = mkRef "pg-backup-recent" (cfg.pgb_database, cfg.pgb_dir)
                , check = checkBackupFreshness cfg
                , up = runScript (contramap RunBackup r)
                , -- Deleting backups is not what tearing a backup job down
                  -- means, and doing it here would make `run down` the most
                  -- destructive command in the system.
                  down = pure ()
                }

-------------------------------------------------------------------------------

{- | The dump filenames this recipe writes and reads: @\<database\>_@ … @.sql.gz@.

Shared by the script (which creates them), the retention @find@ (which
deletes them) and 'checkBackupFreshness' (which ages them) so the three
cannot drift apart.
-}
backupFilePattern :: PgBackupConfig -> (String, String)
backupFilePattern cfg = (Text.unpack cfg.pgb_database <> "_", ".sql.gz")

-- | Ages the newest dump in 'pgb_dir'.
checkBackupFreshness :: PgBackupConfig -> IO CheckResult
checkBackupFreshness cfg = do
    now <- getCurrentTime
    newest <- newestBackup cfg
    pure (interpretBackupAge cfg.pgb_maxAge now newest)

{- | The verdict, split out from the filesystem so it is testable.

A directory with no dump at all is a 'Failure' rather than an
'Salmon.Actions.UpDown.Unknown': "we have never managed to back this up" is
the most actionable thing this check ever says, and the least worth being
tentative about.
-}
interpretBackupAge :: NominalDiffTime -> UTCTime -> Maybe (FilePath, UTCTime) -> CheckResult
interpretBackupAge _maxAge _now Nothing = Failure "no backup found"
interpretBackupAge maxAge now (Just (path, modified))
    | age <= maxAge = Success
    | otherwise =
        Failure $
            Text.pack path
                <> " is the newest backup and is "
                <> Text.pack (show (round (age / 3600) :: Int))
                <> "h old"
  where
    age = diffUTCTime now modified

{- | The most recently modified dump, or 'Nothing'.

An unreadable directory reads as "no backup", which is the same verdict and
the same call to action: whatever is wrong, there is nothing here to restore
from.
-}
newestBackup :: PgBackupConfig -> IO (Maybe (FilePath, UTCTime))
newestBackup cfg = do
    present <- doesDirectoryExist cfg.pgb_dir
    if not present
        then pure Nothing
        else do
            entries <- either (const []) id <$> tryIO (listDirectory cfg.pgb_dir)
            let (prefix, suffix) = backupFilePattern cfg
                dumps = [cfg.pgb_dir </> e | e <- entries, prefix `isPrefixOf` e, suffix `isSuffixOf` e]
            stamped <- traverse stamp dumps
            pure $ case [x | Just x <- stamped] of
                [] -> Nothing
                xs -> Just (maximumOn snd xs)
  where
    stamp path = either (const Nothing) (Just . (,) path) <$> tryIO (getModificationTime path)

    maximumOn f = foldr1 (\a b -> if f a >= f b then a else b)

tryIO :: IO a -> IO (Either SomeException a)
tryIO = try

-------------------------------------------------------------------------------

{- | The backup script.

Everything it needs is interpolated at graph-declaration time, so the file on
disk is fully explicit -- readable by an operator who has never heard of
salmon, and diffable when the config changes (which is also what lets
'FS.filecontents' notice a hand-edit and put it back).
-}
renderBackupScript :: PgBackupConfig -> Text
renderBackupScript cfg =
    Text.unlines $
        [ "#!/bin/bash"
        , -- pipefail is the whole point: without it `pg_dump | gzip` reports
          -- gzip's success and a failed dump is stored as a valid archive.
          "set -euo pipefail"
        , ""
        , "BACKUP_DIR=" <> shellQuote (Text.pack cfg.pgb_dir)
        , "DATABASE=" <> shellQuote cfg.pgb_database
        , "TIMESTAMP=$(date +%Y%m%d_%H%M%S)"
        , "BACKUP_FILE=\"${BACKUP_DIR}/${DATABASE}_${TIMESTAMP}.sql.gz\""
        , ""
        , "mkdir -p \"$BACKUP_DIR\""
        ]
            <> passFileLines
            <> [ ""
               , "pg_dump"
                    <> " -h "
                    <> shellQuote cfg.pgb_server.serverHost
                    <> " -p "
                    <> Text.pack (show cfg.pgb_server.serverPort)
                    <> " -U "
                    <> shellQuote cfg.pgb_role
                    <> " -d \"$DATABASE\" | gzip > \"$BACKUP_FILE\""
               , "echo \"backup completed: $BACKUP_FILE\""
               ]
            <> uploadLines
            <> [ ""
               , -- pruning last, and only if everything above succeeded:
                 -- under `set -e` a failed upload stops the script here, so a
                 -- dump that never reached the bucket is not also deleted
                 -- locally.
                 "find \"$BACKUP_DIR\" -name "
                    <> shellQuote (cfg.pgb_database <> "_*.sql.gz")
                    <> " -mtime +"
                    <> Text.pack (show cfg.pgb_retentionDays)
                    <> " -delete"
               ]
  where
    passFileLines = case cfg.pgb_passFile of
        Nothing -> []
        Just path -> ["export PGPASSFILE=" <> shellQuote (Text.pack path)]

    uploadLines = case cfg.pgb_gcs of
        Nothing -> []
        Just dest ->
            [ ""
            , "gcloud storage cp \"$BACKUP_FILE\" " <> shellQuote (destination dest)
            ]

    destination :: GcsDestination -> Text
    destination dest
        | Text.null dest.gcs_prefix = "gs://" <> dest.gcs_bucket <> "/"
        | otherwise = "gs://" <> dest.gcs_bucket <> "/" <> dest.gcs_prefix <> "/"

{- | POSIX single-quoting, the same rule as
"Salmon.Builtin.Nodes.Gcp.LoadBalancing".@shellQuote@: every value
interpolated into the script goes through it, because database and role names
come from a caller and end up inside a @bash -c@ context.
-}
shellQuote :: Text -> Text
shellQuote t = "'" <> Text.replace "'" "'\\''" t <> "'"
