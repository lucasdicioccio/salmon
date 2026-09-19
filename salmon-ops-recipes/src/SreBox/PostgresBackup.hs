{-# LANGUAGE DeriveGeneric #-}
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
    Credentials (..),
    credentialEnvironment,
    NamingPolicy (..),
    defaultNamingPolicy,
    dumpNameGlob,
    dumpNameFor,
    PgBackupConfig (..),
    defaultBackupConfig,

    -- * The recipe
    postgresBackup,
    scheduledBackup,
    backupScript,
    recentBackup,
    namedBackup,

    -- * The script
    renderBackupScript,
    backupFilePattern,
    shellQuote,
    shellExpand,

    -- * Freshness
    checkBackupFreshness,
    interpretBackupAge,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, getModificationTime, listDirectory)
import System.FilePath ((</>))

import Salmon.Actions.UpDown (CheckResult (..), skipIfFileExists)
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
    deriving (Eq, Show, Generic)

instance FromJSON GcsDestination
instance ToJSON GcsDestination

{- | How @pg_dump@ is told who it is, without ever putting a credential on a
command line.

Every one of these renders to __environment variables only__. That is the
whole point of the type: @\/proc\/\<pid\>\/cmdline@ is world-readable for as
long as the process lives, so a connection string on @pg_dump@'s argv is a
password any user on the box can read by running @ps@ at the right moment.
@\/proc\/\<pid\>\/environ@ is readable only by the process's own owner.

The four mechanisms, in rough order of how much they leave lying around:

* 'PeerAuth' -- nothing at all. The job runs as an OS user the cluster
  trusts over the local socket, which is what a backup job on the database
  host should normally use.
* 'ClientCertificate' -- a key on disk and no password anywhere, the client
  side of "SreBox.PostgresTls".
* 'ServiceFile' -- a libpq service file naming a connection, credentials
  included. The connection string never becomes an argument.
* 'PassFile' -- a @.pgpass@.

None of them is /created/ by this recipe: how a credential reaches the
machine is the caller's business, same rule as everywhere else here.
-}
data Credentials
    = -- | the OS user is the authentication
      PeerAuth
    | -- | @PGPASSFILE@
      PassFile FilePath
    | -- | @PGSERVICEFILE@ and @PGSERVICE@
      ServiceFile FilePath Text
    | -- | @PGSSLCERT@, @PGSSLKEY@, @PGSSLROOTCERT@ (with @PGSSLMODE=verify-ca@)
      ClientCertificate FilePath FilePath FilePath
    deriving (Eq, Show, Generic)

instance FromJSON Credentials
instance ToJSON Credentials

-- | The @export@ lines a 'Credentials' turns into.
credentialEnvironment :: Credentials -> [Text]
credentialEnvironment PeerAuth = []
credentialEnvironment (PassFile path) =
    ["export PGPASSFILE=" <> shellQuote (Text.pack path)]
credentialEnvironment (ServiceFile path service) =
    [ "export PGSERVICEFILE=" <> shellQuote (Text.pack path)
    , "export PGSERVICE=" <> shellQuote service
    ]
credentialEnvironment (ClientCertificate cert key ca) =
    [ "export PGSSLMODE=verify-ca"
    , "export PGSSLCERT=" <> shellQuote (Text.pack cert)
    , "export PGSSLKEY=" <> shellQuote (Text.pack key)
    , "export PGSSLROOTCERT=" <> shellQuote (Text.pack ca)
    ]

{- | What a dump is called.

Worth being a policy rather than a constant for two reasons that pull in
opposite directions. Operators want dumps that sort and are recognisable
from a listing months later. And anything that has to /fetch/ a dump has to be able
to predict its name -- which is why 'dumpNameFor' takes the timestamp as an
argument rather than reading the clock: a controller driving a backup on
another machine freezes the timestamp when it builds the directive, so both
ends name the same file. Letting each side call @date@ is the obvious
version and it loses the file whenever the two land either side of a second.
-}
data NamingPolicy = NamingPolicy
    { np_prefix :: Text
    , np_timestampFormat :: Text
    -- ^ a @date(1)@ format string, without the leading @+@
    , np_suffix :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON NamingPolicy
instance ToJSON NamingPolicy

-- | @\<database\>_%Y%m%d_%H%M%S.sql.gz@, flat.
defaultNamingPolicy :: Postgres.DatabaseName -> NamingPolicy
defaultNamingPolicy db =
    NamingPolicy
        { np_prefix = db
        , np_timestampFormat = "%Y%m%d_%H%M%S"
        , np_suffix = ".sql.gz"
        }

{- | The shell glob matching every dump this policy produces, used by the
retention @find@ and by the freshness check so the two cannot drift.
-}
dumpNameGlob :: NamingPolicy -> Text
dumpNameGlob policy = policy.np_prefix <> "_*" <> policy.np_suffix

{- | The name of the dump taken at a given (already formatted) timestamp,
relative to the backup directory.
-}
dumpNameFor :: NamingPolicy -> Text -> FilePath
dumpNameFor policy stamp =
    Text.unpack (policy.np_prefix <> "_" <> stamp <> policy.np_suffix)

data PgBackupConfig = PgBackupConfig
    { pgb_database :: Postgres.DatabaseName
    , pgb_role :: Maybe Postgres.RoleName
    -- ^ 'Nothing' lets libpq default the role to the OS user, which is what
    -- peer authentication over the local socket wants.
    , pgb_server :: Maybe Postgres.Server
    -- ^ 'Nothing' connects over the __unix socket__ rather than TCP. That is
    -- not a detail: @peer@ authentication only exists on the socket, so a
    -- backup job on the database host that names @127.0.0.1@ is asking for a
    -- password it does not have.
    , pgb_sudoUser :: Maybe Text
    -- ^ run @pg_dump@ as this OS user (@sudo -u@). With 'PeerAuth' this /is/
    -- the authentication; the cluster believes whoever the kernel says is on
    -- the other end of the socket.
    , pgb_dir :: FilePath
    -- ^ where dumps are written
    , pgb_scriptPath :: FilePath
    , pgb_osUser :: Text
    -- ^ the account cron runs the job as, and whose credentials @pg_dump@ uses
    , pgb_credentials :: Credentials
    , pgb_naming :: NamingPolicy
    , pgb_fixedTimestamp :: Maybe Text
    -- ^ when set, the script writes exactly this dump rather than one named
    -- for the moment it runs -- which is what lets a controller on another
    -- machine know the path to fetch. Pointless (and wrong) for a scheduled
    -- job, which would overwrite the same file forever.
    , pgb_retentionDays :: Int
    , pgb_schedule :: Cron.Schedule
    , pgb_gcs :: Maybe GcsDestination
    , pgb_maxAge :: NominalDiffTime
    -- ^ how old the newest dump may be before 'recentBackup' calls it stale.
    -- Give this slack over 'pgb_schedule': equal values make every check that
    -- lands just before the next run report a failure.
    }
    deriving (Eq, Show, Generic)

-- | Serialisable so a whole backup configuration can travel as a directive to
-- the machine that will run it -- see @salmon-pg-backup@.
instance FromJSON PgBackupConfig
instance ToJSON PgBackupConfig

{- | Daily at 03:17, keeping a week, with a freshness window of 26 hours.

The odd minute is deliberate (see 'Cron.dailyAt'), and the window is the
period plus two hours rather than exactly a day, so a run that is merely late
is not reported as a missing backup.
-}
defaultBackupConfig :: Postgres.DatabaseName -> Maybe Postgres.RoleName -> PgBackupConfig
defaultBackupConfig db role =
    PgBackupConfig
        { pgb_database = db
        , pgb_role = role
        , pgb_server = Nothing
        , pgb_sudoUser = Just "postgres"
        , pgb_dir = "/data/backups/postgresql"
        , pgb_scriptPath = "/opt/salmon/postgres/backup-" <> Text.unpack db <> ".sh"
        , pgb_osUser = "postgres"
        , pgb_credentials = PeerAuth
        , pgb_naming = defaultNamingPolicy db
        , pgb_fixedTimestamp = Nothing
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
backupFilePattern cfg =
    (Text.unpack (cfg.pgb_naming.np_prefix <> "_"), Text.unpack cfg.pgb_naming.np_suffix)

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
        , timestampLine
        , "BACKUP_FILE=\"${BACKUP_DIR}/" <> shellExpand (cfg.pgb_naming.np_prefix <> "_") <> "${TIMESTAMP}" <> shellExpand cfg.pgb_naming.np_suffix <> "\""
        , ""
        , "mkdir -p \"$BACKUP_DIR\""
        ]
            <> credentialLines
            <> [ ""
               , dumpLine
               , "echo \"backup completed: $BACKUP_FILE\""
               ]
            <> uploadLines
            <> [ ""
               , -- pruning last, and only if everything above succeeded:
                 -- under `set -e` a failed upload stops the script here, so a
                 -- dump that never reached the bucket is not also deleted
                 -- locally.
                 "find \"$BACKUP_DIR\" -name "
                    <> shellQuote (dumpNameGlob cfg.pgb_naming)
                    <> " -mtime +"
                    <> Text.pack (show cfg.pgb_retentionDays)
                    <> " -delete"
               ]
  where
    -- A driven backup freezes the timestamp in the directive so the machine
    -- that will fetch the dump knows its name; a scheduled one must not, or
    -- every run would overwrite one file forever.
    timestampLine = case cfg.pgb_fixedTimestamp of
        Just stamp -> "TIMESTAMP=" <> shellQuote stamp
        Nothing -> "TIMESTAMP=$(date +" <> shellQuote cfg.pgb_naming.np_timestampFormat <> ")"

    credentialLines = credentialEnvironment cfg.pgb_credentials

    -- `sudo -u postgres` and no -h is what peer authentication looks like;
    -- naming a host turns the same call into a TCP connection that pg_hba
    -- will want a password for.
    dumpLine =
        Text.concat
            [ maybe "" (\u -> "sudo -u " <> shellQuote u <> " ") cfg.pgb_sudoUser
            , "pg_dump"
            , maybe "" (\srv -> " -h " <> shellQuote srv.serverHost <> " -p " <> Text.pack (show srv.serverPort)) cfg.pgb_server
            , maybe "" (\u -> " -U " <> shellQuote u) cfg.pgb_role
            , " -d \"$DATABASE\" | gzip > \"$BACKUP_FILE\""
            ]

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

{- | Escapes a literal for use /inside/ a double-quoted string, where
'shellQuote' cannot be used because the surrounding quotes have to stay open
for a @${…}@ expansion next to it.

Backslash first, or the escapes added by the later passes get escaped again.
-}
shellExpand :: Text -> Text
shellExpand =
    Text.replace "\"" "\\\""
        . Text.replace "`" "\\`"
        . Text.replace "$" "\\$"
        . Text.replace "\\" "\\\\"

-------------------------------------------------------------------------------

{- | "The dump named by this timestamp exists", with @up@ being "produce it".

The one-shot counterpart to 'recentBackup', and the node a /driven/ backup
needs. Where 'recentBackup' asks a question about the state of the backup
directory ("is anything in here recent enough"), this one asks about a
single named file — which is the only question whose answer another machine
can act on, because the dump it is about to fetch has to be a path it can
name before the dump exists.

Requires 'pgb_fixedTimestamp' to be set to the same stamp, or the script
will name its output after the moment it runs and this check will never be
satisfied by it.
-}
namedBackup :: Reporter Report -> Track' (Binary "bash") -> PgBackupConfig -> Text -> Op
namedBackup r bash cfg stamp =
    withBinary bash Bash.bashrun (Bash.BashCommand cfg.pgb_scriptPath) $ \runScript ->
        op "pg-backup-named" (deps [backupScript cfg]) $ \actions ->
            actions
                { help = Text.unwords ["takes the backup", Text.pack path]
                , ref = mkRef "pg-backup-named" path
                , check = skipIfFileExists path
                , up = runScript (contramap RunBackup r)
                , down = pure ()
                }
  where
    path = cfg.pgb_dir </> dumpNameFor cfg.pgb_naming stamp
