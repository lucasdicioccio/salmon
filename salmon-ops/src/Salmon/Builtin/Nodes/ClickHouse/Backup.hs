{- | ClickHouse backups as files, made on a schedule, shipped to GCS, and
restored from either, with ClickHouse's own @BACKUP@ and @RESTORE@.

__The disk.__ Backups are archives (@NAME-YYYYMMDDTHHMMSSZ.zip@) on a local
disk ClickHouse knows as @backups@ ('renderBackupConfig'): a host directory
('backupDirectory', created if missing and never removed) bound into the
container at 'backupMountPoint'. 'backupServerOp' is
"Salmon.Builtin.Nodes.ClickHouse".'ClickHouse.serverOp' with that drop-in
written, bound read-only and watched, and the directory bound read-write.
The image's entrypoint gives every disk path of the configuration to the
@clickhouse@ user at start, so the files are that user's (mode @0640@).

__The schedule.__ A backup is a run of a shell script ('renderBackupScript')
by a 'Job.Job' on a 'Job.Timer' ('scheduledBackup'): @BACKUP DATABASE ...
TO Disk('backups', NAME)@ through the server's client, then, when a
'GcsTarget' is declared, @gcloud storage rsync@ of this plan's files to the
bucket (never deleting there: retention in the bucket is a
'Storage.bucketLifecycle''s), then, when 'planKeepLocal' says so, the oldest
local files of this plan removed. The node that says how the last run went
is 'Job.completedRun' on that job ('scheduledBackup' stands on it): a pass
finding the last run failed, never run, or older than the script runs one,
and fails when it fails.

__GCS credentials__ are gcloud's own (the machine's service account, or the
account gcloud is logged in as), or a pre-provisioned credential file named
by 'gcsCredentialFile', which gcloud reads when it runs
(@CLOUDSDK_AUTH_CREDENTIAL_FILE_OVERRIDE@). Salmon never reads it, and only
its path appears anywhere.

__Restore__ ('restore') puts one database of one backup file back, from the
local directory or fetched from GCS into it first. It destroys data when the
database is there, so it is guarded by identity: a restored database carries
a comment naming the backup it came from ('restoreMarker'), and

* no such database: restored;
* the database is that restore already: nothing to do;
* the database holds no table: dropped (nothing is lost), then restored;
* anything else (data salmon cannot prove is this restore): refused, unless
  the declaration says 'restoreMayDiscard', the operator's flag, as
  @pair_may_discard@ is for a Postgres pair.

Not covered: incremental backups (@base_backup@), backups of a cluster
(@ON CLUSTER@: see the module notes), a remote or password-protected
administrator (as for every node of "Salmon.Builtin.Nodes.ClickHouse").
-}
module Salmon.Builtin.Nodes.ClickHouse.Backup (
    -- * The backup disk
    backupDiskName,
    backupMountPoint,
    renderBackupConfig,
    backupConfigPath,
    backupDirectory,
    backupServerContainer,
    backupServerOp,

    -- * Scheduled backups
    GcsTarget (..),
    gcsTarget,
    gcsDestination,
    BackupPlan (..),
    backupPlan,
    planProblems,
    backupFileRegex,
    backupSqlParts,
    shipRsync,
    renderBackupScript,
    backupScriptPath,
    backupStampPath,
    backupJob,
    backupCompletion,
    backupScript,
    scheduledBackup,

    -- * Restore
    RestoreSource (..),
    Restore (..),
    restoreFrom,
    restoreProblems,
    restoreMarker,
    restoreSql,
    markSql,
    dropSql,
    targetSql,
    TargetState (..),
    interpretTarget,
    restoreVerdict,
    RestoreStep (..),
    restoreDecision,
    RestoreRefused (..),
    restore,
    Report (..),

    -- * Helpers
    shellQuote,
    gcsEnvironment,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, unless)
import Data.Char (isAlphaNum, isAscii)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesFileExist, renameFile)
import System.Environment (getEnvironment)
import System.FilePath (isAbsolute, (</>))
import System.Process (CmdSpec (..), CreateProcess (..))

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), justInstall)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.ClickHouse (Client, DatabaseName, Server (..), adminQuery, adminRun, AdminBatch (..), clientArgv, clientKey, quoteIdent, quoteLiteral)
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = BackupFetch !Text !Storage.Report
    | RestoreDrop !DatabaseName !Binary.Report
    | RestoreRun !DatabaseName !Binary.Report
    | RestoreMark !DatabaseName !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
-- The backup disk

-- | The disk the backups are written to, as ClickHouse names it.
backupDiskName :: Text
backupDiskName = "backups"

-- | Where the backup directory is in the container.
backupMountPoint :: FilePath
backupMountPoint = "/var/lib/clickhouse-backups"

{- | The @config.d@ drop-in declaring the @backups@ disk (a local one at
'backupMountPoint') and allowing @BACKUP@ and @RESTORE@ on it and nowhere
else. Fixed text: nothing in it is declared.
-}
renderBackupConfig :: Text
renderBackupConfig =
    Text.unlines
        [ "<clickhouse>"
        , "    <storage_configuration>"
        , "        <disks>"
        , "            <" <> backupDiskName <> ">"
        , "                <type>local</type>"
        , "                <path>" <> mount <> "</path>"
        , "            </" <> backupDiskName <> ">"
        , "        </disks>"
        , "    </storage_configuration>"
        , "    <backups>"
        , "        <allowed_disk>" <> backupDiskName <> "</allowed_disk>"
        , "        <allowed_path>" <> mount <> "</allowed_path>"
        , "    </backups>"
        , "</clickhouse>"
        ]
  where
    mount = Text.pack backupMountPoint <> "/"

-- | The drop-in's path on the host, next to the server's two.
backupConfigPath :: Server -> FilePath
backupConfigPath s = s.serverConfigDir </> "salmon-backups.xml"

{- | The host directory holding the backups: created if missing, left by
@down@ (it holds what ClickHouse and the restores wrote, and salmon removes
none of it). 'FS.sharedDir', so it is keyed @"shared-directory"@ on the path.
-}
backupDirectory :: FilePath -> Op
backupDirectory path = FS.sharedDir (FS.Directory path)

{- | 'ClickHouse.serverContainer' with the backup directory bound read-write
at 'backupMountPoint' and the drop-in bound read-only and watched (a change
restarts the container). The directory's bind does not create it: that is
'backupDirectory''s, which 'backupServerOp' stands on.
-}
backupServerContainer :: FilePath -> Server -> Quadlet.Container
backupServerContainer hostDir s =
    base
        { Quadlet.containerBinds =
            base.containerBinds
                <> [ (Quadlet.bind hostDir backupMountPoint){Quadlet.bindCreate = Nothing}
                   , (Quadlet.bind (backupConfigPath s) "/etc/clickhouse-server/config.d/salmon-backups.xml")
                        { Quadlet.bindMode = Podman.ReadOnly
                        , Quadlet.bindCreate = Nothing
                        }
                   ]
        , Quadlet.containerWatched = base.containerWatched <> [backupConfigPath s]
        }
  where
    base = ClickHouse.serverContainer s

{- | 'ClickHouse.serverOp' for a server that takes backups in this host
directory: the server's two drop-ins and 'renderBackupConfig', the
directory ('backupDirectory'), then the quadlet ('backupServerContainer').
A server declared with 'ClickHouse.serverOp' is unchanged by this module.
-}
backupServerOp :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> FilePath -> Server -> Op
backupServerOp r systemctl hostDir s =
    Quadlet.quadletContainer r systemctl (Track (const files)) (backupServerContainer hostDir s)
  where
    files =
        op
            "clickhouse-config-files"
            ( deps
                [ ClickHouse.serverConfigFile (ClickHouse.serverConfigPath s) s.serverConfig
                , ClickHouse.adminAccessFile (ClickHouse.adminAccessPath s)
                , FS.filecontentsInSharedDir (FS.FileContents (backupConfigPath s) renderBackupConfig)
                , backupDirectory hostDir
                ]
            )
            $ \actions -> actions{help = "ClickHouse drop-ins in " <> Text.pack s.serverConfigDir <> " and backups in " <> Text.pack hostDir}

-------------------------------------------------------------------------------
-- Scheduled backups

-- | Where in GCS a plan's files go, and with whose credentials.
data GcsTarget = GcsTarget
    { gcsBucket :: Storage.Bucket
    , gcsPrefix :: Text
    -- ^ without the slashes around it; empty for the bucket's root
    , gcsCredentialFile :: Maybe FilePath
    -- ^ a pre-provisioned credential file (a service account key) gcloud
    -- reads when it runs; 'Nothing' is gcloud's own account. Its path is
    -- public, its contents are never read by salmon.
    }
    deriving (Eq, Show)

-- | gcloud's own credentials.
gcsTarget :: Storage.Bucket -> Text -> GcsTarget
gcsTarget b prefix = GcsTarget b prefix Nothing

-- | @gs:\/\/BUCKET@ or @gs:\/\/BUCKET\/PREFIX@.
gcsDestination :: GcsTarget -> Text
gcsDestination g = case Text.dropAround (== '/') (Text.strip g.gcsPrefix) of
    "" -> "gs://" <> g.gcsBucket.bucketName
    prefix -> "gs://" <> g.gcsBucket.bucketName <> "/" <> prefix

-- | What a backup is of, where it goes, and what runs it.
data BackupPlan = BackupPlan
    { planName :: Text
    -- ^ names the files (@NAME-YYYYMMDDTHHMMSSZ.zip@), the job, the timer
    -- and the script. Letters, digits, @_@ and @-@.
    , planClient :: Client
    , planDatabases :: [DatabaseName]
    , planHostDir :: FilePath
    -- ^ the host side of the @backups@ disk ('backupServerOp''s directory)
    , planScriptDir :: FilePath
    -- ^ where the script and the job's stamp are written: a directory of
    -- the caller's, created if missing
    , planGcs :: Maybe GcsTarget
    , planKeepLocal :: Maybe Int
    -- ^ how many of this plan's files to keep in 'planHostDir' after a run
    -- (the newest); 'Nothing' keeps them all
    , planTimeout :: Maybe Int
    -- ^ seconds one run may take ('Job.jobTimeout'); 'Nothing' is no limit
    }
    deriving (Eq, Show)

-- | No GCS, every local file kept, no time limit.
backupPlan :: Text -> Client -> [DatabaseName] -> FilePath -> FilePath -> BackupPlan
backupPlan name c dbs hostDir scriptDir =
    BackupPlan
        { planName = name
        , planClient = c
        , planDatabases = dbs
        , planHostDir = hostDir
        , planScriptDir = scriptDir
        , planGcs = Nothing
        , planKeepLocal = Nothing
        , planTimeout = Nothing
        }

-- | Why the plan cannot be rendered.
planProblems :: BackupPlan -> [Text]
planProblems p =
    mconcat
        [ ["the plan has no name" | Text.null p.planName]
        , ["the plan name is not letters, digits, _ and -: " <> p.planName | not (Text.all plainChar p.planName)]
        , ["the plan backs up no database" | null p.planDatabases]
        , ["an empty database name" | any Text.null p.planDatabases]
        , ["the backup directory is not an absolute path" | not (isAbsolute p.planHostDir)]
        , ["the script directory is not an absolute path" | not (isAbsolute p.planScriptDir)]
        , ["a path holds a line break" | any (Text.any (`elem` ("\n\r" :: String)) . Text.pack) [p.planHostDir, p.planScriptDir]]
        , ["keeping fewer than one local backup" | Just n <- [p.planKeepLocal], n < 1]
        , ["the GCS prefix has an empty or dotted segment" | Just g <- [p.planGcs], badPrefix g.gcsPrefix]
        , ["the credential file is not an absolute path" | Just g <- [p.planGcs], Just f <- [g.gcsCredentialFile], not (isAbsolute f)]
        ]
  where
    badPrefix raw = case Text.dropAround (== '/') (Text.strip raw) of
        "" -> False
        prefix -> any (`elem` ["", ".", ".."]) (Text.splitOn "/" prefix)

plainChar :: Char -> Bool
plainChar c = isAscii c && (isAlphaNum c || c `elem` ("_-" :: String))

-- | The files of this plan, and nothing else: a Python regular expression, anchored.
backupFileRegex :: BackupPlan -> Text
backupFileRegex p = "^" <> p.planName <> "-[0-9]{8}T[0-9]{6}Z\\.zip$"

{- | The statement making a backup, around the file name the script picks
at run time: @BACKUP DATABASE `a`, DATABASE `b` TO Disk('backups', '@ and
@')@.
-}
backupSqlParts :: BackupPlan -> (Text, Text)
backupSqlParts p =
    ( "BACKUP "
        <> Text.intercalate ", " ["DATABASE " <> quoteIdent db | db <- p.planDatabases]
        <> " TO Disk("
        <> quoteLiteral backupDiskName
        <> ", '"
    , "')"
    )

{- | The @gcloud storage rsync@ shipping this plan's files: the backup
directory to the destination, files of other plans excluded, nothing
deleted in the bucket.
-}
shipRsync :: BackupPlan -> GcsTarget -> Storage.Rsync
shipRsync p g =
    Storage.Rsync
        { Storage.rsyncProject = g.gcsBucket.bucketProject
        , Storage.rsyncSource = p.planHostDir
        , Storage.rsyncDestination = gcsDestination g
        , Storage.rsyncDeleteUnmatched = False
        , Storage.rsyncCacheControl = Nothing
        , Storage.rsyncExclude = Just ("^(?!" <> Text.drop 1 (backupFileRegex p) <> ").*$")
        , Storage.rsyncDryRun = False
        }

{- | The script a run is. The backup goes through the server's client
(@podman exec -i@ for a container), the statement on its standard input;
@set -e@ stops the run at the first command that fails, so a backup that
fails ships and prunes nothing, and a failed shipment prunes nothing.
-}
renderBackupScript :: BackupPlan -> Text
renderBackupScript p =
    Text.unlines $
        mconcat
            [ ["#!/bin/sh", "# salmon: ClickHouse backup " <> p.planName <> " of " <> Text.intercalate ", " p.planDatabases, "set -eu"]
            , case p.planGcs of
                Nothing -> []
                Just g -> Core.disablePromptsLine : ["export " <> k <> "=" <> shellQuote v | (k, v) <- gcsEnvironment g]
            , [ "dir=" <> shellQuote (Text.pack p.planHostDir)
              , "name=" <> shellQuote (p.planName <> "-") <> "\"$(date -u +%Y%m%dT%H%M%SZ)\"" <> shellQuote ".zip"
              , "printf '%s%s%s\\n' " <> shellQuote sqlHead <> " \"$name\" " <> shellQuote sqlTail <> " | " <> argv (clientArgv p.planClient [])
              ]
            , [argv (gcloudArgv (Storage.BucketsRsync (shipRsync p g))) | Just g <- [p.planGcs]]
            , [ "ls -1 \"$dir\" | grep -E " <> shellQuote (backupFileRegex p) <> " | sort -r | tail -n +" <> Text.pack (show (n + 1)) <> " | while read -r old; do rm -f -- \"$dir/$old\"; done"
              | Just n <- [p.planKeepLocal]
              ]
            ]
  where
    (sqlHead, sqlTail) = backupSqlParts p
    argv :: [String] -> Text
    argv = Text.unwords . fmap (shellQuote . Text.pack)

gcloudArgv :: Storage.StorageCommand -> [String]
gcloudArgv cmd = case cmdspec (prepare Storage.storageCommand cmd) of
    RawCommand exe args -> exe : args
    ShellCommand s -> ["/bin/sh", "-c", s]

-- | The environment gcloud runs with for this target: the credential file's path, if one is named.
gcsEnvironment :: GcsTarget -> [(Text, Text)]
gcsEnvironment g = [("CLOUDSDK_AUTH_CREDENTIAL_FILE_OVERRIDE", Text.pack f) | f <- maybeToList g.gcsCredentialFile]

-- | A single-quoted shell word.
shellQuote :: Text -> Text
shellQuote t = "'" <> Text.replace "'" "'\\''" t <> "'"

backupScriptPath :: BackupPlan -> FilePath
backupScriptPath p = p.planScriptDir </> Text.unpack ("clickhouse-backup-" <> p.planName <> ".sh")

-- | The job's stamp ('Job.jobStamp'): the last successful run, across reboots.
backupStampPath :: BackupPlan -> FilePath
backupStampPath p = p.planScriptDir </> Text.unpack ("clickhouse-backup-" <> p.planName <> ".stamp")

{- | The job: @\/bin\/sh SCRIPT@ as @clickhouse-backup-NAME.service@, with a
stamp, system scope, as root (who can read the files the container's user
wrote and remove the old ones). Change scope, user or unit directory with a
record update.
-}
backupJob :: BackupPlan -> Job.Job
backupJob p =
    (Job.job ("clickhouse-backup-" <> p.planName) ["/bin/sh", Text.pack (backupScriptPath p)])
        { Job.jobDescription = "ClickHouse backup " <> p.planName <> " (salmon)"
        , Job.jobStamp = Just (backupStampPath p)
        , Job.jobTimeout = p.planTimeout
        }

{- | What 'Job.completedRun' asks of a backup job: a successful run newer
than its unit file and its script.
-}
backupCompletion :: Job.Job -> BackupPlan -> Job.Completion
backupCompletion j p =
    let c = Job.jobCompletion j
     in c{Job.completionWritten = c.completionWritten <> [backupScriptPath p]}

data InvalidPlan = InvalidPlan !Text ![Text]
    deriving (Show)

instance Exception InvalidPlan

{- | The script, written in 'planScriptDir' (a 'FS.filecontents', so its
directory is a 'FS.dir'). A plan with 'planProblems' fails at @up@ and
writes nothing.
-}
backupScript :: BackupPlan -> Op
backupScript p = case planProblems p of
    [] -> FS.filecontents (FS.FileContents (backupScriptPath p) (renderBackupScript p))
    problems ->
        op "clickhouse-backup-script" nodeps $ \actions ->
            actions
                { help = "refuses to write the ClickHouse backup script " <> Text.pack (backupScriptPath p)
                , notes = problems
                , ref = mkRef "file-contents" (backupScriptPath p)
                , up = throwIO (InvalidPlan p.planName problems)
                }

{- | A backup on a schedule, and the question of how its last run went.

The job ('backupJob', amended by the function given: scope, user, unit
directory) is installed standing on the script, the backup directory and
the 'Track'' (the server, gcloud's installation, a bucket); the timer
('Job.jobTimer') waits on these schedules; and the node itself is
'Job.completedRun' on the job: 'Completed' when the last run succeeded
after the unit file and the script were last written, otherwise a run now
(the first backup is taken by the pass that schedules them), failing the
pass when that run fails.
-}
scheduledBackup ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Job.Job ->
    (Job.Job -> Job.Job) ->
    BackupPlan ->
    [Job.Schedule] ->
    Op
scheduledBackup r systemctl t amend p schedules =
    Job.completedRun r systemctl (backupCompletion j p)
        `inject` Job.scheduledJob r systemctl prerequisites j schedules
  where
    j = amend (backupJob p)
    prerequisites = Track $ \jj ->
        op "clickhouse-backup-prerequisites" (deps [run t jj, backupScript p, backupDirectory p.planHostDir]) $ \actions ->
            actions{help = "what the ClickHouse backup " <> p.planName <> " stands on"}

-------------------------------------------------------------------------------
-- Restore

-- | Where the backup file is.
data RestoreSource
    = -- | in the backup directory already
      LocalBackup
    | -- | under this target, fetched into the backup directory unless it is there
      FromGcs GcsTarget
    deriving (Eq, Show)

-- | One database of one backup file, put back.
data Restore = Restore
    { restoreClient :: Client
    , restoreHostDir :: FilePath
    -- ^ the host side of the @backups@ disk
    , restoreBackup :: Text
    -- ^ the file's name in it (@NAME-20261031T020000Z.zip@)
    , restoreDatabase :: DatabaseName
    -- ^ the database in the backup
    , restoreAs :: DatabaseName
    -- ^ the database it becomes; 'restoreFrom' makes it the same name
    , restoreSource :: RestoreSource
    , restoreMayDiscard :: Bool
    -- ^ the operator's flag: a database holding tables that is not this
    -- restore may be dropped. 'False' refuses it.
    }
    deriving (Eq, Show)

-- | The database under its own name, from the local directory, discarding nothing.
restoreFrom :: Client -> FilePath -> Text -> DatabaseName -> Restore
restoreFrom c hostDir backup db = Restore c hostDir backup db db LocalBackup False

restoreProblems :: Restore -> [Text]
restoreProblems x =
    mconcat
        [ ["the backup file name is not letters, digits, ., _ and -: " <> x.restoreBackup | Text.null x.restoreBackup || not (Text.all (\c -> plainChar c || c == '.') x.restoreBackup) || "." `Text.isPrefixOf` x.restoreBackup]
        , ["an empty database name" | Text.null x.restoreDatabase || Text.null x.restoreAs]
        , ["the backup directory is not an absolute path" | not (isAbsolute x.restoreHostDir)]
        ]

{- | The comment a restored database carries: which file, which database of
it. Public (it is in @system.databases@ and in this node's notes).
-}
restoreMarker :: Restore -> Text
restoreMarker x = "salmon-restore: " <> x.restoreBackup <> " database " <> x.restoreDatabase

restoreSql :: Restore -> Text
restoreSql x =
    "RESTORE DATABASE "
        <> quoteIdent x.restoreDatabase
        <> (if x.restoreAs == x.restoreDatabase then "" else " AS " <> quoteIdent x.restoreAs)
        <> " FROM Disk("
        <> quoteLiteral backupDiskName
        <> ", "
        <> quoteLiteral x.restoreBackup
        <> ");\n"

markSql :: Restore -> Text
markSql x = "ALTER DATABASE " <> quoteIdent x.restoreAs <> " MODIFY COMMENT " <> quoteLiteral (restoreMarker x) <> ";\n"

dropSql :: Restore -> Text
dropSql x = "DROP DATABASE " <> quoteIdent x.restoreAs <> " SYNC;\n"

-- | The target's comment and how many tables it holds, as one TSV line; no line when there is no such database.
targetSql :: Restore -> Text
targetSql x =
    "SELECT comment, (SELECT count() FROM system.tables WHERE database = "
        <> quoteLiteral x.restoreAs
        <> ") FROM system.databases WHERE name = "
        <> quoteLiteral x.restoreAs
        <> " FORMAT TSV"

-- | What is at the target's name.
data TargetState
    = TargetAbsent
    | -- | this restore, by its comment
      TargetRestored
    | -- | a database with no table
      TargetEmpty
    | -- | a database with this many tables that is not this restore
      TargetHolds Int
    deriving (Eq, Show)

-- | 'targetSql''s answer; 'Nothing' when it cannot be read.
interpretTarget :: Restore -> Text -> Maybe TargetState
interpretTarget x out = case filter (not . Text.null) (Text.lines out) of
    [] -> Just TargetAbsent
    [line] -> case Text.splitOn "\t" line of
        [comment, tables] -> case reads (Text.unpack tables) of
            [(n, "")]
                | comment == restoreMarker x -> Just TargetRestored
                | n == (0 :: Int) -> Just TargetEmpty
                | otherwise -> Just (TargetHolds n)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

restoreVerdict :: Restore -> TargetState -> CheckResult
restoreVerdict x st = case st of
    TargetRestored -> Success
    TargetAbsent -> Failure ("database " <> x.restoreAs <> " does not exist")
    TargetEmpty -> Failure ("database " <> x.restoreAs <> " is empty")
    TargetHolds n -> Failure ("database " <> x.restoreAs <> " holds " <> Text.pack (show n) <> " tables and is not the restore of " <> x.restoreBackup)

-- | What @up@ does.
data RestoreStep
    = NothingToRestore
    | RestoreInto
    | DropThenRestore
    deriving (Eq, Show)

restoreDecision :: Restore -> TargetState -> Either Text RestoreStep
restoreDecision x st = case st of
    TargetRestored -> Right NothingToRestore
    TargetAbsent -> Right RestoreInto
    TargetEmpty -> Right DropThenRestore
    TargetHolds n
        | x.restoreMayDiscard -> Right DropThenRestore
        | otherwise ->
            Left
                ( "database "
                    <> x.restoreAs
                    <> " holds "
                    <> Text.pack (show n)
                    <> " tables and salmon cannot prove it is the restore of "
                    <> x.restoreBackup
                    <> "; declare restoreMayDiscard to replace it"
                )

data RestoreRefused = RestoreRefused !DatabaseName !Text
    deriving (Show)

instance Exception RestoreRefused

{- | Restores one database ('Restore'), guarded as the module header says.

* @check@: 'targetSql' read by 'interpretTarget' and 'restoreVerdict':
  'Success' only for the database carrying this restore's 'restoreMarker';
  'Unknown' when the server does not answer.
* @up@: for 'FromGcs', the file is fetched into the backup directory when
  it is not there (@gcloud storage cp@ to @FILE.download@, then renamed, so
  a fetch cut short is never taken for the file); then 'restoreDecision':
  refuse ('RestoreRefused', nothing changed), or drop the target
  (@DROP DATABASE ... SYNC@), @RESTORE@, and set the marker.
* @down@: nothing. The restored database stays.

The gcloud 'Track'' is a dependency only for 'FromGcs'. The node stands on
the client's; the server and the backup directory are the caller's to
inject.
-}
restore :: Reporter Report -> Track' (Binary "clickhouse-client") -> Track' (Binary "gcloud") -> Restore -> Op
restore r clientT gcloudT x =
    op "clickhouse-restore" (deps ([justInstall clientT] <> [justInstall gcloudT | FromGcs _ <- [x.restoreSource]])) $ \actions ->
        actions
            { ref = mkRef "clickhouse-restore" (clientKey c, x.restoreAs)
            , help = "restore ClickHouse database " <> x.restoreAs <> " from " <> x.restoreBackup
            , notes =
                [ "server: " <> clientKey c
                , "from: " <> source
                , "database in the backup: " <> x.restoreDatabase
                , "marker: " <> restoreMarker x
                , "an existing database holding tables: " <> (if x.restoreMayDiscard then "dropped" else "refused")
                ]
            , check = case restoreProblems x of
                [] -> either (const Unknown) (maybe Unknown (restoreVerdict x) . interpretTarget x) <$> adminQuery c (targetSql x)
                problems -> pure (Failure (Text.intercalate "; " problems))
            , up = bringUp
            }
  where
    c = x.restoreClient
    local = x.restoreHostDir </> Text.unpack x.restoreBackup
    source = case x.restoreSource of
        LocalBackup -> Text.pack local
        FromGcs g -> gcsDestination g <> "/" <> x.restoreBackup
    sql f what = Binary.untrackedExec (adminRun c) AdminBatch (Text.encodeUtf8 f) (contramap (what x.restoreAs) r)

    bringUp = do
        case restoreProblems x of
            [] -> pure ()
            problems -> throwIO (RestoreRefused x.restoreAs (Text.intercalate "; " problems))
        answer <- adminQuery c (targetSql x)
        st <- case answer of
            Left err -> throwIO (RestoreRefused x.restoreAs ("cannot read the target: " <> err))
            Right out -> maybe (throwIO (RestoreRefused x.restoreAs ("cannot read the target: " <> out))) pure (interpretTarget x out)
        case restoreDecision x st of
            Left why -> throwIO (RestoreRefused x.restoreAs why)
            Right NothingToRestore -> pure ()
            Right step -> do
                forM_ [g | FromGcs g <- [x.restoreSource]] fetch
                case step of
                    DropThenRestore -> sql (dropSql x) RestoreDrop
                    _ -> pure ()
                sql (restoreSql x) RestoreRun
                sql (markSql x) RestoreMark

    fetch :: GcsTarget -> IO ()
    fetch g = do
        present <- doesFileExist local
        unless present $ do
            inherited <- getEnvironment
            let extra = [(Text.unpack k, Text.unpack v) | (k, v) <- gcsEnvironment g]
                environment = extra <> filter ((`notElem` fmap fst extra) . fst) inherited
                cmd = Storage.ObjectCopy g.gcsBucket.bucketProject source (Text.pack partial)
                gcloud = Command $ \cc -> (prepare Storage.storageCommand cc){env = Just environment}
            Binary.untrackedExec gcloud cmd "" (contramap (BackupFetch x.restoreBackup . Storage.RunStorageCommand cmd) r)
            renameFile partial local
    partial = local <> ".download"
