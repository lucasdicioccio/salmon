{-# LANGUAGE OverloadedStrings #-}

{- | pgBackRest: a continuous WAL archive plus base backups of one Postgres
cluster, which is what point-in-time recovery needs and what
"SreBox.PostgresBackup" (@pg_dump@) cannot give (@specs\/pg-patroni.md@,
"Nodes that must run on exactly one member").

It earns its place twice: as the disaster recovery story ('restoredCluster',
'restoreArgs'), and as Patroni's @create_replica_methods@ ('patroniArchive'),
which builds a replica out of the archive instead of loading the leader with a
base backup.

__The repository is shared or it is useless.__ A replica can only be built
from an archive it can read, and a restore after the machine is lost can only
read an archive that was somewhere else. A 'PosixRepo' is therefore a mount
every member sees (NFS, or one machine in a test), and 'S3Repo' is the usual
answer.

__Credentials are not rendered.__ @pgbackrest.conf@ is written here, as a
'Text' (so its fingerprint reaches @notes@). Bucket keys and the repository
cipher passphrase live in a pre-provisioned @*.conf@ fragment
('pbr_secrets_file') that this module only /owns/ (@chown@, mode @0600@) and
never writes; pgBackRest loads every @*.conf@ of that file's directory
(@--config-include-path@). Every command this module renders names both the
configuration file and that directory explicitly, so nothing depends on
pgBackRest's defaults and nothing else on the machine is read by accident.

__What runs where.__ @stanza-create@ and @backup@ need the /primary/: with one
@pg1-path@ configured, pgBackRest refuses both on a standby. Under Patroni the
primary is not known when the directive is written, so 'stanza' and
'firstBackup' answer from the repository (@info@, readable from any member)
and their @up@ only succeeds on the member that leads at that moment; on the
others it fails until the leader has done it, after which their checks pass.
Nothing here names a primary. 'backupScript' gates itself on Patroni's REST
API for the same reason.

__The restore is guarded by pgBackRest itself.__ 'restoredCluster' never passes
@--delta@ or @--force@, so a data directory that holds anything is refused by
the tool, and its check leaves a directory that already holds a cluster alone.
The only command here that overwrites a data directory is
'replicaCreateCommand', and that one is handed to Patroni, which owns the
directory and decides when a member is to be rebuilt.
-}
module Salmon.Builtin.Nodes.PgBackRest where

import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import qualified System.Posix.Types as Posix
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), Report, justInstall, withBinary)
import qualified Salmon.Builtin.Nodes.CronTask as Cron
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Patroni as Patroni
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | Where the archive lives. Keys and passphrases are never part of it.
data Repo
    = -- | a directory every member can read and write
      PosixRepo FilePath
    | S3Repo
        { s3_bucket :: Text
        , s3_endpoint :: Text
        , s3_region :: Text
        , s3_path :: FilePath
        -- ^ the prefix inside the bucket, e.g. @\/pgbackrest@
        , s3_path_style :: Bool
        -- ^ @repo1-s3-uri-style=path@, which most non-AWS stores need
        }
    deriving (Eq, Show)

data PgBackRestConfig
    = PgBackRestConfig
    { pbr_stanza :: Text
    -- ^ the archive's name for this cluster; under Patroni, the scope
    , pbr_pg_path :: FilePath
    -- ^ the data directory
    , pbr_pg_port :: Int
    , pbr_repo :: Repo
    , pbr_retention_full :: Int
    -- ^ how many full backups to keep; WAL older than the oldest kept one expires with it
    , pbr_global :: [(Text, Text)]
    -- ^ further @[global]@ options, verbatim (@compress-type@, @process-max@, ...)
    , pbr_stanza_options :: [(Text, Text)]
    -- ^ further stanza options, verbatim (@pg1-socket-path@, ...)
    , pbr_config_file :: FilePath
    -- ^ e.g. @\/etc\/pgbackrest\/pgbackrest.conf@
    , pbr_secrets_file :: Maybe FilePath
    -- ^ the pre-provisioned fragment; every @*.conf@ beside it is loaded too
    , pbr_log_path :: FilePath
    , pbr_user :: Text
    -- ^ the system user that owns the cluster, normally @postgres@
    , pbr_run_as :: Maybe Text
    -- ^ @sudo -u@ this user for the commands the nodes run; 'Nothing' when salmon already is that user
    , pbr_binary :: FilePath
    -- ^ as Postgres and Patroni will call it, e.g. @\/usr\/bin\/pgbackrest@
    }
    deriving (Eq, Show)

-- | The directory @--config-include-path@ names, when a secrets fragment is declared.
includeDir :: PgBackRestConfig -> Maybe FilePath
includeDir c = takeDirectory <$> c.pbr_secrets_file

-- | @pgbackrest.conf@, without any credential.
renderConf :: PgBackRestConfig -> Text
renderConf c =
    Text.unlines $
        ["[global]"]
            <> fmap kv (repo <> [("repo1-retention-full", tshow c.pbr_retention_full), ("log-path", Text.pack c.pbr_log_path)] <> c.pbr_global)
            <> ["", "[" <> c.pbr_stanza <> "]"]
            <> fmap kv ([("pg1-path", Text.pack c.pbr_pg_path), ("pg1-port", tshow c.pbr_pg_port)] <> c.pbr_stanza_options)
  where
    kv (k, v) = k <> "=" <> v
    repo = case c.pbr_repo of
        PosixRepo path -> [("repo1-type", "posix"), ("repo1-path", Text.pack path)]
        S3Repo bucket endpoint region path pathStyle ->
            [ ("repo1-type", "s3")
            , ("repo1-path", Text.pack path)
            , ("repo1-s3-bucket", bucket)
            , ("repo1-s3-endpoint", endpoint)
            , ("repo1-s3-region", region)
            ]
                <> [("repo1-s3-uri-style", "path") | pathStyle]

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-------------------------------------------------------------------------------
-- Commands, pure: the same words whether a node runs them, Postgres does
-- (archive_command), Patroni does (create_replica_methods) or an operator does.

-- | The options every invocation carries: which configuration, which stanza.
commonArgs :: PgBackRestConfig -> [Text]
commonArgs c =
    ["--config=" <> Text.pack c.pbr_config_file]
        <> maybe [] (\d -> ["--config-include-path=" <> Text.pack d]) (includeDir c)
        <> ["--stanza=" <> c.pbr_stanza]

data BackupType = Full | Differential | Incremental
    deriving (Eq, Show)

backupTypeName :: BackupType -> Text
backupTypeName Full = "full"
backupTypeName Differential = "diff"
backupTypeName Incremental = "incr"

stanzaCreateArgs, infoArgs :: PgBackRestConfig -> [Text]
stanzaCreateArgs c = commonArgs c <> ["stanza-create"]
infoArgs c = commonArgs c <> ["--output=json", "info"]

backupArgs :: PgBackRestConfig -> BackupType -> [Text]
backupArgs c ty = commonArgs c <> ["--type=" <> backupTypeName ty, "backup"]

-- | Where recovery stops.
data RestoreTarget
    = -- | replay everything the archive holds
      Latest
    | -- | stop as soon as the backup is consistent
      Immediate
    | -- | a timestamp Postgres parses, e.g. @2026-10-02 14:03:00+00@
      AtTime Text
    | AtLsn Text
    | AtXid Text
    | -- | a @pg_create_restore_point@ name
      AtName Text
    deriving (Eq, Show)

{- | @pgbackrest restore@ to a target, promoting once it is reached.

No @--delta@, no @--force@: a data directory that is not empty is refused by
pgBackRest, which is the guard. The timeline is left to Postgres' default
(@latest@), which is wrong for one case worth knowing: after an /earlier/
recovery forked a timeline, a target that lies before that fork is never
reached unless @target-timeline@ is set (through 'pbr_global').
-}
restoreArgs :: PgBackRestConfig -> RestoreTarget -> [Text]
restoreArgs c target = commonArgs c <> ty <> ["restore"]
  where
    targeted name v = ["--type=" <> name, "--target=" <> v, "--target-action=promote"]
    ty = case target of
        Latest -> []
        Immediate -> ["--type=immediate", "--target-action=promote"]
        AtTime t -> targeted "time" t
        AtLsn l -> targeted "lsn" l
        AtXid x -> targeted "xid" x
        AtName n -> targeted "name" n

-- | A command line for a shell: Postgres and Patroni both take one string.
commandLine :: PgBackRestConfig -> [Text] -> Text
commandLine c args = Text.unwords (Text.pack c.pbr_binary : fmap shellWord args)

-- | Single-quoted when it has to be; @%p@ and @%f@ are left for Postgres to substitute.
shellWord :: Text -> Text
shellWord w
    | Text.null w = "''"
    | Text.all safe w = w
    | otherwise = "'" <> Text.replace "'" "'\\''" w <> "'"
  where
    safe ch = ch `elem` ("-_=/.:,+%@" :: String) || ch `elem` ['a' .. 'z'] || ch `elem` ['A' .. 'Z'] || ch `elem` ['0' .. '9']

-- | Postgres's @archive_command@.
archiveCommand :: PgBackRestConfig -> Text
archiveCommand c = commandLine c (commonArgs c <> ["archive-push", "%p"])

-- | Postgres's @restore_command@.
restoreCommand :: PgBackRestConfig -> Text
restoreCommand c = commandLine c (commonArgs c <> ["archive-get", "%f"]) <> " \"%p\""

{- | What Patroni runs to make a replica: a delta restore into the member's
data directory. Patroni then writes the recovery configuration itself.
-}
replicaCreateCommand :: PgBackRestConfig -> Text
replicaCreateCommand c = commandLine c (commonArgs c <> ["--delta", "restore"])

-- | The settings a cluster /not/ under Patroni needs to archive; @archive_mode@ is restart-only.
archiveParameters :: PgBackRestConfig -> [(Text, Text)]
archiveParameters c = [("archive_mode", "on"), ("archive_command", archiveCommand c)]

{- | The archive, as Patroni needs it declared ('Patroni.pat_archive'):
replicas are made from it, with a base backup from the leader as the fallback
for when it holds no backup yet, and every member archives and restores WAL
through it.
-}
patroniArchive :: PgBackRestConfig -> Patroni.Archive
patroniArchive c =
    Patroni.Archive
        { Patroni.arc_methods = [Patroni.ReplicaMethod "pgbackrest" (replicaCreateCommand c) True True]
        , Patroni.arc_basebackup_fallback = True
        , Patroni.arc_archive_command = archiveCommand c
        , Patroni.arc_restore_command = restoreCommand c
        , Patroni.arc_bootstrap = Nothing
        }

{- | The same, for a /new/ Patroni cluster whose first member is restored from
the archive to a target instead of @initdb@-ed: point-in-time recovery of a
whole cluster. It only has an effect while the scope has never been
initialised in the DCS; give the recovered cluster a new scope (and so a new
stanza to archive into) rather than reusing the lost one's.
-}
patroniArchiveRecovering :: PgBackRestConfig -> RestoreTarget -> Patroni.Archive
patroniArchiveRecovering c target =
    (patroniArchive c){Patroni.arc_bootstrap = Just (Patroni.BootstrapMethod "pgbackrest" (commandLine c (restoreArgs c target)))}

-------------------------------------------------------------------------------
-- What @pgbackrest info --output=json@ says, as far as this module reads it.

data StanzaInfo
    = StanzaInfo
    { si_name :: Text
    , si_status_code :: Int
    , si_status_message :: Text
    , si_backups :: [BackupInfo]
    }
    deriving (Eq, Show)

data BackupInfo
    = BackupInfo
    { bi_label :: Text
    , bi_type :: Text
    , bi_stop :: Maybe Integer
    -- ^ epoch seconds at which the backup ended; a recovery target must be later
    }
    deriving (Eq, Show)

instance FromJSON StanzaInfo where
    parseJSON = withObject "stanza" $ \o -> do
        st <- o .: "status"
        (code, msg) <- withObject "status" (\s -> (,) <$> s .: "code" <*> s .:? "message" .!= "") st
        StanzaInfo <$> o .: "name" <*> pure code <*> pure msg <*> o .:? "backup" .!= []

instance FromJSON BackupInfo where
    parseJSON = withObject "backup" $ \o -> do
        ts <- o .:? "timestamp"
        stop <- maybe (pure Nothing) (withObject "timestamp" (.:? "stop")) ts
        BackupInfo <$> o .: "label" <*> o .:? "type" .!= "" <*> pure stop

parseInfo :: ByteString.ByteString -> Either Text [StanzaInfo]
parseInfo = either (Left . Text.pack) Right . eitherDecodeStrict

{- | Does the repository hold this stanza? pgBackRest's status codes: 0 is ok,
2 is "no valid backups" (the stanza exists, which is all this asks), 1 and 3
are a missing stanza path or data. Anything else is reported as it reads.
-}
interpretStanza :: Text -> ByteString.ByteString -> CheckResult
interpretStanza stanzaName out = withStanza stanzaName out $ \si ->
    if si.si_status_code `elem` [0, 2]
        then Success
        else Failure ("stanza " <> stanzaName <> ": " <> describe si)

-- | Does the repository hold at least one backup of this stanza?
interpretHasBackup :: Text -> ByteString.ByteString -> CheckResult
interpretHasBackup stanzaName out = withStanza stanzaName out $ \si ->
    case (si.si_status_code, si.si_backups) of
        (0, _ : _) -> Success
        (0, []) -> Failure ("stanza " <> stanzaName <> " lists no backup")
        _ -> Failure ("stanza " <> stanzaName <> ": " <> describe si)

describe :: StanzaInfo -> Text
describe si = si.si_status_message <> " (status " <> tshow si.si_status_code <> ")"

withStanza :: Text -> ByteString.ByteString -> (StanzaInfo -> CheckResult) -> CheckResult
withStanza stanzaName out f = case parseInfo out of
    Left e -> Failure ("pgbackrest info not understood: " <> Text.take 120 e)
    Right infos -> case filter ((== stanzaName) . si_name) infos of
        [] -> Failure ("the repository has no stanza " <> stanzaName)
        (si : _) -> f si

-------------------------------------------------------------------------------

newtype Invocation = Invocation [Text]

pgbackrestRun :: PgBackRestConfig -> Command "pgbackrest" Invocation
pgbackrestRun c = Command (\(Invocation args) -> process c args)

process :: PgBackRestConfig -> [Text] -> CreateProcess
process c args = case c.pbr_run_as of
    Nothing -> proc c.pbr_binary (fmap Text.unpack args)
    Just u -> proc "sudo" (["-u", Text.unpack u, c.pbr_binary] <> fmap Text.unpack args)

-- | Asks the repository; a command that fails could not tell, which is 'Unknown' and lets @up@ say why.
checkInfo :: PgBackRestConfig -> (Text -> ByteString.ByteString -> CheckResult) -> IO CheckResult
checkInfo c interpret = do
    (code, out, _) <- readCreateProcessWithExitCode (process c (infoArgs c)) ""
    pure $ case code of
        ExitSuccess -> interpret c.pbr_stanza out
        ExitFailure _ -> Unknown

{- | The binary, @pgbackrest.conf@, the log directory, a posix repository's
directory, and the secrets fragment's ownership. A missing fragment is a
failure: writing it is somebody else's job.
-}
configuration :: Track' (Binary "pgbackrest") -> PgBackRestConfig -> Op
configuration bin c =
    op "pgbackrest-config" (deps ([justInstall bin, conf, owned c.pbr_log_path] <> repoDir <> secrets)) $ \actions ->
        actions
            { help = "pgBackRest configured for stanza " <> c.pbr_stanza
            , notes = ["credentials are a pre-provisioned fragment, never rendered"]
            , ref = mkRef "pgbackrest-config" c.pbr_config_file
            }
  where
    conf = FS.filecontents (FS.FileContents c.pbr_config_file (renderConf c))
    owned path =
        FS.ownedFile (FS.FileOwnership path (Just c.pbr_user) Nothing (0o750 :: Posix.FileMode))
            `inject` FS.dir (FS.Directory path)
    repoDir = case c.pbr_repo of
        PosixRepo path -> [owned path]
        S3Repo{} -> []
    secrets =
        [ FS.ownedFile (FS.FileOwnership f (Just c.pbr_user) Nothing (0o600 :: Posix.FileMode))
        | Just f <- [c.pbr_secrets_file]
        ]

{- | The stanza exists in the repository. @stanza-create@ is idempotent, and
needs the cluster running and this member to be the primary (see the module
header for what that means under Patroni).
-}
stanza :: Reporter Report -> Track' (Binary "pgbackrest") -> PgBackRestConfig -> Op
stanza r bin c =
    withBinary bin (pgbackrestRun c) (Invocation (stanzaCreateArgs c)) $ \run ->
        op "pgbackrest-stanza" (deps [configuration bin c]) $ \actions ->
            actions
                { help = "the archive holds stanza " <> c.pbr_stanza
                , notes = ["checked in the repository; created from the primary only"]
                , ref = mkRef "pgbackrest-stanza" c.pbr_stanza
                , check = checkInfo c interpretStanza
                , up = run r
                }

{- | The archive holds at least one backup, without which it can restore
nothing and build no replica. Takes a full one when there is none; later
backups are a schedule's ('scheduledBackup'), not a state.
-}
firstBackup :: Reporter Report -> Track' (Binary "pgbackrest") -> PgBackRestConfig -> Op
firstBackup r bin c =
    withBinary bin (pgbackrestRun c) (Invocation (backupArgs c Full)) $ \run ->
        op "pgbackrest-first-backup" (deps [stanza r bin c]) $ \actions ->
            actions
                { help = "the archive holds a backup of stanza " <> c.pbr_stanza
                , ref = mkRef "pgbackrest-first-backup" c.pbr_stanza
                , check = checkInfo c interpretHasBackup
                , up = run r
                }

{- | A data directory restored from the archive, to a target. For disaster
recovery of a cluster salmon runs itself; __not__ for a Patroni member, whose
data directory is Patroni's (see 'patroniArchiveRecovering').

The check is whether the directory already holds a cluster (@PG_VERSION@): one
that does is left alone, whatever it is. The @up@ restores without @--delta@,
so pgBackRest refuses a directory holding anything else. Starting the cluster
afterwards, which is what replays the WAL up to the target, is the caller's
next node. @down@ does nothing: a restored cluster is data.
-}
restoredCluster :: Reporter Report -> Track' (Binary "pgbackrest") -> PgBackRestConfig -> RestoreTarget -> Op
restoredCluster r bin c target =
    withBinary bin (pgbackrestRun c) (Invocation (restoreArgs c target)) $ \run ->
        op "pgbackrest-restore" (deps [configuration bin c]) $ \actions ->
            actions
                { help = "the data directory " <> Text.pack c.pbr_pg_path <> " restored from stanza " <> c.pbr_stanza
                , notes = ["target: " <> tshow target, "a directory already holding a cluster is left alone"]
                , ref = mkRef "pgbackrest-restore" c.pbr_pg_path
                , check = checkHoldsCluster c.pbr_pg_path
                , up = run r
                }

checkHoldsCluster :: FilePath -> IO CheckResult
checkHoldsCluster path = do
    present <- doesFileExist (path </> "PG_VERSION")
    pure (if present then Success else Failure (Text.pack path <> " holds no cluster"))

-------------------------------------------------------------------------------

data BackupSchedule
    = BackupSchedule
    { bs_name :: Text
    -- ^ names the cron entry and the script
    , bs_type :: BackupType
    , bs_schedule :: Cron.Schedule
    , bs_script :: FilePath
    , bs_gate_url :: Maybe Text
    -- ^ run only when this URL answers 2xx, e.g. @http:\/\/127.0.0.1:8008\/primary@
    }

{- | The scheduled command. With a gate, a member that is not what the URL
asks for exits 0 without doing anything, so the same schedule can be declared
on every member and one of them acts.

The gate to use with this module's configuration is Patroni's @\/primary@:
a backup taken /from/ a standby (the spec's preference) needs pgBackRest's
@backup-standby@ and a second @pg@ host reached over ssh or TLS, which is not
rendered here.
-}
backupScript :: PgBackRestConfig -> BackupSchedule -> Text
backupScript c s =
    Text.unlines $
        ["#!/bin/bash", "set -euo pipefail"]
            <> maybe [] (\u -> ["curl -sf -o /dev/null " <> shellWord u <> " || exit 0"]) s.bs_gate_url
            <> ["exec " <> commandLine c (backupArgs c s.bs_type)]

-- | The script and a cron entry running it as 'pbr_user'.
scheduledBackup :: Track' (Binary "pgbackrest") -> PgBackRestConfig -> BackupSchedule -> Op
scheduledBackup bin c s =
    Cron.crontask (Track $ const script) task
  where
    script = FS.filecontents (FS.FileContents s.bs_script (backupScript c s)) `inject` configuration bin c
    task = Cron.CronTask ("pgbackrest-" <> s.bs_name) c.pbr_user s.bs_schedule "/bin/bash" [Text.pack s.bs_script]
