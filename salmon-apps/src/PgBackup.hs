{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-pg-backup@: take a Postgres dump, here or on another machine, or
install the job that keeps taking one.

Two axes, chosen independently, which is the whole shape of the binary:

* __what to do__ — @--action dump@ takes one backup now; @--action schedule@
  installs a @cron.d@ entry (and takes one immediately, so a broken
  credential is discovered now rather than at 03:17 tomorrow).
* __where__ — with no @--over@, on this machine. With @--over user\@host@,
  this binary uploads /itself/ to that machine, runs there with the same
  directive (role flipped), and — for @dump@ — pulls the resulting file
  back.

That is four useful combinations out of two flags, and the remote ones need
nothing installed on the target beyond @rsync@, @bash@ and a Postgres client:
the salmon binary that does the work is the one you are already running.

= Why the timestamp is frozen in the directive

A dump is named after the moment it is taken, and the machine that wants to
/fetch/ it is not the machine that takes it. If each side asks its own clock,
the two disagree whenever they land either side of a second — rarely, which
is the worst frequency for a bug of this kind. So @configure@ resolves the
timestamp once, on the controlling machine, and it travels in the directive.
Both ends then name the same file, and re-running the same directive is
idempotent: the node's check sees the dump already there and skips it.

= Credentials

Never on a command line. @--pgpass@, @--pg-service@ and @--client-cert@ all
become environment variables (see
"SreBox.PostgresBackup".'SreBox.PostgresBackup.Credentials'); the default is
peer authentication as a local OS user over the unix socket, which needs no
credential at all and is what a job running on the database host should use.
This binary creates none of them: getting a credential onto a machine is a
separate problem with a separate answer per site.
-}
module PgBackup (
    main,
    Seed (..),
    Spec (..),
    Role (..),
    Action (..),
    RemoteSpec (..),
    configure,
    program,
    remoteDumpPath,
) where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Options.Applicative (auto, execParser, fullDesc, header, helper, info, long, metavar, option, optional, progDesc, strOption, value, (<**>), (<|>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.FilePath ((</>))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.CronTask as Cron
import qualified Salmon.Builtin.Nodes.Debian.OS as OS
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..), trackedGraph)
import Salmon.Reporter (reportPrint)

import qualified SreBox.PostgresBackup as Backup

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Take or schedule a Postgres backup, here or over ssh" <> header "salmon-pg-backup"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd

-------------------------------------------------------------------------------
-- Seed

data Seed = Seed
    { seedDatabase :: Text
    , seedPgRole :: Maybe Text
    , seedHost :: Maybe Text
    , seedPort :: Int
    , seedSudoUser :: Maybe Text
    , seedDir :: FilePath
    , seedScript :: Maybe FilePath
    , seedCronUser :: Text
    , seedCredentials :: Backup.Credentials
    , seedPrefix :: Maybe Text
    , seedTimestampFormat :: Text
    , seedSuffix :: Text
    , seedRetentionDays :: Int
    , seedMaxAgeHours :: Int
    , seedHour :: Text
    , seedMinute :: Text
    , seedGcsBucket :: Maybe Text
    , seedGcsPrefix :: Text
    , seedAction :: Action
    , seedOver :: Maybe Text
    , seedRemoteDir :: FilePath
    , seedIdentity :: Maybe FilePath
    , seedKnownHosts :: Maybe FilePath
    , seedFetchInto :: Maybe FilePath
    }
    deriving (Eq, Show)

instance ParseRecord Seed where
    parseRecord =
        build <**> helper
      where
        build =
            Seed
                <$> strOption (long "database" <> metavar "DB" <> Opt.help "database to dump")
                <*> optional (strOption (long "pg-role" <> metavar "ROLE" <> Opt.help "connect as this database role (default: whatever the OS user maps to)"))
                <*> optional (strOption (long "host" <> metavar "HOST" <> Opt.help "connect over TCP to this host (default: the local unix socket, which is the only place peer auth exists)"))
                <*> option auto (long "port" <> value 5432 <> Opt.showDefault <> Opt.help "port, when --host is given")
                <*> sudoUserP
                <*> strOption (long "dir" <> value "/var/backups/postgresql" <> Opt.showDefault <> Opt.help "where dumps are written, on whichever machine runs the dump")
                <*> optional (strOption (long "script" <> metavar "PATH" <> Opt.help "where the generated script is written (default: /opt/salmon/postgres/backup-<db>.sh)"))
                <*> strOption (long "cron-user" <> value "root" <> Opt.showDefault <> Opt.help "the user the cron entry runs as")
                <*> credentialsP
                <*> optional (strOption (long "prefix" <> metavar "NAME" <> Opt.help "dump filename prefix (default: the database name)"))
                <*> strOption (long "timestamp-format" <> value "%Y%m%d_%H%M%S" <> Opt.showDefault <> Opt.help "date(1) format for the timestamp in the filename")
                <*> strOption (long "suffix" <> value ".sql.gz" <> Opt.showDefault <> Opt.help "dump filename suffix")
                <*> option auto (long "retention-days" <> value 7 <> Opt.showDefault <> Opt.help "delete local dumps older than this")
                <*> option auto (long "max-age-hours" <> value 26 <> Opt.showDefault <> Opt.help "how old the newest dump may be before the freshness check fails")
                <*> strOption (long "hour" <> value "3" <> Opt.showDefault <> Opt.help "schedule: hour")
                <*> strOption (long "minute" <> value "17" <> Opt.showDefault <> Opt.help "schedule: minute (an odd one, so a fleet does not stampede)")
                <*> optional (strOption (long "gcs-bucket" <> metavar "BUCKET" <> Opt.help "also copy each dump to this GCS bucket"))
                <*> strOption (long "gcs-prefix" <> value "" <> Opt.help "path prefix inside the bucket")
                <*> actionP
                <*> optional (strOption (long "over" <> metavar "USER@HOST" <> Opt.help "do the work on this machine instead, by uploading this binary to it"))
                <*> strOption (long "remote-dir" <> value "/tmp" <> Opt.showDefault <> Opt.help "where this binary is uploaded on the remote")
                <*> optional (strOption (long "ssh-identity" <> metavar "PATH" <> Opt.help "ssh private key for --over"))
                <*> optional (strOption (long "ssh-known-hosts" <> metavar "PATH" <> Opt.help "known_hosts file for --over"))
                <*> optional (strOption (long "fetch-into" <> metavar "DIR" <> Opt.help "with --over and --action dump: pull the dump back into this local directory"))

        -- `--sudo-user` has a default, so it can never be *absent*; opting
        -- out needs a flag of its own.
        sudoUserP =
            Opt.flag' Nothing (long "no-sudo" <> Opt.help "run pg_dump as the current user rather than via sudo")
                <|> (Just <$> strOption (long "sudo-user" <> value "postgres" <> Opt.showDefault <> Opt.help "run pg_dump as this OS user"))

        actionP =
            Opt.option
                (Opt.eitherReader parseAction)
                (long "action" <> value DumpNow <> metavar "dump|schedule" <> Opt.help "take one backup now, or install the periodic job")

        parseAction "dump" = Right DumpNow
        parseAction "schedule" = Right InstallSchedule
        parseAction other = Left ("expected dump or schedule, got: " <> other)

        -- xor by construction: the first branch that matches wins, and the
        -- flags are distinct, so no two credential mechanisms can be given.
        credentialsP =
            (Backup.PassFile <$> strOption (long "pgpass" <> metavar "PATH" <> Opt.help "a .pgpass file (PGPASSFILE)"))
                <|> ( Backup.ServiceFile
                        <$> strOption (long "pg-service-file" <> metavar "PATH" <> Opt.help "a libpq service file (PGSERVICEFILE)")
                        <*> strOption (long "pg-service" <> metavar "NAME" <> Opt.help "the service to use from it (PGSERVICE)")
                    )
                <|> ( Backup.ClientCertificate
                        <$> strOption (long "client-cert" <> metavar "PATH" <> Opt.help "client certificate (PGSSLCERT)")
                        <*> strOption (long "client-key" <> metavar "PATH" <> Opt.help "client key (PGSSLKEY)")
                        <*> strOption (long "client-ca" <> metavar "PATH" <> Opt.help "CA certificate (PGSSLROOTCERT)")
                    )
                <|> pure Backup.PeerAuth

-------------------------------------------------------------------------------
-- Spec

{- | Which side of the hand-off a directive is for. 'OnTarget' is what the
uploaded copy of this binary receives; it names no remote of its own, which
is what stops a directive bouncing forever.
-}
data Role = Controller | OnTarget
    deriving (Eq, Show, Generic)

instance FromJSON Role
instance ToJSON Role

data Action = DumpNow | InstallSchedule
    deriving (Eq, Show, Generic)

instance FromJSON Action
instance ToJSON Action

data RemoteSpec = RemoteSpec
    { rs_user :: Text
    , rs_host :: Text
    , rs_selfDir :: FilePath
    , rs_identity :: Maybe FilePath
    , rs_knownHosts :: Maybe FilePath
    }
    deriving (Eq, Show, Generic)

instance FromJSON RemoteSpec
instance ToJSON RemoteSpec

data Spec = Spec
    { role :: Role
    , action :: Action
    , backup :: Backup.PgBackupConfig
    , stamp :: Maybe Text
    -- ^ the frozen timestamp; see the module header
    , remote :: Maybe RemoteSpec
    , fetchInto :: Maybe FilePath
    , selfPath :: Maybe Self.SelfPath
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> do
    when (seed.seedFetchInto /= Nothing && seed.seedOver == Nothing) $
        fail "--fetch-into only means something with --over (without it, the dump is already local)"
    when (seed.seedFetchInto /= Nothing && seed.seedAction /= DumpNow) $
        fail "--fetch-into only means something with --action dump"
    -- Frozen here, on the controlling machine, so both ends of a driven
    -- backup name the same file. A scheduled job must NOT have one, or every
    -- run would overwrite a single dump forever.
    frozen <- case seed.seedAction of
        InstallSchedule -> pure Nothing
        DumpNow -> do
            now <- getCurrentTime
            pure (Just (Text.pack (formatTime defaultTimeLocale (Text.unpack seed.seedTimestampFormat) now)))
    self <- traverse (const Self.readSelfPath_linux) seed.seedOver
    remoteSpec <- traverse (parseOver seed) seed.seedOver
    pure $
        Spec
            { role = Controller
            , action = seed.seedAction
            , backup = backupConfig seed frozen
            , stamp = frozen
            , remote = remoteSpec
            , fetchInto = seed.seedFetchInto
            , selfPath = self
            }
  where
    parseOver :: Seed -> Text -> IO RemoteSpec
    parseOver seed spec =
        case Text.breakOn "@" spec of
            (user, rest)
                | Just host <- Text.stripPrefix "@" rest
                , not (Text.null user)
                , not (Text.null host) ->
                    pure
                        RemoteSpec
                            { rs_user = user
                            , rs_host = host
                            , rs_selfDir = seed.seedRemoteDir
                            , rs_identity = seed.seedIdentity
                            , rs_knownHosts = seed.seedKnownHosts
                            }
            _ -> fail ("--over wants USER@HOST, got: " <> Text.unpack spec)

backupConfig :: Seed -> Maybe Text -> Backup.PgBackupConfig
backupConfig seed frozen =
    Backup.PgBackupConfig
        { Backup.pgb_database = seed.seedDatabase
        , Backup.pgb_role = seed.seedPgRole
        , Backup.pgb_server = fmap (\h -> Postgres.Server h seed.seedPort) seed.seedHost
        , Backup.pgb_sudoUser = seed.seedSudoUser
        , Backup.pgb_dir = seed.seedDir
        , Backup.pgb_scriptPath =
            maybe ("/opt/salmon/postgres/backup-" <> Text.unpack seed.seedDatabase <> ".sh") id seed.seedScript
        , Backup.pgb_osUser = seed.seedCronUser
        , Backup.pgb_credentials = seed.seedCredentials
        , Backup.pgb_naming =
            Backup.NamingPolicy
                { Backup.np_prefix = maybe seed.seedDatabase id seed.seedPrefix
                , Backup.np_timestampFormat = seed.seedTimestampFormat
                , Backup.np_suffix = seed.seedSuffix
                }
        , Backup.pgb_fixedTimestamp = frozen
        , Backup.pgb_retentionDays = seed.seedRetentionDays
        , Backup.pgb_schedule = Cron.dailyAt seed.seedHour seed.seedMinute
        , Backup.pgb_gcs = fmap (\b -> Backup.GcsDestination b seed.seedGcsPrefix) seed.seedGcsBucket
        , Backup.pgb_maxAge = fromIntegral seed.seedMaxAgeHours * 3600
        }

-------------------------------------------------------------------------------
-- Program

program :: Track' Spec
program = Track $ \spec -> case spec.role of
    OnTarget -> onTarget spec
    Controller -> case spec.remote of
        Nothing -> onTarget spec
        Just rs -> driven spec rs

{- | The work itself, on whichever machine is running it. Reached either
directly (no @--over@) or as the uploaded copy.
-}
onTarget :: Spec -> Op
onTarget spec =
    op "pg-backup-target" (deps [work]) $ \actions ->
        actions
            { help = Text.unwords ["backs up", spec.backup.pgb_database, "on this machine"]
            , ref = mkRef "pg-backup-target" (spec.backup.pgb_database, spec.backup.pgb_dir)
            }
  where
    work = case spec.action of
        InstallSchedule -> Backup.postgresBackup reportPrint OS.bash spec.backup
        DumpNow -> case spec.stamp of
            -- The named form is what makes a re-run of the same directive a
            -- no-op, and it is the only form whose output another machine
            -- can name in advance.
            Just st -> Backup.namedBackup reportPrint OS.bash spec.backup st
            Nothing -> Backup.recentBackup reportPrint OS.bash spec.backup

{- | Upload this binary to the target, run it there, and (for a dump) pull
the result back.

The fetch is injected onto the remote call, not merely declared beside it:
rsync would otherwise be racing the dump it is fetching.
-}
driven :: Spec -> RemoteSpec -> Op
driven spec rs =
    op "pg-backup-driven" (deps [maybe remoteWork id fetch]) $ \actions ->
        actions
            { help = Text.unwords ["backs up", spec.backup.pgb_database, "on", rs.rs_host]
            , ref = mkRef "pg-backup-driven" (rs.rs_user, rs.rs_host, spec.backup.pgb_database)
            }
  where
    opts =
        Ssh.ClientOpts
            { Ssh.optIdentity = rs.rs_identity
            , Ssh.optKnownHosts = rs.rs_knownHosts
            }

    remoteWork :: Op
    remoteWork =
        trackedGraph $
            Self.uploadAndCallSelfAsSudoWith
                opts
                reportPrint
                reportPrint
                rs.rs_selfDir
                (Self.Remote rs.rs_user rs.rs_host)
                (maybe (error "configure should have read the self path") id spec.selfPath)
                Ssh.preExistingRemoteMachine
                program
                CLI.Up
                spec{role = OnTarget, remote = Nothing}

    fetch :: Maybe Op
    fetch = do
        into <- spec.fetchInto
        st <- spec.stamp
        let local = into </> Backup.dumpNameFor spec.backup.pgb_naming st
        pure $
            Rsync.receiveFileWith
                opts
                reportPrint
                OS.rsync
                (Track FS.dir)
                (Rsync.Remote rs.rs_user rs.rs_host)
                (remoteDumpPath spec st)
                local
                `inject` remoteWork

{- | Where the dump will be on the target, given the frozen timestamp.

Exposed because it is the contract between the two halves: the remote writes
this path and the controller fetches it, and nothing else reconciles them.
-}
remoteDumpPath :: Spec -> Text -> FilePath
remoteDumpPath spec st =
    spec.backup.pgb_dir </> Backup.dumpNameFor spec.backup.pgb_naming st
