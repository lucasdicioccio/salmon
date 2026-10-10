{- | @pg_turret@ (<https://github.com/heywinit/pg_turret>): a PostgreSQL
extension that hooks @emit_log_hook@ and ships the server's log lines, as
JSON batches, from background workers to an HTTP endpoint or a Kafka topic.

This module makes one cluster ship its logs. It installs a pre-built
artifact, adds the library to @shared_preload_libraries@ (keeping whatever
else is listed there), restarts the cluster if that is pending, and sets the
@pg_turret.*@ settings. It does not build the extension and it does not run
a collector.

Experimental, and in this package for that reason: the extension is at
version @0.0.0@ with a single maintainer, and every setting name below is
the one registered in its @src\/lib.rs@ at commit @5c7407af@ (2026-04). A
later commit may rename any of them. Nothing in this module has been run
against a server with the extension loaded; see
@resources\/module-notes.md@ for what was read and what was run.

What was read in the extension's source, and shaped this module:

* Every @pg_turret.*@ setting is registered with context @sighup@, so a
  reload applies it and no restart is needed after the first one. The one
  exception in practice is @pg_turret.num_workers@: it is only read when
  the library is loaded, so a change takes effect at the next restart, and
  PostgreSQL does not report it as @pending_restart@. This module sets it
  and does not restart for it.
* The settings are not marked superuser-only: __any role that can connect
  can read them with @SHOW@__, credentials included. They are also in
  @postgresql.auto.conf@, and __PostgreSQL writes a reloaded setting's new
  value to its own log__ (@parameter "..." changed to "..."@), which is
  the log this extension ships. None of that can be avoided while the
  extension takes its credentials as settings. Give the collector a
  credential that can only write logs.
* The Sentry settings are registered but the background worker never reads
  them (it reads a JSON file written by a SQL function instead), so this
  module does not offer a Sentry adapter.
* The Kafka adapter does not pass @kafka.api_key@\/@kafka.api_secret@ to
  its producer. They are offered here because they are documented upstream,
  and they authenticate nothing at that commit.
* A failed export is itself a log line (@pg_turret: failed to send ...@),
  written by the worker and captured by the same hook: a collector that is
  down is sent one more line about itself on every poll. 'defaultFilter'
  excludes those lines.
-}
module Salmon.Builtin.Nodes.PgTurret (
    -- * Declaration
    PgTurret (..),
    pgTurretOn,
    Artifact (..),
    InstallDirs (..),
    debianInstallDirs,
    SecretFile (..),
    HttpAdapter (..),
    httpAdapter,
    KafkaAdapter (..),
    kafkaAdapter,
    Filter (..),
    defaultFilter,
    selfLogPattern,
    Retry (..),
    defaultRetry,

    -- * Nodes
    pgTurret,
    preloadLibrary,
    libraryName,

    -- * Pure parts
    Value (..),
    settings,
    settingNotes,
    problems,
    parseLibraryList,
    addLibrary,
    removeLibrary,
    alterPreloadSql,
    fileSettingSql,
    inspectSettingsSql,
    applySettingsSql,
    resetSettingsSql,
    interpretSettings,
    scrub,
    readSecret,
    PgTurretError (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), justInstall)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Nodes.Postgres (ClusterName, DatabaseName, PgExtension (..), Port)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | The name the library is preloaded under, and the extension's name.
libraryName :: Text
libraryName = "pg_turret"

{- | The three kinds of file @cargo pgrx package@ produces, at paths on the
target machine. They are built for one PostgreSQL major version and are
somebody else's to build and deliver.
-}
data Artifact = Artifact
    { artifactLibrary :: FilePath
    -- ^ the shared object; installed as @pg_turret.so@ whatever it is called here
    , artifactControl :: FilePath
    -- ^ @pg_turret.control@
    , artifactScripts :: [FilePath]
    -- ^ the @pg_turret--VERSION.sql@ scripts, installed under their own names
    }
    deriving (Eq, Show)

-- | Where a server looks for libraries and extension files (@pg_config --pkglibdir@, and @extension@ under @--sharedir@).
data InstallDirs = InstallDirs
    { pkgLibDir :: FilePath
    , extensionDir :: FilePath
    }
    deriving (Eq, Show)

-- | Debian's layout for a major version: @\/usr\/lib\/postgresql\/N\/lib@ and @\/usr\/share\/postgresql\/N\/extension@.
debianInstallDirs :: Int -> InstallDirs
debianInstallDirs major =
    InstallDirs
        { pkgLibDir = "/usr/lib/postgresql" </> show major </> "lib"
        , extensionDir = "/usr/share/postgresql" </> show major </> "extension"
        }

{- | A file on the target machine holding one credential, pre-provisioned by
the caller and readable by whoever runs salmon. It is read when the node
runs, never when the graph is built; trailing newlines are dropped.
-}
newtype SecretFile = SecretFile {secretPath :: FilePath}
    deriving (Eq, Show)

-- | @pg_turret.http.*@. A 'Nothing' leaves the server's value alone.
data HttpAdapter = HttpAdapter
    { httpEndpoint :: Text
    -- ^ public: it is in the node's notes. Do not put a credential in the URL.
    , httpApiKey :: Maybe SecretFile
    -- ^ sent as @Authorization: Bearer@
    , httpBatchSize :: Maybe Int
    , httpTimeoutMs :: Maybe Int
    , httpCompression :: Maybe Bool
    }
    deriving (Eq, Show)

httpAdapter :: Text -> HttpAdapter
httpAdapter endpoint = HttpAdapter endpoint Nothing Nothing Nothing Nothing

-- | @pg_turret.kafka.*@. A 'Nothing' leaves the server's value alone.
data KafkaAdapter = KafkaAdapter
    { kafkaBrokers :: [Text]
    , kafkaTopic :: Text
    , kafkaApiKey :: Maybe SecretFile
    , kafkaApiSecret :: Maybe SecretFile
    , kafkaTimeoutMs :: Maybe Int
    , kafkaBatchSize :: Maybe Int
    }
    deriving (Eq, Show)

kafkaAdapter :: [Text] -> Text -> KafkaAdapter
kafkaAdapter brokers topic = KafkaAdapter brokers topic Nothing Nothing Nothing Nothing

{- | @pg_turret.filter.*@.

By reading the extension, and not by running it: the filter is applied from
state the background worker sets in its own process, so it reaches the lines
the worker itself writes and may not reach the lines ordinary backends
write. Do not rely on it to keep anything out of the collector.
-}
data Filter = Filter
    { filterLevelMin :: Maybe Int
    -- ^ PostgreSQL's @elevel@: 10 is @DEBUG5@, 17 @INFO@, 19 @WARNING@, 20 @ERROR@, 21 @FATAL@
    , filterPattern :: Maybe Text
    , filterPatternExclude :: Maybe Text
    }
    deriving (Eq, Show)

-- | Excludes the exporter's own failure lines ('selfLogPattern') and nothing else.
defaultFilter :: Filter
defaultFilter = Filter Nothing Nothing (Just selfLogPattern)

-- | Every line the extension writes about itself starts with its name.
selfLogPattern :: Text
selfLogPattern = "^pg_turret: "

-- | @pg_turret.retry.*@.
data Retry = Retry
    { retryEnabled :: Maybe Bool
    , retryMaxAttempts :: Maybe Int
    , retryQueueSize :: Maybe Int
    }
    deriving (Eq, Show)

-- | Leaves all three alone.
defaultRetry :: Retry
defaultRetry = Retry Nothing Nothing Nothing

data PgTurret = PgTurret
    { turretCluster :: ClusterName
    , turretPort :: Port
    , turretArtifact :: Artifact
    , turretInstallDirs :: InstallDirs
    , turretHttp :: Maybe HttpAdapter
    -- ^ 'Nothing' sets @pg_turret.http.enabled = off@
    , turretKafka :: Maybe KafkaAdapter
    -- ^ 'Nothing' sets @pg_turret.kafka.enabled = off@
    , turretFilter :: Filter
    , turretRetry :: Retry
    , turretPollIntervalS :: Maybe Int
    , turretRingBufferSize :: Maybe Int
    , turretNumWorkers :: Maybe Int
    -- ^ takes effect at the next restart, which this module does not force
    , turretFunctionsIn :: [DatabaseName]
    -- ^ databases to @CREATE EXTENSION@ in. The extension's SQL objects are
    -- counters (@get_metrics()@ and friends); shipping needs none of them,
    -- because the hook comes from the preloaded library and is
    -- cluster-wide. Empty is a working installation.
    }
    deriving (Eq, Show)

-- | A cluster with the library loaded and both adapters off.
pgTurretOn :: ClusterName -> Port -> InstallDirs -> Artifact -> PgTurret
pgTurretOn cluster port dirs artifact =
    PgTurret
        { turretCluster = cluster
        , turretPort = port
        , turretArtifact = artifact
        , turretInstallDirs = dirs
        , turretHttp = Nothing
        , turretKafka = Nothing
        , turretFilter = defaultFilter
        , turretRetry = defaultRetry
        , turretPollIntervalS = Nothing
        , turretRingBufferSize = Nothing
        , turretNumWorkers = Nothing
        , turretFunctionsIn = []
        }

-------------------------------------------------------------------------------

-- | What a setting is set to: text that is public, or the content of a file that is not.
data Value
    = Plain Text
    | Secret SecretFile
    deriving (Eq, Show)

{- | The declared settings, in a stable order. Only these are written,
compared and (on the way down) reset; a @pg_turret.*@ setting that is not
declared is left as it is.
-}
settings :: PgTurret -> [(Text, Value)]
settings t =
    catMaybes
        [ int "poll_interval_s" t.turretPollIntervalS
        , int "ring_buffer_size" t.turretRingBufferSize
        , int "num_workers" t.turretNumWorkers
        , int "filter.level_min" t.turretFilter.filterLevelMin
        , text "filter.pattern" t.turretFilter.filterPattern
        , text "filter.pattern_exclude" t.turretFilter.filterPatternExclude
        , bool "retry.enabled" t.turretRetry.retryEnabled
        , int "retry.max_attempts" t.turretRetry.retryMaxAttempts
        , int "retry.queue_size" t.turretRetry.retryQueueSize
        ]
        <> maybe [plain "http.enabled" "off"] http t.turretHttp
        <> maybe [plain "kafka.enabled" "off"] kafka t.turretKafka
  where
    http :: HttpAdapter -> [(Text, Value)]
    http h =
        catMaybes
            [ Just (plain "http.enabled" "on")
            , Just (plain "http.endpoint" h.httpEndpoint)
            , secret "http.api_key" h.httpApiKey
            , int "http.batch_size" h.httpBatchSize
            , int "http.timeout_ms" h.httpTimeoutMs
            , bool "http.compression" h.httpCompression
            ]
    kafka :: KafkaAdapter -> [(Text, Value)]
    kafka k =
        catMaybes
            [ Just (plain "kafka.enabled" "on")
            , Just (plain "kafka.brokers" (Text.intercalate "," k.kafkaBrokers))
            , Just (plain "kafka.topic" k.kafkaTopic)
            , secret "kafka.api_key" k.kafkaApiKey
            , secret "kafka.api_secret" k.kafkaApiSecret
            , int "kafka.timeout_ms" k.kafkaTimeoutMs
            , int "kafka.batch_size" k.kafkaBatchSize
            ]
    name k = libraryName <> "." <> k
    plain k v = (name k, Plain v)
    text k = fmap (plain k)
    int k = fmap (plain k . Text.pack . show)
    bool k = fmap (\b -> plain k (if b then "on" else "off"))
    secret k = fmap (\f -> (name k, Secret f))

-- | One line per setting for the node's notes: the value when it is public, the path when it is not.
settingNotes :: PgTurret -> [Text]
settingNotes t = fmap render (settings t)
  where
    render (k, Plain v) = k <> " = " <> v
    render (k, Secret f) = k <> " from " <> Text.pack f.secretPath

{- | Reasons this declaration is refused, all of them. The ranges are the
ones the extension registers; a server with the library loaded refuses the
same values, later and less legibly.
-}
problems :: PgTurret -> [Text]
problems t =
    concat
        [ range "poll_interval_s" 1 3600 t.turretPollIntervalS
        , range "ring_buffer_size" 128 65536 t.turretRingBufferSize
        , range "num_workers" 1 8 t.turretNumWorkers
        , range "filter.level_min" 10 21 t.turretFilter.filterLevelMin
        , range "retry.max_attempts" 1 10 t.turretRetry.retryMaxAttempts
        , range "retry.queue_size" 64 4096 t.turretRetry.retryQueueSize
        , maybe [] http t.turretHttp
        , maybe [] kafka t.turretKafka
        , [ Text.pack s <> " is not a " <> libraryName <> "--VERSION.sql script"
          | s <- t.turretArtifact.artifactScripts
          , not ((libraryName <> "--") `Text.isPrefixOf` Text.pack (takeFileName s) && ".sql" `Text.isSuffixOf` Text.pack s)
          ]
        , ["no extension script in the artifact" | null t.turretArtifact.artifactScripts]
        , [k <> " has a line break in it" | (k, Plain v) <- settings t, Text.any (`elem` ['\n', '\r']) v]
        ]
  where
    http :: HttpAdapter -> [Text]
    http h =
        ["http.endpoint is empty" | Text.null (Text.strip h.httpEndpoint)]
            <> range "http.batch_size" 1 1000 h.httpBatchSize
            <> range "http.timeout_ms" 100 60000 h.httpTimeoutMs
    kafka :: KafkaAdapter -> [Text]
    kafka k =
        ["kafka.brokers is empty" | null k.kafkaBrokers]
            <> ["kafka.brokers has an empty entry or one with a comma" | any (\b -> Text.null (Text.strip b) || Text.any (== ',') b) k.kafkaBrokers]
            <> ["kafka.topic is empty" | Text.null (Text.strip k.kafkaTopic)]
            <> range "kafka.timeout_ms" 100 60000 k.kafkaTimeoutMs
            <> range "kafka.batch_size" 1 1000 k.kafkaBatchSize
    range :: Text -> Int -> Int -> Maybe Int -> [Text]
    range k lo hi mv =
        [ k <> " is " <> Text.pack (show v) <> ", outside " <> Text.pack (show lo) <> ".." <> Text.pack (show hi)
        | Just v <- [mv]
        , v < lo || v > hi
        ]

-------------------------------------------------------------------------------
-- shared_preload_libraries

{- | The libraries in a @shared_preload_libraries@ value, as PostgreSQL
writes one: comma-separated, each possibly double-quoted. A library whose
quoted name contains a comma is not handled.
-}
parseLibraryList :: Text -> [Text]
parseLibraryList = filter (not . Text.null) . fmap (unquote . Text.strip) . Text.splitOn ","
  where
    unquote s = fromMaybe s (Text.stripPrefix "\"" s >>= Text.stripSuffix "\"")

-- | Appends, unless it is there already. The others keep their order: it can matter.
addLibrary :: Text -> [Text] -> [Text]
addLibrary lib libs
    | lib `elem` libs = libs
    | otherwise = libs <> [lib]

removeLibrary :: Text -> [Text] -> [Text]
removeLibrary lib = filter (/= lib)

{- | @ALTER SYSTEM SET@ takes a list, and writes it back as one value. The
reload that follows changes nothing in the running server; it is what makes
the server notice the files and report the setting as @pending_restart@,
which is the question 'Postgres.restartClusterIfPending' asks.
-}
alterPreloadSql :: [Text] -> Text
alterPreloadSql libs =
    "ALTER SYSTEM SET shared_preload_libraries = " <> value <> ";\nSELECT pg_reload_conf();\n"
  where
    value = case libs of
        [] -> "''"
        _ -> Text.intercalate ", " (fmap Postgres.quoteLiteral libs)

{- | What the configuration files say a setting is, which for a
restart-only setting is not what the running server has: a library another
node added and that is still waiting for its restart is in the files and
nowhere else, and a merge that read the running value would drop it.

Rows with an @error@ are not left out: @pg_file_settings@ marks exactly
those entries (\"setting could not be applied\") once the files are reloaded.
-}
fileSettingSql :: Text -> Text
fileSettingSql name =
    "SELECT coalesce((SELECT setting FROM pg_file_settings WHERE name = "
        <> Postgres.quoteLiteral name
        <> " ORDER BY seqno DESC LIMIT 1), '')"

{- | The library is in @shared_preload_libraries@, next to whatever else is.

@ALTER SYSTEM SET shared_preload_libraries@ replaces the whole value, so
this reads the list the files hold and writes it back with one more entry.
That is a read followed by a write: two of these for two libraries need an
edge between them, and a declaration that sets the whole value some other
way (an 'Postgres.alterSystemSet' of the same parameter has no check and
runs every pass) undoes this one every time.

Going down takes the library out of the list and leaves the others. Neither
direction restarts anything; see 'Postgres.restartClusterIfPending'.
-}
preloadLibrary :: Track' (Binary "psql") -> Port -> Text -> Op
preloadLibrary psql port lib =
    op "pg-preload-library" (deps [justInstall psql]) $ \actions ->
        actions
            { ref = mkRef "pg-preload-library" (port, lib)
            , help = Text.unwords ["preload", lib, "in the cluster on port", Text.pack (show port)]
            , notes = ["takes effect at the next restart of the cluster", "keeps the other entries of shared_preload_libraries"]
            , check = either (const Unknown) verdict <$> current
            , up = rewrite (addLibrary lib)
            , down = rewrite (removeLibrary lib)
            }
  where
    current = fmap parseLibraryList <$> Postgres.psqlQuery_Sudo port (fileSettingSql "shared_preload_libraries")
    verdict libs
        | lib `elem` libs = Success
        | otherwise = Failure (lib <> " is not in shared_preload_libraries")
    rewrite f = do
        libs <- either (\err -> throwIO (PgTurretError ("cannot read shared_preload_libraries: " <> Text.strip err))) pure =<< current
        when (f libs /= libs) $ do
            runBatch port [] (alterPreloadSql (f libs))
            awaitReload (20 :: Int)
    -- A reload is a signal, and the restart node's check is a new session
    -- asking the postmaster a moment later. Waits up to two seconds for the
    -- flag, and carries on without it: a list put back to what the running
    -- server already has is never pending.
    awaitReload n = do
        seen <- Postgres.psqlQuery_Sudo port "SELECT pending_restart FROM pg_settings WHERE name = 'shared_preload_libraries'"
        unless (n <= 0 || fmap Text.strip seen == Right "t") $ threadDelay 100000 >> awaitReload (n - 1)

-------------------------------------------------------------------------------
-- settings

{- | One row, one JSON object: the running @shared_preload_libraries@ and
each declared setting. Names only, so it is fit for an argument vector.
@current_setting(_, true)@ rather than @pg_settings@, which does not list a
setting whose library is not loaded.
-}
inspectSettingsSql :: [Text] -> Text
inspectSettingsSql names =
    "SELECT json_build_object("
        <> Text.intercalate ", " (running : fmap declared names)
        <> ")"
  where
    running = "'shared_preload_libraries', current_setting('shared_preload_libraries')"
    declared n = Postgres.quoteLiteral n <> ", current_setting(" <> Postgres.quoteLiteral n <> ", true)"

{- | The batch that sets every declared setting and reloads. It holds the
credentials, so it only ever exists in memory and on @psql@'s standard
input.

The first lines are about where a /statement/ can be repeated: the server's
log (which this very extension ships to a collector), @pg_stat_statements@,
and @psql@'s own error output, which at the default verbosity quotes the
line it failed on. They do nothing about the line the server writes for
each setting a reload changes, which quotes the new value.
-}
applySettingsSql :: [(Text, Text)] -> Text
applySettingsSql resolved =
    Text.unlines $
        quietSession
            <> ["ALTER SYSTEM SET " <> k <> " = " <> Postgres.quoteLiteral v <> ";" | (k, v) <- resolved]
            <> ["SELECT pg_reload_conf();"]

resetSettingsSql :: [Text] -> Text
resetSettingsSql names =
    Text.unlines $
        quietSession
            <> ["ALTER SYSTEM RESET " <> k <> ";" | k <- names]
            <> ["SELECT pg_reload_conf();"]

quietSession :: [Text]
quietSession =
    [ "\\set VERBOSITY terse"
    , "SET log_statement = 'none';"
    , "SET log_min_error_statement = 'panic';"
    , "SET log_min_duration_statement = -1;"
    , "SET pg_stat_statements.track_utility = off;"
    ]

{- | The verdict from 'inspectSettingsSql''s output, given what each setting
should be. A credential that differs is named and never quoted, and neither
is the value the server has for it.
-}
interpretSettings :: [(Text, Value, Text)] -> Text -> CheckResult
interpretSettings wanted out =
    case Aeson.decodeStrict (Text.encodeUtf8 (Text.strip out)) :: Maybe (Map Text (Maybe Text)) of
        Nothing -> Unknown
        Just got
            | libraryName `notElem` parseLibraryList (fromMaybe "" (lookupSetting "shared_preload_libraries" got)) ->
                Failure (libraryName <> " is not loaded: the running shared_preload_libraries does not list it")
            | otherwise -> case concatMap (differs got) wanted of
                [] -> Success
                diffs -> Failure (Text.intercalate "; " diffs)
  where
    lookupSetting k got = fromMaybe Nothing (Map.lookup k got)
    differs got (k, value, want)
        | have == want = []
        | otherwise = case value of
            Plain _ -> [k <> " is " <> Text.pack (show have) <> ", declared " <> Text.pack (show want)]
            Secret f -> [k <> " is not what " <> Text.pack f.secretPath <> " holds"]
      where
        have = fromMaybe "" (lookupSetting k got)

-- | A credential file's content without its trailing line breaks. Refused when empty or of several lines.
readSecret :: SecretFile -> IO (Either Text Text)
readSecret f = do
    r <- try (ByteString.readFile f.secretPath)
    pure $ case r of
        Left (_ :: IOException) -> Left ("cannot read " <> path)
        Right bytes -> case Text.decodeUtf8' bytes of
            Left _ -> Left (path <> " is not UTF-8")
            Right raw
                | Text.null v -> Left (path <> " is empty")
                | Text.any (`elem` ['\n', '\r']) v -> Left (path <> " holds more than one line")
                | otherwise -> Right v
              where
                v = Text.dropWhileEnd (`elem` ['\n', '\r']) raw
  where
    path = Text.pack f.secretPath

resolve :: [(Text, Value)] -> IO (Either [Text] [(Text, Value, Text)])
resolve ss = do
    rs <- traverse one ss
    pure $ case [e | Left e <- rs] of
        [] -> Right [x | Right x <- rs]
        errs -> Left errs
  where
    one (k, v@(Plain t)) = pure (Right (k, v, t))
    one (k, v@(Secret f)) = fmap (\t -> (k, v, t)) <$> readSecret f

-- | Replaces every occurrence of a secret, bare or as it reads inside a SQL literal.
scrub :: [Text] -> Text -> Text
scrub secrets t = foldl (\acc s -> Text.replace s "<redacted>" acc) t forms
  where
    forms = filter (not . Text.null) (concatMap (\s -> [Text.replace "'" "''" s, s]) secrets)

newtype PgTurretError = PgTurretError Text
    deriving (Show)

instance Exception PgTurretError

{- | Feeds a batch to @psql@ on its standard input and throws unless it
exits 0. Not through the tracked commands of "Salmon.Builtin.Nodes.Binary":
those report the command's output and put it in the failure, and what
@psql@ says about a statement holding a credential is not for a report. The
failure carries @psql@'s standard error with the given secrets replaced.
-}
runBatch :: Port -> [Text] -> Text -> IO ()
runBatch port secrets sql = do
    (code, _out, err) <-
        readCreateProcessWithExitCode
            (prepare (Postgres.psqlBatchRun_Sudo port) Postgres.PsqlBatch)
            (Text.encodeUtf8 sql)
    case code of
        ExitSuccess -> pure ()
        ExitFailure n ->
            throwIO . PgTurretError $
                "psql exited "
                    <> Text.pack (show n)
                    <> ": "
                    <> scrub secrets (Text.strip (Text.decodeUtf8With TextError.lenientDecode err))

settingsNode :: Track' (Binary "psql") -> PgTurret -> Op
settingsNode psql t =
    op "pg-turret-settings" (deps [justInstall psql]) $ \actions ->
        actions
            { ref = mkRef "pg-turret-settings" t.turretPort
            , help = Text.unwords ["pg_turret settings of the cluster on port", Text.pack (show t.turretPort)]
            , notes = settingNotes t <> ["any role that can connect can SHOW these, and the server logs a new value on reload: credentials included"]
            , check = chk
            , up = up
            , down = down
            }
  where
    declared = settings t
    names = fmap fst declared
    chk = case problems t of
        ps@(_ : _) -> pure (Failure (Text.intercalate "; " ps))
        [] -> do
            resolved <- resolve declared
            case resolved of
                Left errs -> pure (Failure (Text.intercalate "; " errs))
                Right wanted ->
                    either (const Unknown) (interpretSettings wanted)
                        <$> Postgres.psqlQuery_Sudo t.turretPort (inspectSettingsSql names)
    -- A server without the library loaded does not know these names and
    -- refuses to reset them; it is not exporting anything either.
    down = do
        running <- Postgres.psqlQuery_Sudo t.turretPort "SELECT current_setting('shared_preload_libraries')"
        case running of
            Left err -> throwIO (PgTurretError ("cannot read shared_preload_libraries: " <> Text.strip err))
            Right libs -> when (libraryName `elem` parseLibraryList libs) $ runBatch t.turretPort [] (resetSettingsSql names)
    up = do
        let ps = problems t
        unless (null ps) $ throwIO (PgTurretError (Text.intercalate "; " ps))
        wanted <- either (throwIO . PgTurretError . Text.intercalate "; ") pure =<< resolve declared
        runBatch
            t.turretPort
            [v | (_, Secret _, v) <- wanted]
            (applySettingsSql [(k, v) | (k, _, v) <- wanted])

-------------------------------------------------------------------------------
-- artifact

{- | One file of the artifact at its place under the server's directories.
Compared byte for byte, so a rebuilt artifact is seen; a library that is
replaced is only loaded at the next restart, which nothing here forces.
-}
installedFile :: FilePath -> FilePath -> Op
installedFile src tgt =
    op "pg-turret-file" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-turret-file" tgt
            , help = Text.pack ("installs " <> src <> " as " <> tgt)
            , check = chk
            , up = copyFile src tgt
            , down = FS.removeFileIfPresent tgt
            }
  where
    chk = do
        haveSrc <- doesFileExist src
        haveTgt <- doesFileExist tgt
        case (haveSrc, haveTgt) of
            (False, _) -> pure (Failure (Text.pack src <> " is missing"))
            (_, False) -> pure (Failure (Text.pack tgt <> " is missing"))
            _ -> do
                same <- (==) <$> ByteString.readFile src <*> ByteString.readFile tgt
                pure (if same then Success else Failure (Text.pack tgt <> " is not " <> Text.pack src))

artifactFiles :: PgTurret -> [(FilePath, FilePath)]
artifactFiles t =
    [ (a.artifactLibrary, d.pkgLibDir </> Text.unpack libraryName <> ".so")
    , (a.artifactControl, d.extensionDir </> Text.unpack libraryName <> ".control")
    ]
        <> [(s, d.extensionDir </> takeFileName s) | s <- a.artifactScripts]
  where
    a = t.turretArtifact
    d = t.turretInstallDirs

-------------------------------------------------------------------------------

{- | A cluster that ships its logs through @pg_turret@.

In order: the artifact's files, the library in @shared_preload_libraries@,
a restart if one is pending (and only then), the settings and a reload, and
@CREATE EXTENSION@ in the databases that asked for the counters. The
settings come after the restart because a server only knows a
@pg_turret.*@ name once the library is loaded.

The restart is a real one, of the whole cluster, the first time and
whenever anything else left a restart pending. This node does not know
whether the cluster is a primary somebody is using; declaring it is the
decision. On a standby the library loads and its workers do not start until
recovery ends, so a standby ships nothing.

Going down resets the declared settings to the server's defaults,
takes the library out of the list and removes the files, and restarts
nothing: the library stays in the running server until its next restart.
-}
pgTurret ::
    Reporter Postgres.Report ->
    Track' (Binary "psql") ->
    Track' (Binary "pg_ctlcluster") ->
    -- | how the databases of 'turretFunctionsIn' come to exist
    Track' DatabaseName ->
    PgTurret ->
    Op
pgTurret r psql pgctl mkdb t =
    op "pg-turret" (deps (configured : functions)) $ \actions ->
        actions
            { ref = mkRef "pg-turret" t.turretPort
            , help = Text.unwords ["ship the logs of pg cluster", t.turretCluster, "with pg_turret"]
            }
  where
    files = fmap (uncurry installedFile) (artifactFiles t)
    preload = foldl inject (preloadLibrary psql t.turretPort libraryName) files
    restarted = Postgres.restartClusterIfPending r pgctl t.turretPort t.turretCluster `inject` preload
    configured = settingsNode psql t `inject` restarted
    functions =
        [ Postgres.extension r psql t.turretPort mkdb (PgExtension libraryName db (Just 130000) False) `inject` restarted
        | db <- t.turretFunctionsIn
        ]
