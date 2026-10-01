{-# LANGUAGE OverloadedStrings #-}

{- | Plakar (<https://github.com/PlakarKorp/plakar>): encrypted, deduplicated
snapshots in a /Kloset/ store. Three nodes, in the order they depend on each
other: the binary ('plakarInstall'), a store ('kloset') and a scheduled
backup job with a freshness check ('plakarJob').

What running v1.1.7 taught, and the code is shaped by:

* __Exit codes are not the whole story.__ A failure to start the background
  cache process (@failed to run cached@) is printed and exits @0@. So the
  store is confirmed by the @CONFIG@ file it should have written, the backup
  by the line @backup completed without errors@, and a listing that printed
  nothing and complained on stderr is 'Unknown', never \"no snapshot\".
* __The cache process talks over a unix socket under the cache directory__,
  so a home directory whose path is long enough to overflow @sun_path@ makes
  every command fail as above. Nothing here can fix that; know it when a
  job's @$HOME@ is deep.
* __@prune@ without a filter refuses, and with one it is a dry run__ until
  @-apply@. The job passes @-apply@ only with a declared 'KeepPolicy'.
* __@ls@ prints one snapshot per line, newest first when asked for
  @-latest@__: @TIMESTAMP ID SIZE DURATION PATH@ with an RFC 3339 UTC
  timestamp. @-json@ does not change @ls@ in this version.

Deliberate refusals: a store without a declared, non-empty, owner-only
keyfile (salmon never generates the passphrase: losing it makes the backups
unrecoverable by design); and a job without a retention policy (prune deletes
snapshots irreversibly). A store's @down@ does nothing, because deleting a
backup store is not something a teardown gets to do.

A local store needs nothing more. Remote stores (S3-compatible, GCS) are Plakar
integrations installed per user with @plakar pkg add@: 'plakarIntegration'
installs one, 'gcsStore' declares a GCS store under a name and 'remoteKloset'
creates the Kloset in it ('plakarJobOn' then backs up into it, addressing it as
@\@name@). S3-compatible is not done.

__Unverified against a real plakar__ (none was available when this was
written): the output format of @pkg list@ ('interpretPkgList' is deliberately
tolerant), and the option name the GCS integration reads its credentials file
from ('gcsCredentialsOption'). Both are one-line fixes in pure functions with
tests.
-}
module Salmon.Builtin.Nodes.Plakar (
    PlakarRelease (..),
    plakarInstall,
    plakarBinary,
    KlosetStore (..),
    kloset,
    KeepPolicy (..),
    keepDays,
    pruneArgs,
    PlakarJob (..),
    plakarJob,
    plakarJobOn,

    -- * Integrations and remote stores
    IntegrationVersion (..),
    Integration (..),
    plakarIntegration,
    interpretPkgList,
    pkgAddArg,
    GcsStore (..),
    gcsStore,
    gcsStoreAddArgs,
    interpretStoreShow,
    remoteKloset,

    -- * Pieces, exposed for tests
    interpretVersion,
    interpretSnapshotList,
    keyfileProblem,
    renderBackupScript,
    sha256Hex,
) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as ByteString
import Data.Foldable (asum)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.IO.Exception (ExitCode (..))
import System.Directory (doesFileExist, removeFile)
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)
import Text.Printf (printf)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.CronTask (CronTask (..), Schedule, crontask)
import Salmon.Builtin.Nodes.Filesystem (FileContents (..), filecontents)
import Salmon.Op.Ref
import Salmon.Op.Track

-------------------------------------------------------------------------------

-- | A pinned release: the version it reports, where its @.deb@ is, and the
-- sha256 that file must have (from the release's @checksums.txt@).
data PlakarRelease = PlakarRelease
    { plakarVersion :: Text
    -- ^ @1.1.7@
    , plakarDebUrl :: Text
    , plakarDebSha256 :: Text
    }
    deriving (Eq, Show)

-- | Where a caller wanting @Track' (Binary "plakar")@ gets it from.
plakarBinary :: PlakarRelease -> Track' (Binary "plakar")
plakarBinary = Track . const . plakarInstall

{- | Installs the pinned @.deb@: download, refuse unless the sha256 matches,
@dpkg -i@. The check is @plakar version@ naming the wanted version; @down@ is
@dpkg -r plakar@ (a store is left alone).
-}
plakarInstall :: PlakarRelease -> Op
plakarInstall rel =
    op "plakar-install" nodeps $ \actions ->
        actions
            { help = "installs plakar " <> rel.plakarVersion
            , notes = ["pinned sha256: " <> rel.plakarDebSha256, "from " <> rel.plakarDebUrl]
            , ref = mkRef "plakar-install" rel.plakarVersion
            , check = do
                (code, out, _) <- readCreateProcessWithExitCode (proc "plakar" ["version"]) ""
                pure $ case code of
                    ExitSuccess -> interpretVersion rel.plakarVersion (decode out)
                    ExitFailure _ -> Failure "plakar is not installed"
            , up = do
                let path = "/tmp/salmon-plakar-" <> Text.unpack rel.plakarDebSha256 <> ".deb"
                _ <- run' "curl" ["-fsSL", "-o", path, Text.unpack rel.plakarDebUrl]
                bytes <- ByteString.readFile path
                let actual = sha256Hex bytes
                when (Text.toLower rel.plakarDebSha256 /= actual) $ do
                    removeFile path
                    ioError (userError ("plakar .deb has sha256 " <> Text.unpack actual <> ", expected " <> Text.unpack rel.plakarDebSha256))
                _ <- run' "dpkg" ["-i", path]
                removeFile path
            , down = () <$ run' "dpkg" ["-r", "plakar"]
            }

{- | @plakar version@ prints @plakar/v1.1.7@ (after a one-off welcome text on
a first run, so the lines are searched).
-}
interpretVersion :: Text -> Text -> CheckResult
interpretVersion wanted out
    | ("plakar/v" <> wanted) `elem` Text.lines out = Success
    | otherwise = Failure ("plakar is not at " <> wanted)

sha256Hex :: ByteString.ByteString -> Text
sha256Hex = Text.pack . concatMap (printf "%02x") . ByteString.unpack . SHA256.hash

-------------------------------------------------------------------------------

-- | A local Kloset store and the keyfile holding its passphrase.
data KlosetStore = KlosetStore
    { storePath :: FilePath
    , storeKeyFile :: FilePath
    -- ^ provisioned by somebody else; salmon never writes it
    }
    deriving (Eq, Show)

{- | Creates the store if it is not there. Refuses (at @up@) a keyfile that is
missing, empty, or readable by anyone but its owner. The store exists when its
@CONFIG@ file does, which is also what @create@ is checked against, since it
can fail with exit @0@. @down@ deletes nothing.
-}
kloset :: Track' (Binary "plakar") -> KlosetStore -> Op
kloset plakar store =
    op "plakar-store" (deps [justInstall plakar]) $ \actions ->
        actions
            { help = "kloset store at " <> Text.pack store.storePath
            , notes =
                [ "passphrase from " <> Text.pack store.storeKeyFile <> ", never generated by salmon"
                , "down does not delete the store"
                ]
            , ref = mkRef "plakar-store" store.storePath
            , check = do
                exists <- doesFileExist (configFile store)
                pure (if exists then Success else Failure ("no store at " <> Text.pack store.storePath))
            , up = do
                problem <- keyfileStatus store.storeKeyFile
                maybe (pure ()) (\p -> ioError (userError ("keyfile " <> store.storeKeyFile <> ": " <> Text.unpack p))) problem
                _ <- run' "plakar" ["-keyfile", store.storeKeyFile, "at", store.storePath, "create"]
                created <- doesFileExist (configFile store)
                unless created $ ioError (userError ("plakar create left no store at " <> store.storePath))
            , down = pure ()
            }

configFile :: KlosetStore -> FilePath
configFile store = store.storePath <> "/CONFIG"

-- | What is wrong with a keyfile of this size and mode, if anything.
keyfileProblem :: Integer -> Posix.FileMode -> Maybe Text
keyfileProblem size mode
    | size == 0 = Just "is empty"
    | mode `Posix.intersectFileModes` 0o077 /= 0 = Just "is readable by group or others (want 0600)"
    | otherwise = Nothing

keyfileStatus :: FilePath -> IO (Maybe Text)
keyfileStatus path = do
    exists <- doesFileExist path
    if not exists
        then pure (Just "is missing")
        else do
            st <- Posix.getFileStatus path
            pure (keyfileProblem (fromIntegral (Posix.fileSize st)) (Posix.fileMode st))

-------------------------------------------------------------------------------

-- | What to keep. At least one rule must be set: 'pruneArgs' refuses an empty one.
data KeepPolicy = KeepPolicy
    { keepLastHours :: Maybe Int
    , keepLastDays :: Maybe Int
    , keepLastMonths :: Maybe Int
    }
    deriving (Eq, Show)

-- | Keep the last N days.
keepDays :: Int -> KeepPolicy
keepDays n = KeepPolicy Nothing (Just n) Nothing

-- | The @plakar prune@ filter arguments, or why there are none.
pruneArgs :: KeepPolicy -> Either Text [Text]
pruneArgs p
    | null rules = Left "no retention rule declared: prune deletes snapshots irreversibly, so a job without one is refused"
    | any ((<= 0) . snd) rules = Left "a retention rule must be positive"
    | otherwise = Right (concat [[flag, Text.pack (show n)] | (flag, n) <- rules])
  where
    rules :: [(Text, Int)]
    rules = catMaybes [fmap ((,) "-hours") p.keepLastHours, fmap ((,) "-days") p.keepLastDays, fmap ((,) "-months") p.keepLastMonths]

data PlakarJob = PlakarJob
    { jobName :: Text
    , jobStore :: KlosetStore
    , jobSource :: FilePath
    , jobSchedule :: Schedule
    , jobUser :: Text
    , jobKeep :: KeepPolicy
    , jobMaxAge :: NominalDiffTime
    -- ^ how old the newest snapshot may be before the job is not fresh
    , jobScriptPath :: FilePath
    }

{- | A cron entry running a script that backs up and then prunes, and a check
that answers what a hand-rolled cron line never does: is there a recent
snapshot? When there is not, @up@ runs the script once, which is the remedy.

The check reads the store as whoever runs salmon, so that user must be able
to read the keyfile as well as the job's user.
-}
plakarJob :: Track' (Binary "plakar") -> PlakarJob -> Op
plakarJob plakar job = plakarJobOn (kloset plakar job.jobStore) job

{- | 'plakarJob' over a store node the caller supplies ('remoteKloset', say)
instead of the local 'kloset'. 'jobStore''s path is what is given to @at@, so
for a named remote store it is @\@name@.
-}
plakarJobOn :: Op -> PlakarJob -> Op
plakarJobOn storeOp job = case pruneArgs job.jobKeep of
    Left why ->
        op "plakar-job" nodeps $ \actions ->
            actions
                { help = "refused: " <> why
                , ref = mkRef "plakar-job" job.jobName
                , up = ioError (userError (Text.unpack why))
                }
    Right prune ->
        let script = renderBackupScript job prune
            scriptOp = filecontents (FileContents job.jobScriptPath script)
            cronOp = crontask ignoreTrack (CronTask job.jobName job.jobUser job.jobSchedule "/bin/bash" [Text.pack job.jobScriptPath])
         in op "plakar-job" (deps [storeOp, scriptOp, cronOp]) $ \actions ->
                actions
                    { help = "backs up " <> Text.pack job.jobSource <> " into " <> Text.pack job.jobStore.storePath
                    , notes = ["fresh means a snapshot newer than " <> Text.pack (show job.jobMaxAge), "up runs the backup once"]
                    , ref = mkRef "plakar-job" job.jobName
                    , check = do
                        now <- getCurrentTime
                        (code, out, err) <-
                            readCreateProcessWithExitCode (proc "plakar" ["-keyfile", job.jobStore.storeKeyFile, "at", job.jobStore.storePath, "ls", "-latest"]) ""
                        pure (interpretSnapshotList now job.jobMaxAge code (decode out) (decode err))
                    , up = do
                        (code, _, err) <- readCreateProcessWithExitCode (proc "/bin/bash" [job.jobScriptPath]) ""
                        case code of
                            ExitSuccess -> pure ()
                            ExitFailure n -> throwIO (Binary.CommandFailedSimple ("plakar backup job: " <> take 500 (Text.unpack (decode err))) n)
                    }

{- | The freshness verdict from @plakar ls -latest@.

A listing that printed no snapshot /and/ said something on stderr is
'Unknown': the exit code is @0@ even when plakar could not start its cache
process, and "the store is empty" is not what that means.
-}
interpretSnapshotList :: UTCTime -> NominalDiffTime -> ExitCode -> Text -> Text -> CheckResult
interpretSnapshotList _ _ (ExitFailure _) _ _ = Unknown
interpretSnapshotList now maxAge ExitSuccess out err =
    case asum (fmap stamp (Text.lines out)) of
        Just t
            | now `diffUTCTime` t <= maxAge -> Success
            | otherwise ->
                Failure ("newest snapshot is " <> hours (now `diffUTCTime` t) <> "h old, the limit is " <> hours maxAge <> "h")
        Nothing
            | Text.null (Text.strip err) && all (Text.null . Text.strip) (Text.lines out) -> Failure "the store has no snapshot"
            | otherwise -> Unknown
  where
    stamp :: Text -> Maybe UTCTime
    stamp line = case Text.words line of
        (w : _) -> iso8601ParseM (Text.unpack w)
        [] -> Nothing
    hours :: NominalDiffTime -> Text
    hours d = Text.pack (show (floor (realToFrac d / 3600 :: Double) :: Int))

{- | The script the cron entry runs. Backup output is captured and required to
say it completed without errors, because a plakar that could not start its
cache process exits @0@; the prune only runs after that, and only with the
declared policy.
-}
renderBackupScript :: PlakarJob -> [Text] -> Text
renderBackupScript job prune =
    Text.unlines
        [ "#!/bin/bash"
        , "# generated by salmon (plakar job " <> job.jobName <> "); do not edit"
        , "set -euo pipefail"
        , "out=$(plakar -keyfile " <> q key <> " at " <> q store <> " backup " <> q source <> " 2>&1) || { echo \"$out\" >&2; exit 1; }"
        , "echo \"$out\""
        , "echo \"$out\" | grep -q 'completed without errors' || { echo 'plakar backup did not report success' >&2; exit 1; }"
        , "plakar -keyfile " <> q key <> " at " <> q store <> " prune " <> Text.unwords prune <> " -apply"
        ]
  where
    key = Text.pack job.jobStore.storeKeyFile
    store = Text.pack job.jobStore.storePath
    source = Text.pack job.jobSource
    q t = "'" <> Text.replace "'" "'\\''" t <> "'"

-------------------------------------------------------------------------------

-- | Which release of an integration to install. No default: a directive always says.
data IntegrationVersion
    = Pinned Text
    | -- | Whatever is newest at first install. The check cannot tell newest
      -- from installed, so an installed integration is never upgraded.
      Latest
    deriving (Eq, Show)

-- | A Plakar integration (@gcs@, @s3@, ...), installed for one user.
data Integration = Integration
    { integrationName :: Text
    , integrationVersion :: IntegrationVersion
    , integrationUser :: Maybe Text
    -- ^ whose plugin directory it goes in: the user that runs the backup
    }
    deriving (Eq, Show)

-- | The argument @pkg add@ takes: @name@ or @name\@version@.
pkgAddArg :: Integration -> Text
pkgAddArg i = case i.integrationVersion of
    Latest -> i.integrationName
    Pinned v -> i.integrationName <> "@" <> v

{- | Installs an integration with @plakar pkg add@, checked against @plakar pkg
list@; @down@ is @pkg rm@. Whether @pkg add@ on an installed package is
harmless is unverified, which is why @up@ only runs when the check fails.
-}
plakarIntegration :: Track' (Binary "plakar") -> Integration -> Op
plakarIntegration plakar i =
    op "plakar-integration" (deps [justInstall plakar]) $ \actions ->
        actions
            { help = "installs plakar integration " <> pkgAddArg i
            , notes =
                [ "installed for " <> maybe "the user running salmon" id i.integrationUser
                , "latest means newest at first install: an installed integration is never upgraded"
                ]
            , ref = mkRef "plakar-integration" (i.integrationName, i.integrationUser)
            , check = do
                (code, out, _) <- readCreateProcessWithExitCode (asUser i.integrationUser ["plakar", "pkg", "list"]) ""
                pure (interpretPkgList i code (decode out))
            , up = () <$ runAs i.integrationUser ["plakar", "pkg", "add", Text.unpack (pkgAddArg i)]
            , down = () <$ runAs i.integrationUser ["plakar", "pkg", "rm", Text.unpack i.integrationName]
            }

{- | Is the integration in a @pkg list@ output? Tolerant of the format: some
line has the name as a word (or before an @\@@) and, when pinned, mentions the
version.
-}
interpretPkgList :: Integration -> ExitCode -> Text -> CheckResult
interpretPkgList _ (ExitFailure _) _ = Unknown
interpretPkgList i ExitSuccess out
    | any matches (Text.lines out) = Success
    | otherwise = Failure ("integration " <> pkgAddArg i <> " is not installed")
  where
    matches l =
        let ws = concatMap (Text.splitOn "@") (Text.words l)
            versioned = case i.integrationVersion of
                Latest -> True
                Pinned v -> v `Text.isInfixOf` l
         in i.integrationName `elem` ws && versioned

-- | A GCS store, named in plakar's per-user store configuration.
data GcsStore = GcsStore
    { gcsStoreName :: Text
    , gcsBucket :: Text
    , gcsPrefix :: Text
    -- ^ path inside the bucket, may be empty
    , gcsCredentialsFile :: FilePath
    -- ^ a service-account key, provisioned by somebody else; only its path is ever an argument
    , gcsCredentialsOption :: Text
    -- ^ the integration's option name for it (unverified: @application_credentials@ is a guess)
    , gcsStoreUser :: Maybe Text
    }
    deriving (Eq, Show)

-- | The arguments after @plakar store add@: name, location, credentials option.
gcsStoreAddArgs :: GcsStore -> [Text]
gcsStoreAddArgs s =
    [ s.gcsStoreName
    , "gcs://" <> s.gcsBucket <> (if Text.null s.gcsPrefix then "" else "/" <> Text.dropWhile (== '/') s.gcsPrefix)
    , s.gcsCredentialsOption <> "=" <> Text.pack s.gcsCredentialsFile
    ]

{- | Declares the store with @plakar store add@, once the integration is
installed. @down@ is @store rm@: the configuration entry only, the bucket's
contents are not touched. Refuses at @up@ a credentials file that is missing,
empty or readable by others.
-}
gcsStore :: Op -> GcsStore -> Op
gcsStore integration s =
    op "plakar-gcs-store" (deps [integration]) $ \actions ->
        actions
            { help = "plakar store " <> s.gcsStoreName <> " on gs://" <> s.gcsBucket
            , notes = ["credentials from " <> Text.pack s.gcsCredentialsFile <> ", never copied by salmon"]
            , ref = mkRef "plakar-gcs-store" (s.gcsStoreName, s.gcsStoreUser)
            , check = do
                (code, out, _) <- readCreateProcessWithExitCode (asUser s.gcsStoreUser ["plakar", "store", "show"]) ""
                pure (interpretStoreShow s code (decode out))
            , up = do
                problem <- keyfileStatus s.gcsCredentialsFile
                maybe (pure ()) (\p -> ioError (userError ("credentials " <> s.gcsCredentialsFile <> ": " <> Text.unpack p))) problem
                () <$ runAs s.gcsStoreUser (["plakar", "store", "add"] <> map Text.unpack (gcsStoreAddArgs s))
            , down = () <$ runAs s.gcsStoreUser ["plakar", "store", "rm", Text.unpack s.gcsStoreName]
            }

-- | The store is declared when @store show@ names it and its location.
interpretStoreShow :: GcsStore -> ExitCode -> Text -> CheckResult
interpretStoreShow _ (ExitFailure _) _ = Unknown
interpretStoreShow s ExitSuccess out
    | s.gcsStoreName `Text.isInfixOf` out && ("gcs://" <> s.gcsBucket) `Text.isInfixOf` out = Success
    | otherwise = Failure ("no store " <> s.gcsStoreName <> " on gs://" <> s.gcsBucket)

{- | Creates the Kloset inside a named remote store (@plakar at \@name create@).
The check is a listing: success with nothing on stderr, since the exit code is
@0@ even when plakar could not start its cache process.
-}
remoteKloset :: Op -> Maybe Text -> Text -> FilePath -> Op
remoteKloset storeOp user name keyfile =
    op "plakar-remote-kloset" (deps [storeOp]) $ \actions ->
        actions
            { help = "kloset in remote store @" <> name
            , notes = ["passphrase from " <> Text.pack keyfile, "down does not delete the store"]
            , ref = mkRef "plakar-remote-kloset" (name, user)
            , check = existsNow
            , up = do
                problem <- keyfileStatus keyfile
                maybe (pure ()) (\p -> ioError (userError ("keyfile " <> keyfile <> ": " <> Text.unpack p))) problem
                _ <- runAs user ["plakar", "-keyfile", keyfile, "at", "@" <> Text.unpack name, "create"]
                r <- existsNow
                case r of
                    Success -> pure ()
                    _ -> ioError (userError ("plakar create left no usable store @" <> Text.unpack name))
            , down = pure ()
            }
  where
    existsNow = do
        (code, _, err) <- readCreateProcessWithExitCode (asUser user ["plakar", "-keyfile", keyfile, "at", "@" <> Text.unpack name, "ls", "-latest"]) ""
        pure $ case code of
            ExitSuccess | Text.null (Text.strip (decode err)) -> Success
            ExitSuccess -> Failure ("store @" <> name <> " said: " <> Text.take 200 (decode err))
            ExitFailure _ -> Failure ("store @" <> name <> " is not usable")

-- | @sudo -H -u USER cmd@, or the command itself.
asUser :: Maybe Text -> [String] -> CreateProcess
asUser Nothing (c : args) = proc c args
asUser Nothing [] = proc "true" []
asUser (Just u) argv = proc "sudo" (["-H", "-u", Text.unpack u] <> argv)

runAs :: Maybe Text -> [String] -> IO Text
runAs u argv = do
    (code, out, err) <- readCreateProcessWithExitCode (asUser u argv) ""
    case code of
        ExitSuccess -> pure (decode out)
        ExitFailure n -> throwIO (Binary.CommandFailedSimple (unwords (take 3 argv) <> ": " <> take 500 (Text.unpack (decode err))) n)

-------------------------------------------------------------------------------

decode ::ByteString.ByteString -> Text
decode = Text.decodeUtf8With TextError.lenientDecode

-- | Runs a command, throwing on a non-zero exit; returns its stdout.
run' :: String -> [String] -> IO Text
run' cmd args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc cmd args) ""
    case code of
        ExitSuccess -> pure (decode out)
        ExitFailure n -> throwIO (Binary.CommandFailedSimple (cmd <> ": " <> take 500 (Text.unpack (decode err))) n)
