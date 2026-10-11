{-# LANGUAGE DeriveGeneric #-}

{- | @pg_textsearch@ (<https://github.com/timescale/pg_textsearch>): a
PostgreSQL extension adding a @bm25@ index access method, for full-text
search ranked with BM25.

'pgTextSearch' makes one cluster able to build and use such indexes. In
order: a gate on the server's major version, the release's files, the
library in @shared_preload_libraries@ (keeping whatever else is listed),
a restart if the declaration asks for one, a gate on the library being the
one loaded, the @pg_textsearch.*@ settings, and @CREATE EXTENSION@ in each
declared database. Creating the indexes is schema, and is a migration's job.

What was read upstream, at release @v1.5.1@ (2026-10), and shaped this
module. Re-read it before trusting any of it against a later release:

* __PostgreSQL 17 and 18 only__ (the README; 19 is \"best-effort\" and has no
  published binary). 'supportedMajors'.
* __A release zip is not a tree of files.__ @pg-textsearch-v1.5.1-pg17-amd64.zip@
  holds one entry, a Debian package
  @pg-textsearch-postgresql-17_1.5.1-1_amd64.deb@, whose content is
  @usr\/lib\/postgresql\/17\/lib\/pg_textsearch.so@,
  @usr\/share\/postgresql\/17\/extension\/pg_textsearch.control@ and the
  @pg_textsearch--*.sql@ scripts next to it. 'releaseFiles' takes the
  package out of the zip and unpacks it in a temporary directory
  (@dpkg-deb -x@: nothing is registered with @dpkg@ and no maintainer script
  runs), then copies those files where the server's own @pg_config@ says
  libraries and extension files go.
* __The library must be preloaded__: its @_PG_init@ raises
  @pg_textsearch must be loaded via shared_preload_libraries@ otherwise.
  That setting only changes at a restart.
* __The loaded library says which version it is__, in the read-only setting
  @pg_textsearch.library_version@, which is the control file's
  @default_version@. 'loadedGate' asks it, because replacing the files of a
  running server changes nothing the server reports as pending.
* __The settings__ are the ten of the README's table, registered in
  @src\/mod.c@ under those names: @memory_limit@ with context @sighup@,
  @default_limit@, @compress_segments@ and @memtable_cache_enabled@ with
  @user@, the others with @superuser@. None needs a restart. The settings
  node does not rely on that: it reads @pg_settings.context@ and
  @pending_restart@ and says so when a value it wrote is not in effect.

The pinned sha256 is the digest of a public release artifact (GitHub shows
it next to the asset). It is not a secret, and it is in the node's notes.

Out of scope: BM25 indexes (schema: migrations), managed background
compaction (it needs @pg_durable@ preloaded and granted), building from
source, and choosing how the zip reaches the machine: it is a path, and
somebody else's to provision.
-}
module Salmon.Builtin.Nodes.PgTextSearch (
    -- * Declaration
    PgTextSearch (..),
    pgTextSearchOn,
    Release (..),
    releaseZipName,
    Restart (..),
    Settings (..),
    defaultSettings,
    debianPgConfig,

    -- * Nodes
    pgTextSearch,
    versionGate,
    releaseFiles,
    loadedGate,
    settingsNode,
    libraryName,

    -- * Pure parts
    supportedMajors,
    gateVerdict,
    releaseProblems,
    DebName (..),
    renderDebName,
    parseDebName,
    selectDeb,
    InstallDirs (..),
    Install (..),
    planInstall,
    isExtensionFile,
    parseControlVersion,
    dpkgListFile,
    loadedVersionSql,
    interpretLoaded,
    Setting (..),
    settings,
    settingNotes,
    problems,
    inspectSettingsSql,
    applySettingsSql,
    resetSettingsSql,
    settingDiffs,
    interpretSettings,
    PgTextSearchError (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, IOException, bracket, throwIO, try)
import Control.Monad (filterM, forM, forM_, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.Char (isAsciiLower, isDigit)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.Generics (Generic)
import System.Directory (
    copyFile,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    pathIsSymbolicLink,
    removeDirectoryRecursive,
    renameFile,
 )
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Temp (mkdtemp)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Nodes.LlamaServer (sha256File)
import Salmon.Builtin.Nodes.Postgres (ClusterName, DatabaseName, PgExtension (..), Port)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | The name the library is preloaded under, the extension's name, and the prefix of its settings and files.
libraryName :: Text
libraryName = "pg_textsearch"

-- | The majors a release is published for.
supportedMajors :: [Int]
supportedMajors = [17, 18]

{- | One published release, as a file somebody put on the target machine.

The three fields say the same thing three times on purpose: the version is
what the installed control file is compared to (so a check needs neither the
zip nor a hash of three megabytes), the sha256 is what the zip must be before
anything is taken out of it, and the path is where it is.
-}
data Release = Release
    { releaseVersion :: Text
    -- ^ @1.5.1@: the extension's version, which is the package's without its @-1@ revision
    , releaseZip :: FilePath
    -- ^ the zip for the cluster's major and the machine's architecture; see 'releaseZipName'
    , releaseSha256 :: Text
    -- ^ of the zip, 64 hex digits. Public: GitHub lists it as the asset's digest.
    }
    deriving (Eq, Show)

-- | @pg-textsearch-v1.5.1-pg17-amd64.zip@ for @"1.5.1"@, @17@, @"amd64"@: the asset's name upstream.
releaseZipName :: Text -> Int -> Text -> FilePath
releaseZipName version major arch =
    Text.unpack ("pg-textsearch-v" <> version <> "-pg" <> tshow major <> "-" <> arch <> ".zip")

{- | Who restarts the cluster once the library is in
@shared_preload_libraries@. There is no default: a restart of a server
somebody is using is a thing to declare.
-}
data Restart
    = -- | This declaration does, with 'Postgres.restartClusterIfPending':
      -- a real restart of the whole named cluster, the first time and
      -- whenever anything else left a restart pending.
      RestartWhenPending ClusterName
    | -- | Somebody else does (an operator, or a cluster manager that
      -- restarts replicas first). Until then 'loadedGate' fails, and with
      -- it the settings and the extension, saying a restart is awaited.
      RestartLeftToOperator
    deriving (Eq, Show)

-- | @pg_textsearch.*@. A 'Nothing' leaves the server's value alone; all of them 'Nothing' declares no settings node at all.
data Settings = Settings
    { setDefaultLimit :: Maybe Int
    -- ^ 1..100000 (default 1000): the initial scoring batch of a query with no @LIMIT@
    , setCompressSegments :: Maybe Bool
    , setSegmentsPerLevel :: Maybe Int
    -- ^ 2..64 (default 8)
    , setMaxSegmentSizeMb :: Maybe Int
    -- ^ megabytes, 1..4095 (default 4095)
    , setBackgroundCompactionSchedule :: Maybe Text
    -- ^ a cron schedule. Only a default for indexes entering managed
    -- background compaction, which this module does not set up.
    , setBulkLoadThreshold :: Maybe Int
    -- ^ terms per transaction before a spill; 0 disables
    , setMemtablePagesThreshold :: Maybe Int
    -- ^ chain pages before a spill; 0 disables
    , setAllowRls :: Maybe Bool
    -- ^ whether BM25 indexes are allowed on tables with row-level security.
    -- On by default upstream, which accepts a documented term-frequency leak.
    , setMemtableCacheEnabled :: Maybe Bool
    , setMemoryLimitKb :: Maybe Int
    -- ^ kilobytes (default 2GB); 0 is no limit
    }
    deriving (Eq, Show)

-- | Sets nothing.
defaultSettings :: Settings
defaultSettings = Settings Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data PgTextSearch = PgTextSearch
    { ptsMajor :: Int
    -- ^ the cluster's major version, declared rather than discovered so the
    -- graph does not depend on a running server. 'versionGate' refuses a
    -- server that is of another one.
    , ptsPort :: Port
    , ptsRelease :: Release
    , ptsPgConfig :: FilePath
    -- ^ that server's own @pg_config@ ('debianPgConfig'). Not the one on
    -- @PATH@, which on Debian comes with @libpq-dev@ and may be of another major.
    , ptsRestart :: Restart
    , ptsDatabases :: [DatabaseName]
    -- ^ databases to @CREATE EXTENSION@ in. Empty on a standby, which
    -- receives the catalog rows from its primary and needs the files and
    -- the preload only.
    , ptsUpgrade :: Bool
    -- ^ also @ALTER EXTENSION pg_textsearch UPDATE@ when the files are newer (see 'extUpgrade')
    , ptsSettings :: Settings
    }
    deriving (Eq, Show)

-- | @\/usr\/lib\/postgresql\/N\/bin\/pg_config@, where Debian's server package puts it.
debianPgConfig :: Int -> FilePath
debianPgConfig major = "/usr/lib/postgresql" </> show major </> "bin/pg_config"

-- | A declaration on Debian's layout, with no database, no setting and no upgrade.
pgTextSearchOn :: Int -> Port -> Restart -> Release -> PgTextSearch
pgTextSearchOn major port restart release =
    PgTextSearch
        { ptsMajor = major
        , ptsPort = port
        , ptsRelease = release
        , ptsPgConfig = debianPgConfig major
        , ptsRestart = restart
        , ptsDatabases = []
        , ptsUpgrade = False
        , ptsSettings = defaultSettings
        }

newtype PgTextSearchError = PgTextSearchError Text
    deriving (Show)

instance Exception PgTextSearchError

refuse :: Text -> IO a
refuse = throwIO . PgTextSearchError

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-------------------------------------------------------------------------------
-- version gate

{- | Whether a server answering @SHOW server_version_num@ with this text is
one the declared release can be installed on. The message names the version.
-}
gateVerdict :: Int -> Text -> Either Text ()
gateVerdict declared out =
    case Text.unpack (Text.strip out) of
        ds
            | not (null ds)
            , all isDigit ds
            , length ds <= 9 ->
                let num = read ds :: Int
                    major = num `div` 10000
                 in if major `notElem` supportedMajors
                        then Left (libraryName <> " supports PostgreSQL " <> majors <> " only, this server is PostgreSQL " <> tshow major <> " (server_version_num " <> Text.pack ds <> ")")
                        else
                            if major /= declared
                                then Left ("this server is PostgreSQL " <> tshow major <> " (server_version_num " <> Text.pack ds <> "), the declaration is for " <> tshow declared)
                                else Right ()
        _ -> Left ("cannot read server_version_num from " <> tshow (Text.take 80 out))
  where
    majors = Text.intercalate " and " (fmap tshow supportedMajors)

{- | Refuses, at @up@, a server that is not of a supported major or not of
the declared one. Everything else depends on it, so a wrong-major package
is never copied and the cluster is never restarted with it preloaded.

An unreachable server is 'Unknown' for @check@ and a failure for @up@:
nothing here should proceed on a server nobody could ask.
-}
versionGate :: Int -> Port -> Op
versionGate major port =
    op "pg-textsearch-version-gate" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-textsearch-version-gate" port
            , help = Text.unwords [libraryName, "supports the PostgreSQL major of the cluster on port", tshow port]
            , notes = ["declared major: " <> tshow major, "supported: " <> Text.intercalate ", " (fmap tshow supportedMajors)]
            , check = either (const Unknown) (either Failure (const Success) . gateVerdict major) <$> ask
            , up = do
                out <- either (\err -> refuse ("cannot read server_version_num on port " <> tshow port <> ": " <> Text.strip err)) pure =<< ask
                either refuse pure (gateVerdict major out)
            }
  where
    ask = Postgres.psqlQuery_Sudo port "SHOW server_version_num"

-------------------------------------------------------------------------------
-- the release's files

-- | What is wrong with a release as declared, before anything is read.
releaseProblems :: Int -> Release -> [Text]
releaseProblems major rel =
    concat
        [ [libraryName <> " supports PostgreSQL " <> Text.intercalate " and " (fmap tshow supportedMajors) <> " only, the declaration is for " <> tshow major | major `notElem` supportedMajors]
        , ["the release version " <> tshow rel.releaseVersion <> " is not digits and dots" | not (validVersion rel.releaseVersion)]
        , ["the pinned sha256 is not 64 hex digits" | not (validSha rel.releaseSha256)]
        ]
  where
    validSha s = Text.length s == 64 && Text.all (`elem` (['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F'])) s

validVersion :: Text -> Bool
validVersion v = not (Text.null v) && all part (Text.splitOn "." v)
  where
    part p = not (Text.null p) && Text.all isDigit p

-- | The name of the package inside a release zip, taken apart.
data DebName = DebName
    { debMajor :: Int
    , debVersion :: Text
    -- ^ @1.5.1@
    , debRevision :: Int
    -- ^ the Debian revision, @1@ in every release seen
    , debArch :: Text
    -- ^ @amd64@, @arm64@
    }
    deriving (Eq, Show)

-- | @pg-textsearch-postgresql-17_1.5.1-1_amd64.deb@
renderDebName :: DebName -> Text
renderDebName d =
    "pg-textsearch-postgresql-" <> tshow d.debMajor <> "_" <> d.debVersion <> "-" <> tshow d.debRevision <> "_" <> d.debArch <> ".deb"

{- | The inverse of 'renderDebName', and strict: anything it accepts renders
back to the same text, and holds no character a shell or an archive tool
reads as a pattern or a path.
-}
parseDebName :: Text -> Maybe DebName
parseDebName t = do
    body <- Text.stripPrefix "pg-textsearch-postgresql-" t >>= Text.stripSuffix ".deb"
    (majorT, versionT, revisionT, arch) <- case Text.splitOn "_" body of
        [m, vr, a] -> case Text.splitOn "-" vr of
            [v, r] -> Just (m, v, r, a)
            _ -> Nothing
        _ -> Nothing
    major <- number majorT
    revision <- number revisionT
    if validVersion versionT && not (Text.null arch) && Text.all (\c -> isAsciiLower c || isDigit c) arch
        then Just (DebName major versionT revision arch)
        else Nothing
  where
    -- no sign, no leading zero, and small enough to be an Int anywhere
    number s
        | Text.null s || not (Text.all isDigit s) || Text.length s > 9 = Nothing
        | Text.length s > 1 && Text.head s == '0' = Nothing
        | otherwise = Just (read (Text.unpack s))

{- | The one package a zip must hold, given its listing: for the declared
major, the declared version and the machine's architecture. A zip with
anything else in it is refused rather than searched.
-}
selectDeb :: Int -> Text -> Text -> [Text] -> Either Text DebName
selectDeb major version arch entries =
    case filter (not . Text.null) entries of
        [entry] -> case parseDebName entry of
            Nothing -> Left ("the zip holds " <> tshow entry <> ", which is not a pg-textsearch-postgresql-MAJOR_VERSION-REV_ARCH.deb")
            Just d
                | d.debMajor /= major -> Left ("the zip holds a package for PostgreSQL " <> tshow d.debMajor <> ", the declaration is for " <> tshow major)
                | d.debVersion /= version -> Left ("the zip holds version " <> d.debVersion <> ", the declaration is for " <> version)
                | d.debArch /= arch -> Left ("the zip holds a package for " <> d.debArch <> ", this machine is " <> arch)
                | otherwise -> Right d
        es -> Left ("the zip holds " <> tshow (length es) <> " entries, expected one Debian package")

-- | Where a server looks for libraries and extension files (@pg_config --pkglibdir@, and @extension@ under @--sharedir@).
data InstallDirs = InstallDirs
    { pkgLibDir :: FilePath
    , extensionDir :: FilePath
    }
    deriving (Eq, Show)

-- | One file of the package and where it goes.
data Install = Install
    { installFrom :: FilePath
    -- ^ relative to the unpacked package
    , installTo :: FilePath
    , installExecutable :: Bool
    }
    deriving (Eq, Show)

-- | @pg_textsearch.control@ or a @pg_textsearch--*.sql@ script: the names this module writes into, and removes from, the extension directory.
isExtensionFile :: FilePath -> Bool
isExtensionFile name =
    name == lib <> ".control"
        || (Text.isPrefixOf (Text.pack lib <> "--") (Text.pack name) && Text.isSuffixOf ".sql" (Text.pack name) && '/' `notElem` name)
  where
    lib = Text.unpack libraryName

{- | What to copy where, given the regular files of an unpacked package
(relative paths). The library first and the control file last: the control
file is what 'releaseFiles' checks, so an install that stops half-way is
seen as not done.

Refused: a package without the library, the control file or the script of
the declared version for this major (which is what a package built for
another major looks like), and a package holding any file this module has
no place for. The second is strict on purpose: a later release that ships
something more should be read by a person before it is copied as root.
-}
planInstall :: Int -> Text -> InstallDirs -> [FilePath] -> Either Text [Install]
planInstall major version dirs files
    | libRel `notElem` files = Left ("the package has no " <> Text.pack libRel <> ": it is not built for PostgreSQL " <> tshow major)
    | control `notElem` extNames = Left ("the package has no " <> Text.pack (extRel </> control))
    | script `notElem` extNames = Left ("the package has no " <> Text.pack (extRel </> script) <> ": it is not version " <> version)
    | not (null strangers) = Left ("the package holds files this node has no place for: " <> Text.intercalate ", " (fmap Text.pack strangers))
    | otherwise =
        Right $
            [Install libRel (dirs.pkgLibDir </> takeFileName libRel) True]
                <> [Install (extRel </> n) (dirs.extensionDir </> n) False | n <- sort extNames, n /= control]
                <> [Install (extRel </> control) (dirs.extensionDir </> control) False]
  where
    lib = Text.unpack libraryName
    libRel = "usr/lib/postgresql" </> show major </> "lib" </> lib <> ".so"
    extRel = "usr/share/postgresql" </> show major </> "extension"
    control = lib <> ".control"
    script = lib <> "--" <> Text.unpack version <> ".sql"
    inExt f = [n | Just n <- [stripDir extRel f], isExtensionFile n]
    extNames = concatMap inExt files
    strangers = sort [f | f <- files, f /= libRel, null (inExt f)]
    stripDir d f = Text.unpack <$> Text.stripPrefix (Text.pack d <> "/") (Text.pack f)

{- | The @default_version@ of a control file. Comment lines and other keys
are skipped; the value is read with or without its quotes.
-}
parseControlVersion :: Text -> Maybe Text
parseControlVersion content =
    listToMaybe
        [ v
        | line <- Text.lines content
        , let (key, rest) = Text.breakOn "=" (Text.strip line)
        , Text.strip key == "default_version"
        , Just raw <- [Text.stripPrefix "=" rest]
        , let v = value (Text.strip raw)
        , not (Text.null v)
        ]
  where
    value raw = case Text.stripPrefix "'" raw of
        Just quoted -> Text.takeWhile (/= '\'') quoted
        Nothing -> Text.takeWhile (\c -> c /= '#' && c /= ' ' && c /= '\t') raw

{- | The file @dpkg@ keeps for the package the zip holds, when somebody
installed that package the Debian way. Its presence means the files are
@dpkg@'s: this module then neither replaces nor removes them.
-}
dpkgListFile :: Int -> FilePath
dpkgListFile major = "/var/lib/dpkg/info/pg-textsearch-postgresql-" <> show major <> ".list"

runTool :: FilePath -> [String] -> IO Text
runTool cmd args = do
    r <- try (readCreateProcessWithExitCode (proc cmd args) "")
    case r of
        Left (e :: IOException) -> refuse ("cannot run " <> Text.pack cmd <> ": " <> tshow e)
        Right (ExitSuccess, out, _) -> pure (decode out)
        Right (ExitFailure n, _, err) ->
            refuse (Text.pack cmd <> " exited " <> tshow n <> ": " <> Text.take 500 (Text.strip (decode err)))
  where
    decode = Text.decodeUtf8With TextError.lenientDecode

installDirs :: FilePath -> IO InstallDirs
installDirs pgConfig = do
    lib <- Text.unpack . Text.strip <$> runTool pgConfig ["--pkglibdir"]
    share <- Text.unpack . Text.strip <$> runTool pgConfig ["--sharedir"]
    when (null lib || null share) $ refuse (Text.pack pgConfig <> " named no directory")
    pure (InstallDirs lib (share </> "extension"))

-- | The regular files under a directory, relative to it. A symbolic link is refused: nothing in a release is one.
regularFiles :: FilePath -> IO [FilePath]
regularFiles root = go ""
  where
    go rel = do
        names <- listDirectory (root </> rel)
        fmap concat . forM (sort names) $ \n -> do
            let r = if null rel then n else rel </> n
            link <- pathIsSymbolicLink (root </> r)
            when link $ refuse ("the package holds a symbolic link: " <> Text.pack r)
            isDir <- doesDirectoryExist (root </> r)
            if isDir then go r else pure [r]

{- | The release's files, where the server of that major looks for them.

@check@ reads the installed control file's @default_version@ and looks for
the library and that version's script. It needs neither the zip nor a
running server, so it also answers on a standby and after the zip was
cleaned away. @up@ verifies the zip against the pinned sha256 before
listing it, refuses anything in it that is not the one expected package,
and replaces each file with a rename, so a server that has the library
mapped keeps the one it loaded.

A replaced library is only loaded at the next restart, and PostgreSQL does
not report that as pending: 'loadedGate' is what notices.

@down@ removes @pg_textsearch.so@, @pg_textsearch.control@ and the
@pg_textsearch--*.sql@ scripts, whoever put them there, except when the
Debian package of that name is installed: then they are @dpkg@'s, and @up@
refuses to replace them for the same reason.
-}
releaseFiles :: Int -> FilePath -> Release -> Op
releaseFiles major pgConfig rel =
    op "pg-textsearch-files" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-textsearch-files" pgConfig
            , help = Text.unwords [libraryName, rel.releaseVersion, "files for postgresql", tshow major]
            , notes =
                [ "from " <> Text.pack rel.releaseZip
                , "pinned sha256 of the release zip (a public artifact, not a secret): " <> Text.toLower rel.releaseSha256
                , "directories from " <> Text.pack pgConfig
                , "a replaced library is loaded at the next restart, which this node does not force"
                ]
            , check = chk
            , up = install
            , down = uninstall
            }
  where
    lib = Text.unpack libraryName
    chk = case releaseProblems major rel of
        ps@(_ : _) -> pure (Failure (Text.intercalate "; " ps))
        [] -> do
            dirs <- try (installDirs pgConfig)
            case dirs of
                Left (PgTextSearchError e) -> pure (Failure e)
                Right d -> do
                    let control = d.extensionDir </> lib <> ".control"
                        wanted = [d.pkgLibDir </> lib <> ".so", d.extensionDir </> lib <> "--" <> Text.unpack rel.releaseVersion <> ".sql"]
                    haveControl <- doesFileExist control
                    if not haveControl
                        then pure (Failure (Text.pack control <> " is missing"))
                        else do
                            installed <- parseControlVersion . Text.decodeUtf8With TextError.lenientDecode <$> ByteString.readFile control
                            missing <- filterM (fmap not . doesFileExist) wanted
                            pure $ case (installed, missing) of
                                (Just v, _) | v /= rel.releaseVersion -> Failure (Text.pack control <> " is version " <> v <> ", declared " <> rel.releaseVersion)
                                (Nothing, _) -> Failure (Text.pack control <> " has no default_version")
                                (_, m : _) -> Failure (Text.pack m <> " is missing")
                                _ -> Success
    install = do
        let ps = releaseProblems major rel
        unless (null ps) $ refuse (Text.intercalate "; " ps)
        owned <- doesFileExist (dpkgListFile major)
        when owned $ refuse ("the Debian package pg-textsearch-postgresql-" <> tshow major <> " is installed: its files are dpkg's, and this node does not replace them")
        haveZip <- doesFileExist rel.releaseZip
        unless haveZip $ refuse (Text.pack rel.releaseZip <> " is missing: providing the release zip is the caller's job")
        actual <- sha256File rel.releaseZip
        when (actual /= Text.toLower rel.releaseSha256) $
            refuse (Text.pack rel.releaseZip <> " has sha256 " <> actual <> ", the pinned one is " <> Text.toLower rel.releaseSha256)
        dirs <- installDirs pgConfig
        forM_ [dirs.pkgLibDir, dirs.extensionDir] $ \d -> do
            there <- doesDirectoryExist d
            unless there $ refuse (Text.pack d <> " does not exist: is postgresql " <> tshow major <> " installed?")
        arch <- Text.strip <$> runTool "dpkg" ["--print-architecture"]
        entries <- Text.lines <$> runTool "unzip" ["-Z1", rel.releaseZip]
        deb <- either refuse pure (selectDeb major rel.releaseVersion arch entries)
        withScratch $ \tmp -> do
            let debName = Text.unpack (renderDebName deb)
            _ <- runTool "unzip" ["-q", "-j", rel.releaseZip, debName, "-d", tmp </> "zip"]
            _ <- runTool "dpkg-deb" ["-x", tmp </> "zip" </> debName, tmp </> "pkg"]
            files <- regularFiles (tmp </> "pkg")
            plan <- either refuse pure (planInstall major rel.releaseVersion dirs files)
            forM_ plan $ \i -> do
                let part = i.installTo <> ".salmon-part"
                copyFile (tmp </> "pkg" </> i.installFrom) part
                setFileMode part (if i.installExecutable then 0o755 else 0o644)
                renameFile part i.installTo
    withScratch act = do
        base <- getTemporaryDirectory
        bracket (mkdtemp (base </> "salmon-pg-textsearch-")) removeDirectoryRecursive act
    uninstall = do
        owned <- doesFileExist (dpkgListFile major)
        havePgConfig <- doesFileExist pgConfig
        -- no pg_config: that PostgreSQL is gone, and its directories with it
        when (not owned && havePgConfig) $ do
            dirs <- installDirs pgConfig
            FS.removeFileIfPresent (dirs.pkgLibDir </> lib <> ".so")
            haveExt <- doesDirectoryExist dirs.extensionDir
            when haveExt $ do
                names <- listDirectory dirs.extensionDir
                forM_ (filter isExtensionFile names) $ \n -> FS.removeFileIfPresent (dirs.extensionDir </> n)

-------------------------------------------------------------------------------
-- the library is loaded

-- | The version of the library the running server has loaded, empty when it has none.
loadedVersionSql :: Text
loadedVersionSql = "SELECT coalesce(current_setting('pg_textsearch.library_version', true), '')"

-- | The verdict from 'loadedVersionSql''s output, given the declared version.
interpretLoaded :: Text -> Text -> CheckResult
interpretLoaded version out =
    case Text.strip out of
        "" -> Failure (libraryName <> " is not loaded in the running server: the cluster has to be restarted")
        v
            | v == version -> Success
            | otherwise -> Failure ("the running server has " <> libraryName <> " " <> v <> " loaded, the declared release is " <> version <> ": the cluster has to be restarted")

{- | The running server has the declared version of the library loaded.

This node changes nothing: it is the edge between \"the files and the
setting are in place\" and everything that needs the library in the server
(its settings, @CREATE EXTENSION@). Its @up@ asks again and throws, so that
under 'RestartLeftToOperator' the dependants wait for the restart instead of
failing one by one with PostgreSQL's less direct messages.

It is also the only node that sees a library replaced under a running
server, which no restart node does: @pending_restart@ stays false.
-}
loadedGate :: Port -> Text -> Op
loadedGate port version =
    op "pg-textsearch-loaded" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-textsearch-loaded" port
            , help = Text.unwords [libraryName, version, "is loaded in the cluster on port", tshow port]
            , notes = ["changes nothing: it fails until the cluster has been restarted with the library preloaded"]
            , check = either (const Unknown) (interpretLoaded version) <$> ask
            , up = do
                out <- either (\err -> refuse ("cannot ask the cluster on port " <> tshow port <> " which library it has loaded: " <> Text.strip err)) pure =<< ask
                case interpretLoaded version out of
                    Failure why -> refuse why
                    _ -> pure ()
            }
  where
    ask = Postgres.psqlQuery_Sudo port loadedVersionSql

-------------------------------------------------------------------------------
-- settings

-- | One declared setting.
data Setting = Setting
    { settingName :: Text
    , settingValue :: Text
    -- ^ as @pg_settings.setting@ reports it: a bare number in the setting's unit, @on@\/@off@, or the text
    , settingSql :: Text
    -- ^ what @ALTER SYSTEM SET@ is given, unit included
    }
    deriving (Eq, Show)

{- | The declared settings, in a stable order. Only these are written,
compared and (on the way down) reset; a @pg_textsearch.*@ setting that is
not declared is left as it is.
-}
settings :: Settings -> [Setting]
settings s =
    catMaybes
        [ int "default_limit" "" s.setDefaultLimit
        , bool "compress_segments" s.setCompressSegments
        , int "segments_per_level" "" s.setSegmentsPerLevel
        , int "max_segment_size" "MB" s.setMaxSegmentSizeMb
        , text "background_compaction_schedule" s.setBackgroundCompactionSchedule
        , int "bulk_load_threshold" "" s.setBulkLoadThreshold
        , int "memtable_pages_threshold" "" s.setMemtablePagesThreshold
        , bool "allow_rls" s.setAllowRls
        , bool "memtable_cache_enabled" s.setMemtableCacheEnabled
        , int "memory_limit" "kB" s.setMemoryLimitKb
        ]
  where
    name k = libraryName <> "." <> k
    int k unit = fmap (\n -> Setting (name k) (tshow n) (tshow n <> unit))
    bool k = fmap (\b -> let v = if b then "on" else "off" in Setting (name k) v v)
    text k = fmap (\t -> Setting (name k) t t)

-- | One line per setting for the node's notes.
settingNotes :: Settings -> [Text]
settingNotes = fmap (\s -> s.settingName <> " = " <> s.settingSql) . settings

{- | Reasons a declaration is refused, all of them. The ranges are the ones
the extension registers at v1.5.1; a server refuses the same values, later
and for one setting at a time.
-}
problems :: Settings -> [Text]
problems s =
    concat
        [ range "default_limit" 1 100000 s.setDefaultLimit
        , range "segments_per_level" 2 64 s.setSegmentsPerLevel
        , range "max_segment_size" 1 4095 s.setMaxSegmentSizeMb
        , range "bulk_load_threshold" 0 maxInt32 s.setBulkLoadThreshold
        , range "memtable_pages_threshold" 0 maxInt32 s.setMemtablePagesThreshold
        , range "memory_limit" 0 maxInt32 s.setMemoryLimitKb
        , [ "background_compaction_schedule is empty or has a line break in it"
          | Just t <- [s.setBackgroundCompactionSchedule]
          , Text.null (Text.strip t) || Text.any (`elem` ['\n', '\r']) t
          ]
        ]
  where
    maxInt32 = 2147483647
    range :: Text -> Int -> Int -> Maybe Int -> [Text]
    range k lo hi mv =
        [ k <> " is " <> tshow v <> ", outside " <> tshow lo <> ".." <> tshow hi
        | Just v <- [mv]
        , v < lo || v > hi
        ]

-- | What the running server says about one setting.
data Live = Live
    { setting :: Text
    , context :: Text
    , pending_restart :: Bool
    }
    deriving (Eq, Show, Generic)

instance Aeson.FromJSON Live

{- | One row, one JSON object keyed by setting name. @pg_settings@ rather
than @current_setting@: it gives the value in the setting's own unit, and
the @context@ and @pending_restart@ columns this module reads instead of
assuming what a change takes. A setting whose library is not loaded has no
row.
-}
inspectSettingsSql :: [Text] -> Text
inspectSettingsSql names =
    "SELECT coalesce(json_object_agg(name, json_build_object('setting', setting, 'context', context, 'pending_restart', pending_restart)), '{}') FROM pg_settings WHERE name IN ("
        <> Text.intercalate ", " (fmap Postgres.quoteLiteral names)
        <> ")"

-- | The batch that sets every declared setting and reloads.
applySettingsSql :: [Setting] -> Text
applySettingsSql ss =
    Text.unlines $
        ["ALTER SYSTEM SET " <> s.settingName <> " = " <> Postgres.quoteLiteral s.settingSql <> ";" | s <- ss]
            <> ["SELECT pg_reload_conf();"]

resetSettingsSql :: [Text] -> Text
resetSettingsSql names =
    Text.unlines $
        ["ALTER SYSTEM RESET " <> k <> ";" | k <- names]
            <> ["SELECT pg_reload_conf();"]

{- | What differs between the declaration and the running server, one line
per setting. A value that is written and waits for a restart is said to be
so, from what PostgreSQL reports about it and not from a list kept here.
-}
settingDiffs :: [Setting] -> Map Text Live -> [Text]
settingDiffs wanted got = concatMap one wanted
  where
    one :: Setting -> [Text]
    one s = case Map.lookup s.settingName got of
        Nothing -> [s.settingName <> " is not a setting of the running server (is " <> libraryName <> " loaded, and is it this release?)"]
        Just l
            | l.setting == s.settingValue -> []
            | l.pending_restart || l.context == "postmaster" ->
                [s.settingName <> " is " <> tshow l.setting <> ", declared " <> tshow s.settingValue <> ": it only changes at a restart (context " <> l.context <> "), which this node does not do"]
            | l.context == "internal" -> [s.settingName <> " is " <> tshow l.setting <> " and cannot be set (context internal)"]
            | otherwise -> [s.settingName <> " is " <> tshow l.setting <> ", declared " <> tshow s.settingValue]

decodeLive :: Text -> Maybe (Map Text Live)
decodeLive = Aeson.decodeStrict . Text.encodeUtf8 . Text.strip

-- | The verdict from 'inspectSettingsSql''s output. Output that is not the expected JSON is 'Unknown'.
interpretSettings :: [Setting] -> Text -> CheckResult
interpretSettings wanted out =
    case decodeLive out of
        Nothing -> Unknown
        Just got -> case settingDiffs wanted got of
            [] -> Success
            diffs -> Failure (Text.intercalate "; " diffs)

{- | The declared @pg_textsearch.*@ settings, written with @ALTER SYSTEM@
and applied with a reload. It has no dependency of its own: the library has
to be loaded first (a server without it refuses the names), which is
'loadedGate' in 'pgTextSearch'.

@up@ reads the values back and fails if one is not in effect, naming the
ones PostgreSQL says wait for a restart. Going down resets the declared
settings to the server's defaults, and does nothing on a server that does
not have the library loaded.
-}
settingsNode :: Track' (Binary "psql") -> Port -> Settings -> Op
settingsNode psql port s =
    op "pg-textsearch-settings" (deps [justInstall psql]) $ \actions ->
        actions
            { ref = mkRef "pg-textsearch-settings" port
            , help = Text.unwords [libraryName, "settings of the cluster on port", tshow port]
            , notes = settingNotes s
            , check = chk
            , up = up
            , down = down
            }
  where
    declared = settings s
    names = fmap (.settingName) declared
    ask = Postgres.psqlQuery_Sudo port (inspectSettingsSql names)
    chk = case problems s of
        ps@(_ : _) -> pure (Failure (Text.intercalate "; " ps))
        [] -> either (const Unknown) (interpretSettings declared) <$> ask
    up = do
        let ps = problems s
        unless (null ps) $ refuse (Text.intercalate "; " ps)
        Postgres.psqlBatchOrThrow_Sudo port (applySettingsSql declared)
        settle (20 :: Int)
    -- A reload is a signal: a new session may ask before the postmaster
    -- has read the files. Up to two seconds, then whatever still differs.
    settle n = do
        out <- either (\err -> refuse ("cannot read the settings back: " <> Text.strip err)) pure =<< ask
        case interpretSettings declared out of
            Success -> pure ()
            verdict
                | n > 0 -> threadDelay 100000 >> settle (n - 1)
                | otherwise -> refuse $ case verdict of
                    Failure why -> why
                    _ -> "cannot read the settings back: " <> Text.take 200 out
    down = do
        loaded <- Postgres.psqlQuery_Sudo port loadedVersionSql
        case loaded of
            Left err -> refuse ("cannot ask the cluster on port " <> tshow port <> " which library it has loaded: " <> Text.strip err)
            Right v -> unless (Text.null (Text.strip v)) $ Postgres.psqlBatchOrThrow_Sudo port (resetSettingsSql names)

-------------------------------------------------------------------------------

{- | A cluster on which BM25 indexes can be built and used.

In order: 'versionGate', 'releaseFiles', 'Postgres.preloadLibrary', the
restart ('Restart'), 'loadedGate', then 'settingsNode' (if anything is
declared) and one 'Postgres.extension' per database. The files come before
the preload entry, and the entry before any restart: a server asked to
preload a library it cannot find does not start.

Under 'RestartWhenPending' the restart is a real one, of the whole cluster,
the first time and whenever anything else left a restart pending. This node
does not know whether the cluster is a primary somebody is using; declaring
it is the decision. A release replaced by a newer one is not restarted for
(PostgreSQL does not report it as pending): 'loadedGate' then fails until
somebody restarts.

On a pair or a Patroni cluster every member needs the files and the
preload, standbys included. Declare this with no database on a standby; a
cluster-wide configuration (Patroni's) that owns @shared_preload_libraries@
conflicts with 'Postgres.preloadLibrary' and is not handled here.

Going down drops the extension from each database without @CASCADE@ (a
database with a BM25 index refuses, and that stops the rest), resets the
declared settings, takes the library out of the list and removes the files.
It restarts nothing: the library stays in the running server until its next
restart. A BM25 index in a database that was not declared is not seen, and
is unusable once the library is gone.
-}
pgTextSearch ::
    Reporter Postgres.Report ->
    Track' (Binary "psql") ->
    Track' (Binary "pg_ctlcluster") ->
    -- | how the databases of 'ptsDatabases' come to exist
    Track' DatabaseName ->
    PgTextSearch ->
    Op
pgTextSearch r psql pgctl mkdb cfg =
    op "pg-textsearch" (deps (configured : extensions)) $ \actions ->
        actions
            { ref = mkRef "pg-textsearch" cfg.ptsPort
            , help =
                Text.unwords
                    [libraryName, cfg.ptsRelease.releaseVersion, "on the postgresql", tshow cfg.ptsMajor, "cluster on port", tshow cfg.ptsPort, "in", tshow (length cfg.ptsDatabases), "database(s)"]
            }
  where
    gate = versionGate cfg.ptsMajor cfg.ptsPort
    files = releaseFiles cfg.ptsMajor cfg.ptsPgConfig cfg.ptsRelease `inject` gate
    preload = Postgres.preloadLibrary psql cfg.ptsPort libraryName `inject` files
    restarted = case cfg.ptsRestart of
        RestartWhenPending cluster -> Postgres.restartClusterIfPending r pgctl cfg.ptsPort cluster `inject` preload
        RestartLeftToOperator -> preload
    loaded = loadedGate cfg.ptsPort cfg.ptsRelease.releaseVersion `inject` restarted
    configured
        | null (settings cfg.ptsSettings) = loaded
        | otherwise = settingsNode psql cfg.ptsPort cfg.ptsSettings `inject` loaded
    extensions =
        [ Postgres.extension r psql cfg.ptsPort mkdb (PgExtension libraryName db (Just 170000) cfg.ptsUpgrade) `inject` loaded
        | db <- cfg.ptsDatabases
        ]
