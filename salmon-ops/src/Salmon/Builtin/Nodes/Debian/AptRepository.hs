{-# LANGUAGE OverloadedStrings #-}

{- | An external apt repository as a node: a deb822 @.sources@ file naming a
pre-provisioned signing key, an optional @preferences.d@ pin, and the
@apt-get update@ that makes the index know about it.

Nothing here decides /whether/ a recipe wants an external repository — that
is a risk somebody has to choose to take. A recipe that needs packages from
one takes a @'Salmon.Builtin.Extension.Track'' 'AptRepository'@ argument and
its author (or its seed) passes 'aptRepositoryTrack' to take the risk, or
'Salmon.Builtin.Extension.ignoreTrack' to require that the package is
installable already. See 'pgdg' for the first user.

= What the node refuses

* __A key whose fingerprint is not the declared one.__ The key is a file
  somebody provisioned (this module does not care how it got there: rsync,
  a secret store, the repository's own download page), and the fingerprint
  is what the declaration was written against. It is read with
  @gpg --show-keys --with-colons@ before the key is installed, and a
  mismatch throws 'KeyFingerprintMismatch'. Only /primary/ keys count: a
  subkey's fingerprint is not a statement about who published the file.
  The fingerprint is __declared, never read from a file next to the key__,
  because a pin that travels with the thing it pins pins nothing.
* __Shadowing the distribution.__ A repository that carries packages the
  distribution also has (PGDG ships @postgresql-<major>@) wins by version
  under apt's default priorities. So the default 'Pinning' is
  'OnlyPackages': the repository is pinned to priority 1 for everything and
  500 for the named patterns, and the whole suite ('WholeSuite') is an
  explicit choice.

= down

Removes the sources file, the preference file and the key. Packages that were
installed from the repository are left alone (removing them is the business
of whichever node installed them), and no @apt-get update@ is run: the stale
index entries go on the next update anybody runs.

= Requirements on the machine

@gpg@ (the @gpg@ package on Debian), and root for anything outside a test
root. 'aptRepository' does not install @gpg@ itself: tearing the repository
down should not uninstall a tool something else may be using.
-}
module Salmon.Builtin.Nodes.Debian.AptRepository (
    AptRepository (..),
    Suite (..),
    Pinning (..),
    KeyFingerprintMismatch (..),
    aptRepository,
    aptRepositoryTrack,
    viaRepository,
    pgdg,

    -- * Pieces, exposed for tests
    renderSources,
    renderPreferences,
    resolveSuite,
    parseOsReleaseCodename,
    primaryFingerprints,
    normalizeFingerprint,
    repositoryHost,
    listsPrefix,
    keyDestination,
    sourcesPath,
    preferencesPath,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import qualified Data.ByteString as ByteString
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NEList
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
 )
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem (FileContents (..), checkFileContents, removeFileIfPresent)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track (Track (..))

-- | How the repository's suite is spelled.
data Suite
    = -- | The machine's @VERSION_CODENAME@ (from @/etc/os-release@, read when
      -- the node runs, not when the directive is written) followed by this
      -- suffix: @CodenameSuffixed "-pgdg"@ is @bookworm-pgdg@ on bookworm.
      CodenameSuffixed Text
    | -- | A suite that does not depend on the machine.
      FixedSuite Text
    deriving (Eq, Show)

-- | What the repository is allowed to supply.
data Pinning
    = -- | Only packages matching these patterns (apt's @Package:@ globs,
      -- e.g. @postgresql-*-pgvector@) are taken from the repository; every
      -- other package it carries stays at priority 1, i.e. is installed
      -- from it only when nothing else has it.
      OnlyPackages (NEList.NonEmpty Text)
    | -- | No preference file: the whole suite competes at apt's default
      -- priority, and a newer version there wins over the distribution's.
      WholeSuite
    deriving (Eq, Show)

data AptRepository = AptRepository
    { repoName :: Text
    -- ^ Stem of every file the node writes (@\<name\>.sources@, the key, the
    -- preference file); also the node's identity.
    , repoUris :: Text
    , repoSuite :: Suite
    , repoComponents :: [Text]
    , repoKeyFile :: FilePath
    -- ^ The pre-provisioned key (armored @.asc@ or binary @.gpg@; the
    -- extension is kept on the installed copy, since apt tells them apart
    -- by it).
    , repoKeyFingerprint :: Text
    -- ^ Full primary-key fingerprint, spaces and case ignored.
    , repoPin :: Pinning
    , repoAptDir :: FilePath
    -- ^ @/etc/apt@; a field so that a test can aim the node at a temporary
    -- directory.
    , repoListsDir :: FilePath
    -- ^ @/var/lib/apt/lists@, where the refreshed index shows up.
    }
    deriving (Eq, Show)

-- | The PostgreSQL project's repository (@apt.postgresql.org@), pinned to
-- @postgresql-*-pgvector@ only. Override 'repoPin' for other packages. The
-- fingerprint is the caller's to declare; see the module header for why it
-- is not baked in.
pgdg :: FilePath -> Text -> AptRepository
pgdg keyFile fingerprint =
    AptRepository
        { repoName = "pgdg"
        , repoUris = "https://apt.postgresql.org/pub/repos/apt"
        , repoSuite = CodenameSuffixed "-pgdg"
        , repoComponents = ["main"]
        , repoKeyFile = keyFile
        , repoKeyFingerprint = fingerprint
        , repoPin = OnlyPackages ("postgresql-*-pgvector" NEList.:| [])
        , repoAptDir = "/etc/apt"
        , repoListsDir = "/var/lib/apt/lists"
        }

-- | The value to pass where a recipe takes @Track' AptRepository@ and its
-- author decided to take the risk of the external repository.
aptRepositoryTrack :: Track' AptRepository
aptRepositoryTrack = Track aptRepository

-- | For a builtin that takes its package source as a @'Track'' ()@: provision
-- this repository first. Its counterpart for \"the package is already
-- installable\" is 'Salmon.Builtin.Extension.ignoreTrack'.
viaRepository :: AptRepository -> Track' ()
viaRepository = Track . const . aptRepository

-- | Thrown before a key is installed when none of its primary-key
-- fingerprints is the declared one.
data KeyFingerprintMismatch = KeyFingerprintMismatch
    { mismatchFile :: FilePath
    , mismatchDeclared :: Text
    , mismatchFound :: [Text]
    }

instance Show KeyFingerprintMismatch where
    show e =
        "key fingerprint mismatch for "
            <> e.mismatchFile
            <> ": declared "
            <> Text.unpack e.mismatchDeclared
            <> ", file has "
            <> (if null e.mismatchFound then "no primary key" else Text.unpack (Text.intercalate ", " e.mismatchFound))

instance Exception KeyFingerprintMismatch

-- | Key, then sources file (and preference file), then the index refresh.
-- The node returned is the refresh: depending on it is depending on the
-- repository being usable.
aptRepository :: AptRepository -> Op
aptRepository repo =
    op "apt-repository" (deps [sourcesNode, preferencesNode]) $ \actions ->
        actions
            { help = "refreshes the apt index for " <> repo.repoName
            , notes =
                [ "repository: " <> repo.repoUris
                , "key fingerprint pinned: " <> normalizeFingerprint repo.repoKeyFingerprint
                , case repo.repoPin of
                    OnlyPackages ps -> "only packages: " <> Text.unwords (toList ps)
                    WholeSuite -> "whole suite (no pin)"
                ]
            , ref = mkRef "apt-repository-index" repo.repoName
            , check = checkIndexFresh repo
            , up = runAptUpdate
            , down = pure ()
            }
  where
    keyNode = keyOp repo
    sourcesNode = sourcesOp repo keyNode
    preferencesNode = preferencesOp repo

keyOp :: AptRepository -> Op
keyOp repo =
    op "apt-repository-key" nodeps $ \actions ->
        actions
            { help = "installs the signing key for " <> repo.repoName
            , notes = ["pinned fingerprint: " <> normalizeFingerprint repo.repoKeyFingerprint]
            , ref = mkRef "apt-repository-key" dest
            , check = checkKey
            , up = do
                verifyKeyFingerprint repo.repoKeyFile repo.repoKeyFingerprint
                key <- ByteString.readFile repo.repoKeyFile
                createDirectoryIfMissing True (takeDirectory dest)
                ByteString.writeFile dest key
            , down = removeFileIfPresent dest
            }
  where
    dest = keyDestination repo

    checkKey :: IO CheckResult
    checkKey = do
        installed <- doesFileExist dest
        if not installed
            then pure (Failure ("missing: " <> Text.pack dest))
            else do
                a <- ByteString.readFile repo.repoKeyFile
                b <- ByteString.readFile dest
                pure $
                    if a == b
                        then Success
                        else Failure ("contents differ: " <> Text.pack dest)

sourcesOp :: AptRepository -> Op -> Op
sourcesOp repo keyNode =
    op "apt-repository-sources" (deps [keyNode]) $ \actions ->
        actions
            { help = "writes " <> Text.pack path
            , notes = ["suite is derived from /etc/os-release when this runs"]
            , ref = mkRef "apt-repository-sources" path
            , check = checkFileContents fc
            , up = do
                bytes <- rendered
                createDirectoryIfMissing True (takeDirectory path)
                ByteString.writeFile path bytes
            , down = removeFileIfPresent path
            }
  where
    path = sourcesPath repo
    rendered :: IO ByteString.ByteString
    rendered = do
        suite <- resolveSuite repo.repoSuite <$> readCodename
        pure (Text.encodeUtf8 (renderSources repo suite))
    fc :: FileContents (IO ByteString.ByteString)
    fc = FileContents path rendered

preferencesOp :: AptRepository -> Op
preferencesOp repo = case repo.repoPin of
    WholeSuite -> realNoop
    OnlyPackages pats ->
        op "apt-repository-preferences" nodeps $ \actions ->
            actions
                { help = "writes " <> Text.pack path
                , ref = mkRef "apt-repository-preferences" path
                , check = checkFileContents (FileContents path body)
                , up = do
                    createDirectoryIfMissing True (takeDirectory path)
                    ByteString.writeFile path body
                , down = removeFileIfPresent path
                }
      where
        body = Text.encodeUtf8 (renderPreferences repo pats)
  where
    path = preferencesPath repo

-------------------------------------------------------------------------------

sourcesPath, preferencesPath, keyDestination :: AptRepository -> FilePath
sourcesPath repo = repo.repoAptDir </> "sources.list.d" </> Text.unpack repo.repoName <> ".sources"
preferencesPath repo = repo.repoAptDir </> "preferences.d" </> Text.unpack repo.repoName <> ".pref"
keyDestination repo =
    repo.repoAptDir </> "keyrings" </> Text.unpack repo.repoName <> ext
  where
    ext = case takeExtension repo.repoKeyFile of
        "" -> ".gpg"
        e -> e

-- | The deb822 stanza, for a suite already resolved.
renderSources :: AptRepository -> Text -> Text
renderSources repo suite =
    Text.unlines
        [ "Types: deb"
        , "URIs: " <> repo.repoUris
        , "Suites: " <> suite
        , "Components: " <> Text.unwords repo.repoComponents
        , "Signed-By: " <> Text.pack (keyDestination repo)
        ]

-- | Priority 1 for the whole origin, then 500 for each named pattern.
renderPreferences :: AptRepository -> NEList.NonEmpty Text -> Text
renderPreferences repo pats =
    Text.intercalate "\n" (stanza "*" 1 : [stanza p 500 | p <- toList pats])
  where
    stanza pat prio =
        Text.unlines
            [ "Package: " <> pat
            , "Pin: origin " <> repositoryHost repo.repoUris
            , "Pin-Priority: " <> Text.pack (show (prio :: Int))
            ]

resolveSuite :: Suite -> Text -> Text
resolveSuite (FixedSuite s) _ = s
resolveSuite (CodenameSuffixed suffix) codename = codename <> suffix

-- | @VERSION_CODENAME@ from an @os-release@ file's text, unquoted.
parseOsReleaseCodename :: Text -> Maybe Text
parseOsReleaseCodename contents =
    case [Text.strip v | l <- Text.lines contents, Just v <- [Text.stripPrefix "VERSION_CODENAME=" (Text.strip l)]] of
        (v : _) | not (Text.null (unquote v)) -> Just (unquote v)
        _ -> Nothing
  where
    unquote = Text.dropAround (`elem` ['"', '\''])

readCodename :: IO Text
readCodename = do
    contents <- Text.decodeUtf8 <$> ByteString.readFile "/etc/os-release"
    maybe (ioError (userError "apt repository: no VERSION_CODENAME in /etc/os-release")) pure (parseOsReleaseCodename contents)

-- | The host part of a URI: what a @Pin: origin@ line matches.
repositoryHost :: Text -> Text
repositoryHost uri = Text.takeWhile (/= '/') (afterScheme uri)

afterScheme :: Text -> Text
afterScheme uri = case Text.breakOn "://" uri of
    (_, rest) | not (Text.null rest) -> Text.drop 3 rest
    _ -> uri

-- | The prefix apt gives the index files it downloads for a URI under
-- @lists/@: the URI without its scheme, slashes turned to underscores.
listsPrefix :: Text -> Text
listsPrefix = Text.map (\c -> if c == '/' then '_' else c) . Text.dropWhileEnd (== '/') . afterScheme

-------------------------------------------------------------------------------

-- | Upper-case, no spaces.
normalizeFingerprint :: Text -> Text
normalizeFingerprint = Text.toUpper . Text.filter (\c -> c /= ' ' && c /= '\t')

{- | The fingerprints of the /primary/ keys in @gpg --with-colons@ output:
the @fpr@ record that directly follows a @pub@ record. (A subkey's @fpr@
follows a @sub@; a user id's records follow the @fpr@.)
-}
primaryFingerprints :: Text -> [Text]
primaryFingerprints out = go (Text.splitOn ":" <$> Text.lines out)
  where
    go (("pub" : _) : rest) = case dropWhile (not . isKind ["fpr", "pub"]) rest of
        (("fpr" : fields) : rest') -> fprField fields : go rest'
        rest' -> go rest'
    go (_ : rest) = go rest
    go [] = []
    isKind ks (k : _) = k `elem` ks
    isKind _ [] = False
    -- fpr:::::::::<FINGERPRINT>:
    fprField fields = normalizeFingerprint (case drop 8 fields of (f : _) -> f; [] -> "")

verifyKeyFingerprint :: FilePath -> Text -> IO ()
verifyKeyFingerprint file wanted = do
    (code, out, err) <- readCreateProcessWithExitCode (proc "gpg" ["--show-keys", "--with-colons", "--with-fingerprint", file]) ""
    when (code /= ExitSuccess) $
        ioError (userError ("apt repository: gpg --show-keys failed on " <> file <> ": " <> Text.unpack (Text.decodeUtf8 err)))
    let fps = primaryFingerprints (Text.decodeUtf8 out)
    unless (normalizeFingerprint wanted `elem` fps) $
        throwIO (KeyFingerprintMismatch file (normalizeFingerprint wanted) fps)

-------------------------------------------------------------------------------

{- | Has the index been refreshed since the repository was last (re)declared?
The newest of the sources and preference files is the moment the
declaration last changed; an index file for this URI at least that new means
an @apt-get update@ has seen it. Nothing else can say so: apt keeps no record
of which sources it has fetched other than these files.
-}
checkIndexFresh :: AptRepository -> IO CheckResult
checkIndexFresh repo = do
    haveLists <- doesDirectoryExist repo.repoListsDir
    if not haveLists
        then pure (Failure ("no index directory: " <> Text.pack repo.repoListsDir))
        else do
            sourcesTime <- getModificationTime (sourcesPath repo)
            prefTimes <- traverse getModificationTime =<< filterExisting [preferencesPath repo]
            let declaredAt = maximum (sourcesTime : prefTimes)
            names <- listDirectory repo.repoListsDir
            let prefix = Text.unpack (listsPrefix repo.repoUris)
                ours = [repo.repoListsDir </> n | n <- names, take (length prefix) n == prefix]
            times <- traverse getModificationTime ours
            pure $
                if any (>= declaredAt) times
                    then Success
                    else Failure ("index older than the declaration of " <> repo.repoName)
  where
    filterExisting = fmap concat . traverse (\p -> (\e -> [p | e]) <$> doesFileExist p)

runAptUpdate :: IO ()
runAptUpdate = do
    (code, _out, err) <- readCreateProcessWithExitCode (proc "apt-get" ["update", "-q"]) ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> throwIO (Binary.CommandFailedSimple ("apt-get update: " <> take 500 (Text.unpack (Text.decodeUtf8 err))) n)
