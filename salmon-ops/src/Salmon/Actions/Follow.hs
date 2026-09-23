{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Pull mode for @run serve@: a 'Producer' that fetches the loop's
declarations from a registry instead of waiting to be typed at.

The unit fetched is a 'Document' — the /desired set/ of seeds for one
'Label', not a log of commands — and a host follows a list of labels, its
desired set being the union of their documents. A 'Registry' is anything that
answers "the latest document for this label"; 'directoryRegistry' is the
first one (and the test harness: write a file, watch the world change).

Three rules are load-bearing, and each is what a naive poller gets wrong.

__Change is detected before anything is injected.__ Every line that reaches
the loop's inbox stands the supervisor's machines down ("Salmon.Actions.Serve"
calls @stopTending@ before any command, by design). A poller that injected on
every round would therefore starve supervision: at a five-second interval no
machine would ever reach its check ceiling and a 'Salmon.Builtin.Extension.managed'
node's watch would be cut every tick. So a round first asks the registry
whether the document /might/ have changed (its 'Stamp': an mtime and size for
a file, an ETag or a commit id later), then compares the bytes' 'Digest'
against the one last applied, and only a document whose digest differs
produces anything at all. An unchanged round is invisible to the loop.

__What is injected is a diff, as one batch.__ Against the document last
applied for that label, not against the world: one @up@ per seed newly
present, one @down@ per seed no longer present /and not in any other
followed label's document/ — the union across labels is computed here,
because the ledger identifies a declaration by its directive and could not
tell one label's copy of a seed from another's. The batch is a single
'Serve.Batch' inbox entry: the loop runs it with @autoconverge@ held off,
restores whatever the operator had set, and converges once. Seeds an
operator typed interactively are never in a document's diff and so are left
alone — unless the operator typed the very same seed a document then drops,
which the ledger cannot tell apart (see the spec's "two operators").

__The fetcher is a named actor in @history@.__ Every declaration it makes
carries a 'Serve.Fetched' origin — registry, label, document id, digest — so
that an operator can tell "I typed this" from "the document said so", which
is the only way to find out why a host did something surprising.

__When__ a round runs, and when what it found is injected, is the
scheduler's ("Salmon.Actions.Follow.Scheduler"): a ladder with jitter toward
the registry (a failed round backs off, a successful one — changed or not —
polls at the base), and a quiet window toward the loop (a change waits for
the registry to stop changing, or for @max_wait@, and three documents seen
inside one window are one diff and one convergence pass). The first round
at startup is the exception: synchronous, injected at once, so that the
first convergence is as deterministic as a piped script's. The loop's
@fetch@ command pokes the scheduler through a 'Scheduler.Poke': a round now,
the ladder forgotten, whatever is pending injected the moment the round is
over.

__The last applied document survives a restart__ (milestone 4 of
@specs/pull-mode.md@), if a 'followCache' directory is given: after every
injection the document each label just applied is written there, bytes,
digest and id, atomically (a temp file and a rename, so a crash mid-write
leaves the previous one). At startup, a label whose registry cannot be
reached — the fetch threw, or the bytes it returned do not parse — is
applied from its cached document instead, reported 'Replayed', and the loop
is in 'Serve.Replay' mode: the world is the last thing this host knew, not
necessarily what the registry says now. The mode turns to
'Serve.Following' at the first later round in which every label answers,
changed or not. The cached document is compared by digest exactly as a
previously applied one is, so a registry that comes back with the same
document injects nothing — the starvation rule holds across restarts.
'Serve.Replay' is only ever /entered/ at startup, and the reason is what the
cache stands in for: a world, not a round. Before the first round there is
no world at all, and coming up empty would tear nothing down, look converged
and be wrong; the cache is the better answer to that. After a successful
round the world already is what the registry last said, and a round failing
later changes nothing about it — the last good document stays in force
(the spec's rule for a document that fails verification, applied here too),
and the scheduler's 'Backoff' is what says the registry is unreachable. A
cache that cannot be read is reported ('BadCache') and ignored, one that
cannot be written likewise ('CacheFailed'): the cache never takes the loop
down. Without a cache directory nothing is cached and a host that starts
against an unreachable registry declares nothing, as it did before.

__Ordered ids__, the spec's open question, are settled the way it leaned: a
document may carry a @published@ RFC 3339 timestamp at its top level, and
with 'followRefuseOlder' a fetched document published /before/ the one this
label already applied (or has pending) is reported 'Stale' and not injected
— what a registry serving from a lagging replica would otherwise do to a
host. Off by default, and without a @published@ on both sides the latest
document is whatever the registry says.

What is /not/ here yet: signatures, a status sink, and every registry but
the directory.
-}
module Salmon.Actions.Follow (
    -- * Documents
    Document (..),
    Entry (..),
    formatVersion,
    entryCommand,

    -- * Labels and registries
    Label,
    mkLabel,
    labelText,
    Stamp (..),
    Digest (..),
    Fetch (..),
    Registry (..),
    directoryRegistry,
    documentPath,
    digestOf,

    -- * The cache
    Cached (..),
    cachePath,
    readCache,
    writeCache,

    -- * Following
    Follow (..),
    follower,
    followerWith,
    gated,
    newMode,
    followed,
    Applied (..),
    diffBatch,

    -- * Reporting
    Report (..),
    reportText,
    renderReport,
) where

import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isAlphaNum)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, renameFile)
import System.FilePath ((<.>), (</>))
import System.IO (hFlush, stdout)

import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Query as Query
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Declaration (..), Line (..), Origin (..), Producer (..), Provenance (..), ServeCommand (..))
import Salmon.Reporter

-------------------------------------------------------------------------------
-- documents

{- | One seed of a document: either the words that would follow @config@ on
the command line (what @up@ takes), or a directive's JSON (what
@up-directive@ takes from a file). Two entries are the same seed iff they are
equal here, spelling included — the ledger's finer notion (equal directives)
is applied once the loop configures them.
-}
data Entry
    = -- | @{"seed": ["app", "--version", "42"]}@
      SeedWords [String]
    | -- | @{"directive": {...}}@
      SeedDirective Value
    deriving (Show, Eq)

instance FromJSON Entry where
    parseJSON = withObject "seed entry" $ \o -> do
        ws <- o .:? "seed"
        dv <- o .:? "directive"
        case (ws, dv) of
            (Just w, Nothing) -> pure (SeedWords w)
            (Nothing, Just d) -> pure (SeedDirective d)
            (Nothing, Nothing) -> fail "a seed entry needs a `seed` (words) or a `directive` (JSON)"
            (Just _, Just _) -> fail "a seed entry has either a `seed` or a `directive`, not both"

instance ToJSON Entry where
    toJSON (SeedWords ws) = object ["seed" .= ws]
    toJSON (SeedDirective d) = object ["directive" .= d]

{- | The fetched thing. @salmon@ is 'formatVersion' and must be exactly that;
@id@ is opaque, the publisher's own name for this revision, and is what
@history@ records; @published@ is optional, an RFC 3339 timestamp, and is
only ever compared under 'followRefuseOlder'; anything else at the top level
is ignored so a publisher can annotate.
-}
data Document = Document
    { docId :: Text
    , docSeeds :: [Entry]
    , docPublished :: Maybe UTCTime
    }
    deriving (Show, Eq)

-- | The one value of @salmon@ this reader understands.
formatVersion :: Int
formatVersion = 1

instance FromJSON Document where
    parseJSON = withObject "salmon document" $ \o -> do
        v <- o .: "salmon"
        unless (v == formatVersion) $
            fail ("unsupported document format: salmon=" <> show v <> " (this reader understands " <> show formatVersion <> ")")
        Document <$> o .: "id" <*> o .: "seeds" <*> o .:? "published"

instance ToJSON Document where
    toJSON d =
        object $
            ["salmon" .= formatVersion, "id" .= d.docId, "seeds" .= d.docSeeds]
                ++ ["published" .= p | p <- maybeToList d.docPublished]

-- | The loop command that declares an entry in the given direction.
entryCommand :: Declaration -> Label -> Entry -> ServeCommand
entryCommand decl _ (SeedWords ws) = Declare decl ws
entryCommand decl lbl (SeedDirective v) = DeclareInline decl (labelText lbl) v

-------------------------------------------------------------------------------
-- labels and registries

{- | An address into a registry: "the latest document for @web-api@". A
label is spliced into a path, a URL or a DNS name by the registry backend, so
its alphabet is restricted to what every backend can carry safely — letters,
digits, @.@, @_@, @-@, @\@@ — and it may not start with a dot, which for the
directory backend is what keeps @..@ from escaping the directory.
-}
newtype Label = Label Text
    deriving (Show, Eq, Ord)

mkLabel :: Text -> Either Text Label
mkLabel t
    | Text.null t = Left "a label cannot be empty"
    | Text.isPrefixOf "." t = Left ("a label cannot start with a dot: " <> t)
    | Text.all allowed t = Right (Label t)
    | otherwise = Left ("a label may only contain letters, digits, `.`, `_`, `-` and `@`: " <> t)
  where
    allowed c = isAlphaNum c || c `elem` ("._-@" :: String)

labelText :: Label -> Text
labelText (Label t) = t

{- | The registry's own cheap "has it moved?" token for a document: an
mtime and size for a file, later an ETag, a commit id or an object
generation. Opaque to the fetcher, which only ever hands the last one back
and compares digests when the registry says it may have changed. A stamp is
a shortcut, not the decision: two writes inside one timestamp tick are why
the digest is compared as well.
-}
newtype Stamp = Stamp Text
    deriving (Show, Eq)

-- | Hex-encoded SHA-256 of a document's bytes, exactly as fetched.
newtype Digest = Digest {unDigest :: Text}
    deriving (Show, Eq)

digestOf :: ByteString -> Digest
digestOf = Digest . Query.digestBytes

-- | What a registry answers when asked for a label, given the stamp of what
-- the fetcher last saw for it.
data Fetch
    = -- | no document for this label
      Absent
    | -- | the stamp still matches: nothing read, nothing to compare
      Unchanged
    | -- | the bytes, their digest, and the stamp to hand back next time
      Found !Stamp !Digest !ByteString
    deriving (Show, Eq)

{- | Anything that answers "latest document for @label@". The backend owns
the template that turns a label into an address, and the meaning of its
'Stamp'.
-}
data Registry = Registry
    { registryName :: Text
    -- ^ how @history@ and reports name it: a directory path, a URL, a repo
    , registryFetch :: Label -> Maybe Stamp -> IO Fetch
    -- ^ may throw; the fetcher contains that and reports it
    }

-- | Where 'directoryRegistry' looks for a label: @\<dir\>/\<label\>.json@.
documentPath :: FilePath -> Label -> FilePath
documentPath dir (Label t) = dir </> Text.unpack t <.> "json"

{- | A directory with one file per label, named by 'documentPath'. Its stamp
is the file's modification time and size; a file whose stamp matches the one
handed back is not even read. The directory itself missing is the registry
being unreachable — a throw, the same as a host a URL points at not
answering — and not a label with no document: the difference is what
decides whether a cached document is replayed at startup.
-}
directoryRegistry :: FilePath -> Registry
directoryRegistry dir = Registry (Text.pack dir) fetch
  where
    fetch lbl previous = do
        there <- doesDirectoryExist dir
        unless there $
            ioError (userError ("registry directory does not exist: " <> dir))
        let path = documentPath dir lbl
        present <- doesFileExist path
        if not present
            then pure Absent
            else do
                mtime <- getModificationTime path
                size <- getFileSize path
                let stamp = Stamp (Text.pack (show mtime <> " " <> show size))
                if Just stamp == previous
                    then pure Unchanged
                    else do
                        bytes <- LByteString.readFile path
                        -- forced now: a lazy read holds the handle open, and a
                        -- rewrite under it is precisely the case this is for.
                        let digest = digestOf bytes
                        LByteString.length bytes `seq` pure (Found stamp digest bytes)

-------------------------------------------------------------------------------
-- the cache

{- | What the cache keeps per label: the applied document's bytes exactly as
fetched (so that the digest, kept beside them, is the one a later fetch is
compared against), and its id for the report that replays it. On disk as
@{"salmon-cache": 1, "id": ..., "sha256": ..., "document": ...}@, the bytes
as one JSON string.
-}
data Cached = Cached
    { cachedId :: !Text
    , cachedDigest :: !Digest
    , cachedBytes :: !ByteString
    }
    deriving (Show, Eq)

cacheFormatVersion :: Int
cacheFormatVersion = 1

instance FromJSON Cached where
    parseJSON = withObject "salmon cache entry" $ \o -> do
        v <- o .: "salmon-cache"
        unless (v == cacheFormatVersion) $
            fail ("unsupported cache format: salmon-cache=" <> show v)
        Cached
            <$> o .: "id"
            <*> (Digest <$> o .: "sha256")
            <*> (LByteString.fromStrict . Text.encodeUtf8 <$> o .: "document")

instance ToJSON Cached where
    toJSON c =
        object
            [ "salmon-cache" .= cacheFormatVersion
            , "id" .= c.cachedId
            , "sha256" .= c.cachedDigest.unDigest
            , "document" .= Text.decodeUtf8Lenient (LByteString.toStrict c.cachedBytes)
            ]

{- | The cache's file for a label: @\<dir\>/\<label\>.applied.json@. Not
'documentPath''s name, so that a cache directory pointed at a registry
directory by mistake overwrites nothing the registry serves.
-}
cachePath :: FilePath -> Label -> FilePath
cachePath dir (Label t) = dir </> Text.unpack t <.> "applied" <.> "json"

{- | The cached document for a label, parsed both as a cache entry and as
the 'Document' it holds, its digest checked against the bytes: 'Nothing'
when there is none, a reason when there is one that cannot be used. Never
throws.
-}
readCache :: FilePath -> Label -> IO (Either Text (Maybe (Cached, Document)))
readCache dir lbl = do
    let path = cachePath dir lbl
    present <- doesFileExist path
    if not present
        then pure (Right Nothing)
        else do
            attempt <- try (LByteString.readFile path >>= \b -> LByteString.length b `seq` pure b)
            pure $ case attempt of
                Left (ex :: SomeException) -> Left (Text.pack (show ex))
                Right bytes -> case eitherDecode bytes of
                    Left err -> Left (Text.pack err)
                    Right c
                        | digestOf c.cachedBytes /= c.cachedDigest -> Left "the cached bytes do not hash to the digest kept beside them"
                        | otherwise -> case eitherDecode c.cachedBytes of
                            Left err -> Left ("the cached document does not parse: " <> Text.pack err)
                            Right doc -> Right (Just (c, doc))

{- | Write a label's cache entry: to a temporary file beside it, then
renamed over it, so that a crash mid-write leaves the previous entry rather
than half of this one. May throw; the caller reports and moves on.
-}
writeCache :: FilePath -> Label -> Cached -> IO ()
writeCache dir lbl c = do
    createDirectoryIfMissing True dir
    let path = cachePath dir lbl
        tmp = path <.> "tmp"
    LByteString.writeFile tmp (encode c)
    renameFile tmp path

-------------------------------------------------------------------------------
-- following

data Follow = Follow
    { followRegistry :: Registry
    , followLabels :: [Label]
    , followSchedule :: Scheduler.Config
    -- ^ the ladder toward the registry and the window toward the loop
    , followCache :: Maybe FilePath
    -- ^ where each label's last applied document is kept across restarts;
    -- 'Nothing' keeps none
    , followRefuseOlder :: Bool
    -- ^ refuse a document whose @published@ is older than the one already
    -- applied or pending for its label
    }

{- | The document last applied for a label: what the next diff is against.
The stamp is 'Nothing' for a document replayed from the cache — a stamp
belongs to the registry, and the cache is not it — so the registry reads
the file once and hands the stamp back from then on. -}
data Applied = Applied
    { appliedStamp :: !(Maybe Stamp)
    , appliedDigest :: !Digest
    , appliedId :: !Text
    , appliedSeeds :: [Entry]
    , appliedPublished :: !(Maybe UTCTime)
    }
    deriving (Show, Eq)

data Report
    = -- | registry, labels, schedule
      Following !Text ![Label] !Scheduler.Config
    | -- | a changed document was injected: label, id, digest, seeds up, seeds down
      Injected !Label !Text !Digest !Int !Int
    | -- | a changed document whose seed set is the one already applied
      -- (its id or an annotation changed); recorded, nothing injected
      NoDiff !Label !Text !Digest
    | -- | a changed document, seen after startup: it waits for the quiet
      -- window (label, id, digest); what it turns into is the 'Injected'
      -- or 'NoDiff' that follows
      Deferred !Label !Text !Digest
    | -- | a round failed: consecutive failures, microseconds until the next round
      Backoff !Int !Int
    | -- | no document for this label
      Missing !Label
    | -- | the document was applied once and is now gone; what it declared
      -- stays in force
      Vanished !Label
    | -- | the bytes did not parse as a document: label, digest, why
      Malformed !Label !Digest !Text
    | -- | asking the registry threw
      FetchFailed !Label !Text
    | -- | the registry could not be read at startup and the cached
      -- document was applied instead: label, id, digest. The 'Injected'
      -- that follows is its declarations.
      Replayed !Label !Text !Digest
    | -- | under 'followRefuseOlder', a document published before the one
      -- already applied or pending for its label: label, id. Not injected.
      Stale !Label !Text
    | -- | a cache entry that cannot be used: label, why. Ignored.
      BadCache !Label !Text
    | -- | writing a label's cache entry threw: label, why. The document
      -- was injected all the same; only the next restart is affected.
      CacheFailed !Label !Text
    deriving (Show, Eq)

-- | One write per report, for the same reason as 'Serve.reportText': the
-- loop's own reporter writes to this handle from another thread.
reportText :: Reporter Report
reportText = ReporterM $ \rep -> do
    Text.putStr (Text.unlines (renderReport rep))
    hFlush stdout

renderReport :: Report -> [Text]
renderReport rep =
    case rep of
        Following reg lbls cfg ->
            [ "follow: "
                <> reg
                <> " for "
                <> Text.intercalate ", " (fmap labelText lbls)
                <> " "
                <> Scheduler.renderConfig cfg
            ]
        Injected lbl did dg nup ndown ->
            [ "follow: "
                <> labelText lbl
                <> " id="
                <> did
                <> " sha256="
                <> Text.take 12 dg.unDigest
                <> ": "
                <> Text.pack (show nup)
                <> " seed(s) up, "
                <> Text.pack (show ndown)
                <> " down"
            ]
        NoDiff lbl did dg ->
            ["follow: " <> labelText lbl <> " id=" <> did <> " sha256=" <> Text.take 12 dg.unDigest <> ": same seeds as before, nothing to declare"]
        Deferred lbl did dg ->
            ["follow: " <> labelText lbl <> " id=" <> did <> " sha256=" <> Text.take 12 dg.unDigest <> ": changed, waiting for the registry to go quiet"]
        Backoff n us ->
            ["follow: " <> Text.pack (show n) <> " failed round(s) in a row; next in " <> Text.pack (show (us `div` 1000000)) <> "s"]
        Missing lbl -> ["follow: no document for " <> labelText lbl]
        Vanished lbl -> ["follow: document for " <> labelText lbl <> " is gone; its last declarations stay in force"]
        Malformed lbl dg err -> ("follow: cannot read document for " <> labelText lbl <> " (sha256=" <> Text.take 12 dg.unDigest <> "):") : Text.lines err
        FetchFailed lbl err -> ["follow: fetching " <> labelText lbl <> " failed: " <> err]
        Replayed lbl did dg ->
            ["follow: " <> labelText lbl <> ": registry unreachable; replaying the cached document id=" <> did <> " sha256=" <> Text.take 12 dg.unDigest]
        Stale lbl did ->
            ["follow: " <> labelText lbl <> " id=" <> did <> ": published before the document already applied; refused (--follow-refuse-older)"]
        BadCache lbl err -> ("follow: ignoring the cached document for " <> labelText lbl <> ":") : Text.lines err
        CacheFailed lbl err -> ("follow: could not cache the document for " <> labelText lbl <> ":") : Text.lines err

{- | The diff-batch for a label whose document changed: what to declare up
(in the new document, not in the old one) and down (in the old one, not in
the new one, and not in any other label's document either — the union
across labels, taken over what the other labels are /about to/ say when
several are injected together). The old document is 'Nothing' for a label
seen for the first time.
-}
diffBatch :: Label -> Maybe Applied -> [Entry] -> Map Label Applied -> ([Entry], [Entry])
diffBatch lbl previous new others =
    (ups, downs)
  where
    old = maybe [] appliedSeeds previous
    elsewhere = concat [a.appliedSeeds | (l, a) <- Map.toList others, l /= lbl]
    ups = [e | e <- new, e `notElem` old]
    downs = [e | e <- old, e `notElem` new, e `notElem` elsewhere]

{- | The cell the fetcher keeps its 'Serve.Mode' in, made by the caller so
that the loop can be handed a reader of it ('followed') before the producer
runs. Starts 'Serve.Following'; only the startup round can turn it to
'Serve.Replay'. -}
newMode :: IO (IORef Serve.Mode)
newMode = newIORef Serve.Following

-- | The loop's side: what @fetch@ pokes and what @status@ reads.
followed :: Scheduler.Poke -> IORef Serve.Mode -> Serve.Followed
followed pk mode = Serve.Followed{Serve.followedFetch = Scheduler.poke pk, Serve.followedMode = readIORef mode}

-- | The producer, on the system clock and seeded from the wall clock (so
-- that two hosts started together draw different jitter). See 'followerWith'.
follower :: Reporter Report -> Scheduler.Poke -> IORef Serve.Mode -> Follow -> IO () -> Producer
follower r pk mode follow primed = Producer $ \inbox -> do
    seed <- fromIntegral . (`div` 1000) . fromEnum <$> getPOSIXTime
    produceInto (followerWith r (Scheduler.systemClock pk) (Scheduler.mkRng seed) mode follow primed) inbox

{- | The producer. One round runs synchronously before the given action
(meant to release the standard-input producer, see 'gated'), and whatever it
found is injected at once — no window: there is nothing to coalesce yet, and
the first convergence is meant to be as deterministic as a piped script's.
A label that round could not read is replayed from the cache, if there is
one for it (see the module's summary for why only this round is). Rounds
then run on the schedule ("Salmon.Actions.Follow.Scheduler"), on the clock
given: the system's, or a test's. The thread never sends an 'Eof': a
registry that goes quiet is not the loop ending.
-}
followerWith :: Reporter Report -> Scheduler.Clock -> Scheduler.Rng -> IORef Serve.Mode -> Follow -> IO () -> Producer
followerWith r clock rng mode follow primed = Producer $ \inbox -> do
    runReporter r (Following (registryName follow.followRegistry) follow.followLabels follow.followSchedule)
    st <- Fetcher <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef Set.empty <*> newIORef Map.empty
    cached <- loadCache
    replayed <- forM follow.followLabels $ \lbl -> do
        outcome <- fetchOne r follow st lbl
        case (outcome, Map.lookup lbl cached) of
            (Scheduler.Failed, Just (c, doc)) -> do
                modifyIORef' st.fetcherSeen (Map.insert lbl (Seen Nothing c.cachedDigest doc.docId doc.docSeeds doc.docPublished c.cachedBytes))
                runReporter r (Replayed lbl doc.docId c.cachedDigest)
                pure True
            _ -> pure False
    when (or replayed) $ writeIORef mode Serve.Replay
    injectPending r follow st inbox
    primed
    now <- Scheduler.clockNow clock
    Scheduler.run
        follow.followSchedule
        clock
        Scheduler.Hooks
            { Scheduler.hookFetch = round_ st >>= \o -> deferred st o >> answered o >> pure o
            , Scheduler.hookInject = injectPending r follow st inbox
            , Scheduler.hookBackoff = \n us -> runReporter r (Backoff n us)
            }
        (Scheduler.start follow.followSchedule rng now)
  where
    round_ st = mconcat <$> forM follow.followLabels (fetchOne r follow st)
    -- the changes a scheduled round found are going to wait: say so once
    -- per label, at the round that saw them
    deferred :: Fetcher -> Scheduler.Outcome -> IO ()
    deferred st o =
        when (o == Scheduler.Changed) $ do
            fresh <- atomicModifyIORef' st.fetcherFresh (\s -> (Set.empty, s))
            seen <- readIORef st.fetcherSeen
            forM_ (Set.toList fresh) $ \lbl ->
                forM_ (Map.lookup lbl seen) $ \s -> runReporter r (Deferred lbl s.seenId s.seenDigest)
    -- a round in which every label answered ends a replay: from here on
    -- the world is what the registry says
    answered :: Scheduler.Outcome -> IO ()
    answered o = unless (o == Scheduler.Failed) $ writeIORef mode Serve.Following
    -- every label's cache entry, read once; a bad one is reported and left out
    loadCache :: IO (Map Label (Cached, Document))
    loadCache = case follow.followCache of
        Nothing -> pure Map.empty
        Just dir ->
            fmap (Map.fromList . catMaybes) $
                forM follow.followLabels $ \lbl -> do
                    entry <- readCache dir lbl
                    case entry of
                        Left err -> runReporter r (BadCache lbl err) >> pure Nothing
                        Right found -> pure ((,) lbl <$> found)

{- | A producer that does not start until the 'MVar' is filled — what puts
standard input behind the fetcher's first round. -}
gated :: MVar () -> Producer -> Producer
gated gate p = Producer $ \inbox -> do
    readMVar gate
    produceInto p inbox

-- | A document seen and not yet applied: the latest for its label. The
-- stamp is 'Nothing' for one replayed from the cache, and the bytes are
-- kept so the cache can be written once it is applied.
data Seen = Seen
    { seenStamp :: !(Maybe Stamp)
    , seenDigest :: !Digest
    , seenId :: !Text
    , seenSeeds :: [Entry]
    , seenPublished :: !(Maybe UTCTime)
    , seenBytes :: !ByteString
    }

-- | What a fetcher carries between rounds.
data Fetcher = Fetcher
    { fetcherApplied :: IORef (Map Label Applied)
    -- ^ per label, the document the loop last heard about
    , fetcherSeen :: IORef (Map Label Seen)
    -- ^ per label, a newer document waiting for its window
    , fetcherFresh :: IORef (Set Label)
    -- ^ labels whose 'Seen' changed since last reported
    , fetcherNoise :: IORef (Map Label Report)
    -- ^ the last complaint per label, so an unchanged one is not repeated
    }

-- | The outcome of a round is the worst of its labels'.
instance Semigroup Scheduler.Outcome where
    Scheduler.Failed <> _ = Scheduler.Failed
    _ <> Scheduler.Failed = Scheduler.Failed
    Scheduler.Changed <> _ = Scheduler.Changed
    _ <> Scheduler.Changed = Scheduler.Changed
    Scheduler.Unchanged <> Scheduler.Unchanged = Scheduler.Unchanged

instance Monoid Scheduler.Outcome where
    mempty = Scheduler.Unchanged

{- | One label's share of a round. Nothing here writes to the inbox: a
document whose digest differs from the last one seen is parsed and set
aside as this label's 'Seen', to be diffed and injected by 'injectPending'
when the scheduler says so. Every other outcome is a report at most — and a
repeated one (a label still missing, a file still malformed) is not even
that, since a report per round about a condition that has not changed is
noise. A registry that throws, or bytes that do not parse, is a 'Failed'
round; a label with no document is not (the registry answered), and neither
is a document refused for being 'Stale'. -}
fetchOne :: Reporter Report -> Follow -> Fetcher -> Label -> IO Scheduler.Outcome
fetchOne r follow st lbl = do
    applied <- Map.lookup lbl <$> readIORef st.fetcherApplied
    seen <- Map.lookup lbl <$> readIORef st.fetcherSeen
    -- what was last read, applied or not: the stamp to hand back, the
    -- digest a re-read is compared against, and the publication time a
    -- refused document is older than
    let (lastStamp, lastDigest, lastPublished) = case seen of
            Just s -> (s.seenStamp, Just s.seenDigest, s.seenPublished)
            Nothing -> (appliedStamp =<< applied, appliedDigest <$> applied, appliedPublished =<< applied)
    outcome <- try (registryFetch registry lbl lastStamp) :: IO (Either SomeException Fetch)
    case outcome of
        Left ex -> complain (FetchFailed lbl (Text.pack (show ex))) >> pure Scheduler.Failed
        Right Absent -> complain (maybe (Missing lbl) (const (Vanished lbl)) applied) >> pure Scheduler.Unchanged
        Right Unchanged -> pure Scheduler.Unchanged
        Right (Found stamp digest bytes)
            -- the mtime moved but the bytes did not: the starvation rule.
            -- Remember the new stamp so the file is not re-read every round.
            | Just digest == lastDigest -> do
                case seen of
                    Just s -> modifyIORef' st.fetcherSeen (Map.insert lbl s{seenStamp = Just stamp})
                    Nothing -> modifyIORef' st.fetcherApplied (Map.adjust (\a -> a{appliedStamp = Just stamp}) lbl)
                pure Scheduler.Unchanged
            | otherwise ->
                case eitherDecode bytes :: Either String Document of
                    Left err -> complain (Malformed lbl digest (Text.pack err)) >> pure Scheduler.Failed
                    Right doc
                        | follow.followRefuseOlder
                        , Just newer <- lastPublished
                        , Just published <- doc.docPublished
                        , published < newer ->
                            complain (Stale lbl doc.docId) >> pure Scheduler.Unchanged
                        | otherwise -> do
                            quiet
                            modifyIORef' st.fetcherSeen (Map.insert lbl (Seen (Just stamp) digest doc.docId doc.docSeeds doc.docPublished bytes))
                            modifyIORef' st.fetcherFresh (Set.insert lbl)
                            pure Scheduler.Changed
  where
    registry = follow.followRegistry
    -- report a complaint once per change of complaint, not once per round
    complain rep = do
        last_ <- readIORef st.fetcherNoise
        unless (Map.lookup lbl last_ == Just rep) $ do
            writeIORef st.fetcherNoise (Map.insert lbl rep last_)
            runReporter r rep
    quiet = modifyIORef' st.fetcherNoise (Map.delete lbl)

{- | Inject everything 'Seen' as one batch: per label, the diff against the
document last applied — so three documents seen inside one window amount to
one diff, from the one the loop knows to the latest — and one 'Serve.Batch'
for all of them, each command carrying its own label's provenance. A label
whose latest document turns out to say what was already applied (written
and written back inside the window) is reported 'NoDiff' and adopted
without a declaration. Nothing to inject writes nothing. Each label's
document is then written to the cache, if there is one — after the batch
is in the inbox, since a cache that cannot be written must not hold the
injection back. -}
injectPending :: Reporter Report -> Follow -> Fetcher -> TChan Line -> IO ()
injectPending r follow st inbox = do
    seen <- atomicModifyIORef' st.fetcherSeen (\s -> (Map.empty, s))
    writeIORef st.fetcherFresh Set.empty
    unless (Map.null seen) $ do
        applied <- readIORef st.fetcherApplied
        let adopt :: Seen -> Applied
            adopt s = Applied s.seenStamp s.seenDigest s.seenId s.seenSeeds s.seenPublished
            -- what every label is about to say: the union the diff is against
            upcoming = Map.union (fmap adopt seen) applied
        let perLabel :: (Label, Seen) -> (Report, [(Origin, ServeCommand)])
            perLabel (lbl, s) =
                let previous = Map.lookup lbl applied
                    (ups, downs) = diffBatch lbl previous s.seenSeeds upcoming
                    origin =
                        Fetched
                            Provenance
                                { provRegistry = registryName follow.followRegistry
                                , provLabel = labelText lbl
                                , provDocument = s.seenId
                                , provDigest = s.seenDigest.unDigest
                                }
                 in if null ups && null downs
                        then (NoDiff lbl s.seenId s.seenDigest, [])
                        else
                            ( Injected lbl s.seenId s.seenDigest (length ups) (length downs)
                            , [(origin, cmd) | cmd <- fmap (entryCommand Add lbl) ups ++ fmap (entryCommand Remove lbl) downs]
                            )
            (reports, cmds) = fmap concat (unzip (fmap perLabel (Map.toList seen)))
        writeIORef st.fetcherApplied upcoming
        unless (null cmds) $
            atomically (writeTChan inbox (Batch cmds))
        forM_ reports (runReporter r)
        forM_ follow.followCache $ \dir ->
            forM_ (Map.toList seen) $ \(lbl, s) -> do
                written <- try (writeCache dir lbl (Cached s.seenId s.seenDigest s.seenBytes))
                case written of
                    Left (ex :: SomeException) -> runReporter r (CacheFailed lbl (Text.pack (show ex)))
                    Right () -> pure ()
