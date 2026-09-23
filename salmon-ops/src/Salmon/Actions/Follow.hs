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

What is /not/ here yet: a cached last document that survives a restart
(milestone 4 of @specs/pull-mode.md@), signatures, and every registry but
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

    -- * Following
    Follow (..),
    follower,
    followerWith,
    gated,
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
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isAlphaNum)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesFileExist, getFileSize, getModificationTime)
import System.FilePath ((<.>), (</>))
import System.IO (hFlush, stdout)

import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Query as Query
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
@history@ records; anything else at the top level is ignored so a publisher
can annotate.
-}
data Document = Document
    { docId :: Text
    , docSeeds :: [Entry]
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
        Document <$> o .: "id" <*> o .: "seeds"

instance ToJSON Document where
    toJSON d = object ["salmon" .= formatVersion, "id" .= d.docId, "seeds" .= d.docSeeds]

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
handed back is not even read.
-}
directoryRegistry :: FilePath -> Registry
directoryRegistry dir = Registry (Text.pack dir) fetch
  where
    fetch lbl previous = do
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
-- following

data Follow = Follow
    { followRegistry :: Registry
    , followLabels :: [Label]
    , followSchedule :: Scheduler.Config
    -- ^ the ladder toward the registry and the window toward the loop
    }

-- | The document last applied for a label: what the next diff is against.
data Applied = Applied
    { appliedStamp :: !Stamp
    , appliedDigest :: !Digest
    , appliedId :: !Text
    , appliedSeeds :: [Entry]
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

-- | The producer, on the system clock and seeded from the wall clock (so
-- that two hosts started together draw different jitter). See 'followerWith'.
follower :: Reporter Report -> Scheduler.Poke -> Follow -> IO () -> Producer
follower r pk follow primed = Producer $ \inbox -> do
    seed <- fromIntegral . (`div` 1000) . fromEnum <$> getPOSIXTime
    produceInto (followerWith r (Scheduler.systemClock pk) (Scheduler.mkRng seed) follow primed) inbox

{- | The producer. One round runs synchronously before the given action
(meant to release the standard-input producer, see 'gated'), and whatever it
found is injected at once — no window: there is nothing to coalesce yet, and
the first convergence is meant to be as deterministic as a piped script's.
Rounds then run on the schedule ("Salmon.Actions.Follow.Scheduler"), on the
clock given: the system's, or a test's. The thread never sends an 'Eof': a
registry that goes quiet is not the loop ending.
-}
followerWith :: Reporter Report -> Scheduler.Clock -> Scheduler.Rng -> Follow -> IO () -> Producer
followerWith r clock rng follow primed = Producer $ \inbox -> do
    runReporter r (Following (registryName follow.followRegistry) follow.followLabels follow.followSchedule)
    st <- Fetcher <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef Set.empty <*> newIORef Map.empty
    _ <- round_ st
    injectPending r follow.followRegistry st inbox
    primed
    now <- Scheduler.clockNow clock
    Scheduler.run
        follow.followSchedule
        clock
        Scheduler.Hooks
            { Scheduler.hookFetch = round_ st >>= \o -> deferred st o >> pure o
            , Scheduler.hookInject = injectPending r follow.followRegistry st inbox
            , Scheduler.hookBackoff = \n us -> runReporter r (Backoff n us)
            }
        (Scheduler.start follow.followSchedule rng now)
  where
    round_ st = mconcat <$> forM follow.followLabels (fetchOne r follow.followRegistry st)
    -- the changes a scheduled round found are going to wait: say so once
    -- per label, at the round that saw them
    deferred :: Fetcher -> Scheduler.Outcome -> IO ()
    deferred st o =
        when (o == Scheduler.Changed) $ do
            fresh <- atomicModifyIORef' st.fetcherFresh (\s -> (Set.empty, s))
            seen <- readIORef st.fetcherSeen
            forM_ (Set.toList fresh) $ \lbl ->
                forM_ (Map.lookup lbl seen) $ \s -> runReporter r (Deferred lbl s.seenId s.seenDigest)

{- | A producer that does not start until the 'MVar' is filled — what puts
standard input behind the fetcher's first round. -}
gated :: MVar () -> Producer -> Producer
gated gate p = Producer $ \inbox -> do
    readMVar gate
    produceInto p inbox

-- | A document seen and not yet applied: the latest for its label.
data Seen = Seen
    { seenStamp :: !Stamp
    , seenDigest :: !Digest
    , seenId :: !Text
    , seenSeeds :: [Entry]
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
round; a label with no document is not (the registry answered). -}
fetchOne :: Reporter Report -> Registry -> Fetcher -> Label -> IO Scheduler.Outcome
fetchOne r registry st lbl = do
    applied <- Map.lookup lbl <$> readIORef st.fetcherApplied
    seen <- Map.lookup lbl <$> readIORef st.fetcherSeen
    -- what was last read, applied or not: the stamp to hand back, and the
    -- digest a re-read is compared against
    let (lastStamp, lastDigest) = case seen of
            Just s -> (Just s.seenStamp, Just s.seenDigest)
            Nothing -> (appliedStamp <$> applied, appliedDigest <$> applied)
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
                    Just s -> modifyIORef' st.fetcherSeen (Map.insert lbl s{seenStamp = stamp})
                    Nothing -> modifyIORef' st.fetcherApplied (Map.adjust (\a -> a{appliedStamp = stamp}) lbl)
                pure Scheduler.Unchanged
            | otherwise ->
                case eitherDecode bytes :: Either String Document of
                    Left err -> complain (Malformed lbl digest (Text.pack err)) >> pure Scheduler.Failed
                    Right doc -> do
                        quiet
                        modifyIORef' st.fetcherSeen (Map.insert lbl (Seen stamp digest doc.docId doc.docSeeds))
                        modifyIORef' st.fetcherFresh (Set.insert lbl)
                        pure Scheduler.Changed
  where
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
without a declaration. Nothing to inject writes nothing. -}
injectPending :: Reporter Report -> Registry -> Fetcher -> TChan Line -> IO ()
injectPending r registry st inbox = do
    seen <- atomicModifyIORef' st.fetcherSeen (\s -> (Map.empty, s))
    writeIORef st.fetcherFresh Set.empty
    unless (Map.null seen) $ do
        applied <- readIORef st.fetcherApplied
        let adopt :: Seen -> Applied
            adopt s = Applied s.seenStamp s.seenDigest s.seenId s.seenSeeds
            -- what every label is about to say: the union the diff is against
            upcoming = Map.union (fmap adopt seen) applied
        let perLabel :: (Label, Seen) -> (Report, [(Origin, ServeCommand)])
            perLabel (lbl, s) =
                let previous = Map.lookup lbl applied
                    (ups, downs) = diffBatch lbl previous s.seenSeeds upcoming
                    origin =
                        Fetched
                            Provenance
                                { provRegistry = registryName registry
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
