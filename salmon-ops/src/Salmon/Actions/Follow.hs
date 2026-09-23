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

What is /not/ here yet: a scheduler (backoff toward the registry, debounce
toward the loop, a @fetch@ command — milestone 3 of @specs/pull-mode.md@), a
cached last document that survives a restart (milestone 4), signatures, and
every registry but the directory. Rounds run on a fixed interval, plus one
synchronous round at startup so that the first convergence is as
deterministic as a piped script's.
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
    gated,
    Applied (..),
    diffBatch,

    -- * Reporting
    Report (..),
    reportText,
    renderReport,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, unless)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist, getFileSize, getModificationTime)
import System.FilePath ((<.>), (</>))
import System.IO (hFlush, stdout)

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
    , followInterval :: Int
    -- ^ microseconds between one round's end and the next one's start
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
    = -- | registry, labels, interval in microseconds
      Following !Text ![Label] !Int
    | -- | a changed document was injected: label, id, digest, seeds up, seeds down
      Injected !Label !Text !Digest !Int !Int
    | -- | a changed document whose seed set is the one already applied
      -- (its id or an annotation changed); recorded, nothing injected
      NoDiff !Label !Text !Digest
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
        Following reg lbls us ->
            [ "follow: "
                <> reg
                <> " for "
                <> Text.intercalate ", " (fmap labelText lbls)
                <> " every "
                <> Text.pack (show (us `div` 1000000))
                <> "s"
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
        Missing lbl -> ["follow: no document for " <> labelText lbl]
        Vanished lbl -> ["follow: document for " <> labelText lbl <> " is gone; its last declarations stay in force"]
        Malformed lbl dg err -> ("follow: cannot read document for " <> labelText lbl <> " (sha256=" <> Text.take 12 dg.unDigest <> "):") : Text.lines err
        FetchFailed lbl err -> ["follow: fetching " <> labelText lbl <> " failed: " <> err]

{- | The diff-batch for a label whose document changed: what to declare up
(in the new document, not in the old one) and down (in the old one, not in
the new one, and not in any other label's applied document either — the
union across labels). The old document is 'Nothing' for a label seen for the
first time.
-}
diffBatch :: Label -> Maybe Applied -> [Entry] -> Map Label Applied -> ([Entry], [Entry])
diffBatch lbl previous new others =
    (ups, downs)
  where
    old = maybe [] appliedSeeds previous
    elsewhere = concat [a.appliedSeeds | (l, a) <- Map.toList others, l /= lbl]
    ups = [e | e <- new, e `notElem` old]
    downs = [e | e <- old, e `notElem` new, e `notElem` elsewhere]

{- | The producer. One round runs synchronously before the given action
(meant to release the standard-input producer, see 'gated'), so that
whatever the registry says at startup is in the inbox before anything else
can be; rounds then repeat every 'followInterval'. The thread never sends an
'Eof': a registry that goes quiet is not the loop ending.
-}
follower :: Reporter Report -> Follow -> IO () -> Producer
follower r follow primed = Producer $ \inbox -> do
    runReporter r (Following (registryName follow.followRegistry) follow.followLabels follow.followInterval)
    applied <- newIORef Map.empty
    noise <- newIORef Map.empty
    let round_ = forM_ follow.followLabels (fetchOne r follow.followRegistry inbox applied noise)
    round_
    primed
    forever $ do
        threadDelay follow.followInterval
        round_

{- | A producer that does not start until the 'MVar' is filled — what puts
standard input behind the fetcher's first round. -}
gated :: MVar () -> Producer -> Producer
gated gate p = Producer $ \inbox -> do
    readMVar gate
    produceInto p inbox

{- | One label's share of a round. The only paths that write to the inbox
are a document whose digest differs from the last applied one /and/ whose
seed set differs; every other outcome is a report at most — and a repeated
one (a label still missing, a file still malformed) is not even that, since
a report per round about a condition that has not changed is noise. -}
fetchOne ::
    Reporter Report ->
    Registry ->
    TChan Line ->
    IORef (Map Label Applied) ->
    -- | the last complaint per label, so an unchanged one is not repeated
    IORef (Map Label Report) ->
    Label ->
    IO ()
fetchOne r registry inbox applied noise lbl = do
    current <- readIORef applied
    let previous = Map.lookup lbl current
    outcome <- try (registryFetch registry lbl (appliedStamp <$> previous)) :: IO (Either SomeException Fetch)
    case outcome of
        Left ex -> complain (FetchFailed lbl (Text.pack (show ex)))
        Right Absent -> complain (maybe (Missing lbl) (const (Vanished lbl)) previous)
        Right Unchanged -> pure ()
        Right (Found stamp digest bytes)
            -- the mtime moved but the bytes did not: the starvation rule.
            -- Remember the new stamp so the file is not re-read every round.
            | Just prev <- previous, prev.appliedDigest == digest -> do
                writeIORef applied (Map.insert lbl prev{appliedStamp = stamp} current)
            | otherwise ->
                case eitherDecode bytes :: Either String Document of
                    Left err -> complain (Malformed lbl digest (Text.pack err))
                    Right doc -> do
                        quiet
                        let (ups, downs) = diffBatch lbl previous doc.docSeeds current
                        let now = Applied stamp digest doc.docId doc.docSeeds
                        writeIORef applied (Map.insert lbl now current)
                        if null ups && null downs
                            then runReporter r (NoDiff lbl doc.docId digest)
                            else do
                                let origin =
                                        Fetched
                                            Provenance
                                                { provRegistry = registryName registry
                                                , provLabel = labelText lbl
                                                , provDocument = doc.docId
                                                , provDigest = digest.unDigest
                                                }
                                let cmds =
                                        fmap (entryCommand Add lbl) ups
                                            ++ fmap (entryCommand Remove lbl) downs
                                atomically (writeTChan inbox (Batch origin cmds))
                                runReporter r (Injected lbl doc.docId digest (length ups) (length downs))
  where
    -- report a complaint once per change of complaint, not once per round
    complain rep = do
        last_ <- readIORef noise
        unless (Map.lookup lbl last_ == Just rep) $ do
            writeIORef noise (Map.insert lbl rep last_)
            runReporter r rep
    quiet = do
        last_ <- readIORef noise
        writeIORef noise (Map.delete lbl last_)
