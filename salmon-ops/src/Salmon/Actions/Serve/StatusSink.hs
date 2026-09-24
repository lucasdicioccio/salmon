{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The status sink of @run serve@ (milestone 5 of @specs/pull-mode.md@): a
JSON document about this host, written to a file, for a fleet fold to read.

A host in pull mode fetches its declarations and converges on them with no
controller watching; the sink is how anything learns what came of it. It is
a file first — @--status-sink PATH@ — because a file is the dumbest store
there is and everything else (a bucket object keyed by host, an HTTP @POST@)
is the same document handed to a different writer. Whoever reads the
directory the files land in folds them ("Salmon.Actions.Fleet",
@salmon-fleet status@); no running service keeps fleet state.

Three things about how it is driven are deliberate.

__It is a reporter and a timer, not a producer.__ The sink learns that a
convergence pass ended, or that the fetcher injected a document, from the
loop's own report stream — 'sinkReporter' is composed beside the loop's
reporter with 'reportBoth' and watches for 'Serve.ConvergeStop' and
'Follow.Injected' — and it reads the world through the accessor
'Serve.serveObserved' hands its observer ('sinkObserver'): a plain read of
the loop's cell, never a seat in the inbox. So writing status never stands
the tending machines down, never waits behind a command, and never runs
@stopTending@; every arrival on the inbox still does exactly what it did.
Between triggers a tick every 'configInterval' rewrites the document with
whatever the world looks like now, which is what makes a host that has gone
quiet visible as one whose @written@ is old rather than one whose file
says everything is fine.

__Every write is atomic__: the document goes to a temporary file beside the
path and is renamed over it, so a fold that reads the directory mid-write
sees the previous document whole, never half of this one.

__A sink that cannot be written never takes the loop down.__ The failure is
reported ('Serve.SinkFailed'), once per run of failures rather than once per
attempt, and the loop keeps serving; the next write that succeeds re-arms
the report. The spec sketched the sink as an op in the host's own graph so a
failing sink would show as a @Failed@ node; it is a thread instead, because
a node is applied by a pass and the sink must write /after/ the pass, which
a node in that pass cannot do — the report is the same information, on the
same stream.

The document, @salmon-status: 1@:

> { "salmon-status": 1,
>   "host": "web-3", "written": "2026-09-24T10:41:07.12Z", "mode": "following",
>   "labels": [{"label": "web", "id": "web@42", "sha256": "…", "applied": "…"}],
>   "status": { ...the object `status --json` prints... },
>   "last": { "converge": { ...the last converge-stop object... },
>             "follow":   { ...the last follow report object... } } }

@status@ is the same object the loop's own @status@ emits under @--json@
and the HTTP @/status@ answers; @last.converge@ and @last.follow@ are the
tagged report objects exactly as @--json@ prints them (@stream@ included),
@null@ until there has been one.
-}
module Salmon.Actions.Serve.StatusSink (
    -- * Configuration
    Config (..),
    defaultInterval,
    hostName,

    -- * Running one
    Sink,
    withSink,
    sinkReporter,
    sinkObserver,
    writeNow,

    -- * The document
    Document (..),
    formatVersion,
    writeAtomically,
) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, registerDelay, retry, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory, (<.>))
import System.Posix.Unistd (getSystemID, nodeName)

import qualified Salmon.Actions.Follow as Follow
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (AppliedDocument (..), Followed (..), World (..))
import Salmon.Reporter
import Salmon.Reporter.Tagged (Tagged (..))

-------------------------------------------------------------------------------

data Config = Config
    { configPath :: FilePath
    -- ^ where the document is written; its directory is created if missing
    , configInterval :: Int
    -- ^ microseconds between two writes with no trigger in between
    , configHost :: Text
    -- ^ what @host@ says; 'hostName' for the machine's own
    }
    deriving (Show, Eq)

-- | Ten seconds, in microseconds.
defaultInterval :: Int
defaultInterval = 10 * 1000000

-- | The machine's node name (@uname -n@).
hostName :: IO Text
hostName = Text.pack . nodeName <$> getSystemID

-------------------------------------------------------------------------------

-- | The document version this writer produces and "Salmon.Actions.Fleet" reads.
formatVersion :: Int
formatVersion = 1

data Document = Document
    { docHost :: !Text
    , docWritten :: !UTCTime
    , docMode :: !Text
    -- ^ 'Serve.renderMode' of the loop's 'Serve.Mode'
    , docLabels :: [AppliedDocument]
    , docStatus :: !Value
    -- ^ the @status@ object, as @--json@ prints it
    , docLastConverge :: !(Maybe Value)
    -- ^ the last @converge-stop@ object, as @--json@ prints it
    , docLastFollow :: !(Maybe Value)
    -- ^ the last follow-stream object, as @--json@ prints it
    }
    deriving (Show, Eq)

instance ToJSON Document where
    toJSON d =
        object
            [ "salmon-status" .= formatVersion
            , "host" .= d.docHost
            , "written" .= d.docWritten
            , "mode" .= d.docMode
            , "labels" .= d.docLabels
            , "status" .= d.docStatus
            , "last" .= object ["converge" .= d.docLastConverge, "follow" .= d.docLastFollow]
            ]

instance FromJSON Document where
    parseJSON = withObject "salmon status document" $ \o -> do
        v <- o .: "salmon-status"
        unless (v == formatVersion) $
            fail ("unsupported status format: salmon-status=" <> show v <> " (this reader understands " <> show formatVersion <> ")")
        lastO <- o .:? "last"
        (lc, lf) <- case lastO of
            Nothing -> pure (Nothing, Nothing)
            Just lo -> (,) <$> lo .:? "converge" <*> lo .:? "follow"
        Document
            <$> o .: "host"
            <*> o .: "written"
            <*> o .: "mode"
            <*> o .: "labels"
            <*> o .: "status"
            <*> pure lc
            <*> pure lf

-------------------------------------------------------------------------------

-- | A running sink: what to compose beside the loop.
data Sink = Sink
    { sinkConfig :: Config
    , sinkFollowed :: Maybe Followed
    , sinkOwn :: Reporter Tagged
    -- ^ where a failure to write is reported: the loop's own reporter,
    -- not the composition that includes this sink
    , sinkStatusOf :: IORef (Maybe (IO Value))
    -- ^ installed by 'sinkObserver'; nothing is written before it is
    , sinkLast :: IORef (Maybe Value, Maybe Value)
    -- ^ last converge-stop, last follow report
    , sinkWake :: TVar Bool
    -- ^ a trigger happened: write as soon as possible
    , sinkComplained :: IORef Bool
    -- ^ the current run of failures has been reported
    }

{- | Run a sink for the body's lifetime. The writer thread is cancelled
when the body returns, mid-write or not — the temporary file is the only
casualty, never the document.
-}
withSink :: Config -> Maybe Followed -> Reporter Tagged -> (Sink -> IO a) -> IO a
withSink cfg followed own body = do
    sink <-
        Sink cfg followed own
            <$> newIORef Nothing
            <*> newIORef (Nothing, Nothing)
            <*> newTVarIO False
            <*> newIORef False
    withAsync (writer sink) $ \_ -> body sink
  where
    writer sink = forever $ do
        timer <- registerDelay cfg.configInterval
        atomically $ do
            woken <- readTVar sink.sinkWake
            due <- readTVar timer
            unless (woken || due) retry
            writeTVar sink.sinkWake False
        writeNow sink

{- | What to hand 'Serve.serveObserved': installs the world accessor the
status object is read through. Combine with another observer (the HTTP
server's) by sequencing them; each is a write of one cell.
-}
sinkObserver :: Sink -> IO (World seed directive) -> IO ()
sinkObserver sink readWorld =
    writeIORef sink.sinkStatusOf $
        Just $ do
            w <- readWorld
            mode <- maybe (pure Serve.Interactive) followedMode sink.sinkFollowed
            pure (toJSON (Serve.StatusReport mode (Map.toList w.worldNodes) (Serve.worldPaths w)))

{- | The reporter to compose beside the loop's own. It keeps the last
converge-stop and the last follow report, and wakes the writer on a pass
ending or a document being injected; everything else passes through
unobserved. The objects kept are the tagged ones — @stream@ included — so
what the sink carries is byte-for-byte what @--json@ printed.
-}
sinkReporter :: Sink -> Reporter Tagged
sinkReporter sink = ReporterM $ \tagged ->
    case tagged of
        FromServe Serve.ConvergeStop{} -> do
            modifyIORef' sink.sinkLast (\(_, f) -> (Just (toJSON tagged), f))
            wake
        FromFollow rep -> do
            modifyIORef' sink.sinkLast (\(c, _) -> (c, Just (toJSON tagged)))
            when (injected rep) wake
        _ -> pure ()
  where
    wake = atomically (writeTVar sink.sinkWake True)
    injected Follow.Injected{} = True
    injected _ = False

{- | Write the document once, now. Nothing before the observer has
installed the world accessor; a failure is reported once per run of them.
-}
writeNow :: Sink -> IO ()
writeNow sink = do
    accessor <- readIORef sink.sinkStatusOf
    case accessor of
        Nothing -> pure ()
        Just readStatus -> do
            attempt <- try $ do
                now <- getCurrentTime
                status <- readStatus
                mode <- maybe (pure Serve.Interactive) followedMode sink.sinkFollowed
                labels <- maybe (pure []) followedApplied sink.sinkFollowed
                (lastConverge, lastFollow) <- readIORef sink.sinkLast
                let doc =
                        Document
                            { docHost = sink.sinkConfig.configHost
                            , docWritten = now
                            , docMode = Serve.renderMode mode
                            , docLabels = labels
                            , docStatus = status
                            , docLastConverge = lastConverge
                            , docLastFollow = lastFollow
                            }
                writeAtomically sink.sinkConfig.configPath (encode doc)
            case attempt of
                Right () -> writeIORef sink.sinkComplained False
                Left (ex :: SomeException) -> do
                    complained <- readIORef sink.sinkComplained
                    unless complained $ do
                        writeIORef sink.sinkComplained True
                        runReporter sink.sinkOwn (FromServe (Serve.SinkFailed sink.sinkConfig.configPath (Text.pack (show ex))))

{- | Write bytes to a path through a temporary file beside it and a rename,
creating the directory if missing. May throw; the caller decides what a
failure means.
-}
writeAtomically :: FilePath -> LByteString.ByteString -> IO ()
writeAtomically path bytes = do
    createDirectoryIfMissing True (takeDirectory path)
    let tmp = path <.> "tmp"
    LByteString.writeFile tmp bytes
    renameFile tmp path
