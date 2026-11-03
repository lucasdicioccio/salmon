{-# LANGUAGE OverloadedStrings #-}

{- | Cloud Logging: a log-based (counter) metric, and a log bucket's
retention.

A __log-based metric__ is named by the declaration (@logging.googleapis.com\/user\/NAME@
is what a monitoring filter names, see 'Monitoring.logBasedMetricFilter'),
so its name is its identity, keyed @(project, name)@. The API gives a
metric no labels, so the ownership marker rides at the end of its
description ('metricMarker', on a line of its own): @check@ compares the
filter and the whole description (marker included) with the declaration;
a metric of that name without the marker was not made by salmon, and @up@
refuses to take it over and @down@ leaves it. Only counters
(@--log-filter@): a distribution metric needs a value extractor and buckets,
not done.

A __log bucket's retention__ (@_Default@, typically) is a setting on a
bucket salmon does not create: @check@ reads @retentionDays@, @up@ sets it.
@down@ does __nothing__: the value before salmon set it is not known, and
shortening a retention deletes the logs past it, which no teardown should do
on a guess.

Both go through @gcloud logging@ and need @logging.googleapis.com@ enabled
('loggingApi').
-}
module Salmon.Builtin.Nodes.Gcp.Logging (
    loggingApi,

    -- * Log-based metrics
    LogMetric (..),
    logMetric,
    metricMarker,
    renderedDescription,
    FoundMetric (..),
    lookupMetric,
    interpretMetricList,

    -- * Log bucket retention
    LogBucket (..),
    defaultLogBucket,
    logBucketRetention,
    interpretBucketDescribe,

    -- * Plumbing
    Report (..),
    LoggingCommand (..),
    loggingCommand,
) where

import Control.Exception (throwIO)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.IO.Error (userError)
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), untrackedExec, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import Salmon.Builtin.Nodes.Gcp.Monitoring (Lookup (..), firstLine, textField)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunLoggingCommand !LoggingCommand !Binary.Report
    deriving (Show)

-- | The API these nodes need enabled.
loggingApi :: Text
loggingApi = "logging.googleapis.com"

-------------------------------------------------------------------------------
-- Log-based metrics

data LogMetric = LogMetric
    { lmProject :: Project
    , lmName :: Text
    -- ^ the metric's name, its identity
    , lmDescription :: Text
    -- ^ free text; the marker is appended
    , lmFilter :: Text
    -- ^ a Cloud Logging query, e.g. @resource.type="cloud_run_revision" AND severity>=ERROR@
    }
    deriving (Eq, Show)

-- | The line ending a salmon-made metric's description.
metricMarker :: LogMetric -> Text
metricMarker m = "salmon:log-metric:" <> m.lmName

-- | The description written: the declared one, then the marker on its own line.
renderedDescription :: LogMetric -> Text
renderedDescription m
    | Text.null m.lmDescription = metricMarker m
    | otherwise = m.lmDescription <> "\n" <> metricMarker m

{- | A counter log-based metric. @up@ creates it, or updates the filter and
description of a salmon-marked one that differs; @down@ deletes a
salmon-marked one.
-}
logMetric :: Reporter Report -> Track' (Binary "gcloud") -> LogMetric -> Op
logMetric r gcloudTrack m =
    withBinary gcloudTrack loggingCommand (MetricsList m.lmProject) $ \_list ->
        op "gcp-log-metric" nodeps $ \actions ->
            actions
                { help = Text.unwords ["log-based metric", m.lmName]
                , notes = ["filter: " <> m.lmFilter, "description: " <> m.lmDescription]
                , ref = mkRef "gcp-log-metric" (m.lmProject.projectId, m.lmName)
                , up = upMetric
                , down = downMetric
                , check = interpretMetricList m <$> list
                }
  where
    run :: LoggingCommand -> IO ()
    run cmd = untrackedExec loggingCommand cmd "" (contramap (RunLoggingCommand cmd) r)

    list :: IO (ExitCode, ByteString, ByteString)
    list = readCreateProcessWithExitCode (prepare loggingCommand (MetricsList m.lmProject)) ""

    refuse :: Text -> IO a
    refuse why = throwIO (userError (Text.unpack ("log-based metric " <> m.lmName <> ": " <> why)))

    upMetric = do
        case Text.strip m.lmFilter of
            "" -> refuse "the filter is empty (it would count every log entry)"
            _ -> pure ()
        (code, out, err) <- list
        case lookupMetric m code out err of
            LookupFailed why -> refuse ("listing log-based metrics failed: " <> why)
            Absent -> run (MetricsCreate m)
            Present found
                | not found.fmOwned -> refuse "exists without salmon's marker in its description, so salmon did not make it; not taking it over"
                | found.fmMatches -> pure ()
                | otherwise -> run (MetricsUpdate m)

    downMetric = do
        (code, out, err) <- list
        case lookupMetric m code out err of
            Present found | found.fmOwned -> run (MetricsDelete m.lmProject m.lmName)
            _ -> pure ()

data FoundMetric = FoundMetric
    { fmOwned :: Bool
    -- ^ the description ends with 'metricMarker'
    , fmMatches :: Bool
    -- ^ filter and description as declared
    }
    deriving (Eq, Show)

-- | Pure: what @gcloud logging metrics list --format json@ said about the
-- declared metric. The name is matched bare or as the last segment of a
-- resource name.
lookupMetric :: LogMetric -> ExitCode -> ByteString -> ByteString -> Lookup FoundMetric
lookupMetric m code out err =
    case code of
        ExitFailure n -> LookupFailed (Text.pack ("exit " <> show n) <> firstLine err)
        ExitSuccess -> case Aeson.decodeStrict' out of
            Just (Array items) | (item : _) <- filter named (toList items) ->
                let desc = fromMaybe "" (textField "description" item)
                 in Present
                        FoundMetric
                            { fmOwned = metricMarker m `Text.isSuffixOf` Text.stripEnd desc
                            , fmMatches =
                                desc == renderedDescription m
                                    && textField "filter" item == Just m.lmFilter
                            }
            Just (Array _) -> Absent
            _ -> LookupFailed "unparseable list output"
  where
    named item = case textField "name" item of
        Just n -> n == m.lmName || ("/metrics/" <> m.lmName) `Text.isSuffixOf` n
        Nothing -> False

interpretMetricList :: LogMetric -> (ExitCode, ByteString, ByteString) -> CheckResult
interpretMetricList m (code, out, err) =
    case lookupMetric m code out err of
        LookupFailed why -> Failure ("cannot list log-based metrics: " <> why)
        Absent -> Failure ("no log-based metric named " <> m.lmName)
        Present found
            | not found.fmOwned -> Failure ("log-based metric " <> m.lmName <> " exists without salmon's marker")
            | found.fmMatches -> Success
            | otherwise -> Failure ("log-based metric " <> m.lmName <> " differs from its declaration")

-------------------------------------------------------------------------------
-- Log bucket retention

data LogBucket = LogBucket
    { lbProject :: Project
    , lbLocation :: Text
    -- ^ @global@ for @_Default@
    , lbBucket :: Text
    -- ^ the bucket id, e.g. @_Default@
    }
    deriving (Eq, Show)

-- | The project's @_Default@ bucket, in @global@.
defaultLogBucket :: Project -> LogBucket
defaultLogBucket p = LogBucket p "global" "_Default"

{- | The bucket keeps logs for this many days (Google accepts 1 to 3650; the
@_Default@ bucket's first 30 days are not charged for storage). @check@
compares the bucket's @retentionDays@; @up@ sets it; @down@ does nothing
(see the module header).
-}
logBucketRetention :: Reporter Report -> Track' (Binary "gcloud") -> LogBucket -> Int -> Op
logBucketRetention r gcloudTrack b days =
    withBinary gcloudTrack loggingCommand (BucketDescribe b) $ \_describe ->
        op "gcp-log-bucket-retention" nodeps $ \actions ->
            actions
                { help = Text.unwords ["log bucket", b.lbBucket, "in", b.lbLocation, "keeps", Text.pack (show days), "days"]
                , ref = mkRef "gcp-log-bucket-retention" (b.lbProject.projectId, b.lbLocation, b.lbBucket)
                , up = upRetention
                , check = checkRetention
                }
  where
    checkRetention :: IO CheckResult
    checkRetention = do
        (code, out, err) <- readCreateProcessWithExitCode (prepare loggingCommand (BucketDescribe b)) ""
        pure (interpretBucketDescribe days code out err)

    upRetention
        | days < 1 || days > 3650 =
            throwIO (userError ("log bucket " <> Text.unpack b.lbBucket <> ": retention must be 1 to 3650 days"))
        | otherwise =
            let cmd = BucketSetRetention b days
             in untrackedExec loggingCommand cmd "" (contramap (RunLoggingCommand cmd) r)

-- | Pure: @gcloud logging buckets describe --format json@ against the
-- declared retention.
interpretBucketDescribe :: Int -> ExitCode -> ByteString -> ByteString -> CheckResult
interpretBucketDescribe _ (ExitFailure n) _ err =
    Failure ("cannot describe the log bucket: " <> Text.pack ("exit " <> show n) <> firstLine err)
interpretBucketDescribe days ExitSuccess out _ =
    case Aeson.decodeStrict' out of
        Just (Object o) -> case KeyMap.lookup (Key.fromText "retentionDays") o of
            Just v@(Number _)
                | Aeson.Success n <- Aeson.fromJSON v ->
                    if n == days
                        then Success
                        else Failure ("log bucket keeps " <> Text.pack (show (n :: Int)) <> " days, declared " <> Text.pack (show days))
            _ -> Failure "log bucket describes no retentionDays"
        _ -> Failure "unparseable log bucket description"

-------------------------------------------------------------------------------

data LoggingCommand
    = MetricsList Project
    | MetricsCreate LogMetric
    | MetricsUpdate LogMetric
    | MetricsDelete Project Text
    | BucketDescribe LogBucket
    | BucketSetRetention LogBucket Int
    deriving (Show)

loggingCommand :: Command "gcloud" LoggingCommand
loggingCommand = Command $ \cmd -> case cmd of
    MetricsList project ->
        gcloudProc $ withProject project ["logging", "metrics", "list", "--format", "json"]
    MetricsCreate m ->
        gcloudProc $ withProject m.lmProject (["logging", "metrics", "create", Text.unpack m.lmName] <> metricFlags m)
    MetricsUpdate m ->
        gcloudProc $ withProject m.lmProject (["logging", "metrics", "update", Text.unpack m.lmName] <> metricFlags m)
    MetricsDelete project name ->
        gcloudProc $ withProject project ["logging", "metrics", "delete", Text.unpack name, "--quiet"]
    BucketDescribe b ->
        gcloudProc $ withProject b.lbProject ["logging", "buckets", "describe", Text.unpack b.lbBucket, "--location", Text.unpack b.lbLocation, "--format", "json"]
    BucketSetRetention b days ->
        gcloudProc $ withProject b.lbProject ["logging", "buckets", "update", Text.unpack b.lbBucket, "--location", Text.unpack b.lbLocation, "--retention-days", show days]
  where
    metricFlags m = ["--description", Text.unpack (renderedDescription m), "--log-filter", Text.unpack m.lmFilter]
