{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Google Cloud Monitoring: notification channels and alerting policies.

A policy's conditions are a 'Condition' each: one of the Cloud Run
conditions ('CloudRunCondition', what 'AlertPolicy' and 'alertPolicy' are
for), a threshold on any metric filter ('MetricThreshold': an uptime check,
a VM metric, a log-based metric), a metric going absent ('MetricAbsence'), or
a log line matching a filter ('LogMatch'). 'Policy' and 'monitoringPolicy'
take any mix of them, plus an optional 'AlertStrategy'. An 'AlertPolicy' is
'cloudRunPolicy' of it, and renders to the same bytes it always did.

Both resources get __server-assigned ids__ (@projects\/P\/notificationChannels\/123@,
@projects\/P\/alertPolicies\/456@), so unlike a bucket or a service account
a declaration cannot address its resource by a name it chose. The identity
used here is the __display name__: @check@ is a filtered @list@, @up@
creates when nothing answers to that name and updates when something does,
and @down@ deletes whatever the lookup finds. Two declarations with one
display name in one project are therefore one resource, which is what the
'Salmon.Op.Ref' keyed on @(project, display name)@ says too.

An alert policy is compared by a __fingerprint__: the rendered policy — the
conditions, the combiner, the documentation and the channel ids it names —
hashed into a @salmon-fingerprint@ user label. A policy whose label matches
what the declaration renders to is skipped; anything else (absent, edited in
the console, a channel recreated under a new id) is created or updated. That
is the same "compare the bytes I would write" shape as
"Salmon.Builtin.Nodes.Filesystem".@checkFileContents@, with the label
standing in for the file since the API's own representation of a policy
carries fields (creation records, mutation records, condition names) that a
declaration never wrote.

Policies are passed to @gcloud@ inline (@--policy JSON@) rather than through
a file: nothing in one is secret, and it keeps the node free of temporary
files. Channel kinds are a sum with one constructor today ('Email'), so that
adding webhooks and provider-specific ones is additive.

Everything here goes through @gcloud beta monitoring channels@ and
@gcloud alpha monitoring policies@ (the release tracks those command groups
live on), and needs @monitoring.googleapis.com@ enabled on the project —
declare that with "Salmon.Builtin.Nodes.Gcp.ServiceUsage".@enableService@
as a dependency, the way @run.googleapis.com@ is for a service.
-}
module Salmon.Builtin.Nodes.Gcp.Monitoring (
    -- * Notification channels
    ChannelKind (..),
    NotificationChannel (..),
    notificationChannel,
    channelLabels,
    Lookup (..),
    sequenceLookups,
    FoundChannel (..),
    lookupChannel,
    interpretChannelList,

    -- * Alert policies on Cloud Run services
    CloudRunTarget (..),
    CloudRunCondition (..),
    AlertPolicy (..),
    alertPolicy,
    alertPolicyAfter,
    cloudRunPolicy,
    renderCondition,
    renderPolicy,
    policyFingerprint,
    fingerprintLabel,
    FoundPolicy (..),
    lookupPolicy,
    interpretPolicyList,

    -- * Alert policies on any condition
    Policy (..),
    Condition (..),
    MetricThreshold (..),
    MetricAbsence (..),
    LogMatch (..),
    Aggregation (..),
    Comparison (..),
    AlertStrategy (..),
    noAlertStrategy,
    monitoringPolicy,
    monitoringPolicyAfter,
    policyProblems,
    renderMonitoringCondition,
    renderMonitoringPolicy,
    monitoringPolicyFingerprint,
    lookupMonitoringPolicy,
    interpretMonitoringPolicyList,

    -- ** Filters and ready-made conditions
    metricTypeFilter,
    logBasedMetricFilter,
    uptimeCheckFailing,

    -- * Plumbing
    monitoringApi,
    Report (..),
    MonitoringCommand (..),
    monitoringCommand,
) where

import Control.Exception (throwIO)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.IO.Error (userError)
import System.Process.ByteString (readCreateProcessWithExitCode)
import Text.Printf (printf)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), untrackedExec, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunMonitoringCommand !MonitoringCommand !Binary.Report
    deriving (Show)

-- | The API these nodes need enabled: what to hand @ServiceUsage.enableService@.
monitoringApi :: Text
monitoringApi = "monitoring.googleapis.com"

-------------------------------------------------------------------------------
-- Notification channels

{- | Where a policy's notifications go. One constructor for now; the order
of arrival is email, then webhooks, then provider-specific kinds.
-}
newtype ChannelKind
    = -- | an email address
      Email Text
    deriving (Eq, Show)

channelType :: ChannelKind -> Text
channelType (Email _) = "email"

-- | The @--channel-labels@ a kind needs, as @key=value@ pairs.
channelLabels :: ChannelKind -> [(Text, Text)]
channelLabels (Email address) = [("email_address", address)]

data NotificationChannel = NotificationChannel
    { ncProject :: Project
    , ncDisplayName :: Text
    -- ^ the identity, see the module header; must not contain a double quote
    , ncKind :: ChannelKind
    }
    deriving (Eq, Show)

-- | A notification channel found by display name, and whether it already
-- carries the declared kind and labels.
data FoundChannel = FoundChannel
    { fcName :: Text
    -- ^ @projects\/P\/notificationChannels\/ID@
    , fcMatches :: Bool
    }
    deriving (Eq, Show)

-- | What a filtered @list@ said.
data Lookup a
    = -- | the command failed (exit code, first line of stderr)
      LookupFailed Text
    | -- | nothing answers to the display name
      Absent
    | -- | one does (several: the first, since the name is the identity)
      Present a
    deriving (Eq, Show, Functor)

-- | Every lookup present, or the first that was not.
sequenceLookups :: [Lookup a] -> Lookup [a]
sequenceLookups = foldr step (Present [])
  where
    step (Present x) (Present xs) = Present (x : xs)
    step (Present _) other = other
    step (LookupFailed why) _ = LookupFailed why
    step Absent (LookupFailed why) = LookupFailed why
    step Absent _ = Absent

{- | A notification channel of the declared kind, found by display name.
@up@ creates it, or updates the labels of one that exists with a different
address; @down@ deletes what the lookup finds.
-}
notificationChannel :: Reporter Report -> Track' (Binary "gcloud") -> NotificationChannel -> Op
notificationChannel r gcloudTrack ch =
    withBinary gcloudTrack monitoringCommand (ChannelsCreate ch) $ \_create ->
        op "gcp-monitoring-channel" nodeps $ \actions ->
            actions
                { help = Text.unwords ["notification channel", ch.ncDisplayName, "(" <> channelType ch.ncKind <> ")"]
                , ref = mkRef "gcp-monitoring-channel" (ch.ncProject.projectId, ch.ncDisplayName)
                , up = upChannel
                , down = downChannel
                , check = interpretChannelList ch <$> listChannel
                }
  where
    run :: MonitoringCommand -> IO ()
    run cmd = untrackedExec monitoringCommand cmd "" (contramap (RunMonitoringCommand cmd) r)

    listChannel :: IO (ExitCode, ByteString, ByteString)
    listChannel = readCreateProcessWithExitCode (prepare monitoringCommand (ChannelsList ch)) ""

    upChannel = do
        (code, out, err) <- listChannel
        case lookupChannel ch code out err of
            LookupFailed why -> throwIO (userError ("listing notification channels failed: " <> Text.unpack why))
            Absent -> run (ChannelsCreate ch)
            Present found
                | found.fcMatches -> pure ()
                | otherwise -> run (ChannelsUpdate found.fcName ch)

    downChannel = do
        (code, out, err) <- listChannel
        case lookupChannel ch code out err of
            Present found -> run (ChannelsDelete ch.ncProject found.fcName)
            -- absent, or unreachable: nothing to delete, and erring toward
            -- not-running is the safe direction (see 'Core.downIfPresent')
            _ -> pure ()

-- | Pure: what @gcloud beta monitoring channels list --format json@ said
-- about the declared channel.
lookupChannel :: NotificationChannel -> ExitCode -> ByteString -> ByteString -> Lookup FoundChannel
lookupChannel ch code out err =
    case code of
        ExitFailure n -> LookupFailed (Text.pack ("exit " <> show n) <> firstLine err)
        ExitSuccess -> case Aeson.decodeStrict' out of
            Just (Array items) | (item : _) <- filter (named ch.ncDisplayName) (toList items) ->
                Present
                    FoundChannel
                        { fcName = maybe "" id (textField "name" item)
                        , fcMatches =
                            textField "type" item == Just (channelType ch.ncKind)
                                && all (\(k, v) -> nestedTextField "labels" k item == Just v) (channelLabels ch.ncKind)
                        }
            Just (Array _) -> Absent
            _ -> LookupFailed "unparseable list output"
  where
    named name item = textField "displayName" item == Just name

-- | The 'CheckResult' for a channel: there, with the declared kind and address.
interpretChannelList :: NotificationChannel -> (ExitCode, ByteString, ByteString) -> CheckResult
interpretChannelList ch (code, out, err) =
    case lookupChannel ch code out err of
        LookupFailed why -> Failure ("cannot list notification channels: " <> why)
        Absent -> Failure ("no notification channel named " <> ch.ncDisplayName)
        Present found
            | found.fcMatches -> Success
            | otherwise -> Failure ("notification channel " <> ch.ncDisplayName <> " exists with a different kind or address")

-------------------------------------------------------------------------------
-- Alert policies

-- | The Cloud Run service a condition is about.
data CloudRunTarget = CloudRunTarget
    { crtProject :: Project
    , crtRegion :: Region
    , crtService :: Text
    }
    deriving (Eq, Show)

{- | A condition on one Cloud Run service, each a @conditionThreshold@ on
one of the @run.googleapis.com@ metrics. Durations are seconds the
condition must hold before the policy fires; thresholds are in the metric's
own unit.
-}
data CloudRunCondition
    = -- | the share of requests answered 5xx, over all requests: a ratio in
      -- @[0,1]@ (a @denominatorFilter@ condition on @request_count@)
      ServerErrorRatio {ratio :: Double, seconds :: Int}
    | -- | the 99th percentile of @request_latencies@, in milliseconds
      RequestLatencyP99 {milliseconds :: Double, seconds :: Int}
    | -- | active @container\/instance_count@, summed over the service's
      -- revisions — set it to the service's @--max-instances@ to hear about
      -- a service that is at its ceiling
      InstanceCount {instances :: Int, seconds :: Int}
    | -- | the 99th percentile of @container\/memory\/utilizations@, a
      -- fraction of the limit in @[0,1]@
      MemoryUtilization {fraction :: Double, seconds :: Int}
    deriving (Eq, Show)

data AlertPolicy = AlertPolicy
    { apProject :: Project
    , apDisplayName :: Text
    -- ^ the identity, see the module header; must not contain a double quote
    , apTarget :: CloudRunTarget
    , apConditions :: [CloudRunCondition]
    -- ^ combined with OR: any one firing fires the policy
    , apChannels :: [NotificationChannel]
    -- ^ resolved to their ids when the policy is checked or applied; each is
    -- also a dependency of the policy node
    , apDocumentation :: Text
    -- ^ markdown shown with the notification
    }
    deriving (Eq, Show)

{- | An alert policy on a Cloud Run service. Its channels are dependencies.
@up@ creates or updates so the policy carries the rendered conditions and
the fingerprint label; @down@ deletes what the lookup finds.
-}
alertPolicy :: Reporter Report -> Track' (Binary "gcloud") -> AlertPolicy -> Op
alertPolicy = alertPolicyAfter []

{- | 'alertPolicy', with nodes the policy and each of its channels wait for
(@monitoring.googleapis.com@ enabled, typically).

A node @inject@ed onto the policy waits before the policy only: its channels
are its dependencies, and siblings of what was injected. These go under the
channels as well. With @[]@ the graph is 'alertPolicy''s, unchanged.
-}
alertPolicyAfter :: [Op] -> Reporter Report -> Track' (Binary "gcloud") -> AlertPolicy -> Op
alertPolicyAfter prerequisites r gcloudTrack p =
    policyNode
        (Text.unwords ["alert policy", p.apDisplayName, "on Cloud Run service", p.apTarget.crtService])
        prerequisites
        r
        gcloudTrack
        (cloudRunPolicy p)

{- | The general policy an 'AlertPolicy' is: each condition about its target,
no alert strategy. It renders, fingerprints and is looked up exactly as the
'AlertPolicy' always was.
-}
cloudRunPolicy :: AlertPolicy -> Policy
cloudRunPolicy p =
    Policy
        { poProject = p.apProject
        , poDisplayName = p.apDisplayName
        , poConditions = map (OnCloudRun p.apTarget) p.apConditions
        , poChannels = p.apChannels
        , poDocumentation = p.apDocumentation
        , poAlertStrategy = Nothing
        }

-------------------------------------------------------------------------------
-- Alert policies on any condition

{- | An alerting policy whose conditions need not be about one Cloud Run
service. Same identity (display name) and fingerprint scheme as
'AlertPolicy'; the conditions are combined with OR.
-}
data Policy = Policy
    { poProject :: Project
    , poDisplayName :: Text
    -- ^ the identity, see the module header; must not contain a double quote
    , poConditions :: [Condition]
    -- ^ combined with OR: any one firing fires the policy
    , poChannels :: [NotificationChannel]
    -- ^ resolved to their ids when the policy is checked or applied; each is
    -- also a dependency of the policy node
    , poDocumentation :: Text
    -- ^ markdown shown with the notification
    , poAlertStrategy :: Maybe AlertStrategy
    -- ^ 'Nothing' renders no @alertStrategy@ field; a 'LogMatch' condition
    -- needs one with a notification rate limit (see 'policyProblems')
    }
    deriving (Eq, Show)

-- | One condition of a 'Policy'.
data Condition
    = -- | a Cloud Run condition about that service (what 'AlertPolicy' holds)
      OnCloudRun CloudRunTarget CloudRunCondition
    | -- | @conditionThreshold@ on any metric filter
      Threshold MetricThreshold
    | -- | @conditionAbsent@: the filter's series stop arriving
      Absence MetricAbsence
    | -- | @conditionMatchedLog@: a log entry matches a filter. Google allows
      -- it only as a policy's single condition, with a notification rate limit
      MatchedLog LogMatch
    deriving (Eq, Show)

{- | A threshold on the series a monitoring filter selects, e.g. a VM's disk:

> MetricThreshold
>     { mtName = "disk above 90%"
>     , mtFilter = metricTypeFilter "agent.googleapis.com/disk/percent_used"
>         [("resource.type", "gce_instance"), ("metric.labels.state", "used")]
>     , mtAggregations = [Aggregation 300 "ALIGN_MEAN" (Just "REDUCE_MAX") ["resource.labels.instance_id"]]
>     , mtComparison = GreaterThan
>     , mtThreshold = 90
>     , mtSeconds = 600
>     }

or a log-based metric ('logBasedMetricFilter'), or an uptime check
('uptimeCheckFailing').
-}
data MetricThreshold = MetricThreshold
    { mtName :: Text
    -- ^ the condition's display name, shown in the incident
    , mtFilter :: Text
    -- ^ a monitoring filter, with a @metric.type@
    , mtAggregations :: [Aggregation]
    -- ^ @[]@ renders no @aggregations@ (raw series)
    , mtComparison :: Comparison
    , mtThreshold :: Double
    -- ^ in the metric's (aggregated) unit
    , mtSeconds :: Int
    -- ^ how long the comparison must hold before the condition fires
    }
    deriving (Eq, Show)

-- | No data for the filter's series for 'maSeconds' (Google wants at least
-- 120s).
data MetricAbsence = MetricAbsence
    { maName :: Text
    , maFilter :: Text
    , maAggregations :: [Aggregation]
    , maSeconds :: Int
    }
    deriving (Eq, Show)

-- | A log entry matching a Logging query fires the condition.
data LogMatch = LogMatch
    { lmName :: Text
    , lmFilter :: Text
    -- ^ a Cloud Logging query, e.g. @severity>=ERROR AND resource.type="gce_instance"@
    , lmLabelExtractors :: [(Text, Text)]
    -- ^ label name to an extractor expression (@EXTRACT(jsonPayload.x)@);
    -- @[]@ renders none
    }
    deriving (Eq, Show)

-- | One entry of a condition's @aggregations@.
data Aggregation = Aggregation
    { agAlignmentSeconds :: Int
    , agAligner :: Text
    -- ^ @perSeriesAligner@, e.g. @ALIGN_MEAN@
    , agReducer :: Maybe Text
    -- ^ @crossSeriesReducer@, e.g. @REDUCE_SUM@; 'Nothing' renders none
    , agGroupBy :: [Text]
    -- ^ @groupByFields@; @[]@ renders none
    }
    deriving (Eq, Show)

data Comparison
    = GreaterThan
    | GreaterOrEqual
    | LessThan
    | LessOrEqual
    | EqualTo
    | NotEqualTo
    deriving (Eq, Show, Enum, Bounded)

comparisonText :: Comparison -> Text
comparisonText c = case c of
    GreaterThan -> "COMPARISON_GT"
    GreaterOrEqual -> "COMPARISON_GE"
    LessThan -> "COMPARISON_LT"
    LessOrEqual -> "COMPARISON_LE"
    EqualTo -> "COMPARISON_EQ"
    NotEqualTo -> "COMPARISON_NE"

-- | The policy's @alertStrategy@. Each 'Nothing' renders no field.
data AlertStrategy = AlertStrategy
    { asNotificationRateLimitSeconds :: Maybe Int
    -- ^ at most one notification per this many seconds (log-match policies)
    , asAutoCloseSeconds :: Maybe Int
    -- ^ close an incident after this long without data
    }
    deriving (Eq, Show)

noAlertStrategy :: AlertStrategy
noAlertStrategy = AlertStrategy Nothing Nothing

{- | What Google would refuse about a policy, checked by @up@ before any
command runs: no condition at all, a log-match condition beside other
conditions, or one without a notification rate limit.
-}
policyProblems :: Policy -> [Text]
policyProblems p =
    ["a policy needs at least one condition" | null p.poConditions]
        <> ["a log-match condition must be the policy's only condition" | hasLogMatch, length p.poConditions > 1]
        <> ["a log-match condition needs an alert strategy with a notification rate limit" | hasLogMatch, not rateLimited]
  where
    hasLogMatch = any isLogMatch p.poConditions
    isLogMatch MatchedLog{} = True
    isLogMatch _ = False
    rateLimited = maybe False (\st -> st.asNotificationRateLimitSeconds /= Nothing) p.poAlertStrategy

-- | A filter on a metric type and further @key="value"@ clauses, ANDed.
metricTypeFilter :: Text -> [(Text, Text)] -> Text
metricTypeFilter metric extra =
    Text.intercalate " AND " (("metric.type=\"" <> metric <> "\"") : [k <> "=\"" <> v <> "\"" | (k, v) <- extra])

-- | The filter of a user-defined log-based metric, by its name.
logBasedMetricFilter :: Text -> [(Text, Text)] -> Text
logBasedMetricFilter name = metricTypeFilter ("logging.googleapis.com/user/" <> name)

{- | @uptimeCheckFailing name checkId locations seconds@: an uptime check
(by its check id, the last segment of its resource name) failing from more
than @locations@ checker locations for @seconds@. It is the shape of the
condition the Cloud console makes for a check, over whatever resource type
the check is on.
-}
uptimeCheckFailing :: Text -> Text -> Int -> Int -> MetricThreshold
uptimeCheckFailing name checkId locations secs =
    MetricThreshold
        { mtName = name
        , mtFilter =
            metricTypeFilter
                "monitoring.googleapis.com/uptime_check/check_passed"
                [("metric.labels.check_id", checkId)]
        , mtAggregations = [Aggregation 1200 "ALIGN_NEXT_OLDER" (Just "REDUCE_COUNT_FALSE") ["resource.label.*"]]
        , mtComparison = GreaterThan
        , mtThreshold = fromIntegral locations
        , mtSeconds = secs
        }

{- | An alert policy on any conditions. Its channels are dependencies.
@up@ refuses a policy with 'policyProblems', then creates or updates so the
policy carries the rendered conditions and the fingerprint label; @down@
deletes what the lookup finds.
-}
monitoringPolicy :: Reporter Report -> Track' (Binary "gcloud") -> Policy -> Op
monitoringPolicy = monitoringPolicyAfter []

-- | 'monitoringPolicy' with prerequisites, as 'alertPolicyAfter'.
monitoringPolicyAfter :: [Op] -> Reporter Report -> Track' (Binary "gcloud") -> Policy -> Op
monitoringPolicyAfter prerequisites r gcloudTrack p =
    policyNode (Text.unwords ["alert policy", p.poDisplayName]) prerequisites r gcloudTrack p

policyNode :: Text -> [Op] -> Reporter Report -> Track' (Binary "gcloud") -> Policy -> Op
policyNode helpText prerequisites r gcloudTrack policy =
    after $ withBinary gcloudTrack monitoringCommand (PoliciesList policy) $ \_list ->
        op "gcp-monitoring-policy" (deps channelNodes) $ \actions ->
            actions
                { help = helpText
                , notes =
                    [ "conditions: " <> Text.intercalate ", " (map monitoringConditionName policy.poConditions)
                    , "channels: " <> Text.intercalate ", " (map (.ncDisplayName) policy.poChannels)
                    ]
                        <> maybe [] (\st -> ["alert strategy: " <> strategyNote st]) policy.poAlertStrategy
                , ref = mkRef "gcp-monitoring-policy" (policy.poProject.projectId, policy.poDisplayName)
                , up = upPolicy
                , down = downPolicy
                , check = checkPolicy
                }
  where
    after o = foldl inject o prerequisites
    channelNodes = map (after . notificationChannel r gcloudTrack) policy.poChannels

    strategyNote :: AlertStrategy -> Text
    strategyNote st =
        Text.intercalate
            ", "
            ( maybe [] (\n -> ["at most one notification per " <> secondsText n]) st.asNotificationRateLimitSeconds
                <> maybe [] (\n -> ["auto-close after " <> secondsText n]) st.asAutoCloseSeconds
            )

    run :: MonitoringCommand -> IO ()
    run cmd = untrackedExec monitoringCommand cmd "" (contramap (RunMonitoringCommand cmd) r)

    listPolicy :: IO (ExitCode, ByteString, ByteString)
    listPolicy = readCreateProcessWithExitCode (prepare monitoringCommand (PoliciesList policy)) ""

    -- the channels' resource names, in declaration order; a channel that
    -- cannot be found is an error here, since it is a dependency that
    -- should have been brought up first
    resolveChannels :: IO [Text]
    resolveChannels = mapM resolve policy.poChannels
      where
        resolve ch = do
            (code, out, err) <- readCreateProcessWithExitCode (prepare monitoringCommand (ChannelsList ch)) ""
            case lookupChannel ch code out err of
                Present found -> pure found.fcName
                Absent -> throwIO (userError ("notification channel not found: " <> Text.unpack ch.ncDisplayName))
                LookupFailed why -> throwIO (userError ("listing notification channels failed: " <> Text.unpack why))

    -- a check that cannot resolve a channel says so rather than comparing
    -- against a policy that could not be rendered
    checkPolicy :: IO CheckResult
    checkPolicy = do
        lookups <- mapM lookupOne policy.poChannels
        case sequenceLookups lookups of
            LookupFailed why -> pure (Failure ("cannot list notification channels: " <> why))
            Absent -> pure (Failure "a notification channel of the policy is missing")
            Present names -> interpretMonitoringPolicyList policy names <$> listPolicy
      where
        lookupOne ch = do
            (code, out, err) <- readCreateProcessWithExitCode (prepare monitoringCommand (ChannelsList ch)) ""
            pure (fmap (.fcName) (lookupChannel ch code out err))

    upPolicy = do
        case policyProblems policy of
            [] -> pure ()
            problems ->
                throwIO (userError ("alert policy " <> Text.unpack policy.poDisplayName <> ": " <> Text.unpack (Text.intercalate "; " problems)))
        names <- resolveChannels
        (code, out, err) <- listPolicy
        case lookupMonitoringPolicy policy code out err of
            LookupFailed why -> throwIO (userError ("listing alert policies failed: " <> Text.unpack why))
            Absent -> run (PoliciesCreate policy names)
            Present found
                | found.fpFingerprint == Just (monitoringPolicyFingerprint policy names) -> pure ()
                | otherwise -> run (PoliciesUpdate found.fpName policy names)

    downPolicy = do
        (code, out, err) <- listPolicy
        case lookupMonitoringPolicy policy code out err of
            Present found -> run (PoliciesDelete policy.poProject found.fpName)
            _ -> pure ()

secondsText :: Int -> Text
secondsText n = Text.pack (show n) <> "s"

-- | An alert policy found by display name, and the fingerprint label it carries.
data FoundPolicy = FoundPolicy
    { fpName :: Text
    -- ^ @projects\/P\/alertPolicies\/ID@
    , fpFingerprint :: Maybe Text
    }
    deriving (Eq, Show)

-- | Pure: what @gcloud alpha monitoring policies list --format json@ said
-- about the declared policy.
lookupPolicy :: AlertPolicy -> ExitCode -> ByteString -> ByteString -> Lookup FoundPolicy
lookupPolicy = lookupMonitoringPolicy . cloudRunPolicy

-- | 'lookupPolicy' for a 'Policy'.
lookupMonitoringPolicy :: Policy -> ExitCode -> ByteString -> ByteString -> Lookup FoundPolicy
lookupMonitoringPolicy policy code out err =
    case code of
        ExitFailure n -> LookupFailed (Text.pack ("exit " <> show n) <> firstLine err)
        ExitSuccess -> case Aeson.decodeStrict' out of
            Just (Array items) | (item : _) <- filter (named policy.poDisplayName) (toList items) ->
                Present
                    FoundPolicy
                        { fpName = maybe "" id (textField "name" item)
                        , fpFingerprint = nestedTextField "userLabels" fingerprintLabel item
                        }
            Just (Array _) -> Absent
            _ -> LookupFailed "unparseable list output"
  where
    named name item = textField "displayName" item == Just name

-- | The 'CheckResult' for a policy, given its channels' resolved names:
-- there, and carrying the fingerprint of what the declaration renders to.
interpretPolicyList :: AlertPolicy -> [Text] -> (ExitCode, ByteString, ByteString) -> CheckResult
interpretPolicyList = interpretMonitoringPolicyList . cloudRunPolicy

-- | 'interpretPolicyList' for a 'Policy'.
interpretMonitoringPolicyList :: Policy -> [Text] -> (ExitCode, ByteString, ByteString) -> CheckResult
interpretMonitoringPolicyList policy channelNames (code, out, err) =
    case lookupMonitoringPolicy policy code out err of
        LookupFailed why -> Failure ("cannot list alert policies: " <> why)
        Absent -> Failure ("no alert policy named " <> policy.poDisplayName)
        Present found
            | found.fpFingerprint == Just (monitoringPolicyFingerprint policy channelNames) -> Success
            | otherwise -> Failure ("alert policy " <> policy.poDisplayName <> " differs from its declaration")

-- | The user label the fingerprint rides on.
fingerprintLabel :: Text
fingerprintLabel = "salmon-fingerprint"

{- | A stable hash of everything the declaration renders into the policy,
channel ids included, in the alphabet a user label value allows
(lowercase hex, 16 characters).
-}
policyFingerprint :: AlertPolicy -> [Text] -> Text
policyFingerprint = monitoringPolicyFingerprint . cloudRunPolicy

-- | 'policyFingerprint' for a 'Policy'.
monitoringPolicyFingerprint :: Policy -> [Text] -> Text
monitoringPolicyFingerprint policy channelNames =
    Text.pack (take 16 (concatMap (printf "%02x") (ByteString.unpack digest)))
  where
    digest = SHA256.hash (LByteString.toStrict (Aeson.encode (renderPolicy' policy channelNames)))

-- | The policy JSON @gcloud alpha monitoring policies create --policy@
-- takes, fingerprint label included.
renderPolicy :: AlertPolicy -> [Text] -> Value
renderPolicy = renderMonitoringPolicy . cloudRunPolicy

-- | 'renderPolicy' for a 'Policy'.
renderMonitoringPolicy :: Policy -> [Text] -> Value
renderMonitoringPolicy policy channelNames =
    withLabel (renderPolicy' policy channelNames)
  where
    withLabel (Object o) = Object (KeyMap.insert "userLabels" (object [Key.fromText fingerprintLabel .= monitoringPolicyFingerprint policy channelNames]) o)
    withLabel v = v

{- | The policy without its fingerprint: what the fingerprint is of. An
absent or empty alert strategy adds no key, so a Cloud Run policy renders as
it did before 'Policy' existed.
-}
renderPolicy' :: Policy -> [Text] -> Value
renderPolicy' policy channelNames =
    object
        ( [ "displayName" .= policy.poDisplayName
          , "combiner" .= ("OR" :: Text)
          , "enabled" .= True
          , "notificationChannels" .= channelNames
          , "conditions" .= map renderMonitoringCondition policy.poConditions
          , "documentation" .= object ["content" .= policy.poDocumentation, "mimeType" .= ("text/markdown" :: Text)]
          ]
            <> maybe [] renderStrategy policy.poAlertStrategy
        )
  where
    renderStrategy :: AlertStrategy -> [(Key.Key, Value)]
    renderStrategy st = case fields of
        [] -> []
        _ -> ["alertStrategy" .= object fields]
      where
        fields =
            maybe [] (\n -> ["notificationRateLimit" .= object ["period" .= secondsText n]]) st.asNotificationRateLimitSeconds
                <> maybe [] (\n -> ["autoClose" .= secondsText n]) st.asAutoCloseSeconds

monitoringConditionName :: Condition -> Text
monitoringConditionName c = case c of
    OnCloudRun _ crc -> conditionName crc
    Threshold t -> t.mtName
    Absence a -> a.maName
    MatchedLog l -> l.lmName

-- | One condition as the API's condition object.
renderMonitoringCondition :: Condition -> Value
renderMonitoringCondition c = case c of
    OnCloudRun target crc -> renderCondition target crc
    Threshold t ->
        object
            [ "displayName" .= t.mtName
            , "conditionThreshold"
                .= object
                    ( ["filter" .= t.mtFilter]
                        <> aggregations t.mtAggregations
                        <> [ "comparison" .= comparisonText t.mtComparison
                           , "thresholdValue" .= t.mtThreshold
                           , "duration" .= secondsText t.mtSeconds
                           , "trigger" .= object ["count" .= (1 :: Int)]
                           ]
                    )
            ]
    Absence a ->
        object
            [ "displayName" .= a.maName
            , "conditionAbsent"
                .= object
                    ( ["filter" .= a.maFilter]
                        <> aggregations a.maAggregations
                        <> [ "duration" .= secondsText a.maSeconds
                           , "trigger" .= object ["count" .= (1 :: Int)]
                           ]
                    )
            ]
    MatchedLog l ->
        object
            [ "displayName" .= l.lmName
            , "conditionMatchedLog"
                .= object
                    ( ["filter" .= l.lmFilter]
                        <> case l.lmLabelExtractors of
                            [] -> []
                            kvs -> ["labelExtractors" .= object [Key.fromText k .= v | (k, v) <- kvs]]
                    )
            ]
  where
    aggregations :: [Aggregation] -> [(Key.Key, Value)]
    aggregations [] = []
    aggregations ags = ["aggregations" .= map renderAggregation ags]
    renderAggregation :: Aggregation -> Value
    renderAggregation ag =
        object
            ( [ "alignmentPeriod" .= secondsText ag.agAlignmentSeconds
              , "perSeriesAligner" .= ag.agAligner
              ]
                <> maybe [] (\red -> ["crossSeriesReducer" .= red]) ag.agReducer
                <> case ag.agGroupBy of
                    [] -> []
                    gs -> ["groupByFields" .= gs]
            )

conditionName :: CloudRunCondition -> Text
conditionName c = case c of
    ServerErrorRatio{} -> "5xx ratio"
    RequestLatencyP99{} -> "p99 latency"
    InstanceCount{} -> "instance count"
    MemoryUtilization{} -> "memory utilization"

-- | One condition as the API's @conditionThreshold@ object.
renderCondition :: CloudRunTarget -> CloudRunCondition -> Value
renderCondition target c =
    object
        [ "displayName" .= (target.crtService <> ": " <> conditionName c)
        , "conditionThreshold" .= object (common <> specific)
        ]
  where
    duration = Text.pack (show (seconds c)) <> "s"
    common =
        [ "comparison" .= ("COMPARISON_GT" :: Text)
        , "duration" .= duration
        , "trigger" .= object ["count" .= (1 :: Int)]
        ]
    specific = case c of
        ServerErrorRatio r _ ->
            [ "filter" .= metricFilter target "run.googleapis.com/request_count" [("metric.labels.response_code_class", "5xx")]
            , "denominatorFilter" .= metricFilter target "run.googleapis.com/request_count" []
            , "aggregations" .= [aggregation "ALIGN_RATE" "REDUCE_SUM"]
            , "denominatorAggregations" .= [aggregation "ALIGN_RATE" "REDUCE_SUM"]
            , "thresholdValue" .= r
            ]
        RequestLatencyP99 ms _ ->
            [ "filter" .= metricFilter target "run.googleapis.com/request_latencies" []
            , "aggregations" .= [aggregation "ALIGN_PERCENTILE_99" "REDUCE_MAX"]
            , "thresholdValue" .= ms
            ]
        InstanceCount n _ ->
            [ "filter" .= metricFilter target "run.googleapis.com/container/instance_count" [("metric.labels.state", "active")]
            , "aggregations" .= [aggregation "ALIGN_MAX" "REDUCE_SUM"]
            , "thresholdValue" .= (fromIntegral n - 0.5 :: Double)
            ]
        MemoryUtilization f _ ->
            [ "filter" .= metricFilter target "run.googleapis.com/container/memory/utilizations" []
            , "aggregations" .= [aggregation "ALIGN_PERCENTILE_99" "REDUCE_MAX"]
            , "thresholdValue" .= f
            ]

    aggregation :: Text -> Text -> Value
    aggregation aligner reducer =
        object
            [ "alignmentPeriod" .= ("60s" :: Text)
            , "perSeriesAligner" .= aligner
            , "crossSeriesReducer" .= reducer
            , "groupByFields" .= (["resource.label.service_name"] :: [Text])
            ]

-- | The monitoring filter selecting one Cloud Run service's series of a metric.
metricFilter :: CloudRunTarget -> Text -> [(Text, Text)] -> Text
metricFilter target metric extra =
    Text.intercalate
        " AND "
        ( [ "metric.type=\"" <> metric <> "\""
          , "resource.type=\"cloud_run_revision\""
          , "resource.labels.service_name=\"" <> target.crtService <> "\""
          , "resource.labels.location=\"" <> target.crtRegion.regionName <> "\""
          ]
            <> [k <> "=\"" <> v <> "\"" | (k, v) <- extra]
        )

-------------------------------------------------------------------------------
-- JSON helpers over gcloud's list output

textField :: Text -> Value -> Maybe Text
textField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
    Just (String t) -> Just t
    _ -> Nothing
textField _ _ = Nothing

nestedTextField :: Text -> Text -> Value -> Maybe Text
nestedTextField outer k (Object o) = KeyMap.lookup (Key.fromText outer) o >>= textField k
nestedTextField _ _ _ = Nothing

firstLine :: ByteString -> Text
firstLine err = case Text.lines (Text.decodeUtf8 err) of
    (l : _) -> ": " <> l
    [] -> ""

-------------------------------------------------------------------------------

data MonitoringCommand
    = ChannelsList NotificationChannel
    | ChannelsCreate NotificationChannel
    | -- | update the labels of the channel with this resource name
      ChannelsUpdate Text NotificationChannel
    | ChannelsDelete Project Text
    | PoliciesList Policy
    | -- | create with these resolved channel names
      PoliciesCreate Policy [Text]
    | -- | update the policy with this resource name
      PoliciesUpdate Text Policy [Text]
    | PoliciesDelete Project Text
    deriving (Show)

-- | gcloud's resource filter selecting one display name.
displayNameFilter :: Text -> String
displayNameFilter name = "display_name=\"" <> Text.unpack (Text.filter (/= '"') name) <> "\""

renderLabels :: [(Text, Text)] -> String
renderLabels kvs = Text.unpack (Text.intercalate "," [k <> "=" <> v | (k, v) <- kvs])

policyJson :: Policy -> [Text] -> String
policyJson policy names = Text.unpack (Text.decodeUtf8 (LByteString.toStrict (Aeson.encode (renderMonitoringPolicy policy names))))

monitoringCommand :: Command "gcloud" MonitoringCommand
monitoringCommand = Command $ \cmd -> case cmd of
    ChannelsList ch ->
        gcloudProc $
            withProject ch.ncProject
                ["beta", "monitoring", "channels", "list", "--filter", displayNameFilter ch.ncDisplayName, "--format", "json"]
    ChannelsCreate ch ->
        gcloudProc $
            withProject ch.ncProject
                [ "beta"
                , "monitoring"
                , "channels"
                , "create"
                , "--display-name"
                , Text.unpack ch.ncDisplayName
                , "--type"
                , Text.unpack (channelType ch.ncKind)
                , "--channel-labels"
                , renderLabels (channelLabels ch.ncKind)
                ]
    ChannelsUpdate name ch ->
        gcloudProc $
            withProject ch.ncProject
                [ "beta"
                , "monitoring"
                , "channels"
                , "update"
                , Text.unpack name
                , "--update-channel-labels"
                , renderLabels (channelLabels ch.ncKind)
                ]
    ChannelsDelete project name ->
        gcloudProc $
            withProject project ["beta", "monitoring", "channels", "delete", Text.unpack name, "--quiet"]
    PoliciesList policy ->
        gcloudProc $
            withProject policy.poProject
                ["alpha", "monitoring", "policies", "list", "--filter", displayNameFilter policy.poDisplayName, "--format", "json"]
    PoliciesCreate policy names ->
        gcloudProc $
            withProject policy.poProject
                ["alpha", "monitoring", "policies", "create", "--policy", policyJson policy names]
    PoliciesUpdate name policy names ->
        gcloudProc $
            withProject policy.poProject
                ["alpha", "monitoring", "policies", "update", Text.unpack name, "--policy", policyJson policy names]
    PoliciesDelete project name ->
        gcloudProc $
            withProject project ["alpha", "monitoring", "policies", "delete", Text.unpack name, "--quiet"]
