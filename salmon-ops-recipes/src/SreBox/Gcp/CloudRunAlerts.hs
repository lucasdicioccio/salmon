{- | The standard alerts for one Cloud Run service, to one email address:
what an operator wants to hear about first, as one node.

Four "Salmon.Builtin.Nodes.Gcp.Monitoring".@alertPolicy@ declarations on
one @notificationChannel@, each keyed on its own display name
(@\<service\>: 5xx ratio@ and so on) so that a service's alerts and another
service's are distinct resources while two declarations of one service's
are one:

* the share of requests answered 5xx ('at_errorRatio', 5% by default),
* the 99th-percentile request latency ('at_latencyP99Ms', 2s),
* the 99th-percentile container memory utilisation
  ('at_memoryUtilization', 90%),
* and, when the service has a @--max-instances@ ('cra_maxInstances'),
  the active instance count reaching it,

each having to hold for 'at_duration' seconds (5 minutes) before firing.

Enabling @monitoring.googleapis.com@ is the caller's, as
@run.googleapis.com@ is for "SreBox.Gcp.CloudRunDeploy": a recipe does not
know what else the project's foundation has to come before.
-}
module SreBox.Gcp.CloudRunAlerts (
    AlertThresholds (..),
    defaultAlertThresholds,
    CloudRunAlertsConfig (..),
    standardAlerts,
    standardPolicies,
    Report (..),
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..))
import qualified Salmon.Builtin.Nodes.Gcp.Monitoring as Monitoring
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

newtype Report
    = RunMonitoring Monitoring.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | Where each alert fires. See the module header for the defaults.
data AlertThresholds = AlertThresholds
    { at_errorRatio :: Double
    -- ^ 5xx over all requests, in @[0,1]@
    , at_latencyP99Ms :: Double
    , at_memoryUtilization :: Double
    -- ^ in @[0,1]@
    , at_duration :: Int
    -- ^ seconds a condition must hold
    }
    deriving (Eq, Show)

defaultAlertThresholds :: AlertThresholds
defaultAlertThresholds =
    AlertThresholds
        { at_errorRatio = 0.05
        , at_latencyP99Ms = 2000
        , at_memoryUtilization = 0.9
        , at_duration = 300
        }

data CloudRunAlertsConfig = CloudRunAlertsConfig
    { cra_project :: Project
    , cra_region :: Region
    , cra_service :: Text
    , cra_email :: Text
    -- ^ where the notifications go
    , cra_channelName :: Text
    -- ^ the channel's display name, shared by every service alerting to
    -- the same address
    , cra_maxInstances :: Maybe Int
    -- ^ the service's @--max-instances@, if it has one: the instance-count
    -- alert exists only then, and fires at that number
    , cra_thresholds :: AlertThresholds
    }
    deriving (Eq, Show)

-- | The channel and the policies, under one node.
standardAlerts :: Reporter Report -> Track' (Binary "gcloud") -> CloudRunAlertsConfig -> Op
standardAlerts r gcloudTrack cfg =
    op "gcp-cloudrun-alerts" (deps (map (Monitoring.alertPolicy rMon gcloudTrack) (standardPolicies cfg))) $ \actions ->
        actions
            { help = Text.unwords ["the standard alerts for Cloud Run service", cfg.cra_service, "to", cfg.cra_email]
            , ref = mkRef "gcp-cloudrun-alerts" (cfg.cra_project.projectId, cfg.cra_region.regionName, cfg.cra_service)
            }
  where
    rMon = contramap RunMonitoring r

-- | The policies 'standardAlerts' declares, for a test or a caller wanting
-- to add its own beside them.
standardPolicies :: CloudRunAlertsConfig -> [Monitoring.AlertPolicy]
standardPolicies cfg =
    [ policy "5xx ratio" (Monitoring.ServerErrorRatio t.at_errorRatio t.at_duration) ("More than " <> percent t.at_errorRatio <> " of requests are answered 5xx.")
    , policy "p99 latency" (Monitoring.RequestLatencyP99 t.at_latencyP99Ms t.at_duration) ("The 99th-percentile request latency is above " <> Text.pack (show t.at_latencyP99Ms) <> " ms.")
    , policy "memory" (Monitoring.MemoryUtilization t.at_memoryUtilization t.at_duration) ("Container memory utilisation (p99) is above " <> percent t.at_memoryUtilization <> " of the limit.")
    ]
        <> [ policy "instances at max" (Monitoring.InstanceCount n t.at_duration) ("The service is running its maximum of " <> Text.pack (show n) <> " instances; requests may be queued or refused.")
           | Just n <- [cfg.cra_maxInstances]
           ]
  where
    t = cfg.cra_thresholds
    channel =
        Monitoring.NotificationChannel
            { Monitoring.ncProject = cfg.cra_project
            , Monitoring.ncDisplayName = cfg.cra_channelName
            , Monitoring.ncKind = Monitoring.Email cfg.cra_email
            }
    target =
        Monitoring.CloudRunTarget
            { Monitoring.crtProject = cfg.cra_project
            , Monitoring.crtRegion = cfg.cra_region
            , Monitoring.crtService = cfg.cra_service
            }
    policy name condition doc =
        Monitoring.AlertPolicy
            { Monitoring.apProject = cfg.cra_project
            , Monitoring.apDisplayName = cfg.cra_service <> ": " <> name
            , Monitoring.apTarget = target
            , Monitoring.apConditions = [condition]
            , Monitoring.apChannels = [channel]
            , Monitoring.apDocumentation = "Cloud Run service `" <> cfg.cra_service <> "` in " <> cfg.cra_region.regionName <> ": " <> doc
            }
    percent x = Text.pack (show (round (x * 100) :: Int)) <> "%"
