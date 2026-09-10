{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.LoadBalancing (
    Backend (..),
    HealthCheck (..),
    ApplicationLoadBalancer (..),
    applicationLoadBalancer,
    Report (..),
    LoadBalancingCommand (..),
    loadBalancingCommand,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject, withRegion)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunLoadBalancingCommand !LoadBalancingCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | A health check for instance-group backends.
data HealthCheck = HealthCheck
    { healthCheckName :: Text
    , healthCheckPort :: Int
    }
    deriving (Eq, Show)

-- | Backend kinds supported by the high-level recipe.
data Backend
    = InstanceGroupBackend Text [Int]
    | CloudRunBackend Text
    deriving (Eq, Show)

-- | A high-level HTTP(S) load balancer.
data ApplicationLoadBalancer = ApplicationLoadBalancer
    { albName :: Text
    , albProject :: Project
    , albRegion :: Region
    , albNetwork :: Maybe Text
    , albBackends :: [Backend]
    , albHealthCheck :: Maybe HealthCheck
    }
    deriving (Eq, Show)

-- | Creates the load-balancer sub-resources. This is intentionally a single
-- recipe node rather than forcing users to wire every component manually.
applicationLoadBalancer :: Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Op
applicationLoadBalancer r gcloudTrack alb =
    withBinary gcloudTrack loadBalancingCommand (LbCreate alb) $ \create ->
        withBinary gcloudTrack loadBalancingCommand (LbDescribe alb) $ \describe ->
            withBinary gcloudTrack loadBalancingCommand (LbDelete alb) $ \delete ->
                op "gcp-application-lb" nodeps $ \actions ->
                    actions
                        { help = Text.unwords ["creates application load balancer", alb.albName]
                        , ref = mkRef "gcp-application-lb" alb.albName
                        , up = create r'
                        , down = delete r'
                        , check = checkLb describe
                        }
  where
    r' = contramap (RunLoadBalancingCommand (LbCreate alb)) r

    checkLb :: (Reporter Binary.Report -> IO ()) -> IO CheckResult
    checkLb _describe = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare loadBalancingCommand (LbDescribe alb))
                ""
        pure $ case code of
            ExitSuccess -> Success
            ExitFailure n -> Failure ("load balancer not found (exit " <> Text.pack (show n) <> ")")

-------------------------------------------------------------------------------

data LoadBalancingCommand
    = LbCreate ApplicationLoadBalancer
    | LbDescribe ApplicationLoadBalancer
    | LbDelete ApplicationLoadBalancer
    deriving (Show)

loadBalancingCommand :: Command "gcloud" LoadBalancingCommand
loadBalancingCommand = Command $ \cmd -> case cmd of
    LbCreate alb ->
        -- For Phase 1 we create a simple regional HTTP load balancer using a
        -- single backend service and a URL map. Serverless NEGs and managed SSL
        -- certificates are created as separate gcloud calls in a bash script.
        gcloudProc
            [ "bash"
            , "-c"
            , Text.unpack (renderLbScript alb)
            ]
    LbDescribe alb ->
        gcloudProc $
            withProject alb.albProject
                ( withRegion alb.albRegion
                    [ "compute"
                    , "url-maps"
                    , "describe"
                    , Text.unpack (alb.albName <> "-url-map")
                    ]
                )
    LbDelete alb ->
        gcloudProc
            [ "bash"
            , "-c"
            , Text.unpack (renderLbDeleteScript alb)
            ]

-- | Renders a bash script that idempotently creates the LB components.
renderLbScript :: ApplicationLoadBalancer -> Text
renderLbScript alb =
    Text.unlines $
        [ "set -e"
        , "PROJECT=" <> alb.albProject.projectId
        , "REGION=" <> alb.albRegion.regionName
        , "NAME=" <> alb.albName
        ]
            <> healthCheckLines
            <> backendLines
            <> urlMapLines
            <> proxyLines
            <> forwardingRuleLines
  where
    healthCheckLines = case alb.albHealthCheck of
        Just hc ->
            [ "gcloud compute health-checks create tcp " <> hc.healthCheckName
                <> " --project=$PROJECT --region=$REGION --port="
                <> Text.pack (show hc.healthCheckPort)
                <> " || true"
            ]
        Nothing -> []

    backendLines = flip concatMap alb.albBackends $ \case
        InstanceGroupBackend ig ports ->
            [ "gcloud compute backend-services create " <> alb.albName <> "-backend"
                <> " --project=$PROJECT --region=$REGION --protocol=HTTP"
                <> maybe "" (" --network=" <>) alb.albNetwork
                <> maybe "" (\hc -> " --health-checks=" <> hc.healthCheckName) alb.albHealthCheck
                <> " || true"
            , "gcloud compute backend-services add-backend " <> alb.albName <> "-backend"
                <> " --project=$PROJECT --region=$REGION --instance-group=" <> ig
                <> " --instance-group-region=$REGION || true"
            ]
                <> map (\p -> "gcloud compute instance-groups set-named-ports " <> ig <> " --project=$PROJECT --region=$REGION --named-ports=http:" <> Text.pack (show p) <> " || true") ports
        CloudRunBackend svc ->
            [ "gcloud compute network-endpoint-groups create " <> alb.albName <> "-neg"
                <> " --project=$PROJECT --region=$REGION --network-endpoint-type=serverless --cloud-run-service=" <> svc
                <> " || true"
            , "gcloud compute backend-services create " <> alb.albName <> "-backend"
                <> " --project=$PROJECT --region=$REGION --protocol=HTTP"
                <> " || true"
            , "gcloud compute backend-services add-backend " <> alb.albName <> "-backend"
                <> " --project=$PROJECT --region=$REGION --network-endpoint-group=" <> alb.albName <> "-neg"
                <> " --network-endpoint-group-region=$REGION || true"
            ]

    urlMapLines =
        [ "gcloud compute url-maps create " <> alb.albName <> "-url-map"
            <> " --project=$PROJECT --region=$REGION --default-service=" <> alb.albName <> "-backend"
            <> " || true"
        ]

    proxyLines =
        [ "gcloud compute target-http-proxies create " <> alb.albName <> "-proxy"
            <> " --project=$PROJECT --region=$REGION --url-map=" <> alb.albName <> "-url-map"
            <> " || true"
        ]

    forwardingRuleLines =
        [ "gcloud compute forwarding-rules create " <> alb.albName <> "-fw"
            <> " --project=$PROJECT --region=$REGION --target-http-proxy=" <> alb.albName <> "-proxy"
            <> " --ports=80"
            <> " || true"
        ]

-- | Renders a bash script that deletes the LB components.
renderLbDeleteScript :: ApplicationLoadBalancer -> Text
renderLbDeleteScript alb =
    Text.unlines $
        [ "set -e"
        , "PROJECT=" <> alb.albProject.projectId
        , "REGION=" <> alb.albRegion.regionName
        , "NAME=" <> alb.albName
        , "gcloud compute forwarding-rules delete " <> alb.albName <> "-fw --project=$PROJECT --region=$REGION --quiet || true"
        , "gcloud compute target-http-proxies delete " <> alb.albName <> "-proxy --project=$PROJECT --region=$REGION --quiet || true"
        , "gcloud compute url-maps delete " <> alb.albName <> "-url-map --project=$PROJECT --region=$REGION --quiet || true"
        , "gcloud compute backend-services delete " <> alb.albName <> "-backend --project=$PROJECT --region=$REGION --quiet || true"
        ]
            <> deleteBackendSpecificLines
  where
    deleteBackendSpecificLines = flip concatMap alb.albBackends $ \case
        InstanceGroupBackend _ig _ports -> []
        CloudRunBackend _svc ->
            ["gcloud compute network-endpoint-groups delete " <> alb.albName <> "-neg --project=$PROJECT --region=$REGION --quiet || true"]
