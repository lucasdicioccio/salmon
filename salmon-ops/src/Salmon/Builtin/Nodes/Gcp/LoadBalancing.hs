{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.LoadBalancing (
    Backend (..),
    HealthCheck (..),
    ApplicationLoadBalancer (..),
    applicationLoadBalancer,
    interpretLbDescribe,
    shellQuote,
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
        withBinary gcloudTrack loadBalancingCommand (LbDelete alb) $ \delete ->
            op "gcp-application-lb" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates application load balancer", alb.albName]
                    , ref = mkRef "gcp-application-lb" (alb.albProject.projectId, alb.albRegion.regionName, alb.albName)
                    , up = create r'
                    , down = delete r'
                    , check = checkLb
                    }
  where
    r' = contramap (RunLoadBalancingCommand (LbCreate alb)) r

    checkLb :: IO CheckResult
    checkLb = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare loadBalancingCommand (LbDescribe alb))
                ""
        pure $ interpretLbDescribe code

-- | The verdict drawn from @gcloud compute url-maps describe@'s exit code,
-- split out for testability. This only tells us the URL map exists, not
-- that every sub-resource it points at is healthy -- see the module-level
-- note on richer LB checks.
interpretLbDescribe :: ExitCode -> CheckResult
interpretLbDescribe ExitSuccess = Success
interpretLbDescribe (ExitFailure n) = Failure ("load balancer not found (exit " <> Text.pack (show n) <> ")")

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
        --
        -- The script is run by @bash@ itself, not through 'gcloudProc' (which
        -- would run @gcloud bash -c ...@). 'Command' is still indexed by
        -- @"gcloud"@ because that is the binary the script needs on @PATH@.
        proc "bash" ["-c", Text.unpack (renderLbScript alb)]
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
        proc "bash" ["-c", Text.unpack (renderLbDeleteScript alb)]

{- | Single-quotes a value for safe interpolation into the generated bash
script (POSIX shell quoting: wrap in single quotes, escape embedded single
quotes as @'\''@). Every 'Text' that ends up in 'renderLbScript'\/
'renderLbDeleteScript' -- project id, region, ALB name, backend\/service
names -- must go through this: these scripts are run via @bash -c@, and
those values ultimately trace back to caller-supplied identifiers (e.g. a
tenant name in a multi-tenant recipe), not just author-typed literals.
-}
shellQuote :: Text -> Text
shellQuote t = "'" <> Text.replace "'" "'\\''" t <> "'"

{- | Renders a bash script that idempotently creates the LB components.

Every step is guarded by a @describe@ (or, for backend attachment, a look at
the backend service's current backends) rather than suffixed with
@|| true@: the latter made the script exit 0 whatever happened, so a
misconfigured balancer was reported as successfully brought up. Under
@set -e@ a failing create now fails the node, as the node-author conventions
require.

The balancer is a /regional external/ Application Load Balancer
(@EXTERNAL_MANAGED@), which GCP only accepts in a VPC network that already
has a proxy-only subnet in the region; this script does not create one.
-}
renderLbScript :: ApplicationLoadBalancer -> Text
renderLbScript alb =
    Text.unlines $
        [ "set -euo pipefail"
        , "PROJECT=" <> shellQuote alb.albProject.projectId
        , "REGION=" <> shellQuote alb.albRegion.regionName
        , "exists() { \"$@\" --project=\"$PROJECT\" --region=\"$REGION\" >/dev/null 2>&1; }"
        ]
            <> healthCheckLines
            <> backendLines
            <> urlMapLines
            <> proxyLines
            <> forwardingRuleLines
  where
    resourceName :: Text -> Text
    resourceName suffix = shellQuote (alb.albName <> suffix)

    ensure :: Text -> Text -> Text
    ensure describeCmd createCmd =
        "exists " <> describeCmd <> " || " <> createCmd

    backendName = resourceName "-backend"

    -- attaching the same backend twice is an error, so look first
    attachUnlessPresent :: Text -> Text -> Text
    attachUnlessPresent groupPathSuffix addCmd =
        "gcloud compute backend-services describe "
            <> backendName
            <> " --project=\"$PROJECT\" --region=\"$REGION\" --format='value(backends[].group)'"
            <> " | tr ';' '\\n' | grep -q -- "
            <> shellQuote (groupPathSuffix <> "$")
            <> " || "
            <> addCmd

    createBackendService :: Text
    createBackendService =
        ensure
            ("gcloud compute backend-services describe " <> backendName)
            ( "gcloud compute backend-services create " <> backendName
                <> " --project=\"$PROJECT\" --region=\"$REGION\" --protocol=HTTP --load-balancing-scheme=EXTERNAL_MANAGED"
                <> maybe "" (\hc -> " --health-checks=" <> shellQuote hc.healthCheckName <> " --health-checks-region=\"$REGION\"") (instanceGroupHealthCheck)
            )

    instanceGroupHealthCheck =
        if any isInstanceGroup alb.albBackends then alb.albHealthCheck else Nothing

    isInstanceGroup = \case
        InstanceGroupBackend _ _ -> True
        CloudRunBackend _ -> False

    healthCheckLines = case alb.albHealthCheck of
        Just hc ->
            [ ensure
                ("gcloud compute health-checks describe " <> shellQuote hc.healthCheckName)
                ( "gcloud compute health-checks create tcp " <> shellQuote hc.healthCheckName
                    <> " --project=\"$PROJECT\" --region=\"$REGION\" --port="
                    <> Text.pack (show hc.healthCheckPort)
                )
            ]
        Nothing -> []

    backendLines =
        createBackendService : flip concatMap alb.albBackends (\case
            InstanceGroupBackend ig ports ->
                namedPortsLine ig ports
                    <> [ attachUnlessPresent
                            ("/instanceGroups/" <> ig)
                            ( "gcloud compute backend-services add-backend " <> backendName
                                <> " --project=\"$PROJECT\" --region=\"$REGION\" --instance-group=" <> shellQuote ig
                                <> " --instance-group-region=\"$REGION\""
                            )
                       ]
            CloudRunBackend svc ->
                [ ensure
                    ("gcloud compute network-endpoint-groups describe " <> resourceName "-neg")
                    ( "gcloud compute network-endpoint-groups create " <> resourceName "-neg"
                        <> " --project=\"$PROJECT\" --region=\"$REGION\" --network-endpoint-type=serverless --cloud-run-service=" <> shellQuote svc
                    )
                , attachUnlessPresent
                    ("/networkEndpointGroups/" <> alb.albName <> "-neg")
                    ( "gcloud compute backend-services add-backend " <> backendName
                        <> " --project=\"$PROJECT\" --region=\"$REGION\" --network-endpoint-group=" <> resourceName "-neg"
                        <> " --network-endpoint-group-region=\"$REGION\""
                    )
                ])

    -- set-named-ports replaces the whole set, so one call carrying every
    -- port (the first one named @http@, the backend service's default
    -- @--port-name@) rather than one call per port, each erasing the last.
    namedPortsLine _ [] = []
    namedPortsLine ig ports =
        [ "gcloud compute instance-groups set-named-ports " <> shellQuote ig
            <> " --project=\"$PROJECT\" --region=\"$REGION\" --named-ports="
            <> Text.intercalate "," (zipWith namedPort [0 :: Int ..] ports)
        ]
    namedPort 0 p = "http:" <> Text.pack (show p)
    namedPort i p = "http-" <> Text.pack (show i) <> ":" <> Text.pack (show p)

    urlMapLines =
        [ ensure
            ("gcloud compute url-maps describe " <> resourceName "-url-map")
            ( "gcloud compute url-maps create " <> resourceName "-url-map"
                <> " --project=\"$PROJECT\" --region=\"$REGION\" --default-service=" <> backendName
            )
        ]

    proxyLines =
        [ ensure
            ("gcloud compute target-http-proxies describe " <> resourceName "-proxy")
            ( "gcloud compute target-http-proxies create " <> resourceName "-proxy"
                <> " --project=\"$PROJECT\" --region=\"$REGION\" --url-map=" <> resourceName "-url-map"
                <> " --url-map-region=\"$REGION\""
            )
        ]

    forwardingRuleLines =
        [ ensure
            ("gcloud compute forwarding-rules describe " <> resourceName "-fw")
            ( "gcloud compute forwarding-rules create " <> resourceName "-fw"
                <> " --project=\"$PROJECT\" --region=\"$REGION\" --load-balancing-scheme=EXTERNAL_MANAGED"
                <> maybe "" ((" --network=" <>) . shellQuote) alb.albNetwork
                <> " --target-http-proxy=" <> resourceName "-proxy"
                <> " --target-http-proxy-region=\"$REGION\""
                <> " --ports=80"
            )
        ]

{- | Renders a bash script that deletes the LB components, dependants first.
A component that is already gone is skipped; one that exists and fails to
delete fails the script.
-}
renderLbDeleteScript :: ApplicationLoadBalancer -> Text
renderLbDeleteScript alb =
    Text.unlines $
        [ "set -euo pipefail"
        , "PROJECT=" <> shellQuote alb.albProject.projectId
        , "REGION=" <> shellQuote alb.albRegion.regionName
        , "exists() { \"$@\" --project=\"$PROJECT\" --region=\"$REGION\" >/dev/null 2>&1; }"
        , deleteIfPresent "forwarding-rules" (resourceName "-fw")
        , deleteIfPresent "target-http-proxies" (resourceName "-proxy")
        , deleteIfPresent "url-maps" (resourceName "-url-map")
        , deleteIfPresent "backend-services" (resourceName "-backend")
        ]
            <> deleteBackendSpecificLines
            <> maybe [] (\hc -> [deleteIfPresent "health-checks" (shellQuote hc.healthCheckName)]) alb.albHealthCheck
  where
    resourceName :: Text -> Text
    resourceName suffix = shellQuote (alb.albName <> suffix)

    deleteIfPresent :: Text -> Text -> Text
    deleteIfPresent collection name =
        "if exists gcloud compute " <> collection <> " describe " <> name
            <> "; then gcloud compute " <> collection <> " delete " <> name
            <> " --project=\"$PROJECT\" --region=\"$REGION\" --quiet; fi"

    deleteBackendSpecificLines = flip concatMap alb.albBackends $ \case
        InstanceGroupBackend _ig _ports -> []
        CloudRunBackend _svc -> [deleteIfPresent "network-endpoint-groups" (resourceName "-neg")]
