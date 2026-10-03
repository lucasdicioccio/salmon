{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.LoadBalancing (
    Backend (..),
    InstanceGroupLocation (..),
    HealthCheck (..),
    BackendService (..),
    ServiceRef (..),
    HostRule (..),
    PathRule (..),
    Certificate (..),
    ApplicationLoadBalancer (..),
    httpLoadBalancer,
    applicationLoadBalancer,
    InvalidLoadBalancer (..),
    albProblems,
    certificateManagerApi,
    readAddress,
    DnsAuthorizationRecord (..),
    dnsAuthorizations,
    readDnsAuthorizationRecord,
    parseDnsAuthorizationRecord,
    renderUrlMap,
    interpretLbDescribe,
    interpretLbCheck,
    renderLbCheckScript,
    shellQuote,
    Report (..),
    LoadBalancingCommand (..),
    loadBalancingCommand,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.List (nub, (\\))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextErr
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

{- | Where an instance group lives, which is not a detail the balancer can
guess: an /unmanaged/ group is zonal and a /managed/ one is usually regional,
and every gcloud call naming the group -- @describe@, @set-named-ports@,
@add-backend@ -- wants the matching flag. Passing the balancer's own region
for both (which this module used to do) simply fails against the common case,
an unmanaged group holding VMs that already exist.
-}
data InstanceGroupLocation
    = InstanceGroupZone Text
    | InstanceGroupRegion Text
    deriving (Eq, Show)

{- | Backend kinds supported by the high-level recipe.

An instance group's ports are the ones the backend service naming it sends
to: the first is where the service's traffic goes, under a named port that
is the service's own (its resource name; any further port is that name with
@-1@, @-2@, ...). Several backend services may name one group, each with its
own port -- a VM backs a balancer through one group only, however many
services it runs. With no port at all the group is left alone and the
service keeps GCP's default port name, @http@, which is then the caller's to
have set.
-}
data Backend
    = InstanceGroupBackend Text InstanceGroupLocation [Int]
    | CloudRunBackend Text
    deriving (Eq, Show)

{- | One more backend service beside the balancer's default one, reachable
only through a 'HostRule' or 'PathRule' that names it. Its resource is
@\<balancer\>-\<name\>-backend@.

A backend service cannot mix instance groups and serverless NEGs, which is
one reason to have several rather than a longer 'albBackends'.
-}
data BackendService = BackendService
    { backendServiceName :: Text
    , backendServiceBackends :: [Backend]
    , backendServiceHealthCheck :: Maybe HealthCheck
    , backendServiceTimeoutSec :: Maybe Int
    -- ^ how long the balancer waits for a response; GCP's default is 30
    -- seconds, which a long-poll or an event stream outlives
    }
    deriving (Eq, Show)

-- | Which backend service a rule sends to.
data ServiceRef
    = DefaultService
    | NamedService Text
    deriving (Eq, Show)

-- | Requests whose path matches one of these patterns (@\/api\/*@) go there.
data PathRule = PathRule
    { pathRulePaths :: [Text]
    , pathRuleService :: ServiceRef
    }
    deriving (Eq, Show)

{- | Requests for one of these host names go to 'hostRuleService', except
those matching a 'PathRule'. A request for a host no rule names goes to the
balancer's default backend service.
-}
data HostRule = HostRule
    { hostRuleHosts :: [Text]
    , hostRuleService :: ServiceRef
    , hostRulePaths :: [PathRule]
    }
    deriving (Eq, Show)

{- | A certificate for the HTTPS proxy.

A /regional/ Application Load Balancer cannot use the classic
Google-managed @compute ssl-certificates@ (those are global only, and are
the ones that provision "once DNS points at the balancer"). What it can use
is a regional __Certificate Manager__ certificate, which Google issues
against a /DNS authorization/: a @CNAME@ the domain's zone has to carry,
whose content GCP picks. 'ManagedCertificate' creates one authorization per
domain and the certificate over them; publishing the records is the
caller's (see 'readDnsAuthorizationRecord'), and until they resolve the
certificate stays @PROVISIONING@ and the node's check says 'Unknown'.

'ComputeCertificate' names a regional @compute ssl-certificates@ resource
somebody else made (a self-managed one). The two kinds do not mix on one
proxy.
-}
data Certificate
    = ManagedCertificate Text [Text]
    | ComputeCertificate Text
    deriving (Eq, Show)

-- | A high-level HTTP(S) load balancer.
data ApplicationLoadBalancer = ApplicationLoadBalancer
    { albName :: Text
    , albProject :: Project
    , albRegion :: Region
    , albNetwork :: Maybe Text
    , albBackends :: [Backend]
    -- ^ the default backend service's (@\<balancer\>-backend@)
    , albHealthCheck :: Maybe HealthCheck
    , albTimeoutSec :: Maybe Int
    -- ^ the default backend service's response timeout
    , albServices :: [BackendService]
    , albHostRules :: [HostRule]
    , albCertificates :: [Certificate]
    -- ^ non-empty: also serve HTTPS on :443, with both forwarding rules on
    -- one reserved address (@\<balancer\>-ip@)
    }
    deriving (Eq, Show)

{- | A plain-HTTP balancer with one backend service and no rules: what this
module made before it knew about the rest. Record-update it for more.
-}
httpLoadBalancer :: Text -> Project -> Region -> [Backend] -> Maybe HealthCheck -> ApplicationLoadBalancer
httpLoadBalancer name project region backends hc =
    ApplicationLoadBalancer
        { albName = name
        , albProject = project
        , albRegion = region
        , albNetwork = Nothing
        , albBackends = backends
        , albHealthCheck = hc
        , albTimeoutSec = Nothing
        , albServices = []
        , albHostRules = []
        , albCertificates = []
        }

-- | The API a 'ManagedCertificate' needs enabled (the caller's dependency).
certificateManagerApi :: Text
certificateManagerApi = "certificatemanager.googleapis.com"

-- | A declaration no gcloud call could satisfy, refused before any is made.
newtype InvalidLoadBalancer = InvalidLoadBalancer [Text]
    deriving (Show)

instance Exception InvalidLoadBalancer

{- | What is wrong with a declaration, all of it rather than the first: a
rule naming a service nobody declared, two services under one name, a host
in two rules, a rule with no host or no path, the two certificate kinds
mixed, a managed certificate with no domain, a timeout that is not positive,
one backend service sending to two different ports of one instance group.
-}
albProblems :: ApplicationLoadBalancer -> [Text]
albProblems alb =
    [ "backend service declared twice: " <> n
    | n <- nub (names \\ nub names)
    ]
        <> [ "rule names an undeclared backend service: " <> n
           | NamedService n <- nub refs
           , n `notElem` names
           ]
        <> ["a host rule names no host" | any (null . hostRuleHosts) alb.albHostRules]
        <> ["a path rule names no path" | any (null . pathRulePaths) (concatMap hostRulePaths alb.albHostRules)]
        <> [ "host named by two rules: " <> h
           | h <- nub (hosts \\ nub hosts)
           ]
        <> [ "Certificate Manager and compute certificates cannot share a proxy"
           | not (null [() | ManagedCertificate{} <- alb.albCertificates])
           , not (null [() | ComputeCertificate{} <- alb.albCertificates])
           ]
        <> [ "managed certificate " <> n <> " names no domain"
           | ManagedCertificate n [] <- alb.albCertificates
           ]
        <> [ "instance group " <> ig <> ": named port " <> n <> " declared on several ports: " <> Text.unwords (map (Text.pack . show) ps)
           | ((ig, _), named) <- groupNamedPorts alb
           , n <- nub (map fst named)
           , let ps = nub [p | (n', p) <- named, n' == n]
           , length ps > 1
           ]
        <> [ "timeout must be positive: " <> Text.pack (show t)
           | Just t <- alb.albTimeoutSec : map backendServiceTimeoutSec alb.albServices
           , t <= 0
           ]
  where
    names = map backendServiceName alb.albServices
    hosts = concatMap hostRuleHosts alb.albHostRules
    refs =
        concat
            [ r.hostRuleService : map pathRuleService r.hostRulePaths
            | r <- alb.albHostRules
            ]

-- | Creates the load-balancer sub-resources. This is intentionally a single
-- recipe node rather than forcing users to wire every component manually.
applicationLoadBalancer :: Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Op
applicationLoadBalancer r gcloudTrack alb =
    withBinary gcloudTrack loadBalancingCommand (LbCreate alb) $ \create ->
        withBinary gcloudTrack loadBalancingCommand (LbDelete alb) $ \delete ->
            op "gcp-application-lb" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates application load balancer", alb.albName]
                    , notes = albNotes alb
                    , ref = mkRef "gcp-application-lb" (alb.albProject.projectId, alb.albRegion.regionName, alb.albName)
                    , up = refuseInvalid >> create r'
                    , down = delete r'
                    , check = checkLb
                    }
  where
    r' = contramap (RunLoadBalancingCommand (LbCreate alb)) r

    problems = albProblems alb

    refuseInvalid :: IO ()
    refuseInvalid = unless (null problems) $ throwIO (InvalidLoadBalancer problems)

    checkLb :: IO CheckResult
    checkLb
        | not (null problems) = pure (Failure ("invalid load balancer: " <> Text.intercalate "; " problems))
        | otherwise = do
            (code, out, _err) <-
                readCreateProcessWithExitCode
                    (prepare loadBalancingCommand (LbCheck alb))
                    ""
            pure $ interpretLbCheck code (Text.decodeUtf8With TextErr.lenientDecode out)

{- | What a re-declaration can be seen to change: the hosts served, the
named services, the timeouts, the port each service sends to, the
certificates. Empty for a balancer with none of those.
-}
albNotes :: ApplicationLoadBalancer -> [Text]
albNotes alb =
    [ "hosts " <> Text.unwords r.hostRuleHosts
    | r <- alb.albHostRules
    ]
        <> [ "service " <> svc.svcResource <> maybe "" (\t -> " timeout " <> Text.pack (show t) <> "s") svc.svcTimeout
           | svc <- services alb
           , svc.svcLabel /= "backend" || svc.svcTimeout /= Nothing
           ]
        <> [ "named port " <> n <> ":" <> Text.pack (show p) <> " on " <> ig
           | ((ig, _), named) <- groupNamedPorts alb
           , (n, p) <- nub named
           ]
        <> map certNote alb.albCertificates
  where
    certNote = \case
        ManagedCertificate n ds -> "managed certificate " <> n <> " for " <> Text.unwords ds
        ComputeCertificate n -> "certificate " <> n

{- | The address GCP gave the balancer's forwarding rule. 'Nothing' when the
rule does not exist yet.

Out of graph, like "Salmon.Builtin.Nodes.Gcp.Compute".@readAddress@: the
address is picked at creation, so nothing can name it when the graph is
declared. It is what a DNS record pointing at the balancer resolves at @up@
(see "Salmon.Builtin.Nodes.Gcp.CloudDns".@resolvedRecordSet@).
-}
readAddress :: ApplicationLoadBalancer -> IO (Maybe Text)
readAddress alb = do
    (code, out, _err) <-
        readCreateProcessWithExitCode (prepare loadBalancingCommand (LbAddressDescribe alb)) ""
    let ip = Text.strip (Text.decodeUtf8With TextErr.lenientDecode out)
    pure $ case code of
        ExitSuccess | not (Text.null ip) -> Just ip
        _ -> Nothing

-- | The record a DNS authorization asks the domain's zone to carry.
data DnsAuthorizationRecord = DnsAuthorizationRecord
    { authorizationRecordName :: Text
    , authorizationRecordType :: Text
    , authorizationRecordData :: Text
    }
    deriving (Eq, Show)

{- | The DNS authorizations a balancer's managed certificates need, as
@(domain, authorization name)@. The name is derived from the certificate's
and the domain's, so the node that creates one and the reader that asks for
its record compute the same thing.
-}
dnsAuthorizations :: ApplicationLoadBalancer -> [(Text, Text)]
dnsAuthorizations alb =
    [ (d, authorizationName n d)
    | ManagedCertificate n ds <- alb.albCertificates
    , d <- ds
    ]

authorizationName :: Text -> Text -> Text
authorizationName cert domain =
    cert <> "-" <> Text.map (\c -> if c == '.' || c == '*' then '-' else c) (Text.toLower domain)

{- | The record GCP wants published for one authorization (by its name, see
'dnsAuthorizations'). 'Nothing' until the authorization exists, i.e. until
the balancer's @up@ has run -- out of graph for the reason 'readAddress' is.
-}
readDnsAuthorizationRecord :: ApplicationLoadBalancer -> Text -> IO (Maybe DnsAuthorizationRecord)
readDnsAuthorizationRecord alb authz = do
    (code, out, _err) <-
        readCreateProcessWithExitCode (prepare loadBalancingCommand (LbDnsAuthorizationDescribe alb authz)) ""
    pure $ case code of
        ExitSuccess -> parseDnsAuthorizationRecord (Text.decodeUtf8With TextErr.lenientDecode out)
        _ -> Nothing

{- | Reads @describe --format=value(dnsResourceRecord.name,
dnsResourceRecord.type,dnsResourceRecord.data)@: three tab-separated
fields on one line.
-}
parseDnsAuthorizationRecord :: Text -> Maybe DnsAuthorizationRecord
parseDnsAuthorizationRecord out =
    case filter (not . Text.null) (map Text.strip (Text.lines out)) of
        [l] -> case filter (not . Text.null) (map Text.strip (Text.splitOn "\t" l)) of
            [n, t, d] -> Just (DnsAuthorizationRecord n t d)
            _ -> Nothing
        _ -> Nothing

-- | The verdict drawn from @gcloud compute url-maps describe@'s exit code.
-- This only tells us the URL map exists; 'interpretLbCheck' is what the node
-- itself uses and looks at every sub-resource and the backends' health.
interpretLbDescribe :: ExitCode -> CheckResult
interpretLbDescribe ExitSuccess = Success
interpretLbDescribe (ExitFailure n) = Failure ("load balancer not found (exit " <> Text.pack (show n) <> ")")

{- | The verdict drawn from 'renderLbCheckScript''s exit code and stdout.

The script prints @MISSING <what>@ for each absent sub-resource, unattached
backend, unrouted host, wrong timeout, wrong port name or named port absent
from its instance group, @HEALTH <service> <state>@ per
backend instance of an instance-group backend, and @CERT <name> <state>@ per
managed certificate. A missing piece is a 'Failure' (the node's @up@ is
idempotent and will create it), and so is a certificate GCP reports
@FAILED@. Backends that are not (yet) @HEALTHY@ are 'Unknown': a freshly
brought-up balancer reports @UNHEALTHY@ for roughly two minutes, and
re-running @up@ would not shorten that; a certificate still @PROVISIONING@
is 'Unknown' for the same reason. A Cloud Run (NEG) backend has no health to
ask for, so its presence and attachment is the whole check.
-}
interpretLbCheck :: ExitCode -> Text -> CheckResult
interpretLbCheck (ExitFailure n) _ = Failure ("load balancer check failed (exit " <> Text.pack (show n) <> ")")
interpretLbCheck ExitSuccess out
    | not (null missing) = Failure ("load balancer incomplete: missing " <> Text.intercalate ", " missing)
    | not (null failedCerts) = Failure ("certificate provisioning failed: " <> Text.intercalate ", " failedCerts)
    | not (null unhealthy) || not (null pendingCerts) = Unknown
    | otherwise = Success
  where
    ls = map Text.words (Text.lines out)
    missing = [Text.unwords rest | ("MISSING" : rest) <- ls]
    unhealthy = [st | ["HEALTH", _, st] <- ls, st /= "HEALTHY"]
    failedCerts = [n | ["CERT", n, "FAILED"] <- ls]
    pendingCerts = [n | ["CERT", n, st] <- ls, st /= "ACTIVE", st /= "FAILED"]

-------------------------------------------------------------------------------

data LoadBalancingCommand
    = LbCreate ApplicationLoadBalancer
    | LbCheck ApplicationLoadBalancer
    | LbDescribe ApplicationLoadBalancer
    | LbDelete ApplicationLoadBalancer
    | LbAddressDescribe ApplicationLoadBalancer
    | LbDnsAuthorizationDescribe ApplicationLoadBalancer Text
    deriving (Show)

loadBalancingCommand :: Command "gcloud" LoadBalancingCommand
loadBalancingCommand = Command $ \cmd -> case cmd of
    LbCreate alb ->
        -- One bash script of guarded gcloud calls. It is run by @bash@
        -- itself, not through 'gcloudProc' (which would run
        -- @gcloud bash -c ...@). 'Command' is still indexed by @"gcloud"@
        -- because that is the binary the script needs on @PATH@.
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
    LbCheck alb ->
        proc "bash" ["-c", Text.unpack (renderLbCheckScript alb)]
    LbAddressDescribe alb ->
        gcloudProc $
            withProject alb.albProject
                ( withRegion alb.albRegion
                    [ "compute"
                    , "forwarding-rules"
                    , "describe"
                    , -- with HTTPS on, both rules sit on one reserved
                      -- address, and the HTTPS one is the one that matters
                      Text.unpack (alb.albName <> if serveHttps alb then "-https-fw" else "-fw")
                    , "--format"
                    , "value(IPAddress)"
                    ]
                )
    LbDnsAuthorizationDescribe alb authz ->
        gcloudProc $
            withProject
                alb.albProject
                [ "certificate-manager"
                , "dns-authorizations"
                , "describe"
                , Text.unpack authz
                , "--format"
                , "value(dnsResourceRecord.name,dnsResourceRecord.type,dnsResourceRecord.data)"
                , "--location"
                , Text.unpack alb.albRegion.regionName
                ]
    LbDelete alb ->
        proc "bash" ["-c", Text.unpack (renderLbDeleteScript alb)]

serveHttps :: ApplicationLoadBalancer -> Bool
serveHttps = not . null . albCertificates

{- | A backend service as the scripts see it: the default one and the named
ones, under their resource names.
-}
data Svc = Svc
    { svcLabel :: Text
    -- ^ what a @HEALTH@ line calls it
    , svcResource :: Text
    , svcNeg :: Text
    , svcBackends :: [Backend]
    , svcHealthCheck :: Maybe HealthCheck
    , svcTimeout :: Maybe Int
    }

services :: ApplicationLoadBalancer -> [Svc]
services alb =
    Svc "backend" (alb.albName <> "-backend") (alb.albName <> "-neg") alb.albBackends alb.albHealthCheck alb.albTimeoutSec
        : map named alb.albServices
  where
    named :: BackendService -> Svc
    named s =
        Svc
            s.backendServiceName
            (serviceResource alb (NamedService s.backendServiceName))
            (alb.albName <> "-" <> s.backendServiceName <> "-neg")
            s.backendServiceBackends
            s.backendServiceHealthCheck
            s.backendServiceTimeoutSec

serviceResource :: ApplicationLoadBalancer -> ServiceRef -> Text
serviceResource alb = \case
    DefaultService -> alb.albName <> "-backend"
    NamedService n -> alb.albName <> "-" <> n <> "-backend"

healthChecks :: ApplicationLoadBalancer -> [HealthCheck]
healthChecks alb = nub [hc | Just hc <- map svcHealthCheck (services alb)]

{- | The named port a backend service sends to, when it has an instance
group to send to and a port declared on it: the service's own resource name.

It used to be GCP's default, @http@, for every service -- so two services on
one instance group both sent to whichever port @http@ was last set to, with
healthy backends and no error anywhere. A name per service is what lets one
group carry a port for each.
-}
svcPortName :: Svc -> Maybe Text
svcPortName svc
    | null [() | InstanceGroupBackend _ _ (_ : _) <- svc.svcBackends] = Nothing
    | otherwise = Just svc.svcResource

{- | The named ports this balancer wants on each instance group: the union
over every backend service naming the group, in declaration order. One entry
per group, because @set-named-ports@ replaces a group's whole set and so
cannot be called once per service.
-}
groupNamedPorts :: ApplicationLoadBalancer -> [((Text, InstanceGroupLocation), [(Text, Int)])]
groupNamedPorts alb =
    [ (g, concat [named | (g', named) <- perBackend, g' == g])
    | g <- nub (map fst perBackend)
    ]
  where
    perBackend =
        [ ((ig, loc), zipWith (portName svc.svcResource) [0 :: Int ..] ports)
        | svc <- services alb
        , InstanceGroupBackend ig loc ports@(_ : _) <- svc.svcBackends
        ]
    portName base 0 p = (base, p)
    portName base i p = (base <> "-" <> Text.pack (show i), p)

isInstanceGroup :: Backend -> Bool
isInstanceGroup = \case
    InstanceGroupBackend{} -> True
    CloudRunBackend _ -> False

{- | The URL map as the resource @gcloud compute url-maps import@ reads.
JSON, which is YAML: one path matcher per host rule, named by position.
-}
renderUrlMap :: ApplicationLoadBalancer -> Aeson.Value
renderUrlMap alb =
    Aeson.object
        [ "name" Aeson..= (alb.albName <> "-url-map")
        , "defaultService" Aeson..= serviceUrl DefaultService
        , "hostRules" Aeson..= map hostRule rules
        , "pathMatchers" Aeson..= map pathMatcher rules
        ]
  where
    rules = zip [0 :: Int ..] alb.albHostRules
    matcher :: Int -> Text
    matcher i = "m" <> Text.pack (show i)
    hostRule :: (Int, HostRule) -> Aeson.Value
    hostRule (i, r) =
        Aeson.object ["hosts" Aeson..= r.hostRuleHosts, "pathMatcher" Aeson..= matcher i]
    pathMatcher :: (Int, HostRule) -> Aeson.Value
    pathMatcher (i, r) =
        Aeson.object $
            [ "name" Aeson..= matcher i
            , "defaultService" Aeson..= serviceUrl r.hostRuleService
            ]
                <> ["pathRules" Aeson..= map pathRule r.hostRulePaths | not (null r.hostRulePaths)]
    pathRule :: PathRule -> Aeson.Value
    pathRule p =
        Aeson.object ["paths" Aeson..= p.pathRulePaths, "service" Aeson..= serviceUrl p.pathRuleService]
    serviceUrl :: ServiceRef -> Text
    serviceUrl sref =
        "https://www.googleapis.com/compute/v1/projects/"
            <> alb.albProject.projectId
            <> "/regions/"
            <> alb.albRegion.regionName
            <> "/backendServices/"
            <> serviceResource alb sref

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

Some things are /set/ on every run rather than guarded, because they are
declarations that can change under a resource that already exists: a
backend service's timeout and port name (@update --timeout@,
@update --port-name@), an instance group's named ports and, when there are
host rules, the whole URL map (@url-maps import@, which replaces it).

An instance group's named ports are set once per group, before any backend
service is pointed at one of them, and /merged/ with what the group already
carries: @set-named-ports@ replaces the whole set, and the group is the
caller's -- another balancer, or the caller, may have named ports on it.
Only the names this balancer declares are overwritten. Nothing removes a
name: one this balancer stopped declaring stays on the group, where it does
no harm, and so does everything on @down@.

The balancer is a /regional external/ Application Load Balancer
(@EXTERNAL_MANAGED@), which GCP only accepts in a VPC network that already
has a proxy-only subnet in the region. This script does not create one --
see "Salmon.Builtin.Nodes.Gcp.Compute".@subnet@ with
'Salmon.Builtin.Nodes.Gcp.Compute.RegionalManagedProxy', which is the node to
put underneath this one.
-}
renderLbScript :: ApplicationLoadBalancer -> Text
renderLbScript alb =
    Text.unlines $
        [ "set -euo pipefail"
        , "PROJECT=" <> shellQuote alb.albProject.projectId
        , "REGION=" <> shellQuote alb.albRegion.regionName
        , -- A bare predicate: every caller appends its own location flags,
          -- because not every resource named here is regional (an unmanaged
          -- instance group is zonal) and this used to append --region to all
          -- of them.
          "exists() { \"$@\" >/dev/null 2>&1; }"
        ]
            <> healthCheckLines
            <> concatMap namedPortsLines (groupNamedPorts alb)
            <> concatMap backendLines (services alb)
            <> urlMapLines
            <> certificateLines
            <> addressLines
            <> proxyLines
            <> forwardingRuleLines
  where
    resourceName :: Text -> Text
    resourceName suffix = shellQuote (alb.albName <> suffix)

    ensure :: Text -> Text -> Text
    ensure describeCmd createCmd =
        "exists " <> describeCmd <> " || " <> createCmd

    -- attaching the same backend twice is an error, so look first
    attachUnlessPresent :: Svc -> Text -> Text -> Text
    attachUnlessPresent svc groupPathSuffix addCmd =
        "gcloud compute backend-services describe "
            <> shellQuote svc.svcResource
            <> regional
            <> " --format='value(backends[].group)'"
            <> " | tr ';' '\\n' | grep -q -- "
            <> shellQuote (groupPathSuffix <> "$")
            <> " || "
            <> addCmd

    createBackendService :: Svc -> Text
    createBackendService svc =
        ensure
            ("gcloud compute backend-services describe " <> shellQuote svc.svcResource <> regional)
            ( "gcloud compute backend-services create " <> shellQuote svc.svcResource
                <> regional
                <> " --protocol=HTTP"
                <> maybe "" ((" --port-name=" <>) . shellQuote) (svcPortName svc)
                <> " --load-balancing-scheme=EXTERNAL_MANAGED"
                <> maybe "" (\hc -> " --health-checks=" <> shellQuote hc.healthCheckName <> " --health-checks-region=\"$REGION\"") (instanceGroupHealthCheck svc)
            )

    -- a set, not a create flag: the declared timeout has to reach a
    -- backend service that already exists too
    timeoutLines :: Svc -> [Text]
    timeoutLines svc = case svc.svcTimeout of
        Nothing -> []
        Just t ->
            [ "gcloud compute backend-services update " <> shellQuote svc.svcResource
                <> regional
                <> " --timeout="
                <> Text.pack (show t)
            ]

    -- a set too: a backend service made before it had a port name of its
    -- own is still on @http@
    portNameLines :: Svc -> [Text]
    portNameLines svc =
        [ "gcloud compute backend-services update " <> shellQuote svc.svcResource
            <> regional
            <> " --port-name="
            <> shellQuote n
        | Just n <- [svcPortName svc]
        ]

    instanceGroupHealthCheck :: Svc -> Maybe HealthCheck
    instanceGroupHealthCheck svc =
        if any isInstanceGroup svc.svcBackends then svc.svcHealthCheck else Nothing

    healthCheckLines =
        [ ensure
            ("gcloud compute health-checks describe " <> shellQuote hc.healthCheckName <> regional)
            ( "gcloud compute health-checks create tcp " <> shellQuote hc.healthCheckName
                <> regional
                <> " --port="
                <> Text.pack (show hc.healthCheckPort)
            )
        | hc <- healthChecks alb
        ]

    backendLines :: Svc -> [Text]
    backendLines svc =
        createBackendService svc : portNameLines svc <> timeoutLines svc <> concatMap (attachLines svc) svc.svcBackends

    attachLines :: Svc -> Backend -> [Text]
    attachLines svc = \case
        InstanceGroupBackend ig loc _ ->
            [ attachUnlessPresent
                svc
                ("/instanceGroups/" <> ig)
                ( "gcloud compute backend-services add-backend " <> shellQuote svc.svcResource
                    <> regional
                    <> " --instance-group=" <> shellQuote ig
                    <> groupBackendFlag loc
                )
            ]
        CloudRunBackend cr ->
            [ ensure
                ("gcloud compute network-endpoint-groups describe " <> shellQuote svc.svcNeg <> regional)
                ( "gcloud compute network-endpoint-groups create " <> shellQuote svc.svcNeg
                    <> regional
                    <> " --network-endpoint-type=serverless --cloud-run-service=" <> shellQuote cr
                )
            , attachUnlessPresent
                svc
                ("/networkEndpointGroups/" <> svc.svcNeg)
                ( "gcloud compute backend-services add-backend " <> shellQuote svc.svcResource
                    <> regional
                    <> " --network-endpoint-group=" <> shellQuote svc.svcNeg
                    <> " --network-endpoint-group-region=\"$REGION\""
                )
            ]

    -- set-named-ports replaces the whole set, so: one call per group,
    -- carrying every port of every service naming it, after whatever the
    -- group already has under names that are not ours. The read is an
    -- assignment on a line of its own so that its failing fails the script
    -- (a command substitution inside an argument would not).
    namedPortsLines :: ((Text, InstanceGroupLocation), [(Text, Int)]) -> [Text]
    namedPortsLines ((ig, loc), named) =
        [ "keep=$(gcloud compute instance-groups get-named-ports " <> shellQuote ig
            <> groupLocation loc
            <> " --format='value(name,port)' | awk -v ours="
            <> shellQuote (Text.unwords (map fst named))
            <> " "
            <> shellQuote "BEGIN{n=split(ours,x,\" \");for(i=1;i<=n;i++)o[x[i]]=1} NF==2&&!($1 in o){printf \"%s:%s,\",$1,$2}"
            <> ")"
        , "gcloud compute instance-groups set-named-ports " <> shellQuote ig
            <> groupLocation loc
            <> " --named-ports=\"${keep}\""
            <> shellQuote (Text.intercalate "," [n <> ":" <> Text.pack (show p) | (n, p) <- nub named])
        ]

    -- With rules the map is imported whole on every run: `import` creates
    -- or replaces, which is the only "set" verb a URL map has
    -- (`add-path-matcher` appends, and fails the second time). Without
    -- rules it is created once with its default service, as before.
    urlMapLines
        | null alb.albHostRules =
            [ ensure
                ("gcloud compute url-maps describe " <> resourceName "-url-map" <> regional)
                ( "gcloud compute url-maps create " <> resourceName "-url-map"
                    <> regional
                    <> " --default-service=" <> resourceName "-backend"
                )
            ]
        | otherwise =
            [ "printf '%s\\n' " <> shellQuote (urlMapText alb)
                <> " | gcloud compute url-maps import " <> resourceName "-url-map"
                <> regional
                <> " --quiet"
            ]

    certificateLines = flip concatMap alb.albCertificates $ \case
        ComputeCertificate _ -> []
        ManagedCertificate n ds ->
            [ ensure
                ("gcloud certificate-manager dns-authorizations describe " <> shellQuote (authorizationName n d) <> located)
                ( "gcloud certificate-manager dns-authorizations create " <> shellQuote (authorizationName n d)
                    <> located
                    <> " --domain=" <> shellQuote d
                    <> " --type=PER_PROJECT_RECORD"
                )
            | d <- ds
            ]
                <> [ ensure
                        ("gcloud certificate-manager certificates describe " <> shellQuote n <> located)
                        ( "gcloud certificate-manager certificates create " <> shellQuote n
                            <> located
                            <> " --domains=" <> shellQuote (Text.intercalate "," ds)
                            <> " --dns-authorizations=" <> shellQuote (Text.intercalate "," (map (authorizationName n) ds))
                        )
                   ]

    -- Two forwarding rules can only share an address that is reserved.
    addressLines =
        [ ensure
            ("gcloud compute addresses describe " <> resourceName "-ip" <> regional)
            ("gcloud compute addresses create " <> resourceName "-ip" <> regional)
        | serveHttps alb
        ]

    addressFlag
        | serveHttps alb = " --address=" <> resourceName "-ip" <> " --address-region=\"$REGION\""
        | otherwise = ""

    proxyLines =
        [ ensure
            ("gcloud compute target-http-proxies describe " <> resourceName "-proxy" <> regional)
            ( "gcloud compute target-http-proxies create " <> resourceName "-proxy"
                <> regional
                <> " --url-map=" <> resourceName "-url-map"
                <> " --url-map-region=\"$REGION\""
            )
        ]
            <> [ ensure
                    ("gcloud compute target-https-proxies describe " <> resourceName "-https-proxy" <> regional)
                    ( "gcloud compute target-https-proxies create " <> resourceName "-https-proxy"
                        <> regional
                        <> " --url-map=" <> resourceName "-url-map"
                        <> " --url-map-region=\"$REGION\""
                        <> certificateFlags
                    )
               | serveHttps alb
               ]

    certificateFlags =
        case ([n | ManagedCertificate n _ <- alb.albCertificates], [n | ComputeCertificate n <- alb.albCertificates]) of
            (ms@(_ : _), _) -> " --certificate-manager-certificates=" <> shellQuote (Text.intercalate "," ms)
            ([], cs) -> " --ssl-certificates=" <> shellQuote (Text.intercalate "," cs) <> " --ssl-certificates-region=\"$REGION\""

    forwardingRuleLines =
        [ ensure
            ("gcloud compute forwarding-rules describe " <> resourceName "-fw" <> regional)
            ( "gcloud compute forwarding-rules create " <> resourceName "-fw"
                <> regional
                <> " --load-balancing-scheme=EXTERNAL_MANAGED"
                <> maybe "" ((" --network=" <>) . shellQuote) alb.albNetwork
                <> addressFlag
                <> " --target-http-proxy=" <> resourceName "-proxy"
                <> " --target-http-proxy-region=\"$REGION\""
                <> " --ports=80"
            )
        ]
            <> [ ensure
                    ("gcloud compute forwarding-rules describe " <> resourceName "-https-fw" <> regional)
                    ( "gcloud compute forwarding-rules create " <> resourceName "-https-fw"
                        <> regional
                        <> " --load-balancing-scheme=EXTERNAL_MANAGED"
                        <> maybe "" ((" --network=" <>) . shellQuote) alb.albNetwork
                        <> addressFlag
                        <> " --target-https-proxy=" <> resourceName "-https-proxy"
                        <> " --target-https-proxy-region=\"$REGION\""
                        <> " --ports=443"
                    )
               | serveHttps alb
               ]

urlMapText :: ApplicationLoadBalancer -> Text
urlMapText = Text.decodeUtf8With TextErr.lenientDecode . LByteString.toStrict . Aeson.encode . renderUrlMap

{- | Renders a read-only bash script that describes every sub-resource the
create script makes and asks the backend services for their health. It
always exits 0 unless the script itself breaks; findings are lines on stdout
(see 'interpretLbCheck').

What it does not see: a path rule, or which service a host is sent to. A
host rule is checked by its hosts being in the map, no further. Which port a
service reaches it does see: the service's port name, and that name on each
instance group it sends to.
-}
renderLbCheckScript :: ApplicationLoadBalancer -> Text
renderLbCheckScript alb =
    Text.unlines $
        [ "set -uo pipefail"
        , "PROJECT=" <> shellQuote alb.albProject.projectId
        , "REGION=" <> shellQuote alb.albRegion.regionName
        , "exists() { \"$@\" >/dev/null 2>&1; }"
        , "need() { local what=\"$1\"; shift; exists \"$@\" || echo \"MISSING $what\"; }"
        ]
            <> map hcLine (healthChecks alb)
            <> map (needNamed "backend-services" . svcResource) svcs
            <> [ need' "url-maps" "-url-map"
               , need' "target-http-proxies" "-proxy"
               , need' "forwarding-rules" "-fw"
               ]
            <> httpsLines
            <> concatMap timeoutLines svcs
            <> concatMap portNameLines svcs
            <> concatMap namedPortLines (groupNamedPorts alb)
            <> hostLines
            <> concatMap (\svc -> concatMap (backendLines svc) svc.svcBackends) svcs
            <> [healthLines svc | svc <- svcs, any isInstanceGroup svc.svcBackends]
  where
    svcs = services alb
    q :: Text -> Text
    q suffix = shellQuote (alb.albName <> suffix)
    need' :: Text -> Text -> Text
    need' coll suffix = needNamed coll (alb.albName <> suffix)
    needNamed :: Text -> Text -> Text
    needNamed coll name =
        "need " <> shellQuote (coll <> " " <> name)
            <> " gcloud compute " <> coll <> " describe " <> shellQuote name <> regional
    hcLine :: HealthCheck -> Text
    hcLine hc = needNamed "health-checks" hc.healthCheckName
    httpsLines :: [Text]
    httpsLines
        | not (serveHttps alb) = []
        | otherwise =
            [ need' "addresses" "-ip"
            , need' "target-https-proxies" "-https-proxy"
            , need' "forwarding-rules" "-https-fw"
            ]
                <> concatMap certLines alb.albCertificates
    certLines :: Certificate -> [Text]
    certLines = \case
        ComputeCertificate n -> [needNamed "ssl-certificates" n]
        ManagedCertificate n ds ->
            [ "need " <> shellQuote ("dns-authorization " <> authorizationName n d)
                <> " gcloud certificate-manager dns-authorizations describe " <> shellQuote (authorizationName n d) <> located
            | d <- ds
            ]
                <> [ "need " <> shellQuote ("certificate " <> n)
                        <> " gcloud certificate-manager certificates describe " <> shellQuote n <> located
                   , "st=$(gcloud certificate-manager certificates describe " <> shellQuote n <> located
                        <> " --format='value(managed.state)' 2>/dev/null); [ -n \"$st\" ] && echo "
                        <> shellQuote ("CERT " <> n)
                        <> "\" $st\"; true"
                   ]
    timeoutLines :: Svc -> [Text]
    timeoutLines svc = case svc.svcTimeout of
        Nothing -> []
        Just t ->
            [ "[ \"$(gcloud compute backend-services describe " <> shellQuote svc.svcResource <> regional
                <> " --format='value(timeoutSec)' 2>/dev/null)\" = "
                <> shellQuote (Text.pack (show t))
                <> " ] || echo "
                <> shellQuote ("MISSING timeout " <> Text.pack (show t) <> "s on " <> svc.svcResource)
            ]
    -- a service on another port name sends to another port, or to none
    portNameLines :: Svc -> [Text]
    portNameLines svc =
        [ "[ \"$(gcloud compute backend-services describe " <> shellQuote svc.svcResource <> regional
            <> " --format='value(portName)' 2>/dev/null)\" = "
            <> shellQuote n
            <> " ] || echo "
            <> shellQuote ("MISSING port-name " <> n <> " on " <> svc.svcResource)
        | Just n <- [svcPortName svc]
        ]
    namedPortLines :: ((Text, InstanceGroupLocation), [(Text, Int)]) -> [Text]
    namedPortLines ((ig, loc), named) =
        [ "gcloud compute instance-groups get-named-ports " <> shellQuote ig <> groupLocation loc
            <> " --format='value(name,port)' 2>/dev/null | awk -v n="
            <> shellQuote n
            <> " -v p="
            <> shellQuote (Text.pack (show p))
            <> " "
            <> shellQuote "$1==n&&$2==p{f=1} END{exit !f}"
            <> " || echo "
            <> shellQuote ("MISSING named-port " <> n <> ":" <> Text.pack (show p) <> " on instance-group " <> ig)
        | (n, p) <- nub named
        ]
    -- every host of every rule, one per line, whatever separators gcloud
    -- flattens the nested lists with
    hostLines :: [Text]
    hostLines =
        [ "gcloud compute url-maps describe " <> q "-url-map" <> regional
            <> " --format='value(hostRules[].hosts)' 2>/dev/null | tr \";,[]' \\t\" '\\n' | grep -qxF -- "
            <> shellQuote h
            <> " || echo "
            <> shellQuote ("MISSING host-rule " <> h)
        | h <- concatMap hostRuleHosts alb.albHostRules
        ]
    attached :: Svc -> Text -> Text -> Text
    attached svc suffix what =
        "gcloud compute backend-services describe " <> shellQuote svc.svcResource <> regional
            <> " --format='value(backends[].group)' 2>/dev/null | tr ';' '\\n' | grep -q -- "
            <> shellQuote (suffix <> "$") <> " || echo " <> shellQuote ("MISSING backend " <> what)
    backendLines :: Svc -> Backend -> [Text]
    backendLines svc = \case
        InstanceGroupBackend ig loc _ ->
            [ "need " <> shellQuote ("instance-group " <> ig)
                <> " gcloud compute instance-groups describe " <> shellQuote ig <> groupLocation loc
            , attached svc ("/instanceGroups/" <> ig) ("instance-group " <> ig)
            ]
        CloudRunBackend _ ->
            [ needNamed "network-endpoint-groups" svc.svcNeg
            , attached svc ("/networkEndpointGroups/" <> svc.svcNeg) ("neg " <> svc.svcNeg)
            ]
    -- one state per backend instance; ';' separates a backend's instances
    healthLines :: Svc -> Text
    healthLines svc =
        "gcloud compute backend-services get-health " <> shellQuote svc.svcResource <> regional
            <> " --format='value(status.healthStatus[].healthState)' 2>/dev/null"
            <> " | tr ';' '\\n' | while read -r st; do [ -n \"$st\" ] && echo "
            <> shellQuote ("HEALTH " <> svc.svcLabel)
            <> "\" $st\"; done; true"

-- | The @--project@\/@--region@ pair every regional resource in these scripts
-- is addressed by, reading the variables the script sets up front.
regional :: Text
regional = " --project=\"$PROJECT\" --region=\"$REGION\""

-- | The same for Certificate Manager, which says @--location@.
located :: Text
located = " --project=\"$PROJECT\" --location=\"$REGION\""

-- | How to address the instance group itself.
groupLocation :: InstanceGroupLocation -> Text
groupLocation (InstanceGroupZone z) = " --project=\"$PROJECT\" --zone=" <> shellQuote z
groupLocation (InstanceGroupRegion rg) = " --project=\"$PROJECT\" --region=" <> shellQuote rg

-- | How @backend-services add-backend@ names the group's location.
groupBackendFlag :: InstanceGroupLocation -> Text
groupBackendFlag (InstanceGroupZone z) = " --instance-group-zone=" <> shellQuote z
groupBackendFlag (InstanceGroupRegion rg) = " --instance-group-region=" <> shellQuote rg

{- | Renders a bash script that deletes the LB components, dependants first.
A component that is already gone is skipped; one that exists and fails to
delete fails the script.

A 'ComputeCertificate' is not deleted: this recipe did not create it.
-}
renderLbDeleteScript :: ApplicationLoadBalancer -> Text
renderLbDeleteScript alb =
    Text.unlines $
        [ "set -euo pipefail"
        , "PROJECT=" <> shellQuote alb.albProject.projectId
        , "REGION=" <> shellQuote alb.albRegion.regionName
        , "exists() { \"$@\" >/dev/null 2>&1; }"
        ]
            <> [deleteIfPresent "forwarding-rules" (resourceName "-https-fw") | serveHttps alb]
            <> [deleteIfPresent "forwarding-rules" (resourceName "-fw")]
            <> [deleteIfPresent "target-https-proxies" (resourceName "-https-proxy") | serveHttps alb]
            <> [deleteIfPresent "target-http-proxies" (resourceName "-proxy")]
            <> [deleteIfPresent "addresses" (resourceName "-ip") | serveHttps alb]
            <> [deleteIfPresent "url-maps" (resourceName "-url-map")]
            <> [deleteIfPresent "backend-services" (shellQuote svc.svcResource) | svc <- services alb]
            <> concatMap deleteBackendSpecificLines (services alb)
            <> [deleteIfPresent "health-checks" (shellQuote hc.healthCheckName) | hc <- healthChecks alb]
            <> concatMap deleteCertificateLines alb.albCertificates
  where
    resourceName :: Text -> Text
    resourceName suffix = shellQuote (alb.albName <> suffix)

    deleteIfPresent :: Text -> Text -> Text
    deleteIfPresent collection name =
        "if exists gcloud compute " <> collection <> " describe " <> name <> regional
            <> "; then gcloud compute " <> collection <> " delete " <> name
            <> regional <> " --quiet; fi"

    deleteLocatedIfPresent :: Text -> Text -> Text
    deleteLocatedIfPresent collection name =
        "if exists gcloud certificate-manager " <> collection <> " describe " <> name <> located
            <> "; then gcloud certificate-manager " <> collection <> " delete " <> name
            <> located <> " --quiet; fi"

    -- the certificate first: an authorization in use cannot be deleted
    deleteCertificateLines = \case
        ComputeCertificate _ -> []
        ManagedCertificate n ds ->
            deleteLocatedIfPresent "certificates" (shellQuote n)
                : [deleteLocatedIfPresent "dns-authorizations" (shellQuote (authorizationName n d)) | d <- ds]

    -- The instance group is not deleted here: this recipe did not create it
    -- (it is the caller's, and may well outlive the balancer). Detaching is
    -- implicit in deleting the backend service.
    deleteBackendSpecificLines :: Svc -> [Text]
    deleteBackendSpecificLines svc = flip concatMap svc.svcBackends $ \case
        InstanceGroupBackend{} -> []
        CloudRunBackend _ -> [deleteIfPresent "network-endpoint-groups" (shellQuote svc.svcNeg)]
