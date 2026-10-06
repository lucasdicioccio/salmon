{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.LoadBalancing (
    Backend (..),
    InstanceGroupLocation (..),
    HealthCheck (..),
    BackendService (..),
    ServiceRef (..),
    BackendBucket (..),
    BucketRoutes (..),
    Redirect (..),
    RedirectCode (..),
    redirectToHost,
    HttpListener (..),
    HostRule (..),
    PathRule (..),
    PathRewrite (..),
    Certificate (..),
    certificateResource,
    domainSetTag,
    ApplicationLoadBalancer (..),
    httpLoadBalancer,
    applicationLoadBalancer,
    applicationLoadBalancerAfter,
    applicationLoadBalancerPart,
    applicationLoadBalancerWith,
    applicationLoadBalancerPartWith,
    backendBucketPart,
    afterStorageBuckets,
    Part (..),
    PartSpec (..),
    lbParts,
    InvalidLoadBalancer (..),
    albProblems,
    certificateManagerApi,
    readAddress,
    DnsAuthorizationRecord (..),
    dnsAuthorizations,
    readDnsAuthorizationRecord,
    parseDnsAuthorizationRecord,
    bucketResource,
    renderUrlMap,
    urlMapStamp,
    ownershipMarker,
    renderHttpRedirectUrlMap,
    interpretLbDescribe,
    interpretLbCheck,
    renderLbCheckScript,
    renderLbHealthScript,
    renderPartUpScript,
    renderPartCheckScript,
    renderPartDownScript,
    shellQuote,
    Report (..),
    LoadBalancingCommand (..),
    loadBalancingCommand,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson (Pair)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Function (on)
import Data.List (nub, nubBy, sort, (\\))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextErr
import GHC.IO.Exception (ExitCode (..))
import Numeric (showHex)
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
    deriving (Eq, Ord, Show)

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

{- | A backend bucket: a Cloud Storage bucket the balancer serves objects
from, reachable only through a 'HostRule' or 'PathRule' that names it
('NamedBucket'). Its resource is @\<balancer\>-\<name\>-bucket@, a /regional/
backend bucket (@EXTERNAL_MANAGED@), which is what a regional balancer's URL
map can name.

The Cloud Storage bucket is the caller's, and is neither created nor removed
here. GCP's documented limits for this kind are the caller's to meet too: a
bucket in the balancer's region, readable by @allUsers@, no Cloud CDN, @GET@
only.

__A rule that routes to one is refused by default__ ('albProblems', see
'BucketRoutes' for what was observed). The bucket declared with no rule
naming it is not: the resource alone serves nothing and was not seen to harm
anything, and keeping it declared is what lets a declaration drop the rule
alone.
-}
data BackendBucket = BackendBucket
    { backendBucketName :: Text
    , backendBucketGcsBucket :: Text
    -- ^ the Cloud Storage bucket's name, without @gs:\/\/@
    }
    deriving (Eq, Show)

{- | Whether a rule may route to a backend bucket ('NamedBucket').

'RefuseBucketRoutes' is the default, after an outage. What was observed, on
one live regional external Application Load Balancer (@europe-west1@,
2026-10-05): with a single path matcher on a regional backend bucket in the
URL map, every matcher on a backend service answered 503
(@failed_to_pick_backend@) while the backends were @HEALTHY@ and the bucket's
own host served. Importing the same map without that host rule and its
matcher brought the service hosts back in 80 seconds; a matcher that only
redirects stayed in the map and the services serve with it. The service
rules are rendered identically with and without the bucket ('renderUrlMap'),
so the difference is not in what this module writes about them. Why the
platform does this is not known, and neither is whether a /global/ balancer
(which this module does not make) does the same: nobody here has run one.

The URL map's default service is always a backend service, so any bucket
route on this balancer is such a mixed map.

'AllowBucketRoutesKnownToHaveBrokenALiveBalancer' is for proving the
contrary on a balancer that serves nothing that matters.
-}
data BucketRoutes
    = RefuseBucketRoutes
    | AllowBucketRoutesKnownToHaveBrokenALiveBalancer
    deriving (Eq, Show)

{- | Where a rule sends: a backend service, a backend bucket, or nowhere --
a redirect the balancer answers itself.
-}
data ServiceRef
    = DefaultService
    | NamedService Text
    | -- | a 'BackendBucket' of 'albBuckets', by its name; refused unless
      -- 'albBucketRoutes' says otherwise
      NamedBucket Text
    | RedirectTo Redirect
    deriving (Eq, Show)

{- | A redirect answered by the balancer (the URL map's @urlRedirect@). What
is 'Nothing' is kept from the request: the host, the path. The query string
is always kept.
-}
data Redirect = Redirect
    { redirectHost :: Maybe Text
    , redirectPath :: Maybe Text
    -- ^ the whole path of the answer, starting with @\/@
    , redirectHttps :: Bool
    -- ^ answer with @https:\/\/@ whatever the request came in on; 'False'
    -- keeps the request's scheme
    , redirectCode :: RedirectCode
    }
    deriving (Eq, Show)

data RedirectCode
    = -- | 301
      MovedPermanently
    | -- | 302
      Found
    | -- | 303
      SeeOther
    | -- | 307, the method kept
      TemporaryRedirect
    | -- | 308, the method kept
      PermanentRedirect
    deriving (Eq, Show)

-- | A permanent redirect to another host over HTTPS, path kept: an apex to its @www@.
redirectToHost :: Text -> Redirect
redirectToHost h = Redirect (Just h) Nothing True MovedPermanently

{- | What the balancer does on port 80.

'HttpCreatedOnce' is what this module always did and is the default: an HTTP
proxy and a @:80@ rule, the proxy __created__ pointing at the balancer's URL
map and never set again, so a proxy somebody repointed (at a redirect-only
map of their own, say) stays where they put it. The other three are
declarations about the listener, and make this module the writer of the
proxy's URL map:

* 'ServeHttp': the proxy is on the balancer's URL map, and is put back on it
  when it is found on another;
* 'RedirectToHttps': the proxy is on a second, redirect-only URL map
  (@\<balancer\>-http-redirect-url-map@, see 'renderHttpRedirectUrlMap'), so
  every request on port 80 is answered with a redirect to HTTPS;
* 'NoHttp': no HTTP proxy and no @:80@ rule, and the ones a previous
  declaration made are removed.

The last two need a certificate, or the balancer serves nothing.
-}
data HttpListener
    = HttpCreatedOnce
    | ServeHttp
    | RedirectToHttps RedirectCode
    | NoHttp
    deriving (Eq, Show)

-- | Requests whose path matches one of these patterns (@\/api\/*@) go there.
data PathRule = PathRule
    { pathRulePaths :: [Text]
    , pathRuleService :: ServiceRef
    -- ^ a service, a bucket or a redirect
    , pathRuleRewrite :: PathRewrite
    -- ^ what the backend is asked for; 'KeepPath' for the request's own path
    }
    deriving (Eq, Show)

{- | What a 'PathRule' does to the path before the request reaches the
backend (the URL map's @routeAction.urlRewrite@, beside the rule's
@service@). The client sees nothing of it: this is not a redirect.

'RewritePrefix' is GCP's @pathPrefixRewrite@: the part of the path the rule
/matched/ is replaced by the text given, and the rest is kept. By GCP's
documentation, a pattern ending in @\/*@ matches up to and including that
slash, so @\/static\/*@ with @RewritePrefix \"\/\"@ asks the backend for
@\/a.css@ when the request was for @\/static\/a.css@; and an exact pattern
matches the whole path, so @\/@ with @RewritePrefix \"\/index.html\"@ asks for
@\/index.html@ -- the case this exists for, a backend bucket that serves
nothing for @\/@. Neither reading has been checked against a live balancer.

A rule that redirects has a path of its own ('redirectPath') and cannot
rewrite ('albProblems').
-}
data PathRewrite
    = KeepPath
    | -- | starting with @\/@
      RewritePrefix Text
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

A Certificate Manager certificate's names are __immutable__: nothing edits
the domain list of one that exists. So a 'ManagedCertificate', whose
resource is named exactly as declared, cannot follow a changed list -- its
node says so (a @PROBLEM@ line in the check, a failing @up@) instead of
reporting a certificate that silently covers other names than the declared
ones.

'DomainSetCertificate' is the one that can change on a live balancer. Its
resource is named after its /domain set/ (@\<base\>-\<tag\>@, see
'certificateResource'), so a changed set is a new certificate beside the one
in service: it is created, the HTTPS proxy keeps the old one until the new
one is @ACTIVE@ and is only then updated to it, and the superseded
certificates of that base are deleted after (see 'lbParts'). Its DNS
authorizations are named after the base and the domain, not the set, so the
names both sets cover reuse the authorization (and the published record)
they already have.

'ComputeCertificate' names a regional @compute ssl-certificates@ resource
somebody else made (a self-managed one). It does not mix with the other two
kinds on one proxy.
-}
data Certificate
    = ManagedCertificate Text [Text]
    | -- | a base name and the domains; the resource is 'certificateResource'
      DomainSetCertificate Text [Text]
    | ComputeCertificate Text
    deriving (Eq, Show)

{- | The name of the resource a certificate is: the declared name, except for
a 'DomainSetCertificate', which is its base name and 'domainSetTag' of its
domains.
-}
certificateResource :: Certificate -> Text
certificateResource = \case
    ManagedCertificate n _ -> n
    DomainSetCertificate base ds -> base <> "-" <> domainSetTag ds
    ComputeCertificate n -> n

{- | Eight hex digits naming a set of domains: of the SHA-256 of the names,
lower-cased, sorted and without duplicates. Order and case do not make a new
certificate; one more name, or one fewer, does.
-}
domainSetTag :: [Text] -> Text
domainSetTag ds =
    Text.pack
        . concatMap hex
        . ByteString.unpack
        . ByteString.take 4
        . SHA256.hash
        . Text.encodeUtf8
        $ Text.intercalate "," (normalDomains ds)
  where
    hex w = let h = showHex w "" in if length h < 2 then '0' : h else h

-- | A domain list as a set: what two certificates are compared by.
normalDomains :: [Text] -> [Text]
normalDomains = sort . nub . map Text.toLower

-- | The Certificate Manager certificates of a declaration: (resource, authorization base, domains).
managedCertificates :: ApplicationLoadBalancer -> [(Text, Text, [Text])]
managedCertificates alb =
    concatMap
        ( \c -> case c of
            ManagedCertificate n ds -> [(n, n, ds)]
            DomainSetCertificate base ds -> [(certificateResource c, base, ds)]
            ComputeCertificate _ -> []
        )
        alb.albCertificates

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
    , albBuckets :: [BackendBucket]
    , albBucketRoutes :: BucketRoutes
    -- ^ 'RefuseBucketRoutes' unless the outage it names is the thing to test
    , albHttp :: HttpListener
    -- ^ 'HttpCreatedOnce' unless something else is wanted of port 80
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
        , albBuckets = []
        , albBucketRoutes = RefuseBucketRoutes
        , albHttp = HttpCreatedOnce
        }

-- | The API a 'ManagedCertificate' needs enabled (the caller's dependency).
certificateManagerApi :: Text
certificateManagerApi = "certificatemanager.googleapis.com"

-- | A declaration no gcloud call could satisfy, refused before any is made.
newtype InvalidLoadBalancer = InvalidLoadBalancer [Text]
    deriving (Show)

instance Exception InvalidLoadBalancer

{- | What is wrong with a declaration, all of it rather than the first: a
rule naming a service or a bucket nobody declared, a rule routing to a
backend bucket at all ('BucketRoutes'), two services or two
buckets under one name, a redirect that redirects to the request itself, a
path rule that redirects and rewrites, a rewrite that is no path, an
HTTP listener option that leaves no listener, a host in two rules, a rule with no host or no path, the two certificate kinds
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
           | not (null (managedCertificates alb))
           , not (null [() | ComputeCertificate{} <- alb.albCertificates])
           ]
        <> [ "managed certificate " <> base <> " names no domain"
           | (_, base, []) <- managedCertificates alb
           ]
        -- a base is a lineage the balancer deletes superseded certificates
        -- of, and names its authorizations after: one declaration each
        <> [ "certificate declared twice: " <> n
           | n <- nub (certNames \\ nub certNames)
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
        <> [ "backend bucket declared twice: " <> n
           | n <- nub (buckets \\ nub buckets)
           ]
        <> [ "backend bucket " <> b.backendBucketName <> " names no Cloud Storage bucket"
           | b <- alb.albBuckets
           , Text.null b.backendBucketGcsBucket
           ]
        <> [ "rule names an undeclared backend bucket: " <> n
           | NamedBucket n <- nub refs
           , n `notElem` buckets
           ]
        <> [ bucketRouteRefusal alb routed
           | alb.albBucketRoutes == RefuseBucketRoutes
           , let routed = bucketRoutes alb
           , not (null routed)
           ]
        -- one that changes nothing answers every request with itself
        <> [ "a redirect names neither a host, a path nor HTTPS"
           | RedirectTo (Redirect Nothing Nothing False _) <- nub refs
           ]
        <> [ "a redirect's path must start with /: " <> path
           | RedirectTo Redirect{redirectPath = Just path} <- nub refs
           , not ("/" `Text.isPrefixOf` path)
           ]
        <> [ "a redirect names an empty host"
           | RedirectTo Redirect{redirectHost = Just ""} <- nub refs
           ]
        -- a URL map's rule is a redirect or a route with its action, not both
        <> [ "a path rule both redirects and rewrites its path (" <> Text.unwords p.pathRulePaths <> "): a redirect names its own path"
           | p <- pathRules
           , RedirectTo _ <- [p.pathRuleService]
           , p.pathRuleRewrite /= KeepPath
           ]
        <> [ "a path rewrite must start with /: " <> prefix
           | RewritePrefix prefix <- nub (map pathRuleRewrite pathRules)
           , not ("/" `Text.isPrefixOf` prefix)
           ]
        <> [ what <> " needs a certificate: without one the balancer serves nothing"
           | null alb.albCertificates
           , what <- case alb.albHttp of
                RedirectToHttps _ -> ["redirecting HTTP to HTTPS"]
                NoHttp -> ["no HTTP listener"]
                _ -> []
           ]
  where
    names = map backendServiceName alb.albServices
    buckets = map backendBucketName alb.albBuckets
    certNames =
        [base | (_, base, _) <- managedCertificates alb]
            <> [n | ComputeCertificate n <- alb.albCertificates]
    hosts = concatMap hostRuleHosts alb.albHostRules
    pathRules = concatMap hostRulePaths alb.albHostRules
    refs =
        concat
            [ r.hostRuleService : map pathRuleService r.hostRulePaths
            | r <- alb.albHostRules
            ]

{- | The rules of a declaration that route to a backend bucket, as (what the
rule matches, the bucket's name in 'albBuckets').
-}
bucketRoutes :: ApplicationLoadBalancer -> [(Text, Text)]
bucketRoutes alb =
    concat
        [ [("hosts " <> hosts, n) | NamedBucket n <- [r.hostRuleService]]
            <> [ ("paths " <> Text.unwords p.pathRulePaths <> " of hosts " <> hosts, n)
               | p <- r.hostRulePaths
               , NamedBucket n <- [p.pathRuleService]
               ]
        | r <- alb.albHostRules
        , let hosts = Text.unwords r.hostRuleHosts
        ]

{- | Why a bucket route is refused, and what to do about a balancer that
already has one: the text of the 'albProblems' entry.
-}
bucketRouteRefusal :: ApplicationLoadBalancer -> [(Text, Text)] -> Text
bucketRouteRefusal alb routed =
    "a rule routes to a backend bucket ("
        <> Text.intercalate ", " [what <> " to " <> bucketResource alb n | (what, n) <- routed]
        <> "), which is refused on this regional load balancer: on a live one, a URL map with one"
        <> " matcher on a regional backend bucket made every host on a backend service answer 503"
        <> " (failed_to_pick_backend) while its backends were HEALTHY and the bucket's own host served."
        <> " Nothing was changed by this pass. If the live URL map already has such a rule, drop the"
        <> " rule from the declaration (its albBuckets entry may stay) and run a pass: the URL map is"
        <> " re-imported without it, which is what restored the service hosts."
        <> " albBucketRoutes = AllowBucketRoutesKnownToHaveBrokenALiveBalancer declares it anyway"

{- | The balancer, declared once and unfolded into a node per resource.

The node returned is the one callers depend on (same @ref@ as when the
balancer was a single node running one script): a root over a node per
health check, per instance group's named ports, per backend service, per
serverless NEG, per backend attachment, the URL map, a node per DNS
authorization, per certificate, the address, the proxies and the forwarding
rules. See 'lbParts' for the edges between them. Each has its own @check@
(its own @describe@), @up@ and @down@, so progress, concurrency, failure and
retry are per resource, and teardown is the edges read backwards.

The root's own @up@ creates nothing; its @check@ asks the backend services
for their backends' health ('Unknown' while some are not @HEALTHY@), which
is a statement about the balancer as a whole and not about any one resource.

Nothing here runs before the resources' prerequisites unless they are named:
a dependency @inject@ed into the returned node is a dependency of the /root/,
and the resource nodes are the root's dependencies too, so they would not
wait for it. Use 'applicationLoadBalancerAfter' for what has to exist first
(the APIs, the proxy-only subnet, the instance groups).

The DNS record of a certificate's authorization goes on that authorization's
own node ('applicationLoadBalancerPart' with a 'DnsAuthorizationPart'), __never
on the returned root__. The root comes after the certificates, the proxies and
the forwarding rules, so a record waiting for it is published last, behind
everything that can fail on the way; and since a certificate is only issued
once its records resolve, a record behind a root that waits for the
certificate is a cycle no pass gets out of. A pending certificate swap no
longer blocks the root (see 'lbParts'), but a failed backend, a @FAILED@
certificate or a refused declaration still would.
-}
applicationLoadBalancer :: Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Op
applicationLoadBalancer = applicationLoadBalancerAfter []

{- | 'applicationLoadBalancer' with prerequisites: nodes every resource node
of the balancer depends on.
-}
applicationLoadBalancerAfter :: [Op] -> Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Op
applicationLoadBalancerAfter = applicationLoadBalancerWith (const [])

{- | 'applicationLoadBalancerAfter' with prerequisites of single resources
too: for each resource node, the nodes that one alone depends on, beside the
ones every resource does. It is how a backend bucket is ordered after the
Cloud Storage bucket it serves ('afterStorageBuckets') without the health
checks and the certificates waiting for that bucket as well.

The function is asked about every 'Part' of the declaration and answers @[]@
for the ones it has nothing to say about. 'applicationLoadBalancerPartWith'
has to be given the same one to hand back the same nodes.
-}
applicationLoadBalancerWith :: (Part -> [Op]) -> [Op] -> Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Op
applicationLoadBalancerWith partPrereqs prereqs r gcloudTrack alb =
    op "gcp-application-lb" (deps (prereqs <> ordered)) $ \actions ->
        actions
            { help = Text.unwords ["application load balancer", alb.albName]
            , notes = albNotes alb
            , ref = mkRef "gcp-application-lb" (alb.albProject.projectId, alb.albRegion.regionName, alb.albName)
            , up = unless (null problems) $ throwIO (InvalidLoadBalancer problems)
            , check = checkHealth
            }
  where
    nodes = partOps partPrereqs prereqs r gcloudTrack alb
    ordered = mapMaybe ((`Map.lookup` nodes) . partId) (lbParts alb)
    problems = albProblems alb

    checkHealth :: IO CheckResult
    checkHealth
        | not (null problems) = pure (invalid problems)
        | otherwise = runCheck (LbHealth alb)

{- | One resource node of the balancer, to hang something on that needs that
resource and not the whole balancer -- the DNS record of one authorization,
say ('DnsAuthorizationPart', named as 'dnsAuthorizations' names it).
'Nothing' when the declaration has no such resource.

Given the prerequisites, reporter and declaration the balancer itself was
made with, this is the very node the balancer's root depends on.
-}
applicationLoadBalancerPart :: [Op] -> Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Part -> Maybe Op
applicationLoadBalancerPart = applicationLoadBalancerPartWith (const [])

-- | 'applicationLoadBalancerPart' for a balancer made by 'applicationLoadBalancerWith'.
applicationLoadBalancerPartWith :: (Part -> [Op]) -> [Op] -> Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Part -> Maybe Op
applicationLoadBalancerPartWith partPrereqs prereqs r gcloudTrack alb part =
    Map.lookup part (partOps partPrereqs prereqs r gcloudTrack alb)

{- | The resource node of one of 'albBuckets', by the name it is declared
under ('backendBucketName'): what 'applicationLoadBalancerPart' is asked for
that backend bucket's node, and what an 'applicationLoadBalancerWith'
function is asked about.
-}
backendBucketPart :: ApplicationLoadBalancer -> Text -> Part
backendBucketPart alb n = BackendBucketPart (bucketResource alb n)

{- | Prerequisites for 'applicationLoadBalancerWith' that put each backend
bucket after what its Cloud Storage bucket needs -- the node that makes the
bucket, the one that makes it readable -- and say nothing about any other
resource:

> applicationLoadBalancerWith (afterStorageBuckets alb (\b -> [storageBucket b.backendBucketGcsBucket])) prereqs ...

A backend bucket is created whether or not the Cloud Storage bucket it names
exists, so this is an ordering for the caller's sake (the backend bucket
never points at nothing, and a teardown removes it first), not something a
@create@ would otherwise fail on.
-}
afterStorageBuckets :: ApplicationLoadBalancer -> (BackendBucket -> [Op]) -> Part -> [Op]
afterStorageBuckets alb needs part =
    concat [needs b | b <- alb.albBuckets, backendBucketPart alb b.backendBucketName == part]

partOps :: (Part -> [Op]) -> [Op] -> Reporter Report -> Track' (Binary "gcloud") -> ApplicationLoadBalancer -> Map.Map Part Op
partOps partPrereqs prereqs r gcloudTrack alb = nodes
  where
    problems = albProblems alb

    -- lazily: a node's dependencies are looked up in the map being built
    nodes :: Map.Map Part Op
    nodes = Map.fromList [(s.partId, mk s) | s <- lbParts alb]

    mk :: PartSpec -> Op
    mk s =
        withBinary gcloudTrack loadBalancingCommand (LbPartUp alb s) $ \create ->
            withBinary gcloudTrack loadBalancingCommand (LbPartDown alb s) $ \delete ->
                op (partKind s.partId) (deps (prereqs <> partPrereqs s.partId <> mapMaybe (`Map.lookup` nodes) s.partDeps)) $ \actions ->
                    actions
                        { help = s.partHelp
                        , notes = s.partNotes
                        , ref = partRef alb s.partId
                        , up = do
                            -- the declaration's refusals, before anything runs
                            unless (null problems) $ throwIO (InvalidLoadBalancer problems)
                            create (contramap (RunLoadBalancingCommand (LbPartUp alb s)) r)
                        , down =
                            unless (null s.partDown) $
                                delete (contramap (RunLoadBalancingCommand (LbPartDown alb s)) r)
                        , check =
                            if null problems
                                then runCheck (LbPartCheck alb s)
                                else pure (invalid problems)
                        }

invalid :: [Text] -> CheckResult
invalid problems = Failure ("invalid load balancer: " <> Text.intercalate "; " problems)

runCheck :: LoadBalancingCommand -> IO CheckResult
runCheck cmd = do
    (code, out, _err) <- readCreateProcessWithExitCode (prepare loadBalancingCommand cmd) ""
    pure $ interpretLbCheck code (Text.decodeUtf8With TextErr.lenientDecode out)

-- | The kind tag of a resource node: its shorthand and its @ref@'s.
partKind :: Part -> Text
partKind = \case
    HealthCheckPart _ -> "gcp-lb-health-check"
    NamedPortsPart _ _ -> "gcp-lb-named-ports"
    BackendServicePart _ -> "gcp-lb-backend-service"
    NetworkEndpointGroupPart _ -> "gcp-lb-neg"
    BackendPart _ _ -> "gcp-lb-backend"
    BackendBucketPart _ -> "gcp-lb-backend-bucket"
    UrlMapPart -> "gcp-lb-url-map"
    HttpRedirectUrlMapPart -> "gcp-lb-http-redirect-url-map"
    NoHttpPart -> "gcp-lb-no-http"
    DnsAuthorizationPart _ -> "gcp-lb-dns-authorization"
    CertificatePart _ -> "gcp-lb-certificate"
    SupersededCertificatesPart _ -> "gcp-lb-superseded-certificates"
    AddressPart -> "gcp-lb-address"
    HttpProxyPart -> "gcp-lb-http-proxy"
    HttpsProxyPart -> "gcp-lb-https-proxy"
    ForwardingRulePart -> "gcp-lb-forwarding-rule"
    HttpsForwardingRulePart -> "gcp-lb-https-forwarding-rule"
    LeftoversPart -> "gcp-lb-leftovers"

{- | The effect site of a resource node: the resource's own name where it has
one that is not the balancer's (a health check, a certificate), the
balancer's where the resource is named after it. An instance group's named
ports are keyed on the balancer /and/ the group, since each balancer sets
only its own names there.
-}
partRef :: ApplicationLoadBalancer -> Part -> Ref
partRef alb part = mkRef (partKind part) (alb.albProject.projectId, alb.albRegion.regionName, key)
  where
    key :: [Text]
    key = case part of
        HealthCheckPart n -> [n]
        NamedPortsPart ig loc -> [alb.albName, ig, Text.pack (show loc)]
        BackendServicePart n -> [n]
        NetworkEndpointGroupPart n -> [n]
        BackendPart svc g -> [svc, g]
        BackendBucketPart n -> [n]
        DnsAuthorizationPart n -> [n]
        CertificatePart n -> [n]
        SupersededCertificatesPart base -> [base]
        _ -> [alb.albName]

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
        <> [ "backend bucket " <> bucketResource alb b.backendBucketName <> " on " <> b.backendBucketGcsBucket
           | b <- alb.albBuckets
           ]
        <> [ "hosts " <> Text.unwords r.hostRuleHosts <> ": " <> targetText alb r.hostRuleService
           | r <- alb.albHostRules
           , isRedirect r.hostRuleService
           ]
        <> [ "bucket routes allowed (known to have broken a live balancer)"
           | alb.albBucketRoutes == AllowBucketRoutesKnownToHaveBrokenALiveBalancer
           ]
        <> case alb.albHttp of
            HttpCreatedOnce -> []
            ServeHttp -> ["http served"]
            RedirectToHttps code -> ["http redirects to https (" <> redirectCodeText code <> ")"]
            NoHttp -> ["no http listener"]
  where
    isRedirect = \case
        RedirectTo _ -> True
        _ -> False
    certNote = \case
        ManagedCertificate n ds -> "managed certificate " <> n <> " for " <> Text.unwords ds
        c@(DomainSetCertificate _ ds) -> "managed certificate " <> certificateResource c <> " for " <> Text.unwords ds
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
(a 'DomainSetCertificate''s base, so it does not move with the set) and the
domain's, so the node that creates one and the reader that asks for its
record compute the same thing.
-}
dnsAuthorizations :: ApplicationLoadBalancer -> [(Text, Text)]
dnsAuthorizations alb =
    [ (d, authorizationName base d)
    | (_, base, ds) <- managedCertificates alb
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
backend instance of an instance-group backend, @CERT <name> <state>@ per
managed certificate, and @PROBLEM <what>@ for what is there and wrong in a
way no create fixes (a certificate covering other names than the declared
ones, one whose renewal failed, an expired one). A missing piece is a
'Failure' (the node's @up@ is idempotent and will create it), and so is a
problem and a certificate GCP reports @FAILED@. Backends that are not (yet) @HEALTHY@ are 'Unknown': a freshly
brought-up balancer reports @UNHEALTHY@ for roughly two minutes, and
re-running @up@ would not shorten that; a certificate still @PROVISIONING@
is 'Unknown' for the same reason, and so is @WAITING <what>@, something no
@up@ would do yet (a superseded certificate the proxy has not moved off). A
Cloud Run (NEG) backend has no health to
ask for, so its presence and attachment is the whole check.
-}
interpretLbCheck :: ExitCode -> Text -> CheckResult
interpretLbCheck (ExitFailure n) _ = Failure ("load balancer check failed (exit " <> Text.pack (show n) <> ")")
interpretLbCheck ExitSuccess out
    | not (null missing) = Failure ("load balancer incomplete: missing " <> Text.intercalate ", " missing)
    | not (null problems) = Failure ("load balancer: " <> Text.intercalate "; " problems)
    | not (null failedCerts) = Failure ("certificate provisioning failed: " <> Text.intercalate ", " failedCerts)
    | not (null unhealthy) || not (null pendingCerts) || waiting = Unknown
    | otherwise = Success
  where
    ls = map Text.words (Text.lines out)
    waiting = not (null [() | ("WAITING" : _) <- ls])
    missing = [Text.unwords rest | ("MISSING" : rest) <- ls]
    problems = [Text.unwords rest | ("PROBLEM" : rest) <- ls]
    unhealthy = [st | ["HEALTH", _, st] <- ls, st /= "HEALTHY"]
    failedCerts = [n | ["CERT", n, "FAILED"] <- ls]
    pendingCerts = [n | ["CERT", n, st] <- ls, st /= "ACTIVE", st /= "FAILED"]

-------------------------------------------------------------------------------

data LoadBalancingCommand
    = LbCreate ApplicationLoadBalancer
    | LbCheck ApplicationLoadBalancer
    | LbHealth ApplicationLoadBalancer
    | LbPartUp ApplicationLoadBalancer PartSpec
    | LbPartCheck ApplicationLoadBalancer PartSpec
    | LbPartDown ApplicationLoadBalancer PartSpec
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
    LbHealth alb ->
        proc "bash" ["-c", Text.unpack (renderLbHealthScript alb)]
    LbPartUp alb part ->
        proc "bash" ["-c", Text.unpack (renderPartUpScript alb part)]
    LbPartCheck alb part ->
        proc "bash" ["-c", Text.unpack (renderPartCheckScript alb part)]
    LbPartDown alb part ->
        proc "bash" ["-c", Text.unpack (renderPartDownScript alb part)]
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

{- | The resource a rule's target is: a backend service's name, a backend
bucket's. A redirect is no resource, and is described instead.
-}
serviceResource :: ApplicationLoadBalancer -> ServiceRef -> Text
serviceResource alb = \case
    DefaultService -> alb.albName <> "-backend"
    NamedService n -> alb.albName <> "-" <> n <> "-backend"
    NamedBucket n -> bucketResource alb n
    RedirectTo r -> redirectText r

bucketResource :: ApplicationLoadBalancer -> Text -> Text
bucketResource alb n = alb.albName <> "-" <> n <> "-bucket"

-- | A rule's target as a note says it: a backend service by its bare name, as before.
targetText :: ApplicationLoadBalancer -> ServiceRef -> Text
targetText alb = \case
    NamedBucket n -> "backend bucket " <> bucketResource alb n
    sref -> serviceResource alb sref

-- | The resource node a rule's target needs first, when it is a resource.
targetPart :: ApplicationLoadBalancer -> ServiceRef -> Maybe Part
targetPart alb = \case
    RedirectTo _ -> Nothing
    NamedBucket n -> Just (BackendBucketPart (bucketResource alb n))
    sref -> Just (BackendServicePart (serviceResource alb sref))

-- | Every target a declaration's rules name, the default service first.
targets :: ApplicationLoadBalancer -> [ServiceRef]
targets alb =
    DefaultService : concat [r.hostRuleService : map pathRuleService r.hostRulePaths | r <- alb.albHostRules]

-- | A rewrite as a note says it, after the rule's target; nothing for 'KeepPath'.
rewriteText :: PathRewrite -> Text
rewriteText = \case
    KeepPath -> ""
    RewritePrefix prefix -> " with the matched prefix rewritten to " <> prefix

redirectCodeText :: RedirectCode -> Text
redirectCodeText = \case
    MovedPermanently -> "MOVED_PERMANENTLY_DEFAULT"
    Found -> "FOUND"
    SeeOther -> "SEE_OTHER"
    TemporaryRedirect -> "TEMPORARY_REDIRECT"
    PermanentRedirect -> "PERMANENT_REDIRECT"

redirectText :: Redirect -> Text
redirectText r =
    Text.unwords
        [ "redirect"
        , redirectCodeText r.redirectCode
        , (if r.redirectHttps then "https://" else "")
            <> fromMaybe "{host}" r.redirectHost
            <> fromMaybe "{path}" r.redirectPath
        ]

-- | A redirect as a URL map's @urlRedirect@.
renderRedirect :: Redirect -> Aeson.Value
renderRedirect r =
    Aeson.object $
        ["hostRedirect" Aeson..= h | Just h <- [r.redirectHost]]
            <> ["pathRedirect" Aeson..= path | Just path <- [r.redirectPath]]
            <> [ "httpsRedirect" Aeson..= r.redirectHttps
               , "redirectResponseCode" Aeson..= redirectCodeText r.redirectCode
               , "stripQuery" Aeson..= False
               ]

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

A map with host rules carries a @description@: a fingerprint of the rest of
it ('urlMapStamp'), which is how the check tells the map that is there from
the declared one beyond its hosts. A map with no rule has none, and neither
@hostRules@ nor @pathMatchers@: it is what @url-maps create
--default-service@ makes, and is imported only to take rules off a map that
has some (see 'lbParts').
-}
renderUrlMap :: ApplicationLoadBalancer -> Aeson.Value
renderUrlMap alb =
    Aeson.object (urlMapFields alb <> ["description" Aeson..= stamp | Just stamp <- [urlMapStamp alb]])

{- | The fingerprint a URL map with host rules is stamped with (its
@description@), 'Nothing' for a map with none.

It is the /declaration's/ fingerprint: a map edited by hand that kept its
description still reads as the declared one. How much the check leans on it
depends on what the map names, see 'urlMapStampIsRequired'.
-}
urlMapStamp :: ApplicationLoadBalancer -> Maybe Text
urlMapStamp alb
    | null alb.albHostRules = Nothing
    | otherwise = Just (stampOf (Aeson.object (urlMapFields alb)))

{- | Whether the check /requires/ the live map to carry the declared
fingerprint, or only refuses a different one.

Required for a map naming a backend bucket or a redirect, or rewriting a
path, as it has been since those exist: there, the same hosts can be a
different map (a redirect sent elsewhere, another path asked of the backend). A map of backend services only was written without a
description until it got one, and such a map is live under balancers that
are up: requiring the stamp would re-import every one of them on the first
pass, and would never settle if @url-maps import@ turned out not to keep a
@description@ (which nobody has verified on a real project). So for those the
stamp only ever speaks against a map: a live description that is a
@salmon:@ fingerprint other than the declared one is a map salmon wrote for
another declaration, and one with no such description is judged on its host
set alone.
-}
urlMapStampIsRequired :: ApplicationLoadBalancer -> Bool
urlMapStampIsRequired alb =
    any novel (targets alb)
        || any ((/= KeepPath) . pathRuleRewrite) (concatMap hostRulePaths alb.albHostRules)
  where
    novel = \case
        NamedBucket _ -> True
        RedirectTo _ -> True
        _ -> False

stampOf :: Aeson.Value -> Text
stampOf v =
    "salmon:"
        <> Text.pack (concatMap hex (ByteString.unpack (ByteString.take 8 (SHA256.hash (LByteString.toStrict (Aeson.encode v))))))
  where
    hex w = let h = showHex w "" in if length h < 2 then '0' : h else h

{- | What this balancer writes in the @description@ of the backend buckets,
Certificate Manager certificates and DNS authorizations it declares, and the
only thing that makes one of them, once it is no longer declared, this
balancer's to delete (see 'lbParts'). It names the balancer, because a name
alone does not: @web-eu-assets-bucket@ is balancer @web@'s by its shape and
balancer @web-eu@'s in fact.
-}
ownershipMarker :: ApplicationLoadBalancer -> Text
ownershipMarker alb = "salmon:lb:" <> alb.albName

httpRedirectUrlMapName :: ApplicationLoadBalancer -> Text
httpRedirectUrlMapName alb = alb.albName <> "-http-redirect-url-map"

{- | The second URL map of 'RedirectToHttps': no host rule, no service, every
request answered with a redirect to the same host and path over HTTPS. It
carries its own fingerprint as @description@, like a stamped 'renderUrlMap'.
-}
renderHttpRedirectUrlMap :: ApplicationLoadBalancer -> RedirectCode -> Aeson.Value
renderHttpRedirectUrlMap alb code =
    Aeson.object (fields <> ["description" Aeson..= stampOf (Aeson.object fields)])
  where
    fields =
        [ "name" Aeson..= httpRedirectUrlMapName alb
        , "defaultUrlRedirect" Aeson..= renderRedirect (Redirect Nothing Nothing True code)
        ]

{- | Every @description@ a redirect-only map of this balancer can carry: the
fingerprint 'renderHttpRedirectUrlMap' writes, for each code there is. A map
under that name with one of these is one this module wrote.
-}
httpRedirectUrlMapStamps :: ApplicationLoadBalancer -> [Text]
httpRedirectUrlMapStamps alb =
    [ stamp
    | code <- [MovedPermanently, Found, SeeOther, TemporaryRedirect, PermanentRedirect]
    , Aeson.Object o <- [renderHttpRedirectUrlMap alb code]
    , Just (Aeson.String stamp) <- [KeyMap.lookup "description" o]
    ]

urlMapFields :: ApplicationLoadBalancer -> [Aeson.Pair]
urlMapFields alb =
    [ "name" Aeson..= (alb.albName <> "-url-map")
    , "defaultService" Aeson..= serviceUrl DefaultService
    ]
        <> concat
            [ [ "hostRules" Aeson..= map hostRule rules
              , "pathMatchers" Aeson..= map pathMatcher rules
              ]
            | not (null rules)
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
            , target "defaultService" "defaultUrlRedirect" r.hostRuleService
            ]
                <> ["pathRules" Aeson..= map pathRule r.hostRulePaths | not (null r.hostRulePaths)]
    pathRule :: PathRule -> Aeson.Value
    pathRule p =
        Aeson.object $
            ["paths" Aeson..= p.pathRulePaths, target "service" "urlRedirect" p.pathRuleService]
                -- absent, not empty, without a rewrite: the map (and its
                -- fingerprint) of a declaration with none is what it was
                <> [ "routeAction" Aeson..= Aeson.object ["urlRewrite" Aeson..= Aeson.object ["pathPrefixRewrite" Aeson..= prefix]]
                   | RewritePrefix prefix <- [p.pathRuleRewrite]
                   ]
    -- a backend bucket goes where a backend service does; a redirect has a key of its own
    target :: Aeson.Key -> Aeson.Key -> ServiceRef -> Aeson.Pair
    target serviceKey redirectKey = \case
        RedirectTo r -> redirectKey Aeson..= renderRedirect r
        sref -> serviceKey Aeson..= serviceUrl sref
    serviceUrl :: ServiceRef -> Text
    serviceUrl sref =
        "https://www.googleapis.com/compute/v1/projects/"
            <> alb.albProject.projectId
            <> "/regions/"
            <> alb.albRegion.regionName
            <> collection
            <> serviceResource alb sref
      where
        collection = case sref of
            NamedBucket _ -> "/backendBuckets/"
            _ -> "/backendServices/"

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

-- | One resource of a balancer, as 'lbParts' and the node graph name it.
data Part
    = HealthCheckPart Text
    | -- | the named ports this balancer wants on one instance group
      NamedPortsPart Text InstanceGroupLocation
    | -- | by resource name (@\<balancer\>-backend@, @\<balancer\>-\<name\>-backend@)
      BackendServicePart Text
    | NetworkEndpointGroupPart Text
    | -- | a backend service's resource name, and the instance group or NEG attached to it
      BackendPart Text Text
    | -- | by resource name (@\<balancer\>-\<name\>-bucket@)
      BackendBucketPart Text
    | UrlMapPart
    | -- | the redirect-only URL map of 'RedirectToHttps'
      HttpRedirectUrlMapPart
    | -- | 'NoHttp': the removal of the HTTP proxy and the @:80@ rule
      NoHttpPart
    | -- | by authorization name, see 'dnsAuthorizations'
      DnsAuthorizationPart Text
    | -- | by resource name, see 'certificateResource'
      CertificatePart Text
    | -- | by a 'DomainSetCertificate''s base name: the deletion of the
      -- certificates of that base the proxy no longer serves
      SupersededCertificatesPart Text
    | AddressPart
    | HttpProxyPart
    | HttpsProxyPart
    | ForwardingRulePart
    | HttpsForwardingRulePart
    | -- | the removal of what this balancer made and no longer declares: backend
      -- buckets, Certificate Manager certificates, DNS authorizations and the
      -- redirect-only URL map (see 'lbParts')
      LeftoversPart
    deriving (Eq, Ord, Show)

{- | A resource, the resources it needs first, and the lines of bash that
create it, ask after it and remove it. The lines read @$PROJECT@ and
@$REGION@ and the helpers the scripts' headers define, so they only run
inside 'renderPartUpScript' and friends (or the whole-balancer scripts,
which are these lines concatenated).
-}
data PartSpec = PartSpec
    { partId :: Part
    , partDeps :: [Part]
    , partHelp :: Text
    , partNotes :: [Text]
    -- ^ what a re-declaration can be seen to change about this resource
    , partUp :: [Text]
    , partCheck :: [Text]
    -- ^ read-only; findings are @MISSING@\/@CERT@ lines, see 'interpretLbCheck'
    , partDown :: [Text]
    -- ^ empty for what this recipe sets but does not own (a group's named ports)
    }
    deriving (Eq, Show)

{- | The balancer as resources, in an order in which every resource comes
after the ones it depends on.

The edges:

* a backend service depends on its health check;
* a backend attachment depends on its backend service, on the NEG or on the
  instance group's named ports it sends to, and on the attachment declared
  before it on the same service -- two @add-backend@ calls on one backend
  service must not run at once, and an edge is the only thing that says so;
* the URL map depends on every backend service and backend bucket it names;
* a managed certificate depends on its DNS authorizations;
* the HTTP proxy depends on the URL map (on the redirect-only one under
  'RedirectToHttps'), the HTTPS one on the certificates too;
* a forwarding rule depends on its proxy, and on the reserved address when
  there is one.

Some things are /set/ on every @up@ of their node rather than guarded,
because they are declarations that can change under a resource that already
exists: a backend service's timeout and port name (@update --timeout@,
@update --port-name@), an instance group's named ports and, when there are
host rules, the whole URL map (@url-maps import@, which replaces it). A URL
map with no host rule is created once, and imported only when the map that is
there has host rules (the last rule of a declaration taken away).

The URL map's check compares hosts as a set: a declared host the map lacks,
and a host of the map that no rule declares, are both findings, so a rule
removed from the declaration is imported away. The map is this balancer's by
its name, and that makes a host rule added to it by hand one the next @up@
removes (it already did whenever anything else made the map's @up@ run).
Which service a host is sent to, and its path rules, are seen only through
the fingerprint in the map's @description@: required of a map naming a
bucket or a redirect, and for a map of backend services only a finding just
when the live fingerprint is another declaration's -- a map written before
maps of services were stamped has none and is left alone until something
else imports it ('urlMapStampIsRequired').
Everything else is guarded by a @describe@ (or, for an attachment, a look at
the backend service's current backends) and never suffixed with @|| true@:
a failing create fails its node.

An instance group's named ports are set once per group, the union over the
services naming it, and /merged/ with what the group already carries:
@set-named-ports@ replaces the whole set, and the group is the caller's --
another balancer, or the caller, may have named ports on it. Only the names
this balancer declares are overwritten. Nothing removes a name: one this
balancer stopped declaring stays on the group, where it does no harm, and so
does everything on @down@.

The HTTPS proxy's certificate list is the one declaration that is neither
created once nor set on every @up@: it is compared, and updated
(@target-https-proxies update@) only when it differs /and/ every declared
managed certificate is @ACTIVE@. Until then the proxy serves what it served
and its check reads 'Unknown' (the certificate's state) rather than a
'Failure' no @up@ would fix. What its @up@ does in the meantime depends on
the declaration. With a 'DomainSetCertificate' in it, a certificate still
being issued is the expected middle of a swap, which takes minutes to an
hour: @up@ names the certificate on stderr, prints a @PENDING certificate
swap@ line, touches nothing and /succeeds/, so the pass exits 0 with TLS
served on the old certificate and neither the root nor anything hung on it is
'Blocked'; a later pass (or the tending loop, whose check turns to a
'Failure' once the certificate is @ACTIVE@) makes the move. A certificate
that is @FAILED@ or absent will never be @ACTIVE@ and still fails the node.
Without a 'DomainSetCertificate' (a certificate added or renamed under a
'ManagedCertificate') @up@ fails as it always did, naming the certificate
that is not @ACTIVE@. That, with a 'DomainSetCertificate' being a new resource whenever its
domain set changes, is how a certificate is replaced on a live balancer:
create beside, wait, move the proxy, and only then delete -- the last by a
node of its own per base ('SupersededCertificatesPart'), which depends on the
proxy, lists the base's certificates, and deletes those that are not the
declared one and that the proxy does not serve; one the proxy still serves is
left and said (not a failure of @up@, and @WAITING@, so 'Unknown', for the
check) until the swap is made. A managed certificate's own
@down@ refuses the same way while the proxy still lists it, so a retired
declaration's teardown cannot pull a certificate out from under a proxy that
has not moved yet. The proxy's URL map is named at creation and never set
again.

The HTTP proxy's URL map is the same by default ('HttpCreatedOnce'): named
at creation, never set again. Under 'ServeHttp' and 'RedirectToHttps' it is
compared and updated when it differs. 'NoHttp' is a node whose @up@ removes
the @:80@ rule and the HTTP proxy.

Nothing in a declaration remembers what it used to name, so what a balancer
made and no longer declares is found on GCP, by the last node
('LeftoversPart'), which comes after the URL map, the proxies and the
superseded certificates. It deletes four kinds of resource and no other, each
under a proof that this balancer made it, and never one still in use:

* a backend bucket named @\<balancer\>-\<n\>-bucket@ whose @description@ is
  'ownershipMarker', once no URL map of the project names it;
* a Certificate Manager certificate whose @description@ is the marker, once
  no HTTPS proxy of the project serves it (so not the old certificate of a
  swap still pending, nor one the proxy of a failed pass is still on);
* a DNS authorization whose @description@ is the marker, once no certificate
  of the location uses it (the record published for it is the caller's);
* the redirect-only map @\<balancer\>-http-redirect-url-map@, when the
  declaration is no longer 'RedirectToHttps', its @description@ is one of the
  fingerprints this module writes for it and no proxy of the project is on
  it. Under 'HttpCreatedOnce' the HTTP proxy is never moved off it, so there
  it stays for as long as that proxy does.

The marker is written by that same node, on the declared resources that
exist and have no description (@update --description@), so the scripts of the
resources themselves are what they were. A resource that is no longer
declared and was never marked -- one dropped from the declaration before the
marker existed -- is therefore __not__ deleted, and neither is one carrying
somebody's description: both are left for a command by hand. A listing only
enumerates candidates; what is deleted is what carries the marker.

What is still in use is said (@WAITING@, so 'Unknown', for the check; a line
on stderr for @up@) and left. A delete or a marking that fails fails that
node, and so the pass and the balancer's root: it is the last node, so every
resource that serves traffic was applied before it. A listing that fails (the
Certificate Manager API of a balancer that never had a certificate) is said
and is nothing to clean.

Not done: none of this runs at teardown, where a leftover is left behind (as
is a superseded certificate of a swap that never completed); a backend
service, a health check, a NEG, the HTTPS proxy, its rule and the address of
a declaration that no longer names them are not removed.

On the way down an attachment is /detached/ (@remove-backend@) rather than
left to the backend service's deletion, because a NEG still attached cannot
be deleted and the NEG's node knows nothing of the service's. An instance
group is never deleted (it is the caller's), nor is a 'ComputeCertificate'.

The balancer is a /regional external/ Application Load Balancer
(@EXTERNAL_MANAGED@), which GCP only accepts in a VPC network that already
has a proxy-only subnet in the region. Nothing here creates one -- see
"Salmon.Builtin.Nodes.Gcp.Compute".@subnet@ with
'Salmon.Builtin.Nodes.Gcp.Compute.RegionalManagedProxy', which is a
prerequisite to hand 'applicationLoadBalancerAfter'.
-}
lbParts :: ApplicationLoadBalancer -> [PartSpec]
lbParts alb =
    nubBy ((==) `on` partId) $
        map healthCheckPart (healthChecks alb)
            <> map namedPortsPart groups
            <> concatMap serviceParts (services alb)
            <> map bucketPart alb.albBuckets
            <> [urlMapPart]
            <> concatMap certificateParts alb.albCertificates
            <> [addressPart | https]
            <> [httpRedirectUrlMapPart code | RedirectToHttps code <- [alb.albHttp]]
            <> [httpProxyPart | http]
            <> [httpsProxyPart | https]
            <> [supersededPart base (certificateResource c) | c@(DomainSetCertificate base _) <- alb.albCertificates]
            <> [forwardingRulePart | http]
            <> [httpsForwardingRulePart | https]
            <> [noHttpPart | not http]
            <> [leftoversPart]
  where
    https = serveHttps alb
    http = alb.albHttp /= NoHttp
    -- whether a certificate here is replaced by a swap (see 'httpsProxyPart')
    domainSets = not (null [() | DomainSetCertificate _ _ <- alb.albCertificates])
    groups = groupNamedPorts alb

    resourceName :: Text -> Text
    resourceName suffix = shellQuote (alb.albName <> suffix)

    ensure :: Text -> Text -> Text
    ensure describeCmd createCmd =
        "exists " <> describeCmd <> " || " <> createCmd

    need :: Text -> Text -> Text
    need what describeCmd = "need " <> shellQuote what <> " " <> describeCmd

    describeCompute :: Text -> Text -> Text
    describeCompute coll name = "gcloud compute " <> coll <> " describe " <> shellQuote name <> regional

    deleteCompute :: Text -> Text -> Text
    deleteCompute coll name =
        "if exists " <> describeCompute coll name
            <> "; then gcloud compute " <> coll <> " delete " <> shellQuote name
            <> regional <> " --quiet; fi"

    describeLocated :: Text -> Text -> Text
    describeLocated coll name = "gcloud certificate-manager " <> coll <> " describe " <> shellQuote name <> located

    deleteLocated :: Text -> Text -> Text
    deleteLocated coll name =
        "if exists " <> describeLocated coll name
            <> "; then gcloud certificate-manager " <> coll <> " delete " <> shellQuote name
            <> located <> " --quiet; fi"

    -- A resource that is created once and has nothing to set afterwards.
    simple :: Part -> [Part] -> Text -> Text -> Text -> Text -> PartSpec
    simple part needs what coll name createFlags =
        PartSpec
            { partId = part
            , partDeps = needs
            , partHelp = Text.unwords [what, name]
            , partNotes = []
            , partUp =
                [ ensure
                    (describeCompute coll name)
                    ("gcloud compute " <> coll <> " create " <> shellQuote name <> regional <> createFlags)
                ]
            , partCheck = [need (coll <> " " <> name) (describeCompute coll name)]
            , partDown = [deleteCompute coll name]
            }

    healthCheckPart :: HealthCheck -> PartSpec
    healthCheckPart hc =
        ( simple
            (HealthCheckPart hc.healthCheckName)
            []
            "health check"
            "health-checks"
            hc.healthCheckName
            ""
        )
            { partNotes = ["tcp port " <> Text.pack (show hc.healthCheckPort)]
            , partUp =
                [ ensure
                    (describeCompute "health-checks" hc.healthCheckName)
                    ( "gcloud compute health-checks create tcp " <> shellQuote hc.healthCheckName
                        <> regional
                        <> " --port="
                        <> Text.pack (show hc.healthCheckPort)
                    )
                ]
            }

    -- set-named-ports replaces the whole set, so: one call per group,
    -- carrying every port of every service naming it, after whatever the
    -- group already has under names that are not ours. The read is an
    -- assignment on a line of its own so that its failing fails the script
    -- (a command substitution inside an argument would not).
    namedPortsPart :: ((Text, InstanceGroupLocation), [(Text, Int)]) -> PartSpec
    namedPortsPart ((ig, loc), named) =
        PartSpec
            { partId = NamedPortsPart ig loc
            , partDeps = []
            , partHelp = Text.unwords ["named ports of load balancer", alb.albName, "on instance group", ig]
            , partNotes = [n <> ":" <> Text.pack (show p) | (n, p) <- nub named]
            , partUp =
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
            , partCheck =
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
            , partDown = []
            }

    instanceGroupHealthCheck :: Svc -> Maybe HealthCheck
    instanceGroupHealthCheck svc =
        if any isInstanceGroup svc.svcBackends then svc.svcHealthCheck else Nothing

    serviceParts :: Svc -> [PartSpec]
    serviceParts svc = servicePart svc : attachments Nothing backends
      where
        -- one attachment per group or NEG, whatever number of times it is listed
        backends = nubBy ((==) `on` backendKey svc) svc.svcBackends
        attachments _ [] = []
        attachments previous (b : bs) =
            let (neg, attachment) = backendParts svc previous b
             in neg <> [attachment] <> attachments (Just attachment.partId) bs

    servicePart :: Svc -> PartSpec
    servicePart svc =
        PartSpec
            { partId = BackendServicePart svc.svcResource
            , partDeps = [HealthCheckPart hc.healthCheckName | Just hc <- [instanceGroupHealthCheck svc]]
            , partHelp = Text.unwords ["backend service", svc.svcResource]
            , partNotes =
                ["timeout " <> Text.pack (show t) <> "s" | Just t <- [svc.svcTimeout]]
                    <> ["port name " <> n | Just n <- [svcPortName svc]]
                    <> ["health check " <> hc.healthCheckName | Just hc <- [instanceGroupHealthCheck svc]]
            , partUp =
                [ ensure
                    (describeCompute "backend-services" svc.svcResource)
                    ( "gcloud compute backend-services create " <> shellQuote svc.svcResource
                        <> regional
                        <> " --protocol=HTTP"
                        <> maybe "" ((" --port-name=" <>) . shellQuote) (svcPortName svc)
                        <> " --load-balancing-scheme=EXTERNAL_MANAGED"
                        <> maybe "" (\hc -> " --health-checks=" <> shellQuote hc.healthCheckName <> " --health-checks-region=\"$REGION\"") (instanceGroupHealthCheck svc)
                    )
                ]
                    -- a set: a backend service made before it had a port
                    -- name of its own is still on @http@
                    <> [ "gcloud compute backend-services update " <> shellQuote svc.svcResource
                            <> regional
                            <> " --port-name="
                            <> shellQuote n
                       | Just n <- [svcPortName svc]
                       ]
                    -- a set, not a create flag: the declared timeout has to
                    -- reach a backend service that already exists too
                    <> [ "gcloud compute backend-services update " <> shellQuote svc.svcResource
                            <> regional
                            <> " --timeout="
                            <> Text.pack (show t)
                       | Just t <- [svc.svcTimeout]
                       ]
            , partCheck =
                [need ("backend-services " <> svc.svcResource) (describeCompute "backend-services" svc.svcResource)]
                    <> [ "[ \"$(" <> describeCompute "backend-services" svc.svcResource
                            <> " --format='value(timeoutSec)' 2>/dev/null)\" = "
                            <> shellQuote (Text.pack (show t))
                            <> " ] || echo "
                            <> shellQuote ("MISSING timeout " <> Text.pack (show t) <> "s on " <> svc.svcResource)
                       | Just t <- [svc.svcTimeout]
                       ]
                    -- a service on another port name sends to another port, or to none
                    <> [ "[ \"$(" <> describeCompute "backend-services" svc.svcResource
                            <> " --format='value(portName)' 2>/dev/null)\" = "
                            <> shellQuote n
                            <> " ] || echo "
                            <> shellQuote ("MISSING port-name " <> n <> " on " <> svc.svcResource)
                       | Just n <- [svcPortName svc]
                       ]
            , partDown = [deleteCompute "backend-services" svc.svcResource]
            }

    -- what a backend service's @backends[].group@ ends with for this backend
    attachedTo :: Svc -> Text -> Text
    attachedTo svc groupPathSuffix =
        describeCompute "backend-services" svc.svcResource
            <> " --format='value(backends[].group)' 2>/dev/null | tr ';' '\\n' | grep -q -- "
            <> shellQuote (groupPathSuffix <> "$")

    -- The NEG a Cloud Run backend needs (if any), and the attachment.
    -- Attaching the same backend twice is an error, so look first.
    backendParts :: Svc -> Maybe Part -> Backend -> ([PartSpec], PartSpec)
    backendParts svc previous backend = case backend of
        InstanceGroupBackend ig loc _ ->
            ( []
            , attachment
                ig
                [NamedPortsPart ig loc | (ig, loc) `elem` map fst groups]
                ("instance group " <> ig)
                ("/instanceGroups/" <> ig)
                (" --instance-group=" <> shellQuote ig <> groupBackendFlag loc)
                [ need ("instance-group " <> ig) ("gcloud compute instance-groups describe " <> shellQuote ig <> groupLocation loc)
                ]
                ("instance-group " <> ig)
            )
        CloudRunBackend cr ->
            ( [ simple
                    (NetworkEndpointGroupPart svc.svcNeg)
                    []
                    "serverless network endpoint group"
                    "network-endpoint-groups"
                    svc.svcNeg
                    (" --network-endpoint-type=serverless --cloud-run-service=" <> shellQuote cr)
              ]
            , attachment
                svc.svcNeg
                [NetworkEndpointGroupPart svc.svcNeg]
                ("network endpoint group " <> svc.svcNeg)
                ("/networkEndpointGroups/" <> svc.svcNeg)
                (" --network-endpoint-group=" <> shellQuote svc.svcNeg <> " --network-endpoint-group-region=\"$REGION\"")
                []
                ("neg " <> svc.svcNeg)
            )
      where
        attachment key needs what suffix flags checks missing =
            PartSpec
                { partId = BackendPart svc.svcResource key
                , partDeps = BackendServicePart svc.svcResource : needs <> maybe [] pure previous
                , partHelp = Text.unwords ["attaches", what, "to backend service", svc.svcResource]
                , partNotes = []
                , partUp =
                    [ attachedTo svc suffix
                        <> " || gcloud compute backend-services add-backend " <> shellQuote svc.svcResource
                        <> regional
                        <> flags
                    ]
                , partCheck =
                    checks
                        <> [attachedTo svc suffix <> " || echo " <> shellQuote ("MISSING backend " <> missing)]
                , partDown =
                    [ "if " <> attachedTo svc suffix
                        <> "; then gcloud compute backend-services remove-backend " <> shellQuote svc.svcResource
                        <> regional
                        <> flags
                        <> " --quiet; fi"
                    ]
                }

    -- With rules the map is imported whole on every run: `import` creates
    -- or replaces, which is the only "set" verb a URL map has
    -- (`add-path-matcher` appends, and fails the second time). Without
    -- rules it is created once with its default service, and imported
    -- (as that same rule-less map) only when the one that is there has
    -- host rules: a declaration whose last rule was taken away.
    --
    -- The check compares the hosts as a set, both ways: a declared host
    -- the map lacks, and a host of the map no rule declares. Which
    -- service a host goes to, and its paths, are only seen through the
    -- fingerprint (see 'urlMapStampIsRequired').
    urlMapPart :: PartSpec
    urlMapPart =
        PartSpec
            { partId = UrlMapPart
            , partDeps = nub (mapMaybe (targetPart alb) (targets alb))
            , partHelp = Text.unwords ["URL map", urlMap]
            , partNotes =
                concat
                    [ ("hosts " <> Text.unwords r.hostRuleHosts <> " to " <> targetText alb r.hostRuleService)
                        : [ "paths " <> Text.unwords p.pathRulePaths <> " to " <> targetText alb p.pathRuleService <> rewriteText p.pathRuleRewrite
                          | p <- r.hostRulePaths
                          ]
                    | r <- alb.albHostRules
                    ]
            , partUp =
                if null alb.albHostRules
                    then
                        [ ensure
                            (describeCompute "url-maps" urlMap)
                            ( "gcloud compute url-maps create " <> shellQuote urlMap
                                <> regional
                                <> " --default-service=" <> resourceName "-backend"
                            )
                        , "ruled=$(" <> liveHosts <> ")"
                        , "if [ -n \"$ruled\" ]; then " <> importMap urlMap (renderUrlMap alb) <> "; fi"
                        ]
                    else
                        [importMap urlMap (renderUrlMap alb)]
            , partCheck =
                need ("url-maps " <> urlMap) (describeCompute "url-maps" urlMap)
                    : [ liveHosts
                            <> " 2>/dev/null | grep -qxF -- "
                            <> shellQuote h
                            <> " || echo "
                            <> shellQuote ("MISSING host-rule " <> h)
                      | h <- declaredHosts
                      ]
                    <> [ liveHosts
                            <> " 2>/dev/null | while read -r h; do case "
                            <> shellQuote (" " <> Text.unwords declaredHosts <> " ")
                            <> " in *\" $h \"*) ;; *) echo \"MISSING removal of undeclared host-rule $h\";; esac; done; true"
                       ]
                    <> [ (if urlMapStampIsRequired alb then stampCheck else staleStampCheck) urlMap stamp
                       | Just stamp <- [urlMapStamp alb]
                       ]
            , partDown = [deleteCompute "url-maps" urlMap]
            }
      where
        urlMap = alb.albName <> "-url-map"
        declaredHosts = concatMap hostRuleHosts alb.albHostRules
        -- every host of every rule of the map that is there, one per
        -- line, whatever separators gcloud flattens the nested lists with
        liveHosts =
            "{ " <> describeCompute "url-maps" urlMap
                <> " --format='value(hostRules[].hosts)' | tr \";,[]' \\t\" '\\n' | sed '/^$/d'; }"

    -- whether the map that is there is the declared one, by its description
    stampCheck :: Text -> Text -> Text
    stampCheck urlMap stamp =
        "[ \"$(" <> describeCompute "url-maps" urlMap
            <> " --format='value(description)' 2>/dev/null)\" = "
            <> shellQuote stamp
            <> " ] || echo "
            <> shellQuote ("MISSING declared rules on " <> urlMap <> " (" <> stamp <> ")")

    -- the same, for a map that may have been written before it was
    -- stamped: only a fingerprint of another declaration speaks against it
    staleStampCheck :: Text -> Text -> Text
    staleStampCheck urlMap stamp =
        "case \"$(" <> describeCompute "url-maps" urlMap
            <> " --format='value(description)' 2>/dev/null)\" in "
            <> shellQuote stamp
            <> ") ;; salmon:*) echo "
            <> shellQuote ("MISSING declared rules on " <> urlMap <> " (" <> stamp <> ")")
            <> ";; esac"

    importMap :: Text -> Aeson.Value -> Text
    importMap urlMap v =
        "printf '%s\\n' " <> shellQuote (jsonText v)
            <> " | gcloud compute url-maps import " <> shellQuote urlMap
            <> regional
            <> " --quiet"

    -- The Cloud Storage bucket is a declaration that can change under a
    -- backend bucket that exists: compared, and updated when it differs.
    bucketPart :: BackendBucket -> PartSpec
    bucketPart b =
        PartSpec
            { partId = BackendBucketPart res
            , partDeps = []
            , partHelp = Text.unwords ["backend bucket", res]
            , partNotes = ["bucket " <> gcs]
            , partUp =
                [ ensure
                    (describeCompute "backend-buckets" res)
                    ( "gcloud compute backend-buckets create " <> shellQuote res
                        <> regional
                        <> " --gcs-bucket-name=" <> shellQuote gcs
                        <> " --load-balancing-scheme=EXTERNAL_MANAGED"
                    )
                , "have=$(" <> describeCompute "backend-buckets" res <> " --format='value(bucketName)')"
                , "[ \"$have\" = " <> shellQuote gcs <> " ] || gcloud compute backend-buckets update " <> shellQuote res
                    <> regional
                    <> " --gcs-bucket-name=" <> shellQuote gcs
                ]
            , partCheck =
                [ need ("backend-buckets " <> res) (describeCompute "backend-buckets" res)
                , "[ \"$(" <> describeCompute "backend-buckets" res
                    <> " --format='value(bucketName)' 2>/dev/null)\" = "
                    <> shellQuote gcs
                    <> " ] || echo "
                    <> shellQuote ("MISSING gcs-bucket " <> gcs <> " on " <> res)
                ]
            , partDown = [deleteCompute "backend-buckets" res]
            }
      where
        res = bucketResource alb b.backendBucketName
        gcs = b.backendBucketGcsBucket

    -- Imported whole on every run, like the balancer's own map with rules.
    httpRedirectUrlMapPart :: RedirectCode -> PartSpec
    httpRedirectUrlMapPart code =
        PartSpec
            { partId = HttpRedirectUrlMapPart
            , partDeps = []
            , partHelp = Text.unwords ["URL map", urlMap, "(HTTP to HTTPS)"]
            , partNotes = [redirectText (Redirect Nothing Nothing True code)]
            , partUp = [importMap urlMap rendered]
            , partCheck =
                need ("url-maps " <> urlMap) (describeCompute "url-maps" urlMap)
                    : [stampCheck urlMap stamp | Just (Aeson.String stamp) <- [description rendered]]
            , partDown = [deleteCompute "url-maps" urlMap]
            }
      where
        urlMap = httpRedirectUrlMapName alb
        rendered = renderHttpRedirectUrlMap alb code
        description = \case
            Aeson.Object o -> KeyMap.lookup "description" o
            _ -> Nothing

    -- 'NoHttp': what an earlier declaration made for port 80 goes, the rule
    -- before the proxy it points at. Nothing to do on the way down.
    noHttpPart :: PartSpec
    noHttpPart =
        PartSpec
            { partId = NoHttpPart
            , partDeps = []
            , partHelp = Text.unwords ["no HTTP listener on load balancer", alb.albName]
            , partNotes = []
            , partUp = [deleteCompute coll name | (coll, name) <- listener]
            , partCheck =
                [ "if exists " <> describeCompute coll name <> "; then echo "
                    <> shellQuote ("MISSING removal of " <> coll <> " " <> name)
                    <> "; fi"
                | (coll, name) <- listener
                ]
            , partDown = []
            }
      where
        listener = [("forwarding-rules", alb.albName <> "-fw"), ("target-http-proxies", alb.albName <> "-proxy")]

    certificateParts :: Certificate -> [PartSpec]
    certificateParts = \case
        -- Somebody else's: asked after, never created or deleted. Its @up@
        -- fails when it is not there, so that the failure names the
        -- certificate rather than the proxy that could not find it.
        ComputeCertificate n ->
            [ PartSpec
                { partId = CertificatePart n
                , partDeps = []
                , partHelp = Text.unwords ["certificate", n, "(not managed here)"]
                , partNotes = []
                , partUp =
                    [ "exists " <> describeCompute "ssl-certificates" n
                        <> " || { echo "
                        <> shellQuote ("certificate " <> n <> " does not exist, and is not this balancer's to create")
                        <> " >&2; exit 1; }"
                    ]
                , partCheck = [need ("ssl-certificates " <> n) (describeCompute "ssl-certificates" n)]
                , partDown = []
                }
            ]
        c@(ManagedCertificate n ds) -> managedParts (certificateResource c) n ds
        c@(DomainSetCertificate base ds) -> managedParts (certificateResource c) base ds

    -- One line per name, lower-cased and sorted, then joined with a space
    -- after each: how a list read back from gcloud is compared with a
    -- declared one, whatever separators @value()@ flattened it with.
    asSet :: Text
    asSet = " | tr \";,[]' \\t\" '\\n' | sed -e 's|.*/||' -e '/^$/d' | tr 'A-Z' 'a-z' | LC_ALL=C sort -u | tr '\\n' ' '"

    setText :: [Text] -> Text
    setText = Text.concat . map (<> " ") . normalDomains

    httpsProxy :: Text
    httpsProxy = alb.albName <> "-https-proxy"

    -- The names of the certificates the HTTPS proxy serves now, 'asSet'.
    -- A Certificate Manager certificate is listed there by its full
    -- resource path, a compute one by its URL: the last segment either way.
    proxyCertificates :: Text
    proxyCertificates =
        describeCompute "target-https-proxies" httpsProxy
            <> " --format='value(sslCertificates)' 2>/dev/null"
            <> asSet

    -- The DNS authorizations (named after @base@) and the certificate
    -- @cert@ over them.
    managedParts :: Text -> Text -> [Text] -> [PartSpec]
    managedParts cert base ds =
        [ PartSpec
            { partId = DnsAuthorizationPart authz
            , partDeps = []
            , partHelp = Text.unwords ["DNS authorization", authz, "for", d]
            , partNotes = []
            , partUp =
                [ ensure
                    (describeLocated "dns-authorizations" authz)
                    ( "gcloud certificate-manager dns-authorizations create " <> shellQuote authz
                        <> located
                        <> " --domain=" <> shellQuote d
                        <> " --type=PER_PROJECT_RECORD"
                    )
                ]
            , partCheck = [need ("dns-authorization " <> authz) (describeLocated "dns-authorizations" authz)]
            , partDown = [deleteLocated "dns-authorizations" authz]
            }
        | d <- ds
        , let authz = authorizationName base d
        ]
            <> [ PartSpec
                    { partId = CertificatePart cert
                    , -- and, going down, the certificate first: an
                      -- authorization in use cannot be deleted
                      partDeps = map (DnsAuthorizationPart . authorizationName base) ds
                    , partHelp = Text.unwords ["managed certificate", cert]
                    , partNotes = ["for " <> Text.unwords ds]
                    , partUp =
                        [ ensure
                            (describeLocated "certificates" cert)
                            ( "gcloud certificate-manager certificates create " <> shellQuote cert
                                <> located
                                <> " --domains=" <> shellQuote (Text.intercalate "," ds)
                                <> " --dns-authorizations=" <> shellQuote (Text.intercalate "," (map (authorizationName base) ds))
                            )
                        , -- a certificate's names cannot be edited: one that
                          -- covers other names than the declared ones is not
                          -- this declaration's, and saying nothing is how a
                          -- host added to the list never got a certificate
                          "got=$(" <> describeLocated "certificates" cert <> " --format='value(managed.domains)'" <> asSet <> ")"
                        , "[ -z \"$got\" ] || [ \"$got\" = " <> shellQuote (setText ds) <> " ] || { echo "
                            <> shellQuote ("certificate " <> cert <> " exists and covers other names than the declared ones (" <> Text.unwords (normalDomains ds) <> "):")
                            <> "\" $got\""
                            <> shellQuote "-- a certificate's names cannot be edited; declare it as a DomainSetCertificate, or under a new name"
                            <> " >&2; exit 1; }"
                        ]
                    , partCheck =
                        [ need ("certificate " <> cert) (describeLocated "certificates" cert)
                        , "st=$(" <> describeLocated "certificates" cert
                            <> " --format='value(managed.state)' 2>/dev/null); [ -n \"$st\" ] && echo "
                            <> shellQuote ("CERT " <> cert)
                            <> "\" $st\"; true"
                        , "got=$(" <> describeLocated "certificates" cert <> " --format='value(managed.domains)' 2>/dev/null" <> asSet <> ")"
                        , "[ -z \"$got\" ] || [ \"$got\" = " <> shellQuote (setText ds) <> " ] || echo "
                            <> shellQuote ("PROBLEM certificate " <> cert <> " covers other names than the declared ones (immutable): has")
                            <> "\" $got\""
                        , -- a certificate whose renewal failed stays ACTIVE,
                          -- until the day it expires
                          "if [ \"$st\" = ACTIVE ]; then"
                        , "  if " <> describeLocated "certificates" cert
                            <> " --format='value(managed.authorizationAttemptInfo[].state)' 2>/dev/null | tr \";,[]' \\t\" '\\n' | grep -qxF FAILED; then echo "
                            <> shellQuote ("PROBLEM certificate " <> cert <> " is ACTIVE but an authorization attempt FAILED (it will not renew)")
                            <> "; fi"
                        , "  exp=$(" <> describeLocated "certificates" cert <> " --format='value(expireTime)' 2>/dev/null)"
                        , "  if [ -n \"$exp\" ] && t=$(date -d \"$exp\" +%s 2>/dev/null) && [ \"$t\" -le \"$(date +%s)\" ]; then echo "
                            <> shellQuote ("PROBLEM certificate " <> cert <> " expired")
                            <> "\" $exp\"; fi"
                        , "fi"
                        ]
                    , -- never from under the proxy: a certificate the HTTPS
                      -- proxy still serves is left, and its node fails, until
                      -- the proxy is gone or has been moved off it
                      partDown =
                        [ "if exists " <> describeLocated "certificates" cert <> "; then"
                        , "  if " <> servedByProxy (shellQuote cert) <> "; then echo "
                            <> shellQuote ("certificate " <> cert <> " is still served by " <> httpsProxy <> ": not deleted")
                            <> " >&2; exit 1; fi"
                        , "  gcloud certificate-manager certificates delete " <> shellQuote cert <> located <> " --quiet"
                        , "fi"
                        ]
                    }
               ]

    -- whether the HTTPS proxy lists a certificate (a shell word naming it)
    servedByProxy :: Text -> Text
    servedByProxy word =
        describeCompute "target-https-proxies" httpsProxy
            <> " --format='value(sslCertificates)' 2>/dev/null | tr \";,[]' \\t\" '\\n' | sed 's|.*/||' | grep -qxF -- "
            <> word

    -- The certificates of one base that are not the declared one: the bare
    -- base name (what a 'ManagedCertificate' of that name was) and the base
    -- with another set's tag. Asked of Certificate Manager, since nothing in
    -- a declaration remembers the sets it used to name.
    supersededPart :: Text -> Text -> PartSpec
    supersededPart base current =
        PartSpec
            { partId = SupersededCertificatesPart base
            , -- after the proxy: it is the proxy's update that supersedes
              partDeps = [HttpsProxyPart, CertificatePart current]
            , partHelp = Text.unwords ["superseded certificates of", base]
            , partNotes = ["keeps " <> current]
            , partUp =
                [ "stale=$(" <> listSuperseded <> ")"
                , "for c in $stale; do"
                , -- one the proxy still serves is the swap not made yet (see
                  -- 'httpsProxyPart'): left, said, and not a failure
                  "  if " <> servedByProxy "\"$c\"" <> "; then echo \"certificate $c is still served by \""
                    <> shellQuote httpsProxy
                    <> "\": not deleted\" >&2; continue; fi"
                , "  gcloud certificate-manager certificates delete \"$c\"" <> located <> " --quiet"
                , "done"
                ]
            , partCheck =
                [ listSuperseded <> " 2>/dev/null | while read -r c; do if " <> servedByProxy "\"$c\""
                    <> "; then echo \"WAITING removal of superseded certificate $c, still served by \""
                    <> shellQuote httpsProxy
                    <> "; else echo \"MISSING removal of superseded certificate $c\"; fi; done; true"
                ]
            , -- the declared certificate's own node removes it; a superseded
              -- one still around at teardown is left (see 'lbParts')
              partDown = []
            }
      where
        listSuperseded =
            "gcloud certificate-manager certificates list" <> located <> " --format='value(name)'"
                <> " | awk -v b=" <> shellQuote base
                <> " -v cur=" <> shellQuote current
                <> " "
                <> shellQuote "{n=$0; sub(/.*\\//,\"\",n); t=substr(n,length(b)+2)} n!=cur && (n==b || (index(n,b\"-\")==1 && length(t)==8 && t ~ /^[0-9a-f]+$/)) {print n}"

    -- What this balancer made and no longer declares. Each @plan_*@ function
    -- prints one line per finding and changes nothing: @MARK coll name@ (a
    -- declared resource with no description), @DELETE coll name@ (ours, not
    -- declared, not in use), @KEEP coll name why@ (ours, not declared, in
    -- use) and @NOTE coll name why@ (said by @up@, nothing for the check).
    -- The check and @up@ read the same lines.
    --
    -- A reference is looked for with a here-string, never @printf | grep
    -- -q@: under @pipefail@ a @grep -q@ that stops reading early can fail
    -- the pipeline, which here would read as "not in use".
    leftoversPart :: PartSpec
    leftoversPart =
        PartSpec
            { partId = LeftoversPart
            , partDeps =
                nub $
                    [UrlMapPart]
                        <> [BackendBucketPart n | n <- declaredBuckets]
                        <> [CertificatePart n | n <- declaredCertificates]
                        <> [HttpProxyPart | http]
                        <> [NoHttpPart | not http]
                        <> [HttpsProxyPart | https]
                        <> [SupersededCertificatesPart base | DomainSetCertificate base _ <- alb.albCertificates]
            , partHelp = Text.unwords ["leftovers of load balancer", alb.albName]
            , partNotes = []
            , partUp =
                plans
                    <> [ "mark() { case \"$1\" in backend-buckets) gcloud compute \"$1\" update \"$2\"" <> regional
                            <> " --description=\"$marker\" ;; *) gcloud certificate-manager \"$1\" update \"$2\""
                            <> located
                            <> " --description=\"$marker\" ;; esac; }"
                       , "remove() { case \"$1\" in backend-buckets|url-maps) gcloud compute \"$1\" delete \"$2\"" <> regional
                            <> " --quiet ;; *) gcloud certificate-manager \"$1\" delete \"$2\""
                            <> located
                            <> " --quiet ;; esac; }"
                       , "apply() {"
                       , "  local plan what coll n rest"
                       , "  plan=$(\"$1\")"
                       , "  while read -r what coll n rest; do"
                       , "    case \"$what\" in"
                       , "      MARK) mark \"$coll\" \"$n\" </dev/null ;;"
                       , "      DELETE) echo \"removing $coll $n: made by load balancer \"" <> shellQuote alb.albName <> "\", no longer declared\" >&2; remove \"$coll\" \"$n\" </dev/null ;;"
                       , "      KEEP|NOTE) echo \"left: $coll $n ($rest)\" >&2 ;;"
                       , "    esac"
                       , "  done <<< \"$plan\""
                       , "}"
                       ]
                    -- certificates before the authorizations they use
                    <> map ("apply " <>) planNames
            , partCheck =
                plans
                    <> [ "{ " <> Text.concat (map (<> "; ") planNames) <> "} | while read -r what coll n rest; do case \"$what\" in"
                            <> " MARK) echo \"MISSING ownership mark on $coll $n\";;"
                            <> " DELETE) echo \"MISSING removal of undeclared $coll $n\";;"
                            <> " KEEP) echo \"WAITING removal of undeclared $coll $n, $rest\";;"
                            <> " esac; done; true"
                       ]
            , -- a leftover still there at teardown is left (see 'lbParts')
              partDown = []
            }
      where
        declaredBuckets = map (bucketResource alb . backendBucketName) alb.albBuckets
        declaredCertificates = [n | (n, _, _) <- managedCertificates alb]
        declaredAuthorizations = map snd (dnsAuthorizations alb)
        redirectMap = httpRedirectUrlMapName alb
        redirecting = case alb.albHttp of
            RedirectToHttps _ -> True
            _ -> False
        planNames =
            ["plan_buckets", "plan_certificates", "plan_authorizations"]
                <> ["plan_redirect_map" | not redirecting]
        declared = shellQuote . Text.unwords
        unlisted coll = "{ echo " <> shellQuote ("NOTE " <> coll <> " - could not be listed") <> "; return 0; }"
        -- the last path segment of every name of a flattened list, one per line
        names = " | tr \";,[]' \\t\" '\\n' | sed 's|.*/||'"
        plans =
            [ "marker=" <> shellQuote (ownershipMarker alb)
            , -- reads a 'value(name,description)' listing; $1 is the declared names
              "classify() { awk -F'\\t' -v m=\"$marker\" -v declared=\" $1 \" "
                <> shellQuote "{n=$1; sub(/.*\\//,\"\",n)} n==\"\"{next} index(declared,\" \" n \" \"){if($2==\"\")print \"MARK:\" n; next} $2==m{print \"OURS:\" n}"
                <> "; }"
            , "plan_buckets() {"
            , "  local all maps listed line n d"
            , "  all=$(gcloud compute backend-buckets list --project=\"$PROJECT\" --format='value(name,description)' 2>/dev/null) || " <> unlisted "backend-buckets"
            , "  maps=''; listed=''"
            , "  for line in $(printf '%s\\n' \"$all\" | classify " <> declared declaredBuckets <> "); do"
            , "    n=${line#*:}"
            , -- the listing is the project's: only a bucket of this region, read again, counts
              "    d=$(gcloud compute backend-buckets describe \"$n\"" <> regional <> " --format='value(description)' 2>/dev/null) || continue"
            , "    case \"$line\" in"
            , "      MARK:*) [ -n \"$d\" ] || echo \"MARK backend-buckets $n\" ;;"
            , "      OURS:*)"
            , "        case \"$n\" in " <> shellQuote (alb.albName <> "-") <> "*-bucket) ;; *) continue ;; esac"
            , "        [ \"$d\" = \"$marker\" ] || continue"
            , "        if [ -z \"$listed\" ]; then maps=$(gcloud compute url-maps list --project=\"$PROJECT\" --format=json 2>/dev/null) || { echo \"KEEP backend-buckets $n the URL maps could not be listed\"; continue; }; listed=1; fi"
            , "        if grep -qF -- \"/regions/$REGION/backendBuckets/$n\\\"\" <<< \"$maps\"; then echo \"KEEP backend-buckets $n a URL map still names it\"; else echo \"DELETE backend-buckets $n\"; fi ;;"
            , "    esac"
            , "  done"
            , "}"
            , "plan_certificates() {"
            , "  local all served listed line n"
            , "  all=$(gcloud certificate-manager certificates list" <> located <> " --format='value(name,description)' 2>/dev/null) || " <> unlisted "certificates"
            , "  served=''; listed=''"
            , "  for line in $(printf '%s\\n' \"$all\" | classify " <> declared declaredCertificates <> "); do"
            , "    n=${line#*:}"
            , "    case \"$line\" in"
            , "      MARK:*) echo \"MARK certificates $n\" ;;"
            , "      OURS:*)"
            , "        if [ -z \"$listed\" ]; then served=$(gcloud compute target-https-proxies list --project=\"$PROJECT\" --format='value(sslCertificates,certificateManagerCertificates)' 2>/dev/null"
                <> names
                <> ") || { echo \"KEEP certificates $n the HTTPS proxies could not be listed\"; continue; }; listed=1; fi"
            , "        if grep -qxF -- \"$n\" <<< \"$served\"; then echo \"KEEP certificates $n an HTTPS proxy still serves it\"; else echo \"DELETE certificates $n\"; fi ;;"
            , "    esac"
            , "  done"
            , "}"
            , "plan_authorizations() {"
            , "  local all used listed line n"
            , "  all=$(gcloud certificate-manager dns-authorizations list" <> located <> " --format='value(name,description)' 2>/dev/null) || " <> unlisted "dns-authorizations"
            , "  used=''; listed=''"
            , "  for line in $(printf '%s\\n' \"$all\" | classify " <> declared declaredAuthorizations <> "); do"
            , "    n=${line#*:}"
            , "    case \"$line\" in"
            , "      MARK:*) echo \"MARK dns-authorizations $n\" ;;"
            , "      OURS:*)"
            , "        if [ -z \"$listed\" ]; then used=$(gcloud certificate-manager certificates list" <> located <> " --format='value(managed.dnsAuthorizations)' 2>/dev/null"
                <> names
                <> ") || { echo \"KEEP dns-authorizations $n the certificates could not be listed\"; continue; }; listed=1; fi"
            , "        if grep -qxF -- \"$n\" <<< \"$used\"; then echo \"KEEP dns-authorizations $n a certificate still uses it\"; else echo \"DELETE dns-authorizations $n\"; fi ;;"
            , "    esac"
            , "  done"
            , "}"
            ]
                <> if redirecting
                    then []
                    else
                        [ "plan_redirect_map() {"
                        , "  local d http https"
                        , "  d=$(" <> describeCompute "url-maps" redirectMap <> " --format='value(description)' 2>/dev/null) || return 0"
                        , -- only a map this module wrote: its description is the fingerprint of its content
                          "  case \"$d\" in " <> Text.intercalate "|" (map shellQuote (httpRedirectUrlMapStamps alb)) <> ") ;; *) return 0 ;; esac"
                        , "  http=$(gcloud compute target-http-proxies list --project=\"$PROJECT\" --format='value(urlMap)' 2>/dev/null) && https=$(gcloud compute target-https-proxies list --project=\"$PROJECT\" --format='value(urlMap)' 2>/dev/null) || { echo "
                            <> shellQuote ("KEEP url-maps " <> redirectMap <> " the proxies could not be listed")
                            <> "; return 0; }"
                        , "  if grep -qE -- \"/regions/$REGION/urlMaps/\"" <> shellQuote redirectMap <> "'[[:space:]]*$' <<< \"$http\"$'\\n'\"$https\"; then echo "
                            <> shellQuote
                                ( -- by default the HTTP proxy is never moved, so a map it is on is not waiting for anything
                                  (if alb.albHttp == HttpCreatedOnce then "NOTE" else "KEEP")
                                    <> " url-maps "
                                    <> redirectMap
                                    <> " a proxy is still on it"
                                )
                            <> "; else echo "
                            <> shellQuote ("DELETE url-maps " <> redirectMap)
                            <> "; fi"
                        , "}"
                        ]

    -- Two forwarding rules can only share an address that is reserved.
    addressPart :: PartSpec
    addressPart = simple AddressPart [] "reserved address" "addresses" (alb.albName <> "-ip") ""

    addressFlag
        | https = " --address=" <> resourceName "-ip" <> " --address-region=\"$REGION\""
        | otherwise = ""

    -- Created once with its URL map, which by default is never set again:
    -- somebody may have repointed it, and a second writer of one proxy's map
    -- is a fight every pass. Only a declaration about the listener
    -- ('ServeHttp', 'RedirectToHttps') makes the map something this node
    -- sets, and then it is compared and updated when it differs.
    httpProxyPart :: PartSpec
    httpProxyPart = case alb.albHttp of
        ServeHttp -> onMap UrlMapPart (alb.albName <> "-url-map")
        RedirectToHttps _ -> onMap HttpRedirectUrlMapPart (httpRedirectUrlMapName alb)
        _ -> createdOnce
      where
        proxy = alb.albName <> "-proxy"
        mapFlags urlMap = " --url-map=" <> shellQuote urlMap <> " --url-map-region=\"$REGION\""
        createdOnce =
            simple
                HttpProxyPart
                [UrlMapPart]
                "HTTP proxy"
                "target-http-proxies"
                proxy
                (mapFlags (alb.albName <> "-url-map"))
        -- the proxy's map is listed by URL: the last segment is its name
        currentMap = describeCompute "target-http-proxies" proxy <> " --format='value(urlMap)'"
        onMap mapPart urlMap =
            createdOnce
                { partDeps = [mapPart]
                , partNotes = ["on URL map " <> urlMap]
                , partUp =
                    [ "if exists " <> describeCompute "target-http-proxies" proxy <> "; then"
                    , "  have=$(" <> currentMap <> " | sed 's|.*/||')"
                    , "  [ \"$have\" = " <> shellQuote urlMap <> " ] || gcloud compute target-http-proxies update " <> shellQuote proxy
                        <> regional
                        <> mapFlags urlMap
                    , "else"
                    , "  gcloud compute target-http-proxies create " <> shellQuote proxy <> regional <> mapFlags urlMap
                    , "fi"
                    ]
                , partCheck =
                    [ need ("target-http-proxies " <> proxy) (describeCompute "target-http-proxies" proxy)
                    , "[ \"$(" <> currentMap <> " 2>/dev/null | sed 's|.*/||')\" = "
                        <> shellQuote urlMap
                        <> " ] || echo "
                        <> shellQuote ("MISSING url-map " <> urlMap <> " on " <> proxy)
                    ]
                }

    -- Created once with its URL map, which is never set again (somebody may
    -- have repointed it). Its certificate list is a declaration that can
    -- change under it, and the one thing here that must not be set as soon
    -- as it differs: a managed certificate is issued over minutes to an
    -- hour, and a proxy moved onto one that is not ACTIVE serves no
    -- certificate at all for its names. So the list is updated only once
    -- every declared managed certificate is ACTIVE; until then @up@ says
    -- which is not and the proxy keeps serving what it has (see 'lbParts'
    -- for when that is a failed @up@ and when a pending swap).
    httpsProxyPart :: PartSpec
    httpsProxyPart =
        PartSpec
            { partId = HttpsProxyPart
            , partDeps = UrlMapPart : map CertificatePart declared
            , partHelp = Text.unwords ["HTTPS proxy", httpsProxy]
            , partNotes = ["certificates " <> Text.unwords declared]
            , partUp =
                [ "if exists " <> describeCompute "target-https-proxies" httpsProxy <> "; then"
                , "  have=$(" <> proxyCertificates <> ")"
                , "  if [ \"$have\" != " <> shellQuote (setText declared) <> " ]; then"
                ]
                    <> swap
                    <> [ "  fi"
                       , "else"
                       , "  gcloud compute target-https-proxies create " <> shellQuote httpsProxy
                            <> regional
                            <> " --url-map=" <> resourceName "-url-map" <> " --url-map-region=\"$REGION\""
                            <> certificateFlags
                       , "fi"
                       ]
            , partCheck =
                [ need ("target-https-proxies " <> httpsProxy) (describeCompute "target-https-proxies" httpsProxy)
                , "have=$(" <> proxyCertificates <> ")"
                , "if exists " <> describeCompute "target-https-proxies" httpsProxy
                    <> " && [ \"$have\" != " <> shellQuote (setText declared) <> " ]; then"
                , "  waiting=''"
                ]
                    -- not yet swappable is not something up would fix: the
                    -- certificate's state, which reads as Unknown
                    <> concat
                        [ [ "  st=$(" <> describeLocated "certificates" n <> " --format='value(managed.state)' 2>/dev/null) || st=''"
                          , "  [ \"$st\" = ACTIVE ] || { echo " <> shellQuote ("CERT " <> n) <> "\" ${st:-ABSENT}\"; waiting=1; }"
                          ]
                        | n <- managed
                        ]
                    <> [ "  [ -n \"$waiting\" ] || echo "
                            <> shellQuote ("MISSING certificates " <> Text.unwords declared <> " on " <> httpsProxy <> ": it serves")
                            <> "\" $have\""
                       , "fi"
                       ]
            , partDown = [deleteCompute "target-https-proxies" httpsProxy]
            }
      where
        managed = [n | (n, _, _) <- managedCertificates alb]
        compute = [n | ComputeCertificate n <- alb.albCertificates]
        declared = managed <> compute
        update = "gcloud compute target-https-proxies update " <> shellQuote httpsProxy <> regional <> certificateFlags
        notActive n =
            "echo "
                <> shellQuote ("certificate " <> n <> " is not ACTIVE yet:")
                <> "\" ${st:-absent}\""
                <> shellQuote ("; " <> httpsProxy <> " keeps the certificates it serves until it is. Run again then.")
                <> " >&2"
        stateOf n = "    st=$(" <> describeLocated "certificates" n <> " --format='value(managed.state)' 2>/dev/null) || st=''"
        -- The move onto the declared certificates, inside "the proxy exists
        -- and serves another list". Under a 'DomainSetCertificate' a
        -- certificate still being issued is the expected middle of a swap:
        -- said on stderr, the proxy left alone, and the node's up /done/ --
        -- no line here may @exit 0@, the lines also run inside
        -- 'renderLbScript'. One that is FAILED or absent will never be
        -- ACTIVE, and fails as before.
        swap
            | domainSets =
                ["    pending=''"]
                    <> concat
                        [ [ stateOf n
                          , "    case \"$st\" in"
                          , "      ACTIVE) ;;"
                          , "      ''|FAILED) " <> notActive n <> "; exit 1 ;;"
                          , "      *) " <> notActive n <> "; pending=1 ;;"
                          , "    esac"
                          ]
                        | n <- managed
                        ]
                    <> [ "    if [ -z \"$pending\" ]; then"
                       , "      " <> update
                       , "    else"
                       , "      echo "
                            <> shellQuote ("PENDING certificate swap on " <> httpsProxy <> ": it serves")
                            <> "\" $have\" >&2"
                       , "    fi"
                       ]
            | otherwise =
                concat
                    [ [ stateOf n
                      , "    [ \"$st\" = ACTIVE ] || { " <> notActive n <> "; exit 1; }"
                      ]
                    | n <- managed
                    ]
                    <> ["    " <> update]
        certificateFlags = case managed of
            (_ : _) -> " --certificate-manager-certificates=" <> shellQuote (Text.intercalate "," managed)
            [] -> " --ssl-certificates=" <> shellQuote (Text.intercalate "," compute) <> " --ssl-certificates-region=\"$REGION\""

    forwardingRulePart :: PartSpec
    forwardingRulePart =
        simple
            ForwardingRulePart
            (HttpProxyPart : [AddressPart | https])
            "forwarding rule"
            "forwarding-rules"
            (alb.albName <> "-fw")
            ( " --load-balancing-scheme=EXTERNAL_MANAGED"
                <> maybe "" ((" --network=" <>) . shellQuote) alb.albNetwork
                <> addressFlag
                <> " --target-http-proxy=" <> resourceName "-proxy"
                <> " --target-http-proxy-region=\"$REGION\""
                <> " --ports=80"
            )

    httpsForwardingRulePart :: PartSpec
    httpsForwardingRulePart =
        simple
            HttpsForwardingRulePart
            [HttpsProxyPart, AddressPart]
            "HTTPS forwarding rule"
            "forwarding-rules"
            (alb.albName <> "-https-fw")
            ( " --load-balancing-scheme=EXTERNAL_MANAGED"
                <> maybe "" ((" --network=" <>) . shellQuote) alb.albNetwork
                <> addressFlag
                <> " --target-https-proxy=" <> resourceName "-https-proxy"
                <> " --target-https-proxy-region=\"$REGION\""
                <> " --ports=443"
            )

-- | What identifies a backend among one service's: its group, or the service's NEG.
backendKey :: Svc -> Backend -> Text
backendKey svc = \case
    InstanceGroupBackend ig _ _ -> ig
    CloudRunBackend _ -> svc.svcNeg

jsonText :: Aeson.Value -> Text
jsonText = Text.decodeUtf8With TextErr.lenientDecode . LByteString.toStrict . Aeson.encode

{- | What every creating or deleting script starts with. @exists@ is a bare
predicate: every caller appends its own location flags, because not every
resource named here is regional (an unmanaged instance group is zonal).
-}
mutatingHeader :: ApplicationLoadBalancer -> [Text]
mutatingHeader alb =
    [ "set -euo pipefail"
    , "PROJECT=" <> shellQuote alb.albProject.projectId
    , "REGION=" <> shellQuote alb.albRegion.regionName
    , "exists() { \"$@\" >/dev/null 2>&1; }"
    ]

-- | What every read-only script starts with: no @-e@, findings are lines.
checkHeader :: ApplicationLoadBalancer -> [Text]
checkHeader alb =
    [ "set -uo pipefail"
    , "PROJECT=" <> shellQuote alb.albProject.projectId
    , "REGION=" <> shellQuote alb.albRegion.regionName
    , "exists() { \"$@\" >/dev/null 2>&1; }"
    , "need() { local what=\"$1\"; shift; exists \"$@\" || echo \"MISSING $what\"; }"
    ]

-- | The script one resource node's @up@ runs.
renderPartUpScript :: ApplicationLoadBalancer -> PartSpec -> Text
renderPartUpScript alb part = Text.unlines (mutatingHeader alb <> part.partUp)

{- | The script one resource node's @check@ runs, read by 'interpretLbCheck':
it always exits 0 unless the script itself breaks.
-}
renderPartCheckScript :: ApplicationLoadBalancer -> PartSpec -> Text
renderPartCheckScript alb part = Text.unlines (checkHeader alb <> part.partCheck)

{- | The script one resource node's @down@ runs: a resource that is already
gone is skipped, one that exists and fails to delete fails the script.
-}
renderPartDownScript :: ApplicationLoadBalancer -> PartSpec -> Text
renderPartDownScript alb part = Text.unlines (mutatingHeader alb <> part.partDown)

{- | Every resource's @up@ in one script, in 'lbParts'' order: what the
balancer's node ran when it was a single node. No node runs it any more; it
is the same lines, kept for running a balancer up by hand and for tests that
exercise the lines together.
-}
renderLbScript :: ApplicationLoadBalancer -> Text
renderLbScript alb = Text.unlines (mutatingHeader alb <> concatMap partUp (lbParts alb))

{- | Every resource's check in one read-only script, then the backends'
health ('renderLbHealthScript''s lines). Findings are lines on stdout (see
'interpretLbCheck').

What it sees of the URL map: its hosts, as a set (a declared host that is
absent, a host no rule declares), and the fingerprint in its @description@
('urlMapStamp', 'urlMapStampIsRequired'), which is what covers a path rule
and which service a host is sent to -- always in a map naming a backend
bucket or a redirect, and in a map of backend services only once salmon has
imported it with one. Which port a
service reaches it does see: the service's port name, and that name on each
instance group it sends to.
-}
renderLbCheckScript :: ApplicationLoadBalancer -> Text
renderLbCheckScript alb =
    Text.unlines (checkHeader alb <> concatMap partCheck (lbParts alb) <> healthLines alb)

{- | What the balancer's root node checks: the health of each instance-group
backend, one @HEALTH \<service\> \<state\>@ line per backend instance. A
Cloud Run (NEG) backend has no health to ask for.
-}
renderLbHealthScript :: ApplicationLoadBalancer -> Text
renderLbHealthScript alb = Text.unlines (checkHeader alb <> healthLines alb)

-- one state per backend instance; ';' separates a backend's instances
healthLines :: ApplicationLoadBalancer -> [Text]
healthLines alb =
    [ "gcloud compute backend-services get-health " <> shellQuote svc.svcResource <> regional
        <> " --format='value(status.healthStatus[].healthState)' 2>/dev/null"
        <> " | tr ';' '\\n' | while read -r st; do [ -n \"$st\" ] && echo "
        <> shellQuote ("HEALTH " <> svc.svcLabel)
        <> "\" $st\"; done; true"
    | svc <- services alb
    , any isInstanceGroup svc.svcBackends
    ]

{- | Every resource's @down@ in one script, dependants first ('lbParts''
order reversed).
-}
renderLbDeleteScript :: ApplicationLoadBalancer -> Text
renderLbDeleteScript alb = Text.unlines (mutatingHeader alb <> concatMap partDown (reverse (lbParts alb)))

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
