{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- | Layer 0: no gcloud invocation salmon renders can prompt.

No process a node starts reads the pass's standard input, so a prompt could
never be answered: gcloud would read end-of-file and do whatever it does then.
Every argv a @Command "gcloud"@ prepares must therefore carry @--quiet@ among
gcloud's own flags (before any @--@), and every @bash -c@ script of gcloud calls
(the load balancer's) must start by exporting @CLOUDSDK_CORE_DISABLE_PROMPTS=1@.

Each command type below has one sample per constructor, and a total match on
it ('guard'): this module is compiled with @-Werror=incomplete-patterns@, so a
constructor added to a command type fails the build here until it has a sample.
-}
module Test.GcpQuietSpec (tests) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Process.ListLike (CmdSpec (..), CreateProcess, cmdspec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import Salmon.Builtin.Nodes.Binary (Command, prepare)
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.Billing as Billing
import qualified Salmon.Builtin.Nodes.Gcp.CloudDns as CloudDns
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Dashboard as Dashboard
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as LoadBalancing
import qualified Salmon.Builtin.Nodes.Gcp.Logging as Logging
import qualified Salmon.Builtin.Nodes.Gcp.Monitoring as Monitoring
import qualified Salmon.Builtin.Nodes.Gcp.ResourceManager as ResourceManager
import qualified Salmon.Builtin.Nodes.Gcp.SecretManager as SecretManager
import qualified Salmon.Builtin.Nodes.Gcp.ServiceUsage as ServiceUsage
import qualified Salmon.Builtin.Nodes.Gcp.SshAccess as SshAccess
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Gcp.Uptime as Uptime

tests :: TestTree
tests =
    testGroup
        "gcloud never prompts"
        [ group "Core" Core.gcloudCommand guardCore coreSamples
        , group "ArtifactRegistry" ArtifactRegistry.artifactRegistryCommand guardArtifactRegistry artifactRegistrySamples
        , group "Billing" Billing.billingCommand guardBilling billingSamples
        , group "CloudDns" CloudDns.cloudDnsCommand guardCloudDns cloudDnsSamples
        , group "CloudRun" CloudRun.cloudRunCommand guardCloudRun cloudRunSamples
        , group "Compute" Compute.computeCommand guardCompute computeSamples
        , group "Dashboard" Dashboard.dashboardCommand guardDashboard dashboardSamples
        , group "Iam" Iam.iamCommand guardIam iamSamples
        , group "LoadBalancing" LoadBalancing.loadBalancingCommand guardLoadBalancing loadBalancingSamples
        , group "Logging" Logging.loggingCommand guardLogging loggingSamples
        , group "Monitoring" Monitoring.monitoringCommand guardMonitoring monitoringSamples
        , group "ResourceManager" ResourceManager.resourceManagerCommand guardResourceManager resourceManagerSamples
        , group "SecretManager" SecretManager.secretManagerCommand guardSecretManager secretManagerSamples
        , group "ServiceUsage" ServiceUsage.serviceUsageCommand guardServiceUsage serviceUsageSamples
        , group "SshAccess" SshAccess.sshAccessCommand guardSshAccess sshAccessSamples
        , group "Storage" Storage.storageCommand guardStorage storageSamples
        , group "Uptime" Uptime.uptimeCommand guardUptime uptimeSamples
        , testCase "quietly puts --quiet before a --, once" $ do
            assertBool "" (Core.quietly ["compute", "ssh", "vm", "--", "ls", "--quiet"] == ["compute", "ssh", "vm", "--quiet", "--", "ls", "--quiet"])
            assertBool "" (Core.quietly ["x", "--quiet"] == ["x", "--quiet"])
        ]

{- | One test per sample. The guard is applied to every sample so that it is
used (and so compiled with its exhaustiveness check); its value is the
constructor's name, which names the test.
-}
group :: String -> Command "gcloud" cmd -> (cmd -> String) -> [cmd] -> TestTree
group name command guard samples =
    testGroup name [testCase (guard c) (assertNonInteractive (prepare command c)) | c <- samples]

assertNonInteractive :: CreateProcess -> IO ()
assertNonInteractive p = case cmdspec p of
    RawCommand "gcloud" args ->
        assertBool (unwords args) ("--quiet" `elem` takeWhile (/= "--") args)
    RawCommand "bash" ["-c", script] ->
        case lines script of
            (first : _) -> assertBool script (Text.pack first == Core.disablePromptsLine)
            [] -> assertFailure "empty script"
    other -> assertFailure ("neither a gcloud argv nor a bash script of gcloud calls: " <> show other)

-------------------------------------------------------------------------------
-- Fixtures

proj :: Core.Project
proj = Core.Project "p"

region :: Core.Region
region = Core.Region "europe-west1"

zone :: Core.Zone
zone = Core.Zone "europe-west1-b"

instance_ :: Compute.Instance
instance_ =
    Compute.Instance
        { Compute.instanceName = "toy-vm"
        , Compute.instanceProject = proj
        , Compute.instanceZone = zone
        , Compute.instanceMachineType = Compute.Custom "e2-micro"
        , Compute.instanceBootDisk = Compute.BootDisk 10 Nothing (Just "ubuntu-2404-lts-amd64") (Just "ubuntu-os-cloud")
        , Compute.instanceNetwork = "default"
        , Compute.instanceSubnet = "default"
        , Compute.instanceServiceAccount = Nothing
        , Compute.instanceMetadata = Map.fromList [("enable-oslogin", "FALSE")]
        , Compute.instanceMetadataFiles = Map.fromList [("startup-script", "/tmp/w/startup-script.sh")]
        , Compute.instanceExternalAddress = Compute.ReservedExternal "toy-ip"
        , Compute.instanceInternalAddress = Compute.EphemeralInternal
        , Compute.instanceTags = ["toy-ssh"]
        , Compute.instancePower = Compute.PoweredOn
        , Compute.instanceScopes = Compute.DefaultScopes
        , Compute.instanceMachineTypeChange = Compute.RefuseMachineTypeChange
        , Compute.instanceRunLimit = Compute.NoRunLimit
        }

cloudRunService :: CloudRun.CloudRunService
cloudRunService =
    CloudRun.CloudRunService
        { CloudRun.crsName = "svc"
        , CloudRun.crsProject = proj
        , CloudRun.crsRegion = region
        , CloudRun.crsImage = "us-docker.pkg.dev/p/r/img:1"
        , CloudRun.crsEnv = Map.fromList [("A", "1")]
        , CloudRun.crsServiceAccount = "sa@p.iam.gserviceaccount.com"
        , CloudRun.crsIngress = CloudRun.All
        , CloudRun.crsMaxInstances = Nothing
        , CloudRun.crsOptions = CloudRun.defaultCloudRunOptions
        }

budget :: Billing.Budget
budget =
    Billing.Budget
        { Billing.bgAccount = Billing.BillingAccount "000000-111111-222222"
        , Billing.bgDisplayName = "monthly"
        , Billing.bgUnits = 100
        , Billing.bgCurrency = "EUR"
        , Billing.bgThresholds = [Billing.ThresholdRule 0.5 Billing.CurrentSpend]
        , Billing.bgProject = Just proj
        }

managedZone :: CloudDns.ManagedZone
managedZone = CloudDns.ManagedZone "example-org" proj "example.org." "example"

recordSet :: CloudDns.RecordSet
recordSet = CloudDns.RecordSet managedZone "www.example.org." CloudDns.A 300 ["192.0.2.1"]

bucket :: Storage.Bucket
bucket = Storage.Bucket "site-bucket" proj region True

bucketBinding :: Storage.BucketIamBinding
bucketBinding = Storage.BucketIamBinding bucket "roles/storage.objectCreator" (Storage.ServiceAccountMember "sa@p.iam.gserviceaccount.com")

rsync :: Storage.Rsync
rsync =
    Storage.Rsync
        { Storage.rsyncProject = proj
        , Storage.rsyncSource = "out/site"
        , Storage.rsyncDestination = "gs://site-bucket/docs"
        , Storage.rsyncDeleteUnmatched = False
        , Storage.rsyncCacheControl = Nothing
        , Storage.rsyncExclude = Nothing
        , Storage.rsyncDryRun = False
        }

secret :: SecretManager.Secret
secret = SecretManager.Secret "my-secret" proj "automatic"

channel :: Monitoring.NotificationChannel
channel = Monitoring.NotificationChannel proj "ops mail" (Monitoring.Email "ops@example.org")

policy :: Monitoring.Policy
policy =
    Monitoring.Policy
        { Monitoring.poProject = proj
        , Monitoring.poDisplayName = "errors in logs"
        , Monitoring.poConditions = [Monitoring.MatchedLog (Monitoring.LogMatch "error logged" "severity>=ERROR" [])]
        , Monitoring.poChannels = [channel]
        , Monitoring.poDocumentation = "doc"
        , Monitoring.poAlertStrategy = Just (Monitoring.AlertStrategy (Just 300) Nothing)
        }

iamBinding :: Iam.IamBinding
iamBinding = Iam.IamBinding (Iam.ServiceAccount "sa-1@p.iam.gserviceaccount.com") "roles/viewer" "projects/p"

-- | Regional and global, so both scopes of every script are rendered.
balancers :: [LoadBalancing.ApplicationLoadBalancer]
balancers =
    [ LoadBalancing.httpLoadBalancer "web" proj region [LoadBalancing.CloudRunBackend "svc"] Nothing
    , LoadBalancing.globalHttpLoadBalancer "web" proj region [(region, "svc")]
    ]

-------------------------------------------------------------------------------
-- Samples, and the guards that keep them exhaustive

coreSamples :: [Core.GcloudCommand]
coreSamples = [Core.AdcPrintAccessToken, Core.ConfigGetAccount]

guardCore :: Core.GcloudCommand -> String
guardCore c = case c of
    Core.AdcPrintAccessToken -> "AdcPrintAccessToken"
    Core.ConfigGetAccount -> "ConfigGetAccount"

artifactRegistrySamples :: [ArtifactRegistry.ArtifactRegistryCommand]
artifactRegistrySamples =
    [ArtifactRegistry.ReposCreate repo, ArtifactRegistry.ReposDescribe repo, ArtifactRegistry.ReposDelete repo, ArtifactRegistry.AuthConfigureDocker proj region]
  where
    repo = ArtifactRegistry.ArtifactRepo "my-repo" proj region ArtifactRegistry.Docker

guardArtifactRegistry :: ArtifactRegistry.ArtifactRegistryCommand -> String
guardArtifactRegistry c = case c of
    ArtifactRegistry.ReposCreate _ -> "ReposCreate"
    ArtifactRegistry.ReposDescribe _ -> "ReposDescribe"
    ArtifactRegistry.ReposDelete _ -> "ReposDelete"
    ArtifactRegistry.AuthConfigureDocker _ _ -> "AuthConfigureDocker"

billingSamples :: [Billing.BillingCommand]
billingSamples =
    [ Billing.ProjectsLink proj budget.bgAccount
    , Billing.ProjectsDescribe proj
    , Billing.ProjectsUnlink proj
    , Billing.BudgetsList budget.bgAccount
    , Billing.BudgetsCreate budget
    , Billing.BudgetsUpdate "billingAccounts/000000-111111-222222/budgets/b1" budget
    , Billing.BudgetsDelete "billingAccounts/000000-111111-222222/budgets/b1"
    ]

guardBilling :: Billing.BillingCommand -> String
guardBilling c = case c of
    Billing.ProjectsLink _ _ -> "ProjectsLink"
    Billing.ProjectsDescribe _ -> "ProjectsDescribe"
    Billing.ProjectsUnlink _ -> "ProjectsUnlink"
    Billing.BudgetsList _ -> "BudgetsList"
    Billing.BudgetsCreate _ -> "BudgetsCreate"
    Billing.BudgetsUpdate _ _ -> "BudgetsUpdate"
    Billing.BudgetsDelete _ -> "BudgetsDelete"

cloudDnsSamples :: [CloudDns.CloudDnsCommand]
cloudDnsSamples =
    [ CloudDns.ZonesCreate managedZone
    , CloudDns.ZonesDescribe managedZone
    , CloudDns.ZonesDelete managedZone
    , CloudDns.RecordSetsCreate recordSet
    , CloudDns.RecordSetsUpdate recordSet
    , CloudDns.RecordSetsDescribe recordSet
    , CloudDns.RecordSetsDelete recordSet
    ]

guardCloudDns :: CloudDns.CloudDnsCommand -> String
guardCloudDns c = case c of
    CloudDns.ZonesCreate _ -> "ZonesCreate"
    CloudDns.ZonesDescribe _ -> "ZonesDescribe"
    CloudDns.ZonesDelete _ -> "ZonesDelete"
    CloudDns.RecordSetsCreate _ -> "RecordSetsCreate"
    CloudDns.RecordSetsUpdate _ -> "RecordSetsUpdate"
    CloudDns.RecordSetsDescribe _ -> "RecordSetsDescribe"
    CloudDns.RecordSetsDelete _ -> "RecordSetsDelete"

cloudRunSamples :: [CloudRun.CloudRunCommand]
cloudRunSamples = map ($ cloudRunService) [CloudRun.RunDeploy, CloudRun.RunDescribe, CloudRun.RunDelete, CloudRun.RunListNamed]

guardCloudRun :: CloudRun.CloudRunCommand -> String
guardCloudRun c = case c of
    CloudRun.RunDeploy _ -> "RunDeploy"
    CloudRun.RunDescribe _ -> "RunDescribe"
    CloudRun.RunDelete _ -> "RunDelete"
    CloudRun.RunListNamed _ -> "RunListNamed"

computeSamples :: [Compute.ComputeCommand]
computeSamples =
    map
        ($ instance_)
        [ Compute.InstancesCreate
        , Compute.InstancesDescribe
        , Compute.InstancesDescribeStatus
        , Compute.InstancesDescribeScopes
        , Compute.InstancesDescribeMachineType
        , Compute.InstancesSetMachineType
        , Compute.InstancesDescribeRunLimit
        , Compute.InstancesSetScheduling
        , Compute.InstancesStart
        , Compute.InstancesResume
        , Compute.InstancesStop
        , Compute.InstancesDelete
        ]
        <> concat [[Compute.AddressesCreate a, Compute.AddressesDescribe a, Compute.AddressesDelete a] | a <- addresses]
        <> map ($ firewall) [Compute.FirewallCreate, Compute.FirewallDescribe, Compute.FirewallUpdate, Compute.FirewallDelete]
        <> map ($ subnet) [Compute.SubnetsCreate, Compute.SubnetsDescribe, Compute.SubnetsDelete]
        <> [ Compute.InstanceGroupsCreate group'
           , Compute.InstanceGroupsDescribe group'
           , Compute.InstanceGroupsDelete group'
           , Compute.InstanceGroupsAddInstance group' "toy-vm"
           , Compute.InstanceGroupsRemoveInstance group' "toy-vm"
           , Compute.InstanceGroupsListInstances group'
           ]
  where
    addresses =
        [ Compute.Address "toy-ip" proj region Compute.ExternalAddress
        , Compute.Address "toy-int" proj region (Compute.InternalAddress "default" (Just "10.0.0.5"))
        ]
    firewall = Compute.FirewallRule "allow-ssh" proj "default" "tcp:22" ["0.0.0.0/0"] ["toy-ssh"]
    subnet = Compute.Subnet "proxy-only" proj region "default" "10.129.0.0/23" Compute.RegionalManagedProxy
    group' = Compute.InstanceGroup "ig" proj zone

guardCompute :: Compute.ComputeCommand -> String
guardCompute c = case c of
    Compute.InstancesCreate _ -> "InstancesCreate"
    Compute.InstancesDescribe _ -> "InstancesDescribe"
    Compute.InstancesDescribeStatus _ -> "InstancesDescribeStatus"
    Compute.InstancesDescribeScopes _ -> "InstancesDescribeScopes"
    Compute.InstancesDescribeMachineType _ -> "InstancesDescribeMachineType"
    Compute.InstancesSetMachineType _ -> "InstancesSetMachineType"
    Compute.InstancesDescribeRunLimit _ -> "InstancesDescribeRunLimit"
    Compute.InstancesSetScheduling _ -> "InstancesSetScheduling"
    Compute.InstancesStart _ -> "InstancesStart"
    Compute.InstancesResume _ -> "InstancesResume"
    Compute.InstancesStop _ -> "InstancesStop"
    Compute.InstancesDelete _ -> "InstancesDelete"
    Compute.AddressesCreate a -> "AddressesCreate " <> show a.addressKind
    Compute.AddressesDescribe a -> "AddressesDescribe " <> show a.addressKind
    Compute.AddressesDelete a -> "AddressesDelete " <> show a.addressKind
    Compute.FirewallCreate _ -> "FirewallCreate"
    Compute.FirewallDescribe _ -> "FirewallDescribe"
    Compute.FirewallUpdate _ -> "FirewallUpdate"
    Compute.FirewallDelete _ -> "FirewallDelete"
    Compute.SubnetsCreate _ -> "SubnetsCreate"
    Compute.SubnetsDescribe _ -> "SubnetsDescribe"
    Compute.SubnetsDelete _ -> "SubnetsDelete"
    Compute.InstanceGroupsCreate _ -> "InstanceGroupsCreate"
    Compute.InstanceGroupsDescribe _ -> "InstanceGroupsDescribe"
    Compute.InstanceGroupsDelete _ -> "InstanceGroupsDelete"
    Compute.InstanceGroupsAddInstance _ _ -> "InstanceGroupsAddInstance"
    Compute.InstanceGroupsRemoveInstance _ _ -> "InstanceGroupsRemoveInstance"
    Compute.InstanceGroupsListInstances _ -> "InstanceGroupsListInstances"

dashboardSamples :: [Dashboard.DashboardCommand]
dashboardSamples =
    [ Dashboard.DashboardsList proj
    , Dashboard.DashboardsCreate proj
    , Dashboard.DashboardsUpdate proj "d1"
    , Dashboard.DashboardsDelete proj "d1"
    ]

guardDashboard :: Dashboard.DashboardCommand -> String
guardDashboard c = case c of
    Dashboard.DashboardsList _ -> "DashboardsList"
    Dashboard.DashboardsCreate _ -> "DashboardsCreate"
    Dashboard.DashboardsUpdate _ _ -> "DashboardsUpdate"
    Dashboard.DashboardsDelete _ _ -> "DashboardsDelete"

iamSamples :: [Iam.IamCommand]
iamSamples =
    [ Iam.ServiceAccountsCreate proj "sa-1"
    , Iam.ServiceAccountsDescribe proj "sa-1"
    , Iam.ServiceAccountsDelete proj "sa-1"
    , Iam.RolesCreate role
    , Iam.RolesDescribe role
    , Iam.RolesDelete role
    , Iam.ServiceAccountKeysCreate (Iam.ServiceAccountKey proj "uploader" "secrets/uploader.key.json")
    , Iam.ProjectNumberDescribe proj
    ]
        <> concat
            [ [Iam.IamPolicyAddBinding b, Iam.IamPolicyRemoveBinding b, Iam.IamPolicyGetBinding b]
            | resource <-
                [ "projects/p"
                , "my-secret"
                , "projects/p/secrets/my-secret"
                , "projects/p/locations/us-west1/repositories/my-repo"
                , "buckets/my-bucket"
                ]
            , let b = iamBinding{Iam.iamResource = resource}
            ]
  where
    role = Iam.CustomRole "registryUploader" proj "infra/roles/registryUploader.yaml"

guardIam :: Iam.IamCommand -> String
guardIam c = case c of
    Iam.ServiceAccountsCreate _ _ -> "ServiceAccountsCreate"
    Iam.ServiceAccountsDescribe _ _ -> "ServiceAccountsDescribe"
    Iam.ServiceAccountsDelete _ _ -> "ServiceAccountsDelete"
    Iam.IamPolicyAddBinding b -> "IamPolicyAddBinding " <> Text.unpack b.iamResource
    Iam.IamPolicyRemoveBinding b -> "IamPolicyRemoveBinding " <> Text.unpack b.iamResource
    Iam.IamPolicyGetBinding b -> "IamPolicyGetBinding " <> Text.unpack b.iamResource
    Iam.RolesCreate _ -> "RolesCreate"
    Iam.RolesDescribe _ -> "RolesDescribe"
    Iam.RolesDelete _ -> "RolesDelete"
    Iam.ServiceAccountKeysCreate _ -> "ServiceAccountKeysCreate"
    Iam.ProjectNumberDescribe _ -> "ProjectNumberDescribe"

loadBalancingSamples :: [LoadBalancing.LoadBalancingCommand]
loadBalancingSamples =
    concat
        [ [ LoadBalancing.LbCreate alb
          , LoadBalancing.LbCheck alb
          , LoadBalancing.LbHealth alb
          , LoadBalancing.LbDescribe alb
          , LoadBalancing.LbDelete alb
          , LoadBalancing.LbAddressDescribe alb
          , LoadBalancing.LbDnsAuthorizationDescribe alb "web-authz"
          ]
            <> concat [[LoadBalancing.LbPartUp alb p, LoadBalancing.LbPartCheck alb p, LoadBalancing.LbPartDown alb p] | p <- LoadBalancing.lbParts alb]
        | alb <- balancers
        ]

guardLoadBalancing :: LoadBalancing.LoadBalancingCommand -> String
guardLoadBalancing c = case c of
    LoadBalancing.LbCreate alb -> "LbCreate " <> scope alb
    LoadBalancing.LbCheck alb -> "LbCheck " <> scope alb
    LoadBalancing.LbHealth alb -> "LbHealth " <> scope alb
    LoadBalancing.LbPartUp alb p -> "LbPartUp " <> scope alb <> " " <> show p.partId
    LoadBalancing.LbPartCheck alb p -> "LbPartCheck " <> scope alb <> " " <> show p.partId
    LoadBalancing.LbPartDown alb p -> "LbPartDown " <> scope alb <> " " <> show p.partId
    LoadBalancing.LbDescribe alb -> "LbDescribe " <> scope alb
    LoadBalancing.LbDelete alb -> "LbDelete " <> scope alb
    LoadBalancing.LbAddressDescribe alb -> "LbAddressDescribe " <> scope alb
    LoadBalancing.LbDnsAuthorizationDescribe alb _ -> "LbDnsAuthorizationDescribe " <> scope alb
  where
    scope :: LoadBalancing.ApplicationLoadBalancer -> String
    scope alb = show alb.albScope

loggingSamples :: [Logging.LoggingCommand]
loggingSamples =
    [ Logging.MetricsList proj
    , Logging.MetricsCreate metric
    , Logging.MetricsUpdate metric
    , Logging.MetricsDelete proj "errors"
    , Logging.BucketDescribe (Logging.defaultLogBucket proj)
    , Logging.BucketSetRetention (Logging.defaultLogBucket proj) 30
    ]
  where
    metric = Logging.LogMetric proj "errors" "errors logged" "severity>=ERROR"

guardLogging :: Logging.LoggingCommand -> String
guardLogging c = case c of
    Logging.MetricsList _ -> "MetricsList"
    Logging.MetricsCreate _ -> "MetricsCreate"
    Logging.MetricsUpdate _ -> "MetricsUpdate"
    Logging.MetricsDelete _ _ -> "MetricsDelete"
    Logging.BucketDescribe _ -> "BucketDescribe"
    Logging.BucketSetRetention _ _ -> "BucketSetRetention"

monitoringSamples :: [Monitoring.MonitoringCommand]
monitoringSamples =
    [ Monitoring.ChannelsList channel
    , Monitoring.ChannelsCreate channel
    , Monitoring.ChannelsUpdate "projects/p/notificationChannels/1" channel
    , Monitoring.ChannelsDelete proj "projects/p/notificationChannels/1"
    , Monitoring.PoliciesList policy
    , Monitoring.PoliciesCreate policy ["projects/p/notificationChannels/1"]
    , Monitoring.PoliciesUpdate "projects/p/alertPolicies/1" policy ["projects/p/notificationChannels/1"]
    , Monitoring.PoliciesDelete proj "projects/p/alertPolicies/1"
    ]

guardMonitoring :: Monitoring.MonitoringCommand -> String
guardMonitoring c = case c of
    Monitoring.ChannelsList _ -> "ChannelsList"
    Monitoring.ChannelsCreate _ -> "ChannelsCreate"
    Monitoring.ChannelsUpdate _ _ -> "ChannelsUpdate"
    Monitoring.ChannelsDelete _ _ -> "ChannelsDelete"
    Monitoring.PoliciesList _ -> "PoliciesList"
    Monitoring.PoliciesCreate _ _ -> "PoliciesCreate"
    Monitoring.PoliciesUpdate _ _ _ -> "PoliciesUpdate"
    Monitoring.PoliciesDelete _ _ -> "PoliciesDelete"

resourceManagerSamples :: [ResourceManager.ResourceManagerCommand]
resourceManagerSamples =
    [ ResourceManager.ProjectsCreate (ResourceManager.ProjectSpec proj (ResourceManager.Folder "123") (Map.fromList [("purpose", "salmon-toy")]))
    , ResourceManager.ProjectsDescribeState proj
    , ResourceManager.ProjectsDelete proj
    ]

guardResourceManager :: ResourceManager.ResourceManagerCommand -> String
guardResourceManager c = case c of
    ResourceManager.ProjectsCreate _ -> "ProjectsCreate"
    ResourceManager.ProjectsDescribeState _ -> "ProjectsDescribeState"
    ResourceManager.ProjectsDelete _ -> "ProjectsDelete"

secretManagerSamples :: [SecretManager.SecretManagerCommand]
secretManagerSamples =
    [ SecretManager.SecretsCreate secret
    , SecretManager.SecretsDescribe secret
    , SecretManager.SecretsDelete secret
    , SecretManager.VersionsAdd (SecretManager.SecretVersion secret "secrets/my-secret.txt")
    , SecretManager.VersionsAccessLatest secret
    , SecretManager.VersionsAccess proj "my-secret" "3"
    ]

guardSecretManager :: SecretManager.SecretManagerCommand -> String
guardSecretManager c = case c of
    SecretManager.SecretsCreate _ -> "SecretsCreate"
    SecretManager.SecretsDescribe _ -> "SecretsDescribe"
    SecretManager.SecretsDelete _ -> "SecretsDelete"
    SecretManager.VersionsAdd _ -> "VersionsAdd"
    SecretManager.VersionsAccessLatest _ -> "VersionsAccessLatest"
    SecretManager.VersionsAccess{} -> "VersionsAccess"

serviceUsageSamples :: [ServiceUsage.ServiceUsageCommand]
serviceUsageSamples = [ServiceUsage.ServicesEnable proj api, ServiceUsage.ServicesList proj api]
  where
    api = ServiceUsage.Api "billingbudgets.googleapis.com"

guardServiceUsage :: ServiceUsage.ServiceUsageCommand -> String
guardServiceUsage c = case c of
    ServiceUsage.ServicesEnable _ _ -> "ServicesEnable"
    ServiceUsage.ServicesList _ _ -> "ServicesList"

sshAccessSamples :: [SshAccess.SshAccessCommand]
sshAccessSamples = [SshAccess.MetadataAddSshCa (SshAccess.MetadataSshCa proj "certs/ssh-ca.pub")]

guardSshAccess :: SshAccess.SshAccessCommand -> String
guardSshAccess c = case c of
    SshAccess.MetadataAddSshCa _ -> "MetadataAddSshCa"

storageSamples :: [Storage.StorageCommand]
storageSamples =
    [ Storage.BucketsCreate bucket
    , Storage.BucketsDescribe bucket
    , Storage.BucketsDelete bucket
    , Storage.BucketsDescribeJson bucket
    , Storage.BucketsGetIamPolicy bucket
    , Storage.BucketsAddIamBinding bucketBinding
    , Storage.BucketsRemoveIamBinding bucketBinding
    , Storage.BucketsSetLifecycle bucket "/tmp/rules.json"
    , Storage.BucketsClearLifecycle bucket
    , Storage.BucketsSetWebsite (Storage.BucketWebsite bucket (Just "index.html") Nothing)
    , Storage.BucketsRsync rsync
    , Storage.BucketsRsync rsync{Storage.rsyncDeleteUnmatched = True, Storage.rsyncDryRun = True}
    , Storage.ObjectCopy proj "gs://site-bucket/a" "/tmp/a"
    ]

guardStorage :: Storage.StorageCommand -> String
guardStorage c = case c of
    Storage.BucketsCreate _ -> "BucketsCreate"
    Storage.BucketsDescribe _ -> "BucketsDescribe"
    Storage.BucketsDelete _ -> "BucketsDelete"
    Storage.BucketsDescribeJson _ -> "BucketsDescribeJson"
    Storage.BucketsGetIamPolicy _ -> "BucketsGetIamPolicy"
    Storage.BucketsAddIamBinding _ -> "BucketsAddIamBinding"
    Storage.BucketsRemoveIamBinding _ -> "BucketsRemoveIamBinding"
    Storage.BucketsSetLifecycle _ _ -> "BucketsSetLifecycle"
    Storage.BucketsClearLifecycle _ -> "BucketsClearLifecycle"
    Storage.BucketsSetWebsite _ -> "BucketsSetWebsite"
    Storage.BucketsRsync r -> "BucketsRsync" <> (if r.rsyncDryRun then " dry run" else "")
    Storage.ObjectCopy{} -> "ObjectCopy"

uptimeSamples :: [Uptime.UptimeCommand]
uptimeSamples =
    [ Uptime.UptimeList proj
    , Uptime.UptimeCreate check
    , Uptime.UptimeUpdate "api-up-1" check
    , Uptime.UptimeDelete proj "api-up-1"
    ]
  where
    check = Uptime.httpsUptimeCheck proj "api up" "api.example.org" "/healthz"

guardUptime :: Uptime.UptimeCommand -> String
guardUptime c = case c of
    Uptime.UptimeList _ -> "UptimeList"
    Uptime.UptimeCreate _ -> "UptimeCreate"
    Uptime.UptimeUpdate _ _ -> "UptimeUpdate"
    Uptime.UptimeDelete _ _ -> "UptimeDelete"

