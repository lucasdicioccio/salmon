{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for the pure @check@-verdict interpreters under
"Salmon.Builtin.Nodes.Gcp" -- each node shells out to @gcloud@/@ssh@ to get
its raw exit code and output, then hands that to a pure function that draws
the 'CheckResult'. Splitting the decision out (the same shape as
"Salmon.Builtin.Nodes.Systemd"'s @interpretShow@, see @Test.SystemdSpec@) is
what makes it testable without a real GCP project.
-}
module Test.GcpSpec (tests) where

import Data.List (isInfixOf, isSubsequenceOf)
import qualified Data.Map as Map
import GHC.IO.Exception (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Process.ListLike (CmdSpec (..), CreateProcess, cmdspec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.Binary (prepare)
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.Billing as Billing
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as LoadBalancing
import qualified Salmon.Builtin.Nodes.Gcp.ResourceManager as ResourceManager
import qualified Salmon.Builtin.Nodes.Gcp.ServiceUsage as ServiceUsage
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Gcp"
        [ testGroup "Core.interpretAdc" adcTests
        , testGroup "Compute.interpretInstanceStatus" instanceTests
        , testGroup "Storage.interpretBucketDescribe" bucketTests
        , testGroup "ArtifactRegistry.interpretRepoDescribe" repoTests
        , testGroup "CloudRun.interpretServiceDescribe" cloudRunTests
        , testGroup "Iam" iamTests
        , testGroup "LoadBalancing" lbTests
        , testGroup "ServiceUsage" serviceUsageTests
        , testGroup "Billing.interpretBillingDescribe" billingTests
        , testGroup "ResourceManager" projectTests
        , testGroup "Compute (tier 2 resources)" vmTests
        , testGroup "Ssh.ClientOpts" clientOptsTests
        ]

-- | Extracts the argument list of a prepared gcloud 'CreateProcess', for
-- asserting on rendered command-line shape without actually invoking gcloud.
processArgs :: CreateProcess -> [String]
processArgs p = case cmdspec p of
    RawCommand _path args -> args
    ShellCommand s -> [s]

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

-------------------------------------------------------------------------------

adcTests :: [TestTree]
adcTests =
    [ testCase "a printed token means ADC is usable" $
        assertEqual "" Success (Core.interpretAdc ExitSuccess)
    , testCase "no token means ADC is not configured" $
        assertBool "" (isFailure (Core.interpretAdc (ExitFailure 1)))
    ]

-------------------------------------------------------------------------------

instanceTests :: [TestTree]
instanceTests =
    [ testCase "RUNNING is satisfied" $
        assertEqual "" Success (Compute.interpretInstanceStatus ExitSuccess "RUNNING")
    , testCase "TERMINATED needs bringing up" $
        assertBool "" (isFailure (Compute.interpretInstanceStatus ExitSuccess "TERMINATED"))
    , testCase "transitional states are Unknown, not Failure" $ do
        assertEqual "provisioning" Unknown (Compute.interpretInstanceStatus ExitSuccess "PROVISIONING")
        assertEqual "staging" Unknown (Compute.interpretInstanceStatus ExitSuccess "STAGING")
        assertEqual "stopping" Unknown (Compute.interpretInstanceStatus ExitSuccess "STOPPING")
    , testCase "an unrecognized status is a Failure, not a crash" $
        assertBool "" (isFailure (Compute.interpretInstanceStatus ExitSuccess "SOME-NEW-STATUS"))
    , testCase "describe failing outright (e.g. instance absent) is a Failure" $
        assertBool "" (isFailure (Compute.interpretInstanceStatus (ExitFailure 1) ""))
    , testCase "up creates an absent instance" $
        assertEqual "" Compute.CreateInstance (Compute.planInstanceUp (ExitFailure 1) "")
    , testCase "up starts a stopped instance rather than re-creating it" $
        assertEqual "" Compute.StartInstance (Compute.planInstanceUp ExitSuccess "TERMINATED")
    , testCase "up resumes a suspended instance" $
        assertEqual "" Compute.ResumeInstance (Compute.planInstanceUp ExitSuccess "SUSPENDED")
    , testCase "up leaves a running instance alone" $
        assertEqual "" Compute.AlreadyRunning (Compute.planInstanceUp ExitSuccess "RUNNING")
    , testCase "up refuses to act on a transitional status" $
        assertEqual "" (Compute.CannotActYet "STOPPING") (Compute.planInstanceUp ExitSuccess "STOPPING")
    ]

-------------------------------------------------------------------------------

bucketTests :: [TestTree]
bucketTests =
    [ testCase "describe succeeding means the bucket exists" $
        assertEqual "" Success (Storage.interpretBucketDescribe "my-bucket" ExitSuccess)
    , testCase "describe failing means the bucket is absent" $
        assertBool "" (isFailure (Storage.interpretBucketDescribe "my-bucket" (ExitFailure 1)))
    ]

-------------------------------------------------------------------------------

repoTests :: [TestTree]
repoTests =
    [ testCase "gcloud artifacts takes --location, not --region" $
        assertEqual
            ""
            ["artifacts", "repositories", "create", "my-repo", "--repository-format", "docker", "--location", "us-west1", "--project", "p"]
            ( processArgs
                ( prepare
                    ArtifactRegistry.artifactRegistryCommand
                    (ArtifactRegistry.ReposCreate (ArtifactRegistry.ArtifactRepo "my-repo" (Core.Project "p") (Core.Region "us-west1") ArtifactRegistry.Docker))
                )
            )
    , testCase "describe succeeding means the repo exists" $
        assertEqual "" Success (ArtifactRegistry.interpretRepoDescribe "my-repo" ExitSuccess)
    , testCase "describe failing means the repo is absent" $
        assertBool "" (isFailure (ArtifactRegistry.interpretRepoDescribe "my-repo" (ExitFailure 1)))
    ]

-------------------------------------------------------------------------------

cloudRunTests :: [TestTree]
cloudRunTests =
    [ testCase "describe succeeding with the right image is satisfied" $
        assertEqual
            ""
            Success
            (CloudRun.interpretServiceDescribe "us-docker.pkg.dev/p/r/img:1" ExitSuccess "image: us-docker.pkg.dev/p/r/img:1\n")
    , testCase "describe succeeding with a stale image is not satisfied" $
        assertBool
            "wrong revision deployed"
            (isFailure (CloudRun.interpretServiceDescribe "us-docker.pkg.dev/p/r/img:2" ExitSuccess "image: us-docker.pkg.dev/p/r/img:1\n"))
    , testCase "describe failing means the service is absent" $
        assertBool "" (isFailure (CloudRun.interpretServiceDescribe "img:1" (ExitFailure 1) ""))
    ]

-------------------------------------------------------------------------------

iamTests :: [TestTree]
iamTests =
    [ testCase "service account describe succeeding means it exists" $
        assertEqual "" Success (Iam.interpretServiceAccountDescribe "sa-1" ExitSuccess)
    , testCase "service account describe failing means it is absent" $
        assertBool "" (isFailure (Iam.interpretServiceAccountDescribe "sa-1" (ExitFailure 1)))
    , testCase "a binding present in the policy is satisfied" $
        assertEqual
            ""
            Success
            ( Iam.interpretBindingPolicy
                binding
                ExitSuccess
                "bindings:\n- members:\n  - serviceAccount:sa-1@p.iam.gserviceaccount.com\n  role: roles/storage.objectViewer\n"
            )
    , testCase "a binding absent from the policy is not satisfied" $
        assertBool
            ""
            ( isFailure
                ( Iam.interpretBindingPolicy
                    binding
                    ExitSuccess
                    "bindings:\n- members:\n  - user:someone@example.com\n  role: roles/viewer\n"
                )
            )
    , testCase "get-iam-policy failing outright is not satisfied" $
        assertBool "" (isFailure (Iam.interpretBindingPolicy binding (ExitFailure 1) ""))
    , testCase "a secrets/ resource binds against the secrets group" $
        assertEqual
            ""
            ["secrets", "add-iam-policy-binding", "my-secret", "--member", "serviceAccount:sa-1@p.iam.gserviceaccount.com", "--role", "roles/secretmanager.secretAccessor"]
            (processArgs (prepare Iam.iamCommand (Iam.IamPolicyAddBinding secretBinding)))
    , testCase "an artifacts/repositories/ resource carries its location as a trailing flag, after the resource" $
        assertEqual
            ""
            ["artifacts", "repositories", "add-iam-policy-binding", "my-repo", "--location", "us-west1", "--member", "serviceAccount:sa-1@p.iam.gserviceaccount.com", "--role", "roles/uploader"]
            (processArgs (prepare Iam.iamCommand (Iam.IamPolicyAddBinding repoBinding)))
    , testCase "a project-qualified repository passes --project rather than relying on gcloud's configured project" $
        assertEqual
            ""
            ["artifacts", "repositories", "get-iam-policy", "my-repo", "--location", "us-west1", "--project", "p"]
            (processArgs (prepare Iam.iamCommand (Iam.IamPolicyGetBinding (binding {Iam.iamResource = "projects/p/locations/us-west1/repositories/my-repo"}))))
    , testCase "a project-qualified secret passes --project" $
        assertEqual
            ""
            ["secrets", "get-iam-policy", "my-secret", "--project", "p"]
            (processArgs (prepare Iam.iamCommand (Iam.IamPolicyGetBinding (binding {Iam.iamResource = "projects/p/secrets/my-secret"}))))
    , testCase "a bucket resource is rendered as the gs:// URL gcloud storage requires" $
        assertEqual
            ""
            ["storage", "buckets", "get-iam-policy", "gs://my-bucket"]
            (processArgs (prepare Iam.iamCommand (Iam.IamPolicyGetBinding (binding {Iam.iamResource = "buckets/my-bucket"}))))
    , testCase "role describe succeeding means the custom role exists" $
        assertEqual "" Success (Iam.interpretRoleDescribe "registryUploader" ExitSuccess)
    , testCase "role describe failing means the custom role is absent" $
        assertBool "" (isFailure (Iam.interpretRoleDescribe "registryUploader" (ExitFailure 1)))
    , testCase "a custom role is created from its definition file" $
        assertEqual
            ""
            ["iam", "roles", "create", "registryUploader", "--file", "infra/roles/registryUploader.yaml", "--project", "p"]
            (processArgs (prepare Iam.iamCommand (Iam.RolesCreate role)))
    , testCase "a service account key is written to its target path" $
        assertEqual
            ""
            ["iam", "service-accounts", "keys", "create", "secrets/gh-ci/uploader.key.json", "--iam-account", "uploader@p.iam.gserviceaccount.com", "--project", "p"]
            (processArgs (prepare Iam.iamCommand (Iam.ServiceAccountKeysCreate key)))
    ]
  where
    binding =
        Iam.IamBinding
            { Iam.iamPrincipal = Iam.ServiceAccount "sa-1@p.iam.gserviceaccount.com"
            , Iam.iamRole = "roles/storage.objectViewer"
            , Iam.iamResource = "projects/p"
            }
    secretBinding =
        binding
            { Iam.iamRole = "roles/secretmanager.secretAccessor"
            , Iam.iamResource = "secrets/my-secret"
            }
    repoBinding =
        binding
            { Iam.iamRole = "roles/uploader"
            , Iam.iamResource = "artifacts/repositories/us-west1/my-repo"
            }
    role =
        Iam.CustomRole
            { Iam.roleId = "registryUploader"
            , Iam.roleProject = Core.Project "p"
            , Iam.roleDefinitionFile = "infra/roles/registryUploader.yaml"
            }
    key =
        Iam.ServiceAccountKey
            { Iam.sakProject = Core.Project "p"
            , Iam.sakAccountId = "uploader"
            , Iam.sakPath = "secrets/gh-ci/uploader.key.json"
            }

-------------------------------------------------------------------------------

serviceUsageTests :: [TestTree]
serviceUsageTests =
    [ testCase "the API appearing in the enabled listing is satisfied" $
        assertEqual
            ""
            Success
            (ServiceUsage.interpretServiceList (ServiceUsage.Api "run.googleapis.com") ExitSuccess "NAME\nrun.googleapis.com\n")
    , testCase "the API absent from the enabled listing is not satisfied" $
        assertBool
            ""
            (isFailure (ServiceUsage.interpretServiceList (ServiceUsage.Api "run.googleapis.com") ExitSuccess "NAME\n"))
    , testCase "listing failing outright is not satisfied" $
        assertBool "" (isFailure (ServiceUsage.interpretServiceList (ServiceUsage.Api "run.googleapis.com") (ExitFailure 1) ""))
    ]

-------------------------------------------------------------------------------

lbTests :: [TestTree]
lbTests =
    [ testCase "describe succeeding means the url map exists" $
        assertEqual "" Success (LoadBalancing.interpretLbDescribe ExitSuccess)
    , testCase "describe failing means the load balancer is absent" $
        assertBool "" (isFailure (LoadBalancing.interpretLbDescribe (ExitFailure 1)))
    , testCase "shellQuote neutralizes a value that would otherwise break out of quoting" $ do
        assertEqual "no special characters" "'tenant-1'" (LoadBalancing.shellQuote "tenant-1")
        assertEqual
            "an embedded single quote and shell metacharacters stay inside the quoting"
            "'tenant'\\''; rm -rf / #'"
            (LoadBalancing.shellQuote "tenant'; rm -rf / #")
    , testCase "create/delete run the script with bash, not as a gcloud subcommand" $ do
        assertBool "create" (isBash (prepare LoadBalancing.loadBalancingCommand (LoadBalancing.LbCreate alb)))
        assertBool "delete" (isBash (prepare LoadBalancing.loadBalancingCommand (LoadBalancing.LbDelete alb)))
    , testCase "scripts never swallow failures with || true" $ do
        let scripts = concatMap (processArgs . prepare LoadBalancing.loadBalancingCommand) [LoadBalancing.LbCreate alb, LoadBalancing.LbDelete alb]
        assertBool "" (not (any ("|| true" `isInfixOf`) scripts))
    , testCase "rendered scripts parse as bash" $ do
        let scripts = [s' | cmd <- [LoadBalancing.LbCreate alb, LoadBalancing.LbDelete alb], (_ : s' : _) <- [processArgs (prepare LoadBalancing.loadBalancingCommand cmd)]]
        mapM_
            ( \script -> do
                (code, _, err) <- readProcessWithExitCode "bash" ["-n", "-c", script] ""
                assertEqual err ExitSuccess code
            )
            scripts
    ]
  where
    isBash p = case cmdspec p of
        RawCommand "bash" ("-c" : _) -> True
        _ -> False
    alb =
        LoadBalancing.ApplicationLoadBalancer
            { LoadBalancing.albName = "web"
            , LoadBalancing.albProject = Core.Project "p"
            , LoadBalancing.albRegion = Core.Region "europe-west1"
            , LoadBalancing.albNetwork = Just "default"
            , LoadBalancing.albBackends = [LoadBalancing.InstanceGroupBackend "ig" [8080, 8081], LoadBalancing.CloudRunBackend "svc"]
            , LoadBalancing.albHealthCheck = Just (LoadBalancing.HealthCheck "hc" 8080)
            }

-------------------------------------------------------------------------------

billingTests :: [TestTree]
billingTests =
    [ testCase "a project linked and billing-enabled is satisfied" $
        assertEqual
            ""
            Success
            (Billing.interpretBillingDescribe account ExitSuccess "billingAccountName: billingAccounts/XXXXXX-XXXXXX-XXXXXX\nbillingEnabled: true\nname: projects/p\n")
    , testCase "a project linked to a different account is not satisfied" $
        assertBool
            ""
            (isFailure (Billing.interpretBillingDescribe account ExitSuccess "billingAccountName: billingAccounts/OTHER-ACCOUNT\nbillingEnabled: true\n"))
    , testCase "a project with billing disabled is not satisfied" $
        assertBool
            ""
            (isFailure (Billing.interpretBillingDescribe account ExitSuccess "billingAccountName: billingAccounts/XXXXXX-XXXXXX-XXXXXX\nbillingEnabled: false\n"))
    , testCase "describe failing outright is not satisfied" $
        assertBool "" (isFailure (Billing.interpretBillingDescribe account (ExitFailure 1) ""))
    ]
  where
    account = Billing.BillingAccount "XXXXXX-XXXXXX-XXXXXX"

-------------------------------------------------------------------------------

projectTests :: [TestTree]
projectTests =
    [ testCase "an ACTIVE project is satisfied" $
        assertEqual "" Success (ResourceManager.interpretProjectState "p" ExitSuccess "ACTIVE")
    , testCase "a project pending deletion is not satisfied, and says why" $
        case ResourceManager.interpretProjectState "p" ExitSuccess "DELETE_REQUESTED" of
            Failure msg -> assertBool "mentions the id cannot be reused" ("cannot be reused" `isInfixOf` show msg)
            other -> assertBool ("expected Failure, got " <> show other) False
    , testCase "describe failing means the project is absent" $
        assertBool "" (isFailure (ResourceManager.interpretProjectState "p" (ExitFailure 1) ""))
    , testCase "create passes the parent and labels" $ do
        let args =
                processArgs $
                    prepare
                        ResourceManager.resourceManagerCommand
                        ( ResourceManager.ProjectsCreate
                            (ResourceManager.ProjectSpec (Core.Project "p") (ResourceManager.Folder "123") (Map.fromList [("purpose", "salmon-toy")]))
                        )
        assertEqual "" ["projects", "create", "p", "--folder", "123", "--labels", "purpose=salmon-toy"] args
    ]

-------------------------------------------------------------------------------

vmTests :: [TestTree]
vmTests =
    [ testCase "an address is reserved regionally, and read back as a bare IP" $ do
        assertEqual
            "create"
            ["compute", "addresses", "create", "toy-ip", "--region", "europe-west1", "--project", "p"]
            (processArgs (prepare Compute.computeCommand (Compute.AddressesCreate addr)))
        assertEqual
            "describe asks for the address itself, which is what a driver needs"
            ["compute", "addresses", "describe", "toy-ip", "--region", "europe-west1", "--format=value(address)", "--project", "p"]
            (processArgs (prepare Compute.computeCommand (Compute.AddressesDescribe addr)))
    , testCase "a reserved address with no IP yet is not satisfied" $
        assertBool "" (isFailure (Compute.interpretAddressDescribe "toy-ip" ExitSuccess ""))
    , testCase "a reserved address with an IP is satisfied" $
        assertEqual "" Success (Compute.interpretAddressDescribe "toy-ip" ExitSuccess "34.1.2.3")
    , testCase "a firewall rule carries its allow, ranges and target tags" $
        assertEqual
            ""
            [ "compute", "firewall-rules", "create", "toy-ssh"
            , "--network", "default", "--allow", "tcp:22"
            , "--source-ranges", "0.0.0.0/0", "--project", "p"
            , "--target-tags", "toy-ssh"
            ]
            (processArgs (prepare Compute.computeCommand (Compute.FirewallCreate fw)))
    , testCase "an instance boots from an image family, in its publisher's project" $
        assertBool
            "--image-family and --image-project are passed"
            (["--image-family", "ubuntu-2404-lts-amd64"] `isSubsequenceOf` args && ["--image-project", "ubuntu-os-cloud"] `isSubsequenceOf` args)
    , testCase "a multi-line startup script goes through --metadata-from-file" $
        assertBool
            "a newline-bearing value cannot ride in --metadata KEY=VALUE"
            (["--metadata-from-file", "startup-script=/tmp/w/startup-script.sh"] `isSubsequenceOf` args)
    , testCase "the instance claims the reserved address by name" $
        assertBool "" (["--address", "toy-ip"] `isSubsequenceOf` args)
    ]
  where
    args = processArgs (prepare Compute.computeCommand (Compute.InstancesCreate inst))
    addr = Compute.Address "toy-ip" (Core.Project "p") (Core.Region "europe-west1")
    fw =
        Compute.FirewallRule
            { Compute.firewallName = "toy-ssh"
            , Compute.firewallProject = Core.Project "p"
            , Compute.firewallNetwork = "default"
            , Compute.firewallAllow = "tcp:22"
            , Compute.firewallSourceRanges = ["0.0.0.0/0"]
            , Compute.firewallTargetTags = ["toy-ssh"]
            }
    inst =
        Compute.Instance
            { Compute.instanceName = "toy-vm"
            , Compute.instanceProject = Core.Project "p"
            , Compute.instanceZone = Core.Zone "europe-west1-b"
            , Compute.instanceMachineType = Compute.Custom "e2-micro"
            , Compute.instanceBootDisk = Compute.BootDisk 10 Nothing (Just "ubuntu-2404-lts-amd64") (Just "ubuntu-os-cloud")
            , Compute.instanceNetwork = "default"
            , Compute.instanceSubnet = "default"
            , Compute.instanceServiceAccount = Nothing
            , Compute.instanceMetadata = Map.fromList [("enable-oslogin", "FALSE")]
            , Compute.instanceMetadataFiles = Map.fromList [("startup-script", "/tmp/w/startup-script.sh")]
            , Compute.instanceAddress = Just "toy-ip"
            , Compute.instanceTags = ["toy-ssh"]
            }

-------------------------------------------------------------------------------

clientOptsTests :: [TestTree]
clientOptsTests =
    [ testCase "no options means ssh authenticates as it always did" $
        assertEqual "" [] (Ssh.clientArgs Ssh.noClientOpts)
    , testCase "an identity is offered exclusively" $
        assertEqual
            "IdentitiesOnly, or an agent key can be tried first and the cert never reached"
            ["-i", "/w/ssh/toy-client", "-o", "IdentitiesOnly=yes"]
            (Ssh.clientArgs Ssh.noClientOpts{Ssh.optIdentity = Just "/w/ssh/toy-client"})
    , testCase "a known-hosts file comes with accept-new" $
        assertEqual
            ""
            ["-o", "UserKnownHostsFile=/w/ssh/known_hosts", "-o", "StrictHostKeyChecking=accept-new"]
            (Ssh.clientArgs Ssh.noClientOpts{Ssh.optKnownHosts = Just "/w/ssh/known_hosts"})
    , testCase "rsync carries the same options through --rsh" $
        assertEqual
            "rsync has no -i of its own"
            [ "--copy-links"
            , "--rsh"
            , "ssh -i /w/ssh/toy-client -o IdentitiesOnly=yes -o UserKnownHostsFile=/w/ssh/known_hosts -o StrictHostKeyChecking=accept-new"
            , "/local/bin"
            , "salmon@1.2.3.4:/home/salmon/bin"
            ]
            (processArgs (prepare Rsync.rsyncRun (Rsync.SendFile "/local/bin" (Rsync.Remote "salmon" "1.2.3.4") "/home/salmon/bin" opts)))
    , testCase "a changed host key is recognised as such" $
        assertBool
            "the one ssh failure that never resolves by waiting"
            (Ssh.isHostKeyMismatch "@@@@\nWARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!\n")
    , testCase "an ordinary refusal is not a host key mismatch" $
        assertBool
            "a VM still booting must be waited out, not have its host key forgotten"
            (not (Ssh.isHostKeyMismatch "salmon@1.2.3.4: Permission denied (publickey)."))
    ]
  where
    opts = Ssh.ClientOpts (Just "/w/ssh/toy-client") (Just "/w/ssh/known_hosts")
