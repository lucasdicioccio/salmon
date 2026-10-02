{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for the pure @check@-verdict interpreters under
"Salmon.Builtin.Nodes.Gcp" -- each node shells out to @gcloud@/@ssh@ to get
its raw exit code and output, then hands that to a pure function that draws
the 'CheckResult'. Splitting the decision out (the same shape as
"Salmon.Builtin.Nodes.Systemd"'s @interpretShow@, see @Test.SystemdSpec@) is
what makes it testable without a real GCP project.
-}
module Test.GcpSpec (tests) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isAsciiLower, isDigit)
import Data.List (isInfixOf, isSubsequenceOf, nub)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Foldable (toList)
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
import qualified Salmon.Builtin.Nodes.Gcp.CloudDns as CloudDns
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as LoadBalancing
import qualified Salmon.Builtin.Nodes.Gcp.Monitoring as Monitoring
import qualified Salmon.Builtin.Nodes.Gcp.ResourceManager as ResourceManager
import qualified Salmon.Builtin.Nodes.Gcp.SecretManager as SecretManager
import qualified Salmon.Builtin.Nodes.Gcp.ServiceUsage as ServiceUsage
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified SreBox.Gcp.CloudRunAlerts as CloudRunAlerts
import qualified SreBox.Gcp.VmProvision as VmProvision

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
        , testGroup "Compute (tier 3 resources)" lbBackendTests
        , testGroup "SecretManager" secretTests
        , testGroup "CloudRun options" cloudRunOptionTests
        , testGroup "Ssh.ClientOpts" clientOptsTests
        , testGroup "Monitoring" monitoringTests
        , testGroup "CloudDns" cloudDnsTests
        , testGroup "SreBox.Gcp.CloudRunAlerts" cloudRunAlertsTests
        , testGroup "SreBox.Gcp.VmProvision.caTrustStartupScript" caTrustScriptTests
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
        assertEqual "" Success (Compute.interpretInstanceStatus on ExitSuccess "RUNNING")
    , testCase "TERMINATED needs bringing up" $
        assertBool "" (isFailure (Compute.interpretInstanceStatus on ExitSuccess "TERMINATED"))
    , testCase "transitional states are Unknown, not Failure" $ do
        assertEqual "provisioning" Unknown (Compute.interpretInstanceStatus on ExitSuccess "PROVISIONING")
        assertEqual "staging" Unknown (Compute.interpretInstanceStatus on ExitSuccess "STAGING")
        assertEqual "stopping" Unknown (Compute.interpretInstanceStatus on ExitSuccess "STOPPING")
    , testCase "an unrecognized status is a Failure, not a crash" $
        assertBool "" (isFailure (Compute.interpretInstanceStatus on ExitSuccess "SOME-NEW-STATUS"))
    , testCase "describe failing outright (e.g. instance absent) is a Failure" $
        assertBool "" (isFailure (Compute.interpretInstanceStatus on (ExitFailure 1) ""))
    , testCase "up creates an absent instance" $
        assertEqual "" Compute.CreateInstance (Compute.planInstanceUp on (ExitFailure 1) "")
    , testCase "up starts a stopped instance rather than re-creating it" $
        assertEqual "" Compute.StartInstance (Compute.planInstanceUp on ExitSuccess "TERMINATED")
    , testCase "up resumes a suspended instance" $
        assertEqual "" Compute.ResumeInstance (Compute.planInstanceUp on ExitSuccess "SUSPENDED")
    , testCase "up leaves a running instance alone" $
        assertEqual "" Compute.AlreadyThere (Compute.planInstanceUp on ExitSuccess "RUNNING")
    , testCase "up refuses to act on a transitional status" $
        assertEqual "" (Compute.CannotActYet "STOPPING") (Compute.planInstanceUp on ExitSuccess "STOPPING")
    , testCase "declared stopped: TERMINATED is satisfied and RUNNING is not" $ do
        assertEqual "" Success (Compute.interpretInstanceStatus off ExitSuccess "TERMINATED")
        assertBool "" (isFailure (Compute.interpretInstanceStatus off ExitSuccess "RUNNING"))
    , testCase "declared stopped: up stops a running instance and leaves a stopped one alone" $ do
        assertEqual "" Compute.StopInstance (Compute.planInstanceUp off ExitSuccess "RUNNING")
        assertEqual "" Compute.AlreadyThere (Compute.planInstanceUp off ExitSuccess "TERMINATED")
        assertEqual "" Compute.CreateInstance (Compute.planInstanceUp off (ExitFailure 1) "")
    , testCase "declared stopped: a suspended instance is not stopped blindly" $
        assertBool "" (case Compute.planInstanceUp off ExitSuccess "SUSPENDED" of Compute.CannotActYet _ -> True; _ -> False)
    , testCase "for down, a stopped instance is still present; an absent one is not" $ do
        assertEqual "" Success (Compute.interpretInstancePresence ExitSuccess "TERMINATED")
        assertBool "" (isFailure (Compute.interpretInstancePresence (ExitFailure 1) ""))
    , testCase "stop is rendered like start" $
        assertEqual
            ""
            (RawCommand "gcloud" ["compute", "instances", "stop", "toy-vm", "--zone", "europe-west1-b", "--project", "p"])
            (cmdspec (prepare Compute.computeCommand (Compute.InstancesStop toyInstance)))
    ]
  where
    on = Compute.PoweredOn
    off = Compute.PoweredOff
    toyInstance =
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
            , Compute.instanceExternalAddress = Compute.ReservedExternal "toy-ip"
            , Compute.instanceInternalAddress = Compute.EphemeralInternal
            , Compute.instanceTags = ["toy-ssh"]
            , Compute.instancePower = Compute.PoweredOn
            }

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
    [ testCase "describe succeeding with what was declared is satisfied" $
        assertEqual "" Success (verdict declared (describeJson "us-docker.pkg.dev/p/r/img:1" (Just "sa@p.iam.gserviceaccount.com") [plain "A" "1", secret "S"]))
    , testCase "a stale image is not satisfied, and the reason names both" $
        case verdict declared (describeJson "us-docker.pkg.dev/p/r/img:0" (Just "sa@p.iam.gserviceaccount.com") [plain "A" "1"]) of
            Failure why -> do
                assertBool (Text.unpack why) ("img:0" `Text.isInfixOf` why)
                assertBool (Text.unpack why) ("img:1" `Text.isInfixOf` why)
            other -> assertBool (show other) False
    , -- the substring match this replaces called these the same
      testCase "img:1 is not img:10" $ do
        assertBool "" (isFailure (verdict declared (describeJson "us-docker.pkg.dev/p/r/img:10" (Just "sa@p.iam.gserviceaccount.com") [plain "A" "1"])))
        assertBool "" (isFailure (verdict declared{CloudRun.crsImage = "us-docker.pkg.dev/p/r/img:10"} (describeJson "us-docker.pkg.dev/p/r/img:1" (Just "sa@p.iam.gserviceaccount.com") [plain "A" "1"])))
    , testCase "an image that merely appears elsewhere in the output is not the image" $
        assertBool "" (isFailure (verdict declared (Text.replace "\"containers\"" "\"note\": \"us-docker.pkg.dev/p/r/img:1\", \"containers\"" (describeJson "other:2" (Just "sa@p.iam.gserviceaccount.com") [plain "A" "1"]))))
    , testCase "a changed service account is drift" $
        assertBool "" (isFailure (verdict declared (describeJson "us-docker.pkg.dev/p/r/img:1" (Just "someone-else@p.iam.gserviceaccount.com") [plain "A" "1"])))
    , testCase "a service with no service account at all is drift" $
        assertBool "" (isFailure (verdict declared (describeJson "us-docker.pkg.dev/p/r/img:1" Nothing [plain "A" "1"])))
    , testCase "a changed, missing and extra environment variable are each drift, named and never quoted" $ do
        let why env = case verdict declared (describeJson "us-docker.pkg.dev/p/r/img:1" (Just "sa@p.iam.gserviceaccount.com") env) of
                Failure w -> w
                other -> Text.pack (show other)
        assertBool "" ("A" `Text.isInfixOf` why [plain "A" "changed-value-9"])
        assertBool "the value is not in the reason" (not ("changed-value-9" `Text.isInfixOf` why [plain "A" "changed-value-9"]))
        assertBool "" ("environment variable A is missing" `Text.isInfixOf` why [])
        assertBool "" ("environment variable EXTRA is set but not declared" `Text.isInfixOf` why [plain "A" "1", plain "EXTRA" "x"])
    , testCase "a secret-bound variable is not a plain one: extra secrets are not env drift" $
        assertEqual "" Success (verdict declared (describeJson "us-docker.pkg.dev/p/r/img:1" (Just "sa@p.iam.gserviceaccount.com") [plain "A" "1", secret "PGRST_JWT_SECRET"]))
    , testCase "output that is not the expected JSON is Unknown, not a redeploy" $ do
        assertEqual "" Unknown (verdict declared "image: us-docker.pkg.dev/p/r/img:1\n")
        assertEqual "" Unknown (verdict declared "{\"spec\": {}}")
    , testCase "describe failing means the service is absent" $
        assertBool "" (isFailure (CloudRun.interpretServiceDescribe declared (ExitFailure 1) ""))
    , testCase "the describe asks for JSON" $
        assertBool "" ("--format=json" `elem` processArgs (prepare CloudRun.cloudRunCommand (CloudRun.RunDescribe declared)))
    , testCase "for down, a service on a stale image is still present" $ do
        -- down deletes what exists; the image only matters for up.
        assertEqual "" Success (CloudRun.interpretServicePresence ExitSuccess "image: us-docker.pkg.dev/p/r/img:1\n")
        assertBool "" (isFailure (CloudRun.interpretServicePresence (ExitFailure 1) ""))
    ]

declared :: CloudRun.CloudRunService
declared =
    CloudRun.CloudRunService
        { CloudRun.crsName = "svc"
        , CloudRun.crsProject = Core.Project "p"
        , CloudRun.crsRegion = Core.Region "europe-west1"
        , CloudRun.crsImage = "us-docker.pkg.dev/p/r/img:1"
        , CloudRun.crsEnv = Map.fromList [("A", "1")]
        , CloudRun.crsServiceAccount = "sa@p.iam.gserviceaccount.com"
        , CloudRun.crsIngress = CloudRun.All
        , CloudRun.crsMaxInstances = Nothing
        , CloudRun.crsOptions = CloudRun.defaultCloudRunOptions
        }

verdict :: CloudRun.CloudRunService -> Text.Text -> CheckResult
verdict svc = CloudRun.interpretServiceDescribe svc ExitSuccess

plain :: Text.Text -> Text.Text -> Value
plain k v = object ["name" .= k, "value" .= v]

secret :: Text.Text -> Value
secret k = object ["name" .= k, "valueFrom" .= object ["secretKeyRef" .= object ["name" .= ("s" :: Text.Text), "key" .= ("latest" :: Text.Text)]]]

-- | The parts of @gcloud run services describe --format=json@ the check reads.
describeJson :: Text.Text -> Maybe Text.Text -> [Value] -> Text.Text
describeJson = describeJsonWith []

-- | The same, with annotations on the revision template.
describeJsonWith :: [(Text.Text, Text.Text)] -> Text.Text -> Maybe Text.Text -> [Value] -> Text.Text
describeJsonWith annotations image sa env =
    Text.decodeUtf8 . LByteString.toStrict . encode $
        object
            [ "spec"
                .= object
                    [ "template"
                        .= object
                            [ "metadata" .= object ["annotations" .= Map.fromList annotations]
                            , "spec"
                                .= object
                                    ( [ "containers" .= [object ["image" .= image, "env" .= env]]
                                      ]
                                        <> maybe [] (\a -> ["serviceAccountName" .= a]) sa
                                    )
                            ]
                    ]
            , "status" .= object ["latestReadyRevisionName" .= ("svc-00001" :: Text.Text)]
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

secretTests :: [TestTree]
secretTests =
    [ testCase "a version is added from a file, never from argv" $ do
        -- argv is world-readable through /proc for the life of the process.
        let args = processArgs (prepare SecretManager.secretManagerCommand (SecretManager.VersionsAdd version))
        assertBool (show args) (["--data-file", "/certs/db.key"] `isSubsequenceOf` args)
        assertBool (show args) (["secrets", "versions", "add", "db-key"] `isSubsequenceOf` args)
    , testCase "reading back compares the exact bytes, trailing newline included" $ do
        -- Trimming would make a node that had just uploaded its own PEM
        -- report a difference on every later pass.
        assertEqual "" Success (SecretManager.interpretSecretContents "s" "abc\n" ExitSuccess "abc\n")
        assertBool "" (isFailure (SecretManager.interpretSecretContents "s" "abc\n" ExitSuccess "abc"))
        assertBool "" (isFailure (SecretManager.interpretSecretContents "s" "abc" (ExitFailure 1) ""))
    , testCase "a secret that does not describe is absent" $ do
        assertEqual "" Success (SecretManager.interpretSecretDescribe "s" ExitSuccess)
        assertBool "" (isFailure (SecretManager.interpretSecretDescribe "s" (ExitFailure 1)))
    ]
  where
    sec = SecretManager.Secret "db-key" (Core.Project "p") "automatic"
    version = SecretManager.SecretVersion sec "/certs/db.key"

cloudRunOptionTests :: [TestTree]
cloudRunOptionTests =
    [ testCase "every secret rides on ONE --set-secrets flag" $ do
        -- gcloud treats a repeated --set-secrets as a replacement, so the
        -- one-flag-per-binding form silently deploys with only the last.
        let args = processArgs (prepare CloudRun.cloudRunCommand (CloudRun.RunDeploy svc))
            flags = length (filter (== "--set-secrets") args)
        assertEqual (show args) 1 flags
        assertBool (show args) ("/opt/vault/cert.pem=svc-cert:latest,PGRST_JWT_SECRET=svc-jwt:latest" `elem` args)
    , testCase "resource knobs are passed only when set" $ do
        let bare = processArgs (prepare CloudRun.cloudRunCommand (CloudRun.RunDeploy svc{CloudRun.crsOptions = CloudRun.defaultCloudRunOptions}))
        assertBool (show bare) (not ("--set-secrets" `elem` bare))
        assertBool (show bare) (not ("--cpu" `elem` bare))
        assertBool (show bare) (not ("--allow-unauthenticated" `elem` bare))
        assertBool (show bare) (not ("--no-invoker-iam-check" `elem` bare))
        assertBool (show bare) (not ("--min-instances" `elem` bare))
        assertBool (show bare) (not ("--no-cpu-throttling" `elem` bare))
        assertBool (show bare) (not ("--cpu-throttling" `elem` bare))
        let full = processArgs (prepare CloudRun.cloudRunCommand (CloudRun.RunDeploy svc))
        assertBool (show full) (["--cpu", "1000m"] `isSubsequenceOf` full)
        assertBool (show full) (["--memory", "256Mi"] `isSubsequenceOf` full)
        assertBool (show full) (["--concurrency", "80"] `isSubsequenceOf` full)
    , testCase "disabling the invoker IAM check is a deploy flag, not an IAM write" $ do
        -- Under iam.allowedPolicyMemberDomains, --allow-unauthenticated
        -- deploys and only warns that allUsers was refused; the flag form is
        -- part of the spec, so it either lands or the deploy fails.
        let opts = CloudRun.defaultCloudRunOptions{CloudRun.croInvokerIamCheckDisabled = True}
            args = processArgs (prepare CloudRun.cloudRunCommand (CloudRun.RunDeploy svc{CloudRun.crsOptions = opts}))
        assertBool (show args) ("--no-invoker-iam-check" `elem` args)
        assertBool (show args) (not ("--allow-unauthenticated" `elem` args))
    , testCase "min instances and always-allocated CPU are deploy flags" $ do
        -- What a service with a background loop needs: an instance that
        -- stays, and CPU for it outside a request.
        let args = processArgs (prepare CloudRun.cloudRunCommand (CloudRun.RunDeploy svc{CloudRun.crsOptions = alwaysOn}))
        assertBool (show args) (["--min-instances", "1"] `isSubsequenceOf` args)
        assertBool (show args) ("--no-cpu-throttling" `elem` args)
        -- Just 0 is said, not dropped: it is how a service is put back to
        -- scaling to zero.
        let zero = processArgs (prepare CloudRun.cloudRunCommand (CloudRun.RunDeploy svc{CloudRun.crsOptions = CloudRun.defaultCloudRunOptions{CloudRun.croMinInstances = Just 0}}))
        assertBool (show zero) (["--min-instances", "0"] `isSubsequenceOf` zero)
    , testCase "declared scaling knobs are compared against the template's annotations" $ do
        let on = declared{CloudRun.crsOptions = alwaysOn}
            out anns = describeJsonWith anns "us-docker.pkg.dev/p/r/img:1" (Just "sa@p.iam.gserviceaccount.com") [plain "A" "1"]
            both = [("autoscaling.knative.dev/minScale", "1"), ("run.googleapis.com/cpu-throttling", "false")]
        assertEqual "" Success (verdict on (out both))
        -- no annotation at all is scale-to-zero and throttled
        assertBool "" (isFailure (verdict on (out [])))
        assertBool "" (isFailure (verdict on (out [("autoscaling.knative.dev/minScale", "2"), ("run.googleapis.com/cpu-throttling", "false")])))
        assertBool "" (isFailure (verdict on (out [("autoscaling.knative.dev/minScale", "1"), ("run.googleapis.com/cpu-throttling", "true")])))
        -- Just 0 is satisfied by the annotation being absent
        let zero = declared{CloudRun.crsOptions = CloudRun.defaultCloudRunOptions{CloudRun.croMinInstances = Just 0}}
        assertEqual "" Success (verdict zero (out []))
        assertBool "" (isFailure (verdict zero (out both)))
    , testCase "undeclared scaling knobs are not compared" $
        -- Whatever the service has is left alone, as the deploy leaves it.
        assertEqual
            ""
            Success
            ( verdict
                declared
                ( describeJsonWith
                    [("autoscaling.knative.dev/minScale", "3"), ("run.googleapis.com/cpu-throttling", "false")]
                    "us-docker.pkg.dev/p/r/img:1"
                    (Just "sa@p.iam.gserviceaccount.com")
                    [plain "A" "1"]
                )
            )
    ]
  where
    alwaysOn =
        CloudRun.defaultCloudRunOptions
            { CloudRun.croMinInstances = Just 1
            , CloudRun.croCpuAlwaysAllocated = True
            }
    svc =
        CloudRun.CloudRunService
            { CloudRun.crsName = "svc"
            , CloudRun.crsProject = Core.Project "p"
            , CloudRun.crsRegion = Core.Region "europe-west1"
            , CloudRun.crsImage = "img:1"
            , CloudRun.crsEnv = Map.empty
            , CloudRun.crsServiceAccount = "sa@p.iam.gserviceaccount.com"
            , CloudRun.crsIngress = CloudRun.All
            , CloudRun.crsMaxInstances = Just 1
            , CloudRun.crsOptions =
                CloudRun.defaultCloudRunOptions
                    { CloudRun.croSecrets =
                        [ CloudRun.SecretFile "/opt/vault/cert.pem" "svc-cert" "latest"
                        , CloudRun.SecretEnvVar "PGRST_JWT_SECRET" "svc-jwt" "latest"
                        ]
                    , CloudRun.croCpu = Just "1000m"
                    , CloudRun.croMemory = Just "256Mi"
                    , CloudRun.croConcurrency = Just 80
                    }
            }

lbBackendTests :: [TestTree]
lbBackendTests =
    [ testCase "a proxy-only subnet is created ACTIVE, with its purpose" $ do
        let args = processArgs (prepare Compute.computeCommand (Compute.SubnetsCreate proxySubnet))
        assertBool (show args) (["--purpose", "REGIONAL_MANAGED_PROXY"] `isSubsequenceOf` args)
        assertBool (show args) (["--role", "ACTIVE"] `isSubsequenceOf` args)
        assertBool (show args) (["--range", "192.168.100.0/24"] `isSubsequenceOf` args)
    , testCase "an ordinary subnet is not given a role" $ do
        let args = processArgs (prepare Compute.computeCommand (Compute.SubnetsCreate proxySubnet{Compute.subnetPurpose = Compute.PrivateSubnet}))
        assertBool (show args) (not ("--role" `elem` args))
    , testCase "a subnet of the wrong purpose is not the subnet that was asked for" $ do
        assertEqual "" Success (Compute.interpretSubnetDescribe "s" "REGIONAL_MANAGED_PROXY" ExitSuccess "REGIONAL_MANAGED_PROXY")
        assertBool "a plain subnet under that name" (isFailure (Compute.interpretSubnetDescribe "s" "REGIONAL_MANAGED_PROXY" ExitSuccess "PRIVATE"))
        assertBool "absent" (isFailure (Compute.interpretSubnetDescribe "s" "REGIONAL_MANAGED_PROXY" (ExitFailure 1) ""))
    , testCase "gcloud leaving an ordinary subnet's purpose empty still satisfies PRIVATE" $
        assertEqual "" Success (Compute.interpretSubnetDescribe "s" "PRIVATE" ExitSuccess "")
    , testCase "the instance group is unmanaged and zonal" $ do
        let args = processArgs (prepare Compute.computeCommand (Compute.InstanceGroupsCreate group))
        assertBool (show args) (["instance-groups", "unmanaged", "create", "ig"] `isSubsequenceOf` args)
        assertBool (show args) (["--zone", "europe-west1-b"] `isSubsequenceOf` args)
    , testCase "membership is read off the listing's last path segment" $ do
        let listing = "https://www.googleapis.com/compute/v1/projects/p/zones/europe-west1-b/instances/web\n"
        assertEqual "" Success (Compute.interpretGroupMembership "web" ExitSuccess listing)
        -- the whole reason not to use a substring match
        assertBool "a longer name containing this one" (isFailure (Compute.interpretGroupMembership "web" ExitSuccess "projects/p/zones/z/instances/web-canary\n"))
        assertBool "empty listing" (isFailure (Compute.interpretGroupMembership "web" ExitSuccess ""))
        assertBool "listing failed" (isFailure (Compute.interpretGroupMembership "web" (ExitFailure 1) ""))
    ]
  where
    proxySubnet =
        Compute.Subnet
            { Compute.subnetName = "proxy"
            , Compute.subnetProject = Core.Project "p"
            , Compute.subnetRegion = Core.Region "europe-west1"
            , Compute.subnetNetwork = "default"
            , Compute.subnetRange = "192.168.100.0/24"
            , Compute.subnetPurpose = Compute.RegionalManagedProxy
            }
    group = Compute.InstanceGroup "ig" (Core.Project "p") (Core.Zone "europe-west1-b")

-------------------------------------------------------------------------------

lbTests :: [TestTree]
lbTests =
    [ testCase "describe succeeding means the url map exists" $
        assertEqual "" Success (LoadBalancing.interpretLbDescribe ExitSuccess)
    , testCase "describe failing means the load balancer is absent" $
        assertBool "" (isFailure (LoadBalancing.interpretLbDescribe (ExitFailure 1)))
    , testCase "a complete, healthy balancer is Success" $
        assertEqual "" Success (LoadBalancing.interpretLbCheck ExitSuccess "HEALTH backend HEALTHY\nHEALTH backend HEALTHY\n")
    , testCase "no health lines (Cloud Run NEG) and nothing missing is Success" $
        assertEqual "" Success (LoadBalancing.interpretLbCheck ExitSuccess "")
    , testCase "a missing sub-resource is a Failure naming it" $ do
        let v = LoadBalancing.interpretLbCheck ExitSuccess "MISSING url-maps x-url-map\nHEALTH backend HEALTHY\n"
        assertBool "" (isFailure v)
        case v of
            Failure t -> assertBool "names it" ("url-maps x-url-map" `isInfixOf` Text.unpack t)
            _ -> pure ()
    , testCase "unhealthy backends (the post-up window) are Unknown, not Failure" $
        assertEqual "" Unknown (LoadBalancing.interpretLbCheck ExitSuccess "HEALTH backend HEALTHY\nHEALTH backend UNHEALTHY\n")
    , testCase "missing outranks unhealthy" $
        assertBool "" (isFailure (LoadBalancing.interpretLbCheck ExitSuccess "MISSING backend neg x\nHEALTH backend UNHEALTHY\n"))
    , testCase "a broken check script is a Failure" $
        assertBool "" (isFailure (LoadBalancing.interpretLbCheck (ExitFailure 2) ""))
    , testCase "check script describes every sub-resource and asks for health" $ do
        let sc = Text.unpack (LoadBalancing.renderLbCheckScript alb)
        mapM_ (\w -> assertBool w (w `isInfixOf` sc)) ["url-maps describe", "target-http-proxies describe", "forwarding-rules describe", "get-health", "health-checks describe"]
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
    , testCase "a zonal instance group is addressed by zone, not by the balancer's region" $ do
        -- The bug this pins: every gcloud call naming the group used to get
        -- the balancer's --region, which an unmanaged (zonal) group rejects
        -- outright -- so the one backend kind made of VMs salmon declared
        -- could never be attached at all.
        assertBool script ("--instance-group-zone='europe-west1-b'" `isInfixOf` script)
        assertBool script (not ("--instance-group-region" `isInfixOf` script))
        assertBool script ("set-named-ports 'ig' --project=\"$PROJECT\" --zone='europe-west1-b'" `isInfixOf` script)
    , testCase "exists() is a bare predicate, so each caller says where its resource lives" $ do
        -- It used to append --project/--region to whatever it was handed,
        -- which silently made every describe a regional one.
        assertBool script ("exists() { \"$@\" >/dev/null 2>&1; }" `isInfixOf` script)
        assertBool script ("exists gcloud compute url-maps describe 'web-url-map' --project=\"$PROJECT\" --region=\"$REGION\"" `isInfixOf` script)
    , testCase "a regional instance group keeps the regional flag" $ do
        let regionalScript = createScript alb{LoadBalancing.albBackends = [LoadBalancing.InstanceGroupBackend "ig" (LoadBalancing.InstanceGroupRegion "europe-west1") [8080]]}
        assertBool regionalScript ("--instance-group-region='europe-west1'" `isInfixOf` regionalScript)
    , testCase "rendered scripts parse as bash" $ do
        let scripts = [s' | cmd <- [LoadBalancing.LbCreate alb, LoadBalancing.LbCheck alb, LoadBalancing.LbDelete alb], (_ : s' : _) <- [processArgs (prepare LoadBalancing.loadBalancingCommand cmd)]]
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
    createScript a = case processArgs (prepare LoadBalancing.loadBalancingCommand (LoadBalancing.LbCreate a)) of
        (_ : s : _) -> s
        other -> error (show other)
    script = createScript alb
    alb =
        LoadBalancing.ApplicationLoadBalancer
            { LoadBalancing.albName = "web"
            , LoadBalancing.albProject = Core.Project "p"
            , LoadBalancing.albRegion = Core.Region "europe-west1"
            , LoadBalancing.albNetwork = Just "default"
            , LoadBalancing.albBackends =
                [ LoadBalancing.InstanceGroupBackend "ig" (LoadBalancing.InstanceGroupZone "europe-west1-b") [8080, 8081]
                , LoadBalancing.CloudRunBackend "svc"
                ]
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
    , testCase "an instance left to GCP names neither address" $ do
        let plain = createArgs inst{Compute.instanceExternalAddress = Compute.EphemeralExternal}
        assertBool (show plain) (not (any (`elem` ["--address", "--no-address", "--private-network-ip"]) plain))
    , testCase "a pinned internal address is passed as --private-network-ip" $
        assertBool
            ""
            (["--private-network-ip", "10.132.0.10"] `isSubsequenceOf` createArgs inst{Compute.instanceInternalAddress = Compute.PinnedInternal "10.132.0.10"})
    , testCase "an instance with no external address says so, and can still be pinned" $ do
        let private =
                createArgs
                    inst
                        { Compute.instanceExternalAddress = Compute.NoExternalAddress
                        , Compute.instanceInternalAddress = Compute.PinnedInternal "10.132.0.11"
                        }
        assertBool (show private) ("--no-address" `elem` private && "--address" `notElem` private)
        assertBool (show private) (["--private-network-ip", "10.132.0.11"] `isSubsequenceOf` private)
    , testCase "an internal address is reserved in its subnet, at the declared literal" $ do
        assertEqual
            "pinned"
            ["compute", "addresses", "create", "toy-int", "--region", "europe-west1", "--project", "p", "--subnet", "default", "--addresses", "10.132.0.10"]
            (processArgs (prepare Compute.computeCommand (Compute.AddressesCreate (internal (Just "10.132.0.10")))))
        assertEqual
            "left to GCP"
            ["compute", "addresses", "create", "toy-int", "--region", "europe-west1", "--project", "p", "--subnet", "default"]
            (processArgs (prepare Compute.computeCommand (Compute.AddressesCreate (internal Nothing))))
    , testCase "an internal address reserved at the declared literal is satisfied" $
        assertEqual "" Success (Compute.interpretAddress (internal (Just "10.132.0.10")) ExitSuccess "10.132.0.10")
    , testCase "an internal address reserved at another literal is not" $
        assertBool "" (isFailure (Compute.interpretAddress (internal (Just "10.132.0.10")) ExitSuccess "10.132.0.99"))
    , testCase "an address with no declared literal is satisfied by whatever was reserved" $ do
        assertEqual "internal" Success (Compute.interpretAddress (internal Nothing) ExitSuccess "10.132.0.99")
        assertEqual "external" Success (Compute.interpretAddress addr ExitSuccess "34.1.2.3")
        assertBool "absent" (isFailure (Compute.interpretAddress (internal (Just "10.132.0.10")) (ExitFailure 1) ""))
    ]
  where
    args = createArgs inst
    createArgs i = processArgs (prepare Compute.computeCommand (Compute.InstancesCreate i))
    addr = Compute.Address "toy-ip" (Core.Project "p") (Core.Region "europe-west1") Compute.ExternalAddress
    internal ip = Compute.Address "toy-int" (Core.Project "p") (Core.Region "europe-west1") (Compute.InternalAddress "default" ip)
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
            , Compute.instanceExternalAddress = Compute.ReservedExternal "toy-ip"
            , Compute.instanceInternalAddress = Compute.EphemeralInternal
            , Compute.instanceTags = ["toy-ssh"]
            , Compute.instancePower = Compute.PoweredOn
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
    , testCase "a directory upload carries them too" $
        assertEqual
            "sendDirWith is sendFileWith's --rsh treatment, recursively"
            [ "--copy-links"
            , "--recursive"
            , "--rsh"
            , "ssh -i /w/ssh/toy-client -o IdentitiesOnly=yes -o UserKnownHostsFile=/w/ssh/known_hosts -o StrictHostKeyChecking=accept-new"
            , "/local/files"
            , "salmon@1.2.3.4:/home/salmon/files"
            ]
            (processArgs (prepare Rsync.rsyncRun (Rsync.SendDir "/local/files" (Rsync.Remote "salmon" "1.2.3.4") "/home/salmon/files" opts)))
    , testCase "a directory upload without options is the same command as before" $
        assertEqual
            ""
            ["--copy-links", "--recursive", "/local/files", "salmon@1.2.3.4:/home/salmon/files"]
            (processArgs (prepare Rsync.rsyncRun (Rsync.SendDir "/local/files" (Rsync.Remote "salmon" "1.2.3.4") "/home/salmon/files" Ssh.noClientOpts)))
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

-------------------------------------------------------------------------------

monitoringTests :: [TestTree]
monitoringTests =
    [ testCase "an email channel is created with its type and address as channel labels" $ do
        let args = processArgs (prepare Monitoring.monitoringCommand (Monitoring.ChannelsCreate channel))
        assertBool (show args) (["beta", "monitoring", "channels", "create"] `isSubsequenceOf` args)
        assertBool (show args) (["--type", "email"] `isSubsequenceOf` args)
        assertBool (show args) (["--channel-labels", "email_address=ops@example.org"] `isSubsequenceOf` args)
        assertBool (show args) (["--display-name", "ops mail"] `isSubsequenceOf` args)
        assertBool (show args) (["--project", "p"] `isSubsequenceOf` args)
    , testCase "channels and policies are looked up by display name, as JSON" $ do
        let cargs = processArgs (prepare Monitoring.monitoringCommand (Monitoring.ChannelsList channel))
            pargs = processArgs (prepare Monitoring.monitoringCommand (Monitoring.PoliciesList policy))
        assertBool (show cargs) (["--filter", "display_name=\"ops mail\"", "--format", "json"] `isSubsequenceOf` cargs)
        assertBool (show pargs) (["--filter", "display_name=\"svc: 5xx ratio\"", "--format", "json"] `isSubsequenceOf` pargs)
    , testCase "channel lookup: failed, absent, present matching, present with another address" $ do
        assertEqual "" (Monitoring.LookupFailed "exit 1: boom") (Monitoring.lookupChannel channel (ExitFailure 1) "" "boom\n")
        assertEqual "" Monitoring.Absent (Monitoring.lookupChannel channel ExitSuccess "[]" "")
        assertEqual
            ""
            (Monitoring.Present (Monitoring.FoundChannel "projects/p/notificationChannels/1" True))
            (Monitoring.lookupChannel channel ExitSuccess (channelJson "ops@example.org") "")
        assertEqual
            ""
            (Monitoring.Present (Monitoring.FoundChannel "projects/p/notificationChannels/1" False))
            (Monitoring.lookupChannel channel ExitSuccess (channelJson "other@example.org") "")
        -- and as a check: only the matching one is satisfied
        assertEqual "" Success (Monitoring.interpretChannelList channel (ExitSuccess, channelJson "ops@example.org", ""))
        assertBool "" (isFailure (Monitoring.interpretChannelList channel (ExitSuccess, channelJson "other@example.org", "")))
        assertBool "" (isFailure (Monitoring.interpretChannelList channel (ExitSuccess, "[]", "")))
        assertBool "" (isFailure (Monitoring.interpretChannelList channel (ExitFailure 1, "", "")))
    , testCase "the 5xx condition is a ratio: numerator on the 5xx class, denominator on every request, same service" $ do
        let v = Monitoring.renderCondition target (Monitoring.ServerErrorRatio 0.05 300)
            threshold = fieldAt ["conditionThreshold"] v
            filt = textAt ["conditionThreshold", "filter"] v
            denom = textAt ["conditionThreshold", "denominatorFilter"] v
        assertBool (show filt) (maybe False ("metric.labels.response_code_class=\"5xx\"" `Text.isInfixOf`) filt)
        assertBool (show filt) (maybe False ("resource.labels.service_name=\"svc\"" `Text.isInfixOf`) filt)
        assertBool (show filt) (maybe False ("resource.labels.location=\"europe-west1\"" `Text.isInfixOf`) filt)
        assertBool (show denom) (maybe False (\d -> "request_count" `Text.isInfixOf` d && not ("5xx" `Text.isInfixOf` d)) denom)
        assertEqual "" (Just "300s") (textAt ["conditionThreshold", "duration"] v)
        assertBool (show threshold) (threshold /= Nothing)
    , testCase "latency and memory read the 99th percentile; instance count sums active instances" $ do
        let lat = Monitoring.renderCondition target (Monitoring.RequestLatencyP99 2000 300)
            mem = Monitoring.renderCondition target (Monitoring.MemoryUtilization 0.9 300)
            cnt = Monitoring.renderCondition target (Monitoring.InstanceCount 3 300)
        assertEqual "" (Just "ALIGN_PERCENTILE_99") (textAt ["conditionThreshold", "aggregations", "0", "perSeriesAligner"] lat)
        assertBool "" (maybe False ("request_latencies" `Text.isInfixOf`) (textAt ["conditionThreshold", "filter"] lat))
        assertEqual "" (Just "ALIGN_PERCENTILE_99") (textAt ["conditionThreshold", "aggregations", "0", "perSeriesAligner"] mem)
        assertBool "" (maybe False ("memory/utilizations" `Text.isInfixOf`) (textAt ["conditionThreshold", "filter"] mem))
        assertEqual "" (Just "REDUCE_SUM") (textAt ["conditionThreshold", "aggregations", "0", "crossSeriesReducer"] cnt)
        assertBool "" (maybe False ("metric.labels.state=\"active\"" `Text.isInfixOf`) (textAt ["conditionThreshold", "filter"] cnt))
    , testCase "the rendered policy names its channels and carries the fingerprint as a user label" $ do
        let names = ["projects/p/notificationChannels/1"]
            v = Monitoring.renderPolicy policy names
        assertEqual "" (Just "svc: 5xx ratio") (textAt ["displayName"] v)
        assertEqual "" (Just "OR") (textAt ["combiner"] v)
        assertEqual "" (Just "projects/p/notificationChannels/1") (textAt ["notificationChannels", "0"] v)
        assertEqual "" (Just (Monitoring.policyFingerprint policy names)) (textAt ["userLabels", Monitoring.fingerprintLabel] v)
        -- and it is what --policy carries, inline
        let args = processArgs (prepare Monitoring.monitoringCommand (Monitoring.PoliciesCreate policy names))
        assertBool (show args) (["alpha", "monitoring", "policies", "create", "--policy"] `isSubsequenceOf` args)
        assertBool (show args) (Text.unpack (Text.decodeUtf8 (LByteString.toStrict (encode v))) `elem` args)
    , testCase "the fingerprint is label-safe, and moves with a threshold or a channel id" $ do
        let names = ["projects/p/notificationChannels/1"]
            fp = Monitoring.policyFingerprint policy names
        assertEqual (show fp) 16 (Text.length fp)
        assertBool (show fp) (Text.all (\c -> isAsciiLower c || isDigit c) fp)
        assertBool "same declaration, same fingerprint" (fp == Monitoring.policyFingerprint policy names)
        assertBool "threshold" (fp /= Monitoring.policyFingerprint policy{Monitoring.apConditions = [Monitoring.ServerErrorRatio 0.1 300]} names)
        assertBool "channel id" (fp /= Monitoring.policyFingerprint policy ["projects/p/notificationChannels/2"])
        assertBool "documentation" (fp /= Monitoring.policyFingerprint policy{Monitoring.apDocumentation = "other"} names)
    , testCase "policy lookup: a matching fingerprint is satisfied, anything else is not" $ do
        let names = ["projects/p/notificationChannels/1"]
            fp = Monitoring.policyFingerprint policy names
        assertEqual "" Success (Monitoring.interpretPolicyList policy names (ExitSuccess, policyJson (Just fp), ""))
        assertBool "edited or older" (isFailure (Monitoring.interpretPolicyList policy names (ExitSuccess, policyJson (Just "0000000000000000"), "")))
        assertBool "no label" (isFailure (Monitoring.interpretPolicyList policy names (ExitSuccess, policyJson Nothing, "")))
        assertBool "absent" (isFailure (Monitoring.interpretPolicyList policy names (ExitSuccess, "[]", "")))
        assertBool "unreachable" (isFailure (Monitoring.interpretPolicyList policy names (ExitFailure 1, "", "")))
        assertEqual
            ""
            (Monitoring.Present (Monitoring.FoundPolicy "projects/p/alertPolicies/9" (Just fp)))
            (Monitoring.lookupPolicy policy ExitSuccess (policyJson (Just fp)) "")
    , testCase "an update names the policy found, a delete the resource, both under the project" $ do
        let up = processArgs (prepare Monitoring.monitoringCommand (Monitoring.PoliciesUpdate "projects/p/alertPolicies/9" policy []))
            del = processArgs (prepare Monitoring.monitoringCommand (Monitoring.PoliciesDelete (Core.Project "p") "projects/p/alertPolicies/9"))
            cdel = processArgs (prepare Monitoring.monitoringCommand (Monitoring.ChannelsDelete (Core.Project "p") "projects/p/notificationChannels/1"))
        assertBool (show up) (["policies", "update", "projects/p/alertPolicies/9", "--policy"] `isSubsequenceOf` up)
        assertBool (show del) (["policies", "delete", "projects/p/alertPolicies/9", "--quiet", "--project", "p"] `isSubsequenceOf` del)
        assertBool (show cdel) (["channels", "delete", "projects/p/notificationChannels/1", "--quiet"] `isSubsequenceOf` cdel)
    ]
  where
    channel = Monitoring.NotificationChannel (Core.Project "p") "ops mail" (Monitoring.Email "ops@example.org")
    target = Monitoring.CloudRunTarget (Core.Project "p") (Core.Region "europe-west1") "svc"
    policy =
        Monitoring.AlertPolicy
            { Monitoring.apProject = Core.Project "p"
            , Monitoring.apDisplayName = "svc: 5xx ratio"
            , Monitoring.apTarget = target
            , Monitoring.apConditions = [Monitoring.ServerErrorRatio 0.05 300]
            , Monitoring.apChannels = [channel]
            , Monitoring.apDocumentation = "doc"
            }
    channelJson address =
        "[{\"name\": \"projects/p/notificationChannels/1\", \"type\": \"email\", \"displayName\": \"ops mail\", \"labels\": {\"email_address\": \"" <> Text.encodeUtf8 address <> "\"}, \"enabled\": true}]"
    policyJson mfp =
        "[{\"name\": \"projects/p/alertPolicies/9\", \"displayName\": \"svc: 5xx ratio\", \"combiner\": \"OR\""
            <> maybe "" (\fp -> ", \"userLabels\": {\"salmon-fingerprint\": \"" <> Text.encodeUtf8 fp <> "\"}") mfp
            <> "}]"

-- | A field of a JSON value by path; an array index is spelled as a number.
fieldAt :: [Text.Text] -> Value -> Maybe Value
fieldAt [] v = Just v
fieldAt (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= fieldAt ks
fieldAt (k : ks) (Array xs) = case reads (Text.unpack k) of
    [(i, "")] | i >= 0, i < length xs -> fieldAt ks (toList xs !! i)
    _ -> Nothing
fieldAt _ _ = Nothing

textAt :: [Text.Text] -> Value -> Maybe Text.Text
textAt ks v = case fieldAt ks v of
    Just (String t) -> Just t
    _ -> Nothing

cloudRunAlertsTests :: [TestTree]
cloudRunAlertsTests =
    [ testCase "three policies without a maximum, four with; one channel shared by all" $ do
        let without = CloudRunAlerts.standardPolicies cfg{CloudRunAlerts.cra_maxInstances = Nothing}
            with = CloudRunAlerts.standardPolicies cfg
        assertEqual "" 3 (length without)
        assertEqual "" 4 (length with)
        assertEqual "" 1 (length (nub (concatMap (.apChannels) with)))
        assertBool "" (any (\p -> p.apConditions == [Monitoring.InstanceCount 2 300]) with)
    , testCase "display names are distinct per service, so two services' alerts are distinct resources" $ do
        let a = map (.apDisplayName) (CloudRunAlerts.standardPolicies cfg)
            b = map (.apDisplayName) (CloudRunAlerts.standardPolicies cfg{CloudRunAlerts.cra_service = "other"})
        assertEqual "" 4 (length (nub a))
        assertBool (show (a, b)) (null (filter (`elem` b) a))
        assertBool "" (all ("svc: " `Text.isPrefixOf`) a)
    , testCase "the defaults are the documented ones" $ do
        let t = CloudRunAlerts.defaultAlertThresholds
        assertEqual "" 0.05 t.at_errorRatio
        assertEqual "" 2000 t.at_latencyP99Ms
        assertEqual "" 0.9 t.at_memoryUtilization
        assertEqual "" 300 t.at_duration
    ]
  where
    cfg =
        CloudRunAlerts.CloudRunAlertsConfig
            { CloudRunAlerts.cra_project = Core.Project "p"
            , CloudRunAlerts.cra_region = Core.Region "europe-west1"
            , CloudRunAlerts.cra_service = "svc"
            , CloudRunAlerts.cra_email = "ops@example.org"
            , CloudRunAlerts.cra_channelName = "ops mail"
            , CloudRunAlerts.cra_maxInstances = Just 2
            , CloudRunAlerts.cra_thresholds = CloudRunAlerts.defaultAlertThresholds
            }

-------------------------------------------------------------------------------

caTrustScriptTests :: [TestTree]
caTrustScriptTests =
    [ testCase "the fetch is inside a retry loop, never a bare command under set -e" $ do
        assertEqual "errexit is on, which is what makes a bare fetch fatal" (Just "set -eux") (lookup 2 numbered)
        assertBool "the curl is the condition of an `if`, so a 404 does not abort the script" $
            any (\l -> "if curl -fsS" `Text.isInfixOf` l && "Metadata-Flavor: Google" `Text.isInfixOf` l) loopLines
        assertBool "the loop retries with a pause" $
            any ("sleep 2" `Text.isInfixOf`) loopLines
        assertBool "it reads the attribute installMetadataCaKey publishes" $
            any ("/computeMetadata/v1/project/attributes/ssh-ca" `Text.isInfixOf`) loopLines
    , testCase "running out of attempts still fails, on an empty key file" $
        assertEqual
            "an empty TrustedUserCAKeys file locks everybody out, so sshd is not touched without a key"
            ["done", "test -s /etc/ssh/salmon_ca.pub"]
            (take 2 (dropWhile (/= "done") scriptLines))
    , testCase "sshd is told about the key only after it is there, and restarted last" $ do
        let at needle = [n | (n, l) <- numbered, needle `Text.isInfixOf` l]
        assertBool "" (at "test -s" < at "TrustedUserCAKeys /etc/ssh/salmon_ca.pub' >>")
        assertEqual "" (Just "systemctl restart ssh || systemctl restart sshd") (lookup (length scriptLines) numbered)
    , testCase "the sshd_config line is appended only if missing (a startup script runs every boot)" $
        assertBool "" (any ("grep -qxF 'TrustedUserCAKeys /etc/ssh/salmon_ca.pub' /etc/ssh/sshd_config" `Text.isPrefixOf`) scriptLines)
    , testCase "the login user is created, given passwordless sudo, and named nowhere else" $ do
        assertBool "" ("id -u deployer >/dev/null 2>&1 || useradd -m -s /bin/bash deployer" `elem` scriptLines)
        assertBool "" ("printf '%s ALL=(ALL) NOPASSWD:ALL\\n' deployer > /etc/sudoers.d/deployer" `elem` scriptLines)
        assertBool "" ("chmod 440 /etc/sudoers.d/deployer" `elem` scriptLines)
        assertEqual
            "a different user changes those three lines only"
            3
            (length (filter id (zipWith (/=) scriptLines (Text.lines (VmProvision.caTrustStartupScript "other")))))
    ]
  where
    scriptLines = Text.lines (VmProvision.caTrustStartupScript "deployer")
    numbered = zip [1 :: Int ..] scriptLines
    loopLines =
        takeWhile (/= "done") (dropWhile (not . ("for attempt in" `Text.isPrefixOf`)) scriptLines)

-------------------------------------------------------------------------------
-- Cloud DNS

{- | The shape of @gcloud dns managed-zones describe --format json@, written
from the Cloud DNS API's @ManagedZone@ resource rather than captured from a
live project.
-}
zoneDescribeJson :: Text.Text
zoneDescribeJson =
    Text.unlines
        [ "{"
        , "  \"cloudLoggingConfig\": {\"kind\": \"dns#managedZoneCloudLoggingConfig\"},"
        , "  \"creationTime\": \"2026-10-02T15:04:05.678Z\","
        , "  \"description\": \"a zone\","
        , "  \"dnsName\": \"example.org.\","
        , "  \"id\": \"1234567890123456789\","
        , "  \"kind\": \"dns#managedZone\","
        , "  \"name\": \"example-zone\","
        , "  \"nameServers\": ["
        , "    \"ns-cloud-c1.googledomains.com.\","
        , "    \"ns-cloud-c2.googledomains.com.\","
        , "    \"ns-cloud-c3.googledomains.com.\","
        , "    \"ns-cloud-c4.googledomains.com.\""
        , "  ],"
        , "  \"visibility\": \"public\""
        , "}"
        ]

cloudDnsTests :: [TestTree]
cloudDnsTests =
    [ testCase "create names the zone, its DNS name with the trailing dot, a description and public visibility" $
        assertEqual
            ""
            ["dns", "managed-zones", "create", "example-zone", "--dns-name", "example.org.", "--description", "a zone", "--visibility", "public", "--project", "my-project"]
            (processArgs (prepare CloudDns.cloudDnsCommand (CloudDns.ZonesCreate zone)))
    , testCase "describe asks for JSON" $
        assertEqual
            ""
            ["dns", "managed-zones", "describe", "example-zone", "--format", "json", "--project", "my-project"]
            (processArgs (prepare CloudDns.cloudDnsCommand (CloudDns.ZonesDescribe zone)))
    , testCase "delete is quiet" $
        assertEqual
            ""
            ["dns", "managed-zones", "delete", "example-zone", "--quiet", "--project", "my-project"]
            (processArgs (prepare CloudDns.cloudDnsCommand (CloudDns.ZonesDelete zone)))
    , testCase "fqdn lower-cases and ends in exactly one dot" $
        assertEqual "" ["example.org.", "example.org.", "example.org."] (map CloudDns.fqdn ["example.org", "Example.ORG.", " example.org..\n"])
    , testCase "the assigned name servers are read in order" $
        assertEqual
            ""
            (Right ["ns-cloud-c1.googledomains.com.", "ns-cloud-c2.googledomains.com.", "ns-cloud-c3.googledomains.com.", "ns-cloud-c4.googledomains.com."])
            (CloudDns.describedNameServers <$> CloudDns.parseZoneDescribe described)
    , testCase "a description with no nameServers parses to none" $
        assertEqual
            ""
            (Right (CloudDns.ZoneDescription "example.org." []))
            (CloudDns.parseZoneDescribe "{\"dnsName\": \"example.org.\"}")
    , testCase "a zone described for the declared DNS name is satisfied" $
        assertEqual "" Success (CloudDns.interpretZoneDescribe zone ExitSuccess described)
    , testCase "the declared name's dot and case do not matter" $
        assertEqual "" Success (CloudDns.interpretZoneDescribe (zone {CloudDns.zoneDnsName = "Example.org."}) ExitSuccess described)
    , testCase "describe failing means the zone is absent" $
        assertBool "" (isFailure (CloudDns.interpretZoneDescribe zone (ExitFailure 1) ""))
    , testCase "a zone of that name serving another domain is a failure naming both" $
        case CloudDns.interpretZoneDescribe (zone {CloudDns.zoneDnsName = "example.net"}) ExitSuccess described of
            Failure why -> do
                assertBool (Text.unpack why) ("example.org." `Text.isInfixOf` why)
                assertBool (Text.unpack why) ("example.net." `Text.isInfixOf` why)
            other -> assertBool ("expected a Failure, got " <> show other) False
    , testCase "output that is not a zone description cannot be judged" $
        assertEqual "" Unknown (CloudDns.interpretZoneDescribe zone ExitSuccess "not json")
    ]
  where
    zone = CloudDns.ManagedZone "example-zone" (Core.Project "my-project") "example.org" "a zone"
    described = Text.encodeUtf8 zoneDescribeJson
