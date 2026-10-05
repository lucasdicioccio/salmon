{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for the pure @check@-verdict interpreters under
"Salmon.Builtin.Nodes.Gcp" -- each node shells out to @gcloud@/@ssh@ to get
its raw exit code and output, then hands that to a pure function that draws
the 'CheckResult'. Splitting the decision out (the same shape as
"Salmon.Builtin.Nodes.Systemd"'s @interpretShow@, see @Test.SystemdSpec@) is
what makes it testable without a real GCP project.
-}
module Test.GcpSpec (tests) where

import Control.Exception (try)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isAsciiLower, isDigit)
import Data.List (isInfixOf, isPrefixOf, isSubsequenceOf, nub, sort, tails)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.IO.Exception (ExitCode (..))
import System.Directory (createDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import System.Process.ListLike (CmdSpec (..), CreateProcess, cmdspec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension (Extension (..), Op, evalDeps, ignoreTrack, nodeps, op)
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
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (silent)
import qualified SreBox.Gcp.CloudRunAlerts as CloudRunAlerts
import qualified SreBox.Gcp.VmProvision as VmProvision

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Gcp"
        [ testGroup "Core.interpretAdc" adcTests
        , testGroup "Core.declaredAccount" accountTests
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
        , testGroup "CloudDns record sets" cloudDnsRecordTests
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

accountTests :: [TestTree]
accountTests =
    [ testCase "the active account is read from the core/account property" $
        assertEqual "" ["config", "get-value", "account"] Core.activeAccountArgs
    , testCase "the declared account being active is satisfied" $
        assertEqual "" Success (verdict ExitSuccess "deployer@example.com\n")
    , testCase "addresses compare without regard to case" $
        assertEqual "" Success (verdict ExitSuccess "Deployer@Example.com\n")
    , testCase "another active account is refused, naming both" $
        case verdict ExitSuccess "someone-else@example.com\n" of
            Failure why -> do
                assertBool "names the active one" ("someone-else@example.com" `Text.isInfixOf` why)
                assertBool "names the declared one" ("deployer@example.com" `Text.isInfixOf` why)
            other -> assertBool ("expected Failure, got " <> show other) False
    , testCase "an account that merely contains the declared one is refused" $
        assertBool "" (isFailure (verdict ExitSuccess "not-deployer@example.com\n"))
    , testCase "no active account is refused (empty stdout, or the old (unset) on stdout)" $ do
        assertBool "empty" (isFailure (verdict ExitSuccess ""))
        assertBool "unset" (isFailure (verdict ExitSuccess "(unset)\n"))
    , testCase "gcloud failing is a Failure, never a pass" $
        assertBool "" (isFailure (verdict (ExitFailure 1) "deployer@example.com\n"))
    , testCase "--account is appended to an argument list" $
        assertEqual
            ""
            ["auth", "print-access-token", "--account", "deployer@example.com"]
            (Core.withAccount declared ["auth", "print-access-token"])
    ]
  where
    declared = Core.Account "deployer@example.com"
    verdict = Core.interpretAccount declared

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
    , testCase "the balancer's address is read off its forwarding rule" $
        assertEqual
            ""
            ["compute", "forwarding-rules", "describe", "x-fw", "--format", "value(IPAddress)", "--region", "europe-west1", "--project", "my-project"]
            (processArgs (prepare LoadBalancing.loadBalancingCommand (LoadBalancing.LbAddressDescribe (alb {LoadBalancing.albName = "x", LoadBalancing.albProject = Core.Project "my-project", LoadBalancing.albRegion = Core.Region "europe-west1"}))))
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
    , testCase "a plain balancer renders none of the HTTPS, rule or timeout steps" $ do
        mapM_
            (\w -> assertBool (w <> "\n" <> script) (not (w `isInfixOf` script)))
            ["target-https-proxies", "certificate-manager", "addresses", "--address", "url-maps import", "--timeout", "ssl-certificates"]
        assertBool script ("gcloud compute url-maps create 'web-url-map' --project=\"$PROJECT\" --region=\"$REGION\" --default-service='web-backend'" `isInfixOf` script)
    , testCase "a managed certificate is an authorization per domain, a certificate over them, an HTTPS proxy and a :443 rule" $ do
        let sc = createScript full
        mapM_
            (\w -> assertBool (w <> "\n" <> sc) (w `isInfixOf` sc))
            [ "gcloud certificate-manager dns-authorizations create 'web-cert-app-example-org' --project=\"$PROJECT\" --location=\"$REGION\" --domain='app.example.org' --type=PER_PROJECT_RECORD"
            , "gcloud certificate-manager dns-authorizations create 'web-cert-api-example-org'"
            , "gcloud certificate-manager certificates create 'web-cert' --project=\"$PROJECT\" --location=\"$REGION\" --domains='app.example.org,api.example.org' --dns-authorizations='web-cert-app-example-org,web-cert-api-example-org'"
            , "gcloud compute target-https-proxies create 'web-https-proxy' --project=\"$PROJECT\" --region=\"$REGION\" --url-map='web-url-map' --url-map-region=\"$REGION\" --certificate-manager-certificates='web-cert'"
            , "--target-https-proxy='web-https-proxy' --target-https-proxy-region=\"$REGION\" --ports=443"
            , "gcloud compute addresses create 'web-ip' --project=\"$PROJECT\" --region=\"$REGION\""
            ]
    , testCase "with HTTPS both forwarding rules sit on the one reserved address" $ do
        let rules = [l | l <- lines (createScript full), "forwarding-rules create" `isInfixOf` l]
        assertEqual "" 2 (length rules)
        mapM_ (\l -> assertBool l ("--address='web-ip' --address-region=\"$REGION\"" `isInfixOf` l)) rules
    , testCase "the certificate exists before the proxy that names it, the address before the rules" $ do
        let sc = createScript full
        assertBool sc (["certificates create", "target-https-proxies create", "forwarding-rules create"] `inOrder` sc)
        assertBool sc (["addresses create", "forwarding-rules create"] `inOrder` sc)
        assertBool sc (["backend-services create 'web-slow-backend'", "url-maps import"] `inOrder` sc)
    , testCase "a certificate somebody else made is named by --ssl-certificates, and not created or deleted" $ do
        let own = full{LoadBalancing.albCertificates = [LoadBalancing.ComputeCertificate "own"]}
        let sc = createScript own
        assertBool sc ("--ssl-certificates='own' --ssl-certificates-region=\"$REGION\"" `isInfixOf` sc)
        assertBool sc (not ("certificate-manager" `isInfixOf` sc))
        assertBool "" (not ("ssl-certificates delete" `isInfixOf` deleteScript own))
        assertBool "" ("need 'ssl-certificates own'" `isInfixOf` Text.unpack (LoadBalancing.renderLbCheckScript own))
    , testCase "host rules make the URL map an import of the whole map, every run" $ do
        let sc = createScript full
        assertBool sc ("| gcloud compute url-maps import 'web-url-map' --project=\"$PROJECT\" --region=\"$REGION\" --quiet" `isInfixOf` sc)
        assertBool sc (not ("url-maps create" `isInfixOf` sc))
        assertBool sc (not ("exists gcloud compute url-maps" `isInfixOf` sc))
    , testCase "the URL map sends each host to its service and each path rule to its own" $
        assertEqual
            ""
            ( object
                [ "name" .= ("web-url-map" :: Text.Text)
                , "defaultService" .= svcUrl "web-backend"
                , "hostRules"
                    .= [ object ["hosts" .= (["app.example.org", "www.example.org"] :: [Text.Text]), "pathMatcher" .= ("m0" :: Text.Text)]
                       , object ["hosts" .= (["api.example.org"] :: [Text.Text]), "pathMatcher" .= ("m1" :: Text.Text)]
                       ]
                , "pathMatchers"
                    .= [ object
                            [ "name" .= ("m0" :: Text.Text)
                            , "defaultService" .= svcUrl "web-backend"
                            , "pathRules" .= [object ["paths" .= (["/events/*", "/poll"] :: [Text.Text]), "service" .= svcUrl "web-slow-backend"]]
                            ]
                       , object ["name" .= ("m1" :: Text.Text), "defaultService" .= svcUrl "web-api-backend"]
                       ]
                ]
            )
            (LoadBalancing.renderUrlMap full)
    , testCase "a named backend service has its own resource, NEG and backends" $ do
        let sc = createScript full
        mapM_
            (\w -> assertBool (w <> "\n" <> sc) (w `isInfixOf` sc))
            [ "gcloud compute backend-services create 'web-slow-backend'"
            , "gcloud compute backend-services add-backend 'web-slow-backend' --project=\"$PROJECT\" --region=\"$REGION\" --instance-group='ig-slow' --instance-group-zone='europe-west1-c'"
            , "gcloud compute network-endpoint-groups create 'web-api-neg'"
            , "gcloud compute backend-services add-backend 'web-api-backend' --project=\"$PROJECT\" --region=\"$REGION\" --network-endpoint-group='web-api-neg'"
            ]
    , testCase "a health check shared by two services is created once" $ do
        let creates = [l | l <- lines (createScript full), "health-checks create" `isInfixOf` l]
        assertEqual (unlines creates) 1 (length creates)
    , testCase "a timeout is set on every run, not only at creation" $ do
        let sc = createScript full
        assertBool sc ("\ngcloud compute backend-services update 'web-slow-backend' --project=\"$PROJECT\" --region=\"$REGION\" --timeout=3600\n" `isInfixOf` sc)
        assertBool sc ("\ngcloud compute backend-services update 'web-backend' --project=\"$PROJECT\" --region=\"$REGION\" --timeout=60\n" `isInfixOf` sc)
        assertBool sc (not ("update 'web-api-backend'" `isInfixOf` sc))
    , testCase "the check asks after the HTTPS pieces, the timeouts, the hosts and the certificate's state" $ do
        let sc = Text.unpack (LoadBalancing.renderLbCheckScript full)
        mapM_
            (\w -> assertBool (w <> "\n" <> sc) (w `isInfixOf` sc))
            [ "target-https-proxies describe 'web-https-proxy'"
            , "forwarding-rules describe 'web-https-fw'"
            , "addresses describe 'web-ip'"
            , "certificate-manager certificates describe 'web-cert'"
            , "certificate-manager dns-authorizations describe 'web-cert-api-example-org'"
            , "value(managed.state)"
            , "value(timeoutSec)"
            , "MISSING timeout 3600s on web-slow-backend"
            , "MISSING host-rule api.example.org"
            , "get-health 'web-slow-backend'"
            , "backend-services describe 'web-api-backend'"
            ]
    , testCase "teardown takes dependants first: rules, proxies, address, certificate, authorizations, map, backends, services, health checks" $ do
        let sc = deleteScript full
        assertBool
            sc
            ( [ "forwarding-rules delete 'web-https-fw'"
              , "forwarding-rules delete 'web-fw'"
              , "target-https-proxies delete 'web-https-proxy'"
              , "target-http-proxies delete 'web-proxy'"
              , "addresses delete 'web-ip'"
              , "certificate-manager certificates delete 'web-cert'"
              , "certificate-manager dns-authorizations delete 'web-cert-app-example-org'"
              , "url-maps delete 'web-url-map'"
              , -- a NEG still attached cannot be deleted
                "backend-services remove-backend 'web-api-backend'"
              , "network-endpoint-groups delete 'web-api-neg'"
              , "backend-services delete 'web-api-backend'"
              , "backend-services delete 'web-slow-backend'"
              , "backend-services remove-backend 'web-backend'"
              , "backend-services delete 'web-backend'"
              , "health-checks delete 'hc'"
              ]
                `inOrder` sc
            )
    , testCase "with HTTPS the balancer's address is read off the HTTPS rule" $
        assertBool "" ("web-https-fw" `elem` processArgs (prepare LoadBalancing.loadBalancingCommand (LoadBalancing.LbAddressDescribe full)))
    , testCase "an authorization's record is asked for by location, as three fields" $
        assertEqual
            ""
            ["certificate-manager", "dns-authorizations", "describe", "web-cert-app-example-org", "--format", "value(dnsResourceRecord.name,dnsResourceRecord.type,dnsResourceRecord.data)", "--location", "europe-west1", "--project", "p"]
            (processArgs (prepare LoadBalancing.loadBalancingCommand (LoadBalancing.LbDnsAuthorizationDescribe full "web-cert-app-example-org")))
    , testCase "the authorizations are named from the certificate and the domain" $
        assertEqual
            ""
            [("app.example.org", "web-cert-app-example-org"), ("api.example.org", "web-cert-api-example-org")]
            (LoadBalancing.dnsAuthorizations full)
    , testCase "an authorization's record is three tab-separated fields" $
        assertEqual
            ""
            (Just (LoadBalancing.DnsAuthorizationRecord "_acme-challenge_abc.app.example.org." "CNAME" "0123.4.europe-west1.authorize.certificatemanager.goog."))
            (LoadBalancing.parseDnsAuthorizationRecord "_acme-challenge_abc.app.example.org.\tCNAME\t0123.4.europe-west1.authorize.certificatemanager.goog.\n")
    , testCase "an authorization with no record yet reads as nothing" $ do
        assertEqual "" Nothing (LoadBalancing.parseDnsAuthorizationRecord "")
        assertEqual "" Nothing (LoadBalancing.parseDnsAuthorizationRecord "\t\t\n")
        assertEqual "" Nothing (LoadBalancing.parseDnsAuthorizationRecord "a\tCNAME\n")
    , testCase "a certificate still provisioning is Unknown, an active one Success, a failed one a Failure naming it" $ do
        assertEqual "" Unknown (LoadBalancing.interpretLbCheck ExitSuccess "HEALTH backend HEALTHY\nCERT web-cert PROVISIONING\n")
        assertEqual "" Success (LoadBalancing.interpretLbCheck ExitSuccess "HEALTH backend HEALTHY\nCERT web-cert ACTIVE\n")
        case LoadBalancing.interpretLbCheck ExitSuccess "CERT web-cert FAILED\n" of
            Failure t -> assertBool (Text.unpack t) ("web-cert" `isInfixOf` Text.unpack t)
            other -> assertBool (show other) False
    , testCase "a wrong timeout or an unrouted host is a Failure" $ do
        assertBool "" (isFailure (LoadBalancing.interpretLbCheck ExitSuccess "MISSING timeout 3600s on web-slow-backend\n"))
        assertBool "" (isFailure (LoadBalancing.interpretLbCheck ExitSuccess "MISSING host-rule api.example.org\nCERT web-cert ACTIVE\n"))
    , testCase "an unhealthy named service is Unknown like the default one" $
        assertEqual "" Unknown (LoadBalancing.interpretLbCheck ExitSuccess "HEALTH backend HEALTHY\nHEALTH slow UNHEALTHY\n")
    , testCase "a sound declaration has no problems" $ do
        assertEqual "" [] (LoadBalancing.albProblems alb)
        assertEqual "" [] (LoadBalancing.albProblems full)
    , testCase "every problem of a declaration is named, not the first" $ do
        let bad =
                full
                    { LoadBalancing.albServices = full.albServices <> [LoadBalancing.BackendService "slow" [] Nothing (Just 0)]
                    , LoadBalancing.albHostRules =
                        [ LoadBalancing.HostRule ["app.example.org"] (LoadBalancing.NamedService "nope") [LoadBalancing.PathRule [] LoadBalancing.DefaultService]
                        , LoadBalancing.HostRule ["app.example.org"] LoadBalancing.DefaultService []
                        , LoadBalancing.HostRule [] LoadBalancing.DefaultService []
                        ]
                    , LoadBalancing.albCertificates = [LoadBalancing.ManagedCertificate "c" [], LoadBalancing.ComputeCertificate "own"]
                    }
        let problems = unlines (map Text.unpack (LoadBalancing.albProblems bad))
        mapM_
            (\w -> assertBool (w <> "\n" <> problems) (w `isInfixOf` problems))
            ["declared twice: slow", "undeclared backend service: nope", "names no host", "names no path", "two rules: app.example.org", "cannot share a proxy", "c names no domain", "must be positive: 0"]
    , testCase "a domain-set certificate is named after its set: order and case do not move it, one more name does" $ do
        let name ds = LoadBalancing.certificateResource (LoadBalancing.DomainSetCertificate "web-cert" ds)
        assertEqual "" (name ["app.example.org", "api.example.org"]) (name ["API.example.org", "app.example.org", "app.example.org"])
        assertBool "" (name ["app.example.org", "api.example.org"] /= name ["app.example.org", "api.example.org", "www.example.org"])
        assertEqual "" ("web-cert-" <> LoadBalancing.domainSetTag ["app.example.org"]) (name ["app.example.org"])
        assertEqual "eight hex digits" 8 (Text.length (LoadBalancing.domainSetTag ["app.example.org"]))
        assertEqual "a managed certificate keeps its declared name" "web-cert" (LoadBalancing.certificateResource (LoadBalancing.ManagedCertificate "web-cert" ["app.example.org"]))
    , testCase "a domain-set certificate's authorizations are named after its base, so a changed set reuses them" $ do
        assertEqual "" (LoadBalancing.dnsAuthorizations full) (LoadBalancing.dnsAuthorizations rotating)
        assertEqual
            ""
            (LoadBalancing.dnsAuthorizations rotating <> [("www.example.org", "web-cert-www-example-org")])
            (LoadBalancing.dnsAuthorizations rotated)
    , testCase "a changed domain set is another certificate node, the same proxy node and one cleanup node per base" $ do
        let ids a = map LoadBalancing.partId (LoadBalancing.lbParts a)
        assertBool "" (LoadBalancing.CertificatePart certV1 `elem` ids rotating)
        assertBool "" (LoadBalancing.CertificatePart certV2 `elem` ids rotated)
        assertBool "" (LoadBalancing.CertificatePart certV1 `notElem` ids rotated)
        assertBool "" (LoadBalancing.SupersededCertificatesPart "web-cert" `elem` ids rotated)
        assertBool "none for a certificate under a fixed name" (LoadBalancing.SupersededCertificatesPart "web-cert" `notElem` ids full)
        assertEqual "" [LoadBalancing.HttpsProxyPart, LoadBalancing.CertificatePart certV2] (depsOf rotated (LoadBalancing.SupersededCertificatesPart "web-cert"))
        assertBool "" (LoadBalancing.CertificatePart certV2 `elem` depsOf rotated LoadBalancing.HttpsProxyPart)
    , testCase "a problem line is a Failure naming it" $
        case LoadBalancing.interpretLbCheck ExitSuccess "CERT web-cert ACTIVE\nPROBLEM certificate web-cert expired 2026-01-01T00:00:00Z\n" of
            Failure t -> assertBool (Text.unpack t) ("certificate web-cert expired" `isInfixOf` Text.unpack t)
            other -> assertBool (show other) False
    , testCase "one name for two certificates is a problem" $ do
        let bad = full{LoadBalancing.albCertificates = [LoadBalancing.ManagedCertificate "web-cert" ["a.example.org"], LoadBalancing.DomainSetCertificate "web-cert" ["b.example.org"]]}
        let problems = unlines (map Text.unpack (LoadBalancing.albProblems bad))
        assertBool problems ("certificate declared twice: web-cert" `isInfixOf` problems)
        assertEqual "" [] (LoadBalancing.albProblems rotated)
    , testCase "the HTTPS proxy's URL map is named at creation only" $ do
        let ups = unlines [Text.unpack l | part <- LoadBalancing.lbParts rotated, part.partId == LoadBalancing.HttpsProxyPart, l <- part.partUp]
        assertEqual ups 1 (length [() | l <- lines ups, "--url-map=" `isInfixOf` l])
        assertBool ups (not (any (\l -> "update" `isInfixOf` l && "--url-map" `isInfixOf` l) (lines ups)))
    , testCase "rendered scripts with a domain-set certificate parse as bash" $ do
        let scripts = [s' | cmd <- [LoadBalancing.LbCreate rotated, LoadBalancing.LbCheck rotated, LoadBalancing.LbDelete rotated], (_ : s' : _) <- [processArgs (prepare LoadBalancing.loadBalancingCommand cmd)]]
        mapM_
            ( \sc -> do
                (code, _, err) <- readProcessWithExitCode "bash" ["-n", "-c", sc] ""
                assertEqual err ExitSuccess code
            )
            scripts
    , testCase "a group's named ports are set once, as the union over the services naming it" $ do
        let sc = createScript shared
        let sets = [l | l <- lines sc, "set-named-ports" `isInfixOf` l]
        assertEqual sc 1 (length sets)
        mapM_ (\w -> assertBool (w <> "\n" <> unlines sets) (any (w `isInfixOf`) sets)) ["web-a-backend:4272", "web-b-backend:4273"]
        -- and each service is created on, and kept on, its own port name
        mapM_
            (\w -> assertBool (w <> "\n" <> sc) (w `isInfixOf` sc))
            [ "backend-services create 'web-a-backend' --project=\"$PROJECT\" --region=\"$REGION\" --protocol=HTTP --port-name='web-a-backend'"
            , "backend-services update 'web-a-backend' --project=\"$PROJECT\" --region=\"$REGION\" --port-name='web-a-backend'"
            , "backend-services update 'web-b-backend' --project=\"$PROJECT\" --region=\"$REGION\" --port-name='web-b-backend'"
            ]
        assertBool sc (["set-named-ports 'ig'", "backend-services update 'web-a-backend'"] `inOrder` sc)
    , testCase "a Cloud Run service is given no port name" $ do
        let sc = createScript full
        assertBool sc (not ("--port-name='web-api-backend'" `isInfixOf` sc))
    , testCase "one port name on two ports of a group is a problem" $ do
        let twice = LoadBalancing.InstanceGroupBackend "ig" (LoadBalancing.InstanceGroupZone "europe-west1-b")
        let bad = alb{LoadBalancing.albBackends = [twice [8080], twice [8081]]}
        let problems = unlines (map Text.unpack (LoadBalancing.albProblems bad))
        assertBool problems ("named port web-backend" `isInfixOf` problems)
        assertEqual "" [] (LoadBalancing.albProblems shared)
    , testCase "httpLoadBalancer is the plain balancer" $
        assertEqual
            ""
            alb{LoadBalancing.albNetwork = Nothing}
            (LoadBalancing.httpLoadBalancer "web" (Core.Project "p") (Core.Region "europe-west1") alb.albBackends alb.albHealthCheck)
    , testCase "rendered scripts with every feature parse as bash" $ do
        let scripts = [s' | cmd <- [LoadBalancing.LbCreate full, LoadBalancing.LbCheck full, LoadBalancing.LbDelete full], (_ : s' : _) <- [processArgs (prepare LoadBalancing.loadBalancingCommand cmd)]]
        mapM_
            ( \sc -> do
                (code, _, err) <- readProcessWithExitCode "bash" ["-n", "-c", sc] ""
                assertEqual err ExitSuccess code
            )
            scripts
    , testGroup "a node per resource" nodeTests
    , testGroup "against a stand-in gcloud" $
        -- Not GCP: a shell script that keeps "resources" as files and answers
        -- describe/create/delete the way these scripts assume gcloud does.
        -- What it shows is the scripts' own logic -- guards, ordering,
        -- quoting, what a second run does -- and nothing about the API.
        [ testCase "up creates everything once, and a second up creates nothing more" $
            withFakeGcloud $ \run mutations -> do
                (code, _, err) <- run (createScript full)
                assertEqual err ExitSuccess code
                first <- mutations
                mapM_
                    (\w -> assertBool (w <> "\n" <> unlines first) (any (w `isInfixOf`) first))
                    ["target-https-proxies create web-https-proxy", "certificates create web-cert", "url-maps import web-url-map", "forwarding-rules create web-https-fw", "backend-services add-backend web-slow-backend"]
                (code2, _, err2) <- run (createScript full)
                assertEqual err2 ExitSuccess code2
                second <- drop (length first) <$> mutations
                -- the "set" verbs run again by design; nothing is created or attached twice
                assertBool (unlines second) (not (any (\l -> " create " `isInfixOf` l || "add-backend" `isInfixOf` l) second))
                assertBool (unlines second) (any ("url-maps import" `isInfixOf`) second)
        , testCase "the check of what up made is Success, and of nothing at all a Failure" $
            withFakeGcloud $ \run _ -> do
                let checkScript = Text.unpack (LoadBalancing.renderLbCheckScript full)
                (code0, out0, _) <- run checkScript
                let before = LoadBalancing.interpretLbCheck code0 (Text.pack out0)
                assertBool (show before) (isFailure before)
                _ <- run (createScript full)
                (code1, out1, err1) <- run checkScript
                assertEqual (out1 <> err1) Success (LoadBalancing.interpretLbCheck code1 (Text.pack out1))
        , testCase "the check notices a host dropped from the map and a timeout changed behind it" $
            withFakeGcloud $ \run _ -> do
                _ <- run (createScript full{LoadBalancing.albHostRules = take 1 full.albHostRules, LoadBalancing.albTimeoutSec = Just 30})
                (code, out, _) <- run (Text.unpack (LoadBalancing.renderLbCheckScript full))
                case LoadBalancing.interpretLbCheck code (Text.pack out) of
                    Failure t -> do
                        assertBool (Text.unpack t) ("host-rule api.example.org" `isInfixOf` Text.unpack t)
                        assertBool (Text.unpack t) ("timeout 60s on web-backend" `isInfixOf` Text.unpack t)
                        assertBool (Text.unpack t) (not ("host-rule app.example.org" `isInfixOf` Text.unpack t))
                    other -> assertBool (show other) False
        , testCase "down removes everything up made and leaves the caller's instance groups" $
            withFakeGcloud $ \run mutations -> do
                _ <- run (createScript full)
                (code, _, err) <- run (deleteScript full)
                assertEqual err ExitSuccess code
                (_, out, _) <- run "ls \"$FAKE_GCLOUD_STATE\""
                assertEqual "" "" out
                made <- mutations
                assertBool (unlines made) (not (any ("instance-groups delete" `isInfixOf`) made))
        , testCase "two services on one instance group each reach their own port" $
            -- The bug this pins: every backend service used the default port
            -- name, http, and every attach reset the group's http to its own
            -- port, so all of them sent to whichever service came last.
            withFakeGcloud $ \run _ -> do
                (code, _, err) <- run (createScript shared)
                assertEqual err ExitSuccess code
                let portOf svc =
                        run
                            ( "n=$(gcloud compute backend-services describe "
                                <> svc
                                <> " --format='value(portName)'); gcloud compute instance-groups get-named-ports ig | awk -v n=\"$n\" '$1==n{print $2}'"
                            )
                (_, a, _) <- portOf "web-a-backend"
                (_, b, _) <- portOf "web-b-backend"
                assertEqual "a" "4272\n" a
                assertEqual "b" "4273\n" b
                (code1, out1, err1) <- run (Text.unpack (LoadBalancing.renderLbCheckScript shared))
                assertEqual (out1 <> err1) Success (LoadBalancing.interpretLbCheck code1 (Text.pack out1))
        , testCase "a group's named ports somebody else set survive, and a second up changes nothing" $
            withFakeGcloud $ \run _ -> do
                _ <- run "gcloud compute instance-groups set-named-ports ig --named-ports=other:9000,web-a-backend:1"
                _ <- run (createScript shared)
                _ <- run (createScript shared)
                (_, out, _) <- run "gcloud compute instance-groups get-named-ports ig | tr '\\t' ':' | sort"
                assertEqual "" ["other:9000", "web-a-backend:4272", "web-b-backend:4273"] (lines out)
        , testCase "the check names a backend service on the wrong port name and a group missing a named port" $
            withFakeGcloud $ \run _ -> do
                _ <- run (createScript shared)
                _ <- run "gcloud compute backend-services update web-a-backend --port-name=http"
                _ <- run "gcloud compute instance-groups set-named-ports ig --named-ports=web-a-backend:4272,web-b-backend:9999"
                (code, out, _) <- run (Text.unpack (LoadBalancing.renderLbCheckScript shared))
                case LoadBalancing.interpretLbCheck code (Text.pack out) of
                    Failure t -> do
                        assertBool (Text.unpack t) ("port-name web-a-backend on web-a-backend" `isInfixOf` Text.unpack t)
                        assertBool (Text.unpack t) ("named-port web-b-backend:4273 on instance-group ig" `isInfixOf` Text.unpack t)
                        assertBool (Text.unpack t) (not ("named-port web-a-backend:4272" `isInfixOf` Text.unpack t))
                    other -> assertBool (show other) False
        , testCase "resource by resource: each up makes its own, each check answers for its own, each down removes its own" $
            withFakeGcloud $ \run _ -> do
                let parts = LoadBalancing.lbParts full
                let verdict part = do
                        (code, out, _) <- run (Text.unpack (LoadBalancing.renderPartCheckScript full part))
                        pure (LoadBalancing.interpretLbCheck code (Text.pack out))
                before <- mapM verdict parts
                assertBool (show before) (all isFailure before)
                mapM_
                    ( \part -> do
                        (code, _, err) <- run (Text.unpack (LoadBalancing.renderPartUpScript full part))
                        assertEqual (show part.partId <> ": " <> err) ExitSuccess code
                        v <- verdict part
                        assertEqual (show part.partId) Success v
                    )
                    parts
                -- one resource removed behind the balancer is one node's Failure
                _ <- run "gcloud compute url-maps delete web-url-map"
                after <- mapM verdict parts
                assertEqual
                    ""
                    [LoadBalancing.UrlMapPart]
                    [part.partId | (part, v) <- zip parts after, v /= Success]
                mapM_ (run . Text.unpack . LoadBalancing.renderPartUpScript full) [p | p <- parts, p.partId == LoadBalancing.UrlMapPart]
                (codeH, outH, _) <- run (Text.unpack (LoadBalancing.renderLbHealthScript full))
                assertEqual outH Success (LoadBalancing.interpretLbCheck codeH (Text.pack outH))
                assertBool outH ("HEALTH slow HEALTHY" `isInfixOf` outH)
                mapM_
                    ( \part -> do
                        (code, _, err) <- run (Text.unpack (LoadBalancing.renderPartDownScript full part))
                        assertEqual (show part.partId <> ": " <> err) ExitSuccess code
                    )
                    (reverse parts)
                (_, out, _) <- run "ls \"$FAKE_GCLOUD_STATE\""
                assertEqual "" "" out
        , testCase "a certificate that is somebody else's and absent fails its own node's up" $
            withFakeGcloud $ \run _ -> do
                let own = full{LoadBalancing.albCertificates = [LoadBalancing.ComputeCertificate "own"]}
                results <- mapM (run . Text.unpack . LoadBalancing.renderPartUpScript own) [p | p <- LoadBalancing.lbParts own, p.partId == LoadBalancing.CertificatePart "own"]
                assertEqual "" 1 (length results)
                mapM_
                    ( \(code, _, err) -> do
                        assertBool err (code /= ExitSuccess)
                        assertBool err ("certificate own does not exist" `isInfixOf` err)
                    )
                    results
        , testCase "a changed domain set: the proxy keeps the old certificate until the new one is ACTIVE, then moves, then the old one is deleted" $
            withFakeGcloud $ \run mutations -> do
                (code, _, err) <- run (createScript rotating)
                assertEqual err ExitSuccess code
                first <- mutations
                -- the new certificate is created and is not issued yet
                (code1, _, err1) <- run ("export FAKE_GCLOUD_NEW_CERT_STATE=PROVISIONING\n" <> createScript rotated)
                assertBool err1 (code1 /= ExitSuccess)
                assertBool err1 (("certificate " <> Text.unpack certV2 <> " is not ACTIVE yet: PROVISIONING") `isInfixOf` err1)
                waiting <- drop (length first) <$> mutations
                assertBool (unlines waiting) (("certificates create " <> Text.unpack certV2) `elem` waiting)
                -- only the name the old set did not cover gets an authorization
                assertEqual (unlines waiting) ["dns-authorizations create web-cert-www-example-org"] (filter ("dns-authorizations" `isInfixOf`) waiting)
                assertBool (unlines waiting) (not (any (\l -> "target-https-proxies" `isInfixOf` l || " delete " `isInfixOf` l) waiting))
                (_, served, _) <- run "gcloud compute target-https-proxies describe web-https-proxy --format='value(sslCertificates)'"
                assertBool served (Text.unpack certV1 `isInfixOf` served)
                -- which the proxy's check reads as waiting, not as something up would fix
                proxyBefore <- partVerdict run rotated LoadBalancing.HttpsProxyPart
                assertEqual "" [Unknown] proxyBefore
                -- issued: the check wants the move, and up makes it, then cleans up
                _ <- run ("echo ACTIVE > \"$FAKE_GCLOUD_STATE\"/certificates." <> Text.unpack certV2 <> ".state")
                proxyReady <- partVerdict run rotated LoadBalancing.HttpsProxyPart
                assertBool (show proxyReady) (all isFailure proxyReady)
                before <- length <$> mutations
                (code2, _, err2) <- run (createScript rotated)
                assertEqual err2 ExitSuccess code2
                moved <- drop before <$> mutations
                assertEqual
                    (unlines moved)
                    ["target-https-proxies update web-https-proxy", "certificates delete " <> Text.unpack certV1]
                    (filter (\l -> "target-https-proxies" `isInfixOf` l || "certificates" `isInfixOf` l) moved)
                (code3, out3, err3) <- run (Text.unpack (LoadBalancing.renderLbCheckScript rotated))
                assertEqual (out3 <> err3) Success (LoadBalancing.interpretLbCheck code3 (Text.pack out3))
                -- and nothing more the next time
                before' <- length <$> mutations
                _ <- run (createScript rotated)
                again <- drop before' <$> mutations
                assertBool (unlines again) (not (any (\l -> "target-https-proxies" `isInfixOf` l || "certificates" `isInfixOf` l) again))
                -- down leaves nothing
                (code4, _, err4) <- run (deleteScript rotated)
                assertEqual err4 ExitSuccess code4
                (_, left, _) <- run "ls \"$FAKE_GCLOUD_STATE\""
                assertEqual "" "" left
        , testCase "a certificate under a fixed name is superseded by the domain-set one of that base" $
            withFakeGcloud $ \run mutations -> do
                _ <- run (createScript full)
                before <- length <$> mutations
                (code, _, err) <- run (createScript rotated)
                assertEqual err ExitSuccess code
                moved <- drop before <$> mutations
                assertEqual
                    (unlines moved)
                    ["certificates create " <> Text.unpack certV2, "target-https-proxies update web-https-proxy", "certificates delete web-cert"]
                    (filter (\l -> "target-https-proxies" `isInfixOf` l || "certificates" `isInfixOf` l) moved)
        , testCase "a certificate under a fixed name whose domains changed is refused, by its up and by its check" $
            withFakeGcloud $ \run _ -> do
                _ <- run (createScript full)
                let more = full{LoadBalancing.albCertificates = [LoadBalancing.ManagedCertificate "web-cert" ["app.example.org", "api.example.org", "www.example.org"]]}
                results <- mapM (run . Text.unpack . LoadBalancing.renderPartUpScript more) [p | p <- LoadBalancing.lbParts more, p.partId == LoadBalancing.CertificatePart "web-cert"]
                assertEqual "" 1 (length results)
                mapM_
                    ( \(code, _, err) -> do
                        assertBool err (code /= ExitSuccess)
                        assertBool err ("covers other names than the declared ones" `isInfixOf` err)
                        assertBool err ("DomainSetCertificate" `isInfixOf` err)
                    )
                    results
                verdicts <- partVerdict run more (LoadBalancing.CertificatePart "web-cert")
                case verdicts of
                    [Failure t] -> assertBool (Text.unpack t) ("covers other names" `isInfixOf` Text.unpack t)
                    other -> assertBool (show other) False
                -- the same names in another order and case are the same certificate
                let same = full{LoadBalancing.albCertificates = [LoadBalancing.ManagedCertificate "web-cert" ["API.example.org", "app.example.org"]]}
                sameVerdicts <- partVerdict run same (LoadBalancing.CertificatePart "web-cert")
                assertEqual "" [Success] sameVerdicts
        , testCase "a certificate the proxy still serves is not deleted: not by its own down, not as superseded" $
            withFakeGcloud $ \run mutations -> do
                _ <- run (createScript rotating)
                results <- mapM (run . Text.unpack . LoadBalancing.renderPartDownScript rotating) [p | p <- LoadBalancing.lbParts rotating, p.partId == LoadBalancing.CertificatePart certV1]
                mapM_
                    ( \(code, _, err) -> do
                        assertBool err (code /= ExitSuccess)
                        assertBool err ("still served by web-https-proxy" `isInfixOf` err)
                    )
                    results
                -- the cleanup of the next declaration, run before the proxy has moved
                _ <- run "gcloud certificate-manager certificates create web-cert-00000000 --domains=x.example.org"
                swept <- mapM (run . Text.unpack . LoadBalancing.renderPartUpScript rotated) [p | p <- LoadBalancing.lbParts rotated, p.partId == LoadBalancing.SupersededCertificatesPart "web-cert"]
                assertEqual "" 1 (length swept)
                mapM_ (\(code, _, err) -> assertBool err (code /= ExitSuccess && "still served by" `isInfixOf` err)) swept
                made <- mutations
                assertBool (unlines made) (("certificates delete " <> Text.unpack certV1) `notElem` made)
        , testCase "the cleanup removes only its base's certificates, and its check names what is left" $
            withFakeGcloud $ \run _ -> do
                _ <- run (createScript rotated)
                mapM_
                    (\n -> run ("gcloud certificate-manager certificates create " <> n <> " --domains=x.example.org"))
                    ["web-cert-0123abcd", "web-cert", "web-cert-other", "web-certificate", "other-cert-0123abcd", "web-cert-0123abcdef"]
                verdicts <- partVerdict run rotated (LoadBalancing.SupersededCertificatesPart "web-cert")
                case verdicts of
                    [Failure t] -> assertBool (Text.unpack t) ("superseded certificate web-cert-0123abcd" `isInfixOf` Text.unpack t)
                    other -> assertBool (show other) False
                swept <- mapM (run . Text.unpack . LoadBalancing.renderPartUpScript rotated) [p | p <- LoadBalancing.lbParts rotated, p.partId == LoadBalancing.SupersededCertificatesPart "web-cert"]
                mapM_ (\(code, _, err) -> assertEqual err ExitSuccess code) swept
                (_, out, _) <- run "gcloud certificate-manager certificates list --format='value(name)' | sed 's|.*/||' | sort"
                assertEqual "" (sort [Text.unpack certV2, "web-cert-other", "web-certificate", "other-cert-0123abcd", "web-cert-0123abcdef"]) (lines out)
                after <- partVerdict run rotated (LoadBalancing.SupersededCertificatesPart "web-cert")
                assertEqual "" [Success] after
        , testCase "an ACTIVE certificate whose renewal failed, or that expired, is a Failure" $
            withFakeGcloud $ \run _ -> do
                _ <- run (createScript rotated)
                healthy <- partVerdict run rotated (LoadBalancing.CertificatePart certV2)
                assertEqual "" [Success] healthy
                let stateFile suffix = "\"$FAKE_GCLOUD_STATE\"/certificates." <> Text.unpack certV2 <> suffix
                _ <- run ("echo 'AUTHORIZED;FAILED' > " <> stateFile ".attempts")
                failed <- partVerdict run rotated (LoadBalancing.CertificatePart certV2)
                case failed of
                    [Failure t] -> assertBool (Text.unpack t) ("authorization attempt FAILED" `isInfixOf` Text.unpack t)
                    other -> assertBool (show other) False
                _ <- run ("rm " <> stateFile ".attempts; echo 2001-01-01T00:00:00Z > " <> stateFile ".expire")
                expired <- partVerdict run rotated (LoadBalancing.CertificatePart certV2)
                case expired of
                    [Failure t] -> assertBool (Text.unpack t) ("expired 2001-01-01T00:00:00Z" `isInfixOf` Text.unpack t)
                    other -> assertBool (show other) False
                _ <- run ("echo 2999-01-01T00:00:00Z > " <> stateFile ".expire")
                later <- partVerdict run rotated (LoadBalancing.CertificatePart certV2)
                assertEqual "" [Success] later
        , testCase "a compute certificate swapped for another is an update of the proxy, with nothing to wait for" $
            withFakeGcloud $ \run mutations -> do
                let own n = full{LoadBalancing.albCertificates = [LoadBalancing.ComputeCertificate n]}
                _ <- run "gcloud compute ssl-certificates create own-1; gcloud compute ssl-certificates create own-2"
                _ <- run (createScript (own "own-1"))
                stale <- partVerdict run (own "own-2") LoadBalancing.HttpsProxyPart
                assertBool (show stale) (all isFailure stale)
                before <- length <$> mutations
                (code, _, err) <- run (createScript (own "own-2"))
                assertEqual err ExitSuccess code
                moved <- drop before <$> mutations
                assertEqual (unlines moved) ["target-https-proxies update web-https-proxy"] (filter (\l -> "target-https-proxies" `isInfixOf` l || "certificates" `isInfixOf` l) moved)
                fresh <- partVerdict run (own "own-2") LoadBalancing.HttpsProxyPart
                assertEqual "" [Success] fresh
        , testCase "a plain balancer goes up, checks and comes down the same way" $
            withFakeGcloud $ \run _ -> do
                (code, _, err) <- run (createScript alb)
                assertEqual err ExitSuccess code
                (code1, out1, _) <- run (Text.unpack (LoadBalancing.renderLbCheckScript alb))
                assertEqual out1 Success (LoadBalancing.interpretLbCheck code1 (Text.pack out1))
                (code2, _, err2) <- run (deleteScript alb)
                assertEqual err2 ExitSuccess code2
                (_, out, _) <- run "ls \"$FAKE_GCLOUD_STATE\""
                assertEqual "" "" out
        ]
    ]
  where
    dagOf :: Op -> Dag.Dag Extension
    dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps
    depsOf a part = concat [p.partDeps | p <- LoadBalancing.lbParts a, p.partId == part]
    nodeTests =
        [ testCase "every resource comes after the ones it depends on, and names only resources that exist" $
            mapM_
                ( \a -> do
                    let parts = LoadBalancing.lbParts a
                    let ids = map LoadBalancing.partId parts
                    assertEqual "no resource twice" (nub ids) ids
                    mapM_
                        ( \(i, part) ->
                            mapM_
                                (\d -> assertBool (show part.partId <> " after " <> show d) (d `elem` take i ids))
                                part.partDeps
                        )
                        (zip [0 :: Int ..] parts)
                )
                [alb, full, shared]
        , testCase "the declared resources of a balancer with every feature" $
            assertEqual
                ""
                [ LoadBalancing.HealthCheckPart "hc"
                , LoadBalancing.NamedPortsPart "ig" (LoadBalancing.InstanceGroupZone "europe-west1-b")
                , LoadBalancing.NamedPortsPart "ig-slow" (LoadBalancing.InstanceGroupZone "europe-west1-c")
                , LoadBalancing.BackendServicePart "web-backend"
                , LoadBalancing.BackendPart "web-backend" "ig"
                , LoadBalancing.BackendServicePart "web-slow-backend"
                , LoadBalancing.BackendPart "web-slow-backend" "ig-slow"
                , LoadBalancing.BackendServicePart "web-api-backend"
                , LoadBalancing.NetworkEndpointGroupPart "web-api-neg"
                , LoadBalancing.BackendPart "web-api-backend" "web-api-neg"
                , LoadBalancing.UrlMapPart
                , LoadBalancing.DnsAuthorizationPart "web-cert-app-example-org"
                , LoadBalancing.DnsAuthorizationPart "web-cert-api-example-org"
                , LoadBalancing.CertificatePart "web-cert"
                , LoadBalancing.AddressPart
                , LoadBalancing.HttpProxyPart
                , LoadBalancing.HttpsProxyPart
                , LoadBalancing.ForwardingRulePart
                , LoadBalancing.HttpsForwardingRulePart
                ]
                (map LoadBalancing.partId (LoadBalancing.lbParts full))
        , testCase "the edges between resources" $ do
            assertEqual "a backend service needs its health check" [LoadBalancing.HealthCheckPart "hc"] (depsOf full (LoadBalancing.BackendServicePart "web-slow-backend"))
            assertEqual "a Cloud Run service has none" [] (depsOf full (LoadBalancing.BackendServicePart "web-api-backend"))
            assertEqual
                "an attachment needs its service and the group's named ports"
                [LoadBalancing.BackendServicePart "web-backend", LoadBalancing.NamedPortsPart "ig" (LoadBalancing.InstanceGroupZone "europe-west1-b")]
                (depsOf full (LoadBalancing.BackendPart "web-backend" "ig"))
            assertEqual
                "a second attachment on one service waits for the first"
                [LoadBalancing.BackendServicePart "web-backend", LoadBalancing.NetworkEndpointGroupPart "web-neg", LoadBalancing.BackendPart "web-backend" "ig"]
                (depsOf alb (LoadBalancing.BackendPart "web-backend" "web-neg"))
            assertEqual
                "the URL map needs every service it names"
                (map LoadBalancing.BackendServicePart ["web-backend", "web-slow-backend", "web-api-backend"])
                (depsOf full LoadBalancing.UrlMapPart)
            assertEqual
                "a certificate needs its authorizations"
                (map LoadBalancing.DnsAuthorizationPart ["web-cert-app-example-org", "web-cert-api-example-org"])
                (depsOf full (LoadBalancing.CertificatePart "web-cert"))
            assertEqual "the HTTPS proxy needs the map and the certificate" [LoadBalancing.UrlMapPart, LoadBalancing.CertificatePart "web-cert"] (depsOf full LoadBalancing.HttpsProxyPart)
            assertEqual "a rule needs its proxy and the address" [LoadBalancing.HttpsProxyPart, LoadBalancing.AddressPart] (depsOf full LoadBalancing.HttpsForwardingRulePart)
            assertEqual "without HTTPS there is no address to need" [LoadBalancing.HttpProxyPart] (depsOf alb LoadBalancing.ForwardingRulePart)
        , testCase "the balancer is a root over one node per resource, with the declared edges" $ do
            let dag = dagOf (LoadBalancing.applicationLoadBalancer silent ignoreTrack full)
            let parts = LoadBalancing.lbParts full
            assertEqual "one node per resource, and the root" (length parts + 1) (Map.size (Dag.dagNodes dag))
            let byHelp = Map.fromList [(act.extension.help, r) | (r, act) <- Map.toList (Dag.dagNodes dag)]
            let refsOf ps = Set.fromList [r | p <- ps, Just r <- [Map.lookup (LoadBalancing.partHelp p) byHelp]]
            let dependencies r = Set.fromList (Map.findWithDefault [] r (Dag.dagDependencies dag))
            assertEqual "helps tell the nodes apart" (length parts + 1) (Map.size byHelp)
            assertEqual "every resource is a node" (length parts) (Set.size (refsOf parts))
            mapM_
                ( \part ->
                    assertEqual
                        (show part.partId)
                        [refsOf [p | p <- parts, p.partId `elem` part.partDeps]]
                        (map dependencies (Set.toList (refsOf [part])))
                )
                parts
            assertEqual
                "the root depends on every resource"
                (Just (refsOf parts))
                (dependencies <$> Map.lookup "application load balancer web" byHelp)
        , testCase "prerequisites go under every resource, not only under the root" $ do
            let beforeRef = mkRef "test-before" ("x" :: Text.Text)
            let before = op "before" nodeps (\actions -> actions{help = "before", ref = beforeRef})
            let dag = dagOf (LoadBalancing.applicationLoadBalancerAfter [before] silent ignoreTrack alb)
            mapM_
                ( \(r, act) ->
                    assertBool
                        (Text.unpack act.extension.help)
                        (r == beforeRef || beforeRef `elem` Map.findWithDefault [] r (Dag.dagDependencies dag))
                )
                (Map.toList (Dag.dagNodes dag))
        , testCase "one resource's node is the balancer's own" $ do
            let part = LoadBalancing.applicationLoadBalancerPart [] silent ignoreTrack full (LoadBalancing.DnsAuthorizationPart "web-cert-app-example-org")
            case part of
                Nothing -> assertBool "no such node" False
                Just o -> do
                    let dag = dagOf (LoadBalancing.applicationLoadBalancer silent ignoreTrack full `inject` o)
                    assertEqual "nothing new" (length (LoadBalancing.lbParts full) + 1) (Map.size (Dag.dagNodes dag))
                    assertEqual "nothing conflicting" 0 (length (Dag.dagConflicts dag))
                    assertEqual "an authorization stands alone" 1 (Map.size (Dag.dagNodes (dagOf o)))
            assertBool
                "a resource the declaration does not have"
                (null (LoadBalancing.applicationLoadBalancerPart [] silent ignoreTrack alb LoadBalancing.AddressPart))
        , testCase "an invalid declaration is every node's Failure and every node's refusal, before any call" $ do
            let bad = alb{LoadBalancing.albHostRules = [LoadBalancing.HostRule ["x.example.org"] (LoadBalancing.NamedService "nope") []]}
            mapM_
                ( \act -> do
                    v <- act.extension.check
                    assertBool (Text.unpack act.extension.help <> ": " <> show v) (isFailure v)
                    r <- try act.extension.up :: IO (Either LoadBalancing.InvalidLoadBalancer ())
                    assertBool (Text.unpack act.extension.help) (either (const True) (const False) r)
                )
                (Map.elems (Dag.dagNodes (dagOf (LoadBalancing.applicationLoadBalancer silent ignoreTrack bad))))
        , testCase "each resource's scripts parse as bash" $
            mapM_
                ( \sc -> do
                    (code, _, err) <- readProcessWithExitCode "bash" ["-n", "-c", Text.unpack sc] ""
                    assertEqual err ExitSuccess code
                )
                [ render full part
                | part <- LoadBalancing.lbParts full
                , render <- [LoadBalancing.renderPartUpScript, LoadBalancing.renderPartCheckScript, LoadBalancing.renderPartDownScript]
                ]
        ]
    svcUrl :: Text.Text -> Text.Text
    svcUrl n = "https://www.googleapis.com/compute/v1/projects/p/regions/europe-west1/backendServices/" <> n
    inOrder :: [String] -> String -> Bool
    inOrder [] _ = True
    inOrder (w : ws) hay = case breakOn w hay of
        Nothing -> False
        Just rest -> inOrder ws rest
    breakOn :: String -> String -> Maybe String
    breakOn w hay = case [drop (length w) t | t <- tails hay, w `isPrefixOf` t] of
        (rest : _) -> Just rest
        [] -> Nothing
    deleteScript a = case processArgs (prepare LoadBalancing.loadBalancingCommand (LoadBalancing.LbDelete a)) of
        (_ : s : _) -> s
        other -> error (show other)
    withFakeGcloud :: ((String -> IO (ExitCode, String, String)) -> IO [String] -> IO a) -> IO a
    withFakeGcloud body =
        withSystemTempDirectory "salmon-fake-gcloud" $ \dir -> do
            let fake = dir </> "gcloud.sh"
            let state = dir </> "state"
            let logFile = dir </> "mutations.log"
            createDirectory state
            writeFile logFile ""
            writeFile fake fakeGcloud
            path <- getEnv "PATH"
            -- A shell function that has bash /read/ the stand-in, rather
            -- than an executable on PATH: this suite runs its groups in
            -- parallel in one process, and exec'ing a file some other
            -- test's fork still holds open for writing is ETXTBSY.
            let run sc =
                    readCreateProcessWithExitCode
                        (proc "bash" ["-c", "gcloud() { bash \"$FAKE_GCLOUD\" \"$@\"; }\n" <> sc])
                            { env = Just [("PATH", path), ("FAKE_GCLOUD", fake), ("FAKE_GCLOUD_STATE", state), ("FAKE_GCLOUD_LOG", logFile)]
                            }
                        ""
            body run (lines <$> (readFile logFile >>= \c -> length c `seq` pure c))
    -- one VM, one instance group, two services on two ports
    shared =
        alb
            { LoadBalancing.albBackends = []
            , LoadBalancing.albServices =
                [ LoadBalancing.BackendService n [LoadBalancing.InstanceGroupBackend "ig" (LoadBalancing.InstanceGroupZone "europe-west1-b") [p]] (Just (LoadBalancing.HealthCheck ("hc-" <> n) p)) Nothing
                | (n, p) <- [("a", 4272), ("b", 4273)]
                ]
            , LoadBalancing.albHostRules =
                [ LoadBalancing.HostRule [n <> ".example.org"] (LoadBalancing.NamedService n) []
                | n <- ["a", "b"]
                ]
            }
    full =
        alb
            { LoadBalancing.albBackends = [LoadBalancing.InstanceGroupBackend "ig" (LoadBalancing.InstanceGroupZone "europe-west1-b") [8080]]
            , LoadBalancing.albTimeoutSec = Just 60
            , LoadBalancing.albServices =
                [ LoadBalancing.BackendService
                    "slow"
                    [LoadBalancing.InstanceGroupBackend "ig-slow" (LoadBalancing.InstanceGroupZone "europe-west1-c") [9090]]
                    (Just (LoadBalancing.HealthCheck "hc" 8080))
                    (Just 3600)
                , LoadBalancing.BackendService "api" [LoadBalancing.CloudRunBackend "api-svc"] Nothing Nothing
                ]
            , LoadBalancing.albHostRules =
                [ LoadBalancing.HostRule
                    ["app.example.org", "www.example.org"]
                    LoadBalancing.DefaultService
                    [LoadBalancing.PathRule ["/events/*", "/poll"] (LoadBalancing.NamedService "slow")]
                , LoadBalancing.HostRule ["api.example.org"] (LoadBalancing.NamedService "api") []
                ]
            , LoadBalancing.albCertificates = [LoadBalancing.ManagedCertificate "web-cert" ["app.example.org", "api.example.org"]]
            }
    -- the same balancer with its certificate named after its domain set, before and after one more host
    rotating = full{LoadBalancing.albCertificates = [LoadBalancing.DomainSetCertificate "web-cert" ["app.example.org", "api.example.org"]]}
    rotated = full{LoadBalancing.albCertificates = [LoadBalancing.DomainSetCertificate "web-cert" ["app.example.org", "api.example.org", "www.example.org"]]}
    certV1 = "web-cert-" <> LoadBalancing.domainSetTag ["app.example.org", "api.example.org"]
    certV2 = "web-cert-" <> LoadBalancing.domainSetTag ["app.example.org", "api.example.org", "www.example.org"]
    partVerdict run a part =
        mapM
            ( \spec -> do
                (code, out, _) <- run (Text.unpack (LoadBalancing.renderPartCheckScript a spec))
                pure (LoadBalancing.interpretLbCheck code (Text.pack out))
            )
            [spec | spec <- LoadBalancing.lbParts a, spec.partId == part]
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
            , LoadBalancing.albTimeoutSec = Nothing
            , LoadBalancing.albServices = []
            , LoadBalancing.albHostRules = []
            , LoadBalancing.albCertificates = []
            }

{- | A stand-in for @gcloud@: resources are files named
@\<collection\>.\<name\>@ under @$FAKE_GCLOUD_STATE@, and every call that
would change something is appended to @$FAKE_GCLOUD_LOG@. It answers only
the calls "Salmon.Builtin.Nodes.Gcp.LoadBalancing"'s scripts make, and its
@--format@ output is this module's guess at gcloud's, not a recording.
-}
fakeGcloud :: String
fakeGcloud =
    unlines
        [ "#!/usr/bin/env bash"
        , "set -euo pipefail"
        , "S=\"$FAKE_GCLOUD_STATE\""
        , "coll=\"$2\"; verb=\"$3\"; name=\"$4\""
        , "if [ \"$verb\" = create ] && [ \"$name\" = tcp ]; then name=\"$5\"; fi"
        , "format=''; group=''; timeout=''; portname=''; namedports=''; domains=''; certs=''"
        , "for a in \"$@\"; do case \"$a\" in"
        , "  --format=*) format=\"${a#--format=}\";;"
        , "  --instance-group=*) group=\"/instanceGroups/${a#--instance-group=}\";;"
        , "  --network-endpoint-group=*) group=\"/networkEndpointGroups/${a#--network-endpoint-group=}\";;"
        , "  --timeout=*) timeout=\"${a#--timeout=}\";;"
        , "  --port-name=*) portname=\"${a#--port-name=}\";;"
        , "  --named-ports=*) namedports=\"${a#--named-ports=}\";;"
        , "  --domains=*) domains=\"${a#--domains=}\";;"
        , "  --certificate-manager-certificates=*) certs=\"${a#--certificate-manager-certificates=}\";;"
        , "  --ssl-certificates=*) certs=\"${a#--ssl-certificates=}\";;"
        , "esac; done"
        , "f=\"$S/$coll.$name\""
        , "mutate() { echo \"$coll $verb $name\" >> \"$FAKE_GCLOUD_LOG\"; }"
        , -- a proxy lists its certificates by path, as (it is assumed) the real one does
          "served() { for p in \"$S\"/target-https-proxies.*.certs; do [ -e \"$p\" ] && tr ',' '\\n' < \"$p\"; done; true; }"
        , "case \"$verb\" in"
        , "  describe)"
        , "    [ \"$coll\" = instance-groups ] && exit 0"
        , "    [ -e \"$f\" ] || { echo \"NOT_FOUND $coll $name\" >&2; exit 1; }"
        , "    case \"$format\" in"
        , "      'value(backends[].group)') paste -sd';' \"$f.backends\" 2>/dev/null || true;;"
        , "      'value(timeoutSec)') cat \"$f.timeout\" 2>/dev/null || echo 30;;"
        , "      'value(portName)') cat \"$f.portname\" 2>/dev/null || echo http;;"
        , "      'value(managed.state)') cat \"$f.state\" 2>/dev/null || echo ACTIVE;;"
        , "      'value(managed.domains)') tr ',' ';' < \"$f.domains\" 2>/dev/null || true;;"
        , "      'value(managed.authorizationAttemptInfo[].state)') cat \"$f.attempts\" 2>/dev/null || echo AUTHORIZED;;"
        , "      'value(expireTime)') cat \"$f.expire\" 2>/dev/null || true;;"
        , "      'value(sslCertificates)') tr ',' '\\n' < \"$f.certs\" 2>/dev/null | sed 's|^|//certificatemanager.googleapis.com/projects/p/locations/r/certificates/|' | paste -sd';' || true;;"
        , "      'value(hostRules[].hosts)') grep -o '\"hosts\":\\[[^]]*\\]' \"$f\" | sed -e 's/\"hosts\"://' -e 's/[]\\[\"]//g' | paste -sd';' || true;;"
        , "      'value(IPAddress)') echo 203.0.113.7;;"
        , "    esac;;"
        , "  create) [ -e \"$f\" ] && { echo \"ALREADY_EXISTS $coll $name\" >&2; exit 1; }; mutate; : > \"$f\"; if [ -n \"$portname\" ]; then echo \"$portname\" > \"$f.portname\"; fi"
        , "    if [ -n \"$certs\" ]; then echo \"$certs\" > \"$f.certs\"; fi"
        , -- a new certificate is not issued at once, when the test says so
          "    if [ -n \"$domains\" ]; then echo \"$domains\" > \"$f.domains\"; echo \"${FAKE_GCLOUD_NEW_CERT_STATE:-ACTIVE}\" > \"$f.state\"; fi;;"
        , "  list) for c in \"$S\"/\"$coll\".*; do n=\"${c##*/}\"; n=\"${n#\"$coll\".}\"; case \"$n\" in *.*|'*') ;; *) echo \"projects/p/locations/r/$coll/$n\";; esac; done;;"
        , "  import) mutate; cat > \"$f\";;"
        , "  update) [ -e \"$f\" ] || exit 1; mutate; if [ -n \"$timeout\" ]; then echo \"$timeout\" > \"$f.timeout\"; fi; if [ -n \"$portname\" ]; then echo \"$portname\" > \"$f.portname\"; fi; if [ -n \"$certs\" ]; then echo \"$certs\" > \"$f.certs\"; fi;;"
        , "  add-backend) [ -e \"$f\" ] || exit 1; grep -qxF \"$group\" \"$f.backends\" 2>/dev/null && { echo 'already a backend' >&2; exit 1; }; mutate; echo \"$group\" >> \"$f.backends\";;"
        , "  remove-backend) [ -e \"$f\" ] || exit 1; grep -qxF \"$group\" \"$f.backends\" || { echo 'not a backend' >&2; exit 1; }; mutate; { grep -vxF \"$group\" \"$f.backends\" || true; } > \"$f.backends.new\"; mv \"$f.backends.new\" \"$f.backends\";;"
        , -- like the real one, it replaces the group's whole set
          "  set-named-ports) mutate; printf '%s\\n' \"$namedports\" | tr ',' '\\n' | tr ':' '\\t' > \"$FAKE_GCLOUD_LOG.ports.$name\";;"
        , "  get-named-ports) cat \"$FAKE_GCLOUD_LOG.ports.$name\" 2>/dev/null || true;;"
        , "  get-health) echo 'HEALTHY;HEALTHY';;"
        , "  delete) [ -e \"$f\" ] || exit 1"
        , "    if [ \"$coll\" = certificates ] && served | grep -qxF \"$name\"; then echo \"IN_USE $coll $name\" >&2; exit 1; fi"
        , "    mutate; rm -f \"$f\" \"$f.backends\" \"$f.timeout\" \"$f.portname\" \"$f.certs\" \"$f.domains\" \"$f.state\" \"$f.attempts\" \"$f.expire\";;"
        , "  *) echo \"fake gcloud: unhandled $*\" >&2; exit 2;;"
        , "esac"
        ]

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

cloudDnsRecordTests :: [TestTree]
cloudDnsRecordTests =
    [ testCase "create names the record, its zone, type, TTL and data" $
        assertEqual
            ""
            ["dns", "record-sets", "create", "www.example.org.", "--zone", "example-zone", "--type", "A", "--project", "my-project", "--ttl", "300", "--rrdatas=192.0.2.1,192.0.2.2"]
            (args (CloudDns.RecordSetsCreate a))
    , testCase "update differs from create by its verb only" $
        assertEqual
            ""
            (map (\w -> if w == "create" then "update" else w) (args (CloudDns.RecordSetsCreate a)))
            (args (CloudDns.RecordSetsUpdate a))
    , testCase "describe asks for JSON, by name and type" $
        assertEqual
            ""
            ["dns", "record-sets", "describe", "www.example.org.", "--zone", "example-zone", "--type", "A", "--project", "my-project", "--format", "json"]
            (args (CloudDns.RecordSetsDescribe a))
    , testCase "delete names the record and its type" $
        assertEqual
            ""
            ["dns", "record-sets", "delete", "www.example.org.", "--zone", "example-zone", "--type", "A", "--project", "my-project"]
            (args (CloudDns.RecordSetsDelete a))
    , testCase "a CNAME's target gets its trailing dot" $
        assertEqual "" "--rrdatas=target.example.net." (last (args (CloudDns.RecordSetsCreate cname)))
    , testCase "a TXT is quoted, and a comma in it moves the list separator" $
        assertEqual "" "--rrdatas=^;^\"v=spf1 ip4:192.0.2.0/24,-all\";\"second\"" (last (args (CloudDns.RecordSetsCreate txt)))
    , testCase "the separator is one no datum contains" $ do
        assertEqual "" "a,b" (CloudDns.renderRrdatas ["a", "b"])
        assertEqual "" "^|^a,;|b" (CloudDns.renderRrdatas ["a,;", "b"])
        assertEqual "" "^|||^,;|#~%@!|||x||" (CloudDns.renderRrdatas [",;|#~%@!", "x||"])
    , testCase "TXT data escapes quotes and backslashes" $
        assertEqual "" "\"say \\\"hi\\\" \\\\ bye\"" (CloudDns.txtRdata "say \"hi\" \\ bye")
    , testCase "a long TXT is split into strings of 255 and reads back whole" $ do
        let long = Text.replicate 60 "0123456789"
            rdata = CloudDns.txtRdata long
        assertEqual "" [257, 257, 92] (map Text.length (Text.splitOn " " rdata))
        assertEqual "" long (CloudDns.txtContent rdata)
    , testCase "txtContent undoes txtRdata, spaces and escapes included" $
        mapM_
            (\t -> assertEqual (Text.unpack t) t (CloudDns.txtContent (CloudDns.txtRdata t)))
            ["v=spf1 -all", "say \"hi\" \\ bye", "", "a  b", "trailing \\"]
    , testCase "txtContent takes unquoted data as it is" $
        assertEqual "" "plain" (CloudDns.txtContent "plain")
    , testCase "the description's TTL and data are read" $
        assertEqual "" (Right (CloudDns.RecordDescription 300 ["192.0.2.2", "192.0.2.1"])) (CloudDns.parseRecordDescribe (described 300 ["192.0.2.2", "192.0.2.1"]))
    , testCase "the declared data in any order is satisfied" $
        assertEqual "" Success (CloudDns.interpretRecordDescribe a ExitSuccess (described 300 ["192.0.2.2", "192.0.2.1"]))
    , testCase "describe failing means the record is absent" $
        assertBool "" (isFailure (CloudDns.interpretRecordDescribe a (ExitFailure 1) ""))
    , testCase "other data is a failure naming both" $
        case CloudDns.interpretRecordDescribe a ExitSuccess (described 300 ["192.0.2.9"]) of
            Failure why -> do
                assertBool (Text.unpack why) ("192.0.2.9" `Text.isInfixOf` why)
                assertBool (Text.unpack why) ("192.0.2.1, 192.0.2.2" `Text.isInfixOf` why)
            other -> assertBool ("expected a Failure, got " <> show other) False
    , testCase "a subset of the declared data is a failure" $
        assertBool "" (isFailure (CloudDns.interpretRecordDescribe a ExitSuccess (described 300 ["192.0.2.1"])))
    , testCase "another TTL is a failure naming both" $
        case CloudDns.interpretRecordDescribe a ExitSuccess (described 60 ["192.0.2.1", "192.0.2.2"]) of
            Failure why -> assertBool (Text.unpack why) ("60" `Text.isInfixOf` why && "300" `Text.isInfixOf` why)
            other -> assertBool ("expected a Failure, got " <> show other) False
    , testCase "output that is not a record description cannot be judged" $
        assertEqual "" Unknown (CloudDns.interpretRecordDescribe a ExitSuccess "not json")
    , testCase "a CNAME compares whatever the dot and case" $
        assertEqual "" Success (CloudDns.interpretRecordDescribe (cname {CloudDns.recordData = ["Target.Example.NET"]}) ExitSuccess (described 300 ["target.example.net."]))
    , testCase "a TXT compares by content, quoted as Cloud DNS reports it" $
        assertEqual "" Success (CloudDns.interpretRecordDescribe txt ExitSuccess (described 300 ["\"second\"", "\"v=spf1 ip4:192.0.2.0/24,\" \"-all\""]))
    , testCase "an AAAA compares whatever the case" $
        assertEqual "" Success (CloudDns.interpretRecordDescribe (a {CloudDns.recordType = CloudDns.AAAA, CloudDns.recordData = ["2001:DB8::1"]}) ExitSuccess (described 300 ["2001:db8::1"]))
    , testCase "up creates what describe did not find and updates what it did" $ do
        assertEqual "" ["create"] (verb (CloudDns.recordUpCommand a (ExitFailure 1)))
        assertEqual "" ["update"] (verb (CloudDns.recordUpCommand a ExitSuccess))
    , testCase "a writable record set has no problems, at the apex included" $ do
        assertEqual "" [] (CloudDns.recordSetProblems a)
        assertEqual "" [] (CloudDns.recordSetProblems (a {CloudDns.recordName = "Example.org"}))
        assertEqual "" [] (CloudDns.recordSetProblems cname)
    , testCase "no data, a name outside the zone, a CNAME at the apex or with two targets are refused" $ do
        assertEqual "" 1 (length (CloudDns.recordSetProblems (a {CloudDns.recordData = []})))
        assertEqual "" 1 (length (CloudDns.recordSetProblems (a {CloudDns.recordName = "www.example.net"})))
        assertEqual "" 1 (length (CloudDns.recordSetProblems (a {CloudDns.recordName = "wwwexample.org"})))
        assertEqual "" 1 (length (CloudDns.recordSetProblems (cname {CloudDns.recordName = "example.org"})))
        assertEqual "" 1 (length (CloudDns.recordSetProblems (cname {CloudDns.recordData = ["a.example.net", "b.example.net"]})))
    , testCase "a record set that cannot be written fails its check whatever is live" $
        assertBool "" (isFailure (CloudDns.interpretRecordDescribe (a {CloudDns.recordName = "www.example.net"}) ExitSuccess (described 300 ["192.0.2.1", "192.0.2.2"])))
    ]
  where
    zone = CloudDns.ManagedZone "example-zone" (Core.Project "my-project") "example.org" "a zone"
    a = CloudDns.RecordSet zone "www.example.org" CloudDns.A 300 ["192.0.2.1", "192.0.2.2"]
    cname = CloudDns.RecordSet zone "alias.example.org." CloudDns.CNAME 300 ["target.example.net"]
    txt = CloudDns.RecordSet zone "example.org" CloudDns.TXT 300 ["v=spf1 ip4:192.0.2.0/24,-all", "second"]
    args = processArgs . prepare CloudDns.cloudDnsCommand
    verb = take 1 . drop 2 . args
    described :: Int -> [Text.Text] -> ByteString.ByteString
    described ttl rrdatas =
        LByteString.toStrict $
            encode $
                object
                    [ "kind" .= ("dns#resourceRecordSet" :: Text.Text)
                    , "name" .= ("www.example.org." :: Text.Text)
                    , "type" .= ("A" :: Text.Text)
                    , "ttl" .= ttl
                    , "rrdatas" .= rrdatas
                    ]
