{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for the pure @check@-verdict interpreters under
"Salmon.Builtin.Nodes.Gcp" -- each node shells out to @gcloud@/@ssh@ to get
its raw exit code and output, then hands that to a pure function that draws
the 'CheckResult'. Splitting the decision out (the same shape as
"Salmon.Builtin.Nodes.Systemd"'s @interpretShow@, see @Test.SystemdSpec@) is
what makes it testable without a real GCP project.
-}
module Test.GcpSpec (tests) where

import GHC.IO.Exception (ExitCode (..))
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
import qualified Salmon.Builtin.Nodes.Gcp.ServiceUsage as ServiceUsage
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage

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
    [ testCase "describe succeeding means the repo exists" $
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
