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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as LoadBalancing
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
        ]

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
    ]
  where
    binding =
        Iam.IamBinding
            { Iam.iamPrincipal = Iam.ServiceAccount "sa-1@p.iam.gserviceaccount.com"
            , Iam.iamRole = "roles/storage.objectViewer"
            , Iam.iamResource = "projects/p"
            }

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
