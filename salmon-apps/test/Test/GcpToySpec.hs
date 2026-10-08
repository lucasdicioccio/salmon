{- | Layer 0: what @salmon-gcp-toy@ declares for tier 3's container, on both
sides of the hand-off. No gcloud, no podman and no systemd is run, and no
node's @up@.

Tier 3 serves its page from a container, and three things have to agree for
that to work on a machine nobody has logged in to: the control side pushes
the image the VM pulls, the VM is created as an account that may read the
repository, and the login is ahead of the pull. These tests are that
agreement, read from the folded graphs.
-}
module Test.GcpToySpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- GHC only solves a `HasField` constraint when the selector is in scope, and
-- `Dag.sameRepresentative` needs these whether or not this module says them.
import Salmon.Builtin.Extension (Extension, Op, dynamics, evalDeps, help, notes, ref)
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Self as Self
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)
import Salmon.Op.Track (Track (..))

import GcpToy

tests :: TestTree
tests =
    testGroup
        "salmon-gcp-toy (tier 3's container, Layer 0)"
        [ testCase "the VM runs the image the control side pushes" sameImageOnBothSides
        , testCase "on the VM: podman, then the instance login, then the pull, then the quadlet" vmSideOrder
        , testCase "the VM no longer declares the python unit" noAuthoredUnit
        , testCase "the quadlet publishes the balancer's port and pulls with the login's file" quadletFile
        , testCase "the instance stands on the pushed image, the account and its grant" instancePrerequisites
        , testCase "at tier 3 the instance runs as the toy's account, with a scope a pull can use" instanceIdentity
        , testCase "below tier 3 the instance is the one it was" tierTwoUnchanged
        , testCase "the peer keeps the default account and scopes" peerUnchanged
        , testCase "tiers 1 and 3 log in to the registry through one node" oneLogin
        , testCase "no declaration collides with another" noConflicts
        ]

-------------------------------------------------------------------------------

vm :: VmConfig
vm =
    VmConfig
        { vmZone = "europe-west1-b"
        , vmMachineType = "e2-micro"
        , vmImageFamily = "ubuntu-2404-lts-amd64"
        , vmImageProject = "ubuntu-os-cloud"
        , vmUser = "salmon"
        , vmSshSourceRange = "0.0.0.0/0"
        , vmIp = Just "203.0.113.7"
        , vmSelfPath = Self.SelfPath "/usr/local/bin/salmon-gcp-toy"
        , vmMarkerPath = "/var/lib/salmon-toy/provisioned"
        }

lb :: LbConfig
lb = LbConfig "192.168.100.0/24" 8080 Nothing

specAt :: Int -> Spec
specAt n =
    Spec
        { role = Control
        , project = "toy-project"
        , createProjectUnder = Nothing
        , billingAccount = Nothing
        , region = "europe-west1"
        , tier = n
        , prefix = "salmon-toy"
        , imageTag = "v1"
        , imageSource = FromBaseImage defaultBaseImage
        , workDir = "/work"
        , vmConfig = if n >= 2 then Just vm else Nothing
        , lbConfig = if n >= 3 then Just lb else Nothing
        , alertEmail = Nothing
        , peerConfig = Nothing
        , dnsZone = Nothing
        , account = Nothing
        }

withPeer :: Spec -> Spec
withPeer s = s{peerConfig = Just (PeerConfig "10.132.0.50" "10.132.0.51" 8081)}

onTheVm :: Spec -> Spec
onTheVm s = s{role = OnVm}

dagOf :: Spec -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps . graph
  where
    graph :: Spec -> Op
    graph = run program

-- | The refs of the nodes whose @help@ holds these words.
named :: Text -> Dag.Dag Extension -> [Ref]
named words' dag = [rf | (rf, a) <- Map.toList (Dag.dagNodes dag), words' `Text.isInfixOf` a.extension.help]

theOne :: Text -> Dag.Dag Extension -> IO Ref
theOne words' dag = case named words' dag of
    [rf] -> pure rf
    others -> assertFailure ("expected one node saying " <> show words' <> ", found " <> show (length others))

-- | Everything a node waits for, however far down.
below :: Dag.Dag Extension -> Ref -> Set.Set Ref
below dag = go Set.empty . Dag.dependenciesOf dag
  where
    go seen [] = seen
    go seen (r : rest)
        | r `Set.member` seen = go seen rest
        | otherwise = go (Set.insert r seen) (Dag.dependenciesOf dag r <> rest)

image :: Text
image = "europe-west1-docker.pkg.dev/toy-project/salmon-toy-repo/page:v1"

-------------------------------------------------------------------------------

sameImageOnBothSides :: IO ()
sameImageOnBothSides = do
    assertEqual "the reference" image (pageImage (specAt 3))
    assertEqual "the same from the directive the VM is handed" image (pageImage (onTheVm (specAt 3)))
    _ <- theOne ("pushes " <> image) (dagOf (specAt 3))
    _ <- theOne ("pulls " <> image) (dagOf (onTheVm (specAt 3)))
    _ <- theOne ("runs " <> image <> " as salmon-toy-page.service") (dagOf (onTheVm (specAt 3)))
    assertBool "the page names the project and the image" (all (`Text.isInfixOf` pageText (specAt 3)) ["toy-project", image])

vmSideOrder :: IO ()
vmSideOrder = do
    let dag = dagOf (onTheVm (specAt 3))
    podman <- theOne "installs podman" dag
    login <- theOne "logs in to europe-west1-docker.pkg.dev via /var/lib/salmon-toy/registry-auth.json" dag
    pull <- theOne ("pulls " <> image) dag
    unit <- theOne "as salmon-toy-page.service" dag
    assertBool "the login waits for podman" (podman `Set.member` below dag login)
    assertBool "the pull waits for the login" (login `Set.member` below dag pull)
    assertBool "the service waits for the pull" (pull `Set.member` below dag unit)
    root <- theOne "the tier-2 payload" dag
    assertBool "and the payload for the service" (unit `Set.member` below dag root)

noAuthoredUnit :: IO ()
noAuthoredUnit = do
    let helps = [a.extension.help | a <- Map.elems (Dag.dagNodes (dagOf (onTheVm (specAt 3))))]
    assertEqual "nodes naming the old unit or its interpreter" [] [h | h <- helps, any (`Text.isInfixOf` h) ["salmon-toy-web", "python3"]]
    -- and at tier 2 there is no container either
    assertEqual "a tier-2 VM declares no container" [] (named "salmon-toy-page" (dagOf (onTheVm (specAt 2))))

quadletFile :: IO ()
quadletFile = do
    let c = pageContainer (specAt 3) lb
        text = Text.unpack (Quadlet.renderContainer c)
    assertEqual "nothing the node would refuse" [] (Quadlet.containerProblems c)
    assertEqual "in system scope" Quadlet.systemQuadletDir c.containerUnitDir
    assertEqual "the file" "/etc/containers/systemd/salmon-toy-page.container" (Quadlet.quadletPath c)
    mapM_
        (\line -> assertBool (line <> " in:\n" <> text) (line `isInfixOf` text))
        [ "Image=" <> Text.unpack image <> "\n"
        , "PublishPort=8080:80"
        , "--authfile=" <> Podman.getAuthFile pageAuthFile
        ]
    assertEqual "the login writes the file the pull reads" (Just pageAuthFile) c.containerAuthFile

instancePrerequisites :: IO ()
instancePrerequisites = do
    let dag = dagOf (specAt 3)
    inst <- theOne "GCE instance salmon-toy-vm" dag
    push <- theOne ("pushes " <> image) dag
    sa <- theOne "service account salmon-toy-sa" dag
    let under = below dag inst
        grants = [rf | rf <- Set.toList under, Just a <- [Map.lookup rf (Dag.dagNodes dag)], any ("roles/artifactregistry.reader" `Text.isInfixOf`) (a.extension.help : a.extension.notes)]
    assertBool "the image is pushed before there is a machine to pull it" (push `Set.member` under)
    assertBool "the account exists before an instance is created as it" (sa `Set.member` under)
    assertEqual "the account may read the repository before the machine asks" 1 (length grants)
    -- and none of it at tier 2
    let dag2 = dagOf (specAt 2)
    inst2 <- theOne "GCE instance salmon-toy-vm" dag2
    assertEqual "a tier-2 instance waits for no image" [] (named ("pushes " <> image) dag2)
    assertEqual "nor for the account" [] [rf | rf <- named "service account salmon-toy-sa" dag2, rf `Set.member` below dag2 inst2]

instanceIdentity :: IO ()
instanceIdentity = do
    let inst = gceInstance (specAt 3) vm
    assertEqual "the account" (Just "salmon-toy-sa@toy-project.iam.gserviceaccount.com") inst.instanceServiceAccount
    assertEqual "the scope" (Compute.DeclaredScopes ["cloud-platform"]) inst.instanceScopes

tierTwoUnchanged :: IO ()
tierTwoUnchanged = do
    let inst = gceInstance (specAt 2) vm
    assertEqual "the account" Nothing inst.instanceServiceAccount
    assertEqual "the scopes" Compute.DefaultScopes inst.instanceScopes
    assertEqual "the tags" ["salmon-toy-ssh"] inst.instanceTags

peerUnchanged :: IO ()
peerUnchanged = do
    let dag = dagOf (withPeer (specAt 3))
    peer <- theOne "GCE instance salmon-toy-peer" dag
    machine <- theOne "GCE instance salmon-toy-vm" dag
    let notesOf rf = maybe [] (\a -> a.extension.notes) (Map.lookup rf (Dag.dagNodes dag))
    assertEqual "the peer declares no scopes" [] (filter ("scopes" `Text.isPrefixOf`) (notesOf peer))
    assertEqual "the VM declares its own" ["scopes: cloud-platform"] (filter ("scopes" `Text.isPrefixOf`) (notesOf machine))

oneLogin :: IO ()
oneLogin = do
    _ <- theOne "logs in to europe-west1-docker.pkg.dev via /work/podman-auth.json" (dagOf (specAt 3))
    pure ()

noConflicts :: IO ()
noConflicts =
    mapM_
        (\(what, s) -> assertEqual what 0 (length (Dag.dagConflicts (dagOf s))))
        [ ("control, tier 3", specAt 3)
        , ("control, tier 3 with a peer", withPeer (specAt 3))
        , ("control, tier 2", specAt 2)
        , ("on the VM, tier 3", onTheVm (specAt 3))
        ]

_unused :: ()
_unused = const () (dynamics, notes, ref)
