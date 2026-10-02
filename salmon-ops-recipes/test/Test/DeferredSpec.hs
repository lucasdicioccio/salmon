{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Deferred": a sub-graph built
at @up@ from a value another node of the same pass produced, and for the one
recipe built on it, 'VmProvision.provisionedVmReadingHost', as far as its
/declared/ shape goes (what it does to a machine needs a GCP project).

Everything here is in-process: the "value" is an 'IORef' a node's @up@
writes, which is the whole of what the address GCP picks is to the graph.
-}
module Test.DeferredSpec (tests) where

import Control.Exception (throwIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Builtin.Extension (Extension, Op, down, dynamics, evalDeps, help, ignoreTrack, nodeps, notes, op, realNoop, ref, up)
import qualified Salmon.Builtin.Nodes.Deferred as Deferred
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Keys as Keys
import qualified Salmon.Builtin.Nodes.Self as Self
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)
import qualified SreBox.Gcp.VmProvision as VmProvision

import Test.Harness (runDown, runUp)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Deferred"
        [ testCase "one pass: the producer's up feeds the sub-graph built after it" onePass
        , testCase "declaring the node reads nothing" declarationReadsNothing
        , testCase "a read that finds nothing fails up and builds no graph" unresolvedFailsUp
        , testCase "a failure in the nested walk fails the node" nestedFailureSurfaces
        , testCase "down walks the sub-graph down, before the producer" downWalksSubgraph
        , testCase "down with nothing to read is a no-op, not a failure" downWithoutValue
        , testGroup
            "VmProvision.provisionedVmReadingHost"
            [ testCase "declares the machine, not what names it" readingHostShape
            , testCase "provisionedVm still declares the hand-off as nodes" knownHostShape
            ]
        ]

-------------------------------------------------------------------------------

type Log = IORef [Text]

say :: Log -> Text -> IO ()
say l t = modifyIORef' l (t :)

logged :: Log -> IO [Text]
logged l = reverse <$> readIORef l

-- | A node standing for "GCP picks an address": its @up@ makes the value
-- exist, its @down@ takes it away.
producer :: Log -> IORef (Maybe Text) -> Op
producer l cell =
    op "producer" nodeps $ \x ->
        x
            { ref = mkRef "deferred-spec" ("producer" :: Text)
            , up = say l "producer up" >> writeIORef cell (Just "198.51.100.7")
            , down = say l "producer down" >> writeIORef cell Nothing
            }

-- | What needs the value: a node whose effects name it.
consumer :: Log -> Text -> Op
consumer l host =
    op "consumer" nodeps $ \x ->
        x
            { ref = mkRef "deferred-spec" ("consumer" :: Text, host)
            , up = say l ("consumer up " <> host)
            , down = say l ("consumer down " <> host)
            }

late :: IO (Maybe Text) -> (Text -> Op) -> Op
late readIt graph =
    Deferred.deferred
        silent
        Deferred.Deferred
            { Deferred.deferredName = "spec"
            , Deferred.deferredHelp = "uses what the producer made"
            , Deferred.deferredRead = readIt
            , Deferred.deferredGraph = graph
            }

-------------------------------------------------------------------------------

onePass :: IO ()
onePass = do
    l <- newIORef []
    cell <- newIORef Nothing
    ok <- runUp (late (readIORef cell) (consumer l) `inject` producer l cell)
    assertBool "the pass succeeds" ok
    assertEqual "the consumer saw the value the producer made" ["producer up", "consumer up 198.51.100.7"] =<< logged l

declarationReadsNothing :: IO ()
declarationReadsNothing = do
    reads_ <- newIORef (0 :: Int)
    let node = late (modifyIORef' reads_ (+ 1) >> pure (Just "x")) (const realNoop)
        dag = Dag.foldDag Dag.sameRepresentative (evalDeps node) :: Dag.Dag Extension
    assertEqual "one declared node" 1 (Map.size (Dag.dagNodes dag))
    assertEqual "no read at declaration" 0 =<< readIORef reads_

unresolvedFailsUp :: IO ()
unresolvedFailsUp = do
    l <- newIORef []
    ok <- runUp (late (pure Nothing) (consumer l))
    assertBool "the pass reports the failure" (not ok)
    assertEqual "nothing was built from a guess" [] =<< logged l

nestedFailureSurfaces :: IO ()
nestedFailureSurfaces = do
    l <- newIORef []
    let failing host =
            op "failing" nodeps $ \x ->
                x{ref = mkRef "deferred-spec" ("failing" :: Text, host), up = throwIO (userError "nope")}
        dependant =
            op "dependant" nodeps $ \x ->
                x{ref = mkRef "deferred-spec" ("dependant" :: Text), up = say l "dependant up"}
    ok <- runUp (dependant `inject` late (pure (Just "h")) failing)
    assertBool "the outer pass fails" (not ok)
    assertEqual "what stands on the node is not run" [] =<< logged l

downWalksSubgraph :: IO ()
downWalksSubgraph = do
    l <- newIORef []
    cell <- newIORef (Just "198.51.100.7")
    ok <- runDown (late (readIORef cell) (consumer l) `inject` producer l cell)
    assertBool "the teardown succeeds" ok
    assertEqual
        "the sub-graph goes down while the value can still be read"
        ["consumer down 198.51.100.7", "producer down"]
        =<< logged l

downWithoutValue :: IO ()
downWithoutValue = do
    l <- newIORef []
    cell <- newIORef Nothing
    ok <- runDown (late (readIORef cell) (consumer l) `inject` producer l cell)
    assertBool "not a failure" ok
    assertEqual "the producer is still torn down" ["producer down"] =<< logged l

-------------------------------------------------------------------------------

shorthands :: Op -> [Text]
shorthands o = [act.shorthand | act <- Map.elems (Dag.dagNodes dag)]
  where
    dag = Dag.foldDag Dag.sameRepresentative (evalDeps o) :: Dag.Dag Extension

readingHostShape :: IO ()
readingHostShape = do
    reads_ <- newIORef (0 :: Int)
    let node =
            VmProvision.provisionedVmReadingHost
                silent
                ignoreTrack
                ignoreTrack
                (modifyIORef' reads_ (+ 1) >> pure (Just "198.51.100.7"))
                (\_ cfg -> cfg)
                (vmConfig "")
        names = shorthands node
    assertBool "the instance is an ordinary node" ("gcp-instance" `elem` names)
    assertBool "the CA is published by an ordinary node" ("gcp-metadata-ssh-ca" `elem` names)
    assertBool "the hand-off is one deferred node" ("deferred" `elem` names)
    assertBool "no ssh probe is declared: it would have to name a host" ("gcp-ssh-available" `notElem` names)
    assertEqual "the address is not read at declaration" 0 =<< readIORef reads_

knownHostShape :: IO ()
knownHostShape = do
    let names = shorthands (VmProvision.provisionedVm silent ignoreTrack ignoreTrack (vmConfig "198.51.100.7"))
    assertBool "the ssh probe is a declared node" ("gcp-ssh-available" `elem` names)
    assertBool "nothing is deferred" ("deferred" `notElem` names)

vmConfig :: Text -> VmProvision.VmProvisionConfig ()
vmConfig host =
    VmProvision.VmProvisionConfig
        { VmProvision.vmp_name = "toy-vm"
        , VmProvision.vmp_instance = inst
        , VmProvision.vmp_ca = Keys.SSHKeyPair Keys.ED25519 "/tmp/w/keys" "ca"
        , VmProvision.vmp_clientIdentity = Keys.SSHKeyPair Keys.ED25519 "/tmp/w/keys" "client"
        , VmProvision.vmp_sshUser = "deployer"
        , VmProvision.vmp_sshHost = host
        , VmProvision.vmp_sshPort = 22
        , VmProvision.vmp_prerequisites = []
        , VmProvision.vmp_beforeCall = const []
        , VmProvision.vmp_remoteDir = "/home/deployer"
        , VmProvision.vmp_selfPath = Self.SelfPath "/tmp/w/self"
        , VmProvision.vmp_directiveTrack = Track (const realNoop)
        , VmProvision.vmp_directive = ()
        }
  where
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
            , Compute.instanceMetadata = mempty
            , Compute.instanceMetadataFiles = mempty
            , Compute.instanceExternalAddress = Compute.ReservedExternal "toy-ip"
            , Compute.instanceInternalAddress = Compute.EphemeralInternal
            , Compute.instanceTags = []
            , Compute.instancePower = Compute.PoweredOn
            }
