{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Compute (
    MachineType (..),
    BootDisk (..),
    Instance (..),
    gceInstance,
    Address (..),
    address,
    readAddress,
    interpretAddressDescribe,
    FirewallRule (..),
    firewallRule,
    interpretFirewallDescribe,
    SubnetPurpose (..),
    renderSubnetPurpose,
    Subnet (..),
    subnet,
    interpretSubnetDescribe,
    InstanceGroup (..),
    instanceGroup,
    interpretInstanceGroupDescribe,
    instanceGroupMember,
    interpretGroupMembership,
    interpretInstanceStatus,
    InstanceUpPlan (..),
    planInstanceUp,
    Report (..),
    ComputeCommand (..),
    computeCommand,
) where

import Control.Exception (throwIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.IO.Error (userError)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), Zone (..), gcloudProc, withProject, withZone)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunComputeCommand !ComputeCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | GCE machine type.
data MachineType
    = E2Medium
    | E2Standard2
    | N2Standard4
    | Custom Text
    deriving (Eq, Show)

renderMachineType :: MachineType -> Text
renderMachineType E2Medium = "e2-medium"
renderMachineType E2Standard2 = "e2-standard-2"
renderMachineType N2Standard4 = "n2-standard-4"
renderMachineType (Custom t) = t

-- | Boot disk configuration.
{- | A boot disk, named either by a specific image or by an image family (in
whichever project publishes it). A family is the usual choice: it tracks the
publisher's current image, where a name pins one that is eventually deleted.
-}
data BootDisk = BootDisk
    { bootDiskSizeGb :: Int
    , bootDiskImage :: Maybe Text
    , bootDiskImageFamily :: Maybe Text
    , bootDiskImageProject :: Maybe Text
    }
    deriving (Eq, Show)

-- | A GCE instance.
data Instance = Instance
    { instanceName :: Text
    , instanceProject :: Project
    , instanceZone :: Zone
    , instanceMachineType :: MachineType
    , instanceBootDisk :: BootDisk
    , instanceNetwork :: Text
    , instanceSubnet :: Text
    , instanceServiceAccount :: Maybe Text
    , instanceMetadata :: Map Text Text
    , instanceMetadataFiles :: Map Text FilePath
    -- ^ metadata whose value is read from a local file
    -- (@--metadata-from-file@) -- how a multi-line @startup-script@ is
    -- passed without quoting it into a single argv value.
    , instanceAddress :: Maybe Text
    -- ^ a reserved static address to attach, by name (see 'address'); an
    -- instance with none gets an ephemeral one GCP picks.
    , instanceTags :: [Text]
    }
    deriving (Eq, Show)

-- | Idempotently manages a GCE instance.
--
-- * 'up': create the instance if absent, start it if stopped (@TERMINATED@),
--   resume it if @SUSPENDED@. See 'planInstanceUp'.
-- * 'down': delete the instance.
-- * 'check': report 'Success' if the instance is @RUNNING@.
gceInstance :: Reporter Report -> Track' (Binary "gcloud") -> Instance -> Op
gceInstance r gcloudTrack inst =
    withBinary gcloudTrack computeCommand (InstancesCreate inst) $ \create ->
        withBinary gcloudTrack computeCommand (InstancesStart inst) $ \start ->
            withBinary gcloudTrack computeCommand (InstancesResume inst) $ \resume ->
                withBinary gcloudTrack computeCommand (InstancesDelete inst) $ \delete ->
                    op "gcp-instance" nodeps $ \actions ->
                        actions
                            { help = Text.unwords ["creates GCE instance", inst.instanceName]
                            , ref = mkRef "gcp-instance" (inst.instanceProject.projectId, inst.instanceZone.zoneName, inst.instanceName)
                            , up = bringUp create start resume
                            , down = Core.downIfPresent (uncurry interpretInstanceStatus <$> describeStatus) (delete (contramap (RunComputeCommand (InstancesDelete inst)) r))
                            , check = uncurry interpretInstanceStatus <$> describeStatus
                            }
  where
    rFor cmd = contramap (RunComputeCommand cmd) r

    describeStatus :: IO (ExitCode, Text)
    describeStatus = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare computeCommand (InstancesDescribeStatus inst))
                ""
        pure (code, Text.strip (Text.decodeUtf8 out))

    -- 'create' alone is what 'up' used to be, which made a stopped instance
    -- unrecoverable: the check says 'Failure', 'up' runs @create@, and
    -- @create@ refuses because the instance exists. Asking first costs one
    -- describe that the check has usually just done.
    bringUp create start resume = do
        plan <- uncurry planInstanceUp <$> describeStatus
        case plan of
            CreateInstance -> create (rFor (InstancesCreate inst))
            StartInstance -> start (rFor (InstancesStart inst))
            ResumeInstance -> resume (rFor (InstancesResume inst))
            AlreadyRunning -> pure ()
            CannotActYet status ->
                throwIO (userError ("instance " <> Text.unpack inst.instanceName <> " is " <> Text.unpack status <> "; retry once it settles"))

-- | The verdict drawn from @gcloud compute instances describe
-- --format=value(status)@, split out for testability.
interpretInstanceStatus :: ExitCode -> Text -> CheckResult
interpretInstanceStatus (ExitFailure n) _ =
    Failure ("could not describe instance (exit " <> Text.pack (show n) <> ")")
interpretInstanceStatus ExitSuccess status =
    case status of
        "RUNNING" -> Success
        "PROVISIONING" -> Unknown
        "STAGING" -> Unknown
        "STOPPING" -> Unknown
        "SUSPENDING" -> Unknown
        "REPAIRING" -> Unknown
        "TERMINATED" -> Failure "instance is TERMINATED"
        "SUSPENDED" -> Failure "instance is SUSPENDED"
        _ -> Failure ("unexpected instance status: " <> status)

-- | What 'gceInstance'\'s 'up' does given the instance's current status.
data InstanceUpPlan
    = CreateInstance
    | StartInstance
    | ResumeInstance
    | AlreadyRunning
    | -- | a transitional (or unrecognized) status: nothing safe to run now
      CannotActYet Text
    deriving (Eq, Show)

{- | Split out of 'gceInstance' for testability. A failing describe is read
as "absent": if it failed for another reason (credentials, a missing API)
the @create@ that follows fails too, and says why more clearly than a
describe would.
-}
planInstanceUp :: ExitCode -> Text -> InstanceUpPlan
planInstanceUp (ExitFailure _) _ = CreateInstance
planInstanceUp ExitSuccess status =
    case status of
        "RUNNING" -> AlreadyRunning
        "TERMINATED" -> StartInstance
        "SUSPENDED" -> ResumeInstance
        _ -> CannotActYet status

-------------------------------------------------------------------------------

{- | A reserved regional external IP.

Reserved rather than ephemeral because an ephemeral address is handed out at
instance-create time and taken back when the instance goes away, so nothing
that has to /name/ the machine (an SSH client, a DNS record, a config file)
can be written before it exists. A reserved one is a resource in its own
right: it can be created, read, attached and released on its own schedule.

It still cannot be known when the graph is /declared/ -- GCP picks the
address -- which is why 'readAddress' exists as a separate, out-of-graph
read for a driver to use between two passes.
-}
data Address = Address
    { addressName :: Text
    , addressProject :: Project
    , addressRegion :: Region
    }
    deriving (Eq, Show)

-- | Idempotently reserves a regional external IP.
address :: Reporter Report -> Track' (Binary "gcloud") -> Address -> Op
address r gcloudTrack addr =
    withBinary gcloudTrack computeCommand (AddressesCreate addr) $ \create ->
        withBinary gcloudTrack computeCommand (AddressesDelete addr) $ \delete ->
            op "gcp-address" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["reserves external IP", addr.addressName]
                    , ref = mkRef "gcp-address" (addr.addressProject.projectId, addr.addressRegion.regionName, addr.addressName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rFor' (AddressesCreate addr)))
                    , down = Core.downIfPresent checkAddress (delete (rFor' (AddressesDelete addr)))
                    , check = checkAddress
                    }
  where
    rFor' cmd = contramap (RunComputeCommand cmd) r

    checkAddress :: IO CheckResult
    checkAddress = do
        (code, out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (AddressesDescribe addr)) ""
        pure $ interpretAddressDescribe addr.addressName code (Text.strip (Text.decodeUtf8 out))

-- | The verdict drawn from @gcloud compute addresses describe
-- --format=value(address)@, split out for testability.
interpretAddressDescribe :: Text -> ExitCode -> Text -> CheckResult
interpretAddressDescribe name (ExitFailure _) _ = Failure ("address not reserved: " <> name)
interpretAddressDescribe name ExitSuccess out
    | Text.null out = Failure ("address reserved but has no IP: " <> name)
    | otherwise = Success

{- | Reads a reserved address's actual IP, outside any graph.

Deliberately not an 'Op': what GCP picked is knowable only after the address
node's @up@, while an 'Op' that needs the IP (an ssh endpoint, say) is built
before any @up@ runs. A driver that wants both therefore converges once,
calls this, and declares the rest -- see @salmon-apps@'s @GcpToy@ tier 2 and
its driver script. 'Nothing' when the address does not exist yet.
-}
readAddress :: Address -> IO (Maybe Text)
readAddress addr = do
    (code, out, _err) <-
        readCreateProcessWithExitCode (prepare computeCommand (AddressesDescribe addr)) ""
    let ip = Text.strip (Text.decodeUtf8 out)
    pure $ case code of
        ExitSuccess | not (Text.null ip) -> Just ip
        _ -> Nothing

-------------------------------------------------------------------------------

-- | An ingress firewall rule on a network, scoped to instances carrying a tag.
data FirewallRule = FirewallRule
    { firewallName :: Text
    , firewallProject :: Project
    , firewallNetwork :: Text
    , firewallAllow :: Text
    -- ^ gcloud's own @--allow@ syntax, e.g. @tcp:22@
    , firewallSourceRanges :: [Text]
    , firewallTargetTags :: [Text]
    }
    deriving (Eq, Show)

-- | Idempotently creates an ingress firewall rule.
firewallRule :: Reporter Report -> Track' (Binary "gcloud") -> FirewallRule -> Op
firewallRule r gcloudTrack fw =
    withBinary gcloudTrack computeCommand (FirewallCreate fw) $ \create ->
        withBinary gcloudTrack computeCommand (FirewallDelete fw) $ \delete ->
            op "gcp-firewall-rule" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["allows", fw.firewallAllow, "to", Text.intercalate "," fw.firewallTargetTags]
                    , ref = mkRef "gcp-firewall-rule" (fw.firewallProject.projectId, fw.firewallName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rFor'' (FirewallCreate fw)))
                    , down = Core.downIfPresent checkFirewall (delete (rFor'' (FirewallDelete fw)))
                    , check = checkFirewall
                    }
  where
    rFor'' cmd = contramap (RunComputeCommand cmd) r

    checkFirewall :: IO CheckResult
    checkFirewall = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (FirewallDescribe fw)) ""
        pure $ interpretFirewallDescribe fw.firewallName code

-- | The verdict drawn from @gcloud compute firewall-rules describe@.
interpretFirewallDescribe :: Text -> ExitCode -> CheckResult
interpretFirewallDescribe _name ExitSuccess = Success
interpretFirewallDescribe name (ExitFailure _) = Failure ("firewall rule not found: " <> name)

-------------------------------------------------------------------------------

{- | What a subnetwork is /for/, which for one kind of subnet is the whole
point of creating it.

@REGIONAL_MANAGED_PROXY@ is the proxy-only subnet a regional
@EXTERNAL_MANAGED@ Application Load Balancer runs its Envoy proxies in.
Nothing is ever placed in it by hand -- it holds no instances, and its range
is where the load balancer's connections to the backends /come from/, which
is what a backend's firewall rule has to allow. One @ACTIVE@ proxy-only
subnet may exist per network per region, and until it does, every attempt to
create such a balancer's forwarding rule fails.
-}
data SubnetPurpose
    = PrivateSubnet
    | RegionalManagedProxy
    deriving (Eq, Show)

renderSubnetPurpose :: SubnetPurpose -> Text
renderSubnetPurpose PrivateSubnet = "PRIVATE"
renderSubnetPurpose RegionalManagedProxy = "REGIONAL_MANAGED_PROXY"

-- | A subnetwork of a VPC network, in one region.
data Subnet = Subnet
    { subnetName :: Text
    , subnetProject :: Project
    , subnetRegion :: Region
    , subnetNetwork :: Text
    , subnetRange :: Text
    -- ^ CIDR. In an /auto mode/ network (which @default@ is), it must not
    -- overlap @10.128.0.0\/9@: that whole block is reserved for the subnets
    -- GCP creates per region on its own, including regions that do not exist
    -- yet.
    , subnetPurpose :: SubnetPurpose
    }
    deriving (Eq, Show)

-- | Idempotently creates a subnetwork.
subnet :: Reporter Report -> Track' (Binary "gcloud") -> Subnet -> Op
subnet r gcloudTrack net =
    withBinary gcloudTrack computeCommand (SubnetsCreate net) $ \create ->
        withBinary gcloudTrack computeCommand (SubnetsDelete net) $ \delete ->
            op "gcp-subnet" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates subnet", net.subnetName, "for", renderSubnetPurpose net.subnetPurpose]
                    , ref = mkRef "gcp-subnet" (net.subnetProject.projectId, net.subnetRegion.regionName, net.subnetName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rSub (SubnetsCreate net)))
                    , down = Core.downIfPresent checkSubnet (delete (rSub (SubnetsDelete net)))
                    , check = checkSubnet
                    }
  where
    rSub cmd = contramap (RunComputeCommand cmd) r

    checkSubnet :: IO CheckResult
    checkSubnet = do
        (code, out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (SubnetsDescribe net)) ""
        pure $ interpretSubnetDescribe net.subnetName (renderSubnetPurpose net.subnetPurpose) code (Text.strip (Text.decodeUtf8 out))

{- | The verdict drawn from @gcloud compute networks subnets describe
--format=value(purpose)@, split out for testability.

The purpose is compared rather than merely noting the subnet exists, because
a subnet of the wrong purpose is the one failure mode worth catching here: a
plain subnet answers @describe@ perfectly well and then the balancer refuses
to use it, at a point far away from this node.
-}
interpretSubnetDescribe :: Text -> Text -> ExitCode -> Text -> CheckResult
interpretSubnetDescribe name _ (ExitFailure _) _ = Failure ("subnet not found: " <> name)
interpretSubnetDescribe name wanted ExitSuccess out
    | out == wanted = Success
    -- gcloud renders an ordinary subnet's purpose as PRIVATE, but has also
    -- left it empty in the past; an empty answer is only satisfying if that
    -- is what was asked for.
    | Text.null out && wanted == "PRIVATE" = Success
    | otherwise = Failure ("subnet " <> name <> " has purpose " <> out <> ", wanted " <> wanted)

-------------------------------------------------------------------------------

{- | An /unmanaged/, zonal instance group: a bag of instances that already
exist, which is what makes it the right backend for a load balancer in front
of VMs salmon itself declared. (A managed group is the other way round -- it
creates the instances, from a template.)
-}
data InstanceGroup = InstanceGroup
    { groupName :: Text
    , groupProject :: Project
    , groupZone :: Zone
    }
    deriving (Eq, Show)

-- | Idempotently creates an unmanaged instance group.
instanceGroup :: Reporter Report -> Track' (Binary "gcloud") -> InstanceGroup -> Op
instanceGroup r gcloudTrack grp =
    withBinary gcloudTrack computeCommand (InstanceGroupsCreate grp) $ \create ->
        withBinary gcloudTrack computeCommand (InstanceGroupsDelete grp) $ \delete ->
            op "gcp-instance-group" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates unmanaged instance group", grp.groupName]
                    , ref = mkRef "gcp-instance-group" (grp.groupProject.projectId, grp.groupZone.zoneName, grp.groupName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rGrp (InstanceGroupsCreate grp)))
                    , down = Core.downIfPresent checkGroup (delete (rGrp (InstanceGroupsDelete grp)))
                    , check = checkGroup
                    }
  where
    rGrp cmd = contramap (RunComputeCommand cmd) r

    checkGroup :: IO CheckResult
    checkGroup = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (InstanceGroupsDescribe grp)) ""
        pure $ interpretInstanceGroupDescribe grp.groupName code

-- | The verdict drawn from @gcloud compute instance-groups unmanaged describe@.
interpretInstanceGroupDescribe :: Text -> ExitCode -> CheckResult
interpretInstanceGroupDescribe _name ExitSuccess = Success
interpretInstanceGroupDescribe name (ExitFailure _) = Failure ("instance group not found: " <> name)

{- | One instance's membership of an unmanaged group, as a node of its own
rather than a field of 'InstanceGroup'.

Separate because the two effects have genuinely different lifetimes and
different failure modes: the group can exist while the instance does not,
@add-instances@ is an error if the instance is already in, and a teardown
has to take the membership out before either end can go. Keeping them apart
also means the dependency edge that matters -- "the instance must exist
first" -- is expressible, which it would not be if membership were a field
of the group.
-}
instanceGroupMember :: Reporter Report -> Track' (Binary "gcloud") -> InstanceGroup -> Text -> Op
instanceGroupMember r gcloudTrack grp instName =
    withBinary gcloudTrack computeCommand (InstanceGroupsAddInstance grp instName) $ \add ->
        withBinary gcloudTrack computeCommand (InstanceGroupsRemoveInstance grp instName) $ \remove ->
            op "gcp-instance-group-member" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["adds", instName, "to instance group", grp.groupName]
                    , ref = mkRef "gcp-instance-group-member" (grp.groupProject.projectId, grp.groupZone.zoneName, grp.groupName, instName)
                    , up = add (rMem (InstanceGroupsAddInstance grp instName))
                    , down = Core.downIfPresent checkMember (remove (rMem (InstanceGroupsRemoveInstance grp instName)))
                    , check = checkMember
                    }
  where
    rMem cmd = contramap (RunComputeCommand cmd) r

    checkMember :: IO CheckResult
    checkMember = do
        (code, out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (InstanceGroupsListInstances grp)) ""
        pure $ interpretGroupMembership instName code (Text.decodeUtf8 out)

{- | The verdict drawn from @gcloud compute instance-groups list-instances
--format=value(instance)@, whose lines are full resource URLs.

Matching on the last path segment rather than by substring, so that an
instance named @web@ is not read as present because @web-canary@ is.
-}
interpretGroupMembership :: Text -> ExitCode -> Text -> CheckResult
interpretGroupMembership name (ExitFailure _) _ = Failure ("could not list the group's instances (looking for " <> name <> ")")
interpretGroupMembership name ExitSuccess out
    | name `elem` map lastSegment (Text.lines out) = Success
    | otherwise = Failure ("instance not in the group: " <> name)
  where
    lastSegment = last . Text.splitOn "/" . Text.strip

-------------------------------------------------------------------------------

data ComputeCommand
    = InstancesCreate Instance
    | InstancesDescribe Instance
    | InstancesDescribeStatus Instance
    | InstancesStart Instance
    | InstancesResume Instance
    | InstancesDelete Instance
    | AddressesCreate Address
    | AddressesDescribe Address
    | AddressesDelete Address
    | FirewallCreate FirewallRule
    | FirewallDescribe FirewallRule
    | FirewallDelete FirewallRule
    | SubnetsCreate Subnet
    | SubnetsDescribe Subnet
    | SubnetsDelete Subnet
    | InstanceGroupsCreate InstanceGroup
    | InstanceGroupsDescribe InstanceGroup
    | InstanceGroupsDelete InstanceGroup
    | InstanceGroupsAddInstance InstanceGroup Text
    | InstanceGroupsRemoveInstance InstanceGroup Text
    | InstanceGroupsListInstances InstanceGroup
    deriving (Show)

computeCommand :: Command "gcloud" ComputeCommand
computeCommand = Command $ \cmd -> case cmd of
    InstancesCreate inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "create"
                    , Text.unpack inst.instanceName
                    , "--machine-type"
                    , Text.unpack (renderMachineType inst.instanceMachineType)
                    , "--boot-disk-size"
                    , show inst.instanceBootDisk.bootDiskSizeGb <> "GB"
                    , "--network"
                    , Text.unpack inst.instanceNetwork
                    , "--subnet"
                    , Text.unpack inst.instanceSubnet
                    ]
                )
                <> maybe [] (\img -> ["--image", Text.unpack img]) inst.instanceBootDisk.bootDiskImage
                <> maybe [] (\fam -> ["--image-family", Text.unpack fam]) inst.instanceBootDisk.bootDiskImageFamily
                <> maybe [] (\proj -> ["--image-project", Text.unpack proj]) inst.instanceBootDisk.bootDiskImageProject
                <> maybe [] (\sa -> ["--service-account", Text.unpack sa]) inst.instanceServiceAccount
                <> concatMap (\(k, v) -> ["--metadata", Text.unpack k <> "=" <> Text.unpack v]) (Map.toList inst.instanceMetadata)
                <> concatMap (\(k, v) -> ["--metadata-from-file", Text.unpack k <> "=" <> v]) (Map.toList inst.instanceMetadataFiles)
                <> maybe [] (\addr -> ["--address", Text.unpack addr]) inst.instanceAddress
                <> if null inst.instanceTags then [] else ["--tags", Text.unpack (Text.intercalate "," inst.instanceTags)]
    InstancesDescribe inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "describe"
                    , Text.unpack inst.instanceName
                    ]
                )
    InstancesDescribeStatus inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "describe"
                    , Text.unpack inst.instanceName
                    , "--format=value(status)"
                    ]
                )
    InstancesStart inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "start"
                    , Text.unpack inst.instanceName
                    ]
                )
    InstancesResume inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "resume"
                    , Text.unpack inst.instanceName
                    ]
                )
    InstancesDelete inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "delete"
                    , Text.unpack inst.instanceName
                    , "--quiet"
                    ]
                )
    AddressesCreate addr ->
        gcloudProc $
            withProject addr.addressProject
                [ "compute"
                , "addresses"
                , "create"
                , Text.unpack addr.addressName
                , "--region"
                , Text.unpack addr.addressRegion.regionName
                ]
    AddressesDescribe addr ->
        gcloudProc $
            withProject addr.addressProject
                [ "compute"
                , "addresses"
                , "describe"
                , Text.unpack addr.addressName
                , "--region"
                , Text.unpack addr.addressRegion.regionName
                , "--format=value(address)"
                ]
    AddressesDelete addr ->
        gcloudProc $
            withProject addr.addressProject
                [ "compute"
                , "addresses"
                , "delete"
                , Text.unpack addr.addressName
                , "--region"
                , Text.unpack addr.addressRegion.regionName
                , "--quiet"
                ]
    FirewallCreate fw ->
        gcloudProc $
            withProject fw.firewallProject
                [ "compute"
                , "firewall-rules"
                , "create"
                , Text.unpack fw.firewallName
                , "--network"
                , Text.unpack fw.firewallNetwork
                , "--allow"
                , Text.unpack fw.firewallAllow
                , "--source-ranges"
                , Text.unpack (Text.intercalate "," fw.firewallSourceRanges)
                ]
                <> if null fw.firewallTargetTags then [] else ["--target-tags", Text.unpack (Text.intercalate "," fw.firewallTargetTags)]
    FirewallDescribe fw ->
        gcloudProc $
            withProject fw.firewallProject
                [ "compute"
                , "firewall-rules"
                , "describe"
                , Text.unpack fw.firewallName
                ]
    FirewallDelete fw ->
        gcloudProc $
            withProject fw.firewallProject
                [ "compute"
                , "firewall-rules"
                , "delete"
                , Text.unpack fw.firewallName
                , "--quiet"
                ]
    SubnetsCreate net ->
        gcloudProc $
            withProject net.subnetProject
                [ "compute"
                , "networks"
                , "subnets"
                , "create"
                , Text.unpack net.subnetName
                , "--network"
                , Text.unpack net.subnetNetwork
                , "--region"
                , Text.unpack net.subnetRegion.regionName
                , "--range"
                , Text.unpack net.subnetRange
                , "--purpose"
                , Text.unpack (renderSubnetPurpose net.subnetPurpose)
                ]
                -- a proxy-only subnet is either the region's ACTIVE one or a
                -- BACKUP held for a migration; gcloud demands the choice.
                <> case net.subnetPurpose of
                    RegionalManagedProxy -> ["--role", "ACTIVE"]
                    PrivateSubnet -> []
    SubnetsDescribe net ->
        gcloudProc $
            withProject net.subnetProject
                [ "compute"
                , "networks"
                , "subnets"
                , "describe"
                , Text.unpack net.subnetName
                , "--region"
                , Text.unpack net.subnetRegion.regionName
                , "--format=value(purpose)"
                ]
    SubnetsDelete net ->
        gcloudProc $
            withProject net.subnetProject
                [ "compute"
                , "networks"
                , "subnets"
                , "delete"
                , Text.unpack net.subnetName
                , "--region"
                , Text.unpack net.subnetRegion.regionName
                , "--quiet"
                ]
    InstanceGroupsCreate grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    ["compute", "instance-groups", "unmanaged", "create", Text.unpack grp.groupName]
                )
    InstanceGroupsDescribe grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    ["compute", "instance-groups", "unmanaged", "describe", Text.unpack grp.groupName]
                )
    InstanceGroupsDelete grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    ["compute", "instance-groups", "unmanaged", "delete", Text.unpack grp.groupName, "--quiet"]
                )
    InstanceGroupsAddInstance grp instName ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    [ "compute"
                    , "instance-groups"
                    , "unmanaged"
                    , "add-instances"
                    , Text.unpack grp.groupName
                    , "--instances"
                    , Text.unpack instName
                    ]
                )
    InstanceGroupsRemoveInstance grp instName ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    [ "compute"
                    , "instance-groups"
                    , "unmanaged"
                    , "remove-instances"
                    , Text.unpack grp.groupName
                    , "--instances"
                    , Text.unpack instName
                    ]
                )
    InstanceGroupsListInstances grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    [ "compute"
                    , "instance-groups"
                    , "list-instances"
                    , Text.unpack grp.groupName
                    , "--format=value(instance)"
                    ]
                )
