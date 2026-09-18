{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Compute (
    MachineType (..),
    BootDisk (..),
    Instance (..),
    gceInstance,
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
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Zone (..), gcloudProc, withProject, withZone)
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
data BootDisk = BootDisk
    { bootDiskSizeGb :: Int
    , bootDiskImage :: Text
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
                            , down = delete (contramap (RunComputeCommand (InstancesDelete inst)) r)
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

data ComputeCommand
    = InstancesCreate Instance
    | InstancesDescribe Instance
    | InstancesDescribeStatus Instance
    | InstancesStart Instance
    | InstancesResume Instance
    | InstancesDelete Instance
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
                    , "--image"
                    , Text.unpack inst.instanceBootDisk.bootDiskImage
                    , "--boot-disk-size"
                    , show inst.instanceBootDisk.bootDiskSizeGb <> "GB"
                    , "--network"
                    , Text.unpack inst.instanceNetwork
                    , "--subnet"
                    , Text.unpack inst.instanceSubnet
                    ]
                )
                <> maybe [] (\sa -> ["--service-account", Text.unpack sa]) inst.instanceServiceAccount
                <> concatMap (\(k, v) -> ["--metadata", Text.unpack k <> "=" <> Text.unpack v]) (Map.toList inst.instanceMetadata)
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
