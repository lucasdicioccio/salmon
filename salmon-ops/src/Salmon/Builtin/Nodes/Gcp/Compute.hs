{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Compute (
    MachineType (..),
    BootDisk (..),
    Instance (..),
    gceInstance,
    Report (..),
    ComputeCommand (..),
    computeCommand,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

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
-- * 'up': create the instance if absent.
-- * 'down': delete the instance.
-- * 'check': report 'Success' if the instance is @RUNNING@.
gceInstance :: Reporter Report -> Track' (Binary "gcloud") -> Instance -> Op
gceInstance r gcloudTrack inst =
    withBinary gcloudTrack computeCommand (InstancesCreate inst) $ \create ->
        withBinary gcloudTrack computeCommand (InstancesDescribe inst) $ \describe ->
            withBinary gcloudTrack computeCommand (InstancesDelete inst) $ \delete ->
                op "gcp-instance" nodeps $ \actions ->
                    actions
                        { help = Text.unwords ["creates GCE instance", inst.instanceName]
                        , ref = mkRef "gcp-instance" inst.instanceName
                        , up = create r'
                        , down = delete r'
                        , check = checkInstance describe
                        }
  where
    r' = contramap (RunComputeCommand (InstancesCreate inst)) r

    checkInstance :: (Reporter Binary.Report -> IO ()) -> IO CheckResult
    checkInstance _describeInst = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                ( prepare
                    computeCommand
                    (InstancesDescribeStatus inst)
                )
                ""
        pure $ case code of
            ExitSuccess ->
                let status = Text.strip (Text.decodeUtf8 out)
                 in case status of
                        "RUNNING" -> Success
                        "PROVISIONING" -> Unknown
                        "STAGING" -> Unknown
                        "STOPPING" -> Unknown
                        "TERMINATED" -> Failure "instance is TERMINATED"
                        _ -> Failure ("unexpected instance status: " <> status)
            ExitFailure n ->
                Failure ("could not describe instance (exit " <> Text.pack (show n) <> ")")

-------------------------------------------------------------------------------

data ComputeCommand
    = InstancesCreate Instance
    | InstancesDescribe Instance
    | InstancesDescribeStatus Instance
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
