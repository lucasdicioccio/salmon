{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.CloudRun (
    IngressSetting (..),
    CloudRunService (..),
    cloudRunService,
    interpretServiceDescribe,
    Report (..),
    CloudRunCommand (..),
    cloudRunCommand,
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
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject, withRegion)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunCloudRunCommand !CloudRunCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | Ingress settings for a CloudRun service.
data IngressSetting
    = All
    | Internal
    | InternalAndLoadBalancing
    deriving (Eq, Show)

renderIngress :: IngressSetting -> Text
renderIngress All = "all"
renderIngress Internal = "internal"
renderIngress InternalAndLoadBalancing = "internal-and-cloud-load-balancing"

-- | A CloudRun service.
data CloudRunService = CloudRunService
    { crsName :: Text
    , crsProject :: Project
    , crsRegion :: Region
    , crsImage :: Text
    , crsEnv :: Map Text Text
    , crsServiceAccount :: Text
    , crsIngress :: IngressSetting
    , crsMaxInstances :: Maybe Int
    }
    deriving (Eq, Show)

-- | Deploys a CloudRun service from an image already pushed to Artifact
-- Registry.
cloudRunService :: Reporter Report -> Track' (Binary "gcloud") -> CloudRunService -> Op
cloudRunService r gcloudTrack svc =
    withBinary gcloudTrack cloudRunCommand (RunDeploy svc) $ \deploy ->
        withBinary gcloudTrack cloudRunCommand (RunDelete svc) $ \delete ->
            op "gcp-cloudrun-service" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["deploys CloudRun service", svc.crsName]
                    , ref = mkRef "gcp-cloudrun-service" (svc.crsProject.projectId, svc.crsRegion.regionName, svc.crsName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (deploy r')
                    , down = delete r'
                    , check = checkService
                    }
  where
    r' = contramap (RunCloudRunCommand (RunDeploy svc)) r

    checkService :: IO CheckResult
    checkService = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare cloudRunCommand (RunDescribe svc))
                ""
        pure $ interpretServiceDescribe svc.crsImage code (Text.decodeUtf8 out)

-- | The verdict drawn from @gcloud run services describe@'s exit code and
-- output, split out for testability.
interpretServiceDescribe :: Text -> ExitCode -> Text -> CheckResult
interpretServiceDescribe _image (ExitFailure n) _outText =
    Failure ("CloudRun service not found (exit " <> Text.pack (show n) <> ")")
interpretServiceDescribe image ExitSuccess outText =
    if image `Text.isInfixOf` outText
        then Success
        else Failure ("CloudRun service found but image does not match " <> image)

-------------------------------------------------------------------------------

data CloudRunCommand
    = RunDeploy CloudRunService
    | RunDescribe CloudRunService
    | RunDelete CloudRunService
    deriving (Show)

cloudRunCommand :: Command "gcloud" CloudRunCommand
cloudRunCommand = Command $ \cmd -> case cmd of
    RunDeploy svc ->
        gcloudProc $
            withProject svc.crsProject
                ( withRegion svc.crsRegion
                    ( [ "run"
                      , "deploy"
                      , Text.unpack svc.crsName
                      , "--image"
                      , Text.unpack svc.crsImage
                      , "--service-account"
                      , Text.unpack svc.crsServiceAccount
                      , "--ingress"
                      , Text.unpack (renderIngress svc.crsIngress)
                      ]
                        <> concatMap (\(k, v) -> ["--set-env-vars", Text.unpack k <> "=" <> Text.unpack v]) (Map.toList svc.crsEnv)
                        <> maybe [] (\n -> ["--max-instances", show n]) svc.crsMaxInstances
                    )
                )
    RunDescribe svc ->
        gcloudProc $
            withProject svc.crsProject
                ( withRegion svc.crsRegion
                    [ "run"
                    , "services"
                    , "describe"
                    , Text.unpack svc.crsName
                    ]
                )
    RunDelete svc ->
        gcloudProc $
            withProject svc.crsProject
                ( withRegion svc.crsRegion
                    [ "run"
                    , "services"
                    , "delete"
                    , Text.unpack svc.crsName
                    , "--quiet"
                    ]
                )
