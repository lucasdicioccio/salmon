{-# LANGUAGE OverloadedStrings #-}

{- | Enabling GCP APIs (@gcloud services enable ...@) -- the prerequisite
almost every other 'Salmon.Builtin.Nodes.Gcp' node silently assumes (Cloud
Run, Artifact Registry, and IAM all 404 a caller who hasn't flipped the
corresponding service on for the project first). Split out on its own
rather than folded into "Salmon.Builtin.Nodes.Gcp.Core" because a project's
set of enabled services is itself just data -- one node per API, so a
recipe depends on exactly the services it needs.
-}
module Salmon.Builtin.Nodes.Gcp.ServiceUsage (
    Api (..),
    enableService,
    interpretServiceList,
    Report (..),
    ServiceUsageCommand (..),
    serviceUsageCommand,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunServiceUsageCommand !ServiceUsageCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | A GCP service/API identifier, e.g. @run.googleapis.com@.
newtype Api = Api {apiName :: Text}
    deriving (Eq, Ord, Show)

-- | Idempotently enables an API on a project.
enableService :: Reporter Report -> Track' (Binary "gcloud") -> Project -> Api -> Op
enableService r gcloudTrack project api =
    withBinary gcloudTrack serviceUsageCommand (ServicesEnable project api) $ \up ->
        op "gcp-service-enable" nodeps $ \actions ->
            actions
                { help = Text.unwords ["enables the", api.apiName, "API"]
                , ref = mkRef "gcp-service-enable" (project.projectId, api.apiName)
                , up = up r'
                , check = checkEnabled
                }
  where
    r' = contramap (RunServiceUsageCommand (ServicesEnable project api)) r

    checkEnabled :: IO CheckResult
    checkEnabled = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare serviceUsageCommand (ServicesList project api))
                ""
        pure $ interpretServiceList api code (Text.decodeUtf8 out)

{- | The verdict drawn from @gcloud services list --enabled@'s exit code and
output, split out for testability. @--filter@ narrows the listing to the
API in question, so any non-empty output line naming it means it's enabled.
-}
interpretServiceList :: Api -> ExitCode -> Text -> CheckResult
interpretServiceList _api (ExitFailure n) _outText =
    Failure ("could not list enabled services (exit " <> Text.pack (show n) <> ")")
interpretServiceList api ExitSuccess outText =
    if Text.isInfixOf api.apiName outText
        then Success
        else Failure ("service not enabled: " <> api.apiName)

-------------------------------------------------------------------------------

data ServiceUsageCommand
    = ServicesEnable Project Api
    | ServicesList Project Api
    deriving (Show)

serviceUsageCommand :: Command "gcloud" ServiceUsageCommand
serviceUsageCommand = Command $ \cmd -> case cmd of
    ServicesEnable project api ->
        gcloudProc $
            withProject project
                [ "services"
                , "enable"
                , Text.unpack api.apiName
                ]
    ServicesList project api ->
        gcloudProc $
            withProject project
                [ "services"
                , "list"
                , "--enabled"
                , "--filter"
                , "config.name:" <> Text.unpack api.apiName
                ]
