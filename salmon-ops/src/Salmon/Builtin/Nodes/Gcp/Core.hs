{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Core (
    -- * GCP identity
    Project (..),
    Zone (..),
    Region (..),

    -- * Errors
    GcpError (..),

    -- * gcloud binary
    gcloud,

    -- * Application Default Credentials
    applicationDefaultCredentials,
    Report (..),
    GcloudCommand (..),
    gcloudCommand,

    -- * CLI helpers
    gcloudProc,
    withProject,
    withZone,
    withRegion,
) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | A GCP project identifier.
newtype Project = Project {projectId :: Text}
    deriving (Eq, Ord, Show)

-- | A GCP zone.
newtype Zone = Zone {zoneName :: Text}
    deriving (Eq, Ord, Show)

-- | A GCP region.
newtype Region = Region {regionName :: Text}
    deriving (Eq, Ord, Show)

-- | Errors raised by GCP operations.
data GcpError = GcpCliError Text Int Text
    deriving (Exception, Show)

-------------------------------------------------------------------------------

data Report
    = RunGcloud !GcloudCommand !Binary.Report
    | RunAdc !Binary.Report
    deriving (Show)

-- | The various gcloud invocations that 'Core' knows how to run.
data GcloudCommand
    = AdcPrintAccessToken
    deriving (Show)

-- | Builds a 'CreateProcess' for a gcloud invocation.
gcloudCommand :: Command "gcloud" GcloudCommand
gcloudCommand = Command $ \cmd -> case cmd of
    AdcPrintAccessToken ->
        gcloudProc ["auth", "application-default", "print-access-token"]

-- | A provider for the @gcloud@ binary. For Phase 1 we assume @gcloud@ is on
-- @PATH@; callers can override with a real installer if they prefer.
gcloud :: Track' (Binary "gcloud")
gcloud = Track $ \_ ->
    op "gcloud" nodeps $ \actions ->
        actions
            { help = "gcloud CLI on PATH"
            , ref = mkRef "gcloud" ("gcloud" :: Text)
            }

-- | Validates Application Default Credentials. Almost every other GCP op
-- should depend on this node.
applicationDefaultCredentials :: Reporter Report -> Track' (Binary "gcloud") -> Op
applicationDefaultCredentials r gcloudTrack =
    withBinary gcloudTrack gcloudCommand AdcPrintAccessToken $ \up ->
        op "gcp-adc" nodeps $ \actions ->
            actions
                { help = "validates GCP Application Default Credentials"
                , ref = mkRef "gcp-adc" ("application-default-credentials" :: Text)
                , up = up r'
                , check = checkAdc
                }
  where
    r' = contramap RunAdc r

    checkAdc :: IO CheckResult
    checkAdc = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (gcloudProc ["auth", "application-default", "print-access-token"])
                ""
        pure $ case code of
            ExitSuccess -> Success
            ExitFailure n -> Failure ("gcloud ADC not available (exit " <> Text.pack (show n) <> ")")

-------------------------------------------------------------------------------
-- CLI helpers

-- | A bare @gcloud@ process with the given sub-command arguments.
gcloudProc :: [String] -> CreateProcess
gcloudProc args = proc "gcloud" args

-- | Append @--project@ to a gcloud argument list.
withProject :: Project -> [String] -> [String]
withProject p args = args <> ["--project", Text.unpack p.projectId]

-- | Append @--zone@ to a gcloud argument list.
withZone :: Zone -> [String] -> [String]
withZone z args = args <> ["--zone", Text.unpack z.zoneName]

-- | Append @--region@ to a gcloud argument list.
withRegion :: Region -> [String] -> [String]
withRegion rgn args = args <> ["--region", Text.unpack rgn.regionName]
