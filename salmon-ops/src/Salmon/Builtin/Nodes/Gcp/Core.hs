{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Core (
    -- * GCP identity
    Project (..),
    Zone (..),
    Region (..),

    -- * gcloud binary
    gcloud,

    -- * Application Default Credentials
    applicationDefaultCredentials,
    interpretAdc,
    printAccessToken,
    Report (..),
    GcloudCommand (..),
    gcloudCommand,

    -- * eventual consistency
    retryingIO,
    afterEnableRetries,
    afterEnableDelay,

    -- * CLI helpers
    gcloudProc,
    withProject,
    withZone,
    withRegion,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.IO.Exception (ExitCode (..))
import System.IO.Error (userError)
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
        pure $ interpretAdc code

-- | The verdict drawn from @gcloud auth application-default
-- print-access-token@'s exit code, split out for testability.
interpretAdc :: ExitCode -> CheckResult
interpretAdc ExitSuccess = Success
interpretAdc (ExitFailure n) = Failure ("gcloud ADC not available (exit " <> Text.pack (show n) <> ")")

{- | Fetches a fresh OAuth2 access token for the active gcloud identity
(ADC, unless a service account or user has been separately configured).

This is the credential a container registry expects for username
@oauth2accesstoken@ -- see "SreBox.Gcp.CloudRunDeploy", which feeds this
straight into "Salmon.Builtin.Nodes.Podman".@login@'s @IO Text@ password
argument so a fresh token is fetched right when @up@ runs rather than
baked into the graph when it was built (tokens like this are short-lived,
typically ~1h).
-}
printAccessToken :: IO Text
printAccessToken = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (gcloudProc ["auth", "print-access-token"])
            ""
    case code of
        ExitSuccess -> pure (Text.strip (Text.decodeUtf8 out))
        ExitFailure n ->
            throwIO (userError ("gcloud auth print-access-token failed (exit " <> show n <> "): " <> Text.unpack (Text.decodeUtf8With TextError.lenientDecode err)))

-------------------------------------------------------------------------------
-- Eventual consistency

{- | Runs an action, retrying on failure with a fixed delay, rethrowing the
last failure.

GCP grants access asynchronously, past the point where the thing granting it
reports success. Two cases hit this tree, both found by
@salmon-apps@'s @salmon-gcp-toy@ against a real project:

* a __freshly enabled API__ answers @PERMISSION_DENIED ... (or it may not
  exist)@ to the very next create, for up to about a minute, even for a
  project owner;
* a __freshly created service account__ is not yet resolvable by the service
  owning the resource a binding names it on.

So a node whose @up@ can run moments after such a grant retries rather than
failing the whole traversal, since a one-shot driver has no other way to
wait. A genuine permission error still fails the node, just later.
-}
retryingIO :: Int -> Int -> IO () -> IO ()
retryingIO attempts delay act = do
    result <- try act
    case result of
        Right () -> pure ()
        Left (e :: SomeException)
            | attempts <= 1 -> throwIO e
            | otherwise -> threadDelay delay >> retryingIO (attempts - 1) delay act

-- | Attempts for a create that may follow an API enablement: 6 over ~50s.
afterEnableRetries :: Int
afterEnableRetries = 6

-- | Delay between those attempts.
afterEnableDelay :: Int
afterEnableDelay = 10000000

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
