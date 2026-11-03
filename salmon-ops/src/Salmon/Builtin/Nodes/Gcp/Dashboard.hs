{-# LANGUAGE OverloadedStrings #-}

{- | A Cloud Monitoring dashboard, declared as the API's dashboard JSON.

A dashboard gets a __server-assigned id__, so the identity is the __display
name__ within the project, as for the alert policies of
"Salmon.Builtin.Nodes.Gcp.Monitoring".

It is compared by a __fingerprint__, as a policy is: the declared body and
display name hashed into a @salmon-fingerprint@ label. The API's own
representation of a dashboard carries fields a declaration never wrote
(an @etag@, the widgets' generated ids and defaults), so comparing the body
read back would never settle. A dashboard edited in the console keeps its
labels and so goes unseen until the declaration changes; that is the known
limit of a fingerprint.

The dashboard also carries the 'Monitoring.ownerLabelKey' label. One found
by display name without it was not made by salmon: @check@ says so, @up@
refuses to replace it, @down@ leaves it.

An update replaces the whole dashboard and must name the @etag@ it replaces
(the API refuses a stale one), which @up@ takes from the listing it has just
read. The body goes on gcloud's standard input (@--config-from-file -@), not
on argv: nothing in it is secret, but a dashboard can be larger than one
argument may be.

Needs @monitoring.googleapis.com@ enabled ('Monitoring.monitoringApi').
-}
module Salmon.Builtin.Nodes.Gcp.Dashboard (
    Dashboard (..),
    dashboard,
    dashboardProblems,
    renderDashboard,
    dashboardFingerprint,
    FoundDashboard (..),
    lookupDashboard,
    interpretDashboardList,
    Report (..),
    DashboardCommand (..),
    dashboardCommand,
) where

import Control.Exception (throwIO)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.IO.Error (userError)
import System.Process.ByteString (readCreateProcessWithExitCode)
import Text.Printf (printf)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), untrackedExec, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import Salmon.Builtin.Nodes.Gcp.Monitoring (Lookup (..), fingerprintLabel, firstLine, nestedTextField, ownerLabelKey, ownerLabelValue, shortId, textField)
import qualified Data.Aeson.Key as Key
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunDashboardCommand !DashboardCommand !Binary.Report
    deriving (Show)

data Dashboard = Dashboard
    { dbProject :: Project
    , dbDisplayName :: Text
    -- ^ the identity, see the module header
    , dbBody :: Value
    -- ^ the dashboard object without @displayName@, @labels@, @etag@ or
    -- @name@ (which this module sets), e.g. @{"mosaicLayout": {...}}@
    }
    deriving (Eq, Show)

-- | What @up@ refuses before any command: a body that is not an object, or
-- one setting a field this module owns.
dashboardProblems :: Dashboard -> [Text]
dashboardProblems d = case d.dbBody of
    Object o ->
        [ "the body must not set " <> k
        | k <- ["displayName", "labels", "etag", "name"]
        , KeyMap.member (Key.fromText k) o
        ]
    _ -> ["the body must be a JSON object"]

{- | A dashboard, found by display name. @up@ creates it, or replaces a
salmon-labelled one whose fingerprint differs; @down@ deletes a
salmon-labelled one.
-}
dashboard :: Reporter Report -> Track' (Binary "gcloud") -> Dashboard -> Op
dashboard r gcloudTrack d =
    withBinary gcloudTrack dashboardCommand (DashboardsList d.dbProject) $ \_list ->
        op "gcp-monitoring-dashboard" nodeps $ \actions ->
            actions
                { help = Text.unwords ["monitoring dashboard", d.dbDisplayName]
                , notes = ["fingerprint: " <> dashboardFingerprint d]
                , ref = mkRef "gcp-monitoring-dashboard" (d.dbProject.projectId, d.dbDisplayName)
                , up = upDashboard
                , down = downDashboard
                , check = interpretDashboardList d <$> list
                }
  where
    run :: DashboardCommand -> ByteString -> IO ()
    run cmd stdin = untrackedExec dashboardCommand cmd stdin (contramap (RunDashboardCommand cmd) r)

    list :: IO (ExitCode, ByteString, ByteString)
    list = readCreateProcessWithExitCode (prepare dashboardCommand (DashboardsList d.dbProject)) ""

    refuse :: Text -> IO a
    refuse why = throwIO (userError (Text.unpack ("dashboard " <> d.dbDisplayName <> ": " <> why)))

    upDashboard = do
        case dashboardProblems d of
            [] -> pure ()
            problems -> refuse (Text.intercalate "; " problems)
        (code, out, err) <- list
        case lookupDashboard d code out err of
            LookupFailed why -> refuse ("listing dashboards failed: " <> why)
            Absent -> run (DashboardsCreate d.dbProject) (encodeStrict (renderDashboard d Nothing))
            Present found
                | not found.fdOwned -> refuse "exists without salmon's label, so salmon did not make it; not replacing it"
                | found.fdFingerprint == Just (dashboardFingerprint d) -> pure ()
                | otherwise ->
                    run (DashboardsUpdate d.dbProject (shortId found.fdName)) (encodeStrict (renderDashboard d found.fdEtag))

    downDashboard = do
        (code, out, err) <- list
        case lookupDashboard d code out err of
            Present found | found.fdOwned -> run (DashboardsDelete d.dbProject (shortId found.fdName)) ""
            _ -> pure ()

encodeStrict :: Value -> ByteString
encodeStrict = LByteString.toStrict . Aeson.encode

{- | The dashboard JSON gcloud reads: the body, the display name, the owner
and fingerprint labels, and the etag being replaced when there is one.
-}
renderDashboard :: Dashboard -> Maybe Text -> Value
renderDashboard d etag = case d.dbBody of
    Object o ->
        Object
            ( KeyMap.insert "displayName" (String d.dbDisplayName)
                . KeyMap.insert "labels" (object [Key.fromText ownerLabelKey .= ownerLabelValue, Key.fromText fingerprintLabel .= dashboardFingerprint d])
                . maybe id (KeyMap.insert "etag" . String) etag
                $ o
            )
    other -> other

-- | A hash of the display name and body, 16 lowercase hex characters (a label
-- value's alphabet).
dashboardFingerprint :: Dashboard -> Text
dashboardFingerprint d =
    Text.pack (take 16 (concatMap (printf "%02x") (ByteString.unpack digest)))
  where
    digest = SHA256.hash (encodeStrict (object ["displayName" .= d.dbDisplayName, "body" .= d.dbBody]))

data FoundDashboard = FoundDashboard
    { fdName :: Text
    -- ^ @projects\/NUMBER\/dashboards\/ID@
    , fdEtag :: Maybe Text
    , fdOwned :: Bool
    , fdFingerprint :: Maybe Text
    }
    deriving (Eq, Show)

-- | Pure: what @gcloud monitoring dashboards list --format json@ said about
-- the declared dashboard.
lookupDashboard :: Dashboard -> ExitCode -> ByteString -> ByteString -> Lookup FoundDashboard
lookupDashboard d code out err =
    case code of
        ExitFailure n -> LookupFailed (Text.pack ("exit " <> show n) <> firstLine err)
        ExitSuccess -> case Aeson.decodeStrict' out of
            Just (Array items) | (item : _) <- filter named (toList items) ->
                Present
                    FoundDashboard
                        { fdName = fromMaybe "" (textField "name" item)
                        , fdEtag = textField "etag" item
                        , fdOwned = nestedTextField "labels" ownerLabelKey item == Just ownerLabelValue
                        , fdFingerprint = nestedTextField "labels" fingerprintLabel item
                        }
            Just (Array _) -> Absent
            _ -> LookupFailed "unparseable list output"
  where
    named item = textField "displayName" item == Just d.dbDisplayName

interpretDashboardList :: Dashboard -> (ExitCode, ByteString, ByteString) -> CheckResult
interpretDashboardList d (code, out, err) =
    case lookupDashboard d code out err of
        LookupFailed why -> Failure ("cannot list dashboards: " <> why)
        Absent -> Failure ("no dashboard named " <> d.dbDisplayName)
        Present found
            | not found.fdOwned -> Failure ("dashboard " <> d.dbDisplayName <> " exists without salmon's label")
            | found.fdFingerprint == Just (dashboardFingerprint d) -> Success
            | otherwise -> Failure ("dashboard " <> d.dbDisplayName <> " differs from its declaration")

-------------------------------------------------------------------------------

data DashboardCommand
    = DashboardsList Project
    | -- | the dashboard JSON is read on standard input
      DashboardsCreate Project
    | -- | replace the dashboard with this id; JSON (with the etag) on stdin
      DashboardsUpdate Project Text
    | DashboardsDelete Project Text
    deriving (Show)

dashboardCommand :: Command "gcloud" DashboardCommand
dashboardCommand = Command $ \cmd -> case cmd of
    DashboardsList project ->
        gcloudProc $ withProject project ["monitoring", "dashboards", "list", "--format", "json"]
    DashboardsCreate project ->
        gcloudProc $ withProject project ["monitoring", "dashboards", "create", "--config-from-file", "-"]
    DashboardsUpdate project dashId ->
        gcloudProc $ withProject project ["monitoring", "dashboards", "update", Text.unpack dashId, "--config-from-file", "-"]
    DashboardsDelete project dashId ->
        gcloudProc $ withProject project ["monitoring", "dashboards", "delete", Text.unpack dashId, "--quiet"]
