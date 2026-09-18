{-# LANGUAGE OverloadedStrings #-}

{- | Creating (and deleting) a GCP project itself -- the node every other
"Salmon.Builtin.Nodes.Gcp" node sits on top of once a recipe owns the
project rather than being handed one.

Two properties of projects shape this module and are worth knowing before
using it as a sandbox:

* A deleted project is not gone: it sits in @DELETE_REQUESTED@ for ~30 days
  (restorable with @gcloud projects undelete@) and its id cannot be reused by
  anybody in that window. 'interpretProjectState' therefore reports that
  state with its own message rather than as a plain "not found", because the
  @up@ that follows is going to fail and the operator needs to know it is the
  id, not the credentials.
* Deleting a project deletes everything in it. That is exactly what makes it
  a good blast-radius bound for a throwaway validation (see
  @salmon-apps@'s @GcpToy@), and exactly why a recipe that did not create the
  project should not use this node.
-}
module Salmon.Builtin.Nodes.Gcp.ResourceManager (
    Parent (..),
    ProjectSpec (..),
    project,
    interpretProjectState,
    Report (..),
    ResourceManagerCommand (..),
    resourceManagerCommand,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunResourceManagerCommand !ResourceManagerCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | Where a project is created in the resource hierarchy.
data Parent
    = -- | bare numeric organization id
      Organization Text
    | -- | bare numeric folder id
      Folder Text
    | -- | no parent: only possible for accounts outside any organization
      NoParent
    deriving (Eq, Show)

data ProjectSpec = ProjectSpec
    { projectSpecProject :: Project
    , projectSpecParent :: Parent
    , projectSpecLabels :: Map Text Text
    -- ^ labels are the cheap way to find leaked throwaway projects later
    -- (@gcloud projects list --filter=labels.KEY=VALUE@).
    }
    deriving (Eq, Show)

-- | Creates a project if it does not exist; deletes it on 'down'.
project :: Reporter Report -> Track' (Binary "gcloud") -> ProjectSpec -> Op
project r gcloudTrack spec =
    withBinary gcloudTrack resourceManagerCommand (ProjectsCreate spec) $ \create ->
        withBinary gcloudTrack resourceManagerCommand (ProjectsDelete spec.projectSpecProject) $ \delete ->
            op "gcp-project" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates GCP project", pid]
                    , notes = ["down deletes the project and everything still in it"]
                    , ref = mkRef "gcp-project" pid
                    , up = create r'
                    , down = Core.downIfPresent checkProject (delete r')
                    , check = checkProject
                    }
  where
    pid = spec.projectSpecProject.projectId
    r' = contramap (RunResourceManagerCommand (ProjectsCreate spec)) r

    checkProject :: IO CheckResult
    checkProject = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare resourceManagerCommand (ProjectsDescribeState spec.projectSpecProject))
                ""
        pure $ interpretProjectState pid code (Text.strip (Text.decodeUtf8 out))

{- | The verdict drawn from @gcloud projects describe
--format=value(lifecycleState)@, split out for testability.
-}
interpretProjectState :: Text -> ExitCode -> Text -> CheckResult
interpretProjectState pid (ExitFailure n) _ =
    Failure ("project not found or not visible: " <> pid <> " (exit " <> Text.pack (show n) <> ")")
interpretProjectState pid ExitSuccess state =
    case state of
        "ACTIVE" -> Success
        "DELETE_REQUESTED" ->
            Failure ("project " <> pid <> " is pending deletion: its id cannot be reused for ~30 days (undelete it, or pick another id)")
        _ -> Failure ("unexpected project lifecycle state for " <> pid <> ": " <> state)

-------------------------------------------------------------------------------

data ResourceManagerCommand
    = ProjectsCreate ProjectSpec
    | ProjectsDescribeState Project
    | ProjectsDelete Project
    deriving (Show)

resourceManagerCommand :: Command "gcloud" ResourceManagerCommand
resourceManagerCommand = Command $ \cmd -> case cmd of
    ProjectsCreate spec ->
        gcloudProc $
            [ "projects"
            , "create"
            , Text.unpack spec.projectSpecProject.projectId
            ]
                <> parentArgs spec.projectSpecParent
                <> labelArgs spec.projectSpecLabels
    ProjectsDescribeState p ->
        gcloudProc
            [ "projects"
            , "describe"
            , Text.unpack p.projectId
            , "--format=value(lifecycleState)"
            ]
    ProjectsDelete p ->
        gcloudProc
            [ "projects"
            , "delete"
            , Text.unpack p.projectId
            , "--quiet"
            ]
  where
    parentArgs (Organization org) = ["--organization", Text.unpack org]
    parentArgs (Folder folder) = ["--folder", Text.unpack folder]
    parentArgs NoParent = []

    labelArgs labels
        | Map.null labels = []
        | otherwise =
            [ "--labels"
            , Text.unpack (Text.intercalate "," [k <> "=" <> v | (k, v) <- Map.toList labels])
            ]
