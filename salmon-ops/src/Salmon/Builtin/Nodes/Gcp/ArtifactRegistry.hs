{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.ArtifactRegistry (
    RepoFormat (..),
    ArtifactRepo (..),
    artifactRepository,
    configureDockerAuth,
    interpretRepoDescribe,
    Report (..),
    ArtifactRegistryCommand (..),
    artifactRegistryCommand,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunArtifactRegistryCommand !ArtifactRegistryCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | Format of an Artifact Registry repository.
data RepoFormat
    = Docker
    | Maven
    | Npm
    | Python
    | Apt
    | Yum
    deriving (Eq, Show)

renderRepoFormat :: RepoFormat -> Text
renderRepoFormat Docker = "docker"
renderRepoFormat Maven = "maven"
renderRepoFormat Npm = "npm"
renderRepoFormat Python = "python"
renderRepoFormat Apt = "apt"
renderRepoFormat Yum = "yum"

-- | An Artifact Registry repository.
data ArtifactRepo = ArtifactRepo
    { repoName :: Text
    , repoProject :: Project
    , repoLocation :: Region
    , repoFormat :: RepoFormat
    }
    deriving (Eq, Show)

-- | Idempotently creates an Artifact Registry repository.
artifactRepository :: Reporter Report -> Track' (Binary "gcloud") -> ArtifactRepo -> Op
artifactRepository r gcloudTrack repo =
    withBinary gcloudTrack artifactRegistryCommand (ReposCreate repo) $ \create ->
        withBinary gcloudTrack artifactRegistryCommand (ReposDelete repo) $ \delete ->
            op "gcp-artifact-registry" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates Artifact Registry repository", repo.repoName]
                    , ref = mkRef "gcp-artifact-registry" (repo.repoProject.projectId, repo.repoLocation.regionName, repo.repoName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create r')
                    , down = Core.downIfPresent checkRepo (delete r')
                    , check = checkRepo
                    }
  where
    r' = contramap (RunArtifactRegistryCommand (ReposCreate repo)) r
    checkRepo :: IO CheckResult
    checkRepo = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare artifactRegistryCommand (ReposDescribe repo))
                ""
        pure $ interpretRepoDescribe repo.repoName code

-- | The verdict drawn from @gcloud artifacts repositories describe@'s exit
-- code, split out for testability.
interpretRepoDescribe :: Text -> ExitCode -> CheckResult
interpretRepoDescribe _name ExitSuccess = Success
interpretRepoDescribe name (ExitFailure _) = Failure ("repository not found: " <> name)

-- | Configures the local docker client to authenticate with Artifact Registry.
configureDockerAuth :: Reporter Report -> Track' (Binary "gcloud") -> Project -> Region -> Op
configureDockerAuth r gcloudTrack project region =
    withBinary gcloudTrack artifactRegistryCommand (AuthConfigureDocker project region) $ \up ->
        op "gcp-docker-auth" nodeps $ \actions ->
            actions
                { help = Text.unwords ["configures docker auth for", dockerHost region]
                , ref = mkRef "gcp-docker-auth" (dockerHost region)
                , up = up r'
                }
  where
    r' = contramap (RunArtifactRegistryCommand (AuthConfigureDocker project region)) r

    dockerHost :: Region -> Text
    dockerHost rgn = rgn.regionName <> "-docker.pkg.dev"

-------------------------------------------------------------------------------

data ArtifactRegistryCommand
    = ReposCreate ArtifactRepo
    | ReposDescribe ArtifactRepo
    | ReposDelete ArtifactRepo
    | AuthConfigureDocker Project Region
    deriving (Show)

{- | @gcloud artifacts@ spells its regional flag @--location@; @--region@ is
rejected outright (@unrecognized arguments@), so this does not go through
"Salmon.Builtin.Nodes.Gcp.Core".@withRegion@ the way @run@\/@compute@ do.
-}
withLocation :: Region -> [String] -> [String]
withLocation rgn args = args <> ["--location", Text.unpack rgn.regionName]

artifactRegistryCommand :: Command "gcloud" ArtifactRegistryCommand
artifactRegistryCommand = Command $ \cmd -> case cmd of
    ReposCreate repo ->
        gcloudProc $
            withProject repo.repoProject
                ( withLocation repo.repoLocation
                    [ "artifacts"
                    , "repositories"
                    , "create"
                    , Text.unpack repo.repoName
                    , "--repository-format"
                    , Text.unpack (renderRepoFormat repo.repoFormat)
                    ]
                )
    ReposDescribe repo ->
        gcloudProc $
            withProject repo.repoProject
                ( withLocation repo.repoLocation
                    [ "artifacts"
                    , "repositories"
                    , "describe"
                    , Text.unpack repo.repoName
                    ]
                )
    ReposDelete repo ->
        gcloudProc $
            withProject repo.repoProject
                ( withLocation repo.repoLocation
                    [ "artifacts"
                    , "repositories"
                    , "delete"
                    , Text.unpack repo.repoName
                    , "--quiet"
                    ]
                )
    AuthConfigureDocker _project region ->
        gcloudProc
            [ "auth"
            , "configure-docker"
            , Text.unpack region.regionName <> "-docker.pkg.dev"
            ]
