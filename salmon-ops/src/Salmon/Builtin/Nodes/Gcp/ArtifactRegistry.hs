{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.ArtifactRegistry (
    RepoFormat (..),
    ArtifactRepo (..),
    artifactRepository,
    configureDockerAuth,
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
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject, withRegion)
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
        withBinary gcloudTrack artifactRegistryCommand (ReposDescribe repo) $ \describe ->
            withBinary gcloudTrack artifactRegistryCommand (ReposDelete repo) $ \delete ->
                op "gcp-artifact-registry" nodeps $ \actions ->
                    actions
                        { help = Text.unwords ["creates Artifact Registry repository", repo.repoName]
                        , ref = mkRef "gcp-artifact-registry" repo.repoName
                        , up = create r'
                        , down = delete r'
                        , check = checkRepo describe
                        }
  where
    r' = contramap (RunArtifactRegistryCommand (ReposCreate repo)) r
    checkRepo :: (Reporter Binary.Report -> IO ()) -> IO CheckResult
    checkRepo _describe = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare artifactRegistryCommand (ReposDescribe repo))
                ""
        pure $ case code of
            ExitSuccess -> Success
            ExitFailure _ -> Failure ("repository not found: " <> repo.repoName)

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

artifactRegistryCommand :: Command "gcloud" ArtifactRegistryCommand
artifactRegistryCommand = Command $ \cmd -> case cmd of
    ReposCreate repo ->
        gcloudProc $
            withProject repo.repoProject
                ( withRegion repo.repoLocation
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
                ( withRegion repo.repoLocation
                    [ "artifacts"
                    , "repositories"
                    , "describe"
                    , Text.unpack repo.repoName
                    ]
                )
    ReposDelete repo ->
        gcloudProc $
            withProject repo.repoProject
                ( withRegion repo.repoLocation
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
