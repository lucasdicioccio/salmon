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

    -- * Pulling from a GCE instance
    dockerRegistry,
    instanceLogin,
    instanceLoginWith,
    InstanceToken (..),
    parseInstanceToken,
    instanceToken,
    tokenStampPath,
    interpretTokenStamp,
    refreshMargin,
    MetadataError (..),
) where

import Control.Exception (Exception, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime)
import GHC.IO.Exception (ExitCode (..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject)
import qualified Salmon.Builtin.Nodes.Podman as Podman
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

-- | The docker-format registry of a region, as a podman 'Podman.Registry'.
dockerRegistry :: Region -> Podman.Registry
dockerRegistry region = Podman.Registry (region.regionName <> "-docker.pkg.dev")

{- | A GCE instance logging podman in to its region's registry /as the
instance's own service account/.

'configureDockerAuth' is the workstation's road: it needs @gcloud@ and
somebody's credentials. A VM has neither and needs neither -- the metadata
server hands any process on the instance an access token for the service
account the instance runs as, and Artifact Registry takes that token as the
password of the user @oauth2accesstoken@. So this is 'Podman.login' with the
metadata server as the password, and no secret is shipped to the machine.

What it adds to 'Podman.login' is a @check@. That node has none, which is
right for a credential nobody can ask after; this one can be asked after,
because the metadata server says when the token expires. The expiry is
written beside the auth file ('tokenStampPath') after a successful login, and
'interpretTokenStamp' answers 'Success' while more than 'refreshMargin' of it
is left. A one-shot @run up@ therefore logs in again only when it has to, and
under @run serve@ the credential is /tended/: without the check the node
would be parked, the token would lapse within the hour, and the next image
change would fail its pull with credentials that look present.

The margin is under the five minutes before expiry at which the metadata
server starts handing out a new token, so a login the check asked for gets a
token that outlives the margin, and the check does not fail again at once.

Needs, on the GCP side and declared elsewhere:
@roles\/artifactregistry.reader@ on the repository for the instance's service
account, and an instance whose access scopes allow it (@cloud-platform@, or
the read-only storage scope).
-}
instanceLogin :: Reporter Podman.Report -> Track' (Binary "podman") -> Podman.AuthFile -> Region -> Op
instanceLogin = instanceLoginWith instanceToken

{- | 'instanceLogin' with the token's source as an argument, so that a test
can stand in for the metadata server.

The check and the expiry stamp are 'Podman.loginExpiring''s, which this is:
they began here and moved there once a second caller needed them. See
'Podman.expiring' for why they sit on the login node alone.
-}
instanceLoginWith :: IO InstanceToken -> Reporter Podman.Report -> Track' (Binary "podman") -> Podman.AuthFile -> Region -> Op
instanceLoginWith getToken r podman authfile region =
    Podman.loginExpiring r podman authfile (dockerRegistry region) (Podman.Username "oauth2accesstoken") (credential <$> getToken)
  where
    credential :: InstanceToken -> Podman.Credential
    credential token = Podman.Credential token.tokenValue token.tokenLifetime

-- | Where the expiry of the token in an auth file is recorded.
tokenStampPath :: Podman.AuthFile -> FilePath
tokenStampPath = Podman.loginStampPath

-- | How much of a token's life must be left for it to be left alone.
refreshMargin :: NominalDiffTime
refreshMargin = Podman.loginRefreshMargin

{- | Is the recorded login still good for a pull? The stamp is the expiry in
seconds since the epoch; 'Nothing' is no stamp, or no auth file to go with it.
-}
interpretTokenStamp :: UTCTime -> Maybe Text -> CheckResult
interpretTokenStamp = Podman.interpretLoginStamp

-- | An access token and how long it is good for from when it was handed out.
data InstanceToken
    = InstanceToken
    { tokenValue :: !Text
    , tokenLifetime :: !NominalDiffTime
    }

data MetadataError
    = MetadataStatus !Int
    | MetadataUnreadable !String
    deriving (Show)

instance Exception MetadataError

-- | The metadata server's answer: @{"access_token": .., "expires_in": .., "token_type": ..}@.
parseInstanceToken :: LByteString.ByteString -> Either String InstanceToken
parseInstanceToken body = do
    value <- Aeson.eitherDecode body
    flip Aeson.parseEither value $ Aeson.withObject "token" $ \o -> do
        token <- o Aeson..: "access_token"
        seconds <- o Aeson..: "expires_in"
        if Text.null token
            then fail "empty access_token"
            else pure (InstanceToken token (fromInteger seconds))

{- | The instance's default service account's token, from the metadata
server. Only answers on a GCE instance; anywhere else the name does not
resolve and this throws, which fails the node that asked.
-}
instanceToken :: IO InstanceToken
instanceToken = do
    manager <- Http.newManager Http.defaultManagerSettings{Http.managerResponseTimeout = Http.responseTimeoutMicro 10000000}
    request <- Http.parseRequest "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
    response <- Http.httpLbs request{Http.requestHeaders = [("Metadata-Flavor", "Google")]} manager
    case Http.statusCode (Http.responseStatus response) of
        200 -> either (throwIO . MetadataUnreadable) pure (parseInstanceToken (Http.responseBody response))
        other -> throwIO (MetadataStatus other)

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
