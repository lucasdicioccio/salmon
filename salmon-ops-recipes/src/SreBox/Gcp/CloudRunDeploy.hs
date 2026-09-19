{- | Bootstraps a podman image onto CloudRun: builds it, pushes it to
Artifact Registry, and deploys it -- the piece
"Salmon.Builtin.Nodes.Gcp.CloudRun".@cloudRunService@ explicitly says it
does not do itself ("a CloudRun service node does not build or push
images. It depends on an upstream node that pushes a Podman-built image to
Artifact Registry"), and which nothing before this module provided.

The image is tagged, pushed, and deployed under __one__ fully-qualified
Artifact Registry reference (see "Salmon.Builtin.Nodes.Podman".@push@'s own
note) so there is exactly one string in this whole pipeline that means "the
image" -- 'crd_image' -- rather than a local tag and a remote ref that a
caller has to keep in sync by hand.

Authentication goes through "Salmon.Builtin.Nodes.Podman".@login@ against
an explicit, caller-chosen 'Podman.AuthFile' ('crd_authFile') rather than
@gcloud auth configure-docker@'s ambient, user-global Docker credential
store: two concurrent salmon processes deploying under two different GCP
identities to the same registry would otherwise race on that one shared
file. Picking two different 'crd_authFile' paths makes that race
impossible rather than merely unlikely -- see 'Podman.AuthFile'’s own note.
-}
module SreBox.Gcp.CloudRunDeploy (
    CloudRunDeployConfig (..),
    buildPushDeploy,
    Report (..),
) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Builtin.Nodes.Gcp.Core (Project, Region)
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunArtifactRegistry !ArtifactRegistry.Report
    | RunPodman !Podman.Report
    | RunCloudRun !CloudRun.Report
    deriving (Show)

-------------------------------------------------------------------------------

{- | Everything needed to go from a Containerfile to a running CloudRun
revision.
-}
data CloudRunDeployConfig = CloudRunDeployConfig
    { crd_repo :: ArtifactRegistry.ArtifactRepo
    -- ^ the Artifact Registry repository the image is pushed to; created
    -- (idempotently) as part of this pipeline rather than assumed to exist.
    , crd_authFile :: Podman.AuthFile
    -- ^ where podman login writes credentials for this deploy's push --
    -- give two concurrent deploys (e.g. two tenants, or two identities
    -- against the same registry) two different paths.
    , crd_containerfile :: FS.File "containerfile"
    , crd_image :: Text
    -- ^ the fully-qualified image reference, e.g.
    -- @us-docker.pkg.dev\/my-project\/my-repo\/my-image:my-tag@ -- used
    -- unchanged as the podman build tag, the podman push target, and
    -- 'CloudRun.crsImage'.
    , crd_service :: Text
    , crd_project :: Project
    , crd_region :: Region
    , crd_env :: Map Text Text
    , crd_serviceAccount :: Text
    , crd_ingress :: CloudRun.IngressSetting
    , crd_maxInstances :: Maybe Int
    , crd_options :: CloudRun.CloudRunOptions
    -- ^ secrets, cpu/memory, concurrency, port; 'CloudRun.defaultCloudRunOptions'
    -- is the deploy this recipe made before any of them existed.
    }

-- | Builds, pushes, and deploys 'crd_image' as 'crd_service'.
buildPushDeploy ::
    Reporter Report ->
    Track' (Binary "gcloud") ->
    Track' (Binary "podman") ->
    CloudRunDeployConfig ->
    Op
buildPushDeploy r gcloudTrack podmanTrack cfg =
    op "gcp-cloudrun-deploy" (deps [deployed]) $ \actions ->
        actions
            { help = Text.unwords ["builds, pushes and deploys", cfg.crd_image, "as CloudRun service", cfg.crd_service]
            , ref = mkRef "gcp-cloudrun-deploy" (cfg.crd_service, cfg.crd_image)
            }
  where
    rAr = contramap RunArtifactRegistry r
    rPodman = contramap RunPodman r
    rCloudRun = contramap RunCloudRun r

    -- Artifact Registry's docker/podman-facing hostname for this repo's region.
    registry :: Podman.Registry
    registry = Podman.Registry (cfg.crd_region.regionName <> "-docker.pkg.dev")

    repo :: Op
    repo = ArtifactRegistry.artifactRepository rAr gcloudTrack cfg.crd_repo

    loggedIn :: Op
    loggedIn =
        Podman.login rPodman podmanTrack cfg.crd_authFile registry (Podman.Username "oauth2accesstoken") Core.printAccessToken
            `inject` repo

    built :: Op
    built = Podman.buildImage rPodman podmanTrack cfg.crd_containerfile cfg.crd_image

    pushed :: Op
    pushed =
        Podman.push rPodman podmanTrack (Just cfg.crd_authFile) cfg.crd_image
            `inject` built
            `inject` loggedIn

    deployed :: Op
    deployed =
        CloudRun.cloudRunService
            rCloudRun
            gcloudTrack
            ( CloudRun.CloudRunService
                { CloudRun.crsName = cfg.crd_service
                , CloudRun.crsProject = cfg.crd_project
                , CloudRun.crsRegion = cfg.crd_region
                , CloudRun.crsImage = cfg.crd_image
                , CloudRun.crsEnv = cfg.crd_env
                , CloudRun.crsServiceAccount = cfg.crd_serviceAccount
                , CloudRun.crsIngress = cfg.crd_ingress
                , CloudRun.crsMaxInstances = cfg.crd_maxInstances
                , CloudRun.crsOptions = cfg.crd_options
                }
            )
            `inject` pushed
