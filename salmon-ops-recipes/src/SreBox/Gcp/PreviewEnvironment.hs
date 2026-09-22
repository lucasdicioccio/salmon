{- | Composes several 'CloudRunDeploy.CloudRunDeployConfig's (api, proxy,
webapp -- or however many a caller has) into __one__ named node: a preview
environment.

"SreBox.Gcp.CloudRunDeploy".@buildPushDeploy@ already builds, pushes and
deploys a single service. Nothing before this module gave a caller one
thing to point @run up@/@run down@/@run tree@ at for "every service this
branch's preview needs" -- each service was its own independent 'Op', so
standing one up or tearing one down meant naming each of them by hand and
keeping that list in sync by hand too.

This is deliberately thin and deliberately generic: it knows nothing about
which services a preview environment needs are called api/proxy/webapp, or
about koli. It is a list of named deploys plus an environment name, folded
into one 'Op' whose dependencies are exactly those deploys. Teardown falls
out of the ordinary DAG semantics ("Salmon.Actions.UpDown": a node comes
down only after everything depending on it has) -- @run down@ against the
environment's own ref tears down every service it composes, in one pass,
provided nothing else in the graph still depends on one of them.

The environment name (typically derived from a git branch -- see the
sibling koli repo's preview-env glue script) is folded into every member
service's own name by the __caller__, not by this module: 'CloudRunDeployConfig'
already carries 'crd_service', and two preview environments must not collide
on one Cloud Run service name. This module only groups whatever
already-uniquely-named configs it is handed.
-}
module SreBox.Gcp.PreviewEnvironment (
    PreviewService (..),
    PreviewEnvironment (..),
    previewEnvironment,
    Report (..),
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import SreBox.Gcp.CloudRunDeploy (CloudRunDeployConfig, buildPushDeploy)
import qualified SreBox.Gcp.CloudRunDeploy as CloudRunDeploy

-------------------------------------------------------------------------------

data Report
    = RunService Text CloudRunDeploy.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | One service belonging to a preview environment, named for reporting
-- (e.g. @"api"@, @"proxy"@, @"webapp"@) -- independent of 'crd_service',
-- which is the actual Cloud Run service name and must already be unique
-- per environment.
data PreviewService = PreviewService
    { psRole :: Text
    , psDeploy :: CloudRunDeployConfig
    }

{- | A named group of services making up one short-lived preview
environment (typically one per branch/PR).

'peProject'/'peRegion' are carried here only for the node's own @ref@/@help@
text (each member config already carries its own project/region, which is
what actually gets deployed to) -- most callers will pass the same project
and region for every member, but this module does not enforce that.
-}
data PreviewEnvironment = PreviewEnvironment
    { peName :: Text
    -- ^ the environment's name, e.g. @"preview-" <> sanitizedBranch@
    , peProject :: Core.Project
    , peRegion :: Core.Region
    , peServices :: [PreviewService]
    }

-- | Builds, pushes and deploys every member service, grouped under one ref.
previewEnvironment ::
    Reporter Report ->
    Track' (Binary "gcloud") ->
    Track' (Binary "podman") ->
    PreviewEnvironment ->
    Op
previewEnvironment r gcloudTrack podmanTrack env =
    op "gcp-preview-environment" (deps (map deployOne env.peServices)) $ \actions ->
        actions
            { help = Text.unwords ["preview environment", env.peName, "(" <> Text.intercalate ", " roleNames <> ")"]
            , notes =
                [ "torn down as a unit: `run down` on this node's ref tears down every"
                    <> " service it composes, provided nothing else in the graph still"
                    <> " depends on one of them"
                ]
            , ref = mkRef "gcp-preview-environment" (env.peProject.projectId, env.peRegion.regionName, env.peName)
            }
  where
    roleNames = map psRole env.peServices

    deployOne :: PreviewService -> Op
    deployOne svc =
        buildPushDeploy (contramap (RunService svc.psRole) r) gcloudTrack podmanTrack svc.psDeploy
