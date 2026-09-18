{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A throwaway, tiered exercise of the "Salmon.Builtin.Nodes.Gcp" builtins
against a real GCP project, meant to be run in a sandbox organization or
billing account where deleting everything afterwards is the point.

Tiers are cumulative and ordered by cost:

* __tier 0__ (≈free): project (optional), billing link, APIs, a bucket, a
  service account, IAM bindings on both, an Artifact Registry repository.
* __tier 1__ (cents): build an image with podman, push it to the
  repository, deploy it to Cloud Run as that service account. The image is
  either a one-line @FROM --base-image@ (Google's hello sample by default) or
  the caller's own @--containerfile@, built with that file's directory as
  context; either way it must serve HTTP on @$PORT@, as Cloud Run requires.

When the project is created by this binary (the default), it is the deepest
node of the graph, so @run down@ tears every resource down individually
first -- which is what is being validated -- and then deletes the project,
which sweeps whatever a buggy @down@ left behind. A failed @down@ leaves the
project standing (its dependants were not all removed), which is the signal
to go and look.

See @salmon-apps/scripts/gcp-toy-validate.sh@ for the up → up → down driver.
-}
module GcpToy (
    main,
    Seed (..),
    Spec (..),
    ParentRef (..),
    ImageSource (..),
    defaultBaseImage,
    configure,
    program,
) where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isDigit)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative (auto, execParser, flag', fullDesc, header, helper, info, long, metavar, option, optional, progDesc, strOption, switch, value, (<**>), (<|>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Directory (doesFileExist, makeAbsolute)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.Billing as Billing
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.ResourceManager as ResourceManager
import qualified Salmon.Builtin.Nodes.Gcp.ServiceUsage as ServiceUsage
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

import qualified SreBox.Gcp.CloudRunDeploy as CloudRunDeploy

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Tiered throwaway validation of salmon's GCP builtins" <> header "salmon-gcp-toy"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd

-------------------------------------------------------------------------------
-- Seed

data SeedParent = SeedOrganization Text | SeedFolder Text | SeedNoParent | SeedExistingProject
    deriving (Eq, Show)

data Seed = Seed
    { seedProject :: Text
    , seedParent :: SeedParent
    , seedBillingAccount :: Maybe Text
    , seedRegion :: Text
    , seedTier :: Int
    , seedPrefix :: Text
    , seedImageTag :: Text
    , seedImageSource :: ImageSource
    , seedWorkDir :: FilePath
    }
    deriving (Eq, Show)

instance ParseRecord Seed where
    parseRecord =
        build <**> helper
      where
        build =
            Seed
                <$> strOption (long "project" <> metavar "PROJECT_ID" <> Opt.help "project id to create (or use, with --existing-project)")
                <*> parent
                <*> optional (strOption (long "billing-account" <> metavar "XXXXXX-XXXXXX-XXXXXX" <> Opt.help "billing account to link; required unless --existing-project"))
                <*> strOption (long "region" <> value "europe-west1" <> Opt.help "region for the bucket, repository and Cloud Run service")
                <*> option auto (long "tier" <> value 0 <> Opt.help "0: storage/iam/registry; 1: also build+push+deploy to Cloud Run (needs podman)")
                <*> strOption (long "prefix" <> value "salmon-toy" <> Opt.help "prefix for every resource name")
                <*> strOption (long "image-tag" <> value "v1" <> Opt.help "tier 1 image tag; bump it to exercise a redeploy")
                <*> imageSourceP
                <*> strOption (long "workdir" <> value "gcp-toy-work" <> Opt.help "local directory for the Containerfile and podman auth file")
        -- xor: once one branch has matched, the other flag is rejected by the parser
        imageSourceP =
            (FromContainerfile <$> strOption (long "containerfile" <> metavar "PATH" <> Opt.help "tier 1: build this Containerfile, with its directory as build context"))
                <|> (FromBaseImage <$> strOption (long "base-image" <> metavar "IMAGE" <> value defaultBaseImage <> Opt.showDefault <> Opt.help "tier 1: build `FROM IMAGE`"))
        parent =
            (SeedOrganization <$> strOption (long "organization" <> metavar "ORG_ID" <> Opt.help "create the project under this organization"))
                <|> (SeedFolder <$> strOption (long "folder" <> metavar "FOLDER_ID" <> Opt.help "create the project under this folder"))
                <|> flag' SeedExistingProject (long "existing-project" <> Opt.help "do not create, link or delete the project")
                <|> (const SeedNoParent <$> switch (long "no-parent" <> Opt.help "create the project with no parent (default)"))

-------------------------------------------------------------------------------
-- Spec

data ParentRef = OrganizationParent Text | FolderParent Text | NoParentRef
    deriving (Eq, Show, Generic)

instance FromJSON ParentRef
instance ToJSON ParentRef

-- | What tier 1 builds.
data ImageSource
    = -- | a generated one-line Containerfile, @FROM@ this image
      FromBaseImage Text
    | -- | the caller's own Containerfile (absolute once configured)
      FromContainerfile FilePath
    deriving (Eq, Show, Generic)

instance FromJSON ImageSource
instance ToJSON ImageSource

-- | Google's Cloud Run sample: a tiny server answering on @$PORT@.
defaultBaseImage :: Text
defaultBaseImage = "us-docker.pkg.dev/cloudrun/container/hello"

data Spec = Spec
    { project :: Text
    , createProjectUnder :: Maybe ParentRef
    -- ^ 'Nothing': the project pre-exists and is left alone
    , billingAccount :: Maybe Text
    , region :: Text
    , tier :: Int
    , prefix :: Text
    , imageTag :: Text
    , imageSource :: ImageSource
    , workDir :: FilePath
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> do
    let creating = seed.seedParent /= SeedExistingProject
    when (creating && seed.seedBillingAccount == Nothing) $
        fail "--billing-account is required when the project is created (pass --existing-project to use one as-is)"
    when (seed.seedTier < 0 || seed.seedTier > 1) $
        fail "--tier must be 0 or 1"
    -- GCP's own constraints, checked here so a typo fails before anything is created
    when (not (validProjectId seed.seedProject)) $
        fail "--project must be 6-30 characters of [a-z0-9-], starting with a letter"
    when (Text.length (seed.seedPrefix <> "-sa") > 30 || not (validPrefix seed.seedPrefix)) $
        fail "--prefix must be [a-z0-9-] starting with a letter, at most 27 characters"
    source <- case seed.seedImageSource of
        FromBaseImage img -> do
            when (Text.null img || Text.any (`elem` [' ', '\t', '\n', '\r']) img) $
                fail "--base-image must be a single image reference"
            pure (FromBaseImage img)
        FromContainerfile path -> do
            exists <- doesFileExist path
            when (not exists) $
                fail ("--containerfile not found: " <> path)
            FromContainerfile <$> makeAbsolute path
    dir <- makeAbsolute seed.seedWorkDir
    pure $
        Spec
            { project = seed.seedProject
            , createProjectUnder = case seed.seedParent of
                SeedOrganization org -> Just (OrganizationParent org)
                SeedFolder folder -> Just (FolderParent folder)
                SeedNoParent -> Just NoParentRef
                SeedExistingProject -> Nothing
            , billingAccount = seed.seedBillingAccount
            , region = seed.seedRegion
            , tier = seed.seedTier
            , prefix = seed.seedPrefix
            , imageTag = seed.seedImageTag
            , imageSource = source
            , workDir = dir
            }
  where
    validProjectId t =
        Text.length t >= 6 && Text.length t <= 30 && validPrefix t && not ("-" `Text.isSuffixOf` t)
    validPrefix t =
        case Text.uncons t of
            Just (c, _) -> isAsciiLower c && Text.all (\x -> isAsciiLower x || isDigit x || x == '-') t
            Nothing -> False

-------------------------------------------------------------------------------
-- Program

program :: Track' Spec
program = Track $ \spec ->
    op "gcp-toy" (deps (tier0 spec <> if spec.tier >= 1 then tier1 spec else [])) $ \actions ->
        actions
            { help = Text.unwords ["salmon GCP toy validation, tier", Text.pack (show spec.tier), "in", spec.project]
            , ref = mkRef "gcp-toy" (spec.project, spec.prefix)
            }

projectOf :: Spec -> Core.Project
projectOf spec = Core.Project spec.project

regionOf :: Spec -> Core.Region
regionOf spec = Core.Region spec.region

adc :: Op
adc = Core.applicationDefaultCredentials reportPrint Core.gcloud

-- | Whatever every project-scoped resource must wait for.
foundation :: Spec -> [Op]
foundation spec = adc : catMaybes [projectNode, billingNode]
  where
    projectNode =
        fmap
            ( \parent ->
                ResourceManager.project
                    reportPrint
                    Core.gcloud
                    (ResourceManager.ProjectSpec (projectOf spec) (toParent parent) mempty)
                    `inject` adc
            )
            spec.createProjectUnder
    billingNode =
        fmap
            ( \acct ->
                foldl
                    inject
                    (Billing.linkBillingAccount reportPrint Core.gcloud (projectOf spec) (Billing.BillingAccount acct))
                    (adc : catMaybes [projectNode])
            )
            spec.billingAccount

    toParent (OrganizationParent org) = ResourceManager.Organization org
    toParent (FolderParent folder) = ResourceManager.Folder folder
    toParent NoParentRef = ResourceManager.NoParent

onFoundation :: Spec -> Op -> Op
onFoundation spec o = foldl inject o (foundation spec)

api :: Spec -> Text -> Op
api spec name =
    onFoundation spec (ServiceUsage.enableService reportPrint Core.gcloud (projectOf spec) (ServiceUsage.Api name))

serviceAccountId :: Spec -> Text
serviceAccountId spec = spec.prefix <> "-sa"

serviceAccountEmail :: Spec -> Text
serviceAccountEmail spec = serviceAccountId spec <> "@" <> spec.project <> ".iam.gserviceaccount.com"

repo :: Spec -> ArtifactRegistry.ArtifactRepo
repo spec = ArtifactRegistry.ArtifactRepo (spec.prefix <> "-repo") (projectOf spec) (regionOf spec) ArtifactRegistry.Docker

serviceAccount :: Spec -> Op
serviceAccount spec =
    Iam.serviceAccount reportPrint Core.gcloud (projectOf spec) (serviceAccountId spec)
        `inject` api spec "iam.googleapis.com"

repository :: Spec -> Op
repository spec =
    ArtifactRegistry.artifactRepository reportPrint Core.gcloud (repo spec)
        `inject` api spec "artifactregistry.googleapis.com"

grant :: Spec -> Text -> Text -> Op
grant spec role resource =
    Iam.iamBinding
        reportPrint
        Core.gcloud
        (Iam.IamBinding (Iam.ServiceAccount (serviceAccountEmail spec)) role resource)
        `inject` serviceAccount spec

tier0 :: Spec -> [Op]
tier0 spec =
    [ grant spec "roles/storage.objectViewer" ("buckets/" <> bucketName) `inject` bucket
    , grant spec "roles/artifactregistry.reader" repoResource `inject` repository spec
    ]
  where
    -- bucket names are global: scoping by project id keeps two sandboxes apart
    bucketName = spec.project <> "-" <> spec.prefix
    bucket =
        Storage.bucket reportPrint Core.gcloud (Storage.Bucket bucketName (projectOf spec) (regionOf spec) True)
            `inject` api spec "storage.googleapis.com"
    repoResource =
        Text.intercalate "/" ["projects", spec.project, "locations", spec.region, "repositories", (repo spec).repoName]

tier1 :: Spec -> [Op]
tier1 spec =
    [ CloudRunDeploy.buildPushDeploy
        reportPrint
        Core.gcloud
        ignoreTrack
        CloudRunDeploy.CloudRunDeployConfig
            { CloudRunDeploy.crd_repo = repo spec
            , CloudRunDeploy.crd_authFile = Podman.AuthFile (spec.workDir <> "/podman-auth.json")
            , CloudRunDeploy.crd_containerfile = containerfile
            , CloudRunDeploy.crd_image = image
            , CloudRunDeploy.crd_service = spec.prefix <> "-hello"
            , CloudRunDeploy.crd_project = projectOf spec
            , CloudRunDeploy.crd_region = regionOf spec
            , CloudRunDeploy.crd_env = mempty
            , CloudRunDeploy.crd_serviceAccount = serviceAccountEmail spec
            , CloudRunDeploy.crd_ingress = CloudRun.All
            , CloudRunDeploy.crd_maxInstances = Just 1
            }
        `inject` api spec "run.googleapis.com"
        `inject` repository spec
        `inject` serviceAccount spec
    ]
  where
    image =
        Text.concat [spec.region, "-docker.pkg.dev/", spec.project, "/", (repo spec).repoName, "/hello:", spec.imageTag]
    containerfile :: FS.File "containerfile"
    containerfile = case spec.imageSource of
        -- the label makes each --image-tag build a distinct image rather
        -- than the base image re-pushed under another name
        FromBaseImage base ->
            FS.generateFileContents
                ( Text.unlines
                    [ "FROM " <> base
                    , "LABEL salmon-toy-tag=\"" <> spec.imageTag <> "\""
                    ]
                )
                (spec.workDir <> "/Containerfile")
        FromContainerfile path -> FS.PreExisting path
