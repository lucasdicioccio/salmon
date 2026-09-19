{-# LANGUAGE OverloadedStrings #-}

{- | PostgREST on Cloud Run, talking to a Postgres somewhere else over a
client certificate.

This is <https://dicioccio.fr/postgrest-over-cloudrun.html the write-up>
expressed as ops: an image, four secrets, the IAM that lets the service read
them, and a deploy. The database half — what makes that certificate mean
anything — is "SreBox.PostgresTls", and the two are designed to be used
together: 'pcr_role' here and
'SreBox.PostgresTls.ca_role' there are the same string, which is also the
@CN@ of the certificate, which is what @clientcert=verify-full@ compares.

= The wrinkle that makes this a recipe rather than a deploy command

Cloud Run mounts secrets __read-only, owned by root, mode 0444__, and there
is no way to ask for anything else. @libpq@ refuses to use a client key
whose mode is wider than @0600@ and says so in a message about file
permissions that mentions neither Cloud Run nor the secret. So the container
cannot use a mounted key directly, and the way through is an entrypoint that
copies the mounted files somewhere writable and narrows them before exec'ing
the real binary. 'renderEntrypoint' is that script, and it is the reason a
stock @postgrest@ image will not do.

= What is deliberately not here

No database roles, grants or migrations: those are PostgREST's real
difficulty and they belong to whoever owns the schema
("SreBox.PostgresInit" and "SreBox.PostgresMigrations" are the tools). This
module gets a correctly-configured PostgREST talking to a database; what it
is allowed to see once it arrives is somebody else's design.
-}
module SreBox.Gcp.PostgrestCloudRun (
    Report (..),
    PostgrestCloudRunConfig (..),
    postgrestCloudRun,

    -- * The image
    renderContainerfile,
    renderEntrypoint,

    -- * The deploy's shape, exposed for inspection and testing
    secretBindings,
    environment,

    -- * Where things land inside the container
    vaultDir,
    secretsDir,
    mountedMaterial,
    runtimeMaterial,

    -- * Secret names
    certSecretName,
    keySecretName,
    caSecretName,
    jwtSecretName,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.SecretManager as SecretManager
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track
import Salmon.Reporter

import qualified SreBox.Gcp.CloudRunDeploy as CloudRunDeploy
import qualified SreBox.PostgresTls as Tls

-------------------------------------------------------------------------------

data Report
    = UploadSecret !SecretManager.Report
    | GrantAccess !Iam.Report
    | Deploy !CloudRunDeploy.Report
    deriving (Show)

-------------------------------------------------------------------------------

data PostgrestCloudRunConfig = PostgrestCloudRunConfig
    { pcr_service :: Text
    , pcr_project :: Core.Project
    , pcr_region :: Core.Region
    , pcr_serviceAccount :: Text
    -- ^ the service account the revision runs as, and the principal granted
    -- @secretAccessor@ on each secret below
    , pcr_repo :: ArtifactRegistry.ArtifactRepo
    , pcr_image :: Text
    -- ^ fully-qualified image reference, tag included
    , pcr_authFile :: Podman.AuthFile
    , pcr_workDir :: FilePath
    -- ^ where the generated @Containerfile@ and @run.sh@ are written
    , pcr_postgrestImage :: Text
    -- ^ the upstream image the binary is copied out of, e.g.
    -- @postgrest\/postgrest:v12.2.1@. Pinned, not @latest@: this is the
    -- thing that decides how your JWT claims are read.
    , pcr_db :: Postgres.Server
    , pcr_database :: Postgres.DatabaseName
    , pcr_role :: Postgres.RoleName
    -- ^ the authenticator role; also the @CN@ of the client certificate
    , pcr_anonRole :: Text
    , pcr_jwtRoleClaimKey :: Maybe Text
    , pcr_schemas :: Maybe Text
    , pcr_sslMode :: Tls.SslMode
    , pcr_clientMaterial :: (FilePath, FilePath, FilePath)
    -- ^ local (certificate, key, CA certificate) — typically
    -- 'SreBox.PostgresTls.clientMaterialPaths' of the material this same
    -- graph generated
    , pcr_jwtSecretFile :: Maybe FilePath
    -- ^ a local file holding the JWT signing secret. This recipe does not
    -- create it: see "SreBox.JWTSigning" or
    -- "Salmon.Builtin.Nodes.Secrets".@sharedSecretFile@.
    , pcr_ingress :: CloudRun.IngressSetting
    , pcr_maxInstances :: Maybe Int
    , pcr_cpu :: Maybe Text
    , pcr_memory :: Maybe Text
    , pcr_concurrency :: Maybe Int
    , pcr_allowUnauthenticated :: Bool
    }

-------------------------------------------------------------------------------

{- | Uploads the credentials, grants the service account access to them,
builds the wrapper image and deploys it.

The deploy is injected onto every secret and every binding, which is the
ordering that matters: a revision naming a secret that does not exist yet
fails to start, and one naming a secret it may not read fails at the first
connection instead — later, and much less legibly.
-}
postgrestCloudRun ::
    Reporter Report ->
    Track' (Binary "gcloud") ->
    Track' (Binary "podman") ->
    PostgrestCloudRunConfig ->
    Op
postgrestCloudRun r gcloudTrack podmanTrack cfg =
    op "gcp-postgrest-cloudrun" (deps [deploy]) $ \actions ->
        actions
            { help = Text.unwords ["deploys PostgREST", cfg.pcr_service, "against", cfg.pcr_database]
            , ref = mkRef "gcp-postgrest-cloudrun" (cfg.pcr_project.projectId, cfg.pcr_service)
            }
  where
    rSecret = contramap UploadSecret r
    rIam = contramap GrantAccess r
    rDeploy = contramap Deploy r

    (localCert, localKey, localCa) = cfg.pcr_clientMaterial

    deploy :: Op
    deploy =
        foldl
            inject
            ( CloudRunDeploy.buildPushDeploy
                rDeploy
                gcloudTrack
                podmanTrack
                CloudRunDeploy.CloudRunDeployConfig
                    { CloudRunDeploy.crd_repo = cfg.pcr_repo
                    , CloudRunDeploy.crd_authFile = cfg.pcr_authFile
                    , CloudRunDeploy.crd_containerfile = containerfile
                    , CloudRunDeploy.crd_image = cfg.pcr_image
                    , CloudRunDeploy.crd_service = cfg.pcr_service
                    , CloudRunDeploy.crd_project = cfg.pcr_project
                    , CloudRunDeploy.crd_region = cfg.pcr_region
                    , CloudRunDeploy.crd_env = environment cfg
                    , CloudRunDeploy.crd_serviceAccount = cfg.pcr_serviceAccount
                    , CloudRunDeploy.crd_ingress = cfg.pcr_ingress
                    , CloudRunDeploy.crd_maxInstances = cfg.pcr_maxInstances
                    , CloudRunDeploy.crd_options =
                        CloudRun.defaultCloudRunOptions
                            { CloudRun.croSecrets = secretBindings cfg
                            , CloudRun.croCpu = cfg.pcr_cpu
                            , CloudRun.croMemory = cfg.pcr_memory
                            , CloudRun.croConcurrency = cfg.pcr_concurrency
                            , CloudRun.croAllowUnauthenticated = cfg.pcr_allowUnauthenticated
                            }
                    }
                `inject` entrypoint
            )
            (secretsAndGrants <> [entrypoint])

    -- The entrypoint is COPY'd by the Containerfile, so it has to exist
    -- before the build, not merely before the deploy.
    entrypoint :: Op
    entrypoint =
        FS.filecontents (FS.FileContents (cfg.pcr_workDir </> "run.sh") (renderEntrypoint cfg))

    containerfile :: FS.File "containerfile"
    containerfile =
        FS.generateFileContents (renderContainerfile cfg) (cfg.pcr_workDir </> "Containerfile")

    secretsAndGrants :: [Op]
    secretsAndGrants =
        concat
            [ uploaded (certSecretName cfg) localCert
            , uploaded (keySecretName cfg) localKey
            , uploaded (caSecretName cfg) localCa
            , maybe [] (uploaded (jwtSecretName cfg)) cfg.pcr_jwtSecretFile
            ]

    uploaded :: Text -> FilePath -> [Op]
    uploaded name source =
        [ version
        , -- Without this the revision deploys happily and then fails to
          -- start, because a service account has no access to a project's
          -- secrets by virtue of being in the project.
          Iam.iamBinding
            rIam
            gcloudTrack
            (Iam.IamBinding (Iam.ServiceAccount cfg.pcr_serviceAccount) "roles/secretmanager.secretAccessor" resource)
            `inject` version
        ]
      where
        sec =
            SecretManager.Secret
                { SecretManager.secretName = name
                , SecretManager.secretProject = cfg.pcr_project
                , SecretManager.secretReplication = "automatic"
                }
        version =
            SecretManager.secretVersion rSecret gcloudTrack (SecretManager.SecretVersion sec source)
        resource =
            Text.intercalate "/" ["projects", cfg.pcr_project.projectId, "secrets", name]

-------------------------------------------------------------------------------
-- Names and paths

certSecretName, keySecretName, caSecretName, jwtSecretName :: PostgrestCloudRunConfig -> Text
certSecretName cfg = cfg.pcr_service <> "-db-cert"
keySecretName cfg = cfg.pcr_service <> "-db-key"
caSecretName cfg = cfg.pcr_service <> "-db-ca"
jwtSecretName cfg = cfg.pcr_service <> "-jwt"

{- | Where Cloud Run mounts the secrets: read-only, root-owned, @0444@, and
not changeable.
-}
vaultDir :: FilePath
vaultDir = "/opt/vault"

-- | Where 'renderEntrypoint' copies them so libpq will accept them.
secretsDir :: FilePath
secretsDir = "/opt/secrets"

-- | (certificate, key, CA) as mounted.
mountedMaterial :: (FilePath, FilePath, FilePath)
mountedMaterial =
    (vaultDir </> "cert.pem", vaultDir </> "key.pem", vaultDir </> "ca.pem")

-- | (certificate, key, CA) as the connection string names them.
runtimeMaterial :: (FilePath, FilePath, FilePath)
runtimeMaterial =
    (secretsDir </> "cert.pem", secretsDir </> "key.pem", secretsDir </> "ca.pem")

-------------------------------------------------------------------------------
-- The deploy's shape

secretBindings :: PostgrestCloudRunConfig -> [CloudRun.SecretBinding]
secretBindings cfg =
    [ CloudRun.SecretFile mCert (certSecretName cfg) "latest"
    , CloudRun.SecretFile mKey (keySecretName cfg) "latest"
    , CloudRun.SecretFile mCa (caSecretName cfg) "latest"
    ]
        <> maybe
            []
            -- The JWT secret is an env var rather than a file because that is
            -- what PostgREST reads; it is also the one credential here that a
            -- crash report could leak, which is an argument for the file form
            -- and PGRST_JWT_SECRET_FILE if your version supports it.
            (const [CloudRun.SecretEnvVar "PGRST_JWT_SECRET" (jwtSecretName cfg) "latest"])
            cfg.pcr_jwtSecretFile
  where
    (mCert, mKey, mCa) = mountedMaterial

environment :: PostgrestCloudRunConfig -> Map Text Text
environment cfg =
    Map.fromList $
        [ ("PGRST_DB_URI", Tls.clientConnString cfg.pcr_db cfg.pcr_database cfg.pcr_role cfg.pcr_sslMode runtimeMaterial)
        , ("PGRST_DB_ANON_ROLE", cfg.pcr_anonRole)
        ]
            <> catMaybes
                [ (,) "PGRST_JWT_ROLE_CLAIM_KEY" <$> cfg.pcr_jwtRoleClaimKey
                , (,) "PGRST_DB_SCHEMAS" <$> cfg.pcr_schemas
                ]

-------------------------------------------------------------------------------
-- The image

{- | A two-stage build: the upstream PostgREST binary, on a base that has
@libpq@ and a shell.

Copying the binary out rather than deriving @FROM@ the upstream image is what
makes room for the entrypoint — the upstream image is deliberately minimal
and has neither the shell nor the tools the copy-and-chmod dance needs.
-}
renderContainerfile :: PostgrestCloudRunConfig -> Text
renderContainerfile cfg =
    Text.unlines
        [ "FROM " <> cfg.pcr_postgrestImage <> " AS upstream"
        , ""
        , "FROM debian:bookworm-slim"
        , "RUN apt-get update \\"
        , "  && apt-get install -y --no-install-recommends libpq5 ca-certificates \\"
        , "  && rm -rf /var/lib/apt/lists/*"
        , "COPY --from=upstream /bin/postgrest /bin/postgrest"
        , "COPY run.sh /run.sh"
        , "RUN chmod 0755 /run.sh"
        , "ENTRYPOINT [\"/bin/bash\", \"/run.sh\"]"
        ]

{- | The entrypoint: copy the mounted secrets somewhere writable, narrow
them, hand the port over, exec PostgREST.

Every line of it is load-bearing:

* @cp -L@ dereferences, because a Cloud Run secret mount is a symlink farm
  into a @..data@ directory and copying the links would copy the permissions
  with them.
* the @chmod@ is the entire point (see the module header): @0600@ on the
  files, because libpq refuses anything wider, and @0700@ on the directory
  because there is no reason to be less careful about it.
* @PGRST_SERVER_PORT@ is taken from @$PORT@, which is Cloud Run's side of
  the contract and is not necessarily the port anyone configured.
* @exec@, so PostgREST is PID 1 and receives the @SIGTERM@ Cloud Run sends
  when it drains an instance. Without it the shell gets the signal, the
  server does not, and every deploy ends in a ten-second kill instead of a
  clean shutdown.
-}
renderEntrypoint :: PostgrestCloudRunConfig -> Text
renderEntrypoint _cfg =
    Text.unlines
        [ "#!/bin/bash"
        , "set -euo pipefail"
        , ""
        , "# Cloud Run mounts secrets read-only at 0444 and there is no way to ask"
        , "# for anything else; libpq refuses a client key wider than 0600. So the"
        , "# credentials are copied somewhere writable before they are used."
        , "mkdir -p " <> Text.pack secretsDir
        , "cp -L " <> Text.pack vaultDir <> "/* " <> Text.pack secretsDir <> "/"
        , "chmod 0700 " <> Text.pack secretsDir
        , "find " <> Text.pack secretsDir <> " -type f -exec chmod 0600 {} +"
        , ""
        , "# Cloud Run decides the port, not the configuration."
        , "export PGRST_SERVER_PORT=\"${PORT:-3000}\""
        , "export PGRST_SERVER_HOST='*4'"
        , ""
        , "exec /bin/postgrest"
        ]
