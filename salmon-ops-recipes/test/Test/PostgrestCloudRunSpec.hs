{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "SreBox.Gcp.PostgrestCloudRun".

The interesting assertions are all about one seam: the credentials are
mounted at one set of paths and /used/ at another, because Cloud Run's mount
is read-only at @0444@ and libpq refuses a key that wide. So the entrypoint
copies them, and the connection string has to name the copies. Getting those
two out of step produces a service that deploys cleanly, starts, and then
fails every request with a permissions error naming a file the operator can
see is present.
-}
module Test.PostgrestCloudRunSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified SreBox.Gcp.PostgrestCloudRun as PCR
import qualified SreBox.PostgresTls as Tls

tests :: TestTree
tests =
    testGroup
        "SreBox.Gcp.PostgrestCloudRun"
        [ testGroup "the entrypoint" entrypointTests
        , testGroup "the image" containerfileTests
        , testGroup "secret bindings" bindingTests
        ]

cfg :: PCR.PostgrestCloudRunConfig
cfg =
    PCR.PostgrestCloudRunConfig
        { PCR.pcr_service = "api"
        , PCR.pcr_project = Core.Project "acme"
        , PCR.pcr_region = Core.Region "europe-west1"
        , PCR.pcr_serviceAccount = "api-sa@acme.iam.gserviceaccount.com"
        , PCR.pcr_repo = ArtifactRegistry.ArtifactRepo "repo" (Core.Project "acme") (Core.Region "europe-west1") ArtifactRegistry.Docker
        , PCR.pcr_image = "europe-west1-docker.pkg.dev/acme/repo/postgrest:v1"
        , PCR.pcr_authFile = Podman.AuthFile "/tmp/auth.json"
        , PCR.pcr_workDir = "/tmp/work"
        , PCR.pcr_postgrestImage = "postgrest/postgrest:v12.2.1"
        , PCR.pcr_db = Postgres.Server "db.example" 5432
        , PCR.pcr_database = "api_db"
        , PCR.pcr_role = "api_authenticator"
        , PCR.pcr_anonRole = "api_anonymous"
        , PCR.pcr_jwtRoleClaimKey = Just ".acme.jwt-claims.postgrest-role"
        , PCR.pcr_schemas = Nothing
        , PCR.pcr_sslMode = Tls.VerifyCa
        , PCR.pcr_clientMaterial = ("/certs/api_authenticator.pem", "/certs/api_authenticator.key", "/certs/ca.pem")
        , PCR.pcr_jwtSecretFile = Just "/secrets/jwt"
        , PCR.pcr_ingress = CloudRun.All
        , PCR.pcr_maxInstances = Just 1
        , PCR.pcr_cpu = Just "1000m"
        , PCR.pcr_memory = Just "256Mi"
        , PCR.pcr_concurrency = Just 80
        , PCR.pcr_allowUnauthenticated = False
        }

entrypoint :: String
entrypoint = Text.unpack (PCR.renderEntrypoint cfg)

entrypointTests :: [TestTree]
entrypointTests =
    [ testCase "narrows the copied credentials to 0600, which is the whole point" $ do
        assertBool entrypoint ("-type f -exec chmod 0600" `isInfixOf` entrypoint)
        assertBool entrypoint ("chmod 0700 /opt/secrets" `isInfixOf` entrypoint)
    , testCase "dereferences the mount, which is a symlink farm" $
        assertBool entrypoint ("cp -L /opt/vault/* /opt/secrets/" `isInfixOf` entrypoint)
    , testCase "execs, so PostgREST gets the SIGTERM Cloud Run sends when draining" $
        assertBool entrypoint ("exec /bin/postgrest" `isInfixOf` entrypoint)
    , testCase "takes the port from Cloud Run rather than from configuration" $
        assertBool entrypoint ("PGRST_SERVER_PORT=\"${PORT:-3000}\"" `isInfixOf` entrypoint)
    , testCase "parses as bash" $ do
        (code, _, err) <- readProcessWithExitCode "bash" ["-n", "-c", entrypoint] ""
        assertEqual err ExitSuccess code
    ]

containerfileTests :: [TestTree]
containerfileTests =
    [ testCase "copies the binary out of the pinned upstream image" $ do
        let c = Text.unpack (PCR.renderContainerfile cfg)
        assertBool c ("FROM postgrest/postgrest:v12.2.1 AS upstream" `isInfixOf` c)
        assertBool c ("COPY --from=upstream /bin/postgrest /bin/postgrest" `isInfixOf` c)
    , testCase "the base has libpq, without which nothing connects" $ do
        let c = Text.unpack (PCR.renderContainerfile cfg)
        assertBool c ("libpq5" `isInfixOf` c)
    ]

bindingTests :: [TestTree]
bindingTests =
    [ testCase "the connection string names where the files END UP, not where they are mounted" $ do
        -- The seam this module exists around. Naming the mount here gives a
        -- service that starts and then fails every request.
        let (rc, rk, rca) = PCR.runtimeMaterial
            uri =
                Tls.clientConnString
                    (Postgres.Server "db.example" 5432)
                    "api_db"
                    "api_authenticator"
                    Tls.VerifyCa
                    (rc, rk, rca)
        assertBool (Text.unpack uri) ("sslcert=/opt/secrets/cert.pem" `Text.isInfixOf` uri)
        assertBool (Text.unpack uri) (not ("/opt/vault" `Text.isInfixOf` uri))
    , testCase "the mounted paths are the ones the entrypoint copies from" $ do
        let (mc, _, _) = PCR.mountedMaterial
        assertEqual "" "/opt/vault/cert.pem" mc
        assertBool entrypoint (PCR.vaultDir `isInfixOf` entrypoint)
    , testCase "secret names are scoped to the service" $ do
        assertEqual "" "api-db-cert" (PCR.certSecretName cfg)
        assertEqual "" "api-db-key" (PCR.keySecretName cfg)
        assertEqual "" "api-db-ca" (PCR.caSecretName cfg)
        assertEqual "" "api-jwt" (PCR.jwtSecretName cfg)
    , testCase "the deployed environment points PostgREST at the copies" $ do
        let env = PCR.environment cfg
        case Map.lookup "PGRST_DB_URI" env of
            Just uri -> do
                assertBool (Text.unpack uri) ("sslkey=/opt/secrets/key.pem" `Text.isInfixOf` uri)
                assertBool (Text.unpack uri) ("user=api_authenticator" `Text.isInfixOf` uri)
                -- no password anywhere: the certificate is the whole credential
                assertBool (Text.unpack uri) (not ("password" `Text.isInfixOf` uri))
            Nothing -> assertBool "PGRST_DB_URI missing" False
        assertEqual "" (Just "api_anonymous") (Map.lookup "PGRST_DB_ANON_ROLE" env)
    , testCase "no jwt file means no jwt secret is referenced" $ do
        -- A revision naming a secret that was never uploaded fails to start.
        let without = cfg{PCR.pcr_jwtSecretFile = Nothing}
            rendered = map CloudRun.renderSecretBinding (PCR.secretBindings without)
        assertBool (show rendered) (not (any ("api-jwt" `Text.isInfixOf`) rendered))
        assertEqual "" 3 (length (PCR.secretBindings without))
        assertEqual "" 4 (length (PCR.secretBindings cfg))
    ]
