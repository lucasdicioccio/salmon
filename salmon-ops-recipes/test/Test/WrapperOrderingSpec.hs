{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0: what is ordered before the nodes that do the work, rather than
before a node grouping them.

A node built with @op name (deps [...]) id@ (or one that only sets its
@help@ and @ref@) has no effect of its own. 'Salmon.Op.OpGraph.inject' onto
it adds a predecessor to that node, and the fold reads all of a node's
predecessors as "depends on", unordered among themselves: what was injected
is then a /sibling/ of the work, which a one-shot pass happens to visit in
listed order and @run serve@ converges concurrently. Each test here names a
recipe that used to do that, and asserts on the folded 'Dag.Dag' that every
working node now has the prerequisite below it. Graph shape only: nothing
here runs a node.
-}
module Test.WrapperOrderingSpec (tests) where

import Data.Aeson (Value (String))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Builtin.Extension (Extension (..), Op, evalDeps, ignoreTrack)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.WireGuard as WG
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)
import Salmon.Reporter (silent)
import qualified SreBox.Gcp.PostgrestCloudRun as PCR
import qualified SreBox.PostgresInit as PGInit
import qualified SreBox.PostgresTls as Tls
import qualified SreBox.Postgrest as Postgrest
import qualified SreBox.WireGuardMesh as Mesh
import qualified SreBox.WireGuardVpn as Vpn

tests :: TestTree
tests =
    testGroup
        "inject lands on the working nodes, not on a grouping node (Layer 0)"
        [ testCase "PostgresInit.setupPG: grants and ownership wait for the database, roles for the groups" setupPGOrder
        , testCase "Postgrest.postgrestMigratedApi: the migration waits for the init, the service for the migration" postgrestOrder
        , testCase "PostgresTls.postgresClientCertAuth: the TLS settings and the reload wait for the material" serverTlsOrder
        , testCase "WireGuardMesh.hostOp: every peer waits for the interface's key and port" meshPeersOrder
        , testCase "WireGuardVpn.serverVpn: every peer waits for the interface's key and port" vpnPeersOrder
        , testCase "Gcp.PostgrestCloudRun: the build waits for the entrypoint, the deploy for secrets and grants" cloudRunOrder
        ]

-------------------------------------------------------------------------------

dagOf :: Op -> Dag.Dag Extension
dagOf = Dag.foldDag Dag.sameRepresentative . evalDeps

type Pick = Act Extension -> Bool

short :: Text -> Pick
short s a = a.shorthand == s

helpHas :: Text -> Pick
helpHas t a = t `Text.isInfixOf` help a.extension

(&&&) :: Pick -> Pick -> Pick
(&&&) p q a = p a && q a

picked :: Pick -> Dag.Dag Extension -> [Ref]
picked p dag = [rf | (rf, a) <- Map.toList (Dag.dagNodes dag), p a]

-- | Everything a node waits for, however far down.
below :: Dag.Dag Extension -> Ref -> Set.Set Ref
below dag = go Set.empty . Dag.dependenciesOf dag
  where
    go seen [] = seen
    go seen (r : rest)
        | r `Set.member` seen = go seen rest
        | otherwise = go (Set.insert r seen) (Dag.dependenciesOf dag r <> rest)

{- | There are nodes picked by each, and every node picked by the first has
one picked by the second below it.
-}
assertAllWaitFor :: String -> Dag.Dag Extension -> Pick -> Pick -> IO ()
assertAllWaitFor what dag from to = do
    let starts = picked from dag
        targets = Set.fromList (picked to dag)
    assertBool (what <> ": no node to start from") (not (null starts))
    assertBool (what <> ": no node to wait for") (not (Set.null targets))
    mapM_
        (\s -> assertBool (what <> ": " <> show s) (not (Set.null (Set.intersection targets (below dag s)))))
        starts

assertNoConflicts :: Dag.Dag Extension -> IO ()
assertNoConflicts dag = assertEqual "no declaration collides with another" 0 (length (Dag.dagConflicts dag))

-------------------------------------------------------------------------------

setupPGOrder :: IO ()
setupPGOrder = do
    let dag =
            dagOf . PGInit.setupPG silent $
                PGInit.InitSetup
                    (Postgres.Database "appdb")
                    (Postgres.User "owner", FS.PreExisting "/secrets/owner")
                    [(Postgres.User "alice", FS.PreExisting "/secrets/alice", [Postgres.CONNECT], [Postgres.Group "readers"])]
                    [(Postgres.Group "readers", [Postgres.CONNECT])]
        db = short "pg-database" &&& helpHas "create db appdb"
    assertAllWaitFor "every grant waits for the database" dag (short "pg-grant") db
    assertAllWaitFor "the ownership waits for the database" dag (helpHas "appdb ownership to owner") db
    assertAllWaitFor "a role waits for the database" dag (helpHas "set user password for alice") db
    assertAllWaitFor "a role waits for the groups" dag (helpHas "set user password for alice") (short "pg-group")
    assertNoConflicts dag

postgrestOrder :: IO ()
postgrestOrder = do
    let dag = dagOf node
        migration = short "opaque-migrate"
        calls = picked (short "ssh:call") dag
        migrations = picked migration dag
    assertEqual "one migration" 1 (length migrations)
    -- the service's call is the one with the migration below it; the init's
    -- call is below the migration
    let callsAbove = [c | c <- calls, any (`Set.member` below dag c) migrations]
        callsBelow = [c | m <- migrations, c <- calls, c `Set.member` below dag m]
    assertBool "the service's remote call waits for the migration" (not (null callsAbove))
    assertBool "the migration waits for the init's remote call" (not (null callsBelow))
    assertBool "two different calls" (all (`notElem` callsBelow) callsAbove)
    assertNoConflicts dag
  where
    node :: Op
    node =
        Postgrest.postgrestMigratedApi
            silent
            ignoreTrack
            -- three directives that differ, or the three remote calls are
            -- one site
            (const (String "init"))
            (const (String "migrate"))
            (const (String "postgrest"))
            (Self.SelfPath "/usr/local/bin/salmon")
            (Self.Remote "salmon" "db.example")
            ignoreTrack
            "/secrets/jwt"
            Postgrest.defaultFilePaths
            Postgrest.PostgrestMigratedApiConfig
                { Postgrest.pma_serviceName = "api"
                , Postgrest.pma_apiPort = 3000
                , Postgrest.pma_repo = Git.Repo "/src" "app" (Git.Remote "https://git.example/app.git") (Git.Branch "main")
                , Postgrest.pma_migrationTip = "migrations/tip.sql"
                , Postgrest.pma_migrationsPrefix = "migrations"
                , Postgrest.pma_migrate_connstring = conn "migrator" "/secrets/migrator"
                , Postgrest.pma_setrole_connstring = conn "authenticator" "/secrets/authenticator"
                , Postgrest.pma_fallback_role = Postgres.Group "anon"
                , Postgrest.pma_token_protected_roles = [Postgres.Group "user"]
                }
    conn u p = Postgres.ConnString (Postgres.Server "localhost" 5432) (Postgres.User u) p (Postgres.Database "appdb")

serverTlsOrder :: IO ()
serverTlsOrder = do
    let dag =
            dagOf $
                Tls.postgresClientCertAuth
                    silent
                    ignoreTrack
                    ignoreTrack
                    ignoreTrack
                    Tls.PostgresClientCertConfig
                        { Tls.pgcc_cluster = "main"
                        , Tls.pgcc_port = 5432
                        , Tls.pgcc_tls = Postgres.ServerTls "/etc/pg/server.pem" "/etc/pg/server.key" "/etc/pg/ca.pem"
                        , Tls.pgcc_owner = "postgres"
                        , Tls.pgcc_clients = [Tls.ClientAccess "api" "appdb" "0.0.0.0/0"]
                        }
        key = short "file-ownership" &&& helpHas "server.key"
    assertAllWaitFor "every TLS setting waits for the key's ownership" dag (short "pg-alter-system") key
    assertAllWaitFor "the reload waits for the key's ownership" dag (short "pg-reload-conf") key
    assertNoConflicts dag

binaries :: Vpn.Binaries
binaries = Vpn.Binaries ignoreTrack ignoreTrack ignoreTrack ignoreTrack

meshPeersOrder :: IO ()
meshPeersOrder = do
    let seed =
            Mesh.MeshSeed
                { Mesh.mesh_subnet = "10.66.0.0/24"
                , Mesh.mesh_iface = "wg0"
                , Mesh.mesh_port = 51820
                , Mesh.mesh_privkey_path = "/etc/wireguard/wg0.key"
                , Mesh.mesh_peers =
                    [ Mesh.PeerDecl "a" "10.66.0.1" "a-key=" (Just "a.example:51820") []
                    , Mesh.PeerDecl "b" "10.66.0.2" "b-key=" (Just "b.example:51820") []
                    , Mesh.PeerDecl "c" "10.66.0.3" "c-key=" Nothing []
                    ]
                , Mesh.mesh_policies = []
                , Mesh.mesh_routers = []
                }
        h = either (error . show) id (Mesh.genHost seed "a")
        dag = dagOf (Mesh.hostOp silent binaries h)
    assertAllWaitFor "every peer waits for the interface's key and port" dag (short "wireguard-peer") (short "wireguard-server")
    assertNoConflicts dag

vpnPeersOrder :: IO ()
vpnPeersOrder = do
    let dag =
            dagOf . Vpn.serverVpn silent binaries $
                Vpn.ServerVpnSpec
                    { Vpn.server_wg_iface = "wg0"
                    , Vpn.server_wg_port = 51820
                    , Vpn.server_wg_privkey_path = "/etc/wireguard/wg0.key"
                    , Vpn.server_vpn_net = WG.Ipv4Cidr "10.0.3.1" 24
                    , Vpn.server_clients =
                        [ Vpn.ClientPeer (WG.Ipv4Cidr "10.0.3.2" 32) "/etc/wireguard/client-a.pub"
                        , Vpn.ClientPeer (WG.Ipv4Cidr "10.0.3.3" 32) "/etc/wireguard/client-b.pub"
                        ]
                    }
    assertAllWaitFor "every peer waits for the interface's key and port" dag (short "wireguard-peer") (short "wireguard-server")
    assertNoConflicts dag

cloudRunOrder :: IO ()
cloudRunOrder = do
    let dag = dagOf (PCR.postgrestCloudRun silent ignoreTrack ignoreTrack cfg)
        deploy = helpHas "deploys CloudRun service api"
    assertAllWaitFor "the build waits for the entrypoint it copies" dag (short "podman-build") (helpHas "/tmp/work/run.sh")
    assertAllWaitFor "the deploy waits for every secret's upload" dag deploy (helpHas "to secret")
    mapM_
        (\name -> assertAllWaitFor ("the deploy waits for " <> Text.unpack name) dag deploy (helpHas ("to secret " <> name)))
        [PCR.certSecretName cfg, PCR.keySecretName cfg, PCR.caSecretName cfg, PCR.jwtSecretName cfg]
    assertAllWaitFor "the deploy waits for the grants" dag deploy (helpHas "grants roles/secretmanager.secretAccessor")
    assertNoConflicts dag
  where
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
            , PCR.pcr_jwtRoleClaimKey = Nothing
            , PCR.pcr_schemas = Nothing
            , PCR.pcr_sslMode = Tls.VerifyCa
            , PCR.pcr_clientMaterial = ("/certs/api_authenticator.pem", "/certs/api_authenticator.key", "/certs/ca.pem")
            , PCR.pcr_jwtSecretFile = Just "/secrets/jwt"
            , PCR.pcr_ingress = CloudRun.All
            , PCR.pcr_maxInstances = Just 1
            , PCR.pcr_cpu = Nothing
            , PCR.pcr_memory = Nothing
            , PCR.pcr_concurrency = Nothing
            , PCR.pcr_allowUnauthenticated = False
            }
