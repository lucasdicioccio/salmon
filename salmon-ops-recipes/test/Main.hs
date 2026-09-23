module Main (main) where

import qualified Test.CheckSpec as CheckSpec
import qualified Test.ConcurrentSpec as ConcurrentSpec
import qualified Test.DaemonSpec as DaemonSpec
import qualified Test.DagSpec as DagSpec
import qualified Test.DebianPackageSpec as DebianPackageSpec
import qualified Test.DebootstrapSpec as DebootstrapSpec
import qualified Test.DownTreeSpec as DownTreeSpec
import qualified Test.FilesystemSpec as FilesystemSpec
import qualified Test.FollowSchedulerSpec as FollowSchedulerSpec
import qualified Test.FollowSpec as FollowSpec
import qualified Test.GcpSpec as GcpSpec
import qualified Test.JWTSigningSpec as JWTSigningSpec
import qualified Test.LedgerSpec as LedgerSpec
import qualified Test.PodmanCommandSpec as PodmanCommandSpec
import qualified Test.MigratorTemplateSpec as MigratorTemplateSpec
import qualified Test.PgBackupSpec as PgBackupSpec
import qualified Test.PodmanSpec as PodmanSpec
import qualified Test.PostgresBackupSpec as PostgresBackupSpec
import qualified Test.PostgresInitSpec as PostgresInitSpec
import qualified Test.PostgresReplicationSpec as PostgresReplicationSpec
import qualified Test.PostgresClusterSpec as PostgresClusterSpec
import qualified Test.PgBouncerSpec as PgBouncerSpec
import qualified Test.PgPairDemoSpec as PgPairDemoSpec
import qualified Test.PostgresPairSpec as PostgresPairSpec
import qualified Test.PostgresSwitchoverSpec as PostgresSwitchoverSpec
import qualified Test.PostgresTemplateSpec as PostgresTemplateSpec
import qualified Test.PostgresTlsSpec as PostgresTlsSpec
import qualified Test.PostgrestCloudRunSpec as PostgrestCloudRunSpec
import qualified Test.QemuResolveKernelSpec as QemuResolveKernelSpec
import qualified Test.QemuSmokeSpec as QemuSmokeSpec
import qualified Test.QuerySpec as QuerySpec
import qualified Test.ReportJsonSpec as ReportJsonSpec
import qualified Test.RewriteSpec as RewriteSpec
import qualified Test.ServeModelSpec as ServeModelSpec
import qualified Test.ServeSocketSpec as ServeSocketSpec
import qualified Test.ServeSpec as ServeSpec
import qualified Test.SystemdSpec as SystemdSpec
import qualified Test.UpTreeSpec as UpTreeSpec
import qualified Test.UpkeepSpec as UpkeepSpec

import Test.Tasty (DependencyType (..), TestTree, defaultMain, sequentialTestGroup, testGroup)

{- | The cheap tiers run concurrently, as tasty does by default; the tiers
that reach for a machine-wide resource do not.

Layer 2 and Layer 3 contend in ways that have nothing to do with what they
assert. Every VM-based spec asks the harness for the same bridge address, so
two of them at once fight over one tap and one IP; the podman specs mutate
@PATH@ process-globally to shim binaries, which is not a thing two threads
can do at once. Before this, three qemu specs in one suite failed together
and individually passed — which reads exactly like a real bug and is not
one.

'sequentialTestGroup' rather than @localOption (NumThreads 1)@: tasty reads
'NumThreads' once, for the whole run, so setting it on a subtree left these
running alongside one another all the same.
-}
heavy :: [TestTree] -> TestTree
heavy = sequentialTestGroup "containers and VMs (serialized)" AllFinish

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-ops-recipes"
            [ heavy
                [ DebootstrapSpec.tests
                , -- these two shim PATH, which is process-global
                  PostgresInitSpec.tests
                , PostgresTemplateSpec.sandboxTests
                , MigratorTemplateSpec.tests
                , -- the rest of the VM specs: they share one bridge and a
                  -- handful of fixed addresses, so two at once is two guests
                  -- claiming one address.
                  QemuSmokeSpec.tests
                , PostgresReplicationSpec.tests
                , PostgresSwitchoverSpec.tests
                , PgBackupSpec.tests
                , PgPairDemoSpec.tests
                ]
            , CheckSpec.tests
            , ConcurrentSpec.tests
            , DaemonSpec.tests
            , DagSpec.tests
            , DebianPackageSpec.tests
            , DownTreeSpec.tests
            , FilesystemSpec.tests
            , FollowSchedulerSpec.tests
            , FollowSpec.tests
            , GcpSpec.tests
            , JWTSigningSpec.tests
            , LedgerSpec.tests
            , PodmanCommandSpec.tests
            , PodmanSpec.tests
            , PostgresBackupSpec.tests
            , PostgresClusterSpec.tests
            , PgBouncerSpec.tests
            , PostgresPairSpec.tests
            , PostgresTemplateSpec.tests
            , PostgresTlsSpec.tests
            , PostgrestCloudRunSpec.tests
            , QemuResolveKernelSpec.tests
            , QuerySpec.tests
            , ReportJsonSpec.tests
            , RewriteSpec.tests
            , ServeModelSpec.tests
            , ServeSocketSpec.tests
            , ServeSpec.tests
            , SystemdSpec.tests
            , UpTreeSpec.tests
            , UpkeepSpec.tests
            ]
