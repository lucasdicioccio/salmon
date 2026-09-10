module Main (main) where

import qualified Test.CheckSpec as CheckSpec
import qualified Test.ConcurrentSpec as ConcurrentSpec
import qualified Test.DaemonSpec as DaemonSpec
import qualified Test.DagSpec as DagSpec
import qualified Test.DebootstrapSpec as DebootstrapSpec
import qualified Test.DownTreeSpec as DownTreeSpec
import qualified Test.FilesystemSpec as FilesystemSpec
import qualified Test.JWTSigningSpec as JWTSigningSpec
import qualified Test.LedgerSpec as LedgerSpec
import qualified Test.PodmanSpec as PodmanSpec
import qualified Test.PostgresInitSpec as PostgresInitSpec
import qualified Test.PostgresReplicationSpec as PostgresReplicationSpec
import qualified Test.QemuResolveKernelSpec as QemuResolveKernelSpec
import qualified Test.QemuSmokeSpec as QemuSmokeSpec
import qualified Test.QuerySpec as QuerySpec
import qualified Test.RewriteSpec as RewriteSpec
import qualified Test.ServeSpec as ServeSpec
import qualified Test.SystemdSpec as SystemdSpec
import qualified Test.UpTreeSpec as UpTreeSpec
import qualified Test.UpkeepSpec as UpkeepSpec

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-ops-recipes"
            [ CheckSpec.tests
            , ConcurrentSpec.tests
            , DaemonSpec.tests
            , DagSpec.tests
            , DebootstrapSpec.tests
            , DownTreeSpec.tests
            , FilesystemSpec.tests
            , JWTSigningSpec.tests
            , LedgerSpec.tests
            , PodmanSpec.tests
            , PostgresInitSpec.tests
            , PostgresReplicationSpec.tests
            , QemuResolveKernelSpec.tests
            , QemuSmokeSpec.tests
            , QuerySpec.tests
            , RewriteSpec.tests
            , ServeSpec.tests
            , SystemdSpec.tests
            , UpTreeSpec.tests
            , UpkeepSpec.tests
            ]
