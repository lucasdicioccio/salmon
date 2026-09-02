module Main (main) where

import qualified Test.DebootstrapSpec as DebootstrapSpec
import qualified Test.DownTreeSpec as DownTreeSpec
import qualified Test.JWTSigningSpec as JWTSigningSpec
import qualified Test.PodmanSpec as PodmanSpec
import qualified Test.PostgresInitSpec as PostgresInitSpec
import qualified Test.PostgresReplicationSpec as PostgresReplicationSpec
import qualified Test.QemuResolveKernelSpec as QemuResolveKernelSpec
import qualified Test.QemuSmokeSpec as QemuSmokeSpec
import qualified Test.QuerySpec as QuerySpec
import qualified Test.ServeSpec as ServeSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-ops-recipes"
            [ DebootstrapSpec.tests
            , DownTreeSpec.tests
            , JWTSigningSpec.tests
            , PodmanSpec.tests
            , PostgresInitSpec.tests
            , PostgresReplicationSpec.tests
            , QemuResolveKernelSpec.tests
            , QemuSmokeSpec.tests
            , QuerySpec.tests
            , ServeSpec.tests
            ]
