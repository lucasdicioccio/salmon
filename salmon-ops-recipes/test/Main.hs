module Main (main) where

import qualified Test.DownTreeSpec as DownTreeSpec
import qualified Test.JWTSigningSpec as JWTSigningSpec
import qualified Test.PodmanSpec as PodmanSpec
import qualified Test.PostgresInitSpec as PostgresInitSpec
import qualified Test.ServeSpec as ServeSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-ops-recipes"
            [ DownTreeSpec.tests
            , JWTSigningSpec.tests
            , PodmanSpec.tests
            , PostgresInitSpec.tests
            , ServeSpec.tests
            ]
