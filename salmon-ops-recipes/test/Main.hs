module Main (main) where

import qualified Test.JWTSigningSpec as JWTSigningSpec
import qualified Test.PodmanSpec as PodmanSpec
import qualified Test.PostgresInitSpec as PostgresInitSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-ops-recipes"
            [ JWTSigningSpec.tests
            , PodmanSpec.tests
            , PostgresInitSpec.tests
            ]
