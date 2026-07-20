module Main (main) where

import qualified Test.JWTSigningSpec as JWTSigningSpec
import qualified Test.PodmanSpec as PodmanSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-ops-recipes"
            [ JWTSigningSpec.tests
            , PodmanSpec.tests
            ]
