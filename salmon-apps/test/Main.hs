module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.FleetSpec as FleetSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-apps"
            [ FleetSpec.tests
            ]
