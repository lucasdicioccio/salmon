module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.FleetSpec as FleetSpec
import qualified Test.ReportSpec as ReportSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-apps"
            [ FleetSpec.tests
            , ReportSpec.tests
            ]
