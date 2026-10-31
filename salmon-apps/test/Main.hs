module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.ClickHouseToySpec as ClickHouseToySpec
import qualified Test.CloudrunSpec as CloudrunSpec
import qualified Test.FleetSpec as FleetSpec
import qualified Test.GcpToySpec as GcpToySpec
import qualified Test.QemuPgHaToySpec as QemuPgHaToySpec
import qualified Test.ReportHairpinSpec as ReportHairpinSpec
import qualified Test.ReportInboundSpec as ReportInboundSpec
import qualified Test.ReportSpec as ReportSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "salmon-apps"
            [ ClickHouseToySpec.tests
            , CloudrunSpec.tests
            , FleetSpec.tests
            , GcpToySpec.tests
            , QemuPgHaToySpec.tests
            , ReportHairpinSpec.tests
            , ReportInboundSpec.tests
            , ReportSpec.tests
            ]
