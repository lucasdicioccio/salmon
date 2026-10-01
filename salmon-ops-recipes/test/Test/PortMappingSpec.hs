{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.PortMapping": the parsers over
captured @upnpc@ output, and the plan/verdict each listing leads to. No
gateway, no network.

Two fixtures are real captures (miniupnpc 2.2.6, an owner's LAN, UUID
redacted): "nothing answered" and "a WPS device answered but it is not an
IGD". The listings of a real IGD are written from @upnpc.c@'s format and have
not been captured from a live router.
-}
module Test.PortMappingSpec (tests) where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.PortMapping

notIgdOutput :: Text
notIgdOutput =
    Text.unlines
        [ "No valid UPNP Internet Gateway Device found."
        , "upnpc : miniupnpc library test client, version 2.2.6."
        , " (c) 2005-2024 Thomas Bernard."
        , "List of UPNP devices found on the network :"
        , " desc: http://192.168.1.1:1990/<uuid>/WFADevice.xml"
        , " st: upnp:rootdevice"
        , ""
        , "UPnP device found. Is it an IGD ? : http://192.168.1.1:1990/"
        ]

noDeviceOutput :: Text
noDeviceOutput =
    Text.unlines
        [ "No valid UPNP Internet Gateway Device found."
        , "upnpc : miniupnpc library test client, version 2.2.6."
        ]

igdListing :: Text -> [Text] -> Text
igdListing ext rows =
    Text.unlines $
        [ "upnpc : miniupnpc library test client, version 2.2.6."
        , "List of UPNP devices found on the network :"
        , "Found valid IGD : http://192.168.1.1:5000/ctl/IPConn"
        , "Local LAN ip address : 192.168.1.5"
        , "Connection Type : IP_Routed"
        , "Status : Connected, uptime=100s, LastConnectionError : ERROR_NONE"
        , "ExternalIPAddress = " <> ext
        , " i protocol exPort->inAddr:inPort description remoteHost leaseTime"
        ]
            <> rows

pm :: PortMap
pm = PortMap "wg0" "192.168.1.5" 51820 51820 UDP 3600

ours :: Text
ours = " 0 UDP 51820->192.168.1.5:51820 'salmon:wg0' '' 3600"

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.PortMapping"
        [ testCase "gateway: nothing answered" $ assertEqual "" NoDevice (parseGateway noDeviceOutput)
        , testCase "gateway: a device answered but is not an IGD (real capture)" $ assertEqual "" NotIgd (parseGateway notIgdOutput)
        , testCase "gateway: an IGD with its external address" $
            assertEqual "" (Igd (Just "203.0.113.7") (Just "192.168.1.5")) (parseGateway (igdListing "203.0.113.7" []))
        , testCase "mappings: a listing is parsed, including a description with spaces" $
            assertEqual
                ""
                [ Mapping UDP 51820 "192.168.1.5" 51820 "salmon:wg0" "" 3600
                , Mapping TCP 80 "192.168.1.9" 8080 "my web server" "" 0
                ]
                (parseMappings (igdListing "203.0.113.7" [ours, " 1 TCP    80->192.168.1.9:8080  'my web server' '' 0"]))
        , testCase "mappings: header and garbage lines are skipped" $
            assertEqual "" [] (parseMappings (igdListing "203.0.113.7" ["GetGenericPortMappingEntry() returned 713 (SpecifiedArrayIndexInvalid)"]))
        , testCase "address classes" $
            assertEqual
                ""
                [Public, Private, Private, Private, CGNAT, CGNAT, Public, Public, Unparseable]
                (map classifyAddress ["203.0.113.7", "10.1.2.3", "172.16.0.1", "192.168.1.1", "100.64.0.1", "100.127.255.1", "100.128.0.1", "172.32.0.1", "not an address"])
        , testCase "plan: present with our address is in place, check Success" $ do
            let out = igdListing "203.0.113.7" [ours]
            assertEqual "" InPlace (planFor pm out)
            assertBool "success" (isSuccess (checkOf (planFor pm out)))
        , testCase "plan: absent needs an add, check Failure" $ do
            let out = igdListing "203.0.113.7" []
            assertBool "needs add" (isNeedsAdd (planFor pm out))
            assertBool "failure" (isFailure (checkOf (planFor pm out)))
        , testCase "plan: a hand-configured map pointing at us is satisfied (not ours to delete)" $ do
            let out = igdListing "203.0.113.7" [" 0 UDP 51820->192.168.1.5:51820 'by hand' '' 0"]
            assertEqual "" InPlace (planFor pm out)
            assertEqual "" (NotOurs "refusing to delete mapping 'by hand' on external port 51820: it does not carry our marker salmon:wg0") (teardownFor pm out)
        , testCase "plan: somebody else's mapping is Taken, never taken over" $ do
            let out = igdListing "203.0.113.7" [" 0 UDP 51820->192.168.1.77:51820 'laptop' '' 0"]
            assertBool "taken" (isTaken (planFor pm out))
            assertBool "failure" (isFailure (checkOf (planFor pm out)))
        , testCase "plan: our own stale mapping (pointing elsewhere) is re-added" $ do
            let out = igdListing "203.0.113.7" [" 0 UDP 51820->192.168.1.77:51820 'salmon:wg0' '' 3600"]
            assertBool "needs add" (isNeedsAdd (planFor pm out))
        , testCase "plan: a mapping near expiry is re-added" $ do
            let out = igdListing "203.0.113.7" [" 0 UDP 51820->192.168.1.5:51820 'salmon:wg0' '' 600"]
            assertBool "needs add" (isNeedsAdd (planFor pm out))
        , testCase "plan: the same external port under the other protocol is not ours" $ do
            let out = igdListing "203.0.113.7" [" 0 TCP 51820->192.168.1.77:51820 'laptop' '' 0"]
            assertBool "needs add" (isNeedsAdd (planFor pm out))
        , testCase "plan: a private external address is double NAT, Failure with a reason" $ do
            let out = igdListing "192.168.0.2" [ours]
            case checkOf (planFor pm out) of
                Failure r -> assertBool "names the problem" ("cannot be reached" `Text.isInfixOf` r)
                other -> assertBool ("expected Failure, got " <> show other) False
        , testCase "plan: a CGNAT external address is double NAT too" $
            assertBool "double nat" (isDoubleNat (planFor pm (igdListing "100.64.3.2" [])))
        , testCase "plan: no device and not-an-IGD are Unknown, not Failure (real captures)" $ do
            assertBool "unknown" (isUnknown (checkOf (planFor pm noDeviceOutput)))
            assertBool "unknown" (isUnknown (checkOf (planFor pm notIgdOutput)))
        , testCase "teardown: absent is already gone, ours is deleted, no gateway cannot tell" $ do
            assertEqual "" AlreadyGone (teardownFor pm (igdListing "203.0.113.7" []))
            assertEqual "" DeleteIt (teardownFor pm (igdListing "203.0.113.7" [ours]))
            assertBool "cannot tell" (case teardownFor pm notIgdOutput of CannotTell _ -> True; _ -> False)
        ]
  where
    isSuccess Success = True
    isSuccess _ = False
    isFailure (Failure _) = True
    isFailure _ = False
    isUnknown Unknown = True
    isUnknown _ = False
    isNeedsAdd (NeedsAdd _) = True
    isNeedsAdd _ = False
    isTaken (Taken _) = True
    isTaken _ = False
    isDoubleNat (DoubleNat _) = True
    isDoubleNat _ = False
