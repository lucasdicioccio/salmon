{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.WireGuard"'s checks and
commands: what @wg show IF dump@ and @ip -o ...@ output has to say about a
declared peer or interface, and the argv of the new @wg@ commands. The dump
lines are tab separated, as @wg@ prints them (interface line: 4 fields;
peer line: 8).
-}
module Test.WireGuardSpec (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.Binary (Command (..))
import Salmon.Builtin.Nodes.WireGuard
import System.Process.ListLike (CreateProcess (..), CmdSpec (..))

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.WireGuard"
        [ testGroup
            "interpretWgDump"
            [ testCase "a peer as declared is satisfied" $
                assertEqual "" Success (interpretWgDump spec dump)
            , testCase "a peer that is not on the interface is missing" $
                assertEqual "" (Failure "peer not on the interface") (interpretWgDump spec{specKey = "zzz="} dump)
            , testCase "changed allowed-ips are reported" $
                assertEqual "" (Failure "allowed-ips differ") (interpretWgDump spec{specAllowedIps = "10.0.0.2/32,10.9.0.0/16"} dump)
            , testCase "allowed-ips compare as a set, a bare address is its /32" $
                assertEqual "" Success (interpretWgDump spec{specAllowedIps = "10.1.0.0/24, 10.0.0.2"} dump)
            , testCase "a different endpoint address is reported" $
                assertEqual "" (Failure "endpoint differs") (interpretWgDump spec{specEndpoint = Just "5.6.7.8:51820"} dump)
            , testCase "an endpoint given as a host name is not compared" $
                assertEqual "" Success (interpretWgDump spec{specEndpoint = Just "vpn.example:51820"} dump)
            , testCase "no declared endpoint or keepalive means not compared" $
                assertEqual "" Success (interpretWgDump spec{specEndpoint = Nothing, specKeepalive = Nothing} dump)
            , testCase "a different keepalive is reported" $
                assertEqual "" (Failure "persistent-keepalive differs") (interpretWgDump spec{specKeepalive = Just 60} dump)
            , testCase "several differences are all named" $
                assertEqual
                    ""
                    (Failure "allowed-ips differ; persistent-keepalive differs")
                    (interpretWgDump spec{specAllowedIps = "1.1.1.1/32", specKeepalive = Just 5} dump)
            , testCase "the reason never quotes the dump" $
                case interpretWgDump spec{specKey = "zzz="} dump of
                    Failure msg -> assertBool "private key leaked" (not ("PRIVATEKEY" `Text.isInfixOf` msg))
                    other -> assertBool ("expected Failure, got " <> show other) False
            ]
        , testGroup
            "interpretIface"
            [ testCase "an up link with its address is satisfied" $
                assertEqual "" Success (interpretIface "wg0" net upLink addrs)
            , testCase "a link that is down is reported" $
                assertEqual "" (Failure "link not up: wg0") (interpretIface "wg0" net downLink addrs)
            , testCase "a missing address is reported" $
                assertEqual "" (Failure "address missing on wg0: 10.0.0.1/24") (interpretIface "wg0" net upLink "")
            , testCase "another address on the link does not count" $
                assertEqual
                    ""
                    (Failure "address missing on wg0: 10.0.0.1/24")
                    (interpretIface "wg0" net upLink "5: wg0    inet 10.0.0.9/24 scope global wg0\\       valid_lft forever\n")
            ]
        , testGroup
            "commands"
            [ testCase "removing a peer removes only that peer" $
                assertEqual "" (RawCommand "wg" ["set", "wg0", "peer", "abc=", "remove"]) (cmdspec (prepare wgcommand (RemovePeer "wg0" "abc=")))
            , testCase "the dump is read from the named interface" $
                assertEqual "" (RawCommand "wg" ["show", "wg0", "dump"]) (cmdspec (prepare wgcommand (ShowDump "wg0")))
            , testCase "the address is set, not added, so a second run is a no-op" $
                assertEqual
                    ""
                    (RawCommand "ip" ["address", "replace", "dev", "wg0", "10.0.0.1/24"])
                    (cmdspec (prepare ipcommand (SetWgAddr "wg0" net)))
            ]
        ]
  where
    net = Ipv4Cidr "10.0.0.1" 24
    spec = PeerSpec "peer1=" (Just "1.2.3.4:51820") "10.0.0.2/32, 10.1.0.0/24" (Just 25)
    dump =
        "PRIVATEKEY=\tpubself=\t51820\toff\n\
        \peer1=\t(none)\t1.2.3.4:51820\t10.0.0.2/32,10.1.0.0/24\t1700000000\t100\t200\t25\n\
        \peer2=\t(none)\t(none)\t(none)\t0\t0\t0\toff\n"
    upLink = "5: wg0: <POINTOPOINT,NOARP,UP,LOWER_UP> mtu 1420 qdisc noqueue state UNKNOWN mode DEFAULT group default qlen 1000\\    link/none \n"
    downLink = "5: wg0: <POINTOPOINT,NOARP> mtu 1420 qdisc noop state DOWN mode DEFAULT group default qlen 1000\\    link/none \n"
    addrs = "5: wg0    inet 10.0.0.1/24 scope global wg0\\       valid_lft forever preferred_lft forever\n"
