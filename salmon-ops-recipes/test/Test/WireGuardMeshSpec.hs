{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 for "SreBox.WireGuardMesh": a table of seeds to per-host views
(full mesh, a peer without an endpoint, a router, an exit node, policies) and
the refusals. No Op, no IO.
-}
module Test.WireGuardMeshSpec (tests) where

import Data.Aeson (decode, encode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import SreBox.WireGuardMesh

peerD :: Text -> Text -> Maybe Text -> [Text] -> PeerDecl
peerD n a ep gs = PeerDecl n a (n <> "-key=") ep gs

seed :: MeshSeed
seed =
    MeshSeed
        { mesh_subnet = "10.66.0.0/24"
        , mesh_iface = "wg0"
        , mesh_port = 51820
        , mesh_privkey_path = "/etc/wireguard/wg0.key"
        , mesh_peers =
            [ peerD "a" "10.66.0.1" (Just "a.example:51820") ["web"]
            , peerD "b" "10.66.0.2" (Just "b.example:51820") ["db"]
            , peerD "c" "10.66.0.3" Nothing ["web"]
            ]
        , mesh_policies = []
        , mesh_routers = []
        }

host :: MeshSeed -> Text -> HostSpec
host s n = either (error . show) id (genHost s n)

linkNames :: HostSpec -> [Text]
linkNames h = link_name <$> host_peers h

link :: HostSpec -> Text -> PeerLink
link h n = case [l | l <- host_peers h, link_name l == n] of
    (l : _) -> l
    [] -> error ("no link " <> show n)

tests :: TestTree
tests =
    testGroup
        "SreBox.WireGuardMesh"
        [ testGroup
            "fan-out"
            [ testCase "hosts with endpoints peer with everyone, as /32s" $ do
                let a = host seed "a"
                assertEqual "" ["b", "c"] (linkNames a)
                assertEqual "" ["10.66.0.2/32"] (link_allowed_ips (link a "b"))
                assertEqual "" (Just "b.example:51820") (link_endpoint (link a "b"))
                assertEqual "" "10.66.0.1/24" (host_addr a)
            , testCase "a host with an endpoint does not keepalive, and does not dial a peer without one" $ do
                let a = host seed "a"
                assertEqual "" Nothing (link_endpoint (link a "c"))
                assertEqual "" Nothing (link_keepalive (link a "c"))
            , testCase "a peer with no endpoint dials out and keeps alive" $ do
                let c = host seed "c"
                assertEqual "" ["a", "b"] (linkNames c)
                assertEqual "" (Just 25) (link_keepalive (link c "a"))
            , testCase "two peers without endpoints have no link" $ do
                let s = seed{mesh_peers = mesh_peers seed <> [peerD "d" "10.66.0.4" Nothing []]}
                assertEqual "" ["a", "b"] (linkNames (host s "d"))
                assertEqual "" ["a", "b"] (linkNames (host s "c"))
            , testCase "genAll covers every peer, in order" $
                assertEqual "" (Right ["a", "b", "c"]) (fmap (fmap host_name) (genAll seed))
            , testCase "adding a peer changes nobody else's address" $ do
                let s = seed{mesh_peers = mesh_peers seed <> [peerD "d" "10.66.0.9" (Just "d.example:1") []]}
                assertEqual "" (host_addr (host seed "b")) (host_addr (host s "b"))
            ]
        , testGroup
            "routers"
            [ testCase "a router's network rides its allowed-ips on the others, and they route it" $ do
                let s = seed{mesh_routers = [RouterDecl "a" "192.168.5.0/24"]}
                    b = host s "b"
                assertEqual "" ["10.66.0.1/32", "192.168.5.0/24"] (link_allowed_ips (link b "a"))
                assertEqual "" ["192.168.5.0/24"] (host_routes b)
                assertEqual "" (Just "10.66.0.0/24") (host_forwards (host s "a"))
                assertEqual "" Nothing (host_forwards b)
                assertEqual "" [] (host_routes (host s "a"))
            , testCase "an exit node: split default routes and the endpoint pinned" $ do
                let s =
                        seed
                            { mesh_peers = peerD "a" "10.66.0.1" (Just "203.0.113.7:51820") ["web"] : drop 1 (mesh_peers seed)
                            , mesh_routers = [RouterDecl "a" "0.0.0.0/0"]
                            }
                    b = host s "b"
                assertEqual "" ["0.0.0.0/1", "128.0.0.0/1"] (host_routes b)
                assertEqual "" ["203.0.113.7"] (host_exit_pins b)
                assertEqual "" ["10.66.0.1/32", "0.0.0.0/0"] (link_allowed_ips (link b "a"))
            ]
        , testGroup
            "policies"
            [ testCase "a rule per policy on the hosts in its destination groups" $ do
                let s = seed{mesh_policies = [Policy ["web"] ["db"] Tcp [PortRange 5432 5432]]}
                assertEqual "" [] (host_input_rules (host s "a"))
                assertEqual
                    ""
                    [NftRuleSpec ["iifname", "\"wg0\"", "ip", "saddr", "{ 10.66.0.1, 10.66.0.3 }", "tcp", "dport", "5432", "accept"]]
                    (host_input_rules (host s "b"))
            , testCase "ranges and several ports render as a set" $ do
                let s = seed{mesh_policies = [Policy ["db"] ["web"] Udp [PortRange 53 53, PortRange 8000 8100]]}
                assertEqual
                    ""
                    [NftRuleSpec ["iifname", "\"wg0\"", "ip", "saddr", "10.66.0.2", "udp", "dport", "{ 53, 8000-8100 }", "accept"]]
                    (host_input_rules (host s "a"))
            , testCase "icmp and any" $ do
                let s = seed{mesh_policies = [Policy ["db"] ["web"] Icmp [], Policy ["db"] ["web"] AnyProto []]}
                assertEqual
                    ""
                    [ NftRuleSpec ["iifname", "\"wg0\"", "ip", "saddr", "10.66.0.2", "ip", "protocol", "icmp", "accept"]
                    , NftRuleSpec ["iifname", "\"wg0\"", "ip", "saddr", "10.66.0.2", "accept"]
                    ]
                    (host_input_rules (host s "a"))
            ]
        , testGroup
            "refusals"
            [ refused "two peers with one address" (seed{mesh_peers = mesh_peers seed <> [peerD "d" "10.66.0.1" Nothing []]}) $ \es ->
                DuplicateAddress "10.66.0.1" ["a", "d"] `elem` es
            , refused "a peer outside the subnet" (seed{mesh_peers = mesh_peers seed <> [peerD "d" "10.67.0.1" Nothing []]}) $ \es ->
                AddressOutsideSubnet "d" "10.67.0.1" `elem` es
            , refused "an address that is not IPv4" (seed{mesh_peers = mesh_peers seed <> [peerD "d" "10.66.0.300" Nothing []]}) $ \es ->
                BadAddress "d" "10.66.0.300" `elem` es
            , refused "a policy naming an unknown group" (seed{mesh_policies = [Policy ["web"] ["nope"] Tcp []]}) $ \es ->
                UnknownGroup 1 "nope" `elem` es
            , refused "ports on icmp" (seed{mesh_policies = [Policy ["web"] ["db"] Icmp [PortRange 1 2]]}) $ \es ->
                any isBadPorts es
            , refused "a reversed port range" (seed{mesh_policies = [Policy ["web"] ["db"] Tcp [PortRange 9 1]]}) $ \es ->
                any isBadPorts es
            , refused "a router that is no peer" (seed{mesh_routers = [RouterDecl "zz" "10.9.0.0/16"]}) $ \es ->
                UnknownRouterPeer "zz" `elem` es
            , refused "two exit nodes" (seed{mesh_routers = [RouterDecl "a" "0.0.0.0/0", RouterDecl "b" "0.0.0.0/0"]}) $ \es ->
                any isSeveralExit es
            , refused "an exit node endpoint that is a name" (seed{mesh_routers = [RouterDecl "a" "0.0.0.0/0"]}) $ \es ->
                ExitEndpointNotLiteral "a" `elem` es
            , refused "a duplicate public key" (seed{mesh_peers = [p{peer_pubkey = "same="} | p <- mesh_peers seed]}) $ \es ->
                any isDupKey es
            , refused "a bad subnet" (seed{mesh_subnet = "10.66.0.0"}) $ \es ->
                BadSubnet "10.66.0.0" `elem` es
            , testCase "an unknown host" $
                assertEqual "" (Left [UnknownHost "zz"]) (genHost seed "zz")
            , testCase "refusals are reported together" $
                case genAll seed{mesh_subnet = "x", mesh_policies = [Policy ["web"] ["nope"] Tcp []]} of
                    Left es -> assertBool (show es) (length es >= 2)
                    Right _ -> assertFailure "accepted"
            ]
        , testGroup
            "pure ipv4"
            [ testCase "parse" $ do
                assertEqual "" (Just 0x0A420001) (parseIpv4 "10.66.0.1")
                assertEqual "" Nothing (parseIpv4 "10.66.0")
                assertEqual "" Nothing (parseIpv4 "10.66.0.256")
            , testCase "membership" $ do
                assertBool "in" (maybe False (\n -> maybe False (inSubnet n) (parseIpv4 "10.66.0.200")) (parseCidr "10.66.0.0/24"))
                assertBool "out" (not (maybe True (\n -> maybe True (inSubnet n) (parseIpv4 "10.66.1.1")) (parseCidr "10.66.0.0/24")))
            ]
        , testGroup
            "delivery"
            [ testCase "a host spec survives JSON" $
                assertEqual "" (Just (host seed "a")) (decode (encode (host seed "a")))
            , testCase "a seed survives JSON" $
                assertEqual "" (Just seed) (decode (encode seed))
            , testCase "a refused seed produces no documents" $
                assertBool "" (isLeft (genAll seed{mesh_subnet = "x"}))
            ]
        ]
  where
    refused name s p = testCase ("refuses " <> name) $ case genAll s of
        Left es -> assertBool (show es) (p es)
        Right _ -> assertFailure "accepted"
    isBadPorts e = case e of BadPorts _ _ -> True; _ -> False
    isSeveralExit e = case e of SeveralExitRouters _ -> True; _ -> False
    isDupKey e = case e of DuplicatePubkey _ -> True; _ -> False
