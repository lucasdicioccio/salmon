{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Etcd": rendering, the
membership verdict and the seed decision. The three-VM Layer 3 case (seed,
then a member restart without re-bootstrapping) needs the Patroni harness and
is not here.
-}
module Test.EtcdSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.Etcd

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Etcd"
        [ testCase "the config seeds with state new and lists every member" $ do
            assertBool cfgText ("initial-cluster-state: new" `isInfixOf` cfgText)
            assertBool cfgText ("initial-cluster: a=https://10.0.0.1:2380,b=https://10.0.0.2:2380,c=https://10.0.0.3:2380" `isInfixOf` cfgText)
            assertBool cfgText ("name: b" `isInfixOf` cfgText)
        , testCase "the initial cluster does not depend on declaration order" $
            assertEqual "" (renderInitialCluster members) (renderInitialCluster (reverse members))
        , testCase "TLS paths are used as given" $
            assertBool cfgText ("cert-file: /pki/peer.crt" `isInfixOf` cfgText)
        , testCase "the member list parses, and an unstarted member has no name" $
            assertEqual
                ""
                (Right [Listed "a" ["https://10.0.0.1:2380"], Listed "" ["https://10.0.0.9:2380"]])
                (parseMemberList "{\"header\":{},\"members\":[{\"ID\":1,\"name\":\"a\",\"peerURLs\":[\"https://10.0.0.1:2380\"]},{\"ID\":2,\"peerURLs\":[\"https://10.0.0.9:2380\"]}]}")
        , testCase "garbage is a Left, not an exception" $
            assertBool "" (either (const True) (const False) (parseMemberList "nope"))
        , testCase "membership matching the declaration is Success" $
            assertEqual "" Success (interpretMembers members (fmap listedOf members))
        , testCase "a missing member is a Failure naming it" $
            case interpretMembers members (fmap listedOf (take 2 members)) of
                Failure why -> assertBool (Text.unpack why) ("10.0.0.3" `isInfixOf` Text.unpack why)
                other -> assertFailure (show other)
        , testCase "an unexpected member is a Failure too" $
            case interpretMembers (take 2 members) (fmap listedOf members) of
                Failure why -> assertBool (Text.unpack why) ("unexpected" `isInfixOf` Text.unpack why)
                other -> assertFailure (show other)
        , testCase "seed: nobody answering is a seed" $
            assertEqual "" True (ok (seedDecision self [(m, Nothing) | m <- others]))
        , testCase "seed: a cluster that lists us is a late bootstrap member, not a refusal" $
            assertEqual "" True (ok (seedDecision self [(head others, Just (fmap listedOf members))]))
        , testCase "seed: a cluster that does not list us is refused" $
            assertEqual "" False (ok (seedDecision self [(head others, Just (fmap listedOf others))]))
        ]
  where
    cfgText = Text.unpack (renderConfig cfg)
    ok = either (const False) (const True)

members :: [Member]
members =
    [ Member "a" "https://10.0.0.1:2380" "https://10.0.0.1:2379"
    , Member "b" "https://10.0.0.2:2380" "https://10.0.0.2:2379"
    , Member "c" "https://10.0.0.3:2380" "https://10.0.0.3:2379"
    ]

self :: Member
self = members !! 1

others :: [Member]
others = filter (/= self) members

listedOf :: Member -> Listed
listedOf m = Listed (member_name m) [member_peer_url m]

cfg :: EtcdConfig
cfg =
    EtcdConfig
        { etcd_self = self
        , etcd_cluster = members
        , etcd_cluster_token = "tok"
        , etcd_data_dir = "/var/lib/etcd"
        , etcd_config_file = "/etc/etcd/etcd.yaml"
        , etcd_user = "etcd"
        , etcd_client_tls = TlsFiles "/pki/ca.crt" "/pki/client.crt" "/pki/client.key"
        , etcd_peer_tls = TlsFiles "/pki/ca.crt" "/pki/peer.crt" "/pki/peer.key"
        , etcd_ready_timeout_seconds = 30
        }
