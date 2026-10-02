{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Haproxy": what it renders and
what it refuses to render.

The property the module exists for is not a line of configuration but the
absence of one: nothing in @haproxy.cfg@ says which member leads, so a
failover is not a configuration change (@specs\/pg-patroni.md@, T3). The
golden text below is that claim spelled out -- every member is behind every
listener, and only the health-check path differs.
-}
module Test.HaproxySpec (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Builtin.Nodes.Haproxy as Haproxy
import Test.Harness (runUp)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Haproxy"
        [ testCase "the leader port and the replica port, over the same members" $
            assertEqual "" golden (Haproxy.renderConfig cfg)
        , testCase "each role asks Patroni's own endpoint" $
            assertEqual
                ""
                ["/primary", "/replica", "/replica?lag=1048576", "/read-only"]
                (fmap Haproxy.checkPath [Haproxy.Primary, Haproxy.Replica, Haproxy.ReplicaWithin 1048576, Haproxy.ReadOnly])
        , testCase "members are listed in declared order under every listener, whoever leads" $ do
            let servers = filter ("    server " `Text.isPrefixOf`) (Text.lines (Haproxy.renderConfig cfg))
            assertEqual "" ["pg-a", "pg-b", "pg-c", "pg-a", "pg-b", "pg-c"] (fmap ((!! 1) . Text.words) servers)
        , testCase "a demoted leader's sessions are closed, not left writing into a replica" $
            assertBool "" ("on-marked-down shutdown-sessions" `Text.isInfixOf` Haproxy.renderConfig cfg)
        , testCase "the status page is there only when asked for" $ do
            assertBool "" (not ("listen stats" `Text.isInfixOf` Haproxy.renderConfig cfg))
            let withStats = Haproxy.renderConfig cfg{Haproxy.haproxy_stats = Just ("127.0.0.1", 7000)}
            assertBool "" ("listen stats\n    mode http\n    bind 127.0.0.1:7000\n" `Text.isInfixOf` withStats)
        , testCase "the watched file is the one rendered" $
            assertEqual "" "/etc/haproxy/haproxy.cfg" (Haproxy.configPath cfg)
        , testCase "a sound configuration has nothing said against it" $
            assertEqual "" [] (Haproxy.validate cfg)
        , testCase "objections are collected, not first-found" $
            assertEqual
                ""
                [ "listener name \"primary\" is declared more than once"
                , "member name \"pg-a\" is declared more than once"
                , "port 5000 is bound more than once"
                ]
                ( Haproxy.validate
                    cfg
                        { Haproxy.haproxy_listeners =
                            [ Haproxy.Listener "primary" "*" 5000 Haproxy.Primary
                            , Haproxy.Listener "primary" "*" 5000 Haproxy.Replica
                            ]
                        , Haproxy.haproxy_members = [member "pg-a" "10.0.0.1", member "pg-a" "10.0.0.2"]
                        }
                )
        , testCase "an empty configuration is refused" $
            assertEqual
                ""
                ["no listeners", "no members"]
                (Haproxy.validate cfg{Haproxy.haproxy_listeners = [], Haproxy.haproxy_members = []})
        , testCase "a name or host that would split into two words is refused" $ do
            let bad = Haproxy.validate cfg{Haproxy.haproxy_members = [(member "pg a" "10.0.0.1 backup")]}
            assertEqual
                ""
                [ "member name \"pg a\" is not a valid server name"
                , "member \"pg a\" has host \"10.0.0.1 backup\", which is not one word"
                ]
                bad
        , testCase "the status page's port and name are not a listener's to take" $
            assertEqual
                ""
                [ "listener name \"stats\" is taken by the status page"
                , "port 5000 is bound more than once"
                ]
                ( Haproxy.validate
                    cfg
                        { Haproxy.haproxy_stats = Just ("*", 5000)
                        , Haproxy.haproxy_listeners = [Haproxy.Listener "stats" "*" 5000 Haproxy.Primary]
                        }
                )
        , testCase "out-of-range ports and non-positive check settings are refused" $
            assertEqual
                ""
                [ "port 0 is not a port"
                , "port 70000 is not a port"
                , "checks need a positive interval, fall, rise and timeout"
                ]
                ( Haproxy.validate
                    cfg
                        { Haproxy.haproxy_members = [(member "pg-a" "10.0.0.1"){Haproxy.member_pg_port = 0, Haproxy.member_rest_port = 70000}]
                        , Haproxy.haproxy_checks = Haproxy.defaultChecks{Haproxy.checks_fall = 0}
                        }
                )
        , -- a file haproxy refuses is a router that stays down at its next
          -- restart, so the node fails instead of writing it.
          testCase "an invalid configuration fails the node rather than being written" $ do
            ok <- runUp (Haproxy.configFiles cfg{Haproxy.haproxy_config_dir = "/nonexistent-salmon-test", Haproxy.haproxy_members = []})
            assertBool "the pass reported a failure" (not ok)
        ]

member :: Text.Text -> Text.Text -> Haproxy.Member
member name host = Haproxy.Member name host 5432 8008

cfg :: Haproxy.HaproxyConfig
cfg = Haproxy.patroniRouter [member "pg-a" "10.0.0.1", member "pg-b" "10.0.0.2", member "pg-c" "10.0.0.3"]

golden :: Text.Text
golden =
    Text.unlines
        [ "# written by salmon (Salmon.Builtin.Nodes.Haproxy); edits are overwritten"
        , "global"
        , "    maxconn 100"
        , "    log stdout format short daemon"
        , ""
        , "defaults"
        , "    log global"
        , "    mode tcp"
        , "    retries 2"
        , "    timeout connect 4s"
        , "    timeout client 1800s"
        , "    timeout server 1800s"
        , "    timeout check 5s"
        , ""
        , "listen primary"
        , "    bind *:5000"
        , "    option httpchk GET /primary"
        , "    http-check expect status 200"
        , "    default-server inter 3s fall 3 rise 2 on-marked-down shutdown-sessions"
        , "    server pg-a 10.0.0.1:5432 maxconn 100 check port 8008"
        , "    server pg-b 10.0.0.2:5432 maxconn 100 check port 8008"
        , "    server pg-c 10.0.0.3:5432 maxconn 100 check port 8008"
        , ""
        , "listen replicas"
        , "    bind *:5001"
        , "    option httpchk GET /replica"
        , "    http-check expect status 200"
        , "    default-server inter 3s fall 3 rise 2 on-marked-down shutdown-sessions"
        , "    balance leastconn"
        , "    server pg-a 10.0.0.1:5432 maxconn 100 check port 8008"
        , "    server pg-b 10.0.0.2:5432 maxconn 100 check port 8008"
        , "    server pg-c 10.0.0.3:5432 maxconn 100 check port 8008"
        ]
