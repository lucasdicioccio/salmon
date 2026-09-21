{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.PgBouncer"'s rendering.

Two of the fields here exist for @specs\/pg-switchover.md@ phase 4, and both
are about the same thing: moving traffic without dropping it. The admin
console is how a pause and a reload are asked for at all, and the routing
file is what keeps the node that moves traffic from fighting the node that
owns the service -- the ini is watched, so a change to it is applied by a
restart, and a restart drops every client the bouncer is holding.
-}
module Test.PgBouncerSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Builtin.Nodes.PgBouncer as PgBouncer

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.PgBouncer"
        [ testCase "a database line says where its upstream is" $
            assertBool ini ("app = host=10.0.0.1 port=5432 dbname=app" `isInfixOf` ini)
        , testCase "the admin console is opened to the users named, and nobody else" $ do
            assertBool ini ("admin_users = router" `isInfixOf` ini)
            assertBool minimalIni (not ("admin_users" `isInfixOf` minimalIni))
        , -- an include pgbouncer cannot read is a service that will not start
          testCase "the routing file is included when there is one" $ do
            assertBool ini ("%include /etc/pgbouncer/routing.ini" `isInfixOf` ini)
            assertBool minimalIni (not ("%include" `isInfixOf` minimalIni))
        , -- the whole point of the seam: this node restarts on a change to
          -- what it watches, and the routing file must never be that.
          testCase "and it is not one of the files a change to which restarts the service" $
            assertEqual
                ""
                ["/etc/pgbouncer/pgbouncer.ini", "/etc/pgbouncer/userlist.txt"]
                [PgBouncer.configPath cfg, PgBouncer.userlistPath cfg]
        ]
  where
    ini = Text.unpack (PgBouncer.renderIni cfg)
    minimalIni = Text.unpack (PgBouncer.renderIni cfg{PgBouncer.bouncer_admin_users = [], PgBouncer.bouncer_routing_file = Nothing})

cfg :: PgBouncer.BouncerConfig
cfg =
    PgBouncer.BouncerConfig
        { PgBouncer.bouncer_config_dir = "/etc/pgbouncer"
        , PgBouncer.bouncer_listen_addr = "0.0.0.0"
        , PgBouncer.bouncer_listen_port = 6432
        , PgBouncer.bouncer_databases = [PgBouncer.BouncerDatabase "app" (PgBouncer.UpstreamDb "10.0.0.1" 5432 "app")]
        , PgBouncer.bouncer_users = [PgBouncer.AuthUser "router" "hunter2"]
        , PgBouncer.bouncer_pool_mode = PgBouncer.TransactionPooling
        , PgBouncer.bouncer_max_client_conn = 100
        , PgBouncer.bouncer_default_pool_size = 20
        , PgBouncer.bouncer_admin_users = ["router"]
        , PgBouncer.bouncer_routing_file = Just "/etc/pgbouncer/routing.ini"
        }
