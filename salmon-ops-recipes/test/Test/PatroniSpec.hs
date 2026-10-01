{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Patroni" and the masked-unit
node it leans on: rendering, the REST verdict, and what the check reads from
@systemctl is-enabled@. The three-VM case needs the Patroni harness.
-}
module Test.PatroniSpec (tests) where

import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.Etcd (TlsFiles (..))
import Salmon.Builtin.Nodes.Patroni
import Salmon.Builtin.Nodes.Systemd (interpretIsEnabled)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Patroni"
        [ testCase "the config declares the scope, the member and the common layout" $ do
            has "scope: \"demo\""
            has "name: \"n2\""
            has "data_dir: \"/var/lib/postgresql/16/main\""
            has "config_dir: \"/etc/postgresql/16/main\""
            has "bin_dir: \"/usr/lib/postgresql/16/bin\""
        , testCase "etcd is v3, with its TLS paths as given" $ do
            has "etcd3:"
            has "hosts: \"10.0.0.1:2379,10.0.0.2:2379\""
            has "cacert: \"/pki/ca.crt\""
            assertBool "no v2 section" (not ("\netcd:" `isInfixOf` rendered))
        , testCase "plain etcd has no TLS keys" $
            assertBool "" (not ("cacert" `isInfixOf` Text.unpack (renderPatroni cfg{pat_etcd_tls = Nothing})))
        , testCase "no credential is rendered" $ do
            assertBool "" (not ("password" `isInfixOf` rendered))
            assertBool "" (not ("authentication" `isInfixOf` rendered))
        , testCase "the render is sensitive to a change" $
            assertBool "" (renderPatroni cfg /= renderPatroni cfg{pat_name = "n3"})
        , testCase "bootstrap parameters carry wal_log_hints and the hba lines" $ do
            has "wal_log_hints: \"on\""
            has "- \"host replication repl 10.0.0.0/24 scram-sha-256\""
        , testCase "the Debian unit to mask is the cluster's own" $
            assertEqual "" "postgresql@16-main.service" (debianClusterUnit cfg)
        , testCase "running and healthy is Success, whatever the role" $ do
            assertEqual "leader" Success (verdict 200 (body "running" "master"))
            assertEqual "replica" Success (verdict 200 (body "running" "replica"))
        , testCase "running while /health fails is a Failure" $
            assertBool "" (isFailure (verdict 503 (body "running" "replica")))
        , testCase "transitional states are Unknown, not Failure" $ do
            assertEqual "starting" Unknown (verdict 503 (body "starting" "replica"))
            assertEqual "restarting" Unknown (verdict 503 (body "restarting" "master"))
        , testCase "an uninitialised member is Unknown" $
            assertEqual "" Unknown (verdict 503 (body "stopped" "uninitialized"))
        , testCase "stopped and crashed are Failures naming the state" $ do
            assertBool "" (isFailure (verdict 503 (body "stopped" "replica")))
            case verdict 503 (body "crashed" "replica") of
                Failure why -> assertBool (Text.unpack why) ("crashed" `isInfixOf` Text.unpack why)
                other -> assertFailure (show other)
        , testCase "another scope is a Failure" $
            assertBool "" (isFailure (interpretMember "other" 200 (body "running" "replica")))
        , testCase "pending_restart does not change the verdict" $
            assertEqual "" Success (verdict 200 "{\"state\":\"running\",\"role\":\"replica\",\"pending_restart\":true,\"patroni\":{\"scope\":\"demo\"}}")
        , testCase "garbage is a Failure, not an exception" $
            assertBool "" (isFailure (verdict 200 "nope"))
        , testCase "is-enabled: only masked is satisfied" $ do
            assertEqual "masked" Success (interpretIsEnabled "masked\n")
            assertBool "disabled" (isFailure (interpretIsEnabled "disabled\n"))
            assertBool "enabled" (isFailure (interpretIsEnabled "enabled\n"))
            assertBool "runtime mask" (isFailure (interpretIsEnabled "masked-runtime\n"))
            assertEqual "nothing said" Unknown (interpretIsEnabled "")
        ]
  where
    rendered = Text.unpack (renderPatroni cfg)
    has s = assertBool (s <> " in:\n" <> rendered) (s `isInfixOf` rendered)
    verdict = interpretMember "demo"
    isFailure (Failure _) = True
    isFailure _ = False

body :: Text -> Text -> ByteString
body st role =
    Text.encodeUtf8 ("{\"state\":\"" <> st <> "\",\"role\":\"" <> role <> "\",\"patroni\":{\"scope\":\"demo\"}}")

cfg :: PatroniConfig
cfg =
    PatroniConfig
        { pat_scope = "demo"
        , pat_namespace = "/service/"
        , pat_name = "n2"
        , pat_etcd_hosts = ["10.0.0.1:2379", "10.0.0.2:2379"]
        , pat_etcd_tls = Just (TlsFiles "/pki/ca.crt" "/pki/c.crt" "/pki/c.key")
        , pat_rest_listen = "0.0.0.0:8008"
        , pat_rest_connect_address = "10.0.0.2:8008"
        , pat_pg_version = "16"
        , pat_pg_cluster = "main"
        , pat_pg_listen = "0.0.0.0:5432"
        , pat_pg_connect_address = "10.0.0.2:5432"
        , pat_pg_hba = ["host replication repl 10.0.0.0/24 scram-sha-256"]
        , pat_bootstrap_parameters = defaultBootstrapParameters
        , pat_config_dir = "/etc/patroni"
        , pat_secrets_file = "/etc/patroni/secrets.yml"
        , pat_user = "postgres"
        , pat_binary = "/usr/bin/patroni"
        , pat_ready_timeout_seconds = 60
        }
