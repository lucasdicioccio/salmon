{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.ClickHouse".

Layer 0: the drop-ins, the quadlet, the SQL each node sends, and the
readings of what the server answers. Layer 2 ('liveServer'): the official
image run under podman with the rendered drop-ins mounted, and the database,
user and grant nodes run against it through @podman exec@, twice, then with
a rotated password. The quadlet itself ('ClickHouse.serverOp') is Layer 0
only here: it is a system-scope unit.
-}
module Test.ClickHouseSpec (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import qualified Data.ByteString as ByteString
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Op.Actions (Act (..))
import Salmon.Op.OpGraph (inject)
import qualified Salmon.Builtin.Nodes.ClickHouse as ClickHouse
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Readiness as Readiness
import Salmon.Reporter (silent)
import Salmon.Op.Track (Track (..))
import Test.Harness

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.ClickHouse"
        [ testGroup "Layer 0" layer0
        , testCase "Layer 2: database, user and grant nodes against the official image under podman" liveServer
        ]

layer0 :: [TestTree]
layer0 =
    [ testCase "the default configuration renders the root element and nothing else" $
        assertEqual "" "<clickhouse>\n</clickhouse>\n" (ClickHouse.renderServerConfig ClickHouse.defaultServerConfig)
    , testCase "listen hosts, ports and settings are rendered, values escaped" $ do
        let cfg =
                ClickHouse.defaultServerConfig
                    { ClickHouse.serverListenHosts = ["0.0.0.0"]
                    , ClickHouse.serverTcpPort = Just 9440
                    , ClickHouse.serverHttpPort = Just 8124
                    , ClickHouse.serverSettings = [("display_name", "a<b & 'c'")]
                    }
            out = Text.unpack (ClickHouse.renderServerConfig cfg)
        forM_
            [ "<listen_host>0.0.0.0</listen_host>"
            , "<tcp_port>9440</tcp_port>"
            , "<http_port>8124</http_port>"
            , "<display_name>a&lt;b &amp; &apos;c&apos;</display_name>"
            ]
            $ \line -> assertBool (line <> " in\n" <> out) (line `isInfixOf` out)
    , testCase "a setting that is not a plain element name is refused" $ do
        let bad = ClickHouse.defaultServerConfig{ClickHouse.serverSettings = [("a><evil", "1"), ("9lives", "1"), ("max_connections", "10")]}
        assertEqual "" 2 (length (ClickHouse.configProblems bad))
    , testCase "the admin drop-in grants access management and replaces default's networks with loopback" $ do
        let out = Text.unpack ClickHouse.renderAdminAccess
        assertBool out ("<access_management>1</access_management>" `isInfixOf` out)
        assertBool out ("<networks replace=\"replace\">" `isInfixOf` out)
        assertBool out ("<ip>127.0.0.1</ip>" `isInfixOf` out && "<ip>::1</ip>" `isInfixOf` out)
        assertBool out (not ("::/0" `isInfixOf` out))
    , testCase "identifiers and literals are quoted with their quotes escaped" $ do
        assertEqual "" "`we\\`ird`" (ClickHouse.quoteIdent "we`ird")
        assertEqual "" "'it\\'s \\\\ x'" (ClickHouse.quoteLiteral "it's \\ x")
    , testCase "a database is created if missing" $
        assertEqual "" "CREATE DATABASE IF NOT EXISTS `analytics`;\n" (ClickHouse.createDatabaseSql "analytics")
    , testCase "ClickHouse's sha256_hash is the SHA-256 of password then salt" $
        -- printf 's3cretabcd' | sha256sum; and ClickHouse 25.8 accepted
        -- the password once this was set (see liveServer)
        assertEqual "" "35496151d4e1741c05ddca164851a963765345081a76900479fb388696bc073d" (ClickHouse.sha256Hash (ClickHouse.Password "s3cret") "abcd")
    , testCase "the user statements create then alter, with the hash and never the password" $ do
        let sql = Text.unpack (ClickHouse.userSql "peer" (ClickHouse.Password "s3cret") (ByteString.pack [0xab, 0xcd]))
        assertBool sql ("CREATE USER IF NOT EXISTS `peer` IDENTIFIED WITH sha256_hash BY '35496151d4e1741c05ddca164851a963765345081a76900479fb388696bc073d' SALT 'abcd';" `isInfixOf` sql)
        assertBool sql ("ALTER USER `peer` IDENTIFIED WITH sha256_hash" `isInfixOf` sql)
        assertBool sql (not ("s3cret" `isInfixOf` sql))
    , testCase "redaction takes the hash and the salt out of client output" $
        assertEqual "" "failed at '<redacted>' SALT '<redacted>'" (ClickHouse.redactAll ["deadbeef", "abcd"] "failed at 'deadbeef' SALT 'abcd'")
    , testCase "a password-free help, notes and argv for the user node" $ do
        let c = ClickHouse.Client (ClickHouse.InContainer (Podman.ContainerName "ch")) 9000
            argv = ClickHouse.loginArgv c "peer"
        assertEqual "" ["podman", "exec", "-i", "-e", "CLICKHOUSE_PASSWORD", "ch", "clickhouse-client", "--port", "9000", "--user", "peer", "--query", "SELECT currentUser()"] argv
        let o = ClickHouse.userPassFile silent ignoreTrack c (FS.PreExisting "/run/secrets/ch-peer") "peer"
        case opAct o of
            Just a -> assertBool "the help names the file" ("/run/secrets/ch-peer" `Text.isInfixOf` a.extension.help)
            Nothing -> assertFailure "an actionless node"
    , testCase "the counts read" $ do
        assertEqual "" Success (ClickHouse.interpretCount "x" "1\n")
        assertEqual "" (Failure "x") (ClickHouse.interpretCount "x" "0\n")
        assertEqual "" Unknown (ClickHouse.interpretCount "x" "")
    , testCase "a refused login is a Failure, anything else that fails is Unknown" $ do
        assertEqual "" Success (ClickHouse.interpretLogin "peer" ExitSuccess "")
        assertBool "refused" $ case ClickHouse.interpretLogin "peer" (ExitFailure 4) "Code: 516. DB::Exception: peer: Authentication failed (AUTHENTICATION_FAILED)" of
            Failure _ -> True
            _ -> False
        assertEqual "" Unknown (ClickHouse.interpretLogin "peer" (ExitFailure 210) "Connection refused (NETWORK_ERROR)")
    , testCase "privileges are compared without case, and one missing is named" $ do
        let g = ClickHouse.Grant ["select", "create  table", "dictGet"] "analytics" "peer"
        assertEqual "" "GRANT SELECT, CREATE TABLE, DICTGET ON `analytics`.* TO `peer`;\n" (ClickHouse.grantSql g)
        assertEqual "" Success (ClickHouse.interpretGrants g "SELECT\nINSERT\nCREATE TABLE\ndictGet\n")
        assertEqual "" (Failure "peer lacks CREATE TABLE on analytics.*") (ClickHouse.interpretGrants g "SELECT\ndictGet\n")
    , testCase "the quadlet mounts data and drop-ins, watches the drop-ins, and waits for SELECT 1" $ do
        let s = ClickHouse.server (Podman.ContainerName "ch") "docker.io/clickhouse/clickhouse-server:25.8" "/etc/salmon/ch" "/var/lib/salmon/ch"
            c = ClickHouse.serverContainer s
            out = Text.unpack (Quadlet.renderContainer c)
        assertEqual "no problems" [] (Quadlet.containerProblems c)
        forM_
            [ "Image=docker.io/clickhouse/clickhouse-server:25.8"
            , "Volume=/var/lib/salmon/ch:/var/lib/clickhouse:rw"
            , "Volume=/etc/salmon/ch/salmon-server.xml:/etc/clickhouse-server/config.d/salmon-server.xml:ro"
            , "Volume=/etc/salmon/ch/salmon-admin.xml:/etc/clickhouse-server/users.d/salmon-admin.xml:ro"
            ]
            $ \line -> assertBool (line <> " in\n" <> out) (line `isInfixOf` out)
        assertEqual "" ["/etc/salmon/ch/salmon-server.xml", "/etc/salmon/ch/salmon-admin.xml"] (Quadlet.watchedFiles c)
        assertEqual "" (Just (Quadlet.readyWhen (ClickHouse.readyProbe (ClickHouse.serverClient s)) 60)) (Quadlet.containerReady c)
    ]

-------------------------------------------------------------------------------

{- | The official image, the rendered drop-ins mounted, the nodes run twice
(the second pass skips all three on their checks), the password rotated
(the user check fails, @up@ puts it back), and @default@ allowed from the
local machine only (the admin drop-in did apply).
-}
liveServer :: IO ()
liveServer = requireExecutable "podman" $
    withTempDir $ \dir -> do
        let serverXml = dir </> "salmon-server.xml"
            adminXml = dir </> "salmon-admin.xml"
            passfile = dir </> "peer.pass"
        writeFile serverXml (Text.unpack (ClickHouse.renderServerConfig ClickHouse.defaultServerConfig{ClickHouse.serverSettings = [("max_connections", "512")]}))
        writeFile adminXml (Text.unpack ClickHouse.renderAdminAccess)
        writeFile passfile "first-password"
        cname <- freshContainerName
        (reporter, readBack) <- capture
        let img = Podman.Image "clickhouse/clickhouse-server:25.8"
            opts =
                Podman.noRunOptions
                    { Podman.runVolumes =
                        [ Podman.VolumeMount serverXml "/etc/clickhouse-server/config.d/salmon-server.xml" Podman.ReadOnly
                        , Podman.VolumeMount adminXml "/etc/clickhouse-server/users.d/salmon-admin.xml" Podman.ReadOnly
                        ]
                    }
            runOp = Podman.runContainer reporter podmanTrack Podman.dockerRegistry img cname opts
            pullOp = Podman.pullImage reporter podmanTrack Podman.dockerRegistry img
            container = Text.unpack (Podman.getContainerName cname)
        pulled <- runUp pullOp
        assertBool "pull the image" pulled
        bracket (runUp runOp) (\_ -> void (runDown runOp)) $ \started -> do
            failures <- podmanFailures <$> readBack
            assertBool ("start the container" <> concat failures) started
            let client = ClickHouse.Client (ClickHouse.InContainer cname) ClickHouse.defaultTcpPort
                waitOp = Readiness.waitUntil (Readiness.untilProbe "clickhouse-test-ready" (ClickHouse.readyProbe client) 60)
                waitT = Track (const waitOp)
            ready <- runUp waitOp
            assertBool "the server answers within 60s" ready
            (chr, _) <- capture
            let grant = ClickHouse.Grant ["SELECT", "INSERT", "CREATE TABLE", "ALTER"] "analytics" "peer"
                dbOp = ClickHouse.database chr waitT client "analytics"
                userOp = ClickHouse.userPassFile chr waitT client (FS.PreExisting passfile) "peer"
                -- a grant needs its user and its database: edges, as in a recipe
                grantOp = (ClickHouse.grant chr waitT client grant `inject` userOp) `inject` dbOp
                graph = op "clickhouse-test" (deps [grantOp]) id
            first <- runUpCapturing graph
            assertBool ("first pass succeeds: " <> show (failed first)) (null (failed first))
            assertEqual "database" Success =<< ClickHouse.checkDatabase client "analytics"
            assertEqual "user" Success =<< ClickHouse.checkUser client "peer" (ClickHouse.readPassword passfile)
            assertEqual "grant" Success =<< ClickHouse.checkGrant client grant
            second <- runUpCapturing graph
            assertEqual "the second pass applies nothing" [] [showAct a | UpDown.Done a <- second, isClickHouse a]
            writeFile passfile "rotated-password"
            rotated <- ClickHouse.checkUser client "peer" (ClickHouse.readPassword passfile)
            assertBool ("a rotated file is a Failure: " <> show rotated) (isFailure rotated)
            third <- runUpCapturing graph
            assertBool ("third pass succeeds: " <> show (failed third)) (null (failed third))
            assertEqual "the rotated password logs in" Success =<< ClickHouse.checkUser client "peer" (ClickHouse.readPassword passfile)
            -- default is loopback-only: ClickHouse folds ::1 and 127.0.0.1
            -- into "localhost" (it counts every address of the machine as
            -- local, so a client inside the container on its own address
            -- is let in; a remote one is not, which is not tried here)
            (_, hostsOut, _) <- podmanExecCapture container ["clickhouse-client", "--query", "SELECT host_ip, host_names FROM system.users WHERE name = 'default' FORMAT TSV"]
            assertEqual "default's allowed hosts" "[]\t['localhost']" (concat (lines hostsOut))
            -- the hash never reaches the query log as written
            (_, logOut, _) <- podmanExecCapture container ["clickhouse-client", "--query", "SYSTEM FLUSH LOGS; SELECT query FROM system.query_log WHERE query ILIKE '%IDENTIFIED%'", "--multiquery"]
            assertBool logOut (not ("sha256_hash BY '" `isInfixOf` logOut))
  where
    showAct :: Act' -> String
    showAct a = Text.unpack a.extension.help
    isClickHouse :: Act' -> Bool
    isClickHouse a = "ClickHouse" `Text.isInfixOf` a.extension.help || "grant " `Text.isPrefixOf` a.extension.help
    failed reports = [showAct a <> ": " <> show e | UpDown.Failed a e <- reports]
    isFailure (Failure _) = True
    isFailure _ = False
