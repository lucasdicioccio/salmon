{- | Layer 0 coverage for "Salmon.Builtin.Nodes.PgTurret": the settings a
declaration renders, the @shared_preload_libraries@ merge, the SQL, the
verdict drawn from the server's answer, and the order of the graph. Nothing
here talks to a server, and no server with the extension loaded has been
used to check any of it.
-}
module Test.PgTurretSpec (tests) where

import qualified Data.ByteString as ByteString
import Data.Functor.Identity (runIdentity)
import Data.List (isSubsequenceOf)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.Query (pathedNodes)
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.PgTurret
import Salmon.Op.Eval (expand)
import Salmon.Op.Ref (Ref)
import Salmon.Reporter (silent)

artifact :: Artifact
artifact = Artifact "/srv/artifacts/libpg_turret.so" "/srv/artifacts/pg_turret.control" ["/srv/artifacts/pg_turret--0.0.0.sql"]

base :: PgTurret
base = pgTurretOn "main" 5432 (debianInstallDirs 16) artifact

withHttp :: PgTurret
withHttp = base{turretHttp = Just (httpAdapter "https://logs.example.com/v1/ingest"){httpApiKey = Just (SecretFile "/etc/salmon/turret-http.key")}}

withKafka :: PgTurret
withKafka = base{turretKafka = Just (kafkaAdapter ["k1:9092", "k2:9092"] "pg-logs"){kafkaApiSecret = Just (SecretFile "/etc/salmon/kafka.secret")}}

secretValue :: Text.Text
secretValue = "s3cr3t-it's-here"

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.PgTurret"
        [ testGroup
            "settings"
            [ testCase "a bare declaration turns both adapters off and excludes its own lines" $
                assertEqual
                    ""
                    [ ("pg_turret.filter.pattern_exclude", Plain "^pg_turret: ")
                    , ("pg_turret.http.enabled", Plain "off")
                    , ("pg_turret.kafka.enabled", Plain "off")
                    ]
                    (settings base)
            , testCase "the http adapter" $
                assertEqual
                    ""
                    [ ("pg_turret.http.enabled", Plain "on")
                    , ("pg_turret.http.endpoint", Plain "https://logs.example.com/v1/ingest")
                    , ("pg_turret.http.api_key", Secret (SecretFile "/etc/salmon/turret-http.key"))
                    ]
                    (filter (Text.isPrefixOf "pg_turret.http." . fst) (settings withHttp))
            , testCase "the kafka adapter" $
                assertEqual
                    ""
                    [ ("pg_turret.kafka.enabled", Plain "on")
                    , ("pg_turret.kafka.brokers", Plain "k1:9092,k2:9092")
                    , ("pg_turret.kafka.topic", Plain "pg-logs")
                    , ("pg_turret.kafka.api_secret", Secret (SecretFile "/etc/salmon/kafka.secret"))
                    ]
                    (filter (Text.isPrefixOf "pg_turret.kafka." . fst) (settings withKafka))
            , testCase "numbers and booleans are written the way the server reads them back" $ do
                let t = base{turretPollIntervalS = Just 5, turretNumWorkers = Just 2, turretRetry = Retry (Just False) (Just 4) Nothing, turretFilter = Filter (Just 19) Nothing Nothing}
                assertEqual
                    ""
                    [ ("pg_turret.poll_interval_s", Plain "5")
                    , ("pg_turret.num_workers", Plain "2")
                    , ("pg_turret.filter.level_min", Plain "19")
                    , ("pg_turret.retry.enabled", Plain "off")
                    , ("pg_turret.retry.max_attempts", Plain "4")
                    ]
                    (take 5 (settings t))
            , testCase "every name is one the extension registers" $ do
                let full =
                        base
                            { turretHttp = Just (HttpAdapter "e" (Just (SecretFile "/k")) (Just 1) (Just 100) (Just True))
                            , turretKafka = Just (KafkaAdapter ["b"] "t" (Just (SecretFile "/k")) (Just (SecretFile "/s")) (Just 100) (Just 1))
                            , turretFilter = Filter (Just 10) (Just "a") (Just "b")
                            , turretRetry = Retry (Just True) (Just 1) (Just 64)
                            , turretPollIntervalS = Just 1
                            , turretRingBufferSize = Just 128
                            , turretNumWorkers = Just 1
                            }
                assertEqual "" [] (filter (`notElem` upstreamNames) (fmap fst (settings full)))
                assertEqual "all but the sentry ones" (length upstreamNames) (length (settings full))
            , testCase "notes show a public value and only the path of a secret" $ do
                let ns = settingNotes withHttp
                assertBool "endpoint" ("pg_turret.http.endpoint = https://logs.example.com/v1/ingest" `elem` ns)
                assertBool "path" ("pg_turret.http.api_key from /etc/salmon/turret-http.key" `elem` ns)
            ]
        , testGroup
            "problems"
            [ testCase "the declarations above have none" $
                assertEqual "" [] (concatMap problems [base, withHttp, withKafka])
            , testCase "out-of-range values are all reported" $
                assertEqual
                    ""
                    ["poll_interval_s is 0, outside 1..3600", "num_workers is 9, outside 1..8"]
                    (problems base{turretPollIntervalS = Just 0, turretNumWorkers = Just 9})
            , testCase "an adapter needs its destination" $ do
                assertEqual "" ["http.endpoint is empty"] (problems base{turretHttp = Just (httpAdapter " ")})
                assertEqual "" ["kafka.brokers is empty", "kafka.topic is empty"] (problems base{turretKafka = Just (kafkaAdapter [] "")})
            , testCase "a script that is not the extension's is refused" $
                assertEqual
                    ""
                    ["/srv/other.sql is not a pg_turret--VERSION.sql script"]
                    (problems base{turretArtifact = artifact{artifactScripts = ["/srv/other.sql"]}})
            , testCase "a line break in a value is refused" $
                assertEqual
                    ""
                    ["pg_turret.filter.pattern has a line break in it"]
                    (problems base{turretFilter = defaultFilter{filterPattern = Just "a\nb"}})
            ]
        , testGroup
            "shared_preload_libraries"
            [ testCase "empty" $ do
                assertEqual "" [] (parseLibraryList "")
                assertEqual "" ["pg_turret"] (addLibrary "pg_turret" (parseLibraryList ""))
            , testCase "already present is left exactly as it is" $ do
                let libs = parseLibraryList "pg_turret, pg_stat_statements"
                assertEqual "" libs (addLibrary "pg_turret" libs)
            , testCase "other libraries are kept, in order, and it goes last" $
                assertEqual
                    ""
                    ["pg_stat_statements", "auto_explain", "pg_turret"]
                    (addLibrary "pg_turret" (parseLibraryList "pg_stat_statements,auto_explain"))
            , testCase "spaces and quotes around a name are not part of it" $
                assertEqual "" ["pg_stat_statements", "pg_turret"] (parseLibraryList " \"pg_stat_statements\" , pg_turret ,")
            , testCase "a longer name is not the library" $
                assertEqual "" ["pg_turret_other", "pg_turret"] (addLibrary "pg_turret" ["pg_turret_other"])
            , testCase "removing leaves the others" $ do
                assertEqual "" ["a", "b"] (removeLibrary "pg_turret" ["a", "pg_turret", "b"])
                assertEqual "" [] (removeLibrary "pg_turret" ["pg_turret"])
            , testCase "the statement lists every library" $ do
                assertEqual "" "ALTER SYSTEM SET shared_preload_libraries = 'pg_stat_statements', 'pg_turret';\nSELECT pg_reload_conf();\n" (alterPreloadSql ["pg_stat_statements", "pg_turret"])
                assertEqual "" "ALTER SYSTEM SET shared_preload_libraries = '';\nSELECT pg_reload_conf();\n" (alterPreloadSql [])
            , testCase "an emptied list reads back as no library" $
                assertEqual "" [] (parseLibraryList "\"\"")
            , testCase "the list is read from the files, not from the running server" $
                assertEqual
                    ""
                    "SELECT coalesce((SELECT setting FROM pg_file_settings WHERE name = 'shared_preload_libraries' ORDER BY seqno DESC LIMIT 1), '')"
                    (fileSettingSql "shared_preload_libraries")
            ]
        , testGroup
            "SQL"
            [ testCase "the query that goes on a command line names settings and holds no value" $ do
                let sql = inspectSettingsSql (fmap fst (settings withHttp))
                assertBool "running list" ("current_setting('shared_preload_libraries')" `Text.isInfixOf` sql)
                assertBool "declared" ("'pg_turret.http.api_key', current_setting('pg_turret.http.api_key', true)" `Text.isInfixOf` sql)
                assertBool "no value" (not ("logs.example.com" `Text.isInfixOf` sql))
            , testCase "the batch quotes its values, silences statement logging first and reloads last" $ do
                let ls = Text.lines (applySettingsSql [("pg_turret.http.enabled", "on"), ("pg_turret.http.api_key", secretValue)])
                assertEqual "" ["\\set VERBOSITY terse"] (take 1 ls)
                assertBool "quiet before the first ALTER" $
                    ["SET log_statement = 'none';", "SET log_min_error_statement = 'panic';", "ALTER SYSTEM SET pg_turret.http.enabled = 'on';"] `isSubsequenceOf` ls
                assertBool "quoted" ("ALTER SYSTEM SET pg_turret.http.api_key = 's3cr3t-it''s-here';" `elem` ls)
                assertEqual "" "SELECT pg_reload_conf();" (last ls)
            , testCase "going down resets the declared settings and no others" $
                assertEqual
                    ""
                    ["ALTER SYSTEM RESET pg_turret.filter.pattern_exclude;", "ALTER SYSTEM RESET pg_turret.http.enabled;", "ALTER SYSTEM RESET pg_turret.kafka.enabled;"]
                    (filter ("ALTER" `Text.isPrefixOf`) (Text.lines (resetSettingsSql (fmap fst (settings base)))))
            ]
        , testGroup
            "interpretSettings"
            [ testCase "everything as declared" $
                assertEqual "" Success (interpretSettings wanted (answer "pg_stat_statements, pg_turret" "on" secretValue))
            , testCase "the library missing from the running list is the first thing said" $
                assertEqual
                    ""
                    (Failure "pg_turret is not loaded: the running shared_preload_libraries does not list it")
                    (interpretSettings wanted (answer "pg_stat_statements" "on" secretValue))
            , testCase "a public setting that differs is quoted" $
                assertEqual
                    ""
                    (Failure "pg_turret.http.enabled is \"off\", declared \"on\"")
                    (interpretSettings wanted (answer "pg_turret" "off" secretValue))
            , testCase "a credential that differs is named, and neither value is in the text" $ do
                let verdict = interpretSettings wanted (answer "pg_turret" "on" "the-old-one")
                assertEqual "" (Failure "pg_turret.http.api_key is not what /etc/salmon/turret-http.key holds") verdict
                assertBool "" (not (any (`Text.isInfixOf` Text.pack (show verdict)) [secretValue, "the-old-one"]))
            , testCase "a setting the server does not have reads as empty" $
                assertEqual
                    ""
                    (Failure "pg_turret.http.enabled is \"\", declared \"on\"; pg_turret.http.api_key is not what /etc/salmon/turret-http.key holds")
                    (interpretSettings wanted "{\"shared_preload_libraries\":\"pg_turret\",\"pg_turret.http.enabled\":null}")
            , testCase "an answer that is not the object asked for is not a verdict" $
                assertEqual "" Unknown (interpretSettings wanted "psql: error")
            ]
        , testGroup
            "secrets"
            [ testCase "scrub removes a secret as written and as quoted in a statement" $
                assertEqual
                    ""
                    "ERROR: near '<redacted>' and <redacted>"
                    (scrub [secretValue] "ERROR: near 's3cr3t-it''s-here' and s3cr3t-it's-here")
            , testCase "a secret file is read without its trailing line break" $ do
                r <- withSecretFile "token-123\n" (readSecret . SecretFile)
                assertEqual "" (Right "token-123") r
            , testCase "an empty, multi-line or missing secret file is refused by its path" $ do
                empty <- withSecretFile "\n" (\p -> (,) p <$> readSecret (SecretFile p))
                assertEqual "" (Left (Text.pack (fst empty) <> " is empty")) (snd empty)
                multi <- withSecretFile "a\nb\n" (\p -> (,) p <$> readSecret (SecretFile p))
                assertEqual "" (Left (Text.pack (fst multi) <> " holds more than one line")) (snd multi)
                missing <- readSecret (SecretFile "/nonexistent/salmon-turret")
                assertEqual "" (Left "cannot read /nonexistent/salmon-turret") missing
            ]
        , testGroup
            "graph"
            [ testCase "files, then the preload, then the restart, then the settings" $ do
                let paths = [p | (p, _, _) <- nodes (node withHttp)]
                assertBool (show paths) $
                    any (["pg-turret", "pg-turret-settings", "pg-cluster-ctl", "pg-preload-library", "pg-turret-file"] `isSubsequenceOf`) paths
            , testCase "the artifact lands under the declared major's directories" $ do
                let helps = [h | (_, _, h) <- nodes (node base)]
                mapM_
                    (\h -> assertBool (Text.unpack h) (h `elem` helps))
                    [ "installs /srv/artifacts/libpg_turret.so as /usr/lib/postgresql/16/lib/pg_turret.so"
                    , "installs /srv/artifacts/pg_turret.control as /usr/share/postgresql/16/extension/pg_turret.control"
                    , "installs /srv/artifacts/pg_turret--0.0.0.sql as /usr/share/postgresql/16/extension/pg_turret--0.0.0.sql"
                    ]
            , testCase "CREATE EXTENSION is only declared for the databases asked for, after the restart" $ do
                assertBool "none by default" (not (any (\(p, _, _) -> "pg-extension" `elem` p) (nodes (node base))))
                let paths = [p | (p, _, _) <- nodes (node base{turretFunctionsIn = ["app"]})]
                assertBool (show paths) (any (["pg-turret", "pg-extension", "pg-cluster-ctl", "pg-preload-library"] `isSubsequenceOf`) paths)
            ]
        ]
  where
    wanted =
        [ ("pg_turret.http.enabled", Plain "on", "on")
        , ("pg_turret.http.api_key", Secret (SecretFile "/etc/salmon/turret-http.key"), secretValue)
        ]
    answer libs enabled key =
        "{\"shared_preload_libraries\" : \"" <> libs <> "\", \"pg_turret.http.enabled\" : \"" <> enabled <> "\", \"pg_turret.http.api_key\" : \"" <> key <> "\"}\n"
    node = pgTurret silent ignoreTrack ignoreTrack ignoreTrack
    nodes :: Op -> [([Text.Text], Ref, Text.Text)]
    nodes o = pathedNodes (runIdentity (expand o))

withSecretFile :: ByteString.ByteString -> (FilePath -> IO a) -> IO a
withSecretFile content f = do
    tmp <- getTemporaryDirectory
    (path, h) <- openTempFile tmp "salmon-turret-secret"
    ByteString.hPut h content
    hClose h
    r <- f path
    removeFile path
    pure r

{- | The settings registered by the extension's @src\/lib.rs@ at commit
@5c7407af@, minus the ten @pg_turret.sentry.*@ ones this module does not
offer. Written out by hand from that file: a name this module renders that
is not in this list is a name nobody checked.
-}
upstreamNames :: [Text.Text]
upstreamNames =
    fmap
        ("pg_turret." <>)
        [ "http.enabled"
        , "http.endpoint"
        , "http.api_key"
        , "http.timeout_ms"
        , "http.batch_size"
        , "http.compression"
        , "poll_interval_s"
        , "ring_buffer_size"
        , "num_workers"
        , "retry.enabled"
        , "retry.max_attempts"
        , "retry.queue_size"
        , "filter.level_min"
        , "filter.pattern"
        , "filter.pattern_exclude"
        , "kafka.enabled"
        , "kafka.brokers"
        , "kafka.topic"
        , "kafka.api_key"
        , "kafka.api_secret"
        , "kafka.timeout_ms"
        , "kafka.batch_size"
        ]
