{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for "Salmon.Builtin.Nodes.PgTextSearch", and for the
@shared_preload_libraries@ node it shares with @pg_turret@
('Salmon.Builtin.Nodes.Postgres.preloadLibrary').

Layer 0: the version gate, the names of a release's files, the install
plan, the control file, the settings and their SQL, the verdicts, and the
shape of the graph. The listings and the control file below are the ones of
the upstream release v1.5.1 (the zip and the package were listed, never
installed or loaded), and 'upstreamSettings' is the README's table, checked
against @src\/mod.c@ at that tag, written by hand.

Layer 1 ('installs'): 'releaseFiles' run for real against a package built
here with @dpkg-deb@ and a stand-in @pg_config@ naming two temporary
directories. It is not the upstream package and no PostgreSQL is involved.

Layer 2 ('sandboxTests'): the upstream release on a real PostgreSQL 17 in a
podman container, when @SALMON_TEST_PG_TEXTSEARCH_ZIP@ names the zip. It
is the only place the preload, the restart, the gate on the loaded library,
the settings round trip, @CREATE EXTENSION@ and a @bm25@ index are run.
-}
module Test.PgTextSearchSpec (tests, sandboxTests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getPermissions, executable, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (Gen, Property, choose, chooseInt, counterexample, elements, forAll, frequency, listOf, listOf1, oneof, shuffle, sublistOf, vectorOf, (===), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.LlamaServer (sha256File)
import Salmon.Builtin.Nodes.PgTextSearch
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Reporter (silent)

import Test.Harness (podmanExec_, requireExecutable, runDown, runUp, withContainer, withShimmedPath, withTempDir)

-------------------------------------------------------------------------------
-- what was read upstream

-- | The ten settings of the README's table at v1.5.1.
upstreamSettings :: [Text]
upstreamSettings =
    [ "pg_textsearch.default_limit"
    , "pg_textsearch.compress_segments"
    , "pg_textsearch.segments_per_level"
    , "pg_textsearch.max_segment_size"
    , "pg_textsearch.background_compaction_schedule"
    , "pg_textsearch.bulk_load_threshold"
    , "pg_textsearch.memtable_pages_threshold"
    , "pg_textsearch.allow_rls"
    , "pg_textsearch.memtable_cache_enabled"
    , "pg_textsearch.memory_limit"
    ]

-- | @unzip -Z1 pg-textsearch-v1.5.1-pg17-amd64.zip@.
zipListing :: [Text]
zipListing = ["pg-textsearch-postgresql-17_1.5.1-1_amd64.deb"]

-- | The regular files of that package (@dpkg-deb -c@), some of the upgrade scripts left out.
packageListing :: [FilePath]
packageListing =
    [ "usr/lib/postgresql/17/lib/pg_textsearch.so"
    , "usr/share/postgresql/17/extension/pg_textsearch--1.4.0--1.5.0.sql"
    , "usr/share/postgresql/17/extension/pg_textsearch--1.5.0--1.5.1.sql"
    , "usr/share/postgresql/17/extension/pg_textsearch--1.5.1.sql"
    , "usr/share/postgresql/17/extension/pg_textsearch--0.5.0.sql"
    , "usr/share/postgresql/17/extension/pg_textsearch.control"
    ]

-- | The package's @pg_textsearch.control@, whole.
controlFile :: Text
controlFile =
    Text.unlines
        [ "# pg_textsearch extension"
        , "comment = 'Full-text search with BM25 ranking'"
        , "default_version = '1.5.1'"
        , "module_pathname = '$libdir/pg_textsearch'"
        , "relocatable = false"
        ]

dirs :: InstallDirs
dirs = InstallDirs "/usr/lib/postgresql/17/lib" "/usr/share/postgresql/17/extension"

release :: Release
release = Release "1.5.1" "/srv/artifacts/pg-textsearch-v1.5.1-pg17-amd64.zip" "ed4410e8b6879a8971cd87fd03853f182e47b85b199c9bfcd9b34f159caba9fb"

someSettings :: Settings
someSettings = defaultSettings{setMemoryLimitKb = Just 1048576, setAllowRls = Just False, setMaxSegmentSizeMb = Just 512}

config :: PgTextSearch
config = (pgTextSearchOn 17 5432 (RestartWhenPending "main") release){ptsDatabases = ["app", "other"], ptsSettings = someSettings}

-------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.PgTextSearch"
        [ testGroup
            "version gate"
            [ testCase "17 and 18 pass when they are the declared major" $ do
                assertEqual "" (Right ()) (gateVerdict 17 "170004\n")
                assertEqual "" (Right ()) (gateVerdict 18 "180000")
            , testCase "an older server is refused with its version in the message" $
                assertEqual
                    ""
                    (Left "pg_textsearch supports PostgreSQL 17 and 18 only, this server is PostgreSQL 16 (server_version_num 160009)")
                    (gateVerdict 17 "160009")
            , testCase "a newer server is refused too: there is a ceiling" $
                assertEqual
                    ""
                    (Left "pg_textsearch supports PostgreSQL 17 and 18 only, this server is PostgreSQL 19 (server_version_num 190000)")
                    (gateVerdict 18 "190000")
            , testCase "a supported server of another major than the declared one is refused" $
                assertEqual "" (Left "this server is PostgreSQL 18 (server_version_num 180001), the declaration is for 17") (gateVerdict 17 "180001")
            , testCase "something that is not a number is refused, not read as zero" $ do
                assertBool "" (either (const True) (const False) (gateVerdict 17 ""))
                assertBool "" (either (const True) (const False) (gateVerdict 17 "17.4"))
            , testCase "the supported majors are the two upstream publishes for" $
                assertEqual "" [17, 18] supportedMajors
            ]
        , testGroup
            "release names"
            [ testCase "the zip is named as the asset is upstream" $
                assertEqual "" "pg-textsearch-v1.5.1-pg17-amd64.zip" (releaseZipName "1.5.1" 17 "amd64")
            , testCase "the package name of v1.5.1 is read" $
                assertEqual "" (Just (DebName 17 "1.5.1" 1 "amd64")) (parseDebName "pg-textsearch-postgresql-17_1.5.1-1_amd64.deb")
            , testCase "and written back" $
                assertEqual "" "pg-textsearch-postgresql-18_1.4.0-1_arm64.deb" (renderDebName (DebName 18 "1.4.0" 1 "arm64"))
            , testCase "names with a path, a pattern or a leading zero are not package names" $
                forM_
                    [ "../pg-textsearch-postgresql-17_1.5.1-1_amd64.deb"
                    , "x/pg-textsearch-postgresql-17_1.5.1-1_amd64.deb"
                    , "pg-textsearch-postgresql-17_1.5.1-1_*.deb"
                    , "pg-textsearch-postgresql-017_1.5.1-1_amd64.deb"
                    , "pg-textsearch-postgresql-17_1.5.1-1_amd64.deb "
                    , "pg-textsearch-postgresql-17_1.5.1_amd64.deb"
                    , "pg-textsearch-postgresql-17_1..1-1_amd64.deb"
                    , "pg-textsearch-postgresql-17_1.5.1-1_amd64.zip"
                    , "postgresql-17-pgvector_0.8.0-1_amd64.deb"
                    ]
                    (\n -> assertEqual (Text.unpack n) Nothing (parseDebName n))
            , testCase "the zip's one package is selected" $
                assertEqual "" (Right (DebName 17 "1.5.1" 1 "amd64")) (selectDeb 17 "1.5.1" "amd64" zipListing)
            , testCase "a zip for another major, version or architecture is refused, saying which" $ do
                assertEqual "" (Left "the zip holds a package for PostgreSQL 17, the declaration is for 18") (selectDeb 18 "1.5.1" "amd64" zipListing)
                assertEqual "" (Left "the zip holds version 1.5.1, the declaration is for 1.4.0") (selectDeb 17 "1.4.0" "amd64" zipListing)
                assertEqual "" (Left "the zip holds a package for amd64, this machine is arm64") (selectDeb 17 "1.5.1" "arm64" zipListing)
            , testCase "a zip with anything else in it is refused, not searched" $ do
                assertEqual "" (Left "the zip holds 2 entries, expected one Debian package") (selectDeb 17 "1.5.1" "amd64" (zipListing <> ["README"]))
                assertEqual "" (Left "the zip holds 0 entries, expected one Debian package") (selectDeb 17 "1.5.1" "amd64" [""])
                assertBool "" (either (const True) (const False) (selectDeb 17 "1.5.1" "amd64" ["evil.sh"]))
            , testCase "a declaration that cannot be right is refused before anything is read" $ do
                assertEqual "" [] (releaseProblems 17 release)
                assertEqual "" ["pg_textsearch supports PostgreSQL 17 and 18 only, the declaration is for 16"] (releaseProblems 16 release)
                assertEqual "" ["the pinned sha256 is not 64 hex digits"] (releaseProblems 17 release{releaseSha256 = "abc"})
                assertEqual "" ["the release version \"v1.5.1\" is not digits and dots"] (releaseProblems 17 release{releaseVersion = "v1.5.1"})
            ]
        , testGroup
            "install plan"
            [ testCase "the library goes first, the control file last, each under the server's own directory" $
                assertEqual
                    ""
                    ( Right
                        [ Install "usr/lib/postgresql/17/lib/pg_textsearch.so" "/L/pg_textsearch.so" True
                        , Install "usr/share/postgresql/17/extension/pg_textsearch--0.5.0.sql" "/E/pg_textsearch--0.5.0.sql" False
                        , Install "usr/share/postgresql/17/extension/pg_textsearch--1.4.0--1.5.0.sql" "/E/pg_textsearch--1.4.0--1.5.0.sql" False
                        , Install "usr/share/postgresql/17/extension/pg_textsearch--1.5.0--1.5.1.sql" "/E/pg_textsearch--1.5.0--1.5.1.sql" False
                        , Install "usr/share/postgresql/17/extension/pg_textsearch--1.5.1.sql" "/E/pg_textsearch--1.5.1.sql" False
                        , Install "usr/share/postgresql/17/extension/pg_textsearch.control" "/E/pg_textsearch.control" False
                        ]
                    )
                    (planInstall 17 "1.5.1" (InstallDirs "/L" "/E") packageListing)
            , testCase "a package built for another major is refused, saying so" $
                assertEqual
                    ""
                    (Left "the package has no usr/lib/postgresql/18/lib/pg_textsearch.so: it is not built for PostgreSQL 18")
                    (planInstall 18 "1.5.1" dirs packageListing)
            , testCase "a package of another version is refused" $
                assertEqual
                    ""
                    (Left "the package has no usr/share/postgresql/17/extension/pg_textsearch--1.6.0.sql: it is not version 1.6.0")
                    (planInstall 17 "1.6.0" dirs packageListing)
            , testCase "a file with no place is refused, and named" $ do
                assertEqual
                    ""
                    (Left "the package holds files this node has no place for: etc/cron.d/x, usr/share/postgresql/17/extension/other.control")
                    (planInstall 17 "1.5.1" dirs (packageListing <> ["usr/share/postgresql/17/extension/other.control", "etc/cron.d/x"]))
                assertBool "a script in a sub-directory" (either (const True) (const False) (planInstall 17 "1.5.1" dirs (packageListing <> ["usr/share/postgresql/17/extension/x/pg_textsearch--9.sql"])))
            , testCase "the names this module owns in the extension directory" $ do
                assertBool "" (all isExtensionFile ["pg_textsearch.control", "pg_textsearch--1.5.1.sql", "pg_textsearch--1.5.0--1.5.1.sql"])
                assertBool "" (not (any isExtensionFile ["pg_textsearch.so", "pg_textsearch_other.control", "vector.control", "pg_textsearch--1.sql.bak", "plpgsql--1.0.sql"]))
            , testCase "the control file's version is read" $ do
                assertEqual "" (Just "1.5.1") (parseControlVersion controlFile)
                assertEqual "" (Just "1.5.1") (parseControlVersion "default_version=1.5.1 # unquoted\n")
                assertEqual "" Nothing (parseControlVersion "comment = 'default_version = 9'\n# default_version = '2'\n")
            , testCase "dpkg's own record of the package, when it was installed the Debian way" $
                assertEqual "" "/var/lib/dpkg/info/pg-textsearch-postgresql-17.list" (dpkgListFile 17)
            ]
        , testGroup
            "loaded library"
            [ testCase "the declared version loaded is satisfied" $
                assertEqual "" Success (interpretLoaded "1.5.1" "1.5.1\n")
            , testCase "nothing loaded waits for a restart" $
                assertEqual "" (Failure "pg_textsearch is not loaded in the running server: the cluster has to be restarted") (interpretLoaded "1.5.1" "\n")
            , testCase "another version loaded waits for a restart, naming both" $
                assertEqual
                    ""
                    (Failure "the running server has pg_textsearch 1.4.0 loaded, the declared release is 1.5.1: the cluster has to be restarted")
                    (interpretLoaded "1.5.1" "1.4.0")
            ]
        , testGroup
            "settings"
            [ testCase "nothing declared renders nothing" $ do
                assertEqual "" [] (settings defaultSettings)
                assertEqual "" [] (problems defaultSettings)
            , testCase "every setting has the name upstream registers, in the README's order" $
                assertEqual "" upstreamSettings (fmap settingName (settings everySetting))
            , testCase "sizes are written with their unit and compared without it" $
                assertEqual
                    ""
                    [ Setting "pg_textsearch.max_segment_size" "512" "512MB"
                    , Setting "pg_textsearch.allow_rls" "off" "off"
                    , Setting "pg_textsearch.memory_limit" "1048576" "1048576kB"
                    ]
                    (settings someSettings)
            , testCase "the batch sets each one and reloads" $
                assertEqual
                    ""
                    "ALTER SYSTEM SET pg_textsearch.max_segment_size = '512MB';\nALTER SYSTEM SET pg_textsearch.allow_rls = 'off';\nALTER SYSTEM SET pg_textsearch.memory_limit = '1048576kB';\nSELECT pg_reload_conf();\n"
                    (applySettingsSql (settings someSettings))
            , testCase "a schedule with a quote in it stays inside its literal" $
                assertBool
                    ""
                    ("= '*/5 * * * * ''; DROP';" `Text.isInfixOf` applySettingsSql (settings defaultSettings{setBackgroundCompactionSchedule = Just "*/5 * * * * '; DROP"}))
            , testCase "going down resets the declared ones and reloads" $
                assertEqual
                    ""
                    "ALTER SYSTEM RESET pg_textsearch.allow_rls;\nSELECT pg_reload_conf();\n"
                    (resetSettingsSql ["pg_textsearch.allow_rls"])
            , testCase "the question reads value, context and pending_restart" $
                assertEqual
                    ""
                    "SELECT coalesce(json_object_agg(name, json_build_object('setting', setting, 'context', context, 'pending_restart', pending_restart)), '{}') FROM pg_settings WHERE name IN ('pg_textsearch.allow_rls', 'pg_textsearch.memory_limit')"
                    (inspectSettingsSql ["pg_textsearch.allow_rls", "pg_textsearch.memory_limit"])
            , testCase "values out of the ranges upstream registers are refused, all of them" $
                assertEqual
                    ""
                    [ "default_limit is 0, outside 1..100000"
                    , "segments_per_level is 65, outside 2..64"
                    , "max_segment_size is 4096, outside 1..4095"
                    , "memory_limit is -1, outside 0..2147483647"
                    , "background_compaction_schedule is empty or has a line break in it"
                    ]
                    ( problems
                        defaultSettings
                            { setDefaultLimit = Just 0
                            , setSegmentsPerLevel = Just 65
                            , setMaxSegmentSizeMb = Just 4096
                            , setMemoryLimitKb = Just (-1)
                            , setBackgroundCompactionSchedule = Just "a\nb"
                            }
                    )
            , testCase "the server agreeing is satisfied" $
                assertEqual "" Success (interpretSettings (settings someSettings) (live [("max_segment_size", "512", "superuser", False), ("allow_rls", "off", "superuser", False), ("memory_limit", "1048576", "sighup", False)]))
            , testCase "a value that differs is named with both sides" $
                assertEqual
                    ""
                    (Failure "pg_textsearch.memory_limit is \"2097152\", declared \"1048576\"")
                    (interpretSettings (settings someSettings) (live [("max_segment_size", "512", "superuser", False), ("allow_rls", "off", "superuser", False), ("memory_limit", "2097152", "sighup", False)]))
            , testCase "a server without the library has no such setting" $
                assertEqual
                    ""
                    (Failure "pg_textsearch.allow_rls is not a setting of the running server (is pg_textsearch loaded, and is it this release?)")
                    (interpretSettings (settings defaultSettings{setAllowRls = Just False}) "{}")
            , testCase "a setting PostgreSQL says waits for a restart is said to" $ do
                let one = settings defaultSettings{setAllowRls = Just False}
                assertEqual
                    "by context"
                    (Failure "pg_textsearch.allow_rls is \"on\", declared \"off\": it only changes at a restart (context postmaster), which this node does not do")
                    (interpretSettings one (live [("allow_rls", "on", "postmaster", False)]))
                assertEqual
                    "by flag"
                    (Failure "pg_textsearch.allow_rls is \"on\", declared \"off\": it only changes at a restart (context sighup), which this node does not do")
                    (interpretSettings one (live [("allow_rls", "on", "sighup", True)]))
            , testCase "output that is not the expected JSON is not an answer" $ do
                assertEqual "" Unknown (interpretSettings (settings someSettings) "")
                assertEqual "" Unknown (interpretSettings (settings someSettings) "psql: error")
            ]
        , testGroup
            "shared_preload_libraries"
            [ testCase "an empty list gets the one entry" $
                assertEqual "" ["pg_textsearch"] (Postgres.addLibrary "pg_textsearch" (Postgres.parseLibraryList ""))
            , testCase "a library already listed is left where it is" $ do
                let libs = Postgres.parseLibraryList "pg_textsearch, pg_stat_statements"
                assertEqual "" libs (Postgres.addLibrary "pg_textsearch" libs)
            , testCase "other libraries are kept, in their order, with ours last" $
                assertEqual
                    ""
                    ["pg_stat_statements", "auto_explain", "pg_textsearch"]
                    (Postgres.addLibrary "pg_textsearch" (Postgres.parseLibraryList "pg_stat_statements,\"auto_explain\""))
            , testCase "a library whose name starts with ours is another library" $
                assertEqual "" ["pg_textsearch_x", "pg_textsearch"] (Postgres.addLibrary "pg_textsearch" ["pg_textsearch_x"])
            , testCase "the list is written back as one value, then reloaded" $
                assertEqual
                    ""
                    "ALTER SYSTEM SET shared_preload_libraries = 'pg_stat_statements', 'pg_textsearch';\nSELECT pg_reload_conf();\n"
                    (Postgres.alterPreloadSql ["pg_stat_statements", "pg_textsearch"])
            , testCase "the last library taken out is a reset: = '' is written as '\"\"', and a server does not start on that" $ do
                assertEqual "" "ALTER SYSTEM RESET shared_preload_libraries;\nSELECT pg_reload_conf();\n" (Postgres.alterPreloadSql (Postgres.removeLibrary "pg_textsearch" ["pg_textsearch"]))
                assertEqual "what the old statement left in the files reads as no library" [] (Postgres.parseLibraryList "\"\"")
            , testCase "the node is the one pg_turret declared before it moved: same ref, help and notes" $ do
                let act = only (Postgres.preloadLibrary ignoreTrack 5432 "pg_turret")
                assertEqual "ref" (mkRef "pg-preload-library" (5432 :: Int, "pg_turret" :: Text)) act.extension.ref
                assertEqual "help" "preload pg_turret in the cluster on port 5432" act.extension.help
                assertEqual "notes" ["takes effect at the next restart of the cluster", "keeps the other entries of shared_preload_libraries"] act.extension.notes
            ]
        , testGroup
            "graph"
            [ testCase "the nodes of a full declaration" $
                assertEqual
                    ""
                    (sort ["pg-textsearch", "pg-textsearch-version-gate", "pg-textsearch-files", "pg-preload-library", "pg-cluster-ctl", "pg-textsearch-loaded", "pg-textsearch-settings", "pg-extension", "pg-extension"])
                    (sort (shorthands (graph config)))
            , testCase "each step waits for the one before it" $ do
                let d = graph config
                assertEqual "files after the gate" [gateRef] (deps' d filesRef)
                assertEqual "preload after the files" [filesRef] (deps' d preloadRef)
                assertEqual "restart after the preload" [preloadRef] (deps' d restartRef)
                assertEqual "loaded after the restart" [restartRef] (deps' d loadedRef)
                assertEqual "settings after loaded" [loadedRef] (deps' d settingsRef)
                assertEqual "extension after loaded" [loadedRef] (deps' d (extRef "app"))
                assertEqual "the root" (sort [settingsRef, extRef "app", extRef "other"]) (sort (deps' d rootRef))
            , testCase "a restart left to the operator declares no restart node" $ do
                let d = graph config{ptsRestart = RestartLeftToOperator}
                assertBool "no cluster node" ("pg-cluster-ctl" `notElem` shorthands d)
                assertEqual "loaded straight after the preload" [preloadRef] (deps' d loadedRef)
            , testCase "no setting declared, no settings node" $ do
                let d = graph config{ptsSettings = defaultSettings, ptsDatabases = []}
                assertBool "" ("pg-textsearch-settings" `notElem` shorthands d)
                assertEqual "the root waits for the library all the same" [loadedRef] (deps' d rootRef)
            , testCase "the extension is created with the 17 floor, and upgraded only when asked" $ do
                let ext upgrade = (nodeAt (graph config{ptsUpgrade = upgrade}) (extRef "app")).extension
                assertEqual "" ["needs server_version_num >= 170000"] (ext False).notes
                assertEqual "" ["upgrades the extension when the package is newer", "needs server_version_num >= 170000"] (ext True).notes
            , testCase "what the files node says in public" $ do
                let e = (only (releaseFiles 17 (debianPgConfig 17) release)).extension
                assertEqual "help" "pg_textsearch 1.5.1 files for postgresql 17" e.help
                assertEqual
                    "notes"
                    [ "from /srv/artifacts/pg-textsearch-v1.5.1-pg17-amd64.zip"
                    , "pinned sha256 of the release zip (a public artifact, not a secret): ed4410e8b6879a8971cd87fd03853f182e47b85b199c9bfcd9b34f159caba9fb"
                    , "directories from /usr/lib/postgresql/17/bin/pg_config"
                    , "a replaced library is loaded at the next restart, which this node does not force"
                    ]
                    e.notes
            , testCase "a declaration for an unsupported major fails its files node without touching anything" $ do
                r <- (only (releaseFiles 16 "/nonexistent/pg_config" release)).extension.check
                assertEqual "" (Failure "pg_textsearch supports PostgreSQL 17 and 18 only, the declaration is for 16") r
                ok <- runUp (releaseFiles 16 "/nonexistent/pg_config" release)
                assertBool "up refuses" (not ok)
            ]
        , testGroup
            "properties"
            [ testProperty "the gate passes exactly a supported server of the declared major, and names the version it refuses" prop_gate
            , testProperty "a package name renders and parses back" prop_debRoundTrip
            , testProperty "whatever parses as a package name renders back to the same text, and is only [a-z0-9._-]" prop_debStrict
            , testProperty "a zip's listing selects its package only when major, version and architecture all agree" prop_selectDeb
            , testProperty "the plan of a well-formed package: library first, control last, one target per file, all under the two directories" prop_plan
            , testProperty "one file more than the release's shape and nothing is planned" prop_planStranger
            , testProperty "a control file's version is read back" prop_control
            , testProperty "adding a library is idempotent, keeps the others in order, and survives the value PostgreSQL writes" prop_preload
            , testProperty "declared settings: upstream's names, once each, as many as were declared" prop_settingNames
            , testProperty "a declaration within upstream's ranges has no problem, and one outside has" prop_ranges
            , testProperty "a server reporting the declared values is satisfied, and one differing value is a failure naming it" prop_settingsVerdict
            , testProperty "the batch is one statement per setting and a reload, with every value inside a literal" prop_batch
            , testProperty "refs are effect sites: the release, the settings and the databases do not move them" prop_refs
            ]
        , testGroup
            "Layer 1: the files, from a package built here"
            [ testCase "installs, is satisfied, replaces on a new release, refuses a wrong digest, and removes only its own" installs
            ]
        ]
  where
    everySetting =
        Settings (Just 500) (Just True) (Just 4) (Just 1024) (Just "*/10 * * * *") (Just 0) (Just 32) (Just False) (Just True) (Just 0)
    live :: [(Text, Text, Text, Bool)] -> Text
    live rows =
        "{"
            <> Text.intercalate
                ", "
                [ Text.pack (show ("pg_textsearch." <> k)) <> ": {\"setting\": " <> Text.pack (show v) <> ", \"context\": " <> Text.pack (show c) <> ", \"pending_restart\": " <> (if p then "true" else "false") <> "}"
                | (k, v, c, p) <- rows
                ]
            <> "}\n"

-------------------------------------------------------------------------------
-- graph helpers

graph :: PgTextSearch -> Dag.Dag Extension
graph cfg = Dag.foldDag Dag.sameRepresentative (evalDeps (pgTextSearch silent ignoreTrack ignoreTrack ignoreTrack cfg))

only :: Op -> Act Extension
only o = case Map.elems (Dag.dagNodes (Dag.foldDag Dag.sameRepresentative (evalDeps o))) of
    [act] -> act
    acts -> error ("expected one node, got " <> show (length acts))

shorthands :: Dag.Dag Extension -> [Text]
shorthands d = fmap (.shorthand) (Map.elems (Dag.dagNodes d))

nodeAt :: Dag.Dag Extension -> Ref -> Act Extension
nodeAt d r = maybe (error ("no node " <> show r)) id (Map.lookup r (Dag.dagNodes d))

deps' :: Dag.Dag Extension -> Ref -> [Ref]
deps' d r = Map.findWithDefault [] r (Dag.dagDependencies d)

rootRef, gateRef, filesRef, preloadRef, restartRef, loadedRef, settingsRef :: Ref
rootRef = mkRef "pg-textsearch" (5432 :: Int)
gateRef = mkRef "pg-textsearch-version-gate" (5432 :: Int)
filesRef = mkRef "pg-textsearch-files" ("/usr/lib/postgresql/17/bin/pg_config" :: FilePath)
preloadRef = mkRef "pg-preload-library" (5432 :: Int, "pg_textsearch" :: Text)
restartRef = mkRef "pg-cluster-ctl" ("main" :: Text, "restart" :: Text)
loadedRef = mkRef "pg-textsearch-loaded" (5432 :: Int)
settingsRef = mkRef "pg-textsearch-settings" (5432 :: Int)

extRef :: Text -> Ref
extRef db = mkRef "pg-extension" (5432 :: Int, db, "pg_textsearch" :: Text)

-------------------------------------------------------------------------------
-- generators

genVersion :: Gen Text
genVersion = do
    n <- chooseInt (1, 4)
    parts <- vectorOf n (chooseInt (0, 120))
    pure (Text.intercalate "." (fmap (Text.pack . show) parts))

genArch :: Gen Text
genArch = oneof [elements ["amd64", "arm64", "riscv64", "s390x"], Text.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9']))]

genDeb :: Gen DebName
genDeb = DebName <$> chooseInt (0, 99) <*> genVersion <*> chooseInt (0, 30) <*> genArch

-- | Texts near a package name: real ones, and real ones damaged one character at a time.
genNearName :: Gen Text
genNearName = do
    base <- renderDebName <$> genDeb
    frequency
        [ (2, pure base)
        , (6, damage base)
        , (1, Text.pack <$> listOf (elements alphabet))
        ]
  where
    alphabet = "pg-textsearch-postgresql_.deb0123456789/* ?[A\n"
    damage t = do
        i <- chooseInt (0, Text.length t)
        c <- elements alphabet
        op' <- chooseInt (0, 2 :: Int)
        let (a, b) = Text.splitAt i t
        pure $ case op' of
            0 -> a <> Text.singleton c <> b
            1 -> a <> Text.drop 1 b
            _ -> a <> Text.singleton c <> Text.drop 1 b

genLibs :: Gen [Text]
genLibs = nub <$> listOf (elements ["pg_stat_statements", "auto_explain", "pg_textsearch", "pg_turret", "timescaledb", "pg_textsearch_x", "a"])

genSettings :: Gen Settings
genSettings =
    Settings
        <$> opt (chooseInt (1, 100000))
        <*> opt bool
        <*> opt (chooseInt (2, 64))
        <*> opt (chooseInt (1, 4095))
        <*> opt (elements ["*/5 * * * *", "0 3 * * *", "@daily", "it's 5"])
        <*> opt (chooseInt (0, 2147483647))
        <*> opt (chooseInt (0, 2147483647))
        <*> opt bool
        <*> opt bool
        <*> opt (chooseInt (0, 2147483647))
  where
    bool = elements [True, False]
    opt g = frequency [(1, pure Nothing), (2, Just <$> g)]

declaredCount :: Settings -> Int
declaredCount s =
    length (filter id [isJust s.setDefaultLimit, isJust s.setCompressSegments, isJust s.setSegmentsPerLevel, isJust s.setMaxSegmentSizeMb, isJust s.setBackgroundCompactionSchedule, isJust s.setBulkLoadThreshold, isJust s.setMemtablePagesThreshold, isJust s.setAllowRls, isJust s.setMemtableCacheEnabled, isJust s.setMemoryLimitKb])

-- | The JSON a server would answer 'inspectSettingsSql' with, were its values these.
liveJson :: [(Text, Text)] -> Text
liveJson rows =
    "{" <> Text.intercalate ", " [Text.pack (show k) <> ": {\"setting\": " <> Text.pack (show v) <> ", \"context\": \"superuser\", \"pending_restart\": false}" | (k, v) <- rows] <> "}"

-------------------------------------------------------------------------------
-- properties

prop_gate :: Property
prop_gate =
    forAll (chooseInt (9, 25)) $ \major ->
        forAll (chooseInt (0, 99)) $ \minor ->
            forAll (elements [17, 18, 16, 19]) $ \declared ->
                let num = major * 10000 + minor
                    verdict = gateVerdict declared (Text.pack (show num) <> "\n")
                    expected = major `elem` [17, 18] && major == declared
                 in counterexample (show verdict) $ case verdict of
                        Right () -> expected
                        Left why -> not expected && Text.pack (show num) `Text.isInfixOf` why && Text.pack (show major) `Text.isInfixOf` why

prop_debRoundTrip :: Property
prop_debRoundTrip = forAll genDeb $ \d -> parseDebName (renderDebName d) === Just d

prop_debStrict :: Property
prop_debStrict =
    forAll genNearName $ \t ->
        case parseDebName t of
            Nothing -> counterexample "" True
            Just d ->
                counterexample (show (t, d)) $
                    renderDebName d == t && Text.all (`elem` (['a' .. 'z'] <> ['0' .. '9'] <> "._-")) t

prop_selectDeb :: Property
prop_selectDeb =
    forAll genDeb $ \d ->
        forAll genDeb $ \want ->
            let agree = d.debMajor == want.debMajor && d.debVersion == want.debVersion && d.debArch == want.debArch
                verdict = selectDeb want.debMajor want.debVersion want.debArch [renderDebName d]
             in counterexample (show verdict) $
                    (if agree then verdict == Right d else either (const True) (const False) verdict)
                        && selectDeb d.debMajor d.debVersion d.debArch [renderDebName d] == Right d
                        && either (const True) (const False) (selectDeb d.debMajor d.debVersion d.debArch [renderDebName d, renderDebName d])

-- | A package of the release's shape: the library, the control file, the version's script and some upgrade scripts.
genPackage :: Gen (Int, Text, [FilePath])
genPackage = do
    major <- elements [17, 18]
    version <- genVersion
    olds <- nub <$> listOf genVersion
    let ext = "usr/share/postgresql" </> show major </> "extension"
        scripts = [ext </> ("pg_textsearch--" <> Text.unpack o <> "--" <> Text.unpack version <> ".sql") | o <- olds, o /= version]
    files <-
        shuffle
            ( ["usr/lib/postgresql" </> show major </> "lib/pg_textsearch.so", ext </> "pg_textsearch.control", ext </> ("pg_textsearch--" <> Text.unpack version <> ".sql")]
                <> scripts
            )
    pure (major, version, files)

prop_plan :: Property
prop_plan =
    forAll genPackage $ \(major, version, files) ->
        let other = if major == 17 then 18 else 17
         in case planInstall major version (InstallDirs "/L" "/E") files of
                Left why -> counterexample (Text.unpack why) False
                Right plan ->
                    counterexample (show plan) $
                        sort (fmap installFrom plan) == sort files
                            && fmap (\i -> (i.installTo, i.installExecutable)) (take 1 plan) == [("/L/pg_textsearch.so", True)]
                            && fmap installTo (take 1 (reverse plan)) == ["/E/pg_textsearch.control"]
                            && all (\i -> "/E/" `isPrefixOf'` i.installTo && not i.installExecutable) (drop 1 plan)
                            && length (nub (fmap installTo plan)) == length plan
                            && all (\i -> not (".." `isInfixOf` i.installTo)) plan
                            && either (const True) (const False) (planInstall other version (InstallDirs "/L" "/E") files)
  where
    isPrefixOf' p s = take (length p) s == p

prop_planStranger :: Property
prop_planStranger =
    forAll genPackage $ \(major, version, files) ->
        forAll (elements ["etc/passwd", "usr/lib/postgresql/" <> show major <> "/lib/other.so", "usr/share/postgresql/" <> show major <> "/extension/plpgsql.control", "usr/share/postgresql/" <> show major <> "/extension/sub/pg_textsearch--1.sql", "usr/share/doc/x/README"]) $ \stranger ->
            either (const True) (const False) (planInstall major version (InstallDirs "/L" "/E") (stranger : files))

prop_control :: Property
prop_control =
    forAll genVersion $ \v ->
        forAll (elements ["default_version = '" <> v <> "'", "default_version='" <> v <> "'  # c", "  default_version = " <> v, "default_version\t=\t" <> v <> " # c"]) $ \line ->
            parseControlVersion (Text.unlines ["# pg_textsearch extension", "comment = 'x'", line, "relocatable = false"]) === Just v

prop_preload :: Property
prop_preload =
    forAll genLibs $ \libs ->
        forAll (elements ["pg_textsearch", "pg_turret", "new"]) $ \lib ->
            let added = Postgres.addLibrary lib libs
                -- the value as PostgreSQL shows it back: comma-separated, quoted or not
                shown quoted = Text.intercalate ", " (fmap (\l -> if quoted then "\"" <> l <> "\"" else l) added)
             in counterexample (show added) $
                    Postgres.addLibrary lib added == added
                        && lib `elem` added
                        && Postgres.removeLibrary lib added == Postgres.removeLibrary lib libs
                        && take (length libs) added == libs
                        && Postgres.parseLibraryList (shown False) == added
                        && Postgres.parseLibraryList (shown True) == added
                        && Postgres.removeLibrary lib (Postgres.removeLibrary lib added) == Postgres.removeLibrary lib added

prop_settingNames :: Property
prop_settingNames =
    forAll genSettings $ \s ->
        let names = fmap settingName (settings s)
         in counterexample (show names) $
                all (`elem` upstreamSettings) names
                    && nub names == names
                    && length names == declaredCount s
                    && length (settingNotes s) == length names
                    && filter (`elem` names) upstreamSettings == names

prop_ranges :: Property
prop_ranges =
    forAll genSettings $ \s ->
        forAll (elements [(-1), 0, 1, 65, 4096, 100001]) $ \n ->
            counterexample (show (problems s)) $
                null (problems s)
                    && (not (n < 1 || n > 100000) || not (null (problems s{setDefaultLimit = Just n})))
                    && (not (n < 2 || n > 64) || not (null (problems s{setSegmentsPerLevel = Just n})))
                    && (not (n < 1 || n > 4095) || not (null (problems s{setMaxSegmentSizeMb = Just n})))
                    && (not (n < 0) || not (null (problems s{setMemoryLimitKb = Just n})))

prop_settingsVerdict :: Property
prop_settingsVerdict =
    forAll genSettings $ \s ->
        let declared = settings s
            rows = [(d.settingName, d.settingValue) | d <- declared]
         in not (null declared) ==>
                forAll (chooseInt (0, length declared - 1)) $ \i ->
                    let victim = fst (rows !! i)
                        changed = [(k, if k == victim then v <> "x" else v) | (k, v) <- rows]
                        dropped = [r | r@(k, _) <- rows, k /= victim]
                        names r = case r of
                            Failure why -> victim `Text.isInfixOf` why
                            _ -> False
                     in counterexample (show (declared, victim)) $
                            interpretSettings declared (liveJson rows) == Success
                                && names (interpretSettings declared (liveJson changed))
                                && names (interpretSettings declared (liveJson dropped))

prop_batch :: Property
prop_batch =
    forAll genSettings $ \s ->
        let declared = settings s
            ls = Text.lines (applySettingsSql declared)
         in counterexample (show ls) $
                length ls == length declared + 1
                    && last ls == "SELECT pg_reload_conf();"
                    && and
                        [ l == "ALTER SYSTEM SET " <> d.settingName <> " = " <> Postgres.quoteLiteral d.settingSql <> ";"
                            -- an odd number of quotes would leave the literal open
                            && even (Text.count "'" l)
                        | (l, d) <- zip ls declared
                        ]
                    && Text.lines (resetSettingsSql (fmap settingName declared)) == ["ALTER SYSTEM RESET " <> d.settingName <> ";" | d <- declared] <> ["SELECT pg_reload_conf();"]

prop_refs :: Property
prop_refs =
    forAll genSettings $ \s ->
        forAll genVersion $ \v ->
            forAll (sublistOf ["app", "other", "third"]) $ \dbs ->
                let cfg = config{ptsSettings = s, ptsDatabases = dbs, ptsRelease = release{releaseVersion = v, releaseSha256 = Text.replicate 64 "a"}}
                    d = graph cfg
                    refs = Map.keys (Dag.dagNodes d)
                    fixed = [rootRef, gateRef, filesRef, preloadRef, restartRef, loadedRef] <> [settingsRef | not (null (settings s))] <> fmap extRef dbs
                 in counterexample (show (length refs, length fixed)) $
                        sort refs == sort fixed
                            -- and the same declaration is the same graph
                            && Map.keys (Dag.dagNodes (graph cfg)) == refs
                            && isNothing (Map.lookup settingsRef (Dag.dagNodes (graph cfg{ptsSettings = defaultSettings})))

-------------------------------------------------------------------------------
-- Layer 1

{- | Builds a package of the release's shape with @dpkg-deb@, zips it, and
runs 'releaseFiles' on it with a stand-in @pg_config@ that names two
directories under a temporary one. Skipped, loudly, without the tools.
-}
installs :: IO ()
installs = requireAll ["dpkg-deb", "dpkg", "zip", "unzip", "sh"] $ withTempDir $ \tmp -> do
    arch <- trim <$> run "dpkg" ["--print-architecture"] ""
    let lib = tmp </> "lib"
        ext = tmp </> "share" </> "extension"
        pgConfig = tmp </> "pg_config"
    createDirectoryIfMissing True lib
    createDirectoryIfMissing True ext
    standInPgConfig pgConfig lib (tmp </> "share")
    zipA <- buildRelease tmp arch "9.9.9" "library, first build"
    zipB <- buildRelease tmp arch "9.9.10" "library, second build"
    shaA <- sha256File zipA
    shaB <- sha256File zipB
    let node zipPath v sha = releaseFiles 17 pgConfig (Release v zipPath sha)
        checkOf o = (only o).extension.check
        a = node zipA "9.9.9" shaA

    before <- checkOf a
    assertEqual "nothing installed yet" (Failure (Text.pack (ext </> "pg_textsearch.control") <> " is missing")) before

    -- a zip that is not the pinned one is refused before anything is taken out of it
    refused <- runUp (node zipA "9.9.9" shaB)
    assertBool "wrong digest refused" (not refused)
    listDirectory lib >>= assertEqual "nothing was copied" []
    listDirectory ext >>= assertEqual "nothing was copied" []

    -- so is the right zip declared as another version
    refused' <- runUp (node zipA "9.9.8" shaA)
    assertBool "wrong version refused" (not refused')
    listDirectory ext >>= assertEqual "nothing was copied" []

    ok <- runUp a
    assertBool "installs" ok
    readFile (lib </> "pg_textsearch.so") >>= assertEqual "the library" "library, first build\n"
    (executable <$> getPermissions (lib </> "pg_textsearch.so")) >>= assertBool "the library is executable"
    (sort <$> listDirectory ext) >>= assertEqual "the extension files" ["pg_textsearch--9.9.8--9.9.9.sql", "pg_textsearch--9.9.9.sql", "pg_textsearch.control"]
    checkOf a >>= assertEqual "satisfied" Success
    again <- runUp a
    assertBool "a second pass succeeds" again

    -- the zip is not needed once the files are there
    checkOf (node (tmp </> "gone.zip") "9.9.9" shaA) >>= assertEqual "satisfied without the zip" Success

    -- a newer release declared: seen, and replaced
    let b = node zipB "9.9.10" shaB
    checkOf b >>= assertEqual "the older release is seen" (Failure (Text.pack (ext </> "pg_textsearch.control") <> " is version 9.9.9, declared 9.9.10"))
    upgraded <- runUp b
    assertBool "upgrades" upgraded
    readFile (lib </> "pg_textsearch.so") >>= assertEqual "the library was replaced" "library, second build\n"
    checkOf b >>= assertEqual "satisfied" Success
    leftovers <- filter (".salmon-part" `isInfixOf`) <$> ((<>) <$> listDirectory lib <*> listDirectory ext)
    assertEqual "no partial file left" [] leftovers

    -- down removes its own names and nothing else
    writeFile (ext </> "plpgsql.control") "somebody else's\n"
    writeFile (lib </> "plpgsql.so") "somebody else's\n"
    down' <- runDown b
    assertBool "goes down" down'
    listDirectory lib >>= assertEqual "the other library stays" ["plpgsql.so"]
    listDirectory ext >>= assertEqual "the other extension stays" ["plpgsql.control"]
    downAgain <- runDown b
    assertBool "down twice" downAgain
  where
    trim = reverse . dropWhile (`elem` ("\n " :: String)) . reverse
    run cmd args input = do
        (code, out, err) <- readProcessWithExitCode cmd args input
        unless (code == ExitSuccess) $ assertFailure (cmd <> " " <> show args <> " failed:\n" <> out <> err)
        pure out
    buildRelease tmp arch version content = do
        let root = tmp </> ("pkg-" <> version)
            extDir = root </> "usr/share/postgresql/17/extension"
            deb = tmp </> ("pg-textsearch-postgresql-17_" <> version <> "-1_" <> arch <> ".deb")
            zipPath = tmp </> ("pg-textsearch-v" <> version <> "-pg17-" <> arch <> ".zip")
        createDirectoryIfMissing True (root </> "DEBIAN")
        createDirectoryIfMissing True (root </> "usr/lib/postgresql/17/lib")
        createDirectoryIfMissing True extDir
        writeFile
            (root </> "DEBIAN/control")
            (unlines ["Package: pg-textsearch-postgresql-17", "Version: " <> version <> "-1", "Architecture: " <> arch, "Maintainer: a test <test@example.invalid>", "Description: a stand-in built by Test.PgTextSearchSpec"])
        writeFile (root </> "usr/lib/postgresql/17/lib/pg_textsearch.so") (content <> "\n")
        writeFile (extDir </> "pg_textsearch.control") ("# stand-in\ndefault_version = '" <> version <> "'\n")
        writeFile (extDir </> ("pg_textsearch--" <> version <> ".sql")) "-- stand-in\n"
        writeFile (extDir </> ("pg_textsearch--9.9.8--" <> version <> ".sql")) "-- stand-in\n"
        _ <- run "dpkg-deb" ["--root-owner-group", "-b", root, deb] ""
        _ <- run "zip" ["-q", "-j", zipPath, deb] ""
        there <- doesFileExist zipPath
        assertBool "the zip was built" there
        pure zipPath

-------------------------------------------------------------------------------
-- Layer 2

{- | The upstream release, on a real PostgreSQL 17, in a disposable
container. It needs the release zip, which this repository does not hold
and no test downloads: point @SALMON_TEST_PG_TEXTSEARCH_ZIP@ at
@pg-textsearch-v1.5.1-pg17-amd64.zip@ (it must be that file: the digest
below is the one upstream publishes for it).
-}
sandboxTests :: TestTree
sandboxTests =
    testGroup
        "Salmon.Builtin.Nodes.PgTextSearch (Layer 2, the upstream release on PostgreSQL 17 in a podman sandbox)"
        [ testCase "preloads, restarts once, sets, creates the extension, ranks with a bm25 index, and goes down" sandbox
        ]

upstreamZipSha256 :: Text
upstreamZipSha256 = "ed4410e8b6879a8971cd87fd03853f182e47b85b199c9bfcd9b34f159caba9fb"

{- | Where the run happens, since the node's file copies are this process's
and its @psql@ and @pg_ctlcluster@ are shimmed into the container:

* 'releaseFiles' runs here, on the real zip, into a temporary directory a
  stand-in @pg_config@ names; what it wrote is then copied into the
  container's own directories with @podman cp@. So the zip, the package
  and the plan are the node's work, and the last copy is the test's.
* 'pgTextSearch' then runs with @sudo@ and @bash@ shimmed: the version
  gate, the preload, the restart, the loaded gate, the settings and the
  extension are the node's, against the container's server.
-}
sandbox :: IO ()
sandbox = requireExecutable "podman" $ requireAll ["dpkg-deb", "dpkg", "unzip", "sh"] $ do
    declared <- lookupEnv "SALMON_TEST_PG_TEXTSEARCH_ZIP"
    case declared of
        Nothing -> hPutStrLn stderr "SKIPPED: SALMON_TEST_PG_TEXTSEARCH_ZIP is not set; this Layer 2 test needs upstream's pg-textsearch-v1.5.1-pg17-amd64.zip"
        Just zipPath -> withTempDir $ \tmp ->
            withContainer (Podman.Image "debian:trixie") (Podman.PortMapping "15441" "5432" Podman.TCPPort) $ \cid -> do
                podmanExec_ cid ["apt-get", "update", "-qq"]
                podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo postgresql"]
                version <- trim <$> sh cid "ls /usr/lib/postgresql | sort -n | tail -n1"
                assertEqual "debian:trixie is expected to ship PostgreSQL 17" "17" version
                sh_ cid "pg_ctlcluster 17 main status >/dev/null || pg_ctlcluster 17 main start"
                waitFor cid "SELECT 1" "1"
                sh_ cid "sudo -u postgres createdb app"

                let lib = tmp </> "lib"
                    ext = tmp </> "share" </> "extension"
                    pgConfig = tmp </> "pg_config"
                    rel = Release "1.5.1" zipPath upstreamZipSha256
                    cfg =
                        (pgTextSearchOn 17 5432 (RestartWhenPending "main") rel)
                            { ptsPgConfig = pgConfig
                            , ptsDatabases = ["app"]
                            , ptsSettings = defaultSettings{setMemoryLimitKb = Just 1048576, setSegmentsPerLevel = Just 4, setAllowRls = Just False, setDefaultLimit = Just 500}
                            }
                    whole = pgTextSearch silent ignoreTrack ignoreTrack ignoreTrack cfg
                createDirectoryIfMissing True lib
                createDirectoryIfMissing True ext
                standInPgConfig pgConfig lib (tmp </> "share")

                -- the files: the node's own work on the real zip
                filesOk <- runUp (releaseFiles 17 pgConfig rel)
                assertBool "the release's files are installed from the upstream zip" filesOk
                names <- sort <$> listDirectory ext
                assertBool ("control and 1.5.1 script present: " <> show names) (all (`elem` names) ["pg_textsearch.control", "pg_textsearch--1.5.1.sql"])
                copyInto cid (lib </> "pg_textsearch.so") "/usr/lib/postgresql/17/lib/pg_textsearch.so"
                forM_ names $ \n -> copyInto cid (ext </> n) ("/usr/share/postgresql/17/extension/" <> n)

                started <- psql cid "postgres" "SELECT pg_postmaster_start_time()"
                withShimmedPath cid ["sudo", "bash"] $ do
                    ok <- runUp whole
                    assertBool "the whole declaration converges" ok
                restarted <- psql cid "postgres" "SELECT pg_postmaster_start_time()"
                assertBool "the cluster was restarted" (started /= restarted)
                psql cid "postgres" "SHOW shared_preload_libraries" >>= assertEqual "preloaded" "pg_textsearch"
                psql cid "postgres" "SHOW pg_textsearch.library_version" >>= assertEqual "the loaded library's version" "1.5.1"
                psql cid "app" "SELECT extversion FROM pg_extension WHERE extname = 'pg_textsearch'" >>= assertEqual "the extension" "1.5.1"
                psql cid "postgres" "SELECT string_agg(name || '=' || setting || '/' || context, ' ' ORDER BY name) FROM pg_settings WHERE name IN ('pg_textsearch.memory_limit','pg_textsearch.segments_per_level','pg_textsearch.allow_rls','pg_textsearch.default_limit')"
                    >>= assertEqual
                        "the settings, and the contexts upstream registers them with"
                        "pg_textsearch.allow_rls=off/superuser pg_textsearch.default_limit=500/user pg_textsearch.memory_limit=1048576/sighup pg_textsearch.segments_per_level=4/superuser"
                psql cid "postgres" ("SELECT string_agg(name, ',' ORDER BY name) FROM pg_settings WHERE name = ANY (string_to_array('" <> Text.unpack (Text.intercalate "," upstreamSettings) <> "', ','))")
                    >>= assertEqual "every setting this module knows is one the server has" (Text.unpack (Text.intercalate "," (sort upstreamSettings)))

                -- what all of it is for
                _ <-
                    psql
                        cid
                        "app"
                        "CREATE TABLE docs (id int PRIMARY KEY, content text); INSERT INTO docs VALUES (1, 'the quick brown fox'), (2, 'postgres is a database system, a database for systems'), (3, 'a database'), (4, 'nothing relevant here at all'); CREATE INDEX docs_idx ON docs USING bm25(content) WITH (text_config='english')"
                psql cid "app" "SELECT string_agg(id::text, ',') FROM (SELECT id FROM docs ORDER BY content <@> 'database system' LIMIT 2) t"
                    >>= assertEqual "ranked: the document with both terms first, then the one with one" "2,3"

                -- a second pass changes nothing, and restarts nothing
                withShimmedPath cid ["sudo", "bash"] $ do
                    again <- runUp whole
                    assertBool "a second pass succeeds" again
                    forM_ (Map.elems (Dag.dagNodes (graph cfg))) $ \act -> do
                        r <- act.extension.check
                        assertBool (Text.unpack act.shorthand <> " is satisfied or has no check: " <> show r) (r `elem` [Success, Immaterial])
                psql cid "postgres" "SELECT pg_postmaster_start_time()" >>= assertEqual "no second restart" restarted

                -- down: refused while an index needs the extension, then clean
                withShimmedPath cid ["sudo", "bash"] $ do
                    refused <- runDown whole
                    assertBool "a database with a bm25 index refuses the drop" (not refused)
                psql cid "postgres" "SHOW shared_preload_libraries" >>= assertEqual "and the refusal stopped the rest" "pg_textsearch"
                _ <- psql cid "app" "DROP TABLE docs"
                withShimmedPath cid ["sudo", "bash"] $ do
                    down' <- runDown whole
                    assertBool "goes down" down'
                psql cid "app" "SELECT count(*) FROM pg_extension WHERE extname = 'pg_textsearch'" >>= assertEqual "extension dropped" "0"
                psql cid "postgres" "SELECT coalesce((SELECT setting FROM pg_file_settings WHERE name = 'shared_preload_libraries' ORDER BY seqno DESC LIMIT 1), '')"
                    >>= assertEqual "out of the files' list" ""
                psql cid "postgres" "SELECT count(*) FROM pg_file_settings WHERE name LIKE 'pg\\_textsearch.%'" >>= assertEqual "settings reset" "0"
                psql cid "postgres" "SELECT pg_postmaster_start_time()" >>= assertEqual "down restarted nothing" restarted
                psql cid "postgres" "SHOW pg_textsearch.library_version" >>= assertEqual "the library stays in the running server" "1.5.1"

                -- and the cluster starts again on what down left in the files:
                -- an emptied list written as '' would not (see 'Postgres.alterPreloadSql')
                sh_ cid "pg_ctlcluster 17 main restart"
                waitFor cid "SHOW shared_preload_libraries" ""
                psql cid "postgres" "SELECT coalesce(current_setting('pg_textsearch.library_version', true), 'none')" >>= assertEqual "gone after the restart" "none"
  where
    trim = reverse . dropWhile (`elem` ("\n " :: String)) . reverse
    exec cid user args = readProcessWithExitCode "podman" (["exec", "-i", "--user", user, cid] <> args) ""
    sh cid script = do
        (code, out, err) <- exec cid "root" ["bash", "-c", script]
        unless (code == ExitSuccess) $ assertFailure (script <> " failed:\n" <> out <> "\n" <> err)
        pure out
    sh_ cid script = () <$ sh cid script
    psql cid db q = do
        (code, out, err) <- exec cid "postgres" ["psql", "-X", "-d", db, "-tAc", q]
        unless (code == ExitSuccess) $ assertFailure (q <> " failed:\n" <> err)
        pure (concat (take 1 (reverse (filter (not . null) (lines out)))))
    waitFor cid q expected = go (60 :: Int) ""
      where
        go 0 lastSeen = assertFailure (q <> " never answered " <> expected <> "; last: " <> lastSeen)
        go n _ = do
            (code, out, err) <- exec cid "postgres" ["psql", "-X", "-tAc", q]
            if code == ExitSuccess && concat (take 1 (lines out)) == expected
                then pure ()
                else threadDelay 1000000 >> go (n - 1 :: Int) (out <> err)
    copyInto cid src dst = do
        (code, out, err) <- readProcessWithExitCode "podman" ["cp", src, cid <> ":" <> dst] ""
        unless (code == ExitSuccess) $ assertFailure ("podman cp " <> src <> " failed:\n" <> out <> err)
        sh_ cid ("chown root:root '" <> dst <> "' && chmod a+rX '" <> dst <> "'")

{- | A @pg_config@ that names two directories. Written by a child, not by
this process: the suite runs its groups in parallel, and exec'ing a file
another test's fork still holds open for writing is ETXTBSY.
-}
standInPgConfig :: FilePath -> FilePath -> FilePath -> IO ()
standInPgConfig path lib share = do
    (code, out, err) <-
        readProcessWithExitCode
            "sh"
            ["-c", "cat > \"$1\" && chmod 755 \"$1\"", "sh", path]
            (unlines ["#!/bin/sh", "case \"$1\" in", "  --pkglibdir) echo '" <> lib <> "' ;;", "  --sharedir) echo '" <> share <> "' ;;", "  *) exit 2 ;;", "esac"])
    unless (code == ExitSuccess) $ assertFailure ("writing the stand-in pg_config failed:\n" <> out <> err)

requireAll :: [String] -> IO () -> IO ()
requireAll names act = do
    missing <- fmap concat . mapM (\n -> maybe [n] (const []) <$> findExecutable n) $ names
    case missing of
        [] -> act
        ms -> hPutStrLn stderr ("SKIPPED: " <> unwords ms <> " not found on PATH; the pg_textsearch Layer 1 install test needs them")
