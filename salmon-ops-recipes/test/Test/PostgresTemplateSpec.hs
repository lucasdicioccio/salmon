{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for template databases: the builtins in
"Salmon.Builtin.Nodes.Postgres" and "SreBox.PostgresTemplate" on top.

Layer 0 is the verdicts and the rendered SQL, which is where the safety
lives: every statement that drops or adopts a database is guarded by a
marker, and the guard has to come first and survive a hostile name.

Layer 2 builds, rebuilds, clones and drops against a real cluster in a
podman container, because the claims that matter are Postgres's to judge --
that a locked template refuses connections, that a rebuild really starts
from nothing, that the guard really stops a drop.
-}
module Test.PostgresTemplateSpec (tests, sandboxTests) where

import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (prepare)
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Op.Actions (Act (..))
import Salmon.Reporter (silent)
import qualified SreBox.PostgresTemplate as PGTemplate
import System.Process.ListLike (CmdSpec (..), cmdspec)
import Test.Harness

tests :: TestTree
tests =
    testGroup
        "SreBox.PostgresTemplate"
        [ testGroup "interpretTemplateRow" templateVerdicts
        , testGroup "interpretCloneRow" cloneVerdicts
        , testGroup "the rendered SQL" sqlTests
        , testGroup "refs" refTests
        , testCase "fingerprint frames its parts" $
            assertBool "" (PGTemplate.fingerprint ["ab", "c"] /= PGTemplate.fingerprint ["a", "bc"])
        ]

-- | Kept apart from 'tests' so "Main" can serialize it: it shims @PATH@.
sandboxTests :: TestTree
sandboxTests =
    testGroup
        "SreBox.PostgresTemplate (Layer 2, dogfooded via a podman sandbox)"
        [testCase "build, skip, rebuild, clone, refuse, drop" templateLifecycle]

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

failureText :: CheckResult -> Text
failureText (Failure t) = t
failureText _ = ""

-------------------------------------------------------------------------------

templateVerdicts :: [TestTree]
templateVerdicts =
    [ testCase "locked and stamped with these inputs is satisfied" $
        assertEqual "" Success (verdict "t|f|salmon-template:abc\n")
    , testCase "no row means no template" $
        assertBool "" ("does not exist" `Text.isInfixOf` failureText (verdict ""))
    , testCase "other inputs is a rebuild" $
        assertBool "" ("different inputs" `Text.isInfixOf` failureText (verdict "t|f|salmon-template:old"))
    , testCase "a build that died is recognisably half-built" $
        assertBool "" ("half-built" `Text.isInfixOf` failureText (verdict "f|t|salmon-template:building"))
    , testCase "stamped but connectable is not a template to trust" $ do
        assertBool "" ("not locked" `Text.isInfixOf` failureText (verdict "t|t|salmon-template:abc"))
        assertBool "" ("not locked" `Text.isInfixOf` failureText (verdict "f|f|salmon-template:abc"))
    , testCase "a database salmon did not build says so" $ do
        assertBool "" ("did not build" `Text.isInfixOf` failureText (verdict "f|t|"))
        assertBool "" ("did not build" `Text.isInfixOf` failureText (verdict "t|f|hand-made template"))
    , testCase "a comment containing the separator is still read whole" $
        assertBool "" (isFailure (verdict "t|f|salmon-template:abc|x"))
    , testCase "output that is not a row is not a verdict" $
        assertEqual "" Unknown (verdict "garbage")
    ]
  where
    verdict = Postgres.interpretTemplateRow "tpl" "abc"

cloneVerdicts :: [TestTree]
cloneVerdicts =
    [ testCase "a clone salmon made is satisfied, from whichever template" $
        assertEqual "" Success (Postgres.interpretCloneRow "c" "row:salmon-clone:older_tpl\n")
    , testCase "no row means no clone" $
        assertBool "" ("does not exist" `Text.isInfixOf` failureText (Postgres.interpretCloneRow "c" ""))
    , testCase "an uncommented database is present, and not ours" $
        assertBool "" ("did not clone" `Text.isInfixOf` failureText (Postgres.interpretCloneRow "c" "row:"))
    ]

-------------------------------------------------------------------------------

sqlTests :: [TestTree]
sqlTests =
    [ testCase "the batch stops at the first error" $ do
        -- without ON_ERROR_STOP a refused guard is followed by the DROP it
        -- guards, and psql still exits 0.
        let args = processArgs (prepare (Postgres.psqlBatchRun_Sudo 5433) Postgres.PsqlBatch)
        assertBool (show args) ("ON_ERROR_STOP=1" `elem` args)
        assertBool (show args) ("5433" `elem` args)
    , testCase "a rebuild refuses before it drops" $
        guardPrecedes "DROP DATABASE" (Postgres.prepareTemplateSql "tpl")
    , testCase "a template teardown refuses before it drops" $
        guardPrecedes "DROP DATABASE" (Postgres.dropTemplateSql "tpl")
    , testCase "a clone refuses to adopt before it creates" $
        guardPrecedes "CREATE DATABASE" (Postgres.cloneDatabaseSql plainClone)
    , testCase "a clone teardown refuses before it drops" $
        guardPrecedes "DROP DATABASE" (Postgres.dropCloneSql "c")
    , testCase "a fresh build is marked half-built until it is locked" $ do
        let sql = Postgres.prepareTemplateSql "tpl"
        assertBool (Text.unpack sql) ("salmon-template:building" `Text.isInfixOf` sql)
    , testCase "locking stamps, disallows connections and evicts sessions" $ do
        let sql = Postgres.lockTemplateSql "tpl" "abc"
        assertBool (Text.unpack sql) ("IS 'salmon-template:abc'" `Text.isInfixOf` sql)
        assertBool (Text.unpack sql) ("IS_TEMPLATE true ALLOW_CONNECTIONS false" `Text.isInfixOf` sql)
        assertBool (Text.unpack sql) ("pg_terminate_backend" `Text.isInfixOf` sql)
    , testCase "a clone is created conditionally, with its owner" $ do
        let sql = Postgres.cloneDatabaseSql plainClone{Postgres.clone_owner = Just "app"}
        assertBool (Text.unpack sql) ("\\gexec" `Text.isInfixOf` sql)
        assertBool (Text.unpack sql) ("TEMPLATE \"tpl\" OWNER \"app\"" `Text.isInfixOf` sql)
    , testCase "a hostile name stays inside its quotes" $ do
        let hostile = "x\"; DROP TABLE t; '$$ $salmon0$"
            sql = Postgres.dropTemplateSql hostile
        assertBool (Text.unpack sql) ("\"x\"\"; DROP TABLE t; '$$ $salmon0$\"" `Text.isInfixOf` sql)
        assertBool (Text.unpack sql) ("'x\"; DROP TABLE t; ''$$ $salmon0$'" `Text.isInfixOf` sql)
        -- the DO body's tag is one the name does not contain
        assertBool (Text.unpack sql) ("DO $salmon1$" `Text.isInfixOf` sql)
    ]
  where
    plainClone = Postgres.Clone "c" "tpl" Nothing
    processArgs p = case cmdspec p of
        RawCommand _ args -> args
        ShellCommand s -> [s]
    guardPrecedes stmt sql =
        case (Text.breakOn "RAISE EXCEPTION" sql, Text.breakOn stmt sql) of
            ((before, rest), (beforeStmt, _)) -> do
                assertBool (Text.unpack sql) (not (Text.null rest))
                assertBool (Text.unpack sql) (Text.length before < Text.length beforeStmt)

refTests :: [TestTree]
refTests =
    [ testCase "a database is keyed by its port as well as its name" $ do
        let at port = refOf (Postgres.database silent ignoreTrack ignoreTrack port (Postgres.Database "appdb"))
        assertBool "" (at 5432 /= at 5433)
        assertEqual "" (at 5432) (at 5432)
    , testCase "a clone and a database of one name are one site" $
        assertEqual
            ""
            (refOf (Postgres.database silent ignoreTrack ignoreTrack 5432 (Postgres.Database "c")))
            (refOf (Postgres.disposableClone silent ignoreTrack 5432 ignoreTrack (Postgres.Clone "c" "tpl" Nothing)))
    , testCase "retention is visible in a clone's description" $ do
        -- so `run serve` sees a flip from Retain to Discard as a change to
        -- the node, rather than keeping the old one's no-op teardown.
        let notesOf mk = fmap (\act -> act.extension.notes) (opAct (mk silent ignoreTrack 5432 ignoreTrack (Postgres.Clone "c" "tpl" Nothing)))
        assertBool "" (notesOf Postgres.retainedClone /= notesOf Postgres.disposableClone)
        assertEqual "" (refOf (Postgres.retainedClone silent ignoreTrack 5432 ignoreTrack (Postgres.Clone "c" "tpl" Nothing))) (refOf (Postgres.disposableClone silent ignoreTrack 5432 ignoreTrack (Postgres.Clone "c" "tpl" Nothing)))
    ]
  where
    refOf o = fmap (\act -> act.extension.ref) (opAct o)

-------------------------------------------------------------------------------

-- | The binaries the recipe reaches along its chain, as in "Test.PostgresInitSpec".
shimmedCommands :: [String]
shimmedCommands = ["apt-get", "dpkg-query", "sudo", "bash", "chmod"]

templateLifecycle :: IO ()
templateLifecycle = requireExecutable "podman" $
    withContainer (Podman.Image "debian:bookworm") (Podman.PortMapping "15434" "5432" Podman.TCPPort) $ \cid -> do
        podmanExec_ cid ["apt-get", "update", "-qq"]
        podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo"]
        builds <- newIORef (0 :: Int)

        let cluster = Track $ Postgres.pgLocalCluster silent Debian.postgres Debian.pg_ctlcluster
            tplOp fp table =
                PGTemplate.template
                    silent
                    cluster
                    Debian.psql
                    Postgres.localServer
                    (PGTemplate.Template "fixture_tpl" fp)
                    (createTable builds "fixture_tpl" table)
            -- teardown needs none of the dependencies: tearing those down
            -- would uninstall postgres from under the next step.
            bare fp = PGTemplate.template silent ignoreTrack ignoreTrack Postgres.localServer (PGTemplate.Template "fixture_tpl" fp) realNoop
            clone name = Postgres.disposableClone silent ignoreTrack 5432 ignoreTrack (Postgres.Clone name "fixture_tpl" Nothing)
            kept name = Postgres.retainedClone silent ignoreTrack 5432 ignoreTrack (Postgres.Clone name "fixture_tpl" Nothing)
            catalog db = psqlIn cid "postgres" ("SELECT datistemplate, datallowconn FROM pg_database WHERE datname = '" <> db <> "'")

        withShimmedPath cid shimmedCommands $ do
            ok1 <- runUp (tplOp "v1" "one")
            assertBool "the first build succeeds" ok1
            (catalog "fixture_tpl" >>=) $ assertEqual "locked as a template" "t|f"

            reports <- runUpCapturing (tplOp "v1" "one")
            assertBool ("the second pass succeeds: " <> show [(a.extension.help, e) | UpDown.Failed a e <- reports]) (null [() | UpDown.Failed _ _ <- reports])
            (readIORef builds >>=) $ assertEqual "unchanged inputs are not rebuilt" 1

            ok3 <- runUp (tplOp "v2" "two")
            assertBool "the rebuild succeeds" ok3
            (readIORef builds >>=) $ assertEqual "changed inputs are rebuilt" 2

            okc <- runUp (clone "copy")
            assertBool "the clone succeeds" okc
            tables <- psqlIn cid "copy" "SELECT string_agg(tablename, ',') FROM pg_tables WHERE schemaname = 'public'"
            assertEqual "a rebuild starts from nothing: only v2's table is in the copy" "two" tables

            podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-c", "CREATE DATABASE precious"]
            okt <- runUp (PGTemplate.template silent ignoreTrack ignoreTrack Postgres.localServer (PGTemplate.Template "precious" "v1") realNoop)
            assertBool "a template refuses to replace a foreign database" (not okt)
            okp <- runUp (clone "precious")
            assertBool "a clone refuses to adopt a foreign database" (not okp)
            (catalog "precious" >>=) $ assertEqual "and the foreign database is untouched" "f|t"

            okk <- runDown (kept "copy")
            assertBool "a retained clone's teardown succeeds" okk
            (catalog "copy" >>=) $ assertEqual "and leaves the database, data included" "f|t"
            (psqlIn cid "copy" "SELECT count(*) FROM two" >>=) $ assertEqual "" "0"

            okd <- runDown (clone "copy")
            assertBool "the clone goes down" okd
            (catalog "copy" >>=) $ assertEqual "and is gone" ""
            okdt <- runDown (bare "v2")
            assertBool "the template goes down" okdt
            (catalog "fixture_tpl" >>=) $ assertEqual "and is gone" ""
            okdp <- runDown (clone "precious")
            assertBool "a foreign database is not dropped as a clone" (not okdp)
            (catalog "precious" >>=) $ assertEqual "and survives" "f|t"
  where
    psqlIn :: String -> String -> String -> IO String
    psqlIn cid db sql = do
        (code, out, err) <- podmanExecCapture cid ["sudo", "-u", "postgres", "psql", "-tAX", "-d", db, "-c", sql]
        assertEqual err ExitSuccess code
        pure (filter (/= '\n') out)

-- | The build under test: one table, and a count of how often it ran.
createTable :: IORef Int -> String -> String -> Op
createTable builds db table =
    op "create-fixture-table" nodeps $ \actions ->
        actions
            { ref = mkRef "create-fixture-table" (db, table)
            , up = do
                modifyIORef' builds (+ 1)
                (code, _, err) <- readProcessWithExitCode "sudo" ["-u", "postgres", "psql", "-X", "-d", db, "-c", "CREATE TABLE " <> table <> " (id int)"] ""
                assertEqual err ExitSuccess code
            }
