{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "SreBox.PostgresPairPrereqs": what a pair assumed
was done before it ran.

Three kinds of test. The declaration is checked as data ('validate', the
@pg_hba.conf@ lines, the shape of the graph). The scripts are checked as
text and through @bash -n@. And the two parts where a mistake is quiet are
/run/, here, against a temporary directory and stand-ins on @PATH@ for the
commands that need a machine: the auth file that must restart pgbouncer when
it changed and only then, and the password that must reach @psql@ without
passing through an argument or a report.

Nothing here reaches a machine over ssh; that is the Layer 3 specs' job.
-}
module Test.PostgresPairPrereqsSpec (tests) where

import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist, getPermissions, setPermissions, setOwnerExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes, accessModes)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- the field selectors are named so that GHC can solve the HasField
-- constraints `Dag.sameRepresentative` carries.
import Salmon.Builtin.Extension (Extension, Op, dynamics, evalDeps, help, notes, ref)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (silent)

import qualified SreBox.PostgresPair as Pair
import qualified SreBox.PostgresPairPrereqs as Prereqs

tests :: TestTree
tests =
    testGroup
        "SreBox.PostgresPairPrereqs"
        [ testGroup "validate" validateTests
        , testGroup "pg_hba lines" hbaTests
        , testGroup "the scripts, as text" scriptTests
        , testGroup "the auth file and the passfiles, run" installTests
        , testGroup "an application's role, run against stand-ins" applicationTests
        , testGroup "the graph" graphTests
        ]

-------------------------------------------------------------------------------

validateTests :: [TestTree]
validateTests =
    [ testCase "the defaults are fine" $
        assertEqual "" [] (Prereqs.validate pair Prereqs.defaultPrereqs)
    , testCase "and so is a full declaration" $
        assertEqual "" [] (Prereqs.validate pair full)
    , testCase "every refusal is reported, not the first" $ do
        let bad =
                full
                    { Prereqs.prereq_member_packages = ["postgresql; rm -rf /"]
                    , Prereqs.prereq_userlist = (Prereqs.postgresOwned "relative/userlist.txt"){Prereqs.secret_mode = "rw"}
                    , Prereqs.prereq_applications =
                        [ app
                            { Prereqs.app_role = "app'; DROP"
                            , Prereqs.app_clients = ["10.0.0.3 trust"]
                            , Prereqs.app_hba_method = "trust"
                            }
                        ]
                    }
            refusals = Prereqs.validate pair bad
        assertEqual (show refusals) 6 (length refusals)
    , testCase "an application may not be the pair's own role" $ do
        let refusals = Prereqs.validate pair full{Prereqs.prereq_applications = [app{Prereqs.app_role = "replicator"}]}
        assertBool (show refusals) (any ("the pair's own" `Text.isInfixOf`) refusals)
    ]

hbaTests :: [TestTree]
hbaTests =
    [ testCase "a bare address is one host, v4 or v6, and a network is taken as written" $
        assertEqual
            ""
            [ "host app app 10.0.0.3/32 md5"
            , "host app app fd00::3/128 md5"
            , "host app app 10.0.1.0/24 md5"
            ]
            (Prereqs.hbaLines full{Prereqs.prereq_applications = [app{Prereqs.app_clients = ["10.0.0.3", "fd00::3", "10.0.1.0/24"]}]})
    ]

scriptTests :: [TestTree]
scriptTests =
    [ testCase "every script parses" $
        mapM_
            bashParses
            [ Prereqs.memberPrereqsScript pair full Pair.A
            , Prereqs.bouncerPrereqsScript full bouncer
            , Prereqs.applicationsScript pair full Pair.A
            , Prereqs.applicationsScript pair Prereqs.defaultPrereqs Pair.A
            ]
    , testCase "packages are installed only if missing" $ do
        let s = Prereqs.memberPrereqsScript pair full Pair.A
        assertBool s ("dpkg-query" `isInfixOf` s)
        assertBool s (at "dpkg-query" s < at "apt-get install" s)
    , testCase "and a machine whose packages are somebody else's is not asked" $ do
        let s = Prereqs.memberPrereqsScript pair full{Prereqs.prereq_member_packages = []} Pair.A
        assertBool s (not ("apt-get" `isInfixOf` s))
    , testCase "a member's passfiles land where the pair reads them, as postgres, 0600" $ do
        let s = Prereqs.memberPrereqsScript pair full Pair.A
        assertBool s ("install -m '0600' -o 'postgres' -g 'postgres' '/run/secrets/repl.pgpass' '/etc/postgresql/repl.pgpass'" `isInfixOf` s)
        assertBool s ("'/run/secrets/rewind.pgpass' '/etc/postgresql/rewind.pass'" `isInfixOf` s)
    , testCase "a secret left in place is not copied, only held" $ do
        let s = Prereqs.memberPrereqsScript pair Prereqs.defaultPrereqs Pair.A
        assertBool s (not ("install -m" `isInfixOf` s))
        assertBool s ("chmod '0600' '/etc/postgresql/repl.pgpass'" `isInfixOf` s)
    , testCase "pgbouncer is restarted through try-restart, behind the changed flag" $ do
        let s = Prereqs.bouncerPrereqsScript full bouncer
        assertBool s ("if [ \"$salmon_changed\" = 1 ]; then systemctl try-restart pgbouncer; fi" `isInfixOf` s)
        -- the console passfile is installed before the flag is reset, so a
        -- changed console password alone restarts nothing
        assertBool s (at "console.pgpass" s < at "salmon_changed=0" s)
        assertBool s (at "salmon_changed=0" s < at "userlist.txt" s)
    , testCase "roles and databases are made on a primary only, hba lines on either" $ do
        let s = Prereqs.applicationsScript pair full Pair.A
        assertBool s (at "pg_hba.conf" s < at "pg_is_in_recovery" s)
        assertBool s (at "host app app 10.0.0.3/32 md5" s < at "pg_is_in_recovery" s)
        assertBool s (at "pg_is_in_recovery" s < at "CREATE ROLE app" s)
        assertBool s (at "pg_is_in_recovery" s < at "CREATE DATABASE app OWNER app" s)
    , testCase "the script names the passfile and holds no password" $ do
        let s = Prereqs.applicationsScript pair full Pair.A
        assertBool s ("/run/secrets/app.pgpass" `isInfixOf` s)
        assertBool s ("PASSWORD '$salmon_pw'" `isInfixOf` s)
    ]

-------------------------------------------------------------------------------
-- Run: the auth file.

installTests :: [TestTree]
installTests =
    [ testCase "the auth file restarts pgbouncer when it changed, and only then" $
        withBouncerDir $ \dir run restarts -> do
            writeFile (dir </> "src/userlist.txt") "\"app\" \"md5-first\"\n"
            writeFile (dir </> "src/console.pgpass") "*:*:*:router:console-secret\n"
            ok =<< run
            assertEqual "first install" 1 =<< restarts
            assertEqual "" "\"app\" \"md5-first\"\n" =<< readFile (dir </> "etc/userlist.txt")
            assertEqual "auth file mode" 0o640 =<< modeOf (dir </> "etc/userlist.txt")
            assertEqual "passfile mode" 0o600 =<< modeOf (dir </> "etc/console.pgpass")

            ok =<< run
            assertEqual "an unchanged file is not a restart" 1 =<< restarts

            -- a console password alone is not pgbouncer's business
            writeFile (dir </> "src/console.pgpass") "*:*:*:router:rotated\n"
            ok =<< run
            assertEqual "nor is a changed console passfile" 1 =<< restarts
            assertEqual "though it is installed" "*:*:*:router:rotated\n" =<< readFile (dir </> "etc/console.pgpass")

            writeFile (dir </> "src/userlist.txt") "\"app\" \"md5-second\"\n"
            ok =<< run
            assertEqual "a changed one is" 2 =<< restarts
    , testCase "a missing file is named, and nothing else is said" $
        withBouncerDir $ \dir run restarts -> do
            writeFile (dir </> "src/console.pgpass") "*:*:*:router:console-secret\n"
            (code, out, err) <- run
            assertBool "fails" (code /= ExitSuccess)
            assertBool err ((dir </> "src/userlist.txt") `isInfixOf` err)
            assertBool (out <> err) (not ("console-secret" `isInfixOf` (out <> err)))
            assertEqual "and nothing was restarted" 0 =<< restarts
    ]
  where
    ok (code, out, err) = assertEqual (out <> err) ExitSuccess code

{- | A bouncer whose configuration directory is a temporary one, whose files
belong to whoever runs the tests, and whose @systemctl@ writes down what it
was asked.
-}
withBouncerDir :: (FilePath -> IO (ExitCode, String, String) -> IO Int -> IO a) -> IO a
withBouncerDir k =
    withSystemTempDirectory "salmon-pair-prereqs" $ \dir -> do
        mapM_ (createDirectoryIfMissing True . (dir </>)) ["src", "etc", "bin"]
        user <- trim <$> readProcess "id" ["-un"] ""
        group <- trim <$> readProcess "id" ["-gn"] ""
        let secret from mode = Prereqs.SecretFile (Just (dir </> "src" </> from)) (Text.pack user) (Text.pack group) mode
            pre =
                Prereqs.defaultPrereqs
                    { Prereqs.prereq_bouncer_packages = []
                    , Prereqs.prereq_console_passfile = secret "console.pgpass" "0600"
                    , Prereqs.prereq_userlist = secret "userlist.txt" "0640"
                    }
            b =
                bouncer
                    { Pair.bouncer_config_dir = dir </> "etc"
                    , Pair.bouncer_console_passfile = dir </> "etc/console.pgpass"
                    }
            logFile = dir </> "systemctl.log"
        standIn (dir </> "bin/systemctl") ["echo \"$@\" >> " <> logFile]
        let restarts = do
                there <- doesFileExist logFile
                if there then length . filter (== "try-restart pgbouncer") . lines <$> readFile logFile else pure 0
        k dir (runWith (dir </> "bin") (Prereqs.bouncerPrereqsScript pre b)) restarts

-------------------------------------------------------------------------------
-- Run: the application's password.

applicationTests :: [TestTree]
applicationTests =
    [ testCase "the password reaches psql on standard input, quoted, and nowhere else" $
        withMember "f" False $ \dir run -> do
            (code, out, err) <- run
            assertEqual (out <> err) ExitSuccess code
            args <- readFile (dir </> "args.log")
            stdin' <- readFile (dir </> "stdin.log")
            assertBool stdin' ("ALTER ROLE app LOGIN PASSWORD 'it''s-a-secret';" `isInfixOf` stdin')
            assertBool args (not ("secret" `isInfixOf` args))
            assertBool (out <> err) (not ("secret" `isInfixOf` (out <> err)))
            assertBool args ("CREATE ROLE app LOGIN" `isInfixOf` args)
            assertBool args ("CREATE DATABASE app OWNER app" `isInfixOf` args)
    , testCase "a standby makes no role and no database" $
        withMember "t" False $ \dir run -> do
            (code, out, err) <- run
            assertEqual (out <> err) ExitSuccess code
            args <- readFile (dir </> "args.log")
            assertBool args (not ("CREATE" `isInfixOf` args))
            assertBool "nothing went to standard input" . not =<< doesFileExist (dir </> "stdin.log")
    , testCase "a psql that fails on the password statement, and quotes it, is not relayed" $
        withMember "f" True $ \_ run -> do
            (code, out, err) <- run
            assertBool "fails" (code /= ExitSuccess)
            assertBool err ("could not set the password of role app" `isInfixOf` err)
            assertBool (out <> err) (not ("secret" `isInfixOf` (out <> err)))
    ]

{- | A member whose @pg_lsclusters@ and @sudo@ are stand-ins: the first
names a cluster, the second writes down the arguments it was given and
whatever came on standard input, and answers the two questions the script
asks. Given @failing@, it fails the statement that arrives on standard input
the way psql does, by quoting it.
-}
withMember :: String -> Bool -> (FilePath -> IO (ExitCode, String, String) -> IO a) -> IO a
withMember inRecovery failing k =
    withSystemTempDirectory "salmon-pair-prereqs" $ \dir -> do
        createDirectoryIfMissing True (dir </> "bin")
        writeFile (dir </> "app.pgpass") "*:*:*:app:it's-a-secret\n"
        standIn (dir </> "bin/pg_lsclusters") ["echo '15 main 5432 online postgres /var/lib/postgresql/15/main /var/log/postgresql/postgresql-15-main.log'"]
        standIn
            (dir </> "bin/sudo")
            [ "echo \"$@\" >> " <> (dir </> "args.log")
            , "case \"$*\" in"
            , "  *pg_is_in_recovery*) echo " <> inRecovery <> " ;;"
            , "  *pg_reload_conf*|*pg_database*|*' -c '*) : ;;"
            , "  *)"
            , "    statement=$(cat)"
            , "    printf '%s\\n' \"$statement\" >> " <> (dir </> "stdin.log")
            , if failing then "    echo \"ERROR: LINE 1: $statement\" >&2; exit 3" else "    :"
            , "    ;;"
            , "esac"
            ]
        let pre = full{Prereqs.prereq_applications = [app{Prereqs.app_passfile = dir </> "app.pgpass", Prereqs.app_clients = []}]}
        k dir (runWith (dir </> "bin") (Prereqs.applicationsScript pair pre Pair.A))

-------------------------------------------------------------------------------

graphTests :: [TestTree]
graphTests =
    [ testCase "a machine's prerequisites come before the pair's node for it" $ do
        assertBool "member A" (refOf "pg-pair-member-prereqs" "app@10.0.0.1" `elem` Dag.dependenciesOf dag (refOf "pg-pair-member" "app@10.0.0.1"))
        assertBool "member B" (refOf "pg-pair-member-prereqs" "app@10.0.0.2" `elem` Dag.dependenciesOf dag (refOf "pg-pair-member" "app@10.0.0.2"))
        assertBool "bouncer" (refOf "pg-pair-bouncer-prereqs" "app@10.0.0.3" `elem` Dag.dependenciesOf dag (refOf "pg-pair-bouncer" "app@10.0.0.3"))
    , testCase "the applications come after the role node, on both members" $ do
        assertBool "A" (refOf "pg-pair-role" "app" `elem` Dag.dependenciesOf dag (refOf "pg-pair-applications" "app@10.0.0.1"))
        assertBool "B" (refOf "pg-pair-role" "app" `elem` Dag.dependenciesOf dag (refOf "pg-pair-applications" "app@10.0.0.2"))
    , testCase "naming the pair's nodes again is a merge, not a conflict" $
        assertEqual "" 0 (length (Dag.dagConflicts dag))
    , testCase "the role node still stands on the pair's own nodes" $
        assertBool "" (refOf "pg-pair-member" "app@10.0.0.1" `elem` Dag.dependenciesOf dag (refOf "pg-pair-role" "app"))
    , testCase "no application, no application node" $
        assertBool "" (refOf "pg-pair-applications" "app@10.0.0.1" `notElem` Dag.dagOrder (foldOf (Prereqs.pairWithPrereqs silent pair Prereqs.defaultPrereqs)))
    ]
  where
    dag = foldOf (Prereqs.pairWithPrereqs silent pair full)
    refOf kind key = mkRef kind (key :: Text)

foldOf :: Op -> Dag.Dag Extension
foldOf = Dag.foldDag Dag.sameRepresentative . evalDeps

-------------------------------------------------------------------------------

bashParses :: String -> IO ()
bashParses script = do
    (code, _, err) <- readCreateProcessWithExitCode (proc "bash" ["-n", "-c", script]) ""
    assertEqual (err <> "\n" <> script) ExitSuccess code

-- | Runs a script with a directory of stand-ins ahead of everything on @PATH@.
runWith :: FilePath -> String -> IO (ExitCode, String, String)
runWith bin script = do
    environment <- getEnvironment
    let path = maybe bin (\p -> bin <> ":" <> p) (lookup "PATH" environment)
    readCreateProcessWithExitCode
        (proc "bash" ["-c", script]){env = Just (("PATH", path) : filter ((/= "PATH") . fst) environment)}
        ""

standIn :: FilePath -> [String] -> IO ()
standIn path body = do
    writeFile path (unlines ("#!/bin/bash" : body))
    perms <- getPermissions path
    setPermissions path (setOwnerExecutable True perms)

modeOf :: FilePath -> IO Int
modeOf path = fromIntegral . intersectFileModes accessModes . fileMode <$> getFileStatus path

trim :: String -> String
trim = reverse . dropWhile (`elem` ("\n " :: String)) . reverse

at :: String -> String -> Int
at needle hay = go 0 hay
  where
    go n rest
        | needle `isInfixOf` take (length needle) rest = n
        | null rest = maxBound
        | otherwise = go (n + 1) (drop 1 rest)

-------------------------------------------------------------------------------

full :: Prereqs.Prereqs
full =
    Prereqs.defaultPrereqs
        { Prereqs.prereq_repl_passfile = Prereqs.postgresOwned "/run/secrets/repl.pgpass"
        , Prereqs.prereq_rewind_passfile = Prereqs.postgresOwned "/run/secrets/rewind.pgpass"
        , Prereqs.prereq_console_passfile = Prereqs.postgresOwned "/run/secrets/console.pgpass"
        , Prereqs.prereq_userlist = (Prereqs.postgresOwned "/run/secrets/userlist.txt"){Prereqs.secret_mode = "0640"}
        , Prereqs.prereq_applications = [app]
        }

app :: Prereqs.Application
app =
    Prereqs.Application
        { Prereqs.app_role = "app"
        , Prereqs.app_database = "app"
        , Prereqs.app_passfile = "/run/secrets/app.pgpass"
        , Prereqs.app_clients = ["10.0.0.3"]
        , Prereqs.app_hba_method = "md5"
        }

pair :: Pair.Pair
pair =
    Pair.Pair
        { Pair.pair_name = "app"
        , Pair.pair_a = member "10.0.0.1"
        , Pair.pair_b = member "10.0.0.2"
        , Pair.pair_primary = Pair.A
        , Pair.pair_seed = Just Pair.B
        , Pair.pair_bouncers = [bouncer]
        , Pair.pair_may_discard = Nothing
        , Pair.pair_reseed = Nothing
        , Pair.pair_conn_security = Nothing
        , Pair.pair_repl_role = "replicator"
        , Pair.pair_repl_passfile = "/etc/postgresql/repl.pgpass"
        , Pair.pair_rewind_role = "rewinder"
        , Pair.pair_rewind_passfile = "/etc/postgresql/rewind.pass"
        , Pair.pair_ssh_known_hosts = Nothing
        , Pair.pair_catch_up_seconds = 60
        }
  where
    member host = Pair.Member "root" host "main" 5432 Nothing Nothing

bouncer :: Pair.Bouncer
bouncer =
    Pair.Bouncer
        { Pair.bouncer_name = "bouncer-1"
        , Pair.bouncer_ssh_user = "root"
        , Pair.bouncer_ssh_host = "10.0.0.3"
        , Pair.bouncer_ssh_identity = Nothing
        , Pair.bouncer_console_user = "router"
        , Pair.bouncer_console_port = 6432
        , Pair.bouncer_console_passfile = "/etc/pgbouncer/console.pgpass"
        , Pair.bouncer_alias = "app"
        , Pair.bouncer_dbname = "app"
        , Pair.bouncer_routing_path = "/etc/pgbouncer/routing.ini"
        , Pair.bouncer_config_dir = "/etc/pgbouncer"
        , Pair.bouncer_listen_port = 6432
        }
