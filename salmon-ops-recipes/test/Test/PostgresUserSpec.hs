{- | The check of the @pg-user@ node ('Postgres.user', 'Postgres.userPassFile'):
the password the cluster holds against the declared one.

Layer 0 is the pure reading of @pg_authid@ ('Postgres.interpretRoleAuthRow'),
against verifiers computed independently (Python's @hashlib@, quoted below)
and against the module's own encoders by property. Layer 2 runs the node
against a real PostgreSQL in a podman container: the verifiers there are
PostgreSQL's own, so the SCRAM and MD5 readings are checked against the
server that writes them.
-}
module Test.PostgresUserSpec (tests, sandboxTests) where

import Control.Exception (try)
import qualified Data.ByteString as ByteString
import Data.Char (chr)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Exit (ExitCode (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process.ListLike (CmdSpec (..), CreateProcess (..))
import Test.QuickCheck (Gen, chooseInt, counterexample, elements, forAll, listOf, vectorOf, (.&&.), (===), (==>))
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Command (..), CommandFailed, untrackedExec)
import Salmon.Builtin.Nodes.Filesystem (File (..))
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (silent)

import Test.Harness

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Postgres pg-user check (Layer 0)"
        [ testGroup "verdicts" verdicts
        , testGroup "properties" properties
        , testGroup "the declaration" declaration
        , testGroup "the password is not in argv" argv
        , testGroup "pg-script: the password is not in argv" pgScript
        ]

sandboxTests :: TestTree
sandboxTests =
    -- sequential: both shim PATH, which is process-global, and 'heavy' in
    -- Main serializes its direct children only
    sequentialTestGroup
        "Salmon.Builtin.Nodes.Postgres pg-user check (Layer 2, podman sandbox)"
        AllFinish
        [ testCase "create, satisfied, drift, rotation, md5" userLifecycle
        , testCase "SQL on stdin: quotes, a failing up, the replication role" stdinLifecycle
        ]

pw :: Text -> Postgres.Password
pw = Postgres.Password

verdict :: Text -> Text -> Text -> CheckResult
verdict role pass = Postgres.interpretRoleAuthRow role (pw pass)

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

failureText :: CheckResult -> Text
failureText (Failure t) = t
failureText _ = ""

-------------------------------------------------------------------------------

{- | Computed with Python's @hashlib.pbkdf2_hmac@ and @hmac@, not with this
module:

> s = hashlib.pbkdf2_hmac("sha256", p.encode(), salt, it)
> stored = sha256(hmac(s, b"Client Key")); server = hmac(s, b"Server Key")
-}
pencil4096 :: Text
pencil4096 = "SCRAM-SHA-256$4096:AAECAwQFBgcICQoLDA0ODw==$zHCdol2044/ZyWzPLi7oxApCkamKw9Z+E4U/QApd/5Y=:dd5peBOitVnLNFu7VmwP+HiDaaw4OUCv396eVCWhYiE="

-- | Password @"hunter2\\n"@ (a passfile's trailing newline is part of the password), salt @"salmon-salt-0123"@, one round.
hunterNewline1 :: Text
hunterNewline1 = "SCRAM-SHA-256$1:c2FsbW9uLXNhbHQtMDEyMw==$p7/g8OA9+/BZbfP7B5DA24bQX4ND5K/liyZbhqI1kGw=:lckKKrz5pkMfxq2rvcO1eSlKOCyQUcPzqMkU911fZlg="

-- | @"md5" + md5("pencil" + "app")@.
pencilAppMd5 :: Text
pencilAppMd5 = "md506d701b4dd9f7cbb00816251a451b978"

verdicts :: [TestTree]
verdicts =
    [ testCase "a SCRAM verifier of the declared password is satisfied" $
        assertEqual "" Success (verdict "app" "pencil" ("t|" <> pencil4096 <> "\n"))
    , testCase "the encoder agrees with an independent implementation" $ do
        assertEqual "4096 rounds" pencil4096 (Postgres.scramVerifier 4096 (ByteString.pack [0 .. 15]) (pw "pencil"))
        assertEqual "one round, trailing newline" hunterNewline1 (Postgres.scramVerifier 1 "salmon-salt-0123" (pw "hunter2\n"))
        assertEqual "md5" pencilAppMd5 (Postgres.md5Verifier "app" (pw "pencil"))
    , testCase "a passfile's trailing newline is part of the password" $ do
        assertEqual "" Success (verdict "app" "hunter2\n" ("t|" <> hunterNewline1))
        assertBool "" (isFailure (verdict "app" "hunter2" ("t|" <> hunterNewline1)))
    , testCase "another password is a failure naming the role and nothing else" $
        assertEqual "" (Failure "the password of role app is not the declared one") (verdict "app" "pencil2" ("t|" <> pencil4096))
    , testCase "an MD5 verifier of the declared password is satisfied, salted by the role name" $ do
        assertEqual "" Success (verdict "app" "pencil" ("t|" <> pencilAppMd5))
        assertBool "another role" (isFailure (verdict "other" "pencil" ("t|" <> pencilAppMd5)))
        assertBool "another password" (isFailure (verdict "app" "pen" ("t|" <> pencilAppMd5)))
    , testCase "no row: the role does not exist" $
        assertEqual "" (Failure "role app does not exist") (verdict "app" "pencil" "")
    , testCase "NOLOGIN is a failure, whatever the password" $
        assertEqual "" (Failure "role app cannot log in") (verdict "app" "pencil" ("f|" <> pencil4096))
    , testCase "no password is a failure" $
        assertEqual "" (Failure "role app has no password") (verdict "app" "pencil" "t|\n")
    , testCase "what cannot be recomputed here is Unknown, not a drift" $ do
        assertEqual "unknown scheme" Unknown (verdict "app" "pencil" "t|SCRAM-SHA-512$4096:AAAA$x:y")
        assertEqual "plaintext of a very old server" Unknown (verdict "app" "pencil" "t|pencil")
        assertEqual "garbled SCRAM" Unknown (verdict "app" "pencil" "t|SCRAM-SHA-256$abc")
        assertEqual "bad salt" Unknown (verdict "app" "pencil" "t|SCRAM-SHA-256$4096:!!!$x:y")
        assertEqual "absurd rounds" Unknown (verdict "app" "pencil" "t|SCRAM-SHA-256$2000000000:AAECAwQFBgcICQoLDA0ODw==$x:y")
        assertEqual "zero rounds" Unknown (verdict "app" "pencil" "t|SCRAM-SHA-256$0:AAECAwQFBgcICQoLDA0ODw==$x:y")
        assertEqual
            "non-ASCII password under SCRAM (SASLprep)"
            Unknown
            (verdict "app" "p\233ncil" ("t|" <> Postgres.scramVerifier 1 "salt" (pw "p\233ncil")))
    , testCase "a non-ASCII password is still compared under MD5, which has no normalization" $
        assertEqual "" Success (verdict "app" "p\233ncil" ("t|" <> Postgres.md5Verifier "app" (pw "p\233ncil")))
    , testCase "the query names the role, quoted, and reads pg_authid" $ do
        let sql = Postgres.roleAuthSql "o'brien"
        assertBool (Text.unpack sql) ("rolname = 'o''brien'" `Text.isInfixOf` sql)
        assertBool (Text.unpack sql) ("FROM pg_authid" `Text.isInfixOf` sql)
    , testCase "a passfile that cannot be read is Unknown, and the cluster is not asked" $ do
        r <- Postgres.checkUserPassword 1 "app" (Postgres.readPassword "/nonexistent/salmon-test/passfile")
        assertEqual "" Unknown r
    ]

-------------------------------------------------------------------------------

-- | Printable ASCII and the whitespace a file can end with: what a passfile holds.
genAsciiPassword :: Gen Text
genAsciiPassword = Text.pack <$> listOf (elements (['\t', '\n'] <> [' ' .. '~']))

-- | A password no role name, scheme name or base64 text can contain by accident.
genMarkedPassword :: Gen Text
genMarkedPassword = ("SECRET~" <>) <$> genAsciiPassword

genSalt :: Gen ByteString.ByteString
genSalt = ByteString.pack . fmap fromIntegral <$> (chooseInt (1, 32) >>= \n -> vectorOf n (chooseInt (0, 255)))

genRole :: Gen Text
genRole = Text.pack <$> (chooseInt (1, 12) >>= \n -> vectorOf n (elements (['a' .. 'z'] <> ['0' .. '9'] <> "_")))

genIters :: Gen Int
genIters = chooseInt (1, 40)

properties :: [TestTree]
properties =
    [ testProperty "a SCRAM verifier made from a password is satisfied by it" $
        forAll genRole $ \role -> forAll genAsciiPassword $ \p -> forAll genSalt $ \salt -> forAll genIters $ \it ->
            verdict role p ("t|" <> Postgres.scramVerifier it salt (pw p)) === Success
    , testProperty "a SCRAM verifier made from another password is a failure" $
        forAll genRole $ \role -> forAll genAsciiPassword $ \p -> forAll genAsciiPassword $ \q -> forAll genSalt $ \salt -> forAll genIters $ \it ->
            p /= q ==> isFailure (verdict role q ("t|" <> Postgres.scramVerifier it salt (pw p)))
    , testProperty "an MD5 verifier is satisfied by its password and role, and only those" $
        forAll genRole $ \role -> forAll genRole $ \role' -> forAll genAsciiPassword $ \p -> forAll genAsciiPassword $ \q ->
            let v = "t|" <> Postgres.md5Verifier role (pw p)
             in (verdict role p v === Success)
                    .&&. ((p /= q) ==> isFailure (verdict role q v))
                    .&&. ((role /= role') ==> isFailure (verdict role' p v))
    , testProperty "a verifier's parameters change it: salt and rounds" $
        forAll genAsciiPassword $ \p -> forAll genSalt $ \s1 -> forAll genSalt $ \s2 -> forAll genIters $ \it ->
            s1 /= s2 ==> Postgres.scramVerifier it s1 (pw p) /= Postgres.scramVerifier it s2 (pw p)
                && Postgres.scramVerifier it s1 (pw p) /= Postgres.scramVerifier (it + 1) s1 (pw p)
    , testProperty "no failure text holds the password, the verifier or either key" $
        forAll genRole $ \role -> forAll genMarkedPassword $ \p -> forAll genMarkedPassword $ \q -> forAll genSalt $ \salt -> forAll genIters $ \it ->
            forAll (elements ["t|", "f|"]) $ \login ->
                let scram = Postgres.scramVerifier it salt (pw p)
                    md5 = Postgres.md5Verifier role (pw p)
                    secrets = [p, q, scram, md5, Text.drop 3 md5] <> Text.splitOn ":" (last (Text.splitOn "$" scram))
                    texts = [failureText (verdict role q (login <> v)) | v <- [scram, md5]]
                 in counterexample (show texts) $
                        and [not (s `Text.isInfixOf` t) | s <- secrets, t <- texts]
    , testProperty "the verdict depends on the first row only, and on surrounding whitespace not at all" $
        forAll genRole $ \role -> forAll genAsciiPassword $ \p -> forAll genSalt $ \salt ->
            let row = "t|" <> Postgres.scramVerifier 2 salt (pw p)
             in verdict role p ("\n" <> row <> "\nt|junk\n") === verdict role p row
    , testProperty "a password with a character above ASCII is never judged under SCRAM" $
        forAll genAsciiPassword $ \p -> forAll (chooseInt (128, 0x2FFF)) $ \c ->
            let p' = p <> Text.singleton (chr c)
             in verdict "app" p' ("t|" <> Postgres.scramVerifier 1 "salt" (pw p')) === Unknown
    ]

-------------------------------------------------------------------------------

pgUserAct :: Op -> Act Extension
pgUserAct = actAt "pg-user" "app"

actAt :: Text -> Text -> Op -> Act Extension
actAt kind key o =
    maybe (error ("no " <> Text.unpack kind <> " node")) id $
        Map.lookup (mkRef kind key) (Dag.dagNodes (Dag.foldDag Dag.sameRepresentative (evalDeps o)))

{- | What the node renders is what it rendered before it had a check: a
changed representative is a restart of what stands on it.
-}
declaration :: [TestTree]
declaration =
    [ testCase "user: same ref and help, no notes" $ do
        let act = pgUserAct (Postgres.user silent ignoreTrack ignoreTrack 5432 (Postgres.User "app") (pw "pencil"))
        assertEqual "help" "create user app" act.extension.help
        assertEqual "notes" [] act.extension.notes
    , testCase "userPassFile: same ref and help, no notes, nothing of the password" $ do
        let act = pgUserAct (Postgres.userPassFile silent ignoreTrack ignoreTrack 5432 (PreExisting "/run/secrets/app.pass") (Postgres.User "app"))
        assertEqual "help" "set user password for app from file at /run/secrets/app.pass" act.extension.help
        assertEqual "notes" [] act.extension.notes
    , testCase "replicationUser: same ref and help, no notes" $ do
        let act = actAt "pg-replication-user" "repl" (Postgres.replicationUser silent ignoreTrack ignoreTrack 5432 (Postgres.User "repl") (pw "pencil"))
        assertEqual "help" "create replication user repl" act.extension.help
        assertEqual "notes" [] act.extension.notes
    , testProperty "user: nothing public holds the password" $
        forAll genMarkedPassword $ \p ->
            let act = pgUserAct (Postgres.user silent ignoreTrack ignoreTrack 5432 (Postgres.User "app") (pw p))
             in not (any (p `Text.isInfixOf`) (act.extension.help : act.extension.notes))
    ]

-------------------------------------------------------------------------------

-- | The two commands that carry a password, as the nodes build them.
roleCommands :: Text -> [Postgres.PsqlAdmin]
roleCommands p = [Postgres.CreateUser "app" (pw p), Postgres.CreateReplicationUser "repl" (pw p)]

argvOf :: Postgres.PsqlAdmin -> [String]
argvOf c = case cmdspec (prepare (Postgres.psqlAdminRun_Sudo 5432) c) of
    RawCommand exe args -> exe : args
    ShellCommand line -> [line]

{- | 'Postgres.psqlAdminRun_Sudo' with the executable swapped for a stand-in
that records what it was given, its arguments untouched. The swap is in the
'CreateProcess', not on @PATH@: the suite runs groups in parallel in one
process, and @PATH@ is the process's.
-}
recordingCommand :: FilePath -> Command "psql" Postgres.PsqlAdmin
recordingCommand standIn = Command $ \c ->
    let p = prepare (Postgres.psqlAdminRun_Sudo 5432) c
     in case cmdspec p of
            RawCommand _ args -> p{cmdspec = RawCommand standIn args}
            ShellCommand _ -> p

isInfixOfS :: String -> String -> Bool
isInfixOfS needle hay = Text.pack needle `Text.isInfixOf` Text.pack hay

{- | @pg-user@ and @pg-replication-user@ used to put @ALTER ROLE ... PASSWORD
'...'@ in @psql -c@, so in argv, which every local user reads in @/proc@
while psql runs and which 'Binary.CommandStart' reports. The SQL now goes on
stdin; argv, every report and the failure text are free of it.
-}
argv :: [TestTree]
argv =
    [ testProperty "the role commands' argv never holds the password" $
        forAll genMarkedPassword $ \p ->
            let argvs = fmap argvOf (roleCommands p)
             in counterexample (show argvs) $
                    not (any (any ("SECRET~" `isInfixOfS`)) argvs)
    , testProperty "their stdin holds verifiers of an ASCII password, never the password" $
        forAll genMarkedPassword $ \p -> forAll genSalt $ \salt ->
            and
                [ not (p `Text.isInfixOf` sql)
                    && (" PASSWORD " <> quoted (Postgres.scramVerifier 4096 salt (pw p)) <> ";\n") `Text.isInfixOf` sql
                    && (" PASSWORD " <> quoted (Postgres.md5Verifier role (pw p)) <> ";\n") `Text.isInfixOf` sql
                | (role, c) <- zip ["app", "repl"] (roleCommands p)
                , let sql = Text.decodeUtf8 (Postgres.psqlAdminStdinSalted salt c)
                ]
    , testProperty "the verifiers it sends are ones the check accepts" $
        forAll genRole $ \role -> forAll genAsciiPassword $ \p -> forAll genSalt $ \salt ->
            (verdict role p ("t|" <> Postgres.scramVerifier 4096 salt (pw p)) === Success)
                .&&. (verdict role p ("t|" <> Postgres.md5Verifier role (pw p)) === Success)
    , testCase "the scheme follows password_encryption, read with \\gset" $ do
        let sql = Text.decodeUtf8 (Postgres.psqlAdminStdinSalted "0123456789abcdef" (Postgres.CreateUser "app" (pw "pencil")))
        assertEqual
            "script"
            ( Text.unlines
                [ "DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'app') THEN CREATE ROLE app WITH LOGIN; END IF; END $$;"
                , "SELECT current_setting('password_encryption') IN ('md5', 'on') AS salmon_md5 \\gset"
                , "\\if :salmon_md5"
                , "ALTER ROLE app WITH LOGIN PASSWORD " <> quoted (Postgres.md5Verifier "app" (pw "pencil")) <> ";"
                , "\\else"
                , "ALTER ROLE app WITH LOGIN PASSWORD " <> quoted (Postgres.scramVerifier 4096 "0123456789abcdef" (pw "pencil")) <> ";"
                , "\\endif"
                ]
            )
            sql
    , testCase "each up draws a fresh 16-byte salt" $ do
        let c = Postgres.CreateUser "app" (pw "pencil")
            saltOf dat =
                [ Text.takeWhile (/= '$') rest
                | l <- Text.lines (Text.decodeUtf8 dat)
                , Just rest <- [Text.stripPrefix "ALTER ROLE app WITH LOGIN PASSWORD 'SCRAM-SHA-256$4096:" l]
                ]
        a <- Postgres.psqlAdminStdin c
        b <- Postgres.psqlAdminStdin c
        assertBool "two runs, two salts" (saltOf a /= saltOf b)
        assertEqual "a 16-byte salt is 24 base64 characters" [24] (fmap Text.length (saltOf a))
    , testCase "psql reads the script on stdin: no psqlrc, terse errors, stops on the first one" $
        mapM_
            ( assertEqual
                "argv"
                ["sudo", "-u", "postgres", "psql", "-p", "5432", "-X", "-q", "-v", "ON_ERROR_STOP=1", "-v", "VERBOSITY=terse", "-f", "-"]
                . argvOf
            )
            (roleCommands "pencil")
    , testCase "a password above ASCII is still sent as it is, one quoted literal" $ do
        let sql = Text.decodeUtf8 (Postgres.psqlAdminStdinSalted "salt" (Postgres.CreateUser "app" (pw "it's \233t\233")))
        assertBool (Text.unpack sql) ("ALTER ROLE app WITH LOGIN PASSWORD 'it''s \233t\233';\n" `Text.isSuffixOf` sql)
        assertBool "no \\if" (not ("\\if" `Text.isInfixOf` sql))
    , testCase "the role attributes are what they were" $ do
        let sqlOf = Text.decodeUtf8 . Postgres.psqlAdminStdinSalted "salt"
            u = Postgres.CreateUser "app" (pw "pencil")
            repl = Postgres.CreateReplicationUser "repl" (pw "pencil")
        assertBool "user" ("ALTER ROLE app WITH LOGIN PASSWORD 'SCRAM-SHA-256$" `Text.isInfixOf` sqlOf u)
        assertBool "user, created" ("THEN CREATE ROLE app WITH LOGIN; END IF" `Text.isInfixOf` sqlOf u)
        assertBool "repl" ("ALTER ROLE repl WITH REPLICATION LOGIN PASSWORD 'md5" `Text.isInfixOf` sqlOf repl)
        assertBool "repl, created" ("THEN CREATE ROLE repl WITH REPLICATION LOGIN; END IF" `Text.isInfixOf` sqlOf repl)
    , testCase "no other command sends anything on stdin" $
        Postgres.psqlAdminStdin Postgres.ReloadConf >>= assertEqual "" ""
    , testCase "run for real: argv, stdin, every report and the failure text are free of the password" $
        withTempDir $ \tmp -> do
            let secret = "SECRET~run-for-real" :: Text
                ok = tmp </> "ok"
                failing = tmp </> "failing"
            writeExecutable ok $
                unlines
                    [ "#!/bin/bash"
                    , "printf '%s\\n' \"$@\" > " <> show (tmp </> "argv")
                    , "cat > " <> show (tmp </> "stdin")
                    ]
            writeExecutable failing $
                unlines ["#!/bin/bash", "cat > /dev/null", "echo 'psql:<stdin>:2: ERROR:  boom' >&2", "exit 3"]
            mapM_
                ( \c -> do
                    (r, readBack) <- capture
                    dat <- Postgres.psqlAdminStdin c
                    untrackedExec (recordingCommand ok) c dat r
                    recorded <- readFile (tmp </> "argv")
                    stdin <- ByteString.readFile (tmp </> "stdin")
                    reports <- readBack
                    assertBool recorded ("-f\n-\n" `isInfixOfS` recorded)
                    assertBool "argv as executed" (not (Text.unpack secret `isInfixOfS` recorded))
                    assertBool "a verifier went on stdin" ("PASSWORD 'SCRAM-SHA-256$4096:" `Text.isInfixOf` Text.decodeUtf8 stdin)
                    assertBool "the password did not" (not (secret `Text.isInfixOf` Text.decodeUtf8 stdin))
                    assertBool "reports" (not (any ((Text.unpack secret `isInfixOfS`) . show) reports))

                    (r', readBack') <- capture
                    failed <- try (untrackedExec (recordingCommand failing) c dat r') :: IO (Either CommandFailed ())
                    reports' <- readBack'
                    case failed of
                        Right () -> assertBool "a failing psql fails up" False
                        Left e -> assertBool (show e) (not (Text.unpack secret `isInfixOfS` show e))
                    assertBool "reports of the failure" (not (any ((Text.unpack secret `isInfixOfS`) . show) reports'))
                )
                (roleCommands secret)
    ]

quoted :: Text -> Text
quoted t = "'" <> Text.replace "'" "''" t <> "'"

-------------------------------------------------------------------------------

scriptConn :: Text -> Postgres.ConnString Postgres.Password
scriptConn p = Postgres.ConnString (Postgres.Server "localhost" 5432) (Postgres.User "app") (pw p) (Postgres.Database "appdb")

scriptArgv :: Postgres.ConnString a -> [String]
scriptArgv c = case cmdspec (prepare (Postgres.psqlUserRun c) (Postgres.UserScript "/srv/migrations/001.sql")) of
    RawCommand exe args -> exe : args
    ShellCommand line -> [line]

{- | @pg-script@ ('Postgres.userScript', 'Postgres.userScriptInMemoryPass')
used to give psql a @postgresql://user:password\@…@ URL in argv. The password
now goes on a bash's stdin, which puts it in @PGPASSWORD@ and @exec@s psql: it
is in no argv, no 'CreateProcess' (whose environment is reported too) and no
failure text.
-}
pgScript :: [TestTree]
pgScript =
    [ testProperty "argv never holds the password" $
        forAll genMarkedPassword $ \p ->
            let a = scriptArgv (scriptConn p)
             in counterexample (show a) (not (any ("SECRET~" `isInfixOfS`) a))
    , testCase "argv: a bash reading the password, then psql with a URL without one" $ do
        assertEqual
            "argv"
            [ "bash"
            , "-c"
            , "IFS= read -r -d '' PGPASSWORD || true; export PGPASSWORD; exec psql \"$@\" </dev/null"
            , "psql"
            , "postgresql://app@localhost:5432/appdb"
            , "-f"
            , "/srv/migrations/001.sql"
            ]
            (scriptArgv (scriptConn "pencil"))
        assertBool "no environment of its own" (null (env (prepare (Postgres.psqlUserRun (scriptConn "pencil")) (Postgres.UserScript "/x.sql"))))
    , testCase "stdin is the password's bytes, nothing else" $
        assertEqual "" (Text.encodeUtf8 "pencil\n") (Postgres.passwordStdin (pw "pencil\n"))
    , testCase "same ref and help, no notes" $ do
        let o = Postgres.userScriptInMemoryPass silent ignoreTrack ignoreTrack (scriptConn "SECRET~") (PreExisting "/srv/migrations/001.sql")
            -- keyed by the script's path, a FilePath
            act =
                maybe (error "no pg-script node") id $
                    Map.lookup (mkRef "pg-script" ("/srv/migrations/001.sql" :: FilePath)) (Dag.dagNodes (Dag.foldDag Dag.sameRepresentative (evalDeps o)))
        assertEqual "help" "runs pg script /srv/migrations/001.sql" act.extension.help
        assertEqual "notes" [] act.extension.notes
    , testCase "run for real: psql gets the password in PGPASSWORD only, newlines kept; reports and failure text are free of it" $
        withTempDir $ \tmp -> do
            let secret = "SECRET~it's a \\ back:slash\n" :: Text
                c = scriptConn secret
                bin = tmp </> "bin"
                withPath p = Command $ \u -> (prepare (Postgres.psqlUserRun c) u){env = Just [("PATH", p <> ":/usr/bin:/bin")]}
            createDirectoryIfMissing True (bin </> "ok")
            createDirectoryIfMissing True (bin </> "failing")
            writeExecutable (bin </> "ok" </> "psql") $
                unlines
                    [ "#!/bin/bash"
                    , "printf '%s\\n' \"$@\" > " <> show (tmp </> "argv")
                    , "printf '%s' \"$PGPASSWORD\" > " <> show (tmp </> "pgpassword")
                    , "cat > " <> show (tmp </> "stdin")
                    ]
            writeExecutable (bin </> "failing" </> "psql") $
                unlines ["#!/bin/bash", "echo 'psql:/srv/migrations/001.sql:2: ERROR:  boom' >&2", "exit 3"]
            (r, readBack) <- capture
            untrackedExec (withPath (bin </> "ok")) (Postgres.UserScript "/srv/migrations/001.sql") (Postgres.passwordStdin (pw secret)) r
            recorded <- readFile (tmp </> "argv")
            passed <- ByteString.readFile (tmp </> "pgpassword")
            stdin <- ByteString.readFile (tmp </> "stdin")
            reports <- readBack
            assertEqual "argv as executed" "postgresql://app@localhost:5432/appdb\n-f\n/srv/migrations/001.sql\n" recorded
            assertEqual "PGPASSWORD is the password, byte for byte" (Text.encodeUtf8 secret) passed
            assertEqual "psql's stdin is empty" "" stdin
            assertBool "reports" (not (any (("SECRET~" `isInfixOfS`) . show) reports))

            (r', readBack') <- capture
            failed <- try (untrackedExec (withPath (bin </> "failing")) (Postgres.UserScript "/srv/migrations/001.sql") (Postgres.passwordStdin (pw secret)) r') :: IO (Either CommandFailed ())
            reports' <- readBack'
            case failed of
                Right () -> assertBool "a failing psql fails up" False
                Left e -> assertBool (show e) (not ("SECRET~" `isInfixOfS` show e))
            assertBool "reports of the failure" (not (any (("SECRET~" `isInfixOfS`) . show) reports'))
    ]

-------------------------------------------------------------------------------

{- | Against PostgreSQL 15 (Debian bookworm): the role is created by @up@,
the check is satisfied after it, a password changed by hand is seen, a new
passfile is seen, @up@ puts it back, and an MD5 verifier (what an older
cluster or @password_encryption = md5@ writes) is read too.
-}
userLifecycle :: IO ()
userLifecycle = requireExecutable "podman" $
    withContainer (Podman.Image "debian:bookworm") (Podman.PortMapping "15442" "5432" Podman.TCPPort) $ \cid -> do
        podmanExec_ cid ["apt-get", "update", "-qq"]
        podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo postgresql"]
        podmanExec_ cid ["bash", "-c", "v=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1); pg_ctlcluster \"$v\" main status >/dev/null || pg_ctlcluster \"$v\" main start"]
        let asPostgres sql = podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", sql]
        withTempDir $ \tmp -> do
            let passfile = tmp </> "app.pass"
                node = Postgres.userPassFile silent ignoreTrack ignoreTrack 5432 (PreExisting passfile) (Postgres.User "app")
                checkNow = (pgUserAct node).extension.check
            writeFile passfile "first-password\n"
            withShimmedPath cid ["sudo"] $ do
                checkNow >>= assertEqual "before up" (Failure "role app does not exist")
                runUp node >>= assertBool "up creates the role"
                checkNow >>= assertEqual "after up" Success

                -- the verifier really is SCRAM, and the password really works
                (code, out, err) <- podmanExecCapture cid ["sudo", "-u", "postgres", "psql", "-tAX", "-c", "SELECT left(rolpassword, 14) FROM pg_authid WHERE rolname = 'app'"]
                assertEqual err ExitSuccess code
                assertEqual "scheme" "SCRAM-SHA-256$" (filter (/= '\n') out)

                asPostgres "ALTER ROLE app PASSWORD 'changed-by-hand'"
                checkNow >>= assertEqual "a drift is seen" (Failure "the password of role app is not the declared one")
                runUp node >>= assertBool "up puts the declared one back"
                checkNow >>= assertEqual "converged again" Success

                writeFile passfile "rotated-password\n"
                checkNow >>= assertBool "a rotation is seen before up" . isFailure
                runUp node >>= assertBool "up applies the rotation"
                checkNow >>= assertEqual "rotated" Success

                asPostgres "ALTER ROLE app NOLOGIN"
                checkNow >>= assertEqual "nologin is seen" (Failure "role app cannot log in")
                runUp node >>= assertBool "up gives login back"
                checkNow >>= assertEqual "login again" Success

                writeFile passfile "md5-password"
                asPostgres "SET password_encryption = 'md5'; ALTER ROLE app PASSWORD 'md5-password'"
                checkNow >>= assertEqual "an MD5 verifier of the declared password" Success
                asPostgres "SET password_encryption = 'md5'; ALTER ROLE app PASSWORD 'other'"
                checkNow >>= assertBool "an MD5 verifier of another" . isFailure

{- | The role SQL as psql reads it on stdin, against PostgreSQL 15: a password
with a quote and a backslash is set as it is (the check, which reads the
verifier, is satisfied by it), a failing statement fails @up@ (a script read
from stdin exits 0 after an error unless @ON_ERROR_STOP@ is set), and the
replication role is created with its attribute and password.
-}
stdinLifecycle :: IO ()
stdinLifecycle = requireExecutable "podman" $
    withContainer (Podman.Image "debian:bookworm") (Podman.PortMapping "15443" "5432" Podman.TCPPort) $ \cid -> do
        podmanExec_ cid ["apt-get", "update", "-qq"]
        podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo postgresql"]
        podmanExec_ cid ["bash", "-c", "v=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1); pg_ctlcluster \"$v\" main status >/dev/null || pg_ctlcluster \"$v\" main start"]
        withTempDir $ \tmp -> do
            let passfile = tmp </> "app.pass"
                node = Postgres.userPassFile silent ignoreTrack ignoreTrack 5432 (PreExisting passfile) (Postgres.User "app")
            writeFile passfile "it's a \\ back\\slash"
            withShimmedPath cid ["sudo"] $ do
                runUp node >>= assertBool "up with a quote and a backslash"
                (pgUserAct node).extension.check >>= assertEqual "the password is the file's, as it is" Success

                let reserved = Postgres.userPassFile silent ignoreTrack ignoreTrack 5432 (PreExisting passfile) (Postgres.User "pg_app")
                runUp reserved >>= assertBool "a reserved role name fails up" . not

                let repl = Postgres.replicationUser silent ignoreTrack ignoreTrack 5432 (Postgres.User "repl") (pw "repl-password")
                runUp repl >>= assertBool "up creates the replication role"
                (code, out, err) <- podmanExecCapture cid ["sudo", "-u", "postgres", "psql", "-tAX", "-c", "SELECT rolreplication, rolcanlogin FROM pg_roles WHERE rolname = 'repl'"]
                assertEqual err ExitSuccess code
                assertEqual "attributes" "t|t" (filter (/= '\n') out)
                Postgres.checkUserPassword 5432 "repl" (pure (pw "repl-password")) >>= assertEqual "its password" Success

                -- the verifier salmon computed is one the server logs a client in with
                (code', out', err') <- podmanExecCapture cid ["env", "PGPASSWORD=repl-password", "psql", "-X", "-tA", "-h", "127.0.0.1", "-U", "repl", "-d", "postgres", "-c", "SELECT 1"]
                assertEqual (out' <> err') ExitSuccess code'

                -- with every statement logged, the log holds a verifier, not the password
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "ALTER SYSTEM SET log_statement = 'all'"]
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "SELECT pg_reload_conf()"]
                let logged = Postgres.replicationUser silent ignoreTrack ignoreTrack 5432 (Postgres.User "logged") (pw "never-in-the-log")
                runUp logged >>= assertBool "up with log_statement = all"
                Postgres.checkUserPassword 5432 "logged" (pure (pw "never-in-the-log")) >>= assertEqual "its password" Success
                (_, logs, _) <- podmanExecCapture cid ["bash", "-c", "cat /var/log/postgresql/*.log"]
                assertBool "the ALTER ROLE was logged" ("ALTER ROLE logged WITH REPLICATION LOGIN PASSWORD 'SCRAM-SHA-256$4096:" `isInfixOfS` logs)
                assertBool "the password was not" (not ("never-in-the-log" `isInfixOfS` logs))

                -- password_encryption = md5: an MD5 verifier is sent, and the check reads it
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "ALTER SYSTEM SET password_encryption = 'md5'"]
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "SELECT pg_reload_conf()"]
                let md5 = Postgres.replicationUser silent ignoreTrack ignoreTrack 5432 (Postgres.User "md5role") (pw "md5-password")
                runUp md5 >>= assertBool "up under md5"
                (code'', out'', err'') <- podmanExecCapture cid ["sudo", "-u", "postgres", "psql", "-tAX", "-c", "SELECT rolpassword FROM pg_authid WHERE rolname = 'md5role'"]
                assertEqual err'' ExitSuccess code''
                assertEqual "an MD5 verifier" (Text.unpack (Postgres.md5Verifier "md5role" (pw "md5-password"))) (filter (/= '\n') out'')
                Postgres.checkUserPassword 5432 "md5role" (pure (pw "md5-password")) >>= assertEqual "its password" Success

                -- a role whose password file ends with a newline, as openssl rand -hex writes it
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "ALTER SYSTEM RESET password_encryption"]
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "SELECT pg_reload_conf()"]
                let scriptedPass = tmp </> "scripted.pass"
                writeFile scriptedPass "scripted-password\n"
                runUp (Postgres.userPassFile silent ignoreTrack ignoreTrack 5432 (PreExisting scriptedPass) (Postgres.User "scripted"))
                    >>= assertBool "up creates the scripted role"

            -- pg-script: the host's psql (bash, PGPASSWORD) against the container's published port
            requireExecutable "psql" $ do
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "ALTER SYSTEM SET listen_addresses = '*'"]
                podmanExec_ cid ["bash", "-c", "echo 'host all all 0.0.0.0/0 md5' >> \"$(sudo -u postgres psql -tAX -c 'SHOW hba_file')\""]
                podmanExec_ cid ["bash", "-c", "v=$(pg_lsclusters --no-header | awk '{print $1}' | sort -n | tail -n1); pg_ctlcluster \"$v\" main restart"]
                podmanExec_ cid ["sudo", "-u", "postgres", "psql", "-X", "-c", "GRANT CREATE ON SCHEMA public TO scripted"]
                let script = tmp </> "001.sql"
                    conn = Postgres.ConnString (Postgres.Server "127.0.0.1" 15443) (Postgres.User "scripted") (tmp </> "scripted.pass") (Postgres.Database "postgres")
                writeFile script "CREATE TABLE scripted_was_here (x int);\n"
                runUp (Postgres.userScript silent ignoreTrack ignoreTrack conn (PreExisting script)) >>= assertBool "pg-script logs in with the file's password, newline and all"
                (code, out, err) <- podmanExecCapture cid ["sudo", "-u", "postgres", "psql", "-tAX", "-c", "SELECT count(*) FROM pg_tables WHERE tablename = 'scripted_was_here'"]
                assertEqual err ExitSuccess code
                assertEqual "the script ran" "1" (filter (/= '\n') out)
                writeFile (tmp </> "wrong.pass") "not-the-password\n"
                runUp (Postgres.userScript silent ignoreTrack ignoreTrack conn{Postgres.connstring_user_pass = tmp </> "wrong.pass"} (PreExisting script))
                    >>= assertBool "a wrong password fails up" . not
                (_, logs, _) <- podmanExecCapture cid ["bash", "-c", "cat /var/log/postgresql/*.log"]
                assertBool "no password in the log" (not ("scripted-password" `isInfixOfS` logs))
