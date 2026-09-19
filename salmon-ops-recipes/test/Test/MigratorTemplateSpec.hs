{-# LANGUAGE OverloadedStrings #-}

{- | Layer 3 for @salmon-migrator config template@: the real binary, on a real
Postgres, in a VM.

"Test.PostgresTemplateSpec" already covers the nodes against a container,
with a hand-written build. This covers what only the shipped binary can: that
the migrator's own graph -- cluster, owner role and password file, admin
migrations, owner migrations over TCP -- runs as the template's nested build,
and that what comes out is a template anybody can clone with plain SQL.

What the test drives, all inside the guest:

1. @config template … | run up@ from two migration files;
2. checks the result is locked, stamped, and refuses a connection;
3. runs it again and checks the template was __not__ rebuilt (same oid);
4. clones it with @CREATE DATABASE … TEMPLATE@ and looks inside: the row the
   owner migration wrote, the extension the superuser migration created, and
   the owner role still owning the table -- roles are cluster-wide, so a
   clone inherits the template's ownership rather than its creator's;
5. changes a migration, runs again, and checks the template __was__ rebuilt
   (new oid), a new clone sees the change, and the old clone does not.

= Prerequisites

Everything "Test.PgBackupSpec" needs except rsync (the binary goes over
@scp@), and a rootfs of its own for the reason given there -- two VMs on one
9p rootfs corrupt it:

> sudo mkdir -p /var/lib/salmon-test-vms/pg-template/root
> sudo rsync -aHAX --numeric-ids /var/lib/salmon-test-vms/pg-master/root/ /var/lib/salmon-test-vms/pg-template/root/
> sudo $(cabal list-bin salmon-qemu-host-setup-fixture) "$USER" /var/lib/salmon-test-vms/pg-template/root

and @cabal build salmon-migrator@ first. The guest has no route out, so every
package the migrator's graph installs must already be in the rootfs;
@pg-master@'s has them all (postgresql, postgresql-client, postgresql-common,
openssl).
-}
module Test.MigratorTemplateSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as Text
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Test.Harness (VmAccess (..), hasVmPrivileges, quoteForRemoteShell, scpToVm, sshToVm, testVmAddr4, withTempDir, withVmAt)

tests :: TestTree
tests =
    testGroup
        "salmon-migrator config template (Layer 3, a real database in a VM)"
        [ testCase "builds, skips, clones with plain SQL, and rebuilds on a changed migration" buildsAndRebuilds
        ]

-- | This spec's __own__ rootfs; see the module header.
rootfs :: FilePath
rootfs = "/var/lib/salmon-test-vms/pg-template/root"

templateDb, ownerRole, workDir :: String
templateDb = "fixture_tpl"
ownerRole = "fixture_owner"
workDir = "/root/tpl"

canaryRow :: String
canaryRow = "salmon-template-canary"

-------------------------------------------------------------------------------

buildsAndRebuilds :: IO ()
buildsAndRebuilds = requirePrereqs $ \binary ->
    withVmAt testVmAddr4 rootfs $ \vm -> withTempDir $ \tmp -> do
        waitForPostgres vm
        resetGuest vm

        run_ vm ["mkdir", "-p", workDir </> "migrations/superuser", workDir </> "migrations/owner"]
        scpToVm vm binary "/root/salmon-migrator"

        -- CREATE EXTENSION needs a superuser, so finding it in a clone shows
        -- the admin migrations went into the template too, not only the
        -- owner's.
        let superuser = "CREATE EXTENSION IF NOT EXISTS pgcrypto;\n"
            ownerV1 =
                unlines
                    [ "CREATE TABLE IF NOT EXISTS fixture (v text);"
                    , "INSERT INTO fixture VALUES ('" <> canaryRow <> "');"
                    ]
            ownerV2 = ownerV1 <> "CREATE TABLE IF NOT EXISTS fixture_two (v text);\n"
        upload vm tmp "superuser.sql" superuser (workDir </> "migrations/superuser/tip.sql")
        upload vm tmp "owner.sql" ownerV1 (workDir </> "migrations/owner/tip.sql")

        -- 1. build
        migrate vm
        (sql vm "postgres" (catalogQuery "datistemplate::text || '|' || datallowconn::text") >>=) $
            assertEqual "the template is locked" "true|false"
        comment <- sql vm "postgres" (catalogQuery "shobj_description(oid, 'pg_database')")
        assertBool ("the template is stamped with its inputs: " <> comment) ("salmon-template:" `isPrefixOf` comment)
        (code, _, err) <- sshToVm vm ["sudo", "-u", "postgres", "psql", "-d", templateDb, "-tAc", quoteForRemoteShell "SELECT 1"]
        assertBool "a locked template refuses a connection" (code /= ExitSuccess)
        assertBool ("and says why: " <> err) ("not currently accepting connections" `isInfixOf` err)

        -- 2. unchanged inputs are not rebuilt
        built <- templateOid vm
        migrate vm
        (templateOid vm >>=) $ assertEqual "unchanged migrations leave the template as it was" built

        -- 3. clone with plain SQL, as any consumer would
        run_ vm ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell ("CREATE DATABASE fixture_copy TEMPLATE " <> templateDb)]
        (sql vm "fixture_copy" "SELECT v FROM fixture" >>=) $
            assertEqual "the owner migration's row is in the copy" canaryRow
        (sql vm "fixture_copy" "SELECT extname FROM pg_extension WHERE extname = 'pgcrypto'" >>=) $
            assertEqual "the superuser migration's extension is in the copy" "pgcrypto"
        (sql vm "fixture_copy" "SELECT tableowner FROM pg_tables WHERE tablename = 'fixture'" >>=) $
            assertEqual "the copy keeps the template's owner, not its creator" ownerRole

        -- 4. a changed migration rebuilds
        upload vm tmp "owner.sql" ownerV2 (workDir </> "migrations/owner/tip.sql")
        migrate vm
        rebuilt <- templateOid vm
        assertBool "a changed migration rebuilds the template" (rebuilt /= built)
        run_ vm ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell ("CREATE DATABASE fixture_copy_two TEMPLATE " <> templateDb)]
        (sql vm "fixture_copy_two" "SELECT count(*) FROM pg_tables WHERE tablename = 'fixture_two'" >>=) $
            assertEqual "a new copy sees the change" "1"
        (sql vm "fixture_copy_two" "SELECT count(*) FROM fixture" >>=) $
            assertEqual "rebuilt from nothing: the row is written once, not twice" "1"
        (sql vm "fixture_copy" "SELECT count(*) FROM pg_tables WHERE tablename = 'fixture_two'" >>=) $
            assertEqual "an existing copy was taken once, and does not" "0"
  where
    catalogQuery :: String -> String
    catalogQuery col = "SELECT " <> col <> " FROM pg_database WHERE datname = '" <> templateDb <> "'"

    templateOid vm = sql vm "postgres" (catalogQuery "oid::text")

{- | @config template … | run up@, run inside the guest from its work
directory, so the migration paths in the directive are the guest's.
-}
migrate :: VmAccess -> IO ()
migrate vm =
    run_
        vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell $
            unwords
                [ "set -eo pipefail;"
                , "cd " <> workDir <> ";"
                , "/root/salmon-migrator config template"
                , "--superuser-root migrations/superuser"
                , "--owner-root migrations/owner"
                , "--db " <> templateDb
                , "--db-owner " <> ownerRole
                , "--db-passfile " <> workDir <> "/owner.pass"
                , "> directive.json;"
                , "/root/salmon-migrator run up < directive.json"
                ]
        ]

upload :: VmAccess -> FilePath -> FilePath -> String -> FilePath -> IO ()
upload vm tmp name contents remote = do
    writeFile (tmp </> name) contents
    scpToVm vm (tmp </> name) remote

-- | One value, as the @postgres@ OS user.
sql :: VmAccess -> String -> String -> IO String
sql vm db query = do
    (code, out, err) <- sshToVm vm ["sudo", "-u", "postgres", "psql", "-X", "-d", db, "-tAc", quoteForRemoteShell query]
    unless (code == ExitSuccess) $
        assertBool ("query failed on " <> db <> ": " <> query <> "\n" <> err) False
    pure (filter (/= '\n') out)

run_ :: VmAccess -> [String] -> IO ()
run_ vm args = do
    (code, out, err) <- sshToVm vm args
    unless (code == ExitSuccess) $
        assertBool ("command failed in the guest: " <> show args <> "\n--- stdout ---\n" <> lastLines out <> "\n--- stderr ---\n" <> err) False
  where
    -- `run up` reports every node; the failure is at the end
    lastLines = unlines . reverse . take 40 . reverse . lines

{- | Undoes whatever a previous run left in the rootfs, which persists (9p).

The role goes too, not only the databases: the migrator creates it with the
password in the pass file, and never changes the password of a role that
already exists, so a fresh pass file against a surviving role fails the
owner migrations on authentication.
-}
resetGuest :: VmAccess -> IO ()
resetGuest vm = do
    let psql q = run_ vm ["sudo", "-u", "postgres", "psql", "-X", "-tAc", quoteForRemoteShell q]
    psql "DROP DATABASE IF EXISTS fixture_copy WITH (FORCE)"
    psql "DROP DATABASE IF EXISTS fixture_copy_two WITH (FORCE)"
    -- a DO block rather than \\gexec: psql -c will not mix SQL with a meta-command
    psql ("DO $$ BEGIN IF EXISTS (SELECT FROM pg_database WHERE datname = '" <> templateDb <> "') THEN ALTER DATABASE " <> templateDb <> " IS_TEMPLATE false; END IF; END $$")
    psql ("DROP DATABASE IF EXISTS " <> templateDb <> " WITH (FORCE)")
    psql ("DROP ROLE IF EXISTS " <> ownerRole)
    run_ vm ["rm", "-rf", workDir]

-- | As in "Test.PgBackupSpec": sshd answering says nothing about Postgres.
waitForPostgres :: VmAccess -> IO ()
waitForPostgres vm = go (30 :: Int)
  where
    go 0 = fail "postgres never accepted a connection in the VM"
    go n = do
        (code, _, _) <- sshToVm vm ["sudo", "-u", "postgres", "psql", "-tAc", quoteForRemoteShell "SELECT 1"]
        if code == ExitSuccess
            then pure ()
            else threadDelay 2000000 >> go (n - 1)

-------------------------------------------------------------------------------

requirePrereqs :: (FilePath -> IO ()) -> IO ()
requirePrereqs act = do
    privileged <- hasVmPrivileges
    qemu <- findExecutable "qemu-system-x86_64"
    scp <- findExecutable "scp"
    rootfsOk <- doesFileExist (rootfs </> "etc/issue")
    case () of
        _
            | not privileged -> skip "no VM privileges (run salmon-qemu-host-setup-fixture, or use sudo)"
            | Nothing <- qemu -> skip "qemu-system-x86_64 not on PATH"
            | Nothing <- scp -> skip "scp not on PATH on the host"
            | not rootfsOk ->
                skip
                    ( "no rootfs at "
                        <> rootfs
                        <> ". It must be this spec's own (two VMs on one 9p rootfs corrupt it). Build it with:\n"
                        <> "  sudo mkdir -p "
                        <> rootfs
                        <> "\n  sudo rsync -aHAX --numeric-ids /var/lib/salmon-test-vms/pg-master/root/ "
                        <> rootfs
                        <> "/\n  sudo $(cabal list-bin salmon-qemu-host-setup-fixture) \"$USER\" "
                        <> rootfs
                    )
            | otherwise -> resolveBinary >>= act

skip :: String -> IO ()
skip why = putStrLn ("SKIPPED: " <> why)

-- | @cabal list-bin@, last non-blank line: under sudo, cabal prints notices first.
resolveBinary :: IO FilePath
resolveBinary = do
    (code, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "salmon-migrator"] ""
    case (code, reverse (filter (not . null) (lines out))) of
        (ExitSuccess, path : _) -> pure path
        _ -> error ("could not locate salmon-migrator (build it first): " <> err)
