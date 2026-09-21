{-# LANGUAGE OverloadedStrings #-}

{- | Layer 3, and the demo: three machines, a client that never stops
writing, and a primary that moves twice underneath it.

This is scenario S1 of @specs\/pg-switchover.md@ with the half that was
missing until pgbouncer was wired up -- /the client saw zero errors/ -- and
it drives the @salmon-pgpair@ binary rather than the library, because the
claim being made is about what an operator types:

> salmon-pgpair config --primary A ... | salmon-pgpair run up
> salmon-pgpair config --primary B ... | salmon-pgpair run up

Nothing else changes between those two lines. What the pair does about them
-- stop the old primary cleanly, promote the new one, rewind the old one
onto it, and hold the clients for as long as that takes -- is the recipe's
business, and the client's only evidence of it is a pause.

The machines are the two Postgres rootfses the other specs use, plus a third
with pgbouncer on it:

> sudo debootstrap --include=linux-image-amd64,openssh-server,pgbouncer,postgresql-client stable /var/lib/salmon-test-vms/pg-bouncer/root

followed by 'Salmon.Builtin.Nodes.Debian.Debootstrap.ensureVm9pBoot' on it,
same as the others.

What the test does /not/ do for you is provision secrets, because the recipe
does not either: the @.pgpass@ files and pgbouncer's @userlist.txt@ are put
in place here the way a deployment would put them there, and salmon is
handed paths.
-}
module Test.PgPairDemoSpec (tests) where

import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Harness
import Test.PostgresVms
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
    testGroup
        "salmon-pgpair (Layer 3, a primary moved under a live client)"
        [ testCase "a client writing through pgbouncer sees a pause, not an error" clientKeepsWriting
        ]

bouncerRootfs :: FilePath
bouncerRootfs = "/var/lib/salmon-test-vms/pg-bouncer/root"

appPassword, consolePassword :: String
appPassword = "demo-app-password"
consolePassword = "demo-console-password"

clientKeepsWriting :: IO ()
clientKeepsWriting = requirePgVmPrereqs $ requireBouncerRootfs $ do
    binary <- resolvePgPairBinary
    withVmAt testVmAddr primaryRootfs $ \a ->
        withVmAt testVmAddr2 standbyRootfs $ \b ->
            withVmAt testVmAddr3 bouncerRootfs $ \bouncer -> do
                -- what a deployment provisions, and salmon is only given paths to
                mapM_ provisionMemberSecrets [a, b]
                provisionBouncerSecrets bouncer

                -- a machine with a database on it, and a machine with nothing
                ensurePrimary a
                resetCluster b
                psqlOrDie a "DROP DATABASE IF EXISTS app;"
                psqlOrDie a "CREATE DATABASE app;"
                psqlOrDie a "DO $$ BEGIN CREATE ROLE app LOGIN PASSWORD 'demo-app-password'; EXCEPTION WHEN duplicate_object THEN NULL; END $$;"
                sshOrDie a ["bash", "-c", quoteForRemoteShell ("sudo -u postgres psql -d app -c " <> quoteForRemoteShell "CREATE TABLE IF NOT EXISTS canary (n int primary key); GRANT ALL ON canary TO app;")]
                appendHba a "host all app 10.99.0.4/32 md5"
                appendHba b "host all app 10.99.0.4/32 md5"

                -- one command stands the whole thing up
                runPgPair binary (a, b, bouncer) ["--primary", "A", "--seed", "B"]
                assertPrimaryIs a
                assertStandbyOf b testVmAddr

                startClient bouncer
                waitForClientProgress bouncer 10

                -- and one word of it moves the primary, twice
                runPgPair binary (a, b, bouncer) ["--primary", "B"]
                assertPrimaryIs b
                waitForClientProgress bouncer 10

                runPgPair binary (a, b, bouncer) ["--primary", "A"]
                assertPrimaryIs a
                waitForClientProgress bouncer 10

                (acknowledged, failures, stderrs) <- stopClient bouncer
                assertBool
                    ( "the client saw "
                        <> show (length failures)
                        <> " failed inserts (of "
                        <> show (length acknowledged + length failures)
                        <> "), the first few being "
                        <> show (take 5 failures)
                        <> "; its stderr ended with:\n"
                        <> unlines (take 8 (reverse (lines stderrs)))
                    )
                    (null failures)
                assertBool "the client never got anywhere" (length acknowledged > 20)

                -- every insert the client was told had happened, did
                missing <- rowsMissing a acknowledged
                assertEqual
                    ("acknowledged but absent after the switchovers: " <> show (take 20 missing))
                    []
                    missing

                -- the demo's whole point, in three numbers
                putStrLn ""
                putStrLn ("  inserts acknowledged through the bouncer: " <> show (length acknowledged))
                putStrLn ("  client errors across two switchovers:     " <> show (length failures))
                putStrLn ("  acknowledged rows missing afterwards:     " <> show (length missing))

-------------------------------------------------------------------------------

{- | Runs the binary the way the demo does: a seed on one side of a pipe, a
directive on the other.
-}
runPgPair :: FilePath -> (VmAccess, VmAccess, VmAccess) -> [String] -> IO ()
runPgPair binary (a, b, bouncer) args = do
    let common =
            [ "--a"
            , Text.unpack testVmAddr
            , "--b"
            , Text.unpack testVmAddr2
            , "--bouncer"
            , Text.unpack testVmAddr3
            , -- one key per guest, because this harness mints a CA per guest;
              -- a deployment passes --ssh-identity once
              "--ssh-identity-a"
            , vmIdentityFile a
            , "--ssh-identity-b"
            , vmIdentityFile b
            , "--ssh-identity-bouncer"
            , vmIdentityFile bouncer
            , "--ssh-known-hosts"
            , "/dev/null"
            ]
    (code, directive, err) <- readProcessWithExitCode binary ("config" : args <> common) ""
    unless (code == ExitSuccess) (fail ("salmon-pgpair config failed: " <> err))
    (upCode, out, upErr) <- readProcessWithExitCode binary ["run", "up"] directive
    unless (upCode == ExitSuccess) (fail ("salmon-pgpair run up " <> unwords args <> " failed:\n" <> out <> upErr))

resolvePgPairBinary :: IO FilePath
resolvePgPairBinary = do
    (code, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "salmon-pgpair"] ""
    case (code, filter (not . null) (lines out)) of
        (ExitSuccess, ls@(_ : _)) -> pure (last ls)
        _ -> fail ("could not find salmon-pgpair; build it first\n" <> err)

{- | Skips loudly rather than failing, and distinguishes the two ways this
rootfs is not ready -- because the second one fails a long way from its
cause: a guest whose initrd cannot mount a 9p root panics at boot, and what
the test sees is an ssh that never connects.
-}
requireBouncerRootfs :: IO () -> IO ()
requireBouncerRootfs act = do
    there <- fileExists (bouncerRootfs <> "/etc/issue")
    bootable <- grepQuiet "9pnet_virtio" (bouncerRootfs <> "/etc/initramfs-tools/modules")
    -- the harness signs a CA into the guest's sshd config before boot, and
    -- that write happens on the host as whoever runs the tests
    writableSsh <- writable (bouncerRootfs <> "/etc/ssh")
    case () of
        _
            | not there ->
                skip ("no VM rootfs at " <> bouncerRootfs <> " (see this module's haddock for the debootstrap)")
            | not writableSsh ->
                skip
                    ( bouncerRootfs
                        <> "/etc/ssh is not writable: the harness puts its SSH CA there before the guest boots."
                        <> " chown it to whoever runs the tests, as the other rootfses have it"
                    )
            | not bootable ->
                skip
                    ( bouncerRootfs
                        <> " cannot boot its root over 9p: run Salmon.Builtin.Nodes.Debian.Debootstrap.ensureVm9pBoot"
                        <> " on it, as the other rootfses have had"
                    )
            | otherwise -> act
  where
    skip msg = putStrLn ("SKIPPED: " <> msg)
    fileExists path = do
        (code, _, _) <- readProcessWithExitCode "test" ["-e", path] ""
        pure (code == ExitSuccess)
    writable path = do
        (code, _, _) <- readProcessWithExitCode "test" ["-w", path] ""
        pure (code == ExitSuccess)
    grepQuiet needle path = do
        (code, _, _) <- readProcessWithExitCode "grep" ["-q", needle, path] ""
        pure (code == ExitSuccess)

-------------------------------------------------------------------------------
-- What a deployment provisions, and this recipe never ships.

provisionMemberSecrets :: VmAccess -> IO ()
provisionMemberSecrets vm =
    forM_
        [ ("/etc/postgresql/salmon-replication.pgpass", "replicator", "fixture-replication-password")
        , ("/etc/postgresql/salmon-rewind.pgpass", "rewinder", "fixture-rewind-password")
        ]
        $ \(path, role, pwd) ->
            sshOrDie
                vm
                [ "bash"
                , "-c"
                , quoteForRemoteShell . unwords $
                    [ "set -e;"
                    , "printf '*:*:*:" <> role <> ":" <> pwd <> "\\n' > " <> path <> ";"
                    , "chown postgres:postgres " <> path <> ";"
                    , "chmod 0600 " <> path
                    ]
                ]

{- | The bouncer's own secrets: the console password the pair uses to pause
it, and the userlist both it and the client authenticate against.
-}
provisionBouncerSecrets :: VmAccess -> IO ()
provisionBouncerSecrets vm =
    sshOrDie
        vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell . unlines $
            [ "set -e"
            , "mkdir -p /etc/pgbouncer"
            , "md5() { printf 'md5%s' \"$(printf '%s%s' \"$2\" \"$1\" | md5sum | cut -d' ' -f1)\"; }"
            , "{"
            , "  printf '\"router\" \"%s\"\\n' \"$(md5 router " <> consolePassword <> ")\""
            , "  printf '\"app\" \"%s\"\\n' \"$(md5 app " <> appPassword <> ")\""
            , "} > /etc/pgbouncer/userlist.txt"
            , "printf '*:*:*:router:" <> consolePassword <> "\\n' > /etc/pgbouncer/console.pgpass"
            , "printf '*:*:*:app:" <> appPassword <> "\\n' > /root/app.pgpass"
            , "chmod 0600 /etc/pgbouncer/console.pgpass /root/app.pgpass"
            , "chown -R postgres:postgres /etc/pgbouncer"
            , "chmod 0644 /etc/pgbouncer/userlist.txt"
            ]
        ]

appendHba :: VmAccess -> String -> IO ()
appendHba vm line =
    sshOrDie
        vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell . unwords $
            [ "set -e;"
            , "version=$(pg_lsclusters --no-header | awk '{print $1}' | head -n1);"
            , "hba=/etc/postgresql/$version/main/pg_hba.conf;"
            , "grep -qxF '" <> line <> "' \"$hba\" || echo '" <> line <> "' >> \"$hba\";"
            , "pg_ctlcluster \"$version\" main reload"
            ]
        ]

-------------------------------------------------------------------------------
-- The client: one insert at a time, through the bouncer, recording what it
-- was told had happened.

startClient :: VmAccess -> IO ()
startClient vm = do
    sshOrDie
        vm
        [ "bash"
        , "-c"
        , quoteForRemoteShell . unlines $
            [ "set -e"
            , "rm -f /root/client.ok /root/client.err /root/client.stop"
            , "cat > /root/client.sh <<'CLIENT'"
            , "#!/bin/bash"
            , "n=0"
            , "export PGPASSFILE=/root/app.pgpass"
            , -- Debian's psql is a perl wrapper, and perl complains to
              -- stderr about every locale it cannot find. Left alone it
              -- writes two lines of noise per insert into the file this test
              -- reads for evidence.
              "export LANG=C LC_ALL=C"
            , "while [ ! -e /root/client.stop ]; do"
            , "  n=$((n+1))"
            , "  if psql -h 127.0.0.1 -p 6432 -U app -d app -v ON_ERROR_STOP=1 -tAXc \"INSERT INTO canary VALUES ($n)\" >/dev/null 2>>/root/client.err; then"
            , "    echo \"$n\" >> /root/client.ok"
            , "  else"
            , "    echo \"$n\" >> /root/client.fail"
            , "  fi"
            , "  sleep 0.2"
            , "done"
            , "CLIENT"
            , "chmod +x /root/client.sh"
            , "setsid /root/client.sh >/dev/null 2>&1 </dev/null &"
            ]
        ]

-- | Waits until the client has had at least @n@ more inserts acknowledged.
waitForClientProgress :: VmAccess -> Int -> IO ()
waitForClientProgress vm n = do
    before <- countOk vm
    waitForUpTo 60 ("the client stopped making progress past " <> show before) $ do
        now <- countOk vm
        pure (now >= before + n, show now <> " acknowledged")

countOk :: VmAccess -> IO Int
countOk vm = do
    (_, out, _) <- sshToVm vm ["bash", "-c", quoteForRemoteShell "wc -l < /root/client.ok 2>/dev/null || echo 0"]
    pure (maybe 0 fst (listToMaybe (reads (takeWhile (/= '\n') out))))
  where
    listToMaybe [] = Nothing
    listToMaybe (x : _) = Just x

{- | Stops it, and says what it was told: the inserts acknowledged, the ones
that failed, and whatever the client wrote to stderr.

A failure is an insert that came back non-zero, not a line on stderr --
Debian's psql is a perl wrapper that warns there about locales, which says
nothing about whether the write happened.
-}
stopClient :: VmAccess -> IO ([String], [String], String)
stopClient vm = do
    sshOrDie vm ["bash", "-c", quoteForRemoteShell "touch /root/client.stop; sleep 2"]
    (_, ok, _) <- sshToVm vm ["bash", "-c", quoteForRemoteShell "cat /root/client.ok 2>/dev/null"]
    (_, failed, _) <- sshToVm vm ["bash", "-c", quoteForRemoteShell "cat /root/client.fail 2>/dev/null"]
    (_, errs, _) <- sshToVm vm ["bash", "-c", quoteForRemoteShell "cat /root/client.err 2>/dev/null"]
    pure (lines ok, filter (not . null) (lines failed), errs)

-- | Of the inserts the client was told had happened, which are not there.
rowsMissing :: VmAccess -> [String] -> IO [String]
rowsMissing vm acknowledged = do
    (code, out, err) <- sshToVm vm ["bash", "-c", quoteForRemoteShell "sudo -u postgres psql -d app -tAXc 'SELECT n FROM canary ORDER BY n'"]
    unless (code == ExitSuccess) (fail ("could not read the canary table: " <> out <> err))
    let present = words out
    pure [n | n <- acknowledged, n `notElem` present]
