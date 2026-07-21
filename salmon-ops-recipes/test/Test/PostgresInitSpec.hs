-- | Layer 2: run the real, unmodified 'SreBox.PostgresInit.setupNakedPG'
-- recipe against a disposable Debian container, instead of only checking
-- its graph shape.
--
-- The recipe shells out directly to \"apt-get\", \"sudo\", \"pg_ctlcluster\"
-- by name (there is no indirection to hook into), so the only way to run
-- its *real* logic against a sandbox is 'Test.Harness.withShimmedPath':
-- lookalike scripts earlier on PATH that forward each invocation into the
-- container via @podman exec@. Nothing in the recipe is touched or aware of
-- this — it is exercising unmodified production code.
--
-- IMPORTANT (historical): this test is what originally caught a real bug —
-- 'Postgres.pgLocalCluster' used to hardcode @hardcodedVersion = 12@, which
-- silently failed to start on Debian bookworm (which ships postgresql 15 by
-- default): @pg_ctlcluster 12 main start@ exited 1 and nobody noticed,
-- because at the time 'Salmon.Actions.UpDown.upTree' never inspected a
-- failing subprocess's exit code (@Binary.untrackedExec@ recorded the
-- 'ExitCode' into a 'Report' but never threw on non-zero), so a Haskell
-- exception from 'runUp' not being thrown was NOT proof the recipe worked.
-- 'Postgres.pgLocalCluster' now detects the installed cluster version at
-- 'up' time instead of hardcoding it, and separately, 'untrackedExec' now
-- throws on a non-zero exit and 'upTree'/'runUp' surface that as a real
-- @False@ return instead of silently continuing — so the explicit
-- postcondition check below is now belt-and-suspenders rather than the only
-- thing standing between this test and a false green. Both are kept: the
-- 'runUp' result proves the graph traversal itself didn't skip/fail
-- anything, the postcondition proves the *specific* effect we care about
-- actually happened.
module Test.PostgresInitSpec (tests) where

import Data.List (isInfixOf)
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Reporter (silent)
import qualified SreBox.PostgresInit as PostgresInit
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "SreBox.PostgresInit (Layer 2, dogfooded via a podman sandbox)"
        [ testCase "setupNakedPG against a fresh debian:bookworm container" setupNakedPGAgainstSandbox
        ]

-- | These are exactly the binaries 'PostgresInit.setupNakedPG' invokes by
-- name along its dependency chain: apt-get to install postgresql, sudo to
-- run psql as the postgres user, and bash (which in turn resolves
-- pg_ctlcluster/pg_lsclusters *inside* the container, so those two don't
-- need their own shims) to detect and start the cluster.
shimmedCommands :: [String]
shimmedCommands = ["apt-get", "sudo", "bash", "chmod"]

setupNakedPGAgainstSandbox :: IO ()
setupNakedPGAgainstSandbox = requireExecutable "podman" $
    withContainer (Podman.Image "debian:bookworm") (Podman.PortMapping "15432" "5432" Podman.TCPPort) $ \cid -> do
        -- sandbox prep, not part of the recipe under test: a fresh base
        -- image has no apt cache and no sudo, both of which a real target
        -- server is assumed to already have.
        podmanExec_ cid ["apt-get", "update", "-qq"]
        podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo"]

        withShimmedPath cid shimmedCommands $ do
            let op = PostgresInit.setupNakedPG silent "appdb"
            -- this is the real recipe, unmodified, run exactly like
            -- production code would via upTree — only the binaries it
            -- shells out to have been redirected into the container.
            ok <- runUp op
            assertBool "expected the recipe's graph traversal to fully succeed (no failed/blocked node)" ok

        -- postcondition: did "appdb" actually get created for real?
        (code, out, err) <- podmanExecCapture cid ["sudo", "-u", "postgres", "psql", "-lqt"]
        assertBool
            ("expected \"appdb\" to be a real database inside the container; `psql -l` said ("
                <> show code
                <> "):\nstdout:\n"
                <> out
                <> "\nstderr:\n"
                <> err
            )
            ("appdb" `isInfixOf` out)
