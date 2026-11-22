{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for the plain-function face of
"Salmon.Builtin.Nodes.Gcp.SshAccess": the probe argv, the retry loop driven
by a scripted probe (no ssh), and one live 'waitSshReachable' against a
closed loopback port (needs @ssh@ on PATH, skipped loudly otherwise; no
network beyond 127.0.0.1).
-}
module Test.SshAccessProbeSpec (tests) where

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Builtin.Nodes.Gcp.SshAccess
import qualified Salmon.Builtin.Nodes.Ssh as Ssh

import Test.Harness (freeTcpPort, requireExecutable)

tests :: TestTree
tests =
    testGroup
        "Gcp.SshAccess probe"
        [ testCase "sshProbeArgs: the argv the node has always run" $
            assertEqual
                "argv"
                [ "-o"
                , "ConnectTimeout=5"
                , "-o"
                , "BatchMode=yes"
                , "-i"
                , "/k/id"
                , "-o"
                , "IdentitiesOnly=yes"
                , "-o"
                , "UserKnownHostsFile=/k/hosts"
                , "-o"
                , "StrictHostKeyChecking=accept-new"
                , "-p"
                , "2222"
                , "root@203.0.113.7"
                , "true"
                ]
                (sshProbeArgs endpointWithOpts)
        , testCase "sshProbeArgs: no user, no options" $
            assertEqual
                "argv"
                ["-o", "ConnectTimeout=5", "-o", "BatchMode=yes", "-p", "22", "203.0.113.7", "true"]
                (sshProbeArgs (SshEndpoint Nothing "203.0.113.7" 22 Ssh.noClientOpts))
        , testCase "waitReachableWith: succeeds on the attempt that answers" $ do
            (probe, calls) <- scripted [Left "h: refused", Left "h: refused", Right ()]
            r <- waitReachableWith (ProbePolicy 5 0) "h" Nothing probe
            assertBool "reachable" (either (const False) (const True) r)
            calls >>= assertEqual "attempts" 3
        , testCase "waitReachableWith: out of attempts names host, count, last error" $ do
            (probe, calls) <- scripted [Left "h: one", Left "h: two"]
            r <- waitReachableWith (ProbePolicy 2 0) "h" Nothing probe
            case r of
                Right () -> assertFailure "expected unreachable"
                Left u -> do
                    assertEqual "host" "h" u.unreachableHost
                    assertEqual "attempts" 2 u.unreachableAttempts
                    assertEqual "last error" "h: two" u.unreachableLastError
            calls >>= assertEqual "probes" 2
        , testCase "waitReachableWith: zero attempts probes nothing" $ do
            (probe, calls) <- scripted []
            r <- waitReachableWith (ProbePolicy 0 0) "h" Nothing probe
            assertBool "unreachable" (either (const True) (const False) r)
            calls >>= assertEqual "probes" 0
        , testCase "waitReachableWith: a host key mismatch runs the forget action and retries" $ do
            (probe, _) <- scripted [Left "h: Host key verification failed.", Right ()]
            forgot <- newIORef (0 :: Int)
            r <- waitReachableWith (ProbePolicy 3 0) "h" (Just (atomicModifyIORef' forgot (\n -> (n + 1, ())))) probe
            assertBool "reachable" (either (const False) (const True) r)
            readIORef forgot >>= assertEqual "forgot once" 1
        , testCase "waitSshReachable: a closed loopback port is Left with ssh's reason" $
            requireExecutable "ssh" $ do
                port <- read . Text.unpack <$> freeTcpPort
                r <- waitSshReachable (ProbePolicy 1 0) (SshEndpoint Nothing "127.0.0.1" port Ssh.noClientOpts)
                case r of
                    Right () -> assertFailure "a closed port answered"
                    Left u -> do
                        assertEqual "attempts" 1 u.unreachableAttempts
                        assertBool
                            ("reason names the host: " <> show u.unreachableLastError)
                            ("127.0.0.1: " `Text.isPrefixOf` u.unreachableLastError)
        ]
  where
    endpointWithOpts =
        SshEndpoint
            (Just "root")
            "203.0.113.7"
            2222
            Ssh.noClientOpts{Ssh.optIdentity = Just "/k/id", Ssh.optKnownHosts = Just "/k/hosts"}

-- | A probe answering from a script (then failing), and a count of calls.
scripted :: [Either Text ()] -> IO (IO (Either Text ()), IO Int)
scripted answers = do
    ref <- newIORef (answers, 0 :: Int)
    let probe = atomicModifyIORef' ref $ \(as, n) -> case as of
            a : rest -> ((rest, n + 1), a)
            [] -> (([], n + 1), Left "script exhausted")
    pure (probe, snd <$> readIORef ref)
