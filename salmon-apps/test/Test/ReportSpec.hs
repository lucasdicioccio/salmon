{-# LANGUAGE OverloadedStrings #-}

-- | Layer 0 coverage for @salmon-report@: parsers over captured output, and the
-- driver against fake probes. No network.
module Test.ReportSpec (tests) where

import Control.Concurrent (threadDelay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Data.Text as Text

import Report

fake :: Verdict -> Finding
fake v = Finding "q" v ["e"] "fake" Nothing []

tests :: TestTree
tests =
    testGroup
        "salmon-report"
        [ testCase "dig keeps addresses and drops CNAME targets" $
            assertEqual "" ["192.0.2.1", "2001:db8::1"] (parseDigAddresses "alias.example.org.\n192.0.2.1\n2001:db8::1\n")
        , testCase "natpmpc public address" $
            assertEqual "" (NatpmpPublic "203.0.113.4") (parseNatpmpc "initnatpmp() returned 0 (SUCCESS)\nPublic IP address : 203.0.113.4\n")
        , testCase "natpmpc without gateway" $
            assertEqual "" NatpmpNoGateway (parseNatpmpc "Cannot get default gateway ip address")
        , testCase "host:port" $ do
            assertEqual "" (Just ("example.org", 443)) (parseHostPort "example.org:443")
            assertEqual "" (Just ("::1", 80)) (parseHostPort "[::1]:80")
            assertEqual "" Nothing (parseHostPort "example.org")
        , testCase "driver: success, failure, throw and timeout each print one finding" $ do
            c <- newCollector
            let ok = probeOp c "ok" (pure (fake Yes))
                no = probeOp c "no" (pure (fake No))
                boom = probeOp c "boom" (ioError (userError "bang"))
                slow = probeOp c "slow" (threadDelay 5000000 >> pure (fake Yes))
            fs <- runReport 200000 c (reportOp [ok, no, boom, slow])
            assertEqual "verdicts" [Yes, No, Unknown, Unknown] (fVerdict <$> fs)
            assertEqual "timeout cause" True (any (any (Text.isPrefixOf "timed out") . fEvidence) fs)
        , testCase "a probe's up is never called by the report and throws if it were" $ do
            c <- newCollector
            fs <- runReport 200000 c (reportOp [probeOp c "ok" (pure (fake Yes))])
            assertEqual "" 1 (length fs)
        , testCase "cross-check marks a name pointing at the external address" $ do
            let ext = Finding "What is the external address, and can it be mapped to?" Yes [] "m" Nothing ["203.0.113.4"]
                dns = Finding "Does a.example resolve?" Yes ["x"] "m" Nothing ["203.0.113.4"]
            assertEqual "" ["x", "points at the external address 203.0.113.4"] (fEvidence (crossCheck [ext, dns] !! 1))
        ]
