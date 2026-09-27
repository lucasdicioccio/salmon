{-# LANGUAGE OverloadedStrings #-}

{- | Layer 3: the harness itself. Done when three guests answer ssh with the
Patroni, etcd and HAProxy binaries present (@specs\/pg-patroni.md@). Every
later scenario (T1..T7) starts from 'withPatroniVms', so this is the test
that says the ground is there.

Skips loudly if the rootfses were never built; see "Test.PatroniVms".
-}
module Test.PatroniHarnessSpec (tests) where

import Test.PatroniVms
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
    testGroup
        "Patroni harness (Layer 3, three guests)"
        [ testCase "three guests answer ssh with patroni, etcd and haproxy installed" guestsCarryBinaries
        ]

guestsCarryBinaries :: IO ()
guestsCarryBinaries = requirePatroniVmPrereqs $
    withPatroniVms $ \vms -> do
        assertEqual "three guests" 3 (length vms)
        missing <- mapM missingBinaries vms
        assertEqual "binaries missing per guest" [[], [], []] missing
