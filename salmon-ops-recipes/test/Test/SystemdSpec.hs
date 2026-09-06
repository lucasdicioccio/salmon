{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Systemd"'s @check@.

'Systemd.checkService' is a @systemctl show@ away from being untestable
without a systemd, so the decision it draws from that output is split out as
'Systemd.interpretShow' and asserted here. What is /not/ here is that
@systemctl@ prints what these cases assume — the Layer 3 tiers
(@Test.QemuSmokeSpec@, @Test.PostgresReplicationSpec@) run real units on a
real init and are what actually exercise the shelling-out.

The interesting cases are the two that are not "is it running": a unit whose
file changed since systemd loaded it, and a unit part-way through a
transition. Both were what made this node worth giving a check at all.
-}
module Test.SystemdSpec (tests) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Builtin.Nodes.Systemd as Systemd

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Systemd"
        [ testCase "a running, enabled, loaded unit is satisfied" runningIsSuccess
        , testCase "a stopped or failed unit needs bringing up" stoppedIsFailure
        , testCase "a unit whose file changed needs bringing up, running or not" staleFileIsFailure
        , testCase "a unit in transition is Unknown, not Failure" transitionIsUnknown
        , testCase "a running but disabled unit is not satisfied" disabledIsFailure
        , testCase "a unit systemd has never heard of is not satisfied" unknownUnitIsFailure
        , testCase "properties are read by name, not by position" orderIndependent
        ]

shown :: [Text] -> CheckResult
shown = Systemd.interpretShow

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

runningIsSuccess :: IO ()
runningIsSuccess =
    assertEqual
        "active, enabled and loaded as written"
        Success
        (shown ["ActiveState=active", "UnitFileState=enabled", "NeedDaemonReload=no"])

stoppedIsFailure :: IO ()
stoppedIsFailure = do
    assertBool "inactive" (isFailure (shown ["ActiveState=inactive", "UnitFileState=enabled", "NeedDaemonReload=no"]))
    assertBool "failed" (isFailure (shown ["ActiveState=failed", "UnitFileState=enabled", "NeedDaemonReload=no"]))

{- | The case that keeps @run up@ meaning what it used to mean. This node's
own dependency rewrites the unit file before the check ever runs, so nothing
on disk can still say the running service is stale — only systemd's own
record of it.
-}
staleFileIsFailure :: IO ()
staleFileIsFailure =
    assertBool
        "running happily, against a unit file that is no longer the one on disk"
        (isFailure (shown ["ActiveState=active", "UnitFileState=enabled", "NeedDaemonReload=yes"]))

{- | A service part-way through starting has not gone away, and treating it
as gone is how a slow starter becomes a restart loop. 'Unknown' is the
verdict that makes "Salmon.Actions.Upkeep" wait and look again.
-}
transitionIsUnknown :: IO ()
transitionIsUnknown = do
    assertEqual "activating" Unknown (shown ["ActiveState=activating", "UnitFileState=enabled", "NeedDaemonReload=no"])
    assertEqual "deactivating" Unknown (shown ["ActiveState=deactivating", "UnitFileState=enabled", "NeedDaemonReload=no"])
    assertEqual "reloading" Unknown (shown ["ActiveState=reloading", "UnitFileState=enabled", "NeedDaemonReload=no"])

-- | Still running, so nothing is obviously wrong today; gone at the next
-- reboot, which is exactly the sort of drift nobody notices by hand.
disabledIsFailure :: IO ()
disabledIsFailure =
    assertBool
        "running but disabled"
        (isFailure (shown ["ActiveState=active", "UnitFileState=disabled", "NeedDaemonReload=no"]))

unknownUnitIsFailure :: IO ()
unknownUnitIsFailure = do
    -- what `systemctl show` says about a unit it has never heard of: a state,
    -- and no install state at all.
    assertBool "never heard of it" (isFailure (shown ["ActiveState=inactive", "NeedDaemonReload=no"]))
    assertBool "said nothing whatsoever" (isFailure (shown []))

orderIndependent :: IO ()
orderIndependent =
    assertEqual
        "the same three properties, whatever order systemd emits them in"
        Success
        (shown ["NeedDaemonReload=no", "UnitFileState=enabled", "ActiveState=active"])
