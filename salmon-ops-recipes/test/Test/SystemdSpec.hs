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
import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Harness (withTempDir)

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
        , testGroup "watched config files" watchedTests
        ]

{- | A service's config file leaves no trace in anything systemd knows, so
'Systemd.systemdServiceWatching' folds it into the unit file, where
@NeedDaemonReload@ already notices changes. These are assertions about that
fold: same bytes, same unit; different bytes, different unit.
-}
watchedTests :: [TestTree]
watchedTests =
    [ testCase "a changed config changes the unit file" $ withTempDir $ \dir -> do
        let ini = dir </> "pgbouncer.ini"
        writeFile ini "[databases]\nx = host=a\n"
        before <- Systemd.withWatchedFingerprint [ini] unit
        writeFile ini "[databases]\nx = host=b\n"
        after <- Systemd.withWatchedFingerprint [ini] unit
        assertBool "the unit did not change with the config" (before /= after)
    , testCase "an unchanged config leaves the unit alone" $ withTempDir $ \dir -> do
        let ini = dir </> "pgbouncer.ini"
        writeFile ini "[databases]\nx = host=a\n"
        before <- Systemd.withWatchedFingerprint [ini] unit
        after <- Systemd.withWatchedFingerprint [ini] unit
        assertEqual "" before after
    , testCase "a config appearing later is a change" $ withTempDir $ \dir -> do
        let ini = dir </> "userlist.txt"
        missing <- Systemd.withWatchedFingerprint [ini] unit
        writeFile ini "\"u\" \"secret\"\n"
        present <- Systemd.withWatchedFingerprint [ini] unit
        assertBool "a file appearing went unnoticed" (missing /= present)
    , testCase "the watched files are framed, not concatenated" $ withTempDir $ \dir -> do
        let (a, b) = (dir </> "a", dir </> "b")
        writeFile a "xy" >> writeFile b "z"
        one <- Systemd.withWatchedFingerprint [a, b] unit
        writeFile a "x" >> writeFile b "yz"
        two <- Systemd.withWatchedFingerprint [a, b] unit
        assertBool "moving a byte between two files went unnoticed" (one /= two)
    , testCase "the unit text itself is kept, with the hash appended" $ withTempDir $ \dir -> do
        let ini = dir </> "conf"
        writeFile ini "anything"
        out <- Systemd.withWatchedFingerprint [ini] unit
        assertBool (Text.unpack out) (unit `Text.isPrefixOf` out)
        assertBool (Text.unpack out) ("# salmon-watches: " `Text.isInfixOf` out)
    ]
  where
    unit :: Text
    unit = "[Unit]\nDescription=a service\n\n[Service]\nExecStart=/bin/true\n"

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
