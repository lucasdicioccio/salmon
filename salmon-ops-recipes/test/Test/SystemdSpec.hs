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

'unitDirTests' is about the unit file's directory: a real @upTree@\/@downTree@
of the unit file in a temporary directory (no systemd), graph shape, and
QuickCheck properties that the file node renders what it rendered before.
-}
module Test.SystemdSpec (tests) where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.List (intercalate, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Test.QuickCheck (Gen, counterexample, elements, forAll, listOf1, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Harness (runDown, runDownCapturing, runUp, withTempDir)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Reporter (silent)

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
        , testGroup "the unit directory" unitDirTests
        ]

{- | The unit file stands on a 'Systemd.unitDirectory', not on
'FS.dir': the directory holds every other unit on the machine, and
'FS.dir''s @down@ refuses a non-empty directory, so tearing one service down
among several reported the directory @Failed@ (and blocked whatever stood
under it). The teardown cases run real file IO in a temporary directory and
stop at the unit file: the service node's own @down@ is @systemctl stop@,
which is not run here.
-}
unitDirTests :: [TestTree]
unitDirTests =
    [ testCase "tearing down one unit file among several is clean and leaves the others" $ withTempDir $ \dir -> do
        let other = dir </> "other.service"
            cfg = service dir "mine.service"
        writeFile other "[Unit]\nDescription=not ours\n"
        upOk <- runUp (Systemd.unitFileNode [] cfg)
        assertBool "up failed" upOk
        assertBool "the unit file was not written" =<< doesFileExist (dir </> "mine.service")
        reports <- runDownCapturing (Systemd.unitFileNode [] cfg)
        assertEqual "failed or blocked nodes" [] [shorthandOf rep | rep <- reports, isBad rep]
        assertBool "our unit file is still there" . not =<< doesFileExist (dir </> "mine.service")
        assertBool "another unit was removed" =<< doesFileExist other
        assertBool "the shared directory was removed" =<< doesDirectoryExist dir
        downAgain <- runDown (Systemd.unitFileNode [] cfg)
        assertBool "a second down is not clean" downAgain
    , testCase "the same file over Filesystem.dir is the shape that failed" $ withTempDir $ \dir -> do
        -- pins why the predecessor was swapped: this is what systemdService
        -- declared before, and its teardown is not clean.
        let other = dir </> "other.service"
            plain = FS.filecontents (FS.FileContents (dir </> "mine.service") ("[Unit]\n" :: Text))
        writeFile other "[Unit]\nDescription=not ours\n"
        _ <- runUp plain
        reports <- runDownCapturing plain
        assertEqual "" ["directory"] [shorthandOf rep | rep <- reports, isBad rep]
    , testCase "a service's graph has a unit directory and no plain directory" $ do
        let dag = Dag.foldDag Dag.sameRepresentative (evalDeps (serviceNode (service "/etc/systemd/system" "a.service")))
        assertEqual "" ["file-contents", "systemd-service", "systemd-unit-dir"] (sort [act.shorthand | act <- Map.elems (Dag.dagNodes dag)])
    , testCase "a service and a job in one directory share it without a conflict" $ do
        let both = op "both" (deps [serviceNode (service Job.systemUnitDir "a.service"), Job.jobService silent ignoreTrack ignoreTrack (Job.job "b" ["/bin/true"])]) id
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps both)
        assertEqual "" 0 (length (Dag.dagConflicts dag))
        assertEqual "" 1 (length [() | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "systemd-unit-dir"])
    , testProperty "the unit file renders what filecontents rendered" $
        forAll genService $ \(d, name) ->
            let cfg = service d name
                plain = FS.filecontents (FS.FileContents (d </> Text.unpack name) (Systemd.render_config cfg))
             in describe (Systemd.unitFileNode [] cfg) === describe plain
    , testProperty "the unit file stands on exactly one systemd-unit-dir, keyed on its directory" $
        forAll genService $ \(d, name) ->
            let preds = [fmap describe' (opAct o) | o <- predecessorsOf (Systemd.unitFileNode [] (service d name))]
             in preds === [Just ("systemd-unit-dir", mkRef "systemd-unit-dir" d)]
    , testProperty "any number of services in one directory share one directory node" $
        forAll genServices $ \(d, names) ->
            let root = op "all" (deps [serviceNode (service d n) | n <- names]) id
                dag = Dag.foldDag Dag.sameRepresentative (evalDeps root)
                dirs = [() | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "systemd-unit-dir"]
             in counterexample (show names) (length dirs === 1 .&&. length (Dag.dagConflicts dag) === 0)
    ]
  where
    serviceNode :: Systemd.Config -> Op
    serviceNode = Systemd.systemdService silent ignoreTrack ignoreTrack

    describe :: Op -> Maybe (Text, Ref, Text, [Text])
    describe o = fmap (\act -> (act.shorthand, act.extension.ref, act.extension.help, act.extension.notes)) (opAct o)

    describe' :: Act Extension -> (Text, Ref)
    describe' act = (act.shorthand, act.extension.ref)

    predecessorsOf :: Op -> [Op]
    predecessorsOf o = toList (runIdentity o.predecessors)

    shorthandOf :: UpDown.Report Extension -> Text
    shorthandOf rep = case rep of
        UpDown.Failed act _ -> act.shorthand
        UpDown.Blocked act -> act.shorthand
        _ -> ""

    isBad :: UpDown.Report Extension -> Bool
    isBad rep = case rep of
        UpDown.Failed _ _ -> True
        UpDown.Blocked _ -> True
        _ -> False

    genDir :: Gen FilePath
    genDir = ("/" <>) . intercalate "/" <$> listOf1 (listOf1 (elements "abcxyz-_."))

    genName :: Gen Text
    genName = (\s -> Text.pack s <> ".service") <$> listOf1 (elements "abcxyz0123-_@")

    genService :: Gen (FilePath, Text)
    genService = (,) <$> genDir <*> genName

    genServices :: Gen (FilePath, [Text])
    genServices = (,) <$> genDir <*> (nub <$> listOf1 genName)

service :: FilePath -> Text -> Systemd.Config
service d name =
    Systemd.Config Systemd.System d name unit svc install
  where
    unit = Systemd.Unit "a service" "network-online.target"
    svc = Systemd.Service Systemd.Simple "nobody" "nogroup" "0022" (Systemd.Start "/bin/true" []) Systemd.OnFailure Systemd.Process "/"
    install = Systemd.Install "multi-user.target"

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
