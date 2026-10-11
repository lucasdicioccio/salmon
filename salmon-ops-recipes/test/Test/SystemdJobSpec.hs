{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.Systemd.Job" and the job half of
"Salmon.Builtin.Nodes.Podman.Quadlet".

'tests' is Layer 0 (what is rendered, what is refused, what the check
concludes, how the nodes are keyed) plus two groups that hand the rendered
files to the tools that read them, skipped loudly where the tool is missing:
@systemd-analyze verify@ for the service and its timer, and podman's quadlet
generator in dry-run for the container job. Neither starts anything.

'userTests' is Layer 2: the nodes through 'upTree' and 'downTree' against
this user's own systemd manager, in user scope, with no root and no
container. It asserts what the haddock claims: installing a job runs
nothing, a second pass is a skip, the timer is enabled and waiting, a
changed schedule re-arms it, a changed command is re-installed without
being run, 'Job.runJob' runs the command with its words
unexpanded and fails when the command does, and @down@ removes all of it and
can be run twice. Every unit gets a random name and a bracket that removes it
whatever the case did. The next two cases are 'Job.completedRun': a job with
a stamp is run once and then skipped, run again when a file it stands on is
written, fails every pass while its last run failed (also once systemd has
forgotten the failure), and a job with a waiting timer and no stamp is
remembered by systemd alone. The fifth is 'Job.launchedRun': a job of a few
seconds is started and the pass returns while it runs, a second pass starts
nothing, a later one finds it completed, and a failing one is found failed
and not run again until a file it stands on is written.

The pure half of 'Job.launchedRun' is also held by QuickCheck properties
('launchProperties') over generated @systemctl show@ output: its check
answers what 'Job.completedRun''s does, and nothing running or completed is
ever started.

Not run anywhere: system scope, a timer actually firing, a reboot (what
systemd forgets at one is imitated with @reset-failed@), and a container job
actually running (the generator's output is as far as that goes).
-}
module Test.SystemdJobSpec (tests, userTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgConfig), doesDirectoryExist, doesFileExist, findExecutable, getHomeDirectory, getModificationTime, getXdgDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import System.Posix.User (getRealUserID)
import System.Process (CmdSpec (..), CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.QuickCheck (Gen, choose, counterexample, elements, forAll, frequency, listOf, listOf1, oneof, property, shuffle, (.&&.), (=/=), (===), (==>))
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, testCaseSteps)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Command (..))
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (silent)
import Test.Harness (runDownCapturing, runUpCapturing, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Systemd.Job"
        [ testGroup "command words" wordTests
        , testGroup "job rendering" jobRenderTests
        , testGroup "timer rendering" timerRenderTests
        , testGroup "refusals" problemTests
        , testGroup "check" checkTests
        , testGroup "last run" lastRunTests
        , testGroup "nodes" nodeTests
        , testGroup "launched run" launchTests
        , testGroup "launched run (properties)" launchProperties
        , testGroup "container job" containerTests
        , testGroup "systemd-analyze" verifyTests
        , testGroup "quadlet generator" generatorTests
        ]

backup :: Job.Job
backup =
    (Job.job "backup" ["/usr/local/bin/backup", "--to", "/srv/backups", "two words"])
        { Job.jobDescription = "nightly backup"
        , Job.jobAfter = ["network-online.target", "postgresql.service"]
        , Job.jobUser = Just "backup"
        , Job.jobGroup = Just "backup"
        , Job.jobWorkingDir = Just "/srv"
        , Job.jobEnvironment = [("PGHOST", "/run/postgresql"), ("NOTE", "50% done")]
        , Job.jobEnvFile = Just "/etc/backup/env"
        , Job.jobTimeout = Just 3600
        }

nightly :: Job.Timer
nightly =
    (Job.jobTimer backup [Job.OnCalendar "*-*-* 03:00:00", Job.OnBootSec "15min"])
        { Job.timerPersistent = True
        , Job.timerRandomizedDelay = Just "10min"
        }

wordTests :: [TestTree]
wordTests =
    [ testCase "a plain word is left alone" $
        assertEqual "" "--to" (Systemd.literalArg "--to")
    , testCase "a word with whitespace is one word" $
        assertEqual "" "\"two words\"" (Systemd.literalArg "two words")
    , testCase "a dollar and a percent reach the program" $
        -- systemd substitutes both inside quotes too
        assertEqual "" "\"echo $$HOME %%h $${X}\"" (Systemd.literalArg "echo $HOME %h ${X}")
    , testCase "quotes and backslashes are escaped" $
        assertEqual "" "\"a\\\"b\\\\c\"" (Systemd.literalArg "a\"b\\c")
    , testCase "an empty word stays a word, and a semicolon does not end the command" $ do
        assertEqual "" "\"\"" (Systemd.literalArg "")
        assertEqual "" "\";\"" (Systemd.literalArg ";")
    , testCase "an authored service's words are quoted as they were" $ do
        assertEqual "" "plain" (Systemd.quoteArg "plain")
        assertEqual "" "\"console=ttyS0 root=/dev/root\"" (Systemd.quoteArg "console=ttyS0 root=/dev/root")
    ]

jobRenderTests :: [TestTree]
jobRenderTests =
    [ testCase "a full job, key by key" $
        assertEqual
            ""
            ( Text.unlines
                [ "[Unit]"
                , "Description=nightly backup"
                , "After=network-online.target postgresql.service"
                , ""
                , "[Service]"
                , "Type=oneshot"
                , "User=backup"
                , "Group=backup"
                , "WorkingDirectory=/srv"
                , "Environment=PGHOST=/run/postgresql"
                , "Environment=\"NOTE=50%% done\""
                , "EnvironmentFile=/etc/backup/env"
                , "TimeoutStartSec=3600"
                , "ExecStart=/usr/local/bin/backup --to /srv/backups \"two words\""
                ]
            )
            (Job.renderJobService backup)
    , testCase "the smallest job" $
        assertEqual
            ""
            (Text.unlines ["[Unit]", "Description=job tidy (salmon)", "", "[Service]", "Type=oneshot", "ExecStart=/usr/bin/tidy"])
            (Job.renderJobService (Job.job "tidy" ["/usr/bin/tidy"]))
    , testCase "nothing starts a job at boot" $
        assertBool "" (not ("[Install]" `Text.isInfixOf` Job.renderJobService backup))
    , testCase "a user-scope job names no user" $ do
        let rendered = Job.renderJobService backup{Job.jobScope = Systemd.User}
        assertBool (Text.unpack rendered) (not ("User=" `Text.isInfixOf` rendered))
        assertBool (Text.unpack rendered) (not ("Group=" `Text.isInfixOf` rendered))
    , testCase "the unit and its file are named after the job" $ do
        assertEqual "" "backup.service" (Job.jobServiceTarget backup)
        assertEqual "" "/etc/systemd/system/backup.service" (Job.jobServicePath backup)
    ]

timerRenderTests :: [TestTree]
timerRenderTests =
    [ testCase "a full timer, key by key" $
        assertEqual
            ""
            ( Text.unlines
                [ "[Unit]"
                , "Description=runs backup.service (salmon)"
                , ""
                , "[Timer]"
                , "Unit=backup.service"
                , "OnCalendar=*-*-* 03:00:00"
                , "OnBootSec=15min"
                , "Persistent=true"
                , "RandomizedDelaySec=10min"
                , ""
                , "[Install]"
                , "WantedBy=timers.target"
                ]
            )
            (Job.renderTimer nightly)
    , testCase "every kind of schedule has its key" $ do
        let rendered =
                Job.renderTimer
                    ( Job.timer
                        "t"
                        "x.service"
                        [Job.OnStartupSec "1min", Job.OnUnitActiveSec "1h", Job.OnUnitInactiveSec "30s"]
                    )
        forM_ ["OnStartupSec=1min", "OnUnitActiveSec=1h", "OnUnitInactiveSec=30s"] $ \line ->
            assertBool (Text.unpack line) (line `elem` Text.lines rendered)
        assertBool "a timer that makes up nothing does not say Persistent" (not ("Persistent" `Text.isInfixOf` rendered))
    , testCase "a job's timer shares its name, scope and directory" $ do
        let j = backup{Job.jobScope = Systemd.User, Job.jobUnitDir = "/home/u/.config/systemd/user"}
            tm = Job.jobTimer j [Job.OnCalendar "daily"]
        assertEqual "" "backup.timer" (Job.timerTarget tm)
        assertEqual "" "/home/u/.config/systemd/user/backup.timer" (Job.timerPath tm)
        assertEqual "" Systemd.User tm.timerScope
        assertEqual "" "backup.service" tm.timerTriggers
    , testCase "a timer can trigger a container job's service" $ do
        let tm = Job.timer "report" (Quadlet.serviceTarget report) [Job.OnCalendar "weekly"]
        assertBool "" ("Unit=report.service" `elem` Text.lines (Job.renderTimer tm))
    ]

problemTests :: [TestTree]
problemTests =
    [ testCase "the examples have none" $ do
        assertEqual "" [] (Job.jobProblems backup)
        assertEqual "" [] (Job.timerProblems nightly)
        assertEqual "" [] (Quadlet.containerProblems report)
    , testCase "a job needs a name that is a unit name" $ do
        assertBool "" (not (null (Job.jobProblems backup{Job.jobName = ""})))
        assertBool "" (not (null (Job.jobProblems backup{Job.jobName = "a b"})))
        assertBool "" (not (null (Job.jobProblems backup{Job.jobName = "../etc/x"})))
    , testCase "a job needs a command, by absolute path" $ do
        assertBool "" (not (null (Job.jobProblems backup{Job.jobCommand = []})))
        assertBool "" (not (null (Job.jobProblems backup{Job.jobCommand = ["backup", "--now"]})))
    , testCase "a line break anywhere is refused" $ do
        assertBool "" (not (null (Job.jobProblems backup{Job.jobCommand = ["/bin/sh", "-c", "a\nExecStart=/bin/rm -rf /"]})))
        assertBool "" (not (null (Job.jobProblems backup{Job.jobDescription = "x\n[Service]"})))
        assertBool "" (not (null (Job.jobProblems backup{Job.jobEnvironment = [("A", "b\nc")]})))
        assertBool "" (not (null (Job.timerProblems nightly{Job.timerSchedules = [Job.OnCalendar "daily\nUnit=other.service"]})))
    , testCase "an environment name is one word with no equals sign" $ do
        assertBool "" (not (null (Job.jobProblems backup{Job.jobEnvironment = [("", "x")]})))
        assertBool "" (not (null (Job.jobProblems backup{Job.jobEnvironment = [("A=B", "x")]})))
    , testCase "a timer needs a schedule, and something else to trigger" $ do
        assertBool "" (not (null (Job.timerProblems nightly{Job.timerSchedules = []})))
        assertBool "" (not (null (Job.timerProblems nightly{Job.timerSchedules = [Job.OnCalendar " "]})))
        assertBool "" (not (null (Job.timerProblems nightly{Job.timerTriggers = ""})))
        assertBool "" (not (null (Job.timerProblems nightly{Job.timerTriggers = "backup.timer"})))
    , testCase "problems are collected, not first-found" $
        assertBool "" (length (Job.jobProblems backup{Job.jobName = "", Job.jobCommand = []}) >= 2)
    , testCase "a job node with problems refuses to write its file" $ withTempDir $ \dir -> do
        let j = backup{Job.jobUnitDir = dir, Job.jobCommand = ["relative"]}
        reports <- runUpCapturing (Job.unitFile silent Systemd.User (Job.jobServicePath j) (Job.jobProblems j) (Job.renderJobService j))
        assertBool "the file node did not fail" (not (null [() | UpDown.Failed _ _ <- reports]))
        assertBool "the file was written" . not =<< doesFileExist (Job.jobServicePath j)
    ]

checkTests :: [TestTree]
checkTests =
    [ testCase "loaded as written is installed, running or not" $
        assertEqual "" Success (Systemd.interpretLoaded ["LoadState=loaded", "NeedDaemonReload=no"])
    , testCase "a file changed since it was loaded needs a reload" $
        assertBool "" (isFailure (Systemd.interpretLoaded ["LoadState=loaded", "NeedDaemonReload=yes"]))
    , testCase "a unit systemd has never heard of is not installed" $
        assertBool "" (isFailure (Systemd.interpretLoaded ["LoadState=not-found", "NeedDaemonReload=no"]))
    , testCase "a unit systemd refuses is not installed" $ do
        assertBool "" (isFailure (Systemd.interpretLoaded ["LoadState=bad-setting", "NeedDaemonReload=no"]))
        assertBool "" (isFailure (Systemd.interpretLoaded ["LoadState=masked", "NeedDaemonReload=no"]))
    , testCase "no answer is not an installed unit" $
        assertBool "" (isFailure (Systemd.interpretLoaded []))
    , testCase "an idle job is not a broken service" $
        -- the reason a job has a check of its own: the service one would
        -- restart it at every pass
        assertBool "" (isFailure (Systemd.interpretShow ["ActiveState=inactive", "UnitFileState=static", "NeedDaemonReload=no"]))
    , testCase "a waiting timer is a running unit" $
        assertEqual "" Success (Systemd.interpretShow ["ActiveState=active", "UnitFileState=enabled", "NeedDaemonReload=no"])
    , testCase "the new systemctl calls" $ do
        assertEqual "" ["--user", "start", "x.service"] (argv (Systemd.StartUnit Systemd.User "x.service"))
        assertEqual "" ["disable", "--now", "x.timer"] (argv (Systemd.Disable Systemd.System "x.timer"))
    ]
  where
    argv call = case cmdspec (prepare Systemd.callSystemctl call) of
        RawCommand _ args -> args
        ShellCommand line -> [line]

{- | @systemctl --user show --timestamp=us+utc@ on systemd 255, for the
properties 'Job.lastRunProperties' names: a oneshot unit a waiting timer
keeps loaded after a successful run, one whose run exited 1, and one with
nothing referring to it after a successful run (the same lines as one never
run).
-}
ranOk, ranBadly, forgotten :: [Text]
ranOk =
    [ "Result=success"
    , "ExecMainStartTimestamp=Mon 2026-10-05 09:56:02.675581 UTC"
    , "ExecMainStatus=0"
    , "LoadState=loaded"
    , "ActiveState=inactive"
    ]
ranBadly =
    [ "Result=exit-code"
    , "ExecMainStartTimestamp=Mon 2026-10-05 09:56:02.744529 UTC"
    , "ExecMainStatus=1"
    , "LoadState=loaded"
    , "ActiveState=failed"
    ]
forgotten =
    [ "Result=success"
    , "ExecMainStartTimestamp="
    , "ExecMainStatus=0"
    , "LoadState=loaded"
    , "ActiveState=inactive"
    ]

-- | When 'ranOk' started, and that many seconds from it.
ranAt :: UTCTime
ranAt = posixSecondsToUTCTime 1791194162.675581

after, before :: Double -> UTCTime
after seconds = addUTCTime (realToFrac seconds) ranAt
before seconds = after (negate seconds)

lastRunTests :: [TestTree]
lastRunTests =
    [ testCase "the timestamp systemd prints is read to the microsecond" $
        assertEqual "" (Just ranAt) (Job.parseTimestamp "Mon 2026-10-05 09:56:02.675581 UTC")
    , testCase "a run that succeeded is a completed job, not a stopped service" $
        assertEqual "" Completed (Job.interpretLastRun Job.noEvidence ranOk)
    , testCase "a run that failed is a failure naming how" $
        case Job.interpretLastRun Job.noEvidence ranBadly of
            Failure why -> do
                assertBool (Text.unpack why) ("exit-code" `Text.isInfixOf` why)
                assertBool (Text.unpack why) ("exit status 1" `Text.isInfixOf` why)
            other -> assertFailure (show other)
    , testCase "a failed run is not excused by an older success" $
        assertBool "" (isFailure (Job.interpretLastRun Job.noEvidence{Job.evidenceSucceeded = Just (before 60)} ranBadly))
    , testCase "a result other than success fails whatever the active state" $
        assertBool "" (isFailure (Job.interpretLastRun Job.noEvidence (map (Text.replace "Result=success" "Result=timeout") ranOk)))
    , testCase "a job systemd has no run of has not run" $
        assertBool "" (isFailure (Job.interpretLastRun Job.noEvidence forgotten))
    , testCase "a run under way is neither" $
        assertEqual "" Unknown (Job.interpretLastRun Job.noEvidence (map (Text.replace "ActiveState=inactive" "ActiveState=activating") ranOk))
    , testCase "a unit systemd does not hold has not run" $ do
        assertBool "" (isFailure (Job.interpretLastRun Job.noEvidence (map (Text.replace "LoadState=loaded" "LoadState=not-found") forgotten)))
        assertBool "" (isFailure (Job.interpretLastRun Job.noEvidence []))
    , testCase "a run older than a file it stands on does not count, and the file is named" $ do
        let evidence = Job.noEvidence{Job.evidenceWritten = [("/etc/backup/env", before 5), ("/usr/local/bin/backup", after 0.5)]}
        case Job.interpretLastRun evidence ranOk of
            Failure why -> assertBool (Text.unpack why) ("/usr/local/bin/backup" `Text.isInfixOf` why)
            other -> assertFailure (show other)
    , testCase "a run newer than every file counts" $
        assertEqual "" Completed (Job.interpretLastRun Job.noEvidence{Job.evidenceWritten = [("/etc/backup/env", before 5), ("/x", before 0.001)]} ranOk)
    , testCase "the stamp stands in for what systemd forgot" $ do
        assertEqual "" Completed (Job.interpretLastRun Job.noEvidence{Job.evidenceSucceeded = Just ranAt} forgotten)
        assertEqual "" Completed (Job.interpretLastRun (Job.RunEvidence [("/x", before 1)] (Just ranAt) Nothing) forgotten)
        assertBool "" (isFailure (Job.interpretLastRun (Job.RunEvidence [("/x", after 1)] (Just ranAt) Nothing) forgotten))
    , testCase "the later of systemd's run and the stamp's is the last one" $ do
        assertEqual "" Completed (Job.interpretLastRun (Job.RunEvidence [("/x", before 1)] (Just (before 3600)) Nothing) ranOk)
        assertEqual "" Completed (Job.interpretLastRun (Job.RunEvidence [("/x", after 1)] (Just (after 2)) Nothing) ranOk)
    , testCase "a run that started and left no stamp failed, though systemd forgot it" $ do
        assertBool "" (isFailure (Job.interpretLastRun (Job.RunEvidence [] (Just ranAt) (Just (after 60))) forgotten))
        assertBool "" (isFailure (Job.interpretLastRun (Job.RunEvidence [] Nothing (Just ranAt)) forgotten))
    , testCase "a running stamp left by an older failure does not undo a later success" $
        assertEqual "" Completed (Job.interpretLastRun (Job.RunEvidence [] (Just ranAt) (Just (before 60))) forgotten)
    , testCase "a timestamp that cannot be read is not a missing run" $
        assertEqual "" Unknown (Job.interpretLastRun Job.noEvidence (map (Text.replace "Mon 2026-10-05 09:56:02.675581 UTC" "lundi matin") ranOk))
    , testCase "a stamped job records its runs around the command" $ do
        let ls = Text.lines (Job.renderJobService backup{Job.jobStamp = Just "/var/lib/backup/last run"})
            execs = [l | l <- ls, "Exec" `Text.isPrefixOf` l]
        assertEqual
            ""
            [ "ExecStartPre=touch \"/var/lib/backup/last run.running\""
            , "ExecStart=/usr/local/bin/backup --to /srv/backups \"two words\""
            , "ExecStartPost=mv -f \"/var/lib/backup/last run.running\" \"/var/lib/backup/last run\""
            ]
            execs
    , testCase "a job with no stamp is rendered as before" $
        assertBool "" (not ("ExecStartP" `Text.isInfixOf` Job.renderJobService backup))
    , testCase "a stamp that is not an absolute path, or holds a line break, is refused" $ do
        assertBool "" (not (null (Job.jobProblems backup{Job.jobStamp = Just "stamp"})))
        assertBool "" (not (null (Job.jobProblems backup{Job.jobStamp = Just "/a\nExecStart=/bin/evil"})))
        assertEqual "" [] (Job.jobProblems backup{Job.jobStamp = Just "/var/lib/backup/stamp"})
    , testCase "a job's run is judged against its unit file, its environment file and its stamp" $
        assertEqual
            ""
            (Job.Completion Systemd.System "backup.service" ["/etc/systemd/system/backup.service", "/etc/backup/env"] (Just "/s"))
            (Job.jobCompletion backup{Job.jobStamp = Just "/s"})
    , testCase "a completed job is the run's effect site, standing on the installed job" $ do
        assertEqual "" (refOf (runNode backup)) (refOf (completedNode backup))
        let dag = Dag.foldDag Dag.sameRepresentative (evalDeps (completedNode backup))
        assertEqual "" ["file-contents", "systemd-job", "systemd-job-completed", "systemd-unit-dir"] (sort [act.shorthand | act <- Map.elems (Dag.dagNodes dag)])
    , testCase "what a run is compared with is visible in the node's description" $
        assertBool "" (notesOf (completedNode backup) /= notesOf (completedNode backup{Job.jobEnvFile = Nothing}))
    ]
  where
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)

completedNode :: Job.Job -> Op
completedNode = Job.completedJob silent ignoreTrack ignoreTrack

nodeTests :: [TestTree]
nodeTests =
    [ testCase "a job is keyed on its unit, not on its command" $ do
        assertEqual "" (refOf (jobNode backup)) (refOf (jobNode backup{Job.jobCommand = ["/bin/true"]}))
        assertBool "" (refOf (jobNode backup) /= refOf (jobNode backup{Job.jobName = "other"}))
    , testCase "a changed command is visible in the job's description" $
        -- so `run serve` sees the re-declaration as a change
        assertBool "" (notesOf (jobNode backup) /= notesOf (jobNode backup{Job.jobCommand = ["/bin/true"]}))
    , testCase "a changed schedule is visible in the timer's description" $
        assertBool "" (notesOf (timerNode nightly) /= notesOf (timerNode nightly{Job.timerSchedules = [Job.OnCalendar "hourly"]}))
    , testCase "the job, its timer and a run of it are three nodes" $ do
        let refs = [refOf (jobNode backup), refOf (timerNode nightly), refOf (runNode backup)]
        assertBool "a node has no ref" (Nothing `notElem` refs)
        assertBool "two of them share a ref" (and [x /= y | (i, x) <- zip [0 :: Int ..] refs, (k, y) <- zip [0 ..] refs, i /= k])
    , testCase "a scheduled job is the timer standing on the job and both files" $ do
        let dag = Dag.foldDag Dag.sameRepresentative (evalDeps (scheduled backup))
            names = [act.shorthand | act <- Map.elems (Dag.dagNodes dag)]
        assertEqual "" ["file-contents", "file-contents", "systemd-job", "systemd-timer", "systemd-unit-dir"] (sort names)
        assertEqual "the shared directory was described two ways" 0 (length (Dag.dagConflicts dag))
    , testCase "two jobs in one directory share it without a conflict" $ do
        let both = op "both" (deps [scheduled backup, scheduled backup{Job.jobName = "other"}]) id
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps both)
        assertEqual "" 0 (length (Dag.dagConflicts dag))
        assertEqual "" 1 (length [() | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "systemd-unit-dir"])
    , testCase "a container job is keyed on its unit, and is not the service node" $ do
        assertEqual "" (refOf (containerNode report)) (refOf (containerNode report{Quadlet.containerExec = ["/bin/true"]}))
        assertEqual "" (Just "podman-quadlet-job") (fmap (\act -> act.shorthand) (opAct (containerNode report)))
        assertBool "" (notesOf (containerNode report) /= notesOf (containerNode report{Quadlet.containerExec = ["/bin/true"]}))
    ]
  where
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)

jobNode :: Job.Job -> Op
jobNode = Job.jobService silent ignoreTrack ignoreTrack

timerNode :: Job.Timer -> Op
timerNode = Job.timerUnit silent ignoreTrack ignoreTrack

runNode :: Job.Job -> Op
runNode j = Job.runJob silent ignoreTrack j.jobScope (Job.jobServiceTarget j)

scheduled :: Job.Job -> Op
scheduled j = Job.scheduledJob silent ignoreTrack ignoreTrack j [Job.OnCalendar "*-*-* 03:00:00"]

containerNode :: Quadlet.Container -> Op
containerNode = Quadlet.quadletJob silent ignoreTrack ignoreTrack

-------------------------------------------------------------------------------

{- | @systemctl --user show --timestamp=us+utc@ on systemd 255 for
'Job.runStateProperties', captured around a @start --no-block@ of a oneshot
unit that sleeps and exits 3: right after the start, and once it had failed.
-}
launching, launchedBadly :: [Text]
launching =
    [ "Result=success"
    , "ExecMainStartTimestamp=Sun 2026-10-11 08:12:59.712251 UTC"
    , "ExecMainStatus=0"
    , "LoadState=loaded"
    , "ActiveState=activating"
    , "InactiveEnterTimestamp="
    ]
launchedBadly =
    [ "Result=exit-code"
    , "ExecMainStartTimestamp=Sun 2026-10-11 08:12:59.712251 UTC"
    , "ExecMainStatus=3"
    , "LoadState=loaded"
    , "ActiveState=failed"
    , "InactiveEnterTimestamp=Sun 2026-10-11 08:13:01.716922 UTC"
    ]

-- | When 'launchedBadly' failed, and that many seconds from it.
failedAt :: UTCTime
failedAt = posixSecondsToUTCTime 1791706381.716922

sinceFailure :: Double -> UTCTime
sinceFailure seconds = addUTCTime (realToFrac seconds) failedAt

stamped :: Job.Job
stamped = backup{Job.jobStamp = Just "/var/lib/backup/stamp"}

launchedNode :: Job.Polling -> Job.Job -> Op
launchedNode = Job.launchedJob silent ignoreTrack ignoreTrack

launchTests :: [TestTree]
launchTests =
    [ testCase "the start that does not wait" $ do
        assertEqual "" ["start", "--no-block", "x.service"] (argv (Systemd.StartUnitNoBlock Systemd.System "x.service"))
        assertEqual "" ["--user", "start", "--no-block", "x.service"] (argv (Systemd.StartUnitNoBlock Systemd.User "x.service"))
    , testCase "the start that waits is the one it was" $
        assertEqual "" ["start", "x.service"] (argv (Systemd.StartUnit Systemd.System "x.service"))
    , testCase "the failure's time is read" $
        assertEqual "" (Just failedAt) (Job.parseTimestamp "Sun 2026-10-11 08:13:01.716922 UTC")
    , testCase "a run under way is running, and is left alone" $ do
        assertEqual "" Job.Running (Job.classifyRun Job.noEvidence launching)
        assertEqual "" Job.LeaveRunning (Job.launchDecision Job.Running)
        assertEqual "" Job.Pending (Job.runOutcome Job.Running)
        assertEqual "the check restarts nothing under serve" Unknown (Job.runVerdict Job.Running)
    , testCase "a run under way is running whatever its stamps say" $
        -- the running stamp is newer than the stamp for as long as a run lasts
        assertEqual "" Job.Running (Job.classifyRun (Job.RunEvidence [] (Just (before 60)) (Just ranAt)) launching)
    , testCase "a job never run is started" $ do
        assertEqual "" Job.NeverRan (Job.classifyRun Job.noEvidence forgotten)
        assertEqual "" Job.Start (Job.launchDecision Job.NeverRan)
    , testCase "a job that ran is not started again" $ do
        assertEqual "" Job.Succeeded (Job.classifyRun Job.noEvidence{Job.evidenceSucceeded = Just ranAt} forgotten)
        assertEqual "" Job.LeaveDone (Job.launchDecision Job.Succeeded)
        assertEqual "" Job.Finished (Job.runOutcome Job.Succeeded)
    , testCase "a run older than a file it stands on is started again" $ do
        let st = Job.classifyRun (Job.RunEvidence [("/etc/backup/env", after 1)] (Just ranAt) Nothing) forgotten
        assertEqual "" (Job.Outdated "/etc/backup/env") st
        assertEqual "" Job.Start (Job.launchDecision st)
    , testCase "a failed run is reported, not run again" $ do
        let st = Job.classifyRun Job.noEvidence launchedBadly
        assertEqual "" (Job.LastFailed "the last run failed: Result=exit-code, exit status 3" Nothing) st
        assertEqual "" (Job.ReportFailure "the last run failed: Result=exit-code, exit status 3") (Job.launchDecision st)
        assertEqual "" (Job.Broken "the last run failed: Result=exit-code, exit status 3") (Job.runOutcome st)
    , testCase "a failed run is not excused by files written before it failed" $
        assertEqual
            ""
            (Job.LastFailed "the last run failed: Result=exit-code, exit status 3" Nothing)
            -- written while it ran: after it started, before it failed
            (Job.classifyRun Job.noEvidence{Job.evidenceWritten = [("/x", sinceFailure (-1)), ("/y", sinceFailure (-3600))]} launchedBadly)
    , testCase "a file written since the failure has the job run again, and is named" $ do
        let st = Job.classifyRun Job.noEvidence{Job.evidenceWritten = [("/x", sinceFailure (-1)), ("/etc/backup/env", sinceFailure 0.5)]} launchedBadly
        assertEqual "" (Job.LastFailed "the last run failed: Result=exit-code, exit status 3" (Just "/etc/backup/env")) st
        assertEqual "" Job.Start (Job.launchDecision st)
        assertEqual "started, and not yet shown as running" Job.Pending (Job.runOutcome st)
    , testCase "a failure systemd forgot is reported from the running stamp" $ do
        let left = Job.RunEvidence [("/x", before 1)] Nothing (Just ranAt)
        assertEqual "" (Job.LastFailed "the last run that started did not succeed" Nothing) (Job.classifyRun left forgotten)
        assertEqual
            ""
            (Job.LastFailed "the last run that started did not succeed" (Just "/x"))
            (Job.classifyRun left{Job.evidenceWritten = [("/x", after 1)]} forgotten)
    , testCase "a failure whose time systemd does not give is reported, not retried" $ do
        let untimed = filter (not . ("InactiveEnterTimestamp" `Text.isPrefixOf`)) launchedBadly
        assertEqual
            ""
            (Job.ReportFailure "the last run failed: Result=exit-code, exit status 3")
            (Job.launchDecision (Job.classifyRun Job.noEvidence{Job.evidenceWritten = [("/x", sinceFailure 3600)]} untimed))
    , testCase "what cannot be read starts nothing" $ do
        let st = Job.classifyRun Job.noEvidence (map (Text.replace "Mon 2026-10-05 09:56:02.675581 UTC" "lundi matin") ranOk)
        assertEqual "" Job.Unreadable st
        assertEqual "" Job.CannotTell (Job.launchDecision st)
        assertEqual "" Job.Undetermined (Job.runOutcome st)
    , testCase "a unit systemd does not hold is left to systemctl to refuse" $ do
        assertEqual "" (Job.NotLoaded "the unit is not-found") (Job.classifyRun Job.noEvidence ["LoadState=not-found"])
        assertEqual "" Job.Start (Job.launchDecision (Job.NotLoaded "the unit is not-found"))
    , testCase "no wait asks once" $
        assertEqual "" [] (Job.pollDelays Job.noWait)
    , testCase "a wait is cut into pauses" $ do
        assertEqual "" [5, 5, 2] (Job.pollDelays (Job.Polling 12 5))
        assertEqual "" [1, 1, 1] (Job.pollDelays (Job.Polling 3 0))
        assertEqual "" [] (Job.pollDelays (Job.Polling (-4) 5))
    , testCase "a run that is not waited for needs a stamp" $ do
        assertBool "" (not (null (Job.launchProblems (Job.jobCompletion backup))))
        assertEqual "" [] (Job.launchProblems (Job.jobCompletion stamped))
    , testCase "a job still running says so, and not that it failed" $ do
        let said = show (Job.RunInProgress "backup.service")
        assertBool said ("in progress" `isInfixOf` said)
        assertBool said ("backup.service" `isInfixOf` said)
    , testCase "a launched job is the run's effect site, standing on the installed job" $ do
        assertEqual "" (refOf (runNode stamped)) (refOf (launchedNode Job.noWait stamped))
        assertEqual "" (refOf (completedNode stamped)) (refOf (launchedNode Job.noWait stamped))
        let dag = Dag.foldDag Dag.sameRepresentative (evalDeps (launchedNode Job.noWait stamped))
        assertEqual "" ["file-contents", "systemd-job", "systemd-job-launched", "systemd-unit-dir"] (sort [act.shorthand | act <- Map.elems (Dag.dagNodes dag)])
    , testCase "a launched job is told from a waited one, and a wait from none" $ do
        assertBool "" (helpOf (launchedNode Job.noWait stamped) /= helpOf (completedNode stamped))
        assertEqual "" (notesOf (completedNode stamped)) (notesOf (launchedNode Job.noWait stamped))
        assertEqual
            ""
            (Just ["newer than: /etc/systemd/system/backup.service", "newer than: /etc/backup/env", "stamp: /var/lib/backup/stamp", "waits: up to 600s, asking every 10s"])
            (notesOf (launchedNode (Job.Polling 600 10) stamped))
    , testCase "the waited nodes describe themselves as they did" $ do
        -- written out, not computed: a changed description restarts nothing
        -- here, but it is a changed representative under `run serve`
        assertEqual "" (Just "runs backup.service to completion") (helpOf (runNode backup))
        assertEqual "" (Just []) (notesOf (runNode backup))
        assertEqual "" (Just "has backup.service run to completion since it was last changed") (helpOf (completedNode stamped))
        assertEqual
            ""
            (Just ["newer than: /etc/systemd/system/backup.service", "newer than: /etc/backup/env", "stamp: /var/lib/backup/stamp"])
            (notesOf (completedNode stamped))
        assertEqual "" (Just (mkRef "systemd-job-run" ("backup.service" :: Text))) (refOf (completedNode stamped))
    , testCase "the existing check asks systemd what it asked" $
        assertEqual "" ["LoadState", "ActiveState", "Result", "ExecMainStatus", "ExecMainStartTimestamp"] Job.lastRunProperties
    ]
  where
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)
    helpOf o = fmap (\act -> act.extension.help) (opAct o)
    argv call = case cmdspec (prepare Systemd.callSystemctl call) of
        RawCommand _ args -> args
        ShellCommand line -> [line]

-- | A time within a quarter of an hour of 'ranAt', to the microsecond.
genTime :: Gen UTCTime
genTime = do
    micros <- choose (-900000000, 900000000 :: Integer)
    pure (addUTCTime (fromInteger micros / 1000000) ranAt)

-- | As @--timestamp=us+utc@ prints one.
printTimestamp :: UTCTime -> Text
printTimestamp = Text.pack . formatTime defaultTimeLocale "%a %Y-%m-%d %H:%M:%S%Q UTC"

genEvidence :: Gen Job.RunEvidence
genEvidence =
    Job.RunEvidence
        <$> listOf ((,) <$> elements ["/etc/backup/env", "/usr/local/bin/backup", "/etc/systemd/system/backup.service"] <*> genTime)
        <*> oneof [pure Nothing, Just <$> genTime]
        <*> oneof [pure Nothing, Just <$> genTime]

{- | What @systemctl show@ may print for 'Job.runStateProperties': each
property with one of the values systemd has for it, an empty one, an
unreadable one, or not there at all, in any order.
-}
genShow :: Gen [Text]
genShow = do
    ls <-
        sequence
            [ line "LoadState" (frequency [(6, pure "loaded"), (1, elements ["not-found", "masked", "bad-setting", ""])])
            , line "ActiveState" (frequency [(4, pure "inactive"), (2, pure "failed"), (2, pure "activating"), (1, elements ["active", "deactivating", "reloading", ""])])
            , line "Result" (frequency [(4, pure "success"), (2, elements ["exit-code", "timeout", "signal", ""])])
            , line "ExecMainStatus" (elements ["0", "1", "3", ""])
            , line "ExecMainStartTimestamp" genStamp
            , line "InactiveEnterTimestamp" genStamp
            ]
    shuffle (concat ls)
  where
    line :: Text -> Gen Text -> Gen [Text]
    line name value = frequency [(9, (\v -> [name <> "=" <> v]) <$> value), (1, pure [])]

    genStamp :: Gen Text
    genStamp = frequency [(5, printTimestamp <$> genTime), (2, pure ""), (1, pure "n/a"), (1, pure "lundi matin")]

genCompletion :: Gen Job.Completion
genCompletion =
    Job.Completion
        <$> elements [Systemd.System, Systemd.User]
        <*> ((<> ".service") <$> genWord)
        <*> listOf (path <$> genWord)
        <*> oneof [pure Nothing, Just . path <$> genWord]
  where
    path w = "/var/lib/" <> Text.unpack w

genWord :: Gen Text
genWord = Text.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9'] <> "-_@"))

genPolling :: Gen Job.Polling
genPolling = Job.Polling <$> choose (-50, 5000) <*> choose (-5, 120)

launchProperties :: [TestTree]
launchProperties =
    [ testProperty "a timestamp is read back as printed" $
        forAll genTime $ \t -> Job.parseTimestamp (printTimestamp t) === Just t
    , testProperty "the launched node's check answers what the waited node's does" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            Job.runVerdict (Job.classifyRun evidence ls) === Job.interpretLastRun evidence ls
    , testProperty "the waited node's check does not read when the unit came to rest" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            Job.interpretLastRun evidence (filter (not . ("InactiveEnterTimestamp" `Text.isPrefixOf`)) ls) === Job.interpretLastRun evidence ls
    , testProperty "the order systemd prints in does not matter" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls -> forAll (shuffle ls) $ \again ->
            Job.classifyRun evidence again === Job.classifyRun evidence ls
    , testProperty "only a job at rest whose last run does not count is started" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            let st = Job.classifyRun evidence ls
                verdict = Job.interpretLastRun evidence ls
             in counterexample (show st) $ case Job.launchDecision st of
                    Job.Start -> property (isFailure verdict)
                    Job.LeaveDone -> verdict === Completed
                    Job.LeaveRunning -> verdict === Unknown
                    Job.CannotTell -> verdict === Unknown
                    Job.ReportFailure why -> verdict === Failure why
    , testProperty "a loaded unit between states is never started" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls -> forAll (elements ["activating", "deactivating", "reloading"]) $ \active ->
            let moving = ["LoadState=loaded", "ActiveState=" <> active] <> ls
             in Job.launchDecision (Job.classifyRun evidence moving) === Job.LeaveRunning
    , testProperty "a completed job is never started" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            case Job.interpretLastRun evidence ls of
                Completed -> Job.launchDecision (Job.classifyRun evidence ls) === Job.LeaveDone
                _ -> property True
    , testProperty "with nothing written, a failed run is reported and never run again" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            case Job.classifyRun evidence{Job.evidenceWritten = []} ls of
                st@(Job.LastFailed why since) ->
                    counterexample (show st) $ since === Nothing .&&. Job.launchDecision st === Job.ReportFailure why
                _ -> property True
    , testProperty "a file is named as written since only when it is newer than the failure" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            case Job.classifyRun evidence ls of
                Job.LastFailed _ (Just path) -> property (path `elem` map fst evidence.evidenceWritten)
                Job.Outdated path -> property (path `elem` map fst evidence.evidenceWritten)
                _ -> property True
    , testProperty "a failure systemd timed is retried once a file is written after it" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls -> forAll genTime $ \at ->
            let timed = ["InactiveEnterTimestamp=" <> printTimestamp at] <> ls
                late = evidence{Job.evidenceWritten = evidence.evidenceWritten <> [("/late", addUTCTime 3600 ranAt)]}
             in case Job.classifyRun late timed of
                    st@(Job.LastFailed _ since)
                        -- the running stamp's failure is timed by the stamp
                        | "the last run failed" `Text.isPrefixOf` reason st ->
                            counterexample (show st) $ property (since /= Nothing) .&&. Job.launchDecision st === Job.Start
                    _ -> property True
    , testProperty "writing a file never turns a job that should run into one that should not" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            let late = evidence{Job.evidenceWritten = evidence.evidenceWritten <> [("/late", addUTCTime 3600 ranAt)]}
             in case Job.launchDecision (Job.classifyRun evidence ls) of
                    Job.Start -> Job.launchDecision (Job.classifyRun late ls) === Job.Start
                    _ -> property True
    , testProperty "an outcome is a finished run exactly when the check says completed" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            let st = Job.classifyRun evidence ls
             in (Job.runOutcome st == Job.Finished) === (Job.interpretLastRun evidence ls == Completed)
    , testProperty "an outcome is a failure only for a failure nobody answered" $
        forAll genEvidence $ \evidence -> forAll genShow $ \ls ->
            let st = Job.classifyRun evidence ls
             in case Job.runOutcome st of
                    Job.Broken why -> st === Job.LastFailed why Nothing
                    Job.Pending -> property (Job.launchDecision st `elem` [Job.Start, Job.LeaveRunning])
                    Job.Finished -> st === Job.Succeeded
                    Job.Undetermined -> st === Job.Unreadable
    , testProperty "the pauses add up to the wait, none longer than asked" $
        forAll genPolling $ \p ->
            let pauses = Job.pollDelays p
             in counterexample (show pauses) $
                    sum pauses === max 0 p.pollingWait
                        .&&. property (all (\d -> d >= 1 && d <= max 1 p.pollingEvery) pauses)
    , testProperty "only the last pause is a short one" $
        forAll genPolling $ \p ->
            case reverse (Job.pollDelays p) of
                [] -> property True
                (_ : earlier) -> property (all (== max 1 p.pollingEvery) earlier)
    , testProperty "a stamp is what a launched run needs, and all it needs" $
        forAll genCompletion $ \c -> null (Job.launchProblems c) === (c.completionStamp /= Nothing)
    , testProperty "the launched node is keyed as the waited ones are, whatever the wait" $
        forAll genCompletion $ \c -> forAll genPolling $ \p ->
            let launched = Job.launchedRun silent ignoreTrack p c
                waited = Job.completedRun silent ignoreTrack c
                run = Job.runJob silent ignoreTrack c.completionScope c.completionUnit
             in refOf launched === refOf waited .&&. refOf launched === refOf run .&&. refOf launched === Just (mkRef "systemd-job-run" c.completionUnit)
    , testProperty "the launched node's description is the same at every rendering" $
        forAll genCompletion $ \c -> forAll genPolling $ \p ->
            let node = Job.launchedRun silent ignoreTrack p c
             in (helpOf node, notesOf node) === (helpOf (Job.launchedRun silent ignoreTrack p c), Just (Job.launchNotes p c))
    , testProperty "with no wait, the launched node's notes are the waited node's" $
        forAll genCompletion $ \c -> forAll genPolling $ \p ->
            let quiet = p{Job.pollingWait = min 0 p.pollingWait}
             in notesOf (Job.launchedRun silent ignoreTrack quiet c) === notesOf (Job.completedRun silent ignoreTrack c)
    , testProperty "a wait shows in the notes, so a changed one is a changed declaration" $
        forAll genCompletion $ \c -> forAll genPolling $ \p ->
            p.pollingWait > 0 ==>
                Job.launchNotes p c =/= Job.launchNotes p{Job.pollingWait = p.pollingWait + 1} c
    , testProperty "the waited node describes itself as before the launched one existed" $
        forAll genCompletion $ \c ->
            let waited = Job.completedRun silent ignoreTrack c
             in helpOf waited === Just ("has " <> c.completionUnit <> " run to completion since it was last changed")
                    .&&. notesOf waited
                        === Just
                            ( ["newer than: " <> Text.pack path | path <- c.completionWritten]
                                <> ["stamp: " <> Text.pack s | Just s <- [c.completionStamp]]
                            )
    , testProperty "the two starts differ by one word" $
        forAll genCompletion $ \c ->
            let scope = Systemd.scopeArgs c.completionScope
                unit = Text.unpack c.completionUnit
             in argv (Systemd.StartUnit c.completionScope c.completionUnit) === scope <> ["start", unit]
                    .&&. argv (Systemd.StartUnitNoBlock c.completionScope c.completionUnit) === scope <> ["start", "--no-block", unit]
    ]
  where
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)
    helpOf o = fmap (\act -> act.extension.help) (opAct o)
    reason (Job.LastFailed why _) = why
    reason _ = ""
    argv call = case cmdspec (prepare Systemd.callSystemctl call) of
        RawCommand _ args -> args
        ShellCommand line -> [line]

containerTests :: [TestTree]
containerTests =
    [ testCase "a container job, key by key" $
        assertEqual
            ""
            ( Text.unlines
                [ "[Unit]"
                , "Description=job report (salmon)"
                , ""
                , "[Container]"
                , "ContainerName=report"
                , "Image=docker.io/library/alpine:3"
                , "Exec=/bin/sh -c \"echo $$HOME 100%%\""
                , ""
                , "[Service]"
                , "Type=oneshot"
                , "Restart=no"
                ]
            )
            (Quadlet.renderContainer report)
    , testCase "a service container says nothing new" $ do
        let rendered = Quadlet.renderContainer (Quadlet.container (Podman.ContainerName "web") "docker.io/library/nginx:1.27")
        assertBool "" (not ("Exec=" `Text.isInfixOf` rendered))
        assertBool "" (not ("Type=" `Text.isInfixOf` rendered))
    , testCase "a service container can be given a command too" $ do
        let c = (Quadlet.container (Podman.ContainerName "web") "img:1"){Quadlet.containerExec = ["serve", "--port", "80"]}
        assertBool "" ("Exec=serve --port 80" `elem` Text.lines (Quadlet.renderContainer c))
    , testCase "a job restarted always is refused, on failure is not" $ do
        assertBool "" (not (null (Quadlet.containerProblems report{Quadlet.containerRestart = Quadlet.RestartAlways})))
        assertEqual "" [] (Quadlet.containerProblems report{Quadlet.containerRestart = Quadlet.RestartOnFailure})
    , testCase "a line break in the command is refused" $
        assertBool "" (not (null (Quadlet.containerProblems report{Quadlet.containerExec = ["sh", "-c", "a\nImage=evil"]})))
    , testCase "a generated unit that would run the declared quadlet is installed, one made from another is not" $ do
        -- the shape `systemctl show --property=ExecStart --value` prints,
        -- written out by hand; the user-scope case reads the real one
        let shown fp = "{ path=/usr/bin/podman ; argv[]=/usr/bin/podman run --name=report --cidfile=/run/user/1000/report.cid --replace --rm --cgroups=split --label salmon.quadlet=" <> fp <> " docker.io/library/alpine:3 /bin/sh -c exit 0 ; ignore_errors=no ; start_time=[n/a] ; stop_time=[n/a] ; pid=0 ; code=(null) ; status=0/0 }\n"
        assertEqual "" Success (Quadlet.interpretGenerated "ztmC9FbSzVMf" (shown "ztmC9FbSzVMf"))
        assertBool "" (isFailure (Quadlet.interpretGenerated "ztmC9FbSzVMf" (shown "aTe0H65oUH_B")))
        assertBool "" (isFailure (Quadlet.interpretGenerated "ztmC9FbSzVMf" ""))
    , testCase "a stamped container job records its runs around the container, on the host" $
        assertEqual
            ""
            ( Text.unlines
                [ "[Unit]"
                , "Description=job report (salmon)"
                , ""
                , "[Container]"
                , "ContainerName=report"
                , "Image=docker.io/library/alpine:3"
                , "Exec=/bin/sh -c \"echo $$HOME 100%%\""
                , ""
                , "[Service]"
                , "Type=oneshot"
                , "Restart=no"
                , "ExecStartPre=touch \"/var/lib/report/last run.running\""
                , "ExecStartPost=mv -f \"/var/lib/report/last run.running\" \"/var/lib/report/last run\""
                ]
            )
            (Quadlet.renderContainer report{Quadlet.containerStamp = Just "/var/lib/report/last run"})
    , testCase "a container job with no stamp is described and keyed as before" $ do
        -- the hash of the text "a container job, key by key" pins, worked
        -- out outside this code
        assertEqual "" (Just ["image: docker.io/library/alpine:3", "quadlet: ztmC9FbSzVMf"]) (notesOf (containerNode report))
        assertEqual "" (Just (mkRef "systemd-unit" ("report.service" :: Text))) (refOf (containerNode report))
        assertEqual "" (refOf (containerNode report)) (refOf (containerNode stamped))
        assertBool "a stamp is not visible in the job's description" (notesOf (containerNode report) /= notesOf (containerNode stamped))
    , testCase "a stamp that is not an absolute path, or holds a line break, is refused" $ do
        assertEqual "" [] (Quadlet.containerProblems stamped)
        assertBool "" (not (null (Quadlet.containerProblems report{Quadlet.containerStamp = Just "stamp"})))
        assertBool "" (not (null (Quadlet.containerProblems report{Quadlet.containerStamp = Just "/a\nExecStart=/bin/evil"})))
    , testCase "a container job's run is judged against its quadlet, what it reads at start and its stamp" $
        assertEqual
            ""
            (Job.Completion Systemd.System "report.service" ["/etc/containers/systemd/report.container", "/etc/report/env", "/etc/report/conf"] (Just "/s"))
            (Quadlet.containerCompletion stamped{Quadlet.containerEnvFile = Just "/etc/report/env", Quadlet.containerWatched = ["/etc/report/conf"]})
    , testCase "a completed container job is the run's effect site, standing on the installed job" $ do
        let completed = Quadlet.completedQuadletJob silent ignoreTrack ignoreTrack stamped
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps completed)
        assertEqual "" (refOf (Job.runJob silent ignoreTrack Systemd.System "report.service")) (refOf completed)
        assertEqual "" ["file-contents", "podman-quadlet-dir", "podman-quadlet-job", "systemd-job-completed"] (sort [act.shorthand | act <- Map.elems (Dag.dagNodes dag)])
        assertEqual "" 0 (length (Dag.dagConflicts dag))
        assertEqual "" (Just ["newer than: /etc/containers/systemd/report.container", "stamp: /s"]) (notesOf completed)
    ]
  where
    stamped = report{Quadlet.containerStamp = Just "/s"}
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)

-------------------------------------------------------------------------------

verifyTests :: [TestTree]
verifyTests =
    [ testCase "systemd reads the job and its timer without complaint" $ withAnalyze $ withTempDir $ \dir -> do
        let j = (Job.job "salmon-verify-job" ["/bin/sh", "-c", "echo $HOME 100%; exit 0", ""]){Job.jobScope = Systemd.User, Job.jobUnitDir = dir, Job.jobEnvironment = [("A", "b c"), ("P", "1%")]}
            tm = (Job.jobTimer j [Job.OnCalendar "Mon..Fri *-*-* 03:00:00", Job.OnBootSec "15min"]){Job.timerPersistent = True, Job.timerRandomizedDelay = Just "10min", Job.timerAccuracy = Just "1s"}
        Text.writeFile (Job.jobServicePath j) (Job.renderJobService j)
        Text.writeFile (Job.timerPath tm) (Job.renderTimer tm)
        (code, out, err) <- readProcessWithExitCode "systemd-analyze" ["verify", "--user", "--recursive-errors=no", Job.jobServicePath j, Job.timerPath tm] ""
        let said = out <> err
        assertEqual said ExitSuccess code
        assertBool said (not ("salmon-verify-job" `isInfixOf` said))
    , testCase "systemd refuses a schedule that means nothing, which is what fails up" $ withAnalyze $ withTempDir $ \dir -> do
        let j = (Job.job "salmon-verify-bad" ["/bin/true"]){Job.jobScope = Systemd.User, Job.jobUnitDir = dir}
            tm = Job.jobTimer j [Job.OnCalendar "tuesdayish"]
        assertEqual "the pure check does not judge calendar expressions" [] (Job.timerProblems tm)
        Text.writeFile (Job.jobServicePath j) (Job.renderJobService j)
        Text.writeFile (Job.timerPath tm) (Job.renderTimer tm)
        (code, out, err) <- readProcessWithExitCode "systemd-analyze" ["verify", "--user", "--recursive-errors=no", Job.timerPath tm] ""
        assertBool (out <> err) (code /= ExitSuccess)
    ]
  where
    withAnalyze :: IO () -> IO ()
    withAnalyze act = do
        found <- findExecutable "systemd-analyze"
        case found of
            Just _ -> act
            Nothing -> hPutStrLn stderr "SKIPPED: no systemd-analyze on PATH; the rendered units were not read by systemd"

generatorPath :: FilePath
generatorPath = "/usr/libexec/podman/quadlet"

generatorTests :: [TestTree]
generatorTests =
    [ testCase "podman's generator turns the job into a foreground oneshot run" $ withGenerator $ withTempDir $ \dir -> do
        let c = report{Quadlet.containerUnitDir = dir}
        Text.writeFile (Quadlet.quadletPath c) =<< Quadlet.renderQuadlet c
        environment <- getEnvironment
        (code, out, err) <-
            readCreateProcessWithExitCode
                (proc generatorPath ["--user", "--dryrun"]){env = Just (("QUADLET_UNIT_DIRS", dir) : environment)}
                ""
        let said = out <> err
            start = [l | l <- lines said, "ExecStart=" `isInfixOf` l]
        assertEqual said ExitSuccess code
        assertBool said ("---report.service---" `isInfixOf` said)
        assertBool said (not ("unsupported key" `isInfixOf` said))
        assertBool said ("Type=oneshot" `elem` lines said)
        case start of
            [l] -> do
                -- detached, `systemctl start` would return before the command ran
                assertBool l (not (" -d " `isInfixOf` l))
                assertBool l ("docker.io/library/alpine:3 /bin/sh -c \"echo $$HOME 100%%\"" `isInfixOf` l)
            other -> assertFailure ("not exactly one ExecStart: " <> show other)
    , testCase "podman's generator keeps the stamp's two commands around its own" $ withGenerator $ withTempDir $ \dir -> do
        let c = report{Quadlet.containerUnitDir = dir, Quadlet.containerStamp = Just (dir </> "last run")}
        Text.writeFile (Quadlet.quadletPath c) =<< Quadlet.renderQuadlet c
        environment <- getEnvironment
        (code, out, err) <-
            readCreateProcessWithExitCode
                (proc generatorPath ["--user", "--dryrun"]){env = Just (("QUADLET_UNIT_DIRS", dir) : environment)}
                ""
        let said = out <> err
            execs = [takeWhile (/= '=') l | l <- lines said, "ExecStart" `isInfixOf` l]
        assertEqual said ExitSuccess code
        assertBool said (not ("unsupported key" `isInfixOf` said))
        assertEqual said ["ExecStart", "ExecStartPost", "ExecStartPre"] (sort execs)
        assertBool said (("ExecStartPre=touch \"" <> dir </> "last run.running\"") `elem` lines said)
        assertBool said (("ExecStartPost=mv -f \"" <> dir </> "last run.running\" \"" <> dir </> "last run\"") `elem` lines said)
    ]
  where
    withGenerator :: IO () -> IO ()
    withGenerator act = do
        present <- doesFileExist generatorPath
        if present
            then act
            else hPutStrLn stderr ("SKIPPED: no quadlet generator at " <> generatorPath <> "; the container job was not checked against podman")

report :: Quadlet.Container
report = Quadlet.containerJob (Podman.ContainerName "report") "docker.io/library/alpine:3" ["/bin/sh", "-c", "echo $HOME 100%"]

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

-------------------------------------------------------------------------------

userTests :: TestTree
userTests =
    sequentialTestGroup
        "Salmon.Builtin.Nodes.Systemd.Job (user scope, Layer 2)"
        AllFinish
        [ testCaseSteps "installed without running, scheduled, re-armed, removed" $ \step ->
            withUserUnits $ \unitDir -> withTempDir $ \tmp -> withFreshJob unitDir (tmp </> "out") $ \j -> lifecycle step (tmp </> "out") j
        , testCaseSteps "a run waits for the command and fails when it does" $ \step ->
            withUserUnits $ \unitDir -> withTempDir $ \tmp -> withFreshJob unitDir (tmp </> "out") $ \j -> running step (tmp </> "out") j
        , testCaseSteps "a stamped job runs once, again after a change, and fails while its last run did" $ \step ->
            withUserUnits $ \unitDir -> withTempDir $ \tmp -> withFreshJob unitDir (tmp </> "out") $ \j -> completing step tmp j
        , testCaseSteps "a job with a waiting timer is remembered by systemd alone" $ \step ->
            withUserUnits $ \unitDir -> withTempDir $ \tmp -> withFreshJob unitDir (tmp </> "out") $ \j -> remembered step (tmp </> "out") j
        , testCaseSteps "a launched job is started without being waited for, and found completed or failed later" $ \step ->
            withUserUnits $ \unitDir -> withTempDir $ \tmp -> withFreshJob unitDir (tmp </> "out") $ \j -> launched step tmp j
        ]

lifecycle :: (String -> IO ()) -> FilePath -> Job.Job -> IO ()
lifecycle step out j = do
    let tm = Job.jobTimer j [Job.OnCalendar "*-*-* 03:00:00"]
        nodeOf job' schedule = Job.scheduledJob silent ignoreTrack ignoreTrack job' [Job.OnCalendar schedule]
        node = nodeOf j

    step "first up"
    reports <- runUpCapturing (node "*-*-* 03:00:00")
    assertUp reports
    assertEqual "the timer was not applied once" 1 (count "systemd-timer" isEval reports)
    -- the job itself may be a skip already: asked about a file in its search
    -- path that it has not read yet, systemd reads it there and then
    assertEqual "the job node was not visited once" 1 (count "systemd-job" isEval reports + count "systemd-job" isSkip reports)
    assertBool "no service file" =<< doesFileExist (Job.jobServicePath j)
    assertBool "no timer file" =<< doesFileExist (Job.timerPath tm)
    assertEqual "" "loaded" =<< showProperty (Job.jobServiceTarget j) "LoadState"
    assertEqual "installing the job started it" "inactive" =<< showProperty (Job.jobServiceTarget j) "ActiveState"
    assertBool "installing the job ran it" . not =<< doesFileExist out
    assertEqual "the timer is not waiting" "active" =<< showProperty (Job.timerTarget tm) "ActiveState"
    assertEqual "the timer is not enabled" "enabled" =<< showProperty (Job.timerTarget tm) "UnitFileState"
    assertEqual "the timer triggers something else" (Job.jobServiceTarget j) =<< showProperty (Job.timerTarget tm) "Unit"
    calendar <- showProperty (Job.timerTarget tm) "TimersCalendar"
    assertBool (Text.unpack calendar) ("03:00:00" `Text.isInfixOf` calendar)

    step "second up"
    again <- runUpCapturing (node "*-*-* 03:00:00")
    assertUp again
    assertEqual "the job was not skipped" (1, 0) (count "systemd-job" isSkip again, count "systemd-job" isEval again)
    assertEqual "the timer was not skipped" (1, 0) (count "systemd-timer" isSkip again, count "systemd-timer" isEval again)

    step "a new schedule"
    -- systemd compares mtimes; a rewrite inside the same second is not a change to it
    void (readProcessWithExitCode "sleep" ["1.1"] "")
    moved <- runUpCapturing (node "*-*-* 04:30:00")
    assertUp moved
    assertEqual "the timer was not re-applied" 1 (count "systemd-timer" isEval moved)
    assertEqual "the job was touched by a change to its timer" 0 (count "systemd-job" isEval moved)
    rearmed <- showProperty (Job.timerTarget tm) "TimersCalendar"
    assertBool (Text.unpack rearmed) ("04:30:00" `Text.isInfixOf` rearmed)
    assertBool "re-arming the timer ran the job" . not =<< doesFileExist out

    step "a new command"
    -- the waiting timer keeps the job's unit loaded, so this one is a
    -- changed file systemd has to be told about
    void (readProcessWithExitCode "sleep" ["1.1"] "")
    let changed = j{Job.jobCommand = ["/bin/sh", "-c", "exit 0"]}
    recommanded <- runUpCapturing (nodeOf changed "*-*-* 04:30:00")
    assertUp recommanded
    assertEqual "the changed job was not re-installed" 1 (count "systemd-job" isEval recommanded)
    assertEqual "" "no" =<< showProperty (Job.jobServiceTarget j) "NeedDaemonReload"
    started <- showProperty (Job.jobServiceTarget j) "ExecStart"
    assertBool (Text.unpack started) ("exit 0" `Text.isInfixOf` started)
    assertBool "re-installing the job ran it" . not =<< doesFileExist out

    step "down"
    down <- runDownCapturing (nodeOf changed "*-*-* 04:30:00")
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    assertBool "the service file is still there" . not =<< doesFileExist (Job.jobServicePath j)
    assertBool "the timer file is still there" . not =<< doesFileExist (Job.timerPath tm)
    assertEqual "the job outlived its file" "not-found" =<< showProperty (Job.jobServiceTarget j) "LoadState"
    assertEqual "the timer outlived its file" "not-found" =<< showProperty (Job.timerTarget tm) "LoadState"
    assertEqual "the timer is still waiting" "inactive" =<< showProperty (Job.timerTarget tm) "ActiveState"
    linked <- doesFileExist (j.jobUnitDir </> "timers.target.wants" </> Text.unpack (Job.timerTarget tm))
    assertBool "the timer is still linked into timers.target" (not linked)
    assertBool "the shared unit directory was removed" =<< doesDirectoryExist j.jobUnitDir

    step "down again"
    twice <- runDownCapturing (nodeOf changed "*-*-* 04:30:00")
    assertBool ("a second down failed: " <> show (failures twice)) (null (failures twice))

running :: (String -> IO ()) -> FilePath -> Job.Job -> IO ()
running step out j = do
    let run job' = Job.runJob silent ignoreTrack job'.jobScope (Job.jobServiceTarget job') `inject` jobNode job'

    step "a run"
    reports <- runUpCapturing (run j)
    assertUp reports
    home <- getHomeDirectory
    wrote <- Text.readFile out
    -- $HOME is the shell's to expand, %h nobody's
    assertEqual "the command did not get its words as declared" [Text.pack home, "%h 100%"] (Text.lines wrote)
    assertEqual "" "success" =<< showProperty (Job.jobServiceTarget j) "Result"

    step "a second pass runs it again"
    removeFile out
    again <- runUpCapturing (run j)
    assertUp again
    assertBool "the job did not run" =<< doesFileExist out

    step "a failing command"
    void (readProcessWithExitCode "sleep" ["1.1"] "")
    let failing = j{Job.jobCommand = ["/bin/sh", "-c", "exit 3"]}
    failed <- runUpCapturing (run failing)
    assertEqual "the failed run was not a failed up" ["systemd-job-run"] [act.shorthand | UpDown.Failed act _ <- failed]
    assertEqual "" "3" =<< showProperty (Job.jobServiceTarget j) "ExecMainStatus"

    step "down"
    down <- runDownCapturing (run failing)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    assertEqual "" "not-found" =<< showProperty (Job.jobServiceTarget j) "LoadState"

completing :: (String -> IO ()) -> FilePath -> Job.Job -> IO ()
completing step tmp j0 = do
    let out = tmp </> "out"
        conf = tmp </> "conf"
        stamp = tmp </> "stamp"
        j = j0{Job.jobStamp = Just stamp}
        completion job' = let c = Job.jobCompletion job' in c{Job.completionWritten = c.completionWritten <> [conf]}
        node job' = Job.completedRun silent ignoreTrack (completion job') `inject` jobNode job'
        ran = "systemd-job-completed"
    Text.writeFile conf "v1"

    step "first up runs it"
    reports <- runUpCapturing (node j)
    assertUp reports
    assertEqual "the job was not run once" 1 (count ran isEval reports)
    assertBool "the job did not run" =<< doesFileExist out
    assertBool "the run left no stamp" =<< doesFileExist stamp
    assertBool "the run left its running stamp" . not =<< doesFileExist (Job.stampRunning stamp)
    -- with nothing referring to the unit systemd has already forgotten the run
    assertEqual "" "" =<< showProperty (Job.jobServiceTarget j) "ExecMainStartTimestamp"
    assertEqual "" Completed =<< Job.checkLastRun (completion j)

    step "second up skips it"
    removeFile out
    again <- runUpCapturing (node j)
    assertUp again
    assertEqual "the job was not skipped" (1, 0) (count ran isSkip again, count ran isEval again)
    assertBool "a completed job was run again" . not =<< doesFileExist out

    step "a file it stands on is written"
    Text.writeFile conf "v2"
    assertBool "a run older than its configuration counted" . isFailure =<< Job.checkLastRun (completion j)
    rerun <- runUpCapturing (node j)
    assertUp rerun
    assertEqual "the job was not run again" 1 (count ran isEval rerun)
    assertBool "the job did not run" =<< doesFileExist out
    assertEqual "" Completed =<< Job.checkLastRun (completion j)

    step "a failing command fails the pass"
    void (readProcessWithExitCode "sleep" ["1.1"] "")
    let failing = j{Job.jobCommand = ["/bin/sh", "-c", "exit 3"]}
    failed <- runUpCapturing (node failing)
    assertEqual "the failed run was not a failed up" [ran] [act.shorthand | UpDown.Failed act _ <- failed]
    verdict <- Job.checkLastRun (completion failing)
    case verdict of
        Failure why -> assertBool (Text.unpack why) ("exit status 3" `Text.isInfixOf` why)
        other -> assertFailure ("a failed job reads as " <> show other)

    step "and every pass after it"
    stillFailed <- runUpCapturing (node failing)
    assertEqual "a failed job was reported done" [ran] [act.shorthand | UpDown.Failed act _ <- stillFailed]

    step "also once systemd has forgotten the failure"
    void (readProcessWithExitCode "systemctl" ["--user", "reset-failed", Text.unpack (Job.jobServiceTarget j)] "")
    assertEqual "" "success" =<< showProperty (Job.jobServiceTarget j) "Result"
    assertBool "the failure was forgotten with systemd's record of it" . isFailure =<< Job.checkLastRun (completion failing)

    step "a run that succeeds clears it"
    void (readProcessWithExitCode "sleep" ["1.1"] "")
    fixed <- runUpCapturing (node j)
    assertUp fixed
    assertEqual "" Completed =<< Job.checkLastRun (completion j)

    step "down"
    down <- runDownCapturing (node j)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    assertBool "the stamp is still there" . not =<< doesFileExist stamp
    assertEqual "" "not-found" =<< showProperty (Job.jobServiceTarget j) "LoadState"
    assertBool "a job that is gone reads as run" . isFailure =<< Job.checkLastRun (completion j)

remembered :: (String -> IO ()) -> FilePath -> Job.Job -> IO ()
remembered step out j = do
    let node = Job.completedScheduledJob silent ignoreTrack ignoreTrack j [Job.OnCalendar "*-*-* 03:00:00"]
        ran = "systemd-job-completed"

    step "first up runs it"
    reports <- runUpCapturing node
    assertUp reports
    assertEqual "the job was not run once" 1 (count ran isEval reports)
    assertBool "the job did not run" =<< doesFileExist out
    started <- showProperty (Job.jobServiceTarget j) "ExecMainStartTimestamp"
    assertBool "systemd forgot a run its timer refers to" (not (Text.null started))
    assertEqual "" Completed =<< Job.checkLastRun (Job.jobCompletion j)

    step "second up skips it"
    removeFile out
    again <- runUpCapturing node
    assertUp again
    assertEqual "the job was not skipped" (1, 0) (count ran isSkip again, count ran isEval again)
    assertBool "a completed job was run again" . not =<< doesFileExist out

    step "down"
    down <- runDownCapturing node
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    assertEqual "" "not-found" =<< showProperty (Job.jobServiceTarget j) "LoadState"

launched :: (String -> IO ()) -> FilePath -> Job.Job -> IO ()
launched step tmp j0 = do
    let out = tmp </> "out"
        tries = tmp </> "tries"
        conf = tmp </> "conf"
        stamp = tmp </> "stamp"
        sh script = ["/bin/sh", "-c", script]
        j = j0{Job.jobStamp = Just stamp, Job.jobCommand = sh ("sleep 3; echo run >> " <> Text.pack out)}
        failing = j{Job.jobCommand = sh ("echo try >> " <> Text.pack tries <> "; sleep 1; exit 3")}
        completion job' = let c = Job.jobCompletion job' in c{Job.completionWritten = c.completionWritten <> [conf]}
        node polling job' = Job.launchedRun silent ignoreTrack polling (completion job') `inject` jobNode job'
        ran = "systemd-job-launched"
        unit = Job.jobServiceTarget j
        linesOf path = do
            present <- doesFileExist path
            if present then length . Text.lines <$> Text.readFile path else pure (0 :: Int)
        failedWith name reports = [act.shorthand | UpDown.Failed act e <- reports, name `isInfixOf` show e]
        hold = Job.Polling{Job.pollingWait = 30, Job.pollingEvery = 1}
    Text.writeFile conf "v1"

    step "first up starts it and returns"
    reports <- runUpCapturing (node Job.noWait j)
    assertEqual ("the pass did not say the job is in progress: " <> show (failures reports)) [ran] (failedWith "RunInProgress" reports)
    assertEqual "something else failed" 1 (length (failures reports))
    assertEqual "up waited for the job" 0 =<< linesOf out
    waitFor "the run to show" (doesFileExist (Job.stampRunning stamp))
    assertEqual "" "activating" =<< showProperty unit "ActiveState"
    assertEqual "a run under way is not a verdict" Unknown =<< Job.checkLastRun (completion j)
    assertEqual "" Job.Running =<< Job.readRunState (completion j)
    began <- getModificationTime (Job.stampRunning stamp)

    step "a pass while it runs starts nothing"
    again <- runUpCapturing (node Job.noWait j)
    assertEqual ("the pass did not say the job is in progress: " <> show (failures again)) [ran] (failedWith "RunInProgress" again)
    assertEqual "the job was started over" began =<< getModificationTime (Job.stampRunning stamp)

    step "a pass after it ended finds it completed"
    waitFor "the run to end" (doesFileExist stamp)
    done <- runUpCapturing (node Job.noWait j)
    assertUp done
    assertEqual "the job was not skipped" (1, 0) (count ran isSkip done, count ran isEval done)
    assertEqual "the job ran more than once" 1 =<< linesOf out
    assertEqual "" Completed =<< Job.checkLastRun (completion j)

    step "a bounded wait sees a run through"
    Text.writeFile conf "v2"
    waited <- runUpCapturing (node hold j)
    assertUp waited
    assertEqual "the job was not run again" 1 (count ran isEval waited)
    assertEqual "up returned before the run ended" 2 =<< linesOf out
    assertEqual "" Job.Succeeded =<< Job.readRunState (completion j)

    step "a failing job is started like any other"
    void (readProcessWithExitCode "sleep" ["1.1"] "")
    first <- runUpCapturing (node Job.noWait failing)
    assertEqual ("the pass did not say the job is in progress: " <> show (failures first)) [ran] (failedWith "RunInProgress" first)

    step "and a later pass finds it failed, without running it again"
    waitFor "the run to fail" ((== "failed") <$> showProperty unit "ActiveState")
    broken <- runUpCapturing (node Job.noWait failing)
    assertEqual ("the failed run was not reported: " <> show (failures broken)) [ran] (failedWith "RunFailed" broken)
    assertBool (show (failures broken)) (any ("exit status 3" `isInfixOf`) (failures broken))
    assertEqual "a failed job was run again" 1 =<< linesOf tries

    step "also once systemd has forgotten the failure"
    void (readProcessWithExitCode "systemctl" ["--user", "reset-failed", Text.unpack unit] "")
    forgot <- runUpCapturing (node Job.noWait failing)
    assertEqual ("the failed run was not reported: " <> show (failures forgot)) [ran] (failedWith "RunFailed" forgot)
    assertEqual "a failed job was run again" 1 =<< linesOf tries

    step "a file written since has it run again"
    Text.writeFile conf "v3"
    retried <- runUpCapturing (node hold failing)
    assertEqual ("the second failure was not reported: " <> show (failures retried)) [ran] (failedWith "RunFailed" retried)
    assertEqual "the job was not tried again" 2 =<< linesOf tries

    step "a repaired job runs and completes"
    void (readProcessWithExitCode "sleep" ["1.1"] "")
    fixed <- runUpCapturing (node hold j)
    assertUp fixed
    assertEqual "" 3 =<< linesOf out
    assertEqual "" Completed =<< Job.checkLastRun (completion j)

    step "down"
    down <- runDownCapturing (node Job.noWait j)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    assertBool "the stamp is still there" . not =<< doesFileExist stamp
    assertEqual "" "not-found" =<< showProperty unit "LoadState"

-- | Asks every tenth of a second, for twenty seconds at most.
waitFor :: String -> IO Bool -> IO ()
waitFor what ask = go (200 :: Int)
  where
    go 0 = assertFailure ("waited twenty seconds for " <> what)
    go n = do
        there <- ask
        if there then pure () else threadDelay 100000 >> go (n - 1)

assertUp :: [UpDown.Report Extension] -> IO ()
assertUp reports = do
    assertBool ("up failed: " <> show (failures reports)) (null (failures reports))
    assertEqual "nothing was blocked" 0 (length [() | UpDown.Blocked _ <- reports])

failures :: [UpDown.Report Extension] -> [String]
failures reports = [Text.unpack act.shorthand <> ": " <> show e | UpDown.Failed act e <- reports]

count :: Text -> (UpDown.Report Extension -> Maybe (Act Extension)) -> [UpDown.Report Extension] -> Int
count name which reports = length [() | Just act <- map which reports, act.shorthand == name]

isEval, isSkip :: UpDown.Report Extension -> Maybe (Act Extension)
isEval (UpDown.Eval act) = Just act
isEval _ = Nothing
isSkip (UpDown.Skip act) = Just act
isSkip _ = Nothing

showProperty :: Systemd.UnitTarget -> String -> IO Text
showProperty target property = do
    (_, out, _) <- readProcessWithExitCode "systemctl" ["--user", "show", Text.unpack target, "--property=" <> property, "--value"] ""
    pure (Text.strip (Text.pack out))

{- | Runs the action with the user's unit directory, or skips loudly. The
directory has to be there already: making @~\/.config\/systemd\/user@ on
somebody's machine is more than a test should do.
-}
withUserUnits :: (FilePath -> IO ()) -> IO ()
withUserUnits act = do
    uid <- getRealUserID
    (managerCode, managerOut, _) <- readProcessWithExitCode "systemctl" ["--user", "is-system-running"] ""
    let manager = managerCode == ExitSuccess || Text.strip (Text.pack managerOut) `elem` ["degraded", "starting"]
    dir <- getXdgDirectory XdgConfig ("systemd" </> "user")
    present <- doesDirectoryExist dir
    case () of
        _
            | uid == 0 -> skip "running as root; this test is the user-scope one"
            | not manager -> skip "no `systemd --user` manager is answering"
            | not present -> skip (dir <> " does not exist")
            | otherwise -> act dir
  where
    skip why = hPutStrLn stderr ("SKIPPED: " <> why <> "; the job nodes were not run against a real systemd")

{- | A job under a name nothing real has, writing two lines to @out@, and a
cleanup that does not trust the nodes' own @down@.
-}
withFreshJob :: FilePath -> FilePath -> (Job.Job -> IO a) -> IO a
withFreshJob unitDir out = bracket fresh cleanup
  where
    fresh = do
        now <- getPOSIXTime
        pid <- getProcessID
        let name = Text.pack ("salmon-job-test-" <> showHex (floor (now * 1000000) :: Integer) "" <> "-" <> show pid)
            script = "echo \"$HOME\" > " <> Text.pack out <> "; echo %h 100% >> " <> Text.pack out
        pure (Job.job name ["/bin/sh", "-c", script]){Job.jobScope = Systemd.User, Job.jobUnitDir = unitDir}
    cleanup j = do
        let quiet cmd args = void (readProcessWithExitCode cmd args "")
            service = Text.unpack (Job.jobServiceTarget j)
            timerName = Text.unpack j.jobName <> ".timer"
        quiet "systemctl" ["--user", "disable", "--now", timerName]
        quiet "systemctl" ["--user", "stop", service]
        forM_ [Job.jobServicePath j, unitDir </> timerName, unitDir </> "timers.target.wants" </> timerName] $ \path -> do
            present <- doesFileExist path
            when present (removeFile path)
        quiet "systemctl" ["--user", "daemon-reload"]
        quiet "systemctl" ["--user", "reset-failed", service]
