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
whatever the case did.

Not run anywhere: system scope, a timer actually firing, and a container job
actually running (the generator's output is as far as that goes).
-}
module Test.SystemdJobSpec (tests, userTests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgConfig), doesDirectoryExist, doesFileExist, findExecutable, getHomeDirectory, getXdgDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import System.Posix.User (getRealUserID)
import System.Process (CmdSpec (..), CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, testCaseSteps)

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
        , testGroup "nodes" nodeTests
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
    ]

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
