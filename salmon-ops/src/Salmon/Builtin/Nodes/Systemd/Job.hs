{-# LANGUAGE OverloadedStrings #-}

{- | Jobs under systemd: a command that runs to completion, and what runs it.

'Salmon.Builtin.Nodes.Systemd.systemdService' declares a /service/: a unit
that is enabled, started and expected to stay @active@. A job is the other
kind of unit and none of that fits it. A @Type=oneshot@ service is
@inactive@ whenever it is not running, which is nearly always, so a check
that wants @ActiveState=active@ calls a healthy job broken and an @up@ that
restarts the unit runs the job at every pass.

So there are three nodes here, and they are separate on purpose:

* 'jobService' /installs/ a job: the unit file is written and systemd knows
  it as written. Nothing starts it. Its check is
  'Salmon.Builtin.Nodes.Systemd.checkLoaded'.
* 'timerUnit' /schedules/ a unit: a @.timer@ that is enabled and waiting. A
  timer is long-lived, so its check is the service one
  ('Salmon.Builtin.Nodes.Systemd.interpretShow': active, enabled, loaded as
  written). 'scheduledJob' is the two together.
* 'runJob' /runs/ a job now and waits for it, failing if the job fails.

The container equivalent of 'jobService' is
'Salmon.Builtin.Nodes.Podman.Quadlet.quadletJob', whose generated service a
'timerUnit' or a 'runJob' names like any other.

Changing a schedule is a changed timer file. systemd re-arms a waiting timer
with the new schedule at @daemon-reload@ (seen on systemd 255: the next
elapse moves without a restart), so it does not matter which node's reload
got there first.

How the last run /went/ is a fourth node's question, not 'jobService''s
(re-installing a job does not un-fail it, and a check that said otherwise
would have a supervisor reload systemd forever). 'completedRun' is 'runJob'
with a check: it reads the unit's last run from systemd ('interpretLastRun')
and answers 'Completed' when that run succeeded and started after everything
the job stands on was last written, a 'Failure' when it failed, never
happened or predates a write. Its @up@ is 'runJob''s, so a job that fails
fails the pass. 'completedJob' and 'completedScheduledJob' are that node
standing on the job.

What systemd remembers is the limit of it, and it is less than one would
think (all seen on systemd 255). A /failed/ run is remembered until the unit
is started again, @reset-failed@ or the machine reboots. A /successful/ run
is remembered only while something keeps the unit loaded: a waiting timer
that triggers it does, and with nothing referring to it the unit is
collected the moment the run ends and reads exactly like one never run. And
nothing survives a reboot. 'jobStamp' is for the cases that leaves (a job
with no timer; a verdict that must hold across a reboot): the unit then
records its own runs in two files the check reads beside systemd's answer.

'runJob' and 'completedRun' /wait/: for a job of half an hour that is one
call held for half an hour, and over ssh a dropped link fails the pass
although the unit goes on and ends well. 'launchedRun' is the same
declaration ("this job has run to completion") for such a job: its @up@
queues the start and returns, and the answer comes from asking again. A pass
that finds the job running starts nothing and says so ('RunInProgress');
see 'launchedRun' for what that looks like to a driver.
-}
module Salmon.Builtin.Nodes.Systemd.Job (
    -- * a job
    Job (..),
    job,
    systemUnitDir,
    jobServiceTarget,
    jobServicePath,
    renderJobService,
    jobProblems,
    jobService,

    -- * a schedule
    Schedule (..),
    Timer (..),
    timer,
    jobTimer,
    timerTarget,
    timerPath,
    renderTimer,
    timerProblems,
    timerUnit,
    scheduledJob,

    -- * running one now
    runJob,

    -- * a run that succeeded, and since when
    Completion (..),
    jobCompletion,
    stampRunning,
    RunEvidence (..),
    noEvidence,
    interpretLastRun,
    parseTimestamp,
    lastRunProperties,
    checkLastRun,
    completedRun,
    completedJob,
    completedScheduledJob,

    -- * a run started and asked about later
    RunState (..),
    classifyRun,
    runVerdict,
    runStateProperties,
    readRunState,
    Launch (..),
    launchDecision,
    Outcome (..),
    runOutcome,
    Polling (..),
    noWait,
    pollDelays,
    launchProblems,
    launchNotes,
    CannotLaunch (..),
    RunInProgress (..),
    RunFailed (..),
    RunUndetermined (..),
    launchedRun,
    launchedJob,

    -- * shared with other unit-writing nodes
    unitDirectory,
    unitFile,
    reloadAndRequireLoaded,
    stopIfKnown,
    unitNameProblems,
    InvalidUnit (..),
    UnitNotLoaded (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.Char (isAlphaNum, isAscii)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.Process (proc, readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.OpGraph (OpGraph (..), inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

{- | A command run to completion as @NAME.service@. 'job' is the starting
point; set the rest with record update.
-}
data Job
    = Job
    { jobScope :: Systemd.Scope
    , jobUnitDir :: FilePath
    -- ^ 'systemUnitDir' for 'Systemd.System'; a caller-resolved
    -- @~\/.config\/systemd\/user@ for 'Systemd.User'
    , jobName :: Text
    -- ^ names the unit, @NAME.service@
    , jobDescription :: Text
    , jobAfter :: [Systemd.UnitTarget]
    , jobCommand :: [Text]
    -- ^ the argv, program first as an absolute path. Each word reaches the
    -- program as written ('Systemd.literalArg'): systemd expands no @$VAR@
    -- and no @%@ specifier in it.
    , jobUser :: Maybe Text
    -- ^ who runs it; 'Nothing' is the manager's own user (root in
    -- 'Systemd.System'). Not rendered in 'Systemd.User' scope, where
    -- systemd refuses it.
    , jobGroup :: Maybe Text
    , jobWorkingDir :: Maybe FilePath
    , jobEnvironment :: [(Text, Text)]
    , jobEnvFile :: Maybe FilePath
    -- ^ an @EnvironmentFile=@ somebody else provisions, read at each run
    , jobTimeout :: Maybe Int
    -- ^ @TimeoutStartSec=@ in seconds: how long one run may take. systemd's
    -- default for a oneshot unit is no limit at all.
    , jobStamp :: Maybe FilePath
    -- ^ a file the unit keeps as the record of its last successful run, for
    -- 'completedRun' to read where systemd's own memory does not reach (see
    -- the module's header). Each run touches @STAMP.running@ before the
    -- command and renames it to @STAMP@ after the command succeeded, so
    -- @STAMP@'s time is when the last successful run /started/, and a
    -- @STAMP.running@ newer than it is a run that did not succeed. The
    -- directory is the caller's: it has to exist and be writable by whoever
    -- runs the job. @touch@ and @mv@ are found by systemd on its own search
    -- path.
    }
    deriving (Eq, Show)

-- | Where an administrator's system units live.
systemUnitDir :: FilePath
systemUnitDir = "/etc/systemd/system"

-- | A system-scope job run as root with no limit on how long it takes.
job :: Text -> [Text] -> Job
job name command =
    Job
        { jobScope = Systemd.System
        , jobUnitDir = systemUnitDir
        , jobName = name
        , jobDescription = "job " <> name <> " (salmon)"
        , jobAfter = []
        , jobCommand = command
        , jobUser = Nothing
        , jobGroup = Nothing
        , jobWorkingDir = Nothing
        , jobEnvironment = []
        , jobEnvFile = Nothing
        , jobTimeout = Nothing
        , jobStamp = Nothing
        }

jobServiceTarget :: Job -> Systemd.UnitTarget
jobServiceTarget j = j.jobName <> ".service"

jobServicePath :: Job -> FilePath
jobServicePath j = j.jobUnitDir </> Text.unpack (jobServiceTarget j)

{- | The unit file. There is no @[Install]@ section: nothing starts a job at
boot, a timer or an operator does.
-}
renderJobService :: Job -> Text
renderJobService j =
    Text.unlines $
        mconcat
            [ ["[Unit]", "Description=" <> j.jobDescription]
            , ["After=" <> Text.unwords j.jobAfter | not (null j.jobAfter)]
            , ["", "[Service]", "Type=oneshot"]
            , case j.jobScope of
                Systemd.System ->
                    ["User=" <> u | u <- maybeToList j.jobUser] <> ["Group=" <> g | g <- maybeToList j.jobGroup]
                Systemd.User -> []
            , ["WorkingDirectory=" <> Text.pack d | d <- maybeToList j.jobWorkingDir]
            , ["Environment=" <> assignment k v | (k, v) <- j.jobEnvironment]
            , ["EnvironmentFile=" <> Text.pack f | f <- maybeToList j.jobEnvFile]
            , ["TimeoutStartSec=" <> Text.pack (show t) | t <- maybeToList j.jobTimeout]
            , ["ExecStartPre=" <> command ["touch", Text.pack (stampRunning s)] | s <- maybeToList j.jobStamp]
            , ["ExecStart=" <> command j.jobCommand]
            , -- only reached when every ExecStart= exited 0
              ["ExecStartPost=" <> command ["mv", "-f", Text.pack (stampRunning s), Text.pack s] | s <- maybeToList j.jobStamp]
            ]
  where
    command :: [Text] -> Text
    command = Text.unwords . map Systemd.literalArg

    -- specifiers are expanded in @Environment=@, variables are not
    assignment :: Text -> Text -> Text
    assignment k v = Systemd.quoteArg (k <> "=" <> Text.replace "%" "%%" v)

{- | Why this job cannot be written, if it cannot. Every value lands on a
line of a file systemd parses, so a line break inside one is another key.
-}
jobProblems :: Job -> [Text]
jobProblems j =
    mconcat
        [ unitNameProblems "job" j.jobName
        , case j.jobCommand of
            [] -> ["the job has no command"]
            (program : _)
                | not ("/" `Text.isPrefixOf` program) ->
                    ["the job's program is not an absolute path: " <> program]
                | otherwise -> []
        , ["an environment variable with no name" | (k, _) <- j.jobEnvironment, Text.null k]
        , ["not an environment variable name: " <> k | (k, _) <- j.jobEnvironment, Text.any (`elem` ("= \t" :: String)) k]
        , ["the timeout is not positive" | Just t <- [j.jobTimeout], t <= 0]
        , ["the stamp is not an absolute path: " <> Text.pack s | s <- maybeToList j.jobStamp, not (isAbsolute s)]
        , lineBreaks fields
        ]
  where
    fields :: [(Text, Text)]
    fields =
        mconcat
            [ [("description", j.jobDescription)]
            , [("after", a) | a <- j.jobAfter]
            , [("command", w) | w <- j.jobCommand]
            , [("user", u) | u <- maybeToList j.jobUser]
            , [("group", g) | g <- maybeToList j.jobGroup]
            , [("working directory", Text.pack d) | d <- maybeToList j.jobWorkingDir]
            , [("environment", k <> v) | (k, v) <- j.jobEnvironment]
            , [("environment file", Text.pack f) | f <- maybeToList j.jobEnvFile]
            , [("stamp", Text.pack s) | s <- maybeToList j.jobStamp]
            ]

-- | The file a run touches when it starts; see 'jobStamp'.
stampRunning :: FilePath -> FilePath
stampRunning stamp = stamp <> ".running"

lineBreaks :: [(Text, Text)] -> [Text]
lineBreaks fields = ["a line break in the " <> what | (what, value) <- fields, Text.any (`elem` ("\n\r" :: String)) value]

-- | What stops a name from being the stem of a unit (and of its file).
unitNameProblems :: Text -> Text -> [Text]
unitNameProblems what name
    | Text.null name = ["the " <> what <> " has no name"]
    | Text.all allowed name = []
    | otherwise = ["the " <> what <> " name is not a unit name: " <> name]
  where
    allowed c = (isAscii c && isAlphaNum c) || c `elem` (":_.-@\\" :: String)

data InvalidUnit = InvalidUnit !FilePath ![Text]
    deriving (Show)

instance Exception InvalidUnit

-- | systemd was reloaded and still does not hold the unit as a usable one.
data UnitNotLoaded = UnitNotLoaded !Systemd.UnitTarget !Text
    deriving (Show)

instance Exception UnitNotLoaded

-------------------------------------------------------------------------------

{- | Installs the job: writes @NAME.service@ and has systemd load it. It
does not run it.

@up@ is @daemon-reload@, then systemd is asked again and anything but a
loaded unit is thrown ('UnitNotLoaded'): a file systemd refuses
(@bad-setting@) would otherwise be an @up@ that succeeds at every pass.
@down@ stops the job if it is running, and the file's own @down@ removes the
file and reloads. The unit directory is created if missing and never removed
('unitDirectory').

The 'Track'' is what the job stands on: the program it runs, its user, its
environment file. The node's 'Ref' is the unit's (@"systemd-unit"@
@NAME.service@), the effect site an authored service of that name has.
-}
jobService ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Job ->
    Job ->
    Op
jobService r systemctl t j =
    withSystemctl r systemctl (Systemd.DaemonReload j.jobScope) $ \reload ->
        withSystemctl r systemctl (Systemd.Stop j.jobScope target) $ \stop ->
            op "systemd-job" (deps [file, run t j]) $ \actions ->
                actions
                    { help = "installs the job " <> target <> " without running it"
                    , notes = ["unit: " <> FS.hashBytes (Text.encodeUtf8 rendered)]
                    , ref = mkRef "systemd-unit" target
                    , check = Systemd.checkLoaded j.jobScope target
                    , up = reloadAndRequireLoaded j.jobScope target reload
                    , down = stopIfKnown j.jobScope target stop
                    }
  where
    target = jobServiceTarget j
    rendered = renderJobService j
    file = unitFile r j.jobScope (jobServicePath j) (jobProblems j) rendered

-- | @reload@, then throw unless systemd now holds the unit as written.
reloadAndRequireLoaded :: Systemd.Scope -> Systemd.UnitTarget -> IO () -> IO ()
reloadAndRequireLoaded scope target reload = do
    reload
    loaded <- Systemd.checkLoaded scope target
    case loaded of
        Failure why -> throwIO (UnitNotLoaded target why)
        _ -> pure ()

{- | Runs @stop@ unless systemd has never heard of the unit: @systemctl stop@
exits 5 for one, and a @down@ that throws for a job that was never installed
blocks the teardown of everything under it.
-}
stopIfKnown :: Systemd.Scope -> Systemd.UnitTarget -> IO () -> IO ()
stopIfKnown scope target stop = do
    loaded <- Systemd.checkLoaded scope target
    unless (loaded == Failure unknownUnit) stop
  where
    unknownUnit = case Systemd.interpretLoaded ["LoadState=not-found"] of
        Failure why -> why
        _ -> ""

{- | 'Systemd.unitDirectory': one node per unit directory, shared with the
authored services of "Salmon.Builtin.Nodes.Systemd"; created if missing,
left alone by @down@.
-}
unitDirectory :: FilePath -> Op
unitDirectory = Systemd.unitDirectory

{- | A unit file: 'FS.filecontents' in a 'unitDirectory', refusing to write
when there are problems, and reloading systemd after its @down@ removed the
file (a unit outlives its file until the next reload).
-}
unitFile :: Reporter Systemd.Report -> Systemd.Scope -> FilePath -> [Text] -> Text -> Op
unitFile r scope path problems contents =
    let file = FS.filecontents (FS.FileContents path contents)
     in file{node = fmap own file.node, predecessors = deps [unitDirectory (takeDirectory path)]}
  where
    own :: Extension -> Extension
    own ext =
        ext
            { up = do
                unless (null problems) $ throwIO (InvalidUnit path problems)
                ext.up
            , down = do
                ext.down
                Binary.untrackedExec
                    Systemd.callSystemctl
                    (Systemd.DaemonReload scope)
                    ""
                    (contramap (Systemd.CallSystemCtl (Systemd.DaemonReload scope)) r)
            }

withSystemctl ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Systemd.SystemCtlCall ->
    (IO () -> Op) ->
    Op
withSystemctl r systemctl cmd f =
    let
        g :: (Reporter Binary.Report -> IO ()) -> Op
        g callbin = f (callbin (contramap (Systemd.CallSystemCtl cmd) r))
     in
        withBinary systemctl Systemd.callSystemctl cmd g

-------------------------------------------------------------------------------

{- | When a timer fires. The texts are systemd's own (@systemd.time(7)@): a
calendar expression for 'OnCalendar' (@"*-*-* 03:00:00"@, @"Mon..Fri 08:00"@,
@"hourly"@), a time span for the others (@"15min"@, @"1h 30min"@).
-}
data Schedule
    = -- | at these wall-clock times
      OnCalendar Text
    | -- | this long after the machine booted
      OnBootSec Text
    | -- | this long after the manager started (the user's login, in user scope)
      OnStartupSec Text
    | -- | this long after the triggered unit was last /started/
      OnUnitActiveSec Text
    | -- | this long after the triggered unit last /finished/
      OnUnitInactiveSec Text
    deriving (Eq, Ord, Show)

{- | @NAME.timer@, starting 'timerTriggers' on a schedule. 'timer' and
'jobTimer' are the starting points.
-}
data Timer
    = Timer
    { timerScope :: Systemd.Scope
    , timerUnitDir :: FilePath
    , timerName :: Text
    , timerDescription :: Text
    , timerTriggers :: Systemd.UnitTarget
    -- ^ written out as @Unit=@ even when it is the default @NAME.service@
    , timerSchedules :: [Schedule]
    -- ^ any of them firing starts the unit
    , timerPersistent :: Bool
    -- ^ @Persistent=@: a calendar run missed while the machine was off is
    -- made up at the next boot. Only 'OnCalendar' schedules are affected.
    , timerRandomizedDelay :: Maybe Text
    -- ^ @RandomizedDelaySec=@, a time span: so a fleet does not fire at once
    , timerAccuracy :: Maybe Text
    -- ^ @AccuracySec=@; systemd's default is a minute
    , timerWantedBy :: Systemd.UnitTarget
    }
    deriving (Eq, Show)

-- | A system-scope timer, started with @timers.target@, that makes up no missed run.
timer :: Text -> Systemd.UnitTarget -> [Schedule] -> Timer
timer name triggers schedules =
    Timer
        { timerScope = Systemd.System
        , timerUnitDir = systemUnitDir
        , timerName = name
        , timerDescription = "runs " <> triggers <> " (salmon)"
        , timerTriggers = triggers
        , timerSchedules = schedules
        , timerPersistent = False
        , timerRandomizedDelay = Nothing
        , timerAccuracy = Nothing
        , timerWantedBy = "timers.target"
        }

-- | The timer for a 'Job': same name, scope and directory.
jobTimer :: Job -> [Schedule] -> Timer
jobTimer j schedules =
    (timer j.jobName (jobServiceTarget j) schedules)
        { timerScope = j.jobScope
        , timerUnitDir = j.jobUnitDir
        }

timerTarget :: Timer -> Systemd.UnitTarget
timerTarget tm = tm.timerName <> ".timer"

timerPath :: Timer -> FilePath
timerPath tm = tm.timerUnitDir </> Text.unpack (timerTarget tm)

renderTimer :: Timer -> Text
renderTimer tm =
    Text.unlines $
        mconcat
            [ ["[Unit]", "Description=" <> tm.timerDescription]
            , ["", "[Timer]", "Unit=" <> tm.timerTriggers]
            , map schedule tm.timerSchedules
            , ["Persistent=true" | tm.timerPersistent]
            , ["RandomizedDelaySec=" <> d | d <- maybeToList tm.timerRandomizedDelay]
            , ["AccuracySec=" <> a | a <- maybeToList tm.timerAccuracy]
            , ["", "[Install]", "WantedBy=" <> tm.timerWantedBy]
            ]
  where
    schedule :: Schedule -> Text
    schedule (OnCalendar e) = "OnCalendar=" <> e
    schedule (OnBootSec s) = "OnBootSec=" <> s
    schedule (OnStartupSec s) = "OnStartupSec=" <> s
    schedule (OnUnitActiveSec s) = "OnUnitActiveSec=" <> s
    schedule (OnUnitInactiveSec s) = "OnUnitInactiveSec=" <> s

{- | Why this timer cannot be written. Whether a calendar expression or a
time span /means/ anything is systemd's to say: a timer it cannot read fails
to start, which fails @up@.
-}
timerProblems :: Timer -> [Text]
timerProblems tm =
    mconcat
        [ unitNameProblems "timer" tm.timerName
        , ["the timer has no schedule" | null tm.timerSchedules]
        , ["an empty schedule" | s <- tm.timerSchedules, Text.null (Text.strip (scheduleText s))]
        , ["the timer triggers nothing" | Text.null tm.timerTriggers]
        , ["the timer triggers itself" | tm.timerTriggers == timerTarget tm]
        , lineBreaks fields
        ]
  where
    scheduleText :: Schedule -> Text
    scheduleText (OnCalendar e) = e
    scheduleText (OnBootSec s) = s
    scheduleText (OnStartupSec s) = s
    scheduleText (OnUnitActiveSec s) = s
    scheduleText (OnUnitInactiveSec s) = s

    fields :: [(Text, Text)]
    fields =
        mconcat
            [ [("description", tm.timerDescription), ("triggered unit", tm.timerTriggers), ("wanted-by", tm.timerWantedBy)]
            , [("schedule", scheduleText s) | s <- tm.timerSchedules]
            , [("randomized delay", d) | d <- maybeToList tm.timerRandomizedDelay]
            , [("accuracy", a) | a <- maybeToList tm.timerAccuracy]
            ]

{- | Installs the timer and keeps it waiting: written, enabled, started.

The 'Track'' is where the caller says what it triggers (a 'jobService', a
'Salmon.Builtin.Nodes.Podman.Quadlet.quadletJob'); 'scheduledJob' does that
for a 'Job'. A timer naming a unit systemd does not know refuses to start,
so the triggered unit has to be there first.

@up@ is reload, enable, restart; restarting a timer runs nothing, it re-arms
it. @down@ is @disable --now@ (the triggered unit is left to its own node),
then the file's @down@ removes the file and reloads.
-}
timerUnit ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Timer ->
    Timer ->
    Op
timerUnit r systemctl t tm =
    withSystemctl r systemctl (Systemd.DaemonReload tm.timerScope) $ \reload ->
        withSystemctl r systemctl (Systemd.Enable tm.timerScope target) $ \enable ->
            withSystemctl r systemctl (Systemd.Up tm.timerScope target) $ \restart ->
                withSystemctl r systemctl (Systemd.Disable tm.timerScope target) $ \disable ->
                    op "systemd-timer" (deps [file, run t tm]) $ \actions ->
                        actions
                            { help = "schedules " <> tm.timerTriggers <> " with " <> target
                            , notes = ["unit: " <> FS.hashBytes (Text.encodeUtf8 rendered)]
                            , ref = mkRef "systemd-unit" target
                            , check = Systemd.checkUnit Systemd.interpretShow tm.timerScope target
                            , up = reload >> enable >> restart
                            , -- @disable@ fails for a unit file that is not
                              -- there, which is a timer already gone
                              down = do
                                present <- doesFileExist (timerPath tm)
                                when present disable
                            }
  where
    target = timerTarget tm
    rendered = renderTimer tm
    file = unitFile r tm.timerScope (timerPath tm) (timerProblems tm) rendered

{- | A 'Job' and the timer that runs it: 'timerUnit' over 'jobTimer', standing
on 'jobService'. The 'Track'' is the job's.
-}
scheduledJob ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Job ->
    Job ->
    [Schedule] ->
    Op
scheduledJob r systemctl t j schedules =
    timerUnit r systemctl (Track (const (jobService r systemctl t j))) (jobTimer j schedules)

{- | Runs a job now: @systemctl start@, which for a oneshot unit returns when
the command has exited and fails if it did not exit 0, so a failed job is a
failed @up@. A run already under way (the timer fired) is joined, not killed.

It has no check, so it runs at every pass, which is what a step of a
deployment wants ("migrate, then restart") and not what a once-ever job
wants; such a caller gives the node its own check. It has no dependency
either: the caller injects whatever installs the unit. @down@ does nothing,
since a run that finished left nothing of its own behind.
-}
runJob ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Systemd.Scope ->
    Systemd.UnitTarget ->
    Op
runJob r systemctl scope target =
    withSystemctl r systemctl (Systemd.StartUnit scope target) $ \start ->
        op "systemd-job-run" nodeps $ \actions ->
            actions
                { help = "runs " <> target <> " to completion"
                , ref = mkRef "systemd-job-run" target
                , up = start
                }

-------------------------------------------------------------------------------

{- | What 'completedRun' asks about: a unit, the files a run of it has to be
newer than, and the stamp its runs leave, if it leaves one.
-}
data Completion
    = Completion
    { completionScope :: Systemd.Scope
    , completionUnit :: Systemd.UnitTarget
    , completionWritten :: [FilePath]
    -- ^ a run that started before any of these was last written does not
    -- count: the unit file, its configuration, its script. A file that is
    -- not there is not counted; it was not written.
    , completionStamp :: Maybe FilePath
    -- ^ the 'jobStamp' of the unit, which has to be the same path
    }
    deriving (Eq, Show)

{- | The question for a 'Job': a run newer than its unit file and its
environment file, read from its stamp if it has one. The program is not in
the list (for most jobs it is @\/bin\/sh@ or a packaged binary, whose upgrade
is not a reason to run a backup); a script or a configuration file the
command reads is the caller's to add.
-}
jobCompletion :: Job -> Completion
jobCompletion j =
    Completion
        { completionScope = j.jobScope
        , completionUnit = jobServiceTarget j
        , completionWritten = jobServicePath j : maybeToList j.jobEnvFile
        , completionStamp = j.jobStamp
        }

-- | What is known about a unit's runs besides what systemd says.
data RunEvidence
    = RunEvidence
    { evidenceWritten :: [(FilePath, UTCTime)]
    -- ^ when each file the job stands on was last written
    , evidenceSucceeded :: Maybe UTCTime
    -- ^ the stamp's time: when the last successful run started
    , evidenceStarted :: Maybe UTCTime
    -- ^ the running stamp's time: when the last run started, if it has not
    -- succeeded
    }
    deriving (Eq, Show)

-- | No file to be newer than and no stamp: systemd's answer alone.
noEvidence :: RunEvidence
noEvidence = RunEvidence [] Nothing Nothing

-- | The properties 'checkLastRun' asks @systemctl show@ for.
lastRunProperties :: [String]
lastRunProperties = ["LoadState", "ActiveState", "Result", "ExecMainStatus", "ExecMainStartTimestamp"]

{- | The verdict on a job's last run, from @systemctl show
--timestamp=us+utc@'s lines and the 'RunEvidence', pure.

In the order asked:

* a unit systemd does not hold is a 'Failure' (there is nothing to have run);
* a unit on its way somewhere (@activating@ is a oneshot unit /running/) is
  'Unknown': the run under way has not gone either way yet;
* @ActiveState=failed@, or a @Result@ other than @success@, is a 'Failure'
  naming the result and the exit status. This is the last run, since a
  failed unit stays loaded;
* a running stamp newer than the stamp is a run that started and did not
  succeed, a 'Failure'. This is what is left of a failure after a reboot;
* the last successful run is the later of systemd's @ExecMainStartTimestamp@
  (only there while the unit stayed loaded) and the stamp. None is a
  'Failure': the job never ran, or ran and was forgotten, and the two cannot
  be told apart;
* a file written after that run started is a 'Failure' naming the file;
* otherwise 'Completed'.

A timestamp that is there and cannot be read is 'Unknown' rather than a
missing run.
-}
interpretLastRun :: RunEvidence -> [Text] -> CheckResult
interpretLastRun evidence ls
    | loadState /= Just "loaded" =
        Failure (maybe "systemctl said nothing about the unit's load state" ("the unit is " <>) loadState)
    | activeState `elem` map Just ["activating", "deactivating", "reloading"] = Unknown
    | activeState == Just "failed" || maybe False (/= "success") result =
        Failure
            ( "the last run failed: Result="
                <> fromMaybe "?" result
                <> maybe "" (", exit status " <>) (property "ExecMainStatus")
            )
    | unsucceeded = Failure "the last run that started did not succeed"
    | otherwise = case systemdStarted of
        Nothing -> Unknown
        Just started -> case catMaybes [started, evidence.evidenceSucceeded] of
            [] -> Failure "there is no record of a successful run"
            runs ->
                let lastSuccess = maximum runs
                 in case [path | (path, written) <- evidence.evidenceWritten, written > lastSuccess] of
                        (path : _) -> Failure ("no run has succeeded since " <> Text.pack path <> " was written")
                        [] -> Completed
  where
    loadState = property "LoadState"
    activeState = property "ActiveState"
    result = property "Result"

    unsucceeded :: Bool
    unsucceeded = case (evidence.evidenceStarted, evidence.evidenceSucceeded) of
        (Just started, Just succeeded) -> started > succeeded
        (Just _, Nothing) -> True
        (Nothing, _) -> False

    -- 'Nothing' is a timestamp that could not be read, @Just Nothing@ none
    systemdStarted :: Maybe (Maybe UTCTime)
    systemdStarted = case property "ExecMainStartTimestamp" of
        Nothing -> Just Nothing
        Just "n/a" -> Just Nothing
        Just raw -> Just <$> parseTimestamp raw

    property :: Text -> Maybe Text
    property name =
        case [Text.strip (Text.drop 1 v) | l <- ls, let (k, v) = Text.breakOn "=" l, k == name] of
            (x : _) | not (Text.null x) -> Just x
            _ -> Nothing

-- | A timestamp as @--timestamp=us+utc@ prints it: @Mon 2026-10-05 09:56:02.675581 UTC@.
parseTimestamp :: Text -> Maybe UTCTime
parseTimestamp raw = parseTimeM True defaultTimeLocale "%a %Y-%m-%d %H:%M:%S%Q UTC" (Text.unpack raw)

{- | Asks systemd and the filesystem, then 'interpretLastRun'. @systemctl@
not answering (or not knowing @--timestamp@, which is systemd 247's) is
'Unknown'.
-}
checkLastRun :: Completion -> IO CheckResult
checkLastRun c = maybe Unknown (uncurry interpretLastRun) <$> readRun lastRunProperties c

{- | The files' times and @systemctl show@'s lines for these properties;
'Nothing' when @systemctl@ did not answer.
-}
readRun :: [String] -> Completion -> IO (Maybe (RunEvidence, [Text]))
readRun properties c = do
    written <- fmap catMaybes (forM c.completionWritten (\path -> fmap (fmap ((,) path)) (modified path)))
    succeeded <- maybe (pure Nothing) modified c.completionStamp
    started <- maybe (pure Nothing) (modified . stampRunning) c.completionStamp
    (code, out, _err) <-
        readCreateProcessWithExitCode
            ( proc
                "systemctl"
                ( Systemd.scopeArgs c.completionScope
                    <> ["show", Text.unpack c.completionUnit, "--timestamp=us+utc"]
                    <> map ("--property=" <>) properties
                )
            )
            ""
    pure $ case code of
        ExitSuccess -> Just (RunEvidence written succeeded started, Text.lines (Text.pack out))
        ExitFailure _ -> Nothing
  where
    modified :: FilePath -> IO (Maybe UTCTime)
    modified path = do
        present <- doesFileExist path
        if present then Just <$> getModificationTime path else pure Nothing

{- | A job that has run: 'runJob' with 'checkLastRun' as its check. A pass
skips it when the last run succeeded after everything in 'completionWritten'
was written, runs it otherwise, and fails when that run fails, so a job
whose last run failed (the timer's or anybody's) fails every pass until a
run succeeds. Under @run serve@ the verdict of a job at rest is 'Completed'.

It is keyed like 'runJob' (@"systemd-job-run"@ and the unit): the two are
one effect site. It has no dependency: the caller injects what installs the
unit, or uses 'completedJob'. Without a timer keeping the unit loaded and
without a stamp, a successful run is not remembered and the job runs at
every pass, as 'runJob' does (see the module's header).

@down@ removes the stamp files, the one thing a run leaves that is this
node's.
-}
completedRun ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Completion ->
    Op
completedRun r systemctl c =
    withSystemctl r systemctl (Systemd.StartUnit c.completionScope target) $ \start ->
        op "systemd-job-completed" nodeps $ \actions ->
            actions
                { help = "has " <> target <> " run to completion since it was last changed"
                , notes =
                    ["newer than: " <> Text.pack path | path <- c.completionWritten]
                        <> ["stamp: " <> Text.pack s | s <- maybeToList c.completionStamp]
                , ref = mkRef "systemd-job-run" target
                , check = checkLastRun c
                , up = start
                , down = removeStamps c
                }
  where
    target = c.completionUnit

-- | Removes the stamp files, the one thing a run leaves that is a run node's.
removeStamps :: Completion -> IO ()
removeStamps c =
    forM_ c.completionStamp $ \s ->
        forM_ [s, stampRunning s] $ \path -> do
            present <- doesFileExist path
            when present (removeFile path)

-- | 'completedRun' for a 'Job', standing on its 'jobService'.
completedJob ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Job ->
    Job ->
    Op
completedJob r systemctl t j =
    completedRun r systemctl (jobCompletion j) `inject` jobService r systemctl t j

{- | 'completedRun' for a 'Job', standing on its 'scheduledJob': the timer is
waiting before the first run, so systemd keeps that run's outcome.
-}
completedScheduledJob ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Job ->
    Job ->
    [Schedule] ->
    Op
completedScheduledJob r systemctl t j schedules =
    completedRun r systemctl (jobCompletion j) `inject` scheduledJob r systemctl t j schedules

-------------------------------------------------------------------------------

{- | Where a job's runs stand, as much of it as @up@ needs to decide whether
to start one. 'interpretLastRun' folds the same reading into a
'CheckResult'; 'runVerdict' of this is that answer, for every input.
-}
data RunState
    = -- | systemd does not hold the unit, and why
      NotLoaded Text
    | -- | a run is under way (or the unit is otherwise between states)
      Running
    | -- | the last run failed, and how. The path is a file written /after/
      -- that failure, if there is one: somebody changed what the job stands
      -- on since, so the failure is of a job that is no longer the one
      -- declared.
      LastFailed Text (Maybe FilePath)
    | -- | systemd answered with a timestamp that cannot be read, or did not
      -- answer
      Unreadable
    | -- | there is no record of a successful run
      NeverRan
    | -- | the last successful run started before this file was written
      Outdated FilePath
    | -- | the last run succeeded, after everything was written
      Succeeded
    deriving (Eq, Show)

{- | 'lastRunProperties' and when the unit last came to rest:
@InactiveEnterTimestamp@ is, for a failed unit, when the run failed (seen on
systemd 255; it is empty while the unit is @activating@).
-}
runStateProperties :: [String]
runStateProperties = lastRunProperties <> ["InactiveEnterTimestamp"]

{- | 'interpretLastRun', keeping what it found apart. The branches are its
branches in its order; the one thing added is /when/ a failure happened
(@InactiveEnterTimestamp@, or the running stamp's time where systemd has
forgotten), so that a file written since can be told from one written
before.
-}
classifyRun :: RunEvidence -> [Text] -> RunState
classifyRun evidence ls
    | loadState /= Just "loaded" =
        NotLoaded (maybe "systemctl said nothing about the unit's load state" ("the unit is " <>) loadState)
    | activeState `elem` map Just ["activating", "deactivating", "reloading"] = Running
    | activeState == Just "failed" || maybe False (/= "success") result =
        LastFailed
            ( "the last run failed: Result="
                <> fromMaybe "?" result
                <> maybe "" (", exit status " <>) (property "ExecMainStatus")
            )
            (writtenSince =<< failedAt)
    | unsucceeded = LastFailed "the last run that started did not succeed" (writtenSince =<< evidence.evidenceStarted)
    | otherwise = case timestamp "ExecMainStartTimestamp" of
        Nothing -> Unreadable
        Just started -> case catMaybes [started, evidence.evidenceSucceeded] of
            [] -> NeverRan
            runs -> maybe Succeeded Outdated (writtenSince (maximum runs))
  where
    loadState = property "LoadState"
    activeState = property "ActiveState"
    result = property "Result"

    unsucceeded :: Bool
    unsucceeded = case (evidence.evidenceStarted, evidence.evidenceSucceeded) of
        (Just started, Just succeeded) -> started > succeeded
        (Just _, Nothing) -> True
        (Nothing, _) -> False

    -- when systemd saw the run fail; failing that, when the run that left
    -- no stamp started
    failedAt :: Maybe UTCTime
    failedAt = case timestamp "InactiveEnterTimestamp" of
        Just (Just at) -> Just at
        _ | unsucceeded -> evidence.evidenceStarted
        _ -> Nothing

    writtenSince :: UTCTime -> Maybe FilePath
    writtenSince at = case [path | (path, written) <- evidence.evidenceWritten, written > at] of
        (path : _) -> Just path
        [] -> Nothing

    -- 'Nothing' is a timestamp that could not be read, @Just Nothing@ none
    timestamp :: Text -> Maybe (Maybe UTCTime)
    timestamp name = case property name of
        Nothing -> Just Nothing
        Just "n/a" -> Just Nothing
        Just raw -> Just <$> parseTimestamp raw

    property :: Text -> Maybe Text
    property name =
        case [Text.strip (Text.drop 1 v) | l <- ls, let (k, v) = Text.breakOn "=" l, k == name] of
            (x : _) | not (Text.null x) -> Just x
            _ -> Nothing

-- | What a check answers for a 'RunState': 'interpretLastRun''s answer.
runVerdict :: RunState -> CheckResult
runVerdict (NotLoaded why) = Failure why
runVerdict Running = Unknown
runVerdict (LastFailed why _) = Failure why
runVerdict Unreadable = Unknown
runVerdict NeverRan = Failure "there is no record of a successful run"
runVerdict (Outdated path) = Failure ("no run has succeeded since " <> Text.pack path <> " was written")
runVerdict Succeeded = Completed

-- | Asks systemd and the filesystem, then 'classifyRun'.
readRunState :: Completion -> IO RunState
readRunState c = maybe Unreadable (uncurry classifyRun) <$> readRun runStateProperties c

-- | What 'launchedRun''s @up@ does about a 'RunState'.
data Launch
    = -- | queue a start
      Start
    | -- | a run is under way: start nothing
      LeaveRunning
    | -- | the job has run: start nothing
      LeaveDone
    | -- | the last run failed and nothing was written since: start nothing, fail
      ReportFailure Text
    | -- | what is there cannot be read: start nothing, fail
      CannotTell
    deriving (Eq, Show)

{- | A start is queued only for a job that is at rest and whose last run does
not count: never run, older than a file it stands on, or failed /before/
such a file was written. A run under way is never started over, a completed
job is never run again, and a failure nobody has answered is reported and
not retried: a pass asking twice must not be a job run twice.

A unit systemd does not hold is started all the same, for @systemctl@ to
refuse and say why.
-}
launchDecision :: RunState -> Launch
launchDecision (NotLoaded _) = Start
launchDecision Running = LeaveRunning
launchDecision (LastFailed _ (Just _)) = Start
launchDecision (LastFailed why Nothing) = ReportFailure why
launchDecision Unreadable = CannotTell
launchDecision NeverRan = Start
launchDecision (Outdated _) = Start
launchDecision Succeeded = LeaveDone

-- | How a run that was started, or found running, stands when asked again.
data Outcome
    = -- | it ran to completion
      Finished
    | -- | it has not ended as far as can be seen: running, or queued and not
      -- yet shown as running by systemd
      Pending
    | -- | it failed
      Broken Text
    | -- | what is there cannot be read
      Undetermined
    deriving (Eq, Show)

{- | Read after a start was queued or a run was found under way, so a state
'launchDecision' would start from is the moment before systemd shows the
queued run, not a reason to start another.
-}
runOutcome :: RunState -> Outcome
runOutcome st = case launchDecision st of
    LeaveDone -> Finished
    ReportFailure why -> Broken why
    CannotTell -> Undetermined
    LeaveRunning -> Pending
    Start -> Pending

{- | How long 'launchedRun''s @up@ stays to see the run end. 'noWait' is the
starting point.
-}
data Polling
    = Polling
    { pollingWait :: Int
    -- ^ seconds @up@ may stay asking, at most; none or less is "ask once
    -- and return"
    , pollingEvery :: Int
    -- ^ seconds between two questions; less than one is one
    }
    deriving (Eq, Show)

-- | Start and return: the answer is a later pass's.
noWait :: Polling
noWait = Polling{pollingWait = 0, pollingEvery = 5}

{- | The pauses between questions, in seconds: each at most 'pollingEvery',
together exactly 'pollingWait'.
-}
pollDelays :: Polling -> [Int]
pollDelays p = go (max 0 p.pollingWait)
  where
    every = max 1 p.pollingEvery
    go 0 = []
    go left = let pause = min every left in pause : go (left - pause)

{- | Why this run cannot be started and asked about later. It needs a stamp:
with nothing referring to the unit systemd forgets a successful run as it
ends, and a later pass would find a job that never ran and start it a
second time. (A waiting timer keeps the record too, but not across a
reboot.)
-}
launchProblems :: Completion -> [Text]
launchProblems c =
    ["a run that is not waited for needs a stamp to be found completed later (jobStamp)" | Nothing <- [c.completionStamp]]

-- | 'launchedRun''s notes: 'completedRun''s, and the wait when there is one.
launchNotes :: Polling -> Completion -> [Text]
launchNotes polling c =
    mconcat
        [ ["newer than: " <> Text.pack path | path <- c.completionWritten]
        , ["stamp: " <> Text.pack s | s <- maybeToList c.completionStamp]
        , [ "waits: up to " <> Text.pack (show polling.pollingWait) <> "s, asking every " <> Text.pack (show (max 1 polling.pollingEvery)) <> "s"
          | polling.pollingWait > 0
          ]
        ]

-- | The run cannot be declared this way; see 'launchProblems'.
data CannotLaunch = CannotLaunch !Systemd.UnitTarget ![Text]
    deriving (Show)

instance Exception CannotLaunch

{- | The job is running and @up@ did not stay to see it end. Not a failure of
the job: the next pass asks again.
-}
newtype RunInProgress = RunInProgress Systemd.UnitTarget

instance Show RunInProgress where
    show (RunInProgress target) =
        "RunInProgress: " <> Text.unpack target <> " is still running (in progress, neither failed nor done); a later pass will find how it ended"

instance Exception RunInProgress

-- | The job's last run failed and nothing it stands on was written since.
data RunFailed = RunFailed !Systemd.UnitTarget !Text
    deriving (Show)

instance Exception RunFailed

-- | Neither systemd nor the stamps said where the job stands; nothing was started.
newtype RunUndetermined = RunUndetermined Systemd.UnitTarget
    deriving (Show)

instance Exception RunUndetermined

{- | A job that has run, started without being waited for: 'completedRun'
whose @up@ is @systemctl start --no-block@ and returns.

The check is 'completedRun''s ('runVerdict' of 'readRunState'): 'Completed'
once a run succeeded after everything in 'completionWritten' was written,
'Unknown' while a run is under way, a 'Failure' otherwise.

@up@ reads the same state and follows 'launchDecision': it queues a start
for a job at rest whose last run does not count, starts nothing for a job
that is running or has run, and for a job whose last run failed throws
'RunFailed' /without/ running it again (write one of the files it stands
on, which is what fixing it usually is, or @systemctl reset-failed@ and
remove @STAMP.running@, to have it run again). It then asks again, for up
to 'pollingWait' seconds, and:

* returns if the run succeeded;
* throws 'RunFailed' if it failed;
* throws 'RunInProgress' if it is still running.

The last one is how "in progress" reaches a driver, which knows a node's
@up@ as done or failed and nothing between: what stands on the job is not
run against a job that has not finished, the pass ends unsuccessful, and
the report names 'RunInProgress' and not a failure of the job. A later pass
finds the run under way (and starts nothing), completed (a skip) or failed.
Each pass is a call of its own, so a link that drops costs a pass and not
the run; a caller wanting one command that returns when the job is over
repeats the pass, with a 'pollingWait' as long as it cares to hold one call.

The 'Completion' has to name a stamp ('launchProblems'): @up@ throws
'CannotLaunch' without one. Keyed like 'runJob' and 'completedRun', being
the same effect site; no dependency, the caller injects what installs the
unit or uses 'launchedJob'. @down@ removes the stamp files and stops
nothing: a run under way is the installing node's to stop.
-}
launchedRun ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Polling ->
    Completion ->
    Op
launchedRun r systemctl polling c =
    withSystemctl r systemctl (Systemd.StartUnitNoBlock c.completionScope target) $ \start ->
        op "systemd-job-launched" nodeps $ \actions ->
            actions
                { help = "has " <> target <> " run to completion since it was last changed, started without waiting for it"
                , notes = launchNotes polling c
                , ref = mkRef "systemd-job-run" target
                , check = runVerdict <$> readRunState c
                , up = launch start
                , down = removeStamps c
                }
  where
    target = c.completionUnit

    launch :: IO () -> IO ()
    launch start = do
        let problems = launchProblems c
        unless (null problems) $ throwIO (CannotLaunch target problems)
        found <- readRunState c
        case launchDecision found of
            Start -> start >> settle (pollDelays polling)
            LeaveRunning -> settle (pollDelays polling)
            LeaveDone -> pure ()
            ReportFailure why -> throwIO (RunFailed target why)
            CannotTell -> throwIO (RunUndetermined target)

    settle :: [Int] -> IO ()
    settle pauses = do
        outcome <- runOutcome <$> readRunState c
        case (outcome, pauses) of
            (Finished, _) -> pure ()
            (Broken why, _) -> throwIO (RunFailed target why)
            (_, pause : rest) -> threadDelay (pause * 1000000) >> settle rest
            (Pending, []) -> throwIO (RunInProgress target)
            (Undetermined, []) -> throwIO (RunUndetermined target)

-- | 'launchedRun' for a 'Job', standing on its 'jobService'.
launchedJob ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Job ->
    Polling ->
    Job ->
    Op
launchedJob r systemctl t polling j =
    launchedRun r systemctl polling (jobCompletion j) `inject` jobService r systemctl t j
