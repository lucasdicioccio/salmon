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

What is not here: a verdict on how the last run /went/. A failed run leaves
the job's unit @failed@, which @systemctl@ and the journal show, but no node
reads it: re-installing a job does not un-fail it, and a check that said
otherwise would have a supervisor reload systemd forever.
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

    -- * shared with other unit-writing nodes
    unitDirectory,
    unitFile,
    reloadAndRequireLoaded,
    stopIfKnown,
    unitNameProblems,
    InvalidUnit (..),
    UnitNotLoaded (..),
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.Char (isAlphaNum, isAscii)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.OpGraph (OpGraph (..))
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
            , ["ExecStart=" <> Text.unwords (map Systemd.literalArg j.jobCommand)]
            ]
  where
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
            ]

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

{- | The directory unit files go in, shared by every unit on the machine:
created if missing, left alone by @down@.
'Salmon.Builtin.Nodes.Filesystem.dir' would refuse to go down while anything
else is in it, which for this directory is always.
-}
unitDirectory :: FilePath -> Op
unitDirectory path =
    op "systemd-unit-dir" nodeps $ \actions ->
        actions
            { help = "ensures " <> Text.pack path <> " exists"
            , notes = ["shared by every unit in it: down leaves it"]
            , ref = mkRef "systemd-unit-dir" path
            , up = createDirectoryIfMissing True path
            }

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
