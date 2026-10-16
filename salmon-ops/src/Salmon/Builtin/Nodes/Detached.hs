{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A process that outlives the @run up@ that started it.

"Salmon.Builtin.Nodes.Daemon" owns its process from a thread that never
returns, so only @run serve@ can hold one, and its @up@ throws
'Salmon.Builtin.Nodes.Daemon.NeedsSupervisor' on purpose.
"Salmon.Builtin.Nodes.Systemd" hands the problem to systemd. This is for
the case with neither: a developer's rig, a test bench, a machine without
a unit manager, where a one-shot @run up@ starts a server and exits and a
later @run down@ (another process) stops it.

= What @up@ does

Starts the command in a __new session__ (so it leads its own process group
and has no controlling terminal), with standard input from @\/dev\/null@
(never the pass's own, see 'Binary.detachedStdin'), standard output and
error appended to 'detached_logFile', every other descriptor closed
(@close_fds@), an optional working directory and an environment overlay.
Then it writes 'detached_pidFile'. salmon does not wait for the process;
when the starting binary exits, init (or the nearest subreaper) adopts it.
Under @run serve@ the process stays salmon's child, and a thread reaps it
when it exits so no zombie piles up.

Readiness is not this node's business: put a
'Salmon.Builtin.Nodes.Readiness.waitUntil' after it.

= Identity, not a pid

@specs\/per-node-state-machines.md@ lists the pidfile convention as a
non-goal because a pidfile plus @kill -0@ cannot survive pid reuse. This
node does not trust the pid on its own. The pid file records, next to the
pid, three facts taken when the process was started:

* its start time, in clock ticks since boot (@\/proc\/PID\/stat@ field 22),
  which a later process reusing the pid cannot share;
* the boot id (@\/proc\/sys\/kernel\/random\/boot_id@), so a pid file that
  survived a reboot proves nothing;
* the argv salmon started it with, which must equal @\/proc\/PID\/cmdline@.

A process is __ours__ only when all three still hold and it is not a
zombie. Nothing is ever signalled otherwise: a pid file naming a process
that is not ours is /forgotten/ (removed, reported 'Forgot'), never acted
upon. The cost is that a program that rewrites its own argv (a process
title, as @postgres@ or @nginx@ workers do) is never recognised as ours;
such a program is not for this node.

= @check@

'Success' when the pid file names a process that is ours and was started
with the declared argv and working directory (and, with
'detached_restartOnReplacedBinary', whose executable has not been replaced
on disk). 'Failure' otherwise, with the reason: no pid file, not ours any
more, or ours but declared differently.

= Stopping

The configured signal (@SIGTERM@ by default) to the process group, a grace
period, then @SIGKILL@ and a second grace period, checking identity again
before each signal. @down@ throws if the process is still there after
that. @down@ removes the pid file in every case it does not throw.

= Linux only

It reads @\/proc@.
-}
module Salmon.Builtin.Nodes.Detached (
    -- * The node
    Detached (..),
    defaultDetached,
    detached,
    detachedRef,

    -- * How it stops (shared with "Salmon.Builtin.Nodes.Daemon")
    Stop (..),
    defaultStop,

    -- * Observing
    Report (..),
    StillRunning (..),

    -- * The parts, for tests and nodes building on this one
    PidRecord (..),
    readPidFile,
    renderPidFile,
    parsePidFile,
    Identity (..),
    identify,
    Verdict (..),
    judge,
    parseStat,
    stopRecorded,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (void)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (IOMode (..), hClose, openFile)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Signals (Signal, sigKILL, signalProcess, signalProcessGroup)
import System.Posix.Types (CPid (..))
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, waitForProcess)
import Text.Read (readMaybe)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Daemon (Stop (..), defaultStop)
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Supervision (Micros (..), millis)
import Salmon.Reporter

-------------------------------------------------------------------------------

data Detached = Detached
    { detached_name :: !Text
    -- ^ names it in reports, and is its 'Ref' key.
    , detached_command :: !FilePath
    -- ^ the executable, as argv[0]: identity compares argv as given, so a
    -- bare name found on @PATH@ is fine as long as it stays the same.
    , detached_args :: ![String]
    , detached_cwd :: !(Maybe FilePath)
    , detached_env :: ![(String, String)]
    -- ^ added to (and overriding) salmon's own environment, read at @up@
    -- time. Values never appear in @help@, @notes@ or the pid file; only
    -- names do (in @notes@).
    , detached_pidFile :: !FilePath
    -- ^ its directory must exist (a directory is its own node).
    , detached_logFile :: !FilePath
    -- ^ standard output and error, appended. Its directory must exist.
    , detached_stop :: !Stop
    , detached_restartOnReplacedBinary :: !Bool
    -- ^ 'check' also fails when @\/proc\/PID\/exe@ reads @(deleted)@: the
    -- executable was rebuilt or upgraded under the running process, and
    -- @up@ then replaces it.
    }

-- | No cwd, no env overlay, 'defaultStop', replaced binaries tolerated.
defaultDetached :: Text -> FilePath -> [String] -> FilePath -> FilePath -> Detached
defaultDetached name cmd args pidFile logFile =
    Detached
        { detached_name = name
        , detached_command = cmd
        , detached_args = args
        , detached_cwd = Nothing
        , detached_env = []
        , detached_pidFile = pidFile
        , detached_logFile = logFile
        , detached_stop = defaultStop
        , detached_restartOnReplacedBinary = False
        }

detachedRef :: Detached -> Ref
detachedRef d = mkRef "detached" d.detached_name

argvOf :: Detached -> [String]
argvOf d = d.detached_command : d.detached_args

data Report
    = Started !Text !Int
    | -- | a pid file named a process that is not ours: removed, nothing signalled
      Forgot !Text !Text
    | -- | ours, but declared differently (or replaced on disk): stopping it to start anew
      Replacing !Text !Text
    | Signalling !Text !Int !Signal
    | Killing !Text !Int
    | Stopped !Text !Int
    | -- | the pid file was there, its process gone on its own
      AlreadyGone !Text !Int
    deriving (Show)

-- | Thrown by @down@ (and by @up@ when replacing) when the process survived @SIGKILL@.
data StillRunning = StillRunning !Text !Int
    deriving (Show)

instance Exception StillRunning

-------------------------------------------------------------------------------

-- | What the pid file says.
data PidRecord = PidRecord
    { pr_pid :: !Int
    , pr_start :: !Integer
    -- ^ @\/proc\/PID\/stat@ field 22 when started
    , pr_boot :: !Text
    , pr_argv :: ![String]
    , pr_cwd :: !(Maybe FilePath)
    }
    deriving (Show, Eq)

{- | Line one is the pid alone, so @kill $(head -1 FILE)@ habits keep working
(and stay as unsafe as they always were). Line two is a JSON object with the
rest.
-}
renderPidFile :: PidRecord -> BS.ByteString
renderPidFile r =
    BS8.pack (show r.pr_pid)
        <> "\n"
        <> LBS.toStrict
            ( Aeson.encode $
                Aeson.object
                    [ "start" .= r.pr_start
                    , "boot" .= r.pr_boot
                    , "argv" .= r.pr_argv
                    , "cwd" .= r.pr_cwd
                    ]
            )
        <> "\n"

{- | 'Nothing' for anything this module did not write, including a bare pid:
a pid alone proves nothing, so it is never signalled.
-}
parsePidFile :: BS.ByteString -> Maybe PidRecord
parsePidFile bs = case BS8.lines bs of
    (pidLine : jsonLine : _) -> do
        pid <- readMaybe (BS8.unpack pidLine)
        (start, boot, argv, cwd) <-
            Aeson.decodeStrict jsonLine
                >>= Aeson.parseMaybe
                    ( Aeson.withObject "pid record" $ \o ->
                        (,,,) <$> o .: "start" <*> o .: "boot" <*> o .: "argv" <*> o .:? "cwd"
                    )
        pure (PidRecord pid start boot argv cwd)
    _ -> Nothing

-- | 'Nothing' when there is no pid file; @Just Nothing@ when it is not ours to read.
readPidFile :: FilePath -> IO (Maybe (Maybe PidRecord))
readPidFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            r <- try @SomeException (BS.readFile path)
            pure (Just (either (const Nothing) parsePidFile r))

writePidFile :: FilePath -> PidRecord -> IO ()
writePidFile path r = do
    let tmp = takeDirectory path </> ("." <> takeFileName path <> ".tmp")
    BS.writeFile tmp (renderPidFile r)
    renameFile tmp path

forget :: FilePath -> IO ()
forget path = void (try @SomeException (removeFile path))

-------------------------------------------------------------------------------

-- | What @\/proc@ says about a pid now.
data Identity = Identity
    { id_start :: !Integer
    , id_pgrp :: !Int
    , id_zombie :: !Bool
    , id_argv :: ![String]
    , id_boot :: !Text
    , id_exeDeleted :: !Bool
    }
    deriving (Show, Eq)

-- | 'Nothing' when there is no such process (or @\/proc@ will not say).
identify :: Int -> IO (Maybe Identity)
identify pid = do
    r <- try @SomeException $ do
        stat <- BS.readFile (procDir </> "stat")
        cmdline <- BS.readFile (procDir </> "cmdline")
        boot <- readBootId
        exe <- try @SomeException (readSymbolicLink (procDir </> "exe"))
        pure (stat, cmdline, boot, exe)
    pure $ case r of
        Left _ -> Nothing
        Right (stat, cmdline, boot, exe) -> do
            (state, pgrp, start) <- parseStat stat
            pure
                Identity
                    { id_start = start
                    , id_pgrp = pgrp
                    , id_zombie = state == 'Z' || state == 'X'
                    , id_argv = splitCmdline cmdline
                    , id_boot = boot
                    , id_exeDeleted = either (const False) (" (deleted)" `Text.isSuffixOf`) (Text.pack <$> exe)
                    }
  where
    procDir = "/proc" </> show pid

readBootId :: IO Text
readBootId = Text.strip . Text.decodeUtf8 <$> BS.readFile "/proc/sys/kernel/random/boot_id"

{- | State, process group and start time. The command name (field 2) is in
parentheses and may hold spaces and parentheses itself, so split after the
/last/ closing one.
-}
parseStat :: BS.ByteString -> Maybe (Char, Int, Integer)
parseStat bs = do
    let (_, rest) = BS8.breakEnd (== ')') bs
    -- fields from 3 on: index 0 is field 3.
    let fields = BS8.words rest
    state <- case fields of
        (s : _) | not (BS.null s) -> Just (BS8.head s)
        _ -> Nothing
    pgrp <- at 2 fields >>= readMaybe . BS8.unpack
    start <- at 19 fields >>= readMaybe . BS8.unpack
    pure (state, pgrp, start)
  where
    at n xs = case drop n xs of
        (x : _) -> Just x
        [] -> Nothing

splitCmdline :: BS.ByteString -> [String]
splitCmdline bs =
    map (Text.unpack . Text.decodeUtf8Lenient) $
        case BS.split 0 bs of
            -- the trailing NUL leaves one empty piece
            xs | not (null xs) && BS.null (last xs) -> init xs
            xs -> xs

-------------------------------------------------------------------------------

data Verdict
    = -- | no pid file
      NoRecord
    | -- | the pid file names nothing we can prove we started
      NotOurs !Text
    | -- | ours, as declared
      Running !PidRecord
    | -- | ours, but not as declared now (argv, cwd, or a replaced executable)
      Drifted !PidRecord !Text
    deriving (Show, Eq)

-- | Read the pid file and @\/proc@, and say what is there. Signals nothing.
judge :: Detached -> IO Verdict
judge d = do
    mrec <- readPidFile d.detached_pidFile
    case mrec of
        Nothing -> pure NoRecord
        Just Nothing -> pure (NotOurs "the pid file is not one this node wrote")
        Just (Just rec) -> do
            ours <- isOurs rec
            case ours of
                Left why -> pure (NotOurs why)
                Right ident
                    | rec.pr_argv /= argvOf d -> pure (Drifted rec "started with another command line")
                    | rec.pr_cwd /= d.detached_cwd -> pure (Drifted rec "started in another directory")
                    | d.detached_restartOnReplacedBinary && ident.id_exeDeleted ->
                        pure (Drifted rec "its executable was replaced on disk")
                    | otherwise -> pure (Running rec)

-- | The identity check: every fact recorded at start still holds.
isOurs :: PidRecord -> IO (Either Text Identity)
isOurs rec = do
    mident <- identify rec.pr_pid
    pure $ case mident of
        Nothing -> Left ("no process " <> pidText)
        Just ident
            | ident.id_zombie -> Left ("process " <> pidText <> " has exited")
            | ident.id_boot /= rec.pr_boot -> Left "the pid file is from an earlier boot"
            | ident.id_start /= rec.pr_start -> Left ("process " <> pidText <> " is another process (start time differs)")
            | ident.id_argv /= rec.pr_argv -> Left ("process " <> pidText <> " is another process (command line differs)")
            | otherwise -> Right ident
  where
    pidText = Text.pack (show rec.pr_pid)

-------------------------------------------------------------------------------

detached :: Reporter Report -> Detached -> Op
detached r d =
    op "detached" nodeps $ \actions ->
        actions
            { help = "keeps " <> d.detached_name <> " running, detached"
            , notes =
                [ "argv: " <> Text.pack (show (argvOf d))
                , "cwd: " <> maybe "(inherited)" Text.pack d.detached_cwd
                , "env names: " <> Text.pack (show (map fst d.detached_env))
                , "pid file: " <> Text.pack d.detached_pidFile
                , "log file: " <> Text.pack d.detached_logFile
                ]
                    <> ["restarts when its executable is replaced on disk" | d.detached_restartOnReplacedBinary]
            , ref = detachedRef d
            , check = do
                v <- judge d
                pure $ case v of
                    Running _ -> Success
                    NoRecord -> Failure "no pid file"
                    NotOurs why -> Failure why
                    Drifted _ why -> Failure why
            , up = upDetached r d
            , down = downDetached r d
            }

upDetached :: Reporter Report -> Detached -> IO ()
upDetached r d = do
    v <- judge d
    case v of
        Running _ -> pure ()
        NoRecord -> start
        NotOurs why -> do
            runReporter r (Forgot d.detached_name why)
            forget d.detached_pidFile
            start
        Drifted rec why -> do
            runReporter r (Replacing d.detached_name why)
            stopRecorded r d.detached_name d.detached_stop rec
            forget d.detached_pidFile
            start
  where
    start = do
        overlayEnv <-
            if null d.detached_env
                then pure Nothing
                else do
                    current <- getEnvironment
                    let names = map fst d.detached_env
                    pure (Just (d.detached_env <> filter ((`notElem` names) . fst) current))
        logH <- openFile d.detached_logFile AppendMode
        let cp =
                (proc d.detached_command d.detached_args)
                    { cwd = d.detached_cwd
                    , env = overlayEnv
                    , std_out = UseHandle logH
                    , std_err = UseHandle logH
                    , new_session = True
                    , close_fds = True
                    }
        -- createProcess closes the UseHandle in this process; a second close
        -- is harmless.
        (_, _, _, ph) <- Binary.detachedStdin cp createProcess
        hClose logH
        mpid <- getPid ph
        -- Under `serve` this process lives on and the child stays its own:
        -- reap it when it exits, or it lingers as a zombie (which identity
        -- already reads as gone).
        _ <- forkIO (void (try @SomeException (waitForProcess ph)))
        case mpid of
            Nothing -> fail ("detached: " <> Text.unpack d.detached_name <> " exited before its pid could be read")
            Just (CPid p) -> do
                let pid = fromIntegral p
                mident <- identify pid
                boot <- readBootId
                case mident of
                    Nothing -> fail ("detached: " <> Text.unpack d.detached_name <> " exited at once; see " <> d.detached_logFile)
                    Just ident ->
                        writePidFile d.detached_pidFile $
                            PidRecord
                                { pr_pid = pid
                                , pr_start = ident.id_start
                                , pr_boot = boot
                                , pr_argv = argvOf d
                                , pr_cwd = d.detached_cwd
                                }
                runReporter r (Started d.detached_name pid)

downDetached :: Reporter Report -> Detached -> IO ()
downDetached r d = do
    mrec <- readPidFile d.detached_pidFile
    case mrec of
        Nothing -> pure ()
        Just Nothing -> do
            runReporter r (Forgot d.detached_name "the pid file is not one this node wrote")
            forget d.detached_pidFile
        Just (Just rec) -> do
            ours <- isOurs rec
            case ours of
                Left why -> do
                    runReporter r (Forgot d.detached_name why)
                    forget d.detached_pidFile
                Right _ -> do
                    stopRecorded r d.detached_name d.detached_stop rec
                    forget d.detached_pidFile

{- | Stop the process a pid record names, re-proving identity before each
signal. Returns once it is gone (or was never ours); throws 'StillRunning'
if it survives @SIGKILL@ and the second grace period.
-}
stopRecorded :: Reporter Report -> Text -> Stop -> PidRecord -> IO ()
stopRecorded r name stop rec = do
    sent <- signalIfOurs stop.stop_signal
    if not sent
        then runReporter r (AlreadyGone name pid)
        else do
            runReporter r (Signalling name pid stop.stop_signal)
            gone <- waitGone stop.stop_grace
            if gone
                then runReporter r (Stopped name pid)
                else do
                    runReporter r (Killing name pid)
                    _ <- signalIfOurs sigKILL
                    gone' <- waitGone stop.stop_grace
                    if gone'
                        then runReporter r (Stopped name pid)
                        else throwIO (StillRunning name pid)
  where
    pid = rec.pr_pid
    cpid = CPid (fromIntegral pid)

    signalIfOurs :: Signal -> IO Bool
    signalIfOurs sig = do
        ours <- isOurs rec
        case ours of
            Left _ -> pure False
            Right ident -> do
                -- the group, when it still leads the one it was started
                -- with (new_session), so the workers it forked go too.
                void . try @SomeException $
                    if ident.id_pgrp == pid
                        then signalProcessGroup sig cpid
                        else signalProcess sig cpid
                pure True

    waitGone :: Micros -> IO Bool
    waitGone grace = do
        deadline <- (+ toNanos grace) <$> getMonotonicTimeNSec
        let go = do
                ours <- isOurs rec
                case ours of
                    Left _ -> pure True
                    Right _ -> do
                        now <- getMonotonicTimeNSec
                        if now >= deadline
                            then pure False
                            else threadDelay (unMicros (millis 20)) >> go
        go
    toNanos (Micros n) = fromIntegral n * 1000
