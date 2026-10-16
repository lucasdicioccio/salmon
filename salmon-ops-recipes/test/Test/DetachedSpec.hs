{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Detached": real, short-lived
children (@sleep@, @sh@) started in a temporary directory, driven through
'Salmon.Actions.UpDown.upTree'/'downTree'. Every process a case starts is
stopped by that case. No container, unit or VM.
-}
module Test.DetachedSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (CreateProcess (..), createProcess, getPid, proc, waitForProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.UpDown (Report (Blocked, Done, Failed, Skip))
import Salmon.Builtin.Extension (Extension (..))
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Detached hiding (Report)
import Salmon.Builtin.Nodes.Detached (Report (Forgot, Killing))
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Supervision (millis)
import Salmon.Reporter (ReporterM (..), silent)

import Test.Harness (runDownCapturing, runUpCapturing)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Detached"
        [ testCase "up starts it detached; a second up skips it; down stops it and removes the pid file" upSkipDown
        , testCase "kill it, and up starts it again" killedRestarts
        , testCase "a pid file naming an unrelated live process: down signals nothing" unrelatedPid
        , testCase "one that ignores SIGTERM is killed, with the process group" escalation
        , testCase "a changed command line replaces the running process" drift
        , testCase "pid file round trip; a bare pid is not a record" pidFileFormat
        , testCase "/proc/PID/stat with a command name holding ') '" statParsing
        , testCase "notes name env variables, never their values" notesNoEnvValues
        ]

-------------------------------------------------------------------------------

sleeper :: FilePath -> [String] -> Detached
sleeper dir args =
    (defaultDetached "sleeper" "sleep" args (dir </> "sleeper.pid") (dir </> "sleeper.log"))
        { detached_stop = Stop (detached_stop (defaultDetached "" "" [] "" "")).stop_signal (millis 500)
        }

outcomes :: [UpDown.Report Extension] -> [Text]
outcomes = concatMap one
  where
    one r = case r of
        Done a -> ["Done " <> a.shorthand]
        Skip a -> ["Skip " <> a.shorthand]
        Failed a _ -> ["Failed " <> a.shorthand]
        Blocked a -> ["Blocked " <> a.shorthand]
        _ -> []

recorded :: Detached -> IO PidRecord
recorded d = do
    m <- readPidFile d.detached_pidFile
    case m of
        Just (Just rec) -> pure rec
        other -> assertFailure ("no pid record: " <> show other) >> error "unreachable"

-- | Gone, or a zombie nobody reaped yet.
goneWithin :: Int -> Int -> IO Bool
goneWithin ms pid = do
    i <- identify pid
    case i of
        Nothing -> pure True
        Just ident | ident.id_zombie -> pure True
        _ | ms <= 0 -> pure False
        _ -> threadDelay 20000 >> goneWithin (ms - 20) pid

alive :: Int -> IO Bool
alive pid = maybe False (not . (.id_zombie)) <$> identify pid

-- | Last resort so a failing case never leaves its child behind.
cleanup :: Detached -> IO ()
cleanup d = do
    m <- readPidFile d.detached_pidFile
    case m of
        Just (Just rec) -> stopRecorded silent "cleanup" d.detached_stop rec
        _ -> pure ()

-------------------------------------------------------------------------------

upSkipDown :: IO ()
upSkipDown = withSystemTempDirectory "detached" $ \dir -> do
    let d = sleeper dir ["30"]
    flip finally (cleanup d) $ do
        r1 <- runUpCapturing (detached silent d)
        assertEqual "first up" ["Done detached"] (outcomes r1)
        rec <- recorded d
        ident <- identify rec.pr_pid
        case ident of
            Nothing -> assertFailure "not running after up"
            Just i -> do
                assertEqual "leads its own process group (new session)" rec.pr_pid i.id_pgrp
                assertEqual "argv" ["sleep", "30"] i.id_argv
        stdin0 <- readSymbolicLink ("/proc" </> show rec.pr_pid </> "fd" </> "0")
        assertEqual "stdin" "/dev/null" stdin0
        r2 <- runUpCapturing (detached silent d)
        assertEqual "second up" ["Skip detached"] (outcomes r2)
        rec2 <- recorded d
        assertEqual "same process" rec.pr_pid rec2.pr_pid
        r3 <- runDownCapturing (detached silent d)
        assertEqual "down" ["Done detached"] (outcomes r3)
        gone <- goneWithin 2000 rec.pr_pid
        assertBool "process gone" gone
        exists <- doesFileExist d.detached_pidFile
        assertBool "pid file removed" (not exists)

killedRestarts :: IO ()
killedRestarts = withSystemTempDirectory "detached" $ \dir -> do
    let d = sleeper dir ["30"]
    flip finally (cleanup d) $ do
        _ <- runUpCapturing (detached silent d)
        rec <- recorded d
        signalProcess sigKILL (CPid (fromIntegral rec.pr_pid))
        gone <- goneWithin 2000 rec.pr_pid
        assertBool "killed" gone
        v <- judge d
        case v of
            NotOurs _ -> pure ()
            other -> assertFailure ("expected NotOurs, got " <> show other)
        r <- runUpCapturing (detached silent d)
        assertEqual "up again" ["Done detached"] (outcomes r)
        rec2 <- recorded d
        assertBool "a new process" (rec2.pr_pid /= rec.pr_pid)
        isAlive <- alive rec2.pr_pid
        assertBool "running" isAlive
        _ <- runDownCapturing (detached silent d)
        pure ()

unrelatedPid :: IO ()
unrelatedPid = withSystemTempDirectory "detached" $ \dir -> do
    let d = sleeper dir ["30"]
    -- our own unrelated process, which down must not touch
    (_, _, _, ph) <- Binary.detachedStdin (proc "sleep" ["31"]){close_fds = True} createProcess
    Just (CPid p) <- getPid ph
    let pid = fromIntegral p
    flip finally (signalProcess sigKILL (CPid p) >> waitForProcess ph) $ do
        Just ident <- identify pid
        let real = PidRecord pid ident.id_start ident.id_boot ident.id_argv Nothing
            cases =
                [ ("a bare pid", BS8.pack (show pid) <> "\n")
                , ("another command line", renderPidFile real{pr_argv = ["sleep", "30"]})
                , ("another start time", renderPidFile real{pr_start = ident.id_start + 1})
                , ("another boot", renderPidFile real{pr_boot = "not-this-boot"})
                ]
        mapM_
            ( \(label, contents) -> do
                BS8.writeFile d.detached_pidFile contents
                forgot <- newIORef (0 :: Int)
                let r = ReporterM $ \ev -> case ev of
                        Forgot _ _ -> atomicModifyIORef' forgot (\n -> (n + 1, ()))
                        _ -> pure ()
                out <- runDownCapturing (detached r d)
                assertEqual (label <> ": down") ["Done detached"] (outcomes out)
                threadDelay 100000
                stillAlive <- alive pid
                assertBool (label <> ": the unrelated process is untouched") stillAlive
                exists <- doesFileExist d.detached_pidFile
                assertBool (label <> ": pid file forgotten") (not exists)
                n <- readIORef forgot
                assertEqual (label <> ": reported Forgot") 1 n
            )
            cases

escalation :: IO ()
escalation = withSystemTempDirectory "detached" $ \dir -> do
    let childFile = dir </> "child.pid"
        d =
            (defaultDetached "stubborn" "sh" ["-c", "trap '' TERM; sleep 30 & echo $! > " <> childFile <> "; wait"] (dir </> "s.pid") (dir </> "s.log"))
                { detached_stop = Stop (detached_stop (defaultDetached "" "" [] "" "")).stop_signal (millis 300)
                }
    flip finally (cleanup d) $ do
        _ <- runUpCapturing (detached silent d)
        rec <- recorded d
        child <- waitForFile 3000 childFile
        events <- newIORef []
        let r = ReporterM $ \ev -> atomicModifyIORef' events (\es -> (es <> [ev], ()))
        out <- runDownCapturing (detached r d)
        assertEqual "down" ["Done detached"] (outcomes out)
        es <- readIORef events
        assertBool ("escalated to SIGKILL: " <> show es) (any isKilling es)
        g1 <- goneWithin 2000 rec.pr_pid
        g2 <- goneWithin 2000 child
        assertBool "the leader is gone" g1
        assertBool "its forked child went with the group" g2
  where
    isKilling Killing{} = True
    isKilling _ = False
    waitForFile :: Int -> FilePath -> IO Int
    waitForFile ms path = do
        exists <- doesFileExist path
        contents <- if exists then BS8.readFile path else pure ""
        case BS8.readInt contents of
            Just (n, _) -> pure n
            Nothing
                | ms <= 0 -> assertFailure "the child never wrote its pid" >> error "unreachable"
                | otherwise -> threadDelay 20000 >> waitForFile (ms - 20) path

drift :: IO ()
drift = withSystemTempDirectory "detached" $ \dir -> do
    let d = sleeper dir ["30"]
        d' = sleeper dir ["31"]
    flip finally (cleanup d' >> cleanup d) $ do
        _ <- runUpCapturing (detached silent d)
        rec <- recorded d
        v <- judge d'
        case v of
            Drifted _ _ -> pure ()
            other -> assertFailure ("expected Drifted, got " <> show other)
        out <- runUpCapturing (detached silent d')
        assertEqual "up with the new command line" ["Done detached"] (outcomes out)
        g <- goneWithin 2000 rec.pr_pid
        assertBool "the old process is stopped" g
        rec' <- recorded d'
        Just i <- identify rec'.pr_pid
        assertEqual "the new one runs" ["sleep", "31"] i.id_argv
        _ <- runDownCapturing (detached silent d')
        pure ()

pidFileFormat :: IO ()
pidFileFormat = do
    let rec = PidRecord 4242 123456 "b00t" ["srv", "--port", "80 80", "line\nbreak"] (Just "/srv")
        rendered = renderPidFile rec
    assertEqual "first line is the pid" (Just "4242") (case BS8.lines rendered of (l : _) -> Just l; _ -> Nothing)
    assertEqual "round trip" (Just rec) (parsePidFile rendered)
    assertBool "a bare pid proves nothing" (isNothing (parsePidFile "4242\n"))
    assertBool "garbage" (isNothing (parsePidFile "hello\n{}\n"))

statParsing :: IO ()
statParsing = do
    let line = "777 (we) ird) name) S 1 778 778 0 -1 4194560 0 0 0 0 0 0 0 0 20 0 1 0 98765 0 0\n"
    assertEqual "state, pgrp, start" (Just ('S', 778, 98765)) (parseStat line)
    assertBool "truncated" (isNothing (parseStat "1 (x) S 1"))

notesNoEnvValues :: IO ()
notesNoEnvValues = do
    let d = (sleeper "/nonexistent" ["30"]){detached_env = [("API_TOKEN", "s3cr3t-value")]}
    case (detached silent d).node of
        Actions act -> do
            let ext = act.extension
                text = Text.unlines (ext.help : ext.notes)
            assertBool "names the variable" ("API_TOKEN" `Text.isInfixOf` text)
            assertBool "never its value" (not ("s3cr3t" `Text.isInfixOf` text))
            assertEqual "ref" (detachedRef d) ext.ref
        Actionless -> assertFailure "not an action node"
