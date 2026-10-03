{-# LANGUAGE ScopedTypeVariables #-}

{- | Layer 1 (a unix socket in a temp directory, no qemu, no root): the
monitor-socket half of "Salmon.Builtin.Nodes.Qemu".'shutdown' against a fake
monitor. It shows the three answers the down hook depends on: a guest that
powers off (the socket goes away, no hard stop needed), one that ignores ACPI
(still listening at the deadline, so the caller must stop it hard) and a VM
that was never running (nothing to send to).

The run state ('runState', 'pause', 'resume') is read two ways: the parser on
what a real monitor printed (QEMU 8.2.2, captured, terminal echo and all),
and the three calls against a real @qemu-system-x86_64@ started with no disk
and no guest -- a process, not a VM -- skipped loudly where qemu is missing.
-}
module Test.QemuShutdownSpec (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar_, readMVar, tryPutMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as C8
import Network.Socket hiding (shutdown)
import qualified Network.Socket.ByteString as SocketBS
import Salmon.Builtin.Nodes.Qemu (RunState (..), interpretStatus, listening, pause, reset, resume, runState, shutdown)
import System.Directory (findExecutable, removeFile)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), StdStream (..), proc, terminateProcess, waitForProcess, withCreateProcess)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Qemu.shutdown (monitor socket)"
        [ testCase "a guest that powers off: True, and the command was sent" powersOff
        , testCase "a guest that ignores ACPI: False at the deadline" ignoresAcpi
        , testCase "no monitor listening: True, nothing sent" neverRan
        , testCase "reset sends system_reset" sendsReset
        , testCase "info status: running, paused, and neither" readsStatus
        , testCase "info status: half an answer is no answer" partialStatus
        , testCase "no monitor listening: the run state is unknown" unknownWithoutMonitor
        , testCase "a real qemu: paused, resumed, paused again" realMonitor
        ]

-- | What QEMU 8.2.2's monitor writes back for @info status@ on a fresh connection.
captured :: String -> C8.ByteString
captured status =
    C8.pack $
        "QEMU 8.2.2 monitor - type 'help' for more information\r\n"
            <> "(qemu) i\ESC[K\ESC[Din\ESC[K\ESC[D\ESC[Dinfo status\ESC[K\r\n"
            <> "VM status: "
            <> status
            <> "\r\n(qemu) "

readsStatus :: IO ()
readsStatus = do
    interpretStatus (captured "running") @?= Just Running
    interpretStatus (captured "paused") @?= Just Paused
    -- qemu started with -S, before anything told it to go
    interpretStatus (captured "paused (prelaunch)") @?= Just Paused
    interpretStatus (captured "shutdown") @?= Just (OtherState "shutdown")

partialStatus :: IO ()
partialStatus = do
    interpretStatus (C8.pack "QEMU 8.2.2 monitor\r\n(qemu) info status\r\n") @?= Nothing
    -- the line is there but not finished: "paused" may yet follow "VM status: "
    interpretStatus (C8.pack "(qemu) info status\r\nVM status: ") @?= Nothing
    interpretStatus (C8.pack "(qemu) info status\r\nVM status: runn") @?= Nothing

unknownWithoutMonitor :: IO ()
unknownWithoutMonitor = withSystemTempDirectory "qemu-mon" $ \dir -> do
    st <- runState (dir </> "absent.sock")
    st @?= Nothing
    ok <- pause (dir </> "absent.sock")
    ok @?= False

{- | qemu itself, with nothing to run: @-S@ starts it stopped, which is the
state 'pause' produces, so the three calls can be read back without a guest.
-}
realMonitor :: IO ()
realMonitor = do
    qemu <- findExecutable "qemu-system-x86_64"
    case qemu of
        Nothing -> hPutStrLn stderr "SKIPPED: `qemu-system-x86_64` not found on PATH; the monitor's run state is only checked against captured output"
        Just bin -> withSystemTempDirectory "qm" $ \dir -> do
            let sock = dir </> "m.sock"
                cp =
                    (proc bin ["-display", "none", "-machine", "accel=tcg", "-m", "32", "-S", "-monitor", "unix:" <> sock <> ",server,nowait"])
                        { std_in = NoStream
                        , std_out = NoStream
                        , std_err = NoStream
                        }
            withCreateProcess cp $ \_ _ _ ph -> do
                waitListening sock (50 :: Int)
                runState sock >>= (@?= Just Paused)
                resume sock >>= (@?= True)
                runState sock >>= (@?= Just Running)
                pause sock >>= (@?= True)
                runState sock >>= (@?= Just Paused)
                terminateProcess ph
                void (waitForProcess ph)
  where
    waitListening _ 0 = fail "qemu never opened its monitor"
    waitListening sock n = do
        up <- listening sock
        if up then pure () else threadDelay 100000 >> waitListening sock (n - 1)

-- | The commands received: the liveness probes (connect, hang up) send nothing and are dropped.
commands :: Fake -> IO [String]
commands fake = filter (not . null) <$> readMVar (received fake)

-- | What the fake monitor received, and whether it exits on @system_powerdown@.
data Fake = Fake {received :: MVar [String]}

withFake :: Bool -> (FilePath -> Fake -> IO a) -> IO a
withFake exitsOnPowerdown act =
    withSystemTempDirectory "qemu-mon" $ \dir -> do
        let path = dir </> "mon.sock"
        got <- newMVar []
        listener <- socket AF_UNIX Stream defaultProtocol
        bind listener (SockAddrUnix path)
        listen listener 5
        gone <- newEmptyMVar
        tid <- forkIO $ forever $ do
            (conn, _) <- accept listener
            void $ forkIO $ do
                line <- readLine conn
                modifyMVar_ got (pure . (line :))
                if exitsOnPowerdown && line == "system_powerdown"
                    then do
                        close listener
                        void (try @SomeException (removeFile path))
                        void (tryPutMVar gone ())
                    else pure ()
                close conn
        r <- act path (Fake got)
        killThread tid
        void (try @SomeException (close listener))
        pure r
  where
    readLine conn = do
        bs <- SocketBS.recv conn 4096
        pure (takeWhile (/= '\n') (C8.unpack bs))

powersOff :: IO ()
powersOff = withFake True $ \path fake -> do
    ok <- shutdown 5 path
    ok @?= True
    commands fake >>= (@?= ["system_powerdown"])

ignoresAcpi :: IO ()
ignoresAcpi = withFake False $ \path fake -> do
    ok <- shutdown 1 path
    ok @?= False
    commands fake >>= (@?= ["system_powerdown"])

neverRan :: IO ()
neverRan = withSystemTempDirectory "qemu-mon" $ \dir -> do
    ok <- shutdown 1 (dir </> "absent.sock")
    ok @?= True

sendsReset :: IO ()
sendsReset = withFake False $ \path fake -> do
    ok <- reset path
    ok @?= True
    threadDelay 100000
    commands fake >>= (@?= ["system_reset"])
