{-# LANGUAGE ScopedTypeVariables #-}

{- | Layer 1 (a unix socket in a temp directory, no qemu, no root): the
monitor-socket half of "Salmon.Builtin.Nodes.Qemu".'shutdown' against a fake
monitor. It shows the three answers the down hook depends on: a guest that
powers off (the socket goes away, no hard stop needed), one that ignores ACPI
(still listening at the deadline, so the caller must stop it hard) and a VM
that was never running (nothing to send to).
-}
module Test.QemuShutdownSpec (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar_, readMVar, tryPutMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as C8
import Network.Socket hiding (shutdown)
import qualified Network.Socket.ByteString as SocketBS
import Salmon.Builtin.Nodes.Qemu (reset, shutdown)
import System.Directory (removeFile)
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
        ]

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
