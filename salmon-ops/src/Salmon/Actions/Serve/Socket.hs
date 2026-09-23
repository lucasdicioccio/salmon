{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The line protocol over a unix socket: @run serve --listen PATH@.

Milestone 2 of @specs\/generic-server.md@. A 'Listener' is a bound unix
socket plus the connections currently open on it. It plugs into
"Salmon.Actions.Serve" at the two places that module leaves open: as one
more 'Serve.Producer' into the loop's inbox ('listenerProducer', one
'Serve.Origin' per connection, standard input untouched beside it), and as
the reporters the loop is handed ('listenerReporters'), which write every
report /typed on a connection/ back to that connection as JSON lines —
"Salmon.Reporter.Tagged"'s encoding, the same objects @--json@ prints — and
hand every report, whoever typed it, on to the loop's own reporter
unchanged. A client therefore sees exactly the reports for its own lines;
what the tending machines say between commands, and what other clients
typed, goes to the loop's own reporter only.

Which report belongs to whom is the loop's knowledge, not this module's:
'Serve.serveAttributed' stamps each report with the 'Serve.Origin' of the
line being handled, and this module only looks the origin up. The one
ordering fact this leans on: a connection is closed when the loop reports
'Serve.HungUp' for it, which the loop does after every line typed on it has
been handled — so a client that sends a line and shuts its writing side
still gets its reports.

The protocol is the input language as typed on standard input, one command
per line; @quit@ from any client ends the loop exactly as it does from
standard input. There is no per-connection @text@ mode and no
authentication: a unix socket inherits the filesystem's permissions, which
is why the socket file is created owner-only (mode 0600) and why TCP is not
here (see the spec's security section). A stale socket file at the path is
replaced only if nothing answers on it; something answering means another
@serve@ is listening there, and this one refuses to start rather than take
its path.
-}
module Salmon.Actions.Serve.Socket (
    -- * Listening
    Listener,
    listenerPath,
    listenerSocket,
    withUnixListener,
    ListenError (..),

    -- * Plugging into the loop
    listenerProducer,
    listenerReporters,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, swapTVar, writeTChan, writeTVar)
import Control.Exception (Exception, IOException, bracket, finally, throwIO, try)
import Control.Monad (forM_, forever, void)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Network.Socket as Socket
import Network.Socket (Socket)
import System.Directory (removeFile)
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hGetLine, hIsEOF, hSetBuffering)
import System.Posix.Files (fileExist, getFileStatus, isSocket, setFileMode)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), Line (..), Origin (..), Producer (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension)
import Salmon.Reporter
import Salmon.Reporter.Tagged (Tagged (..), reportJSONLines)

-------------------------------------------------------------------------------

-- | A bound, listening unix socket and the connections open on it.
data Listener = Listener
    { listenerPath :: FilePath
    , listenerSocket :: Socket
    , listenerClients :: TVar (Map Origin Handle)
    -- ^ every connection still open, by the origin its lines carry
    , listenerCounter :: IORef Int
    -- ^ next connection number; an origin is never reused within a run
    }

-- | Why a 'Listener' could not be made.
data ListenError
    = -- | something answered a connection attempt at the path: another
      -- @serve@ is listening there
      AlreadyListening FilePath
    | -- | the path exists and is not a socket, so it is not ours to remove
      NotASocket FilePath
    deriving (Show, Eq)

instance Exception ListenError

{- | Bind a unix socket at the path, owner-only, and hand it over; on the
way out close every connection still open, close the socket, and remove
the file.

Refuses with 'AlreadyListening' if a connection to the path succeeds — the
path is somebody's — and with 'NotASocket' if a non-socket sits there. A
socket file nothing answers on is stale (its @serve@ died without removing
it) and is removed first.

The mode is set between the bind and the listen. That order is what makes
it race-free without touching the process's file creation mask (which is
process-global, and a test suite running this beside anything that creates
files would notice): a socket that is bound but not yet listening refuses
every connection, so nobody can get in during the moment it exists with
the default mode.
-}
withUnixListener :: FilePath -> (Listener -> IO a) -> IO a
withUnixListener path = bracket acquire release
  where
    acquire :: IO Listener
    acquire = do
        clearStale
        sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
        Socket.bind sock (Socket.SockAddrUnix path) `onFailure` Socket.close sock
        (setFileMode path 0o600 >> Socket.listen sock 16) `onFailure` (Socket.close sock >> removeFile path)
        Listener path sock <$> newTVarIO Map.empty <*> newIORef 0

    release :: Listener -> IO ()
    release l = do
        clients <- readTVarIO (listenerClients l)
        traverse_ (void . tryIO . hClose) (Map.elems clients)
        Socket.close (listenerSocket l)
        void (tryIO (removeFile path))

    onFailure :: IO a -> IO () -> IO a
    onFailure act cleanup = do
        r <- tryIO act
        case r of
            Right a -> pure a
            Left e -> cleanup >> throwIO e

    clearStale :: IO ()
    clearStale = do
        there <- fileExist path
        if not there
            then pure ()
            else do
                st <- getFileStatus path
                if not (isSocket st)
                    then throwIO (NotASocket path)
                    else do
                        answered <- answers
                        if answered
                            then throwIO (AlreadyListening path)
                            else removeFile path

    -- connect first: a listener on the path accepts, a stale file refuses
    answers :: IO Bool
    answers =
        bracket
            (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
            Socket.close
            ( \probe -> do
                r <- tryIO (Socket.connect probe (Socket.SockAddrUnix path))
                pure (either (const False) (const True) r)
            )

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-------------------------------------------------------------------------------

{- | Accept connections for as long as the loop runs, each one a reader
thread pushing the lines it types into the inbox under an 'Origin' of its
own, then an 'Eof' when it hangs up.

The connection is /not/ closed when its reader sees end of input: the loop
may still be handling — or not yet have reached — a line this client typed,
and the client is owed those reports. 'listenerReporters' closes it on the
loop's 'Serve.HungUp' for this origin instead. Only when the loop ends and
this producer's thread is killed with it are the connections still open
closed outright — here, so that a client still attached reads end of file
the moment the loop is gone rather than whenever the listener is released;
which is what @quit@ promises: nothing changes on the way out, and whoever
is still connected is simply hung up on.
-}
listenerProducer :: Listener -> Producer
listenerProducer l = Producer $ \inbox -> do
    readers <- newIORef []
    let accepting = forever $ do
            (conn, _) <- Socket.accept (listenerSocket l)
            n <- atomicModifyIORef' (listenerCounter l) (\k -> (k + 1, k))
            h <- Socket.socketToHandle conn ReadWriteMode
            hSetBuffering h LineBuffering
            let origin = Origin (Text.pack (listenerPath l <> "#" <> show n))
            atomically (modifyTVar' (listenerClients l) (Map.insert origin h))
            tid <- forkIO (readLines origin h inbox `finally` atomically (writeTChan inbox (Eof origin)))
            atomicModifyIORef' readers (\ts -> (tid : ts, ()))
    accepting `finally` hangUp readers
  where
    hangUp readers = do
        atomicModifyIORef' readers (\ts -> ([], ts)) >>= traverse_ killThread
        clients <- atomically (swapTVar (listenerClients l) Map.empty)
        traverse_ (void . tryIO . hClose) (Map.elems clients)

    -- a client whose socket errors out mid-read is the same to the loop
    -- as one that finished: its 'Eof' follows from the finally either way.
    readLines origin h inbox = do
        r <- tryIO $ do
            eof <- hIsEOF h
            if eof
                then pure False
                else do
                    line <- hGetLine h
                    atomically (writeTChan inbox (Line origin line))
                    pure True
        case r of
            Right True -> readLines origin h inbox
            _ -> pure ()

{- | The two reporters the loop takes, built over the loop's own.

Every report goes to @own@ exactly as it would without a listener. A report
stamped with one of this listener's origins is also encoded as one JSON
line ('reportJSONLines') on that connection. A write that fails — the
client went away while its command was being handled — drops the
connection; the loop's 'Serve.HungUp' for it, which follows, then finds
nothing to close. The 'Serve.HungUp' itself is the one report that is also
an instruction here: the connection it names is closed, since every line it
typed has been handled by the time the loop says so.
-}
listenerReporters ::
    Listener ->
    Reporter Tagged ->
    (Reporter (Attributed Serve.Report), Reporter (Attributed (UpDown.Report Extension)))
listenerReporters l own = (serveR, updownR)
  where
    serveR :: Reporter (Attributed Serve.Report)
    serveR = ReporterM $ \(Attributed origin rep) -> do
        runReporter own (FromServe rep)
        forM_ origin (echo (FromServe rep))
        case rep of
            Serve.HungUp gone -> disconnect gone
            _ -> pure ()

    updownR :: Reporter (Attributed (UpDown.Report Extension))
    updownR = ReporterM $ \(Attributed origin rep) -> do
        runReporter own (FromUpDown rep)
        forM_ origin (echo (FromUpDown rep))

    echo :: Tagged -> Origin -> IO ()
    echo tagged origin = do
        clients <- readTVarIO (listenerClients l)
        forM_ (Map.lookup origin clients) $ \h -> do
            r <- tryIO (runReporter (reportJSONLines h) tagged)
            case r of
                Right () -> pure ()
                Left _ -> disconnect origin

    disconnect :: Origin -> IO ()
    disconnect origin = do
        mh <- atomically $ do
            clients <- readTVar (listenerClients l)
            let (mh, clients') = Map.updateLookupWithKey (\_ _ -> Nothing) origin clients
            writeTVar (listenerClients l) clients'
            pure mh
        forM_ mh (void . tryIO . hClose)
