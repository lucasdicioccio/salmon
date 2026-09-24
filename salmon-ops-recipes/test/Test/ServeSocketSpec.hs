{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for "Salmon.Actions.Serve.Socket" (milestone 2 of
@specs\/generic-server.md@): the @run serve@ loop with a unix socket
listener beside its standard input, driven by real clients over real
connections.

What is under test is the plumbing, not the recipe — the nodes are the
same counter-bumping stubs 'Test.ServeModelSpec' uses. The claims: two
clients interleaving commands each read exactly the reports for their own
lines and nothing else, and the world they leave is the one the same
script typed on one stdin would leave; a client hanging up is reported and
does not end the loop; @quit@ from a client does; a client that half-closes
after typing still gets its reports; and the socket file is owner-only,
refused while something listens on it, and replaced when stale.
-}
module Test.ServeSocketSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecodeStrict)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Network.Socket as Socket
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as SocketBS
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode), hClose, hPutStr, withFile)
import System.IO.Temp (withSystemTempFile)
import System.Posix.Files (fileMode, getFileStatus)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Direction (..), NodeState (..), World (..))
import qualified Salmon.Actions.Serve.Socket as Socket
import Salmon.Builtin.Extension (Track', deps, down, nodeps, op, ref, up)
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)
import qualified Salmon.Reporter.Tagged as Tagged

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Serve.Socket"
        [ testCase "two clients interleaving commands each read only their own reports" twoClientsInterleave
        , testCase "a client hanging up is reported and does not end the loop" hangUpDoesNotEndTheLoop
        , testCase "`quit` from a client ends the loop" quitFromAClientEndsTheLoop
        , testCase "a client that half-closes after typing still gets its reports" halfCloseStillAnswered
        , testCase "the socket is owner-only, refused while live, replaced when stale" socketFileRules
        ]

-------------------------------------------------------------------------------
-- the thing served: counters per node name, as in Test.ServeModelSpec

newtype Spec = Spec {specNames :: [String]}
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: [String] -> Either Text Spec
parseSpec [] = Left "expected at least one node name"
parseSpec args = Right (Spec args)

spyProgram :: IORef (Map String Int) -> IORef (Map String Int) -> Track' Spec
spyProgram upsRef downsRef = Track $ \spec ->
    op "socket-root" (deps (fmap nodeOp spec.specNames)) $ \actions ->
        actions{ref = mkRef "socket-root" spec.specNames}
  where
    nodeOp name =
        op "socket-node" nodeps $ \actions ->
            actions
                { ref = mkRef "socket-node" name
                , up = bump upsRef name
                , down = bump downsRef name
                }
    bump r name = atomicModifyIORef' r (\m -> (Map.insertWith (+) name 1 m, ()))

-------------------------------------------------------------------------------
-- a running loop with a listener

data Running = Running
    { runningPath :: FilePath
    , runningStdin :: Handle
    -- ^ the writing end of the loop's standard input; closing it ends the loop
    , runningWorld :: MVar (World Spec Spec)
    , runningOwn :: IO [Tagged.Tagged]
    -- ^ everything the loop's own reporter saw
    , runningUps :: IORef (Map String Int)
    , runningDowns :: IORef (Map String Int)
    }

{- | Start the loop on a temp socket with a pipe for standard input, hand
it to the test, and make sure it has ended before the temp dir goes.
-}
withRunning :: (Running -> IO a) -> IO a
withRunning act =
    withTempDir $ \dir -> do
        let path = dir </> "serve.sock"
        (stdinR, stdinW) <- createPipe
        worldVar <- newEmptyMVar
        (own, seen) <- capture
        upsRef <- newIORef Map.empty
        downsRef <- newIORef Map.empty
        Socket.withUnixListener path $ \listener -> do
            let (serveR, updownR) = Socket.listenerReporters listener own
            _ <- forkIO $ do
                w <-
                    Serve.serveAttributed
                        []
                        Nothing
                        True
                        serveR
                        updownR
                        parseSpec
                        (Configure pure)
                        (spyProgram upsRef downsRef)
                        Nothing
                        [Serve.stdinProducer stdinR, Socket.listenerProducer listener]
                putMVar worldVar w
            let running = Running path stdinW worldVar seen upsRef downsRef
            r <- act running
            -- whatever the test did, the loop must be gone before the socket
            -- and the directory are: close stdin (harmless if the loop
            -- already ended on `quit`) and wait for it.
            _ <- try (hClose stdinW) :: IO (Either IOError ())
            _ <- awaitWorld running
            pure r

awaitWorld :: Running -> IO (World Spec Spec)
awaitWorld running = do
    mw <- timeout (10 * 1000000) (takeMVar (runningWorld running))
    case mw of
        Nothing -> assertFailure "the loop did not end"
        Just w -> do
            -- put it back so a second wait (withRunning's own) finds it
            putMVar (runningWorld running) w
            pure w

-------------------------------------------------------------------------------
-- a client: a raw socket, so a test can half-close it

data Client = Client
    { clientSocket :: Socket
    , clientBuffer :: IORef ByteString.ByteString
    , clientSeen :: IORef [Value]
    -- ^ every object read so far, oldest first once reversed
    }

withClient :: Running -> (Client -> IO a) -> IO a
withClient running = bracket (connectClient running) (Socket.close . clientSocket)

connectClient :: Running -> IO Client
connectClient running = do
    sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
    Socket.connect sock (Socket.SockAddrUnix (runningPath running))
    Client sock <$> newIORef ByteString.empty <*> newIORef []

send :: Client -> String -> IO ()
send c line = SocketBS.sendAll (clientSocket c) (Char8.pack (line <> "\n"))

-- | One line off the connection, or 'Nothing' at end of file.
readLine :: Client -> IO (Maybe ByteString.ByteString)
readLine c = do
    buf <- readIORef (clientBuffer c)
    case Char8.elemIndex '\n' buf of
        Just i -> do
            let (line, rest) = ByteString.splitAt i buf
            atomicModifyIORef' (clientBuffer c) (const (ByteString.drop 1 rest, ()))
            pure (Just line)
        Nothing -> do
            chunk <- SocketBS.recv (clientSocket c) 4096
            if ByteString.null chunk
                then pure Nothing
                else do
                    atomicModifyIORef' (clientBuffer c) (\b -> (b <> chunk, ()))
                    readLine c

-- | Read JSON objects until one of the given kind arrives, and return the
-- kinds read in this call, that one included.
readUntil :: Client -> Text -> IO [Text]
readUntil c wanted = do
    r <- timeout (10 * 1000000) (go [])
    case r of
        Nothing -> do
            seen <- readIORef (clientSeen c)
            assertFailure ("timed out waiting for " <> Text.unpack wanted <> "; seen so far: " <> show (fmap kindOf (reverse seen)))
        Just ks -> pure ks
  where
    go acc = do
        mline <- readLine c
        case mline of
            Nothing -> assertFailure ("connection closed while waiting for " <> Text.unpack wanted <> "; got " <> show (reverse acc))
            Just line ->
                case eitherDecodeStrict line of
                    Left err -> assertFailure ("not a JSON line: " <> err <> ": " <> Char8.unpack line)
                    Right v -> do
                        atomicModifyIORef' (clientSeen c) (\vs -> (v : vs, ()))
                        let k = kindOf v
                        if k == wanted then pure (reverse (k : acc)) else go (k : acc)

-- | Send a line and read its reports through to the one that ends them.
ask :: Client -> String -> Text -> IO [Text]
ask c line lastKind = send c line >> readUntil c lastKind

kindOf :: Value -> Text
kindOf (Object o) = case KeyMap.lookup "kind" o of
    Just (String k) -> k
    _ -> "<no kind>"
kindOf _ = "<not an object>"

kindsSeen :: Client -> IO [Text]
kindsSeen c = fmap kindOf . reverse <$> readIORef (clientSeen c)

-------------------------------------------------------------------------------

{- | The script both halves of the comparison run: A declares and asks for
history, B asks for status and retires; each waits for the previous
command's last report before the next client types, so the inbox order is
the script order.
-}
twoClientsInterleave :: IO ()
twoClientsInterleave = do
    (sequentialUps, sequentialDowns, sequentialWorld) <- runScript script
    withRunning $ \running -> do
        withClient running $ \a -> withClient running $ \b -> do
            _ <- ask a "supervise off" "supervised"
            _ <- ask a "up n1" "converge-stop"
            _ <- ask b "status" "status"
            _ <- ask a "up n1 n2" "converge-stop"
            _ <- ask b "status" "status"
            _ <- ask b "down n1" "converge-stop"
            _ <- ask a "history" "history"
            aKinds <- kindsSeen a
            bKinds <- kindsSeen b
            -- A never asked for status; B never declared, asked for history,
            -- or changed supervision
            assertBool ("A read a status that B asked for: " <> show aKinds) ("status" `notElem` aKinds)
            assertBool ("B read something only A asked for: " <> show bKinds) $
                not (any (`elem` ["supervised", "history"]) bKinds)
            -- and each read the shape its own lines produce
            assertEqual "A's first report" (Just "supervised") (headMay aKinds)
            assertEqual "A's last report" (Just "history") (lastMay aKinds)
            assertEqual "B's first two reports" ["status", "status"] (take 2 bKinds)
            assertEqual "B's last report" (Just "converge-stop") (lastMay bKinds)
            assertEqual "declarations A read" 2 (length (filter (== "declared") aKinds))
            assertEqual "declarations B read" 1 (length (filter (== "declared") bKinds))
            -- neither read anything the loop said outside a command
            assertBool "no hang-up reached a client" (all (`notElem` ["hung-up", "started", "stopped"]) (aKinds <> bKinds))
            -- the loop's own reporter saw every one of those reports too,
            -- and nothing more than its own bookends
            own <- runningOwn running
            let ownKinds = fmap kindOf' own
            assertEqual
                "the loop's own reporter saw each client's reports"
                (length aKinds + length bKinds)
                (length (filter (`notElem` ["started", "stopped", "hung-up"]) ownKinds))
        hClose (runningStdin running)
        w <- awaitWorld running
        ups <- readIORef (runningUps running)
        downs <- readIORef (runningDowns running)
        assertEqual "ups agree with the one-stdin run" sequentialUps ups
        assertEqual "downs agree with the one-stdin run" sequentialDowns downs
        assertEqual "the world agrees with the one-stdin run" (worldShape sequentialWorld) (worldShape w)
  where
    script = ["supervise off", "up n1", "status", "up n1 n2", "status", "down n1", "history"]
    headMay xs = case xs of
        [] -> Nothing
        (x : _) -> Just x
    lastMay xs = case xs of
        [] -> Nothing
        _ -> Just (last xs)
    kindOf' t = case t of
        Tagged.FromServe rep -> case rep of
            Serve.Started -> "started"
            Serve.Stopped -> "stopped"
            Serve.HungUp{} -> "hung-up"
            _ -> "serve"
        _ -> "node"

hangUpDoesNotEndTheLoop :: IO ()
hangUpDoesNotEndTheLoop =
    withRunning $ \running -> do
        withClient running $ \a -> do
            _ <- ask a "supervise off" "supervised"
            _ <- ask a "up n1" "converge-stop"
            pure ()
        -- A is gone. The loop says so on its own reporter, once its lines
        -- are handled, and keeps going.
        waitFor "the hang-up to be reported" $ do
            own <- runningOwn running
            pure (any isHangUp own)
        withClient running $ \b -> do
            _ <- ask b "status" "status"
            seen <- readIORef (clientSeen b)
            case seen of
                [Object o] -> case KeyMap.lookup "nodes" o of
                    Just (Array nodes) -> assertEqual "B still sees A's nodes" 2 (length nodes)
                    _ -> assertFailure "status without nodes"
                _ -> assertFailure ("B expected exactly one status object, got " <> show seen)
        -- B's hang-up too, before stdin ends the loop: the two 'Eof's are
        -- pushed by different threads and would otherwise race.
        waitFor "the second hang-up" $ do
            own <- runningOwn running
            pure (length (filter isHangUp own) == 2)
        hClose (runningStdin running)
        _ <- awaitWorld running
        own <- runningOwn running
        assertEqual "hang-ups reported" 2 (length (filter isHangUp own))
        assertBool "the loop ended on stdin's end of input" (any isStopped own)
  where
    isHangUp t = case t of
        Tagged.FromServe Serve.HungUp{} -> True
        _ -> False
    isStopped t = case t of
        Tagged.FromServe Serve.Stopped -> True
        _ -> False

quitFromAClientEndsTheLoop :: IO ()
quitFromAClientEndsTheLoop =
    withRunning $ \running -> do
        withClient running $ \a -> do
            _ <- ask a "up n1" "converge-stop"
            send a "quit"
            -- standard input is still open: only the client's quit can end this
            w <- awaitWorld running
            assertEqual "the declaration survived" 1 (Map.size (worldLedger w))
            -- the loop is gone and so is the connection
            rest <- timeout (10 * 1000000) (readLine a)
            assertEqual "the client reads end of file" (Just Nothing) rest
        own <- runningOwn running
        assertBool "quit is not stdin closing" (not (any isStopped own))
  where
    isStopped t = case t of
        Tagged.FromServe Serve.Stopped -> True
        _ -> False

halfCloseStillAnswered :: IO ()
halfCloseStillAnswered =
    withRunning $ \running ->
        withClient running $ \a -> do
            send a "supervise off"
            send a "up n1 n2"
            send a "status"
            -- nothing more to say, and the loop has not necessarily read a
            -- word of it yet
            Socket.shutdown (clientSocket a) Socket.ShutdownSend
            kinds <- readUntil a "status"
            assertBool ("all three commands were answered: " <> show kinds) $
                all (`elem` kinds) ["supervised", "declared", "converge-stop", "status"]
            -- and then the loop closes the connection, having said hung-up
            -- on its own reporter only
            rest <- timeout (10 * 1000000) (readLine a)
            assertEqual "the connection is closed after the last report" (Just Nothing) rest
            own <- runningOwn running
            assertBool "the hang-up was reported" (any isHangUp own)
  where
    isHangUp t = case t of
        Tagged.FromServe Serve.HungUp{} -> True
        _ -> False

socketFileRules :: IO ()
socketFileRules =
    withTempDir $ \dir -> do
        let path = dir </> "rules.sock"
        Socket.withUnixListener path $ \_ -> do
            st <- getFileStatus path
            assertEqual "owner-only" 0o600 (fileMode st .&. 0o777)
            r <- try (Socket.withUnixListener path (const (pure ())))
            assertEqual "a live socket is refused" (Left (Socket.AlreadyListening path)) r
        -- a stale socket file: bound once, never listened on again
        stale <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
        Socket.bind stale (Socket.SockAddrUnix path)
        Socket.close stale
        replaced <- try (Socket.withUnixListener path (const (pure ())))
        assertEqual "a stale socket file is replaced" (Right ()) (replaced :: Either Socket.ListenError ())
        -- a file that is not a socket is nobody's to remove
        writeFile path "not a socket\n"
        notSock <- try (Socket.withUnixListener path (const (pure ())))
        assertEqual "a regular file is refused" (Left (Socket.NotASocket path)) notSock

-------------------------------------------------------------------------------

waitFor :: String -> IO Bool -> IO ()
waitFor what cond = do
    r <- timeout (10 * 1000000) go
    when (r == Nothing) (assertFailure ("timed out waiting for " <> what))
  where
    go = do
        ok <- cond
        unless ok go

-- | The same script on one stdin, for the comparison.
runScript :: [String] -> IO (Map String Int, Map String Int, World Spec Spec)
runScript script = do
    upsRef <- newIORef Map.empty
    downsRef <- newIORef Map.empty
    w <- withScript script $ \h ->
        Serve.serveWith [] Nothing True silent silent parseSpec (Configure pure) (spyProgram upsRef downsRef) h
    (,,) <$> readIORef upsRef <*> readIORef downsRef <*> pure w

withScript :: [String] -> (Handle -> IO a) -> IO a
withScript ls act =
    withSystemTempFile "salmon-serve-socket-script" $ \path h -> do
        hPutStr h (unlines ls)
        hClose h
        withFile path ReadMode act

-- | The comparable part of a 'World', as 'Test.ServeModelSpec' takes it.
worldShape :: World Spec Spec -> (Map Ref (Direction, Convergence), Set.Set Ref, Int, Int, Int)
worldShape w =
    ( Map.map (\st -> (st.nodeDirection, st.nodeConvergence)) w.worldNodes
    , Map.keysSet w.worldMagma
    , Map.size w.worldLedger
    , length w.worldEpochs
    , length w.worldLog
    )
