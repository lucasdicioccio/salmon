{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Layer 1 coverage for where a command's output goes
("Salmon.Builtin.Nodes.Binary"'s 'Binary.Routing') and for the per-node line
it rides on ("Salmon.Builtin.NodeLog"): real subprocesses, all of them
@\/bin\/sh@, none longer than a second.

What only a real process shows is the point of the feature: that a streamed
line reaches the listener /while the command is still running/ (the case
holds the command open on a file it waits for, and creates that file only
after the first line has arrived), that what is kept is bounded however much
is printed, and that a command which never reads its standard input, or is
routed to a file or to nothing, still ends with its own exit code.

The sink registry is process-wide and this suite runs its groups in parallel
in one process, so every assertion about what a sink saw filters by the
node's own 'Ref'.
-}
module Test.BinaryOutputSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Functor.Identity (runIdentity)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..), Op, Track', ignoreTrack, nodeps, op)
import qualified Salmon.Builtin.NodeLog as NodeLog
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), Routing (..), Sink (..))
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Reporter (ReporterM (..), silent)
import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Binary: where output goes"
        [ testGroup
            "the tail a stream keeps"
            [ testCase "everything, when it fits" tailFits
            , testCase "whole trailing lines, when it does not" tailDropsFromTheFront
            , testCase "the end of a line longer than the bound" tailCutsALongLine
            ]
        , testCase "captured is what it was: both streams whole, nothing streamed" capturedUnchanged
        , testCase "a streamed line arrives while the command is still running" streamsWhileRunning
        , testCase "a stream keeps a bounded tail of megabytes, and reports every line" boundedTail
        , testCase "discarded output is not kept, and the exit code is" discardedKeepsExitCode
        , testCase "output appended to a file lands there, both streams, across runs" appendsToFile
        , testCase "each stream has its own sink" mixedSinks
        , testCase "a command that never reads its standard input is not a failure" unreadStdin
        , testCase "a failing streamed command throws with the tail it kept" failureCarriesTail
        , testCase "a node's streamed lines are node log lines under its ref, not reports" nodeLinesGoToTheLog
        , testCase "a captured node says nothing to the log" capturedNodeIsSilent
        , testCase "a node's own message is split into lines; a sink stops hearing when it leaves" sayAndLeave
        ]

-------------------------------------------------------------------------------

within :: Int -> IO a -> IO a
within secs act = do
    r <- timeout (secs * 1000000) act
    maybe (assertFailure ("timed out after " <> show secs <> "s")) pure r

sh :: Command "sh" String
sh = Command $ \script -> proc "/bin/sh" ["-c", script]

shTrack :: Track' (Binary "sh")
shTrack = ignoreTrack

-- | Collects what a callback or reporter is handed, in order.
collector :: IO (a -> IO (), IO [a])
collector = do
    cell <- newIORef []
    pure (\x -> atomicModifyIORef' cell (\xs -> (x : xs, ())), reverse <$> readIORef cell)

-------------------------------------------------------------------------------

tailFits :: IO ()
tailFits = assertEqual "" "a\nbb\n" (Binary.tailOf 100 ["a", "bb"])

tailDropsFromTheFront :: IO ()
tailDropsFromTheFront = do
    -- "three\n" is 6 bytes, "four\n" is 5: 11 fit in 12, "two\n" would not
    assertEqual "" "three\nfour\n" (Binary.tailOf 12 ["one", "two", "three", "four"])
    assertEqual "nothing but the last" "four\n" (Binary.tailOf 5 ["one", "two", "three", "four"])

tailCutsALongLine :: IO ()
tailCutsALongLine = do
    let kept = Binary.tailOf 8 ["short", C8.replicate 100 'x' <> "END"]
    assertBool "within the bound" (ByteString.length kept <= 8)
    assertBool "the end of the line, not its start" ("END\n" `ByteString.isSuffixOf` kept)

capturedUnchanged :: IO ()
capturedUnchanged = within 10 $ do
    (onLine, seen) <- collector
    r <- Binary.runRouted Binary.captured (\c l -> onLine (c, l)) (proc "/bin/sh" ["-c", "echo out; echo err >&2; exit 3"]) ""
    assertEqual "" (ExitFailure 3, "out\n", "err\n") r
    assertEqual "nothing streamed" [] =<< seen

streamsWhileRunning :: IO ()
streamsWhileRunning = within 20 . withTempDir $ \dir -> do
    let gate = dir </> "gate"
        script = "echo first; while [ ! -e '" <> gate <> "' ]; do sleep 0.05; done; echo second"
    arrived <- newEmptyMVar
    ended <- newEmptyMVar
    _ <- forkIO (putMVar ended =<< (try (Binary.runRouted Binary.streamed (\_ l -> putMVar arrived l) (proc "/bin/sh" ["-c", script]) "") :: IO (Either IOError (ExitCode, ByteString.ByteString, ByteString.ByteString))))
    first <- takeMVar arrived
    assertEqual "" "first" first
    -- the command cannot have ended: what lets it end does not exist yet
    opened <- doesFileExist gate
    assertBool "the gate was still shut when the first line arrived" (not opened)
    writeFile gate ""
    second <- takeMVar arrived
    assertEqual "" "second" second
    r <- takeMVar ended
    assertEqual "and the report still has both" (Right (ExitSuccess, "first\nsecond\n", "")) r

boundedTail :: IO ()
boundedTail = within 60 $ do
    count <- newIORef (0 :: Int)
    -- 200000 lines of 7 bytes: 1.4MB printed, 64 bytes kept
    (code, out, err) <-
        Binary.runRouted
            (Binary.streamedKeeping 64)
            (\_ _ -> atomicModifyIORef' count (\n -> (n + 1, ())))
            (proc "/bin/sh" ["-c", "i=100000; while [ $i -lt 300000 ]; do echo $i; i=$((i+1)); done"])
            ""
    assertEqual "" ExitSuccess code
    assertEqual "every line was reported" 200000 =<< readIORef count
    assertBool ("kept " <> show (ByteString.length out) <> " bytes") (ByteString.length out <= 64)
    assertBool "and it is the end of the stream" ("299999\n" `ByteString.isSuffixOf` out)
    assertEqual "" "" err

discardedKeepsExitCode :: IO ()
discardedKeepsExitCode = within 10 $ do
    (onLine, seen) <- collector
    r <- Binary.runRouted Binary.discarded (\c l -> onLine (c, l)) (proc "/bin/sh" ["-c", "echo out; echo err >&2; exit 4"]) ""
    assertEqual "" (ExitFailure 4, "", "") r
    assertEqual "nothing streamed" [] =<< seen

appendsToFile :: IO ()
appendsToFile = within 10 . withTempDir $ \dir -> do
    let logfile = dir </> "command.log"
        run script = Binary.runRouted (Binary.appendedTo logfile) (\_ _ -> pure ()) (proc "/bin/sh" ["-c", script]) ""
    r1 <- run "echo one; echo two >&2"
    assertEqual "nothing kept for the report" (ExitSuccess, "", "") r1
    _ <- run "echo three"
    written <- C8.readFile logfile
    assertEqual "both streams, then the second run after the first" "one\ntwo\nthree\n" written

mixedSinks :: IO ()
mixedSinks = within 10 $ do
    (onLine, seen) <- collector
    r <-
        Binary.runRouted
            (Routing{stdoutTo = Capture, stderrTo = Stream 1024})
            (\c l -> onLine (c, l))
            (proc "/bin/sh" ["-c", "echo result; echo progress >&2"])
            ""
    assertEqual "" (ExitSuccess, "result\n", "progress\n") r
    assertEqual "only the streamed one was reported" [(NodeLog.Stderr, "progress")] =<< seen

unreadStdin :: IO ()
unreadStdin = within 20 $ do
    -- far more than a pipe holds, to a command that reads none of it
    (code, out, _) <- Binary.runRouted Binary.streamed (\_ _ -> pure ()) (proc "/bin/sh" ["-c", "echo done"]) (C8.replicate (4 * 1024 * 1024) 'x')
    assertEqual "" (ExitSuccess, "done\n") (code, out)
    -- and one that does read it gets it
    (_, counted, _) <- Binary.runRouted Binary.streamed (\_ _ -> pure ()) (proc "/bin/sh" ["-c", "wc -c | tr -d ' '"]) (C8.replicate 100000 'x')
    assertEqual "" "100000\n" counted

failureCarriesTail :: IO ()
failureCarriesTail = within 10 $ do
    (onReport, seen) <- collector
    r <- try (Binary.untrackedExecWith Binary.streamed sh "echo building; echo broken >&2; exit 2" "" (ReporterM onReport))
    case r of
        Right () -> assertFailure "a non-zero exit did not throw"
        Left (Binary.CommandFailed _ code out err) ->
            assertEqual "" (2, "building\n", "broken\n") (code, out, err)
    reports <- seen
    assertEqual
        "start, the two lines, stop"
        ["start", "line", "line", "stopped"]
        (map shape reports)
  where
    shape rep = case rep of
        Binary.CommandStart{} -> "start" :: String
        Binary.CommandOutput{} -> "line"
        Binary.CommandStopped{} -> "stopped"
        Binary.Requested{} -> "requested"

-------------------------------------------------------------------------------

-- | A node running one shell script, with its output routed as asked.
scriptNode :: Routing -> Ref -> String -> (Binary.Report -> IO ()) -> Op
scriptNode routing r script onReport =
    Binary.withBinaryWith routing shTrack sh script $ \run ->
        op "script" nodeps $ \actions ->
            actions
                { help = "runs a script"
                , ref = r
                , up = run (ReporterM onReport)
                }

runUp :: Op -> IO Bool
runUp = UpDown.upTree silent (pure . runIdentity)

-- | The lines a sink saw about one node.
about :: Ref -> [NodeLog.Line] -> [(NodeLog.Channel, String)]
about r ls = [(l.lineChannel, show l.lineText) | l <- ls, l.lineRef == r]

nodeLinesGoToTheLog :: IO ()
nodeLinesGoToTheLog = within 20 $ do
    let r = mkRef "binary-output-spec" ("streamed" :: String)
    (onLine, lines') <- collector
    (onReport, reports) <- collector
    ok <- NodeLog.withSink (ReporterM onLine) (runUp (scriptNode Binary.streamed r "echo compiling; printf 'carriage\\r\\n'; echo careful >&2" onReport))
    assertBool "the node came up" ok
    seen <- about r <$> lines'
    assertEqual
        "stdout in order, under the node's ref, without line terminators"
        [show ("compiling" :: String), show ("carriage" :: String)]
        [t | (NodeLog.Stdout, t) <- seen]
    assertEqual "stderr on its own channel" [show ("careful" :: String)] [t | (NodeLog.Stderr, t) <- seen]
    reps <- reports
    assertEqual
        "the recipe's reporter got the start and the stop, and no line twice"
        ["start", "stopped"]
        (map shape reps)
    assertBool "and the stop carries what was kept" (any Binary.isCommandSuccessful reps)
  where
    shape rep = case rep of
        Binary.Requested _ Binary.CommandStart{} -> "start" :: String
        Binary.Requested _ Binary.CommandStopped{} -> "stopped"
        Binary.Requested _ Binary.CommandOutput{} -> "line"
        _ -> "other"

capturedNodeIsSilent :: IO ()
capturedNodeIsSilent = within 20 $ do
    let r = mkRef "binary-output-spec" ("captured" :: String)
    (onLine, lines') <- collector
    (onReport, reports) <- collector
    ok <- NodeLog.withSink (ReporterM onLine) (runUp (scriptNode Binary.captured r "echo a-secret-token" onReport))
    assertBool "the node came up" ok
    assertEqual "nothing was said" [] . about r =<< lines'
    reps <- reports
    assertEqual
        "and the report is the one it always was"
        ["a-secret-token\n"]
        [out | Binary.Requested _ (Binary.CommandStopped _ ExitSuccess out _) <- reps]

sayAndLeave :: IO ()
sayAndLeave = do
    let r = mkRef "binary-output-spec" ("say" :: String)
    (onLine, lines') <- collector
    NodeLog.withSink (ReporterM onLine) $ do
        NodeLog.say r "waiting for ssh"
        NodeLog.say r "step 1 of 2\nstep 2 of 2"
    NodeLog.say r "nobody is listening"
    seen <- about r <$> lines'
    assertEqual
        ""
        [(NodeLog.Message, show t) | t <- ["waiting for ssh", "step 1 of 2", "step 2 of 2" :: String]]
        seen
