{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Layer 1 coverage for what "Salmon.Builtin.Nodes.Binary" promises about a
child's standard input: real subprocesses, asked what their descriptor 0 is.

The rule under test is that no process started for a node inherits the
standard input of the pass that runs it — under @run serve@ that is the
command channel, and a child that prompts would block the pass and read the
loop's commands. The children here answer through @\/proc\/self\/fd\/0@
rather than by reading, so the cases hold whatever this test process's own
standard input happens to be.
-}
module Test.BinarySpec (tests) where

import Control.Exception (try)
import qualified Data.ByteString.Char8 as C8
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, openFile, withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Salmon.Builtin.Nodes.Binary (CommandFailed (..), CommandIO (..), detachedStdin, execDetached, untrackedExecIO)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Binary"
        [ testCase "a CommandIO naming no standard input reads /dev/null, not ours" ioCommandIsDetached
        , testCase "a CommandIO naming its own standard input keeps it" ioCommandKeepsItsInput
        , testCase "detachedStdin leaves the caller's CreateProcess otherwise as given" detachedOnlyTouchesInherit
        , testCase "execDetached hands back stdout and gives the child an empty input" execDetachedReads
        , testCase "execDetached throws on a non-zero exit" execDetachedThrows
        ]

-- | What the child's descriptor 0 is, as the kernel names it.
whatIsStdin :: CreateProcess
whatIsStdin = proc "readlink" ["/proc/self/fd/0"]

-- | Run a 'CommandIO' to completion with its stdout sent to a file; the file's contents.
capturing :: (Handle -> IO CreateProcess) -> IO String
capturing mk =
    withSystemTempDirectory "salmon-binary-spec" $ \dir -> do
        let outPath = dir </> "out"
        out <- openFile outPath WriteMode
        (_, _, _, ph) <- untrackedExecIO (CommandIO @"readlink" (\() h -> mk h)) () out
        code <- waitForProcess ph
        hClose out
        assertEqual "the child exited cleanly" ExitSuccess code
        s <- readFile outPath
        length s `seq` pure s

ioCommandIsDetached :: IO ()
ioCommandIsDetached = do
    s <- capturing (\out -> pure whatIsStdin{std_out = UseHandle out})
    assertEqual "descriptor 0 of the child" "/dev/null\n" s

ioCommandKeepsItsInput :: IO ()
ioCommandKeepsItsInput =
    withSystemTempDirectory "salmon-binary-spec" $ \dir -> do
        let inPath = dir </> "in"
        writeFile inPath "fed by the node\n"
        s <- capturing $ \out -> do
            input <- openFile inPath ReadMode
            pure (proc "cat" []){std_in = UseHandle input, std_out = UseHandle out}
        assertEqual "the child read what the node gave it" "fed by the node\n" s

detachedOnlyTouchesInherit :: IO ()
detachedOnlyTouchesInherit = do
    piped <- detachedStdin (proc "true" []){std_in = CreatePipe} (pure . std_in)
    assertEqual "an asked-for pipe is still a pipe" "CreatePipe" (show piped)
    none <- detachedStdin (proc "true" []){std_in = NoStream} (pure . std_in)
    assertEqual "an explicit NoStream is left alone" "NoStream" (show none)
    withFile "/dev/null" WriteMode $ \sink -> do
        (_, _, _, ph) <- detachedStdin (proc "true" []){std_out = UseHandle sink} createProcess
        code <- waitForProcess ph
        assertEqual "the process still runs" ExitSuccess code

execDetachedReads :: IO ()
execDetachedReads = do
    out <- execDetached (proc "sh" ["-c", "if read -r line; then echo \"read: $line\"; else echo eof; fi"])
    assertEqual "a prompt reads end-of-file at once" "eof\n" (C8.unpack out)

execDetachedThrows :: IO ()
execDetachedThrows = do
    r <- try @CommandFailed (execDetached (proc "sh" ["-c", "echo no >&2; exit 3"]))
    case r of
        Left e -> do
            assertEqual "exit code" 3 e.commandFailed_exitCode
            assertEqual "stderr is kept" "no\n" (C8.unpack e.commandFailed_stderr)
        Right out -> assertFailure ("expected CommandFailed, got " <> show out)
