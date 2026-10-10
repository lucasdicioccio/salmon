{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What a remote call says while it runs: the routed form of
"Salmon.Builtin.Nodes.Ssh"'s call and the self-call's 'Self.RemoteOutput'
("Salmon.Builtin.Nodes.Self").

Three kinds of case, and what none of them is. /Declared shape/: the calls
that existed before there was a choice declare the node they always did
(same 'Ref', notes, @ssh@ argv), and only 'Self.RemoteReports' changes the
remote command line. /The re-telling/, pure: 'Self.remoteReportLines' over
lines encoded here by the server's own "Salmon.Reporter.Tagged", so a change
of wire shape fails here. /A real process/: a @\/bin\/sh@ standing where
@ssh@ would be, writing those same lines and held open on a file, to show a
remote report is said about the calling node while the command still runs.

No case runs @ssh@: nothing here crosses an ssh hop, real or loopback.

The sink registry is process-wide and this suite runs its groups in parallel
in one process, so every assertion about what a sink saw filters by the
node's own 'Ref'.
-}
module Test.RemoteOutputSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (ErrorCall (..), toException)
import Control.Monad (void, when)
import Data.Aeson (encode)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LByteString
import Data.Functor.Identity (runIdentity)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc)
import System.Process.ListLike (CmdSpec (..), cmdspec)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Actions.Upkeep as Upkeep
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Extension (..), Op, Track', ignoreTrack, nodeps, op, opAct)
import qualified Salmon.Builtin.NodeLog as NodeLog
import Salmon.Builtin.Nodes.Binary (Binary, Command (..))
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref, mkRef, shortRef)
import Salmon.Op.Track (trackedGraph)
import Salmon.Reporter (ReporterM (..), silent)
import qualified Salmon.Reporter.Tagged as Tagged
import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "remote calls: output while running"
        [ testGroup
            "declared shape"
            [ testCase "Ssh.call is the node it was, and a routed call is the same node" sshCallUnchanged
            , testCase "the ssh argv does not depend on the routing" sshArgv
            , testCase "callSelf is the node it was" selfCallUnchanged
            , testCase "callSelfAsSudoWith is the node it was" selfSudoCallUnchanged
            , testCase "following the remote's lines keeps the node; asking for its reports adds --json" selfCallOutputs
            , testCase "only up and down are asked for --json" selfCallJsonOnlyWhereTaken
            ]
        , testGroup
            "re-telling a remote salmon's reports"
            [ testCase "a report about a node names it by short ref and shorthand" tellsANodeReport
            , testCase "a failure carries its error, one line per line" tellsAFailure
            , testCase "a line a remote node said keeps its channel" tellsANodeLine
            , testCase "a wrapper is looked through" tellsThroughWrappers
            , testCase "a held action's output line is a line" tellsHeldOutput
            , testCase "what is not a report is said as it is" passesTheRest
            ]
        , testCase "a remote report is said about the calling node while the command runs" saidWhileRunning
        ]

-------------------------------------------------------------------------------

within :: Int -> IO a -> IO a
within secs act = do
    r <- timeout (secs * 1000000) act
    maybe (assertFailure ("timed out after " <> show secs <> "s")) pure r

actOf :: Op -> IO (Act Extension)
actOf o = maybe (assertFailure "the op has no node") pure (opAct o)

remote :: Ssh.Remote
remote = Ssh.Remote "deployer" "198.51.100.7"

self :: Self.RemoteSelf
self = Self.RemoteSelf (Self.Remote "deployer" "198.51.100.7") "/srv/salmon/self"

-- | The 'Ref' an ssh call has always had: the remote program, the remote,
-- its arguments and its standard input.
sshRunRef :: FilePath -> [Text] -> ByteString.ByteString -> Ref
sshRunRef path args input = mkRef "ssh-run" (show (path, remote, args, input))

selfCall :: Self.CallOpts -> CLI.BaseCommand -> Op
selfCall o base = trackedGraph (Self.callSelfOpts o silent ignoreTrack self ignoreTrack base ())

-- | @()@ as a directive on the wire.
unitDirective :: ByteString.ByteString
unitDirective = LByteString.toStrict (encode ())

sshCallUnchanged :: IO ()
sshCallUnchanged = do
    plain <- actOf (Ssh.call silent ignoreTrack ignoreTrack remote "uname" ["-a"] "")
    routed <- actOf (Ssh.callRouted Binary.streamed Ssh.noClientOpts silent ignoreTrack ignoreTrack remote "uname" ["-a"] "")
    assertEqual "the ref it always had" (sshRunRef "uname" ["-a"] "") plain.extension.ref
    assertEqual "notes" ["uname", "-a"] plain.extension.notes
    assertEqual "same ref when streamed" plain.extension.ref routed.extension.ref
    assertEqual "same help" plain.extension.help routed.extension.help
    assertEqual "same notes" plain.extension.notes routed.extension.notes
    assertEqual "same shorthand" plain.shorthand routed.shorthand

sshArgv :: IO ()
sshArgv =
    case cmdspec (prepare Ssh.sshRun (Ssh.Call "/srv/salmon/self" remote ["run", "up", "--json"] Ssh.noClientOpts)) of
        RawCommand bin args -> do
            assertEqual "" "ssh" bin
            assertEqual "" ["deployer@198.51.100.7", "/srv/salmon/self", "run", "up", "--json"] args
        ShellCommand s -> assertFailure ("a shell command: " <> s)

selfCallUnchanged :: IO ()
selfCallUnchanged = do
    old <- actOf (trackedGraph (Self.callSelf silent ignoreTrack self ignoreTrack CLI.Up ()))
    assertEqual "" (sshRunRef "/srv/salmon/self" ["run", "up"] unitDirective) old.extension.ref
    assertEqual "" ["/srv/salmon/self", "run", "up"] old.extension.notes
    viaOpts <- actOf (selfCall Self.defaultCallOpts CLI.Up)
    assertEqual "the default options are callSelf" old.extension.ref viaOpts.extension.ref

selfSudoCallUnchanged :: IO ()
selfSudoCallUnchanged = do
    let client = Ssh.ClientOpts (Just "/keys/client") (Just "/keys/known_hosts")
    old <- actOf (trackedGraph (Self.callSelfAsSudoWith client silent ignoreTrack self ignoreTrack CLI.Up ()))
    assertEqual "" (sshRunRef "sudo" ["/srv/salmon/self", "run", "up"] unitDirective) old.extension.ref
    assertEqual "" ["sudo", "/srv/salmon/self", "run", "up"] old.extension.notes
    plain <- actOf (trackedGraph (Self.callSelfAsSudo silent ignoreTrack self ignoreTrack CLI.Down ()))
    assertEqual "" (sshRunRef "sudo" ["/srv/salmon/self", "run", "down"] unitDirective) plain.extension.ref

selfCallOutputs :: IO ()
selfCallOutputs = do
    captured <- actOf (selfCall Self.defaultCallOpts CLI.Up)
    followed <- actOf (selfCall Self.defaultCallOpts{Self.callOutput = Self.RemoteLines} CLI.Up)
    reported <- actOf (selfCall Self.defaultCallOpts{Self.callOutput = Self.RemoteReports} CLI.Up)
    sudo <- actOf (selfCall Self.defaultCallOpts{Self.callOutput = Self.RemoteReports, Self.callAsSudo = True} CLI.Down)
    assertEqual "lines: the same node" captured.extension.ref followed.extension.ref
    assertEqual "lines: the same notes" captured.extension.notes followed.extension.notes
    assertEqual "reports: --json after the subcommand" ["/srv/salmon/self", "run", "up", "--json"] reported.extension.notes
    assertBool "reports: another node" (reported.extension.ref /= captured.extension.ref)
    assertEqual "reports under sudo" ["sudo", "/srv/salmon/self", "run", "down", "--json"] sudo.extension.notes

selfCallJsonOnlyWhereTaken :: IO ()
selfCallJsonOnlyWhereTaken = do
    let reports = Self.defaultCallOpts{Self.callOutput = Self.RemoteReports}
    tree <- actOf (selfCall reports CLI.Tree)
    dag <- actOf (selfCall reports CLI.DAG)
    assertEqual "" ["/srv/salmon/self", "run", "tree"] tree.extension.notes
    assertEqual "" ["/srv/salmon/self", "run", "dag"] dag.extension.notes

-------------------------------------------------------------------------------

-- | A node as the /remote/ salmon would report it.
remoteRef :: Ref
remoteRef = mkRef "remote-output-spec" ("remote-node" :: Text)

remoteAct :: Act Extension
remoteAct = case remoteOp.node of
    Actions act -> act
    Actionless -> error "fixture op is Actionless"
  where
    remoteOp :: Op
    remoteOp = op "file:/etc/motd" nodeps $ \ext -> ext{help = "a file", ref = remoteRef}

-- | One line of @run up --json@, as the remote salmon writes it.
wire :: Tagged.Tagged -> Text
wire = Text.decodeUtf8 . LByteString.toStrict . encode

short :: Text
short = "[" <> shortRef remoteRef <> "]"

tell :: Tagged.Tagged -> [(NodeLog.Channel, Text)]
tell = Self.remoteReportLines NodeLog.Stdout . wire

tellsANodeReport :: IO ()
tellsANodeReport = do
    assertEqual "" [(NodeLog.Message, "remote: done " <> short <> " file:/etc/motd")] (tell (Tagged.FromUpDown (UpDown.Done remoteAct)))
    assertEqual "" [(NodeLog.Message, "remote: skip " <> short <> " file:/etc/motd")] (tell (Tagged.FromUpDown (UpDown.Skip remoteAct)))
    assertEqual "" [(NodeLog.Message, "remote: blocked " <> short <> " file:/etc/motd")] (tell (Tagged.FromUpDown (UpDown.Blocked remoteAct)))

tellsAFailure :: IO ()
tellsAFailure =
    assertEqual
        ""
        [ (NodeLog.Message, "remote: failed " <> short <> " file:/etc/motd: command failed (exit 1)")
        , (NodeLog.Message, "remote:   stderr:")
        , (NodeLog.Message, "remote:   no such file")
        ]
        (tell (Tagged.FromUpDown (UpDown.Failed remoteAct (toException (ErrorCall "command failed (exit 1)\nstderr:\nno such file")))))

tellsANodeLine :: IO ()
tellsANodeLine = do
    assertEqual "" [(NodeLog.Stdout, "remote: " <> short <> " STEP 2/5: RUN make")] (tell (Tagged.FromNode (NodeLog.Line remoteRef NodeLog.Stdout "STEP 2/5: RUN make")))
    assertEqual "" [(NodeLog.Stderr, "remote: " <> short <> " warning: deprecated")] (tell (Tagged.FromNode (NodeLog.Line remoteRef NodeLog.Stderr "warning: deprecated")))
    assertEqual "" [(NodeLog.Message, "remote: " <> short <> " waiting for the lock")] (tell (Tagged.FromNode (NodeLog.Line remoteRef NodeLog.Message "waiting for the lock")))

tellsThroughWrappers :: IO ()
tellsThroughWrappers =
    assertEqual
        ""
        [(NodeLog.Message, "remote: done " <> short <> " file:/etc/motd")]
        (tell (Tagged.FromUpkeep (Upkeep.Acted (UpDown.Done remoteAct))))

tellsHeldOutput :: IO ()
tellsHeldOutput =
    assertEqual
        ""
        [(NodeLog.Stdout, "remote: " <> short <> " file:/etc/motd listening on 8080")]
        (tell (Tagged.FromUpkeep (Upkeep.Output remoteAct "listening on 8080")))

passesTheRest :: IO ()
passesTheRest = do
    assertEqual "prose on stdout" [(NodeLog.Stdout, "Reading package lists...")] (Self.remoteReportLines NodeLog.Stdout "Reading package lists...")
    assertEqual "an object that is no report" [(NodeLog.Stdout, "{\"a\":1}")] (Self.remoteReportLines NodeLog.Stdout "{\"a\":1}")
    assertEqual "a JSON value that is no object" [(NodeLog.Stdout, "[1,2]")] (Self.remoteReportLines NodeLog.Stdout "[1,2]")
    let report = wire (Tagged.FromUpDown (UpDown.Done remoteAct))
    assertEqual "a report on stderr is not read as one" [(NodeLog.Stderr, report)] (Self.remoteReportLines NodeLog.Stderr report)

-------------------------------------------------------------------------------

sh :: Command "sh" String
sh = Command $ \script -> proc "/bin/sh" ["-c", script]

shTrack :: Track' (Binary "sh")
shTrack = ignoreTrack

{- | The plumbing 'Ssh.callTapped' is made of, with @\/bin\/sh@ where @ssh@
would be: the command writes one remote report, waits on a file that does not
exist yet, then writes a second. The first must be heard, re-told and under
the calling node's ref, before the file is created.
-}
saidWhileRunning :: IO ()
saidWhileRunning = within 30 . withTempDir $ \dir -> do
    let caller = mkRef "remote-output-spec" ("caller" :: Text)
        gate = dir </> "gate"
        firstFile = dir </> "first"
        secondFile = dir </> "second"
        script =
            "cat '" <> firstFile <> "'; while [ ! -e '" <> gate <> "' ]; do sleep 0.05; done; cat '" <> secondFile <> "'; echo plain >&2"
    ByteString.writeFile firstFile (Text.encodeUtf8 (wire (Tagged.FromUpDown (UpDown.Eval remoteAct))) <> "\n")
    ByteString.writeFile secondFile (Text.encodeUtf8 (wire (Tagged.FromUpDown (UpDown.Done remoteAct))) <> "\n")
    heard <- newIORef []
    firstHeard <- newEmptyMVar
    stopped <- newIORef []
    let sink = ReporterM $ \line ->
            when (line.lineRef == caller) $ do
                atomicModifyIORef' heard (\ls -> (line : ls, ()))
                void (tryPutMVar firstHeard line)
        onReport rep = case rep of
            Binary.Requested _ (Binary.CommandStopped _ code out _) -> atomicModifyIORef' stopped (\xs -> ((code, out) : xs, ()))
            _ -> pure ()
        node :: Op
        node =
            Binary.withBinaryStdinTapped Binary.streamed Self.remoteReportLines shTrack sh script "" $ \run ->
                op "remote-call" nodeps $ \actions ->
                    actions{help = "stands in for a self-call", ref = caller, up = run (ReporterM onReport)}
    done <- newEmptyMVar
    NodeLog.withSink sink $ do
        _ <- forkIO (putMVar done =<< UpDown.upTree silent (pure . runIdentity) node)
        first <- takeMVar firstHeard
        assertEqual "the first remote report, re-told" (NodeLog.Line caller NodeLog.Message ("remote: eval " <> short <> " file:/etc/motd")) first
        -- the command cannot have ended: what lets it end does not exist yet
        opened <- doesFileExist gate
        assertBool "the gate was still shut when the first report was heard" (not opened)
        writeFile gate ""
        ok <- takeMVar done
        assertBool "the node came up" ok
    said <- reverse <$> readIORef heard
    assertEqual
        "every line, about the caller: reports re-told, stderr as it was"
        [ (NodeLog.Message, "remote: eval " <> short <> " file:/etc/motd")
        , (NodeLog.Message, "remote: done " <> short <> " file:/etc/motd")
        ]
        [(l.lineChannel, l.lineText) | l <- said, l.lineChannel /= NodeLog.Stderr]
    assertEqual "" [(NodeLog.Stderr, "plain")] [(l.lineChannel, l.lineText) | l <- said, l.lineChannel == NodeLog.Stderr]
    final <- readIORef stopped
    case final of
        [(ExitSuccess, out)] ->
            assertEqual "the final report keeps what the command wrote, not the re-telling" 2 (length (filter ("{" `C8.isPrefixOf`) (C8.lines out)))
        other -> assertFailure ("expected one successful stop, got " <> show (length other))
