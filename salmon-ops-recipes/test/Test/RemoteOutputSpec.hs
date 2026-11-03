{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What a remote call says while it runs: the routed form of
"Salmon.Builtin.Nodes.Ssh"'s call and the self-call's 'Self.RemoteOutput'
("Salmon.Builtin.Nodes.Self").

Three kinds of case, and what none of them is. /Declared shape/: the calls
that existed before there was a choice declare the node they always did
(same 'Ref', notes, @ssh@ argv; but a standard input is a keyed tag in the
'Ref' and @help@, never the bytes, see /standard input is not public/), and
only 'Self.RemoteReports' changes the remote command line. /The re-telling/, pure: 'Self.remoteReportLines' over
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
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (encode)
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
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
import System.Posix.Files (fileMode, getFileStatus)
import System.Process (proc)
import Text.Printf (printf)
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
            , testCase "callSelf is the node it was, its directive tagged" selfCallUnchanged
            , testCase "callSelfAsSudoWith is the node it was, its directive tagged" selfSudoCallUnchanged
            , testCase "following the remote's lines keeps the node; asking for its reports adds --json" selfCallOutputs
            , testCase "only up and down are asked for --json" selfCallJsonOnlyWhereTaken
            ]
        , testGroup
            "standard input is not public"
            [ testCase "an input is in neither help, notes nor ref, nor is its plain hash" stdinNotPublic
            , testCase "two inputs are two nodes, the same input one" stdinTellsCallsApart
            , testCase "a self-call's directive is not in its help" selfCallDirectiveNotPublic
            , testCase "the tag is HMAC-SHA256 under the key, and says when the key is ephemeral" stdinTagVector
            , testCase "the key file is made once, owner-only, and read back the same" callKeyFileMadeOnce
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
-- its arguments and its standard input. Still the 'Ref' of a call with
-- nothing on standard input; see 'sshRunRefTagged' for the others.
sshRunRef :: FilePath -> [Text] -> ByteString.ByteString -> Ref
sshRunRef path args input = mkRef "ssh-run" (show (path, remote, args, input))

-- | The 'Ref' of a call with something on standard input: the input's
-- keyed tag where the input was.
sshRunRefTagged :: FilePath -> [Text] -> ByteString.ByteString -> Ref
sshRunRefTagged path args input = mkRef "ssh-run" (show (path, remote, args, Ssh.stdinTag Ssh.callKey input))

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
    assertEqual "the help it always had" "calls uname on 198.51.100.7 with args -a and stdin " plain.extension.help
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
    assertEqual "" (sshRunRefTagged "/srv/salmon/self" ["run", "up"] unitDirective) old.extension.ref
    assertEqual "" ["/srv/salmon/self", "run", "up"] old.extension.notes
    viaOpts <- actOf (selfCall Self.defaultCallOpts CLI.Up)
    assertEqual "the default options are callSelf" old.extension.ref viaOpts.extension.ref

selfSudoCallUnchanged :: IO ()
selfSudoCallUnchanged = do
    let client = Ssh.ClientOpts (Just "/keys/client") (Just "/keys/known_hosts")
    old <- actOf (trackedGraph (Self.callSelfAsSudoWith client silent ignoreTrack self ignoreTrack CLI.Up ()))
    assertEqual "" (sshRunRefTagged "sudo" ["/srv/salmon/self", "run", "up"] unitDirective) old.extension.ref
    assertEqual "" ["sudo", "/srv/salmon/self", "run", "up"] old.extension.notes
    plain <- actOf (trackedGraph (Self.callSelfAsSudo silent ignoreTrack self ignoreTrack CLI.Down ()))
    assertEqual "" (sshRunRefTagged "sudo" ["/srv/salmon/self", "run", "down"] unitDirective) plain.extension.ref

-------------------------------------------------------------------------------

-- | A standard input carrying something a reader of a report must not see.
secretInput :: ByteString.ByteString
secretInput = "{\"password\":\"hunter2\"}"

callWithInput :: ByteString.ByteString -> Op
callWithInput = Ssh.call silent ignoreTrack ignoreTrack remote "/srv/salmon/self" ["run", "up"]

stdinNotPublic :: IO ()
stdinNotPublic = do
    act <- actOf (callWithInput secretInput)
    let ext = act.extension
        public = Text.unwords (ext.help : act.shorthand : ext.notes)
        plainHex = Text.pack (concatMap (printf "%02x") (ByteString.unpack (SHA256.hash secretInput)))
        plainB64 = Text.decodeUtf8 (Base64.encode (SHA256.hash secretInput))
    assertBool (Text.unpack ext.help) (not ("hunter2" `Text.isInfixOf` public))
    assertBool "no plain sha256 (hex)" (not (Text.take 16 plainHex `Text.isInfixOf` public))
    assertBool "no plain sha256 (base64)" (not (Text.take 16 plainB64 `Text.isInfixOf` public))
    assertBool "not the ref that hashed the input" (ext.ref /= sshRunRef "/srv/salmon/self" ["run", "up"] secretInput)
    assertEqual "the tag in its place" (sshRunRefTagged "/srv/salmon/self" ["run", "up"] secretInput) ext.ref
    assertEqual
        "help"
        ("calls /srv/salmon/self on 198.51.100.7 with args run up and stdin " <> Ssh.stdinTag Ssh.callKey secretInput)
        ext.help

stdinTellsCallsApart :: IO ()
stdinTellsCallsApart = do
    a <- actOf (callWithInput secretInput)
    a' <- actOf (callWithInput secretInput)
    b <- actOf (callWithInput "{\"password\":\"hunter3\"}")
    assertEqual "same input, same ref" a.extension.ref a'.extension.ref
    assertEqual "same input, same help" a.extension.help a'.extension.help
    assertBool "another input, another ref" (a.extension.ref /= b.extension.ref)
    assertBool "another input, another help" (a.extension.help /= b.extension.help)

selfCallDirectiveNotPublic :: IO ()
selfCallDirectiveNotPublic = do
    act <- actOf (trackedGraph (Self.callSelf silent ignoreTrack self ignoreTrack CLI.Up ("hunter2" :: Text)))
    assertBool (Text.unpack act.extension.help) (not ("hunter2" `Text.isInfixOf` act.extension.help))

stdinTagVector :: IO ()
stdinTagVector = do
    let key = ByteString.replicate 32 107 -- "kkk...": python's hmac computed the vector
    assertEqual "" "hmac-sha256:rnRTLK-HK6f1BjkvSUtycR" (Ssh.stdinTag (Ssh.CallKey Ssh.Persistent key) secretInput)
    assertEqual "" "hmac-sha256-ephemeral:rnRTLK-HK6f1BjkvSUtycR" (Ssh.stdinTag (Ssh.CallKey Ssh.Ephemeral key) secretInput)

callKeyFileMadeOnce :: IO ()
callKeyFileMadeOnce =
    withTempDir $ \dir -> do
        let path = dir </> "state" </> "salmon" </> "call.key"
        k1 <- Ssh.loadCallKeyAt path
        k2 <- Ssh.loadCallKeyAt path
        assertBool "a key" (not (ByteString.null k1))
        assertEqual "the same key read back" k1 k2
        st <- getFileStatus path
        assertEqual "owner-only" 0o600 (fileMode st .&. 0o777)

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
