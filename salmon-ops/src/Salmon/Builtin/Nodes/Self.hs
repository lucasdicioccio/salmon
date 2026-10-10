{-# LANGUAGE DeriveGeneric #-}

-- | todo: pass rsync in
module Salmon.Builtin.Nodes.Self where

import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LByteString
import Data.Dynamic (toDyn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import System.FilePath (takeFileName, (</>))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.NodeLog as NodeLog
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Track (Track (..), Tracked (..), bindTracked, trackedGraph, using, (>*<))
import Salmon.Reporter

import System.Posix.Files (readSymbolicLink)

-------------------------------------------------------------------------------
data Report
    = RunRsync !Rsync.Report
    | RunSsh !Ssh.Report
    deriving (Show)

-------------------------------------------------------------------------------

newtype SelfPath = SelfPath {getSelfPath :: FilePath}
    deriving (Show, Ord, Eq, Generic)

instance ToJSON SelfPath
instance FromJSON SelfPath

readSelfPath_linux :: IO SelfPath
readSelfPath_linux = SelfPath <$> readSymbolicLink "/proc/self/exe"

data Remote = Remote {remoteUser :: Text, remoteHost :: Text}
    deriving (Show, Ord, Eq)

data RemoteSelf = RemoteSelf {selfRemote :: Remote, selfRemotePath :: FilePath}

{- | Copies this binary to a remote host.

The node the result tracks /is/ the copy (@rsync:sendfile@), not a wrapper
standing on it: what a caller injects onto it then precedes the copy itself.
See 'remoteCallNode' for why that matters.
-}
uploadSelf :: Reporter Report -> FilePath -> Remote -> SelfPath -> Tracked' RemoteSelf
uploadSelf = uploadSelfWith Ssh.noClientOpts

-- | 'uploadSelf', with explicit ssh client options.
uploadSelfWith :: Ssh.ClientOpts -> Reporter Report -> FilePath -> Remote -> SelfPath -> Tracked' RemoteSelf
uploadSelfWith opts r remotedir remote path =
    Tracked (Track $ const copy) (RemoteSelf remote selfpathOnRemote)
  where
    selfpathOnRemote = remotedir </> takeFileName (getSelfPath path)
    rsyncRemote = Rsync.Remote remote.remoteUser remote.remoteHost
    copy =
        Rsync.sendFileWith
            opts
            r'
            Debian.rsync
            (FS.PreExisting $ getSelfPath path)
            rsyncRemote
            selfpathOnRemote
    r' = contramap RunRsync r

data RemoteCall a
    = RemoteCall
    { remoteCall_command :: CLI.BaseCommand
    , remoteCall_directive :: a
    }
    deriving (Show, Eq, Generic)
instance (ToJSON a) => ToJSON (RemoteCall a)
instance (FromJSON a) => FromJSON (RemoteCall a)

{- | Marks the ssh call that runs the self binary with the graph the directive
would expand to on the other side ('CLI.RemoteOp'), and hands back /that
node/ -- the one whose @up@ makes the call.

It used to be wrapped: an effect-less @self-call@ node with the ssh call as
one of its predecessors. 'inject' adds a predecessor to the node it is given
and orders nothing among that node's predecessors, so everything a caller
injected onto the wrapper -- the upload ('bindTracked' in
'uploadAndCallSelf'), a recipe's secret uploads, an ssh probe -- became a
sibling of the call rather than something the call waits for. A one-shot pass
visits siblings in listed order, which hid it while everything succeeded; a
failed sibling did not block the call, and @run serve@ converges siblings
concurrently. See @Test.DeferredSpec@.
-}
remoteCallNode :: Track' directive -> directive -> Op -> Op
remoteCallNode simulate directive call =
    call{node = fmap mark call.node}
  where
    mark ext = ext{dynamics = toDyn (CLI.RemoteOp (run simulate directive)) : ext.dynamics}

callSelf ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    Reporter Report ->
    Track' Ssh.Remote ->
    RemoteSelf ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
callSelf = callSelfOpts defaultCallOpts

{- | What is done, on this side, with what a remote salmon prints while it
runs. The choice is the caller's, per call, for the reason
"Salmon.Builtin.Nodes.Binary".'Binary.Routing' gives: only the author of the
directive knows whether the other side can print a secret.
-}
data RemoteOutput
    = {- | Held until ssh exits and handed to the final report: what every
      self-call did before there was a choice, and still the default.
      -}
      RemoteCaptured
    | {- | Each line the remote salmon prints is a "Salmon.Builtin.NodeLog"
      line about the calling node, as it is read. The remote command line is
      the same as under 'RemoteCaptured', so the call is the same node.
      -}
      RemoteLines
    | {- | The remote salmon is run with @--json@ and each report it writes
      is re-told as a line about the calling node ('remoteReportLines'),
      as it is read. The extra flag is part of the remote command line, so
      the call is a /different/ node from the captured one (another 'Ref').
      Only @up@ and @down@ take @--json@; for any other
      'CLI.BaseCommand' this is 'RemoteLines'.
      -}
      RemoteReports
    deriving (Eq, Ord, Show)

-- | How a self-call is made: with which ssh options, under @sudo@ or not,
-- and what becomes of what the other side prints.
data CallOpts = CallOpts
    { callClient :: Ssh.ClientOpts
    , callAsSudo :: Bool
    , callOutput :: RemoteOutput
    }
    deriving (Eq, Show)

-- | Ambient ssh authentication, no @sudo@, output captured: 'callSelf'.
defaultCallOpts :: CallOpts
defaultCallOpts = CallOpts Ssh.noClientOpts False RemoteCaptured

{- | Runs the self binary on the remote with a directive on its standard
input. 'callSelf', 'callSelfAsSudo' and 'callSelfAsSudoWith' are this
function at fixed 'CallOpts' and declare the node they always did.

Nothing here reads the directive: it travels as bytes, and what the remote
salmon says about applying it is public in the sense every report is (see
'RemoteOutput' before following a remote that can print a secret).
-}
callSelfOpts ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    CallOpts ->
    Reporter Report ->
    Track' Ssh.Remote ->
    RemoteSelf ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
callSelfOpts o r mkRemote self simulate base directive =
    Tracked (Track $ const $ remoteCallNode simulate directive callOverSSH) rc
  where
    rc = RemoteCall base directive
    sshRemote = Ssh.Remote self.selfRemote.remoteUser self.selfRemote.remoteHost
    (cmdPath, cmdArgs) = remoteCommand o self base
    cmdStdin = toStrict $ encode directive
    (routing, tap) = remoteRouting o.callOutput base
    callOverSSH = Ssh.callTapped routing tap o.callClient r' Debian.ssh mkRemote sshRemote cmdPath cmdArgs cmdStdin
    r' = contramap RunSsh r

-- | The remote program and its arguments, as ssh is given them.
remoteCommand :: CallOpts -> RemoteSelf -> CLI.BaseCommand -> (FilePath, [Text])
remoteCommand o self base
    | o.callAsSudo = ("sudo", Text.pack self.selfRemotePath : runArgs)
    | otherwise = (self.selfRemotePath, runArgs)
  where
    runArgs = ["run", CLI.argForBaseCommand base] <> ["--json" | reportsAsJson o.callOutput base]

-- | Whether the remote salmon is asked for @--json@: only the two
-- subcommands that take the flag are.
reportsAsJson :: RemoteOutput -> CLI.BaseCommand -> Bool
reportsAsJson output base = output == RemoteReports && base `elem` [CLI.Up, CLI.Down]

remoteRouting :: RemoteOutput -> CLI.BaseCommand -> (Binary.Routing, Binary.LineTap)
remoteRouting output base = case output of
    RemoteCaptured -> (Binary.captured, Binary.plainLines)
    RemoteLines -> (Binary.streamed, Binary.plainLines)
    RemoteReports
        | reportsAsJson output base -> (Binary.streamed, remoteReportLines)
        | otherwise -> (Binary.streamed, Binary.plainLines)

{- | Re-tells one line of a remote @run up --json@ as lines about the node
that made the call.

How a remote report nests under the local node: /as text/. The calling node
is the only node the local side has for the other machine's work (the remote
graph is not in the local DAG), so a remote report becomes a
"Salmon.Builtin.NodeLog" line under the caller's 'Ref' that names the remote
node by its short ref and shorthand:

> remote: done [a1b2c3d4] file:/etc/motd
> remote: failed [a1b2c3d4] file:/etc/motd: <first line of the error>
> remote:   <each further line of the error>
> remote: [a1b2c3d4] STEP 2/5: RUN make

A report about a node is a 'NodeLog.Message'; a line a remote node said
(@kind: log@, or a held action's @output@) keeps the channel it was said on.
Wrappers (@acted@, @tended@) are looked through to the report they carry.
A standard-output line that is not a JSON object with a @kind@ (something a
recipe's own reporter printed) and every standard-error line are said as
they are.

No new event and no new field: these are ordinary @log@ lines on the
@output@ stream, which every client already tails. What that gives up is
structure: a client cannot rebuild the remote graph from them.
-}
remoteReportLines :: Binary.LineTap
remoteReportLines NodeLog.Stdout line
    | Just (Aeson.Object o) <- Aeson.decodeStrict (Text.encodeUtf8 line)
    , Just said <- renderRemoteReport o =
        said
remoteReportLines channel line = [(channel, line)]

renderRemoteReport :: Aeson.Object -> Maybe [(NodeLog.Channel, Text)]
renderRemoteReport outer = do
    kind <- textAt "kind" o
    pure $ case textAt "line" o of
        Just said
            | kind `elem` ["log", "output"] ->
                [(saidOn, Text.unwords (["remote:"] <> subject <> [said]))]
        _ ->
            let headline = Text.unwords (["remote:", kind] <> subject)
             in case maybe [] Text.lines (textAt "error" o) of
                    [] -> [(NodeLog.Message, headline)]
                    (e : es) ->
                        (NodeLog.Message, headline <> ": " <> e)
                            : [(NodeLog.Message, "remote:   " <> l) | l <- es]
  where
    o = innermost outer
    innermost x = case KeyMap.lookup "report" x of
        Just (Aeson.Object inner) | KeyMap.member "kind" inner -> innermost inner
        _ -> x
    textAt k x = case KeyMap.lookup k x of
        Just (Aeson.String t) -> Just t
        _ -> Nothing
    objectAt k x = case KeyMap.lookup k x of
        Just (Aeson.Object y) -> Just y
        _ -> Nothing
    subject =
        maybe [] (\short -> ["[" <> short <> "]"]) (objectAt "ref" o >>= textAt "short")
            <> maybe [] pure (objectAt "node" o >>= textAt "shorthand")
    saidOn = case textAt "channel" o of
        Just "stderr" -> NodeLog.Stderr
        Just "message" -> NodeLog.Message
        _ -> NodeLog.Stdout

callSelfAsSudo ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    Reporter Report ->
    Track' Ssh.Remote ->
    RemoteSelf ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
callSelfAsSudo = callSelfAsSudoWith Ssh.noClientOpts

-- | 'callSelfAsSudo', with explicit ssh client options.
callSelfAsSudoWith ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    Ssh.ClientOpts ->
    Reporter Report ->
    Track' Ssh.Remote ->
    RemoteSelf ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
callSelfAsSudoWith opts = callSelfOpts defaultCallOpts{callClient = opts, callAsSudo = True}

-- | Upload this binary to a remote host, then invoke it there over SSH with
-- a directive — the composition every recipe that self-orchestrates a
-- remote step was hand-rolling via uploadSelf `bindTracked` callSelf.
--
-- The node tracked is the call, standing on the upload; whatever a recipe
-- injects onto it (files the directive reads) precedes the call too.
uploadAndCallSelf ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    Reporter Report ->
    Reporter Report ->
    FilePath ->
    Remote ->
    SelfPath ->
    Track' Ssh.Remote ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
uploadAndCallSelf = uploadAndCallSelfOpts defaultCallOpts

{- | The upload, then the call, both under one 'CallOpts': the upload takes
its ssh options, the call all of it. This is the form to reach for to follow
a remote salmon while it works (@defaultCallOpts{callOutput = RemoteReports}@).
-}
uploadAndCallSelfOpts ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    CallOpts ->
    Reporter Report ->
    Reporter Report ->
    FilePath ->
    Remote ->
    SelfPath ->
    Track' Ssh.Remote ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
uploadAndCallSelfOpts o rUpload rCall remotedir uploadRemote selfpath mkRemote simulate base directive =
    uploadSelfWith o.callClient rUpload remotedir uploadRemote selfpath `bindTracked` \self ->
        callSelfOpts o rCall mkRemote self simulate base directive

-- | Same as 'uploadAndCallSelf', but invokes the remote binary via sudo.
uploadAndCallSelfAsSudo ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    Reporter Report ->
    Reporter Report ->
    FilePath ->
    Remote ->
    SelfPath ->
    Track' Ssh.Remote ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
uploadAndCallSelfAsSudo = uploadAndCallSelfAsSudoWith Ssh.noClientOpts

{- | 'uploadAndCallSelfAsSudo', running both the upload and the call under
explicit ssh client options -- the key an SSH-CA recipe just had signed (which
ssh would otherwise never offer), and a known-hosts file of the recipe's own.
-}
uploadAndCallSelfAsSudoWith ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    Ssh.ClientOpts ->
    Reporter Report ->
    Reporter Report ->
    FilePath ->
    Remote ->
    SelfPath ->
    Track' Ssh.Remote ->
    Track' directive ->
    CLI.BaseCommand ->
    directive ->
    Tracked' (RemoteCall directive)
uploadAndCallSelfAsSudoWith opts = uploadAndCallSelfOpts defaultCallOpts{callClient = opts, callAsSudo = True}

remoteDir ::
    forall directive.
    (ToJSON directive, FromJSON directive) =>
    Reporter Report ->
    RemoteSelf ->
    Track' directive ->
    (FilePath -> directive) ->
    FilePath ->
    Op
remoteDir r self simulate mkpath path =
    trackedGraph $ callSelf r ignoreTrack self simulate CLI.Up (mkpath path)
