{-# LANGUAGE DeriveGeneric #-}

-- | todo: pass rsync in
module Salmon.Builtin.Nodes.Self where

import Data.Aeson (FromJSON, ToJSON, encode)
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
callSelf r mkRemote self simulate base directive =
    Tracked (Track $ const $ remoteCallNode simulate directive callOverSSH) rc
  where
    rc = RemoteCall base directive
    sshRemote = Ssh.Remote self.selfRemote.remoteUser self.selfRemote.remoteHost
    cmdArgs = ["run", CLI.argForBaseCommand base]
    cmdStdin = toStrict $ encode directive
    callOverSSH = Ssh.call r' Debian.ssh mkRemote sshRemote self.selfRemotePath cmdArgs cmdStdin
    r' = contramap RunSsh r

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
callSelfAsSudoWith opts r mkRemote self simulate base directive =
    Tracked (Track $ const $ remoteCallNode simulate directive callOverSSH) rc
  where
    rc = RemoteCall base directive
    sshRemote = Ssh.Remote self.selfRemote.remoteUser self.selfRemote.remoteHost
    cmdArgs = [Text.pack self.selfRemotePath, "run", CLI.argForBaseCommand base]
    cmdStdin = toStrict $ encode directive
    callOverSSH = Ssh.callWith opts r' Debian.ssh mkRemote sshRemote "sudo" cmdArgs cmdStdin
    r' = contramap RunSsh r

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
uploadAndCallSelf rUpload rCall remotedir uploadRemote selfpath mkRemote simulate base directive =
    uploadSelf rUpload remotedir uploadRemote selfpath `bindTracked` \self ->
        callSelf rCall mkRemote self simulate base directive

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
uploadAndCallSelfAsSudoWith opts rUpload rCall remotedir uploadRemote selfpath mkRemote simulate base directive =
    uploadSelfWith opts rUpload remotedir uploadRemote selfpath `bindTracked` \self ->
        callSelfAsSudoWith opts rCall mkRemote self simulate base directive

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
