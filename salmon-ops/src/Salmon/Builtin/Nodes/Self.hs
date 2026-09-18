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
import Salmon.Op.Ref
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

uploadSelf :: Reporter Report -> FilePath -> Remote -> SelfPath -> Tracked' RemoteSelf
uploadSelf = uploadSelfWith Ssh.noClientOpts

-- | 'uploadSelf', with explicit ssh client options.
uploadSelfWith :: Ssh.ClientOpts -> Reporter Report -> FilePath -> Remote -> SelfPath -> Tracked' RemoteSelf
uploadSelfWith opts r remotedir remote path =
    Tracked (Track $ \self -> op "copy-oneself" (deps [copy]) id) (RemoteSelf remote selfpathOnRemote)
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
    Tracked (Track $ \_ -> op "self-call" (deps [callOverSSH]) modActions) rc
  where
    modActions actions =
        actions
            { ref = mkRef "call-self" (encode directive)
            , dynamics = [toDyn $ CLI.RemoteOp $ run simulate directive]
            }
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
    Tracked (Track $ \_ -> op "self-call" (deps [callOverSSH]) modActions) rc
  where
    modActions actions =
        actions
            { ref = mkRef "call-self-sudo" (encode directive)
            , dynamics = [toDyn $ CLI.RemoteOp $ run simulate directive]
            }
    rc = RemoteCall base directive
    sshRemote = Ssh.Remote self.selfRemote.remoteUser self.selfRemote.remoteHost
    cmdArgs = [Text.pack self.selfRemotePath, "run", CLI.argForBaseCommand base]
    cmdStdin = toStrict $ encode directive
    callOverSSH = Ssh.callWith opts r' Debian.ssh mkRemote sshRemote "sudo" cmdArgs cmdStdin
    r' = contramap RunSsh r

-- | Upload this binary to a remote host, then invoke it there over SSH with
-- a directive — the composition every recipe that self-orchestrates a
-- remote step was hand-rolling via uploadSelf `bindTracked` callSelf.
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
