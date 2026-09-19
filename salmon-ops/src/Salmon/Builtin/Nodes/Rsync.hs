module Salmon.Builtin.Nodes.Rsync where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Ref
import Salmon.Reporter

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath (takeDirectory, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

import Salmon.Op.Track

-------------------------------------------------------------------------------
data Report
    = RunRsyncCommand !RsyncCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

data Remote = Remote {remoteUser :: Text, remoteHost :: Text}
    deriving (Show, Ord, Eq)

-- | Copies a file to a remote, authenticating however ssh would by default.
sendFile :: Reporter Report -> Track' (Binary "rsync") -> File "source" -> Remote -> FilePath -> Op
sendFile = sendFileWith Ssh.noClientOpts

{- | 'sendFile', with explicit ssh client options -- rsync has no @-i@, so
they go through @--rsh@. See "Salmon.Builtin.Nodes.Ssh".'Salmon.Builtin.Nodes.Ssh.ClientOpts'.
-}
sendFileWith :: Ssh.ClientOpts -> Reporter Report -> Track' (Binary "rsync") -> File "source" -> Remote -> FilePath -> Op
sendFileWith opts r rsync src remote remotepath =
    withFile src $ \filepath ->
        let cmd = (SendFile filepath remote remotepath opts)
         in withBinary rsync rsyncRun cmd $ \up ->
                op "rsync:sendfile" nodeps $ \actions ->
                    actions
                        { help = "copies " <> Text.pack filepath <> " to " <> Text.pack remotepath <> " over rsync"
                        , ref = mkRef "rsync-sendfile" (filepath, remotepath, remote.remoteUser, remote.remoteHost)
                        , up = up (r' cmd)
                        }
  where
    r' cmd = contramap (RunRsyncCommand cmd) r

{- | Copies a file /from/ a remote to a local path: the direction
'sendFile' does not go.

It exists for fetching something whose /name/ the local side knows but whose
/content/ only the remote can produce -- a database dump being the case it
was written for. That constraint is the interesting one: rsync cannot fetch
a file it cannot name, so a recipe that pulls has to fix the name on the
controlling side and hand it to the remote, rather than letting the remote
choose one (see "SreBox.PostgresBackup"'s @pgb_fixedTimestamp@).

Pulling rather than having the remote push is also the cheaper trust
arrangement: the controller already holds credentials for the remote,
whereas a push would need the remote to hold credentials for wherever the
file is going.

The enclosing directory is created first: rsync will not make a missing
destination directory for a single-file transfer, and fails in a way that
reads like a permissions problem.
-}
receiveFile :: Reporter Report -> Track' (Binary "rsync") -> Track' Directory -> Remote -> FilePath -> FilePath -> Op
receiveFile = receiveFileWith Ssh.noClientOpts

-- | 'receiveFile', with explicit ssh client options.
receiveFileWith ::
    Ssh.ClientOpts ->
    Reporter Report ->
    Track' (Binary "rsync") ->
    Track' Directory ->
    Remote ->
    -- | path on the remote
    FilePath ->
    -- | path to write locally
    FilePath ->
    Op
receiveFileWith opts r rsync mkdir remote remotepath localpath =
    withBinary rsync rsyncRun cmd $ \up ->
        op "rsync:receivefile" (deps [run mkdir (Directory (takeDirectory localpath))]) $ \actions ->
            actions
                { help = "copies " <> Text.pack remotepath <> " from " <> loginAtHost remote <> " over rsync"
                , ref = mkRef "rsync-receivefile" (remotepath, localpath, remote.remoteUser, remote.remoteHost)
                , up = up r'
                , -- the local copy is this node's effect, and a fetch that
                  -- happened is not undone by deleting the only copy of a
                  -- backup. Removing it is the caller's retention policy.
                  down = pure ()
                }
  where
    cmd = ReceiveFile remote remotepath localpath opts
    r' = contramap (RunRsyncCommand cmd) r

sendDir :: Reporter Report -> Track' (Binary "rsync") -> Track' Directory -> Directory -> Remote -> FilePath -> Op
sendDir r rsync mkdir dir remote remotepath =
    withBinary rsync rsyncRun cmd $ \up ->
        op "rsync:send-dir" (deps [run mkdir dir]) $ \actions ->
            actions
                { help = "copies " <> Text.pack dirpath <> " to " <> Text.pack remotepath <> " over rsync"
                , ref = mkRef "rsync-senddir" (dirpath, remote.remoteUser, remote.remoteHost)
                , up = up r'
                }
  where
    cmd = SendDir dirpath remote remotepath
    r' = contramap (RunRsyncCommand cmd) r
    dirpath :: FilePath
    dirpath = dir.directoryPath

data RsyncCommand
    = SendFile FilePath Remote FilePath Ssh.ClientOpts
    | ReceiveFile Remote FilePath FilePath Ssh.ClientOpts
    | SendDir FilePath Remote FilePath
    deriving (Show)

rsyncRun :: Command "rsync" RsyncCommand
rsyncRun = Command $ \run ->
    case run of
        (SendFile src rem dst opts) ->
            proc "rsync" $
                ["--copy-links"]
                    <> (case Ssh.clientArgs opts of [] -> []; args -> ["--rsh", unwords ("ssh" : args)])
                    <> [src, Text.unpack (loginAtHost rem) <> ":" <> dst]
        (ReceiveFile rem src dst opts) ->
            proc "rsync" $
                ["--copy-links"]
                    <> (case Ssh.clientArgs opts of [] -> []; args -> ["--rsh", unwords ("ssh" : args)])
                    <> [Text.unpack (loginAtHost rem) <> ":" <> src, dst]
        (SendDir src rem dst) -> proc "rsync" ["--copy-links", "--recursive", src, Text.unpack (loginAtHost rem) <> ":" <> dst]

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser, "@", rem.remoteHost]
