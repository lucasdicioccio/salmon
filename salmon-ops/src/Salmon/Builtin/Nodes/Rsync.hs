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

import System.FilePath ((</>))
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
        (SendDir src rem dst) -> proc "rsync" ["--copy-links", "--recursive", src, Text.unpack (loginAtHost rem) <> ":" <> dst]

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser, "@", rem.remoteHost]
