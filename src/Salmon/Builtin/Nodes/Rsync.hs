module Salmon.Builtin.Nodes.Rsync where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
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

sendFile :: Reporter Report -> Track' (Binary "rsync") -> File "source" -> Remote -> FilePath -> Op
sendFile r rsync src remote remotepath =
    withFile src $ \filepath ->
        let cmd = (SendFile filepath remote remotepath)
         in withBinary rsync rsyncRun cmd $ \up ->
                op "rsync:sendfile" nodeps $ \actions ->
                    actions
                        { help = "copies " <> Text.pack filepath <> " to " <> Text.pack remotepath <> " over rsync"
                        , ref = dotRef $ "rsync-copy:" <> Text.pack (show (filepath, remotepath, remote))
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
                , ref = dotRef $ "rsync-copy:" <> Text.pack (show (dirpath, remote))
                , up = up r'
                }
  where
    cmd = SendDir dirpath remote remotepath
    r' = contramap (RunRsyncCommand cmd) r
    dirpath :: FilePath
    dirpath = dir.directoryPath

data RsyncCommand
    = SendFile FilePath Remote FilePath
    | SendDir FilePath Remote FilePath
    deriving (Show)

rsyncRun :: Command "rsync" RsyncCommand
rsyncRun = Command $ \run ->
    case run of
        (SendFile src rem dst) -> proc "rsync" ["--copy-links", src, Text.unpack (loginAtHost rem) <> ":" <> dst]
        (SendDir src rem dst) -> proc "rsync" ["--copy-links", "--recursive", src, Text.unpack (loginAtHost rem) <> ":" <> dst]

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser, "@", rem.remoteHost]
