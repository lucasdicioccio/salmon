module Salmon.Builtin.Nodes.Rsync where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

import Salmon.Op.Track

data Remote = Remote {remoteUser :: Text, remoteHost :: Text}
    deriving (Show, Ord, Eq)

sendFile :: Track' (Binary "rsync") -> File "source" -> Remote -> FilePath -> Op
sendFile rsync src remote remotepath =
    withFile src $ \filepath ->
        withBinary rsync rsyncRun (SendFile filepath remote remotepath) $ \up ->
            op "rsync:sendfile" nodeps $ \actions ->
                actions
                    { help = "copies " <> Text.pack filepath <> " to " <> Text.pack remotepath <> " over rsync"
                    , ref = dotRef $ "rsync-copy:" <> Text.pack (show (filepath, remotepath, remote))
                    , up = up
                    }

sendDir :: Track' (Binary "rsync") -> Track' Directory -> Directory -> Remote -> FilePath -> Op
sendDir rsync mkdir dir remote remotepath =
    withBinary rsync rsyncRun (SendDir dirpath remote remotepath) $ \up ->
        op "rsync:send-dir" (deps [run mkdir dir]) $ \actions ->
            actions
                { help = "copies " <> Text.pack dirpath <> " to " <> Text.pack remotepath <> " over rsync"
                , ref = dotRef $ "rsync-copy:" <> Text.pack (show (dirpath, remote))
                , up = up
                }
  where
    dirpath :: FilePath
    dirpath = dir.directoryPath

data Run
    = SendFile FilePath Remote FilePath
    | SendDir FilePath Remote FilePath

rsyncRun :: Command "rsync" Run
rsyncRun = Command $ \run ->
    case run of
        (SendFile src rem dst) -> proc "rsync" ["--copy-links", src, Text.unpack (loginAtHost rem) <> ":" <> dst]
        (SendDir src rem dst) -> proc "rsync" ["--copy-links", "--recursive", src, Text.unpack (loginAtHost rem) <> ":" <> dst]

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser, "@", rem.remoteHost]
