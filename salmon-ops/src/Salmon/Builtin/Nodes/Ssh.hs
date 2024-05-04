module Salmon.Builtin.Nodes.Ssh where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinaryStdin)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track (Track (..), run)
import Salmon.Reporter

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------
data Report
    = RunSSHCommand !SSHCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

data Remote = Remote {remoteUser :: Text, remoteHost :: Text}
    deriving (Show, Ord, Eq)

call :: Reporter Report -> Track' (Binary "ssh") -> Track' Remote -> Remote -> FilePath -> [Text] -> ByteString -> Op
call r ssh tRemote remote remotepath args stdin =
    withBinaryStdin ssh sshRun cmd stdin $ \up ->
        op "ssh:call" (deps [run tRemote remote]) $ \actions ->
            actions
                { help = "calls " <> Text.pack remotepath <> " on " <> remote.remoteHost <> " with args " <> Text.intercalate " " args <> " and stdin " <> Text.decodeUtf8 stdin
                , notes = Text.pack remotepath : args
                , ref = dotRef $ "ssh-run" <> Text.pack (show (remotepath, remote, args, stdin))
                , up = up r'
                }
  where
    cmd = Call remotepath remote args
    r' = contramap (RunSSHCommand cmd) r

data SSHCommand = Call FilePath Remote [Text]
    deriving (Show)

sshRun :: Command "ssh" SSHCommand
sshRun = Command $ \(Call path rem args) ->
    proc
        "ssh"
        ( [ Text.unpack (loginAtHost rem)
          , path
          ]
            <> map Text.unpack args
        )

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser, "@", rem.remoteHost]

preExistingRemoteMachine :: Track' Remote
preExistingRemoteMachine = Track $ \r -> placeholder "remote" ("a remote at" <> r.remoteHost)
