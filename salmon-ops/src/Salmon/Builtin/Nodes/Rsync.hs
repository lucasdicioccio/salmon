module Salmon.Builtin.Nodes.Rsync where

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Ref
import Salmon.Reporter

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import System.Exit (ExitCode (..))

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

-- | Copies a directory to a remote, authenticating however ssh would by default.
sendDir :: Reporter Report -> Track' (Binary "rsync") -> Track' Directory -> Directory -> Remote -> FilePath -> Op
sendDir = sendDirWith Ssh.noClientOpts

{- | 'sendDir', with explicit ssh client options -- the same @--rsh@
treatment as 'sendFileWith', for a key ssh would not offer on its own (one
a recipe generated and had signed, typically).
-}
sendDirWith :: Ssh.ClientOpts -> Reporter Report -> Track' (Binary "rsync") -> Track' Directory -> Directory -> Remote -> FilePath -> Op
sendDirWith opts r rsync mkdir dir remote remotepath =
    withBinary rsync rsyncRun cmd $ \up ->
        op "rsync:send-dir" (deps [run mkdir dir]) $ \actions ->
            actions
                { help = "copies " <> Text.pack dirpath <> " to " <> Text.pack remotepath <> " over rsync"
                , ref = mkRef "rsync-senddir" (dirpath, remote.remoteUser, remote.remoteHost)
                , up = up r'
                }
  where
    cmd = SendDir dirpath remote remotepath opts
    r' = contramap (RunRsyncCommand cmd) r
    dirpath :: FilePath
    dirpath = dir.directoryPath

{- | Copies a directory /from/ a remote under a local one: 'sendDir' in the
other direction, and 'receiveFile' for more than one file. It exists so that
fetching a directory of results does not need an archive made on the remote
first.

The paths mean what they mean to rsync, as they do for 'sendDir': a remote
path without a trailing slash lands as a directory of that name /under/ the
local one (@\/srv\/results@ into @\/var\/in@ is @\/var\/in\/results@), and one
with a trailing slash has its contents put directly in the local one.

Unlike 'sendDir' this node has a 'check', which is what lets a pass run again
without fetching again: rsync is asked, with @--dry-run --itemize-changes@,
what the transfer would do, and the answer is read by 'interpretProbe'. Two
things follow from that:

* the transfer carries @--times@, which 'sendDir' does not. Without it the
  local copies get the time of the transfer, rsync's size-and-time comparison
  never finds a file up to date, and both the check and the fetch would redo
  everything every time. So the local files carry the remote's modification
  times.
* asking costs one ssh connection and a walk of the directory on both sides,
  each time something asks: every pass of @run up@, and every look a
  supervisor takes under @serve@.

A remote that cannot be reached, or an rsync that fails for any other reason,
answers 'Unknown' and not 'Failure': the check could not look, and that is
different from having looked and found the local copy behind.

Nothing is deleted. A file gone from the remote stays in the local copy (no
@--delete@), and @down@ leaves the fetched directory alone for the reason
'receiveFile' gives. A file changed locally is fetched again when its size or
time no longer match the remote's.
-}
receiveDir ::
    Reporter Report ->
    Track' (Binary "rsync") ->
    Track' Directory ->
    Remote ->
    -- | directory on the remote
    FilePath ->
    -- | local directory to copy it under
    Directory ->
    Op
receiveDir = receiveDirWith Ssh.noClientOpts

{- | 'receiveDir', with explicit ssh client options, through @--rsh@ as for
'sendDirWith'. They are not part of the node's 'ref': the same directory
fetched to the same place is the same effect whichever key reached it.
-}
receiveDirWith ::
    Ssh.ClientOpts ->
    Reporter Report ->
    Track' (Binary "rsync") ->
    Track' Directory ->
    Remote ->
    -- | directory on the remote
    FilePath ->
    -- | local directory to copy it under
    Directory ->
    Op
receiveDirWith opts r rsync mkdir remote remotepath dir =
    withBinary rsync rsyncRun cmd $ \up ->
        op "rsync:receive-dir" (deps [run mkdir dir]) $ \actions ->
            actions
                { help = "copies " <> Text.pack remotepath <> " from " <> loginAtHost remote <> " under " <> Text.pack dirpath <> " over rsync"
                , ref = mkRef "rsync-receivedir" (remotepath, dirpath, remote.remoteUser, remote.remoteHost)
                , check = probeReceiveDir r remote remotepath dirpath opts
                , up = up r'
                , down = pure ()
                }
  where
    cmd = ReceiveDir remote remotepath dirpath opts
    r' = contramap (RunRsyncCommand cmd) r
    dirpath :: FilePath
    dirpath = dir.directoryPath

{- | Asks rsync what 'receiveDir' would transfer, without transferring it.

The command's standard input is a pipe that is closed at once, never this
process's own. A process that could not be started at all (no @rsync@) is
'Unknown' like any other way of not having been able to look.
-}
probeReceiveDir :: Reporter Report -> Remote -> FilePath -> FilePath -> Ssh.ClientOpts -> IO CheckResult
probeReceiveDir r remote remotepath dirpath opts = do
    runReporter r' (Binary.CommandStart p)
    ran <- try (readCreateProcessWithExitCode p "")
    case ran of
        Left (_ :: IOException) -> pure Unknown
        Right (code, out, err) -> do
            runReporter r' (Binary.CommandStopped p code out err)
            pure (interpretProbe code out)
  where
    cmd = ProbeReceiveDir remote remotepath dirpath opts
    p = Binary.prepare rsyncRun cmd
    r' = contramap (RunRsyncCommand cmd) r

{- | The verdict drawn from a 'ProbeReceiveDir' run: its exit code and what
it wrote on standard output.

A non-zero exit is 'Unknown' whatever was printed (rsync could not complete
the comparison). Otherwise the copy is in place exactly when no line is
'Pending'. The 'Failure' text counts the differing items and names none of
them.
-}
interpretProbe :: ExitCode -> ByteString -> CheckResult
interpretProbe (ExitFailure _) _ = Unknown
interpretProbe ExitSuccess out =
    case length (filter (== Pending) (probeLines out)) of
        0 -> Success
        n -> Failure (Text.pack (show n) <> " item(s) differ from the remote directory")

-- | What one line of @rsync --dry-run --itemize-changes@ says.
data ProbeLine
    = -- | says nothing about the transfer: a blank line, or rsync's remark
      -- about something it never copies (@skipping non-regular file@)
      Noise
    | -- | an item rsync looked at and would leave alone (only printed when
      -- asked twice for itemizing, which the probe does not do)
      Unchanged
    | -- | anything else: an item to create, update or retouch, or a line
      -- this module does not know
      Pending
    deriving (Show, Eq, Ord)

probeLines :: ByteString -> [ProbeLine]
probeLines = map classifyProbeLine . Text.lines . Text.decodeUtf8With Text.lenientDecode

{- | Reads one line of the probe's output.

Deliberately lopsided: only what is positively recognised as harmless is
'Noise' or 'Unchanged', and everything else is 'Pending'. Being wrong towards
'Pending' costs an rsync run that transfers nothing; being wrong the other way
would leave a stale copy in place for good.

An itemized line is rsync's @YXcstpoguax@ followed by a space and the path:
eleven characters, the first being the kind of update and the last nine the
attributes. An item is unchanged when the update is @.@ and no attribute is
named (blanks or dots).
-}
classifyProbeLine :: Text -> ProbeLine
classifyProbeLine line
    | Text.null (Text.strip line) = Noise
    | "skipping " `Text.isPrefixOf` line = Noise
    | isUnchangedItem = Unchanged
    | otherwise = Pending
  where
    (code, rest) = Text.splitAt itemizedWidth line
    isUnchangedItem =
        Text.length code == itemizedWidth
            && " " `Text.isPrefixOf` rest
            && Text.take 1 code == "."
            && Text.all (`elem` ("fdLDS" :: String)) (Text.take 1 (Text.drop 1 code))
            && Text.all (`elem` (" ." :: String)) (Text.drop 2 code)

-- | The width of rsync's itemized-change code, @YXcstpoguax@.
itemizedWidth :: Int
itemizedWidth = 11

data RsyncCommand
    = SendFile FilePath Remote FilePath Ssh.ClientOpts
    | ReceiveFile Remote FilePath FilePath Ssh.ClientOpts
    | SendDir FilePath Remote FilePath Ssh.ClientOpts
    | -- | remote, directory there, local directory to copy it under
      ReceiveDir Remote FilePath FilePath Ssh.ClientOpts
    | -- | 'ReceiveDir' asked as a question: same operands, nothing written
      ProbeReceiveDir Remote FilePath FilePath Ssh.ClientOpts
    deriving (Show)

{- | The flags of a directory fetch. @--times@ is what makes a second run find
the first one's files up to date; see 'receiveDir'.
-}
receiveDirFlags :: [String]
receiveDirFlags = ["--copy-links", "--recursive", "--times"]

-- | What turns the fetch into a question about it.
probeFlags :: [String]
probeFlags = ["--dry-run", "--itemize-changes"]

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
        (SendDir src rem dst opts) ->
            proc "rsync" $
                ["--copy-links", "--recursive"]
                    <> (case Ssh.clientArgs opts of [] -> []; args -> ["--rsh", unwords ("ssh" : args)])
                    <> [src, Text.unpack (loginAtHost rem) <> ":" <> dst]
        (ReceiveDir rem src dst opts) ->
            proc "rsync" $
                receiveDirFlags
                    <> (case Ssh.clientArgs opts of [] -> []; args -> ["--rsh", unwords ("ssh" : args)])
                    <> [Text.unpack (loginAtHost rem) <> ":" <> src, dst]
        (ProbeReceiveDir rem src dst opts) ->
            proc "rsync" $
                receiveDirFlags
                    <> probeFlags
                    <> (case Ssh.clientArgs opts of [] -> []; args -> ["--rsh", unwords ("ssh" : args)])
                    <> [Text.unpack (loginAtHost rem) <> ":" <> src, dst]

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser, "@", rem.remoteHost]
