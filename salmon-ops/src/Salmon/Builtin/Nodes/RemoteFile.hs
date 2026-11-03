{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Putting a file that is /not/ a secret onto a remote machine over ssh,
with an owner and a mode: a systemd unit, a configuration file.

"Salmon.Builtin.Nodes.SecretDelivery".@uploadSecretFile@ is the same
transport for a secret, and it refuses any mode readable by others; this is
the node for everything that should be. It reuses that module's
'Placement' (through 'validateFilePlacement', which is the same gate on what
reaches a root shell, minus the secrecy rule), its 'Elevation' and its ssh
invocation, and differs where secrecy made that module careful:

* the contents are given when the graph is built, and their SHA-256 is in
  @notes@, so a re-declared file is a changed node under @run serve@ (as
  with @filecontents@);
* @check@ compares a digest of the remote file with that one, so it sends
  nothing down the connection;
* an empty file is a file, not a mistake.
-}
module Salmon.Builtin.Nodes.RemoteFile (
    RemoteFile (..),
    remoteFile,
    Report (..),
    RemoteFileError (..),

    -- * The remote scripts
    contentDigest,
    writeScript,
    digestProbeScript,
    remoteFileCommand,
) where

import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import Numeric (showHex)
import System.IO (Handle, hClose, stderr)
import System.Process (ProcessHandle, StdStream (..), waitForProcess)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, CommandIO (..), untrackedExecIO, withBinaryIO)
import Salmon.Builtin.Nodes.SecretDelivery (
    DeliveryCommand (..),
    Elevation (..),
    Placement (..),
    interpretProbe,
    remoteCommand,
    removeScript,
    shellQuote,
    validateFilePlacement,
 )
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Ref
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | A file's contents, and where they go on which machine.
data RemoteFile = RemoteFile
    { remoteFileContents :: ByteString
    , remoteFileRemote :: Ssh.Remote
    , remoteFilePlacement :: Placement
    -- ^ any mode, including ones readable by others
    , remoteFileElevation :: Elevation
    }
    deriving (Eq, Show)

-- | What is said about a write: the machine, the path, an exit code.
data Report
    = WriteStart !Ssh.Remote !FilePath
    | WriteDone !Ssh.Remote !FilePath !ExitCode
    | RemoveDone !Ssh.Remote !FilePath !ExitCode
    deriving (Show)

-- | Thrown by @up@ and @down@.
newtype RemoteFileError = RemoteFileError Text

instance Show RemoteFileError where
    show (RemoteFileError t) = "remote file: " <> Text.unpack t

instance Exception RemoteFileError

-------------------------------------------------------------------------------

{- | Writes a file onto a remote over ssh, owned and moded as declared.

The contents go down the connection as the remote command's standard input
into a temporary file beside the destination (created @0600@), whose digest
is checked against the declared one -- so a connection dropped mid-transfer
leaves the old file, not a truncated new one -- and which is then chowned,
chmoded and renamed over the destination. A missing enclosing directory is
created @0755@ (owned by whoever runs the script: root under 'WithSudo'); an
existing one is left as it is.

@check@ is a 'Success' when the remote file's SHA-256 is the declared one
and its owner and mode are as declared ('interpretProbe': 3 missing, 4
different contents, 5 owner or mode, 6 unreadable, 255 -- ssh not
connecting -- 'Unknown').

@down@ removes the file, and treats a machine it cannot reach (ssh exit 255)
as having nothing left to remove, as @uploadSecretFile@ does.

The 'Ref' is @("remote-file", host, path)@, the effect site. It is a
different kind from @uploadSecretFile@'s, so a secret and a non-secret
declared at the same path are not seen as colliding.
-}
remoteFile :: Ssh.ClientOpts -> Reporter Report -> Track' (Binary "ssh") -> RemoteFile -> Op
remoteFile opts r ssh file =
    withBinaryIO ssh remoteFileCommand (deliver writeScript) $ \send ->
        op "remote-file" nodeps $ \actions ->
            actions
                { help =
                    Text.unwords
                        [ "writes"
                        , Text.pack (show (ByteString.length file.remoteFileContents))
                        , "bytes to"
                        , site
                        ]
                , notes =
                    [ "owner " <> place.placeOwner <> ":" <> place.placeGroup <> ", mode " <> place.placeMode
                    , "sha256 " <> digest
                    ]
                , ref = mkRef "remote-file" (file.remoteFileRemote.remoteHost, place.placePath)
                , check = case validateFilePlacement place of
                    Left why -> pure (Failure why)
                    Right _ -> interpretProbe place <$> feeding "" (untrackedExecIO remoteFileCommand (deliver digestProbeScript))
                , up = do
                    valid
                    runReporter r (WriteStart file.remoteFileRemote place.placePath)
                    code <- feeding file.remoteFileContents send
                    runReporter r (WriteDone file.remoteFileRemote place.placePath code)
                    exited "write to" code
                , down = do
                    valid
                    code <- feeding "" (untrackedExecIO remoteFileCommand (deliver (removeScript . fst)))
                    runReporter r (RemoveDone file.remoteFileRemote place.placePath code)
                    unless (code == ExitFailure 255) (exited "removal from" code)
                }
  where
    place = file.remoteFilePlacement
    digest = contentDigest file.remoteFileContents
    site = Ssh.loginAtHost file.remoteFileRemote <> ":" <> Text.pack place.placePath

    -- An invalid placement renders a harmless script: every action checks
    -- 'valid' before running anything.
    validPlace = either (const place) id (validateFilePlacement place)
    deliver script = DeliveryCommand opts file.remoteFileRemote file.remoteFileElevation (script (validPlace, digest))

    valid :: IO ()
    valid = either (throwIO . RemoteFileError) (const (pure ())) (validateFilePlacement place)

    exited :: Text -> ExitCode -> IO ()
    exited _ ExitSuccess = pure ()
    exited what (ExitFailure n) =
        throwIO . RemoteFileError $
            Text.unwords [what, site, "failed (ssh exit", Text.pack (show n) <> ")"]

-- | Runs a prepared remote command with these bytes as its standard input.
feeding :: ByteString -> (() -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)) -> IO ExitCode
feeding bytes exec = do
    (mIn, _, _, ph) <- exec ()
    case mIn of
        Nothing -> pure ()
        Just h -> do
            -- ssh exiting early (no connection) closes the pipe under us;
            -- its exit code is the report, not the broken pipe.
            void (try (ByteString.hPut h bytes) :: IO (Either IOException ()))
            void (try (hClose h) :: IO (Either IOException ()))
    waitForProcess ph

-------------------------------------------------------------------------------

-- | The lowercase hex SHA-256 of some contents, as @sha256sum@ prints it.
contentDigest :: ByteString -> Text
contentDigest = Text.pack . concatMap hex . ByteString.unpack . SHA256.hash
  where
    hex w = let s = showHex w "" in if length s == 1 then '0' : s else s

{- | @ssh@ running a script on the remote, fed through a pipe this process
writes and closes. As with @uploadSecretFile@, the remote's standard output
goes to this process's standard error.
-}
remoteFileCommand :: CommandIO "ssh" DeliveryCommand ()
remoteFileCommand = CommandIO $ \(DeliveryCommand opts remote elevation script) () ->
    pure
        ( proc
            "ssh"
            ( ["-o", "BatchMode=yes"]
                <> Ssh.clientArgs opts
                <> [Text.unpack (Ssh.loginAtHost remote), remoteCommand elevation script]
            )
        )
            { std_in = CreatePipe
            , std_out = UseHandle stderr
            }

{- | Reads the file from standard input into place, refusing (exit 7, the
old file kept) when what arrived is not what was declared. Expects a
placement that went through 'validateFilePlacement' and its contents' digest.
-}
writeScript :: (Placement, Text) -> Text
writeScript (p, digest) =
    Text.intercalate
        "\n"
        [ "set -eu"
        , "p=" <> shellQuote (Text.pack p.placePath)
        , "d=$(dirname \"$p\")"
        , "if [ ! -d \"$d\" ]; then"
        , "  (umask 022 && mkdir -p \"$d\")"
        , "fi"
        , "umask 077"
        , "t=$(mktemp \"$p.XXXXXX\")"
        , "trap 'rm -f \"$t\"' EXIT"
        , "cat > \"$t\""
        , "h=$(sha256sum < \"$t\")"
        , "[ \"${h%% *}\" = " <> shellQuote digest <> " ] || exit 7"
        , "chown " <> shellQuote (p.placeOwner <> ":" <> p.placeGroup) <> " \"$t\""
        , "chmod " <> shellQuote p.placeMode <> " \"$t\""
        , "mv -f \"$t\" \"$p\""
        ]

{- | Compares the digest of the file in place with the declared one, then its
owner and mode; reads nothing from standard input and prints nothing. Exits
as "Salmon.Builtin.Nodes.SecretDelivery".@probeScript@ does: 0 all match, 3
absent, 4 different contents, 5 owner or mode, 6 unreadable.
-}
digestProbeScript :: (Placement, Text) -> Text
digestProbeScript (p, digest) =
    Text.intercalate
        "\n"
        [ "p=" <> shellQuote (Text.pack p.placePath)
        , "[ -f \"$p\" ] || exit 3"
        , "h=$(sha256sum < \"$p\" 2>/dev/null) || exit 6"
        , "[ \"${h%% *}\" = " <> shellQuote digest <> " ] || exit 4"
        , "[ \"$(stat -c '%U:%G %a' \"$p\")\" = " <> shellQuote (p.placeOwner <> ":" <> p.placeGroup <> " " <> p.placeMode) <> " ] || exit 5"
        , "exit 0"
        ]
