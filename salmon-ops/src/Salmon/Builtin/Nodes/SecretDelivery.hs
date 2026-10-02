{-# LANGUAGE OverloadedStrings #-}

{- | Putting a secret that already exists onto the machine that needs it.

Recipes in this tree take secrets as /pre-provisioned files/: a path, an
owner, and the assumption that something else put the bytes there. This
module is that something else, and it is deliberately a builtin a caller
opts into rather than a step inside any recipe -- a recipe that shipped its
own secrets would have chosen a transport for everybody who uses it.

Two transports, one per side of a hand-off:

* 'uploadSecretFile' runs on the /controlling/ machine and writes a local
  file onto a remote over ssh, with an owner and a mode. It is what a
  "SreBox.Gcp.VmProvision" @vmp_beforeCall@ wants.
* 'installSecretBytes' / 'checkInstalledSecret' are the on-machine half, the
  part of "Salmon.Builtin.Nodes.Gcp.SecretManager".@secretFile@ that is not
  about GCP: bytes obtained locally, placed atomically.

What every function here has in common is what it does /not/ do with the
bytes:

* they are read at @up@\/@check@ time, never when the graph is built, so
  they are not in the directive, the @ref@, @help@ or @notes@;
* they travel on a process's standard input, never in its argv (which is
  world-readable through @\/proc@ for as long as the process lives);
* no report, failure text or exception carries them, and neither does any
  digest of them. A fingerprint in @notes@ is what would let @run serve@ see
  a re-declared secret as a changed node, and it is left out on purpose: a
  digest of a password is an offline guessing oracle for whoever reads a
  report, and reports are public. The cost is that a changed secret is
  noticed by the node's @check@ (which compares the bytes themselves, over
  the same channel that carries them) and not by @Dag.sameRepresentative@.
-}
module Salmon.Builtin.Nodes.SecretDelivery (
    -- * Where a secret goes
    Placement (..),
    validatePlacement,

    -- * Upload over ssh
    SecretUpload (..),
    Elevation (..),
    uploadSecretFile,
    Report (..),
    SecretDeliveryError (..),

    -- * The remote scripts, and what their exit codes mean
    uploadScript,
    probeScript,
    removeScript,
    interpretProbe,
    deliveryCommand,
    DeliveryCommand (..),
    remoteCommand,
    shellQuote,

    -- * On-machine placement
    installSecretBytes,
    checkInstalledSecret,
    removeInstalledSecret,
) where

import Control.Exception (Exception, onException, throwIO)
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isAlphaNum, isOctDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import Numeric (readOct)
import System.Directory (doesFileExist, getFileSize, removeFile, renameFile)
import System.FilePath (isAbsolute)
import System.IO (Handle, IOMode (..), hClose, stderr, withBinaryFile)
import System.Posix.Files (fileGroup, fileMode, fileOwner, getFileStatus, setFileMode, setOwnerAndGroup)
import System.Posix.IO (createFile, fdToHandle)
import System.Posix.Types (FileMode)
import System.Posix.User (getGroupEntryForName, getUserEntryForName, groupID, userID)
import System.Process (ProcessHandle, StdStream (..), waitForProcess)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, CommandIO (..), untrackedExecIO, withBinaryIO)
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Ref
import Salmon.Reporter

-------------------------------------------------------------------------------

{- | What is said about a delivery. It names files and machines and carries
an exit code; there is no constructor that could hold the bytes.
-}
data Report
    = UploadStart !SecretUpload
    | UploadDone !SecretUpload !ExitCode
    | RemoveDone !SecretUpload !ExitCode
    deriving (Show)

-- | Thrown by @up@. The text names paths and machines, never contents.
newtype SecretDeliveryError = SecretDeliveryError Text

instance Show SecretDeliveryError where
    show (SecretDeliveryError t) = "secret delivery: " <> Text.unpack t

instance Exception SecretDeliveryError

-------------------------------------------------------------------------------

-- | Where a secret file goes, and who may read it.
data Placement = Placement
    { placePath :: FilePath
    -- ^ absolute
    , placeOwner :: Text
    , placeGroup :: Text
    , placeMode :: Text
    -- ^ octal, three or four digits (@"0600"@, @"640"@). A mode granting
    -- anything to /others/ is refused: there is no secret that is right for.
    }
    deriving (Eq, Ord, Show)

{- | The placement with its mode normalized (no leading zeros, the way
@stat -c %a@ prints one), or why it is refused.

Everything here ends up inside a shell script run as root on another
machine, so this is the narrow gate: names are plain account names, the mode
is digits. The path is quoted rather than restricted ('shellQuote'), but a
newline or a NUL in one is refused as a mistake.
-}
validatePlacement :: Placement -> Either Text Placement
validatePlacement p
    | not (isAbsolute p.placePath) = refuse "the path must be absolute"
    | any (`elem` ['\n', '\r', '\0']) p.placePath = refuse "the path holds a control character"
    | not (accountName p.placeOwner) = refuse "the owner must be a plain account name"
    | not (accountName p.placeGroup) = refuse "the group must be a plain group name"
    | Text.length p.placeMode < 3 || Text.length p.placeMode > 4 || not (Text.all isOctDigit p.placeMode) =
        refuse "the mode must be three or four octal digits"
    | Text.last p.placeMode /= '0' = refuse "the mode grants access to others"
    | otherwise = Right p{placeMode = normalized}
  where
    refuse why = Left (Text.pack p.placePath <> ": " <> why)
    normalized = case Text.dropWhile (== '0') p.placeMode of
        "" -> "0"
        m -> m
    accountName t = case Text.uncons t of
        Just (c, _) -> c /= '-' && Text.all (\x -> isAlphaNum x && x < '\x80' || x `elem` ['_', '-', '.']) t
        Nothing -> False

-------------------------------------------------------------------------------

-- | Whether the remote side needs root to write where the secret goes.
data Elevation
    = -- | write as the ssh login user
      AsLoginUser
    | -- | write under @sudo -n@: for any owner but the login user
      WithSudo
    deriving (Eq, Ord, Show)

-- | A local secret file, and where it goes on which machine.
data SecretUpload = SecretUpload
    { uploadSource :: FilePath
    -- ^ on the controlling machine; read when @up@ or @check@ runs, so a
    -- secret generated by an earlier node of the same pass is the one sent.
    , uploadRemote :: Ssh.Remote
    , uploadPlacement :: Placement
    , uploadElevation :: Elevation
    }
    deriving (Eq, Show)

{- | Writes a local secret file onto a remote over ssh, owned and moded as
declared.

The bytes go down the ssh connection as the remote command's standard input,
into a temporary file beside the destination (created @0600@, so there is no
moment at which it is more readable than it will end up), which is then
chowned, chmoded and renamed over the destination: a reader sees the old
secret or the new one, never half of either. A missing enclosing directory
is created owned as the file and @0750@; an existing one is left as it is.

@check@ compares the remote file with the local one byte for byte (@cmp@ on
the remote, fed over the same connection) and then its owner and mode, so a
healthy delivery is skipped and a rotated, edited or re-moded one is put
back. An unreachable machine is 'Unknown', not 'Failure': nothing was
learned about the file.

@down@ removes the remote file (the local one is somebody else's node), and
treats a machine it cannot reach as having nothing left to remove: a failed
@down@ blocks the teardown of everything beneath it, which here includes the
machine.

The 'Ref' is the remote host and path -- the effect site -- so two
declarations putting different files at one path collide, as they should.
-}
uploadSecretFile :: Ssh.ClientOpts -> Reporter Report -> Track' (Binary "ssh") -> SecretUpload -> Op
uploadSecretFile opts r ssh upload =
    withBinaryIO ssh deliveryCommand (deliver uploadScript) $ \send ->
        op "secret:upload" nodeps $ \actions ->
            actions
                { help =
                    Text.unwords
                        [ "uploads the secret file"
                        , Text.pack upload.uploadSource
                        , "to"
                        , Ssh.loginAtHost upload.uploadRemote <> ":" <> Text.pack place.placePath
                        ]
                , notes =
                    [ "owner " <> place.placeOwner <> ":" <> place.placeGroup <> ", mode " <> place.placeMode
                    , "the contents are sent on standard input and never reported"
                    ]
                , ref = mkRef "secret-upload" (upload.uploadRemote.remoteHost, place.placePath)
                , check = checkUpload
                , up = do
                    valid
                    sourceReady
                    runReporter r (UploadStart upload)
                    code <- feeding upload.uploadSource send
                    runReporter r (UploadDone upload code)
                    exited "upload to" code
                , down = do
                    valid
                    code <- feeding "/dev/null" (untrackedExecIO deliveryCommand (deliver removeScript))
                    runReporter r (RemoveDone upload code)
                    -- 255 is ssh failing to connect. A machine that cannot
                    -- be reached is usually one that is already gone, and a
                    -- throw here would leave this node standing and block
                    -- the teardown of the very machine it is on.
                    unless (code == ExitFailure 255) (exited "removal from" code)
                }
  where
    place = upload.uploadPlacement

    -- An invalid placement renders a harmless script: every action checks
    -- 'valid' before running anything.
    deliver script = DeliveryCommand opts upload.uploadRemote upload.uploadElevation (script (either (const place) id (validatePlacement place)))

    valid :: IO ()
    valid = either (throwIO . SecretDeliveryError) (const (pure ())) (validatePlacement place)

    sourceReady :: IO ()
    sourceReady = do
        present <- doesFileExist upload.uploadSource
        unless present $ throwIO (SecretDeliveryError ("nothing to upload: " <> Text.pack upload.uploadSource))
        size <- getFileSize upload.uploadSource
        when (size == 0) $ throwIO (SecretDeliveryError ("refusing to upload an empty secret: " <> Text.pack upload.uploadSource))

    exited :: Text -> ExitCode -> IO ()
    exited _ ExitSuccess = pure ()
    exited what (ExitFailure n) =
        throwIO . SecretDeliveryError $
            Text.unwords [what, Ssh.loginAtHost upload.uploadRemote <> ":" <> Text.pack place.placePath, "failed (ssh exit", Text.pack (show n) <> ")"]

    checkUpload :: IO CheckResult
    checkUpload = case validatePlacement place of
        Left why -> pure (Failure why)
        Right _ -> do
            present <- doesFileExist upload.uploadSource
            if not present
                then pure (Failure ("nothing to upload: " <> Text.pack upload.uploadSource))
                else interpretProbe place <$> feeding upload.uploadSource (untrackedExecIO deliveryCommand (deliver probeScript))

-- | Runs a prepared remote command with a local file as its standard input.
feeding :: FilePath -> (Handle -> IO (a, b, c, ProcessHandle)) -> IO ExitCode
feeding path exec =
    withBinaryFile path ReadMode $ \h -> do
        (_, _, _, ph) <- exec h
        waitForProcess ph

-------------------------------------------------------------------------------

-- | One ssh invocation: who to reach, as whom, and the script to run there.
data DeliveryCommand = DeliveryCommand Ssh.ClientOpts Ssh.Remote Elevation Text
    deriving (Show)

{- | @ssh@ running a script on the remote with the given handle as its
standard input. The remote's standard output is sent to this process's
standard error: the scripts print nothing, and a salmon binary's standard
output may be a @--json@ stream.

@BatchMode@ because the only thing that may ever be read from the terminal
here is nothing.
-}
deliveryCommand :: CommandIO "ssh" DeliveryCommand Handle
deliveryCommand = CommandIO $ \(DeliveryCommand opts remote elevation script) input ->
    pure
        ( proc
            "ssh"
            ( ["-o", "BatchMode=yes"]
                <> Ssh.clientArgs opts
                <> [Text.unpack (Ssh.loginAtHost remote), remoteCommand elevation script]
            )
        )
            { std_in = UseHandle input
            , std_out = UseHandle stderr
            }

{- | The single word handed to ssh as the remote command. ssh joins its
arguments with spaces and gives the result to the remote user's shell, so
the script has to arrive as one quoted argument of @sh -c@.
-}
remoteCommand :: Elevation -> Text -> String
remoteCommand elevation script =
    Text.unpack (prefix <> "sh -c " <> shellQuote script)
  where
    prefix = case elevation of
        AsLoginUser -> ""
        WithSudo -> "sudo -n "

-- | A string as one single-quoted POSIX shell word.
shellQuote :: Text -> Text
shellQuote t = "'" <> Text.replace "'" "'\\''" t <> "'"

{- | Reads the secret from standard input into place. Expects a placement
that went through 'validatePlacement'.
-}
uploadScript :: Placement -> Text
uploadScript p =
    Text.intercalate
        "\n"
        [ "set -eu"
        , "p=" <> path
        , "d=$(dirname \"$p\")"
        , "if [ ! -d \"$d\" ]; then"
        , "  mkdir -p \"$(dirname \"$d\")\""
        , "  install -d -m 0750 -o " <> shellQuote p.placeOwner <> " -g " <> shellQuote p.placeGroup <> " \"$d\""
        , "fi"
        , "umask 077"
        , "t=$(mktemp \"$p.XXXXXX\")"
        , "trap 'rm -f \"$t\"' EXIT"
        , "cat > \"$t\""
        , "test -s \"$t\""
        , "chown " <> shellQuote (p.placeOwner <> ":" <> p.placeGroup) <> " \"$t\""
        , "chmod " <> shellQuote p.placeMode <> " \"$t\""
        , "mv -f \"$t\" \"$p\""
        ]
  where
    path = shellQuote (Text.pack p.placePath)

{- | Compares the file in place with standard input, then its owner and
mode. Exits 0 when all three match, 3 when the file is absent, 4 when the
contents differ, 5 when the owner or mode do, 6 when it could not be read.
Prints nothing.
-}
probeScript :: Placement -> Text
probeScript p =
    Text.intercalate
        "\n"
        [ "p=" <> shellQuote (Text.pack p.placePath)
        , "[ -f \"$p\" ] || exit 3"
        , "cmp -s - \"$p\""
        , "case $? in 0) ;; 1) exit 4 ;; *) exit 6 ;; esac"
        , "[ \"$(stat -c '%U:%G %a' \"$p\")\" = " <> shellQuote (p.placeOwner <> ":" <> p.placeGroup <> " " <> p.placeMode) <> " ] || exit 5"
        , "exit 0"
        ]

-- | Removes the file in place; succeeds when there is none.
removeScript :: Placement -> Text
removeScript p = "rm -f " <> shellQuote (Text.pack p.placePath)

{- | The verdict drawn from 'probeScript'\'s exit code as ssh relays it.

255 is ssh's own "could not connect", which says nothing about the file:
'Unknown', so that a supervisor keeps looking rather than re-uploading at a
machine it cannot reach. (A one-shot pass reads 'Unknown' as "apply", and the
upload then fails loudly, which is the right report.)
-}
interpretProbe :: Placement -> ExitCode -> CheckResult
interpretProbe _ ExitSuccess = Success
interpretProbe p (ExitFailure n) = case n of
    3 -> Failure ("missing: " <> path)
    4 -> Failure (path <> " holds different contents")
    5 -> Failure (path <> " has the wrong owner or mode")
    6 -> Failure (path <> " could not be read")
    255 -> Unknown
    _ -> Failure ("could not inspect " <> path <> " (exit " <> Text.pack (show n) <> ")")
  where
    path = Text.pack p.placePath

-------------------------------------------------------------------------------
-- On-machine placement

{- | Writes bytes to a placement on /this/ machine: a @0600@ temporary file
beside the destination, chowned, chmoded, then renamed over it.

The enclosing directory must exist (declare it as a dependency): unlike the
upload, the caller here is a graph running on the machine and has the
vocabulary to say what that directory should be.

Throws on an invalid placement, an empty secret, an unknown owner or group,
or a chown this process is not allowed to make.
-}
installSecretBytes :: Placement -> ByteString -> IO ()
installSecretBytes place bytes = do
    p <- either (throwIO . SecretDeliveryError) pure (validatePlacement place)
    when (ByteString.null bytes) $
        throwIO (SecretDeliveryError ("refusing to write an empty secret: " <> Text.pack p.placePath))
    let tmp = p.placePath <> ".salmon-tmp"
    -- a leftover from a killed pass keeps whatever mode it had; createFile
    -- would truncate it without tightening it.
    stale <- doesFileExist tmp
    when stale (removeFile tmp)
    ( do
            h <- createFile tmp 0o600 >>= fdToHandle
            ByteString.hPut h bytes `onException` hClose h
            hClose h
            user <- getUserEntryForName (Text.unpack p.placeOwner)
            grp <- getGroupEntryForName (Text.unpack p.placeGroup)
            setOwnerAndGroup tmp (userID user) (groupID grp)
            setFileMode tmp (modeBits p)
            renameFile tmp p.placePath
        )
        `onException` (doesFileExist tmp >>= \left -> when left (removeFile tmp))

{- | Whether a placement on this machine holds exactly these bytes, with the
declared owner and mode. The failure text names the file and never quotes it.
-}
checkInstalledSecret :: Placement -> ByteString -> IO CheckResult
checkInstalledSecret place wanted = case validatePlacement place of
    Left why -> pure (Failure why)
    Right p -> do
        let path = Text.pack p.placePath
        present <- doesFileExist p.placePath
        if not present
            then pure (Failure ("missing: " <> path))
            else do
                size <- getFileSize p.placePath
                same <-
                    if size /= fromIntegral (ByteString.length wanted)
                        then pure False
                        else (== wanted) <$> ByteString.readFile p.placePath
                st <- getFileStatus p.placePath
                user <- getUserEntryForName (Text.unpack p.placeOwner)
                grp <- getGroupEntryForName (Text.unpack p.placeGroup)
                pure $
                    if not same
                        then Failure (path <> " holds different contents")
                        else
                            if fileOwner st /= userID user || fileGroup st /= groupID grp || fileMode st .&. 0o7777 /= modeBits p
                                then Failure (path <> " has the wrong owner or mode")
                                else Success

-- | Removes a placement on this machine; nothing to do when it is absent.
removeInstalledSecret :: Placement -> IO ()
removeInstalledSecret place = do
    present <- doesFileExist place.placePath
    when present (removeFile place.placePath)

-- | The mode of a validated placement.
modeBits :: Placement -> FileMode
modeBits p = case readOct (Text.unpack p.placeMode) of
    [(n, "")] -> n
    _ -> 0o600
