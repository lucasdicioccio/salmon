module Salmon.Builtin.Nodes.Ssh where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..))
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track (Track (..), run)
import Salmon.Reporter

import Salmon.Builtin.Nodes.Systemd (ensureWatchKeyAt, readWatchKeyAt)

import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64.URL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import System.Directory (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------
data Report
    = RunSSHCommand !SSHCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

data Remote = Remote {remoteUser :: Text, remoteHost :: Text}
    deriving (Show, Ord, Eq)

{- | How the local ssh client should authenticate, and where it should keep
host keys.

Additive rather than fields on 'Remote' on purpose: every existing caller
authenticates ambiently against @~\/.ssh@, and changing 'Remote' would break
them all (including out-of-tree ones) for parameters they do not have.
-}
data ClientOpts = ClientOpts
    { optIdentity :: Maybe FilePath
    -- ^ a private key to offer. A certificate signed by
    -- "Salmon.Builtin.Nodes.Keys".@signKey@ sits next to it as
    -- @\<key\>-cert.pub@, which is what @ssh -i \<key\>@ picks up, so this
    -- one path carries both halves of an SSH-CA login.
    , optKnownHosts :: Maybe FilePath
    -- ^ a known-hosts file of this recipe's own. Worth setting whenever the
    -- hosts being reached are ephemeral: a VM rebuilt at a reserved address
    -- presents a new host key, and an entry for the old one in the user's
    -- @~\/.ssh\/known_hosts@ fails every later connection with
    -- @REMOTE HOST IDENTIFICATION HAS CHANGED@ (which @accept-new@ does not,
    -- and should not, override).
    }
    deriving (Eq, Show)

-- | Authenticate however ssh would by default.
noClientOpts :: ClientOpts
noClientOpts = ClientOpts Nothing Nothing

-- | The @ssh@ flags 'ClientOpts' asks for.
clientArgs :: ClientOpts -> [String]
clientArgs opts =
    maybe [] (\key -> ["-i", key, "-o", "IdentitiesOnly=yes"]) opts.optIdentity
        <> maybe [] (\hosts -> ["-o", "UserKnownHostsFile=" <> hosts, "-o", "StrictHostKeyChecking=accept-new"]) opts.optKnownHosts

{- | Calls a command on a remote over ssh, with whatever identities ssh
would offer by default (an agent, or @~\/.ssh\/id_*@).

See 'callWith' when the key to authenticate with is one salmon itself
generated, which ssh has no reason to try.
-}
call ::
    Reporter Report ->
    Track' (Binary "ssh") ->
    Track' Remote ->
    Remote ->
    FilePath ->
    [Text] ->
    ByteString ->
    Op
call = callWith noClientOpts

-- | 'call', with explicit client options.
callWith ::
    ClientOpts ->
    Reporter Report ->
    Track' (Binary "ssh") ->
    Track' Remote ->
    Remote ->
    FilePath ->
    [Text] ->
    ByteString ->
    Op
callWith = callTapped Binary.captured Binary.plainLines

{- | 'callWith', with a say in where the remote command's output goes
("Salmon.Builtin.Nodes.Binary".'Binary.Routing').

Under 'Binary.streamed' each line the remote command writes is a
"Salmon.Builtin.NodeLog" line about this node, said while the command is
still running on the other machine, where 'call' and 'callWith' hold all of
it until ssh exits. Only the caller knows whether that is safe: a streamed
line is public the moment it is read, so a remote command that can print a
secret keeps 'Binary.captured', which is what every call was before there
was a choice.

The node is the same node whatever the routing: same 'Ref', @help@ and
@notes@, so asking for the output to be followed restarts nothing.
-}
callRouted ::
    Binary.Routing ->
    ClientOpts ->
    Reporter Report ->
    Track' (Binary "ssh") ->
    Track' Remote ->
    Remote ->
    FilePath ->
    [Text] ->
    ByteString ->
    Op
callRouted routing = callTapped routing Binary.plainLines

{- | 'callRouted', with a 'Binary.LineTap' deciding what is said for each
streamed line: for a remote command whose output is a report stream to
re-tell rather than text to repeat.
-}
callTapped ::
    Binary.Routing ->
    Binary.LineTap ->
    ClientOpts ->
    Reporter Report ->
    Track' (Binary "ssh") ->
    Track' Remote ->
    Remote ->
    FilePath ->
    [Text] ->
    ByteString ->
    Op
callTapped routing tap opts r ssh tRemote remote remotepath args stdin =
    Binary.withBinaryStdinTapped routing tap ssh sshRun cmd stdin $ \up ->
        op "ssh:call" (deps [run tRemote remote]) $ \actions ->
            actions
                { help = "calls " <> Text.pack remotepath <> " on " <> remote.remoteHost <> " with args " <> Text.intercalate " " args <> " and stdin " <> stdinShown
                , notes = Text.pack remotepath : args
                , ref = callRef remotepath remote args stdin
                , up = up r'
                }
  where
    cmd = Call remotepath remote args opts
    r' = contramap (RunSSHCommand cmd) r
    stdinShown
        | ByteString.null stdin = ""
        | otherwise = stdinTag callKey stdin

{- | The 'Ref' of a call. With nothing on standard input it is the 'Ref' a
call always had. With something, the input itself is no longer hashed into
it (a 'Ref' is a fast unkeyed hash, so that was an offline guessing aid for
anything low-entropy in the input, a password in a directive say); its
'stdinTag' is, so two calls differing only by their input are still two
nodes.
-}
callRef :: FilePath -> Remote -> [Text] -> ByteString -> Ref
callRef remotepath remote args stdin
    | ByteString.null stdin = mkRef "ssh-run" (show (remotepath, remote, args, stdin))
    | otherwise = mkRef "ssh-run" (show (remotepath, remote, args, stdinTag callKey stdin))

{- | What a call's @help@ (and 'callRef') says of its standard input: an
HMAC-SHA256 under the 'CallKey', 128 bits of it in base64url. Enough to tell
two inputs apart, and nothing to guess one from without the key.

The input itself is never in @help@, @notes@ or the 'Ref' (all public, see
CLAUDE.md), and neither is a plain digest of it. A tag made under a key of
this process only ('Ephemeral') says so.
-}
stdinTag :: CallKey -> ByteString -> Text
stdinTag (CallKey origin key) stdin =
    prefix origin <> Text.take 22 (Text.decodeUtf8 (Base64.URL.encode (SHA256.hmac key stdin)))
  where
    prefix Persistent = "hmac-sha256:"
    prefix Ephemeral = "hmac-sha256-ephemeral:"

-- | Where a 'CallKey' came from.
data CallKeyOrigin
    = -- | the key file ('callKeyFile'): one key for every process of this user
      Persistent
    | -- | made for this process alone, the key file being unusable
      Ephemeral
    deriving (Eq, Show)

data CallKey = CallKey !CallKeyOrigin !ByteString

{- | The key file: @$SALMON_CALL_KEY_FILE@ when set and not empty, else
@call.key@ in salmon's XDG state directory (@~\/.local\/state\/salmon@).
-}
callKeyFile :: IO FilePath
callKeyFile = do
    override <- lookupEnv "SALMON_CALL_KEY_FILE"
    case override of
        Just path | not (null path) -> pure path
        _ -> (</> "call.key") <$> getXdgDirectory XdgState "salmon"

{- | The key at a path, made first if there is none (32 random bytes, @0600@,
put in place with @link(2)@ and never replaced: see
"Salmon.Builtin.Nodes.Systemd".'ensureWatchKeyAt').
-}
loadCallKeyAt :: FilePath -> IO ByteString
loadCallKeyAt path = do
    createDirectoryIfMissing True (takeDirectory path)
    ensureWatchKeyAt path
    readWatchKeyAt path

{- | The key 'callRef' and @help@ tag a standard input under: read (made if
missing) once per process, when the first call with an input is rendered.

Persistent on purpose: a 'Ref' is matched across processes (a plan from
@query plan@ is applied by a later @run up --plan@), and a key of one process
would make a node that plan excludes run in the next. When the file cannot be
made or read (a read-only home, say) the key is made for this process alone
and every tag says @ephemeral@.

It is read at graph build, which the secrets rule forbids for /secret/ bytes:
this key protects those, carries none of them, and an 'Op' is pure, so there
is no later moment to read it at.
-}
callKey :: CallKey
callKey = unsafePerformIO $ do
    loaded <- try (callKeyFile >>= loadCallKeyAt) :: IO (Either SomeException ByteString)
    case loaded of
        Right key -> pure (CallKey Persistent key)
        Left _ -> CallKey Ephemeral <$> getRandomBytes 32
{-# NOINLINE callKey #-}

data SSHCommand = Call FilePath Remote [Text] ClientOpts
    deriving (Show)

sshRun :: Command "ssh" SSHCommand
sshRun = Command $ \(Call path rem args opts) ->
    proc
        "ssh"
        ( clientArgs opts
            <> [ Text.unpack (loginAtHost rem)
               , path
               ]
            <> map Text.unpack args
        )

{- | Whether an ssh failure is a host key that no longer matches what the
known-hosts file recorded.

Worth singling out because it is the one ssh failure that never resolves by
waiting, and the one a recipe rebuilding disposable machines at a stable
address produces routinely: same address, new host. See
"Salmon.Builtin.Nodes.Gcp.SshAccess".@sshAvailable@, which forgets the stale
entry and retries rather than spending its whole probe budget on it.
-}
isHostKeyMismatch :: Text -> Bool
isHostKeyMismatch err =
    "REMOTE HOST IDENTIFICATION HAS CHANGED" `Text.isInfixOf` err
        || "Host key verification failed" `Text.isInfixOf` err

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser, "@", rem.remoteHost]

preExistingRemoteMachine :: Track' Remote
preExistingRemoteMachine = Track $ \r -> placeholder "remote" ("a remote at" <> r.remoteHost)
