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
callWith opts r ssh tRemote remote remotepath args stdin =
    withBinaryStdin ssh sshRun cmd stdin $ \up ->
        op "ssh:call" (deps [run tRemote remote]) $ \actions ->
            actions
                { help = "calls " <> Text.pack remotepath <> " on " <> remote.remoteHost <> " with args " <> Text.intercalate " " args <> " and stdin " <> Text.decodeUtf8 stdin
                , notes = Text.pack remotepath : args
                , ref = mkRef "ssh-run" (show (remotepath, remote, args, stdin))
                , up = up r'
                }
  where
    cmd = Call remotepath remote args opts
    r' = contramap (RunSSHCommand cmd) r

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
