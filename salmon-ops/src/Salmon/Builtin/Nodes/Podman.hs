module Salmon.Builtin.Nodes.Podman where

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), CommandIO (..), checkExitCode, withBinary, withBinaryIO)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import GHC.IO.Exception (ExitCode (..))
import GHC.IO.Handle (Handle, hClose)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process (StdStream (CreatePipe), waitForProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = PullImage !Registry !Image !Binary.Report
    | BuildImage !FilePath !TagName !Binary.Report
    | PushImage !(Maybe AuthFile) !TagName !Binary.Report
    | LoginRegistry !AuthFile !Registry !Username
    | LogoutRegistry !AuthFile !Registry !Binary.Report
    | RunContainer !Registry !Image !ContainerName !RunOptions !Binary.Report
    | CreateNetwork !NetworkName !Binary.Report
    | RemoveImage !Registry !Image !Binary.Report
    | RemoveBuiltImage !TagName !Binary.Report
    | RemoveContainer !ContainerName !Binary.Report
    | RemoveNetwork !NetworkName !Binary.Report
    -- todo: prune volumes, import for bootstrap
    deriving (Show)

-------------------------------------------------------------------------------
newtype Registry = Registry {getRegistry :: Text}
    deriving (Eq, Ord, Show)

newtype Image = Image {getImage :: Text}
    deriving (Eq, Ord, Show)

type TagName = Text

{- | An explicit @--authfile@ path (podman's isolated credential store,
distinct from Docker's @~\/.docker\/config.json@ and podman's own default
@\$XDG_RUNTIME_DIR\/containers\/auth.json@).

The whole point of threading one of these through explicitly, rather than
letting 'login'\/'push'\/'pullImage' fall back to the ambient default: two
salmon processes on the same user authenticating to the __same__ registry
under __different__ credentials (e.g. two tenants each with their own
Artifact Registry push token) would otherwise clobber each other's login by
sharing one global credential file. Pointing each process at its own
'AuthFile' path makes that impossible by construction — there is no shared
mutable state left to race on.
-}
newtype AuthFile = AuthFile {getAuthFile :: FilePath}
    deriving (Eq, Ord, Show)

-- | The username 'login' authenticates as (e.g. @oauth2accesstoken@ for GCP
-- Artifact Registry, where the password is a short-lived access token).
newtype Username = Username {getUsername :: Text}
    deriving (Eq, Ord, Show)

-- | A container needs a stable, caller-chosen identity: podman assigns a
-- random name otherwise, which would leave 'down' with nothing to target.
newtype ContainerName = ContainerName {getContainerName :: Text}
    deriving (Eq, Ord, Show)

{- | A user-defined podman network — needed for containers to resolve each
other by name (podman's implicit default network doesn't reliably do this,
at least in rootless mode; a network created via @podman network create@
does, via embedded DNS).
-}
newtype NetworkName = NetworkName {getNetworkName :: Text}
    deriving (Eq, Ord, Show)

type PortSpec = Text

data PortProtocol
    = TCPPort
    | UDPPort
    deriving (Eq, Ord, Show)

data PortMapping
    = PortMapping
    { portOnHost :: PortSpec
    , portInGuest :: PortSpec
    , portProtocol :: PortProtocol
    }
    deriving (Eq, Ord, Show)

-- | A container environment variable, e.g. for a connstring or a secret path.
type EnvVar = (Text, Text)

data VolumeMode
    = ReadOnly
    | ReadWrite
    deriving (Eq, Ord, Show)

data VolumeMount
    = VolumeMount
    { volumeHostPath :: FilePath
    , volumeGuestPath :: FilePath
    , volumeMode :: VolumeMode
    }
    deriving (Eq, Ord, Show)

{- | Everything besides image/name needed to start a container: published
ports, env vars (config/secrets), bind-mounted volumes, and an optional
podman network to join. 'noRunOptions' is the empty starting point.
-}
data RunOptions
    = RunOptions
    { runPorts :: [PortMapping]
    , runEnv :: [EnvVar]
    , runVolumes :: [VolumeMount]
    , runNetwork :: Maybe Text
    }
    deriving (Eq, Ord, Show)

noRunOptions :: RunOptions
noRunOptions = RunOptions [] [] [] Nothing

-------------------------------------------------------------------------------
pullImage :: Reporter Report -> Track' (Binary "podman") -> Registry -> Image -> Op
pullImage r podman reg img =
    withBinary podman podmanCommand (Pull reg img) $ \pull ->
        op "podman-pull" (deps []) $ \actions ->
            actions
                { help = "pulls a podman image"
                , ref = mkRef "podman-pull" (getRegistry reg, getImage img)
                , up = pull r'
                , down = Binary.untrackedExec podmanCommand (Rmi reg img) "" r''
                }
  where
    r' = contramap (PullImage reg img) r
    r'' = contramap (RemoveImage reg img) r

buildImage :: Reporter Report -> Track' (Binary "podman") -> FS.File "containerfile" -> TagName -> Op
buildImage r podman containerfile tagname =
    FS.withFile containerfile $ \containerfilepath ->
        withBinary podman podmanCommand (Build containerfilepath tagname) $ \build ->
            op "podman-build" (deps []) $ \actions ->
                actions
                    { help = "builds a podman image in container path and tag it"
                    , ref = mkRef "podman-build" tagname
                    , up = build (r' containerfilepath)
                    , down = Binary.untrackedExec podmanCommand (RmiTag tagname) "" r''
                    }
  where
    r' containerfilepath = contramap (BuildImage containerfilepath tagname) r
    r'' = contramap (RemoveBuiltImage tagname) r

{- | Logs in to a registry, writing credentials to an explicit 'AuthFile'
rather than the ambient default (see 'AuthFile'’s own note on why that
isolation matters).

The password is read as @IO Text@ rather than a plain 'Text' so a
short-lived, freshly-fetched credential (a GCP access token, say) can be
obtained right when 'up' runs rather than baked into the graph when it was
built — and it is piped over stdin via @--password-stdin@, never passed as
a CLI argument, so it never shows up in @ps@ output or a process-start log
line.

There is deliberately no 'check': whether the credential already in
'AuthFile' is still valid is not answerable without hitting the registry
(and for a short-lived token, "still in the file" and "still valid" are
different questions anyway), so — like 'push' — this defaults to
'Salmon.Actions.UpDown.Immaterial' and simply re-authenticates on every
'up'. 'down' runs @podman logout --authfile@ against the same file and then removes
the file, which logout itself leaves behind (emptied).
-}
login :: Reporter Report -> Track' (Binary "podman") -> AuthFile -> Registry -> Username -> IO Text -> Op
login r podman authfile reg user getPassword =
    withBinaryIO podman logincommand (LoginCmd authfile reg user) $ \mkProc ->
        op "podman-login" (deps [enclosingdir]) $ \actions ->
            actions
                { help = Text.unwords ["logs in to", getRegistry reg, "via", Text.pack (getAuthFile authfile)]
                , ref = mkRef "podman-login" (getAuthFile authfile, getRegistry reg, getUsername user)
                , up = do
                    runReporter r (LoginRegistry authfile reg user)
                    pw <- getPassword
                    (mStdin, _, _, ph) <- mkProc ()
                    case mStdin of
                        Just hin -> Text.hPutStr hin pw >> hClose hin
                        Nothing -> pure ()
                    waitForProcess ph >>= checkExitCode "podman login"
                , down = do
                    Binary.untrackedExec podmanCommand (Logout authfile reg) "" r''
                    -- `podman logout` empties the file's credentials but
                    -- leaves the file itself ({"auths":{}}), which then keeps
                    -- the enclosing directory from being removed when *it*
                    -- goes down. This node is what caused the file to exist,
                    -- so this node removes it.
                    exists <- doesFileExist (getAuthFile authfile)
                    when exists (removeFile (getAuthFile authfile))
                }
  where
    r'' = contramap (LogoutRegistry authfile reg) r
    enclosingdir = FS.dir (FS.Directory (takeDirectory (getAuthFile authfile)))

{- | Pushes a locally-tagged image to whatever registry its tag names.

Deliberately takes only the one 'TagName' rather than a separate
local-tag\/remote-ref pair: the simplest way to make @podman build@,
@podman push@, and a downstream consumer (e.g.
"Salmon.Builtin.Nodes.Gcp.CloudRun"'s @crsImage@) agree on what image is
meant is to tag the build with the fully-qualified remote reference (e.g.
@us-docker.pkg.dev\/project\/repo\/image:tag@) in the first place — see
'buildImage' — rather than push introducing a second name for the same
thing. The optional 'AuthFile' should be the same one passed to 'login';
'Nothing' falls back to podman's ambient default, which is fine for a
public registry but defeats the isolation 'login'\/'AuthFile' exist for.

There is no 'check': whether a remote registry already has the bytes this
tag would push is not answerable any cheaper than pushing, so like
'buildImage' this defaults to 'Salmon.Actions.UpDown.Immaterial'. 'down' is
a no-op — @podman@ has no "unpush", and deleting a remote artifact is a
registry-side operation (e.g. `Gcp.ArtifactRegistry`), not a podman one.
-}
push :: Reporter Report -> Track' (Binary "podman") -> Maybe AuthFile -> TagName -> Op
push r podman mAuthFile tagname =
    withBinary podman podmanCommand (Push mAuthFile tagname) $ \doPush ->
        op "podman-push" (deps []) $ \actions ->
            actions
                { help = "pushes " <> tagname <> " to its registry"
                , ref = mkRef "podman-push" (tagname, fmap getAuthFile mAuthFile)
                , up = doPush r'
                }
  where
    r' = contramap (PushImage mAuthFile tagname) r

-- | Runs a detached container under a caller-chosen 'ContainerName' (so
-- 'down' has a stable target to remove), with the given ports/env/volumes/network.
runContainer :: Reporter Report -> Track' (Binary "podman") -> Registry -> Image -> ContainerName -> RunOptions -> Op
runContainer r podman reg img cname opts =
    withBinary podman podmanCommand (Run reg img cname opts) $ \run ->
        op "podman-run" (deps []) $ \actions ->
            actions
                { help = "runs a podman container"
                , ref = mkRef "podman-run" (getContainerName cname)
                , up = run r'
                , down = Binary.untrackedExec podmanCommand (Rm cname) "" r''
                }
  where
    r' = contramap (RunContainer reg img cname opts) r
    r'' = contramap (RemoveContainer cname) r

-- | Creates a user-defined podman network under a caller-chosen 'NetworkName'
-- (idempotent: skipped via 'check' if @podman network exists@ already says yes,
-- since @podman network create@ itself errors on a duplicate name).
network :: Reporter Report -> Track' (Binary "podman") -> NetworkName -> Op
network r podman name =
    withBinary podman podmanCommand (CreateNetworkCmd name) $ \create ->
        op "podman-network" (deps []) $ \actions ->
            actions
                { help = "creates a podman network"
                , ref = mkRef "podman-network" (getNetworkName name)
                , check = skipIfNetworkExists name
                , up = create r'
                , down = Binary.untrackedExec podmanCommand (RemoveNetworkCmd name) "" r''
                }
  where
    r' = contramap (CreateNetwork name) r
    r'' = contramap (RemoveNetwork name) r

skipIfNetworkExists :: NetworkName -> IO CheckResult
skipIfNetworkExists name = do
    (code, _, _) <- readCreateProcessWithExitCode (proc "podman" ["network", "exists", Text.unpack (getNetworkName name)]) ""
    pure $ case code of
        ExitSuccess -> Success
        _ -> Failure ("no such podman network: " <> getNetworkName name)

-------------------------------------------------------------------------------
data PodmanCommand
    = Pull !Registry !Image
    | Run !Registry !Image !ContainerName !RunOptions
    | Build !FilePath !TagName
    | Push !(Maybe AuthFile) !TagName
    | Logout !AuthFile !Registry
    | CreateNetworkCmd !NetworkName
    | Rmi !Registry !Image
    | RmiTag !TagName
    | Rm !ContainerName
    | RemoveNetworkCmd !NetworkName

podmanCommand :: Command "podman" PodmanCommand
podmanCommand = Command $ \cmd -> case cmd of
    (Pull r i) ->
        proc
            "podman"
            [ "pull"
            , (Text.unpack $ getRegistry r) </> (Text.unpack $ getImage i)
            ]
    (Build fullpath tagname) ->
        ( proc
            "podman"
            [ "build"
            , "-t"
            , Text.unpack tagname
            , "-f"
            , takeFileName fullpath
            ]
        )
            { cwd = Just $ takeDirectory fullpath
            }
    (Push mAuthFile tagname) ->
        proc "podman" $
            -- --authfile is a flag of `podman push`, not a global option:
            -- podman rejects it before the subcommand with "unknown flag".
            ["push"]
                <> maybe [] (\af -> ["--authfile", getAuthFile af]) mAuthFile
                <> [Text.unpack tagname]
    (Logout authfile reg) ->
        proc "podman" ["logout", "--authfile", getAuthFile authfile, Text.unpack (getRegistry reg)]
    (Run r i cname opts) ->
        proc "podman" $
            mconcat
                [
                    [ "run"
                    , "-dt"
                    , "--name"
                    , Text.unpack (getContainerName cname)
                    ]
                , concatMap portArgs opts.runPorts
                , concatMap envArgs opts.runEnv
                , concatMap volumeArgs opts.runVolumes
                , maybe [] (\net -> ["--network", Text.unpack net]) opts.runNetwork
                , [(Text.unpack $ getRegistry r) </> (Text.unpack $ getImage i)]
                ]
      where
        portArgs :: PortMapping -> [String]
        portArgs pm =
            let
                proto = case pm.portProtocol of
                    TCPPort -> "tcp"
                    UDPPort -> "udp"
             in
                ["-p", mconcat [Text.unpack pm.portOnHost, ":", Text.unpack pm.portInGuest, "/", proto]]
        envArgs :: EnvVar -> [String]
        envArgs (k, v) = ["--env", mconcat [Text.unpack k, "=", Text.unpack v]]
        volumeArgs :: VolumeMount -> [String]
        volumeArgs vol =
            let
                mode = case vol.volumeMode of
                    ReadOnly -> "ro"
                    ReadWrite -> "rw"
             in
                ["-v", mconcat [vol.volumeHostPath, ":", vol.volumeGuestPath, ":", mode]]
    (Rmi r i) ->
        proc
            "podman"
            [ "rmi"
            , (Text.unpack $ getRegistry r) </> (Text.unpack $ getImage i)
            ]
    (CreateNetworkCmd name) ->
        proc "podman" ["network", "create", Text.unpack (getNetworkName name)]
    (RmiTag tagname) ->
        proc "podman" ["rmi", Text.unpack tagname]
    (Rm cname) ->
        proc "podman" ["rm", "-f", Text.unpack (getContainerName cname)]
    (RemoveNetworkCmd name) ->
        proc "podman" ["network", "rm", Text.unpack (getNetworkName name)]

-------------------------------------------------------------------------------

data LoginCommand
    = LoginCmd !AuthFile !Registry !Username

{- | @podman login@'s password has to arrive over stdin
(@--password-stdin@, never a CLI argument -- see 'login'), so this needs a
pipe created before the process starts and written to after, which the
plain 'Command' framework (a fixed 'CreateProcess' plus a fixed stdin
'System.Process.ByteString.ByteString' known up front) has no room for.
@CommandIO@'s @ioarg@ would normally carry caller-supplied handles (see
"Salmon.Builtin.Nodes.WireGuard"), but here there is nothing to redirect
*in* — only a pipe this command itself asks 'System.Process.createProcess'
to allocate — so 'logincommand' ignores its @()@ and 'login' reads the pipe
back out of the 'RunningCommand' tuple 'withBinaryIO' hands it.
-}
logincommand :: CommandIO "podman" LoginCommand ()
logincommand = CommandIO $ \(LoginCmd authfile reg user) () ->
    pure
        ( (proc "podman" ["login", "--authfile", getAuthFile authfile, "--username", Text.unpack (getUsername user), "--password-stdin", Text.unpack (getRegistry reg)])
            { std_in = CreatePipe
            }
        )

-------------------------------------------------------------------------------
-- some builtins

-------------------------------------------------------------------------------
dockerRegistry :: Registry
dockerRegistry = Registry "docker.io"

ubuntuLatest :: Image
ubuntuLatest = Image "ubuntu:latest"
