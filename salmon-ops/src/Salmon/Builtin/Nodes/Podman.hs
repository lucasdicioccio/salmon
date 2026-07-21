module Salmon.Builtin.Nodes.Podman where

import Salmon.Actions.UpDown (Requirement (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.IO.Exception (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = PullImage !Registry !Image !Binary.Report
    | BuildImage !FilePath !TagName !Binary.Report
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
-- (idempotent: skipped via 'prelim' if @podman network exists@ already says yes,
-- since @podman network create@ itself errors on a duplicate name).
network :: Reporter Report -> Track' (Binary "podman") -> NetworkName -> Op
network r podman name =
    withBinary podman podmanCommand (CreateNetworkCmd name) $ \create ->
        op "podman-network" (deps []) $ \actions ->
            actions
                { help = "creates a podman network"
                , ref = mkRef "podman-network" (getNetworkName name)
                , prelim = skipIfNetworkExists name
                , up = create r'
                , down = Binary.untrackedExec podmanCommand (RemoveNetworkCmd name) "" r''
                }
  where
    r' = contramap (CreateNetwork name) r
    r'' = contramap (RemoveNetwork name) r

skipIfNetworkExists :: NetworkName -> IO Requirement
skipIfNetworkExists name = do
    (code, _, _) <- readCreateProcessWithExitCode (proc "podman" ["network", "exists", Text.unpack (getNetworkName name)]) ""
    pure $ case code of
        ExitSuccess -> Skippable
        _ -> Required

-------------------------------------------------------------------------------
data PodmanCommand
    = Pull !Registry !Image
    | Run !Registry !Image !ContainerName !RunOptions
    | Build !FilePath !TagName
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
-- some builtins

-------------------------------------------------------------------------------
dockerRegistry :: Registry
dockerRegistry = Registry "docker.io"

ubuntuLatest :: Image
ubuntuLatest = Image "ubuntu:latest"
