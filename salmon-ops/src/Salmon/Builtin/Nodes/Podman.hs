module Salmon.Builtin.Nodes.Podman where

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

import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = PullImage !Registry !Image !Binary.Report
    | BuildImage !FilePath !TagName !Binary.Report
    | RunContainer !Registry !Image !ContainerName !PortMapping !Binary.Report
    | RemoveImage !Registry !Image !Binary.Report
    | RemoveBuiltImage !TagName !Binary.Report
    | RemoveContainer !ContainerName !Binary.Report
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
-- 'down' has a stable target to remove), publishing a single port mapping.
runContainer :: Reporter Report -> Track' (Binary "podman") -> Registry -> Image -> ContainerName -> PortMapping -> Op
runContainer r podman reg img cname pm =
    withBinary podman podmanCommand (Run reg img cname pm) $ \run ->
        op "podman-run" (deps []) $ \actions ->
            actions
                { help = "runs a podman container"
                , ref = mkRef "podman-run" (getContainerName cname)
                , up = run r'
                , down = Binary.untrackedExec podmanCommand (Rm cname) "" r''
                }
  where
    r' = contramap (RunContainer reg img cname pm) r
    r'' = contramap (RemoveContainer cname) r

-------------------------------------------------------------------------------
data PodmanCommand
    = Pull !Registry !Image
    | Run !Registry !Image !ContainerName !PortMapping
    | Build !FilePath !TagName
    | Rmi !Registry !Image
    | RmiTag !TagName
    | Rm !ContainerName

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
    (Run r i cname pm) ->
        let
            port = case pm.portProtocol of
                TCPPort -> "tcp"
                UDPPort -> "udp"
         in
            proc
                "podman"
                [ "run"
                , "-dt"
                , "--name"
                , Text.unpack (getContainerName cname)
                , "-p"
                , mconcat [Text.unpack pm.portOnHost, ":", Text.unpack pm.portInGuest, "/", port]
                , (Text.unpack $ getRegistry r) </> (Text.unpack $ getImage i)
                ]
    (Rmi r i) ->
        proc
            "podman"
            [ "rmi"
            , (Text.unpack $ getRegistry r) </> (Text.unpack $ getImage i)
            ]
    (RmiTag tagname) ->
        proc "podman" ["rmi", Text.unpack tagname]
    (Rm cname) ->
        proc "podman" ["rm", "-f", Text.unpack (getContainerName cname)]

-------------------------------------------------------------------------------
-- some builtins

-------------------------------------------------------------------------------
dockerRegistry :: Registry
dockerRegistry = Registry "docker.io"

ubuntuLatest :: Image
ubuntuLatest = Image "ubuntu:latest"
