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
    | RunContainer !Registry !Image !PortMapping !Binary.Report
    -- todo: prune containers, rm images, volumes, import for bootstrap
    deriving (Show)

-------------------------------------------------------------------------------
newtype Registry = Registry {getRegistry :: Text}
    deriving (Eq, Ord, Show)

newtype Image = Image {getImage :: Text}
    deriving (Eq, Ord, Show)

type TagName = Text

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
                , up = pull r'
                }
  where
    r' = contramap (PullImage reg img) r

buildImage :: Reporter Report -> Track' (Binary "podman") -> FS.File "containerfile" -> TagName -> Op
buildImage r podman containerfile tagname =
    FS.withFile containerfile $ \containerfilepath ->
        withBinary podman podmanCommand (Build containerfilepath tagname) $ \build ->
            op "podman-build" (deps []) $ \actions ->
                actions
                    { help = "builds a podman image in container path and tag it"
                    , up = build (r' containerfilepath)
                    }
  where
    r' containerfilepath = contramap (BuildImage containerfilepath tagname) r

runContainer :: Reporter Report -> Track' (Binary "podman") -> Registry -> Image -> PortMapping -> Op
runContainer r podman reg img pm =
    withBinary podman podmanCommand (Run reg img pm) $ \run ->
        op "podman-run" (deps []) $ \actions ->
            actions
                { help = "runs a podman container"
                , up = run r'
                }
  where
    r' = contramap (RunContainer reg img pm) r

-------------------------------------------------------------------------------
data PodmanCommand
    = Pull !Registry !Image
    | Run !Registry !Image !PortMapping
    | Build !FilePath !TagName

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
    (Run r i pm) ->
        let
            port = case pm.portProtocol of
                TCPPort -> "tcp"
                UDPPort -> "udp"
         in
            proc
                "podman"
                [ "run"
                , "-dt"
                , mconcat [Text.unpack pm.portOnHost, ":", Text.unpack pm.portInGuest, "/", port]
                , (Text.unpack $ getRegistry r) </> (Text.unpack $ getImage i)
                ]

-------------------------------------------------------------------------------
-- some builtins

-------------------------------------------------------------------------------
dockerRegistry :: Registry
dockerRegistry = Registry "docker.io"

ubuntuLatest :: Image
ubuntuLatest = Image "ubuntu:latest"
