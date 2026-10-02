{-# LANGUAGE OverloadedStrings #-}

{- | A container as a systemd service, declared as a podman /quadlet/.

"Salmon.Builtin.Nodes.Podman" can pull an image and @podman run@ a container,
and "Salmon.Builtin.Nodes.Systemd" can author a unit, but neither says "this
image runs as a service on this machine": a @podman run -d@ container is
gone at the next reboot, and a hand-written unit around @podman run@ has to
get the cid file, the @--replace@, the notify socket and the stop command
right. Quadlet is podman's own answer: a @NAME.container@ file in a
directory a systemd generator reads, turned into @NAME.service@ on every
@daemon-reload@.

'quadletContainer' is 'Salmon.Builtin.Nodes.Systemd.systemdService' for that
file, and reuses its mechanism rather than adding one:

* the file is written through 'Salmon.Builtin.Nodes.Filesystem.filecontents',
  so identical bytes leave its mtime alone;
* the @check@ is the same @systemctl show@, because systemd answers
  @NeedDaemonReload=yes@ for a generated unit whose /source/ file changed
  (the generator writes @SourcePath=@) -- so a new image reference is a
  changed file, a stale unit, and a reload and restart;
* the env file (and anything else the caller names in 'containerWatched') is
  hashed into a trailing comment, as
  'Salmon.Builtin.Nodes.Systemd.systemdServiceWatching' does, so a changed
  env file is a changed quadlet too.

Three differences from an authored unit, each of which was a surprise first.
A generated unit's @UnitFileState@ is @generated@ and it cannot be
@systemctl enable@d: the @[Install]@ section is honoured by the generator
itself, so @up@ is reload and restart, with no enable. Removing the file does
not remove the unit until the next reload, so the file node's @down@ reloads
after removing. And the image is pulled /by the service's start/, so a slow
pull is a start timeout: see 'containerStartTimeout'.

What "changed" means is "the file changed". An image reference that stays
the same while the registry moves what it points at (@:latest@) is not a
change this node can see; name images by a tag that moves with the content,
or by digest.

Needs podman 4.4 or later (quadlet's first release). Only keys that 4.9
understands are rendered -- the registry credentials go through
@PodmanArgs=--authfile=@ rather than the @AuthFile=@ key, which 4.9's
generator refuses as unsupported.
-}
module Salmon.Builtin.Nodes.Podman.Quadlet (
    Container (..),
    RestartPolicy (..),
    container,
    quadletContainer,
    serviceTarget,
    quadletPath,
    systemQuadletDir,
    renderContainer,
    renderContainerWatching,
    watchedFiles,
    containerProblems,
    interpretShow,
    InvalidContainer (..),
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.FilePath ((</>))

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | The @Restart=@ systemd applies to the generated service.
data RestartPolicy
    = RestartNo
    | RestartOnFailure
    | RestartAlways
    deriving (Eq, Ord, Show)

{- | One container run as a service. 'container' is the starting point; set
the rest with record update.
-}
data Container
    = Container
    { containerScope :: Systemd.Scope
    , containerUnitDir :: FilePath
    -- ^ where the generator looks: 'systemQuadletDir' for 'Systemd.System', a
    -- caller-resolved @~\/.config\/containers\/systemd@ for 'Systemd.User'
    -- (the same "resolve before constructing" rule as
    -- 'Systemd.config_unit_dir')
    , containerName :: Podman.ContainerName
    -- ^ names the file (@NAME.container@), the unit (@NAME.service@) and the
    -- container itself
    , containerImage :: Text
    -- ^ the full reference, registry included
    -- (@europe-west1-docker.pkg.dev\/project\/repo\/app:v3@); podman does not
    -- guess a registry for a short name under systemd
    , containerDescription :: Text
    , containerAfter :: [Systemd.UnitTarget]
    , containerEnvFile :: Maybe FilePath
    -- ^ an @EnvironmentFile=@, read by podman on the host at every start; it
    -- is pre-provisioned by something else and only watched here
    , containerPorts :: [Podman.PortMapping]
    , containerVolumes :: [Podman.VolumeMount]
    , containerNetwork :: Maybe Text
    , containerAuthFile :: Maybe Podman.AuthFile
    -- ^ the credentials the start's pull uses, the file 'Podman.login' wrote
    , containerRestart :: RestartPolicy
    , containerStartTimeout :: Maybe Int
    -- ^ @TimeoutStartSec=@, in seconds. The start includes the pull when the
    -- image is not on the machine yet, and systemd's default (90s) is short
    -- for a large image on a small machine.
    , containerWantedBy :: Maybe Systemd.UnitTarget
    -- ^ what starts it at boot; 'Nothing' for a service that is only ever
    -- started by hand or by salmon
    , containerWatched :: [FilePath]
    -- ^ other host files the container reads at start (a bind-mounted
    -- config), a change to which should restart it
    }
    deriving (Eq, Show)

-- | The directory the system generator reads for administrator-written units.
systemQuadletDir :: FilePath
systemQuadletDir = "/etc/containers/systemd"

{- | A system-scope container restarted on failure and started at boot, with
nothing published and nothing mounted.
-}
container :: Podman.ContainerName -> Text -> Container
container name image =
    Container
        { containerScope = Systemd.System
        , containerUnitDir = systemQuadletDir
        , containerName = name
        , containerImage = image
        , containerDescription = "container " <> Podman.getContainerName name <> " (salmon)"
        , containerAfter = []
        , containerEnvFile = Nothing
        , containerPorts = []
        , containerVolumes = []
        , containerNetwork = Nothing
        , containerAuthFile = Nothing
        , containerRestart = RestartOnFailure
        , containerStartTimeout = Nothing
        , containerWantedBy = Just "multi-user.target"
        , containerWatched = []
        }

-- | The unit the generator makes out of this container's file.
serviceTarget :: Container -> Systemd.UnitTarget
serviceTarget c = Podman.getContainerName c.containerName <> ".service"

quadletPath :: Container -> FilePath
quadletPath c = c.containerUnitDir </> Text.unpack (Podman.getContainerName c.containerName) <> ".container"

-- | The files whose contents are folded into the quadlet: the env file first.
watchedFiles :: Container -> [FilePath]
watchedFiles c = maybeToList c.containerEnvFile <> c.containerWatched

-------------------------------------------------------------------------------

{- | The @.container@ file, without the watched files' fingerprint: everything
the declaration alone decides.
-}
renderContainer :: Container -> Text
renderContainer c =
    Text.unlines $
        mconcat
            [ ["[Unit]", "Description=" <> c.containerDescription]
            , ["After=" <> Text.unwords c.containerAfter | not (null c.containerAfter)]
            , ["", "[Container]"]
            , ["ContainerName=" <> Podman.getContainerName c.containerName]
            , ["Image=" <> c.containerImage]
            , ["EnvironmentFile=" <> Text.pack f | f <- maybeToList c.containerEnvFile]
            , ["PublishPort=" <> port p | p <- c.containerPorts]
            , ["Volume=" <> volume v | v <- c.containerVolumes]
            , ["Network=" <> n | n <- maybeToList c.containerNetwork]
            , ["PodmanArgs=--authfile=" <> Text.pack (Podman.getAuthFile a) | a <- maybeToList c.containerAuthFile]
            , ["", "[Service]", "Restart=" <> restart c.containerRestart]
            , ["TimeoutStartSec=" <> Text.pack (show t) | t <- maybeToList c.containerStartTimeout]
            , concat [["", "[Install]", "WantedBy=" <> w] | w <- maybeToList c.containerWantedBy]
            ]
  where
    port :: Podman.PortMapping -> Text
    port p =
        mconcat
            [ p.portOnHost
            , ":"
            , p.portInGuest
            , "/"
            , case p.portProtocol of
                Podman.TCPPort -> "tcp"
                Podman.UDPPort -> "udp"
            ]

    volume :: Podman.VolumeMount -> Text
    volume v =
        mconcat
            [ Text.pack v.volumeHostPath
            , ":"
            , Text.pack v.volumeGuestPath
            , ":"
            , case v.volumeMode of
                Podman.ReadOnly -> "ro"
                Podman.ReadWrite -> "rw"
            ]

    restart :: RestartPolicy -> Text
    restart RestartNo = "no"
    restart RestartOnFailure = "on-failure"
    restart RestartAlways = "always"

{- | 'renderContainer' plus the fingerprint of the watched files, which is what
is written. With nothing watched it is 'renderContainer' exactly.
-}
renderContainerWatching :: Container -> IO Text
renderContainerWatching c = case watchedFiles c of
    [] -> pure (renderContainer c)
    files -> Systemd.withWatchedFingerprint files (renderContainer c)

{- | Why this container cannot be written, if it cannot.

Every value lands on a line of its own in a file systemd parses, so a line
break inside one is another key: an image reference read from somewhere that
ends in a newline would otherwise write a quadlet that says something the
declaration does not. The name is also a file and a unit name.
-}
containerProblems :: Container -> [Text]
containerProblems c =
    mconcat
        [ ["the container has no name" | Text.null name]
        , ["the container name is not a unit name: " <> name | Text.any (`elem` ("/ \t" :: String)) name]
        , ["the container has no image" | Text.null (Text.strip c.containerImage)]
        , ["a line break in the " <> what | (what, value) <- fields, Text.any (`elem` ("\n\r" :: String)) value]
        ]
  where
    name = Podman.getContainerName c.containerName
    fields :: [(Text, Text)]
    fields =
        mconcat
            [ [("name", name), ("image", c.containerImage), ("description", c.containerDescription)]
            , [("after", a) | a <- c.containerAfter]
            , [("env file", Text.pack f) | f <- maybeToList c.containerEnvFile]
            , [("published port", p.portOnHost <> p.portInGuest) | p <- c.containerPorts]
            , [("volume", Text.pack (v.volumeHostPath <> v.volumeGuestPath)) | v <- c.containerVolumes]
            , [("network", n) | n <- maybeToList c.containerNetwork]
            , [("auth file", Text.pack (Podman.getAuthFile a)) | a <- maybeToList c.containerAuthFile]
            , [("wanted-by", w) | w <- maybeToList c.containerWantedBy]
            ]

data InvalidContainer = InvalidContainer !FilePath ![Text]
    deriving (Show)

instance Exception InvalidContainer

-------------------------------------------------------------------------------

{- | Installs the quadlet and keeps its service running as written.

The 'Track'' is where the caller says what the container stands on: podman
itself, the 'Podman.login' whose 'Podman.AuthFile' the pull reads, whatever
delivers the env file. They are applied before the file is written.

@up@ is @daemon-reload@ then @restart@, which returns once the container is
running (the generated service is @Type=notify@) and therefore /includes the
pull/ the first time an image is used; a failed pull is a failed @up@. @down@
stops the service, which removes the container, and the file's own @down@
removes the file and reloads so that the generated unit goes with it. The
image is left on the machine.
-}
quadletContainer ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Container ->
    Container ->
    Op
quadletContainer r systemctl t c =
    withCommand (Systemd.DaemonReload c.containerScope) $ \reload ->
        withCommand (Systemd.Up c.containerScope target) $ \restart ->
            withCommand (Systemd.Stop c.containerScope target) $ \stop ->
                op "podman-quadlet" (deps [quadletFile, run t c]) $ \actions ->
                    actions
                        { help = "runs " <> c.containerImage <> " as " <> target
                        , notes =
                            [ "image: " <> c.containerImage
                            , "quadlet: " <> declared
                            ]
                        , ref = mkRef "systemd-unit" target
                        , check = Systemd.checkUnit interpretShow c.containerScope target
                        , up = reload >> restart
                        , down = stop
                        }
  where
    target = serviceTarget c
    path = quadletPath c
    r' cmd = contramap (Systemd.CallSystemCtl cmd) r

    withCommand cmd f =
        let
            g :: (Reporter Binary.Report -> IO ()) -> Op
            g callbin = f (callbin (r' cmd))
         in
            withBinary systemctl Systemd.callSystemctl cmd g

    -- What the declaration alone says the file is. The file node's own
    -- contents are an @IO Text@ once anything is watched, which has no
    -- content fingerprint, so without this a re-declaration under @serve@
    -- that only changes the image would leave both nodes looking unchanged.
    declared :: Text
    declared = FS.hashBytes (Text.encodeUtf8 (renderContainer c))

    quadletFile :: Op
    quadletFile =
        fmap (fmap ownFile) (FS.filecontents (FS.FileContents path (renderContainerWatching c)))

    ownFile :: Extension -> Extension
    ownFile ext =
        ext
            { notes = ext.notes <> ["quadlet: " <> declared]
            , up = do
                let problems = containerProblems c
                unless (null problems) $ throwIO (InvalidContainer path problems)
                ext.up
            , -- the generated unit outlives its source file until the next
              -- reload, and nothing else in this graph would run one.
              down = do
                ext.down
                Binary.untrackedExec
                    Systemd.callSystemctl
                    (Systemd.DaemonReload c.containerScope)
                    ""
                    (r' (Systemd.DaemonReload c.containerScope))
            }

{- | 'Systemd.interpretShow' for a generated unit: @generated@ is the only
install state such a unit ever has. Everything else -- a changed source file,
a transition being 'Unknown', a stopped unit -- reads as it does there.
-}
interpretShow :: [Text] -> CheckResult
interpretShow = Systemd.interpretShowAccepting ("generated" : Systemd.installedStates)
