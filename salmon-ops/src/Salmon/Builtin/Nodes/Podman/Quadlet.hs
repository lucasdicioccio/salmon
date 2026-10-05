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
* the @check@ starts with the same @systemctl show@, because systemd answers
  @NeedDaemonReload=yes@ for a generated unit whose /source/ file changed
  (the generator writes @SourcePath=@) -- so a new image reference is a
  changed file, a stale unit, and a reload and restart;
* and then asks the /running container/ which quadlet it was started from
  (see 'quadletLabel'), because @daemon-reload@ is machine-wide: once
  anything else has run one, systemd no longer remembers that this file
  changed, and a unit still running the old container reads as current;
* the env file (and anything else the caller names in 'containerWatched') is
  hashed into a trailing comment, as
  'Salmon.Builtin.Nodes.Systemd.systemdServiceWatching' does, so a changed
  env file is a changed quadlet too. Unlike there, the hash is /keyed/ (see
  "The watched files' digest" below).

Three differences from an authored unit, each of which was a surprise first.
A generated unit's @UnitFileState@ is @generated@ and it cannot be
@systemctl enable@d: the @[Install]@ section is honoured by the generator
itself, so @up@ is reload and restart, with no enable. Removing the file does
not remove the unit until the next reload, so the file node's @down@ reloads
after removing. And an image that is not on the machine is pulled /by the
service's start/ -- which, on a restart, is after the old container was
stopped. So 'quadletContainer' pulls it first, as a node of its own that the
file stands on ('imageNode'): an image that cannot be pulled fails there,
with the old file, the old unit and the old container untouched.

What "changed" means is "the declaration or a watched file changed". An image reference that stays
the same while the registry moves what it points at (@:latest@) is not a
change this node can see; name images by a tag that moves with the content,
or by digest.

= The watched files' digest

The quadlet file is world-readable, the unit generated from it
(@\/run\/systemd\/generator@) is too, and @systemctl show@ prints its
@ExecStart=@, label included, to any local user. The env file is usually
@0600@ and holds secrets. A plain hash of it in any of those places lets
whoever reads them check guesses at its contents offline, which is all it
takes when the only unknown in the file is a short or human-chosen value. It
is the reason "Salmon.Builtin.Nodes.SecretDelivery" puts no digest of a
secret anywhere.

So the trailing comment is an HMAC-SHA256 of the watched files under a key
only the owner of the quadlet directory can read ('watchKeyPath', made once
by 'watchKeyNode', 32 random bytes, @0600@), and 'quadletFingerprint', which
hashes that comment in, says nothing either without the key. The change
detection is what it was: same files, same key, same line. The key never
leaves the machine and is never removed by @down@, since a new key is a new
fingerprint for every quadlet in the directory. A declaration that watches
nothing has no secret to digest, needs no key, and is the file and the nodes
it always was.

Containers started before the key existed carry a label made from the plain
hash ('unkeyedFingerprint'). They are not restarted for it: the check accepts
that label as long as it is the one the /current/ declaration and files would
have had, and @up@, finding the file rewritten for the key alone, reloads
without restarting ('interpretAdoptable'). The old label stays on that
container, where only its owner's @podman inspect@ reads it, until its next
restart for a reason of its own.

= Jobs

A container can also be a /job/: a command run in the image to completion
('containerExec', 'containerLifetime', and 'containerJob' to start from).
Its service is @Type=oneshot@, for which the generator runs the container in
the foreground, so starting the unit returns when the command has exited and
fails if it failed. 'quadletJob' is the node for it and differs from
'quadletContainer' as 'Salmon.Builtin.Nodes.Systemd.Job.jobService' differs
from a service: it installs the unit and starts nothing. A
'Salmon.Builtin.Nodes.Systemd.Job.timerUnit' naming 'serviceTarget' schedules
it, a 'Salmon.Builtin.Nodes.Systemd.Job.runJob' runs it now.

Whether its last run /succeeded/ is
'Salmon.Builtin.Nodes.Systemd.Job.completedRun''s question, asked of
'containerCompletion'; 'completedQuadletJob' is the two together. systemd
forgets a successful run of a unit nothing keeps loaded, a generated one
included, so a container job with no timer waiting on it is only skipped by
the next pass when it leaves a stamp ('containerStamp').

= Readiness

The generated service is @Type=notify@ over @podman run --sdnotify=conmon@:
systemd is told "started" when the container /exists/, not when what is in it
works. So @systemctl restart@ returns 0 for a container whose entrypoint
exits a moment later, and a pass reported such a node done while systemd was
still restarting it into @failed@. 'containerReady' is the opt-in answer, and
it lives in @up@ alone: after the restart, 'awaitReady' waits for a 'Probe'
to succeed (if one is declared) and then for the unit to stay @active@ with
no restart for 'readyHold' seconds, and @up@ throws 'NotReady' otherwise.
Nothing of it is rendered, so declaring it (or not) leaves the file, the
fingerprint and the running container alone.

= Bind volumes

'containerVolumes' renders a @Volume=@ line and nothing else: the host path
is the caller's, existing before the start, and whoever the image makes its
owner owns it afterwards. 'containerBinds' is the declaration that says more
('Bind', 'bind' to start from): the options podman takes after the access
mode (@:U@, @:z@, @:Z@), and a host /directory/ this module creates
('hostDirNode') ahead of the quadlet file, with an owner and a mode if any
are stated. @down@ never removes such a directory: it holds what the
container wrote. A declaration with no binds is the file, the fingerprint and
the nodes it always was.

Needs podman 4.4 or later (quadlet's first release). Only keys that 4.9
understands are rendered -- the registry credentials go through
@PodmanArgs=--authfile=@ rather than the @AuthFile=@ key, which 4.9's
generator refuses as unsupported.
-}
module Salmon.Builtin.Nodes.Podman.Quadlet (
    Container (..),
    Bind (..),
    Relabel (..),
    HostDir (..),
    bind,
    hostDir,
    renderBind,
    bindProblems,
    hostDirNode,
    hostDirNodes,
    chownArgs,
    interpretHostDir,
    HostDirFailed (..),
    RestartPolicy (..),
    Lifetime (..),
    Readiness (..),
    Probe (..),
    stillUp,
    readyWhen,
    describeReadiness,
    UnitSample (..),
    parseSample,
    sampleArgs,
    interpretStanding,
    Waiting (..),
    awaitReady,
    NotReady (..),
    container,
    containerJob,
    quadletContainer,
    quadletJob,
    containerCompletion,
    completedQuadletJob,
    serviceTarget,
    quadletPath,
    systemQuadletDir,
    renderContainer,
    renderContainerWatching,
    renderQuadlet,
    quadletFingerprint,
    unkeyedFingerprint,
    quadletLabel,
    watchKeyPath,
    watchKeyNode,
    ensureWatchKey,
    interpretWatchKey,
    keyedWatchLine,
    WatchKeyUnusable (..),
    watchedFiles,
    containerProblems,
    interpretShow,
    interpretRunning,
    interpretStarted,
    interpretAdoptable,
    checkJobInstalled,
    interpretGenerated,
    imageNode,
    imagePresentArgs,
    imagePullArgs,
    interpretImagePresent,
    ImageUnavailable (..),
    checkContainer,
    InvalidContainer (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, IOException, SomeException, bracket, finally, onException, throwIO, try)
import Control.Monad (unless, when)
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Random (getRandomBytes)
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import Data.Char (isOctDigit, isSpace)
import Data.List (find)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (readOct)
import qualified Network.Socket as Net
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, removeDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, (</>))
import System.IO (hClose)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Files (createLink, fileMode, fileSize, getFileStatus, setFileMode)
import System.Posix.IO (createFile, fdToHandle)
import System.Posix.Types (FileMode)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.OpGraph (OpGraph (..), inject)
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

-- | Whether the container is expected to stay or to finish.
data Lifetime
    = -- | a service: started, kept running
      LongRunning
    | -- | a job: @Type=oneshot@, the unit is done when the command exits
      RunToCompletion
    deriving (Eq, Ord, Show)

{- | A question asked from the machine salmon runs on, as whoever runs it,
whose answer "yes" means the service does its job.
-}
data Probe
    = -- | a TCP connection to this host and port is accepted (a published port)
      ProbeTcp Text Int
    | -- | this command exits 0. It is run on the /host/; one asking inside
      -- the container is @ProbeCommand "podman" ["exec", NAME, ...]@
      ProbeCommand FilePath [Text]
    deriving (Eq, Ord, Show)

{- | What 'quadletContainer''s @up@ waits for after the restart, before it
calls the container up. See 'awaitReady' for the order of things.
-}
data Readiness
    = Readiness
    { readyProbe :: Maybe Probe
    , readyTimeout :: Int
    -- ^ seconds the probe has to succeed for the first time, counted from
    -- the restart returning. Unused without a probe.
    , readyHold :: Int
    -- ^ seconds the unit must then stay @active@ with no restart by
    -- systemd. A container that dies at boot is restarted within a second
    -- or so under @Restart=on-failure@; a few seconds sees it.
    }
    deriving (Eq, Ord, Show)

-- | No probe: the unit stays @active@, unrestarted, for this many seconds.
stillUp :: Int -> Readiness
stillUp hold = Readiness{readyProbe = Nothing, readyTimeout = 0, readyHold = hold}

{- | This probe succeeds within the timeout (seconds), and the unit is still
standing, unrestarted, three seconds later.
-}
readyWhen :: Probe -> Int -> Readiness
readyWhen probe seconds = Readiness{readyProbe = Just probe, readyTimeout = seconds, readyHold = 3}

-- | One line for the node's @notes@, and the wording of a failed wait.
describeReadiness :: Readiness -> Text
describeReadiness rd =
    Text.intercalate ", then " $
        [describeProbe p <> " within " <> seconds rd.readyTimeout | p <- maybeToList rd.readyProbe]
            <> ["active with no restart for " <> seconds rd.readyHold]
  where
    seconds n = Text.pack (show n) <> "s"

describeProbe :: Probe -> Text
describeProbe (ProbeTcp host port) = "tcp " <> host <> ":" <> Text.pack (show port) <> " accepts"
describeProbe (ProbeCommand cmd args) = "`" <> Text.unwords (Text.pack cmd : args) <> "` exits 0"

-- | The SELinux relabelling podman does on a bind's host path.
data Relabel
    = -- | @:z@, a label several containers can share
      RelabelShared
    | -- | @:Z@, a label private to this container
      RelabelPrivate
    deriving (Eq, Ord, Show)

{- | A host directory this module creates for a 'Bind'. Owner and mode are
the directory's /at creation/: one that already exists is left exactly as it
is, whoever owns it. An image's entrypoint commonly takes its data directory
for its own user at first start (postgres does), and a pass that set the
declared owner back would pull the directory from under a running server.
-}
data HostDir
    = HostDir
    { hostDirOwner :: Maybe Text
    -- ^ what @chown@ is given: @USER@, @USER:GROUP@, or the numeric forms.
    -- Naming anyone but oneself needs root, so in 'Systemd.User' scope it
    -- fails the node, and the directory is not left behind half-made.
    , hostDirMode :: Maybe Text
    -- ^ octal, three or four digits (@0750@). 'Nothing' is whatever the
    -- umask of the salmon process gives.
    }
    deriving (Eq, Ord, Show)

-- | A directory created with no owner and no mode stated.
hostDir :: HostDir
hostDir = HostDir{hostDirOwner = Nothing, hostDirMode = Nothing}

{- | A host directory mounted in the container, with what 'Podman.VolumeMount'
cannot say. Rendered as one more @Volume=@ line, after 'containerVolumes'.
-}
data Bind
    = Bind
    { bindHostPath :: FilePath
    , bindGuestPath :: FilePath
    , bindMode :: Podman.VolumeMode
    , bindChown :: Bool
    -- ^ @:U@: podman chowns the host path, recursively, to the container's
    -- user at /every start/. Rootless, that is a subordinate uid of the
    -- operator's, so the operator can neither list nor remove the directory
    -- without @podman unshare@. It says who owns the data; it does not make
    -- it readable from the host.
    , bindRelabel :: Maybe Relabel
    , bindCreate :: Maybe HostDir
    -- ^ 'Nothing': the directory is the caller's, as for 'containerVolumes'
    }
    deriving (Eq, Ord, Show)

{- | A read-write bind of this host directory at this path in the container,
the directory created if it is missing, with no owner, mode or option stated.
-}
bind :: FilePath -> FilePath -> Bind
bind host guest =
    Bind
        { bindHostPath = host
        , bindGuestPath = guest
        , bindMode = Podman.ReadWrite
        , bindChown = False
        , bindRelabel = Nothing
        , bindCreate = Just hostDir
        }

-- | The value of a 'Bind''s @Volume=@ line: @HOST:GUEST:rw[,U][,z|,Z]@.
renderBind :: Bind -> Text
renderBind b =
    mconcat
        [ Text.pack b.bindHostPath
        , ":"
        , Text.pack b.bindGuestPath
        , ":"
        , Text.intercalate "," $
            mconcat
                [ [volumeModeWord b.bindMode]
                , ["U" | b.bindChown]
                , ["z" | b.bindRelabel == Just RelabelShared]
                , ["Z" | b.bindRelabel == Just RelabelPrivate]
                ]
        ]

volumeModeWord :: Podman.VolumeMode -> Text
volumeModeWord Podman.ReadOnly = "ro"
volumeModeWord Podman.ReadWrite = "rw"

{- | Why this bind cannot be declared, if it cannot. A path with a colon in
it would be read by podman as another field of the @Volume=@ value, and a
relative host path as the name of a volume, which is no directory to create.
-}
bindProblems :: Bind -> [Text]
bindProblems b =
    mconcat
        [ ["the bind's host path is not an absolute path: " <> host | not (isAbsolute b.bindHostPath)]
        , ["the bind's guest path is not an absolute path: " <> guest | not (isAbsolute b.bindGuestPath)]
        , ["a colon in the bind's " <> what <> ": " <> value | (what, value) <- [("host path", host), ("guest path", guest)], Text.any (== ':') value]
        , [ "the bind of " <> host <> " states an owner and :U, which says another at every start"
          | Just HostDir{hostDirOwner = Just _} <- [b.bindCreate]
          , b.bindChown
          ]
        , [ "the owner of " <> host <> " is not what chown takes: " <> o
          | Just HostDir{hostDirOwner = Just o} <- [b.bindCreate]
          , Text.null o || "-" `Text.isPrefixOf` o || Text.any isSpace o
          ]
        , [ "the mode of " <> host <> " is not three or four octal digits: " <> m
          | Just HostDir{hostDirMode = Just m} <- [b.bindCreate]
          , Text.length m `notElem` [3, 4] || not (Text.all isOctDigit m)
          ]
        ]
  where
    host = Text.pack b.bindHostPath
    guest = Text.pack b.bindGuestPath

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
    , containerExec :: [Text]
    -- ^ the command run in the container, as an argv; empty is the image's
    -- own. Each word reaches the container as written
    -- ('Systemd.literalArg').
    , containerLifetime :: Lifetime
    , containerDescription :: Text
    , containerAfter :: [Systemd.UnitTarget]
    , containerEnvFile :: Maybe FilePath
    -- ^ an @EnvironmentFile=@, read by podman on the host at every start; it
    -- is pre-provisioned by something else and only watched here
    , containerPorts :: [Podman.PortMapping]
    , containerVolumes :: [Podman.VolumeMount]
    , containerBinds :: [Bind]
    -- ^ host directories mounted with options, and created by this module
    -- when the bind says so ('hostDirNode'). Rendered after
    -- 'containerVolumes'; empty renders nothing and adds no node, so a
    -- declaration without any is the file it always was.
    , containerNetwork :: Maybe Text
    , containerAuthFile :: Maybe Podman.AuthFile
    -- ^ the credentials the start's pull uses, the file 'Podman.login' wrote
    , containerRestart :: RestartPolicy
    , containerStartTimeout :: Maybe Int
    -- ^ @TimeoutStartSec=@, in seconds. The start includes the pull when the
    -- image is not on the machine yet, and systemd's default (90s) is short
    -- for a large image on a small machine. 'quadletContainer' pulls ahead
    -- of the start ('imageNode'), outside this timeout; a job's first run
    -- ('quadletJob') and a start at boot after the image was removed do not.
    , containerWantedBy :: Maybe Systemd.UnitTarget
    -- ^ what starts it at boot; 'Nothing' for a service that is only ever
    -- started by hand or by salmon
    , containerWatched :: [FilePath]
    -- ^ other host files the container reads at start (a bind-mounted
    -- config), a change to which should restart it
    , containerStamp :: Maybe FilePath
    -- ^ for a job only ('RunToCompletion'; refused otherwise): a file on the
    -- /host/ the unit keeps as the record of its last successful run, as
    -- 'Job.jobStamp' does and with the same two lines in @[Service]@. Each
    -- run touches @STAMP.running@ before the container starts and renames it
    -- to @STAMP@ once the container's command exited 0. The directory is the
    -- caller's: existing, and writable by whoever the unit runs as (root in
    -- 'Systemd.System', the user in 'Systemd.User'). 'Nothing' renders
    -- nothing, so a declaration without it is the file it always was.
    , containerReady :: Maybe Readiness
    -- ^ for a service only ('LongRunning'; refused otherwise): what @up@
    -- waits for after the restart ('awaitReady'), failing if it does not
    -- come. 'Nothing' is @up@ as it always was: done when @systemctl
    -- restart@ returns, which for a container means "it exists". Never
    -- rendered: the file, its fingerprint and the label are the same with
    -- and without it.
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
        , containerExec = []
        , containerLifetime = LongRunning
        , containerDescription = "container " <> Podman.getContainerName name <> " (salmon)"
        , containerAfter = []
        , containerEnvFile = Nothing
        , containerPorts = []
        , containerVolumes = []
        , containerBinds = []
        , containerNetwork = Nothing
        , containerAuthFile = Nothing
        , containerRestart = RestartOnFailure
        , containerStartTimeout = Nothing
        , containerWantedBy = Just "multi-user.target"
        , containerWatched = []
        , containerStamp = Nothing
        , containerReady = Nothing
        }

{- | A system-scope job: this command, run in this image to completion.
Never restarted and not started at boot; something schedules or runs it.
-}
containerJob :: Podman.ContainerName -> Text -> [Text] -> Container
containerJob name image command =
    (container name image)
        { containerExec = command
        , containerLifetime = RunToCompletion
        , containerDescription = "job " <> Podman.getContainerName name <> " (salmon)"
        , containerRestart = RestartNo
        , containerWantedBy = Nothing
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
renderContainer = renderLabelled Nothing

-- | 'renderContainer', with a @Label=@ line when there is a fingerprint to carry.
renderLabelled :: Maybe Text -> Container -> Text
renderLabelled fingerprint c =
    Text.unlines $
        mconcat
            [ ["[Unit]", "Description=" <> c.containerDescription]
            , ["After=" <> Text.unwords c.containerAfter | not (null c.containerAfter)]
            , ["", "[Container]"]
            , ["ContainerName=" <> Podman.getContainerName c.containerName]
            , ["Image=" <> c.containerImage]
            , ["Exec=" <> Text.unwords (map Systemd.literalArg c.containerExec) | not (null c.containerExec)]
            , ["Label=" <> quadletLabel <> "=" <> f | f <- maybeToList fingerprint]
            , ["EnvironmentFile=" <> Text.pack f | f <- maybeToList c.containerEnvFile]
            , ["PublishPort=" <> port p | p <- c.containerPorts]
            , ["Volume=" <> volume v | v <- c.containerVolumes]
            , ["Volume=" <> renderBind b | b <- c.containerBinds]
            , ["Network=" <> n | n <- maybeToList c.containerNetwork]
            , ["PodmanArgs=--authfile=" <> Text.pack (Podman.getAuthFile a) | a <- maybeToList c.containerAuthFile]
            , ["", "[Service]"]
            , ["Type=oneshot" | c.containerLifetime == RunToCompletion]
            , ["Restart=" <> restart c.containerRestart]
            , ["TimeoutStartSec=" <> Text.pack (show t) | t <- maybeToList c.containerStartTimeout]
            , -- systemd runs these on the host, around the generator's own
              -- ExecStart=; the second only when that one exited 0
              concat
                [ [ "ExecStartPre=" <> hostCommand ["touch", Text.pack (Job.stampRunning s)]
                  , "ExecStartPost=" <> hostCommand ["mv", "-f", Text.pack (Job.stampRunning s), Text.pack s]
                  ]
                | s <- maybeToList c.containerStamp
                ]
            , concat [["", "[Install]", "WantedBy=" <> w] | w <- maybeToList c.containerWantedBy]
            ]
  where
    hostCommand :: [Text] -> Text
    hostCommand = Text.unwords . map Systemd.literalArg

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
            , volumeModeWord v.volumeMode
            ]

    restart :: RestartPolicy -> Text
    restart RestartNo = "no"
    restart RestartOnFailure = "on-failure"
    restart RestartAlways = "always"

{- | 'renderContainer' plus the keyed digest of the watched files: everything
that decides what the container should be. With nothing watched it is
'renderContainer' exactly, and no key is read.

Throws 'WatchKeyUnusable' when something is watched and the key cannot be
read: there is no fallback to a plain hash, which is the thing the key is
there to avoid. In a graph the key is a node the file stands on
('watchKeyNode'); outside one, 'ensureWatchKey' first.
-}
renderContainerWatching :: Container -> IO Text
renderContainerWatching = renderWatching Nothing

renderWatching :: Maybe Text -> Container -> IO Text
renderWatching fingerprint c = case watchedFiles c of
    [] -> pure (renderLabelled fingerprint c)
    files -> do
        key <- readWatchKey c
        frames <- Systemd.watchedFrames files
        pure (renderLabelled fingerprint c <> keyedWatchLine key frames)

{- | The trailing comment: @# salmon-watches: hmac-sha256:BASE64@, the
HMAC-SHA256 under the key of what 'Systemd.watchedFrames' read. Pure, and
the whole of what the watched files leave in the quadlet.
-}
keyedWatchLine :: ByteString.ByteString -> [ByteString.ByteString] -> Text
keyedWatchLine key frames =
    "# salmon-watches: hmac-sha256:" <> Text.decodeUtf8 (Base64.encode (SHA256.hmac key (ByteString.concat frames))) <> "\n"

{- | Where the key of this container's directory is: beside the quadlets, so
it has the directory's owner (root for 'systemQuadletDir', the user for a
user's) and its lifetime. The generator reads only the extensions it knows
and leaves this file alone.
-}
watchKeyPath :: Container -> FilePath
watchKeyPath c = c.containerUnitDir </> ".salmon-watch.key"

-- | The key is missing, unreadable or empty: its path, and which.
data WatchKeyUnusable = WatchKeyUnusable !FilePath !Text
    deriving (Show)

instance Exception WatchKeyUnusable

readWatchKey :: Container -> IO ByteString.ByteString
readWatchKey c = do
    let path = watchKeyPath c
    read_ <- try (ByteString.readFile path) :: IO (Either IOException ByteString.ByteString)
    case read_ of
        Left _ -> throwIO (WatchKeyUnusable path "the key of the watched files' digest cannot be read")
        Right bytes
            | ByteString.null (C8.strip bytes) -> throwIO (WatchKeyUnusable path "the key of the watched files' digest is empty")
            | otherwise -> pure (C8.strip bytes)

{- | Makes the key if there is none, and makes it owner-only whatever it was.

A key that exists is never replaced: a new key is a new fingerprint, and so a
restart, for every quadlet in the directory. It is written to a temporary
file created @0600@ and put in place with @link(2)@, which refuses an
existing name, so of two processes making it at once one wins and both read
the winner's.
-}
ensureWatchKey :: Container -> IO ()
ensureWatchKey c = do
    present <- doesFileExist path
    unless present $ do
        secret <- getRandomBytes 32 :: IO ByteString.ByteString
        suffix <- getRandomBytes 6 :: IO ByteString.ByteString
        let tmp = path <> ".tmp-" <> concatMap hex (ByteString.unpack suffix)
        ( do
                h <- fdToHandle =<< createFile tmp 0o600
                ByteString.hPut h (Base64.encode secret <> "\n") `finally` hClose h
                placed <- try (createLink tmp path)
                case placed of
                    Right () -> pure ()
                    Left e
                        | isAlreadyExistsError e -> pure ()
                        | otherwise -> throwIO e
            )
            `finally` removeFile tmp
    setFileMode path 0o600
  where
    path = watchKeyPath c
    hex w = [digits !! fromIntegral (w `div` 16), digits !! fromIntegral (w `mod` 16)]
    digits = "0123456789abcdef" :: String

{- | The verdict on the key file's mode and size, 'Nothing' for no file. A
key others can read is a 'Failure' ('ensureWatchKey' then closes it; it does
not make a new one, see there).
-}
interpretWatchKey :: FilePath -> Maybe (FileMode, Integer) -> CheckResult
interpretWatchKey path Nothing = Failure ("no key at " <> Text.pack path)
interpretWatchKey path (Just (mode, size))
    | size <= 0 = Failure ("the key at " <> Text.pack path <> " is empty")
    | mode .&. 0o077 /= 0 = Failure ("the key at " <> Text.pack path <> " is readable by others than its owner")
    | otherwise = Success

{- | The key of 'containerUnitDir', one node for every quadlet there that
watches a file. It stands on the directory and, like it, is never removed.
-}
watchKeyNode :: Container -> Op
watchKeyNode c =
    op "podman-quadlet-key" (deps [unitDirNode c]) $ \actions ->
        actions
            { help = "ensures " <> Text.pack path <> " exists, readable by its owner alone"
            , notes = ["keys the digest of the files the quadlets watch, shared by every quadlet there: down leaves it"]
            , ref = mkRef "podman-quadlet-key" path
            , check = do
                present <- doesFileExist path
                if present
                    then do
                        st <- getFileStatus path
                        pure (interpretWatchKey path (Just (fileMode st, fromIntegral (fileSize st))))
                    else pure (interpretWatchKey path Nothing)
            , up = ensureWatchKey c
            }
  where
    path = watchKeyPath c

-- | The generator's directory: created if missing, never removed.
unitDirNode :: Container -> Op
unitDirNode c =
    op "podman-quadlet-dir" nodeps $ \actions ->
        actions
            { help = "ensures " <> Text.pack c.containerUnitDir <> " exists"
            , notes = ["the generator's directory, shared by every quadlet: down leaves it"]
            , ref = mkRef "podman-quadlet-dir" c.containerUnitDir
            , up = createDirectoryIfMissing True c.containerUnitDir
            }

{- | The container label that says which quadlet a container was started
from: its value is 'quadletFingerprint' at the time the file was written.
-}
quadletLabel :: Text
quadletLabel = "salmon.quadlet"

{- | A hash of 'renderContainerWatching': the declaration and the watched
files' keyed digest, and nothing that depends on when it is asked. Without
the key it says nothing about the watched files.
-}
quadletFingerprint :: Container -> IO Text
quadletFingerprint c = FS.hashBytes . Text.encodeUtf8 <$> renderContainerWatching c

{- | What 'quadletFingerprint' was before the digest was keyed: the label of
a container started from a file written then. It is computed to be compared
with a running container's label and for nothing else: never written, never
reported. With nothing watched it is 'quadletFingerprint'.
-}
unkeyedFingerprint :: Container -> IO Text
unkeyedFingerprint c =
    FS.hashBytes . Text.encodeUtf8 <$> case watchedFiles c of
        [] -> pure (renderContainer c)
        files -> Systemd.withWatchedFingerprint files (renderContainer c)

{- | The file as written: 'renderContainerWatching' with one more line,
@Label=salmon.quadlet=FINGERPRINT@, so that a container started from this
file says so and 'checkContainer' can ask it.
-}
renderQuadlet :: Container -> IO Text
renderQuadlet c = do
    fingerprint <- quadletFingerprint c
    renderWatching (Just fingerprint) c

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
        , [ "a job cannot be restarted always: systemd refuses it for a oneshot unit"
          | c.containerLifetime == RunToCompletion
          , c.containerRestart == RestartAlways
          ]
        , [ "a stamp records a run that finished, and this container is not a job"
          | Just _ <- [c.containerStamp]
          , c.containerLifetime /= RunToCompletion
          ]
        , ["the stamp is not an absolute path: " <> Text.pack s | s <- maybeToList c.containerStamp, not (isAbsolute s)]
        , [ "readiness is a service's: a job is done when its command exits"
          | Just _ <- [c.containerReady]
          , c.containerLifetime /= LongRunning
          ]
        , concat [readinessProblems rd | rd <- maybeToList c.containerReady]
        , concatMap bindProblems c.containerBinds
        , [ "two binds create " <> Text.pack a.bindHostPath <> " differently"
          | (i, a) <- created
          , (j, b) <- created
          , i < j
          , a.bindHostPath == b.bindHostPath
          , a.bindCreate /= b.bindCreate
          ]
        , ["a line break in the " <> what | (what, value) <- fields, Text.any (`elem` ("\n\r" :: String)) value]
        ]
  where
    name = Podman.getContainerName c.containerName
    created :: [(Int, Bind)]
    created = [(i, b) | (i, b@Bind{bindCreate = Just _}) <- zip [0 ..] c.containerBinds]
    fields :: [(Text, Text)]
    fields =
        mconcat
            [ [("name", name), ("image", c.containerImage), ("description", c.containerDescription)]
            , [("command", w) | w <- c.containerExec]
            , [("after", a) | a <- c.containerAfter]
            , [("env file", Text.pack f) | f <- maybeToList c.containerEnvFile]
            , [("published port", p.portOnHost <> p.portInGuest) | p <- c.containerPorts]
            , [("volume", Text.pack (v.volumeHostPath <> v.volumeGuestPath)) | v <- c.containerVolumes]
            , [("bind", Text.pack (b.bindHostPath <> b.bindGuestPath)) | b <- c.containerBinds]
            , [("network", n) | n <- maybeToList c.containerNetwork]
            , [("auth file", Text.pack (Podman.getAuthFile a)) | a <- maybeToList c.containerAuthFile]
            , [("wanted-by", w) | w <- maybeToList c.containerWantedBy]
            , [("stamp", Text.pack s) | s <- maybeToList c.containerStamp]
            ]

readinessProblems :: Readiness -> [Text]
readinessProblems rd =
    mconcat
        [ ["the readiness hold is negative" | rd.readyHold < 0]
        , ["a readiness probe needs a timeout of at least a second" | Just _ <- [rd.readyProbe], rd.readyTimeout < 1]
        , ["the readiness probe's port is not a port: " <> Text.pack (show p) | Just (ProbeTcp _ p) <- [rd.readyProbe], p < 1 || p > 65535]
        , ["the readiness probe has no host" | Just (ProbeTcp h _) <- [rd.readyProbe], Text.null (Text.strip h)]
        , ["the readiness probe has no command" | Just (ProbeCommand cmd _) <- [rd.readyProbe], null cmd]
        ]

data InvalidContainer = InvalidContainer !FilePath ![Text]
    deriving (Show)

instance Exception InvalidContainer

-------------------------------------------------------------------------------

{- | Installs the quadlet and keeps its service running as written.

The 'Track'' is where the caller says what the container stands on: podman
itself, the 'Podman.login' whose 'Podman.AuthFile' the pull reads, whatever
delivers the env file, a migration the new image needs. They are applied
before the file is written: the file node depends on them, so one that fails
leaves the file @Blocked@ as well as this node, and the machine keeps the
quadlet its running container was started from. (As siblings of the file,
which they once were, a failed one still let the new image be written under
the old container.)

That holds for the 'Track'' only. 'Salmon.Op.OpGraph.inject' on the 'Op'
returned here adds a predecessor of /this/ node, beside the file and in no
order with it; a precondition of the new declaration belongs in the track.

@up@ is @daemon-reload@ then @restart@, which returns once the container is
running (the generated service is @Type=notify@). The one case with no
restart is a container started before the watched files' digest was keyed
and otherwise current ('interpretAdoptable'): the reload is all it needs. The restart stops the old
container before the start would pull the new image, so the image is pulled
before any of that, by 'imageNode', which the quadlet file depends on: a pull
that fails is that node's failed @up@, the file and this node are @Blocked@,
and whatever was running keeps running from the file it was started from.

"Running" there means the container exists, not that it works. With
'containerReady' declared, @up@ goes on to 'awaitReady' and throws 'NotReady'
when the container dies, is restarted by systemd, or does not answer its
probe in time. The failing unit is left as it is (systemd keeps restarting or
has given up; either is what the operator needs to see), and nothing is
rolled back: the old container was stopped by the restart. The node's
@check@ is unchanged by it, so a later pass that happens to sample a
crash-looping unit while it is @active@ still reads it as satisfied.

@down@
stops the service, which removes the container, and the file's own @down@
removes the file and reloads so that the generated unit goes with it. The
image is left on the machine, and so is 'containerUnitDir': the node creates
it if it is missing but never removes it, since every quadlet on the machine
lives there. So is every directory of 'containerBinds' ('hostDirNode'), which
holds the container's data.
-}
quadletContainer ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Container ->
    Container ->
    Op
quadletContainer r systemctl t c =
    withCommand r systemctl (Systemd.DaemonReload c.containerScope) $ \reload ->
        withCommand r systemctl (Systemd.Up c.containerScope target) $ \restart ->
            withCommand r systemctl (Systemd.Stop c.containerScope target) $ \stop ->
                op "podman-quadlet" (deps [quadletFile r ([imageNode c `inject` run t c, run t c] <> hostDirNodes c) c, run t c]) $ \actions ->
                    actions
                        { help = "runs " <> c.containerImage <> " as " <> target
                        , notes =
                            [ "image: " <> c.containerImage
                            , "quadlet: " <> declaredFingerprint c
                            ]
                                <> ["ready: " <> describeReadiness rd | rd <- maybeToList c.containerReady]
                        , ref = mkRef "systemd-unit" target
                        , check = checkContainer c
                        , up = do
                            reload
                            -- a container started from this declaration and
                            -- these files, before the digest was keyed: the
                            -- file changed and what should run did not
                            adopted <- startedUnkeyed c
                            unless adopted $ do
                                restart
                                case c.containerReady of
                                    Nothing -> pure ()
                                    Just rd -> do
                                        verdict <- awaitReady (systemWaiting c) rd
                                        either (throwIO . NotReady target) pure verdict
                        , down = stop
                        }
  where
    target = serviceTarget c

{- | Installs the quadlet of a job and starts nothing: 'quadletContainer' for
a container that runs to completion ('containerJob').

The check is 'checkJobInstalled': systemd holds the generated service, its
source file has not changed since, and the unit was generated from this
declaration. There is no running container to ask, and none is wanted. @up@ is @daemon-reload@, after which a unit systemd
still does not hold is thrown: the generator drops a file it cannot read
without failing the reload, so that is the only place a refused quadlet
shows. @down@ stops a run under way if there is one; the file's @down@
removes the file and reloads. As for 'quadletContainer', what the 'Track''
declares is applied before the file is written.

The image is pulled by the first /run/, not here. A job whose first run
must not wait for a pull stands on a 'Podman.pullImage'.
-}
quadletJob ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Container ->
    Container ->
    Op
quadletJob r systemctl t c =
    withCommand r systemctl (Systemd.DaemonReload c.containerScope) $ \reload ->
        withCommand r systemctl (Systemd.Stop c.containerScope target) $ \stop ->
            op "podman-quadlet-job" (deps [quadletFile r (run t c : hostDirNodes c) c, run t c]) $ \actions ->
                actions
                    { help = "installs the job " <> target <> " running " <> c.containerImage <> ", without running it"
                    , notes =
                        [ "image: " <> c.containerImage
                        , "quadlet: " <> declaredFingerprint c
                        ]
                    , ref = mkRef "systemd-unit" target
                    , check = checkJobInstalled c
                    , up = Job.reloadAndRequireLoaded c.containerScope target reload
                    , down = Job.stopIfKnown c.containerScope target stop
                    }
  where
    target = serviceTarget c

{- | Does systemd hold the job's service, /generated from this declaration/?

'Systemd.checkLoaded' is asked first and anything but 'Success' is the
answer. It is not enough for a generated unit, though. A job at rest that
nothing refers to is unloaded by systemd as its run ends, and asking about
it loads it again from the generator's /last output/: the unit then reads
@loaded@ with @NeedDaemonReload=no@ although the quadlet was rewritten since
that output was made, and a run of it is a run of the old command (seen on
systemd 255 with podman 4.9: a job re-declared from @exit 0@ to @exit 3@
kept succeeding). An authored unit has no such gap, its file being what
systemd reads.

So the unit is asked what it would run. The written file carries
'quadletLabel' ('renderQuadlet'), the generator turns it into a @--label@ of
the unit's @ExecStart=@, and 'interpretGenerated' looks for the declared
'quadletFingerprint' there. A @systemctl@ that cannot answer is 'Unknown'.
-}
checkJobInstalled :: Container -> IO CheckResult
checkJobInstalled c = do
    loaded <- Systemd.checkLoaded c.containerScope (serviceTarget c)
    case loaded of
        Success -> do
            declared <- quadletFingerprint c
            (code, out, _err) <-
                readCreateProcessWithExitCode
                    ( proc
                        "systemctl"
                        ( Systemd.scopeArgs c.containerScope
                            <> ["show", Text.unpack (serviceTarget c), "--property=ExecStart", "--value"]
                        )
                    )
                    ""
            pure $ case code of
                ExitSuccess -> interpretGenerated declared (Text.pack out)
                ExitFailure _ -> Unknown
        other -> pure other

{- | The verdict on a generated unit's @ExecStart=@ (as @systemctl show@
prints it) against the declared 'quadletFingerprint': the unit was generated
from this declaration when its command labels the container with it.
-}
interpretGenerated :: Text -> Text -> CheckResult
interpretGenerated declared execStart
    | (quadletLabel <> "=" <> declared) `Text.isInfixOf` execStart = Success
    | otherwise = Failure ("the generated unit was not made from the declared quadlet " <> declared)

{- | What 'Job.completedRun' asks about a container job: a run of its
generated service newer than the quadlet file and than every file the
container reads at start ('watchedFiles': the env file and
'containerWatched'), read from 'containerStamp' if it has one.

The quadlet file is written through 'FS.filecontents', which leaves
identical bytes alone, so its time is when the declaration (or a watched
file's content) last changed. A watched file is counted by its own time as
well: one rewritten with the same content is a reason to run again here,
though not a changed quadlet.

The image is not in the list: a tag moved at the registry is not a change
this can see, as for 'quadletContainer'.
-}
containerCompletion :: Container -> Job.Completion
containerCompletion c =
    Job.Completion
        { Job.completionScope = c.containerScope
        , Job.completionUnit = serviceTarget c
        , Job.completionWritten = quadletPath c : watchedFiles c
        , Job.completionStamp = c.containerStamp
        }

{- | A container job that has run: 'Job.completedRun' over
'containerCompletion', standing on its 'quadletJob'. A pass runs the job
when no run has succeeded since the quadlet or a watched file was last
written, skips it otherwise, and fails when the run fails.

Keyed like a 'Job.runJob' of the same service. As for any
'Job.completedRun', what remembers a successful run is either something
keeping the unit loaded (a 'Job.timerUnit' triggering it, which the caller
declares and injects) or 'containerStamp'; with neither, the job runs at
every pass. The 'Track'' is the job's.
-}
completedQuadletJob ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' Container ->
    Container ->
    Op
completedQuadletJob r systemctl t c =
    Job.completedRun r systemctl (containerCompletion c) `inject` quadletJob r systemctl t c

withCommand ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Systemd.SystemCtlCall ->
    (IO () -> Op) ->
    Op
withCommand r systemctl cmd f =
    let
        g :: (Reporter Binary.Report -> IO ()) -> Op
        g callbin = f (callbin (contramap (Systemd.CallSystemCtl cmd) r))
     in
        withBinary systemctl Systemd.callSystemctl cmd g

{- | What the declaration alone says the file is. The file node's own
contents are an @IO Text@ once anything is watched, which has no content
fingerprint, so without this a re-declaration under @serve@ that only changes
the image would leave both nodes looking unchanged.
-}
declaredFingerprint :: Container -> Text
declaredFingerprint c = FS.hashBytes (Text.encodeUtf8 (renderContainer c))

{- | The quadlet file's node: 'FS.filecontents' with two changes, both to
what it stands on rather than to the file. 'ownFile' is applied to the file
node alone: an 'fmap' over the 'Op' reaches every node of its graph, and once
reached the enclosing directory, which then carried this container's notes
(two quadlets sharing the directory were a 'Conflicting' pair) and its
reload. And the directory is 'unitDirNode', not 'FS.dir', whose @down@ refuses a
non-empty directory: the generator's directory holds every quadlet on the
machine, so tearing one down failed whenever another was there. A quadlet
that watches a file also stands on 'watchKeyNode', since its contents cannot
be rendered without the key.
-}
quadletFile :: Reporter Systemd.Report -> [Op] -> Container -> Op
quadletFile r before c =
    let file = FS.filecontents (FS.FileContents path (renderQuadlet c))
     in file{node = fmap ownFile file.node, predecessors = deps (unitDirNode c : key <> before)}
  where
    path = quadletPath c

    -- only a quadlet that watches something digests anything
    key :: [Op]
    key = [watchKeyNode c | not (null (watchedFiles c))]

    ownFile :: Extension -> Extension
    ownFile ext =
        ext
            { notes = ext.notes <> ["quadlet: " <> declaredFingerprint c]
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
                    (contramap (Systemd.CallSystemCtl (Systemd.DaemonReload c.containerScope)) r)
            }

-------------------------------------------------------------------------------

-- | The 'hostDirNode' of every bind that creates its directory, one per path.
hostDirNodes :: Container -> [Op]
hostDirNodes c = go [] c.containerBinds
  where
    go _ [] = []
    go seen (b : rest) = case b.bindCreate of
        Just d | b.bindHostPath `notElem` seen -> hostDirNode c b d : go (b.bindHostPath : seen) rest
        _ -> go seen rest

{- | A bind's host directory, on the machine before the quadlet is written
(the file stands on it), so that the start does not fail on a path podman
will not make.

The @check@ is "a directory is there" and nothing more: 'Success' leaves an
existing directory alone whoever owns it and whatever its mode (see
'HostDir' for why), and a path that holds something else is a 'Failure' that
@up@ then throws on. @up@ creates the directory and its missing parents,
then applies the stated mode and owner /only to a directory it has just
made/; if either fails, the directory just made is removed again, so the next
pass does not find it there and call the declaration satisfied.

@down@ is nothing. The directory holds what the container wrote, and
'FS.dir''s @down@, which refuses a non-empty directory, would block the
teardown of the quadlet file standing on it. Removing the data is the
operator's.

Keyed on the path and described by the owner and mode alone, so two
containers binding one directory the same way share the node, and two
stating different owners are a @Conflicting@ pair.
-}
hostDirNode :: Container -> Bind -> HostDir -> Op
hostDirNode c b d =
    op "podman-quadlet-bind-dir" nodeps $ \actions ->
        actions
            { help = "creates " <> Text.pack path <> " unless it is there"
            , notes =
                mconcat
                    [ ["owner at creation: " <> o | o <- maybeToList d.hostDirOwner]
                    , ["mode at creation: " <> m | m <- maybeToList d.hostDirMode]
                    , ["an existing directory is left as it is; down leaves it, with what is in it"]
                    ]
            , ref = mkRef "podman-quadlet-bind-dir" path
            , check = interpretHostDir path <$> doesDirectoryExist path <*> doesPathExist path
            , up = do
                let problems = containerProblems c
                unless (null problems) $ throwIO (InvalidContainer (quadletPath c) problems)
                isDir <- doesDirectoryExist path
                unless isDir $ do
                    createDirectoryIfMissing True path
                    (setMode >> setOwner) `onException` removeDirectory path
            }
  where
    path = b.bindHostPath

    setMode :: IO ()
    setMode = case d.hostDirMode of
        Nothing -> pure ()
        Just m -> case readOct (Text.unpack m) of
            [(bits, "")] -> setFileMode path bits
            _ -> throwIO (HostDirFailed path ("not an octal mode: " <> m))

    setOwner :: IO ()
    setOwner = case d.hostDirOwner of
        Nothing -> pure ()
        Just o -> do
            (code, _out, err) <- readCreateProcessWithExitCode (proc "chown" (chownArgs o path)) ""
            when (code /= ExitSuccess) $
                throwIO (HostDirFailed path ("chown " <> o <> ": " <> Text.strip (Text.pack err)))

-- | @chown -- OWNER PATH@, on the directory alone.
chownArgs :: Text -> FilePath -> [String]
chownArgs owner path = ["--", Text.unpack owner, path]

-- | The verdict on a bind's host path: is a directory there, is anything there.
interpretHostDir :: FilePath -> Bool -> Bool -> CheckResult
interpretHostDir path isDir exists
    | isDir = Success
    | exists = Failure (Text.pack path <> " is there and is not a directory")
    | otherwise = Failure ("the directory " <> Text.pack path <> " is missing")

-- | A bind's directory that was made and could not be given its mode or owner.
data HostDirFailed = HostDirFailed !FilePath !Text
    deriving (Show)

instance Exception HostDirFailed

{- | The container's image, on the machine before its quadlet is written.

Left to itself the image is pulled by the service's start, and on a restart
the start comes after the stop: a reference that cannot be pulled (a typo, a
registry that does not answer, a credential that expired) then takes a
healthy container down and leaves the unit failing to start. With this node
ahead of the file, such a pass fails here instead and changes nothing: the
file still says the old image, so a reboot or a crash restarts the old
container too.

The @check@ is @podman image exists@, so an image already on the machine --
every unchanged declaration, and a locally built or tagged image -- is a
skip and never reaches a registry; the @up@ is @podman pull@ with the
container's 'containerAuthFile', which throws 'ImageUnavailable' when it
fails. Like the check on the running container, podman is run as whoever
runs salmon, whose store is the service's in both scopes. @down@ is nothing:
the image is left on the machine.

Keyed on the reference and the auth file, and described by nothing else, so
several containers of one image share the node. It has no dependencies of
its own; 'quadletContainer' injects what the caller's 'Track'' declares (the
login the pull reads).

What it does not cover: a tag that moved at the registry (the image exists,
nothing is pulled), and an image removed from the machine after the pass.
-}
imageNode :: Container -> Op
imageNode c =
    op "podman-quadlet-image" nodeps $ \actions ->
        actions
            { help = "pulls " <> c.containerImage <> " unless it is on the machine"
            , notes = ["auth file: " <> Text.pack (Podman.getAuthFile a) | a <- maybeToList c.containerAuthFile]
            , ref = mkRef "podman-quadlet-image" (c.containerImage, fmap Podman.getAuthFile c.containerAuthFile)
            , check = do
                (code, _out, _err) <- readCreateProcessWithExitCode (proc "podman" (imagePresentArgs c)) ""
                pure (interpretImagePresent c.containerImage code)
            , up = do
                let problems = containerProblems c
                unless (null problems) $ throwIO (InvalidContainer (quadletPath c) problems)
                (code, _out, err) <- readCreateProcessWithExitCode (proc "podman" (imagePullArgs c)) ""
                case code of
                    ExitSuccess -> pure ()
                    ExitFailure n -> throwIO (ImageUnavailable c.containerImage n (Text.strip (Text.pack err)))
            }

-- | @podman image exists IMAGE@: exits 0 when the local store has it, 1 when not.
imagePresentArgs :: Container -> [String]
imagePresentArgs c = ["image", "exists", Text.unpack c.containerImage]

-- | @podman pull [--authfile FILE] IMAGE@, the credentials being the ones the start would use.
imagePullArgs :: Container -> [String]
imagePullArgs c =
    mconcat
        [ ["pull", "--quiet"]
        , ["--authfile=" <> Podman.getAuthFile a | a <- maybeToList c.containerAuthFile]
        , [Text.unpack c.containerImage]
        ]

{- | The verdict on @podman image exists@'s exit code. Anything but 0 and 1
is podman failing to answer (125, a broken store), which is 'Unknown' and
not "missing": under @serve@ that must not read as an effect that went away.
-}
interpretImagePresent :: Text -> ExitCode -> CheckResult
interpretImagePresent _ ExitSuccess = Success
interpretImagePresent image (ExitFailure 1) = Failure ("the image " <> image <> " is not on the machine")
interpretImagePresent _ (ExitFailure _) = Unknown

-- | A pull that failed: the reference, podman's exit code and what it said.
data ImageUnavailable = ImageUnavailable !Text !Int !Text
    deriving (Show)

instance Exception ImageUnavailable

{- | 'Systemd.interpretShow' for a generated unit: @generated@ is the only
install state such a unit ever has. Everything else -- a changed source file,
a transition being 'Unknown', a stopped unit -- reads as it does there.
-}
interpretShow :: [Text] -> CheckResult
interpretShow = Systemd.interpretShowAccepting ("generated" : Systemd.installedStates)

{- | Is the service running, as written, /the container this declaration
describes/?

The unit is asked first ('interpretShow'), and anything but 'Success' is the
answer. But systemd only knows whether it has re-read its files, and
@daemon-reload@ is machine-wide: with two quadlets changed and one of them
brought up, the other's unit reads @NeedDaemonReload=no@ while still running
the container it was started with -- which is how a pass interrupted between
writing the files and restarting the services was followed by one that
skipped them, converged, and left the old image running. So the running
container is asked for its 'quadletLabel' and that is compared with
'quadletFingerprint' ('interpretStarted'). A @podman@ that cannot answer is
'Unknown', as a @systemctl@ that cannot is in 'Systemd.checkUnit'.

@podman@ is run as whoever runs salmon, which is the store the service's
container is in for both scopes: root for 'Systemd.System', the user for
'Systemd.User'.
-}
checkContainer :: Container -> IO CheckResult
checkContainer c = do
    unit <- Systemd.checkUnit interpretShow c.containerScope (serviceTarget c)
    case unit of
        Success -> do
            declared <- quadletFingerprint c
            unkeyed <- unkeyedFingerprint c
            maybe Unknown (interpretStarted declared unkeyed) <$> runningLabel c
        other -> pure other

-- | The running container's 'quadletLabel' as podman prints it, if podman answers.
runningLabel :: Container -> IO (Maybe Text)
runningLabel c = do
    (code, out, _err) <-
        readCreateProcessWithExitCode
            ( proc
                "podman"
                [ "container"
                , "inspect"
                , "--format"
                , "{{index .Config.Labels \"" <> Text.unpack quadletLabel <> "\"}}"
                , Text.unpack (Podman.getContainerName c.containerName)
                ]
            )
            ""
    pure $ case code of
        ExitSuccess -> Just (Text.pack out)
        ExitFailure _ -> Nothing

{- | 'interpretRunning', also satisfied by a container whose label is the
'unkeyedFingerprint' of the declaration: one started before the digest was
keyed, from this declaration and these watched files. That fingerprint moves
with the declaration and the files exactly as the keyed one does, so a
container started before an env file was rotated is still not satisfied.
-}
interpretStarted :: Text -> Text -> Text -> CheckResult
interpretStarted declared unkeyed printed
    | Text.strip printed == unkeyed = Success
    | otherwise = interpretRunning declared printed

{- | Is this unit's container one a file rewritten for the key alone leaves
as it is? Asked by @up@ after its reload, of the unit and of the running
container's label: yes when the unit is @active@ and the label is the
'unkeyedFingerprint' of the declaration, which is not the declared one (so
something is watched). Anything not known is no, and the restart happens.
-}
interpretAdoptable :: Text -> Text -> Maybe UnitSample -> Maybe Text -> Bool
interpretAdoptable declared unkeyed (Just s) (Just printed) =
    s.sampleActive == "active" && unkeyed /= declared && Text.strip printed == unkeyed
interpretAdoptable _ _ _ _ = False

startedUnkeyed :: Container -> IO Bool
startedUnkeyed c
    | null (watchedFiles c) = pure False
    | otherwise = do
        declared <- quadletFingerprint c
        unkeyed <- unkeyedFingerprint c
        interpretAdoptable declared unkeyed <$> sampleUnit c <*> runningLabel c

{- | The verdict on a running container's 'quadletLabel' (as @podman
container inspect@ printed it) against the declared 'quadletFingerprint'.
A container with no such label was started from a file written before the
label existed, or by something else under this name: either way not from
this declaration.
-}
interpretRunning :: Text -> Text -> CheckResult
interpretRunning declared printed
    | running == declared = Success
    | Text.null running || running == "<no value>" =
        Failure "the running container does not say which quadlet it was started from"
    | otherwise =
        -- the running label is not quoted: it may date from before the
        -- digest was keyed, and failure text goes into reports
        Failure ("the running container was not started from the declared quadlet " <> declared)
  where
    running = Text.strip printed

-------------------------------------------------------------------------------

-- | What @systemctl show@ says about a unit at one instant, as far as standing goes.
data UnitSample
    = UnitSample
    { sampleActive :: Text
    , sampleSub :: Text
    , sampleRestarts :: Maybe Int
    -- ^ @NRestarts@: how many times systemd restarted it by itself
    , sampleResult :: Text
    }
    deriving (Eq, Show)

-- | @systemctl [--user] show UNIT@ for the four properties of a 'UnitSample'.
sampleArgs :: Container -> [String]
sampleArgs c =
    Systemd.scopeArgs c.containerScope
        <> ["show", Text.unpack (serviceTarget c), "--property=ActiveState,SubState,NRestarts,Result"]

-- | The @KEY=VALUE@ lines of 'sampleArgs', in any order; a missing key is empty.
parseSample :: [Text] -> UnitSample
parseSample ls =
    UnitSample
        { sampleActive = value "ActiveState"
        , sampleSub = value "SubState"
        , sampleRestarts = readMaybe (Text.unpack (value "NRestarts"))
        , sampleResult = value "Result"
        }
  where
    value key = maybe "" (Text.strip . Text.drop (Text.length key + 1)) (find ((key <> "=") `Text.isPrefixOf`) ls)

sampleUnit :: Container -> IO (Maybe UnitSample)
sampleUnit c = do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "systemctl" (sampleArgs c)) ""
    pure $ case code of
        ExitSuccess -> Just (parseSample (Text.lines (Text.pack out)))
        ExitFailure _ -> Nothing

{- | Is a unit that @systemctl restart@ just reported started still the
process that was started? Given the @NRestarts@ values that mean "systemd has
not restarted it since" (none: do not judge by the counter).

After a successful restart the unit was @active@, so anything else --
@activating@ included, which is systemd's @auto-restart@ -- is it having gone
down. The reason names the states and never the container's output.
-}
interpretStanding :: [Int] -> UnitSample -> Either Text ()
interpretStanding allowed s
    | s.sampleActive /= "active" =
        Left ("the unit is " <> s.sampleActive <> " (" <> s.sampleSub <> ", result: " <> s.sampleResult <> ")")
    | Just n <- s.sampleRestarts
    , not (null allowed)
    , n `notElem` allowed =
        Left ("systemd has restarted it since it was started (NRestarts=" <> Text.pack (show n) <> ")")
    | otherwise = Right ()

{- | What 'awaitReady' needs of the world, so that the order of things can be
tested without a systemd, a socket or a clock.
-}
data Waiting
    = Waiting
    { waitSample :: IO (Maybe UnitSample)
    -- ^ 'Nothing' when systemd could not be asked
    , waitProbe :: Probe -> IO Bool
    , waitSleep :: Int -> IO ()
    -- ^ milliseconds
    , waitNow :: IO Int
    -- ^ milliseconds, monotonic
    }

{- | Waits for a just-restarted unit to be ready, or says why it is not.

The unit is looked at first, then at every step, and the wait ends at the
first look that finds it down or restarted ('interpretStanding'): a
container that dies at boot fails the pass in about a second, not after the
probe's whole timeout. With a probe, it is asked every half second until it
answers yes, for at most 'readyTimeout' seconds (one attempt is itself cut
at five). Then the unit must stay standing for 'readyHold' seconds.

The restart count every later look is held to is the /first look's/, not 0
and not a reading from before the restart: @systemctl restart@ does not
reset @NRestarts@ on a unit systemd was restarting (seen on this tree's
development machine), and a count read before the restart can move before
the restart does, which would fail a container that came up fine. The price
is a container that died and came back between the restart returning and
the first look: that one restart is the baseline, and it is the next one,
inside the hold, that is seen. A hold shorter than the unit's own restart
delay can therefore miss a crash loop; the default delay is 100ms.
-}
awaitReady :: Waiting -> Readiness -> IO (Either Text ())
awaitReady w rd = do
    start <- w.waitNow
    first <- look []
    case first of
        Left why -> pure (Left why)
        Right s -> probing start (maybeToList s.sampleRestarts)
  where
    poll :: Int
    poll = 500

    look :: [Int] -> IO (Either Text UnitSample)
    look allowed = do
        sample <- w.waitSample
        pure $ case sample of
            Nothing -> Left "systemctl could not say how the unit is doing"
            Just s -> s <$ interpretStanding allowed s

    probing :: Int -> [Int] -> IO (Either Text ())
    probing start allowed = case rd.readyProbe of
        Nothing -> holding allowed =<< w.waitNow
        Just p -> do
            ok <- w.waitProbe p
            now <- w.waitNow
            if ok
                then holding allowed now
                else
                    if now - start >= rd.readyTimeout * 1000
                        then pure (Left ("not ready after " <> Text.pack (show rd.readyTimeout) <> "s: " <> describeProbe p <> " never held"))
                        else do
                            w.waitSleep poll
                            standing <- look allowed
                            either (pure . Left) (const (probing start allowed)) standing

    holding :: [Int] -> Int -> IO (Either Text ())
    holding allowed since = do
        standing <- look allowed
        case standing of
            Left why -> pure (Left why)
            Right _ -> do
                now <- w.waitNow
                let remaining = rd.readyHold * 1000 - (now - since)
                if remaining <= 0
                    then pure (Right ())
                    else w.waitSleep (min poll remaining) >> holding allowed since

-- | 'Waiting' against this machine's systemd, network and clock.
systemWaiting :: Container -> Waiting
systemWaiting c =
    Waiting
        { waitSample = sampleUnit c
        , waitProbe = runProbe
        , waitSleep = \ms -> threadDelay (ms * 1000)
        , waitNow = (\ns -> fromIntegral (ns `div` 1000000)) <$> getMonotonicTimeNSec
        }

-- | One attempt, cut at five seconds; anything thrown is "no".
runProbe :: Probe -> IO Bool
runProbe probe = do
    answer <- timeout 5000000 (try attempt) :: IO (Maybe (Either SomeException Bool))
    pure $ case answer of
        Just (Right ok) -> ok
        _ -> False
  where
    attempt :: IO Bool
    attempt = case probe of
        ProbeCommand cmd args -> do
            (code, _out, _err) <- readCreateProcessWithExitCode (proc cmd (map Text.unpack args)) ""
            pure (code == ExitSuccess)
        ProbeTcp host port -> do
            let hints = Net.defaultHints{Net.addrSocketType = Net.Stream}
            addrs <- Net.getAddrInfo (Just hints) (Just (Text.unpack host)) (Just (show port))
            case addrs of
                [] -> pure False
                addr : _ ->
                    bracket (Net.openSocket addr) Net.close $ \sock ->
                        True <$ Net.connect sock (Net.addrAddress addr)

-- | A container that was restarted and is not ready: its unit, and why.
data NotReady = NotReady !Systemd.UnitTarget !Text
    deriving (Show)

instance Exception NotReady
