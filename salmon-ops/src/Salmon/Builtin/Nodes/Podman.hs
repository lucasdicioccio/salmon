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
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

import Control.Exception (Exception, throwIO)
import GHC.IO.Exception (ExitCode (..))
import GHC.IO.Handle (Handle, hClose)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process (ProcessHandle, StdStream (CreatePipe), waitForProcess)
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

{- | How @podman build@ is told what to build, beyond the Containerfile and
the tag. 'defaultBuildOptions' is what 'buildImage' has always done.

* 'buildContext': the build context directory, i.e. what @COPY@\/@ADD@ paths
  are relative to. 'Nothing' is the Containerfile's own directory (the build
  runs /in/ that directory, with no context argument). A repository whose
  Containerfiles live in a subdirectory but are written for the repository
  root as context sets this to the root.
* 'buildTarget': the stage of a multi-stage Containerfile to stop at
  (@--target@). 'Nothing' builds the last stage. Several images built from
  one file are several nodes, one 'TagName' each.
* 'buildOutput': where the build's output goes while it runs (see
  'Binary.Routing'). 'Binary.captured', the default, holds it until the build
  ends, as before; 'Binary.streamed' reports it line by line under the node,
  which is what makes a long build something to follow. It changes nothing
  about the command, so it is not part of the node's description.
-}
data BuildOptions
    = BuildOptions
    { buildContext :: Maybe FilePath
    , buildTarget :: Maybe Text
    , buildOutput :: Binary.Routing
    }
    deriving (Eq, Ord, Show)

defaultBuildOptions :: BuildOptions
defaultBuildOptions = BuildOptions Nothing Nothing Binary.captured

{- | Builds an image from a Containerfile, with that file's own directory as
the build context and no @--target@: 'buildImageWith' 'defaultBuildOptions'.
-}
buildImage :: Reporter Report -> Track' (Binary "podman") -> FS.File "containerfile" -> TagName -> Op
buildImage r podman = buildImageWith r podman defaultBuildOptions

{- | 'buildImage' with an explicit build context and\/or a @--target@ stage
(see 'BuildOptions').

The 'ref' is the tag alone, as for 'buildImage': a tag is one effect site
whatever it was built from, so two declarations building the same tag from
different contexts or stages are a collision, not two nodes. The options
are spelled in 'notes' when they are not the default, which is what lets
such a collision -- or a re-declaration that only moves the target -- be
seen as a differing representative.
-}
buildImageWith :: Reporter Report -> Track' (Binary "podman") -> BuildOptions -> FS.File "containerfile" -> TagName -> Op
buildImageWith r podman opts containerfile tagname =
    FS.withFile containerfile $ \containerfilepath ->
        Binary.withBinaryWith opts.buildOutput podman podmanCommand (BuildWith opts containerfilepath tagname) $ \build ->
            op "podman-build" (deps []) $ \actions ->
                actions
                    { help = "builds a podman image in container path and tag it"
                    , notes = buildNotes opts
                    , ref = mkRef "podman-build" tagname
                    , up = build (r' containerfilepath)
                    , down = Binary.untrackedExec podmanCommand (RmiTag tagname) "" r''
                    }
  where
    r' containerfilepath = contramap (BuildImage containerfilepath tagname) r
    r'' = contramap (RemoveBuiltImage tagname) r

-- | Nothing for 'defaultBuildOptions', so a plain 'buildImage' node is
-- described exactly as it was before the options existed.
buildNotes :: BuildOptions -> [Text]
buildNotes opts =
    maybe [] (\c -> ["build context: " <> Text.pack c]) opts.buildContext
        <> maybe [] (\t -> ["target stage: " <> t]) opts.buildTarget

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
'up'. That is once per pass, and once per @run serve@ session: a token that
lasts an hour is not renewed by this node. 'loginExpiring' is the node for a
credential that says how long it lasts, and 'pushLoggingIn' the push that
does not rely on a login made earlier in the pass. 'down' runs @podman logout --authfile@ against the same file (only if that
file actually holds credentials for this registry -- logging out of nothing
is an error, and a failing 'down' blocks a whole sub-DAG) and then removes
the file, which logout itself leaves behind, emptied.
-}
login :: Reporter Report -> Track' (Binary "podman") -> AuthFile -> Registry -> Username -> IO Text -> Op
login r podman authfile reg user getPassword =
    loginNode r podman authfile reg user $ \authenticate ext ->
        ext{up = getPassword >>= authenticate}

{- | The node 'login' and 'loginExpiring' share: one effect site (the 'Ref'
is the auth file, the registry and the username), one @down@. The
continuation is handed the action that runs @podman login@ with a password
on its stdin and fills in what differs, which is when to run it.
-}
loginNode ::
    Reporter Report ->
    Track' (Binary "podman") ->
    AuthFile ->
    Registry ->
    Username ->
    ((Text -> IO ()) -> Extension -> Extension) ->
    Op
loginNode r podman authfile reg user fill =
    withBinaryIO podman logincommand (LoginCmd authfile reg user) $ \mkProc ->
        op "podman-login" (deps [enclosingdir]) $ \actions ->
            fill
                (authenticateWith r mkProc authfile reg user)
                actions
                    { help = Text.unwords ["logs in to", getRegistry reg, "via", Text.pack (getAuthFile authfile)]
                    , ref = mkRef "podman-login" (getAuthFile authfile, getRegistry reg, getUsername user)
                    , down = do
                        -- `podman logout` is an error ("not logged into ...",
                        -- exit 125) when there is nothing to log out of, and a
                        -- failing `down` blocks the teardown of everything this
                        -- node was declared on top of. So ask first -- the
                        -- credentials live in this node's own authfile, which
                        -- makes that a file read.
                        exists <- doesFileExist (getAuthFile authfile)
                        when exists $ do
                            creds <- ByteString.readFile (getAuthFile authfile)
                            when (ByteString.pack (Text.unpack (getRegistry reg)) `ByteString.isInfixOf` creds) $
                                Binary.untrackedExec podmanCommand (Logout authfile reg) "" r''
                            -- logout only empties the credentials, leaving the
                            -- file ({"auths":{}}) behind to block the enclosing
                            -- Filesystem.dir's own `down`. This node caused the
                            -- file to exist, so this node removes it.
                            removeFile (getAuthFile authfile)
                    }
  where
    r'' = contramap (LogoutRegistry authfile reg) r
    enclosingdir = FS.dir (FS.Directory (takeDirectory (getAuthFile authfile)))

-- | @podman login@ with the password on its stdin; throws if it is refused.
authenticateWith ::
    Reporter Report ->
    (() -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)) ->
    AuthFile ->
    Registry ->
    Username ->
    Text ->
    IO ()
authenticateWith r mkProc authfile reg user pw = do
    runReporter r (LoginRegistry authfile reg user)
    (mStdin, _, _, ph) <- mkProc ()
    case mStdin of
        Just hin -> Text.hPutStr hin pw >> hClose hin
        Nothing -> pure ()
    waitForProcess ph >>= checkExitCode "podman login"

-- | A password and how long it is good for from when it was handed out.
data Credential
    = Credential
    { credentialSecret :: !Text
    , credentialLifetime :: !NominalDiffTime
    }

{- | 'login' for a credential that says when it expires (an OAuth access
token: an hour, typically), which is what makes a @check@ possible.

'login' has none, so it runs once per pass and, under @run serve@, once per
session: its verdict is 'Salmon.Actions.UpDown.Immaterial', the node is
parked, the token lapses, and whatever next reads the auth file fails with
credentials that look present. Here the expiry is written beside the auth
file after a login that worked ('loginStampPath'), and 'interpretLoginStamp'
answers 'Success' while more than 'loginRefreshMargin' of it is left. So a
@run up@ logs in again only when it has to, and under @run serve@ the
credential is /tended/: the check starts failing two minutes before the
expiry and the loop logs in again.

What this does not do is renew anything /inside/ one pass: a node is
applied once, so a push that starts an hour after the login of the same
pass still reads a lapsed credential. 'pushLoggingIn' is for that.

A credential whose lifetime is not above the margin cannot be tended (the
check would fail the moment the login succeeded) and @up@ refuses it,
'CredentialTooShort'; use 'login' for one of those.

The same effect site as 'login', so the same 'Ref'.
-}
loginExpiring :: Reporter Report -> Track' (Binary "podman") -> AuthFile -> Registry -> Username -> IO Credential -> Op
loginExpiring r podman authfile reg user getCredential =
    loginNode r podman authfile reg user (expiring authfile getCredential)

{- | What 'loginExpiring' adds to the login node, given the action that
authenticates with a password. Exposed for tests, which have no registry to
log in to.

It goes on the login node /alone/. An 'fmap' over the 'Op' reaches every
node of its graph, the enclosing directory of the auth file included, and
that directory is a predecessor: carrying the stamp's check and the stamp's
write, it went first, found the stamp expired, and wrote a fresh one without
logging in -- after which the login's own check read the fresh stamp and the
login was skipped, leaving expired credentials in the auth file under a
stamp vouching for them.
-}
expiring :: AuthFile -> IO Credential -> (Text -> IO ()) -> Extension -> Extension
expiring authfile getCredential authenticate ext =
    ext
        { notes = ext.notes <> ["renewed before the credential's recorded expiry"]
        , check = do
            now <- getCurrentTime
            present <- doesFileExist (getAuthFile authfile)
            recorded <- if present then readStamp else pure Nothing
            pure (interpretLoginStamp now recorded)
        , up = do
            -- read before the credential is asked for: its lifetime counts
            -- from when it was handed out, so this errs on the early side
            asked <- getCurrentTime
            credential <- getCredential
            when (credential.credentialLifetime <= loginRefreshMargin) $
                throwIO (CredentialTooShort credential.credentialLifetime)
            authenticate credential.credentialSecret
            -- only ever written after a login that worked: 'authenticate'
            -- throws otherwise
            writeFile stamp (renderLoginStamp (addUTCTime credential.credentialLifetime asked))
        , down = FS.removeFileIfPresent stamp >> ext.down
        }
  where
    stamp = loginStampPath authfile

    readStamp :: IO (Maybe Text)
    readStamp = (Just <$> Text.readFile stamp) `catchIOError` const (pure Nothing)

data LoginError
    = -- | the credential's lifetime, which 'loginRefreshMargin' already covers
      CredentialTooShort !NominalDiffTime
    deriving (Show)

instance Exception LoginError

-- | Where the expiry of the credential in an auth file is recorded.
loginStampPath :: AuthFile -> FilePath
loginStampPath authfile = getAuthFile authfile <> ".expires"

-- | The stamp: the expiry in whole seconds since the epoch.
renderLoginStamp :: UTCTime -> String
renderLoginStamp expiry = show (floor (utcTimeToPOSIXSeconds expiry) :: Integer) <> "\n"

-- | How much of a credential's life must be left for it to be left alone.
loginRefreshMargin :: NominalDiffTime
loginRefreshMargin = 120

{- | Is the recorded login still good? 'Nothing' is no stamp, or no auth file
to go with it.
-}
interpretLoginStamp :: UTCTime -> Maybe Text -> CheckResult
interpretLoginStamp _ Nothing = Failure "not logged in to the registry"
interpretLoginStamp now (Just recorded) =
    case readMaybe (Text.unpack (Text.strip recorded)) :: Maybe Integer of
        Nothing -> Failure "the recorded token expiry is unreadable"
        Just seconds
            | diffUTCTime (posixSecondsToUTCTime (fromInteger seconds)) now > loginRefreshMargin -> Success
            | otherwise -> Failure "the registry token has expired, or is about to"

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

{- | 'push' that logs in again first, in the same @up@.

A node is applied once per pass, so a 'login' the push depends on ran when
the pass reached it -- before the build, if the push also depends on one --
and an hour-long token can have lapsed by the time the push starts
(@unauthorized@, with credentials that look present). Nothing about the
login node can fix that from where it stands, whatever its check says: the
pass is past it. So the credential is asked for and the registry logged in to
right before the bytes go, which costs one @podman login@ beside a push.

It does not replace the login node: declare one on the same 'AuthFile' and
make this depend on it, as with 'push'. That node is what logs out and
removes the file on the way down; this one's @down@ is a no-op like
'push''s. Same effect site as @'push' (Just authfile)@, so the same 'Ref'.

Beside a 'loginExpiring' node the stamp is left as it was: it then
understates the credential in the file, which is the safe direction.
-}
pushLoggingIn :: Reporter Report -> Track' (Binary "podman") -> AuthFile -> Registry -> Username -> IO Text -> TagName -> Op
pushLoggingIn r podman authfile reg user getPassword tagname =
    withBinaryIO podman logincommand (LoginCmd authfile reg user) $ \mkProc ->
        withBinary podman podmanCommand (Push (Just authfile) tagname) $ \doPush ->
            op "podman-push" (deps []) $ \actions ->
                actions
                    { help = "pushes " <> tagname <> " to its registry"
                    , notes = ["logs in to " <> getRegistry reg <> " again before pushing"]
                    , ref = mkRef "podman-push" (tagname, Just (getAuthFile authfile))
                    , up = loggingInThen (getPassword >>= authenticateWith r mkProc authfile reg user) (doPush r')
                    }
  where
    r' = contramap (PushImage (Just authfile) tagname) r

{- | Log in, then act; a login that fails is the failure, and the action is
not attempted with whatever the auth file held before.
-}
loggingInThen :: IO () -> IO () -> IO ()
loggingInThen authenticate act = authenticate >> act

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
    | -- | 'BuildWith' 'defaultBuildOptions'.
      Build !FilePath !TagName
    | BuildWith !BuildOptions !FilePath !TagName
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
    (Build fullpath tagname) -> buildProc defaultBuildOptions fullpath tagname
    (BuildWith opts fullpath tagname) -> buildProc opts fullpath tagname
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
        -- --ignore: `podman rmi` on an absent image is an error ("image not
        -- known"), and this is a `down`, where a failure blocks the teardown
        -- of everything the node was declared on top of. An image that is
        -- already gone is this node's effect being gone.
        proc "podman" ["rmi", "--ignore", Text.unpack tagname]
    (Rm cname) ->
        proc "podman" ["rm", "-f", Text.unpack (getContainerName cname)]
    (RemoveNetworkCmd name) ->
        proc "podman" ["network", "rm", Text.unpack (getNetworkName name)]

{- | @podman build@, in one of two shapes.

With no explicit context the build runs /in/ the Containerfile's directory,
naming the file by its basename and giving no context argument (podman then
takes the working directory) -- the shape this module always rendered.

With one, the working directory is left alone and both paths are passed as
given: @-f@ the Containerfile, the context as the positional argument. A
relative path is then relative to wherever the salmon process runs, for
both alike.
-}
buildProc :: BuildOptions -> FilePath -> TagName -> CreateProcess
buildProc opts fullpath tagname = case opts.buildContext of
    Nothing ->
        (proc "podman" (["build", "-t", Text.unpack tagname, "-f", takeFileName fullpath] <> target))
            { cwd = Just $ takeDirectory fullpath
            }
    Just context ->
        proc "podman" (["build", "-t", Text.unpack tagname, "-f", fullpath] <> target <> [context])
  where
    target = maybe [] (\t -> ["--target", Text.unpack t]) opts.buildTarget

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
