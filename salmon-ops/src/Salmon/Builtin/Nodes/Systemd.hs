module Salmon.Builtin.Nodes.Systemd where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------
data Report
    = CallSystemCtl !SystemCtlCall !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

systemdService ::
    Reporter Report ->
    Track' (Binary "systemctl") ->
    Track' Config ->
    Config ->
    Op
systemdService = systemdServiceWatching []

{- | 'systemdService', told which files the service /reads/ at start.

Without this, a service whose config file changed is not restarted, and
nothing about that is visible: 'checkService' asks after the __unit__ file,
and a config file the unit merely points at (a @pgbouncer.ini@, a
@postgrest.conf@) leaves no trace in anything systemd knows. The unit is
active, enabled and loaded as written, so the node is skipped and the
running process keeps serving the old configuration -- silently, and
indefinitely.

The fix reuses the mechanism that already works rather than adding a second
one: the watched files' contents are hashed into a comment at the end of the
unit file. A changed config therefore changes the unit file, which is
exactly what @NeedDaemonReload@ is for, and the ordinary path (reload,
enable, restart) takes it from there. Nothing new to check, and a node with
no watched files renders byte-identically to before.

Two things to know. The hash is computed by the encoder, so this node uses
the @IO Text@ 'EncodeFileContents' instance and inherits its hazard: it is
read once by the check and once by @up@, and a file that changes between
those two reads simply gets picked up on the next pass. And the reaction to
a changed config is a __restart__, which for a connection-holding service
(pgbouncer) drops its clients -- the gentler @PAUSE@\/@RELOAD@\/@RESUME@
belongs to whatever node is orchestrating the change, see
@specs\/pg-switchover.md@.
-}
systemdServiceWatching ::
    [FilePath] ->
    Reporter Report ->
    Track' (Binary "systemctl") ->
    Track' Config ->
    Config ->
    Op
systemdServiceWatching watched r systemctl t cfg =
    withCommand (DaemonReload cfg.config_scope) $ \reload ->
        withCommand (Enable cfg.config_scope cfg.config_target) $ \enable ->
            withCommand (Up cfg.config_scope cfg.config_target) $ \up ->
                withCommand (Stop cfg.config_scope cfg.config_target) $ \stop ->
                    op "systemd-service" (deps [configContents, run t cfg]) $ \actions ->
                        actions
                            { help = "installs a systemd-unit and up it"
                            , ref = mkRef "systemd-unit" cfg.config_target
                            , check = checkService cfg
                            , up = reload >> enable >> up
                            , down = stop
                            }
  where
    r' cmd = contramap (CallSystemCtl cmd) r
    withCommand cmd f =
        let
            g :: (Reporter Binary.Report -> IO ()) -> Op
            g callbin = f (callbin (r' cmd))
         in
            withBinary systemctl callSystemctl cmd g
    unitPath :: FilePath
    unitPath = cfg.config_unit_dir </> Text.unpack cfg.config_target

    configContents :: Op
    configContents
        | null watched = filecontents $ FileContents unitPath (render_config cfg)
        | otherwise = filecontents $ FileContents unitPath (withWatchedFingerprint watched (render_config cfg))

{- | The unit text, with a comment carrying a hash of the watched files'
contents. A file that does not exist hashes as empty, so it appearing later
is itself a change.
-}
withWatchedFingerprint :: [FilePath] -> Text -> IO Text
withWatchedFingerprint paths unitText = do
    parts <- watchedFrames paths
    let digest = SHA256.finalize (SHA256.updates SHA256.init parts)
    pure (unitText <> "# salmon-watches: " <> Text.decodeUtf8 (Base64.encode digest) <> "\n")

{- | What 'withWatchedFingerprint' hashes: each file as its path, its length
and its bytes, in the order given, a missing file being an empty one. Exposed
for a caller that digests the same thing another way
("Salmon.Builtin.Nodes.Podman.Quadlet" keys it).
-}
watchedFrames :: [FilePath] -> IO [ByteString.ByteString]
watchedFrames paths = concat <$> traverse framed paths
  where
    framed :: FilePath -> IO [ByteString.ByteString]
    framed path = do
        exists <- doesFileExist path
        bytes <- if exists then ByteString.readFile path else pure ByteString.empty
        pure [C8.pack (path <> ":" <> show (ByteString.length bytes) <> ":"), bytes]

{- | Does this unit already exist, loaded as written, enabled and running?

The first @check@ on a long-running effect salmon does __not__ own, which is
the largest category of node in this repository and the one supervision was
built for. Without it, @systemdService@ takes the default answer,
'Salmon.Actions.UpDown.Immaterial' — and that verdict is a claim, made on
the node author's behalf, that there is nothing here worth asking about. For
a unit that can be stopped, crash, or be disabled behind salmon's back it is
simply false: the node would be brought up once by the declaring pass and
then parked, with nothing left in the system able to notice it had died.
Writing this check is what turns @systemdService@ from a node that is
applied into a node that is /supervised/.

Three properties, one @systemctl show@ (which exits 0 even for a unit it has
never heard of, so there is no error path to distinguish from an answer):

* __@ActiveState@__ is the effect itself. @active@ is
  'Salmon.Actions.UpDown.Success' and @inactive@\/@failed@ are
  'Salmon.Actions.UpDown.Failure'. The transitional states —
  @activating@, @deactivating@, @reloading@ — are
  'Salmon.Actions.UpDown.Unknown', which is exactly what that verdict is
  for: a service that is part-way through starting has not gone away, and
  restarting it on the strength of a half-finished transition is how a slow
  starter becomes a restart loop. @Unknown@ makes the supervisor wait and
  look again, which is the right answer and the only one available.
* __@UnitFileState@__ catches somebody having @systemctl disable@d the unit
  underneath us. The service is still running, so @ActiveState@ alone would
  say everything is fine, right up until the next reboot.
* __@NeedDaemonReload@__ is what makes a /changed/ unit file take effect.
  This node's own dependency rewrites the file before this check ever runs,
  so comparing the bytes on disk against what we would write can only ever
  say "they match"; systemd's own record of "the file changed since I loaded
  it" is the only thing that still remembers. Without it, editing a unit
  would rewrite the file and never restart the service.

= This changes what @run up@ does, deliberately

A @systemdService@ whose unit is already installed, enabled, loaded and
running is now __skipped__ rather than reloaded-enabled-restarted on every
@run up@. That is the point of giving a node a @check@ — and it is a real
behaviour change for existing callers, so it is worth being explicit: if you
were relying on @run up@ to bounce a service whose unit file did not change,
that no longer happens. Change the file (any change) and @NeedDaemonReload@
makes it happen again.
-}
checkService :: Config -> IO CheckResult
checkService cfg = checkUnit interpretShow cfg.config_scope cfg.config_target

{- | 'checkService' for any unit, with the caller's own reading of what
@systemctl show@ printed: the one @systemctl show@ and its "we could not ask"
answer, shared with nodes whose unit is not one 'Config' describes (a quadlet's
generated service, see "Salmon.Builtin.Nodes.Podman.Quadlet").
-}
checkUnit :: ([Text] -> CheckResult) -> Scope -> UnitTarget -> IO CheckResult
checkUnit interpret scope target = do
    (code, out, _err) <-
        readCreateProcessWithExitCode
            ( proc
                "systemctl"
                ( scopeArgs scope
                    <> [ "show"
                       , Text.unpack target
                       , "--property=ActiveState"
                       , "--property=UnitFileState"
                       , "--property=NeedDaemonReload"
                       ]
                )
            )
            ""
    pure $ case code of
        ExitSuccess -> interpret (Text.lines (Text.decodeUtf8With TextError.lenientDecode out))
        ExitFailure _ ->
            -- not "the unit is down": we could not ask. Saying 'Failure'
            -- here would have a supervisor restart every unit on a box
            -- whose systemd is not answering.
            Unknown

{- | The verdict 'checkService' draws from @systemctl show@'s output, split
out because it is the whole of the decision and the only part worth testing
without a systemd to hand.

A property that is missing entirely is treated as absent rather than assumed:
@systemctl show@ omits @UnitFileState@ for a unit it has never heard of, and
"never heard of" is a 'Salmon.Actions.UpDown.Failure' by way of
@ActiveState=inactive@ rather than by way of a special case.
-}
interpretShow :: [Text] -> CheckResult
interpretShow = interpretShowAccepting installedStates

-- | The @UnitFileState@s of a unit this module installed and enabled.
installedStates :: [Text]
installedStates = ["enabled", "enabled-runtime", "static", "indirect"]

{- | 'interpretShow' with the acceptable @UnitFileState@s as an argument: a
unit written by a generator is @generated@, can never be @enabled@, and is
exactly as installed as it will ever be.
-}
interpretShowAccepting :: [Text] -> [Text] -> CheckResult
interpretShowAccepting accepted ls
    | property "NeedDaemonReload" == Just "yes" =
        Failure "the unit file on disk has changed since systemd loaded it"
    | otherwise = case property "ActiveState" of
        Just "active" -> case property "UnitFileState" of
            Just st
                | st `elem` accepted -> Success
                | otherwise -> Failure ("the unit is running but " <> st)
            -- running, and systemd has no install state for it at all: not
            -- a thing this node can author, so not a thing to complain
            -- about either.
            Nothing -> Success
        Just "activating" -> Unknown
        Just "deactivating" -> Unknown
        Just "reloading" -> Unknown
        Just other -> Failure ("the unit is " <> other)
        Nothing -> Failure "systemctl said nothing about the unit's state"
  where
    property :: Text -> Maybe Text
    property name =
        case [Text.drop 1 v | l <- ls, let (k, v) = Text.breakOn "=" l, k == name, not (Text.null v)] of
            (x : _) -> Just (Text.strip x)
            [] -> Nothing

{- | Restarts a pre-existing systemd service (e.g. one shipped by a Debian
package, such as nginx) — unlike 'systemdService', this does not author a
unit file of its own.
-}
restartService :: Reporter Report -> Track' (Binary "systemctl") -> UnitTarget -> Op
restartService r systemctl target =
    withCommand (Up System target) $ \restart ->
        op "systemd-restart-service" nodeps $ \actions ->
            actions
                { help = "restarts " <> target
                , ref = mkRef "systemd-restart" target
                , up = restart
                }
  where
    r' cmd = contramap (CallSystemCtl cmd) r
    withCommand cmd f =
        let
            g :: (Reporter Binary.Report -> IO ()) -> Op
            g callbin = f (callbin (r' cmd))
         in
            withBinary systemctl callSystemctl cmd g

{- | A system-wide unit (@systemctl@ against @\/etc\/systemd\/system@, the
original and still-default behavior) vs. a per-user one (@systemctl --user@
against a caller-resolved @~\/.config\/systemd\/user@, see 'Config's
@config_unit_dir@) — the latter needs no root at all, which is what
"Salmon.Builtin.Nodes.Qemu" uses for its VM units so the whole Layer-3 test
tier (see @specs/qemu-test-vms.md@) doesn't need it either. Systemd itself
rejects @User=@\/@Group=@ directives in a user-manager unit (a user session
can't switch users), so 'render_service' omits them for 'User' scope.
-}
data Scope = System | User
    deriving (Eq, Show)

scopeArgs :: Scope -> [String]
scopeArgs System = []
scopeArgs User = ["--user"]

data SystemCtlCall
    = DaemonReload Scope
    | Enable Scope UnitTarget
    | Up Scope UnitTarget
    | Stop Scope UnitTarget
    | -- | @start@: for a @Type=oneshot@ unit, returns when the command has
      -- exited and fails if it did; a unit already running is joined, where
      -- 'Up' (@restart@) would kill it first
      StartUnit Scope UnitTarget
    | -- | @disable --now@: stops the unit and removes what 'Enable' linked
      Disable Scope UnitTarget
    | -- | @mask --now@: stops the unit and points it at @\/dev\/null@
      Mask Scope UnitTarget
    | Unmask Scope UnitTarget
    deriving (Show)

callSystemctl :: Command "systemctl" SystemCtlCall
callSystemctl = Command go
  where
    go (DaemonReload sc) = proc "systemctl" (scopeArgs sc <> ["daemon-reload"])
    go (Enable sc u) = proc "systemctl" (scopeArgs sc <> ["enable", Text.unpack u])
    go (Up sc u) = proc "systemctl" (scopeArgs sc <> ["restart", Text.unpack u])
    go (Stop sc u) = proc "systemctl" (scopeArgs sc <> ["stop", Text.unpack u])
    go (StartUnit sc u) = proc "systemctl" (scopeArgs sc <> ["start", Text.unpack u])
    go (Disable sc u) = proc "systemctl" (scopeArgs sc <> ["disable", "--now", Text.unpack u])
    go (Mask sc u) = proc "systemctl" (scopeArgs sc <> ["mask", "--now", Text.unpack u])
    go (Unmask sc u) = proc "systemctl" (scopeArgs sc <> ["unmask", Text.unpack u])

{- | A unit that must never run, masked rather than merely disabled: a
disabled unit can still be started by anything that @Wants=@ it (Debian's
@postgresql.service@ pulls in every @postgresql\@V-C@), a masked one cannot.

@up@ is @systemctl mask --now@, so a unit that is running when the mask lands
is stopped; one that is not (a Postgres cluster started by Patroni is not
owned by any unit) is untouched. @down@ unmasks and does not start anything.
The @check@ reads @systemctl is-enabled@ ('interpretIsEnabled').
-}
maskedUnit :: Reporter Report -> Track' (Binary "systemctl") -> Scope -> UnitTarget -> Op
maskedUnit r systemctl scope target =
    withCommand (Mask scope target) $ \mask ->
        withCommand (Unmask scope target) $ \unmask ->
            op "systemd-masked-unit" nodeps $ \actions ->
                actions
                    { help = "masks " <> target <> " so that nothing can start it"
                    , ref = mkRef "systemd-masked" target
                    , check = checkMasked scope target
                    , up = mask
                    , down = unmask
                    }
  where
    r' cmd = contramap (CallSystemCtl cmd) r
    withCommand cmd f =
        let
            g :: (Reporter Binary.Report -> IO ()) -> Op
            g callbin = f (callbin (r' cmd))
         in
            withBinary systemctl callSystemctl cmd g

checkMasked :: Scope -> UnitTarget -> IO CheckResult
checkMasked scope target = do
    -- @is-enabled@ exits non-zero for most states, so the exit code carries
    -- nothing; only the word it prints does.
    (_code, out, _err) <-
        readCreateProcessWithExitCode
            (proc "systemctl" (scopeArgs scope <> ["is-enabled", Text.unpack target]))
            ""
    pure (interpretIsEnabled (Text.decodeUtf8With TextError.lenientDecode out))

{- | The verdict on @systemctl is-enabled@'s output, pure. Only @masked@ is
satisfied: @masked-runtime@ (a mask under @\/run@) does not survive a reboot,
and a unit that is merely @disabled@ is the case this node exists to refuse.
-}
interpretIsEnabled :: Text -> CheckResult
interpretIsEnabled out = case Text.strip (Text.takeWhile (/= '\n') (Text.strip out)) of
    "masked" -> Success
    "" -> Unknown
    other -> Failure ("not masked: " <> Text.take 60 other)

-------------------------------------------------------------------------------

data Config
    = Config
    { config_scope :: Scope
    , config_unit_dir :: FilePath
    -- ^ @\/etc\/systemd\/system@ for 'System' scope; a caller-resolved
    -- @~\/.config\/systemd\/user@ for 'User' scope (this module has no
    -- opinion on how @~@ is found — same "resolve before constructing"
    -- rule as 'Salmon.Builtin.Nodes.Qemu.resolveKernelInitrd').
    , config_target :: UnitTarget
    , config_unit :: Unit
    , config_service :: Service
    , config_install :: Install
    }

render_config :: Config -> Text
render_config c =
    Text.unlines
        [ render_unit c.config_unit
        , ""
        , render_service c.config_scope c.config_service
        , ""
        , render_install c.config_install
        ]

type UnitTarget = Text

data Unit
    = Unit
    { unit_description :: Text
    , unit_after :: UnitTarget
    }

render_unit :: Unit -> Text
render_unit u =
    Text.unlines
        [ "[Unit]"
        , "Description=" <> u.unit_description
        , "After=" <> u.unit_after
        ]

data ServiceType
    = Simple

type User = Text
type Group = Text
type UMask = Text

data Start
    = Start
    { start_path :: FilePath
    , start_args :: [Text]
    }

-- | The @Restart=@ directive salmon writes into a unit file, for systemd
-- itself to act on. Not to be confused with 'Salmon.Op.Supervision.Restart',
-- salmon's own restart decision about a node — the two used to share a name
-- ((R8) in @specs/per-node-state-machines-remaining.md@) until a systemd
-- node with a 'Salmon.Op.Supervision.Supervision' needed both in scope.
data RestartDirective
    = OnFailure

data KillMode
    = Process

data Service
    = Service
    { service_type :: ServiceType
    , service_user :: User
    , service_group :: Group
    , service_umask :: UMask
    , service_execStart :: Start
    , service_restart :: RestartDirective
    , service_killmode :: KillMode
    , service_working_dir :: FilePath
    }

render_service :: Scope -> Service -> Text
render_service scope s =
    Text.unlines $
        mconcat
            [ ["[Service]", "Type=" <> render_type s.service_type]
            , case scope of
                System -> ["User=" <> s.service_user, "Group=" <> s.service_group]
                User -> []
            ,
                [ "UMask=" <> s.service_umask
                , "ExecStart=" <> render_start s.service_execStart
                , "Restart=" <> render_restart s.service_restart
                , "KillMode=" <> render_killmode s.service_killmode
                , "WorkingDirectory=" <> Text.pack s.service_working_dir
                ]
            ]
  where
    render_type :: ServiceType -> Text
    render_type Simple = "simple"

    -- | Quotes an arg that contains whitespace, per systemd's own
    -- @ExecStart=@ word-splitting rules (docs: @systemd.service(5)@ §
    -- "Command lines"): unlike @Text.unwords@ alone, a bare multi-word
    -- string here (e.g. a kernel @-append@ value) would otherwise be split
    -- back into several separate argv entries by systemd's parser when the
    -- unit file is loaded — this bit "Salmon.Builtin.Nodes.Qemu" for
    -- exactly that reason (hand-validated 2026-08-20, see
    -- @specs/qemu-test-vms-progress.md@).
    render_start :: Start -> Text
    render_start s = Text.unwords (Text.pack s.start_path : map quoteArg s.start_args)

    render_restart :: RestartDirective -> Text
    render_restart OnFailure = "on-failure"

    render_killmode :: KillMode -> Text
    render_killmode Process = "process"

{- | One word of a command line as systemd splits it: quoted when it holds
whitespace or a character the splitter reads, left alone otherwise. It does
not touch @$@ or @%@, which systemd expands inside quotes too; see
'literalArg' for a word that must arrive as written.
-}
quoteArg :: Text -> Text
quoteArg a
    | Text.any (`elem` (" \t\"'$`\\" :: String)) a =
        "\"" <> Text.replace "\"" "\\\"" (Text.replace "\\" "\\\\" a) <> "\""
    | otherwise = a

{- | 'quoteArg' for a word the process must receive exactly as declared:
@$@ is written @$$@ and @%@ is written @%%@, the two characters systemd
substitutes in a command line (environment variables and unit specifiers)
whatever the quoting. Without it a @sh -c@ script naming @$HOME@ is handed
to the shell with systemd's idea of @HOME@ already spliced in, or nothing
at all for a variable only the script sets. An empty word is kept as one.
-}
literalArg :: Text -> Text
literalArg a
    -- an empty word is a word, and a bare @;@ would end the command
    | Text.null a = "\"\""
    | a == ";" = "\";\""
    | otherwise = quoteArg (Text.replace "%" "%%" (Text.replace "$" "$$" a))

{- | Is this unit known to systemd as its file is now written, whether or not
it is running? The question for a unit nothing starts at install time: a
@Type=oneshot@ job a timer triggers.

@LoadState=loaded@ and @NeedDaemonReload=no@. A unit file systemd has not
been told about reads @loaded@ already when it sits in a directory systemd
searches (@show@ loads it on demand), and @not-found@ when it is made by a
generator that has not run since; @bad-setting@ and @error@ are a file
systemd refuses, which a reload does not cure.
-}
checkLoaded :: Scope -> UnitTarget -> IO CheckResult
checkLoaded scope target = do
    (code, out, _err) <-
        readCreateProcessWithExitCode
            ( proc
                "systemctl"
                (scopeArgs scope <> ["show", Text.unpack target, "--property=LoadState", "--property=NeedDaemonReload"])
            )
            ""
    pure $ case code of
        ExitSuccess -> interpretLoaded (Text.lines (Text.decodeUtf8With TextError.lenientDecode out))
        ExitFailure _ -> Unknown

-- | The verdict 'checkLoaded' draws from @systemctl show@'s output, pure.
interpretLoaded :: [Text] -> CheckResult
interpretLoaded ls
    | property "NeedDaemonReload" == Just "yes" =
        Failure "the unit file on disk has changed since systemd loaded it"
    | otherwise = case property "LoadState" of
        Just "loaded" -> Success
        Just "not-found" -> Failure "systemd does not know the unit"
        Just other -> Failure ("the unit is " <> other)
        Nothing -> Failure "systemctl said nothing about the unit's load state"
  where
    property :: Text -> Maybe Text
    property name =
        case [Text.drop 1 v | l <- ls, let (k, v) = Text.breakOn "=" l, k == name, not (Text.null v)] of
            (x : _) -> Just (Text.strip x)
            [] -> Nothing

data Install
    = Install
    { install_wantedBy :: UnitTarget
    }

render_install :: Install -> Text
render_install i =
    Text.unlines
        [ "[Install]"
        , "WantedBy=" <> i.install_wantedBy
        ]
