module Salmon.Builtin.Nodes.Systemd where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
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
systemdService r systemctl t cfg =
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
    configContents = filecontents $ FileContents unitPath (render_config cfg)

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
checkService cfg = do
    (code, out, _err) <-
        readCreateProcessWithExitCode
            ( proc
                "systemctl"
                ( scopeArgs cfg.config_scope
                    <> [ "show"
                       , Text.unpack cfg.config_target
                       , "--property=ActiveState"
                       , "--property=UnitFileState"
                       , "--property=NeedDaemonReload"
                       ]
                )
            )
            ""
    pure $ case code of
        ExitSuccess -> interpretShow (Text.lines (Text.decodeUtf8With TextError.lenientDecode out))
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
interpretShow ls
    | property "NeedDaemonReload" == Just "yes" =
        Failure "the unit file on disk has changed since systemd loaded it"
    | otherwise = case property "ActiveState" of
        Just "active" -> case property "UnitFileState" of
            Just st
                | st `elem` ["enabled", "enabled-runtime", "static", "indirect"] -> Success
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
    deriving (Show)

callSystemctl :: Command "systemctl" SystemCtlCall
callSystemctl = Command go
  where
    go (DaemonReload sc) = proc "systemctl" (scopeArgs sc <> ["daemon-reload"])
    go (Enable sc u) = proc "systemctl" (scopeArgs sc <> ["enable", Text.unpack u])
    go (Up sc u) = proc "systemctl" (scopeArgs sc <> ["restart", Text.unpack u])
    go (Stop sc u) = proc "systemctl" (scopeArgs sc <> ["stop", Text.unpack u])

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

    quoteArg :: Text -> Text
    quoteArg a
        | Text.any (`elem` (" \t\"'$`\\" :: String)) a =
            "\"" <> Text.replace "\"" "\\\"" (Text.replace "\\" "\\\\" a) <> "\""
        | otherwise = a

    render_restart :: RestartDirective -> Text
    render_restart OnFailure = "on-failure"

    render_killmode :: KillMode -> Text
    render_killmode Process = "process"

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
