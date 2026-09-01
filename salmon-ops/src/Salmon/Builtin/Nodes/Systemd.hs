module Salmon.Builtin.Nodes.Systemd where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))

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
                op "systemd-service" (deps [configContents, run t cfg]) $ \actions ->
                    actions
                        { help = "installs a systemd-unit and up it"
                        , ref = mkRef "systemd-unit" cfg.config_target
                        , up = reload >> enable >> up
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
    deriving (Show)

callSystemctl :: Command "systemctl" SystemCtlCall
callSystemctl = Command go
  where
    go (DaemonReload sc) = proc "systemctl" (scopeArgs sc <> ["daemon-reload"])
    go (Enable sc u) = proc "systemctl" (scopeArgs sc <> ["enable", Text.unpack u])
    go (Up sc u) = proc "systemctl" (scopeArgs sc <> ["restart", Text.unpack u])

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

data Restart
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
    , service_restart :: Restart
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

    render_restart :: Restart -> Text
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
