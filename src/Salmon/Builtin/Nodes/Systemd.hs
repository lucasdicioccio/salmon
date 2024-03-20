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
    withCommand DaemonReload $ \reload ->
        withCommand (Enable cfg.config_target) $ \enable ->
            withCommand (Up cfg.config_target) $ \up ->
                op "systemd-service" (deps [configContents, run t cfg]) $ \actions ->
                    actions
                        { help = "installs a systemd-unit and up it"
                        , ref = dotRef $ "systemd:unit:" <> cfg.config_target
                        , up = reload >> enable >> up
                        }
  where
    r' cmd = contramap (CallSystemCtl cmd) r
    withCommand cmd f =
        withBinary (r' cmd) systemctl callSystemctl cmd f
    unitPath :: FilePath
    unitPath = "/etc/systemd/system" </> Text.unpack cfg.config_target

    configContents :: Op
    configContents = filecontents $ FileContents unitPath (render_config cfg)

data SystemCtlCall
    = DaemonReload
    | Enable UnitTarget
    | Up UnitTarget
    deriving (Show)

callSystemctl :: Command "systemctl" SystemCtlCall
callSystemctl = Command go
  where
    go DaemonReload = proc "systemctl" ["daemon-reload"]
    go (Enable u) = proc "systemctl" ["enable", Text.unpack u]
    go (Up u) = proc "systemctl" ["restart", Text.unpack u]

-------------------------------------------------------------------------------

data Config
    = Config
    { config_target :: UnitTarget
    , config_unit :: Unit
    , config_service :: Service
    , config_install :: Install
    }

render_config :: Config -> Text
render_config c =
    Text.unlines
        [ render_unit c.config_unit
        , ""
        , render_service c.config_service
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

render_service :: Service -> Text
render_service s =
    Text.unlines
        [ "[Service]"
        , "Type=" <> render_type s.service_type
        , "User=" <> s.service_user
        , "Group=" <> s.service_group
        , "UMask=" <> s.service_umask
        , "ExecStart=" <> render_start s.service_execStart
        , "Restart=" <> render_restart s.service_restart
        , "KillMode=" <> render_killmode s.service_killmode
        , "WorkingDirectory=" <> Text.pack s.service_working_dir
        ]
  where
    render_type :: ServiceType -> Text
    render_type Simple = "Simple"

    render_start :: Start -> Text
    render_start s = Text.unwords (Text.pack s.start_path : s.start_args)

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
