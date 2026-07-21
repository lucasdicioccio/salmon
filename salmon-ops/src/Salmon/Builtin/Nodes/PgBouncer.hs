{- | pgbouncer, configured to sit in front of one or more upstream Postgres
databases and pool connections for a set of client-facing users.

Deliberately decoupled from "Salmon.Builtin.Nodes.Postgres": callers pass
plain host\/port\/dbname\/user\/password values, so this module doesn't need
to know anything about how the upstream cluster/roles were provisioned (they
may not even be managed by salmon on this machine, e.g. a remote replica).
-}
module Salmon.Builtin.Nodes.PgBouncer where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Track
import Salmon.Reporter

import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process (readProcess)

-------------------------------------------------------------------------------

type DbAlias = Text

-- | The upstream Postgres database a client-facing alias is pooled against.
data UpstreamDb
    = UpstreamDb
    { upstream_host :: Text
    , upstream_port :: Int
    , upstream_dbname :: Text
    }
    deriving (Show)

data BouncerDatabase
    = BouncerDatabase
    { bouncer_db_alias :: DbAlias
    -- ^ the name clients connect to through pgbouncer
    , bouncer_db_upstream :: UpstreamDb
    }
    deriving (Show)

-- | A client-facing user allowed to authenticate against pgbouncer.
data AuthUser
    = AuthUser
    { auth_user :: Text
    , auth_password :: Text
    -- ^ cleartext; only ever touches disk already-hashed, in the auth_file
    }

data PoolMode
    = SessionPooling
    | TransactionPooling
    | StatementPooling
    deriving (Show)

poolModeTxt :: PoolMode -> Text
poolModeTxt SessionPooling = "session"
poolModeTxt TransactionPooling = "transaction"
poolModeTxt StatementPooling = "statement"

data BouncerConfig
    = BouncerConfig
    { bouncer_config_dir :: FilePath
    -- ^ e.g. \/etc\/pgbouncer
    , bouncer_listen_addr :: Text
    , bouncer_listen_port :: Int
    , bouncer_databases :: [BouncerDatabase]
    , bouncer_users :: [AuthUser]
    , bouncer_pool_mode :: PoolMode
    , bouncer_max_client_conn :: Int
    , bouncer_default_pool_size :: Int
    }

configPath :: BouncerConfig -> FilePath
configPath cfg = cfg.bouncer_config_dir </> "pgbouncer.ini"

userlistPath :: BouncerConfig -> FilePath
userlistPath cfg = cfg.bouncer_config_dir </> "userlist.txt"

-------------------------------------------------------------------------------

-- | Renders @pgbouncer.ini@ and the @auth_file@ (@userlist.txt@, md5-hashed passwords).
configFiles :: BouncerConfig -> Op
configFiles cfg =
    op "pgbouncer-config" (deps [iniFile, userlistFile]) id
  where
    iniFile = FS.filecontents $ FS.FileContents (configPath cfg) (renderIni cfg)
    userlistFile = FS.filecontents $ FS.FileContents (userlistPath cfg) (renderUserlist cfg.bouncer_users)

renderIni :: BouncerConfig -> Text
renderIni cfg =
    Text.unlines $
        mconcat
            [
                [ "[databases]"
                ]
            , fmap renderDb cfg.bouncer_databases
            ,
                [ ""
                , "[pgbouncer]"
                , "listen_addr = " <> cfg.bouncer_listen_addr
                , "listen_port = " <> Text.pack (show cfg.bouncer_listen_port)
                , "auth_type = md5"
                , "auth_file = " <> Text.pack (userlistPath cfg)
                , "pool_mode = " <> poolModeTxt cfg.bouncer_pool_mode
                , "max_client_conn = " <> Text.pack (show cfg.bouncer_max_client_conn)
                , "default_pool_size = " <> Text.pack (show cfg.bouncer_default_pool_size)
                ]
            ]
  where
    renderDb :: BouncerDatabase -> Text
    renderDb db =
        mconcat
            [ db.bouncer_db_alias
            , " = host="
            , db.bouncer_db_upstream.upstream_host
            , " port="
            , Text.pack (show db.bouncer_db_upstream.upstream_port)
            , " dbname="
            , db.bouncer_db_upstream.upstream_dbname
            ]

-- | @"user" "md5<hex(md5(password<>username))>"@ per entry, one per line —
-- pgbouncer's own md5 @auth_file@ format (matching Postgres's md5 auth scheme).
renderUserlist :: [AuthUser] -> IO Text
renderUserlist users = Text.unlines <$> mapM renderUser users
  where
    renderUser :: AuthUser -> IO Text
    renderUser u = do
        h <- md5AuthHash u.auth_password u.auth_user
        pure $ mconcat ["\"", u.auth_user, "\" \"", h, "\""]

md5AuthHash :: Text -> Text -> IO Text
md5AuthHash password username = do
    out <- readProcess "openssl" ["dgst", "-md5", "-r"] (Text.unpack (password <> username))
    pure $ "md5" <> Text.takeWhile (/= ' ') (Text.pack out)

-------------------------------------------------------------------------------

-- | Installs pgbouncer, renders its config, and runs it as a systemd service.
setup :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> Track' (Binary "pgbouncer") -> BouncerConfig -> Op
setup r systemctl pgbouncerBin cfg =
    Systemd.systemdService r systemctl trackConfig systemdCfg
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \_ -> op "pgbouncer-setup" (deps [configFiles cfg, justInstall pgbouncerBin]) id

    systemdCfg :: Systemd.Config
    systemdCfg = Systemd.Config "pgbouncer.service" unit svc install

    unit :: Systemd.Unit
    unit = Systemd.Unit "PgBouncer (from Salmon)" "network-online.target"

    svc :: Systemd.Service
    svc = Systemd.Service Systemd.Simple "postgres" "postgres" "0022" start Systemd.OnFailure Systemd.Process cfg.bouncer_config_dir

    start :: Systemd.Start
    start = Systemd.Start "/usr/sbin/pgbouncer" [Text.pack (configPath cfg)]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"
