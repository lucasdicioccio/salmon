{- | nginx as a reverse-proxy load balancer, configured purely from a list of
DNS-name -> upstream(s) mappings — one @server{}@ block per name, proxying
to a load-balanced upstream group.
-}
module Salmon.Builtin.Nodes.Nginx where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Reporter

import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))

-------------------------------------------------------------------------------

type ServerName = Text

data Upstream
    = Upstream
    { upstream_host :: Text
    , upstream_port :: Int
    }
    deriving (Show)

data Scheme
    = Http
    | Https
    deriving (Show)

schemeTxt :: Scheme -> Text
schemeTxt Http = "http"
schemeTxt Https = "https"

-- | One DNS name routed to a (possibly load-balanced) set of backends.
data VHost
    = VHost
    { vhost_server_name :: ServerName
    , vhost_listen_port :: Int
    , vhost_upstream_scheme :: Scheme
    , vhost_upstreams :: [Upstream]
    }

data NginxConfig
    = NginxConfig
    { nginx_sites_dir :: FilePath
    -- ^ e.g. \/etc\/nginx\/conf.d
    , nginx_vhosts :: [VHost]
    }

vhostFileName :: VHost -> FilePath
vhostFileName v = Text.unpack v.vhost_server_name <> ".conf"

-------------------------------------------------------------------------------

configFiles :: NginxConfig -> Op
configFiles cfg =
    op "nginx-config" (deps $ fmap vhostFile cfg.nginx_vhosts) id
  where
    vhostFile v = FS.filecontents $ FS.FileContents (cfg.nginx_sites_dir </> vhostFileName v) (renderVHost v)

renderVHost :: VHost -> Text
renderVHost v =
    Text.unlines $
        mconcat
            [
                [ "upstream " <> upstreamName <> " {"
                ]
            , fmap renderUpstream v.vhost_upstreams
            ,
                [ "}"
                , ""
                , "server {"
                , "    listen " <> Text.pack (show v.vhost_listen_port) <> ";"
                , "    server_name " <> v.vhost_server_name <> ";"
                , ""
                , "    location / {"
                , "        proxy_pass " <> schemeTxt v.vhost_upstream_scheme <> "://" <> upstreamName <> ";"
                , "        proxy_set_header Host $host;"
                , "        proxy_set_header X-Real-IP $remote_addr;"
                , "        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
                , "        proxy_set_header X-Forwarded-Proto $scheme;"
                , "    }"
                , "}"
                ]
            ]
  where
    upstreamName :: Text
    upstreamName = "upstream_" <> Text.replace "." "_" v.vhost_server_name

    renderUpstream :: Upstream -> Text
    renderUpstream u = mconcat ["    server ", u.upstream_host, ":", Text.pack (show u.upstream_port), ";"]

-------------------------------------------------------------------------------

-- | Installs nginx, renders every vhost's config, and reloads the (package-shipped) service.
setup :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> Track' (Binary "nginx") -> NginxConfig -> Op
setup r systemctl nginxBin cfg =
    Systemd.restartService r systemctl "nginx.service"
        `inject` configFiles cfg
        `inject` justInstall nginxBin
