{- | HAProxy as a TCP router in front of a Patroni cluster: one port that
reaches whichever member is the leader right now, one that spreads over the
replicas (@specs\/pg-patroni.md@, "Routing to the leader", option 3).

The shape is "Salmon.Builtin.Nodes.Nginx"'s -- a config value, a pure
renderer, a systemd unit -- and the rule is "Salmon.Builtin.Nodes.Patroni"'s:
__nothing here names a primary__. The configuration lists the /members/, all
of them, under every listener, and what tells them apart is an HTTP health
check against each member's Patroni REST API: @GET \/primary@ answers 200 on
the leader and 503 everywhere else, @GET \/replica@ the other way round. So a
failover is HAProxy's checks moving the traffic, with no reload, no rewrite
and no pass of salmon involved; rerunning @run up@ after one changes nothing
(the spec's T3), because nothing this module writes depended on who led.

Deliberately decoupled from the Patroni module, like
"Salmon.Builtin.Nodes.PgBouncer" is from Postgres: a 'Member' is a name, an
address and two ports, since the router usually runs on a machine that is not
a member at all.

What a change to the configuration does is a __restart__ (the file is one of
'Systemd.systemdServiceWatching'\'s watched files), which drops the
connections in flight. That is the price of adding or removing a member, and
only of that: the leader moving is never a configuration change.
-}
module Salmon.Builtin.Nodes.Haproxy where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track
import Salmon.Reporter

import Control.Exception (Exception, throwIO)
import qualified Data.Char as Char
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))

-------------------------------------------------------------------------------

{- | A machine that may be the leader or a replica at any given moment. Which
it is appears nowhere in this value.
-}
data Member
    = Member
    { member_name :: Text
    -- ^ HAProxy's server name: letters, digits, @_ - . :@
    , member_host :: Text
    , member_pg_port :: Int
    -- ^ where traffic is sent: Postgres, or a PgBouncer in front of it
    , member_rest_port :: Int
    -- ^ where the member is asked what it is: Patroni's REST API, 8008 by default
    }
    deriving (Show, Eq)

{- | What a listener asks each member. The constructors are Patroni's own
endpoints; each answers 200 when the member is what the name says and 503
otherwise.
-}
data Role
    = -- | @GET \/primary@: the member holding the leader lock
      Primary
    | -- | @GET \/replica@: a running replica not tagged @noloadbalance@
      Replica
    | -- | @GET \/replica?lag=BYTES@: a replica at most that far behind
      ReplicaWithin Int
    | -- | @GET \/read-only@: any member that can serve reads, the leader included
      ReadOnly
    deriving (Show, Eq)

checkPath :: Role -> Text
checkPath Primary = "/primary"
checkPath Replica = "/replica"
checkPath (ReplicaWithin bytes) = "/replica?lag=" <> tshow bytes
checkPath ReadOnly = "/read-only"

-- | One client-facing port, and the question that decides who is behind it.
data Listener
    = Listener
    { listener_name :: Text
    , listener_bind_addr :: Text
    -- ^ @*@ for every address
    , listener_port :: Int
    , listener_role :: Role
    }
    deriving (Show, Eq)

-- | How often a member is asked, and how many answers change HAProxy's mind.
data Checks
    = Checks
    { checks_interval_seconds :: Int
    , checks_fall :: Int
    -- ^ consecutive failures before a member is taken out
    , checks_rise :: Int
    -- ^ consecutive successes before it is put back
    , checks_timeout_seconds :: Int
    }
    deriving (Show, Eq)

{- | Three seconds, three failures, two successes: what Patroni's own
documentation ships. The time from a leader losing its lock to HAProxy
closing its sessions is @interval * fall@, and it should stay under Patroni's
@ttl@ (30s by default) so that the router is never the slower of the two.
-}
defaultChecks :: Checks
defaultChecks = Checks 3 3 2 5

data HaproxyConfig
    = HaproxyConfig
    { haproxy_config_dir :: FilePath
    -- ^ e.g. \/etc\/haproxy
    , haproxy_listeners :: [Listener]
    , haproxy_members :: [Member]
    -- ^ every member, behind every listener; the checks do the sorting
    , haproxy_checks :: Checks
    , haproxy_maxconn :: Int
    -- ^ process-wide and per member; keep it under what the members accept
    , haproxy_connect_timeout_seconds :: Int
    , haproxy_idle_timeout_seconds :: Int
    -- ^ how long a silent client or server is kept; a pooled connection is
    -- silent for long stretches, so this is minutes, not seconds
    , haproxy_stats :: Maybe (Text, Int)
    -- ^ bind address and port of the read-only status page, if wanted
    , haproxy_user :: Text
    -- ^ who the process runs as. The Debian package creates @haproxy@; a
    -- listener on a port below 1024 needs somebody who may bind it.
    , haproxy_group :: Text
    }
    deriving (Show, Eq)

configPath :: HaproxyConfig -> FilePath
configPath cfg = cfg.haproxy_config_dir </> "haproxy.cfg"

{- | The usual pair: a leader port and a replica port over the same members,
in Debian's layout. 5000 and 5001 are the ports Patroni's documentation uses.
-}
patroniRouter :: [Member] -> HaproxyConfig
patroniRouter members =
    HaproxyConfig
        { haproxy_config_dir = "/etc/haproxy"
        , haproxy_listeners =
            [ Listener "primary" "*" 5000 Primary
            , Listener "replicas" "*" 5001 Replica
            ]
        , haproxy_members = members
        , haproxy_checks = defaultChecks
        , haproxy_maxconn = 100
        , haproxy_connect_timeout_seconds = 4
        , haproxy_idle_timeout_seconds = 1800
        , haproxy_stats = Nothing
        , haproxy_user = "haproxy"
        , haproxy_group = "haproxy"
        }

-------------------------------------------------------------------------------

{- | Everything HAProxy would refuse to start on, or would start on and route
wrongly, collected rather than first-found. Empty means renderable.
-}
validate :: HaproxyConfig -> [Text]
validate cfg =
    mconcat
        [ ["no listeners" | null cfg.haproxy_listeners]
        , ["no members" | null cfg.haproxy_members]
        , ["listener name " <> quoted n <> " is not a valid proxy name" | n <- listenerNames, not (validName n)]
        , ["member name " <> quoted n <> " is not a valid server name" | n <- memberNames, not (validName n)]
        , ["listener name " <> quoted n <> " is declared more than once" | n <- duplicates listenerNames]
        , ["listener name \"stats\" is taken by the status page" | Just _ <- [cfg.haproxy_stats], "stats" `elem` listenerNames]
        , ["member name " <> quoted n <> " is declared more than once" | n <- duplicates memberNames]
        , ["port " <> tshow p <> " is bound more than once" | p <- duplicates boundPorts]
        , ["port " <> tshow p <> " is not a port" | p <- boundPorts <> memberPorts, p < 1 || p > 65535]
        , ["bind address " <> quoted a <> " is not one word" | a <- bindAddrs, not (oneWord a)]
        , ["member " <> quoted m.member_name <> " has host " <> quoted m.member_host <> ", which is not one word" | m <- cfg.haproxy_members, not (oneWord m.member_host)]
        , ["a lag bound cannot be negative" | l <- cfg.haproxy_listeners, ReplicaWithin b <- [l.listener_role], b < 0]
        , ["checks need a positive interval, fall, rise and timeout" | any (< 1) [k.checks_interval_seconds, k.checks_fall, k.checks_rise, k.checks_timeout_seconds]]
        , ["maxconn and the timeouts must be positive" | any (< 1) [cfg.haproxy_maxconn, cfg.haproxy_connect_timeout_seconds, cfg.haproxy_idle_timeout_seconds]]
        ]
  where
    k = cfg.haproxy_checks
    listenerNames = fmap (.listener_name) cfg.haproxy_listeners
    memberNames = fmap (.member_name) cfg.haproxy_members
    boundPorts = fmap (.listener_port) cfg.haproxy_listeners <> maybe [] (\(_, p) -> [p]) cfg.haproxy_stats
    memberPorts = concat [[m.member_pg_port, m.member_rest_port] | m <- cfg.haproxy_members]
    bindAddrs = fmap (.listener_bind_addr) cfg.haproxy_listeners <> maybe [] (\(a, _) -> [a]) cfg.haproxy_stats
    quoted t = "\"" <> t <> "\""
    duplicates :: (Ord a) => [a] -> [a]
    duplicates xs = [x | (x : _ : _) <- group (sort xs)]
    validName n = not (Text.null n) && Text.all (\c -> Char.isAscii c && (Char.isAlphaNum c || c `elem` ("_-.:" :: String))) n
    oneWord t = not (Text.null t) && not (Text.any (\c -> Char.isSpace c || c == '#') t)

newtype InvalidConfig = InvalidConfig [Text]
    deriving (Show)

instance Exception InvalidConfig

{- | @haproxy.cfg@. Pure, and a function of the member /set/ only: the same
text before and after any number of failovers.
-}
renderConfig :: HaproxyConfig -> Text
renderConfig cfg =
    Text.unlines $
        mconcat
            [
                [ "# written by salmon (Salmon.Builtin.Nodes.Haproxy); edits are overwritten"
                , "global"
                , "    maxconn " <> tshow cfg.haproxy_maxconn
                , "    log stdout format short daemon"
                , ""
                , "defaults"
                , "    log global"
                , "    mode tcp"
                , "    retries 2"
                , "    timeout connect " <> tshow cfg.haproxy_connect_timeout_seconds <> "s"
                , "    timeout client " <> tshow cfg.haproxy_idle_timeout_seconds <> "s"
                , "    timeout server " <> tshow cfg.haproxy_idle_timeout_seconds <> "s"
                , "    timeout check " <> tshow k.checks_timeout_seconds <> "s"
                ]
            , maybe [] renderStats cfg.haproxy_stats
            , concatMap renderListener cfg.haproxy_listeners
            ]
  where
    k = cfg.haproxy_checks

    renderStats :: (Text, Int) -> [Text]
    renderStats (addr, port) =
        [ ""
        , "listen stats"
        , "    mode http"
        , "    bind " <> addr <> ":" <> tshow port
        , "    stats enable"
        , "    stats uri /"
        ]

    renderListener :: Listener -> [Text]
    renderListener l =
        [ ""
        , "listen " <> l.listener_name
        , "    bind " <> l.listener_bind_addr <> ":" <> tshow l.listener_port
        , "    option httpchk GET " <> checkPath l.listener_role
        , "    http-check expect status 200"
        , -- shutdown-sessions is what makes a demoted leader's clients
          -- reconnect (and land on the new one) rather than keep writing
          -- into a server that now refuses them.
          "    default-server inter "
            <> tshow k.checks_interval_seconds
            <> "s fall "
            <> tshow k.checks_fall
            <> " rise "
            <> tshow k.checks_rise
            <> " on-marked-down shutdown-sessions"
        ]
            <> balance l.listener_role
            <> fmap renderMember cfg.haproxy_members

    -- one leader at most, so there is nothing to balance behind 'Primary'
    balance :: Role -> [Text]
    balance Primary = []
    balance _ = ["    balance leastconn"]

    renderMember :: Member -> Text
    renderMember m =
        mconcat
            [ "    server "
            , m.member_name
            , " "
            , m.member_host
            , ":"
            , tshow m.member_pg_port
            , " maxconn "
            , tshow cfg.haproxy_maxconn
            , " check port "
            , tshow m.member_rest_port
            ]

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-------------------------------------------------------------------------------

{- | Renders @haproxy.cfg@. A configuration 'validate' objects to is not
written: the node throws every objection instead, since a file HAProxy
refuses is a router that stays down at its next restart.
-}
configFiles :: HaproxyConfig -> Op
configFiles cfg = case validate cfg of
    [] -> op "haproxy-config" (deps [cfgFile]) id
    problems ->
        op "haproxy-config" nodeps $ \actions ->
            actions
                { help = "refuses an invalid haproxy configuration"
                , notes = problems
                , ref = mkRef "haproxy-invalid-config" (configPath cfg)
                , up = throwIO (InvalidConfig problems)
                }
  where
    cfgFile = FS.filecontents $ FS.FileContents (configPath cfg) (renderConfig cfg)

{- | Installs haproxy, renders its config, and runs it as a systemd service.

The unit is salmon's own, written to @\/etc\/systemd\/system@ where it takes
precedence over the one the Debian package ships, for the same reason
'Salmon.Builtin.Nodes.PgBouncer.setup' writes one: the config file is a
watched file of 'Systemd.systemdServiceWatching', so a changed member list
reaches the running process (by a restart) instead of sitting on disk. The
process runs in the foreground in master-worker mode (@-W -db@) as
'haproxy_user', with no chroot and no privilege drop of its own.
-}
setup :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> Track' (Binary "haproxy") -> HaproxyConfig -> Op
setup r systemctl haproxyBin cfg =
    Systemd.systemdServiceWatching [configPath cfg] r systemctl trackConfig systemdCfg
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \_ -> op "haproxy-setup" (deps [configFiles cfg, justInstall haproxyBin]) id

    systemdCfg :: Systemd.Config
    systemdCfg = Systemd.Config Systemd.System "/etc/systemd/system" "haproxy.service" unit svc install

    unit :: Systemd.Unit
    unit = Systemd.Unit "HAProxy (from Salmon)" "network-online.target"

    svc :: Systemd.Service
    svc = Systemd.Service Systemd.Simple cfg.haproxy_user cfg.haproxy_group "0022" start Systemd.OnFailure Systemd.Process cfg.haproxy_config_dir

    start :: Systemd.Start
    start = Systemd.Start "/usr/sbin/haproxy" ["-W", "-db", "-f", Text.pack (configPath cfg)]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"
