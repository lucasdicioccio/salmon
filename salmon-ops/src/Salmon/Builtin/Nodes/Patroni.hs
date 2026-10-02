{-# LANGUAGE OverloadedStrings #-}

{- | Patroni, as one member of a cluster: its config, its systemd unit, Debian's
own per-cluster Postgres units masked out of its way, and a check that asks
Patroni's REST API rather than the disk (@specs\/pg-patroni.md@).

__Patroni owns Postgres here.__ Nothing in this module starts, stops, promotes
or configures a Postgres cluster, and nothing in it names a primary: no node's
@ref@, @help@ or @notes@ says which member leads, because that is Patroni's
decision and a declaration that disagreed with it would be fighting its owner.
Which member is the leader is reported by the check (in a failure's text,
never as a verdict) and nothing more.

__Layout.__ Postgresql-common's, as decided for this builtin: the data
directory is @\/var\/lib\/postgresql\/V\/CLUSTER@, the configuration lives in
@\/etc\/postgresql\/V\/CLUSTER@ (Patroni writes @postgresql.conf@ and
@pg_hba.conf@ there and starts the server with that file), binaries are
@\/usr\/lib\/postgresql\/V\/bin@. What this module does /not/ do is
@pg_createcluster@: the data directory must be absent so that Patroni can
@initdb@ (or clone) it. The @postgresql\@V-CLUSTER@ unit that Debian ships for
that name is __masked__ ('maskedUnit'), since it would otherwise start the same
data directory behind Patroni's back at boot.

__Configuration is a directory.__ Patroni is started on 'pat_config_dir' and
loads every @*.yml@ in it. @patroni.yml@ is rendered here, as a 'Text' (so its
content fingerprint reaches @notes@ and a re-declaration is @Stale@ under
@run serve@). The credentials are __not__ in it: they live in a
pre-provisioned file in the same directory ('pat_secrets_file', a YAML
fragment carrying @restapi.authentication@ and @postgresql.authentication@),
which this builtin only /owns/ (@chown@ to the service user, mode @0600@) and
/watches/ (a change restarts the service), never writes -- a recipe must not
bake a secret transport into a builtin. Etcd is the v3 API only, with its TLS
files as pre-provisioned paths.

__The check__ is @GET \/patroni@ and @GET \/health@ on the member's own REST
address ('interpretMember'): @running@ with a healthy @\/health@ is satisfied,
a transitional state (@starting@, @restarting@, ...) or an uninitialised member
is @Unknown@ (not yet gone, so nothing should be bounced), anything else, or
an API that does not answer, is a failure. The role is reported and never
judged: a replica is as healthy as a leader. @pending_restart@ is likewise
parsed and /not/ judged -- whether a pending restart is the member's problem is
the cluster-wide config node's question, not this one's. The REST API is
spoken over plain HTTP; the unauthenticated @GET@s are all this needs.
-}
module Salmon.Builtin.Nodes.Patroni where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (when)
import Data.Char (isAlphaNum)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, encode, withObject, (.!=), (.:?))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (
    HttpException,
    defaultManagerSettings,
    httpLbs,
    managerResponseTimeout,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
    responseTimeoutMicro,
 )
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified System.Posix.Types as Posix

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import Salmon.Builtin.Nodes.Etcd (TlsFiles (..))
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data PatroniConfig
    = PatroniConfig
    { pat_scope :: Text
    -- ^ the cluster's name, shared by every member
    , pat_namespace :: Text
    -- ^ the DCS key prefix, e.g. @\/service\/@
    , pat_name :: Text
    -- ^ this member's name, unique in the scope
    , pat_etcd_hosts :: [Text]
    -- ^ @host:port@ of the etcd members (v3 API only)
    , pat_etcd_tls :: Maybe TlsFiles
    -- ^ client CA, certificate and key, as pre-provisioned paths; 'Nothing' speaks plain HTTP
    , pat_rest_listen :: Text
    -- ^ @host:port@ the REST API binds, e.g. @0.0.0.0:8008@
    , pat_rest_connect_address :: Text
    -- ^ @host:port@ other members and this node's own check reach it by
    , pat_pg_version :: Text
    -- ^ the Debian major version, e.g. @16@
    , pat_pg_cluster :: Text
    -- ^ the postgresql-common cluster name, e.g. @main@
    , pat_pg_listen :: Text
    -- ^ @host:port@ Postgres binds
    , pat_pg_connect_address :: Text
    -- ^ @host:port@ clients and other members reach this Postgres by
    , pat_pg_hba :: [Text]
    -- ^ @pg_hba.conf@ lines, the whole file, written by Patroni on every member
    , pat_bootstrap_parameters :: [(Text, Text)]
    -- ^ @postgresql.parameters@ at /bootstrap/ only; afterwards the DCS is
    -- owned by whoever patches @\/config@
    , pat_config_dir :: FilePath
    -- ^ the directory Patroni is started on, e.g. @\/etc\/patroni@
    , pat_secrets_file :: FilePath
    -- ^ the pre-provisioned credentials fragment; must be inside 'pat_config_dir'
    , pat_user :: Text
    -- ^ the system user the unit runs as, normally @postgres@
    , pat_binary :: FilePath
    , pat_ready_timeout_seconds :: Int
    -- ^ how long @up@ waits for the check to pass
    , pat_archive :: Maybe Archive
    -- ^ a continuous archive to build replicas from and to archive WAL into; 'Nothing' is base backups from the leader only
    }
    deriving (Show)

{- | A continuous WAL archive, as far as Patroni is concerned: commands, as one
shell string each. Which tool they belong to is not this module's business
("Salmon.Builtin.Nodes.PgBackRest" makes one with @patroniArchive@), and no
credential is in them.
-}
data Archive
    = Archive
    { arc_methods :: [ReplicaMethod]
    -- ^ @create_replica_methods@, tried in order
    , arc_basebackup_fallback :: Bool
    -- ^ end the list with Patroni's own @basebackup@, for an archive that holds no backup yet
    , arc_archive_command :: Text
    , arc_restore_command :: Text
    , arc_bootstrap :: Maybe BootstrapMethod
    -- ^ make a /new/ cluster's first member from the archive instead of @initdb@
    }
    deriving (Eq, Show)

data ReplicaMethod
    = ReplicaMethod
    { rm_name :: Text
    , rm_command :: Text
    , rm_keep_data :: Bool
    -- ^ Patroni leaves the data directory in place for the command (a delta restore reuses it)
    , rm_no_params :: Bool
    -- ^ Patroni appends no @--scope@, @--datadir@, ... of its own
    }
    deriving (Eq, Show)

-- | A custom bootstrap: the command leaves a data directory with its own recovery settings, which Patroni keeps.
data BootstrapMethod
    = BootstrapMethod
    { bm_name :: Text
    , bm_command :: Text
    }
    deriving (Eq, Show)

-- | @wal_log_hints@ is restart-only and what @pg_rewind@ needs, so it has to be set before there is data to lose.
defaultBootstrapParameters :: [(Text, Text)]
defaultBootstrapParameters = [("wal_log_hints", "on")]

pgDataDir, pgConfigDir, pgBinDir :: PatroniConfig -> FilePath
pgDataDir c = "/var/lib/postgresql" </> Text.unpack c.pat_pg_version </> Text.unpack c.pat_pg_cluster
pgConfigDir c = "/etc/postgresql" </> Text.unpack c.pat_pg_version </> Text.unpack c.pat_pg_cluster
pgBinDir c = "/usr/lib/postgresql" </> Text.unpack c.pat_pg_version </> "bin"

patroniFile :: PatroniConfig -> FilePath
patroniFile c = c.pat_config_dir </> "patroni.yml"

-- | Debian's per-cluster unit for the cluster Patroni manages; the one to keep out of the way.
debianClusterUnit :: PatroniConfig -> Text
debianClusterUnit c = "postgresql@" <> c.pat_pg_version <> "-" <> c.pat_pg_cluster <> ".service"

-------------------------------------------------------------------------------

-- | A tiny YAML tree: ordered maps, so a render is stable and readable (JSON-quoted scalars are valid YAML).
data Y = YS Text | YI Int | YB Bool | YM [(Text, Y)] | YL [Y]

renderYaml :: Y -> Text
renderYaml y = Text.unlines (case y of YM kvs -> block 0 kvs; _ -> [scalar y])

block :: Int -> [(Text, Y)] -> [Text]
block n = concatMap entry
  where
    pad = Text.replicate n " "
    entry (k, v) = case v of
        YM [] -> [pad <> key k <> ": {}"]
        YL [] -> [pad <> key k <> ": []"]
        YM kvs -> (pad <> key k <> ":") : block (n + 2) kvs
        YL xs -> (pad <> key k <> ":") : [pad <> "  - " <> scalar x | x <- xs]
        _ -> [pad <> key k <> ": " <> scalar v]
    key k
        | Text.all (\ch -> ch `elem` ("_-" :: String) || isAlphaNum ch) k = k
        | otherwise = scalar (YS k)

scalar :: Y -> Text
scalar (YS t) = Text.decodeUtf8 (LByteString.toStrict (encode t))
scalar (YI i) = Text.pack (show i)
scalar (YB b) = if b then "true" else "false"
scalar (YM _) = "{}"
scalar (YL _) = "[]"

-- | @patroni.yml@, without any credential. Nothing in it names a primary.
renderPatroni :: PatroniConfig -> Text
renderPatroni c =
    renderYaml . YM $
        [ ("scope", YS c.pat_scope)
        , ("namespace", YS c.pat_namespace)
        , ("name", YS c.pat_name)
        ,
            ( "restapi"
            , YM [("listen", YS c.pat_rest_listen), ("connect_address", YS c.pat_rest_connect_address)]
            )
        , ("etcd3", YM etcd)
        ,
            ( "bootstrap"
            , YM $
                [
                    ( "dcs"
                    , YM
                        [ ("ttl", YI 30)
                        , ("loop_wait", YI 10)
                        , ("retry_timeout", YI 10)
                        , ("maximum_lag_on_failover", YI 1048576)
                        ,
                            ( "postgresql"
                            , YM
                                [ ("use_pg_rewind", YB True)
                                , ("use_slots", YB True)
                                , ("parameters", YM [(k, YS v) | (k, v) <- c.pat_bootstrap_parameters])
                                ]
                            )
                        ]
                    )
                , ("initdb", YL [YS "data-checksums"])
                ]
                    <> bootstrapMethod
            )
        ,
            ( "postgresql"
            , YM $
                [ ("listen", YS c.pat_pg_listen)
                , ("connect_address", YS c.pat_pg_connect_address)
                , ("data_dir", YS (Text.pack (pgDataDir c)))
                , ("config_dir", YS (Text.pack (pgConfigDir c)))
                , ("bin_dir", YS (Text.pack (pgBinDir c)))
                , ("use_unix_socket", YB True)
                , -- local, not bootstrap: a replica never runs the bootstrap section, and with the
                  -- configuration outside the data directory nothing else would give it a pg_hba.conf
                  ("pg_hba", YL (fmap YS c.pat_pg_hba))
                ]
                    <> maybe [] archive c.pat_archive
            )
        ]
  where
    -- local parameters, not the DCS's: they hold on a cluster bootstrapped
    -- before the archive was declared, and archive_mode (restart-only) then
    -- shows as pending_restart, which is the cluster config node's to act on
    archive :: Archive -> [(Text, Y)]
    archive a =
        [ ("create_replica_methods", YL (fmap (YS . rm_name) a.arc_methods <> [YS "basebackup" | a.arc_basebackup_fallback]))
        ]
            <> [ (m.rm_name, YM [("command", YS m.rm_command), ("keep_data", YB m.rm_keep_data), ("no_params", YB m.rm_no_params)])
               | m <- a.arc_methods
               ]
            <> [ ("recovery_conf", YM [("restore_command", YS a.arc_restore_command)])
               , ("parameters", YM [("archive_mode", YS "on"), ("archive_command", YS a.arc_archive_command)])
               ]
    bootstrapMethod = case c.pat_archive >>= arc_bootstrap of
        Nothing -> []
        Just b ->
            [ ("method", YS b.bm_name)
            ,
                ( b.bm_name
                , YM
                    [ ("command", YS b.bm_command)
                    , ("keep_existing_recovery_conf", YB True)
                    , ("no_params", YB True)
                    ]
                )
            ]
    etcd =
        [("hosts", YS (Text.intercalate "," c.pat_etcd_hosts))]
            <> maybe
                []
                ( \t ->
                    [ ("protocol", YS "https")
                    , ("cacert", YS (Text.pack t.tls_ca))
                    , ("cert", YS (Text.pack t.tls_cert))
                    , ("key", YS (Text.pack t.tls_key))
                    ]
                )
                c.pat_etcd_tls

unitConfig :: PatroniConfig -> Systemd.Config
unitConfig c =
    Systemd.Config Systemd.System "/etc/systemd/system" "patroni.service" unit svc install
  where
    unit = Systemd.Unit "Patroni (from Salmon)" "network-online.target"
    svc =
        Systemd.Service
            Systemd.Simple
            c.pat_user
            c.pat_user
            "0022"
            (Systemd.Start c.pat_binary [Text.pack c.pat_config_dir])
            Systemd.OnFailure
            -- Process: restarting Patroni must not take Postgres down with it
            Systemd.Process
            "/var/lib/postgresql"
    install = Systemd.Install "multi-user.target"

-------------------------------------------------------------------------------

-- | What @GET \/patroni@ says, as far as this module reads it.
data PatroniStatus
    = PatroniStatus
    { ps_state :: Maybe Text
    , ps_role :: Maybe Text
    , ps_scope :: Maybe Text
    , ps_pending_restart :: Bool
    }
    deriving (Eq, Show)

instance FromJSON PatroniStatus where
    parseJSON = withObject "patroni status" $ \o -> do
        pat <- o .:? "patroni"
        scope <- maybe (pure Nothing) (withObject "patroni" (.:? "scope")) pat
        PatroniStatus <$> o .:? "state" <*> o .:? "role" <*> pure scope <*> o .:? "pending_restart" .!= False

parseStatus :: ByteString.ByteString -> Either Text PatroniStatus
parseStatus = either (Left . Text.pack) Right . eitherDecodeStrict

transitional :: [Text]
transitional =
    [ "starting"
    , "restarting"
    , "stopping"
    , "creating replica"
    , "initializing new cluster"
    , "running custom bootstrap script"
    , "starting after custom bootstrap"
    ]

{- | The verdict, pure: the scope this member is declared in, the HTTP status
of @GET \/health@ and the @GET \/patroni@ body.

Role is never an input to the verdict (@uninitialised@ aside, which says
Patroni has not made this member part of a cluster yet).
-}
interpretMember :: Text -> Int -> ByteString.ByteString -> CheckResult
interpretMember scope health body = case parseStatus body of
    Left e -> Failure ("GET /patroni not understood: " <> Text.take 120 e)
    Right st
        | Just s <- st.ps_scope, s /= scope -> Failure ("member reports scope " <> s <> ", declared " <> scope)
        | st.ps_role == Just "uninitialized" -> Unknown
        | otherwise -> case st.ps_state of
            Nothing -> Failure "GET /patroni carries no state"
            Just "running"
                | health == 200 -> Success
                | otherwise -> Failure ("state is running but GET /health answers " <> Text.pack (show health))
            Just s
                | s `elem` transitional -> Unknown
                | otherwise -> Failure ("state is " <> s)

-- | Asks the member's own REST API; an API that does not answer is a failure, and says so.
checkMember :: PatroniConfig -> IO CheckResult
checkMember c = do
    r <- try $ do
        mgr <- newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutMicro 5000000}
        let get path = parseRequest (Text.unpack ("http://" <> c.pat_rest_connect_address <> path))
        -- /health is 503 when Postgres is down, which httpLbs reports as a status, not an exception
        hReq <- get "/health"
        h <- httpLbs hReq mgr
        pReq <- get "/patroni"
        p <- httpLbs pReq mgr
        pure (statusCode (responseStatus h), LByteString.toStrict (responseBody p))
    pure $ case r of
        Left (e :: HttpException) -> Failure ("REST API not answering: " <> Text.take 150 (Text.pack (show e)))
        Right (health, body) -> interpretMember c.pat_scope health body

-- | Thrown when the member does not become healthy in time.
newtype NotRunning = NotRunning Text

instance Show NotRunning where
    show (NotRunning why) = "patroni: member did not become healthy: " <> Text.unpack why

instance Exception NotRunning

-- | Polls until the check passes; throws 'NotRunning' with the last reason at the deadline.
waitReady :: PatroniConfig -> IO ()
waitReady c = go (max 1 c.pat_ready_timeout_seconds)
  where
    go :: Int -> IO ()
    go left = do
        v <- checkMember c
        case v of
            Success -> pure ()
            Failure why | left <= 1 -> throwIO (NotRunning why)
            _ -> do
                when (left <= 1) $ throwIO (NotRunning "no verdict")
                threadDelay 1000000
                go (left - 1)

-------------------------------------------------------------------------------

{- | One Patroni member: the binary, the configuration directory, @patroni.yml@,
the credentials file's ownership, Debian's cluster unit masked, the unit, and
on top a node whose @check@ is 'checkMember'.

@down@ stops the service (through the unit node) and leaves Postgres' data
directory alone: it is the member's memory, and whether to destroy it is not a
teardown's decision.
-}
patroniMember ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' (Binary "patroni") ->
    PatroniConfig ->
    Op
patroniMember r systemctl patroniBin c =
    op "patroni-member" (deps [service]) $ \actions ->
        actions
            { help = "a Patroni cluster member, answering on its REST API"
            , notes = ["role is reported, never judged", "etcd v3 only", "Postgres layout: postgresql-common"]
            , ref = mkRef "patroni-member" (c.pat_scope, c.pat_name)
            , check = checkMember c
            , up = waitReady c
            , down = pure ()
            }
  where
    service :: Op
    service = Systemd.systemdServiceWatching [c.pat_secrets_file] r systemctl (Track $ \_ -> prereqs) (unitConfig c)

    prereqs :: Op
    prereqs =
        op
            "patroni-setup"
            (deps [justInstall patroniBin, configFile, secrets, mask, pgConfSeed c])
            id

    configFile = FS.filecontents (FS.FileContents (patroniFile c) (renderPatroni c))

    -- the credentials file is somebody else's to write; failing when it is missing is this node's job
    secrets = FS.ownedFile (FS.FileOwnership c.pat_secrets_file (Just c.pat_user) Nothing (0o600 :: Posix.FileMode))

    mask = Systemd.maskedUnit r systemctl Systemd.System (debianClusterUnit c)

{- | Postgres' configuration directory, the service user's, holding a
@postgresql.conf@ for Patroni to build on.

With the configuration outside the data directory, Patroni does not create
the file it then renames to @postgresql.base.conf@: a bootstrap dies on the
rename, having already run @initdb@, and a new replica likewise. Debian's own
integration gets the file from @pg_createcluster@, which this builtin does not
run. So an empty one is laid down, once: afterwards the file is Patroni's
(either name counts as present) and is never rewritten here.
-}
pgConfSeed :: PatroniConfig -> Op
pgConfSeed c =
    op "patroni-pg-conf-seed" (deps [ownedDir]) $ \actions ->
        actions
            { help = "a postgresql.conf for Patroni to build on, in " <> Text.pack (pgConfigDir c)
            , notes = ["written once, empty; Patroni owns it afterwards"]
            , ref = mkRef "patroni-pg-conf-seed" (pgConfigDir c)
            , check = do
                present <- or <$> traverse (doesFileExist . (pgConfigDir c </>)) ["postgresql.conf", "postgresql.base.conf"]
                pure (if present then Success else Failure "no postgresql.conf for Patroni to build on")
            , up = do
                ByteString.writeFile seed ""
                FS.applyOwnership (FS.FileOwnership seed (Just c.pat_user) Nothing (0o644 :: Posix.FileMode))
            }
  where
    seed = pgConfigDir c </> "postgresql.conf"
    ownedDir =
        FS.ownedFile (FS.FileOwnership (pgConfigDir c) (Just c.pat_user) Nothing (0o755 :: Posix.FileMode))
            `inject` FS.dir (FS.Directory (pgConfigDir c))
