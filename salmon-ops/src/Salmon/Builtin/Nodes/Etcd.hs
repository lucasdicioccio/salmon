{-# LANGUAGE OverloadedStrings #-}

{- | etcd, as one member of a cluster: its config file, its systemd unit, and
a check that asks the cluster rather than the disk.

Only the v3 API is spoken (@etcdctl@ with @ETCDCTL_API=3@, which is what
Patroni's @etcd3:@ section wants too); there is no v2 option on purpose.
TLS material is taken as /paths/ to pre-provisioned files -- minting
certificates is a recipe's choice, not this builtin's.

__The trap is bootstrap.__ @initial-cluster-state@ is @new@ exactly once in a
cluster's life. This module implements the /seed/ phase: every member of the
declared cluster is started with @new@ and the same @initial-cluster@. What
it must never do is start a member with @new@ against a cluster that already
exists and does not list it -- that member would either refuse to start or,
worse, found a second cluster. 'seedGuard' is the node that stops this: it
asks every other member, and if one answers and its member list does not
contain this member, it throws 'ClusterExists' instead of letting the unit
start. Joining a member to a running cluster (@etcdctl member add@, then
start with @existing@) is the /join/ phase, 'etcdJoinMember': 'joinGuard'
asks the running members, adds this one to the list if it is not there yet,
and the config it writes says @initial-cluster-state: existing@.

A member whose data directory already holds a bootstrapped member (a restart)
is never re-seeded: etcd ignores @initial-cluster*@ once it has data, and the
guard does not even ask.

The cluster-level 'check' compares /membership/ -- the declared peer URLs
against @etcdctl member list@ -- and not the config file, which says what was
intended and not what is.
-}
module Salmon.Builtin.Nodes.Etcd where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (when)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.!=), (.:?))
import qualified Data.ByteString as ByteString
import Data.List (sort, sortOn, (\\))
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), justInstall)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | A member as the cluster declares it. Every member must be listed in every member's config.
data Member
    = Member
    { member_name :: Text
    , member_peer_url :: Text
    -- ^ e.g. @https://10.0.0.1:2380@; identifies the member in the member list
    , member_client_url :: Text
    -- ^ e.g. @https://10.0.0.1:2379@; where @etcdctl@ reaches it
    }
    deriving (Eq, Show)

-- | Paths of pre-provisioned TLS files (the CA that signs the peers, a certificate and its key).
data TlsFiles
    = TlsFiles
    { tls_ca :: FilePath
    , tls_cert :: FilePath
    , tls_key :: FilePath
    }
    deriving (Eq, Show)

data EtcdConfig
    = EtcdConfig
    { etcd_self :: Member
    , etcd_cluster :: [Member]
    -- ^ every member, this one included
    , etcd_cluster_token :: Text
    , etcd_data_dir :: FilePath
    , etcd_config_file :: FilePath
    , etcd_user :: Text
    -- ^ the system user the unit runs as (must be able to read the TLS files and own the data dir)
    , etcd_client_tls :: TlsFiles
    , etcd_peer_tls :: TlsFiles
    , etcd_ready_timeout_seconds :: Int
    -- ^ how long 'etcdMember''s @up@ waits for health; in a seed it must cover the other members coming up
    }
    deriving (Show)

-------------------------------------------------------------------------------

-- | Which phase of a cluster's life a member is started in.
data Phase = Seed | Join
    deriving (Eq, Show)

phaseState :: Phase -> Text
phaseState Seed = "new"
phaseState Join = "existing"

{- | The config file etcd reads with @--config-file@, in the seed phase:
@initial-cluster-state: new@. Harmless on a restart, since etcd ignores it
once the data directory is bootstrapped.
-}
renderConfig :: EtcdConfig -> Text
renderConfig = renderConfigFor Seed

-- | The config for a phase; only @initial-cluster-state@ differs.
renderConfigFor :: Phase -> EtcdConfig -> Text
renderConfigFor phase cfg =
    Text.unlines
        [ "name: " <> self.member_name
        , "data-dir: " <> Text.pack cfg.etcd_data_dir
        , "listen-peer-urls: " <> self.member_peer_url
        , "listen-client-urls: " <> self.member_client_url
        , "advertise-client-urls: " <> self.member_client_url
        , "initial-advertise-peer-urls: " <> self.member_peer_url
        , "initial-cluster: " <> renderInitialCluster cfg.etcd_cluster
        , "initial-cluster-state: " <> phaseState phase
        , "initial-cluster-token: " <> cfg.etcd_cluster_token
        , "client-transport-security:"
        , "  trusted-ca-file: " <> Text.pack cfg.etcd_client_tls.tls_ca
        , "  cert-file: " <> Text.pack cfg.etcd_client_tls.tls_cert
        , "  key-file: " <> Text.pack cfg.etcd_client_tls.tls_key
        , "  client-cert-auth: true"
        , "peer-transport-security:"
        , "  trusted-ca-file: " <> Text.pack cfg.etcd_peer_tls.tls_ca
        , "  cert-file: " <> Text.pack cfg.etcd_peer_tls.tls_cert
        , "  key-file: " <> Text.pack cfg.etcd_peer_tls.tls_key
        , "  client-cert-auth: true"
        ]
  where
    self = cfg.etcd_self

-- | @name=peerurl,...@, in a stable (name) order so equal clusters render equally.
renderInitialCluster :: [Member] -> Text
renderInitialCluster ms =
    Text.intercalate "," [m.member_name <> "=" <> m.member_peer_url | m <- sortOn (.member_name) ms]

unitConfig :: EtcdConfig -> Systemd.Config
unitConfig cfg =
    Systemd.Config Systemd.System "/etc/systemd/system" "etcd.service" unit svc install
  where
    unit = Systemd.Unit "etcd (from Salmon)" "network-online.target"
    svc =
        Systemd.Service
            Systemd.Simple
            cfg.etcd_user
            cfg.etcd_user
            "0027"
            (Systemd.Start "/usr/bin/etcd" ["--config-file", Text.pack cfg.etcd_config_file])
            Systemd.OnFailure
            Systemd.Process
            cfg.etcd_data_dir
    install = Systemd.Install "multi-user.target"

-------------------------------------------------------------------------------

-- | What we ask of @etcdctl@; always against one endpoint, over the client TLS files.
data EtcdctlCall
    = EndpointHealth TlsFiles Text
    | MemberList TlsFiles Text
    | MemberAdd TlsFiles Text Member
    -- ^ against this endpoint, add this member
    deriving (Show)

etcdctl :: Command "etcdctl" EtcdctlCall
etcdctl = Command go
  where
    go (EndpointHealth tls ep) = proc "etcdctl" (common tls ep <> ["endpoint", "health"])
    go (MemberList tls ep) = proc "etcdctl" (common tls ep <> ["member", "list", "-w", "json"])
    go (MemberAdd tls ep m) =
        proc "etcdctl" (common tls ep <> ["member", "add", Text.unpack m.member_name, "--peer-urls=" <> Text.unpack m.member_peer_url])
    common :: TlsFiles -> Text -> [String]
    common tls ep =
        [ "--endpoints=" <> Text.unpack ep
        , "--cacert=" <> tls.tls_ca
        , "--cert=" <> tls.tls_cert
        , "--key=" <> tls.tls_key
        , "--command-timeout=5s"
        ]

-- | One member as @etcdctl member list -w json@ reports it. A member that was added but has not started has no name.
data Listed = Listed {listed_name :: Text, listed_peer_urls :: [Text]}
    deriving (Eq, Show)

newtype MemberList = MemberList' [Listed]
    deriving (Eq, Show)

instance FromJSON Listed where
    parseJSON = withObject "member" $ \o ->
        Listed <$> o .:? "name" .!= "" <*> o .:? "peerURLs" .!= []

instance FromJSON MemberList where
    parseJSON = withObject "member list" $ \o -> MemberList' <$> o .:? "members" .!= []

parseMemberList :: ByteString.ByteString -> Either Text [Listed]
parseMemberList bs = case eitherDecodeStrict bs of
    Left e -> Left (Text.pack e)
    Right (MemberList' ms) -> Right ms

{- | The membership verdict, pure: the declared peer URLs against the listed
ones. Names are not compared (an unstarted member has none); URLs are what
identify a member.
-}
interpretMembers :: [Member] -> [Listed] -> CheckResult
interpretMembers declared listed
    | null missing && null extra = Success
    | otherwise =
        Failure . Text.intercalate "; " $
            ["not in the cluster: " <> Text.intercalate "," missing | not (null missing)]
                <> ["unexpected in the cluster: " <> Text.intercalate "," extra | not (null extra)]
  where
    want = sort (fmap (.member_peer_url) declared)
    have = sort (concatMap (.listed_peer_urls) listed)
    missing = want \\ have
    extra = have \\ want

-- | Health and membership, as asked of the member itself.
checkMember :: EtcdConfig -> IO CheckResult
checkMember cfg = do
    h <- try (askHealth cfg cfg.etcd_self.member_client_url)
    case h of
        Left e -> pure (Failure ("etcdctl endpoint health: " <> shortErr e))
        Right False -> pure (Failure "endpoint is not healthy")
        Right True -> do
            l <- try (askMembers cfg cfg.etcd_self.member_client_url)
            case l of
                Left e -> pure (Failure ("etcdctl member list: " <> shortErr e))
                Right bs -> case parseMemberList bs of
                    Left e -> pure (Failure ("member list not understood: " <> e))
                    Right ms -> pure (interpretMembers cfg.etcd_cluster ms)

shortErr :: SomeException -> Text
shortErr = Text.take 200 . Text.pack . show

askHealth :: EtcdConfig -> Text -> IO Bool
askHealth cfg ep = do
    r <- try (Binary.untrackedExecOutput etcdctl (EndpointHealth cfg.etcd_client_tls ep) "" silent)
    case r of
        Right _ -> pure True
        Left (Binary.CommandFailed{}) -> pure False

askMembers :: EtcdConfig -> Text -> IO ByteString.ByteString
askMembers cfg ep = Binary.untrackedExecOutput etcdctl (MemberList cfg.etcd_client_tls ep) "" silent

-------------------------------------------------------------------------------

-- | Thrown instead of starting a member with @new@ against a cluster that does not know it.
data ClusterExists = ClusterExists {existing_endpoint :: Text, existing_member :: Text}

instance Show ClusterExists where
    show e =
        Text.unpack $
            "etcd: a cluster already answers at "
                <> e.existing_endpoint
                <> " and does not list member "
                <> e.existing_member
                <> "; starting it with initial-cluster-state=new would not join it. Use the join phase (member add) instead."

instance Exception ClusterExists

-- | Thrown when a member does not become healthy in time.
newtype NotHealthy = NotHealthy Text

instance Show NotHealthy where
    show (NotHealthy why) = "etcd: member did not become healthy: " <> Text.unpack why

instance Exception NotHealthy

{- | The seed decision, pure: given what each /other/ member said when asked
for its member list (Nothing: did not answer), may this member start with
@new@? Refuses iff somebody answered and does not list us.
-}
seedDecision :: Member -> [(Member, Maybe [Listed])] -> Either ClusterExists ()
seedDecision self answers =
    case [(m, ls) | (m, Just ls) <- answers, self.member_peer_url `notElem` concatMap (.listed_peer_urls) ls] of
        [] -> Right ()
        ((m, _) : _) -> Left (ClusterExists m.member_client_url self.member_name)

{- | Guards the seed phase. Satisfied (skipped) when the data directory
already holds a member -- a restart is not a bootstrap. Otherwise asks every
other member for its member list and throws 'ClusterExists' if the answer
says this is not a seed.
-}
seedGuard :: Track' (Binary "etcdctl") -> EtcdConfig -> Op
seedGuard etcdctlBin cfg =
    op "etcd-seed-guard" (deps [justInstall etcdctlBin]) $ \actions ->
        actions
            { help = "refuses to seed a member into a cluster that already exists"
            , notes = ["skipped when the data directory already holds a member"]
            , ref = mkRef "etcd-seed-guard" (Text.pack cfg.etcd_data_dir)
            , check = do
                bootstrapped <- isBootstrapped cfg
                pure (if bootstrapped then Success else Failure "data directory holds no member yet")
            , up = do
                answers <- mapM (askOther cfg) others
                either throwIO pure (seedDecision cfg.etcd_self answers)
            , down = pure ()
            }
  where
    others = filter (/= cfg.etcd_self) cfg.etcd_cluster

-------------------------------------------------------------------------------

-- | Thrown when a join finds no running member to join.
newtype NoClusterToJoin = NoClusterToJoin Text
    deriving (Eq)

instance Show NoClusterToJoin where
    show (NoClusterToJoin who) =
        "etcd: no other member answered, so there is no running cluster for "
            <> Text.unpack who
            <> " to join; seed the cluster first (starting it with existing would never elect anyone)"

instance Exception NoClusterToJoin

-- | What the join phase does next, given what the other members said.
data JoinStep
    = -- | already in the member list (added earlier, perhaps never started): just start
      AlreadyListed
    | -- | ask this member to add us
      AddVia Member
    deriving (Eq, Show)

{- | The join decision, pure. Nobody answering is refused (nothing to join).
Somebody answering and listing us means the add was already done; the first
answerer not listing us is the one to add through.
-}
joinDecision :: Member -> [(Member, Maybe [Listed])] -> Either NoClusterToJoin JoinStep
joinDecision self answers =
    case [(m, ls) | (m, Just ls) <- answers] of
        [] -> Left (NoClusterToJoin self.member_name)
        ((m, ls) : _)
            | self.member_peer_url `elem` concatMap (.listed_peer_urls) ls -> Right AlreadyListed
            | otherwise -> Right (AddVia m)

{- | Guards and performs the join: @etcdctl member add@ through a running
member, idempotently. Satisfied when the data directory already holds a member
(a restart), or when the cluster already lists this member's peer URL. Throws
'NoClusterToJoin' when no other member answers, and lets a failing @member
add@ throw (the unit must not start on a failed add).
-}
joinGuard :: Track' (Binary "etcdctl") -> EtcdConfig -> Op
joinGuard etcdctlBin cfg =
    op "etcd-join-guard" (deps [justInstall etcdctlBin]) $ \actions ->
        actions
            { help = "adds this member to the running cluster (member add) before it starts with existing"
            , notes = ["skipped when the data directory already holds a member"]
            , ref = mkRef "etcd-join-guard" (Text.pack cfg.etcd_data_dir)
            , check = do
                bootstrapped <- isBootstrapped cfg
                pure (if bootstrapped then Success else Failure "data directory holds no member yet")
            , up = do
                answers <- mapM (askOther cfg) others
                step <- either throwIO pure (joinDecision cfg.etcd_self answers)
                case step of
                    AlreadyListed -> pure ()
                    AddVia m ->
                        Binary.untrackedExec etcdctl (MemberAdd cfg.etcd_client_tls m.member_client_url cfg.etcd_self) "" silent
            , down = pure ()
            }
  where
    others = filter (/= cfg.etcd_self) cfg.etcd_cluster

askOther :: EtcdConfig -> Member -> IO (Member, Maybe [Listed])
askOther cfg m = do
    r <- try (askMembers cfg m.member_client_url)
    case r of
        Left (_ :: SomeException) -> pure (m, Nothing)
        Right bs -> pure (m, either (const Nothing) Just (parseMemberList bs))

-- | etcd keeps its raft state in @DATA/member@; its presence means the member has been bootstrapped.
isBootstrapped :: EtcdConfig -> IO Bool
isBootstrapped cfg = doesDirectoryExist (cfg.etcd_data_dir </> "member")

-------------------------------------------------------------------------------

{- | One etcd member: binaries, data directory, config, unit, the seed guard,
and on top a node whose @check@ is 'checkMember'.

The unit watches the config file, so a changed config restarts the service.
@down@ stops the service (through the unit node) and deliberately leaves the
data directory alone: it is the cluster's memory, and removing it is how a
member comes back claiming to be somebody else.
-}
etcdMember ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' (Binary "etcd") ->
    Track' (Binary "etcdctl") ->
    EtcdConfig ->
    Op
etcdMember = etcdMemberIn Seed

{- | A member joining a running cluster: the same node as 'etcdMember' with the
join guard (@member add@) in place of the seed guard and
@initial-cluster-state: existing@ in the config. @etcdctl@ is run against the
other declared members, so at least one must be up.
-}
etcdJoinMember ::
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' (Binary "etcd") ->
    Track' (Binary "etcdctl") ->
    EtcdConfig ->
    Op
etcdJoinMember = etcdMemberIn Join

etcdMemberIn ::
    Phase ->
    Reporter Systemd.Report ->
    Track' (Binary "systemctl") ->
    Track' (Binary "etcd") ->
    Track' (Binary "etcdctl") ->
    EtcdConfig ->
    Op
etcdMemberIn phase r systemctl etcdBin etcdctlBin cfg =
    op "etcd-member" (deps [service]) $ \actions ->
        actions
            { help = "an etcd cluster member, healthy and listed with the declared peers"
            , notes = ["v3 API only", "phase: " <> phaseState phase]
            , ref = mkRef "etcd-member" cfg.etcd_self.member_peer_url
            , check = checkMember cfg
            , up = waitHealthy cfg
            , down = pure ()
            }
  where
    service :: Op
    service = Systemd.systemdServiceWatching [cfg.etcd_config_file] r systemctl (Track $ \_ -> prereqs) (unitConfig cfg)

    prereqs :: Op
    prereqs =
        op
            "etcd-setup"
            (deps [justInstall etcdBin, justInstall etcdctlBin, datadir, configFile, guard])
            id

    guard = case phase of
        Seed -> seedGuard etcdctlBin cfg
        Join -> joinGuard etcdctlBin cfg

    datadir = FS.dir (FS.Directory cfg.etcd_data_dir)
    configFile = FS.filecontents (FS.FileContents cfg.etcd_config_file (renderConfigFor phase cfg))

-- | Polls until the member's check passes; throws 'NotHealthy' (with the last reason) at the deadline.
waitHealthy :: EtcdConfig -> IO ()
waitHealthy cfg = go (max 1 cfg.etcd_ready_timeout_seconds)
  where
    go :: Int -> IO ()
    go left = do
        v <- checkMember cfg
        case v of
            Success -> pure ()
            Failure why | left <= 1 -> throwIO (NotHealthy why)
            _ -> do
                when (left <= 1) $ throwIO (NotHealthy "no verdict")
                threadDelay 1000000
                go (left - 1)
