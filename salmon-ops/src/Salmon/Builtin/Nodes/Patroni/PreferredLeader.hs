{-# LANGUAGE OverloadedStrings #-}

{- | An opt-in __preferred leader__ for a Patroni cluster
(@specs\/pg-patroni.md@, "Switchover, under Patroni").

__Opt-in, and never the default.__ "Salmon.Builtin.Nodes.Patroni" names no
leader anywhere; this node is the one place a declaration says which member it
/prefers/, and only when an operator declares it. With it declared and
supervision on, salmon moves the leader back after every automatic failover as
soon as the preferred member is eligible again: sometimes wanted (a preferred
site, a bigger machine), sometimes a flap. That is why it is a separate node
nobody gets without asking.

It does not decide that a machine is dead and it never fails over: a
switchover is a request to Patroni to move a /healthy/ leader to a /healthy/
candidate, which Patroni itself refuses when it disagrees. With no leader at all
the node waits; electing one is Patroni's business.

* @check@: @GET \/leader@ on the preferred member's REST API answers 200. When
  it does not, the cluster is read (@GET \/cluster@, @GET \/config@) and the
  decision ('decide') turns into a 'Failure' (a switchover is due) or 'Unknown'
  (the preferred member is not an eligible target yet: nothing is applied and,
  under @run serve@, nothing is restarted).
* @up@: re-reads the cluster (the state a pass acts on is re-derived each time,
  never remembered), and when the preferred member is eligible sends
  @POST \/switchover@ with @leader@ and @candidate@, then waits for
  @GET \/leader@ to answer 200. When it is not eligible, @up@ sends nothing and
  throws 'SwitchoverSkipped', so the pass reports the preference as unmet and
  the next pass tries again with the leader left alone.
* @down@: nothing. Retracting a preference does not move the leader anywhere.

__Eligibility__ ('eligible', 'switchoverCandidates', after the Zalando
operator's @getSwitchoverCandidate@): with @synchronous_mode@ on (@true@ or
@quorum@ in @\/config@), only a synchronous standby is a target and salmon
__waits for one rather than falling back__ to an asynchronous member, which may
lack acknowledged writes; otherwise a member whose state is @running@,
@streaming@ or @in archive recovery@. A member tagged @nofailover@ is never a
target. 'switchoverCandidate' (the lowest-lag eligible member) is exported for
the operations that pick a target themselves; this node's target is the
declared member, so it only asks whether that member is eligible.

__Credentials.__ @POST \/switchover@ is an unsafe endpoint, authenticated when
@restapi.authentication@ is set. 'pl_credentials_file' is a pre-provisioned
file holding @user:password@ on one line, read at @up@ time and sent as HTTP
basic auth; it never reaches @help@, @notes@, an error or a log. The REST API
is spoken over plain HTTP, as in "Salmon.Builtin.Nodes.Patroni".
-}
module Salmon.Builtin.Nodes.Patroni.PreferredLeader (
    PreferredLeader (..),
    defaultSettleSeconds,
    preferredLeader,

    -- * The cluster as @GET \/cluster@ and @GET \/config@ show it
    Member (..),
    ClusterView (..),
    parseCluster,
    parseSynchronousMode,

    -- * Pure decisions
    Decision (..),
    decide,
    decisionVerdict,
    leaderOf,
    leaderRoles,
    syncRoles,
    healthyStates,
    eligible,
    switchoverCandidates,
    switchoverCandidate,

    -- * The request
    switchoverBody,
    parseCredentials,

    -- * Errors
    SwitchoverSkipped (..),
    SwitchoverRefused (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO, try)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict, encode, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (
    HttpException,
    Manager,
    RequestBody (..),
    applyBasicAuth,
    defaultManagerSettings,
    httpLbs,
    managerResponseTimeout,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
    responseTimeoutMicro,
 )
import Network.HTTP.Types.Status (statusCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Op.Ref

-------------------------------------------------------------------------------

data PreferredLeader
    = PreferredLeader
    { pl_scope :: Text
    -- ^ the Patroni scope (cluster name)
    , pl_member :: Text
    -- ^ the member preferred as leader, by its Patroni @name@
    , pl_rest_address :: Text
    -- ^ @host:port@ of that member's REST API, where the node asks and requests
    , pl_credentials_file :: Maybe FilePath
    -- ^ a pre-provisioned @user:password@ file for @restapi.authentication@; 'Nothing' sends no credential
    , pl_settle_seconds :: Int
    -- ^ how long @up@ waits, after Patroni accepted the switchover, for the member to lead
    }
    deriving (Eq, Show)

defaultSettleSeconds :: Int
defaultSettleSeconds = 60

{- | The node. Its @ref@ is the scope alone, so two declarations preferring
different members of one cluster collide (and are reported @Conflicting@)
instead of taking turns. The preferred member is in @help@ and @notes@, which is
how a changed preference shows as a changed representative.
-}
preferredLeader :: PreferredLeader -> Op
preferredLeader c =
    op "patroni-preferred-leader" nodeps $ \actions ->
        actions
            { help = "Patroni scope " <> c.pl_scope <> " prefers " <> c.pl_member <> " as leader"
            , notes =
                [ "opt-in: moves the leader back to " <> c.pl_member <> " after a failover"
                , "switchover only to an eligible member; with synchronous_mode, only to a sync standby"
                , "down leaves the leader where it is"
                ]
            , ref = mkRef "patroni-preferred-leader" c.pl_scope
            , check = checkPreferred c
            , up = switchToPreferred c
            , down = pure ()
            }

-------------------------------------------------------------------------------

-- | One entry of @GET \/cluster@'s @members@, as far as this module reads it.
data Member
    = Member
    { m_name :: Text
    , m_role :: Text
    , m_state :: Maybe Text
    , m_lag :: Maybe Integer
    -- ^ bytes behind the leader; 'Nothing' when Patroni says @unknown@ or nothing
    , m_nofailover :: Bool
    }
    deriving (Eq, Show)

instance FromJSON Member where
    parseJSON = withObject "member" $ \o -> do
        tags <- o .:? "tags" .!= Aeson.emptyObject
        nofailover <- withObject "tags" (\t -> t .:? "nofailover" .!= False) tags
        Member
            <$> o .: "name"
            <*> o .: "role"
            <*> o .:? "state"
            <*> (lag <$> o .:? "lag")
            <*> pure nofailover
      where
        lag :: Maybe Value -> Maybe Integer
        lag (Just v@(Number _)) = Aeson.parseMaybe parseJSON v
        lag _ = Nothing

data ClusterView
    = ClusterView
    { cv_scope :: Maybe Text
    , cv_members :: [Member]
    , cv_synchronous :: Bool
    -- ^ from @GET \/config@, not @\/cluster@: with no sync standby, @\/cluster@ alone cannot tell
    }
    deriving (Eq, Show)

-- | @GET \/cluster@'s body; 'cv_synchronous' is left 'False' for 'parseSynchronousMode' to set.
parseCluster :: ByteString.ByteString -> Either Text ClusterView
parseCluster bs = case eitherDecodeStrict bs of
    Left e -> Left (Text.pack e)
    Right v -> either (Left . Text.pack) Right (Aeson.parseEither p v)
  where
    p = withObject "cluster" $ \o ->
        ClusterView <$> o .:? "scope" <*> o .:? "members" .!= [] <*> pure False

{- | @GET \/config@'s @synchronous_mode@: @true@, or a string Patroni accepts as
on (@quorum@ included). Absent is off. Anything not understood is an error,
not \"off\": guessing asynchronous where it is synchronous is the unsafe way.
-}
parseSynchronousMode :: ByteString.ByteString -> Either Text Bool
parseSynchronousMode bs = case eitherDecodeStrict bs of
    Left e -> Left (Text.pack e)
    Right v -> either (Left . Text.pack) Right (Aeson.parseEither p v)
  where
    p = withObject "config" $ \o -> do
        m <- o .:? "synchronous_mode"
        case m of
            Nothing -> pure False
            Just Null -> pure False
            Just (Bool b) -> pure b
            Just (String s)
                | Text.toLower s `elem` ["on", "true", "yes", "quorum"] -> pure True
                | Text.toLower s `elem` ["off", "false", "no"] -> pure False
            Just other -> fail ("synchronous_mode not understood: " <> take 40 (show other))

-------------------------------------------------------------------------------

leaderRoles, syncRoles, healthyStates :: [Text]
leaderRoles = ["leader", "master", "primary"]
syncRoles = ["sync_standby", "quorum_standby"]
healthyStates = ["running", "streaming", "in archive recovery"]

leaderOf :: ClusterView -> Maybe Member
leaderOf cv = listToMaybe [m | m <- cv.cv_members, m.m_role `elem` leaderRoles]

-- | Can this member be a switchover's target, in this cluster? Never the leader.
eligible :: ClusterView -> Member -> Bool
eligible cv m =
    m.m_role `notElem` leaderRoles
        && not m.m_nofailover
        && maybe False (`elem` healthyStates) m.m_state
        && (not cv.cv_synchronous || m.m_role `elem` syncRoles)

-- | The eligible members, lowest lag first (an unknown lag last).
switchoverCandidates :: ClusterView -> [Member]
switchoverCandidates cv = sortOn key (filter (eligible cv) cv.cv_members)
  where
    key :: Member -> (Int, Integer, Text)
    key m = (maybe 1 (const 0) m.m_lag, fromMaybe 0 m.m_lag, m.m_name)

-- | The operator's choice when nobody declared one: the lowest-lag eligible member.
switchoverCandidate :: ClusterView -> Maybe Member
switchoverCandidate = listToMaybe . switchoverCandidates

data Decision
    = -- | the preferred member leads
      AlreadyLeads
    | -- | ask Patroni to move the leader from the first name to the second
      SwitchOver Text Text
    | -- | leave the leader alone this pass, and why
      Wait Text
    deriving (Eq, Show)

-- | Given the declared scope and preferred member, what this pass should do.
decide :: Text -> Text -> ClusterView -> Decision
decide scope preferred cv
    | Just s <- cv.cv_scope, s /= scope = Wait ("cluster reports scope " <> s <> ", declared " <> scope)
    | otherwise = case leaderOf cv of
        Nothing -> Wait "no leader: electing one is Patroni's, not a switchover"
        Just l
            | l.m_name == preferred -> AlreadyLeads
            | otherwise -> case [m | m <- cv.cv_members, m.m_name == preferred] of
                [] -> Wait (preferred <> " is not a member of the cluster")
                (m : _)
                    | eligible cv m -> SwitchOver l.m_name preferred
                    | m.m_nofailover -> Wait (preferred <> " is tagged nofailover")
                    | cv.cv_synchronous && m.m_role `notElem` syncRoles ->
                        Wait (preferred <> " is not a synchronous standby (role " <> m.m_role <> "); waiting rather than falling back")
                    | otherwise -> Wait (preferred <> " is not eligible (state " <> fromMaybe "unknown" m.m_state <> ")")

-- | The decision as a check's answer.
decisionVerdict :: Decision -> CheckResult
decisionVerdict AlreadyLeads = Success
decisionVerdict (SwitchOver from to) = Failure ("leader is " <> from <> ", " <> to <> " is preferred and eligible")
decisionVerdict (Wait _) = Unknown

-------------------------------------------------------------------------------

-- | The @POST \/switchover@ body.
switchoverBody :: Text -> Text -> LByteString.ByteString
switchoverBody from to = encode (object ["leader" .= from, "candidate" .= to])

{- | @user:password@ on the first line, split on the first colon. The error
never quotes the file: it may hold the password.
-}
parseCredentials :: ByteString.ByteString -> Either Text (ByteString.ByteString, ByteString.ByteString)
parseCredentials bs =
    let line = Char8.takeWhile (\ch -> ch /= '\n' && ch /= '\r') bs
        (user, rest) = Char8.break (== ':') line
     in if ByteString.null user || ByteString.null rest
            then Left "credentials file is not user:password on its first line"
            else Right (user, ByteString.drop 1 rest)

-------------------------------------------------------------------------------

-- | Thrown by @up@ when the preferred member is not a target this pass; nothing was sent.
newtype SwitchoverSkipped = SwitchoverSkipped Text

instance Show SwitchoverSkipped where
    show (SwitchoverSkipped why) = "patroni: switchover skipped, leader left alone: " <> Text.unpack why

instance Exception SwitchoverSkipped

-- | Thrown by @up@ when Patroni refused, or the member did not take over in time.
newtype SwitchoverRefused = SwitchoverRefused Text

instance Show SwitchoverRefused where
    show (SwitchoverRefused why) = "patroni: switchover not done: " <> Text.unpack why

instance Exception SwitchoverRefused

newManager' :: IO Manager
newManager' = newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutMicro 10000000}

url :: PreferredLeader -> Text -> String
url c path = Text.unpack ("http://" <> c.pl_rest_address <> path)

getStatus :: Manager -> PreferredLeader -> Text -> IO (Int, ByteString.ByteString)
getStatus mgr c path = do
    req <- parseRequest (url c path)
    rsp <- httpLbs req mgr
    pure (statusCode (responseStatus rsp), LByteString.toStrict (responseBody rsp))

-- | @\/cluster@ then @\/config@; 'Left' says why the cluster could not be read.
readCluster :: Manager -> PreferredLeader -> IO (Either Text ClusterView)
readCluster mgr c = do
    r <- try $ do
        (cs, cb) <- getStatus mgr c "/cluster"
        (fs, fb) <- getStatus mgr c "/config"
        pure (cs, cb, fs, fb)
    pure $ case r of
        Left (e :: HttpException) -> Left ("REST API not answering: " <> Text.take 150 (Text.pack (show e)))
        Right (cs, cb, fs, fb)
            | cs /= 200 -> Left ("GET /cluster answers " <> Text.pack (show cs))
            | fs /= 200 -> Left ("GET /config answers " <> Text.pack (show fs))
            | otherwise -> do
                cv <- either (Left . ("GET /cluster not understood: " <>) . Text.take 120) Right (parseCluster cb)
                sync <- either (Left . ("GET /config not understood: " <>) . Text.take 120) Right (parseSynchronousMode fb)
                pure cv{cv_synchronous = sync}

leads :: Manager -> PreferredLeader -> IO (Either Text Bool)
leads mgr c = do
    r <- try (getStatus mgr c "/leader")
    pure $ case r of
        Left (e :: HttpException) -> Left ("REST API not answering: " <> Text.take 150 (Text.pack (show e)))
        Right (s, _) -> Right (s == 200)

checkPreferred :: PreferredLeader -> IO CheckResult
checkPreferred c = do
    mgr <- newManager'
    l <- leads mgr c
    case l of
        Left why -> pure (Failure why)
        Right True -> pure Success
        Right False -> either Failure (decisionVerdict . decide c.pl_scope c.pl_member) <$> readCluster mgr c

switchToPreferred :: PreferredLeader -> IO ()
switchToPreferred c = do
    mgr <- newManager'
    cv <- readCluster mgr c >>= either (throwIO . SwitchoverSkipped) pure
    case decide c.pl_scope c.pl_member cv of
        AlreadyLeads -> pure ()
        Wait why -> throwIO (SwitchoverSkipped why)
        SwitchOver from to -> do
            auth <- case c.pl_credentials_file of
                Nothing -> pure id
                Just path -> do
                    bs <- ByteString.readFile path
                    either (throwIO . SwitchoverRefused) (pure . uncurry applyBasicAuth) (parseCredentials bs)
            req0 <- parseRequest (url c "/switchover")
            let req =
                    auth
                        req0
                            { method = "POST"
                            , requestHeaders = [("Content-Type", "application/json")]
                            , requestBody = RequestBodyLBS (switchoverBody from to)
                            }
            r <- try (httpLbs req mgr)
            case r of
                Left (e :: HttpException) -> throwIO (SwitchoverRefused ("REST API not answering: " <> Text.take 150 (Text.pack (show e))))
                Right rsp -> do
                    let s = statusCode (responseStatus rsp)
                    -- Patroni's answer is its own message (no credential is echoed)
                    if s >= 200 && s < 300
                        then settle mgr (max 1 c.pl_settle_seconds)
                        else throwIO (SwitchoverRefused ("POST /switchover answers " <> Text.pack (show s) <> ": " <> Text.take 200 (lenientUtf8 (LByteString.toStrict (responseBody rsp)))))
  where
    settle :: Manager -> Int -> IO ()
    settle mgr left = do
        l <- leads mgr c
        case l of
            Right True -> pure ()
            _
                | left <= 1 -> throwIO (SwitchoverRefused (c.pl_member <> " accepted but does not lead yet"))
                | otherwise -> threadDelay 1000000 >> settle mgr (left - 1)

    lenientUtf8 = Text.pack . Char8.unpack
