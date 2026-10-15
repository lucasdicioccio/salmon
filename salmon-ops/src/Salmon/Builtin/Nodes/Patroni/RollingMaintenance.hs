{-# LANGUAGE OverloadedStrings #-}

{- | __Rolling maintenance__ of a Patroni cluster's members
(@specs\/pg-patroni.md@, "Rolling maintenance of members"): restart every
member that needs it (a @pending_restart@, or a running server older than the
declared version after a package update), in the order that is the safety:

1. the replicas, one at a time, each waited for until it is healthy again;
2. a switchover of the leader to a safe candidate;
3. the old leader, which is a replica by then, so step 1 again.

It is two nodes, so that the owner's decision (2026-09-26: replicas first, the
leader behind a maintenance window) is an ordinary edge and an ordinary gate:

* @patroni-rolling-replicas@ does step 1 only. Not 'Window.disruptive': a
  replica restart is not an outage.
* @patroni-rolling-leader@ depends on it and does all three steps; it is
  marked 'Window.disruptive', so with @--maintenance-window@ the switchover and
  the old leader's restart wait for the window, the replicas do not.

Neither node names a leader: which member leads is read from @GET \/cluster@
on every turn, never declared and never remembered. __Nothing decides a
machine is dead__: a member that does not answer, is not healthy, or is
declared but missing from @\/cluster@ holds everything ('Hold'), it is not
skipped. With no safe candidate the leader is left alone (the Zalando
operator's @recreatePods@ does the same); the candidate policy is
"Salmon.Builtin.Nodes.Patroni.PreferredLeader"'s ('switchoverCandidates'): the
synchronous standby under @synchronous_mode@, waited for rather than fallen
back from, else the lowest-lag healthy member, never a @nofailover@ one.

__One step per observation.__ 'step' is pure over what @\/cluster@,
@\/config@ and each member's @\/patroni@ say now. @up@ observes, takes the
step, and observes again, until 'Finished'. Nothing is written down between
steps, so a controller killed halfway is finished by the next pass from
whatever the cluster then shows (the property @PostgresPair@ protects).

* @check@: 'Finished' is 'Success'; a due restart or switchover is 'Failure';
  'Hold' (a member transitioning or unreachable, no leader, no candidate) and
  an unreadable cluster are 'Unknown', so under @run serve@ nothing restarts
  on it.
* @up@: a 'Hold' before anything was done this pass throws
  'MaintenanceHeld' with nothing sent. After a restart or a switchover, a
  'Hold' or the same step again is the cluster settling, and is re-observed
  each second for 'rm_settle_seconds' before @up@ gives up
  ('MaintenanceStuck').
* @down@: nothing.

__Requests.__ A restart is @POST \/restart {\"role\": \"replica\"}@ on that
member's REST API: Patroni itself refuses it if the member leads by then, so
a leader is never restarted in place, whatever this module believes. A
switchover is @POST \/switchover {leader, candidate}@ on the leader's API.
Both are unsafe endpoints; 'rm_credentials_file' is the same pre-provisioned
@user:password@ file as the preferred-leader node's, read at @up@ time and
never shown.

__The two signals.__ @pending_restart@ and @server_version@, both from the
member's own @GET \/patroni@. @server_version@ is the /running/ server, so
after the package is upgraded on disk the member reports the old number until
it is restarted, which is what 'rm_server_version' compares against. A Patroni
package upgrade is not seen: restarting Postgres does not restart Patroni.
-}
module Salmon.Builtin.Nodes.Patroni.RollingMaintenance (
    RollingMaintenance (..),
    defaultSettleSeconds,
    rollingMaintenance,
    rollingReplicas,

    -- * What is observed
    MemberStatus (..),
    parseMemberStatus,
    Observation (..),
    stale,

    -- * Pure decision
    Phase (..),
    Step (..),
    step,
    stepVerdict,

    -- * Requests
    restartBody,

    -- * Errors
    MaintenanceHeld (..),
    MaintenanceStuck (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO, try)
import Data.Aeson (eitherDecodeStrict, encode, object, withObject, (.!=), (.:?), (.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (
    HttpException,
    Manager,
    Request,
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
import Salmon.Builtin.Nodes.Patroni.PreferredLeader (
    ClusterView (..),
    Member (..),
    healthyStates,
    leaderOf,
    parseCluster,
    parseCredentials,
    parseSynchronousMode,
    switchoverBody,
    switchoverCandidates,
 )
import Salmon.Op.Ref
import qualified Salmon.Op.Window as Window

-------------------------------------------------------------------------------

data RollingMaintenance
    = RollingMaintenance
    { rm_scope :: Text
    -- ^ the Patroni scope (cluster name)
    , rm_members :: [(Text, Text)]
    -- ^ every member, by its Patroni @name@, with the @host:port@ of its REST API
    , rm_server_version :: Maybe Int
    -- ^ a member whose running @server_version@ (e.g. @170006@) is below this needs a restart; 'Nothing' looks at @pending_restart@ alone
    , rm_credentials_file :: Maybe FilePath
    -- ^ a pre-provisioned @user:password@ file for @restapi.authentication@; 'Nothing' sends no credential
    , rm_settle_seconds :: Int
    -- ^ how long @up@ waits, after each restart or switchover, for the cluster to settle
    }
    deriving (Eq, Show)

-- | Long enough for a replica restart and catch-up on a small cluster.
defaultSettleSeconds :: Int
defaultSettleSeconds = 120

{- | Both nodes: @patroni-rolling-leader@ (disruptive) depending on
@patroni-rolling-replicas@. The @ref@s are the scope alone; the members, their
addresses and the target version are in @notes@, so a changed declaration is a
changed representative.
-}
rollingMaintenance :: RollingMaintenance -> Op
rollingMaintenance c =
    op "patroni-rolling-leader" (deps [rollingReplicas c]) $ \actions ->
        Window.disruptive
            actions
                { help = "Patroni scope " <> c.rm_scope <> ": switch the leader over to a safe candidate and restart it, after the replicas"
                , notes =
                    phaseNotes c
                        <> [ "leader left alone when no candidate is safe; with synchronous_mode, only a sync standby"
                           , "disruptive: held outside the maintenance windows"
                           ]
                , ref = mkRef "patroni-rolling-leader" c.rm_scope
                , check = checkPhase AllMembers c
                , up = runPhase AllMembers c
                , down = pure ()
                }

-- | Step 1 alone: the replicas, one at a time. 'rollingMaintenance' depends on it.
rollingReplicas :: RollingMaintenance -> Op
rollingReplicas c =
    op "patroni-rolling-replicas" nodeps $ \actions ->
        actions
            { help = "Patroni scope " <> c.rm_scope <> ": restart the replicas that need it, one at a time"
            , notes = phaseNotes c <> ["never restarts the member that leads"]
            , ref = mkRef "patroni-rolling-replicas" c.rm_scope
            , check = checkPhase ReplicasOnly c
            , up = runPhase ReplicasOnly c
            , down = pure ()
            }

phaseNotes :: RollingMaintenance -> [Text]
phaseNotes c =
    [ "members: " <> Text.intercalate ", " [n <> " at " <> a | (n, a) <- c.rm_members]
    , "needs a restart: pending_restart" <> maybe "" (\v -> ", or server_version below " <> Text.pack (show v)) c.rm_server_version
    ]

-------------------------------------------------------------------------------

-- | A member's own @GET \/patroni@, as far as this module reads it.
data MemberStatus
    = MemberStatus
    { ms_name :: Maybe Text
    -- ^ @patroni.name@: who answers at the declared address
    , ms_scope :: Maybe Text
    , ms_server_version :: Maybe Int
    , ms_pending_restart :: Bool
    }
    deriving (Eq, Show)

parseMemberStatus :: ByteString.ByteString -> Either Text MemberStatus
parseMemberStatus bs = case eitherDecodeStrict bs of
    Left e -> Left (Text.pack e)
    Right v -> either (Left . Text.pack) Right (Aeson.parseEither p v)
  where
    p = withObject "patroni" $ \o -> do
        pat <- o .:? "patroni" .!= Aeson.emptyObject
        (name, scope) <- withObject "patroni.patroni" (\q -> (,) <$> q .:? "name" <*> q .:? "scope") pat
        MemberStatus name scope <$> o .:? "server_version" <*> o .:? "pending_restart" .!= False

-- | Everything one turn decides on.
data Observation
    = Observation
    { ob_cluster :: ClusterView
    , ob_members :: [(Text, Either Text MemberStatus)]
    -- ^ each declared member's @\/patroni@, or why it could not be read
    }
    deriving (Eq, Show)

-- | Does this member need a restart? 'Left' when it cannot be told.
stale :: Maybe Int -> MemberStatus -> Either Text Bool
stale target s
    | s.ms_pending_restart = Right True
    | otherwise = case target of
        Nothing -> Right False
        Just t -> maybe (Left "server_version not reported") (Right . (< t)) s.ms_server_version

data Phase
    = -- | the replicas only
      ReplicasOnly
    | -- | replicas, then the leader by a switchover
      AllMembers
    deriving (Eq, Show)

data Step
    = -- | @POST \/restart@ on this member, which is a replica
      Restart Text
    | -- | ask Patroni to move the leader from the first name to the second
      SwitchOverTo Text Text
    | -- | nothing left to do
      Finished
    | -- | do nothing this turn, and why
      Hold Text
    deriving (Eq, Show)

{- | The next step, given the declared scope, the target version and what is
observed now. In order: anything unsure holds; then the stale replicas by
name; then (for 'AllMembers') a stale leader is switched away from, or held
when no candidate is safe.
-}
step :: Phase -> Text -> Maybe Int -> Observation -> Step
step phase scope target ob
    | Just s <- cv.cv_scope, s /= scope = Hold ("cluster reports scope " <> s <> ", declared " <> scope)
    | (why : _) <- unsure = Hold why
    | otherwise = case leaderOf cv of
        Nothing -> Hold "no leader: electing one is Patroni's"
        Just l -> case sort [m.m_name | m <- cv.cv_members, m.m_name /= l.m_name, isStale m] of
            (r : _) -> Restart r
            []
                | phase == ReplicasOnly -> Finished
                | not (isStale l) -> Finished
                | otherwise -> case filter (not . isStale) (switchoverCandidates cv) of
                    (cand : _) -> SwitchOverTo l.m_name cand.m_name
                    []
                        | cv.cv_synchronous -> Hold ("leader " <> l.m_name <> " needs a restart and no synchronous standby is eligible; waiting rather than falling back, leader left alone")
                        | otherwise -> Hold ("leader " <> l.m_name <> " needs a restart and no member is a safe candidate; leader left alone")
  where
    cv = ob.ob_cluster
    statusOf n = lookup n ob.ob_members
    isStale :: Member -> Bool
    isStale m = case statusOf m.m_name of
        Just (Right s) -> stale target s == Right True
        _ -> False
    unsure =
        concatMap memberDoubt cv.cv_members
            <> [n <> " is declared but not a member of the cluster" | (n, _) <- ob.ob_members, n `notElem` fmap (.m_name) cv.cv_members]
            <> [m.m_name <> " is " <> m.m_role <> ": a standby cluster is not handled" | m <- cv.cv_members, m.m_role == "standby_leader"]
    memberDoubt :: Member -> [Text]
    memberDoubt m = case statusOf m.m_name of
        Nothing -> [m.m_name <> " has no declared REST address"]
        Just (Left why) -> [m.m_name <> ": " <> why]
        Just (Right s)
            | Just n <- s.ms_name, n /= m.m_name -> [m.m_name <> "'s address answers as " <> n]
            | Just sc <- s.ms_scope, sc /= scope -> [m.m_name <> " reports scope " <> sc]
            | Left why <- stale target s -> [m.m_name <> ": " <> why]
            | maybe True (`notElem` healthyStates) m.m_state ->
                [m.m_name <> " is " <> fromMaybe "in no reported state" m.m_state <> "; waiting for it"]
            | otherwise -> []

-- | The step as a check's answer.
stepVerdict :: Step -> CheckResult
stepVerdict Finished = Success
stepVerdict (Restart m) = Failure (m <> " needs a restart")
stepVerdict (SwitchOverTo from to) = Failure ("leader " <> from <> " needs a restart; switch over to " <> to <> " first")
stepVerdict (Hold _) = Unknown

-- | The @POST \/restart@ body: Patroni restarts only if the member is a replica.
restartBody :: LByteString.ByteString
restartBody = encode (object ["role" .= ("replica" :: Text)])

-------------------------------------------------------------------------------

-- | Thrown by @up@ when the first step of a pass is a 'Hold'; nothing was sent.
newtype MaintenanceHeld = MaintenanceHeld Text

instance Show MaintenanceHeld where
    show (MaintenanceHeld why) = "patroni: rolling maintenance held, nothing restarted: " <> Text.unpack why

instance Exception MaintenanceHeld

-- | Thrown by @up@ when Patroni refused a request, or the cluster did not settle after one.
newtype MaintenanceStuck = MaintenanceStuck Text

instance Show MaintenanceStuck where
    show (MaintenanceStuck why) = "patroni: rolling maintenance stopped: " <> Text.unpack why

instance Exception MaintenanceStuck

newManager' :: IO Manager
newManager' = newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutMicro 30000000}

getStatus :: Manager -> Text -> Text -> IO (Int, ByteString.ByteString)
getStatus mgr addr path = do
    req <- parseRequest (Text.unpack ("http://" <> addr <> path))
    rsp <- httpLbs req mgr
    pure (statusCode (responseStatus rsp), LByteString.toStrict (responseBody rsp))

httpError :: HttpException -> Text
httpError e = "REST API not answering: " <> Text.take 150 (Text.pack (show e))

-- | @\/cluster@ and @\/config@ from the first declared member that answers both, and every member's @\/patroni@.
observe :: Manager -> RollingMaintenance -> IO (Either Text Observation)
observe mgr c = do
    cv <- firstCluster c.rm_members []
    statuses <- traverse (\(n, a) -> (,) n <$> memberStatus a) c.rm_members
    pure (fmap (\v -> Observation v statuses) cv)
  where
    firstCluster [] errs = pure (Left ("cluster not readable from any member: " <> Text.intercalate "; " (reverse errs)))
    firstCluster ((n, a) : rest) errs = do
        r <- clusterAt a
        case r of
            Right v -> pure (Right v)
            Left why -> firstCluster rest ((n <> ": " <> why) : errs)
    clusterAt a = do
        r <- try ((,) <$> getStatus mgr a "/cluster" <*> getStatus mgr a "/config")
        pure $ case r of
            Left e -> Left (httpError e)
            Right ((cs, cb), (fs, fb))
                | cs /= 200 -> Left ("GET /cluster answers " <> Text.pack (show cs))
                | fs /= 200 -> Left ("GET /config answers " <> Text.pack (show fs))
                | otherwise -> do
                    v <- either (Left . ("GET /cluster not understood: " <>) . Text.take 120) Right (parseCluster cb)
                    sync <- either (Left . ("GET /config not understood: " <>) . Text.take 120) Right (parseSynchronousMode fb)
                    pure v{cv_synchronous = sync}
    memberStatus a = do
        r <- try (getStatus mgr a "/patroni")
        pure $ case r of
            Left e -> Left (httpError e)
            Right (s, b)
                -- 503 is a member that is not running: its body still says why, but that is a Hold either way
                | s /= 200 -> Left ("GET /patroni answers " <> Text.pack (show s))
                | otherwise -> either (Left . ("GET /patroni not understood: " <>) . Text.take 120) Right (parseMemberStatus b)

checkPhase :: Phase -> RollingMaintenance -> IO CheckResult
checkPhase phase c = do
    mgr <- newManager'
    either (const Unknown) (stepVerdict . step phase c.rm_scope c.rm_server_version) <$> observe mgr c

runPhase :: Phase -> RollingMaintenance -> IO ()
runPhase phase c = do
    mgr <- newManager'
    loop mgr Nothing
  where
    -- @lastDone@: the step taken last this pass and the seconds left for the cluster to settle after it
    loop :: Manager -> Maybe (Step, Int) -> IO ()
    loop mgr lastDone = do
        ob <- observe mgr c
        let s = either Hold (step phase c.rm_scope c.rm_server_version) ob
        case (s, lastDone) of
            (Finished, _) -> pure ()
            (Hold why, Nothing) -> throwIO (MaintenanceHeld why)
            (_, Just (prev, left))
                | isHold s || s == prev ->
                    if left <= 0
                        then throwIO (MaintenanceStuck ("after " <> describe prev <> ", the cluster did not settle: " <> describe s))
                        else threadDelay 1000000 >> loop mgr (Just (prev, left - 1))
            _ -> do
                act mgr s
                loop mgr (Just (s, max 1 c.rm_settle_seconds))

    isHold (Hold _) = True
    isHold _ = False

    describe (Restart m) = "restarting " <> m
    describe (SwitchOverTo from to) = "a switchover from " <> from <> " to " <> to
    describe Finished = "finished"
    describe (Hold why) = why

    act mgr (Restart m) = case lookup m c.rm_members of
        Nothing -> throwIO (MaintenanceStuck (m <> " has no declared REST address"))
        Just a -> post mgr a "/restart" restartBody
    act mgr (SwitchOverTo from to) = case lookup from c.rm_members of
        Nothing -> throwIO (MaintenanceStuck (from <> " has no declared REST address"))
        Just a -> post mgr a "/switchover" (switchoverBody from to)
    act _ _ = pure ()

    post :: Manager -> Text -> Text -> LByteString.ByteString -> IO ()
    post mgr addr path body = do
        auth <- credentials
        req0 <- parseRequest (Text.unpack ("http://" <> addr <> path))
        let req =
                auth
                    req0
                        { method = "POST"
                        , requestHeaders = [("Content-Type", "application/json")]
                        , requestBody = RequestBodyLBS body
                        }
        r <- try (httpLbs req mgr)
        case r of
            Left e -> throwIO (MaintenanceStuck ("POST " <> path <> ": " <> httpError e))
            Right rsp -> do
                let s = statusCode (responseStatus rsp)
                -- Patroni's answer is its own message (no credential is echoed)
                if s >= 200 && s < 300
                    then pure ()
                    else throwIO (MaintenanceStuck ("POST " <> path <> " on " <> addr <> " answers " <> Text.pack (show s) <> ": " <> Text.take 200 (Text.pack (Char8.unpack (LByteString.toStrict (responseBody rsp))))))

    credentials :: IO (Request -> Request)
    credentials = case c.rm_credentials_file of
        Nothing -> pure id
        Just path -> do
            bs <- ByteString.readFile path
            either (throwIO . MaintenanceStuck) (pure . uncurry applyBasicAuth) (parseCredentials bs)

