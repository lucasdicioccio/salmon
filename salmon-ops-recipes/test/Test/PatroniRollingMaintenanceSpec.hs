{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.Patroni.RollingMaintenance": the pure step over
member tables (unit cases and QuickCheck properties, including a simulated
run to the end and a run "killed" after every step), the nodes' public text
and graph shape, and @check@\/@up@ against three in-process fakes of Patroni's
REST API (warp), one per member. No real Patroni is run here.
-}
module Test.PatroniRollingMaintenanceSpec (tests) where

import Control.Exception (try)
import Control.Monad (forM)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Functor.Identity (runIdentity)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.FilePath ((</>))
import Test.QuickCheck (Gen, chooseInt, counterexample, elements, forAll, frequency, listOf1, oneof, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Patroni.PreferredLeader (ClusterView (..), Member (..), eligible, leaderOf, leaderRoles, syncRoles)
import Salmon.Builtin.Nodes.Patroni.RollingMaintenance
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.Graph (Graph (..))
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (mkRef)
import qualified Salmon.Op.Window as Window

import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Patroni.RollingMaintenance"
        [ testGroup "parsing" parsing
        , testGroup "step" steps
        , testGroup "properties" properties
        , testGroup "nodes" nodeText
        , testGroup "against fake REST APIs" fakeApi
        ]

-------------------------------------------------------------------------------

parsing :: [TestTree]
parsing =
    [ testCase "a Patroni 4 /patroni body" $
        assertEqual
            ""
            (Right (MemberStatus (Just "n2") (Just "demo") (Just 170004) True))
            ( parseMemberStatus
                "{\"state\":\"running\",\"postmaster_start_time\":\"2026-10-01 10:00:00+00:00\",\"role\":\"replica\",\
                \\"server_version\":170004,\"xlog\":{\"received_location\":1,\"replayed_location\":1,\"paused\":false},\
                \\"timeline\":2,\"pending_restart\":true,\"pending_restart_reason\":{\"max_connections\":{\"old_value\":\"100\",\"new_value\":\"200\"}},\
                \\"dcs_last_seen\":1,\"database_system_identifier\":\"7\",\"patroni\":{\"version\":\"4.0.7\",\"scope\":\"demo\",\"name\":\"n2\"}}"
            )
    , testCase "absent fields: no name, no version, nothing pending" $
        assertEqual "" (Right (MemberStatus Nothing Nothing Nothing False)) (parseMemberStatus "{\"state\":\"running\"}")
    , testCase "not JSON is an error" $
        assertBool "" (either (const True) (const False) (parseMemberStatus "nope"))
    , testCase "the restart body asks Patroni to restart only a replica" $
        assertEqual "" (Just (object ["role" .= ("replica" :: Text)])) (decode restartBody :: Maybe Value)
    , testCase "stale: pending_restart, or a running version below the target" $ do
        assertEqual "pending" (Right True) (stale Nothing (fresh 170004){ms_pending_restart = True})
        assertEqual "no target" (Right False) (stale Nothing (fresh 170004))
        assertEqual "older" (Right True) (stale (Just 170005) (fresh 170004))
        assertEqual "same" (Right False) (stale (Just 170004) (fresh 170004))
        assertBool "unknown version" (either (const True) (const False) (stale (Just 170005) (fresh 170004){ms_server_version = Nothing}))
    ]

steps :: [TestTree]
steps =
    [ testCase "nothing stale: finished" $
        assertEqual "" Finished (step AllMembers "demo" target (world ["n1"] [] False))
    , testCase "stale replicas first, by name, one at a time" $ do
        assertEqual "" (Restart "n2") (step AllMembers "demo" target (world ["n1", "n2", "n3"] ["n1", "n3", "n2"] False))
        assertEqual "" (Restart "n3") (step AllMembers "demo" target (world ["n1", "n2", "n3"] ["n1", "n3"] False))
    , testCase "replicas done, leader stale: switch over to the lowest lag" $
        assertEqual "" (SwitchOverTo "n1" "n3") (step AllMembers "demo" target (lagged [("n2", 50), ("n3", 10)] (world ["n1", "n2", "n3"] ["n1"] False)))
    , testCase "the replicas node never touches the leader" $
        assertEqual "" Finished (step ReplicasOnly "demo" target (world ["n1", "n2", "n3"] ["n1"] False))
    , testCase "sync mode: only the sync standby, and no fallback" $ do
        let w = world ["n1", "n2", "n3"] ["n1"] True
        assertIsHold (step AllMembers "demo" target w)
        assertEqual "" (SwitchOverTo "n1" "n3") (step AllMembers "demo" target (roled "n3" "sync_standby" w))
    , testCase "no candidate at all: the leader is left alone" $ do
        assertIsHold (step AllMembers "demo" target (world ["n1"] ["n1"] False))
        assertIsHold (step AllMembers "demo" target (nofailover "n2" (world ["n1", "n2"] ["n1"] False)))
    , testCase "a transitioning, unreadable, missing or mislabelled member holds everything" $ do
        let w = world ["n1", "n2", "n3"] ["n2"] False
        assertIsHold (step AllMembers "demo" target (stated "n3" (Just "starting") w))
        assertIsHold (step AllMembers "demo" target (stated "n3" Nothing w))
        assertIsHold (step AllMembers "demo" target w{ob_members = [(n, if n == "n3" then Left "down" else s) | (n, s) <- w.ob_members]})
        assertIsHold (step AllMembers "demo" target w{ob_members = filter ((/= "n3") . fst) w.ob_members})
        assertIsHold (step AllMembers "demo" target w{ob_members = w.ob_members <> [("n9", Right (fresh 170005))]})
        assertIsHold (step AllMembers "demo" target w{ob_members = [(n, if n == "n3" then Right (fresh 170005){ms_name = Just "n1"} else s) | (n, s) <- w.ob_members]})
    , testCase "no leader, another scope, a standby cluster: hold" $ do
        let w = world ["n1", "n2"] ["n2"] False
        assertIsHold (step AllMembers "demo" target w{ob_cluster = w.ob_cluster{cv_members = [m{m_role = "replica", m_state = Just "streaming"} | m <- w.ob_cluster.cv_members]}})
        assertIsHold (step AllMembers "other" target w)
        assertIsHold (step AllMembers "demo" target (roled "n1" "standby_leader" w))
    , testCase "verdicts: finished Success, due Failure, held Unknown" $ do
        assertEqual "" Success (stepVerdict Finished)
        assertBool "" (isFailure (stepVerdict (Restart "n2")))
        assertBool "" (isFailure (stepVerdict (SwitchOverTo "n1" "n2")))
        assertEqual "" Unknown (stepVerdict (Hold "x"))
    ]
  where
    assertIsHold (Hold _) = pure ()
    assertIsHold s = assertFailure ("expected Hold, got " <> show s)

-------------------------------------------------------------------------------

properties :: [TestTree]
properties =
    [ testProperty "a restart is never of the member that leads" $
        forAll genWorld $ \(phase, w) -> case step phase "demo" target w of
            Restart r -> counterexample (show w) (fmap (.m_name) (leaderOf w.ob_cluster) =/= Just r)
            _ -> ok
    , testProperty "a switchover goes from the leader to an eligible, up-to-date member, and only once every replica is" $
        forAll genWorld $ \(phase, w) -> case step phase "demo" target w of
            SwitchOverTo from to ->
                counterexample (show w) $
                    (phase === AllMembers)
                        .&&. (fmap (.m_name) (leaderOf w.ob_cluster) === Just from)
                        .&&. counterexample "eligible" (any (\m -> m.m_name == to && eligible w.ob_cluster m) w.ob_cluster.cv_members)
                        .&&. counterexample "stale" ([n | n <- names w, n /= from, isStale w n] === [])
            _ -> ok
    , testProperty "synchronous mode never targets an asynchronous member" $
        forAll genWorld $ \(phase, w0) ->
            let w = w0{ob_cluster = w0.ob_cluster{cv_synchronous = True}}
             in case step phase "demo" target w of
                    SwitchOverTo _ to -> counterexample (show w) (any (\m -> m.m_name == to && m.m_role `elem` syncRoles) w.ob_cluster.cv_members)
                    _ -> ok
    , testProperty "anything transitioning or unreadable holds, whatever else is due" $
        forAll ((,) <$> genWorld <*> elements [Just "starting", Just "stopped", Just "restarting", Nothing]) $ \((phase, w), st) ->
            case names w of
                [] -> ok
                (n : _) -> case step phase "demo" target (stated n st w) of
                    Hold _ -> ok
                    s -> counterexample (show s) False
    , testProperty "a run ends Finished with nothing stale, or held with the leader left alone; replicas strictly before the switchover" $
        forAll genWorld $ \(phase, w) ->
            let (trace, end) = simulate phase w
                switchAt = [i | (i, SwitchOverTo{}) <- zip [0 :: Int ..] trace]
                firstLeader = fmap (.m_name) (leaderOf w.ob_cluster)
             in counterexample (show (trace, end)) $
                    (length trace <= 2 * length (names w) + 1)
                        .&&. case step phase "demo" target end of
                            Finished ->
                                counterexample "stale left" $
                                    [n | n <- names end, isStale end n, phase == AllMembers || Just n /= fmap (.m_name) (leaderOf end.ob_cluster)] === []
                            Hold _ -> counterexample "held after a switchover" (null switchAt)
                            s -> counterexample ("not a fixed point: " <> show s) False
                        .&&. case switchAt of
                            [] -> ok
                            [i] -> counterexample "a replica restarted before the switchover is not done" ([r | Restart r <- take i trace, Just r == firstLeader] === [])
                            _ -> counterexample "more than one switchover" False
    , testProperty "killed after any step, the next pass finishes the same way (no progress is remembered)" $
        forAll genWorld $ \(phase, w) ->
            let (trace, end) = simulate phase w
             in conjoin'
                    [ simulate phase mid === (drop k trace, end)
                    | k <- [0 .. length trace]
                    , let mid = applyAll (take k trace) w
                    ]
    , testProperty "/patroni parses what Patroni would render" $
        forAll genStatus $ \s -> parseMemberStatus (LByteString.toStrict (encode (renderStatus "running" s))) === Right s
    , testProperty "refs are the scope's alone; notes carry the members, never the credentials" $
        forAll ((,,) <$> genWord <*> genWord <*> genWord) $ \(m, addr, cred) ->
            let c = sample{rm_members = [(Text.pack m, Text.pack addr)], rm_credentials_file = Just ("/etc/patroni" </> cred)}
                (l, r) = (ext (rollingMaintenance c), ext (rollingReplicas c))
                public = Text.unpack (Text.unlines (l.help : l.notes <> (r.help : r.notes)))
             in (l.ref === mkRef "patroni-rolling-leader" ("demo" :: Text))
                    .&&. (r.ref === mkRef "patroni-rolling-replicas" ("demo" :: Text))
                    .&&. counterexample public ((m <> " at " <> addr) `isInfixOf` public)
                    .&&. counterexample public (not (("/etc/patroni" </> cred) `isInfixOf` public))
    ]
  where
    ok = counterexample "" True
    a =/= b = counterexample (show a <> " == " <> show b) (a /= b)
    conjoin' = foldr (.&&.) ok

-------------------------------------------------------------------------------

nodeText :: [TestTree]
nodeText =
    [ testCase "the leader node depends on the replicas node, and only it is disruptive" $ do
        let top = rollingMaintenance sample
            preds = case runIdentity top.predecessors of
                Vertices xs -> xs
                _ -> []
        case preds of
            [p] -> assertEqual "it is the replicas node" (ext (rollingReplicas sample)).ref (ext p).ref
            _ -> assertFailure ("expected one predecessor, got " <> show (length preds))
        assertBool "leader disruptive" (Window.isDisruptive (ext top))
        assertBool "replicas not" (not (Window.isDisruptive (ext (rollingReplicas sample))))
    , testCase "help and notes" $ do
        let r = ext (rollingReplicas sample)
        assertEqual "help" "Patroni scope demo: restart the replicas that need it, one at a time" r.help
        assertEqual
            "notes"
            [ "members: n1 at 10.0.0.1:8008, n2 at 10.0.0.2:8008"
            , "needs a restart: pending_restart, or server_version below 170005"
            , "never restarts the member that leads"
            ]
            r.notes
    , testCase "a changed target version is a changed representative, same ref" $ do
        let a = ext (rollingMaintenance sample)
            b = ext (rollingMaintenance sample{rm_server_version = Just 170006})
        assertEqual "" a.ref b.ref
        assertBool "" (a.notes /= b.notes)
    ]

-------------------------------------------------------------------------------

-- | Shared state of the three fakes: per member (role, version, pending), and the requests seen.
data Fake = Fake
    { fMembers :: IORef (Map.Map Text (Text, Int, Bool))
    , fRequests :: IORef [(Text, Text, Maybe Value)]
    }

fakeApi :: [TestTree]
fakeApi =
    [ testCase "replicas, then a switchover, then the old leader; a second up sends nothing" $ withTempDir $ \tmp -> do
        let creds = tmp </> "rest-auth"
        writeFile creds "patroni:hunter2\n"
        withFakes [("n1", ("leader", 170004, False)), ("n2", ("replica", 170004, False)), ("n3", ("replica", 170004, True))] $ \c fake -> do
            let c' = c{rm_credentials_file = Just creds, rm_settle_seconds = 5}
                (leaderNode, replicasNode) = (ext (rollingMaintenance c'), ext (rollingReplicas c'))
            assertBool "replicas due" . isFailure =<< replicasNode.check
            replicasNode.up
            assertEqual "replicas done" Success =<< replicasNode.check
            assertBool "leader due" . isFailure =<< leaderNode.check
            leaderNode.up
            assertEqual "all done" Success =<< leaderNode.check
            reqs <- readIORef fake.fRequests
            assertEqual
                "order"
                [ ("n2", "restart", Just (object ["role" .= ("replica" :: Text)]))
                , ("n3", "restart", Just (object ["role" .= ("replica" :: Text)]))
                , ("n1", "switchover", Just (object ["leader" .= ("n1" :: Text), "candidate" .= ("n2" :: Text)]))
                , ("n1", "restart", Just (object ["role" .= ("replica" :: Text)]))
                ]
                [(m, p, b) | (m, p, b) <- reqs]
            leaderNode.up
            assertEqual "nothing more" 4 . length =<< readIORef fake.fRequests
    , testCase "a member with no declared address: Unknown, and up sends nothing" $
        withFakes [("n1", ("leader", 170004, False)), ("n2", ("replica", 170005, False))] $ \c fake -> do
            let c' = c{rm_members = [m | m <- c.rm_members, fst m == "n1"]}
            assertEqual "" Unknown =<< (ext (rollingMaintenance c')).check
            r <- try (ext (rollingMaintenance c')).up
            case r of
                Left (MaintenanceHeld why) -> assertBool (Text.unpack why) ("n2" `Text.isInfixOf` why)
                Right () -> assertFailure "up should have thrown"
            assertEqual "no request" [] =<< readIORef fake.fRequests
    , testCase "nobody listening is Unknown, not Failure" $ do
        v <- (ext (rollingMaintenance sample{rm_members = [("n1", "127.0.0.1:1")]})).check
        assertEqual "" Unknown v
    ]

{- | Three (or more) fake members on their own ports. A restart sets the
member to 170005 and clears pending; one of a leader is refused, as Patroni
does with @role: replica@. A switchover moves the leader.
-}
withFakes :: [(Text, (Text, Int, Bool))] -> (RollingMaintenance -> Fake -> IO a) -> IO a
withFakes initial act = do
    fake <- Fake <$> newIORef (Map.fromList initial) <*> newIORef []
    go fake (fmap fst initial) []
  where
    go fake [] ports = act sample{rm_members = reverse ports} fake
    go fake (n : rest) ports =
        Warp.testWithApplication (pure (app fake n)) $ \port ->
            go fake rest ((n, "127.0.0.1:" <> Text.pack (show port)) : ports)

    app :: Fake -> Text -> Wai.Application
    app fake self req respond = do
        ms <- readIORef fake.fMembers
        let members = [Member n r (Just (if r == "leader" then "running" else "streaming")) (Just 0) False | (n, (r, _, _)) <- Map.toList ms]
        case (Wai.requestMethod req, Wai.pathInfo req) of
            ("GET", ["cluster"]) -> respond (json (renderCluster members))
            ("GET", ["config"]) -> respond (json (object ["ttl" .= (30 :: Int)]))
            ("GET", ["patroni"]) -> case Map.lookup self ms of
                Just (_, v, p) -> respond (json (renderStatus "running" (MemberStatus (Just self) (Just "demo") (Just v) p)))
                Nothing -> respond (Wai.responseLBS HTTP.status503 [] "")
            ("POST", [path]) -> do
                body <- Wai.strictRequestBody req
                let auth = lookup "Authorization" (Wai.requestHeaders req)
                atomicModifyIORef' fake.fRequests (\xs -> (xs <> [(self, path, decode body)], ()))
                case (auth, path) of
                    (Just "Basic cGF0cm9uaTpodW50ZXIy", "restart")
                        | Just ("leader", _, _) <- Map.lookup self ms -> respond (Wai.responseLBS HTTP.status503 [] "restart conditions are not satisfied")
                        | otherwise -> do
                            atomicModifyIORef' fake.fMembers (\m -> (Map.adjust (\(r, _, _) -> (r, 170005, False)) self m, ()))
                            respond (Wai.responseLBS HTTP.status200 [] "restarted successfully")
                    (Just "Basic cGF0cm9uaTpodW50ZXIy", "switchover") | Just (Object o) <- decode body, Just (String cand) <- KeyMap.lookup "candidate" o -> do
                        atomicModifyIORef' fake.fMembers (\m -> (Map.mapWithKey (\k (_, v, p) -> (if k == cand then "leader" else "replica", v, p)) m, ()))
                        respond (Wai.responseLBS HTTP.status200 [] "Successfully switched over")
                    _ -> respond (Wai.responseLBS HTTP.status401 [] "no auth header received")
            _ -> respond (Wai.responseLBS HTTP.status404 [] "")
    json v = Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/json")] (encode (v :: Value))

-------------------------------------------------------------------------------

target :: Maybe Int
target = Just 170005

sample :: RollingMaintenance
sample = RollingMaintenance "demo" [("n1", "10.0.0.1:8008"), ("n2", "10.0.0.2:8008")] target Nothing defaultSettleSeconds

ext :: Op -> Extension
ext o = case o.node of
    Actions a -> a.extension
    Actionless -> error "no actions"

fresh :: Int -> MemberStatus
fresh v = MemberStatus Nothing (Just "demo") (Just v) False

-- | The first name leads; the second list is who is stale (on 170004, others on 170005).
world :: [Text] -> [Text] -> Bool -> Observation
world ns staleOnes sync =
    Observation
        (ClusterView (Just "demo") [Member n (if i == 0 then "leader" else "replica") (Just (if i == 0 then "running" else "streaming")) (Just 0) False | (i, n) <- zip [0 :: Int ..] ns] sync)
        [(n, Right (MemberStatus (Just n) (Just "demo") (Just (if n `elem` staleOnes then 170004 else 170005)) False)) | n <- ns]

onMember :: Text -> (Member -> Member) -> Observation -> Observation
onMember n f w = w{ob_cluster = w.ob_cluster{cv_members = [if m.m_name == n then f m else m | m <- w.ob_cluster.cv_members]}}

lagged :: [(Text, Integer)] -> Observation -> Observation
lagged ls w = foldr (\(n, l) -> onMember n (\m -> m{m_lag = Just l})) w ls

roled :: Text -> Text -> Observation -> Observation
roled n r = onMember n (\m -> m{m_role = r})

stated :: Text -> Maybe Text -> Observation -> Observation
stated n s = onMember n (\m -> m{m_state = s})

nofailover :: Text -> Observation -> Observation
nofailover n = onMember n (\m -> m{m_nofailover = True})

names :: Observation -> [Text]
names w = fmap (.m_name) w.ob_cluster.cv_members

isStale :: Observation -> Text -> Bool
isStale w n = case lookup n w.ob_members of
    Just (Right s) -> stale target s == Right True
    _ -> False

-- | What the step would do to the cluster, had Patroni done it at once.
apply :: Step -> Observation -> Observation
apply (Restart r) w = w{ob_members = [(n, if n == r then fmap (\s -> s{ms_pending_restart = False, ms_server_version = target}) s else s) | (n, s) <- w.ob_members]}
apply (SwitchOverTo from to) w =
    onMember from (\m -> m{m_role = "replica", m_state = Just "streaming"}) $
        onMember to (\m -> m{m_role = "leader", m_state = Just "running"}) w
apply _ w = w

applyAll :: [Step] -> Observation -> Observation
applyAll ss w = foldl (flip apply) w ss

-- | Steps taken until Finished or Hold (bounded), and the world they leave.
simulate :: Phase -> Observation -> ([Step], Observation)
simulate phase = go (50 :: Int)
  where
    go 0 w = ([], w)
    go k w = case step phase "demo" target w of
        Finished -> ([], w)
        Hold _ -> ([], w)
        s -> let (rest, end) = go (k - 1) (apply s w) in (s : rest, end)

-- | A cluster where everyone answers and is healthy, which members are stale varies.
genWorld :: Gen (Phase, Observation)
genWorld = do
    ns <- nub <$> listOf1 (elements ["n0", "n1", "n2", "n3", "n4"])
    ms <- forM (zip [0 :: Int ..] ns) $ \(i, n) ->
        if i == 0
            then Member n <$> elements leaderRoles <*> pure (Just "running") <*> pure Nothing <*> pure False
            else
                Member n
                    <$> elements ["replica", "replica", "sync_standby", "quorum_standby"]
                    <*> elements [Just "streaming", Just "running", Just "in archive recovery"]
                    <*> oneof [pure Nothing, Just . fromIntegral <$> chooseInt (0, 1000)]
                    <*> frequency [(4, pure False), (1, pure True)]
    sts <- forM ns $ \n -> (,) n . Right <$> (MemberStatus (Just n) (Just "demo") <$> (Just <$> elements [170004, 170005]) <*> frequency [(4, pure False), (1, pure True)])
    sync <- elements [False, True]
    phase <- elements [ReplicasOnly, AllMembers]
    pure (phase, Observation (ClusterView (Just "demo") ms sync) sts)

genStatus :: Gen MemberStatus
genStatus =
    MemberStatus
        <$> oneof [pure Nothing, Just . Text.pack <$> genWord]
        <*> oneof [pure Nothing, Just . Text.pack <$> genWord]
        <*> oneof [pure Nothing, Just <$> chooseInt (90000, 180000)]
        <*> elements [False, True]

genWord :: Gen String
genWord = listOf1 (elements (['a' .. 'z'] <> ['0' .. '9']))

renderStatus :: Text -> MemberStatus -> Value
renderStatus st s =
    Object . KeyMap.fromList $
        [("state", String st), ("pending_restart", Bool s.ms_pending_restart), ("timeline", Number 2)]
            <> maybe [] (\v -> [("server_version", Number (fromIntegral v))]) s.ms_server_version
            <> [ ( Key.fromText "patroni"
                 , Object . KeyMap.fromList $
                    [("version", String "4.0.7")]
                        <> maybe [] (\n -> [("name", String n)]) s.ms_name
                        <> maybe [] (\sc -> [("scope", String sc)]) s.ms_scope
                 )
               ]

renderCluster :: [Member] -> Value
renderCluster ms = object ["scope" .= ("demo" :: Text), "members" .= fmap member ms]
  where
    member :: Member -> Value
    member m =
        object
            [ "name" .= m.m_name
            , "role" .= m.m_role
            , "state" .= m.m_state
            , "lag" .= m.m_lag
            , "host" .= ("127.0.0.1" :: Text)
            , "port" .= (5432 :: Int)
            ]

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False
