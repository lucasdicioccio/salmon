{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Builtin.Nodes.Patroni.PreferredLeader": the pure decision and its
parsers (unit cases and QuickCheck properties), the node's public text, and
@check@\/@up@ against an in-process fake of Patroni's REST API (warp). No real
Patroni is run here.
-}
module Test.PatroniPreferredLeaderSpec (tests) where

import Control.Exception (try)
import Control.Monad (forM)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, nub)
import Data.Maybe (mapMaybe)
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
import Salmon.Builtin.Nodes.Patroni.PreferredLeader
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (mkRef)

import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Patroni.PreferredLeader"
        [ testGroup "parsing" parsing
        , testGroup "decision" decisions
        , testGroup "properties" properties
        , testGroup "node" nodeText
        , testGroup "against a fake REST API" fakeApi
        ]

-------------------------------------------------------------------------------

parsing :: [TestTree]
parsing =
    [ testCase "a Patroni 4 /cluster body" $
        case parseCluster patroni4Cluster of
            Left e -> assertFailure (Text.unpack e)
            Right cv -> do
                assertEqual "scope" (Just "demo") cv.cv_scope
                assertEqual
                    "members"
                    [ Member "n1" "leader" (Just "running") Nothing False
                    , Member "n2" "replica" (Just "streaming") (Just 0) False
                    , Member "n3" "replica" (Just "streaming") Nothing True
                    ]
                    cv.cv_members
                assertEqual "sync is /config's to say" False cv.cv_synchronous
    , testCase "synchronous_mode: true, quorum, absent, off" $ do
        assertEqual "true" (Right True) (parseSynchronousMode "{\"synchronous_mode\":true}")
        assertEqual "quorum" (Right True) (parseSynchronousMode "{\"synchronous_mode\":\"quorum\"}")
        assertEqual "absent" (Right False) (parseSynchronousMode "{\"ttl\":30}")
        assertEqual "false" (Right False) (parseSynchronousMode "{\"synchronous_mode\":false}")
    , testCase "a synchronous_mode not understood is an error, not off" $ do
        assertBool "" (isLeft (parseSynchronousMode "{\"synchronous_mode\":\"maybe\"}"))
        assertBool "" (isLeft (parseSynchronousMode "{\"synchronous_mode\":3}"))
        assertBool "" (isLeft (parseSynchronousMode "nope"))
    , testCase "credentials: user:password on the first line" $ do
        assertEqual "" (Right ("patroni", "s3:cr3t")) (parseCredentials "patroni:s3:cr3t\nignored\n")
        assertEqual "crlf" (Right ("u", "p")) (parseCredentials "u:p\r\n")
        assertBool "no colon" (isLeft (parseCredentials "just-a-password\n"))
        assertBool "no user" (isLeft (parseCredentials ":pw\n"))
    , testCase "the switchover body names leader and candidate" $
        assertEqual "" (Just (object ["leader" .= ("n1" :: Text), "candidate" .= ("n2" :: Text)])) (decode (switchoverBody "n1" "n2"))
    ]

decisions :: [TestTree]
decisions =
    [ testCase "the preferred member leads" $
        assertEqual "" AlreadyLeads (decide "demo" "n1" (cluster False [leader "n1", replica "n2"]))
    , testCase "an eligible replica is switched to, from the actual leader" $
        assertEqual "" (SwitchOver "n1" "n2") (decide "demo" "n2" (cluster False [leader "n1", replica "n2"]))
    , testCase "in archive recovery is eligible; stopped and starting are not" $ do
        assertEqual "" (SwitchOver "n1" "n2") (decide "demo" "n2" (cluster False [leader "n1", (replica "n2"){m_state = Just "in archive recovery"}]))
        assertIsWait (decide "demo" "n2" (cluster False [leader "n1", (replica "n2"){m_state = Just "stopped"}]))
        assertIsWait (decide "demo" "n2" (cluster False [leader "n1", (replica "n2"){m_state = Just "starting"}]))
    , testCase "synchronous mode: an async preferred member waits, it does not fall back" $ do
        assertIsWait (decide "demo" "n2" (cluster True [leader "n1", replica "n2", (replica "n3"){m_role = "sync_standby"}]))
        assertEqual "" (SwitchOver "n1" "n3") (decide "demo" "n3" (cluster True [leader "n1", replica "n2", (replica "n3"){m_role = "sync_standby"}]))
        assertEqual "quorum" (SwitchOver "n1" "n3") (decide "demo" "n3" (cluster True [leader "n1", (replica "n3"){m_role = "quorum_standby"}]))
    , testCase "no leader: wait, a failover is Patroni's" $
        assertIsWait (decide "demo" "n2" (cluster False [replica "n1", replica "n2"]))
    , testCase "nofailover, unknown member and another scope wait" $ do
        assertIsWait (decide "demo" "n2" (cluster False [leader "n1", (replica "n2"){m_nofailover = True}]))
        assertIsWait (decide "demo" "n9" (cluster False [leader "n1", replica "n2"]))
        assertIsWait (decide "other" "n2" (cluster False [leader "n1", replica "n2"]))
    , testCase "the verdicts: lead is Success, due is Failure, wait is Unknown" $ do
        assertEqual "" Success (decisionVerdict AlreadyLeads)
        assertBool "" (isFailure (decisionVerdict (SwitchOver "a" "b")))
        assertEqual "" Unknown (decisionVerdict (Wait "x"))
    , testCase "the candidate is the lowest lag, unknown lag last, never the leader" $ do
        let cv = cluster False [(leader "n0"){m_lag = Just 0}, (replica "n1"){m_lag = Nothing}, (replica "n2"){m_lag = Just 50}, (replica "n3"){m_lag = Just 10}]
        assertEqual "" ["n3", "n2", "n1"] (fmap (.m_name) (switchoverCandidates cv))
    , testCase "no candidate in sync mode without a sync standby" $
        assertEqual "" Nothing (switchoverCandidate (cluster True [leader "n1", replica "n2"]))
    ]
  where
    assertIsWait (Wait _) = pure ()
    assertIsWait d = assertFailure ("expected Wait, got " <> show d)

-------------------------------------------------------------------------------

properties :: [TestTree]
properties =
    [ testProperty "a switchover goes from the leader to the preferred member, which is eligible" $
        forAll genCase $ \(pref, cv) -> case decide "demo" pref cv of
            SwitchOver from to ->
                counterexample (show cv) $
                    (fmap (.m_name) (leaderOf cv) === Just from)
                        .&&. (to === pref)
                        .&&. counterexample "eligible" (any (\m -> m.m_name == to && eligible cv m) cv.cv_members)
            _ -> counterexample "" True
    , testProperty "synchronous mode never targets an asynchronous member" $
        forAll genCase $ \(pref, cv0) ->
            let cv = cv0{cv_synchronous = True}
             in case decide "demo" pref cv of
                    SwitchOver _ to -> counterexample (show cv) (any (\m -> m.m_name == to && m.m_role `elem` syncRoles) cv.cv_members)
                    _ -> counterexample "" True
    , testProperty "the preferred member leads iff the decision is AlreadyLeads" $
        forAll genCase $ \(pref, cv) ->
            (decide "demo" pref cv == AlreadyLeads) === (fmap (.m_name) (leaderOf cv) == Just pref)
    , testProperty "after the switchover it asked for, the node asks for nothing more" $
        forAll genCase $ \(pref, cv) -> case decide "demo" pref cv of
            SwitchOver from to -> decide "demo" pref (applySwitch from to cv) === AlreadyLeads
            _ -> counterexample "" True
    , testProperty "the candidate is eligible, not the leader, and has the lowest known lag" $
        forAll genCluster $ \cv -> case switchoverCandidate cv of
            Nothing -> filter (eligible cv) cv.cv_members === []
            Just c ->
                let others = filter (eligible cv) cv.cv_members
                    known = mapMaybe (.m_lag) others
                 in counterexample (show cv) $
                        eligible cv c
                            && c.m_role `notElem` leaderRoles
                            && case c.m_lag of
                                Just l -> all (l <=) known
                                Nothing -> null known
    , testProperty "/cluster parses what Patroni would render" $
        forAll genCluster $ \cv ->
            parseCluster (LByteString.toStrict (encode (renderCluster cv))) === Right cv{cv_synchronous = False}
    , testProperty "credentials round-trip, and an error never quotes the file" $
        -- the Z marks the password (the error text has no Z), so a one-letter one is not "found" in it
        forAll ((,) <$> genWord False <*> (("Z" <>) <$> genWord True)) $ \(u, p) ->
            let file = Char8.pack (u <> ":" <> p <> "\n")
             in (parseCredentials file === Right (Char8.pack u, Char8.pack p))
                    .&&. case parseCredentials (Char8.pack p) of
                        Left e -> counterexample (Text.unpack e) (not (Text.pack p `Text.isInfixOf` e))
                        Right _ -> counterexample "" True
    , testProperty "the ref is the scope's alone; help and notes name the member, never the credentials" $
        forAll ((,,) <$> genWord False <*> genWord False <*> genWord False) $ \(m, addr, cred) ->
            let c = sample{pl_member = Text.pack m, pl_rest_address = Text.pack addr, pl_credentials_file = Just ("/etc/patroni" </> cred)}
                e = ext (preferredLeader c)
                public = Text.unpack (Text.unlines (e.help : e.notes))
             in (e.ref === mkRef "patroni-preferred-leader" ("demo" :: Text))
                    .&&. counterexample public (m `isInfixOf` public)
                    .&&. counterexample public (not (("/etc/patroni" </> cred) `isInfixOf` public))
    ]

-------------------------------------------------------------------------------

nodeText :: [TestTree]
nodeText =
    [ testCase "ref, help and notes" $ do
        let e = ext (preferredLeader sample)
        assertEqual "ref" (mkRef "patroni-preferred-leader" ("demo" :: Text)) e.ref
        assertEqual "help" "Patroni scope demo prefers n2 as leader" e.help
        assertEqual
            "notes"
            [ "opt-in: moves the leader back to n2 after a failover"
            , "switchover only to an eligible member; with synchronous_mode, only to a sync standby"
            , "down leaves the leader where it is"
            ]
            e.notes
    , testCase "a changed preference is a changed representative, same ref" $ do
        let a = ext (preferredLeader sample)
            b = ext (preferredLeader sample{pl_member = "n3"})
        assertEqual "" a.ref b.ref
        assertBool "" (a.help /= b.help && a.notes /= b.notes)
    ]

-------------------------------------------------------------------------------

-- | What the fake saw: switchover requests, as (authorization header, body).
data Fake = Fake
    { fLeader :: IORef Text
    , fRequests :: IORef [(Maybe ByteString.ByteString, LByteString.ByteString)]
    }

fakeApi :: [TestTree]
fakeApi =
    [ testCase "check, up with credentials, check again" $ withTempDir $ \tmp -> do
        let creds = tmp </> "rest-auth"
        writeFile creds "patroni:hunter2\n"
        withFake False [replica "n2"] $ \port fake -> do
            let c = sample{pl_rest_address = local port, pl_credentials_file = Just creds, pl_settle_seconds = 5}
                e = ext (preferredLeader c)
            v <- e.check
            assertBool (show v) (isFailure v)
            e.up
            reqs <- readIORef fake.fRequests
            assertEqual "one POST, basic auth, the body" [(Just "Basic cGF0cm9uaTpodW50ZXIy", switchoverBody "n1" "n2")] reqs
            assertEqual "leads now" Success =<< e.check
            e.up
            assertEqual "a second up sends nothing" 1 . length =<< readIORef fake.fRequests
    , testCase "sync mode without a sync standby: Unknown, and up sends nothing" $
        withFake True [replica "n2"] $ \port fake -> do
            let e = ext (preferredLeader sample{pl_rest_address = local port})
            assertEqual "" Unknown =<< e.check
            r <- try e.up
            case r of
                Left (SwitchoverSkipped why) -> assertBool (Text.unpack why) ("synchronous" `Text.isInfixOf` why)
                Right () -> assertFailure "up should have thrown"
            assertEqual "no POST" [] =<< readIORef fake.fRequests
            assertEqual "leader untouched" "n1" =<< readIORef fake.fLeader
    , testCase "refused credentials: the error says so and does not quote them" $ withTempDir $ \tmp -> do
        let creds = tmp </> "rest-auth"
        writeFile creds "patroni:wrong-one\n"
        withFake False [replica "n2"] $ \port _ -> do
            r <- try (ext (preferredLeader sample{pl_rest_address = local port, pl_credentials_file = Just creds})).up
            case r of
                Left (SwitchoverRefused why) -> do
                    assertBool (Text.unpack why) ("401" `Text.isInfixOf` why)
                    assertBool (Text.unpack why) (not ("wrong-one" `Text.isInfixOf` why))
                Right () -> assertFailure "up should have thrown"
    , testCase "nobody listening is a Failure" $ do
        v <- (ext (preferredLeader sample{pl_rest_address = "127.0.0.1:1"})).check
        assertBool (show v) (isFailure v)
    ]
  where
    local port = "127.0.0.1:" <> Text.pack (show port)

-- | A cluster whose leader is @n1@, the given replicas, and @n2@ as the member answering.
withFake :: Bool -> [Member] -> (Int -> Fake -> IO a) -> IO a
withFake sync replicas act = do
    fake <- Fake <$> newIORef "n1" <*> newIORef []
    Warp.testWithApplication (pure (app fake)) $ \port -> act port fake
  where
    app :: Fake -> Wai.Application
    app fake req respond = do
        current <- readIORef fake.fLeader
        let members = leader current : [if r.m_name == current then replica "n1" else r | r <- replicas]
        case (Wai.requestMethod req, Wai.pathInfo req) of
            ("GET", ["leader"]) -> respond (Wai.responseLBS (if current == "n2" then HTTP.status200 else HTTP.status503) [] "{}")
            ("GET", ["cluster"]) -> respond (json HTTP.status200 (renderCluster (cluster sync members)))
            ("GET", ["config"]) -> respond (json HTTP.status200 (object ["synchronous_mode" .= sync, "ttl" .= (30 :: Int)]))
            ("POST", ["switchover"]) -> do
                body <- Wai.strictRequestBody req
                let auth = lookup "Authorization" (Wai.requestHeaders req)
                atomicModifyIORef' fake.fRequests (\xs -> (xs <> [(auth, body)], ()))
                if auth == Just "Basic cGF0cm9uaTpodW50ZXIy"
                    then do
                        atomicModifyIORef' fake.fLeader (const ("n2", ()))
                        respond (Wai.responseLBS HTTP.status200 [] "Successfully switched over to \"n2\"")
                    else respond (Wai.responseLBS HTTP.status401 [] "no auth header received")
            _ -> respond (Wai.responseLBS HTTP.status404 [] "")
    json st v = Wai.responseLBS st [(HTTP.hContentType, "application/json")] (encode (v :: Value))

-------------------------------------------------------------------------------

sample :: PreferredLeader
sample = PreferredLeader "demo" "n2" "10.0.0.2:8008" Nothing defaultSettleSeconds

ext :: Op -> Extension
ext o = case o.node of
    Actions a -> a.extension
    Actionless -> error "no actions"

cluster :: Bool -> [Member] -> ClusterView
cluster sync ms = ClusterView (Just "demo") ms sync

leader, replica :: Text -> Member
leader n = Member n "leader" (Just "running") Nothing False
replica n = Member n "replica" (Just "streaming") (Just 0) False

applySwitch :: Text -> Text -> ClusterView -> ClusterView
applySwitch from to cv = cv{cv_members = fmap swap cv.cv_members}
  where
    swap m
        | m.m_name == to = m{m_role = "leader", m_state = Just "running", m_lag = Nothing}
        | m.m_name == from = m{m_role = "replica", m_state = Just "streaming"}
        | otherwise = m

-- | @GET \/cluster@ as Patroni renders it (the fields this module reads, plus some it does not).
renderCluster :: ClusterView -> Value
renderCluster cv =
    object $
        ["members" .= fmap member cv.cv_members]
            <> maybe [] (\s -> ["scope" .= s]) cv.cv_scope
  where
    member :: Member -> Value
    member m =
        Object . KeyMap.fromList $
            [ ("name", String m.m_name)
            , ("role", String m.m_role)
            , ("host", String "10.0.0.9")
            , ("port", Number 5432)
            , ("lag", maybe (String "unknown") (Number . fromInteger) m.m_lag)
            ]
                <> maybe [] (\s -> [("state", String s)]) m.m_state
                <> [(Key.fromText "tags", object ["nofailover" .= True]) | m.m_nofailover]

patroni4Cluster :: ByteString.ByteString
patroni4Cluster =
    "{\"members\":[\
    \{\"name\":\"n1\",\"role\":\"leader\",\"state\":\"running\",\"api_url\":\"http://10.0.0.1:8008/patroni\",\"host\":\"10.0.0.1\",\"port\":5432,\"timeline\":2},\
    \{\"name\":\"n2\",\"role\":\"replica\",\"state\":\"streaming\",\"api_url\":\"http://10.0.0.2:8008/patroni\",\"host\":\"10.0.0.2\",\"port\":5432,\"timeline\":2,\"receive_lag\":0,\"replay_lag\":0,\"lag\":0},\
    \{\"name\":\"n3\",\"role\":\"replica\",\"state\":\"streaming\",\"host\":\"10.0.0.3\",\"port\":5432,\"timeline\":2,\"lag\":\"unknown\",\"tags\":{\"nofailover\":true}}\
    \],\"scope\":\"demo\"}"

-------------------------------------------------------------------------------

genCluster :: Gen ClusterView
genCluster = do
    names <- nub <$> listOf1 (elements ["n0", "n1", "n2", "n3", "n4", "n5"])
    hasLeader <- frequency [(5, pure True), (1, pure False)]
    leaderIdx <- chooseInt (0, length names - 1)
    ms <- forM (zip [0 :: Int ..] names) $ \(i, n) ->
        if hasLeader && i == leaderIdx
            then Member n <$> elements leaderRoles <*> pure (Just "running") <*> pure Nothing <*> pure False
            else
                Member n
                    <$> elements ["replica", "replica", "sync_standby", "quorum_standby", "standby_leader"]
                    <*> oneof [pure Nothing, Just <$> elements (healthyStates <> ["stopped", "starting", "crashed", "creating replica"])]
                    <*> oneof [pure Nothing, Just . fromIntegral <$> chooseInt (0, 1000)]
                    <*> frequency [(4, pure False), (1, pure True)]
    sync <- elements [False, True]
    scope <- elements [Just "demo", Nothing]
    pure (ClusterView scope ms sync)

genCase :: Gen (Text, ClusterView)
genCase = do
    cv <- genCluster
    pref <- elements ("n9" : fmap (.m_name) cv.cv_members)
    pure (pref, cv)

-- | A short word; with @free@, any printable but newline (a password may hold colons).
genWord :: Bool -> Gen String
genWord free = listOf1 (elements (if free then ['a' .. 'z'] <> ":/+= !@" else ['a' .. 'z'] <> ['0' .. '9']))

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
