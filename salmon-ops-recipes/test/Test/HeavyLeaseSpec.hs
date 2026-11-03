{- | "Test.HeavyLease": the environment gate, the request and answer shapes,
and acquire\/refresh\/release against an in-process fake of buckette's
reservation routes (warp). No live buckette is involved.
-}
module Test.HeavyLeaseSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Test.Harness (withTempDir)
import Test.HeavyLease

tests :: TestTree
tests =
    testGroup
        "Test.HeavyLease"
        [ testGroup "gate" gate
        , testGroup "wire format" wire
        , testGroup "against a fake buckette" fake
        ]

envOf :: [(String, String)] -> String -> Maybe String
envOf kvs k = lookup k kvs

gate :: [TestTree]
gate =
    [ testCase "unset: no lease" $
        assertBool "" (either (const False) isNothing (leaseConfigFrom (envOf [])))
    , testCase "empty or blank: no lease" $
        assertBool "" (either (const False) isNothing (leaseConfigFrom (envOf [(gateVar, "  ")])))
    , testCase "set without a base URL or key file: an error, not a silent run without it" $ do
        assertBool "no base" (isLeft (leaseConfigFrom (envOf [(gateVar, "b"), (keyFileVar, "/k")])))
        assertBool "no key" (isLeft (leaseConfigFrom (envOf [(gateVar, "b"), (baseUrlVar, "http://x")])))
    , testCase "set: the agreed bucket, dimension, expiry and refresh period" $
        case leaseConfigFrom (envOf [(gateVar, "bkt"), (baseUrlVar, "https://h/p/"), (keyFileVar, "/k")]) of
            Right (Just c) -> do
                assertEqual "bucket" "bkt" c.lc_bucket
                assertEqual "base, trailing slash dropped" "https://h/p" c.lc_baseUrl
                assertEqual "key file" "/k" c.lc_keyFile
                assertEqual "dimension" "slots" c.lc_dimension
                assertEqual "ttl" 600 c.lc_ttl
                assertEqual "refresh" (180 * 1000000) c.lc_refreshEvery
                assertBool "wait within buckette's 300s cap" (c.lc_waitSecs > 0 && c.lc_waitSecs <= 300)
            other -> assertFailure ("expected a config, got " <> either id (const "Nothing") other)
    ]
  where
    isLeft = either (const True) (const False)

sampleConfig :: String -> FilePath -> LeaseConfig
sampleConfig base key =
    LeaseConfig
        { lc_baseUrl = base
        , lc_bucket = "bkt"
        , lc_keyFile = key
        , lc_dimension = "slots"
        , lc_ttl = 600
        , lc_refreshEvery = 50000
        , lc_waitSecs = 1
        }

wire :: [TestTree]
wire =
    [ testCase "reserve body: one slot, Wait mode, expiry, transaction id" $ do
        let t = UTCTime (fromGregorian 2026 11 3) 0
        assertEqual
            ""
            ( Just $
                object
                    [ "rrvAmounts" .= object ["slots" .= (1 :: Int)]
                    , "rrvMode" .= object ["tag" .= ("Wait" :: Text), "contents" .= object ["wcMaxDelaySecs" .= (1 :: Int)]]
                    , "rrvExpiresAt" .= ("2026-11-03T00:00:00Z" :: Text)
                    , "rrvTransactionId" .= ("tx" :: Text)
                    ]
            )
            (decode (encode (reserveBody (sampleConfig "" "") t "tx")) :: Maybe Value)
    , testCase "refresh body" $
        assertEqual
            ""
            "{\"rfrExpiresAt\":\"2026-11-03T00:00:00Z\"}"
            (encode (refreshBody (UTCTime (fromGregorian 2026 11 3) 0)))
    , testCase "ReserveSuccess: the reservation id" $
        assertEqual "" (Right (Reserved "r-1")) (parseReserveAnswer (successBody "r-1"))
    , testCase "ReserveShortfall: asked again, with the shortfall" $
        assertEqual "" (Right (Shortfall (object ["slots" .= (1 :: Int)]))) (parseReserveAnswer shortfallBody)
    , testCase "ReservePending, unknown tags, non-JSON" $ do
        assertEqual "pending" (Right Pending) (parseReserveAnswer "{\"tag\":\"ReservePending\",\"roPendingId\":\"x\"}")
        assertBool "unknown" (either (const True) (const False) (parseReserveAnswer "{\"tag\":\"Nope\"}"))
        assertBool "garbage" (either (const True) (const False) (parseReserveAnswer "<html>"))
        assertBool "success without id" (either (const True) (const False) (parseReserveAnswer "{\"tag\":\"ReserveSuccess\",\"rsReservation\":{}}"))
    , testCase "transaction id: host, pid, tier, start, attempt" $
        assertEqual "" "box:42:qemu:1700000000:3" (transactionId "box" 42 "qemu" 1700000000 3)
    ]

successBody :: Text -> LByteString.ByteString
successBody rid =
    encode $
        object
            [ "tag" .= ("ReserveSuccess" :: Text)
            , "rsReservation"
                .= object
                    [ "rvId" .= rid
                    , "rvBucketId" .= ("bkt" :: Text)
                    , "rvAmounts" .= object ["slots" .= (1 :: Int)]
                    , "rvCreatedAt" .= ("2026-11-03T00:00:00Z" :: Text)
                    , "rvStatus" .= ("active" :: Text)
                    ]
            ]

shortfallBody :: LByteString.ByteString
shortfallBody =
    encode $
        object
            [ "tag" .= ("ReserveShortfall" :: Text)
            , "roShortfall" .= object ["slots" .= (1 :: Int)]
            , "roAvailable" .= object []
            ]

-------------------------------------------------------------------------------
-- fake buckette

data Seen = Seen
    { seenMethod :: String
    , seenPath :: String
    , seenAuth :: Maybe String
    , seenBody :: LByteString.ByteString
    }

{- | Answers reserves with the given list in turn (the last one repeats),
refreshes with the given status, ETA with 403 (a key scoped to the lease
cannot read), DELETE with 204, and records every request.
-}
fakeBuckette :: IORef [Seen] -> IORef [LByteString.ByteString] -> HTTP.Status -> Wai.Application
fakeBuckette seen reserves refreshStatus req respond = do
    body <- Wai.strictRequestBody req
    let m = Char8.unpack (Wai.requestMethod req)
        p = Char8.unpack (Wai.rawPathInfo req)
        auth = Char8.unpack <$> lookup "Authorization" (Wai.requestHeaders req)
    atomicModifyIORef' seen (\xs -> (xs <> [Seen m p auth body], ()))
    let json s b = respond (Wai.responseLBS s [("Content-Type", "application/json")] b)
    case (m, p) of
        ("POST", "/buckets/bkt/reservations") -> do
            b <- atomicModifyIORef' reserves (\xs -> case xs of
                [x] -> ([x], x)
                (x : rest) -> (rest, x)
                [] -> ([], "{}"))
            json HTTP.status200 b
        ("POST", "/buckets/bkt/eta") -> json HTTP.status403 "{\"errCode\":\"forbidden\"}"
        ("DELETE", "/buckets/bkt/reservations/r-1") -> respond (Wai.responseLBS HTTP.status204 [] "")
        ("POST", "/buckets/bkt/reservations/r-1/refresh") -> json refreshStatus "{}"
        _ -> json HTTP.status404 "{}"

withFake :: [LByteString.ByteString] -> HTTP.Status -> (LeaseConfig -> IO [Seen] -> IO a) -> IO a
withFake answers refreshStatus act =
    withTempDir $ \dir -> do
        let keyFile = dir </> "key"
        writeFile keyFile "test-key\n"
        seen <- newIORef []
        reserves <- newIORef answers
        Warp.testWithApplication (pure (fakeBuckette seen reserves refreshStatus)) $ \port ->
            act (sampleConfig ("http://127.0.0.1:" <> show port) keyFile) (readIORef seen)

count :: String -> String -> [Seen] -> Int
count m p = length . filter (\s -> s.seenMethod == m && s.seenPath == p)

fake :: [TestTree]
fake =
    [ testCase "waits through a shortfall, holds and refreshes, then cancels (never commits)" $
        withFake [shortfallBody, successBody "r-1"] HTTP.status200 $ \cfg readSeen -> do
            lease <- acquireLease cfg "qemu"
            assertEqual "lease id" "r-1" lease.lease_id
            threadDelay 300000
            releaseLease cfg lease
            afterRelease <- readSeen
            threadDelay 200000
            later <- readSeen
            let reservesSeen = filter (\s -> s.seenPath == "/buckets/bkt/reservations") afterRelease
                txids = [t | s <- reservesSeen, Just (Object o) <- [decode s.seenBody], Just (String t) <- [KeyMap.lookup "rrvTransactionId" o]]
            assertEqual "two reserve calls" 2 (length reservesSeen)
            assertBool ("attempts numbered: " <> show txids) (case txids of [a, b] -> ":qemu:" `Text.isInfixOf` a && ":1" `Text.isSuffixOf` a && ":2" `Text.isSuffixOf` b; _ -> False)
            assertBool "every call carries the key as a bearer token" (all (\s -> s.seenAuth == Just "Bearer test-key") afterRelease)
            assertEqual "asked the ETA once, between attempts" 1 (count "POST" "/buckets/bkt/eta" afterRelease)
            assertBool "refreshed while held" (count "POST" "/buckets/bkt/reservations/r-1/refresh" afterRelease >= 2)
            assertEqual "cancelled once" 1 (count "DELETE" "/buckets/bkt/reservations/r-1" afterRelease)
            assertEqual "never committed" 0 (length (filter (("/commit" `isSuffixOf`) . (.seenPath)) afterRelease))
            assertEqual "no refresh after release" (length afterRelease) (length later)
    , testCase "a refused reserve fails the lease, without the key in the error" $
        withTempDir $ \dir -> do
            let keyFile = dir </> "key"
            writeFile keyFile "test-key"
            let app _ respond = respond (Wai.responseLBS HTTP.status401 [] "{\"errCode\":\"unauthorized\"}")
            Warp.testWithApplication (pure app) $ \port -> do
                r <- try (acquireLease (sampleConfig ("http://127.0.0.1:" <> show port) keyFile) "qemu")
                case r of
                    Right _ -> assertFailure "acquired against a 401"
                    Left (e :: SomeException) -> do
                        let msg = displayException e
                        assertBool msg ("401" `isInfixOf` msg)
                        assertBool msg (not ("test-key" `isInfixOf` msg))
    , testCase "an unreachable buckette fails the lease, without the key in the error" $
        withTempDir $ \dir -> do
            let keyFile = dir </> "key"
            writeFile keyFile "test-key"
            -- a port nothing listens on: bind one with warp, then let it go
            port <- Warp.testWithApplication (pure (\_ respond -> respond (Wai.responseLBS HTTP.status200 [] ""))) pure
            r <- try (acquireLease (sampleConfig ("http://127.0.0.1:" <> show port) keyFile) "qemu")
            case r of
                Right _ -> assertFailure "acquired against nothing"
                Left (e :: SomeException) -> assertBool (displayException e) (not ("test-key" `isInfixOf` displayException e))
    , testCase "a refresh answered 400 (lease lost) stops refreshing" $
        withFake [successBody "r-1"] HTTP.status400 $ \cfg readSeen -> do
            lease <- acquireLease cfg "qemu"
            threadDelay 400000
            seen <- readSeen
            releaseLease cfg lease
            assertEqual "one refresh, then none" 1 (count "POST" "/buckets/bkt/reservations/r-1/refresh" seen)
    ]
