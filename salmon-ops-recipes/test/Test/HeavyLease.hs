{- | A machine-wide lease around the heavy test tiers, held as a one-slot
reservation on a buckette bucket, so that heavy runs from several projects on
one shared machine queue for one another instead of overlapping.

Off unless 'gateVar' names a bucket: with it unset the suite's tree is not
touched at all ('leaseConfigFrom' answers 'Nothing' and 'withHeavyLease' is
the identity), which is the default.

With it set:

* __Acquire__: @POST {base}/buckets/{bucket}/reservations@ with
  @rrvAmounts: {"slots": 1}@, @rrvExpiresAt: now + ttl@, a @Wait@ mode
  (buckette polls for up to 'waitSecs', capped at 300 by buckette) and a
  @rrvTransactionId@. A @ReserveShortfall@ answer (the wait ran out) is asked
  again, with the ETA printed between attempts; anything else that is not a
  @ReserveSuccess@ (bad key, frozen bucket, unreachable server) fails the
  lease, and with it every test it guards, loudly.
* __Hold__: a thread refreshes it (@POST .../reservations/{rid}/refresh@,
  @rfrExpiresAt: now + ttl@) every 'refreshEvery'.
* __Release__: @DELETE .../reservations/{rid}@ (a cancel, never a commit:
  a commit would spend the slot), when the guarded tree finishes, whether it
  passed, failed or was interrupted. A harness that dies is freed by expiry.

The routes, the @Wait@ shape and the reservation's JSON were checked against
buckette's own @/docs/llm.md@, @/help/reservations@ and @/openapi.json@ on
2026-11-03 (all three readable without a key). Nothing here has been run
against a live bucket.

The key is read from 'keyFileVar''s file at acquire time, sent only as the
@Authorization@ header, and never printed: no message here includes a request.
-}
module Test.HeavyLease (
    -- * configuration
    LeaseConfig (..),
    gateVar,
    baseUrlVar,
    keyFileVar,
    leaseConfigFrom,
    leaseConfigFromEnv,

    -- * the lease
    Lease (..),
    withHeavyLease,
    acquireLease,
    releaseLease,

    -- * wire format (exposed for tests)
    ReserveAnswer (..),
    reserveBody,
    refreshBody,
    parseReserveAnswer,
    transactionId,
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, displayException, finally, throwIO, try)
import Control.Monad (forever)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isSpace)
import qualified Data.List as List
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTls
import qualified Network.HTTP.Types as HttpTypes
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import System.Posix.Unistd (getSystemID, nodeName)
import Test.Tasty (TestTree, withResource)

-- | Names the bucket to hold a slot of. Unset or empty: no lease, the
-- harness behaves as it did before the lease existed.
gateVar :: String
gateVar = "BUCKETTE_HEAVY_BUCKET"

-- | buckette's base URL, e.g. @https://host/prefix@ (no trailing slash
-- needed). Required once 'gateVar' is set; there is no default instance.
baseUrlVar :: String
baseUrlVar = "BUCKETTE_BASE_URL"

-- | A file holding a key scoped to that one bucket (reserve, refresh and
-- release). Required once 'gateVar' is set. Provisioned out of band.
keyFileVar :: String
keyFileVar = "BUCKETTE_HEAVY_KEY_FILE"

data LeaseConfig = LeaseConfig
    { lc_baseUrl :: String
    , lc_bucket :: Text
    , lc_keyFile :: FilePath
    , lc_dimension :: Text
    -- ^ @slots@, one unit of which is one heavy run
    , lc_ttl :: NominalDiffTime
    -- ^ how far ahead each acquire and refresh sets the expiry
    , lc_refreshEvery :: Int
    -- ^ microseconds between refreshes; well under 'lc_ttl'
    , lc_waitSecs :: Int
    -- ^ how long one @Wait@ reserve may block before asking again
    }

{- | The configuration from an environment lookup. @Right Nothing@ when the
gate is unset or empty; @Left@ when it is set but the base URL or key file is
not, since a run that asked for the lease should not silently go without it.
-}
leaseConfigFrom :: (String -> Maybe String) -> Either String (Maybe LeaseConfig)
leaseConfigFrom env =
    case nonEmpty gateVar of
        Nothing -> Right Nothing
        Just bucket -> do
            base <- required baseUrlVar
            key <- required keyFileVar
            pure $
                Just
                    LeaseConfig
                        { lc_baseUrl = dropWhileEnd' (== '/') base
                        , lc_bucket = Text.pack bucket
                        , lc_keyFile = key
                        , lc_dimension = "slots"
                        , lc_ttl = 10 * 60
                        , lc_refreshEvery = 3 * 60 * 1000000
                        , lc_waitSecs = 60
                        }
  where
    nonEmpty k = case trim <$> env k of
        Just v | not (null v) -> Just v
        _ -> Nothing
    required k = maybe (Left (gateVar <> " is set but " <> k <> " is not")) Right (nonEmpty k)
    trim = dropWhileEnd' isSpace . dropWhile isSpace
    dropWhileEnd' p = reverse . dropWhile p . reverse

leaseConfigFromEnv :: IO (Maybe LeaseConfig)
leaseConfigFromEnv = do
    let names = [gateVar, baseUrlVar, keyFileVar]
    vals <- traverse lookupEnv names
    let env k = lookup k (zip names vals) >>= id
    either (ioError . userError) pure (leaseConfigFrom env)

{- | Guard a test tree with the lease, or leave it exactly as it is when
there is no configuration. The lease is taken before the tree's first test
runs and released after its last one (tasty's 'withResource'), so a tree
that @-p@ filters out entirely takes nothing.
-}
withHeavyLease :: Maybe LeaseConfig -> String -> TestTree -> TestTree
withHeavyLease Nothing _ t = t
withHeavyLease (Just cfg) tier t =
    withResource (acquireLease cfg tier) (releaseLease cfg) (const t)

data Lease = Lease
    { lease_id :: Text
    , lease_stopRefreshing :: IO ()
    }

-------------------------------------------------------------------------------
-- wire format

data ReserveAnswer
    = Reserved Text
    | -- | the @Wait@ ran out; the per-dimension shortfall
      Shortfall Value
    | Pending
    deriving (Eq, Show)

reserveBody :: LeaseConfig -> UTCTime -> Text -> Value
reserveBody cfg expiry txid =
    object
        [ "rrvAmounts" .= object [Key.fromText cfg.lc_dimension .= (1 :: Int)]
        , "rrvMode" .= object ["tag" .= ("Wait" :: Text), "contents" .= object ["wcMaxDelaySecs" .= cfg.lc_waitSecs]]
        , "rrvExpiresAt" .= expiry
        , "rrvTransactionId" .= txid
        ]

refreshBody :: UTCTime -> Value
refreshBody expiry = object ["rfrExpiresAt" .= expiry]

-- | A @ReserveOutcome@ (tagged object), per buckette's @/openapi.json@.
parseReserveAnswer :: LByteString.ByteString -> Either String ReserveAnswer
parseReserveAnswer body = do
    v <- Aeson.eitherDecode body
    case v of
        Object o -> case KeyMap.lookup "tag" o of
            Just (String "ReserveSuccess") -> case KeyMap.lookup "rsReservation" o of
                Just (Object r) | Just (String rid) <- KeyMap.lookup "rvId" r -> Right (Reserved rid)
                _ -> Left "ReserveSuccess without rsReservation.rvId"
            Just (String "ReserveShortfall") -> Right (Shortfall (maybe Null id (KeyMap.lookup "roShortfall" o)))
            Just (String "ReservePending") -> Right Pending
            other -> Left ("unexpected reserve outcome tag: " <> show other)
        _ -> Left "reserve outcome is not an object"

{- | @host:pid:tier:start:attempt@. buckette refuses a transaction id it has
seen in the last seven days (and holds nothing), so the agreed
@host:pid:tier@ alone would collide with a recycled pid, and possibly with a
second attempt after a @Wait@ ran out; the start time and attempt number keep
each request's id its own.
-}
transactionId :: String -> Int -> String -> Integer -> Int -> Text
transactionId host pid tier start attempt =
    Text.pack (List.intercalate ":" [host, show pid, tier, show start, show attempt])

-------------------------------------------------------------------------------
-- the lease

say :: String -> IO ()
say = hPutStrLn stderr . ("heavy-ops lease: " <>)

acquireLease :: LeaseConfig -> String -> IO Lease
acquireLease cfg tier = do
    key <- readKey cfg
    mgr <- HttpTls.newTlsManager
    host <- nodeName <$> getSystemID
    pid <- fromIntegral <$> getProcessID
    start <- floor <$> getPOSIXTime
    let go :: Int -> IO Text
        go attempt = do
            expiry <- addUTCTime cfg.lc_ttl <$> getCurrentTime
            let txid = transactionId host pid tier start attempt
            (status, body) <- call cfg key mgr "POST" "/reservations" (Just (reserveBody cfg expiry txid))
            if not (HttpTypes.statusIsSuccessful status)
                then failWith ("reserve answered HTTP " <> show (HttpTypes.statusCode status) <> ": " <> lazyString (LByteString.take 400 body))
                else case parseReserveAnswer body of
                    Left e -> failWith ("reserve: " <> e)
                    Right (Reserved rid) -> do
                        say ("holding " <> Text.unpack rid <> " on " <> Text.unpack cfg.lc_bucket <> " for " <> tier)
                        pure rid
                    Right Pending -> failWith "reserve answered ReservePending, which a Wait reserve should not"
                    Right (Shortfall short) -> do
                        eta <- askEta cfg key mgr
                        say ("no free slot after " <> show (cfg.lc_waitSecs * attempt) <> "s (short " <> lazyString (encode short) <> "); " <> eta <> "; waiting again")
                        go (attempt + 1)
    say ("asking for a slot on " <> Text.unpack cfg.lc_bucket <> " for " <> tier)
    rid <- go 1
    lost <- newIORef False
    refresher <- forkIO . forever $ do
        threadDelay cfg.lc_refreshEvery
        gone <- readIORef lost
        if gone
            then pure ()
            else do
                expiry <- addUTCTime cfg.lc_ttl <$> getCurrentTime
                r <- try (call cfg key mgr "POST" ("/reservations/" <> Text.unpack rid <> "/refresh") (Just (refreshBody expiry)))
                case r of
                    Left (e :: SomeException) -> say ("refresh failed, will try again: " <> displayException e)
                    Right (status, _)
                        | HttpTypes.statusIsSuccessful status -> pure ()
                        | HttpTypes.statusCode status `elem` [400, 404] -> do
                            -- not active any more (expired, cancelled) or gone:
                            -- another run may hold the slot now
                            atomicWriteIORef lost True
                            say ("LOST " <> Text.unpack rid <> ": refresh answered HTTP " <> show (HttpTypes.statusCode status) <> "; the rest of this tier runs unguarded")
                        | otherwise -> say ("refresh answered HTTP " <> show (HttpTypes.statusCode status) <> ", will try again")
    pure Lease{lease_id = rid, lease_stopRefreshing = killThread refresher}
  where
    failWith msg = say msg >> throwIO (userError ("heavy-ops lease: " <> msg))

lazyString :: LByteString.ByteString -> String
lazyString = Char8.unpack . LByteString.toStrict

releaseLease :: LeaseConfig -> Lease -> IO ()
releaseLease cfg lease =
    lease.lease_stopRefreshing `finally` remove
  where
    remove = do
        r <- try $ do
            key <- readKey cfg
            mgr <- HttpTls.newTlsManager
            call cfg key mgr "DELETE" ("/reservations/" <> Text.unpack lease.lease_id) Nothing
        case r of
            Left (e :: SomeException) -> say ("release of " <> Text.unpack lease.lease_id <> " failed (" <> displayException e <> "); it expires on its own")
            Right (status, _)
                | HttpTypes.statusIsSuccessful status -> say ("released " <> Text.unpack lease.lease_id)
                | otherwise -> say ("release of " <> Text.unpack lease.lease_id <> " answered HTTP " <> show (HttpTypes.statusCode status) <> "; it expires on its own")

-- | Best effort: a key scoped to reserve\/refresh\/release may not be allowed
-- to read the ETA, and with no replenish schedule the answer is most likely
-- "held by reservations" anyway.
askEta :: LeaseConfig -> Char8.ByteString -> Http.Manager -> IO String
askEta cfg key mgr = do
    r <- try (call cfg key mgr "POST" "/eta" (Just (object ["etqAmounts" .= object [Key.fromText cfg.lc_dimension .= (1 :: Int)]])))
    pure $ case r of
        Left (e :: SomeException) -> "eta unavailable (" <> takeWhile (/= '\n') (displayException e) <> ")"
        Right (status, body)
            | HttpTypes.statusIsSuccessful status -> "eta " <> lazyString (LByteString.take 300 body)
            | otherwise -> "eta unavailable (HTTP " <> show (HttpTypes.statusCode status) <> ")"

readKey :: LeaseConfig -> IO Char8.ByteString
readKey cfg = do
    raw <- Char8.readFile cfg.lc_keyFile
    let k = Char8.filter (not . isSpace) raw
    if Char8.null k
        then throwIO (userError ("heavy-ops lease: " <> keyFileVar <> " names an empty file"))
        else pure k

{- | One call under @/buckets/{bucket}@. Never throws on an HTTP status (the
caller reads it); a connection failure is rethrown without the request in its
text, since the request carries the key.
-}
call ::
    LeaseConfig ->
    Char8.ByteString ->
    Http.Manager ->
    HttpTypes.Method ->
    String ->
    Maybe Value ->
    IO (HttpTypes.Status, LByteString.ByteString)
call cfg key mgr method path body = do
    let url = cfg.lc_baseUrl <> "/buckets/" <> Text.unpack cfg.lc_bucket <> path
    req0 <- Http.parseRequest url
    let req =
            req0
                { Http.method = method
                , Http.requestHeaders =
                    [ ("Authorization", "Bearer " <> key)
                    , ("Content-Type", "application/json")
                    , ("Accept", "application/json")
                    ]
                , Http.requestBody = Http.RequestBodyLBS (maybe "" encode body)
                , Http.checkResponse = \_ _ -> pure ()
                , -- a Wait reserve holds the connection for up to its delay
                  Http.responseTimeout = Http.responseTimeoutMicro ((cfg.lc_waitSecs + 60) * 1000000)
                }
    r <- try (Http.httpLbs req mgr)
    case r of
        Right resp -> pure (Http.responseStatus resp, Http.responseBody resp)
        Left (Http.HttpExceptionRequest _ content) -> throwIO (userError ("buckette " <> Char8.unpack method <> " " <> path <> ": " <> show content))
        Left (Http.InvalidUrlException u why) -> throwIO (userError ("invalid buckette URL " <> u <> ": " <> why))
