{-# LANGUAGE DeriveGeneric #-}

{- | Signed documents (the "Signed documents" section of @specs/pull-mode.md@):
"Salmon.Actions.Follow.Signature" at Layer 0 — the envelope, its
canonicalisation, and every refusal with its reason — and at Layer 1 the
verifier in a following loop over a directory registry with a cache: a
signed document is applied, a tampered one is refused and never cached, and
the cache written by the good one replays through the verifier, so that
swapping the host's key refuses it too.

Keys are generated here, in the test, and never touch the working tree.
-}
module Test.FollowSignatureSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Foldable (toList)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Follow as Follow
import Salmon.Actions.Follow (Document (..), Entry (..), Label)
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Follow.Signature as Signature
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Direction (..), Line (..), NodeState (..), Origin (..), Producer (..), World (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension, Op, Track', deps, op, ref)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (Reporter)

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Follow.Signature"
        [ testGroup
            "Layer 0: the envelope"
            [ testCase "sign then verify: the inner document comes back, canonical, and parses as the document signed" roundTrip
            , testCase "a document altered inside its envelope is refused, and the reason says so" tampered
            , testCase "a document signed by a key the host does not hold is refused, naming the unknown key" unknownKey
            , testCase "one of several keys suffices, whichever order they are given in" severalKeys
            , testCase "an unsigned document under a key is refused as unsigned" unsignedUnderKey
            , testCase "an envelope re-serialised with other key order and whitespace still verifies, to the same bytes" reserialised
            , testCase "no signatures, an envelope that does not parse, `none`, and a key file that is not one: each refused with its reason" refusals
            , testCase "the key id is the public key's SHA-256 thumbprint, the same from either half of the pair" keyIds
            ]
        , testCase "Layer 1: a signed document is applied through a directory registry; a tampered one is refused and never cached; the cache replays through the verifier, and a swapped key refuses it" followingSigned
        ]

-------------------------------------------------------------------------------
-- Layer 0

-- | A document naming these seeds, as bytes — a publisher's own spelling
-- (pretty-ish, keys in the order a human writes them), not aeson's.
document :: Text -> [[String]] -> ByteString
document did seeds =
    LByteString.fromStrict . Text.encodeUtf8 $
        "{ \"salmon\": 1,\n  \"id\": " <> quote did <> ",\n  \"seeds\": [" <> Text.intercalate ", " (fmap seed seeds) <> "],\n  \"note\": \"a publisher's annotation\" }\n"
  where
    seed ws = "{\"seed\": [" <> Text.intercalate ", " (fmap (quote . Text.pack) ws) <> "]}"
    quote t = Text.decodeUtf8 (LByteString.toStrict (encode (String t)))

sign :: Signature.PrivateKey -> ByteString -> IO ByteString
sign key bytes = do
    signed <- Signature.signDocument key bytes
    either (assertFailure . ("signing failed: " <>) . Text.unpack) pure signed

-- | Rewrite the @document@ member of an envelope, keeping everything else.
withDocument :: (Value -> Value) -> ByteString -> ByteString
withDocument f envelope = case eitherDecode envelope of
    Right (Object o) -> encode (Object (KeyMap.mapWithKey (\k v -> if k == "document" then f v else v) o))
    _ -> error "not an envelope"

-- | Give the document another id: a content change a signature must catch.
retitle :: Text -> Value -> Value
retitle did (Object o) = Object (KeyMap.insert "id" (String did) o)
retitle _ v = v

parsed :: ByteString -> Document
parsed bytes = either (error . ("document does not parse: " <>)) id (eitherDecode bytes)

expectLeft :: String -> Text -> Either Text a -> IO ()
expectLeft what needle verdict = case verdict of
    Right _ -> assertFailure (what <> ": accepted, expected a refusal mentioning " <> show needle)
    Left why -> assertBool (what <> ": the reason " <> show why <> " does not mention " <> show needle) (needle `Text.isInfixOf` why)

roundTrip :: IO ()
roundTrip = do
    key <- Signature.generateKeyPair
    let pub = Signature.publicKey key
        original = document "web@1" [["a"], ["b", "--flag"]]
    envelope <- sign key original
    -- the envelope is what a host fetches; it is not the document
    assertBool "the envelope is not the document" (envelope /= original)
    case Signature.verifyEnvelope [pub] envelope of
        Left why -> assertFailure ("refused: " <> Text.unpack why)
        Right inner -> do
            assertEqual "the inner document is the canonical form of what was signed" (encodeCanonical original) inner
            assertEqual "and parses as the document" (parsed original) (parsed inner)
            assertEqual "seeds intact" [SeedWords ["a"], SeedWords ["b", "--flag"]] (parsed inner).docSeeds
  where
    encodeCanonical bytes = case eitherDecode bytes :: Either String Value of
        Right v -> Signature.canonicalBytes v
        Left err -> error err

tampered :: IO ()
tampered = do
    key <- Signature.generateKeyPair
    envelope <- sign key (document "web@1" [["a"]])
    let evil = withDocument (retitle "web@evil") envelope
    -- the tampered envelope still parses as an envelope, so the refusal is
    -- the signature's, not the parser's
    expectLeft "tampered" "does not verify" (Signature.verifyEnvelope [Signature.publicKey key] evil)
    expectLeft "tampered" "altered after signing" (Signature.verifyEnvelope [Signature.publicKey key] evil)

unknownKey :: IO ()
unknownKey = do
    signer <- Signature.generateKeyPair
    other <- Signature.generateKeyPair
    envelope <- sign signer (document "web@1" [["a"]])
    let verdict = Signature.verifyEnvelope [Signature.publicKey other] envelope
    expectLeft "unknown key" "names no configured key" verdict
    expectLeft "unknown key" (Text.take 12 (Signature.keyId (Signature.publicKey signer))) verdict
    expectLeft "unknown key" "1 configured key" verdict

severalKeys :: IO ()
severalKeys = do
    k1 <- Signature.generateKeyPair
    k2 <- Signature.generateKeyPair
    k3 <- Signature.generateKeyPair
    envelope <- sign k2 (document "web@1" [["a"]])
    let pubs = fmap Signature.publicKey [k1, k2, k3]
    assertBool "k2 among three accepts" (either (const False) (const True) (Signature.verifyEnvelope pubs envelope))
    assertBool "in any order" (either (const False) (const True) (Signature.verifyEnvelope (reverse pubs) envelope))
    expectLeft "without k2" "2 configured key" (Signature.verifyEnvelope (fmap Signature.publicKey [k1, k3]) envelope)

unsignedUnderKey :: IO ()
unsignedUnderKey = do
    key <- Signature.generateKeyPair
    let verdict = Signature.verifyEnvelope [Signature.publicKey key] (document "web@1" [["a"]])
    expectLeft "unsigned" "unsigned document" verdict
    expectLeft "unsigned" "--follow-key" verdict

reserialised :: IO ()
reserialised = do
    key <- Signature.generateKeyPair
    let pub = Signature.publicKey key
    envelope <- sign key (document "web@1" [["a", "--n", "1"], ["b"]])
    let other = rerender envelope
    assertBool "the rendering differs" (other /= envelope)
    assertBool "and is still JSON with the same content" (eitherDecode other == (eitherDecode envelope :: Either String Value))
    case (Signature.verifyEnvelope [pub] envelope, Signature.verifyEnvelope [pub] other) of
        (Right a, Right b) -> assertEqual "both verify to the same inner bytes" a b
        (a, b) -> assertFailure ("expected both to verify: " <> show (a, b))

{- | The same JSON value with every object's keys in /descending/ order,
spaces everywhere aeson puts none, and a trailing newline: what a
pretty-printer, a proxy or a registry written in another language might
turn an envelope into. -}
rerender :: ByteString -> ByteString
rerender bytes = case eitherDecode bytes of
    Left err -> error err
    Right v -> LByteString.fromStrict (Text.encodeUtf8 (go v)) <> "\n"
  where
    go :: Value -> Text
    go (Object o) =
        "{ " <> Text.intercalate " , " [quoteKey k <> " : " <> go x | (k, x) <- sortOn (Down . fst) (KeyMap.toList o)] <> " }"
    go (Array xs) = "[ " <> Text.intercalate " , " (fmap go (toList xs)) <> " ]"
    go scalar = Text.decodeUtf8 (LByteString.toStrict (encode scalar))
    quoteKey k = go (String (Key.toText k))

refusals :: IO ()
refusals = withTempDir $ \dir -> do
    key <- Signature.generateKeyPair
    let pub = Signature.publicKey key
        envelopeWith :: Text -> ByteString
        envelopeWith sigs = LByteString.fromStrict (Text.encodeUtf8 ("{\"salmon-signed\": 1, \"document\": {}, \"signatures\": " <> sigs <> "}"))
    expectLeft "no signatures" "carries no signatures" (Signature.verifyEnvelope [pub] (envelopeWith "[]"))
    expectLeft "signatures not a list" "does not parse" (Signature.verifyEnvelope [pub] (envelopeWith "\"nope\""))
    expectLeft "a signature without its key" "does not parse" (Signature.verifyEnvelope [pub] (envelopeWith "[{\"alg\": \"EdDSA\", \"sig\": \"\"}]"))
    expectLeft "not JSON" "not even JSON" (Signature.verifyEnvelope [pub] "{{{")
    expectLeft "another version" "does not parse" (Signature.verifyEnvelope [pub] "{\"salmon-signed\": 2, \"document\": {}, \"signatures\": []}")
    -- `none` with an empty signature is what jose's own `verify` would
    -- accept; the verifier must not hand it that
    let none = envelopeWith ("[{\"key\": \"" <> Signature.keyId pub <> "\", \"alg\": \"none\", \"sig\": \"\"}]")
    expectLeft "alg none" "no public key can verify" (Signature.verifyEnvelope [pub] none)
    -- an HMAC named by the key id: same refusal, a public key has no secret
    let hmac = envelopeWith ("[{\"key\": \"" <> Signature.keyId pub <> "\", \"alg\": \"HS256\", \"sig\": \"AAAA\"}]")
    expectLeft "alg HS256" "no public key can verify" (Signature.verifyEnvelope [pub] hmac)
    -- no key at all refuses rather than accepts
    envelope <- sign key (document "web@1" [["a"]])
    expectLeft "no keys" "no signing key" (Signature.verifyEnvelope [] envelope)
    -- key files
    LByteString.writeFile (dir </> "garbage") "not a key\n"
    badFile <- Signature.readPublicKeyFile (dir </> "garbage")
    expectLeft "not a JWK" "not a JWK" (() <$ badFile)
    missing <- Signature.readPublicKeyFile (dir </> "absent")
    expectLeft "missing file" "absent" (() <$ missing)
    Signature.writeKeyPair (dir </> "k") key
    onlyPublic <- Signature.readPrivateKeyFile (dir </> "k.pub")
    expectLeft "the public half cannot sign" "no private material" (() <$ onlyPublic)

keyIds :: IO ()
keyIds = withTempDir $ \dir -> do
    key <- Signature.generateKeyPair
    Signature.writeKeyPair (dir </> "k") key
    fromPrivate <- Signature.readPublicKeyFile (dir </> "k")
    fromPublic <- Signature.readPublicKeyFile (dir </> "k.pub")
    case (fromPrivate, fromPublic) of
        (Right a, Right b) -> do
            assertEqual "the same public key from either file" a b
            assertEqual "the same id" (Signature.keyId a) (Signature.keyId (Signature.publicKey key))
            assertEqual "64 hex characters" 64 (Text.length (Signature.keyId a))
            assertBool "hex" (Text.all (`elem` ("0123456789abcdef" :: String)) (Signature.keyId a))
        other -> assertFailure ("could not read the pair back: " <> show other)
    another <- Signature.generateKeyPair
    assertBool "two keys, two ids" (Signature.keyId (Signature.publicKey another) /= Signature.keyId (Signature.publicKey key))

-------------------------------------------------------------------------------
-- Layer 1: the served thing and the loop, same harness as Test.FollowRegistrySpec

data Spec = Spec
    { specDir :: FilePath
    , specNames :: [String]
    }
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: FilePath -> [String] -> Either Text Spec
parseSpec root args
    | null args = Left "expected at least one file name"
    | otherwise = Right (Spec (root </> "files") args)

program :: Track' Spec
program = Track $ \spec ->
    op "follow-signature-root" (deps (fmap (fileOp spec.specDir) spec.specNames)) $ \actions ->
        actions{ref = mkRef "follow-signature-root" (spec.specDir, spec.specNames)}

fileOp :: FilePath -> String -> Op
fileOp d n = FS.filecontents (FS.FileContents (d </> n) ("contents of " <> n))

data Driver = Driver
    { typeLine :: String -> IO ()
    , serveReports :: IO [Serve.Report]
    , followReports :: IO [Follow.Report]
    }

interval :: Int
interval = 100000

schedule :: Scheduler.Config
schedule =
    Scheduler.Config
        { Scheduler.schedBase = interval
        , Scheduler.schedFactor = 2
        , Scheduler.schedCap = 4 * interval
        , Scheduler.schedJitter = 0
        , Scheduler.schedDebounce = 0
        , Scheduler.schedMaxWait = 0
        }

withFollowing :: FilePath -> FilePath -> Follow.Verifier -> [Label] -> (Driver -> IO a) -> IO (World Spec Spec, [Serve.Report], [Follow.Report], a)
withFollowing root reg verifier labels body = do
    (serveReporter, readServe) <- capture
    (followReporter, readFollow) <- capture
    (nodeReporter, _) <- capture :: IO (Reporter (UpDown.Report Extension), IO [UpDown.Report Extension])
    stdinChan <- newTChanIO
    gate <- newEmptyMVar
    pk <- Scheduler.newPoke
    modeVar <- Follow.newMode
    appliedVar <- Follow.newApplied
    let follow =
            Follow.Follow
                { Follow.followRegistry = Follow.directoryRegistry reg
                , Follow.followLabels = labels
                , Follow.followSchedule = schedule
                , Follow.followCache = Just (root </> "cache")
                , Follow.followRefuseOlder = False
                , Follow.followVerify = verifier
                }
        producers =
            [ Follow.follower followReporter pk modeVar appliedVar follow (putMVar gate ())
            , Follow.gated gate (chanProducer stdinChan)
            ]
        driver =
            Driver
                { typeLine = \l -> atomically (writeTChan stdinChan (Just l))
                , serveReports = readServe
                , followReports = readFollow
                }
    resultVar <- newTChanIO
    _ <- forkIO $ do
        outcome <- try (body driver)
        atomically (writeTChan stdinChan Nothing)
        atomically (writeTChan resultVar outcome)
    w <- Serve.serveFollowing [] Nothing True serveReporter nodeReporter (parseSpec root) (Configure pure) program (Just (Follow.followed pk modeVar appliedVar)) producers
    outcome <- atomically (readTChan resultVar)
    case outcome of
        Left (ex :: SomeException) -> throwIO ex
        Right a -> (,,,) w <$> readServe <*> readFollow <*> pure a

chanProducer :: TChan (Maybe String) -> Producer
chanProducer ch = Producer go
  where
    go inbox = do
        next <- atomically (readTChan ch)
        case next of
            Nothing -> atomically (writeTChan inbox (Eof Stdin))
            Just l -> atomically (writeTChan inbox (Line Stdin l)) >> go inbox

label :: Text -> Label
label t = either (error . Text.unpack) id (Follow.mkLabel t)

waitFor :: String -> IO Bool -> IO ()
waitFor what cond = do
    ok <- timeout (10 * 1000000) go
    case ok of
        Just () -> pure ()
        Nothing -> assertFailure ("timed out waiting for " <> what)
  where
    go = do
        done <- cond
        if done then pure () else threadDelay 20000 >> go

fileExists :: FilePath -> String -> IO Bool
fileExists root n = doesFileExist (root </> "files" </> n)

injections :: [Follow.Report] -> [(Int, Int)]
injections reports = [(nup, ndown) | Follow.Injected _ _ _ nup ndown <- reports]

rejections :: [Follow.Report] -> [(Follow.Digest, Text)]
rejections reports = [(dg, why) | Follow.Rejected _ dg why <- reports]

allConvergedUp :: World seed directive -> Bool
allConvergedUp w = not (Map.null w.worldNodes) && all (\st -> st.nodeDirection == TurnUp && st.nodeConvergence == Converged) (Map.elems w.worldNodes)

followingSigned :: IO ()
followingSigned =
    withTempDir $ \root -> do
        key <- Signature.generateKeyPair
        other <- Signature.generateKeyPair
        let web = label "web"
            reg = root </> "reg"
            cache = root </> "cache"
            verifier = Signature.signedVerifier [Signature.publicKey key]
            publish bytes = createDirectoryIfMissing True reg >> LByteString.writeFile (Follow.documentPath reg web) bytes
        good <- sign key (document "web@1" [["a"]])
        let evil = withDocument (retitle "web@evil") good
        publish good
        (w, _, freports, good2) <- withFollowing root reg verifier [web] $ \d -> do
            waitFor "the file" (fileExists root "a")
            -- the tampered envelope: refused, never applied, never cached
            publish evil
            waitFor "the refusal" (not . null . rejections <$> d.followReports)
            threadDelay (2 * interval)
            declared <- (\rs -> [() | Serve.Declared{} <- rs]) <$> d.serveReports
            assertEqual "one declaration, the signed document's" 1 (length declared)
            cached <- Follow.readCacheEntry cache web
            assertEqual "the cache still holds the signed document, envelope and all" (Right (Just ("web@1", Follow.digestOf good, good))) (fmap (fmap (\c -> (c.cachedId, c.cachedDigest, c.cachedBytes))) cached)
            -- and a good document again is applied on top
            good2 <- sign key (document "web@2" [["a"], ["b"]])
            publish good2
            waitFor "the next file" (fileExists root "b")
            pure good2
        case rejections freports of
            [(dg, why)] -> do
                assertBool ("the one refusal is the signature's: " <> Text.unpack why) ("does not verify" `Text.isInfixOf` why)
                assertEqual "and it names the envelope's digest, the bytes as fetched" (Follow.digestOf evil) dg
            rs -> assertFailure ("expected exactly one refusal, got " <> show rs)
        assertEqual "two injections" [(1, 0), (1, 0)] (injections freports)
        assertBool "the world converged" (allConvergedUp w)
        -- what the fetcher records is the document's id, from inside the
        -- envelope
        assertEqual "the injections name the documents' ids" ["web@1", "web@2"] [did | Follow.Injected _ did _ _ _ <- freports]
        -- the registry goes away: the cache replays through the same key
        renameDirectory reg (reg <> ".away")
        renameDirectory (root </> "files") (root </> "files.away")
        (w2, _, freports2, ()) <- withFollowing root reg verifier [web] $ \_ ->
            waitFor "the files, rebuilt from the cache" ((&&) <$> fileExists root "a" <*> fileExists root "b")
        assertEqual "replayed, once" ["web@2"] [did | Follow.Replayed _ did _ <- freports2]
        assertEqual "nothing refused" [] (rejections freports2)
        assertBool "the replayed world converged" (allConvergedUp w2)
        -- the host's key is swapped: the same cache entry is refused on replay
        renameDirectory (root </> "files") (root </> "files.away2")
        (_, sreports3, freports3, ()) <- withFollowing root reg (Signature.signedVerifier [Signature.publicKey other]) [web] $ \d -> do
            waitFor "the refusal" (not . null . rejections <$> d.followReports)
            threadDelay (2 * interval)
        assertEqual "nothing replayed" [] [() | Follow.Replayed{} <- freports3]
        assertEqual "nothing declared" [] [() | Serve.Declared{} <- sreports3]
        present <- fileExists root "a"
        assertBool "nothing rebuilt" (not present)
        case rejections freports3 of
            [(dg, why)] -> do
                assertBool ("the refusal names the unknown key: " <> Text.unpack why) ("names no configured key" `Text.isInfixOf` why)
                assertEqual "the refusal names the cache entry's digest, the last good envelope's" (Follow.digestOf good2) dg
            rs -> assertFailure ("expected exactly one refusal, got " <> show rs)
