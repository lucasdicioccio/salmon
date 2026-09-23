{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for milestone 6 of @specs/pull-mode.md@: the registries
beyond the directory, and the verify-before-inject hook.

Same harness as "Test.FollowSpec" — a @run serve@ over a temp dir, standard
input driven from a channel — pointed at local fixtures: a bare git
repository committed to from a second clone, a @warp@ server answering with
@ETag@s (and, on request, @500@), a stubbed 'Dns.Resolver' over that server,
and the bucket backend as the URL template it is. Every backend is asserted
on the same three things: the world after a document, /nothing/ injected
when the registry says unchanged (a commit that does not touch the file, a
@304@, a record whose digest did not move), and what a failure climbs.
-}
module Test.FollowRegistrySpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (createDirectoryIfMissing, doesFileExist, renameDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readCreateProcessWithExitCode, proc)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Follow as Follow
import Salmon.Actions.Follow (Digest (..), Document (..), Entry (..), Label, Registry (..), Stamp (..))
import qualified Salmon.Actions.Follow.Registry as Registry
import qualified Salmon.Actions.Follow.Registry.Dns as Dns
import qualified Salmon.Actions.Follow.Registry.Git as Git
import qualified Salmon.Actions.Follow.Registry.Http as Http
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
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
        "Salmon.Actions.Follow.Registry"
        [ testGroup
            "addresses"
            [ testCase "--follow's shape picks the backend" addressShapes
            , testCase "git+URL#BRANCH:SUBDIR, and the URL's own colons left alone" gitSources
            , testCase "the HTTP template: {label} placed, else /<label>.json appended" httpTemplate
            , testCase "the bucket templates: virtual-hosted S3, path-style under an endpoint, GCS" bucketTemplates
            , testCase "dig +short's output: quoted strings joined, comments skipped" digOutput
            , testCase "the index record: v=salmon1 url= sha256=, and what is refused" indexRecords
            ]
        , testCase "git: a commit is applied; a commit that leaves the file alone injects nothing; a second commit is diffed; the repository gone is a failed round" gitRegistry
        , testCase "http: a document is applied; 304 injects nothing and moves no bytes; 404 is absent; 500 climbs the ladder" httpRegistry
        , testCase "dns: the record's digest is the stamp; the store disagreeing with the index is refused; no record is absent" dnsRegistry
        , testCase "bucket: an s3:// address under an endpoint is the HTTP backend at BUCKET/PREFIX/<label>.json" bucketRegistry
        , testCase "verify: a refused document is a failed round, reaches neither the loop nor the cache; a refused cache entry is not replayed" verifyHook
        ]

-------------------------------------------------------------------------------
-- the served thing: "make these files exist", same as Test.FollowSpec

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
    op "follow-registry-root" (deps (fmap (fileOp spec.specDir) spec.specNames)) $ \actions ->
        actions{ref = mkRef "follow-registry-root" (spec.specDir, spec.specNames)}

fileOp :: FilePath -> String -> Op
fileOp d n = FS.filecontents (FS.FileContents (d </> n) ("contents of " <> n))

-------------------------------------------------------------------------------
-- driving the loop

data Driver = Driver
    { typeLine :: String -> IO ()
    , serveReports :: IO [Serve.Report]
    , followReports :: IO [Follow.Report]
    }

interval :: Int
interval = 100000

-- | Rounds at 'interval', no jitter, no window, a short cap.
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

data Knobs = Knobs
    { knobCache :: Maybe FilePath
    , knobVerify :: Follow.Verifier
    }

plain :: Knobs
plain = Knobs Nothing Follow.noVerifier

{- | One session of a following loop over @root@ against the given registry;
the files land in @root/files@. Ends when the body returns (or throws). -}
withFollowing :: FilePath -> Registry -> Knobs -> [Label] -> (Driver -> IO a) -> IO (World Spec Spec, [Serve.Report], [Follow.Report], a)
withFollowing root registry knobs labels body = do
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
                { Follow.followRegistry = registry
                , Follow.followLabels = labels
                , Follow.followSchedule = schedule
                , Follow.followCache = knobs.knobCache
                , Follow.followRefuseOlder = False
                , Follow.followVerify = knobs.knobVerify
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

-- | A document naming these seeds, as bytes.
document :: Text -> [[String]] -> ByteString
document did seeds = encode (Document did (fmap SeedWords seeds) Nothing)

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

fetchFailures :: [Follow.Report] -> [Text]
fetchFailures reports = [err | Follow.FetchFailed _ err <- reports]

backoffs :: [Follow.Report] -> [Int]
backoffs reports = [n | Follow.Backoff n _ <- reports]

allConvergedUp :: World seed directive -> Bool
allConvergedUp w = not (Map.null w.worldNodes) && all (\st -> st.nodeDirection == TurnUp && st.nodeConvergence == Converged) (Map.elems w.worldNodes)

-------------------------------------------------------------------------------
-- the pure half

addressShapes :: IO ()
addressShapes = do
    assertEqual "a path" (Right (Registry.Directory "/srv/reg")) (Registry.parseAddress "/srv/reg")
    assertEqual "a relative path" (Right (Registry.Directory "reg")) (Registry.parseAddress "reg")
    assertEqual "http" (Right (Registry.Http "http://h/p")) (Registry.parseAddress "http://h/p")
    assertEqual "https" (Right (Registry.Http "https://h/seed/latest/{label}")) (Registry.parseAddress "https://h/seed/latest/{label}")
    assertEqual "dns" (Right (Registry.Dns "fleet.example")) (Registry.parseAddress "dns:fleet.example")
    assertBool "dns without a zone" (either (const True) (const False) (Registry.parseAddress "dns:"))
    assertEqual "s3" (Right (Registry.InBucket (Registry.Bucket Registry.S3 "b" "p/q"))) (Registry.parseAddress "s3://b/p/q/")
    assertEqual "gs, no prefix" (Right (Registry.InBucket (Registry.Bucket Registry.Gcs "b" ""))) (Registry.parseAddress "gs://b")
    assertBool "a bucket without a name" (either (const True) (const False) (Registry.parseAddress "s3://"))
    assertEqual "git" (Right (Registry.Git (Git.Source "https://h/r.git" Nothing Nothing))) (Registry.parseAddress "git+https://h/r.git")

gitSources :: IO ()
gitSources = do
    let src = Git.Source
    assertEqual "url only" (Right (src "ssh://git@h:22/r" Nothing Nothing)) (Git.parseSource "ssh://git@h:22/r")
    assertEqual "branch" (Right (src "git@h:r.git" (Just "main") Nothing)) (Git.parseSource "git@h:r.git#main")
    assertEqual "branch and subdir" (Right (src "https://h/r" (Just "main") (Just "hosts/eu"))) (Git.parseSource "https://h/r#main:hosts/eu")
    assertEqual "default branch, subdir" (Right (src "https://h/r" Nothing (Just "hosts"))) (Git.parseSource "https://h/r#:hosts")
    assertBool "no url" (either (const True) (const False) (Git.parseSource "#main"))
    assertEqual "rendered back" "git+https://h/r#main:hosts/eu" (Git.renderSource (src "https://h/r" (Just "main") (Just "hosts/eu")))
    assertEqual "rendered back, plain" "git+https://h/r" (Git.renderSource (src "https://h/r" Nothing Nothing))
    assertEqual "the document's path" ("/w/hosts" </> "web.json") (Git.documentPathIn "/w" (src "u" Nothing (Just "hosts")) (label "web"))
    assertEqual "the document's path, no subdir" ("/w" </> "web.json") (Git.documentPathIn "/w" (src "u" Nothing Nothing) (label "web"))

httpTemplate :: IO ()
httpTemplate = do
    assertEqual "appended" "https://h/reg/web.json" (Http.addressFor "https://h/reg" (label "web"))
    assertEqual "trailing slash not doubled" "https://h/reg/web.json" (Http.addressFor "https://h/reg/" (label "web"))
    assertEqual "placed" "https://h/seed/latest/web" (Http.addressFor "https://h/seed/latest/{label}" (label "web"))
    assertEqual "placed twice" "https://web.h/web" (Http.addressFor "https://{label}.h/{label}" (label "web"))

bucketTemplates :: IO ()
bucketTemplates = do
    assertEqual "s3" "https://b.s3.amazonaws.com/p" (Registry.bucketTemplate Nothing (Registry.Bucket Registry.S3 "b" "p"))
    assertEqual "s3, no prefix" "https://b.s3.amazonaws.com" (Registry.bucketTemplate Nothing (Registry.Bucket Registry.S3 "b" ""))
    assertEqual "s3 under an endpoint" "https://minio.local:9000/b/p" (Registry.bucketTemplate (Just "https://minio.local:9000/") (Registry.Bucket Registry.S3 "b" "p"))
    assertEqual "gcs" "https://storage.googleapis.com/b/p" (Registry.bucketTemplate Nothing (Registry.Bucket Registry.Gcs "b" "p"))
    assertEqual "and then the label" "https://b.s3.amazonaws.com/p/web.json" (Http.addressFor (Registry.bucketTemplate Nothing (Registry.Bucket Registry.S3 "b" "p")) (label "web"))

digOutput :: IO ()
digOutput = do
    assertEqual "one string" ["v=salmon1 url=https://h/d sha256=ab"] (Dns.parseDigTxt "\"v=salmon1 url=https://h/d sha256=ab\"\n")
    assertEqual "two records" ["a", "b"] (Dns.parseDigTxt "\"a\"\n\"b\"\n")
    assertEqual "strings joined" ["abcdef"] (Dns.parseDigTxt "\"abc\" \"def\"\n")
    assertEqual "escapes" ["say \"hi\" \\ there"] (Dns.parseDigTxt "\"say \\\"hi\\\" \\\\ there\"\n")
    assertEqual "comments and blanks skipped" [] (Dns.parseDigTxt ";; communications error to 127.0.0.1#53: timed out\n\n")
    assertEqual "nothing" [] (Dns.parseDigTxt "")

indexRecords :: IO ()
indexRecords = do
    let hex = Text.replicate 64 "a"
    assertEqual "parsed" (Right (Dns.IndexRecord "https://h/d.json" (Digest hex))) (Dns.parseIndexRecord ("v=salmon1 url=https://h/d.json sha256=" <> hex))
    assertEqual "any order, upper-case hex lowered" (Right (Dns.IndexRecord "https://h/d.json" (Digest hex))) (Dns.parseIndexRecord ("v=salmon1  sha256=" <> Text.toUpper hex <> " url=https://h/d.json"))
    assertBool "another version" (either (const True) (const False) (Dns.parseIndexRecord ("v=salmon2 url=u sha256=" <> hex)))
    assertBool "no url" (either (const True) (const False) (Dns.parseIndexRecord ("v=salmon1 sha256=" <> hex)))
    assertBool "no digest" (either (const True) (const False) (Dns.parseIndexRecord "v=salmon1 url=u"))
    assertBool "a short digest" (either (const True) (const False) (Dns.parseIndexRecord "v=salmon1 url=u sha256=abc"))
    assertEqual "the name" "web.fleet.example" (Dns.recordName "fleet.example." (label "web"))

-------------------------------------------------------------------------------
-- git

-- | Run git in a directory, failing the test on a non-zero exit.
git :: FilePath -> [String] -> IO String
git dir args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc "git" (["-C", dir, "-c", "user.name=test", "-c", "user.email=test@example", "-c", "commit.gpgsign=false"] ++ args)) ""
    case code of
        ExitSuccess -> pure out
        ExitFailure n -> assertFailure ("git " <> unwords args <> " exited " <> show n <> ": " <> err) >> pure out

-- | Commit a document for a label into the publisher's clone and push it.
publishGit :: FilePath -> Label -> ByteString -> String -> IO ()
publishGit pub lbl bytes message = do
    let path = Git.documentPathIn pub (Git.Source "" Nothing (Just "hosts")) lbl
    createDirectoryIfMissing True (takeDirectory path)
    LByteString.writeFile path bytes
    _ <- git pub ["add", "-A"]
    _ <- git pub ["commit", "--quiet", "-m", message]
    _ <- git pub ["push", "--quiet", "origin", "main"]
    pure ()

gitRegistry :: IO ()
gitRegistry =
    withTempDir $ \root -> do
        let bare = root </> "repo.git"
            pub = root </> "pub"
            web = label "web"
            api = label "api"
        _ <- git root ["init", "--quiet", "--bare", "-b", "main", bare]
        _ <- git root ["clone", "--quiet", bare, pub]
        _ <- git pub ["checkout", "--quiet", "-b", "main"]
        publishGit pub web (document "web@1" [["a"], ["b"]]) "web@1"
        registry <- Git.gitRegistry (root </> "checkout") (Git.Source (Text.pack ("file://" <> bare)) (Just "main") (Just "hosts"))
        assertEqual "named as given" ("git+file://" <> Text.pack bare <> "#main:hosts") registry.registryName
        (w, _, freports, ()) <- withFollowing root registry plain [web, api] $ \d -> do
            waitFor "both files" ((&&) <$> fileExists root "a" <*> fileExists root "b")
            waitFor "api reported missing" (elem (Follow.Missing api) <$> d.followReports)
            -- a commit that does not touch the document: the stamp moves,
            -- the bytes do not, nothing is injected
            _ <- git pub ["commit", "--quiet", "--allow-empty", "-m", "nothing"]
            _ <- git pub ["push", "--quiet", "origin", "main"]
            threadDelay (4 * interval)
            assertEqual "one injection so far" [(2, 0)] . injections =<< d.followReports
            -- a second commit changes it
            publishGit pub web (document "web@2" [["a"], ["c"]]) "web@2"
            waitFor "the new file" (fileExists root "c")
            waitFor "the dropped one gone" (not <$> fileExists root "b")
            -- the repository gone is a failed round, and the world stands
            renameDirectory bare (bare <> ".away")
            waitFor "the failure" (not . null . fetchFailures <$> d.followReports)
            waitFor "the ladder" (not . null . backoffs <$> d.followReports)
            present <- (&&) <$> fileExists root "a" <*> fileExists root "c"
            assertBool "the last document stays in force" present
            renameDirectory (bare <> ".away") bare
            publishGit pub web (document "web@3" [["a"], ["c"], ["d"]]) "web@3"
            waitFor "recovered: the third document's file" (fileExists root "d")
        assertEqual "three injections" [(2, 0), (1, 1), (1, 0)] (injections freports)
        assertEqual "the ids" ["web@1", "web@2", "web@3"] [did | Follow.Injected _ did _ _ _ <- freports]
        assertBool "the world converged" (allConvergedUp w)
        assertBool "the failure names git" (any (Text.isInfixOf "git") (fetchFailures freports))

-------------------------------------------------------------------------------
-- http

-- | What the fixture server holds, and how it is told to misbehave.
data Store = Store
    { storeDocs :: IORef (Map Text ByteString)
    -- ^ by path, @/reg/web.json@
    , storeFailing :: IORef Bool
    -- ^ answer 500 to everything
    , storeBodies :: IORef Int
    -- ^ how many 200s carried a body
    }

newStore :: IO Store
newStore = Store <$> newIORef Map.empty <*> newIORef False <*> newIORef 0

-- | ETags from the digest, @304@ on a matching @If-None-Match@.
storeApp :: Store -> Wai.Application
storeApp st req respond = do
    failing <- readIORef st.storeFailing
    docs <- readIORef st.storeDocs
    let path = Text.decodeUtf8 (Wai.rawPathInfo req)
    if failing
        then respond (Wai.responseLBS HTTP.status500 [] "down")
        else case Map.lookup path docs of
            Nothing -> respond (Wai.responseLBS HTTP.status404 [] "no such document")
            Just bytes -> do
                let etag = C8.pack ("\"" <> Text.unpack (Follow.digestOf bytes).unDigest <> "\"")
                if lookup "If-None-Match" (Wai.requestHeaders req) == Just etag
                    then respond (Wai.responseLBS HTTP.status304 [("ETag", etag)] "")
                    else do
                        atomicModifyIORef' st.storeBodies (\n -> (n + 1, ()))
                        respond (Wai.responseLBS HTTP.status200 [("ETag", etag), ("Content-Type", "application/json")] bytes)

withStore :: (Store -> Text -> IO a) -> IO a
withStore body = do
    st <- newStore
    Warp.testWithApplication (pure (storeApp st)) $ \port ->
        body st ("http://127.0.0.1:" <> Text.pack (show port))

put :: Store -> Text -> ByteString -> IO ()
put st path bytes = atomicModifyIORef' st.storeDocs (\m -> (Map.insert path bytes m, ()))

httpRegistry :: IO ()
httpRegistry =
    withTempDir $ \root -> withStore $ \st base -> do
        let web = label "web"
            api = label "api"
        put st "/reg/web.json" (document "web@1" [["a"], ["b"]])
        mgr <- Http.newManager Http.defaultOptions
        let registry = Http.httpRegistry mgr (base <> "/reg")
        (w, _, freports, ()) <- withFollowing root registry plain [web, api] $ \d -> do
            waitFor "both files" ((&&) <$> fileExists root "a" <*> fileExists root "b")
            waitFor "api reported missing (404)" (elem (Follow.Missing api) <$> d.followReports)
            -- rounds keep going, and every one of them is a 304: no body,
            -- no injection
            bodies <- readIORef st.storeBodies
            threadDelay (4 * interval)
            bodies' <- readIORef st.storeBodies
            assertEqual "one body was ever sent for web" 1 bodies
            assertEqual "and no more since" bodies bodies'
            assertEqual "one injection" [(2, 0)] . injections =<< d.followReports
            -- 500: the ladder
            writeIORef st.storeFailing True
            waitFor "the failure" (not . null . fetchFailures <$> d.followReports)
            waitFor "two rungs" ((>= 2) . length . backoffs <$> d.followReports)
            present <- (&&) <$> fileExists root "a" <*> fileExists root "b"
            assertBool "the last document stays in force" present
            -- back, changed
            writeIORef st.storeFailing False
            put st "/reg/web.json" (document "web@2" [["a"], ["c"]])
            waitFor "the new file" (fileExists root "c")
        assertEqual "two injections" [(2, 0), (1, 1)] (injections freports)
        assertBool "the failure names the status" (any (Text.isInfixOf "500") (fetchFailures freports))
        assertBool "the ladder climbed" (2 `elem` backoffs freports)
        assertBool "the world converged" (allConvergedUp w)

-------------------------------------------------------------------------------
-- dns

-- | A resolver over a map, counting lookups.
stubResolver :: IORef (Map Text [Text]) -> IORef Int -> Dns.Resolver
stubResolver records lookups =
    Dns.Resolver
        { Dns.resolverName = "stub"
        , Dns.resolveTxt = \name -> do
            atomicModifyIORef' lookups (\n -> (n + 1, ()))
            Map.findWithDefault [] name <$> readIORef records
        }

indexRecord :: Text -> ByteString -> Text
indexRecord url bytes = "v=salmon1 url=" <> url <> " sha256=" <> (Follow.digestOf bytes).unDigest

dnsRegistry :: IO ()
dnsRegistry =
    withTempDir $ \root -> withStore $ \st base -> do
        let web = label "web"
            api = label "api"
            zone = "fleet.test"
            doc1 = document "web@1" [["a"], ["b"]]
            doc2 = document "web@2" [["a"], ["c"]]
            url = base <> "/store/web-latest.json"
        records <- newIORef (Map.fromList [("web.fleet.test", [indexRecord url doc1])])
        lookups <- newIORef 0
        put st "/store/web-latest.json" doc1
        mgr <- Http.newManager Http.defaultOptions
        let registry = Dns.dnsRegistry (stubResolver records lookups) mgr zone
        assertEqual "named as given" "dns:fleet.test" registry.registryName
        (w, _, freports, ()) <- withFollowing root registry plain [web, api] $ \d -> do
            waitFor "both files" ((&&) <$> fileExists root "a" <*> fileExists root "b")
            waitFor "api reported missing (no record)" (elem (Follow.Missing api) <$> d.followReports)
            -- rounds are lookups only: the record's digest is the stamp,
            -- so the store is not asked again
            bodies <- readIORef st.storeBodies
            n <- readIORef lookups
            threadDelay (4 * interval)
            bodies' <- readIORef st.storeBodies
            n' <- readIORef lookups
            assertEqual "one body was ever fetched" 1 bodies
            assertEqual "and none since" bodies bodies'
            assertBool "while the resolver kept being asked" (n' > n)
            -- the index moves before the store does: refused, not applied
            writeIORef records (Map.fromList [("web.fleet.test", [indexRecord url doc2])])
            waitFor "the mismatch" (any (Text.isInfixOf "does not hash") . fetchFailures <$> d.followReports)
            waitFor "the ladder" (not . null . backoffs <$> d.followReports)
            present <- fileExists root "c"
            assertBool "the announced-but-unserved document is not applied" (not present)
            -- the store catches up
            put st "/store/web-latest.json" doc2
            waitFor "the new file" (fileExists root "c")
            -- the record going away: the last document stays in force
            writeIORef records Map.empty
            waitFor "vanished" (elem (Follow.Vanished web) <$> d.followReports)
        assertEqual "two injections" [(2, 0), (1, 1)] (injections freports)
        assertBool "the world converged" (allConvergedUp w)

-------------------------------------------------------------------------------
-- bucket

bucketRegistry :: IO ()
bucketRegistry =
    withTempDir $ \root -> withStore $ \st base -> do
        let web = label "web"
        put st "/bucket/fleet/web.json" (document "web@1" [["a"]])
        registry <-
            Registry.open
                Registry.defaultOptions{Registry.optBucketEndpoint = Just base}
                (Registry.InBucket (Registry.Bucket Registry.S3 "bucket" "fleet"))
        assertEqual "named as given" "s3://bucket/fleet" registry.registryName
        (w, _, freports, ()) <- withFollowing root registry plain [web] $ \_ ->
            waitFor "the file" (fileExists root "a")
        assertEqual "one injection" [(1, 0)] (injections freports)
        assertBool "the world converged" (allConvergedUp w)

-------------------------------------------------------------------------------
-- the verify hook

verifyHook :: IO ()
verifyHook =
    withTempDir $ \root -> do
        let web = label "web"
            reg = root </> "reg"
            cache = root </> "cache"
            -- a verifier that refuses any document mentioning the seed `evil`
            refusing :: Follow.Verifier
            refusing _ bytes
                | "evil" `Text.isInfixOf` Text.decodeUtf8Lenient (LByteString.toStrict bytes) = pure (Left "mentions evil")
                | otherwise = pure (Right bytes)
            knobs = Knobs (Just cache) refusing
            publish bytes = createDirectoryIfMissing True reg >> LByteString.writeFile (Follow.documentPath reg web) bytes
        publish (document "web@1" [["a"]])
        (w, _, freports, ()) <- withFollowing root (Follow.directoryRegistry reg) knobs [web] $ \d -> do
            waitFor "the file" (fileExists root "a")
            publish (document "web@evil" [["a"], ["evil"]])
            waitFor "the refusal" (not . null <$> (\rs -> [() | Follow.Rejected{} <- rs]) <$> d.followReports)
            waitFor "the ladder" (not . null . backoffs <$> d.followReports)
            threadDelay (2 * interval)
            present <- fileExists root "evil"
            assertBool "never applied" (not present)
            declared <- (\rs -> [() | Serve.Declared{} <- rs]) <$> d.serveReports
            assertEqual "one declaration, the first document's" 1 (length declared)
            cached <- Follow.readCache cache web
            assertEqual "the cache still holds the good one" (Right (Just "web@1")) (fmap (fmap (\(c, _) -> c.cachedId)) cached)
            -- a good document again is applied on top of the last good one
            publish (document "web@2" [["a"], ["b"]])
            waitFor "the next good file" (fileExists root "b")
        assertEqual "the refusal, once, with its reason" ["mentions evil"] [why | Follow.Rejected _ _ why <- freports]
        assertEqual "two injections" [(1, 0), (1, 0)] (injections freports)
        assertBool "the world converged" (allConvergedUp w)
        -- a cache entry the verifier refuses is not replayed: write one
        -- by hand and start with the registry gone
        Follow.writeCache cache web (Follow.Cached "web@evil" (Follow.digestOf evilBytes) evilBytes)
        renameDirectory reg (reg <> ".away")
        renameDirectory (root </> "files") (root </> "files.away")
        (_, sreports2, freports2, ()) <- withFollowing root (Follow.directoryRegistry reg) knobs [web] $ \d -> do
            waitFor "the refusal" (not . null <$> (\rs -> [() | Follow.Rejected{} <- rs]) <$> d.followReports)
            threadDelay (2 * interval)
        assertEqual "nothing replayed" [] [() | Follow.Replayed{} <- freports2]
        assertEqual "nothing declared" [] [() | Serve.Declared{} <- sreports2]
        assertEqual "the refusal named the cache's digest" [Follow.digestOf evilBytes] [dg | Follow.Rejected _ dg _ <- freports2]
  where
    evilBytes = document "web@evil" [["evil"]]
