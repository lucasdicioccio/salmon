{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for milestone 4 of @specs/pull-mode.md@: the cached
last document and @mode@ in @status@.

Same harness as "Test.FollowSpec" — a @run serve@ following a directory
registry over a temp dir, standard input driven from a channel — with two
knobs more: a cache directory, and 'Follow.followRefuseOlder'. What is under
test is a restart: a loop that applied a document, quit, and comes back with
the registry moved away must converge to what it last knew and say
@mode: replay@; the registry coming back with the same bytes must inject
nothing (the starvation rule, across restarts) and turn the mode to
@following@; a different document must be diffed against the replayed one.
Then the refusals: an older @published@ under the flag, a cache file that
does not parse, and what @status@ says with nothing followed at all.
-}
module Test.FollowCacheSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, renameDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Follow as Follow
import Salmon.Actions.Follow (Document (..), Entry (..), Label)
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Direction (..), Line (..), Mode (..), NodeState (..), Origin (..), Producer (..), Provenance (..), World (..))
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
        "Salmon.Actions.Follow (cache and mode)"
        [ testCase "a restart with the registry gone replays the cache (mode: replay); the registry back with the same bytes injects nothing (mode: following); a different document is diffed against the replayed one" restartOnTheCache
        , testCase "no cache and no registry: nothing is declared, and the mode is following once a round succeeds" nothingWithoutCacheOrRegistry
        , testCase "--follow-refuse-older: an older `published` is refused and a newer one accepted" refuseOlder
        , testCase "a cache file that does not parse is reported once and ignored" corruptCacheIsIgnored
        , testCase "without --follow, status says mode: interactive" interactiveWithoutFollow
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
    op "follow-cache-root" (deps (fmap (fileOp spec.specDir) spec.specNames)) $ \actions ->
        actions{ref = mkRef "follow-cache-root" (spec.specDir, spec.specNames)}

fileOp :: FilePath -> String -> Op
fileOp d n = FS.filecontents (FS.FileContents (d </> n) ("contents of " <> n))

-------------------------------------------------------------------------------
-- driving the loop

data Driver = Driver
    { typeLine :: String -> IO ()
    , serveReports :: IO [Serve.Report]
    , followReports :: IO [Follow.Report]
    , readMode :: IO Mode
    }

interval :: Int
interval = 100000

-- | Rounds at 'interval', no jitter, no window, a short cap: as
-- "Test.FollowSpec".
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

-- | The knobs a session is started with.
data Knobs = Knobs
    { knobCache :: Maybe FilePath
    , knobRefuseOlder :: Bool
    }

{- | One session of a following loop over @root@: the registry is
@root/reg@, the files land in @root/files@. Ends when the body returns (or
throws); the world comes back once the loop has ended. -}
withFollowing :: FilePath -> Knobs -> [Label] -> (Driver -> IO a) -> IO (World Spec Spec, [Serve.Report], [Follow.Report], a)
withFollowing root knobs labels body = do
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
                { Follow.followRegistry = Follow.directoryRegistry (registryDir root)
                , Follow.followLabels = labels
                , Follow.followSchedule = schedule
                , Follow.followCache = knobs.knobCache
                , Follow.followRefuseOlder = knobs.knobRefuseOlder
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
                , readMode = readIORef modeVar
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

registryDir :: FilePath -> FilePath
registryDir root = root </> "reg"

cacheDir :: FilePath -> FilePath
cacheDir root = root </> "cache"

label :: Text -> Label
label t = either (error . Text.unpack) id (Follow.mkLabel t)

-- | Write a document naming these seeds under a label, published at the
-- given time if any.
publishAt :: FilePath -> Label -> Text -> Maybe UTCTime -> [[String]] -> IO ()
publishAt root lbl did published seeds = do
    createDirectoryIfMissing True (registryDir root)
    LByteString.writeFile (Follow.documentPath (registryDir root) lbl) (encode (Document did (fmap SeedWords seeds) published))

publish :: FilePath -> Label -> Text -> [[String]] -> IO ()
publish root lbl did = publishAt root lbl did Nothing

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

-- | The modes every @status@ answered, in order.
statusModes :: [Serve.Report] -> [Mode]
statusModes reports = [m | Serve.StatusReport m _ _ <- reports]

-- | Type @status@ and wait for one more answer than there were before.
askStatus :: Driver -> IO Mode
askStatus d = do
    before <- length . statusModes <$> d.serveReports
    d.typeLine "status"
    waitFor "a status answer" ((> before) . length . statusModes <$> d.serveReports)
    last . statusModes <$> d.serveReports

fetchedIds :: World seed directive -> [Text]
fetchedIds w = [prov.provDocument | e <- reverse w.worldLog, Fetched prov <- [e.logOrigin]]

allConvergedUp :: World seed directive -> Bool
allConvergedUp w = not (Map.null w.worldNodes) && all (\st -> st.nodeDirection == TurnUp && st.nodeConvergence == Converged) (Map.elems w.worldNodes)

-------------------------------------------------------------------------------

{- | Session one applies a document with a cache directory; session two
starts with the registry renamed away. Then, inside session two, the
registry is put back — the same bytes — and later rewritten. -}
restartOnTheCache :: IO ()
restartOnTheCache =
    withTempDir $ \root -> do
        let web = label "web"
            knobs = Knobs (Just (cacheDir root)) False
        publish root web "web@1" [["a"], ["b"]]
        (w1, _, freports1, ()) <- withFollowing root knobs [web] $ \_ ->
            waitFor "both files" ((&&) <$> fileExists root "a" <*> fileExists root "b")
        assertBool "session one converged" (allConvergedUp w1)
        assertEqual "session one injected once" [(2, 0)] (injections freports1)
        -- the cache holds what was applied, and only that: no temp file left
        cached <- Follow.readCache (cacheDir root) web
        assertEqual "the cache holds web@1" (Right (Just "web@1")) (fmap (fmap (\(c, _) -> c.cachedId)) cached)
        entries <- listDirectory (cacheDir root)
        assertEqual "one file, renamed into place" ["web.applied.json"] entries
        -- the registry goes away, and the files go with it, so that the
        -- world is visibly rebuilt from the cache rather than found there
        renameDirectory (registryDir root) (root </> "reg.away")
        renameDirectory (root </> "files") (root </> "files.away")
        (w2, reports2, freports2, ()) <- withFollowing root knobs [web] $ \d -> do
            waitFor "both files, from the cache" ((&&) <$> fileExists root "a" <*> fileExists root "b")
            m <- askStatus d
            assertEqual "status says replay" Replay m
            m' <- d.readMode
            assertEqual "and so does the accessor" Replay m'
            -- the registry is back with the same bytes: nothing injected,
            -- and the mode turns to following at the round that saw it
            renameDirectory (root </> "reg.away") (registryDir root)
            waitFor "the mode to turn" ((== Following) <$> d.readMode)
            m'' <- askStatus d
            assertEqual "status says following" Following m''
            threadDelay (3 * interval)
            assertEqual "nothing injected beyond the replay" [(2, 0)] . injections =<< d.followReports
            -- a different document is diffed against the replayed one
            publish root web "web@2" [["a"], ["b"], ["c"]]
            waitFor "the new file" (fileExists root "c")
        assertBool "session two converged" (allConvergedUp w2)
        assertEqual "the replay was reported for web@1" ["web@1"] [did | Follow.Replayed _ did _ <- freports2]
        assertBool "the registry's absence was reported" (not (null [() | Follow.FetchFailed{} <- freports2]))
        assertEqual "two injections: the replay, then one seed up" [(2, 0), (1, 0)] (injections freports2)
        assertEqual "history: the replayed declarations name the cached document, the diff the new one" ["web@1", "web@1", "web@2"] (fetchedIds w2)
        assertEqual "status answered replay, then following" [Replay, Following] (statusModes reports2)
        assertBool "the render says so on its first line" $
            case [Serve.renderReport rep | rep@Serve.StatusReport{} <- reports2] of
                (first : _) : _ -> first == "serve: mode: replay"
                _ -> False
        cached' <- Follow.readCache (cacheDir root) web
        assertEqual "the cache moved on to web@2" (Right (Just "web@2")) (fmap (fmap (\(c, _) -> c.cachedId)) cached')

{- | Neither a cache entry nor a registry: the startup round fails, nothing
is replayed, nothing is declared; the first round that succeeds leaves the
mode at following. -}
nothingWithoutCacheOrRegistry :: IO ()
nothingWithoutCacheOrRegistry =
    withTempDir $ \root -> do
        let web = label "web"
        (w, _, freports, ()) <- withFollowing root (Knobs (Just (cacheDir root)) False) [web] $ \d -> do
            waitFor "the failure to be reported" (not . null <$> (\rs -> [() | Follow.FetchFailed{} <- rs]) <$> d.followReports)
            threadDelay (2 * interval)
            declared <- (\rs -> [() | Serve.Declared{} <- rs]) <$> d.serveReports
            assertEqual "nothing declared" [] declared
            publish root web "web@1" [["a"]]
            waitFor "the file, once the registry exists" (fileExists root "a")
            m <- askStatus d
            assertEqual "following after the first successful round" Following m
        assertEqual "one injection, from the registry" [(1, 0)] (injections freports)
        assertEqual "nothing was replayed" [] [() | Follow.Replayed{} <- freports]
        assertBool "the world converged" (allConvergedUp w)

refuseOlder :: IO ()
refuseOlder =
    withTempDir $ \root -> do
        let web = label "web"
            t1 = read "2026-09-23 10:00:00 UTC"
            t2 = read "2026-09-23 11:00:00 UTC"
            t3 = read "2026-09-23 12:00:00 UTC"
        publishAt root web "web@t2" (Just t2) [["a"]]
        (w, _, freports, ()) <- withFollowing root (Knobs Nothing True) [web] $ \d -> do
            waitFor "the first file" (fileExists root "a")
            -- older: refused, and refused once rather than once per round
            publishAt root web "web@t1" (Just t1) [["a"], ["b"]]
            waitFor "the refusal" (not . null <$> (\rs -> [() | Follow.Stale{} <- rs]) <$> d.followReports)
            threadDelay (3 * interval)
            present <- fileExists root "b"
            assertBool "the older document's seed is not applied" (not present)
            stale <- (\rs -> [did | Follow.Stale _ did <- rs]) <$> d.followReports
            assertEqual "one refusal" ["web@t1"] stale
            -- newer: accepted
            publishAt root web "web@t3" (Just t3) [["a"], ["b"]]
            waitFor "the newer document's file" (fileExists root "b")
            -- and one with no `published` at all is never refused
            publish root web "web@untimed" [["a"], ["b"], ["c"]]
            waitFor "the untimed document's file" (fileExists root "c")
        assertEqual "three injections: t2, t3, untimed" [(1, 0), (1, 0), (1, 0)] (injections freports)
        assertEqual "the ids applied" ["web@t2", "web@t3", "web@untimed"] [did | Follow.Injected _ did _ _ _ <- freports]
        assertBool "the world converged" (allConvergedUp w)

corruptCacheIsIgnored :: IO ()
corruptCacheIsIgnored =
    withTempDir $ \root -> do
        let web = label "web"
        createDirectoryIfMissing True (cacheDir root)
        LByteString.writeFile (Follow.cachePath (cacheDir root) web) "{\"salmon-cache\": 1, \"id\": \"x\", \"sha256\": \"not-the-digest\", \"document\": \"{}\"}"
        -- no registry either: the corrupt entry is the only thing that
        -- could have declared anything
        (w, _, freports, ()) <- withFollowing root (Knobs (Just (cacheDir root)) False) [web] $ \d -> do
            waitFor "the complaint" (not . null <$> (\rs -> [() | Follow.BadCache{} <- rs]) <$> d.followReports)
            threadDelay (2 * interval)
            declared <- (\rs -> [() | Serve.Declared{} <- rs]) <$> d.serveReports
            assertEqual "nothing declared" [] declared
            m <- askStatus d
            assertEqual "nothing replayed, so not in replay" Following m
            -- the loop is fine: a registry appearing is applied
            publish root web "web@1" [["a"]]
            waitFor "the file" (fileExists root "a")
        assertEqual "the complaint, once" 1 (length [() | Follow.BadCache{} <- freports])
        assertBool "it names the digest mismatch" (any (Text.isInfixOf "digest") [err | Follow.BadCache _ err <- freports])
        assertEqual "nothing was replayed" [] [() | Follow.Replayed{} <- freports]
        assertBool "the world converged" (allConvergedUp w)
        -- and the good document replaced the corrupt entry
        cached <- Follow.readCache (cacheDir root) web
        assertEqual "the cache now holds web@1" (Right (Just "web@1")) (fmap (fmap (\(c, _) -> c.cachedId)) cached)

interactiveWithoutFollow :: IO ()
interactiveWithoutFollow =
    withTempDir $ \root -> do
        (serveReporter, readServe) <- capture
        (nodeReporter, _) <- capture :: IO (Reporter (UpDown.Report Extension), IO [UpDown.Report Extension])
        stdinChan <- newTChanIO
        atomically (writeTChan stdinChan (Just "status"))
        atomically (writeTChan stdinChan Nothing)
        _ <- Serve.serveProducers [] Nothing True serveReporter nodeReporter (parseSpec root) (Configure pure) program [chanProducer stdinChan]
        reports <- readServe
        assertEqual "interactive" [Interactive] (statusModes reports)
        assertEqual "and rendered first" ["serve: mode: interactive", "serve: no nodes"] (concat [Serve.renderReport rep | rep@Serve.StatusReport{} <- reports])
