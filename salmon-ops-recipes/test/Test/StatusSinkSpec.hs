{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for milestone 5 of @specs/pull-mode.md@: the status
sink and the fleet fold.

Two @run serve@ loops, in process, follow one directory registry under
different labels and write their status documents into one directory —
which is the whole fleet shape in miniature: hosts that never talk to each
other, a registry they read, a directory they write, and a reader that
folds it. What is asserted: each document names its own host, label,
document id and mode; the document is rewritten after a convergence pass a
typed line caused and after a follow injection; an unwritable sink path is
reported once and the loop keeps serving; and the pure fold behind
@salmon-fleet status@ shows both hosts, filters by label and flags a stale
one.
-}
module Test.StatusSinkSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Fleet as Fleet
import qualified Salmon.Actions.Follow as Follow
import Salmon.Actions.Follow (Document (..), Entry (..), Label)
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (AppliedDocument (..), Convergence (..), Direction (..), Line (..), NodeState (..), Origin (..), Producer (..), World (..))
import qualified Salmon.Actions.Serve.StatusSink as StatusSink
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension, Op, Track', deps, op, ref)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (Reporter, contramap, reportBoth, silent)
import qualified Salmon.Reporter.Tagged as Tagged

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Serve.StatusSink and Salmon.Actions.Fleet"
        [ testCase "two loops, one registry, one sink directory: each document names its own host, label, id and mode; rewritten after a typed convergence and after an injection; the fold shows both" twoHostsOneDirectory
        , testCase "an unwritable sink path is reported once and the loop keeps serving" unwritableSink
        , testCase "the fold: both hosts, the label filter, the stale flag, the node counts" pureFold
        ]

-------------------------------------------------------------------------------
-- the served thing: "make these files exist", as Test.FollowSpec

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
    op "status-sink-root" (deps (fmap (fileOp spec.specDir) spec.specNames)) $ \actions ->
        actions{ref = mkRef "status-sink-root" (spec.specDir, spec.specNames)}

fileOp :: FilePath -> String -> Op
fileOp d n = FS.filecontents (FS.FileContents (d </> n) ("contents of " <> n))

-------------------------------------------------------------------------------
-- driving a loop with a sink beside it

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

{- | One following loop over @root@'s registry, as a host called @host@,
writing its status document to @sinkPath@. The files land in
@root/files-<host>@ so two hosts in one temp dir do not share nodes. The
composition is the one "Salmon.Builtin.CommandLine" makes: the loop's own
tagged reporter with the sink's beside it, the sink's observer handed to
'Serve.serveObserved'. -}
withHost :: FilePath -> Text -> FilePath -> [Label] -> (Driver -> IO a) -> IO (World Spec Spec, [Serve.Report], [Follow.Report], a)
withHost root host sinkPath labels body = do
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
                , Follow.followCache = Nothing
                , Follow.followRefuseOlder = False
                }
        followed = Follow.followed pk modeVar appliedVar
        own = Tagged.reportTexts serveReporter nodeReporter silent followReporter
        cfg = StatusSink.Config{StatusSink.configPath = sinkPath, StatusSink.configInterval = 1000000, StatusSink.configHost = host}
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
    w <- StatusSink.withSink cfg (Just followed) own $ \sink -> do
        let tagged = reportBoth own (StatusSink.sinkReporter sink)
            producers =
                [ Follow.follower (Tagged.followStream tagged) pk modeVar appliedVar follow (putMVar gate ())
                , Follow.gated gate (chanProducer stdinChan)
                ]
        Serve.serveObserved
            (StatusSink.sinkObserver sink)
            []
            Nothing
            True
            (contramap Serve.attributed (Tagged.serveStream tagged))
            (contramap Serve.attributed (Tagged.updownStream tagged))
            (parseSpec (root </> ("host-" <> Text.unpack host)))
            (Configure pure)
            program
            (Just followed)
            producers
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

sinkDir :: FilePath -> FilePath
sinkDir root = root </> "sinks"

label :: Text -> Label
label t = either (error . Text.unpack) id (Follow.mkLabel t)

publish :: FilePath -> Label -> Text -> [[String]] -> IO ()
publish root lbl did seeds = do
    createDirectoryIfMissing True (registryDir root)
    LByteString.writeFile (Follow.documentPath (registryDir root) lbl) (encode (Document did (fmap SeedWords seeds) Nothing))

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

hostFile :: FilePath -> Text -> String -> Bool -> IO Bool
hostFile root host n _ = doesFileExist (root </> ("host-" <> Text.unpack host) </> "files" </> n)

-- | The document at a path, if there is one and it parses.
readDoc :: FilePath -> IO (Maybe StatusSink.Document)
readDoc path = do
    present <- doesFileExist path
    if not present
        then pure Nothing
        else do
            attempt <- try (LByteString.readFile path >>= \b -> LByteString.length b `seq` pure b)
            pure $ case attempt of
                Left (_ :: SomeException) -> Nothing
                Right bytes -> either (const Nothing) Just (eitherDecode bytes)

-- | Wait until the document at the path satisfies the predicate, and return it.
waitDoc :: String -> FilePath -> (StatusSink.Document -> Bool) -> IO StatusSink.Document
waitDoc what path p = do
    waitFor what (maybe False p <$> readDoc path)
    readDoc path >>= maybe (assertFailure ("the document vanished: " <> what)) pure

labelIds :: StatusSink.Document -> [(Text, Text)]
labelIds d = [(a.appliedDocLabel, a.appliedDocId) | a <- d.docLabels]

kindOf :: Maybe Value -> Maybe Text
kindOf (Just (Object o)) | Just (String k) <- KeyMap.lookup "kind" o = Just k
kindOf _ = Nothing

allConvergedUp :: World seed directive -> Bool
allConvergedUp w = not (Map.null w.worldNodes) && all (\st -> st.nodeDirection == TurnUp && st.nodeConvergence == Converged) (Map.elems w.worldNodes)

-------------------------------------------------------------------------------

twoHostsOneDirectory :: IO ()
twoHostsOneDirectory =
    withTempDir $ \root -> do
        let web = label "web"
            db = label "db"
            alphaSink = sinkDir root </> "alpha.json"
            betaSink = sinkDir root </> "beta.json"
        publish root web "web@1" [["a"]]
        publish root db "db@1" [["b"]]
        (wAlpha, alphaReports, _, (wBeta, _, _, ())) <- withHost root "alpha" alphaSink [web] $ \alpha ->
            withHost root "beta" betaSink [db] $ \_ -> do
                -- both hosts converged on their own label and wrote about it
                dAlpha <- waitDoc "alpha's document, converged" alphaSink converged
                dBeta <- waitDoc "beta's document, converged" betaSink converged
                assertEqual "alpha names itself" "alpha" dAlpha.docHost
                assertEqual "beta names itself" "beta" dBeta.docHost
                assertEqual "alpha's label and id" [("web", "web@1")] (labelIds dAlpha)
                assertEqual "beta's label and id" [("db", "db@1")] (labelIds dBeta)
                assertEqual "alpha is following" "following" dAlpha.docMode
                assertEqual "beta is following" "following" dBeta.docMode
                assertEqual "the last converge is a converge-stop" (Just "converge-stop") (kindOf dAlpha.docLastConverge)
                assertEqual "the last follow report is the injection" (Just "injected") (kindOf dAlpha.docLastFollow)
                assertEqual "the document version" (Just (Number 1)) =<< rawField alphaSink "salmon-status"
                -- a typed line converges alpha: the document is rewritten
                -- with the new node count and a later `written`
                alpha.typeLine "up c"
                dAlpha' <- waitDoc "alpha's document after a typed up" alphaSink (\d -> d.docWritten > dAlpha.docWritten && nodes d > nodes dAlpha && converged d)
                assertEqual "the label is still web@1: nothing was fetched" [("web", "web@1")] (labelIds dAlpha')
                assertEqual "beta is untouched" (labelIds dBeta) . labelIds =<< waitDoc "beta's document" betaSink (const True)
                -- a new document for web is injected: rewritten again, now
                -- naming web@2
                publish root web "web@2" [["a"], ["d"]]
                dAlpha'' <- waitDoc "alpha's document after web@2" alphaSink (\d -> labelIds d == [("web", "web@2")] && converged d)
                assertBool "written later still" (dAlpha''.docWritten > dAlpha'.docWritten)
                assertEqual "the injection is the last follow report" (Just "injected") (kindOf dAlpha''.docLastFollow)
                -- the fold over the directory sees both, as salmon-fleet would
                (docs, rejected) <- Fleet.readStatusDir (sinkDir root)
                assertEqual "nothing rejected" [] rejected
                now <- getCurrentTime
                let rows = Fleet.fold Fleet.defaultOptions now docs
                assertEqual "both hosts, alpha first" ["alpha", "beta"] (fmap (.rowHost) rows)
                assertEqual "nothing is stale" [False, False] (fmap (.rowStale) rows)
                assertBool "every node converged on both" (all (\r -> r.rowConverged == r.rowNodes && r.rowNodes > 0) rows)
                assertEqual "the label filter" ["beta"] (fmap (.rowHost) (Fleet.fold Fleet.defaultOptions{Fleet.optLabel = Just "db"} now docs))
                -- a temp file is never left behind between writes
                assertEqual "only the two documents" 2 (length docs)
        assertBool "alpha converged" (allConvergedUp wAlpha)
        assertBool "beta converged" (allConvergedUp wBeta)
        assertEqual "no sink failure on alpha" [] [() | Serve.SinkFailed{} <- alphaReports]
  where
    nodes :: StatusSink.Document -> Int
    nodes d = let (_, _, n) = Fleet.nodeCounts d.docStatus in n
    converged :: StatusSink.Document -> Bool
    converged d = let (c, _, n) = Fleet.nodeCounts d.docStatus in n > 0 && c == n
    rawField path key = do
        bytes <- LByteString.readFile path
        pure $ case eitherDecode bytes of
            Right (Object o) -> KeyMap.lookup key o
            _ -> Nothing

{- | The sink's directory is a regular file, so neither the temp file nor
the rename can succeed. One report, then silence; the loop still answers. -}
unwritableSink :: IO ()
unwritableSink =
    withTempDir $ \root -> do
        let web = label "web"
            blocked = root </> "blocked"
        writeFile blocked "not a directory"
        publish root web "web@1" [["a"]]
        (w, reports, _, ()) <- withHost root "gamma" (blocked </> "gamma.json") [web] $ \d -> do
            waitFor "the first file" (hostFile root "gamma" "a" True)
            waitFor "the sink failure to be reported" (not . null <$> (\rs -> [() | Serve.SinkFailed{} <- rs]) <$> d.serveReports)
            -- two more triggers: a typed convergence and an injection
            d.typeLine "up b"
            waitFor "the typed file" (hostFile root "gamma" "b" True)
            publish root web "web@2" [["a"], ["c"]]
            waitFor "the injected file" (hostFile root "gamma" "c" True)
            -- and a couple of interval ticks
            threadDelay (2 * 1000000 + 200000)
            failures <- (\rs -> [path | Serve.SinkFailed path _ <- rs]) <$> d.serveReports
            assertEqual "reported once, for the path" [blocked </> "gamma.json"] failures
            -- the loop is still there
            before <- length . (\rs -> [() | Serve.StatusReport{} <- rs]) <$> d.serveReports
            d.typeLine "status"
            waitFor "a status answer" ((> before) . length . (\rs -> [() | Serve.StatusReport{} <- rs]) <$> d.serveReports)
        assertBool "the world converged regardless" (allConvergedUp w)
        assertBool "the render names the path" $
            any (Text.isInfixOf (Text.pack blocked)) (concat [Serve.renderReport rep | rep@Serve.SinkFailed{} <- reports])
        exists <- doesFileExist (blocked </> "gamma.json")
        assertBool "nothing was written" (not exists)

{- | The fold on documents built by hand: no loop, no clock but the one
given. -}
pureFold :: IO ()
pureFold = do
    let t0 = read "2026-09-24 10:00:00 UTC" :: UTCTime
        at s = addUTCTime (fromIntegral (s :: Int)) t0
        applied lbl did = AppliedDocument lbl did "32ea59311d97a7c0ffff" (at (-30))
        status convergences =
            object ["kind" .= ("status" :: Text), "mode" .= ("following" :: Text), "nodes" .= [object ["convergence" .= c] | c <- convergences :: [Text]]]
        doc host written labels convergences =
            StatusSink.Document
                { StatusSink.docHost = host
                , StatusSink.docWritten = written
                , StatusSink.docMode = "following"
                , StatusSink.docLabels = labels
                , StatusSink.docStatus = status convergences
                , StatusSink.docLastConverge = Nothing
                , StatusSink.docLastFollow = Nothing
                }
        docs =
            [ ("/sinks/web-2.json", doc "web-2" (at (-5)) [applied "web" "web@42", applied "canary" "canary@7"] ["converged", "converged", "errored"])
            , ("/sinks/web-1.json", doc "web-1" (at (-90)) [applied "web" "web@42"] ["converged", "pending"])
            , ("/sinks/db-1.json", doc "db-1" (at 2) [applied "db" "db@3"] [])
            ]
        rows = Fleet.fold Fleet.defaultOptions t0 docs
    assertEqual "hosts in name order" ["db-1", "web-1", "web-2"] (fmap (.rowHost) rows)
    assertEqual "converged / errored / total" [(0, 0, 0), (1, 0, 2), (2, 1, 3)] [(r.rowConverged, r.rowErrored, r.rowNodes) | r <- rows]
    assertEqual "the stale flag past 60s; a clock ahead of the reader's is not stale" [False, True, False] (fmap (.rowStale) rows)
    assertEqual "ages" [-2, 90, 5] (fmap (.rowAge) rows)
    assertEqual "the label filter keeps every host carrying it" ["web-1", "web-2"] (fmap (.rowHost) (Fleet.fold Fleet.defaultOptions{Fleet.optLabel = Just "web"} t0 docs))
    assertEqual "a label nobody carries" [] (Fleet.fold Fleet.defaultOptions{Fleet.optLabel = Just "nope"} t0 docs)
    assertEqual "a looser threshold" [False, False, False] (fmap (.rowStale) (Fleet.fold Fleet.defaultOptions{Fleet.optStale = 120} t0 docs))
    assertEqual
        "the line"
        "web-2\tfollowing\tweb=web@42@32ea59311d97,canary=canary@7@32ea59311d97\t2/3\t1\t5s\t"
        (Fleet.renderRow (rows !! 2))
    assertEqual
        "a stale line"
        "web-1\tfollowing\tweb=web@42@32ea59311d97\t1/2\t0\t90s\tstale"
        (Fleet.renderRow (rows !! 1))
    assertEqual "no nodes, a clock ahead" "db-1\tfollowing\tdb=db@3@32ea59311d97\t0/0\t0\t-2s\t" (Fleet.renderRow (rows !! 0))
    assertEqual "no labels renders as a dash" "-" (Text.splitOn "\t" (Fleet.renderRow (Fleet.rowOf Fleet.defaultOptions t0 "/x.json" (doc "x" t0 [] []))) !! 2)
    -- the JSON row carries the same fields
    case Fleet.rowValue (rows !! 1) of
        Object o -> do
            assertEqual "stale" (Just (Bool True)) (KeyMap.lookup "stale" o)
            assertEqual "host" (Just (String "web-1")) (KeyMap.lookup "host" o)
            assertEqual "converged" (Just (Number 1)) (KeyMap.lookup "converged" o)
        v -> assertFailure ("not an object: " <> show v)
