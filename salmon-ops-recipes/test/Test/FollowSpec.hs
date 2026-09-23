{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for "Salmon.Actions.Follow": a @run serve@ following a
directory registry over a throwaway temp dir, with the loop's standard input
driven from a channel the test writes into.

The seed is 'Test.ServeSpec''s (a list of file names under the temp dir); what
is under test is the fetcher — that a document written to the registry
becomes the world, that rewriting it byte-for-byte injects /nothing/ (the
starvation rule of @specs/pull-mode.md@, as a test), that a seed dropped from
a document goes down unless another label still carries it, and that a
document that cannot be read, or a seed in it that cannot be configured, is
reported without ending the loop.
-}
module Test.FollowSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, toJSON)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Follow as Follow
import Salmon.Actions.Follow (Document (..), Entry (..), Label)
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Direction (..), Line (..), NodeState (..), Origin (..), Producer (..), Provenance (..), World (..))
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
        "Salmon.Actions.Follow"
        [ testCase "a document written to the registry becomes the world, before stdin is read" documentBecomesTheWorld
        , testCase "rewriting the same bytes injects nothing (the starvation rule)" identicalRewriteInjectsNothing
        , testCase "a seed dropped from the document goes down" droppedSeedGoesDown
        , testCase "two labels: a seed stays up while any label still carries it" unionAcrossLabels
        , testCase "a document that cannot be read is reported and the loop keeps serving" malformedIsReported
        , testCase "a seed the binary cannot parse or configure is reported and the rest is applied" badSeedIsContained
        , testCase "the document format round-trips and refuses what it does not understand" documentFormat
        ]

-------------------------------------------------------------------------------
-- the served thing: "make these files exist", same as Test.ServeSpec

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

-- | A seed naming a file called @boom@ cannot be configured: the
-- 'Configure' throws, which is the failure a fetched document must not be
-- able to take the loop down with.
configure :: Configure IO Spec Spec
configure = Configure $ \spec ->
    if "boom" `elem` spec.specNames
        then throwIO (userError "boom: refusing to configure")
        else pure spec

program :: Track' Spec
program = Track $ \spec ->
    op "follow-spec-root" (deps (fmap (fileOp spec.specDir) spec.specNames)) $ \actions ->
        actions{ref = mkRef "follow-spec-root" (spec.specDir, spec.specNames)}

fileOp :: FilePath -> String -> Op
fileOp d n = FS.filecontents (FS.FileContents (d </> n) ("contents of " <> n))

-------------------------------------------------------------------------------
-- driving the loop

data Driver = Driver
    { typeLine :: String -> IO ()
    , serveReports :: IO [Serve.Report]
    , followReports :: IO [Follow.Report]
    }

-- | The poll interval the fetcher runs at under test: short enough that a
-- test waiting several rounds stays cheap, long enough that "several rounds
-- passed" is unambiguous.
interval :: Int
interval = 100000

-- | The schedule under test: rounds at 'interval', no jitter (so "several
-- rounds' worth" means what it says), no quiet window (a change is injected
-- at the round that saw it, as milestone 2 did — the window has its own
-- spec, "Test.FollowSchedulerSpec"), and a short cap so that a test that
-- leaves a document malformed for a few rounds is not left waiting on the
-- ladder for long once it fixes it.
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

{- | Run a following loop over @root@: the registry is @root/reg@, the files
land in @root/files@. The body drives it through the 'Driver' and must end
with @quit@ (or let the block do it); the world comes back once the loop
has ended. -}
withFollowing :: FilePath -> [Label] -> (Driver -> IO a) -> IO (World Spec Spec, [Serve.Report], [Follow.Report], a)
withFollowing root labels body = do
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
                , Follow.followVerify = Follow.noVerifier
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
    -- the loop runs on this thread, the body on another: whatever the body
    -- concludes (or throws — an assertion inside it must fail the test, not
    -- hang it) is handed back once it has closed standard input, which is
    -- what ends the loop.
    resultVar <- newTChanIO
    _ <- forkIO $ do
        outcome <- try (body driver)
        atomically (writeTChan stdinChan Nothing)
        atomically (writeTChan resultVar outcome)
    w <- Serve.serveProducers [] Nothing True serveReporter nodeReporter (parseSpec root) configure program producers
    outcome <- atomically (readTChan resultVar)
    case outcome of
        Left (ex :: SomeException) -> throwIO ex
        Right a -> (,,,) w <$> readServe <*> readFollow <*> pure a

-- | A 'Stdin'-origin producer fed from a channel; 'Nothing' is end of input.
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

label :: Text -> Label
label t = either (error . Text.unpack) id (Follow.mkLabel t)

-- | Write a document naming these seeds (one file name each) under a label.
publish :: FilePath -> Label -> Text -> [[String]] -> IO ()
publish root lbl did seeds = do
    createDirectoryIfMissing True (registryDir root)
    LByteString.writeFile (Follow.documentPath (registryDir root) lbl) (encode (Document did (fmap SeedWords seeds) Nothing))

-- | Poll until the condition holds, or fail after a generous bound.
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

assertFile :: FilePath -> String -> Bool -> IO ()
assertFile root n expected = do
    found <- fileExists root n
    assertEqual (n <> " exists") expected found

convergences :: [Serve.Report] -> Int
convergences reports = length [() | Serve.ConvergeStop{} <- reports]

injections :: [Follow.Report] -> Int
injections reports = length [() | Follow.Injected{} <- reports]

fetchedOrigins :: World seed directive -> [(Serve.Declaration, Provenance, [String])]
fetchedOrigins w = [(e.logDeclaration, prov, e.logTokens) | e <- reverse w.worldLog, Fetched prov <- [e.logOrigin]]

-------------------------------------------------------------------------------

{- | The registry's document is the first thing the loop sees: a @history@
queued on stdin before the loop even starts is answered /after/ the fetched
declarations, because standard input is held behind the fetcher's first
round. -}
documentBecomesTheWorld :: IO ()
documentBecomesTheWorld =
    withTempDir $ \root -> do
        publish root (label "web") "web@1" [["a"], ["b"]]
        (w, reports, freports, ()) <- withFollowing root [label "web"] $ \d -> do
            typeLine d "history"
            waitFor "both files" ((&&) <$> fileExists root "a" <*> fileExists root "b")
        assertBool "every node converged up" (all (\st -> st.nodeDirection == TurnUp && st.nodeConvergence == Converged) (Map.elems w.worldNodes))
        assertEqual "one injection, two seeds up" [2] [nup | Follow.Injected _ _ _ nup _ <- freports]
        assertEqual "one convergence for the batch" 1 (convergences reports)
        let origins = fetchedOrigins w
        assertEqual "both declarations carry the fetcher's origin" 2 (length origins)
        assertBool "the origin names the registry, the label and the document" $
            all
                ( \(decl, prov, _) ->
                    decl == Serve.Add
                        && prov.provRegistry == Text.pack (registryDir root)
                        && prov.provLabel == "web"
                        && prov.provDocument == "web@1"
                        && Text.length prov.provDigest == 64
                )
                origins
        -- the queued history saw the fetched epochs: stdin came second
        assertEqual
            "history, typed before the loop started, lists the two fetched epochs"
            [2]
            [length xs | Serve.HistoryReport xs <- reports]
        assertBool "and history renders the fetcher origin" $
            any (Text.isInfixOf "[fetched") (concatMap Serve.renderReport [rep | rep@Serve.HistoryReport{} <- reports])

{- | The starvation rule. Rewriting the document with identical bytes moves
its mtime, so the registry re-reads it, and the digest says nothing changed:
no batch is injected, no command reaches the loop, no convergence runs. -}
identicalRewriteInjectsNothing :: IO ()
identicalRewriteInjectsNothing =
    withTempDir $ \root -> do
        publish root (label "web") "web@1" [["a"]]
        (_, reports, freports, (nConv, nInj)) <- withFollowing root [label "web"] $ \d -> do
            waitFor "the file" (fileExists root "a")
            waitFor "the batch's convergence" ((>= 1) . convergences <$> serveReports d)
            nConv <- convergences <$> serveReports d
            nInj <- injections <$> followReports d
            -- same bytes, new mtime; then several rounds' worth of waiting
            threadDelay 20000
            publish root (label "web") "web@1" [["a"]]
            threadDelay (5 * interval)
            pure (nConv, nInj)
        assertEqual "no further injection" nInj (injections freports)
        assertEqual "no further convergence" nConv (convergences reports)
        assertEqual "no further declaration either" 1 (length [() | Serve.Declared{} <- reports])

droppedSeedGoesDown :: IO ()
droppedSeedGoesDown =
    withTempDir $ \root -> do
        publish root (label "web") "web@1" [["a"], ["b"]]
        (w, _, freports, ()) <- withFollowing root [label "web"] $ \d -> do
            waitFor "both files" ((&&) <$> fileExists root "a" <*> fileExists root "b")
            publish root (label "web") "web@2" [["a"]]
            waitFor "b torn down" (not <$> fileExists root "b")
            waitFor "the second injection" ((>= 2) . injections <$> followReports d)
        assertFile root "a" True
        assertEqual "second injection: nothing up, one down" [(2, 0), (0, 1)] [(nup, ndown) | Follow.Injected _ _ _ nup ndown <- freports]
        assertBool "a's node is still converged up" (any (\st -> st.nodeDirection == TurnUp && st.nodeConvergence == Converged) (Map.elems w.worldNodes))
        assertEqual
            "history has the fetched down, from the second document"
            [(Serve.Remove, "web@2", ["b"])]
            [(decl, prov.provDocument, toks) | (decl, prov, toks) <- fetchedOrigins w, decl == Serve.Remove]

{- | Two labels sharing a seed. Dropping it from one document changes
nothing (the other still carries it, so the diff is empty and is reported as
such rather than injected); dropping it from the last one takes it down. -}
unionAcrossLabels :: IO ()
unionAcrossLabels =
    withTempDir $ \root -> do
        publish root (label "web") "web@1" [["a"], ["shared"]]
        publish root (label "api") "api@1" [["b"], ["shared"]]
        (_, _, freports, ()) <- withFollowing root [label "web", label "api"] $ \d -> do
            waitFor "all three files" (and <$> traverse (fileExists root) ["a", "b", "shared"])
            publish root (label "web") "web@2" [["a"]]
            waitFor "the web document's empty diff" (any isNoDiff <$> followReports d)
            threadDelay (2 * interval)
            assertFile root "shared" True
            publish root (label "api") "api@2" [["b"]]
            waitFor "shared torn down" (not <$> fileExists root "shared")
        assertFile root "a" True
        assertFile root "b" True
        assertEqual "the empty diff was for web@2" [("web@2")] [did | Follow.NoDiff _ did _ <- freports]
        assertEqual "the teardown came from api@2" ["api@2"] [did | Follow.Injected _ did _ 0 1 <- freports]
  where
    isNoDiff Follow.NoDiff{} = True
    isNoDiff _ = False

malformedIsReported :: IO ()
malformedIsReported =
    withTempDir $ \root -> do
        createDirectoryIfMissing True (registryDir root)
        LByteString.writeFile (Follow.documentPath (registryDir root) (label "web")) "{\"salmon\": 1, \"id\": \"x\", \"seeds\": [{\"neither\": 1}]}"
        (w, _, freports, ()) <- withFollowing root [label "web"] $ \d -> do
            waitFor "the complaint" (any isMalformed <$> followReports d)
            threadDelay (3 * interval)
            -- one complaint, not one per round
            n <- length . filter isMalformed <$> followReports d
            assertEqual "reported once" 1 n
            -- the loop is still serving: a good document is applied
            publish root (label "web") "web@1" [["a"]]
            waitFor "the file" (fileExists root "a")
        assertBool "the world converged after the bad document" (all (\st -> st.nodeConvergence == Converged) (Map.elems w.worldNodes))
        assertEqual "exactly one injection, from the good document" ["web@1"] [did | Follow.Injected _ did _ _ _ <- freports]
  where
    isMalformed Follow.Malformed{} = True
    isMalformed _ = False

{- | A seed the binary's own parser rejects (here, an empty word list) and a
seed whose 'Configure' throws are both reported as bad seeds by the loop; the
document's other seeds are applied and the loop lives on to apply the next
document. -}
badSeedIsContained :: IO ()
badSeedIsContained =
    withTempDir $ \root -> do
        publish root (label "web") "web@1" [[], ["boom"], ["a"]]
        (w, reports, _, ()) <- withFollowing root [label "web"] $ \_ -> do
            waitFor "the good seed's file" (fileExists root "a")
            -- the bad seeds stay in the document, so this diff is one `up`
            -- and does not re-declare (and re-report) them on the way down
            publish root (label "web") "web@2" [[], ["boom"], ["a"], ["b"]]
            waitFor "the next document's file" (fileExists root "b")
        assertEqual "two bad seeds reported" 2 (length [() | Serve.BadSeed{} <- reports])
        assertBool "one of them is the configure that threw" (any (Text.isInfixOf "configure threw") [err | Serve.BadSeed err <- reports])
        assertBool "everything else converged" (all (\st -> st.nodeConvergence == Converged) (Map.elems w.worldNodes))

documentFormat :: IO ()
documentFormat = do
    let doc = Document "web@1" [SeedWords ["app", "--version", "42"], SeedDirective (toJSON (Spec "/x" ["a"]))] Nothing
    assertEqual "round-trips" (Right doc) (eitherDecode (encode doc))
    assertBool "a future format version is refused" (isLeft (decodeDoc "{\"salmon\": 2, \"id\": \"x\", \"seeds\": []}"))
    assertBool "an entry needs exactly one of seed/directive" (isLeft (decodeDoc "{\"salmon\": 1, \"id\": \"x\", \"seeds\": [{\"seed\": [\"a\"], \"directive\": {}}]}"))
    assertEqual "unknown top-level keys are ignored" (Right (Document "x" [] Nothing)) (decodeDoc "{\"salmon\": 1, \"id\": \"x\", \"seeds\": [], \"note\": \"yesterday\"}")
    assertBool "`published` is a known key, and one that does not parse is refused rather than ignored" (isLeft (decodeDoc "{\"salmon\": 1, \"id\": \"x\", \"seeds\": [], \"published\": \"yesterday\"}"))
    assertEqual "`published` round-trips" (Right (Document "x" [] (Just (read "2026-09-23 10:41:07 UTC")))) (decodeDoc "{\"salmon\": 1, \"id\": \"x\", \"seeds\": [], \"published\": \"2026-09-23T10:41:07Z\"}")
    assertBool "a label cannot escape the directory" (isLeft (Follow.mkLabel "../etc"))
    assertBool "a label cannot start with a dot" (isLeft (Follow.mkLabel ".hidden"))
    assertEqual "the file name convention" "/reg/web-api.json" (Follow.documentPath "/reg" (label "web-api"))
  where
    decodeDoc :: LByteString.ByteString -> Either String Document
    decodeDoc = eitherDecode
    isLeft = either (const True) (const False)
