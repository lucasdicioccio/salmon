{- | Layer 0 coverage for @salmon-fleet status@'s @--pretty@ table
(projectz feature 962ca8a9-48c7-4582-854f-e4581fdccc60): 'Fleet.decidePretty'
truth table, a golden of the pretty table (color forced off, so the golden
is stable across runs/terminals), and a golden of the plain TSV
('Salmon.Actions.Fleet.renderHeader'\/'renderRow', untouched by this
feature) — pinning "the default did not change" for existing pipe/script
consumers.
-}
module Test.FleetSpec (tests) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Fleet (decidePretty, describeValue, foldStatusDir, prettyTable)
import Salmon.Actions.Fleet (Row (..), renderHeader, renderRow, rowValue)
import Salmon.Actions.Serve (AppliedDocument (..))

tests :: TestTree
tests =
    testGroup
        "Fleet"
        [ testGroup "decidePretty: --pretty/--no-pretty override, else the terminal" decidePrettyTests
        , testCase "the pretty table, color off (golden)" prettyGolden
        , testCase "the plain TSV is unchanged (golden)" tsvGolden
        , testCase "color on paints only stale/errored data rows, leaving borders/header/ok rows alone" prettyColorOn
        , testGroup "agents-exe bash-toolbox: describe/run" toolboxTests
        ]

-------------------------------------------------------------------------------
-- decidePretty: pretty-when-tty, TSV-when-not, either overridable

decidePrettyTests :: [TestTree]
decidePrettyTests =
    [ testCase "no override, terminal -> pretty" $ assertEqual "" True (decidePretty Nothing True)
    , testCase "no override, not a terminal -> TSV" $ assertEqual "" False (decidePretty Nothing False)
    , testCase "--pretty forces pretty even when piped" $ assertEqual "" True (decidePretty (Just True) False)
    , testCase "--pretty on a terminal stays pretty" $ assertEqual "" True (decidePretty (Just True) True)
    , testCase "--no-pretty forces TSV even on a terminal" $ assertEqual "" False (decidePretty (Just False) True)
    , testCase "--no-pretty stays TSV when piped too" $ assertEqual "" False (decidePretty (Just False) False)
    ]

-------------------------------------------------------------------------------
-- fixture rows: one converged with labels, one errored, one stale, one with no labels

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2024 1 1) 0

rowConvergedWithLabels :: Row
rowConvergedWithLabels =
    Row
        { rowHost = "host-a"
        , rowMode = "following"
        , rowLabels = [AppliedDocument "prod" "doc1" "abcdef0123456789abcdef0123456789" epoch]
        , rowConverged = 3
        , rowErrored = 0
        , rowNodes = 3
        , rowWritten = epoch
        , rowAge = 5
        , rowStale = False
        , rowFile = "/status/host-a.json"
        }

rowWithErrors :: Row
rowWithErrors =
    Row
        { rowHost = "host-b"
        , rowMode = "interactive"
        , rowLabels = []
        , rowConverged = 1
        , rowErrored = 2
        , rowNodes = 3
        , rowWritten = epoch
        , rowAge = 10
        , rowStale = False
        , rowFile = "/status/host-b.json"
        }

rowIsStale :: Row
rowIsStale =
    Row
        { rowHost = "host-c"
        , rowMode = "replay"
        , rowLabels = []
        , rowConverged = 1
        , rowErrored = 0
        , rowNodes = 1
        , rowWritten = epoch
        , rowAge = 3600
        , rowStale = True
        , rowFile = "/status/host-c.json"
        }

rowNoLabels :: Row
rowNoLabels =
    Row
        { rowHost = "host-d"
        , rowMode = "following"
        , rowLabels = []
        , rowConverged = 2
        , rowErrored = 0
        , rowNodes = 2
        , rowWritten = epoch
        , rowAge = 1
        , rowStale = False
        , rowFile = "/status/host-d.json"
        }

fixtureRows :: [Row]
fixtureRows = [rowConvergedWithLabels, rowWithErrors, rowIsStale, rowNoLabels]

-------------------------------------------------------------------------------

prettyGolden :: IO ()
prettyGolden =
    assertEqual "prettyTable, color off" expected (prettyTable False fixtureRows)
  where
    expected =
        Text.intercalate
            "\n"
            [ "┌────────┬─────────────┬────────────────────────┬───────────┬─────────┬───────┬───────┐"
            , "│ HOST   │ MODE        │ LABELS                 │ CONVERGED │ ERRORED │ AGE   │ STALE │"
            , "├────────┼─────────────┼────────────────────────┼───────────┼─────────┼───────┼───────┤"
            , "│ host-a │ following   │ prod=doc1@abcdef012345 │ 3/3       │ 0       │ 5s    │       │"
            , "│ host-b │ interactive │ -                      │ 1/3       │ 2       │ 10s   │       │"
            , "│ host-c │ replay      │ -                      │ 1/1       │ 0       │ 3600s │ STALE │"
            , "│ host-d │ following   │ -                      │ 2/2       │ 0       │ 1s    │       │"
            , "└────────┴─────────────┴────────────────────────┴───────────┴─────────┴───────┴───────┘"
            ]

tsvGolden :: IO ()
tsvGolden =
    assertEqual "renderHeader/renderRow (TSV)" expected actual
  where
    actual = Text.intercalate "\n" (renderHeader : fmap renderRow fixtureRows)
    expected =
        Text.intercalate
            "\n"
            [ "host\tmode\tlabels\tconverged\terrored\tage\tflags"
            , "host-a\tfollowing\tprod=doc1@abcdef012345\t3/3\t0\t5s\t"
            , "host-b\tinteractive\t-\t1/3\t2\t10s\t"
            , "host-c\treplay\t-\t1/1\t0\t3600s\tstale"
            , "host-d\tfollowing\t-\t2/2\t0\t1s\t"
            ]

{- | With color on, every line of a row that is stale or errored carries an
ANSI wrap around the /whole/ line (borders included — cheap and harmless,
since the point is a line painted, not a cell) and every other line —
borders, header, separator, an all-clear row — is untouched. Comparing
line-for-line against the color-off golden, rather than hardcoding escape
codes here too, is what keeps this test from re-encoding
'Fleet.colorizeRows'’s escape sequences by hand. -}
prettyColorOn :: IO ()
prettyColorOn = do
    let plainLines = Text.lines (prettyTable False fixtureRows)
        coloredLines = Text.lines (prettyTable True fixtureRows)
    assertEqual "same number of lines" (length plainLines) (length coloredLines)
    -- lines: 0 top border, 1 header, 2 separator, 3 host-a, 4 host-b, 5 host-c, 6 host-d, 7 bottom border
    let paintedAt i = "\ESC[" `Text.isPrefixOf` (coloredLines !! i) && "\ESC[0m" `Text.isSuffixOf` (coloredLines !! i)
        untouchedAt i = coloredLines !! i == plainLines !! i
    mapM_ (assertEqual "border/header/separator untouched" True . untouchedAt) [0, 1, 2, 7]
    assertEqual "host-a (converged, ok) untouched" True (untouchedAt 3)
    assertEqual "host-b (errored) painted" True (paintedAt 4)
    assertEqual "host-c (stale) painted" True (paintedAt 5)
    assertEqual "host-d (converged, ok) untouched" True (untouchedAt 6)

-------------------------------------------------------------------------------
-- the agents-exe bash-toolbox protocol (documentation/binary-tool.md,
-- projectz feature cf574966-31f3-408c-ac28-ab04ff93531c): `describe`'s JSON
-- shape, and `run`'s output against a real fixture directory.

toolboxTests :: [TestTree]
toolboxTests =
    [ testCase "describeValue has the required top-level fields" describeTopLevel
    , testCase "describeValue's args match dir/label/stale, with modes/arities from the current spec" describeArgs
    , testCase "describeValue round-trips through JSON encode/decode" describeRoundTrips
    , testCase "run DIR is the same JSON status --json would give" runMatchesStatusJson
    , testCase "run DIR --label/--stale filters and flags exactly like status" runWithFilters
    ]

-- | The top-level object the spec requires: @slug@, @description@, @args@
-- (all present, right shapes), plus the optional @empty-result@ this tool
-- declares.
describeTopLevel :: IO ()
describeTopLevel = case describeValue of
    Object o -> do
        assertField o "slug" $ \v -> case v of
            String s -> assertBool "slug has no spaces" (not (Text.any (== ' ') s))
            _ -> assertFailure "slug is not a string"
        assertField o "description" $ \v -> case v of
            String s -> assertBool "description is non-empty" (not (Text.null s))
            _ -> assertFailure "description is not a string"
        assertField o "args" $ \v -> case v of
            Array _ -> pure ()
            _ -> assertFailure "args is not an array"
        assertField o "empty-result" $ \v -> case v of
            Object er -> assertField er "tag" $ \tv -> case tv of
                String "AddMessage" -> pure ()
                _ -> assertFailure "empty-result.tag is not AddMessage"
            _ -> assertFailure "empty-result is not an object"
    _ -> assertFailure "describeValue is not a JSON object"
  where
    assertField o k f = case KeyMap.lookup k o of
        Nothing -> assertFailure ("missing field " <> show k)
        Just v -> f v

-- | Each arg object has exactly the fields
-- @documentation/binary-tool.md@ requires, and the three args this tool
-- declares (@dir@, @label@, @stale@) have the modes/arities that match how
-- @runP@ actually parses them: @dir@ positional/single, @label@ and
-- @stale@ dashdashspace/optional. @arity@ and @mode@ are checked against
-- the *closed* sets the current spec allows -- catching, in particular, any
-- future temptation to invent a "repeatable" arity that doesn't exist yet
-- (see the module haddock's noted gap for @--label@).
describeArgs :: IO ()
describeArgs = case describeValue of
    Object o -> case KeyMap.lookup "args" o of
        Just (Array args) -> do
            assertEqual "three args: dir, label, stale" 3 (length args)
            forM_ args checkArg
            namesAndModesMatch (foldr (:) [] args)
        _ -> assertFailure "args is not an array"
    _ -> assertFailure "describeValue is not a JSON object"
  where
    allowedArity = ["single", "optional"] :: [Text.Text]
    allowedMode = ["positional", "dashdashspace", "dashdashequal", "stdin"] :: [Text.Text]
    checkArg (Object a) = do
        forM_ ["name", "description", "type", "backing_type", "arity", "mode"] $ \k ->
            assertBool ("arg has field " <> show k) (KeyMap.member k a)
        case KeyMap.lookup "arity" a of
            Just (String s) -> assertBool ("arity is one of " <> show allowedArity) (s `elem` allowedArity)
            _ -> assertFailure "arity is not a string"
        case KeyMap.lookup "mode" a of
            Just (String s) -> assertBool ("mode is one of " <> show allowedMode) (s `elem` allowedMode)
            _ -> assertFailure "mode is not a string"
    checkArg _ = assertFailure "an arg is not a JSON object"
    namesAndModesMatch args = do
        let byName n = [a | Object a <- args, Just (String n') <- [KeyMap.lookup "name" a], n' == n]
        assertOne "dir" (byName "dir") "positional" "single"
        assertOne "label" (byName "label") "dashdashspace" "optional"
        assertOne "stale" (byName "stale") "dashdashspace" "optional"
    assertOne n found mode arity = case found of
        [a] -> do
            assertEqual (n <> " mode") (Just (String mode)) (KeyMap.lookup "mode" a)
            assertEqual (n <> " arity") (Just (String arity)) (KeyMap.lookup "arity" a)
        _ -> assertFailure ("expected exactly one arg named " <> n)

describeRoundTrips :: IO ()
describeRoundTrips =
    assertEqual "encode/decode is the identity" (Just describeValue) (decode (encode describeValue))

-- | @run DIR@ and @status DIR --json@ both go through 'foldStatusDir' — the
-- one place the fold happens — so there is structurally nothing for @run@
-- to drift from; what is worth testing is that the shared helper computes
-- what a real fixture directory says it should. The fixture's @written@/
-- @applied@ timestamps are pinned relative to 'getCurrentTime' (not a fixed
-- date) so @age_s@/@rowStale@ are meaningful without the test racing the
-- clock or two calls of 'foldStatusDir' (each of which samples the time
-- itself) disagreeing on it by a few microseconds.
fixtureDoc :: UTCTime -> UTCTime -> LByteString.ByteString
fixtureDoc written applied =
    "{\"salmon-status\":1,\"host\":\"host-a\",\"written\":"
        <> encode written
        <> ",\"mode\":\"following\","
        <> "\"labels\":[{\"label\":\"prod\",\"id\":\"doc1\",\"sha256\":\"abcdef0123456789abcdef0123456789\",\"applied\":"
        <> encode applied
        <> "}],"
        <> "\"status\":{\"nodes\":[{\"convergence\":\"converged\"},{\"convergence\":\"converged\"},{\"convergence\":\"errored\"}]}}"

-- | A fixture directory with one host-a.json, written 5s ago.
withFixtureDir :: (FilePath -> IO a) -> IO a
withFixtureDir act = withSystemTempDirectory "salmon-fleet-toolbox-test" $ \dir -> do
    createDirectoryIfMissing True dir
    now <- getCurrentTime
    let written = addUTCTime (-5) now
    LByteString.writeFile (dir </> "host-a.json") (fixtureDoc written written)
    act dir

runMatchesStatusJson :: IO ()
runMatchesStatusJson = withFixtureDir $ \dir -> do
    -- `run DIR` (no --label/--stale) is `foldStatusDir dir Nothing 60` --
    -- exactly the defaults `runP` gives when neither flag is passed, and
    -- exactly what `Status` takes for `status DIR --json`.
    (_, runRows, rejected) <- foldStatusDir dir Nothing 60
    assertEqual "nothing rejected" [] rejected
    assertEqual "one row, for host-a" 1 (length runRows)
    case runRows of
        [row] -> do
            assertEqual "host" "host-a" row.rowHost
            assertEqual "mode" "following" row.rowMode
            assertEqual "converged/total" (2, 3) (row.rowConverged, row.rowNodes)
            assertEqual "errored" 1 row.rowErrored
            assertEqual "not stale (written 5s ago, default 60s threshold)" False row.rowStale
            assertEqual "one label" 1 (length row.rowLabels)
            -- the JSON `run` actually prints, via the same rowValue `status
            -- --json` uses
            case rowValue row of
                Object o -> forM_ ["host", "mode", "labels", "converged", "errored", "nodes", "written", "age_s", "stale", "file"] $ \k ->
                    assertBool ("rowValue has field " <> show k) (KeyMap.member k o)
                _ -> assertFailure "rowValue is not a JSON object"
        _ -> assertFailure "expected exactly one row"

runWithFilters :: IO ()
runWithFilters = withFixtureDir $ \dir -> do
    (_, matched, _) <- foldStatusDir dir (Just "prod") 60
    assertEqual "matching label keeps the row" 1 (length matched)
    (_, unmatched, _) <- foldStatusDir dir (Just "nope") 60
    assertEqual "non-matching label drops the row" 0 (length unmatched)
    (_, stale, _) <- foldStatusDir dir Nothing 1
    case stale of
        [row] -> assertEqual "a tight --stale (1s, doc written 5s ago) flags the row" True row.rowStale
        _ -> assertFailure "expected exactly one row"
