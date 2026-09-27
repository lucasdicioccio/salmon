{- | Layer 0 coverage for @salmon-fleet status@'s @--pretty@ table
(projectz feature 962ca8a9-48c7-4582-854f-e4581fdccc60): 'Fleet.decidePretty'
truth table, a golden of the pretty table (color forced off, so the golden
is stable across runs/terminals), and a golden of the plain TSV
('Salmon.Actions.Fleet.renderHeader'\/'renderRow', untouched by this
feature) — pinning "the default did not change" for existing pipe/script
consumers.
-}
module Test.FleetSpec (tests) where

import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Fleet (decidePretty, prettyTable)
import Salmon.Actions.Fleet (Row (..), renderHeader, renderRow)
import Salmon.Actions.Serve (AppliedDocument (..))

tests :: TestTree
tests =
    testGroup
        "Fleet"
        [ testGroup "decidePretty: --pretty/--no-pretty override, else the terminal" decidePrettyTests
        , testCase "the pretty table, color off (golden)" prettyGolden
        , testCase "the plain TSV is unchanged (golden)" tsvGolden
        , testCase "color on paints only stale/errored data rows, leaving borders/header/ok rows alone" prettyColorOn
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
