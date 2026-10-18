{- | Layer 0 coverage for "Salmon.Reporter.UpDownText": the one-line text
reporter for @run up@\/@run down@. A golden line per 'UpDown.Report'
constructor, QuickCheck properties on 'renderReport' (one line whatever the
help or the failure text holds; the kind is the JSON one; the node's short
ref is on it), and a real 'UpDown.upTree' over three nodes printing one line
per report.
-}
module Test.UpDownTextSpec (tests) where

import Control.Exception (ErrorCall (..), toException)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isControl)
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (IOMode (..), hClose, withFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, forAll, frequency, listOf, oneof, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..), Op, deps, nodeps, op)
import Salmon.Op.Actions (Act (..), Actions (..))
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref, mkRef, shortRef)
import Salmon.Reporter.Tagged ()
import Salmon.Reporter.UpDownText (renderKind, renderReport, reportHText)

tests :: TestTree
tests =
    testGroup
        "Salmon.Reporter.UpDownText"
        [ testGroup "golden line, one per constructor" [testCase name (assertEqual name expected (renderReport rep)) | (name, rep, expected) <- goldens]
        , testProperty "a report renders as one line" propOneLine
        , testProperty "the line starts with the report's JSON kind" propKindFirst
        , testProperty "the line names the node by #shortRef" propShortRef
        , testProperty "help without control characters is printed verbatim" propHelpVerbatim
        , testCase "run up over three nodes prints one line per report, none of them a Show" threeNodes
        ]

-------------------------------------------------------------------------------

fixtureRef :: Ref
fixtureRef = mkRef "updown-text-spec" ("fixture" :: Text)

actWith :: Text -> Text -> Ref -> Act Extension
actWith shorthand h r = actOf $ op shorthand nodeps $ \ext -> ext{help = h, ref = r}

actOf :: Op -> Act Extension
actOf o = case o.node of
    Actions act -> act
    Actionless -> error "fixture op is Actionless"

fixtureAct :: Act Extension
fixtureAct = actWith "file" "write /etc/motd" fixtureRef

otherAct :: Act Extension
otherAct = actWith "file" "write /etc/motd\nsecond line" fixtureRef

goldens :: [(String, UpDown.Report Extension, Text)]
goldens =
    [ ("Skip", UpDown.Skip fixtureAct, "skip    " <> short <> " file: write /etc/motd")
    , ("Eval", UpDown.Eval fixtureAct, "eval    " <> short <> " file: write /etc/motd")
    , ("Done", UpDown.Done fixtureAct, "done    " <> short <> " file: write /etc/motd")
    , ( "Failed"
      , UpDown.Failed fixtureAct (toException (ErrorCall "exit 1\nstderr: no space left"))
      , "failed  " <> short <> " file: write /etc/motd -- exit 1\\nstderr: no space left"
      )
    , ("Blocked", UpDown.Blocked fixtureAct, "blocked " <> short <> " file: write /etc/motd")
    , ( "Conflicting"
      , UpDown.Conflicting fixtureRef fixtureAct otherAct
      , "conflicting " <> short <> " kept file: write /etc/motd; replaced file: write /etc/motd\\nsecond line"
      )
    , ("Instructed", UpDown.Instructed fixtureAct Mailbox.Force, "instructed " <> short <> " file: write /etc/motd -- force")
    , ("DroppedInstructions", UpDown.DroppedInstructions fixtureAct 3, "dropped-instructions " <> short <> " file: write /etc/motd -- 3 dropped")
    ]
  where
    short = "#" <> shortRef fixtureRef

-------------------------------------------------------------------------------

-- | Text that often holds the characters that would break a line.
genText :: Gen Text
genText = Text.pack <$> listOf (frequency [(6, arbitrary), (1, elements "\n\r\ESC\t\v\f\0")])

genAct :: Gen (Act Extension)
genAct = do
    sh <- genText
    h <- genText
    k <- choose (0, 1000 :: Int)
    pure (actWith sh h (mkRef "updown-text-spec" k))

genReport :: Gen (UpDown.Report Extension)
genReport =
    oneof
        [ UpDown.Skip <$> genAct
        , UpDown.Eval <$> genAct
        , UpDown.Done <$> genAct
        , UpDown.Failed <$> genAct <*> (toException . ErrorCall . Text.unpack <$> genText)
        , UpDown.Blocked <$> genAct
        , (\a b -> UpDown.Conflicting a.extension.ref a b) <$> genAct <*> genAct
        , UpDown.Instructed <$> genAct <*> elements [Mailbox.Force, Mailbox.Satisfy, Mailbox.Recheck, Mailbox.Pause, Mailbox.Resume]
        , UpDown.DroppedInstructions <$> genAct <*> choose (0, 100)
        ]

reportRef :: UpDown.Report Extension -> Ref
reportRef rep = case rep of
    UpDown.Skip a -> a.extension.ref
    UpDown.Eval a -> a.extension.ref
    UpDown.Done a -> a.extension.ref
    UpDown.Failed a _ -> a.extension.ref
    UpDown.Blocked a -> a.extension.ref
    UpDown.Conflicting r _ _ -> r
    UpDown.Instructed a _ -> a.extension.ref
    UpDown.DroppedInstructions a _ -> a.extension.ref

propOneLine :: Property
propOneLine = forAll genReport $ \rep ->
    let line = renderReport rep
     in counterexample (show line) (Text.all (\c -> not (isControl c) || c == '\t') line)

propKindFirst :: Property
propKindFirst = forAll genReport $ \rep ->
    let jsonKind = case toJSON rep of
            Object o -> KeyMap.lookup "kind" o
            _ -> Nothing
     in (Just (String (renderKind rep)), take 1 (Text.words (renderReport rep)))
            === (jsonKind, [renderKind rep])

propShortRef :: Property
propShortRef = forAll genReport $ \rep ->
    let ws = Text.words (renderReport rep)
     in take 1 (drop 1 ws) === ["#" <> shortRef (reportRef rep)]

propHelpVerbatim :: Property
propHelpVerbatim = forAll ((,) <$> genText <*> genText) $ \(sh, h) ->
    let clean = Text.filter (not . isControl)
        act = actWith (clean sh) (clean h) fixtureRef
     in Text.isSuffixOf (" " <> clean sh <> ": " <> clean h) (renderReport (UpDown.Done act)) === True

-------------------------------------------------------------------------------

-- | The acceptance from the feature: three nodes, one line per report.
threeNodes :: IO ()
threeNodes = withSystemTempDirectory "updown-text" $ \dir -> do
    let path = dir <> "/out"
        leaf :: Text -> Op
        leaf n = op "leaf" nodeps $ \ext -> ext{help = "leaf " <> n, ref = mkRef "updown-text-spec" n, up = pure ()}
        root :: Op
        root = op "root" (deps [leaf "a", leaf "b"]) $ \ext -> ext{help = "the root", ref = mkRef "updown-text-spec" ("root" :: Text), up = pure ()}
    ok <- withFile path WriteMode $ \h -> do
        r <- UpDown.upTree (reportHText h) (pure . runIdentity) root
        hClose h
        pure r
    assertBool "upTree succeeded" ok
    out <- Text.readFile path
    let ls = Text.lines out
    assertEqual "two reports (eval, done) per node, one line each" 6 (length ls)
    assertEqual "eval/done in order per node" ["eval", "done", "eval", "done", "eval", "done"] (concatMap (take 1 . Text.words) ls)
    assertBool "the root's done is last" ("done" `Text.isPrefixOf` last ls && "root: the root" `Text.isSuffixOf` last ls)
    assertBool "no Show of Act or Extension" (not (any (\l -> "Act {" `Text.isInfixOf` l || "extension =" `Text.isInfixOf` l) ls))
