{- | Layer 0 coverage for "Salmon.Reporter.Tagged" (milestone 1 of
@specs\/generic-server.md@): the JSON encoding of the three report streams,
and the composition of the text reporters beside a JSON one.

Every constructor of 'UpDown.Report', 'Upkeep.Report' and 'Serve.Report' has
a golden object here, written out as JSON text and compared structurally
(key order is not part of the contract; the set of keys and their values
are). The one thing a golden cannot spell out literally is a 'Ref' — one is
only ever made by hashing — so each golden carries @<REF>@\/@<SHORT>@
placeholders spliced from the one fixture ref before parsing. A constructor
added to any of the three streams is an incomplete-pattern warning in the
sentinels at the bottom of this module, which is the cue to add its golden.
-}
module Test.ReportJsonSpec (tests) where

import Control.Exception (ErrorCall (..), toException)
import Data.Aeson (Value (..), eitherDecode, encode, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Extension (..), Op, nodeps, op)
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.OpGraph (OpGraph (..))
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.Ref (Ref, mkRef, shortRef, unRef)
import qualified Salmon.Op.Status as Status
import Salmon.Op.Supervision (Micros (..), Restart (..), Strategy (..), Supervision (..), defaultSupervision)
import Salmon.Reporter (ReporterM (..), reportBoth, runReporter, silent)
import qualified Salmon.Reporter.Tagged as Tagged

tests :: TestTree
tests =
    testGroup
        "Salmon.Reporter.Tagged"
        [ testGroup "golden JSON, one per constructor" [testCase name (golden rep expected) | (name, rep, expected) <- goldens]
        , testCase "Tagged adds a stream to the inner object and nothing else" taggedOrigin
        , testCase "a Ref encodes as its short tag and its full text" refShape
        , testCase "a Mode encodes as the word status prints" modeShape
        , testCase "the text reporters print the same lines beside a JSON one as they do alone" textUnchangedBesideJson
        , testCase "reportJSONLines writes one object per line" oneObjectPerLine
        ]

-------------------------------------------------------------------------------

-- | The one node every golden is about.
fixtureRef :: Ref
fixtureRef = mkRef "report-json-spec" ("fixture" :: Text)

-- | A second ref, for the reports that mention two.
otherRef :: Ref
otherRef = mkRef "report-json-spec" ("other" :: Text)

fixtureAct :: Act Extension
fixtureAct = actOf fixtureOp
  where
    fixtureOp :: Op
    fixtureOp = op "fixture-node" nodeps $ \ext ->
        ext
            { help = "a node for the goldens"
            , notes = ["first note", "second note"]
            , ref = fixtureRef
            }

otherAct :: Act Extension
otherAct = actOf otherOp
  where
    otherOp :: Op
    otherOp = op "other-node" nodeps $ \ext ->
        ext{help = "the other declaration", notes = [], ref = fixtureRef}

actOf :: Op -> Act Extension
actOf o = case o.node of
    Actions act -> act
    Actionless -> error "fixture op is Actionless"

boom :: ErrorCall
boom = ErrorCall "boom"

-- | The JSON text of the fixture node, as every per-node report carries it.
nodeJson :: Text
nodeJson = "{\"shorthand\":\"fixture-node\",\"help\":\"a node for the goldens\",\"notes\":[\"first note\",\"second note\"]}"

otherNodeJson :: Text
otherNodeJson = "{\"shorthand\":\"other-node\",\"help\":\"the other declaration\",\"notes\":[]}"

refJson :: Text
refJson = "{\"short\":\"<SHORT>\",\"full\":\"<REF>\"}"

otherRefJson :: Text
otherRefJson = "{\"short\":\"<OSHORT>\",\"full\":\"<OREF>\"}"

-- | @ref@ and @node@, the pair every report about one node opens with.
about :: Text
about = "\"ref\":" <> refJson <> ",\"node\":" <> nodeJson

splice :: Text -> Text
splice =
    Text.replace "<SHORT>" (shortRef fixtureRef)
        . Text.replace "<REF>" (unRef fixtureRef)
        . Text.replace "<OSHORT>" (shortRef otherRef)
        . Text.replace "<OREF>" (unRef otherRef)

golden :: Tagged.Tagged -> Text -> Assertion
golden tagged expectedText = do
    expected <- case eitherDecode (LText.encodeUtf8 (LText.fromStrict (splice expectedText))) of
        Left err -> assertFailure ("golden is not valid JSON: " <> err <> "\n" <> Text.unpack expectedText)
        Right v -> pure (v :: Value)
    let actual = stripOrigin (toJSON tagged)
    assertEqual ("encoding of " <> show tagged <> "\n  as " <> LChar8.unpack (encode actual)) expected actual
  where
    -- the goldens are about each stream's own object; 'taggedOrigin' covers
    -- what 'Tagged' adds on top.
    stripOrigin (Object o) = Object (KeyMap.delete "stream" o)
    stripOrigin v = v

-------------------------------------------------------------------------------

goldens :: [(String, Tagged.Tagged, Text)]
goldens = updownGoldens ++ upkeepGoldens ++ serveGoldens

updownGoldens :: [(String, Tagged.Tagged, Text)]
updownGoldens =
    [ ("UpDown.Skip", Tagged.FromUpDown (UpDown.Skip fixtureAct), "{\"kind\":\"skip\"," <> about <> "}")
    , ("UpDown.Eval", Tagged.FromUpDown (UpDown.Eval fixtureAct), "{\"kind\":\"eval\"," <> about <> "}")
    , ("UpDown.Done", Tagged.FromUpDown (UpDown.Done fixtureAct), "{\"kind\":\"done\"," <> about <> "}")
    , ("UpDown.Failed", Tagged.FromUpDown (UpDown.Failed fixtureAct (toException boom)), "{\"kind\":\"failed\"," <> about <> ",\"error\":\"boom\"}")
    , ("UpDown.Blocked", Tagged.FromUpDown (UpDown.Blocked fixtureAct), "{\"kind\":\"blocked\"," <> about <> "}")
    ,
        ( "UpDown.Conflicting"
        , Tagged.FromUpDown (UpDown.Conflicting fixtureRef fixtureAct otherAct)
        , "{\"kind\":\"conflicting\",\"ref\":" <> refJson <> ",\"kept\":" <> nodeJson <> ",\"replaced\":" <> otherNodeJson <> "}"
        )
    , ("UpDown.Instructed", Tagged.FromUpDown (UpDown.Instructed fixtureAct Mailbox.Force), "{\"kind\":\"instructed\"," <> about <> ",\"instruction\":\"force\"}")
    , ("UpDown.DroppedInstructions", Tagged.FromUpDown (UpDown.DroppedInstructions fixtureAct 3), "{\"kind\":\"dropped-instructions\"," <> about <> ",\"dropped\":3}")
    ]

upkeepGoldens :: [(String, Tagged.Tagged, Text)]
upkeepGoldens =
    [ ("Upkeep.Acted", Tagged.FromUpkeep (Upkeep.Acted (UpDown.Done fixtureAct)), "{\"kind\":\"acted\",\"report\":{\"kind\":\"done\"," <> about <> "}}")
    , ("Upkeep.Upkeep", Tagged.FromUpkeep (Upkeep.Upkeep fixtureAct Upkeep.WaitUp), "{\"kind\":\"upkeep\"," <> about <> ",\"state\":\"wait-up\"}")
    , ("Upkeep.Downkeep", Tagged.FromUpkeep (Upkeep.Downkeep fixtureAct Upkeep.Downing), "{\"kind\":\"downkeep\"," <> about <> ",\"state\":\"downing\"}")
    ,
        ( "Upkeep.NextLook"
        , Tagged.FromUpkeep (Upkeep.NextLook fixtureAct (UpDown.Failure "gone") (Micros 500000))
        , "{\"kind\":\"next-look\"," <> about <> ",\"check\":{\"verdict\":\"failure\",\"reason\":\"gone\"},\"delay_us\":500000}"
        )
    , ("Upkeep.Wedged", Tagged.FromUpkeep (Upkeep.Wedged fixtureAct (Micros 30000000)), "{\"kind\":\"wedged\"," <> about <> ",\"silent_us\":30000000}")
    , ("Upkeep.Unwedged", Tagged.FromUpkeep (Upkeep.Unwedged fixtureAct), "{\"kind\":\"unwedged\"," <> about <> "}")
    , ("Upkeep.Demoted", Tagged.FromUpkeep (Upkeep.Demoted fixtureAct otherRef), "{\"kind\":\"demoted\"," <> about <> ",\"dependency\":" <> otherRefJson <> "}")
    , ("Upkeep.Parked", Tagged.FromUpkeep (Upkeep.Parked fixtureAct), "{\"kind\":\"parked\"," <> about <> "}")
    , ("Upkeep.Reapplying", Tagged.FromUpkeep (Upkeep.Reapplying fixtureAct (Micros 1000)), "{\"kind\":\"reapplying\"," <> about <> ",\"delay_us\":1000}")
    , ("Upkeep.Paused", Tagged.FromUpkeep (Upkeep.Paused fixtureAct), "{\"kind\":\"paused\"," <> about <> "}")
    , ("Upkeep.Resumed", Tagged.FromUpkeep (Upkeep.Resumed fixtureAct), "{\"kind\":\"resumed\"," <> about <> "}")
    , ("Upkeep.GaveUp", Tagged.FromUpkeep (Upkeep.GaveUp fixtureAct 5), "{\"kind\":\"gave-up\"," <> about <> ",\"failures\":5}")
    , ("Upkeep.Adopted", Tagged.FromUpkeep (Upkeep.Adopted fixtureAct), "{\"kind\":\"adopted\"," <> about <> "}")
    , ("Upkeep.Released", Tagged.FromUpkeep (Upkeep.Released fixtureAct), "{\"kind\":\"released\"," <> about <> "}")
    ,
        ( "Upkeep.Policy"
        , Tagged.FromUpkeep (Upkeep.Policy fixtureAct defaultSupervision [restForOne])
        , "{\"kind\":\"policy\","
            <> about
            <> ",\"supervision\":{\"restart\":\"on-failure\",\"strategy\":\"one-for-one\",\"reapply\":false,\"watchdog_us\":null,\"stable_after_us\":10000000,\"demote_every_us\":10000000,\"give_up_after\":null}"
            <> ",\"ignored\":[{\"restart\":\"always\",\"strategy\":\"rest-for-one\",\"reapply\":true,\"watchdog_us\":2000000,\"stable_after_us\":10000000,\"demote_every_us\":10000000,\"give_up_after\":4}]}"
        )
    , ("Upkeep.Untended", Tagged.FromUpkeep (Upkeep.Untended fixtureAct), "{\"kind\":\"untended\"," <> about <> "}")
    , ("Upkeep.Escaped", Tagged.FromUpkeep (Upkeep.Escaped fixtureAct (toException boom)), "{\"kind\":\"escaped\"," <> about <> ",\"error\":\"boom\"}")
    , ("Upkeep.Supervising", Tagged.FromUpkeep (Upkeep.Supervising 7 2), "{\"kind\":\"supervising\",\"up\":7,\"down\":2}")
    , ("Upkeep.Retired", Tagged.FromUpkeep (Upkeep.Retired 9), "{\"kind\":\"retired\",\"machines\":9}")
    , ("Upkeep.Holding", Tagged.FromUpkeep (Upkeep.Holding 1), "{\"kind\":\"holding\",\"machines\":1}")
    ]
  where
    restForOne =
        defaultSupervision
            { supRestart = Always
            , supStrategy = RestForOne
            , supReapply = True
            , supWatchdog = Just (Micros 2000000)
            , supGiveUpAfter = Just 4
            }

serveGoldens :: [(String, Tagged.Tagged, Text)]
serveGoldens =
    [ ("Serve.Started", Tagged.FromServe Serve.Started, "{\"kind\":\"started\"}")
    , ("Serve.Stopped", Tagged.FromServe Serve.Stopped, "{\"kind\":\"stopped\"}")
    , ("Serve.HungUp", Tagged.FromServe (Serve.HungUp (Serve.Origin "/tmp/x.sock#0")), "{\"kind\":\"hung-up\",\"from\":\"/tmp/x.sock#0\"}")
    , ("Serve.BadCommand", Tagged.FromServe (Serve.BadCommand "unknown command"), "{\"kind\":\"bad-command\",\"error\":\"unknown command\"}")
    , ("Serve.BadSeed", Tagged.FromServe (Serve.BadSeed "missing --dir"), "{\"kind\":\"bad-seed\",\"error\":\"missing --dir\"}")
    , ("Serve.BadDirective", Tagged.FromServe (Serve.BadDirective "not json"), "{\"kind\":\"bad-directive\",\"error\":\"not json\"}")
    , ("Serve.BadLoad", Tagged.FromServe (Serve.BadLoad "nesting too deep"), "{\"kind\":\"bad-load\",\"error\":\"nesting too deep\"}")
    , ("Serve.Loading", Tagged.FromServe (Serve.Loading "/tmp/script"), "{\"kind\":\"loading\",\"path\":\"/tmp/script\"}")
    , ("Serve.LoadDone", Tagged.FromServe (Serve.LoadDone "/tmp/script" 4), "{\"kind\":\"load-done\",\"path\":\"/tmp/script\",\"lines\":4}")
    ,
        ( "Serve.Declared"
        , Tagged.FromServe (Serve.Declared (Serve.EpochId 3) Status.TurnUp 12 2)
        , "{\"kind\":\"declared\",\"epoch\":3,\"direction\":\"up\",\"nodes\":12,\"active_seeds\":2}"
        )
    , ("Serve.Cleared", Tagged.FromServe (Serve.Cleared 2), "{\"kind\":\"cleared\",\"retired\":2}")
    , ("Serve.Supervised", Tagged.FromServe (Serve.Supervised False), "{\"kind\":\"supervised\",\"on\":false}")
    , ("Serve.AutoConverged", Tagged.FromServe (Serve.AutoConverged True), "{\"kind\":\"auto-converged\",\"on\":true}")
    , ("Serve.Instructed", Tagged.FromServe (Serve.Instructed Mailbox.Recheck 3), "{\"kind\":\"instructed\",\"instruction\":\"recheck\",\"nodes\":3}")
    , ("Serve.FetchRequested", Tagged.FromServe (Serve.FetchRequested False), "{\"kind\":\"fetch-requested\",\"following\":false}")
    ,
        ( "Serve.Tended"
        , Tagged.FromServe (Serve.Tended (Upkeep.Acted (UpDown.Eval fixtureAct)))
        , "{\"kind\":\"tended\",\"report\":{\"kind\":\"acted\",\"report\":{\"kind\":\"eval\"," <> about <> "}}}"
        )
    , ("Serve.ConvergeStart", Tagged.FromServe (Serve.ConvergeStart 1 4), "{\"kind\":\"converge-start\",\"down\":1,\"up\":4}")
    , ("Serve.ConvergeStop", Tagged.FromServe (Serve.ConvergeStop False 2), "{\"kind\":\"converge-stop\",\"ok\":false,\"remaining\":2}")
    ,
        ( "Serve.StatusReport"
        , Tagged.FromServe (Serve.StatusReport Serve.Interactive [(fixtureRef, tendedState), (otherRef, untendedState)] paths)
        , "{\"kind\":\"status\",\"mode\":\"interactive\",\"nodes\":[" <> tendedJson <> "," <> untendedJson <> "]}"
        )
    , ("Serve.StatusReport (following)", Tagged.FromServe (Serve.StatusReport Serve.Following [] Map.empty), "{\"kind\":\"status\",\"mode\":\"following\",\"nodes\":[]}")
    , ("Serve.StatusReport (replay)", Tagged.FromServe (Serve.StatusReport Serve.Replay [] Map.empty), "{\"kind\":\"status\",\"mode\":\"replay\",\"nodes\":[]}")
    ,
        ( "Serve.HistoryReport"
        , Tagged.FromServe
            ( Serve.HistoryReport
                [ (Serve.EpochId 1, Serve.Add, True, Serve.Stdin, ["--dir", "/tmp/play"])
                , (Serve.EpochId 2, Serve.Remove, False, Serve.Loaded "/tmp/script", [])
                , (Serve.EpochId 3, Serve.Add, True, Serve.Fetched (Serve.Provenance "/srv/reg" "web-api" "web-api@2026-09-23T10:41:07Z" "32ea59311d97"), ["--name", "web"])
                ]
            )
        , "{\"kind\":\"history\",\"seeds\":["
            <> "{\"epoch\":1,\"declaration\":\"up\",\"active\":true,\"origin\":{\"kind\":\"stdin\"},\"args\":[\"--dir\",\"/tmp/play\"]},"
            <> "{\"epoch\":2,\"declaration\":\"down\",\"active\":false,\"origin\":{\"kind\":\"loaded\",\"path\":\"/tmp/script\"},\"args\":[]},"
            <> "{\"epoch\":3,\"declaration\":\"up\",\"active\":true,\"origin\":{\"kind\":\"fetched\",\"registry\":\"/srv/reg\",\"label\":\"web-api\",\"document\":\"web-api@2026-09-23T10:41:07Z\",\"sha256\":\"32ea59311d97\"},\"args\":[\"--name\",\"web\"]}]}"
        )
    , ("Serve.HistoryElided", Tagged.FromServe (Serve.HistoryElided 40), "{\"kind\":\"history-elided\",\"elided\":40}")
    ,
        ( "Serve.QueryReport"
        , Tagged.FromServe (Serve.QueryReport [(fixtureRef, tendedState), (otherRef, untendedState)] (Set.singleton fixtureRef) (Set.singleton otherRef) paths)
        , "{\"kind\":\"query\",\"nodes\":["
            <> Text.init tendedJson
            <> ",\"selected\":true,\"excluded\":false},"
            <> Text.init untendedJson
            <> ",\"selected\":false,\"excluded\":true}]}"
        )
    ,
        ( "Serve.HelpText"
        , Tagged.FromServe (Serve.HelpText (Just "no-such-topic"))
        , "{\"kind\":\"help\",\"topic\":\"no-such-topic\",\"lines\":" <> jsonStrings (Serve.renderReport (Serve.HelpText (Just "no-such-topic"))) <> "}"
        )
    ]
  where
    paths = Map.fromList [(fixtureRef, ["/program/fixture-node", "/other/fixture-node"])]
    tendedState =
        Serve.NodeState
            { Serve.nodeShorthand = "fixture-node"
            , Serve.nodeHelp = "a node for the goldens"
            , Serve.nodeDirection = Status.TurnUp
            , Serve.nodeConvergence = Serve.Converged
            , Serve.nodeStatus =
                Just
                    Status.Status
                        { Status.statusCheck = UpDown.Success
                        , Status.statusDirection = Status.TurnUp
                        , Status.statusStability = Status.Stable
                        , Status.statusLastActive = 123456789
                        , Status.statusEpoch = 2
                        , Status.statusOutput = Status.pushRing "second line" (Status.pushRing "first line" Status.emptyRing)
                        }
            }
    untendedState =
        Serve.NodeState
            { Serve.nodeShorthand = "other-node"
            , Serve.nodeHelp = "the other declaration"
            , Serve.nodeDirection = Status.TurnDown
            , Serve.nodeConvergence = Serve.Stale
            , Serve.nodeStatus = Nothing
            }
    tendedJson =
        "{\"ref\":"
            <> refJson
            <> ",\"shorthand\":\"fixture-node\",\"help\":\"a node for the goldens\",\"direction\":\"up\",\"convergence\":\"converged\""
            <> ",\"status\":{\"check\":{\"verdict\":\"success\"},\"direction\":\"up\",\"stability\":\"stable\",\"epoch\":2,\"output\":[\"first line\",\"second line\"]}"
            <> ",\"paths\":[\"/program/fixture-node\",\"/other/fixture-node\"]}"
    untendedJson =
        "{\"ref\":"
            <> otherRefJson
            <> ",\"shorthand\":\"other-node\",\"help\":\"the other declaration\",\"direction\":\"down\",\"convergence\":\"stale\",\"status\":null,\"paths\":[]}"
    jsonStrings :: [Text] -> Text
    jsonStrings = LText.toStrict . LText.decodeUtf8 . encode

-------------------------------------------------------------------------------

taggedOrigin :: Assertion
taggedOrigin = do
    assertEqual "serve" (Just (String "serve")) (originOf (Tagged.FromServe Serve.Started))
    assertEqual "updown" (Just (String "updown")) (originOf (Tagged.FromUpDown (UpDown.Done fixtureAct)))
    assertEqual "upkeep" (Just (String "upkeep")) (originOf (Tagged.FromUpkeep (Upkeep.Holding 1)))
    -- the inner object is carried whole: removing the stream gives it back
    let inner = toJSON (UpDown.Done fixtureAct)
    case toJSON (Tagged.FromUpDown (UpDown.Done fixtureAct)) of
        Object o -> assertEqual "inner object, untouched" inner (Object (KeyMap.delete "stream" o))
        v -> assertFailure ("not an object: " <> show v)
  where
    originOf tagged = case toJSON tagged of
        Object o -> KeyMap.lookup "stream" o
        _ -> Nothing

refShape :: Assertion
refShape = do
    case Tagged.refValue fixtureRef of
        Object o -> do
            assertEqual "short" (Just (String (shortRef fixtureRef))) (KeyMap.lookup "short" o)
            assertEqual "full" (Just (String (unRef fixtureRef))) (KeyMap.lookup "full" o)
            assertEqual "two keys and no more" 2 (KeyMap.size o)
        v -> assertFailure ("not an object: " <> show v)
    assertBool "the short tag is a prefix-searchable 8 characters" (Text.length (shortRef fixtureRef) == 8)

{- | 'Serve.Mode' is on the wire twice (@status@'s object, @\/dag@'s
envelope) through one instance; this is its golden, and what it must keep
saying for either.
-}
modeShape :: Assertion
modeShape = do
    assertEqual "interactive" (String "interactive") (toJSON Serve.Interactive)
    assertEqual "following" (String "following") (toJSON Serve.Following)
    assertEqual "replay" (String "replay") (toJSON Serve.Replay)
    assertEqual "encoded as its rendering" (LText.encodeUtf8 (LText.fromStrict ("\"" <> Serve.renderMode Serve.Replay <> "\""))) (encode Serve.Replay)

{- | The composition "Salmon.Builtin.CommandLine" would make if it ever ran
both: the three text reporters behind one 'Tagged' reporter, 'reportBoth'
a JSON one. The text side must print exactly what the three print on their
own — a report is dispatched, never reshaped — and the JSON side must see
every report the text side did.
-}
textUnchangedBesideJson :: Assertion
textUnchangedBesideJson = do
    aloneServe <- newIORef []
    aloneUpdown <- newIORef []
    besideServe <- newIORef []
    besideUpdown <- newIORef []
    jsonSeen <- newIORef []
    let serveText ref = ReporterM $ \rep -> modifyIORef' ref (++ Serve.renderReport rep)
        updownText ref = ReporterM $ \rep -> modifyIORef' ref (++ [Text.pack (show rep)])
        jsonR = ReporterM $ \tagged -> modifyIORef' jsonSeen (++ [encode tagged])
        composed = reportBoth (Tagged.reportTexts (serveText besideServe) (updownText besideUpdown) silent) jsonR
        serveReports = [Serve.Started, Serve.ConvergeStart 0 2, Serve.Tended (Upkeep.Wedged fixtureAct (Micros 5)), Serve.ConvergeStop True 0]
        updownReports = [UpDown.Eval fixtureAct, UpDown.Done fixtureAct, UpDown.Failed otherAct (toException boom)]
    mapM_ (runReporter (serveText aloneServe)) serveReports
    mapM_ (runReporter (updownText aloneUpdown)) updownReports
    mapM_ (runReporter (Tagged.serveStream composed)) serveReports
    mapM_ (runReporter (Tagged.updownStream composed)) updownReports
    expectedServe <- readIORef aloneServe
    expectedUpdown <- readIORef aloneUpdown
    actualServe <- readIORef besideServe
    actualUpdown <- readIORef besideUpdown
    assertEqual "serve text, beside JSON" expectedServe actualServe
    assertEqual "updown text, beside JSON" expectedUpdown actualUpdown
    assertBool "the text reporters actually printed something" (not (null expectedServe) && not (null expectedUpdown))
    seen <- readIORef jsonSeen
    assertEqual "every report reached the JSON side" (length serveReports + length updownReports) (length seen)

oneObjectPerLine :: Assertion
oneObjectPerLine =
    withSystemTempFile "reports.jsonl" $ \path h -> do
        let r = Tagged.reportJSONLines h
        runReporter r (Tagged.FromUpDown (UpDown.Eval fixtureAct))
        runReporter r (Tagged.FromUpDown (UpDown.Failed fixtureAct (toException (ErrorCall "multi\nline\nerror"))))
        runReporter r (Tagged.FromServe (Serve.HelpText Nothing))
        hClose h
        contents <- LByteString.readFile path
        let ls = LChar8.lines contents
        assertEqual "three reports, three lines" 3 (length ls)
        assertBool "the file ends with a newline" (LChar8.isSuffixOf "\n" contents)
        mapM_ decodesToTaggedObject ls
  where
    decodesToTaggedObject line =
        case eitherDecode line of
            Right (Object o) -> assertBool "has a stream" (KeyMap.member "stream" o)
            Right v -> assertFailure ("not an object: " <> show v)
            Left err -> assertFailure ("not a JSON line: " <> err <> ": " <> LChar8.unpack line)

-------------------------------------------------------------------------------

{- | Exhaustiveness sentinels: a constructor added to a stream shows up here
as an incomplete-pattern warning, which is the cue to add its golden above.
Never called.
-}
_updownCovered :: UpDown.Report Extension -> ()
_updownCovered rep = case rep of
    UpDown.Skip{} -> ()
    UpDown.Eval{} -> ()
    UpDown.Done{} -> ()
    UpDown.Failed{} -> ()
    UpDown.Blocked{} -> ()
    UpDown.Conflicting{} -> ()
    UpDown.Instructed{} -> ()
    UpDown.DroppedInstructions{} -> ()

_upkeepCovered :: Upkeep.Report Extension -> ()
_upkeepCovered rep = case rep of
    Upkeep.Acted{} -> ()
    Upkeep.Upkeep{} -> ()
    Upkeep.Downkeep{} -> ()
    Upkeep.NextLook{} -> ()
    Upkeep.Wedged{} -> ()
    Upkeep.Unwedged{} -> ()
    Upkeep.Demoted{} -> ()
    Upkeep.Parked{} -> ()
    Upkeep.Reapplying{} -> ()
    Upkeep.Paused{} -> ()
    Upkeep.Resumed{} -> ()
    Upkeep.GaveUp{} -> ()
    Upkeep.Adopted{} -> ()
    Upkeep.Released{} -> ()
    Upkeep.Policy{} -> ()
    Upkeep.Untended{} -> ()
    Upkeep.Escaped{} -> ()
    Upkeep.Supervising{} -> ()
    Upkeep.Retired{} -> ()
    Upkeep.Holding{} -> ()

_serveCovered :: Serve.Report -> ()
_serveCovered rep = case rep of
    Serve.Started -> ()
    Serve.Stopped -> ()
    Serve.HungUp{} -> ()
    Serve.BadCommand{} -> ()
    Serve.BadSeed{} -> ()
    Serve.BadDirective{} -> ()
    Serve.BadLoad{} -> ()
    Serve.Loading{} -> ()
    Serve.LoadDone{} -> ()
    Serve.Declared{} -> ()
    Serve.Cleared{} -> ()
    Serve.Supervised{} -> ()
    Serve.AutoConverged{} -> ()
    Serve.Instructed{} -> ()
    Serve.Tended{} -> ()
    Serve.ConvergeStart{} -> ()
    Serve.ConvergeStop{} -> ()
    Serve.StatusReport{} -> ()
    Serve.HistoryReport{} -> ()
    Serve.HistoryElided{} -> ()
    Serve.QueryReport{} -> ()
    Serve.HelpText{} -> ()
