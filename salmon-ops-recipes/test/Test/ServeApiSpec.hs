{-# LANGUAGE OverloadedStrings #-}

{- | The drift guard for @salmon-ops\/openapi\/serve-api.openapi.json@, the
machine-readable description of @run serve@'s HTTP API that the loop also
serves at @GET \/openapi.json@.

Three things are compared with the document, none of them by hand:

* every golden of "Test.ReportJsonSpec" (one per constructor of all four
  report streams), as the object @--json@ prints and as the @data:@ of an
  event, and the status sink's document;
* the set of @(stream, kind)@ the document lists, with the set the goldens
  cover, both ways: a report constructor with no schema, or a schema for one
  that is gone, is a failure here rather than in a client;
* the routes in @Salmon.Actions.Serve.Http@'s source with the document's
  operations, both ways. "Test.ServeHttpSpec" does the rest live: it checks
  every response it gets against the schema of the operation and status, and
  that each documented route answers.

Strict checking (a field the schema does not declare is an error) is what
makes an added field fail rather than pass unnoticed.
-}
module Test.ServeApiSpec (tests) where

import Data.Aeson (Value (..), eitherDecode, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Foldable (toList)
import Data.List (nub, sort, (\\))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import System.Directory (doesFileExist)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Actions.Serve.Events as Events
import Salmon.Reporter.Tagged (Tagged)
import Test.ReportJsonSpec (goldens, sinkDocumentText)
import Test.ServeApi

tests :: TestTree
tests =
    testGroup
        "the OpenAPI description of run serve"
        [ testCase "it is OpenAPI 3.1 and uses only keywords the checker understands" $ do
            assertEqual "" (Just (String "3.1.0")) (field "openapi" document)
            assertEqual "keywords the checker ignores" [] unsupportedKeywords
        , testCase "every $ref resolves" $
            assertEqual "" [] [r | r <- refsIn document, Text.stripPrefix "#/components/schemas/" r `notElem` map (Just . fst) schemaNames]
        , testGroup "reports" [testCase name (assertValid (validateAs True "Report" (toJSON tagged))) | (name, tagged, _) <- goldens]
        , testCase "the kinds the document lists are exactly the constructors that have a golden" $ do
            let goldenKinds = nub (sort [k | (_, t, _) <- goldens, Just k <- [streamKind (toJSON t)]])
            assertEqual "in goldens, not in the document" [] (goldenKinds \\ documentKinds)
            assertEqual "in the document, not in goldens" [] (documentKinds \\ goldenKinds)
        , testGroup
            "as the data of an event"
            [ testCase name $ do
                let ev = Events.eventValue (Events.Event 7 (Just (Serve.Origin "sock#3")) (Events.Reported tagged))
                assertValid (validateEvent ev)
            | (name, tagged, _) <- goldens
            ]
        , testCase "an event with no origin, an enqueued event and a gap" $ do
            assertValid (validateEvent (Events.eventValue (Events.Event 1 Nothing (Events.Reported (taggedOf "started")))))
            assertValid (validateAs True "EnqueuedEvent" (Events.eventValue (Events.Event 2 (Just (Serve.Origin "sock#1")) (Events.Enqueued "up a"))))
            assertValid (validateAs True "GapEvent" (Events.gapValue 40))
        , testCase "the status sink's document" $
            case eitherDecode (LText.encodeUtf8 (LText.fromStrict sinkDocumentText)) of
                Left err -> assertFailure err
                Right v -> assertValid (validateAs True "StatusDocument" v)
        , testCase "a report with a field the schema does not declare fails (the guard bites)" $
            assertBool "" (not (null (validateAs True "Report" (addField "surprise" (toJSON (taggedOf "started"))))))
        , testCase "a report missing a declared field fails" $
            assertBool "" (not (null (validateAs True "Report" (dropField "error" (toJSON (taggedOf "bad-command"))))))
        , testCase "the routes in Http.hs and the document's operations agree" routesAgree
        ]
  where
    assertValid errs = assertEqual "schema errors" [] errs

    -- what a client does with an event: the report is the object minus the two
    -- fields the event adds
    validateEvent = validateEventData

    taggedOf :: Text -> Tagged
    taggedOf k = head [t | (_, t, _) <- goldens, streamKind (toJSON t) == Just ("serve", k)]

field :: Text -> Value -> Maybe Value
field k (Object o) = KeyMap.lookup (Key.fromText k) o
field _ _ = Nothing

addField :: Text -> Value -> Value
addField k (Object o) = Object (KeyMap.insert (Key.fromText k) (String "x") o)
addField _ v = v

dropField :: Text -> Value -> Value
dropField k (Object o) = Object (KeyMap.delete (Key.fromText k) o)
dropField _ v = v

streamKind :: Value -> Maybe (Text, Text)
streamKind v = case (field "stream" v, field "kind" v) of
    (Just (String s), Just (String k)) -> Just (s, k)
    _ -> Nothing

schemaNames :: [(Text, Value)]
schemaNames = case field "components" document >>= field "schemas" of
    Just (Object o) -> [(Key.toText k, v) | (k, v) <- KeyMap.toList o]
    _ -> []

-- | The @(stream, kind)@ of every tagged report schema of the four streams.
documentKinds :: [(Text, Text)]
documentKinds =
    nub . sort $
        [ (s, k)
        | (name, schema) <- schemaNames
        , any (`Text.isPrefixOf` name) ["UpDown_", "Upkeep_", "Serve_", "Follow_"]
        , Just props <- [field "properties" schema]
        , Just (String s) <- [field "stream" props >>= field "const"]
        , Just (String k) <- [field "kind" props >>= field "const"]
        ]

refsIn :: Value -> [Text]
refsIn (Object o) = [r | Just (String r) <- [KeyMap.lookup "$ref" o]] <> concatMap refsIn (KeyMap.elems o)
refsIn (Array xs) = concatMap refsIn (toList xs)
refsIn _ = []

-- | @(METHOD, template)@ pairs the routes in the source name, against the document's.
routesAgree :: IO ()
routesAgree = do
    let path = "../salmon-ops/src/Salmon/Actions/Serve/Http.hs"
    there <- doesFileExist path
    if not there
        then putStrLn "SKIPPED: Http.hs is not next to the test suite; the route table comparison needs the source tree"
        else do
            src <- readFile path
            let coded = nub (sort (concatMap routesOf (lines src)))
                documented = nub (sort [(m, normalise p) | (m, p) <- documentedRoutes])
            assertEqual "routes in the code the document lacks" [] (coded \\ documented)
            assertEqual "operations in the document the code lacks" [] (documented \\ coded)
  where
    normalise p
        | p == "/ui/{file}" = "/ui"
        | otherwise = p

    -- lines like   ("GET", ["dag"]) ->    /   ("POST", ["auth", "logout"]) ->   /   ("GET", ("ui" : rest)) ->
    routesOf :: String -> [(Text, Text)]
    routesOf line =
        case Text.stripPrefix "(\"" (Text.strip (Text.pack line)) of
            Just rest
                | (m, after) <- Text.breakOn "\"" rest
                , m `elem` ["GET", "POST"]
                , Just r <- Text.stripPrefix "\", " after ->
                    [(m, routePath (Text.takeWhile (/= '>') r))]
            _ -> []

    routePath r
        | "[]" `Text.isPrefixOf` r = "/"
        | "(\"ui\"" `Text.isPrefixOf` r = "/ui"
        | "[" `Text.isPrefixOf` r =
            "/" <> Text.intercalate "/" (Text.splitOn "," (Text.filter (`notElem` ['[', ']', ' ', '"']) (Text.takeWhile (/= ']') r)))
        | otherwise = r
