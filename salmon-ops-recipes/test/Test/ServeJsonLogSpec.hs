{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for @run serve --json-log FILE@: what the flag parses
into ('CommandLine.ReportTextJsonLog'), that it excludes @--json@, and that
@run up@\/@run down@ do not take it. What the loop then does with it (the
text reporters on stdout, 'Salmon.Reporter.Tagged.reportJSONLines' on the
file beside them) is composed in 'CommandLine.execCommandOrSeedWithRewrites',
which reads the process's own stdin and so is not run in process here.
-}
module Test.ServeJsonLogSpec (tests) where

import Data.Aeson (Result (..), Value (..), fromJSON, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import qualified Options.Applicative as Opt
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import qualified Salmon.Builtin.CommandLine as CommandLine

tests :: TestTree
tests =
    testGroup
        "serve --json-log"
        [ testCase "--json-log FILE is text on the terminal and JSON in FILE" $ do
            fmt <- serveFormat ["serve", "--json-log", "/var/log/salmon.jsonl"]
            assertEqual "" (CommandLine.ReportTextJsonLog "/var/log/salmon.jsonl") fmt
        , testCase "without a flag, and with --json, nothing changed" $ do
            serveFormat ["serve"] >>= assertEqual "text" CommandLine.ReportText
            serveFormat ["serve", "--json"] >>= assertEqual "json" CommandLine.ReportJson
        , testCase "--json and --json-log together are refused" $ do
            refused ["serve", "--json", "--json-log", "/tmp/x.jsonl"]
            refused ["serve", "--json-log", "/tmp/x.jsonl", "--json"]
        , testCase "run up and run down do not take --json-log" $ do
            refused ["up", "--json-log", "/tmp/x.jsonl"]
            refused ["down", "--json-log", "/tmp/x.jsonl"]
        ]

parse :: [String] -> Opt.ParserResult CommandLine.RunCommand
parse = Opt.execParserPure Opt.defaultPrefs (Opt.info CommandLine.runCommandParser mempty)

refused :: [String] -> IO ()
refused args =
    case parse args of
        Opt.Failure _ -> pure ()
        Opt.Success other -> assertFailure (unwords args <> ": accepted as " <> show other)
        Opt.CompletionInvoked _ -> assertFailure "completion"

{- | The 'CommandLine.ReportFormat' of a parsed @serve@, read as the fourth
of the constructor's fields (after the concurrency cap, the
'CommandLine.StartupOptions' and @--no-autoconverge@) through its JSON encoding rather than by a
pattern naming all of them, so a flag appended to 'CommandLine.RunServe'
does not have to be spelled here too. -}
serveFormat :: [String] -> IO CommandLine.ReportFormat
serveFormat args =
    case parse args of
        Opt.Success cmd@CommandLine.RunServe{} ->
            case toJSON cmd of
                Object o
                    | Just (Array fields) <- KeyMap.lookup "contents" o
                    , (_ : _ : _ : v : _) <- toList fields
                    , Success fmt <- fromJSON v ->
                        pure fmt
                other -> assertFailure ("unexpected encoding of a serve command: " <> show other)
        Opt.Success other -> assertFailure ("not a serve command: " <> show other)
        Opt.Failure f -> assertFailure ("parse failed: " <> fst (Opt.renderFailure f "salmon"))
        Opt.CompletionInvoked _ -> assertFailure "completion"
