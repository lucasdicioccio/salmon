{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Netbird": the argv of the
client's commands (the setup key is a path, never a value), the verdict
drawn from @netbird status --json@, the refusal to move a connected peer,
the redaction of the key from a failed command's output, and the shape of
the graph.

__The status documents below are written by hand__ from the field names in
the upstream client's output structs; none was captured from a running
daemon, and no NetBird binary is run here. The one process the suite starts
is @sh@, standing in for a client that fails while quoting the key.
-}
module Test.NetbirdSpec (tests) where

import Control.Exception (try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import System.Process.ListLike (CmdSpec (..), CreateProcess (..), proc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.Query (pathedNodes)
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Command (..), CommandFailed (..))
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Debian.AptRepository (AptRepository (..), renderSources, resolveSuite, viaRepository)
import Salmon.Builtin.Nodes.Debian.Package (Package (..))
import Salmon.Builtin.Nodes.Netbird
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Eval (expand)
import Salmon.Op.Ref (Ref)
import Salmon.Reporter (silent)

import Test.Harness (capture)

selfHosted :: Enrolment
selfHosted =
    Enrolment
        { enrolSetupKeyFile = "/etc/netbird-setup.key"
        , enrolManagementUrl = Just "https://netbird.example.org"
        , enrolHostname = Just "laptop-1"
        }

-- | A hand-written status document: the fields the check reads, among a few
-- of the others the client prints.
statusDoc :: Maybe C8.ByteString -> C8.ByteString -> Bool -> C8.ByteString
statusDoc daemon url connected =
    mconcat
        [ "{\"peers\":{\"total\":2,\"connected\":1,\"details\":[]},"
        , "\"cliVersion\":\"0.80.0\",\"daemonVersion\":\"0.80.0\","
        , maybe "" (\d -> "\"daemonStatus\":\"" <> d <> "\",") daemon
        , "\"management\":{\"url\":\"" <> url <> "\",\"connected\":" <> (if connected then "true" else "false") <> ",\"error\":\"\"},"
        , "\"signal\":{\"url\":\"https://netbird.example.org:443\",\"connected\":true,\"error\":\"\"},"
        , "\"netbirdIp\":\"100.64.0.7/16\",\"publicKey\":\"PUBKEY=\",\"fqdn\":\"laptop-1.netbird.example\"}"
        ]

verdict :: Enrolment -> C8.ByteString -> Maybe CheckResult
verdict enrol doc = interpretStatus enrol <$> parseStatus doc

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Netbird"
        [ testGroup
            "commands"
            [ testCase "enrolment names the key by its file, with the declared server and name" $
                assertEqual
                    ""
                    (RawCommand "netbird" ["up", "--setup-key-file", "/etc/netbird-setup.key", "--management-url", "https://netbird.example.org", "--hostname", "laptop-1"])
                    (cmdspec (prepare netbirdcommand (Up selfHosted)))
            , testCase "nothing declared, nothing passed" $
                assertEqual
                    ""
                    (RawCommand "netbird" ["up", "--setup-key-file", "/k"])
                    (cmdspec (prepare netbirdcommand (Up (enrolment "/k"))))
            , testCase "no form of the enrolment takes the key as a value" $
                case cmdspec (prepare netbirdcommand (Up selfHosted)) of
                    RawCommand _ args -> assertBool "" (not (any (`elem` ["--setup-key", "-k"]) args))
                    other -> assertFailure ("unexpected " <> show other)
            , testCase "status is asked as json" $
                assertEqual "" (RawCommand "netbird" ["status", "--json"]) (cmdspec (prepare netbirdcommand StatusJson))
            , testCase "down disconnects" $
                assertEqual "" (RawCommand "netbird" ["down"]) (cmdspec (prepare netbirdcommand Down))
            ]
        , testGroup
            "interpretStatus"
            [ testCase "connected to the declared server is satisfied" $
                assertEqual "" (Just Success) (verdict selfHosted (statusDoc (Just "Connected") "https://netbird.example.org:443" True))
            , testCase "an undeclared server is not compared" $
                assertEqual "" (Just Success) (verdict (enrolment "/k") (statusDoc (Just "Connected") "https://api.other.example:443" True))
            , testCase "a peer that needs a login is not enrolled" $
                assertEqual "" (Just (Failure "netbird daemon is NeedsLogin")) (verdict selfHosted (statusDoc (Just "NeedsLogin") "" False))
            , testCase "an idle daemon (after a down) is not connected" $
                assertEqual "" (Just (Failure "netbird daemon is Idle")) (verdict selfHosted (statusDoc (Just "Idle") "https://netbird.example.org:443" False))
            , testCase "connecting is transitional" $
                assertEqual "" (Just Unknown) (verdict selfHosted (statusDoc (Just "Connecting") "https://netbird.example.org:443" False))
            , testCase "connected without the management connection is reported" $
                assertEqual
                    ""
                    (Just (Failure "not connected to the management server"))
                    (verdict selfHosted (statusDoc (Just "Connected") "https://netbird.example.org:443" False))
            , testCase "connected to another server is reported, naming it" $
                assertEqual
                    ""
                    (Just (Failure "connected to another management server: https://api.other.example:443"))
                    (verdict selfHosted (statusDoc (Just "Connected") "https://api.other.example:443" True))
            , testCase "a client without daemonStatus is judged on the management connection" $ do
                assertEqual "" (Just Success) (verdict selfHosted (statusDoc Nothing "https://netbird.example.org:443" True))
                assertEqual
                    ""
                    (Just (Failure "not connected to the management server"))
                    (verdict selfHosted (statusDoc Nothing "https://netbird.example.org:443" False))
            , testCase "output that is not a status document is not interpreted" $ do
                assertEqual "" Nothing (parseStatus "Daemon status: NeedsLogin\n")
                assertEqual "" Nothing (parseStatus "{\"peers\":{}}")
            , testCase "the reason never quotes the rest of the document" $
                case verdict selfHosted (statusDoc (Just "Connected") "https://api.other.example:443" True) of
                    Just (Failure msg) -> assertBool "" (not ("PUBKEY" `Text.isInfixOf` msg) && not ("100.64" `Text.isInfixOf` msg))
                    other -> assertFailure ("expected Failure, got " <> show other)
            ]
        , testGroup
            "management URL"
            [ testCase "a default port, a trailing path and case do not make another server" $ do
                assertBool "" (sameManagement "https://netbird.example.org" "https://NetBird.example.org:443/")
                assertBool "" (sameManagement "http://netbird.example.org" "http://netbird.example.org:80")
                assertBool "" (sameManagement "https://netbird.example.org" "netbird.example.org:443")
            , testCase "another host, port or scheme does" $ do
                assertBool "" (not (sameManagement "https://netbird.example.org" "https://api.other.example"))
                assertBool "" (not (sameManagement "https://netbird.example.org" "https://netbird.example.org:33073"))
                assertBool "" (not (sameManagement "https://netbird.example.org" "http://netbird.example.org"))
            , testCase "only a connected peer counts as enrolled elsewhere" $ do
                let st url connected = Status (Just "Connected") url connected
                assertEqual "" (Just "https://api.other.example:443") (connectedElsewhere selfHosted (st "https://api.other.example:443" True))
                assertEqual "" Nothing (connectedElsewhere selfHosted (st "https://api.other.example:443" False))
                assertEqual "" Nothing (connectedElsewhere selfHosted (st "https://netbird.example.org:443" True))
                assertEqual "" Nothing (connectedElsewhere selfHosted (st "" True))
                assertEqual "" Nothing (connectedElsewhere (enrolment "/k") (st "https://api.other.example:443" True))
            ]
        , testGroup
            "redaction"
            [ testCase "every occurrence of the key is replaced" $
                assertEqual "" "key [redacted] rejected ([redacted])" (redact "S3CR3T" "key S3CR3T rejected (S3CR3T)")
            , testCase "text without the key is untouched" $
                assertEqual "" "login failed" (redact "S3CR3T" "login failed")
            , testCase "an empty key redacts nothing" $
                assertEqual "" "login failed" (redact "" "login failed")
            , testCase "a failing command that quotes the key leaks it nowhere" $ do
                (r, reports) <- capture
                -- the script spells the key in two halves, so that the argv
                -- (which a failure quotes, and which is public) does not hold it
                let p = proc "sh" ["-c", "k=S3C; echo \"invalid setup key ${k}R3T\"; echo \"${k}R3T rejected\" >&2; exit 3"]
                res <- try (runRedacted "S3CR3T" p r)
                case res of
                    Right () -> assertFailure "a non-zero exit must throw"
                    Left e@(CommandFailed{}) -> do
                        assertEqual "" 3 e.commandFailed_exitCode
                        assertEqual "" "invalid setup key [redacted]\n" e.commandFailed_stdout
                        assertEqual "" "[redacted] rejected\n" e.commandFailed_stderr
                        assertBool "exception text" (not ("S3CR3T" `Text.isInfixOf` Text.pack (show e)))
                seen <- reports
                assertBool "reported something" (not (null seen))
                assertBool "reports" (not (any (mentions "S3CR3T") seen))
            , testCase "a command that succeeds does not throw" $
                runRedacted "S3CR3T" (proc "sh" ["-c", "exit 0"]) silent
            ]
        , testGroup
            "graph"
            [ testCase "the key file appears in the notes as a path, the server in the help" $
                case opAct (node ignoreTrack) of
                    Nothing -> assertFailure "no node"
                    Just act -> do
                        assertEqual "" "netbird peer enrolled against https://netbird.example.org" act.extension.help
                        assertEqual
                            ""
                            ["setup key read from: /etc/netbird-setup.key", "registers as: laptop-1"]
                            act.extension.notes
            , testCase "one daemon is one effect site, whatever is declared" $
                assertEqual "" (rootRef (node ignoreTrack)) (rootRef (peer silent ignoreTrack ignoreTrack (enrolment "/other")))
            , testCase "building the graph does not read the key file" $
                assertBool "" (isJust (opAct (peer silent ignoreTrack ignoreTrack (enrolment "/nonexistent/netbird.key"))))
            , testCase "the package provider installs netbird, after the repository when one is given" $ do
                let withRepo = node (package (viaRepository repo))
                    bare = node (package ignoreTrack)
                assertEqual "" [Package "netbird"] (packagesOf bare)
                assertBool "repo node present" (any ("apt index" `Text.isInfixOf`) (shorthands withRepo))
                assertBool "no repo node" (not (any ("apt index" `Text.isInfixOf`) (shorthands bare)))
            , testCase "the repository is upstream's, pinned to the one package" $
                assertEqual
                    ""
                    ( Text.unlines
                        [ "Types: deb"
                        , "URIs: https://pkgs.netbird.io/debian"
                        , "Suites: stable"
                        , "Components: main"
                        , "Signed-By: /etc/apt/keyrings/netbird.asc"
                        ]
                    )
                    (renderSources repo (resolveSuite repo.repoSuite "bookworm"))
            ]
        ]
  where
    repo = netbirdRepository "/k/netbird.asc" "AAAA"
    node bin = peer silent bin ignoreTrack selfHosted
    packagesOf :: Op -> [Package]
    packagesOf = concatMap snd . collectDynamics
    shorthands :: Op -> [Text.Text]
    shorthands o = [h | (_, _, h) <- pathedNodes (runIdentity (expand o))]
    rootRef :: Op -> Maybe Ref
    rootRef o = (\act -> act.extension.ref) <$> opAct o
    mentions :: ByteString.ByteString -> Binary.Report -> Bool
    mentions needle rep = needle `ByteString.isInfixOf` C8.pack (show rep)
