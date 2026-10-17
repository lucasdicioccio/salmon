{- | Layer 0 coverage for the builtin card templates ("Salmon.Builtin.Cards",
feature 91138a85).

* Attaching a card to a builtin changes nothing it renders: the same
  representative, @ref@, @help@ and @notes@, and its dependencies untouched.
* Each card, rendered here, parses and draws under the card contract with
  the vendored tramaj-js (@salmon-ops\/ui-test\/builtin-cards.test.mjs@, run
  under @node@, skipped loudly without it): the right sentence per verdict,
  its facts, no error on random contexts.
* 'Cards.tramajString' round-trips arbitrary text through tramaj-js.
-}
module Test.BuiltinCardsSpec (tests) where

import Control.Monad (unless)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)
import Test.QuickCheck (Gen, arbitrary, frequency, generate, listOf, elements, vectorOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import qualified Salmon.Builtin.Cards as Cards
import Salmon.Builtin.Extension (Extension (..), Op, evalDeps, ignoreTrack)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.CardTemplate (cardTemplateOf)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref)
import Salmon.Reporter (silent)
import Test.Harness (requireExecutable, withTempDir)

-- | A builtin and its card.
data Case = Case
    { caseName :: Text
    , casePlain :: Op
    , caseCard :: Cards.Card
    }

cases :: [Case]
cases =
    [ Case "file-contents" (FS.filecontents (FS.FileContents filePath ("not on the card" :: Text))) (Cards.fileContentsCard filePath)
    , Case "directory" (FS.dir (FS.Directory "/var/lib/demo")) (Cards.directoryCard "/var/lib/demo")
    , Case "systemd-service" (Systemd.systemdService silent ignoreTrack ignoreTrack unitCfg) (Cards.systemdServiceCard unitCfg)
    , Case "pg-database" (Postgres.database silent ignoreTrack ignoreTrack 5433 (Postgres.Database "appdb")) (Cards.pgDatabaseCard 5433 (Postgres.Database "appdb"))
    ]
  where
    filePath = "/etc/demo/app.conf"

unitCfg :: Systemd.Config
unitCfg = Systemd.Config Systemd.System "/etc/systemd/system" "demo.service" unit svc install
  where
    unit = Systemd.Unit "a service" "network-online.target"
    svc = Systemd.Service Systemd.Simple "nobody" "nogroup" "0022" (Systemd.Start "/bin/true" []) Systemd.OnFailure Systemd.Process "/"
    install = Systemd.Install "multi-user.target"

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Cards"
        [ testGroup "a card changes nothing the builtin renders" (fmap presentationOnly cases)
        , testCase "tramajString escapes what would end the literal" $ do
            assertEqual "plain" "\"abc\"" (Cards.tramajString "abc")
            assertEqual "quote, backslash, backtick" "\"\\\"\\\\\\`\"" (Cards.tramajString "\"\\`")
            assertEqual "newline, control" "\"a\\nb\\u{7}\"" (Cards.tramajString "a\nb\a")
        , testCase "every card draws under tramaj-js; tramajString round-trips (node)" runNode
        ]

presentationOnly :: Case -> TestTree
presentationOnly c = testCase (Text.unpack c.caseName) $ do
    let carded = Cards.withCard (Cards.renderCard c.caseCard) c.casePlain
    assertEqual "the root: representative, shorthand, ref, help, notes" (fmap describe (rootOf c.casePlain)) (fmap describe (rootOf carded))
    assertEqual "the card is carried" (Just (Cards.renderCard c.caseCard)) (cardTemplateOf . (.extension.dynamics) =<< rootOf carded)
    assertEqual
        "the dependencies carry no card and are unchanged"
        [describe a | a <- drop 1 (allActs c.casePlain)]
        [describe a | a <- drop 1 (allActs carded)]
    assertEqual "nor do they carry a template" [] [() | a <- drop 1 (allActs carded), Just _ <- [cardTemplateOf a.extension.dynamics]]
  where
    describe :: Act Extension -> (Dag.Representative, Text, Ref, Text, [Text])
    describe a = (Dag.representative a, a.shorthand, a.extension.ref, a.extension.help, a.extension.notes)

rootOf :: Op -> Maybe (Act Extension)
rootOf o = case o.node of
    Actions a -> Just a
    Actionless -> Nothing

allActs :: Op -> [Act Extension]
allActs o = [a | n <- toList (evalDeps o), Actions a <- [n.node]]

-- arbitrary text, weighted towards what a Tramaj literal must escape
genText :: Gen Text
genText =
    Text.pack
        <$> listOf
            ( frequency
                [ (4, elements "\"\\`$@.(){}\n\t\r\0\a\DEL")
                , (2, elements "ab é日")
                , (1, arbitrary)
                ]
            )

runNode :: IO ()
runNode = do
    let script = "../salmon-ops/ui-test/builtin-cards.test.mjs"
    there <- doesFileExist script
    if not there
        then putStrLn "SKIPPED: salmon-ops/ui-test is not next to the test suite; the builtin card checks need the source tree"
        else requireExecutable "node" $ withTempDir $ \dir -> do
            texts <- generate (vectorOf 300 genText)
            let input = dir </> "cards.json"
                quote :: Text -> Aeson.Value
                quote t = object ["text" .= t, "source" .= (".p(" <> Cards.tramajString t <> ")")]
                card :: Case -> Aeson.Value
                card c =
                    object
                        [ "name" .= c.caseName
                        , "source" .= Cards.renderCard c.caseCard
                        , "facts" .= c.caseCard.cardFacts
                        , "verdicts" .= sentences c.caseCard.cardVerdicts
                        ]
            LByteString.writeFile input (Aeson.encode (object ["cards" .= fmap card cases, "quotes" .= fmap quote ("" : texts :: [Text])]))
            (code, out, err) <- readCreateProcessWithExitCode (proc "node" [script, input]) ""
            unless (code == ExitSuccess) $
                assertFailure ("node " <> script <> " failed (" <> show code <> "):\n" <> out <> err)

sentences :: Cards.Verdicts -> Aeson.Value
sentences v =
    object
        [ "unchecked" .= v.verdictUnchecked
        , "inPlace" .= v.verdictInPlace
        , "toApply" .= v.verdictToApply
        , "unsure" .= v.verdictUnsure
        , "noCheck" .= v.verdictNoCheck
        ]
