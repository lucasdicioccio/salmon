{- | Layer 0 coverage for the builtin card templates ("Salmon.Builtin.Cards",
feature 91138a85; "Salmon.Builtin.Cards.Gcp" and "Salmon.Builtin.Cards.Postgres",
feature d8a79380).

* Attaching a card to a builtin changes nothing it renders: the same
  representative, @ref@, @help@ and @notes@, and its dependencies untouched.
* Each card, rendered here, parses and draws under the card contract with
  the vendored tramaj-js (@salmon-ops\/ui-test\/builtin-cards.test.mjs@, run
  under @node@, skipped loudly without it): the right sentence per verdict,
  its facts, no error on random contexts.
* 'Cards.tramajString' round-trips arbitrary text through tramaj-js.
* A card never carries what the builtin keeps out of its public fields: a
  password, a file's contents, an instance's metadata.
* A card's links are https console URLs built from the declaration's public
  names only, percent-encoded ('Cards.percentEncode').
* Every card links to its builtin's page on the docs site
  ('Cards.docsLink'), and that page has a source under
  @resources\/builtins\/@ (feature 42358f5a).
-}
module Test.BuiltinCardsSpec (tests) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as ByteString
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import qualified Data.Map as Map
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (readHex)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)
import Test.QuickCheck (Gen, arbitrary, elements, forAll, frequency, generate, listOf, vectorOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified Salmon.Builtin.Cards as Cards
import qualified Salmon.Builtin.Cards.Gcp as GcpCards
import qualified Salmon.Builtin.Cards.Postgres as PgCards
import Salmon.Builtin.Extension (Extension (..), Op, evalDeps, ignoreTrack)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Gcp.CloudDns as Dns
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as Lb
import qualified Salmon.Builtin.Nodes.Gcp.SecretManager as Secrets
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import Salmon.Builtin.Nodes.SecretDelivery (Placement (..))
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
        <> gcpCases
        <> pgCases
  where
    filePath = "/etc/demo/app.conf"

-- | What no card may show: the declarations below carry these, in places the builtins keep out of their public fields.
secretWords :: [Text]
secretWords = [pgPassword, "metadata-value-not-on-card", "/tmp/w/startup-script.sh"]

pgPassword :: Text
pgPassword = "pg-password-not-on-card"

project :: Core.Project
project = Core.Project "demo-proj"

gcpCases :: [Case]
gcpCases =
    [ Case "gcp-instance" (Compute.gceInstance silent ignoreTrack demoInstance) (GcpCards.instanceCard demoInstance)
    , Case "gcp-address" (Compute.address silent ignoreTrack addr) (GcpCards.addressCard addr)
    , Case "gcp-address-internal" (Compute.address silent ignoreTrack addrIn) (GcpCards.addressCard addrIn)
    , Case "gcp-firewall-rule" (Compute.firewallRule silent ignoreTrack fw) (GcpCards.firewallRuleCard fw)
    , Case "gcp-subnet" (Compute.subnet silent ignoreTrack net) (GcpCards.subnetCard net)
    , Case "gcp-dns-zone" (Dns.managedZone silent ignoreTrack zone) (GcpCards.managedZoneCard zone)
    , Case "gcp-dns-record" (Dns.recordSet silent ignoreTrack rs) (GcpCards.recordSetCard False rs)
    , Case "gcp-dns-record-resolved" (Dns.resolvedRecordSet silent ignoreTrack rs (pure Nothing)) (GcpCards.recordSetCard True rs)
    , Case "gcp-bucket" (Storage.bucket silent ignoreTrack bkt) (GcpCards.bucketCard bkt)
    , Case "gcp-secret" (Secrets.secret silent ignoreTrack sec) (GcpCards.secretCard sec)
    , Case "gcp-secret-file" (Secrets.secretFile silent ignoreTrack secFile) (GcpCards.secretFileCard secFile)
    , Case "gcp-application-lb" (Lb.applicationLoadBalancer silent ignoreTrack alb) (GcpCards.applicationLoadBalancerCard alb)
    ]
  where
    addr = Compute.Address "demo-ip" project (Core.Region "europe-west1") Compute.ExternalAddress
    addrIn = Compute.Address "demo-ip-in" project (Core.Region "europe-west1") (Compute.InternalAddress "demo-subnet" (Just "10.0.0.5"))
    fw = Compute.FirewallRule "demo-ssh" project "default" "tcp:22" ["203.0.113.0/24"] ["demo-ssh"]
    net = Compute.Subnet "demo-subnet" project (Core.Region "europe-west1") "default" "10.0.0.0/24" Compute.PrivateSubnet
    zone = Dns.ManagedZone "demo-zone" project "example.org" "a demo zone"
    rs = Dns.RecordSet zone "www.example.org" Dns.A 300 ["203.0.113.7"]
    bkt = Storage.Bucket "demo-bucket" project (Core.Region "europe-west1") True
    sec = Secrets.Secret "demo-secret" project "automatic"
    secFile = Secrets.SecretFile project "demo-secret" "latest" (Placement "/etc/demo/token" "root" "demo" "0640")
    alb = Lb.httpLoadBalancer "demo-lb" project (Core.Region "europe-west1") [Lb.InstanceGroupBackend "demo-group" (Lb.InstanceGroupZone "europe-west1-b") [8080]] Nothing

demoInstance :: Compute.Instance
demoInstance =
    Compute.Instance
        { Compute.instanceName = "demo-vm"
        , Compute.instanceProject = project
        , Compute.instanceZone = Core.Zone "europe-west1-b"
        , Compute.instanceMachineType = Compute.E2Medium
        , Compute.instanceBootDisk = Compute.BootDisk 10 Nothing (Just "ubuntu-2404-lts-amd64") (Just "ubuntu-os-cloud")
        , Compute.instanceNetwork = "default"
        , Compute.instanceSubnet = "default"
        , Compute.instanceServiceAccount = Nothing
        , Compute.instanceMetadata = Map.fromList [("some-key", "metadata-value-not-on-card")]
        , Compute.instanceMetadataFiles = Map.fromList [("startup-script", "/tmp/w/startup-script.sh")]
        , Compute.instanceExternalAddress = Compute.ReservedExternal "demo-ip"
        , Compute.instanceInternalAddress = Compute.EphemeralInternal
        , Compute.instanceTags = ["demo-ssh"]
        , Compute.instancePower = Compute.PoweredOn
        , Compute.instanceScopes = Compute.DefaultScopes
        , Compute.instanceMachineTypeChange = Compute.RefuseMachineTypeChange
        , Compute.instanceRunLimit = Compute.NoRunLimit
        }

pgCases :: [Case]
pgCases =
    [ Case "pg-user" (Postgres.user silent ignoreTrack ignoreTrack 5433 app (Postgres.Password pgPassword)) (PgCards.userCard 5433 app)
    , Case "pg-user-passfile" (Postgres.userPassFile silent ignoreTrack ignoreTrack 5433 (FS.PreExisting "/run/secrets/app.pass") app) (PgCards.userPassFileCard 5433 "/run/secrets/app.pass" app)
    , Case "pg-replication-user" (Postgres.replicationUser silent ignoreTrack ignoreTrack 5433 repl (Postgres.Password pgPassword)) (PgCards.replicationUserCard 5433 repl)
    , Case "pg-replication-slot" (Postgres.replicationSlot silent ignoreTrack 5433 "standby_1") (PgCards.replicationSlotCard 5433 "standby_1")
    , Case "pg-hba-line" (Postgres.hbaLine silent ignoreTrack "main" hba) (PgCards.hbaLineCard "main" hba)
    , Case "pg-alter-system" (Postgres.alterSystemSet silent ignoreTrack 5433 "wal_level" "replica") (PgCards.alterSystemCard 5433 "wal_level" "replica")
    , Case "pg-server-tls" (Postgres.serverTls silent ignoreTrack 5433 tls) (PgCards.serverTlsCard 5433 tls)
    ]
  where
    app = Postgres.User "app"
    repl = Postgres.User "replicator"
    hba = "hostssl all app 10.0.0.0/24 scram-sha-256"
    tls = Postgres.ServerTls "/etc/demo/tls/server.crt" "/etc/demo/tls/server.key" "/etc/demo/tls/ca.crt"

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
        , testCase "no card carries a password, a file's contents or an instance's metadata" $
            forM_ cases $ \c -> forM_ ("not on the card" : secretWords) $ \w ->
                assertBool (Text.unpack c.caseName <> " shows " <> show w) (not (w `Text.isInfixOf` Cards.renderCard c.caseCard))
        , testCase "every GCP card links to the console, for its own project" $
            forM_ gcpCases $ \c -> do
                assertBool (Text.unpack c.caseName <> ": no link") (not (null c.caseCard.cardLinks))
                forM_ c.caseCard.cardLinks $ \l -> do
                    assertBool (show l) ("https://console.cloud.google.com/" `Text.isPrefixOf` l.linkUrl)
                    assertBool (show l) ("?project=demo-proj" `Text.isSuffixOf` l.linkUrl)
        , testCase "a console link, by hand" $ do
            assertEqual "instance" ["https://console.cloud.google.com/compute/instancesDetail/zones/europe-west1-b/instances/demo-vm?project=demo-proj"] (fmap (.linkUrl) (GcpCards.instanceCard demoInstance).cardLinks)
            assertEqual "encoded" "https://console.cloud.google.com/a%2Fb/%C3%A9%20%3F?project=p%26q%3Dr" (GcpCards.consoleUrl ["a/b", "é ?"] (Core.Project "p&q=r"))
        , testProperty "percentEncode: only unreserved characters and %XX, and decodes back" $
            forAll genText $ \t ->
                let e = Cards.percentEncode t
                 in (Text.all (\ch -> isAsciiLower ch || isAsciiUpper ch || isDigit ch || ch `elem` ("-._~%" :: String)) e, percentDecode e) === (True, Just t)
        , testCase "every card links to its builtin's docs page, last, and the page has a source" docsPages
        , testCase "a docs link, by hand" $
            assertEqual "url" (Cards.Link "docs" "https://lucasdicioccio.github.io/salmon/builtin-filesystem-file-contents.html") (Cards.docsLink "filesystem-file-contents")
        , testCase "every card draws under tramaj-js; tramajString round-trips (node)" runNode
        ]

docsPages :: IO ()
docsPages = do
    let dir = "../resources/builtins"
    there <- doesFileExist (dir </> "README.md")
    forM_ cases $ \c -> case c.caseCard.cardDocs of
        Nothing -> assertFailure (Text.unpack c.caseName <> ": no docs page")
        Just slug -> do
            assertBool (show slug <> ": a slug is lowercase letters, digits and dashes") (not (Text.null slug) && Text.all (\ch -> isAsciiLower ch || isDigit ch || ch == '-') slug)
            assertEqual (Text.unpack c.caseName <> ": the docs link comes last") (Just (Cards.docsLink slug)) (lastMaybe (Cards.allLinks c.caseCard))
            assertBool (Text.unpack c.caseName <> ": the docs link is drawn") (Cards.tramajString (Cards.docsLink slug).linkUrl `Text.isInfixOf` Cards.renderCard c.caseCard)
            if not there
                then putStrLn "SKIPPED: resources/builtins is not next to the test suite; the page-source check needs the source tree"
                else do
                    page <- doesFileExist (dir </> Text.unpack slug <> ".md")
                    assertBool (Text.unpack c.caseName <> ": no page source " <> dir </> Text.unpack slug <> ".md") page
  where
    lastMaybe xs = if null xs then Nothing else Just (last xs)

percentDecode :: Text -> Maybe Text
percentDecode t = either (const Nothing) Just . Text.decodeUtf8' . ByteString.pack =<< go (Text.unpack t)
  where
    go [] = Just []
    go ('%' : a : b : rest)
        | isHexDigit a && isHexDigit b, [(n, "")] <- readHex [a, b] = (fromInteger n :) <$> go rest
    go ('%' : _) = Nothing
    go (ch : rest)
        | ch < chr 128 = (fromIntegral (fromEnum ch) :) <$> go rest
        | otherwise = Nothing

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
                        , "links" .= [(l.linkLabel, l.linkUrl) | l <- Cards.allLinks c.caseCard]
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
