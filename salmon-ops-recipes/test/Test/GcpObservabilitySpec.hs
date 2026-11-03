{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Layer 0 for the observability and budget nodes: "Salmon.Builtin.Nodes.Gcp.Uptime",
"Salmon.Builtin.Nodes.Gcp.Dashboard", "Salmon.Builtin.Nodes.Gcp.Logging" and
the budget of "Salmon.Builtin.Nodes.Gcp.Billing". The argv each command
renders, and the pure readers of @gcloud ... list\/describe --format json@ on
JSON written from the APIs' documented resource shapes (not captured from a
real project: nothing here has met one).
-}
module Test.GcpObservabilitySpec (tests) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), encode, object, toJSONList, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.ByteString (ByteString)
import Data.List (isInfixOf, isSubsequenceOf)
import qualified Data.Map as Map
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ListLike (CmdSpec (..), CreateProcess, cmdspec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension (Extension (..), Op, evalDeps, ignoreTrack)
import Salmon.Builtin.Nodes.Binary (prepare)
import qualified Salmon.Builtin.Nodes.Gcp.Billing as Billing
import Salmon.Builtin.Nodes.Gcp.Core (Project (..))
import qualified Salmon.Builtin.Nodes.Gcp.Dashboard as Dashboard
import qualified Salmon.Builtin.Nodes.Gcp.Logging as Logging
import qualified Salmon.Builtin.Nodes.Gcp.Monitoring as Monitoring
import qualified Salmon.Builtin.Nodes.Gcp.Uptime as Uptime
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Reporter (silent)

tests :: TestTree
tests =
    testGroup
        "Gcp observability and budgets"
        [ testGroup "Uptime" uptimeTests
        , testGroup "Dashboard" dashboardTests
        , testGroup "Logging" loggingTests
        , testGroup "Billing.budget" budgetTests
        ]

argsOf :: CreateProcess -> [String]
argsOf p = case cmdspec p of
    RawCommand _ args -> args
    ShellCommand s -> [s]

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

json :: Value -> ByteString
json = LByteString.toStrict . encode

only :: Op -> Extension
only o = case Map.elems (Dag.dagNodes (Dag.foldDag Dag.sameRepresentative (evalDeps o))) of
    [act] -> act.extension
    acts -> error ("expected one node, got " <> show (length acts))

proj :: Project
proj = Project "p"

-------------------------------------------------------------------------------

uc :: Uptime.UptimeCheck
uc = Uptime.httpsUptimeCheck proj "api up" "api.example.org" "/healthz"

-- | A listed uptime check, as the API's UptimeCheckConfig.
uptimeJson :: Text.Text -> Text.Text -> [(Text.Text, Text.Text)] -> Value
uptimeJson host path labels =
    object
        [ "name" .= ("projects/p/uptimeCheckConfigs/api-up-abc" :: Text.Text)
        , "displayName" .= ("api up" :: Text.Text)
        , "monitoredResource" .= object ["type" .= ("uptime_url" :: Text.Text), "labels" .= object ["host" .= host, "project_id" .= ("p" :: Text.Text)]]
        , "httpCheck" .= object ["path" .= path, "port" .= (443 :: Int), "useSsl" .= True, "requestMethod" .= ("GET" :: Text.Text)]
        , "period" .= ("60s" :: Text.Text)
        , "timeout" .= ("10s" :: Text.Text)
        , "selectedRegions" .= (["USA_OREGON", "EUROPE", "ASIA_PACIFIC", "USA_IOWA", "SOUTH_AMERICA", "USA_VIRGINIA"] :: [Text.Text])
        , "userLabels" .= object [(Key.fromText k, String v) | (k, v) <- labels]
        ]

uptimeTests :: [TestTree]
uptimeTests =
    [ testCase "create: uptime-url on the host, explicit port, regions, salmon's label, --quiet" $ do
        let args = argsOf (prepare Uptime.uptimeCommand (Uptime.UptimeCreate uc))
        assertBool (show args) (["monitoring", "uptime", "create", "api up"] `isSubsequenceOf` args)
        assertBool (show args) (["--resource-type", "uptime-url", "--resource-labels", "host=api.example.org,project_id=p"] `isSubsequenceOf` args)
        assertBool (show args) (["--protocol", "https", "--port", "443", "--path", "/healthz", "--period", "1", "--timeout", "10"] `isSubsequenceOf` args)
        assertBool (show args) (["--regions", "asia-pacific,europe,south-america,usa-iowa,usa-oregon,usa-virginia"] `isSubsequenceOf` args)
        assertBool (show args) (["--user-labels", "managed-by=salmon"] `isSubsequenceOf` args)
        assertBool (show args) ("--quiet" `elem` args)
    , testCase "update sets what can change in place; delete is by id" $ do
        let up = argsOf (prepare Uptime.uptimeCommand (Uptime.UptimeUpdate "api-up-abc" uc))
            del = argsOf (prepare Uptime.uptimeCommand (Uptime.UptimeDelete proj "api-up-abc"))
        assertBool (show up) (["monitoring", "uptime", "update", "api-up-abc", "--port", "443", "--path", "/healthz"] `isSubsequenceOf` up)
        assertBool (show up) (["--set-regions"] `isSubsequenceOf` up)
        assertBool (show up) (not ("--protocol" `elem` up))
        assertEqual "" ["monitoring", "uptime", "delete", "api-up-abc", "--quiet", "--project", "p"] del
    , testCase "lookup: failed, absent, owned and matching, path differs, not salmon's, other host" $ do
        let look v = Uptime.lookupUptime uc ExitSuccess (json v) ""
            owned = [("managed-by", "salmon")]
        assertEqual "" (Monitoring.LookupFailed "exit 2: nope") (Uptime.lookupUptime uc (ExitFailure 2) "" "nope\n")
        assertEqual "" Monitoring.Absent (look (toJSONList' []))
        assertEqual "" (Monitoring.Present (Uptime.FoundUptime "projects/p/uptimeCheckConfigs/api-up-abc" True True True)) (look (toJSONList' [uptimeJson "api.example.org" "/healthz" owned]))
        assertEqual "" (Monitoring.Present (Uptime.FoundUptime "projects/p/uptimeCheckConfigs/api-up-abc" True True False)) (look (toJSONList' [uptimeJson "api.example.org" "/other" owned]))
        assertEqual "" (Monitoring.Present (Uptime.FoundUptime "projects/p/uptimeCheckConfigs/api-up-abc" False True True)) (look (toJSONList' [uptimeJson "api.example.org" "/healthz" []]))
        assertEqual "" (Monitoring.Present (Uptime.FoundUptime "projects/p/uptimeCheckConfigs/api-up-abc" True False False)) (look (toJSONList' [uptimeJson "www.example.org" "/healthz" owned]))
    , testCase "check: only an owned, matching check is satisfied" $ do
        let chk v = Uptime.interpretUptimeList uc (ExitSuccess, json (toJSONList' v), "")
        assertEqual "" Success (chk [uptimeJson "api.example.org" "/healthz" [("managed-by", "salmon")]])
        assertBool "" (isFailure (chk [uptimeJson "api.example.org" "/healthz" []]))
        assertBool "" (isFailure (chk [uptimeJson "api.example.org" "/x" [("managed-by", "salmon")]]))
        assertBool "" (isFailure (chk []))
    , testCase "problems: period, timeout, fewer than three regions, path" $ do
        assertEqual "" [] (Uptime.uptimeProblems uc)
        assertEqual "" 4 (length (Uptime.uptimeProblems uc{Uptime.ucPeriodMinutes = 2, Uptime.ucTimeoutSeconds = 90, Uptime.ucRegions = [Uptime.Europe, Uptime.Europe, Uptime.UsaIowa], Uptime.ucPath = "healthz"}))
    , testCase "the node: help, notes, ref by display name" $ do
        let e = only (Uptime.uptimeCheck silent ignoreTrack uc)
        assertEqual "" "uptime check api up on https://api.example.org:443/healthz" e.help
        assertEqual "" ["every 1min, timeout 10s", "regions: asia-pacific,europe,south-america,usa-iowa,usa-oregon,usa-virginia"] e.notes
        assertBool "" (e.ref /= (only (Uptime.uptimeCheck silent ignoreTrack uc{Uptime.ucDisplayName = "other"})).ref)
    ]

-------------------------------------------------------------------------------

dash :: Dashboard.Dashboard
dash = Dashboard.Dashboard proj "load test" (object ["mosaicLayout" .= object ["columns" .= (48 :: Int), "tiles" .= ([] :: [Value])]])

dashJson :: [(Text.Text, Text.Text)] -> Value
dashJson labels =
    object
        [ "name" .= ("projects/123/dashboards/abc-def" :: Text.Text)
        , "displayName" .= ("load test" :: Text.Text)
        , "etag" .= ("e7a9" :: Text.Text)
        , "labels" .= object [(Key.fromText k, String v) | (k, v) <- labels]
        , "mosaicLayout" .= object ["columns" .= (48 :: Int)]
        ]

dashboardTests :: [TestTree]
dashboardTests =
    [ testCase "rendered: the body, display name, owner and fingerprint labels, and the etag on update only" $ do
        let fp = Dashboard.dashboardFingerprint dash
            created = Dashboard.renderDashboard dash Nothing
            updated = Dashboard.renderDashboard dash (Just "e7a9")
        assertEqual
            ""
            ( object
                [ "displayName" .= ("load test" :: Text.Text)
                , "labels" .= object ["managed-by" .= ("salmon" :: Text.Text), "salmon-fingerprint" .= fp]
                , "mosaicLayout" .= object ["columns" .= (48 :: Int), "tiles" .= ([] :: [Value])]
                ]
            )
            created
        assertEqual "" (Just (String "e7a9")) (field "etag" updated)
        assertEqual "" Nothing (field "etag" created)
        assertEqual "" 16 (Text.length fp)
        assertBool "" (fp /= Dashboard.dashboardFingerprint dash{Dashboard.dbBody = object ["gridLayout" .= object []]})
    , testCase "the body goes on stdin, not argv" $ do
        let c = argsOf (prepare Dashboard.dashboardCommand (Dashboard.DashboardsCreate proj))
            u = argsOf (prepare Dashboard.dashboardCommand (Dashboard.DashboardsUpdate proj "abc-def"))
        assertBool (show c) (["monitoring", "dashboards", "create", "--config-from-file", "-"] `isSubsequenceOf` c)
        assertBool (show u) (["monitoring", "dashboards", "update", "abc-def", "--config-from-file", "-"] `isSubsequenceOf` u)
        assertBool (show c) (not (any ("mosaicLayout" `isInfixOf`) c))
    , testCase "lookup and check: owned with the fingerprint, owned and stale, not salmon's" $ do
        let fp = Dashboard.dashboardFingerprint dash
            look v = Dashboard.lookupDashboard dash ExitSuccess (json (toJSONList' v)) ""
            chk v = Dashboard.interpretDashboardList dash (ExitSuccess, json (toJSONList' v), "")
        assertEqual
            ""
            (Monitoring.Present (Dashboard.FoundDashboard "projects/123/dashboards/abc-def" (Just "e7a9") True (Just fp)))
            (look [dashJson [("managed-by", "salmon"), ("salmon-fingerprint", fp)]])
        assertEqual "" Success (chk [dashJson [("managed-by", "salmon"), ("salmon-fingerprint", fp)]])
        assertBool "" (isFailure (chk [dashJson [("managed-by", "salmon"), ("salmon-fingerprint", "0000")]]))
        assertBool "" (isFailure (chk [dashJson [("salmon-fingerprint", fp)]]))
        assertBool "" (isFailure (chk []))
    , testCase "problems: a body setting a field this module owns, or not an object" $ do
        assertEqual "" [] (Dashboard.dashboardProblems dash)
        assertEqual "" 1 (length (Dashboard.dashboardProblems dash{Dashboard.dbBody = object ["etag" .= ("x" :: Text.Text)]}))
        assertEqual "" 1 (length (Dashboard.dashboardProblems dash{Dashboard.dbBody = String "x"}))
    ]
  where
    field :: Text.Text -> Value -> Maybe Value
    field k (Object o) = KeyMap.lookup (Key.fromText k) o
    field _ _ = Nothing

-------------------------------------------------------------------------------

metric :: Logging.LogMetric
metric = Logging.LogMetric proj "api_errors" "5xx from the API" "resource.type=\"cloud_run_revision\" AND httpRequest.status>=500"

metricJson :: Text.Text -> Text.Text -> Value
metricJson desc filt =
    object
        [ "name" .= ("api_errors" :: Text.Text)
        , "description" .= desc
        , "filter" .= filt
        , "metricDescriptor" .= object ["metricKind" .= ("DELTA" :: Text.Text), "valueType" .= ("INT64" :: Text.Text)]
        ]

loggingTests :: [TestTree]
loggingTests =
    [ testCase "the description ends with the marker" $ do
        assertEqual "" "5xx from the API\nsalmon:log-metric:api_errors" (Logging.renderedDescription metric)
        assertEqual "" "salmon:log-metric:api_errors" (Logging.renderedDescription metric{Logging.lmDescription = ""})
    , testCase "create, update and delete argv" $ do
        let c = argsOf (prepare Logging.loggingCommand (Logging.MetricsCreate metric))
            u = argsOf (prepare Logging.loggingCommand (Logging.MetricsUpdate metric))
            d = argsOf (prepare Logging.loggingCommand (Logging.MetricsDelete proj "api_errors"))
        assertBool (show c) (["logging", "metrics", "create", "api_errors", "--description", "5xx from the API\nsalmon:log-metric:api_errors", "--log-filter", Text.unpack metric.lmFilter] `isSubsequenceOf` c)
        assertBool (show u) (["logging", "metrics", "update", "api_errors", "--description"] `isSubsequenceOf` u)
        assertEqual "" ["logging", "metrics", "delete", "api_errors", "--quiet", "--project", "p"] d
    , testCase "lookup and check: marked and matching, marked with another filter, unmarked, absent" $ do
        let look v = Logging.lookupMetric metric ExitSuccess (json (toJSONList' v)) ""
            chk v = Logging.interpretMetricList metric (ExitSuccess, json (toJSONList' v), "")
            mine = Logging.renderedDescription metric
        assertEqual "" (Monitoring.Present (Logging.FoundMetric True True)) (look [metricJson mine metric.lmFilter])
        assertEqual "" (Monitoring.Present (Logging.FoundMetric True False)) (look [metricJson mine "severity>=ERROR"])
        assertEqual "" (Monitoring.Present (Logging.FoundMetric False False)) (look [metricJson "hand made" metric.lmFilter])
        assertEqual "" Monitoring.Absent (look [])
        assertEqual "" Success (chk [metricJson mine metric.lmFilter])
        assertBool "" (isFailure (chk [metricJson "hand made" metric.lmFilter]))
        assertBool "" (isFailure (Logging.interpretMetricList metric (ExitFailure 1, "", "denied")))
    , testCase "bucket retention: describe and update argv, the check reads retentionDays" $ do
        let b = Logging.defaultLogBucket proj
            dsc = argsOf (prepare Logging.loggingCommand (Logging.BucketDescribe b))
            upd = argsOf (prepare Logging.loggingCommand (Logging.BucketSetRetention b 90))
        assertBool (show dsc) (["logging", "buckets", "describe", "_Default", "--location", "global", "--format", "json"] `isSubsequenceOf` dsc)
        assertBool (show upd) (["logging", "buckets", "update", "_Default", "--location", "global", "--retention-days", "90"] `isSubsequenceOf` upd)
        let described n = json (object ["name" .= ("projects/p/locations/global/buckets/_Default" :: Text.Text), "retentionDays" .= (n :: Int), "lifecycleState" .= ("ACTIVE" :: Text.Text)])
        assertEqual "" Success (Logging.interpretBucketDescribe 90 ExitSuccess (described 90) "")
        assertBool "" (isFailure (Logging.interpretBucketDescribe 90 ExitSuccess (described 30) ""))
        assertBool "" (isFailure (Logging.interpretBucketDescribe 90 ExitSuccess "{}" ""))
        assertBool "" (isFailure (Logging.interpretBucketDescribe 90 (ExitFailure 1) "" "NOT_FOUND"))
    , testCase "bucket retention: down does nothing, up refuses an out-of-range value" $ do
        let e = only (Logging.logBucketRetention silent ignoreTrack (Logging.defaultLogBucket proj) 90)
        assertEqual "" "log bucket _Default in global keeps 90 days" e.help
        e.down -- a no-op: no gcloud is run
        r <- try (only (Logging.logBucketRetention silent ignoreTrack (Logging.defaultLogBucket proj) 0)).up
        assertBool "" (either (\(_ :: SomeException) -> True) (const False) r)
    ]

-------------------------------------------------------------------------------

account :: Billing.BillingAccount
account = Billing.BillingAccount "000000-111111-222222"

bud :: Billing.Budget
bud =
    Billing.Budget
        { Billing.bgAccount = account
        , Billing.bgDisplayName = "monthly"
        , Billing.bgUnits = 100
        , Billing.bgCurrency = "EUR"
        , Billing.bgThresholds = [Billing.ThresholdRule 0.5 Billing.CurrentSpend, Billing.ThresholdRule 1 Billing.ForecastedSpend]
        , Billing.bgProject = Just proj
        }

budgetJson :: Value -> Text.Text -> [Value] -> [Text.Text] -> Value
budgetJson units currency rules projects =
    object
        [ "name" .= ("billingAccounts/000000-111111-222222/budgets/b-1" :: Text.Text)
        , "displayName" .= ("monthly" :: Text.Text)
        , "amount" .= object ["specifiedAmount" .= object ["currencyCode" .= currency, "units" .= units]]
        , "thresholdRules" .= rules
        , "budgetFilter" .= object ["projects" .= projects, "creditTypesTreatment" .= ("INCLUDE_ALL_CREDITS" :: Text.Text)]
        ]

rulesOk :: [Value]
rulesOk =
    [ object ["thresholdPercent" .= (1 :: Double), "spendBasis" .= ("FORECASTED_SPEND" :: Text.Text)]
    , object ["thresholdPercent" .= (0.5 :: Double)]
    ]

budgetTests :: [TestTree]
budgetTests =
    [ testCase "amount and threshold arguments: whole units with the currency, fractions" $ do
        assertEqual "" "100EUR" (Billing.budgetAmountArg bud)
        assertEqual "" "percent=0.5,basis=current-spend" (Billing.thresholdArg (Billing.ThresholdRule 0.5 Billing.CurrentSpend))
        assertEqual "" "percent=1.0,basis=forecasted-spend" (Billing.thresholdArg (Billing.ThresholdRule 1 Billing.ForecastedSpend))
        assertEqual "" "percent=0.01,basis=current-spend" (Billing.thresholdArg (Billing.ThresholdRule 0.01 Billing.CurrentSpend))
    , testCase "create, update (rules cleared then added) and delete argv, all --quiet" $ do
        let c = argsOf (prepare Billing.billingCommand (Billing.BudgetsCreate bud))
            u = argsOf (prepare Billing.billingCommand (Billing.BudgetsUpdate "billingAccounts/000000-111111-222222/budgets/b-1" bud))
            d = argsOf (prepare Billing.billingCommand (Billing.BudgetsDelete "billingAccounts/000000-111111-222222/budgets/b-1"))
        assertBool (show c) (["billing", "budgets", "create", "--billing-account", "000000-111111-222222", "--display-name", "monthly", "--budget-amount", "100EUR"] `isSubsequenceOf` c)
        assertBool (show c) (["--threshold-rule", "percent=0.5,basis=current-spend", "--threshold-rule", "percent=1.0,basis=forecasted-spend", "--filter-projects", "projects/p"] `isSubsequenceOf` c)
        assertBool (show u) (["billing", "budgets", "update", "billingAccounts/000000-111111-222222/budgets/b-1", "--budget-amount", "100EUR", "--clear-threshold-rules", "--add-threshold-rule", "percent=0.5,basis=current-spend"] `isSubsequenceOf` u)
        mapM_ (\a -> assertBool (show a) ("--quiet" `elem` a)) [c, u, d]
    , testCase "lookup: the API's string units and project number, a missing basis is current spend" $ do
        let look n v = Billing.lookupBudget bud n ExitSuccess (json (toJSONList' v)) ""
        assertEqual
            ""
            (Monitoring.Present (Billing.FoundBudget "billingAccounts/000000-111111-222222/budgets/b-1" True True True))
            (look (Just "123") [budgetJson (String "100") "EUR" rulesOk ["projects/123"]])
        -- a number for units reads the same
        assertEqual "" Success (Billing.interpretBudgetList bud (Just "123") (ExitSuccess, json (toJSONList' [budgetJson (Number 100) "EUR" rulesOk ["projects/123"]]), ""))
    , testCase "check: amount, currency, rules, project each differ" $ do
        let chk v = Billing.interpretBudgetList bud (Just "123") (ExitSuccess, json (toJSONList' [v]), "")
        assertBool "amount" (isFailure (chk (budgetJson (String "200") "EUR" rulesOk ["projects/123"])))
        assertBool "currency" (isFailure (chk (budgetJson (String "100") "USD" rulesOk ["projects/123"])))
        assertBool "rules" (isFailure (chk (budgetJson (String "100") "EUR" (take 1 rulesOk) ["projects/123"])))
        assertBool "project" (isFailure (chk (budgetJson (String "100") "EUR" rulesOk ["projects/999"])))
        assertBool "absent" (isFailure (Billing.interpretBudgetList bud (Just "123") (ExitSuccess, "[]", "")))
        let found = Billing.lookupBudget bud (Just "123") ExitSuccess (json (toJSONList' [budgetJson (String "100") "USD" rulesOk ["projects/123"]])) ""
        assertEqual "" (Monitoring.Present (Billing.FoundBudget "billingAccounts/000000-111111-222222/budgets/b-1" False True False)) found
    , testCase "an account-wide budget wants no project filter" $ do
        let wide = bud{Billing.bgProject = Nothing}
        assertEqual "" Success (Billing.interpretBudgetList wide Nothing (ExitSuccess, json (toJSONList' [budgetJson (String "100") "EUR" rulesOk []]), ""))
        assertBool "" (isFailure (Billing.interpretBudgetList wide Nothing (ExitSuccess, json (toJSONList' [budgetJson (String "100") "EUR" rulesOk ["projects/123"]]), "")))
        assertBool "" (not ("--filter-projects" `elem` argsOf (prepare Billing.billingCommand (Billing.BudgetsCreate wide))))
    , testCase "problems: amount, currency, name, thresholds" $ do
        assertEqual "" [] (Billing.budgetProblems bud)
        assertEqual "" 4 (length (Billing.budgetProblems bud{Billing.bgUnits = 0, Billing.bgCurrency = "eur", Billing.bgDisplayName = "", Billing.bgThresholds = [Billing.ThresholdRule 0 Billing.CurrentSpend]}))
    , testCase "the node: help names amount and account; notes the rules and scope" $ do
        let e = only (Billing.budget silent ignoreTrack bud)
        assertEqual "" "budget monthly of 100EUR on billing account 000000-111111-222222" e.help
        assertEqual "" ["thresholds: percent=0.5,basis=current-spend percent=1.0,basis=forecasted-spend", "scope: project p"] e.notes
    ]

toJSONList' :: [Value] -> Value
toJSONList' = toJSONList
