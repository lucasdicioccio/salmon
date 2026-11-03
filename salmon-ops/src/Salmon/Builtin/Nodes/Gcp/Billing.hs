{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Linking a GCP project to a billing account (@gcloud billing projects
link@) -- the other prerequisite (alongside
"Salmon.Builtin.Nodes.Gcp.ServiceUsage") that a freshly-created project
needs before most other APIs will do anything, since GCP refuses to enable
most billable services on a project with no billing account attached.

Resolving a billing account by its human-facing display name (as the koli
provisioning script this was ported from does, via @gcloud billing accounts
list --filter=displayName:...@) is left to config generation, same as
"Salmon.Builtin.Nodes.Gcp.Core".'Salmon.Builtin.Nodes.Gcp.Core.Project' --
this module only ever takes an already-resolved 'BillingAccount' id.

A __budget__ ('budget') lives on the billing account under a server-assigned
id, so its identity is its display name within the account. It is compared
field by field with what @gcloud billing budgets list@ reads back: the
amount and its currency, the threshold rules (as a set), and the project it
is scoped to (the API answers with the project /number/, which the check
resolves). The currency is part of the declaration because Google refuses a
budget in any currency but the billing account's, and an update cannot
change it. Every command is non-interactive ('gcloudProc' passes
@--quiet@), so a machine whose quota project lacks
@billingbudgets.googleapis.com@ ('budgetsApi') fails with gcloud's message
rather than a prompt to enable it. A budget has no labels and no
description, so there is no ownership marker: @up@ updates a same-named
budget, and @down@ deletes one only when it matches the declaration in full
(what salmon would have written), leaving any other alone.
-}
module Salmon.Builtin.Nodes.Gcp.Billing (
    BillingAccount (..),
    linkBillingAccount,
    interpretBillingDescribe,

    -- * Budgets
    budgetsApi,
    SpendBasis (..),
    ThresholdRule (..),
    Budget (..),
    budget,
    budgetProblems,
    budgetAmountArg,
    thresholdArg,
    FoundBudget (..),
    lookupBudget,
    interpretBudgetList,

    -- * Plumbing
    Report (..),
    BillingCommand (..),
    billingCommand,
) where

import Control.Exception (SomeException, throwIO, try)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import Data.Char (isAsciiUpper)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.IO.Error (userError)
import System.Process.ByteString (readCreateProcessWithExitCode)
import Text.Printf (printf)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), untrackedExec, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc)
import Salmon.Builtin.Nodes.Gcp.Iam (readProjectNumber)
import Salmon.Builtin.Nodes.Gcp.Monitoring (Lookup (..), firstLine, textField)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunBillingCommand !BillingCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

{- | A GCP billing account id, e.g. @XXXXXX-XXXXXX-XXXXXX@ -- bare, without
the @billingAccounts/@ resource-name prefix @gcloud billing accounts list@
returns it with, the same convention
"Salmon.Builtin.Nodes.Gcp.Core".'Salmon.Builtin.Nodes.Gcp.Core.Project'
uses for a bare project id.
-}
newtype BillingAccount = BillingAccount {billingAccountId :: Text}
    deriving (Eq, Ord, Show)

-- | Idempotently links a project to a billing account.
linkBillingAccount :: Reporter Report -> Track' (Binary "gcloud") -> Project -> BillingAccount -> Op
linkBillingAccount r gcloudTrack project account =
    withBinary gcloudTrack billingCommand (ProjectsLink project account) $ \link ->
        withBinary gcloudTrack billingCommand (ProjectsUnlink project) $ \unlink ->
            op "gcp-billing-link" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["links project", project.projectId, "to billing account", account.billingAccountId]
                    , ref = mkRef "gcp-billing-link" project.projectId
                    , up = link r'
                    , down = unlink r'
                    , check = checkLink
                    }
  where
    r' = contramap (RunBillingCommand (ProjectsLink project account)) r

    checkLink :: IO CheckResult
    checkLink = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare billingCommand (ProjectsDescribe project))
                ""
        pure $ interpretBillingDescribe account code (Text.decodeUtf8 out)

{- | The verdict drawn from @gcloud billing projects describe@'s exit code
and output, split out for testability. Plain (YAML-ish) output is used
rather than @--format=json@ so this stays a substring check, the same
shape as "Salmon.Builtin.Nodes.Gcp.Iam".@interpretBindingPolicy@.
-}
interpretBillingDescribe :: BillingAccount -> ExitCode -> Text -> CheckResult
interpretBillingDescribe _account (ExitFailure n) _outText =
    Failure ("could not describe project billing (exit " <> Text.pack (show n) <> ")")
interpretBillingDescribe account ExitSuccess outText =
    if accountLine `Text.isInfixOf` outText && enabledLine `Text.isInfixOf` outText
        then Success
        else Failure ("project not linked to billing account " <> account.billingAccountId)
  where
    accountLine = "billingAccountName: billingAccounts/" <> account.billingAccountId
    enabledLine = "billingEnabled: true"

-------------------------------------------------------------------------------
-- Budgets

-- | The API the budget commands need enabled, on the project gcloud bills
-- its quota to.
budgetsApi :: Text
budgetsApi = "billingbudgets.googleapis.com"

-- | What a threshold is compared with.
data SpendBasis = CurrentSpend | ForecastedSpend
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Notify when spend passes this fraction of the amount (0.5 is half).
data ThresholdRule = ThresholdRule
    { trFraction :: Double
    , trBasis :: SpendBasis
    }
    deriving (Eq, Show)

data Budget = Budget
    { bgAccount :: BillingAccount
    , bgDisplayName :: Text
    -- ^ the identity within the account; at most 60 characters
    , bgUnits :: Integer
    -- ^ a whole amount, in 'bgCurrency'
    , bgCurrency :: Text
    -- ^ ISO 4217, e.g. @EUR@: must be the billing account's currency
    , bgThresholds :: [ThresholdRule]
    , bgProject :: Maybe Project
    -- ^ 'Nothing': everything the account pays for
    }
    deriving (Eq, Show)

-- | What @up@ refuses before any command.
budgetProblems :: Budget -> [Text]
budgetProblems b =
    ["the amount must be positive" | b.bgUnits <= 0]
        <> ["the currency must be three capital letters (ISO 4217)" | Text.length b.bgCurrency /= 3 || not (Text.all isAsciiUpper b.bgCurrency)]
        <> ["the display name must be 1 to 60 characters" | Text.null b.bgDisplayName || Text.length b.bgDisplayName > 60]
        <> ["a threshold must be a positive fraction" | any (\t -> t.trFraction <= 0) b.bgThresholds]

-- | @--budget-amount@'s value: whole units and the currency, e.g. @100EUR@.
-- (gcloud reads the digits after a decimal point as nanos, so @100.5@ would
-- be 100 and 5 nanos: whole units only.)
budgetAmountArg :: Budget -> String
budgetAmountArg b = show b.bgUnits <> Text.unpack b.bgCurrency

-- | One threshold as @--threshold-rule@ and @--add-threshold-rule@ take it,
-- e.g. @percent=0.5,basis=current-spend@.
thresholdArg :: ThresholdRule -> String
thresholdArg t = "percent=" <> fraction <> ",basis=" <> basisFlag t.trBasis
  where
    fraction = case reverse (dropWhile (== '0') (reverse (printf "%.6f" t.trFraction))) of
        s | not (null s) && last s == '.' -> s <> "0"
        s -> s

basisFlag :: SpendBasis -> String
basisFlag CurrentSpend = "current-spend"
basisFlag ForecastedSpend = "forecasted-spend"

basisApi :: SpendBasis -> Text
basisApi CurrentSpend = "CURRENT_SPEND"
basisApi ForecastedSpend = "FORECASTED_SPEND"

{- | A budget on a billing account. @up@ creates it, or updates a same-named
one whose amount, thresholds or project differ (another currency, or a
project scope to be cleared, cannot be updated and is refused); @down@
deletes one that matches the declaration.
-}
budget :: Reporter Report -> Track' (Binary "gcloud") -> Budget -> Op
budget r gcloudTrack b =
    withBinary gcloudTrack billingCommand (BudgetsList b.bgAccount) $ \_list ->
        op "gcp-billing-budget" nodeps $ \actions ->
            actions
                { help = Text.unwords ["budget", b.bgDisplayName, "of", Text.pack (budgetAmountArg b), "on billing account", b.bgAccount.billingAccountId]
                , notes =
                    [ "thresholds: " <> Text.intercalate " " [Text.pack (thresholdArg t) | t <- b.bgThresholds]
                    , "scope: " <> maybe "every project of the account" (\p -> "project " <> p.projectId) b.bgProject
                    ]
                , ref = mkRef "gcp-billing-budget" (b.bgAccount.billingAccountId, b.bgDisplayName)
                , up = upBudget
                , down = downBudget
                , check = checkBudget
                }
  where
    run :: BillingCommand -> IO ()
    run cmd = untrackedExec billingCommand cmd "" (contramap (RunBillingCommand cmd) r)

    list :: IO (ExitCode, ByteString, ByteString)
    list = readCreateProcessWithExitCode (prepare billingCommand (BudgetsList b.bgAccount)) ""

    refuse :: Text -> IO a
    refuse why = throwIO (userError (Text.unpack ("budget " <> b.bgDisplayName <> ": " <> why)))

    -- the number the API names a scoped budget's project by
    number :: IO (Maybe Text)
    number = traverse readProjectNumber b.bgProject

    checkBudget :: IO CheckResult
    checkBudget = do
        resolved <- try number
        case resolved of
            Left (e :: SomeException) -> pure (Failure ("cannot resolve the budget's project number: " <> Text.pack (show e)))
            Right n -> interpretBudgetList b n <$> list

    upBudget = do
        case budgetProblems b of
            [] -> pure ()
            problems -> refuse (Text.intercalate "; " problems)
        n <- number
        (code, out, err) <- list
        case lookupBudget b n code out err of
            LookupFailed why -> refuse ("listing budgets failed: " <> why)
            Absent -> run (BudgetsCreate b)
            Present found
                | found.fbMatches -> pure ()
                | not found.fbSameCurrency -> refuse "exists in another currency, which an update cannot change; delete it to recreate it"
                | b.bgProject == Nothing && not found.fbSameProjects -> refuse "exists scoped to projects, which an update cannot clear; delete it to recreate it"
                | otherwise -> run (BudgetsUpdate found.fbName b)

    downBudget = do
        resolved <- try number
        case resolved of
            Left (_ :: SomeException) -> pure ()
            Right n -> do
                (code, out, err) <- list
                case lookupBudget b n code out err of
                    Present found | found.fbMatches -> run (BudgetsDelete found.fbName)
                    _ -> pure ()

data FoundBudget = FoundBudget
    { fbName :: Text
    -- ^ @billingAccounts\/A\/budgets\/ID@
    , fbSameCurrency :: Bool
    , fbSameProjects :: Bool
    , fbMatches :: Bool
    -- ^ amount, currency, thresholds and project as declared
    }
    deriving (Eq, Show)

{- | Pure: what @gcloud billing budgets list --format json@ said about the
declared budget, given its project's number when it is scoped to one. An
amount with @nanos@ is not a whole amount and so differs; a threshold with
no @spendBasis@ is on current spend.
-}
lookupBudget :: Budget -> Maybe Text -> ExitCode -> ByteString -> ByteString -> Lookup FoundBudget
lookupBudget b projectNumber code out err =
    case code of
        ExitFailure n -> LookupFailed (Text.pack ("exit " <> show n) <> firstLine err)
        ExitSuccess -> case Aeson.decodeStrict' out of
            Just (Array items) | (item : _) <- filter named (toList items) ->
                let sameCurrency = textAt ["amount", "specifiedAmount", "currencyCode"] item == Just b.bgCurrency
                    units = integerAt ["amount", "specifiedAmount", "units"] item
                    nanos = fromMaybe 0 (integerAt ["amount", "specifiedAmount", "nanos"] item)
                    projects = case at ["budgetFilter", "projects"] item of
                        Just (Array ps) -> mapMaybe asText (toList ps)
                        _ -> []
                    sameProjects = case b.bgProject of
                        Nothing -> null projects
                        Just p -> projects `elem` ([["projects/" <> p.projectId]] <> [["projects/" <> n] | Just n <- [projectNumber]])
                    liveRules = case at ["thresholdRules"] item of
                        Just (Array rs) -> mapMaybe rule (toList rs)
                        _ -> []
                    sameRules = sameSet liveRules [(basisApi t.trBasis, t.trFraction) | t <- b.bgThresholds]
                 in Present
                        FoundBudget
                            { fbName = fromMaybe "" (textField "name" item)
                            , fbSameCurrency = sameCurrency
                            , fbSameProjects = sameProjects
                            , fbMatches = sameCurrency && units == Just b.bgUnits && nanos == 0 && sameRules && sameProjects
                            }
            Just (Array _) -> Absent
            _ -> LookupFailed "unparseable list output"
  where
    named item = textField "displayName" item == Just b.bgDisplayName
    rule v = do
        pct <- at ["thresholdPercent"] v >>= asDouble
        pure (fromMaybe "CURRENT_SPEND" (textAt ["spendBasis"] v), pct)
    sameSet xs ys =
        length xs == length ys
            && and (zipWith close (sort xs) (sort ys))
    close (b1, f1) (b2, f2) = b1 == b2 && abs (f1 - f2) < 1e-6

interpretBudgetList :: Budget -> Maybe Text -> (ExitCode, ByteString, ByteString) -> CheckResult
interpretBudgetList b projectNumber (code, out, err) =
    case lookupBudget b projectNumber code out err of
        LookupFailed why -> Failure ("cannot list budgets: " <> why)
        Absent -> Failure ("no budget named " <> b.bgDisplayName)
        Present found
            | found.fbMatches -> Success
            | otherwise -> Failure ("budget " <> b.bgDisplayName <> " differs from its declaration")

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

textAt :: [Text] -> Value -> Maybe Text
textAt ks v = at ks v >>= asText

asDouble :: Value -> Maybe Double
asDouble v@(Number _) | Aeson.Success d <- Aeson.fromJSON v = Just d
asDouble _ = Nothing

-- | int64 fields are JSON strings in the API's own JSON and may be numbers
-- once gcloud has rendered them: either.
integerAt :: [Text] -> Value -> Maybe Integer
integerAt ks v = case at ks v of
    Just n@(Number _) | Aeson.Success i <- Aeson.fromJSON n -> Just i
    Just (String t) | [(i, "")] <- reads (Text.unpack t) -> Just i
    _ -> Nothing

-------------------------------------------------------------------------------

data BillingCommand
    = ProjectsLink Project BillingAccount
    | ProjectsDescribe Project
    | ProjectsUnlink Project
    | BudgetsList BillingAccount
    | BudgetsCreate Budget
    | -- | update the budget with this resource name
      BudgetsUpdate Text Budget
    | -- | delete the budget with this resource name
      BudgetsDelete Text
    deriving (Show)

billingCommand :: Command "gcloud" BillingCommand
billingCommand = Command $ \cmd -> case cmd of
    ProjectsLink project account ->
        gcloudProc
            [ "billing"
            , "projects"
            , "link"
            , Text.unpack project.projectId
            , "--billing-account"
            , Text.unpack account.billingAccountId
            ]
    ProjectsDescribe project ->
        gcloudProc
            [ "billing"
            , "projects"
            , "describe"
            , Text.unpack project.projectId
            ]
    ProjectsUnlink project ->
        gcloudProc
            [ "billing"
            , "projects"
            , "unlink"
            , Text.unpack project.projectId
            ]
    BudgetsList account ->
        gcloudProc
            ["billing", "budgets", "list", "--billing-account", Text.unpack account.billingAccountId, "--format", "json"]
    BudgetsCreate b ->
        gcloudProc $
            [ "billing"
            , "budgets"
            , "create"
            , "--billing-account"
            , Text.unpack b.bgAccount.billingAccountId
            , "--display-name"
            , Text.unpack b.bgDisplayName
            , "--budget-amount"
            , budgetAmountArg b
            ]
                <> concat [["--threshold-rule", thresholdArg t] | t <- b.bgThresholds]
                <> projectFilter b
    BudgetsUpdate name b ->
        gcloudProc $
            ["billing", "budgets", "update", Text.unpack name, "--budget-amount", budgetAmountArg b]
                -- cleared, then each declared rule added: a replace
                <> ["--clear-threshold-rules"]
                <> concat [["--add-threshold-rule", thresholdArg t] | t <- b.bgThresholds]
                <> projectFilter b
    BudgetsDelete name ->
        gcloudProc ["billing", "budgets", "delete", Text.unpack name]
  where
    projectFilter :: Budget -> [String]
    projectFilter b = case b.bgProject of
        Nothing -> []
        Just p -> ["--filter-projects", "projects/" <> Text.unpack p.projectId]
