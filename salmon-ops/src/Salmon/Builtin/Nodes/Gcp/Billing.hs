{-# LANGUAGE OverloadedStrings #-}

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
-}
module Salmon.Builtin.Nodes.Gcp.Billing (
    BillingAccount (..),
    linkBillingAccount,
    interpretBillingDescribe,
    Report (..),
    BillingCommand (..),
    billingCommand,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc)
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

data BillingCommand
    = ProjectsLink Project BillingAccount
    | ProjectsDescribe Project
    | ProjectsUnlink Project
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
