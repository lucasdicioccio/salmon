{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Iam (
    Principal (..),
    IamBinding (..),
    serviceAccount,
    iamBinding,
    Report (..),
    IamCommand (..),
    iamCommand,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import qualified Data.Text.Encoding as Text
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunIamCommand !IamCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | A GCP IAM principal.
data Principal
    = ServiceAccount Text
    | User Text
    | Group Text
    deriving (Eq, Show)

-- | A binding of a principal to a role on a resource.
data IamBinding = IamBinding
    { iamPrincipal :: Principal
    , iamRole :: Text
    , iamResource :: Text
    }
    deriving (Eq, Show)

renderPrincipal :: Principal -> Text
renderPrincipal (ServiceAccount x) = "serviceAccount:" <> x
renderPrincipal (User x) = "user:" <> x
renderPrincipal (Group x) = "group:" <> x

-- | Creates a service account if it does not exist.
serviceAccount :: Reporter Report -> Track' (Binary "gcloud") -> Project -> Text -> Op
serviceAccount r gcloudTrack project accountId =
    withBinary gcloudTrack iamCommand (ServiceAccountsCreate project accountId) $ \create ->
        withBinary gcloudTrack iamCommand (ServiceAccountsDescribe project accountId) $ \describe ->
            withBinary gcloudTrack iamCommand (ServiceAccountsDelete project accountId) $ \delete ->
                op "gcp-service-account" nodeps $ \actions ->
                    actions
                        { help = Text.unwords ["creates service account", accountId]
                        , ref = mkRef "gcp-service-account" accountId
                        , up = create r'
                        , down = delete r'
                        , check = checkServiceAccount describe
                        }
  where
    r' = contramap (RunIamCommand (ServiceAccountsCreate project accountId)) r

    checkServiceAccount :: (Reporter Binary.Report -> IO ()) -> IO CheckResult
    checkServiceAccount _describe = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare iamCommand (ServiceAccountsDescribe project accountId))
                ""
        pure $ case code of
            ExitSuccess -> Success
            ExitFailure _ -> Failure ("service account not found: " <> accountId)

-- | Grants a role to a principal on a resource.
--
-- The 'iamResource' should be a gcloud resource reference such as a project
-- id, a bucket name (@buckets\/BUCKET_NAME@), or a service account email.
iamBinding :: Reporter Report -> Track' (Binary "gcloud") -> IamBinding -> Op
iamBinding r gcloudTrack binding =
    withBinary gcloudTrack iamCommand (IamPolicyAddBinding binding) $ \add ->
        withBinary gcloudTrack iamCommand (IamPolicyRemoveBinding binding) $ \remove ->
            withBinary gcloudTrack iamCommand (IamPolicyGetBinding binding) $ \getPolicy ->
                op "gcp-iam-binding" nodeps $ \actions ->
                    actions
                        { help = Text.unwords ["grants", binding.iamRole, "to", renderPrincipal binding.iamPrincipal]
                        , ref = mkRef "gcp-iam-binding" (renderPrincipal binding.iamPrincipal, binding.iamRole, binding.iamResource)
                        , up = add r'
                        , down = remove r'
                        , check = checkBinding getPolicy
                        }
  where
    r' = contramap (RunIamCommand (IamPolicyAddBinding binding)) r

    checkBinding :: (Reporter Binary.Report -> IO ()) -> IO CheckResult
    checkBinding _getPolicy = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare iamCommand (IamPolicyGetBinding binding))
                ""
        pure $ case code of
            ExitSuccess ->
                let member = renderPrincipal binding.iamPrincipal
                    role = binding.iamRole
                    outText = Text.decodeUtf8 out
                 in if isBindingPresent member role outText
                        then Success
                        else Failure ("binding not present for " <> member <> " with role " <> role)
            ExitFailure n ->
                Failure ("could not read IAM policy (exit " <> Text.pack (show n) <> ")")

    isBindingPresent :: Text -> Text -> Text -> Bool
    isBindingPresent member role outText =
        -- Very simple heuristic: look for the role and the member on nearby
        -- lines. A robust implementation would parse the YAML/JSON policy.
        let roleLine = "role: " <> role
            memberLine = "- " <> member
         in Text.isInfixOf roleLine outText && Text.isInfixOf memberLine outText

-------------------------------------------------------------------------------

data IamCommand
    = ServiceAccountsCreate Project Text
    | ServiceAccountsDescribe Project Text
    | ServiceAccountsDelete Project Text
    | IamPolicyAddBinding IamBinding
    | IamPolicyRemoveBinding IamBinding
    | IamPolicyGetBinding IamBinding
    deriving (Show)

iamCommand :: Command "gcloud" IamCommand
iamCommand = Command $ \cmd -> case cmd of
    ServiceAccountsCreate project accountId ->
        gcloudProc $
            withProject project
                [ "iam"
                , "service-accounts"
                , "create"
                , Text.unpack accountId
                ]
    ServiceAccountsDescribe project accountId ->
        gcloudProc $
            withProject project
                [ "iam"
                , "service-accounts"
                , "describe"
                , Text.unpack accountId <> "@" <> Text.unpack project.projectId <> ".iam.gserviceaccount.com"
                ]
    ServiceAccountsDelete project accountId ->
        gcloudProc $
            withProject project
                [ "iam"
                , "service-accounts"
                , "delete"
                , Text.unpack accountId <> "@" <> Text.unpack project.projectId <> ".iam.gserviceaccount.com"
                , "--quiet"
                ]
    IamPolicyAddBinding binding ->
        let (groupArgs, resourceArg) = iamResourceArgs binding.iamResource
         in gcloudProc $
            groupArgs
                <> [ "add-iam-policy-binding"
                   , resourceArg
                   , "--member"
                   , Text.unpack (renderPrincipal binding.iamPrincipal)
                   , "--role"
                   , Text.unpack binding.iamRole
                   ]
    IamPolicyRemoveBinding binding ->
        let (groupArgs, resourceArg) = iamResourceArgs binding.iamResource
         in gcloudProc $
            groupArgs
                <> [ "remove-iam-policy-binding"
                   , resourceArg
                   , "--member"
                   , Text.unpack (renderPrincipal binding.iamPrincipal)
                   , "--role"
                   , Text.unpack binding.iamRole
                   ]
    IamPolicyGetBinding binding ->
        let (groupArgs, resourceArg) = iamResourceArgs binding.iamResource
         in gcloudProc $ groupArgs <> ["get-iam-policy", resourceArg]

-- | Maps a resource reference to the gcloud group arguments and the resource
-- argument to pass to add/remove/get-iam-policy-binding.
iamResourceArgs :: Text -> ([String], String)
iamResourceArgs res
    | Just pid <- Text.stripPrefix "projects/" res =
        (["projects"], Text.unpack pid)
    | Just bkt <- Text.stripPrefix "buckets/" res =
        (["storage", "buckets"], Text.unpack bkt)
    | Just sa <- Text.stripPrefix "serviceAccounts/" res =
        (["iam", "service-accounts"], Text.unpack sa)
    | otherwise =
        -- Default: treat as a project id.
        (["projects"], Text.unpack res)
