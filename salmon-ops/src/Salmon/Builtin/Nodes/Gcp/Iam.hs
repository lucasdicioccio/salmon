{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Iam (
    Principal (..),
    IamBinding (..),
    serviceAccount,
    iamBinding,
    interpretServiceAccountDescribe,
    interpretBindingPolicy,
    CustomRole (..),
    customRole,
    interpretRoleDescribe,
    ServiceAccountKey (..),
    serviceAccountKey,
    Report (..),
    IamCommand (..),
    iamCommand,
) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Directory (doesFileExist, removeFile)
import System.Process.ByteString (readCreateProcessWithExitCode)
import qualified Data.Text.Encoding as Text
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..), skipIfFileExists)
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
        withBinary gcloudTrack iamCommand (ServiceAccountsDelete project accountId) $ \delete ->
            op "gcp-service-account" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates service account", accountId]
                    , ref = mkRef "gcp-service-account" accountId
                    , up = create r'
                    , down = delete r'
                    , check = checkServiceAccount
                    }
  where
    r' = contramap (RunIamCommand (ServiceAccountsCreate project accountId)) r

    checkServiceAccount :: IO CheckResult
    checkServiceAccount = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare iamCommand (ServiceAccountsDescribe project accountId))
                ""
        pure $ interpretServiceAccountDescribe accountId code

-- | The verdict drawn from @gcloud iam service-accounts describe@'s exit
-- code, split out for testability.
interpretServiceAccountDescribe :: Text -> ExitCode -> CheckResult
interpretServiceAccountDescribe _accountId ExitSuccess = Success
interpretServiceAccountDescribe accountId (ExitFailure _) = Failure ("service account not found: " <> accountId)

-- | Grants a role to a principal on a resource.
--
-- The 'iamResource' should be a gcloud resource reference such as a project
-- id, a bucket name (@buckets\/BUCKET_NAME@), or a service account email.
iamBinding :: Reporter Report -> Track' (Binary "gcloud") -> IamBinding -> Op
iamBinding r gcloudTrack binding =
    withBinary gcloudTrack iamCommand (IamPolicyAddBinding binding) $ \add ->
        withBinary gcloudTrack iamCommand (IamPolicyRemoveBinding binding) $ \remove ->
            op "gcp-iam-binding" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["grants", binding.iamRole, "to", renderPrincipal binding.iamPrincipal]
                    , ref = mkRef "gcp-iam-binding" (renderPrincipal binding.iamPrincipal, binding.iamRole, binding.iamResource)
                    , up = add r'
                    , down = remove r'
                    , check = checkBinding
                    }
  where
    r' = contramap (RunIamCommand (IamPolicyAddBinding binding)) r

    checkBinding :: IO CheckResult
    checkBinding = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare iamCommand (IamPolicyGetBinding binding))
                ""
        pure $ interpretBindingPolicy binding code (Text.decodeUtf8 out)

{- | The verdict drawn from @gcloud ... get-iam-policy@'s exit code and
output, split out for testability.

Very simple heuristic: look for the role and the member on nearby lines. A
robust implementation would parse the YAML/JSON policy.
-}
interpretBindingPolicy :: IamBinding -> ExitCode -> Text -> CheckResult
interpretBindingPolicy _binding (ExitFailure n) _outText =
    Failure ("could not read IAM policy (exit " <> Text.pack (show n) <> ")")
interpretBindingPolicy binding ExitSuccess outText =
    if isBindingPresent
        then Success
        else Failure ("binding not present for " <> member <> " with role " <> role)
  where
    member = renderPrincipal binding.iamPrincipal
    role = binding.iamRole
    isBindingPresent =
        let roleLine = "role: " <> role
            memberLine = "- " <> member
         in Text.isInfixOf roleLine outText && Text.isInfixOf memberLine outText

-------------------------------------------------------------------------------

-- | A custom IAM role, defined by a permissions file (YAML or JSON, as
-- @gcloud iam roles create --file@ accepts).
data CustomRole = CustomRole
    { roleId :: Text
    , roleProject :: Project
    , roleDefinitionFile :: FilePath
    }
    deriving (Eq, Show)

{- | Idempotently creates a custom IAM role from a definition file.

Only handles creation, not drift: like
'Salmon.Builtin.Nodes.Gcp.ArtifactRegistry'.'Salmon.Builtin.Nodes.Gcp.ArtifactRegistry.artifactRepository',
'check' only asks whether the role exists, not whether its permissions
still match 'roleDefinitionFile' -- a changed file after the role's first
creation needs an explicit @gcloud iam roles update@ run by hand (or a
future content-aware check, along the lines of
"Salmon.Builtin.Nodes.Filesystem"'s @checkFileContents@).
-}
customRole :: Reporter Report -> Track' (Binary "gcloud") -> CustomRole -> Op
customRole r gcloudTrack role =
    withBinary gcloudTrack iamCommand (RolesCreate role) $ \create ->
        withBinary gcloudTrack iamCommand (RolesDelete role) $ \delete ->
            op "gcp-custom-role" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates custom IAM role", role.roleId]
                    , ref = mkRef "gcp-custom-role" (role.roleProject.projectId, role.roleId)
                    , up = create r'
                    , down = delete r'
                    , check = checkRole
                    }
  where
    r' = contramap (RunIamCommand (RolesCreate role)) r

    checkRole :: IO CheckResult
    checkRole = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare iamCommand (RolesDescribe role))
                ""
        pure $ interpretRoleDescribe role.roleId code

-- | The verdict drawn from @gcloud iam roles describe@'s exit code, split
-- out for testability.
interpretRoleDescribe :: Text -> ExitCode -> CheckResult
interpretRoleDescribe _roleId ExitSuccess = Success
interpretRoleDescribe roleId (ExitFailure _) = Failure ("custom role not found: " <> roleId)

-------------------------------------------------------------------------------

{- | A service-account JSON key, written to a local file the first time
this node's 'up' runs.

Deliberately not idempotent the way most other nodes here are: each
@gcloud iam service-accounts keys create@ call mints a genuinely new key
(GCP allows several live keys per service account, with no "give me the
existing one back" verb), so idempotency instead comes from 'check' asking
whether the local file is already there and skipping if so -- the same
shape as "Salmon.Actions.UpDown".@skipIfFileExists@, and the same shape the
koli provisioning script this was ported from used (@[ ! -e "${keypath}"
]@). A key that gets deleted locally without also being revoked on GCP is
therefore replaced by a /second/, different live key on the next 'up' --
the stale one is orphaned on GCP, not overwritten. 'down' does not revoke
the GCP key (there is no reliable way to recover its key id from just the
local file after the fact); it only removes the local file, so a caller
wanting the key actually revoked has to do so by hand (e.g. @gcloud iam
service-accounts keys list@ against the account, then @... keys delete@).
-}
data ServiceAccountKey = ServiceAccountKey
    { sakProject :: Project
    , sakAccountId :: Text
    , sakPath :: FilePath
    }
    deriving (Eq, Show)

-- | Writes a service-account key to 'sakPath' if it isn't there already.
serviceAccountKey :: Reporter Report -> Track' (Binary "gcloud") -> ServiceAccountKey -> Op
serviceAccountKey r gcloudTrack key =
    withBinary gcloudTrack iamCommand (ServiceAccountKeysCreate key) $ \create ->
        op "gcp-service-account-key" nodeps $ \actions ->
            actions
                { help = Text.unwords ["writes a service account key for", key.sakAccountId, "to", Text.pack key.sakPath]
                , ref = mkRef "gcp-service-account-key" (key.sakProject.projectId, key.sakAccountId, key.sakPath)
                , up = create r'
                , down = removeIfPresent key.sakPath
                , check = skipIfFileExists key.sakPath
                }
  where
    r' = contramap (RunIamCommand (ServiceAccountKeysCreate key)) r

    removeIfPresent :: FilePath -> IO ()
    removeIfPresent path = do
        exists <- doesFileExist path
        when exists (removeFile path)

-------------------------------------------------------------------------------

data IamCommand
    = ServiceAccountsCreate Project Text
    | ServiceAccountsDescribe Project Text
    | ServiceAccountsDelete Project Text
    | IamPolicyAddBinding IamBinding
    | IamPolicyRemoveBinding IamBinding
    | IamPolicyGetBinding IamBinding
    | RolesCreate CustomRole
    | RolesDescribe CustomRole
    | RolesDelete CustomRole
    | ServiceAccountKeysCreate ServiceAccountKey
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
        let (groupArgs, resourceArg, extraArgs) = iamResourceArgs binding.iamResource
         in gcloudProc $
            groupArgs
                <> [ "add-iam-policy-binding"
                   , resourceArg
                   ]
                <> extraArgs
                <> [ "--member"
                   , Text.unpack (renderPrincipal binding.iamPrincipal)
                   , "--role"
                   , Text.unpack binding.iamRole
                   ]
    IamPolicyRemoveBinding binding ->
        let (groupArgs, resourceArg, extraArgs) = iamResourceArgs binding.iamResource
         in gcloudProc $
            groupArgs
                <> [ "remove-iam-policy-binding"
                   , resourceArg
                   ]
                <> extraArgs
                <> [ "--member"
                   , Text.unpack (renderPrincipal binding.iamPrincipal)
                   , "--role"
                   , Text.unpack binding.iamRole
                   ]
    IamPolicyGetBinding binding ->
        let (groupArgs, resourceArg, extraArgs) = iamResourceArgs binding.iamResource
         in gcloudProc $ groupArgs <> ["get-iam-policy", resourceArg] <> extraArgs
    RolesCreate role ->
        gcloudProc $
            withProject role.roleProject
                [ "iam"
                , "roles"
                , "create"
                , Text.unpack role.roleId
                , "--file"
                , role.roleDefinitionFile
                ]
    RolesDescribe role ->
        gcloudProc $
            withProject role.roleProject
                [ "iam"
                , "roles"
                , "describe"
                , Text.unpack role.roleId
                ]
    RolesDelete role ->
        gcloudProc $
            withProject role.roleProject
                [ "iam"
                , "roles"
                , "delete"
                , Text.unpack role.roleId
                , "--quiet"
                ]
    ServiceAccountKeysCreate key ->
        gcloudProc $
            withProject key.sakProject
                [ "iam"
                , "service-accounts"
                , "keys"
                , "create"
                , key.sakPath
                , "--iam-account"
                , Text.unpack key.sakAccountId <> "@" <> Text.unpack key.sakProject.projectId <> ".iam.gserviceaccount.com"
                ]

-- | Maps a resource reference to the gcloud group arguments, the resource
-- argument, and any trailing flags to pass to
-- add\/remove\/get-iam-policy-binding.
--
-- A resource under a regional collection (currently only
-- @artifacts/repositories@) carries its location as a second path segment,
-- e.g. @artifacts\/repositories\/LOCATION\/REPO@, since @gcloud artifacts
-- repositories ... --location=...@ needs it as a flag placed after the verb
-- rather than as part of the resource name the way @secrets@\/@buckets@\/
-- service accounts don't.
iamResourceArgs :: Text -> ([String], String, [String])
iamResourceArgs res
    | Just pid <- Text.stripPrefix "projects/" res =
        (["projects"], Text.unpack pid, [])
    | Just bkt <- Text.stripPrefix "buckets/" res =
        (["storage", "buckets"], Text.unpack bkt, [])
    | Just sa <- Text.stripPrefix "serviceAccounts/" res =
        (["iam", "service-accounts"], Text.unpack sa, [])
    | Just sec <- Text.stripPrefix "secrets/" res =
        (["secrets"], Text.unpack sec, [])
    | Just rest <- Text.stripPrefix "artifacts/repositories/" res
    , (location, repoName) <- Text.breakOn "/" rest
    , Just repo <- Text.stripPrefix "/" repoName =
        (["artifacts", "repositories"], Text.unpack repo, ["--location", Text.unpack location])
    | otherwise =
        -- Default: treat as a project id.
        (["projects"], Text.unpack res, [])
