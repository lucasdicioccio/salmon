{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.CloudRun (
    IngressSetting (..),
    SecretBinding (..),
    renderSecretBinding,
    CloudRunOptions (..),
    defaultCloudRunOptions,
    CloudRunService (..),
    cloudRunService,
    interpretServiceDescribe,
    interpretServicePresence,
    Report (..),
    CloudRunCommand (..),
    cloudRunCommand,
) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject, withRegion)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunCloudRunCommand !CloudRunCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | Ingress settings for a CloudRun service.
data IngressSetting
    = All
    | Internal
    | InternalAndLoadBalancing
    deriving (Eq, Show)

renderIngress :: IngressSetting -> Text
renderIngress All = "all"
renderIngress Internal = "internal"
renderIngress InternalAndLoadBalancing = "internal-and-cloud-load-balancing"

{- | One Secret Manager secret made visible to the container, either as a
file or as an environment variable.

The file form is what credentials want. An environment variable is readable
by anything that can list the process's environment and tends to end up in
logs and crash reports; a mounted file can be read once at start-up and has
a path that is not printed by accident.

Mounting has one wrinkle that has bitten everyone who has done this with
@libpq@: __Cloud Run's secret volumes are read-only and cannot be chmod'ed__,
and libpq refuses a client key whose mode is wider than @0600@. The way
through is to mount somewhere neutral and have the entrypoint copy the
files, which is what "SreBox.Gcp.PostgrestCloudRun" generates.
-}
data SecretBinding
    = -- | mounted at this absolute path
      SecretFile FilePath Text Text
    | -- | injected as this environment variable
      SecretEnvVar Text Text Text
    deriving (Eq, Show)

-- | gcloud's own @--set-secrets@ syntax: @TARGET=SECRET:VERSION@.
renderSecretBinding :: SecretBinding -> Text
renderSecretBinding (SecretFile path name version) =
    Text.pack path <> "=" <> name <> ":" <> version
renderSecretBinding (SecretEnvVar var name version) =
    var <> "=" <> name <> ":" <> version

{- | The knobs beyond "run this image", grouped so that adding one does not
break every record construction in the tree.
-}
data CloudRunOptions = CloudRunOptions
    { croSecrets :: [SecretBinding]
    , croCpu :: Maybe Text
    , croMemory :: Maybe Text
    , croConcurrency :: Maybe Int
    , croTimeoutSeconds :: Maybe Int
    , croPort :: Maybe Int
    , croAllowUnauthenticated :: Bool
    -- ^ whether the service answers unauthenticated callers. 'False' (the
    -- default) leaves the deploy alone rather than passing
    -- @--no-allow-unauthenticated@, so a service fronted by a load balancer
    -- or governed by an org policy is not fought with on every pass.
    , croInvokerIamCheckDisabled :: Bool
    -- ^ @--no-invoker-iam-check@: the service answers every caller without
    -- consulting IAM at all. This is the way to make a service public under
    -- an organization whose @iam.allowedPolicyMemberDomains@ policy forbids
    -- the @allUsers@ binding that 'croAllowUnauthenticated' asks for: there,
    -- @gcloud run deploy --allow-unauthenticated@ deploys fine, only warns
    -- that the binding was refused, and the service answers 403 to everyone.
    -- Being part of the service's spec rather than a separate IAM write, it
    -- either deploys or fails. 'False' (the default) leaves the deploy alone.
    }
    deriving (Eq, Show)

-- | Nothing set: the same deploy this module made before these knobs existed.
defaultCloudRunOptions :: CloudRunOptions
defaultCloudRunOptions =
    CloudRunOptions
        { croSecrets = []
        , croCpu = Nothing
        , croMemory = Nothing
        , croConcurrency = Nothing
        , croTimeoutSeconds = Nothing
        , croPort = Nothing
        , croAllowUnauthenticated = False
        , croInvokerIamCheckDisabled = False
        }

-- | A CloudRun service.
data CloudRunService = CloudRunService
    { crsName :: Text
    , crsProject :: Project
    , crsRegion :: Region
    , crsImage :: Text
    , crsEnv :: Map Text Text
    , crsServiceAccount :: Text
    , crsIngress :: IngressSetting
    , crsMaxInstances :: Maybe Int
    , crsOptions :: CloudRunOptions
    }
    deriving (Eq, Show)

-- | Deploys a CloudRun service from an image already pushed to Artifact
-- Registry.
cloudRunService :: Reporter Report -> Track' (Binary "gcloud") -> CloudRunService -> Op
cloudRunService r gcloudTrack svc =
    withBinary gcloudTrack cloudRunCommand (RunDeploy svc) $ \deploy ->
        withBinary gcloudTrack cloudRunCommand (RunDelete svc) $ \delete ->
            op "gcp-cloudrun-service" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["deploys CloudRun service", svc.crsName]
                    , ref = mkRef "gcp-cloudrun-service" (svc.crsProject.projectId, svc.crsRegion.regionName, svc.crsName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (deploy r')
                    , -- Presence, not the image: a service running an older
                      -- image than the one declared is still there to delete.
                      -- Checking the image here made `down` skip every service
                      -- whose tag had moved with the code since its deploy.
                      down = Core.downIfPresent (uncurry interpretServicePresence <$> describeService) (delete r')
                    , check = checkService
                    }
  where
    r' = contramap (RunCloudRunCommand (RunDeploy svc)) r

    describeService :: IO (ExitCode, Text)
    describeService = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare cloudRunCommand (RunDescribe svc))
                ""
        pure (code, Text.decodeUtf8 out)

    checkService :: IO CheckResult
    checkService = uncurry (interpretServiceDescribe svc) <$> describeService

{- | The verdict drawn from @gcloud run services describe --format=json@'s
exit code and output, split out for testability.

The service is satisfied only when what it runs is what was declared, in
three respects, each compared /exactly/ against the service's template (the
revision a deploy would create):

* __the image__, by equality: @img:1@ is not @img:10@, which a substring
  match called the same;
* __the service account__;
* __the plain environment variables__, as a set. @gcloud run deploy
  --set-env-vars@ /replaces/ the service's variables, so a variable the
  service has and the declaration does not is drift too, as is one that has
  a different value or is missing. Variables bound from Secret Manager
  ('croSecrets') have no @value@ and are the secrets' business, not
  compared here.

Every drift is named in the 'Failure', which is what makes @run up@ deploy
again. The reason gives names, never an environment variable's value: those
go into reports. Output that is not the JSON this expects is 'Unknown' — the
check ran and could not tell — rather than a 'Failure' that would redeploy
every pass.
-}
interpretServiceDescribe :: CloudRunService -> ExitCode -> Text -> CheckResult
interpretServiceDescribe _ (ExitFailure n) _ =
    Failure ("CloudRun service not found (exit " <> Text.pack (show n) <> ")")
interpretServiceDescribe svc ExitSuccess outText =
    case eitherDecodeStrict (Text.encodeUtf8 outText) of
        Left _ -> Unknown
        Right v -> case templateOf v of
            Nothing -> Unknown
            Just tmpl -> case drifts svc tmpl of
                [] -> Success
                ds -> Failure ("CloudRun service found but differs from what is declared: " <> Text.intercalate "; " ds)

-- | The revision template's @spec@: its first container and its service account.
data Template = Template
    { tmplImage :: Maybe Text
    , tmplServiceAccount :: Maybe Text
    , tmplEnv :: Map Text Text
    -- ^ the plain variables only
    }

templateOf :: Value -> Maybe Template
templateOf v = do
    spec <- field "spec" v >>= field "template" >>= field "spec"
    let container = case field "containers" spec of
            Just (Array cs) | (c : _) <- toList cs -> Just c
            _ -> Nothing
        envEntries = case container >>= field "env" of
            Just (Array es) -> toList es
            _ -> []
        plain e = case (field "name" e, field "valueFrom" e) of
            (Just (String n), Nothing) -> Just (n, maybe "" id (textOf =<< field "value" e))
            _ -> Nothing
    pure
        Template
            { tmplImage = textOf =<< (container >>= field "image")
            , tmplServiceAccount = textOf =<< field "serviceAccountName" spec
            , tmplEnv = Map.fromList (mapMaybe plain envEntries)
            }
  where
    field k (Object o) = KeyMap.lookup (Key.fromText k) o
    field _ _ = Nothing
    textOf (String t) = Just t
    textOf _ = Nothing

drifts :: CloudRunService -> Template -> [Text]
drifts svc t =
    concat
        [ [ "image is " <> shown got <> ", not " <> svc.crsImage
          | got <- [t.tmplImage]
          , got /= Just svc.crsImage
          ]
        , [ "service account is " <> shown got <> ", not " <> svc.crsServiceAccount
          | got <- [t.tmplServiceAccount]
          , got /= Just svc.crsServiceAccount
          ]
        , [ "environment variable " <> k <> " is " <> why
          | (k, why) <- envDrift
          ]
        ]
  where
    shown = maybe "unset" id
    envDrift =
        [(k, "missing") | k <- Map.keys svc.crsEnv, not (Map.member k t.tmplEnv)]
            <> [(k, "not the declared value") | (k, v) <- Map.toList svc.crsEnv, Just got <- [Map.lookup k t.tmplEnv], got /= v]
            <> [(k, "set but not declared") | k <- Map.keys t.tmplEnv, not (Map.member k svc.crsEnv)]

-- | Whether the service exists at all, whatever it runs: what @down@ asks.
interpretServicePresence :: ExitCode -> Text -> CheckResult
interpretServicePresence (ExitFailure n) _ = Failure ("CloudRun service not found (exit " <> Text.pack (show n) <> ")")
interpretServicePresence ExitSuccess _ = Success

-------------------------------------------------------------------------------

data CloudRunCommand
    = RunDeploy CloudRunService
    | RunDescribe CloudRunService
    | RunDelete CloudRunService
    deriving (Show)

{- | The @--set-secrets@ family. One flag carrying every binding, not one
flag per binding: gcloud treats a repeated @--set-secrets@ as a replacement
rather than an addition, so the per-binding form silently deploys with only
the last one.
-}
optionArgs :: CloudRunOptions -> [String]
optionArgs opts =
    concat
        [ if null opts.croSecrets
            then []
            else ["--set-secrets", Text.unpack (Text.intercalate "," (map renderSecretBinding opts.croSecrets))]
        , maybe [] (\v -> ["--cpu", Text.unpack v]) opts.croCpu
        , maybe [] (\v -> ["--memory", Text.unpack v]) opts.croMemory
        , maybe [] (\v -> ["--concurrency", show v]) opts.croConcurrency
        , maybe [] (\v -> ["--timeout", show v]) opts.croTimeoutSeconds
        , maybe [] (\v -> ["--port", show v]) opts.croPort
        , ["--allow-unauthenticated" | opts.croAllowUnauthenticated]
        , ["--no-invoker-iam-check" | opts.croInvokerIamCheckDisabled]
        ]

cloudRunCommand :: Command "gcloud" CloudRunCommand
cloudRunCommand = Command $ \cmd -> case cmd of
    RunDeploy svc ->
        gcloudProc $
            withProject svc.crsProject
                ( withRegion svc.crsRegion
                    ( [ "run"
                      , "deploy"
                      , Text.unpack svc.crsName
                      , "--image"
                      , Text.unpack svc.crsImage
                      , "--service-account"
                      , Text.unpack svc.crsServiceAccount
                      , "--ingress"
                      , Text.unpack (renderIngress svc.crsIngress)
                      ]
                        <> concatMap (\(k, v) -> ["--set-env-vars", Text.unpack k <> "=" <> Text.unpack v]) (Map.toList svc.crsEnv)
                        <> maybe [] (\n -> ["--max-instances", show n]) svc.crsMaxInstances
                        <> optionArgs svc.crsOptions
                    )
                )
    RunDescribe svc ->
        gcloudProc $
            withProject svc.crsProject
                ( withRegion svc.crsRegion
                    [ "run"
                    , "services"
                    , "describe"
                    , Text.unpack svc.crsName
                    , "--format=json"
                    ]
                )
    RunDelete svc ->
        gcloudProc $
            withProject svc.crsProject
                ( withRegion svc.crsRegion
                    [ "run"
                    , "services"
                    , "delete"
                    , Text.unpack svc.crsName
                    , "--quiet"
                    ]
                )
