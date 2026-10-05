{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Storage (
    Bucket (..),
    bucket,
    interpretBucketDescribe,

    -- * Access: a bucket-level IAM binding
    Member (..),
    renderMember,
    BucketIamBinding (..),
    bucketIamBinding,
    bindingProblems,
    interpretBucketPolicy,

    -- * Lifecycle rules
    LifecycleRule (..),
    LifecycleAction (..),
    LifecycleCondition (..),
    noCondition,
    expireAfterDays,
    BucketLifecycle (..),
    bucketLifecycle,
    lifecycleProblems,
    renderLifecycle,
    interpretLifecycleDescribe,

    -- * Website settings
    BucketWebsite (..),
    bucketWebsite,
    websiteProblems,
    interpretWebsiteDescribe,
    Report (..),
    StorageCommand (..),
    storageCommand,
) where

import Control.Exception (bracket, throwIO)
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.IO.Exception (ExitCode (..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

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
    = RunStorageCommand !StorageCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | A Google Cloud Storage bucket.
data Bucket = Bucket
    { bucketName :: Text
    , bucketProject :: Project
    , bucketLocation :: Region
    , bucketUniformBucketLevelAccess :: Bool
    }
    deriving (Eq, Show)

-- | Idempotently creates a GCS bucket.
--
-- * 'up': create the bucket if it does not exist.
-- * 'down': delete the bucket.
-- * 'check': describe the bucket and report 'Success' if it exists.
bucket :: Reporter Report -> Track' (Binary "gcloud") -> Bucket -> Op
bucket r gcloudTrack bkt =
    withBinary gcloudTrack storageCommand (BucketsCreate bkt) $ \create ->
        withBinary gcloudTrack storageCommand (BucketsDelete bkt) $ \delete ->
            op "gcp-bucket" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates GCS bucket", bkt.bucketName]
                    , ref = mkRef "gcp-bucket" bkt.bucketName
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create r')
                    , down = Core.downIfPresent checkBucket (delete r')
                    , check = checkBucket
                    }
  where
    r' = contramap (RunStorageCommand (BucketsCreate bkt)) r

    checkBucket :: IO CheckResult
    checkBucket = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare storageCommand (BucketsDescribe bkt))
                ""
        pure $ interpretBucketDescribe bkt.bucketName code

-- | The verdict drawn from @gcloud storage buckets describe@'s exit code,
-- split out for testability.
interpretBucketDescribe :: Text -> ExitCode -> CheckResult
interpretBucketDescribe _name ExitSuccess = Success
interpretBucketDescribe name (ExitFailure _) = Failure ("bucket not found: " <> name)

-------------------------------------------------------------------------------
-- Access

{- | Who a bucket-level binding grants a role to. 'AllUsers' is the public:
with @roles\/storage.legacyObjectReader@ it is "anyone may read an object
whose name they know" without the listing that @roles\/storage.objectViewer@
also grants, which is what a static site's bucket wants.
-}
data Member
    = AllUsers
    | AllAuthenticatedUsers
    | ServiceAccountMember Text
    | UserMember Text
    | GroupMember Text
    | -- | Any other member, spelled as IAM spells it (@domain:example.org@).
      OtherMember Text
    deriving (Eq, Ord, Show)

-- | A member as @--member@ and a policy's @members@ spell it.
renderMember :: Member -> Text
renderMember AllUsers = "allUsers"
renderMember AllAuthenticatedUsers = "allAuthenticatedUsers"
renderMember (ServiceAccountMember email) = "serviceAccount:" <> Text.strip email
renderMember (UserMember email) = "user:" <> Text.strip email
renderMember (GroupMember email) = "group:" <> Text.strip email
renderMember (OtherMember raw) = Text.strip raw

-- | One role granted to one member on one bucket.
data BucketIamBinding = BucketIamBinding
    { bindingBucket :: Bucket
    , bindingRole :: Text
    , bindingMember :: Member
    }
    deriving (Eq, Show)

{- | One unconditional IAM binding on a bucket's own policy.

* 'check': reads @get-iam-policy --format json@ and looks for the member
  under the role ('interpretBucketPolicy').
* 'up': @add-iam-policy-binding@, which is a set insert and so idempotent;
  retried like any grant that may name a service account made a moment ago.
* 'down': @remove-iam-policy-binding@, only when the check finds the binding.

The node has no dependency of its own: the caller puts the bucket (and the
service account, if one is named) underneath. Granting to 'AllUsers' is
refused by GCP on a bucket or organisation enforcing public access
prevention; that refusal is @up@ failing, nothing here anticipates it.
-}
bucketIamBinding :: Reporter Report -> Track' (Binary "gcloud") -> BucketIamBinding -> Op
bucketIamBinding r gcloudTrack b =
    withBinary gcloudTrack storageCommand (BucketsAddIamBinding b) $ \add ->
        withBinary gcloudTrack storageCommand (BucketsRemoveIamBinding b) $ \remove ->
            op "gcp-bucket-iam-binding" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["grants", b.bindingRole, "on GCS bucket", b.bindingBucket.bucketName, "to", member]
                    , ref = mkRef "gcp-bucket-iam-binding" (b.bindingBucket.bucketName, Text.strip b.bindingRole, member)
                    , up = do
                        refuse what (bindingProblems b)
                        Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (add (rFor (BucketsAddIamBinding b)))
                    , down = Core.downIfPresent checkBinding (remove (rFor (BucketsRemoveIamBinding b)))
                    , check = checkBinding
                    }
  where
    member = renderMember b.bindingMember
    what = "binding on bucket " <> b.bindingBucket.bucketName
    rFor cmd = contramap (RunStorageCommand cmd) r

    checkBinding :: IO CheckResult
    checkBinding = uncurry (interpretBucketPolicy b) <$> readStorage (BucketsGetIamPolicy b.bindingBucket)

-- | Why a declared binding cannot be granted, if it cannot.
bindingProblems :: BucketIamBinding -> [Text]
bindingProblems b =
    ["it names no role" | Text.null (Text.strip b.bindingRole)]
        <> ["it names no member" | Text.null member || Text.isSuffixOf ":" member]
  where
    member = renderMember b.bindingMember

{- | The verdict drawn from @gcloud storage buckets get-iam-policy --format
json@. A binding carrying a @condition@ is not this node's: only an
unconditional one counts. Members are compared as IAM spells them, without
regard to case.
-}
interpretBucketPolicy :: BucketIamBinding -> ExitCode -> ByteString -> CheckResult
interpretBucketPolicy b code out
    | problems@(_ : _) <- bindingProblems b = Failure (problemText ("binding on bucket " <> name) problems)
    | ExitFailure n <- code =
        Failure ("could not read the IAM policy of bucket " <> name <> " (exit " <> Text.pack (show n) <> ")")
    | otherwise = case eitherDecodeStrict' out of
        Right (Object policy)
            | any grants (arrayAt "bindings" policy) -> Success
            | otherwise -> Failure (Text.unwords ["bucket", name, "does not grant", b.bindingRole, "to", member])
        -- it answered, and what it said is not a policy: cannot tell
        _ -> Unknown
  where
    name = b.bindingBucket.bucketName
    member = renderMember b.bindingMember
    grants (Object binding) =
        KeyMap.lookup "role" binding == Just (String (Text.strip b.bindingRole))
            && unconditional binding
            && any sameMember (arrayAt "members" binding)
    grants _ = False
    unconditional binding = case KeyMap.lookup "condition" binding of
        Nothing -> True
        Just Null -> True
        Just _ -> False
    sameMember (String m) = Text.toCaseFold m == Text.toCaseFold member
    sameMember _ = False

-------------------------------------------------------------------------------
-- Lifecycle

-- | What a lifecycle rule does to the objects its condition matches.
data LifecycleAction
    = Delete
    | SetStorageClass Text
    | AbortIncompleteMultipartUpload
    deriving (Eq, Ord, Show)

{- | Which objects a rule matches; every field set must hold. The fields are
the storage API's own (@age@ in days, @createdBefore@ a @YYYY-MM-DD@ date).
-}
data LifecycleCondition = LifecycleCondition
    { conditionAge :: Maybe Int
    , conditionCreatedBefore :: Maybe Text
    , conditionIsLive :: Maybe Bool
    , conditionNumNewerVersions :: Maybe Int
    , conditionDaysSinceNoncurrentTime :: Maybe Int
    , conditionMatchesStorageClass :: [Text]
    , conditionMatchesPrefix :: [Text]
    , conditionMatchesSuffix :: [Text]
    }
    deriving (Eq, Ord, Show)

-- | The condition with nothing set, to fill in with record update.
noCondition :: LifecycleCondition
noCondition = LifecycleCondition Nothing Nothing Nothing Nothing Nothing [] [] []

data LifecycleRule = LifecycleRule
    { ruleAction :: LifecycleAction
    , ruleCondition :: LifecycleCondition
    }
    deriving (Eq, Ord, Show)

-- | Delete objects older than this many days: a backups bucket's usual rule.
expireAfterDays :: Int -> LifecycleRule
expireAfterDays days = LifecycleRule Delete noCondition{conditionAge = Just days}

-- | The whole lifecycle configuration of a bucket: these rules and no other.
data BucketLifecycle = BucketLifecycle
    { lifecycleBucket :: Bucket
    , lifecycleRules :: [LifecycleRule]
    }
    deriving (Eq, Show)

{- | A bucket's lifecycle rules, as data.

A bucket has one lifecycle configuration, so the node is keyed on the bucket
and the declared rules /replace/ whatever is there, rules made by hand
included; an empty list is "no rule".

* 'check': reads @describe --format json@ and compares the rule sets, order
  aside ('interpretLifecycleDescribe').
* 'up': writes 'renderLifecycle' to a temporary file and runs @update
  --lifecycle-file@ (@--clear-lifecycle@ for no rule). A "set" verb.
* 'down': @--clear-lifecycle@, only when the check finds the declared rules.

The node has no dependency of its own: the caller puts the bucket underneath.
-}
bucketLifecycle :: Reporter Report -> Track' (Binary "gcloud") -> BucketLifecycle -> Op
bucketLifecycle r gcloudTrack lc =
    withBinary gcloudTrack storageCommand (BucketsClearLifecycle bkt) $ \clear ->
        op "gcp-bucket-lifecycle" nodeps $ \actions ->
            actions
                { help = Text.unwords ["sets the lifecycle rules of GCS bucket", bkt.bucketName]
                , notes = ["lifecycle " <> decodeLenient (renderLifecycle lc.lifecycleRules)]
                , ref = mkRef "gcp-bucket-lifecycle" bkt.bucketName
                , up = bringUp
                , down = Core.downIfPresent checkLifecycle (clear (rFor (BucketsClearLifecycle bkt)))
                , check = checkLifecycle
                }
  where
    bkt = lc.lifecycleBucket
    rFor cmd = contramap (RunStorageCommand cmd) r

    checkLifecycle :: IO CheckResult
    checkLifecycle = uncurry (interpretLifecycleDescribe lc) <$> readStorage (BucketsDescribeJson bkt)

    bringUp :: IO ()
    bringUp = do
        refuse ("lifecycle of bucket " <> bkt.bucketName) (lifecycleProblems lc)
        case lc.lifecycleRules of
            [] -> run (BucketsClearLifecycle bkt)
            rules -> withLifecycleFile rules $ \path -> run (BucketsSetLifecycle bkt path)

    run cmd = Binary.untrackedExec storageCommand cmd "" (rFor cmd)

-- | The rules in a file @--lifecycle-file@ can read, removed afterwards.
withLifecycleFile :: [LifecycleRule] -> (FilePath -> IO a) -> IO a
withLifecycleFile rules act = do
    tmp <- getTemporaryDirectory
    bracket
        ( do
            (path, h) <- openTempFile tmp "salmon-lifecycle.json"
            ByteString.hPut h (renderLifecycle rules)
            hClose h
            pure path
        )
        removeFile
        act

-- | Why declared rules cannot be set, if they cannot.
lifecycleProblems :: BucketLifecycle -> [Text]
lifecycleProblems lc = concat (zipWith ruleProblems [1 :: Int ..] lc.lifecycleRules)
  where
    ruleProblems :: Int -> LifecycleRule -> [Text]
    ruleProblems n rule =
        map
            (\why -> "rule " <> Text.pack (show n) <> " " <> why)
            ( ["has no condition, and would apply to every object" | rule.ruleCondition == noCondition]
                <> ["has a negative " <> name | (name, Just v) <- counts rule.ruleCondition, v < 0]
                <> ["sets no storage class" | SetStorageClass c <- [rule.ruleAction], Text.null (Text.strip c)]
            )
    counts :: LifecycleCondition -> [(Text, Maybe Int)]
    counts c =
        [ ("age", c.conditionAge)
        , ("numNewerVersions", c.conditionNumNewerVersions)
        , ("daysSinceNoncurrentTime", c.conditionDaysSinceNoncurrentTime)
        ]

-- | The rules as the storage API's lifecycle document: @{"rule": [...]}@.
renderLifecycle :: [LifecycleRule] -> ByteString
renderLifecycle rules = LByteString.toStrict (encode (object ["rule" .= map ruleValue rules]))

ruleValue :: LifecycleRule -> Value
ruleValue rule = object ["action" .= action, "condition" .= condition]
  where
    action = case rule.ruleAction of
        Delete -> object ["type" .= ("Delete" :: Text)]
        SetStorageClass cls -> object ["type" .= ("SetStorageClass" :: Text), "storageClass" .= Text.strip cls]
        AbortIncompleteMultipartUpload -> object ["type" .= ("AbortIncompleteMultipartUpload" :: Text)]
    c = rule.ruleCondition
    condition =
        object . catMaybes $
            [ ("age" .=) <$> c.conditionAge
            , ("createdBefore" .=) <$> c.conditionCreatedBefore
            , ("isLive" .=) <$> c.conditionIsLive
            , ("numNewerVersions" .=) <$> c.conditionNumNewerVersions
            , ("daysSinceNoncurrentTime" .=) <$> c.conditionDaysSinceNoncurrentTime
            , list "matchesStorageClass" c.conditionMatchesStorageClass
            , list "matchesPrefix" c.conditionMatchesPrefix
            , list "matchesSuffix" c.conditionMatchesSuffix
            ]
    list _ [] = Nothing
    list k vs = Just (k .= vs)

{- | The verdict drawn from @gcloud storage buckets describe --format json@.

The live rules are compared with the declared ones as JSON values, null
members and empty lists dropped and order aside, so a live rule using a
condition this module has no field for is a difference rather than something
overlooked. The configuration is looked for under @lifecycle@ (the API's
name, what @--raw@ prints) and @lifecycle_config@ (gcloud's own).
-}
interpretLifecycleDescribe :: BucketLifecycle -> ExitCode -> ByteString -> CheckResult
interpretLifecycleDescribe lc code out
    | problems@(_ : _) <- lifecycleProblems lc = Failure (problemText ("lifecycle of bucket " <> name) problems)
    | ExitFailure _ <- code = Failure ("bucket not found: " <> name)
    | otherwise = case eitherDecodeStrict' out of
        Right (Object described)
            | live described == wanted -> Success
            | otherwise ->
                Failure
                    ( Text.unwords
                        ["bucket", name, "has", count (live described), "lifecycle rule(s), which are not the", count wanted, "declared"]
                    )
        _ -> Unknown
  where
    name = lc.lifecycleBucket.bucketName
    wanted = canonical (map ruleValue lc.lifecycleRules)
    live described = canonical $ case firstOf ["lifecycle", "lifecycle_config"] described of
        Just (Object config) -> arrayAt "rule" config
        _ -> []
    canonical = sort . map (encode . pruned)
    count = Text.pack . show . length

-- | A JSON value without its null members and empty lists, at any depth.
pruned :: Value -> Value
pruned (Object o) = Object (KeyMap.filter (not . vacant) (KeyMap.map pruned o))
  where
    vacant Null = True
    vacant (Array a) = null a
    vacant _ = False
pruned (Array a) = Array (fmap pruned a)
pruned v = v

-------------------------------------------------------------------------------
-- Website

{- | A bucket's website settings: the object served for a directory-like
path, and the one served for a missing object. They are what a bucket's
website endpoint needs, and what a load balancer's backend bucket honours.
'Nothing' is "not set".
-}
data BucketWebsite = BucketWebsite
    { websiteBucket :: Bucket
    , websiteMainPageSuffix :: Maybe Text
    , websiteNotFoundPage :: Maybe Text
    }
    deriving (Eq, Show)

{- | A bucket's website settings, keyed on the bucket: both are set to what
is declared, a 'Nothing' clearing what is there.

* 'check': reads @describe --format json@ ('interpretWebsiteDescribe').
* 'up': @update --web-main-page-suffix@ \/ @--web-error-page@ (or their
  @--clear-@ forms). A "set" verb.
* 'down': clears both, only when the check finds the declared settings.

The node has no dependency of its own: the caller puts the bucket underneath.
-}
bucketWebsite :: Reporter Report -> Track' (Binary "gcloud") -> BucketWebsite -> Op
bucketWebsite r gcloudTrack w =
    withBinary gcloudTrack storageCommand (BucketsSetWebsite w) $ \set ->
        withBinary gcloudTrack storageCommand (BucketsSetWebsite cleared) $ \clear ->
            op "gcp-bucket-website" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["sets the website settings of GCS bucket", bkt.bucketName]
                    , notes =
                        [ "main page suffix: " <> fromMaybe "(none)" (setting w.websiteMainPageSuffix)
                        , "not-found page: " <> fromMaybe "(none)" (setting w.websiteNotFoundPage)
                        ]
                    , ref = mkRef "gcp-bucket-website" bkt.bucketName
                    , up = do
                        refuse ("website settings of bucket " <> bkt.bucketName) (websiteProblems w)
                        set (rFor (BucketsSetWebsite w))
                    , down = Core.downIfPresent checkWebsite (clear (rFor (BucketsSetWebsite cleared)))
                    , check = checkWebsite
                    }
  where
    bkt = w.websiteBucket
    cleared = BucketWebsite bkt Nothing Nothing
    rFor cmd = contramap (RunStorageCommand cmd) r

    checkWebsite :: IO CheckResult
    checkWebsite = uncurry (interpretWebsiteDescribe w) <$> readStorage (BucketsDescribeJson bkt)

-- | A declared setting, an empty one being none.
setting :: Maybe Text -> Maybe Text
setting m = case Text.strip <$> m of
    Just t | not (Text.null t) -> Just t
    _ -> Nothing

-- | Why declared website settings cannot be set, if they cannot.
websiteProblems :: BucketWebsite -> [Text]
websiteProblems w =
    [ "the main page suffix is the last part of an object's name, and cannot contain a slash"
    | Just s <- [setting w.websiteMainPageSuffix]
    , Text.isInfixOf "/" s
    ]

{- | The verdict drawn from @gcloud storage buckets describe --format json@,
the settings being looked for under @website@ (the API's name, what @--raw@
prints) and @website_config@ (gcloud's own).
-}
interpretWebsiteDescribe :: BucketWebsite -> ExitCode -> ByteString -> CheckResult
interpretWebsiteDescribe w code out
    | problems@(_ : _) <- websiteProblems w = Failure (problemText ("website settings of bucket " <> name) problems)
    | ExitFailure _ <- code = Failure ("bucket not found: " <> name)
    | otherwise = case eitherDecodeStrict' out of
        Right (Object described) ->
            let config = case firstOf ["website", "website_config"] described of
                    Just (Object o) -> o
                    _ -> KeyMap.empty
                differences =
                    catMaybes
                        [ differs "main page suffix" (setting w.websiteMainPageSuffix) (textAt "mainPageSuffix" config)
                        , differs "not-found page" (setting w.websiteNotFoundPage) (textAt "notFoundPage" config)
                        ]
             in case differences of
                    [] -> Success
                    ds -> Failure (Text.unwords ["bucket", name, "has", Text.intercalate ", " ds])
        _ -> Unknown
  where
    name = w.websiteBucket.bucketName
    differs :: Text -> Maybe Text -> Maybe Text -> Maybe Text
    differs label wanted live
        | wanted == live = Nothing
        | otherwise = Just (Text.unwords [label, shown live <> ",", "not", shown wanted])
    shown = fromMaybe "(none)"
    textAt k o = case KeyMap.lookup k o of
        Just (String t) -> setting (Just t)
        _ -> Nothing

-------------------------------------------------------------------------------
-- Shared by the settings nodes

-- | Runs a read-only storage command: its exit code and stdout.
readStorage :: StorageCommand -> IO (ExitCode, ByteString)
readStorage cmd = do
    (code, out, _err) <- readCreateProcessWithExitCode (prepare storageCommand cmd) ""
    pure (code, out)

refuse :: Text -> [Text] -> IO ()
refuse _ [] = pure ()
refuse what problems = throwIO (userError (Text.unpack (problemText what problems)))

problemText :: Text -> [Text] -> Text
problemText what problems = "the " <> what <> " cannot be applied: " <> Text.intercalate "; " problems

arrayAt :: Text -> KeyMap.KeyMap Value -> [Value]
arrayAt k o = case KeyMap.lookup (Key.fromText k) o of
    Just (Array a) -> toList a
    _ -> []

-- | The first of these members that is there and not null.
firstOf :: [Text] -> KeyMap.KeyMap Value -> Maybe Value
firstOf ks o = case [v | k <- ks, Just v <- [KeyMap.lookup (Key.fromText k) o], v /= Null] of
    v : _ -> Just v
    [] -> Nothing

decodeLenient :: ByteString -> Text
decodeLenient = Text.decodeUtf8With TextError.lenientDecode

-------------------------------------------------------------------------------

data StorageCommand
    = BucketsCreate Bucket
    | BucketsDescribe Bucket
    | BucketsDelete Bucket
    | -- | @describe --raw --format json@: the bucket as the storage API spells it.
      BucketsDescribeJson Bucket
    | BucketsGetIamPolicy Bucket
    | BucketsAddIamBinding BucketIamBinding
    | BucketsRemoveIamBinding BucketIamBinding
    | -- | The path is a file holding 'renderLifecycle''s output.
      BucketsSetLifecycle Bucket FilePath
    | BucketsClearLifecycle Bucket
    | BucketsSetWebsite BucketWebsite
    deriving (Show)

storageCommand :: Command "gcloud" StorageCommand
storageCommand = Command $ \cmd -> case cmd of
    BucketsCreate b ->
        gcloudProc $
            withProject b.bucketProject
                [ "storage"
                , "buckets"
                , "create"
                , "gs://" <> Text.unpack b.bucketName
                , "--location"
                , Text.unpack b.bucketLocation.regionName
                ]
                <> if b.bucketUniformBucketLevelAccess then ["--uniform-bucket-level-access"] else []
    BucketsDescribe b ->
        gcloudProc $
            withProject b.bucketProject
                [ "storage"
                , "buckets"
                , "describe"
                , "gs://" <> Text.unpack b.bucketName
                ]
    BucketsDelete b ->
        gcloudProc $
            withProject b.bucketProject
                [ "storage"
                , "buckets"
                , "delete"
                , "gs://" <> Text.unpack b.bucketName
                , "--quiet"
                ]
    BucketsDescribeJson b ->
        gcloudProc $ withProject b.bucketProject ["storage", "buckets", "describe", url b, "--raw", "--format", "json"]
    BucketsGetIamPolicy b ->
        gcloudProc $ withProject b.bucketProject ["storage", "buckets", "get-iam-policy", url b, "--format", "json"]
    BucketsAddIamBinding b -> bindingProc "add-iam-policy-binding" b
    BucketsRemoveIamBinding b -> bindingProc "remove-iam-policy-binding" b
    BucketsSetLifecycle b path ->
        gcloudProc $ withProject b.bucketProject ["storage", "buckets", "update", url b, "--lifecycle-file", path]
    BucketsClearLifecycle b ->
        gcloudProc $ withProject b.bucketProject ["storage", "buckets", "update", url b, "--clear-lifecycle"]
    BucketsSetWebsite w ->
        gcloudProc $
            withProject w.websiteBucket.bucketProject $
                ["storage", "buckets", "update", url w.websiteBucket]
                    <> maybe ["--clear-web-main-page-suffix"] (\s -> ["--web-main-page-suffix", Text.unpack s]) (setting w.websiteMainPageSuffix)
                    <> maybe ["--clear-web-error-page"] (\s -> ["--web-error-page", Text.unpack s]) (setting w.websiteNotFoundPage)
  where
    url :: Bucket -> String
    url b = "gs://" <> Text.unpack b.bucketName
    bindingProc :: String -> BucketIamBinding -> CreateProcess
    bindingProc verb b =
        gcloudProc $
            withProject b.bindingBucket.bucketProject
                [ "storage"
                , "buckets"
                , verb
                , url b.bindingBucket
                , "--member"
                , Text.unpack (renderMember b.bindingMember)
                , "--role"
                , Text.unpack (Text.strip b.bindingRole)
                ]
