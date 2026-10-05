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

    -- * Contents: a local directory published to a bucket
    CacheRule (..),
    OnDown (..),
    BucketContents (..),
    bucketContents,
    contentsProblems,
    contentsDestination,
    RsyncPass (..),
    contentsPasses,
    globRegex,
    Rsync (..),
    contentsRsync,
    emptyingRsync,
    Planned (..),
    parseDryRun,
    interpretContentsDryRun,
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
import Data.Char (isAlphaNum, isAscii, isControl)
import Data.List (inits, nub, sort, (\\))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.IO.Exception (ExitCode (..))
import System.Directory (createDirectory, doesDirectoryExist, getTemporaryDirectory, listDirectory, removeDirectory, removeFile)
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
-- Contents

{- | A @Cache-Control@ value for the objects whose path matches a glob.

The glob is matched against an object's path relative to the published
directory: @*@ and @?@ stay within one path segment, @**@ crosses segments,
anything else is literal. A glob with no slash matches a file name at any
depth (@*.html@ is every HTML file); one with a slash is anchored at the
directory's root (@assets\/**@, @\/index.html@).
-}
data CacheRule = CacheRule
    { cacheGlob :: Text
    , cacheControl :: Text
    }
    deriving (Eq, Ord, Show)

-- | What the node's @down@ does to what it published.
data OnDown
    = {- | Nothing. The objects stay, and so a bucket node underneath cannot be
      deleted by the same pass: GCS refuses to delete a bucket holding objects.
      -}
      LeaveObjects
    | -- | Removes every object under the destination, whoever put it there.
      EmptyDestination
    deriving (Eq, Ord, Show)

-- | A local directory published under a bucket (or a prefix of one).
data BucketContents = BucketContents
    { contentsBucket :: Bucket
    , contentsPrefix :: Text
    -- ^ Where in the bucket, without a leading slash; empty for its root.
    , contentsSource :: FilePath
    -- ^ The directory, read when the node is checked or applied.
    , contentsCacheRules :: [CacheRule]
    -- ^ The first rule whose glob matches an object decides its header.
    , contentsDefaultCacheControl :: Maybe Text
    -- ^ For objects no rule matches; 'Nothing' leaves the header unset.
    , contentsDeleteExtraneous :: Bool
    -- ^ Whether objects under the destination that the directory does not
    -- hold are removed.
    , contentsOnDown :: OnDown
    }
    deriving (Eq, Show)

{- | Publishes a local directory to a bucket with @gcloud storage rsync@,
each object carrying the @Cache-Control@ its path calls for.

@rsync@ takes one @--cache-control@ per invocation and narrows what it looks
at with an @--exclude@ regex only, so the directory is published in
'contentsPasses': one pass per rule, restricted to the objects that rule is
the first to match, and a last one for the rest. An excluded destination
object is not removed by @--delete-unmatched-destination-objects@, so each
pass removes the extraneous objects of its own share and the passes together
those of the whole destination.

* 'check': the same passes with @--dry-run@, read by
  'interpretContentsDryRun'. The comparison is by checksum
  (@--checksums-only@), and a planned mtime update is not a difference, so a
  fresh checkout of unchanged files is satisfied.
* 'up': the passes, in rule order. A failing pass throws.
* 'down': per 'contentsOnDown'. 'EmptyDestination' syncs an empty directory
  over the destination, and does nothing if the bucket is already gone.

The directory is read by gcloud when the node is checked or applied, never
when the graph is built. A missing or empty directory is a 'Failure' for the
check and a throw for @up@: publishing nothing over a site is what a build
that failed would ask for.

The node is keyed on the destination and has no dependency of its own: the
caller puts the bucket, and whatever builds the directory, underneath.

What the check cannot see: a @Cache-Control@ declared differently for bytes
already published. The dry-run compares content, so the new header reaches an
object only when its content next changes.
-}
bucketContents :: Reporter Report -> Track' (Binary "gcloud") -> BucketContents -> Op
bucketContents r gcloudTrack c =
    withBinary gcloudTrack storageCommand (BucketsDescribe bkt) $ \_describe ->
        op "gcp-bucket-contents" nodeps $ \actions ->
            actions
                { help = Text.unwords ["publishes directory", Text.pack c.contentsSource, "to", contentsDestination c]
                , notes =
                    [ "source: " <> Text.pack c.contentsSource
                    , "extraneous objects: " <> (if c.contentsDeleteExtraneous then "removed" else "left")
                    , "on down: " <> onDown
                    ]
                        <> [ "cache-control " <> p.passLabel <> ": " <> fromMaybe "(unset)" p.passCacheControl
                           | p <- contentsPasses c
                           ]
                , ref = mkRef "gcp-bucket-contents" (bkt.bucketName, prefixOf c)
                , up = bringUp
                , down = bringDown
                , check = checkContents
                }
  where
    bkt = c.contentsBucket
    what = "contents of " <> contentsDestination c
    onDown :: Text
    onDown = case c.contentsOnDown of
        LeaveObjects -> "objects left"
        EmptyDestination -> "destination emptied"
    rFor cmd = contramap (RunStorageCommand cmd) r
    run cmd = Binary.untrackedExec storageCommand cmd "" (rFor cmd)

    -- Why the directory cannot be published right now, if it cannot.
    sourceProblems :: IO [Text]
    sourceProblems = do
        exists <- doesDirectoryExist c.contentsSource
        if not exists
            then pure ["the directory " <> Text.pack c.contentsSource <> " does not exist"]
            else do
                entries <- listDirectory c.contentsSource
                pure ["the directory " <> Text.pack c.contentsSource <> " is empty" | null entries]

    checkContents :: IO CheckResult
    checkContents = case contentsProblems c of
        problems@(_ : _) -> pure (Failure (problemText what problems))
        [] -> do
            missing <- sourceProblems
            case missing of
                _ : _ -> pure (Failure (problemText what missing))
                [] -> interpretContentsDryRun c <$> traverse dryRun (contentsPasses c)

    -- gcloud says what it would do on stderr, and exits 0 either way.
    dryRun :: RsyncPass -> IO (ExitCode, ByteString)
    dryRun p = do
        (code, _out, err) <-
            readCreateProcessWithExitCode
                (prepare storageCommand (BucketsRsync (contentsRsync c p){rsyncDryRun = True}))
                ""
        pure (code, err)

    bringUp :: IO ()
    bringUp = do
        refuse what (contentsProblems c)
        refuse what =<< sourceProblems
        mapM_ (run . BucketsRsync . contentsRsync c) (contentsPasses c)

    bringDown :: IO ()
    bringDown = case c.contentsOnDown of
        LeaveObjects -> pure ()
        EmptyDestination -> do
            refuse what (contentsProblems c)
            (code, _out, _err) <- readCreateProcessWithExitCode (prepare storageCommand (BucketsDescribe bkt)) ""
            case code of
                -- no bucket, no objects
                ExitFailure _ -> pure ()
                ExitSuccess -> withEmptyDirectory $ \empty -> run (BucketsRsync (emptyingRsync c empty))

-- | An empty directory, removed afterwards.
withEmptyDirectory :: (FilePath -> IO a) -> IO a
withEmptyDirectory act = do
    tmp <- getTemporaryDirectory
    bracket
        ( do
            -- a unique name from a file, then a directory beside it
            (path, h) <- openTempFile tmp "salmon-empty"
            hClose h
            let dir = path <> ".d"
            createDirectory dir
            pure (path, dir)
        )
        (\(path, dir) -> removeDirectory dir >> removeFile path)
        (act . snd)

-- | The prefix without the slashes around it.
prefixOf :: BucketContents -> Text
prefixOf c = Text.dropAround (== '/') (Text.strip c.contentsPrefix)

-- | Where the directory is published: @gs:\/\/BUCKET@ or @gs:\/\/BUCKET\/PREFIX@.
contentsDestination :: BucketContents -> Text
contentsDestination c = case prefixOf c of
    "" -> "gs://" <> c.contentsBucket.bucketName
    prefix -> "gs://" <> c.contentsBucket.bucketName <> "/" <> prefix

-- | Why a declared publication cannot be made, if it cannot.
contentsProblems :: BucketContents -> [Text]
contentsProblems c =
    ["it names no directory" | null c.contentsSource]
        <> ["the prefix " <> prefixOf c <> " has an empty or dotted segment" | any badSegment (segments (prefixOf c))]
        <> ["a rule has an empty glob" | any Text.null globs]
        <> ["the glob " <> g <> " is in two rules" | g <- nub (globs \\ nub globs), not (Text.null g)]
        <> [ "the Cache-Control for " <> label <> " is empty or holds a control character"
           | (label, value) <- headers
           , Text.null (Text.strip value) || Text.any isControl value
           ]
  where
    segments "" = []
    segments p = Text.splitOn "/" p
    badSegment s = s `elem` ["", ".", ".."]
    globs = map (\rule -> Text.strip rule.cacheGlob) c.contentsCacheRules
    headers =
        [(Text.strip rule.cacheGlob, rule.cacheControl) | rule <- c.contentsCacheRules]
            <> [("the other objects", v) | Just v <- [c.contentsDefaultCacheControl]]

-- | One @rsync@ invocation's share of the directory.
data RsyncPass = RsyncPass
    { passLabel :: Text
    -- ^ The glob, or what stands for "the rest".
    , passExclude :: Maybe Text
    -- ^ The regex of everything that is /not/ this pass's.
    , passCacheControl :: Maybe Text
    }
    deriving (Eq, Show)

{- | The passes that publish a directory: one per rule, each restricted to
the paths its glob matches and no earlier rule's does, then one for the
paths no rule matches. With no rule it is a single unrestricted pass. Every
path belongs to exactly one pass.
-}
contentsPasses :: BucketContents -> [RsyncPass]
contentsPasses c =
    [ RsyncPass
        { passLabel = Text.strip rule.cacheGlob
        , passExclude = Just (onlyRegex (globRegex rule.cacheGlob) (map regexOf earlier))
        , passCacheControl = Just (Text.strip rule.cacheControl)
        }
    | (earlier, rule) <- zip (inits rules) rules
    ]
        <> [ RsyncPass
                { passLabel = if null rules then "(every object)" else "(other objects)"
                , passExclude = if null rules then Nothing else Just ("^(?:" <> alternatives (map regexOf rules) <> ")$")
                , passCacheControl = Text.strip <$> c.contentsDefaultCacheControl
                }
           ]
  where
    rules = c.contentsCacheRules
    regexOf :: CacheRule -> Text
    regexOf rule = globRegex rule.cacheGlob
    alternatives = Text.intercalate "|"
    -- everything but what matches this one and none of the earlier ones
    onlyRegex :: Text -> [Text] -> Text
    onlyRegex mine [] = "^(?!" <> mine <> "$).*$"
    onlyRegex mine earlier = "^(?!(?=" <> mine <> "$)(?!(?:" <> alternatives earlier <> ")$)).*$"

{- | A glob as an unanchored Python regular expression over a relative path,
which is what @--exclude@ takes. See 'CacheRule' for the glob's meaning. A
comma is spelled @\\x2c@, since gcloud splits the flag's value on commas.
-}
globRegex :: Text -> Text
globRegex raw
    | Just rooted <- Text.stripPrefix "/" glob = "(?:" <> go rooted <> ")"
    | Text.isInfixOf "/" glob = "(?:" <> go glob <> ")"
    | otherwise = "(?:(?:.*/)?" <> go glob <> ")"
  where
    glob = Text.strip raw
    go :: Text -> Text
    go t
        | Just rest <- Text.stripPrefix "**/" t = "(?:.*/)?" <> go rest
        | Just rest <- Text.stripPrefix "**" t = ".*" <> go rest
        | Just (ch, rest) <- Text.uncons t = one ch <> go rest
        | otherwise = ""
    one :: Char -> Text
    one '*' = "[^/]*"
    one '?' = "[^/]"
    one ',' = "\\x2c"
    one ch
        | isAscii ch && not (isAlphaNum ch) && ch `notElem` ("/_-" :: String) = Text.pack ['\\', ch]
        | otherwise = Text.singleton ch

-- | One @gcloud storage rsync@ from a local directory to a destination.
data Rsync = Rsync
    { rsyncProject :: Project
    , rsyncSource :: FilePath
    , rsyncDestination :: Text
    , rsyncDeleteUnmatched :: Bool
    , rsyncCacheControl :: Maybe Text
    , rsyncExclude :: Maybe Text
    , rsyncDryRun :: Bool
    }
    deriving (Eq, Show)

-- | The invocation that publishes one pass of a directory.
contentsRsync :: BucketContents -> RsyncPass -> Rsync
contentsRsync c p =
    Rsync
        { rsyncProject = c.contentsBucket.bucketProject
        , rsyncSource = c.contentsSource
        , rsyncDestination = contentsDestination c
        , rsyncDeleteUnmatched = c.contentsDeleteExtraneous
        , rsyncCacheControl = p.passCacheControl
        , rsyncExclude = p.passExclude
        , rsyncDryRun = False
        }

{- | The invocation that empties a destination: this (empty) directory
synced over it, everything unmatched removed.
-}
emptyingRsync :: BucketContents -> FilePath -> Rsync
emptyingRsync c empty =
    Rsync
        { rsyncProject = c.contentsBucket.bucketProject
        , rsyncSource = empty
        , rsyncDestination = contentsDestination c
        , rsyncDeleteUnmatched = True
        , rsyncCacheControl = Nothing
        , rsyncExclude = Nothing
        , rsyncDryRun = False
        }

-- | One thing a dry-run says it would do.
data Planned
    = -- | Source, destination.
      WouldCopy Text Text
    | WouldRemove Text
    | WouldSetMtime Text
    | -- | A @Would ...@ line this module does not know, whole.
      WouldOther Text
    deriving (Eq, Show)

{- | What @gcloud storage rsync --dry-run@ says it would do, read off its
standard error (its standard output is empty). The listing progress lines and
the blank one are dropped; a destination loses the @#generation@ an existing
object carries.
-}
parseDryRun :: ByteString -> [Planned]
parseDryRun = concatMap planned . Text.lines . decodeLenient
  where
    planned :: Text -> [Planned]
    planned line0
        | Just rest <- Text.stripPrefix "Would copy " line
        , (src, dst) <- Text.breakOnEnd " to " rest
        , Just src' <- Text.stripSuffix " to " src =
            [WouldCopy src' (ungenerated dst)]
        | Just dst <- Text.stripPrefix "Would remove " line = [WouldRemove (ungenerated dst)]
        | Just dst <- Text.stripPrefix "Would set mtime for " line = [WouldSetMtime (ungenerated dst)]
        | Text.isPrefixOf "Would " line = [WouldOther line]
        | otherwise = []
      where
        line = Text.strip line0
    ungenerated :: Text -> Text
    ungenerated url = case Text.breakOnEnd "#" url of
        (before, generation)
            | not (Text.null before) && not (Text.null generation) && Text.all (`elem` ['0' .. '9']) generation ->
                Text.dropEnd 1 before
        _ -> url

{- | The verdict drawn from the dry-runs of a publication's passes, each an
exit code and what the command wrote on standard error.

gcloud exits 0 whether or not anything differs, so the verdict is in the
lines: a copy or a removal is a difference, an mtime update is not (with
@--checksums-only@ it is all a file with the same bytes and a newer mtime
gives), and a @Would@ line of another kind cannot be judged. A non-zero exit
is an error (no bucket, no directory), reported with gcloud's own @ERROR@
line.
-}
interpretContentsDryRun :: BucketContents -> [(ExitCode, ByteString)] -> CheckResult
interpretContentsDryRun c results
    | problems@(_ : _) <- contentsProblems c = Failure (problemText what problems)
    | (n, err) : _ <- [(n, err) | (ExitFailure n, err) <- results] =
        Failure ("could not compare the " <> what <> " (exit " <> Text.pack (show n) <> ")" <> errorLine err)
    | not (null copies && null removals) =
        Failure
            ( Text.unwords
                [ "the"
                , what
                , "differ from"
                , Text.pack c.contentsSource <> ":"
                , Text.intercalate ", " (described "to copy" copies <> described "to remove" removals)
                ]
            )
    | (_ : _) <- [() | WouldOther _ <- plans] = Unknown
    | otherwise = Success
  where
    what = "contents of " <> contentsDestination c
    plans = concatMap (parseDryRun . snd) results
    copies = [dst | WouldCopy _ dst <- plans]
    removals = [dst | WouldRemove dst <- plans]
    described :: Text -> [Text] -> [Text]
    described _ [] = []
    described label objects =
        [ Text.pack (show (length objects))
            <> " "
            <> label
            <> " ("
            <> Text.unwords (map relative (take 3 objects))
            <> (if length objects > 3 then " ..." else "")
            <> ")"
        ]
    relative url = fromMaybe url (Text.stripPrefix (contentsDestination c <> "/") url)
    errorLine err = case [l | l <- Text.lines (decodeLenient err), Text.isPrefixOf "ERROR:" l] of
        l : _ -> ": " <> Text.strip (Text.drop 6 l)
        [] -> ""

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
    | -- | @gcloud storage rsync@, a local directory to a bucket.
      BucketsRsync Rsync
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
    BucketsRsync rs ->
        gcloudProc $
            withProject rs.rsyncProject $
                ["storage", "rsync", rs.rsyncSource, Text.unpack rs.rsyncDestination, "--recursive"]
                    <> ["--delete-unmatched-destination-objects" | rs.rsyncDeleteUnmatched]
                    <> ["--checksums-only"]
                    <> ["--cache-control=" <> Text.unpack v | Just v <- [rs.rsyncCacheControl]]
                    <> ["--exclude=" <> Text.unpack re | Just re <- [rs.rsyncExclude]]
                    <> ["--dry-run" | rs.rsyncDryRun]
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
