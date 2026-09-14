{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Storage (
    Bucket (..),
    bucket,
    interpretBucketDescribe,
    Report (..),
    StorageCommand (..),
    storageCommand,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), gcloudProc, withProject, withRegion)
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
                    , up = create r'
                    , down = delete r'
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

data StorageCommand
    = BucketsCreate Bucket
    | BucketsDescribe Bucket
    | BucketsDelete Bucket
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
