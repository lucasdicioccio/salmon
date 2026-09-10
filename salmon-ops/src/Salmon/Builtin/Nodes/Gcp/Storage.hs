{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Storage (
    Bucket (..),
    bucket,
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
        withBinary gcloudTrack storageCommand (BucketsDescribe bkt) $ \describe ->
            withBinary gcloudTrack storageCommand (BucketsDelete bkt) $ \delete ->
                op "gcp-bucket" nodeps $ \actions ->
                    actions
                        { help = Text.unwords ["creates GCS bucket", bkt.bucketName]
                        , ref = mkRef "gcp-bucket" bkt.bucketName
                        , up = create r'
                        , down = delete r'
                        , check = checkBucket describe
                        }
  where
    r' = contramap (RunStorageCommand (BucketsCreate bkt)) r

    checkBucket :: (Reporter Binary.Report -> IO ()) -> IO CheckResult
    checkBucket describeBucket = do
        -- describe exits non-zero when the bucket is absent, which would throw.
        -- We catch the exception and treat it as Failure.
        res <- tryDescribe describeBucket
        pure $ case res of
            Just _ -> Success
            Nothing -> Failure ("bucket not found: " <> bkt.bucketName)

    tryDescribe :: (Reporter Binary.Report -> IO ()) -> IO (Maybe ())
    tryDescribe describeBucket = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (prepare storageCommand (BucketsDescribe bkt))
                ""
        pure $ case code of
            ExitSuccess -> Just ()
            ExitFailure _ -> Nothing

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
