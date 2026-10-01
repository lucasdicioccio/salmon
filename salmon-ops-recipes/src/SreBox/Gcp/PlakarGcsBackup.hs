{-# LANGUAGE OverloadedStrings #-}

{- | A Plakar backup job whose Kloset lives in a Google Cloud Storage bucket,
built from what the tree already has: 'Storage.bucket', 'Iam.serviceAccount',
'Iam.iamBinding' (object admin on that one bucket) and
'Iam.serviceAccountKey', feeding the GCS store of
"Salmon.Builtin.Nodes.Plakar" and the job on top of it.

= What it sets and what it leaves to the bucket's owner

It creates the bucket with uniform bucket-level access, and nothing else:
__no retention policy, no object versioning, no lifecycle rules__. Plakar
advertises immutable snapshots, but how long a bucket refuses deletes is a
bucket property (as for "SreBox.PostgresBackup"), and a job whose @prune@ has
to delete objects would be broken by a retention lock shorter than its own
'KeepPolicy'. Set those on the bucket yourself.

= Credentials

A service-account key file, written by 'Iam.serviceAccountKey' (so by whoever
runs salmon, with gcloud's own file mode) and then only ever named by path. The
job's user must be able to read it, and the Plakar passphrase keyfile
('gbKeyfile') is provisioned by somebody else, as for the local store. Workload
identity is not supported.

= The integration version has no default

'gbGcsVersion' is a 'Pinned' release or an explicit 'Latest'.
-}
module SreBox.Gcp.PlakarGcsBackup (
    GcsBackup (..),
    gcsBackup,
    gcsServiceAccountEmail,
    gcsBucketRole,
    Report (..),
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import Salmon.Builtin.Nodes.CronTask (Schedule)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import Salmon.Builtin.Nodes.Plakar
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track
import Salmon.Reporter

data Report
    = StorageReport !Storage.Report
    | IamReport !Iam.Report
    deriving (Show)

data GcsBackup = GcsBackup
    { gbProject :: Core.Project
    , gbBucket :: Text
    , gbLocation :: Core.Region
    , gbServiceAccountId :: Text
    -- ^ the account id (before the @\@@), created if absent
    , gbCredentialsFile :: FilePath
    -- ^ where its key is written, and the store reads it
    , gbStoreName :: Text
    , gbStorePrefix :: Text
    , gbGcsVersion :: IntegrationVersion
    , gbCredentialsOption :: Text
    -- ^ the GCS integration's option for the credentials file; see 'gcsCredentialsOption'
    , gbKeyfile :: FilePath
    -- ^ the Kloset passphrase, provisioned by somebody else
    , gbJobName :: Text
    , gbSource :: FilePath
    , gbSchedule :: Schedule
    , gbUser :: Text
    -- ^ runs the job, and owns the integration and the store configuration
    , gbKeep :: KeepPolicy
    , gbMaxAge :: NominalDiffTime
    , gbScriptPath :: FilePath
    }

-- | @ID\@PROJECT.iam.gserviceaccount.com@.
gcsServiceAccountEmail :: GcsBackup -> Text
gcsServiceAccountEmail b = b.gbServiceAccountId <> "@" <> b.gbProject.projectId <> ".iam.gserviceaccount.com"

-- | The role the account gets, on the bucket only.
gcsBucketRole :: Text
gcsBucketRole = "roles/storage.objectAdmin"

gcsBackup :: Reporter Report -> Track' (Binary "plakar") -> GcsBackup -> Op
gcsBackup r plakar b =
    plakarJobOn remote job
  where
    gcloud = Core.gcloud
    sr = contramap StorageReport r
    ir = contramap IamReport r
    user = Just b.gbUser

    bucketOp =
        Storage.bucket
            sr
            gcloud
            (Storage.Bucket b.gbBucket b.gbProject b.gbLocation True)
    accountOp = Iam.serviceAccount ir gcloud b.gbProject b.gbServiceAccountId
    bindingOp =
        Iam.iamBinding
            ir
            gcloud
            (Iam.IamBinding (Iam.ServiceAccount (gcsServiceAccountEmail b)) gcsBucketRole ("buckets/" <> b.gbBucket))
    keyOp = Iam.serviceAccountKey ir gcloud (Iam.ServiceAccountKey b.gbProject b.gbServiceAccountId b.gbCredentialsFile)

    -- everything the store's configuration needs to exist first
    prerequisites =
        op "plakar-gcs-prerequisites" (deps [bucketOp, accountOp, bindingOp, keyOp, integrationOp]) $ \actions ->
            actions
                { help = Text.unwords ["gs://" <> b.gbBucket, "with an account and key for plakar"]
                , notes = ["bucket retention, versioning and lifecycle are left to the bucket's owner"]
                , ref = mkRef "plakar-gcs-prerequisites" (b.gbProject.projectId, b.gbBucket, b.gbStoreName)
                }

    integrationOp = plakarIntegration plakar (Integration "gcs" b.gbGcsVersion user)

    storeOp =
        gcsStore
            prerequisites
            GcsStore
                { gcsStoreName = b.gbStoreName
                , gcsBucket = b.gbBucket
                , gcsPrefix = b.gbStorePrefix
                , gcsCredentialsFile = b.gbCredentialsFile
                , gcsCredentialsOption = b.gbCredentialsOption
                , gcsStoreUser = user
                }

    remote = remoteKloset storeOp user b.gbStoreName b.gbKeyfile

    job =
        PlakarJob
            { jobName = b.gbJobName
            , jobStore = KlosetStore ("@" <> Text.unpack b.gbStoreName) b.gbKeyfile
            , jobSource = b.gbSource
            , jobSchedule = b.gbSchedule
            , jobUser = b.gbUser
            , jobKeep = b.gbKeep
            , jobMaxAge = b.gbMaxAge
            , jobScriptPath = b.gbScriptPath
            }
