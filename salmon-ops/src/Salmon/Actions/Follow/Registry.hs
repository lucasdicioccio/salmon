{-# LANGUAGE OverloadedStrings #-}

{- | Which registry @--follow@ names, from the shape of its argument
(milestone 6 of @specs/pull-mode.md@). Every backend is a
'Salmon.Actions.Follow.Registry' value owning the template that turns a
label into an address; the fetcher and the scheduler see none of this.

> --follow /srv/reg                          a directory: /srv/reg/<label>.json
> --follow git+https://host/repo#main:hosts  a git branch: hosts/<label>.json at origin/main
> --follow https://host/seed/latest/{label}  HTTP: GET that URL, or <base>/<label>.json without {label}
> --follow dns:fleet.example                 a TXT index at <label>.fleet.example, fetched over HTTP
> --follow s3://bucket/prefix                a bucket, over its plain HTTPS object URLs
> --follow gs://bucket/prefix                likewise, Google's

The bucket backends are the HTTP one under a URL template:
@https://\<bucket\>.s3.amazonaws.com/\<prefix\>/\<label\>.json@,
@https://storage.googleapis.com/\<bucket\>/\<prefix\>/\<label\>.json@, or
path-style under an S3-compatible endpoint given with
@--follow-bucket-endpoint@. That covers a public bucket, or one fronted by
something that signs — no SDK, and __no authenticated access__: a private
bucket answers @403@, which is a failed round and says so.
-}
module Salmon.Actions.Follow.Registry (
    Address (..),
    parseAddress,
    Bucket (..),
    Store (..),
    bucketTemplate,
    Options (..),
    defaultOptions,
    open,
    defaultWorkdir,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

import Salmon.Actions.Follow (Registry (..), digestOf, directoryRegistry, unDigest)
import qualified Salmon.Actions.Follow.Registry.Dns as Dns
import qualified Salmon.Actions.Follow.Registry.Git as Git
import qualified Salmon.Actions.Follow.Registry.Http as Http
import qualified Data.ByteString.Lazy.Char8 as LChar8

-- | What @--follow@ can name.
data Address
    = Directory FilePath
    | Git Git.Source
    | Http Text
    | Dns Text
    | InBucket Bucket
    deriving (Show, Eq)

data Store = S3 | Gcs
    deriving (Show, Eq)

-- | @s3://bucket/prefix@ or @gs://bucket/prefix@; the prefix may be empty.
data Bucket = Bucket
    { bucketStore :: Store
    , bucketName :: Text
    , bucketPrefix :: Text
    }
    deriving (Show, Eq)

-- | By shape; anything with no recognised scheme is a directory path.
parseAddress :: Text -> Either Text Address
parseAddress t
    | Just rest <- Text.stripPrefix "git+" t = Git <$> Git.parseSource rest
    | Text.isPrefixOf "http://" t || Text.isPrefixOf "https://" t = Right (Http t)
    | Just zone <- Text.stripPrefix "dns:" t =
        if Text.null zone then Left "dns: needs a zone" else Right (Dns zone)
    | Just rest <- Text.stripPrefix "s3://" t = InBucket <$> bucket S3 rest
    | Just rest <- Text.stripPrefix "gs://" t = InBucket <$> bucket Gcs rest
    | Text.null t = Left "--follow needs a registry"
    | otherwise = Right (Directory (Text.unpack t))
  where
    bucket store rest =
        let (name, prefix) = Text.breakOn "/" rest
         in if Text.null name
                then Left ("a bucket address needs a bucket name: " <> t)
                else Right (Bucket store name (Text.dropWhileEnd (== '/') (Text.drop 1 prefix)))

{- | The bucket's HTTPS base URL, which "Salmon.Actions.Follow.Registry.Http"
then appends @/\<label\>.json@ to. Virtual-hosted for S3 proper, path-style
under an endpoint (what MinIO and friends expect) and for GCS. -}
bucketTemplate :: Maybe Text -> Bucket -> Text
bucketTemplate endpoint b =
    Text.dropWhileEnd (== '/') base <> (if Text.null b.bucketPrefix then "" else "/" <> b.bucketPrefix)
  where
    base = case (endpoint, b.bucketStore) of
        (Just e, _) -> Text.dropWhileEnd (== '/') e <> "/" <> b.bucketName
        (Nothing, S3) -> "https://" <> b.bucketName <> ".s3.amazonaws.com"
        (Nothing, Gcs) -> "https://storage.googleapis.com/" <> b.bucketName

-- | What the backends need beyond their address.
data Options = Options
    { optHttp :: Http.Options
    , optWorkdir :: Maybe FilePath
    -- ^ the git checkout; 'defaultWorkdir' when 'Nothing'
    , optCacheDir :: Maybe FilePath
    -- ^ @--follow-cache@, which the default checkout lives under
    , optBucketEndpoint :: Maybe Text
    -- ^ an S3-compatible endpoint, path-style
    , optResolver :: Dns.Resolver
    }

defaultOptions :: Options
defaultOptions = Options Http.defaultOptions Nothing Nothing Nothing Dns.digResolver

{- | Where a git registry is checked out when nobody said: @checkout@ under
the cache directory (the one place a follower already keeps state across
restarts), else a directory under the system's temporary one named by the
repository, so that two followers of different repositories on one machine
do not share a checkout. -}
defaultWorkdir :: Maybe FilePath -> Git.Source -> IO FilePath
defaultWorkdir (Just cache) _ = pure (cache </> "checkout")
defaultWorkdir Nothing source = do
    tmp <- getTemporaryDirectory
    pure (tmp </> ("salmon-follow-" <> Text.unpack (Text.take 12 (unDigest (digestOf (LChar8.pack (Text.unpack (Git.renderSource source))))))))

-- | The registry for an address.
open :: Options -> Address -> IO Registry
open o addr = case addr of
    Directory dir -> pure (directoryRegistry dir)
    Git source -> do
        workdir <- maybe (defaultWorkdir o.optCacheDir source) pure o.optWorkdir
        Git.gitRegistry workdir source
    Http template -> do
        mgr <- Http.newManager o.optHttp
        pure (Http.httpRegistry mgr template)
    Dns zone -> do
        mgr <- Http.newManager o.optHttp
        pure (Dns.dnsRegistry o.optResolver mgr zone)
    InBucket b -> do
        mgr <- Http.newManager o.optHttp
        let reg = Http.httpRegistry mgr (bucketTemplate o.optBucketEndpoint b)
        pure reg{registryName = renderBucket b}

-- | The address back, as given: what @history@ and reports name.
renderBucket :: Bucket -> Text
renderBucket b = (case b.bucketStore of S3 -> "s3://"; Gcs -> "gs://") <> b.bucketName <> (if Text.null b.bucketPrefix then "" else "/" <> b.bucketPrefix)
