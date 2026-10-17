{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-pgvector-demo@: index a directory into pgvector, then search it
by similarity ("SreBox.PgVectorDemo").

@
salmon-pgvector-demo config --dir ~/notes --hash-dimension 64 > demo.json
salmon-pgvector-demo run up < demo.json
salmon-pgvector-demo search --text "salmon swim upstream" < demo.json
salmon-pgvector-demo search --image cat.jpg < demo.json
@

@search@ reads the same directive as @run@, so it embeds the query with the
embedder the rows were made with. It only reads.

The embedder is one of three, chosen by flags: @--hash-dimension N@ (the
stand-in, no model: it shows the plumbing, not meaning), @--llama-*@ (a
llama-server under systemd, text only) or @--clip-*@ (clip.cpp, text and
images). Releases, models and their sha256 are the operator's to pin, and a
model with no @--…-model-url@ must already be at its path. The local cluster
and the database are created when missing; the pgvector package comes from
the PostgreSQL project's repository when @--pgdg-key@ names its (already
provisioned) key, and must otherwise be installable as is.
-}
module PgVectorDemo (
    main,
    Seed (..),
    Spec (..),
    EmbedderSpec (..),
    configure,
    program,
    embedderOf,
) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Options.Applicative (auto, execParser, fullDesc, header, helper, info, long, metavar, option, optional, progDesc, strOption, value, (<**>), (<|>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Directory (makeAbsolute)
import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.ClipCpp as ClipCpp
import Salmon.Builtin.Nodes.Debian.AptRepository (pgdg, viaRepository)
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import Salmon.Builtin.Nodes.LlamaServer (LlamaRelease (..), LlamaServer (..), Listen (..), ModelFile (..), Pooling (..), defaultLlamaServer, llamaServerSystemd)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

import SreBox.PgVectorDemo (Demo (..), Embedder, Hit (..), Input (..), clipEmbedder, defaultWalkPolicy, hashEmbedder, llamaEmbedder, pgVectorDemo, search)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("search" : rest) -> withArgs rest searchMain
        _ -> do
            let desc = fullDesc <> progDesc "Index a directory into pgvector (search it with: search --text T | --image P)" <> header "salmon-pgvector-demo"
            cmd <- execParser (info parseRecord desc)
            CLI.execCommandOrSeed reportPrint configure program cmd

-------------------------------------------------------------------------------
-- Seed

data Seed = Seed
    { seedDir :: FilePath
    , seedDatabase :: Text
    , seedTable :: Text
    , seedPort :: Int
    , seedMajor :: Int
    , seedPgdg :: Maybe (FilePath, Text)
    , seedEmbedder :: EmbedderSpec
    }
    deriving (Eq, Show)

instance ParseRecord Seed where
    parseRecord = build <**> helper
      where
        build =
            Seed
                <$> strOption (long "dir" <> metavar "DIR" <> Opt.help "the directory to index")
                <*> strOption (long "database" <> value "vectors" <> Opt.showDefault)
                <*> strOption (long "table" <> value "items" <> Opt.showDefault <> Opt.help "another width wants another table")
                <*> option auto (long "port" <> value 5432 <> Opt.showDefault)
                <*> option auto (long "major" <> value 17 <> Opt.showDefault <> Opt.help "the cluster's PostgreSQL major version, for the pgvector package")
                <*> optional ((,) <$> strOption (long "pgdg-key" <> metavar "PATH" <> Opt.help "the PostgreSQL apt repository's key, already provisioned") <*> strOption (long "pgdg-fingerprint" <> metavar "FPR"))
                <*> embedderP
        embedderP = hashP <|> llamaP <|> clipP
        hashP = HashSpec <$> option auto (long "hash-dimension" <> metavar "N" <> Opt.help "the sha256 stand-in: no model, no meaning")
        llamaP =
            LlamaSpec
                <$> strOption (long "llama-build" <> metavar "bNNNN")
                <*> strOption (long "llama-archive-url" <> metavar "URL")
                <*> strOption (long "llama-archive-sha256" <> metavar "HEX")
                <*> strOption (long "llama-install-dir" <> value "/opt/salmon/llama" <> Opt.showDefault)
                <*> strOption (long "llama-model" <> metavar "PATH" <> Opt.help "a GGUF embedding model")
                <*> strOption (long "llama-model-sha256" <> metavar "HEX")
                <*> optional (strOption (long "llama-model-url" <> metavar "URL"))
                <*> option auto (long "dimension" <> metavar "N" <> Opt.help "what the model produces")
                <*> strOption (long "llama-pooling" <> value "mean" <> Opt.showDefault <> Opt.help "none|mean|cls|last|rank, as the model wants")
                <*> option auto (long "llama-port" <> value 8080 <> Opt.showDefault <> Opt.help "on loopback")
                <*> optional (strOption (long "llama-api-key-file" <> metavar "PATH" <> Opt.help "keys, one per line, already provisioned"))
        clipP =
            ClipSpec
                <$> strOption (long "clip-dir" <> metavar "DIR" <> Opt.help "where clip.cpp is checked out and built")
                <*> strOption (long "clip-commit" <> value ClipCpp.knownCommit <> Opt.showDefault)
                <*> strOption (long "clip-model" <> metavar "PATH" <> Opt.help "a two-tower GGUF CLIP model")
                <*> strOption (long "clip-model-sha256" <> metavar "HEX")
                <*> optional (strOption (long "clip-model-url" <> metavar "URL"))
                <*> option auto (long "clip-dimension" <> metavar "N" <> Opt.help "what the model produces (512 for ViT-B/32)")

-------------------------------------------------------------------------------
-- Spec

data EmbedderSpec
    = HashSpec {hashDimension :: Int}
    | LlamaSpec
        { llamaBuild :: Text
        , llamaArchiveUrl :: Text
        , llamaArchiveSha256 :: Text
        , llamaInstallDir :: FilePath
        , llamaModel :: FilePath
        , llamaModelSha256 :: Text
        , llamaModelUrl :: Maybe Text
        , llamaDimension :: Int
        , llamaPooling :: Text
        , llamaPort :: Int
        , llamaApiKeyFile :: Maybe FilePath
        }
    | ClipSpec
        { clipDir :: FilePath
        , clipCommit :: Text
        , clipModel :: FilePath
        , clipModelSha256 :: Text
        , clipModelUrl :: Maybe Text
        , clipDimension :: Int
        }
    deriving (Eq, Show, Generic)

instance FromJSON EmbedderSpec
instance ToJSON EmbedderSpec

data Spec = Spec
    { directory :: FilePath
    , database :: Text
    , table :: Text
    , port :: Int
    , major :: Int
    , pgdgKey :: Maybe (FilePath, Text)
    , embedder :: EmbedderSpec
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> do
    case seed.seedEmbedder of
        LlamaSpec{llamaPooling = p} | Nothing <- pooling p -> fail ("--llama-pooling: not one of none|mean|cls|last|rank: " <> Text.unpack p)
        ClipSpec{clipCommit = c} | not (ClipCpp.validCommit c) -> fail "--clip-commit must be a full commit id"
        _ -> pure ()
    dir <- makeAbsolute seed.seedDir
    pure (Spec dir seed.seedDatabase seed.seedTable seed.seedPort seed.seedMajor seed.seedPgdg seed.seedEmbedder)

pooling :: Text -> Maybe Pooling
pooling t = lookup t [("none", PoolNone), ("mean", PoolMean), ("cls", PoolCls), ("last", PoolLast), ("rank", PoolRank)]

-------------------------------------------------------------------------------
-- Program

demoOf :: Spec -> Demo
demoOf spec = Demo spec.directory spec.major spec.port spec.database spec.table defaultWalkPolicy

embedderOf :: EmbedderSpec -> Embedder
embedderOf e = case e of
    HashSpec n -> hashEmbedder n
    LlamaSpec{} ->
        let rel = LlamaRelease e.llamaBuild e.llamaArchiveUrl e.llamaArchiveSha256 e.llamaInstallDir
            model = ModelFile e.llamaModel e.llamaModelSha256 e.llamaModelUrl
            server =
                (defaultLlamaServer "pgvector-demo" rel model e.llamaDimension (maybe PoolMean id (pooling e.llamaPooling)))
                    { lsListen = Loopback e.llamaPort
                    , lsApiKeyFile = e.llamaApiKeyFile
                    }
         in llamaEmbedder (llamaServerSystemd reportPrint Debian.systemctl server) server
    ClipSpec{} ->
        let src = (ClipCpp.clipSourceAt e.clipDir){ClipCpp.clipCommit = e.clipCommit}
            model = ModelFile e.clipModel e.clipModelSha256 e.clipModelUrl
         in clipEmbedder reportPrint ClipCpp.debianTools (ClipCpp.defaultClipCpp src model e.clipDimension)

program :: Track' Spec
program = Track $ \spec ->
    pgVectorDemo
        reportPrint
        reportPrint
        reportPrint
        Debian.psql
        (maybe ignoreTrack (\(key, fpr) -> viaRepository (pgdg key fpr)) spec.pgdgKey)
        (Track $ Postgres.database reportPrint (Track $ Postgres.pgLocalCluster reportPrint Debian.postgres Debian.pg_ctlcluster) Debian.psql spec.port . Postgres.Database)
        (demoOf spec)
        (embedderOf spec.embedder)

-------------------------------------------------------------------------------
-- Search

data SearchArgs = SearchArgs
    { query :: Input
    , limit :: Int
    }

searchMain :: IO ()
searchMain = do
    args <- execParser (info (parser <**> helper) (fullDesc <> progDesc "Search the indexed directory by similarity; the directive is read on stdin"))
    raw <- LByteString.getContents
    spec <- case Aeson.eitherDecode raw of
        Left err -> hPutStrLn stderr ("not a directive: " <> err) >> exitFailure
        Right s -> pure (s :: Spec)
    hits <- search (demoOf spec) (embedderOf spec.embedder) args.query args.limit
    case hits of
        Left why -> Text.hPutStrLn stderr why >> exitFailure
        Right hs -> mapM_ (\h -> printf "%.4f\t%s\t%s\n" h.hitDistance (Text.unpack h.hitKind) h.hitPath) hs
  where
    parser =
        SearchArgs
            <$> ((InputText <$> strOption (long "text" <> metavar "TEXT")) <|> (InputImage <$> strOption (long "image" <> metavar "PATH")))
            <*> option auto (long "limit" <> value 5 <> Opt.showDefault)
