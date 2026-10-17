{-# LANGUAGE OverloadedStrings #-}

{- | A demonstration of the embedding builtins end to end, on one machine:
__walk a directory, embed each file, keep the vectors in Postgres, and answer
"what is closest to this?"__. One @run up@ yields a table one can query by
meaning. It is a demo of the builtins, not a product.

The graph, in dependency order:

1. pgvector in the database ("Salmon.Builtin.Nodes.PgVector"), which brings
   the database (the caller's 'Track'') and the package (after the caller's
   choice of source).
2. 'schema': the table and its @hnsw@ index. The width of the column is the
   embedder's 'embDimension', never typed a second time, and a table already
   there with another width is refused rather than altered.
3. The embedder's own node ('embNode'): a llama-server for text
   ('llamaEmbedder'), clip.cpp for text and images ('clipEmbedder'), or the
   deterministic stand-in ('hashEmbedder') that needs no model.
4. 'indexDirectory': for each file of the directory whose content (or
   embedder) is not the one its row records, embed it and upsert the row.
   Its @check@ is "every file has a current row", so a second @run up@
   embeds nothing and @run serve@ notices a new or changed file.

'search' is the read side: embed the query with the same embedder and order
the rows by cosine distance. It never writes.

= What it decides

* __One embedder per table.__ Vectors from two models are not comparable,
  so the table records which embedder made each row ('embName') and a row
  from another one counts as stale. Text and images share a table only when
  one embedder takes both (clip.cpp).
* __The walk__ ('WalkPolicy'): recursive, sorted, no hidden files or
  directories, symbolic links not followed, kind by extension, whole files
  under a size cap, no chunking. Every skipped file is reported with why.
* __Rows of deleted files are left__, and counted in the report: dropping
  them is a deletion the operator did not name. Neither node's @down@ drops
  anything either.

= What is public

Paths and the embedder's name are in the reports. The files' contents are
not, but a text file given to 'clipEmbedder' is on @extract@'s command line
(see "Salmon.Builtin.Nodes.ClipCpp"); 'llamaEmbedder' sends it to @curl@ on
stdin. Do not index a directory of secrets.
-}
module SreBox.PgVectorDemo (
    Report (..),

    -- * Embedders
    Kind (..),
    kindName,
    Input (..),
    Embedder (..),
    llamaEmbedder,
    clipEmbedder,
    hashEmbedder,
    hashVector,

    -- * The walk
    WalkPolicy (..),
    defaultWalkPolicy,
    Skip (..),
    classify,
    walk,

    -- * The nodes
    Demo (..),
    defaultDemo,
    pgVectorDemo,
    schema,
    indexDirectory,

    -- * Search
    Hit (..),
    search,

    -- * Pieces, exposed for tests
    Candidate (..),
    vectorLiteral,
    schemaSql,
    inspectSchemaSql,
    interpretSchemaRow,
    indexedSql,
    parseIndexed,
    pendingFiles,
    staleCount,
    upsertSql,
    searchSql,
    parseHits,
    contentHash,
) where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_, unless)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.IO.Exception (ExitCode (..))
import System.Directory (getFileSize, listDirectory, pathIsSymbolicLink, doesDirectoryExist, doesFileExist)
import System.FilePath (splitDirectories, takeExtension, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)
import Text.Printf (printf)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.ClipCpp as ClipCpp
import qualified Salmon.Builtin.Nodes.Debian.Package as Package
import Salmon.Builtin.Nodes.LlamaServer (LlamaServer (..), ModelFile (..), llamaEmbed)
import Salmon.Builtin.Nodes.PgVector (PgVector (..), pgvector)
import Salmon.Builtin.Nodes.Postgres (DatabaseName, Port, dollarQuote, quoteIdent, quoteLiteral)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = FileSkipped !FilePath !Skip
    | Indexed !FilePath !Kind
    | EmbedFailed !FilePath !Text
    | StaleRows !Int
    deriving (Show)

-------------------------------------------------------------------------------
-- Embedders

data Kind = TextKind | ImageKind
    deriving (Eq, Ord, Show)

-- | What the @kind@ column holds.
kindName :: Kind -> Text
kindName TextKind = "text"
kindName ImageKind = "image"

data Input
    = InputText Text
    | InputImage FilePath
    deriving (Eq, Show)

{- | What turns a file into a vector, and what must be up before it can.

'embName' is stored with every row, so a row made by another embedder is
re-embedded. It should change whenever the vectors would (another model,
another build), and must say nothing secret: it is in the table and in the
reports.
-}
data Embedder = Embedder
    { embName :: Text
    , embDimension :: Int
    , embKinds :: [Kind]
    , embNode :: Op
    , embEmbed :: Input -> IO (Either Text [Double])
    }

{- | Text only, through a running server. The node is the caller's (a
'Salmon.Builtin.Nodes.LlamaServer.llamaServerDaemon' for @run serve@, or a
'Salmon.Builtin.Nodes.LlamaServer.llamaServerSystemd'), since which run mode
fits is not this recipe's choice.
-}
llamaEmbedder :: Op -> LlamaServer -> Embedder
llamaEmbedder node s =
    Embedder
        { embName = "llama-server:" <> s.lsName <> ":" <> Text.take 16 s.lsModel.modelSha256
        , embDimension = s.lsDimension
        , embKinds = [TextKind]
        , embNode = node
        , embEmbed = \input -> case input of
            InputText t -> llamaEmbed s t
            InputImage _ -> pure (Left "llama-server embeds no images")
        }

-- | Text and images in one space. A text file is on @extract@'s command line.
clipEmbedder :: Reporter ClipCpp.Report -> ClipCpp.ClipTools -> ClipCpp.ClipCpp -> Embedder
clipEmbedder r tools c =
    Embedder
        { embName = "clip.cpp:" <> Text.take 16 c.ccSource.clipCommit <> ":" <> Text.take 16 c.ccModel.modelSha256
        , embDimension = c.ccDimension
        , embKinds = [TextKind, ImageKind]
        , embNode = ClipCpp.clipCpp r tools c
        , embEmbed = \input ->
            fmap (fmap realToFrac) <$> ClipCpp.embed c (case input of InputText t -> ClipCpp.EmbedText t; InputImage p -> ClipCpp.EmbedImage p)
        }

{- | A stand-in that needs no model: the vector is the sha256 of the bytes,
stretched to the width. It knows nothing of meaning (two files are close
only if they are the same file), but it makes the whole path runnable with
no model and no network, and the same text queried finds its own file at
distance 0. This is what the tests use.
-}
hashEmbedder :: Int -> Embedder
hashEmbedder dim =
    Embedder
        { embName = "sha256-stand-in"
        , embDimension = dim
        , embKinds = [TextKind, ImageKind]
        , embNode = op "pgvector-demo-hash-embedder" nodeps $ \actions ->
            actions
                { help = "a " <> tshow dim <> "-dimensional stand-in embedder (sha256, no model)"
                , ref = mkRef "pgvector-demo-hash-embedder" dim
                , up = pure ()
                }
        , embEmbed = \input -> case input of
            InputText t -> pure (Right (hashVector dim (Text.encodeUtf8 t)))
            InputImage p -> Right . hashVector dim <$> ByteString.readFile p
        }

-- | @dim@ values in @[-1, 1]@ from the bytes; never all zero, so a cosine distance is defined.
hashVector :: Int -> ByteString.ByteString -> [Double]
hashVector dim bytes =
    take dim [scale b | k <- [0 :: Int ..], b <- ByteString.unpack (SHA256.hash (bytes <> C8.pack (show k)))]
  where
    scale b = (fromIntegral b - 127.5) / 127.5

-------------------------------------------------------------------------------
-- The walk

data WalkPolicy = WalkPolicy
    { textExtensions :: [String]
    , imageExtensions :: [String]
    , maxTextBytes :: Integer
    , maxImageBytes :: Integer
    }
    deriving (Eq, Show)

{- | @.txt@, @.md@, @.rst@, @.org@ up to 64 KiB (whole files, no chunking:
a model truncates a long input at its own context anyway); @.jpg@, @.jpeg@,
@.png@, @.gif@ up to 10 MiB.
-}
defaultWalkPolicy :: WalkPolicy
defaultWalkPolicy = WalkPolicy [".txt", ".md", ".rst", ".org"] [".jpg", ".jpeg", ".png", ".gif"] (64 * 1024) (10 * 1024 * 1024)

data Skip
    = Hidden
    | SymbolicLink
    | UnknownExtension
    | TooLarge !Integer
    | Empty
    | NotEmbedded !Kind
    -- ^ the embedder takes no file of this kind
    deriving (Eq, Show)

{- | The verdict on one file, from its path relative to the directory and its
size: a kind, or why it is skipped.
-}
classify :: WalkPolicy -> [Kind] -> FilePath -> Integer -> Either Skip Kind
classify policy kinds rel size
    | any hidden (splitDirectories rel) = Left Hidden
    | otherwise = do
        (kind, cap) <- case map toLower (takeExtension rel) of
            ext
                | ext `elem` policy.textExtensions -> Right (TextKind, policy.maxTextBytes)
                | ext `elem` policy.imageExtensions -> Right (ImageKind, policy.maxImageBytes)
                | otherwise -> Left UnknownExtension
        unless (kind `elem` kinds) (Left (NotEmbedded kind))
        unless (size > 0) (Left Empty)
        unless (size <= cap) (Left (TooLarge size))
        pure kind
  where
    hidden ('.' : _) = True
    hidden _ = False

{- | Every file under the directory, relative to it and sorted, either with
its kind or with why it is skipped. Symbolic links are skipped, not
followed; a hidden directory is not entered.
-}
walk :: WalkPolicy -> [Kind] -> FilePath -> IO ([(FilePath, Kind)], [(FilePath, Skip)])
walk policy kinds root = do
    verdicts <- go ""
    pure ([(p, k) | (p, Right k) <- verdicts], [(p, s) | (p, Left s) <- verdicts])
  where
    go :: FilePath -> IO [(FilePath, Either Skip Kind)]
    go rel = do
        names <- sort <$> listDirectory (root </> rel)
        concat <$> forM names (\name -> visit (if null rel then name else rel </> name))
    visit rel = do
        let path = root </> rel
        link <- pathIsSymbolicLink path
        isDir <- doesDirectoryExist path
        isFile <- doesFileExist path
        case () of
            _
                | link -> pure [(rel, Left SymbolicLink)]
                | isDir -> case classify policy kinds rel 1 of
                    Left Hidden -> pure [(rel, Left Hidden)]
                    _ -> go rel
                | isFile -> do
                    size <- getFileSize path
                    pure [(rel, classify policy kinds rel size)]
                | otherwise -> pure []

-------------------------------------------------------------------------------
-- The nodes

data Demo = Demo
    { pvdDirectory :: FilePath
    , pvdMajor :: Int
    -- ^ the cluster's major version, for the pgvector package
    , pvdPort :: Port
    , pvdDatabase :: DatabaseName
    , pvdTable :: Text
    , pvdPolicy :: WalkPolicy
    }
    deriving (Eq, Show)

-- | Table @items@ and 'defaultWalkPolicy'.
defaultDemo :: FilePath -> Int -> Port -> DatabaseName -> Demo
defaultDemo dir major port db = Demo dir major port db "items" defaultWalkPolicy

{- | The whole demo: pgvector, the schema, the embedder, the index. @psql@,
the package source and the database are the caller's, as for
'Salmon.Builtin.Nodes.PgVector.pgvector'.
-}
pgVectorDemo ::
    Reporter Report ->
    Reporter Postgres.Report ->
    Reporter Package.Report ->
    Track' (Binary "psql") ->
    Track' () ->
    Track' DatabaseName ->
    Demo ->
    Embedder ->
    Op
pgVectorDemo r pgr pkgr psql source mkdb demo emb =
    indexDirectory r demo emb `inject` tables `inject` emb.embNode
  where
    tables = schema demo emb.embDimension `inject` pgvector pgr pkgr psql source mkdb (PgVector demo.pvdMajor demo.pvdPort [demo.pvdDatabase] False)

{- | The table and its index, for vectors of this width. @up@ refuses (and
changes nothing) when the table is there with another width: re-embedding
everything is the operator's decision, made by naming another table. @down@
leaves the table.

Above 2000 dimensions pgvector cannot build an @hnsw@ index on a @vector@,
so there is none and a search is a sequential scan.
-}
schema :: Demo -> Int -> Op
schema demo dim =
    op "pgvector-demo-schema" nodeps $ \actions ->
        actions
            { help = "table " <> demo.pvdTable <> " of vector(" <> tshow dim <> ") in " <> demo.pvdDatabase
            , notes =
                ["down leaves the table"]
                    <> ["no hnsw index above 2000 dimensions: searches scan" | dim > 2000]
            , ref = mkRef "pgvector-demo-schema" (demo.pvdPort, demo.pvdDatabase, demo.pvdTable)
            , check = either (const Unknown) (interpretSchemaRow demo.pvdTable dim) <$> psqlIn demo (inspectSchemaSql demo.pvdTable)
            , up = psqlIn demo (schemaSql demo.pvdTable dim) >>= either (throwIO . userError . Text.unpack) (const (pure ()))
            }

indexName :: Text -> Text
indexName table = table <> "_embedding_hnsw"

-- | Refuse another width, then create the table and (up to 2000 dimensions) the index.
schemaSql :: Text -> Int -> Text
schemaSql table dim =
    Text.unlines $
        [ "DO "
            <> dollarQuote
                ( "DECLARE found text; BEGIN SELECT format_type(atttypid, atttypmod) INTO found FROM pg_attribute WHERE attrelid = to_regclass("
                    <> quoteLiteral (quoteIdent table)
                    <> ") AND attname = 'embedding' AND NOT attisdropped; IF found IS NOT NULL AND found <> "
                    <> quoteLiteral column
                    <> " THEN RAISE EXCEPTION '%', "
                    <> quoteLiteral (table <> ".embedding is ")
                    <> " || found || "
                    <> quoteLiteral (", the embedder produces " <> column <> ": name another table")
                    <> "; END IF; END"
                )
            <> ";"
        , "CREATE TABLE IF NOT EXISTS "
            <> quoteIdent table
            <> " (path text PRIMARY KEY, kind text NOT NULL, content_hash text NOT NULL, embedder text NOT NULL, embedding "
            <> column
            <> " NOT NULL);"
        ]
            <> [ "CREATE INDEX IF NOT EXISTS " <> quoteIdent (indexName table) <> " ON " <> quoteIdent table <> " USING hnsw (embedding vector_cosine_ops);"
               | dim <= 2000
               ]
  where
    column = "vector(" <> tshow dim <> ")"

-- | Empty when there is no table; otherwise @vector(N)|t@ (or @|f@ without the index).
inspectSchemaSql :: Text -> Text
inspectSchemaSql table =
    "SELECT format_type(atttypid, atttypmod) || '|' || (to_regclass("
        <> quoteLiteral (quoteIdent (indexName table))
        <> ") IS NOT NULL)::text FROM pg_attribute WHERE attrelid = to_regclass("
        <> quoteLiteral (quoteIdent table)
        <> ") AND attname = 'embedding' AND NOT attisdropped;"

interpretSchemaRow :: Text -> Int -> Text -> CheckResult
interpretSchemaRow table dim out =
    case Text.splitOn "|" (Text.strip out) of
        [""] -> Failure ("no table " <> table)
        [ty, hasIndex]
            | ty /= column -> Failure (table <> ".embedding is " <> ty <> ", the embedder produces " <> column)
            | dim <= 2000 && hasIndex /= "true" -> Failure ("no hnsw index on " <> table)
            | otherwise -> Success
        _ -> Unknown
  where
    column = "vector(" <> tshow dim <> ")"

-------------------------------------------------------------------------------

-- | A file to index: its path relative to the directory, its kind and the sha256 of its bytes.
data Candidate = Candidate
    { candPath :: FilePath
    , candKind :: Kind
    , candHash :: Text
    }
    deriving (Eq, Show)

{- | Embed every file whose row is missing or stale, and upsert it. A file
that cannot be embedded is reported and the others are still indexed; @up@
then throws, naming how many failed. @down@ leaves the rows.
-}
indexDirectory :: Reporter Report -> Demo -> Embedder -> Op
indexDirectory r demo emb =
    op "pgvector-demo-index" nodeps $ \actions ->
        actions
            { help = "indexes " <> Text.pack demo.pvdDirectory <> " into " <> demo.pvdDatabase <> "." <> demo.pvdTable <> " with " <> emb.embName
            , notes = ["rows of deleted files are left and counted", "down leaves the rows"]
            , ref = mkRef "pgvector-demo-index" (demo.pvdPort, demo.pvdDatabase, demo.pvdTable, demo.pvdDirectory)
            , check = do
                (cands, _) <- candidates
                rows <- psqlIn demo (indexedSql demo.pvdTable)
                pure $ case rows of
                    Left _ -> Unknown
                    Right out -> case pendingFiles emb.embName cands (parseIndexed out) of
                        [] -> Success
                        ps -> Failure (tshow (length ps) <> " file(s) without a current row")
            , up = do
                (cands, skipped) <- candidates
                forM_ skipped $ \(p, s) -> runReporter r (FileSkipped p s)
                rows <- psqlIn demo (indexedSql demo.pvdTable) >>= either (throwIO . userError . Text.unpack) pure
                let indexed = parseIndexed rows
                    stale = staleCount cands indexed
                unless (stale == 0) (runReporter r (StaleRows stale))
                failures <- fmap concat . forM (pendingFiles emb.embName cands indexed) $ \c -> do
                    let path = demo.pvdDirectory </> c.candPath
                    input <- case c.candKind of
                        TextKind -> InputText . Text.decodeUtf8With TextError.lenientDecode <$> ByteString.readFile path
                        ImageKind -> pure (InputImage path)
                    vector <- emb.embEmbed input
                    case vector of
                        Left why -> runReporter r (EmbedFailed c.candPath why) >> pure [c.candPath]
                        Right v
                            | length v /= emb.embDimension -> do
                                runReporter r (EmbedFailed c.candPath ("got " <> tshow (length v) <> " dimensions"))
                                pure [c.candPath]
                            | otherwise -> do
                                psqlIn demo (upsertSql demo.pvdTable emb.embName c v) >>= either (throwIO . userError . Text.unpack) pure
                                runReporter r (Indexed c.candPath c.candKind)
                                pure []
                case failures of
                    [] -> pure ()
                    (first : _) -> throwIO (userError (show (length failures) <> " file(s) could not be embedded, first: " <> first))
            }
  where
    candidates = do
        (files, skipped) <- walk demo.pvdPolicy emb.embKinds demo.pvdDirectory
        cands <- forM files $ \(p, k) -> Candidate p k . contentHash <$> ByteString.readFile (demo.pvdDirectory </> p)
        pure (cands, skipped)

-- | Lower-case hex sha256.
contentHash :: ByteString.ByteString -> Text
contentHash = Text.pack . concatMap (printf "%02x") . ByteString.unpack . SHA256.hash

indexedSql :: Text -> Text
indexedSql table = "SELECT path, content_hash, embedder FROM " <> quoteIdent table <> ";"

-- | Path to (hash, embedder). The two last fields hold no @|@; a path may.
parseIndexed :: Text -> Map.Map FilePath (Text, Text)
parseIndexed = Map.fromList . mapMaybe row . Text.lines
  where
    row line = case reverse (Text.splitOn "|" line) of
        (e : h : rest@(_ : _)) -> Just (Text.unpack (Text.intercalate "|" (reverse rest)), (h, e))
        _ -> Nothing

-- | Files with no row, a row of other content, or a row from another embedder.
pendingFiles :: Text -> [Candidate] -> Map.Map FilePath (Text, Text) -> [Candidate]
pendingFiles name cands indexed =
    [c | c <- cands, Map.lookup c.candPath indexed /= Just (c.candHash, name)]

-- | Rows whose path is no longer a file to index.
staleCount :: [Candidate] -> Map.Map FilePath (Text, Text) -> Int
staleCount cands indexed = Map.size (Map.withoutKeys indexed (Set.fromList (candPath <$> cands)))

upsertSql :: Text -> Text -> Candidate -> [Double] -> Text
upsertSql table name c v =
    "INSERT INTO "
        <> quoteIdent table
        <> " (path, kind, content_hash, embedder, embedding) VALUES ("
        <> Text.intercalate ", " [quoteLiteral (Text.pack c.candPath), quoteLiteral (kindName c.candKind), quoteLiteral c.candHash, quoteLiteral name, quoteLiteral (vectorLiteral v)]
        <> ") ON CONFLICT (path) DO UPDATE SET kind = EXCLUDED.kind, content_hash = EXCLUDED.content_hash, embedder = EXCLUDED.embedder, embedding = EXCLUDED.embedding;"

-- | @[0.25,-1.0e-2]@, as pgvector reads one.
vectorLiteral :: [Double] -> Text
vectorLiteral v = "[" <> Text.intercalate "," (tshow <$> v) <> "]"

-------------------------------------------------------------------------------
-- Search

data Hit = Hit
    { hitPath :: FilePath
    , hitKind :: Text
    , hitDistance :: Double
    }
    deriving (Eq, Show)

{- | The @k@ rows nearest to the query, by cosine distance, among the rows the
same embedder made. Reads only.
-}
search :: Demo -> Embedder -> Input -> Int -> IO (Either Text [Hit])
search demo emb input k = do
    vector <- emb.embEmbed input
    case vector of
        Left why -> pure (Left why)
        Right v -> fmap parseHits <$> psqlIn demo (searchSql demo.pvdTable emb.embName v k)

searchSql :: Text -> Text -> [Double] -> Int -> Text
searchSql table name v k =
    "SELECT path, kind, embedding <=> "
        <> q
        <> " FROM "
        <> quoteIdent table
        <> " WHERE embedder = "
        <> quoteLiteral name
        <> " ORDER BY embedding <=> "
        <> q
        <> " LIMIT "
        <> tshow (max 1 k)
        <> ";"
  where
    q = quoteLiteral (vectorLiteral v) <> "::vector"

parseHits :: Text -> [Hit]
parseHits = mapMaybe row . Text.lines
  where
    row line = case reverse (Text.splitOn "|" line) of
        (d : kind : rest@(_ : _))
            | [(dist, "")] <- reads (Text.unpack d) -> Just (Hit (Text.unpack (Text.intercalate "|" (reverse rest))) kind dist)
        _ -> Nothing

-------------------------------------------------------------------------------

{- | SQL on @psql@'s stdin, as the @postgres@ OS user, unaligned and
@|@-separated, stopping at the first error. On stdin rather than @-c@ since
a statement holds a vector of hundreds of numbers.
-}
psqlIn :: Demo -> Text -> IO (Either Text Text)
psqlIn demo sql = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (proc "sudo" ["-u", "postgres", "psql", "-p", show demo.pvdPort, "-X", "-q", "-tA", "-F", "|", "-v", "ON_ERROR_STOP=1", "-d", Text.unpack demo.pvdDatabase])
            (Text.encodeUtf8 sql)
    pure $ case code of
        ExitSuccess -> Right (decode out)
        ExitFailure n -> Left ("psql exited " <> tshow n <> ": " <> Text.take 500 (Text.strip (decode err)))
  where
    decode = Text.decodeUtf8With TextError.lenientDecode

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
