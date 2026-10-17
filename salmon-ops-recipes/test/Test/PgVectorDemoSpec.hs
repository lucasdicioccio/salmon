{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for "SreBox.PgVectorDemo".

Layer 0 ('tests'): the walk (which files, in what order, what is skipped and
why), the SQL the schema, index and search build, the verdicts drawn from
what @psql@ answers, and the graph's shape.

Layer 2 ('sandboxTests'): the schema, the index and the search for real, on
PostgreSQL 17 with Debian's @postgresql-17-pgvector@ in a podman container
(@sudo@ shimmed into it), with the sha256 stand-in embedder so no model and
no network beyond the image's packages are needed. Skipped loudly without
podman.
-}
module Test.PgVectorDemoSpec (tests, sandboxTests) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Functor.Identity (runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Directory (createDirectoryIfMissing, createFileLink)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.Query (pathedNodes)
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Builtin.Nodes.Postgres (PgExtension (..))
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.Eval (expand)
import Salmon.Op.OpGraph (OpGraph (..), inject)
import Salmon.Reporter (ReporterM (..), silent)
import SreBox.PgVectorDemo
import Test.Harness (freeTcpPort, podmanExec_, requireExecutable, runDown, runUp, withContainer, withShimmedPath, withTempDir)

tests :: TestTree
tests =
    testGroup
        "SreBox.PgVectorDemo"
        [ testGroup
            "classify"
            [ testCase "kinds by extension, case-insensitively" $ do
                assertEqual "" (Right TextKind) (classify defaultWalkPolicy both "notes/a.md" 10)
                assertEqual "" (Right ImageKind) (classify defaultWalkPolicy both "IMG.JPG" 10)
                assertEqual "" (Left UnknownExtension) (classify defaultWalkPolicy both "a.pdf" 10)
            , testCase "hidden files and anything under a hidden directory" $ do
                assertEqual "" (Left Hidden) (classify defaultWalkPolicy both ".env.txt" 10)
                assertEqual "" (Left Hidden) (classify defaultWalkPolicy both ".git/description.txt" 10)
            , testCase "a kind the embedder does not take, before the size" $
                assertEqual "" (Left (NotEmbedded ImageKind)) (classify defaultWalkPolicy [TextKind] "a.png" (10 ^ (9 :: Int)))
            , testCase "empty and too large" $ do
                assertEqual "" (Left Empty) (classify defaultWalkPolicy both "a.txt" 0)
                assertEqual "" (Left (TooLarge 65537)) (classify defaultWalkPolicy both "a.txt" 65537)
                assertEqual "" (Right TextKind) (classify defaultWalkPolicy both "a.txt" 65536)
                assertEqual "" (Right ImageKind) (classify defaultWalkPolicy both "a.png" 65537)
            ]
        , testCase "the walk: recursive, sorted, links and hidden directories not entered, every skip said" $ withTempDir $ \dir -> do
            createDirectoryIfMissing True (dir </> "sub")
            createDirectoryIfMissing True (dir </> ".hidden")
            writeFile (dir </> "b.txt") "b"
            writeFile (dir </> "a.md") "a"
            writeFile (dir </> "sub" </> "c.png") "png"
            writeFile (dir </> "sub" </> "empty.txt") ""
            writeFile (dir </> ".hidden" </> "x.txt") "x"
            writeFile (dir </> "doc.pdf") "pdf"
            createFileLink (dir </> "a.md") (dir </> "link.md")
            (files, skipped) <- walk defaultWalkPolicy [TextKind] dir
            assertEqual "files" [("a.md", TextKind), ("b.txt", TextKind)] files
            assertEqual
                "skipped"
                [(".hidden", Hidden), ("doc.pdf", UnknownExtension), ("link.md", SymbolicLink), ("sub/c.png", NotEmbedded ImageKind), ("sub/empty.txt", Empty)]
                skipped
        , testGroup
            "schema"
            [ testCase "the width is the embedder's, with an hnsw cosine index" $ do
                let sql = schemaSql "items" 384
                assertBool "column" ("embedding vector(384) NOT NULL" `Text.isInfixOf` sql)
                assertBool "index" ("CREATE INDEX IF NOT EXISTS \"items_embedding_hnsw\" ON \"items\" USING hnsw (embedding vector_cosine_ops);" `Text.isInfixOf` sql)
                assertBool "another width is refused first" ("found <> 'vector(384)'" `Text.isInfixOf` fst (Text.breakOn "CREATE TABLE" sql))
            , testCase "no index above 2000 dimensions" $
                assertBool "" (not ("CREATE INDEX" `Text.isInfixOf` schemaSql "items" 3072))
            , testCase "a hostile table name is quoted" $
                assertBool "" ("CREATE TABLE IF NOT EXISTS \"a\"\"b\"" `Text.isInfixOf` schemaSql "a\"b" 3)
            , testCase "verdicts" $ do
                assertEqual "absent" (Failure "no table items") (interpretSchemaRow "items" 384 "\n")
                assertEqual "present" Success (interpretSchemaRow "items" 384 "vector(384)|true\n")
                assertEqual "no index" (Failure "no hnsw index on items") (interpretSchemaRow "items" 384 "vector(384)|false")
                assertEqual "no index wanted" Success (interpretSchemaRow "items" 3072 "vector(3072)|false")
                assertEqual "another width" (Failure "items.embedding is vector(512), the embedder produces vector(384)") (interpretSchemaRow "items" 384 "vector(512)|true")
            ]
        , testGroup
            "index"
            [ testCase "what is pending: no row, other content, another embedder; and what is stale" $ do
                let cands = [Candidate "a.txt" TextKind "h1", Candidate "b|c.txt" TextKind "h2", Candidate "d.txt" TextKind "h3", Candidate "e.png" ImageKind "h4"]
                    rows = parseIndexed "a.txt|h1|emb\nb|c.txt|old|emb\nd.txt|h3|other\ngone.txt|h9|emb\n"
                assertEqual "a path holding the separator" (Just ("old", "emb")) (Map.lookup "b|c.txt" rows)
                assertEqual "pending" ["b|c.txt", "d.txt", "e.png"] (candPath <$> pendingFiles "emb" cands rows)
                assertEqual "stale" 1 (staleCount cands rows)
            , testCase "the upsert quotes every value and replaces on the path" $ do
                let sql = upsertSql "items" "emb" (Candidate "it's.txt" TextKind "h") [0.5, -1]
                assertBool "path" ("'it''s.txt'" `Text.isInfixOf` sql)
                assertBool "vector" ("'[0.5,-1.0]'" `Text.isInfixOf` sql)
                assertBool "upsert" ("ON CONFLICT (path) DO UPDATE SET" `Text.isInfixOf` sql)
            , testCase "the content hash is the file's sha256" $
                assertEqual "" "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" (contentHash "hello")
            ]
        , testGroup
            "search"
            [ testCase "cosine distance, among this embedder's rows" $ do
                let sql = searchSql "items" "emb" [1, 0] 5
                assertBool "order" ("ORDER BY embedding <=> '[1.0,0.0]'::vector LIMIT 5;" `Text.isInfixOf` sql)
                assertBool "embedder" ("WHERE embedder = 'emb'" `Text.isInfixOf` sql)
            , testCase "rows, a path holding the separator included" $
                assertEqual "" [Hit "a|b.txt" "text" 0, Hit "c.png" "image" 0.25] (parseHits "a|b.txt|text|0\nc.png|image|0.25\n")
            ]
        , testGroup
            "embedders"
            [ testCase "the stand-in is deterministic, of the width, and never all zero" $ do
                let v = hashVector 100 "salmon"
                assertEqual "width" 100 (length v)
                assertEqual "deterministic" v (hashVector 100 "salmon")
                assertBool "different input, different vector" (v /= hashVector 100 "trout")
                assertBool "in range, not all zero" (all (\x -> x >= -1 && x <= 1) v && any (/= 0) v)
            ]
        , testCase "the graph holds the schema of the embedder's width, the embedder and pgvector" $ do
            let emb = hashEmbedder 8
                demo = defaultDemo "/srv/docs" 17 5432 "app"
                whole = pgVectorDemo silent silent silent ignoreTrack ignoreTrack ignoreTrack demo emb
                paths = [(p, h) | (p, _, h) <- pathedNodes (runIdentity (expand whole))]
                shorthands = snd <$> paths
            assertBool ("the schema names the width: " <> show shorthands) (any ("vector(8)" `Text.isInfixOf`) shorthands)
            assertBool "the stand-in embedder" (any ("stand-in embedder" `Text.isInfixOf`) shorthands)
            assertBool "pgvector" (any ("pgvector for postgresql 17" `Text.isInfixOf`) shorthands)
        ]
  where
    both = [TextKind, ImageKind]

-------------------------------------------------------------------------------
-- Layer 2

sandboxTests :: TestTree
sandboxTests =
    testGroup
        "SreBox.PgVectorDemo (Layer 2, PostgreSQL 17 and pgvector in a podman sandbox)"
        [ testCase "indexes, indexes nothing twice, re-embeds a change, finds a file by its own content, leaves the table on down" sandbox
        ]

sandbox :: IO ()
sandbox = requireExecutable "podman" $ withTempDir $ \tmp -> do
    hostPort <- freeTcpPort
    withContainer (Podman.Image "debian:trixie") (Podman.PortMapping hostPort "5432" Podman.TCPPort) $ \cid -> do
        podmanExec_ cid ["apt-get", "update", "-qq"]
        podmanExec_ cid ["bash", "-c", "DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo postgresql postgresql-17-pgvector"]
        podmanExec_ cid ["bash", "-c", "pg_ctlcluster 17 main status >/dev/null || pg_ctlcluster 17 main start"]
        waitForServer cid
        podmanExec_ cid ["sudo", "-u", "postgres", "createdb", "app"]

        let dir = tmp </> "docs"
        createDirectoryIfMissing True (dir </> "pictures")
        writeFile (dir </> "salmon.txt") "salmon swim upstream to spawn"
        writeFile (dir </> "trout.md") "trout live in rivers and lakes"
        ByteString.writeFile (dir </> "pictures" </> "fish.png") "\x89PNG not really an image, but the stand-in reads bytes"
        writeFile (dir </> "notes.pdf") "skipped"

        events <- newIORef []
        let emb = hashEmbedder 16
            demo = defaultDemo dir 17 5432 "app"
            ext = PgExtension{extName = "vector", extDatabase = "app", extMinServerVersion = Just 130000, extUpgrade = False}
            reporter = ReporterM (\e -> modifyIORef' events (e :))
            whole =
                indexDirectory reporter demo emb
                    `inject` (schema demo emb.embDimension `inject` Postgres.extension silent ignoreTrack 5432 ignoreTrack ext)
                    `inject` emb.embNode
            indexedNow = do
                es <- readIORef events
                writeIORef events []
                pure [p | Indexed p _ <- reverse es]

        withShimmedPath cid ["sudo"] $ do
            ok <- runUp whole
            assertBool "the first pass converges" ok
            indexedNow >>= assertEqual "three files indexed" ["pictures/fish.png", "salmon.txt", "trout.md"]
            checkOf (indexDirectory silent demo emb) >>= assertEqual "the index is satisfied" Success
            checkOf (schema demo 16) >>= assertEqual "the schema is satisfied" Success
            checkOf (schema demo 32)
                >>= assertEqual "another width is seen" (Failure "items.embedding is vector(16), the embedder produces vector(32)")

            again <- runUp whole
            assertBool "a second pass converges" again
            indexedNow >>= assertEqual "and embeds nothing" []

            writeFile (dir </> "trout.md") "trout, now with a changed description"
            checkOf (indexDirectory silent demo emb) >>= assertEqual "a change is noticed" (Failure "1 file(s) without a current row")
            _ <- runUp whole
            indexedNow >>= assertEqual "only the changed file is re-embedded" ["trout.md"]

            hits <- search demo emb (InputText "salmon swim upstream to spawn") 2
            case hits of
                Right (Hit "salmon.txt" "text" d : _) | d < 1.0e-6 -> pure ()
                other -> assertFailure ("the nearest neighbour of a file's content is that file, got " <> show other)
            imageHits <- search demo emb (InputImage (dir </> "pictures" </> "fish.png")) 1
            assertEqual "and of an image, itself" (Right ["pictures/fish.png"]) (fmap (fmap hitPath) imageHits)

            down <- runDown (indexDirectory silent demo emb `inject` schema demo emb.embDimension)
            assertBool "down succeeds" down
        (code, out, _) <- readProcessWithExitCode "podman" ["exec", "-i", "--user", "postgres", cid, "psql", "-X", "-d", "app", "-tAc", "SELECT count(*) FROM items"] ""
        assertEqual "down leaves the rows" (ExitSuccess, "3") (code, takeWhile (/= '\n') out)
  where
    waitForServer cid = go (60 :: Int)
      where
        go 0 = assertFailure "the cluster never answered"
        go n = do
            (code, _, _) <- readProcessWithExitCode "podman" ["exec", "-i", "--user", "postgres", cid, "psql", "-X", "-tAc", "SELECT 1"] ""
            unless (code == ExitSuccess) (threadDelay 1000000 >> go (n - 1))

checkOf :: Op -> IO CheckResult
checkOf o = case o.node of
    Actions act -> act.extension.check
    Actionless -> pure Immaterial
