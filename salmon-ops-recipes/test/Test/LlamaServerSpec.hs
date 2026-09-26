{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for "Salmon.Builtin.Nodes.LlamaServer".

Layer 0 is the argument rendering and the verdicts. The Layer 2 part needs no
model: a small warp server speaks what @llama-server@ was seen to speak
(@GET \/health@ with no key, @POST \/v1\/embeddings@ wanting a bearer key and
answering @data[0].embedding@), and 'llamaCheck' asks it with the real
@curl@ (skipped loudly without one) -- which is the part worth running for
real, since it is @curl@'s configuration syntax, on stdin, that carries the
key.
-}
module Test.LlamaServerSpec (tests) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Char8 as C8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import System.Posix.Files (setFileMode)
import System.FilePath ((</>))

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.LlamaServer
import Test.Harness (requireExecutable, withTempDir)

release :: LlamaRelease
release = LlamaRelease "b11195" "https://example.invalid/llama.tar.gz" "abc123" "/opt/llama"

model :: ModelFile
model = ModelFile "/var/lib/models/bge-small.gguf" "def456" Nothing

server :: LlamaServer
server = defaultLlamaServer "embed" release model 384 PoolCls

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.LlamaServer"
        [ testGroup
            "arguments"
            [ testCase "an embedding server on loopback, pooling explicit" $
                assertEqual
                    ""
                    ["-m", "/var/lib/models/bge-small.gguf", "--embedding", "--pooling", "cls", "--host", "127.0.0.1", "--port", "8080"]
                    (serverArgs server)
            , testCase "the key file is passed by path, and context and threads when given" $
                assertEqual
                    ""
                    ["--api-key-file", "/etc/llama.key", "-c", "512", "-t", "4"]
                    (drop 9 (serverArgs server{lsApiKeyFile = Just "/etc/llama.key", lsContext = Just 512, lsThreads = Just 4}))
            , testCase "a unix socket is a host with no port" $
                assertEqual "" ["--host", "/run/llama.sock"] (take 2 (drop 5 (serverArgs server{lsListen = UnixSocket "/run/llama.sock"})))
            , testCase "the binary is inside the archive's top directory" $
                assertEqual "" "/opt/llama/llama-b11195/llama-server" (llamaBinary release)
            ]
        , testGroup
            "verdicts"
            [ testCase "healthy" $ assertEqual "" Success (interpretHealth ExitSuccess 200)
            , testCase "not listening is a failure" $ assertEqual "" (Failure "llama-server is not listening") (interpretHealth (ExitFailure 7) 0)
            , testCase "a model still loading is cannot-tell, so it is waited out" $ assertEqual "" Unknown (interpretHealth ExitSuccess 503)
            , testCase "the right dimension" $ assertEqual "" Success (interpretEmbedding 3 200 embedding3)
            , testCase "the wrong dimension names both" $
                assertEqual "" (Failure "the model produces 3 dimensions, 384 declared") (interpretEmbedding 384 200 embedding3)
            , testCase "a refused key" $ assertEqual "" (Failure "the api key was refused") (interpretEmbedding 3 401 "{}")
            , testCase "an unreadable answer is cannot-tell" $ assertEqual "" Unknown (interpretEmbedding 3 200 "not json")
            , testCase "a dimension pgvector cannot index is noted, and one it can is not" $ do
                assertEqual "" Nothing (dimensionNote 384)
                assertEqual "" Nothing (dimensionNote 2000)
                assertBool "halfvec" (maybe False ("halfvec" `Text.isInfixOf`) (dimensionNote 3072))
                assertBool "truncate" (maybe False ("truncate" `Text.isInfixOf`) (dimensionNote 8192))
            ]
        , testCase "the key is in curl's stdin configuration and never its argv" $ do
            let cfg = curlConfig (Loopback 8080) "/v1/embeddings" (Just "{\"input\":\"x\"}") (Just "s3cret\"key")
            assertBool "in the config, quoted" ("header = \"Authorization: Bearer s3cret\\\"key\"" `Text.isInfixOf` cfg)
            assertBool "not in argv" (not (any ("s3cret" `isInfixOf'`) curlBase))
        , testCase "against a fake server, over the real curl: healthy, wrong width, wrong key, nobody home" fakeServer
        ]
  where
    embedding3 = "{\"data\":[{\"embedding\":[0.1,0.2,0.3],\"index\":0,\"object\":\"embedding\"}],\"object\":\"list\"}"
    isInfixOf' needle hay = Text.pack needle `Text.isInfixOf` Text.pack hay

-- | What @llama-server@ was seen to answer, with a key it insists on.
fakeApp :: Int -> Text -> IORef Bool -> Wai.Application
fakeApp dim key healthy req respond =
    case (Wai.requestMethod req, Wai.pathInfo req) of
        ("GET", ["health"]) -> do
            ok <- readIORef healthy
            respond (json (if ok then HTTP.status200 else HTTP.status503) (object ["status" .= ("ok" :: Text)]))
        ("POST", ["v1", "embeddings"])
            | lookup "Authorization" (Wai.requestHeaders req) == Just (C8.pack ("Bearer " <> Text.unpack key)) -> do
                body <- Wai.strictRequestBody req
                respond
                    ( json
                        HTTP.status200
                        (object ["object" .= ("list" :: Text), "data" .= [object ["index" .= (0 :: Int), "object" .= ("embedding" :: Text), "embedding" .= replicate dim (0.25 :: Double)]], "echo" .= (fromIntegral (length (show body)) :: Int)])
                    )
            | otherwise -> respond (json HTTP.status401 (object ["error" .= object ["message" .= ("Invalid API Key" :: Text), "code" .= (401 :: Int)]]))
        _ -> respond (json HTTP.status404 (object ["error" .= ("no" :: Text)]))
  where
    json st v = Wai.responseLBS st [(HTTP.hContentType, "application/json")] (encode (v :: Value))

fakeServer :: IO ()
fakeServer = requireExecutable "curl" $ withTempDir $ \tmp -> do
    healthy <- newIORef True
    let keyFile = tmp </> "key"
    writeFile keyFile "top-secret\n"
    setFileMode keyFile 0o600
    Warp.testWithApplication (pure (fakeApp 384 "top-secret" healthy)) $ \port -> do
        let s = server{lsListen = Loopback port, lsApiKeyFile = Just keyFile}
        assertEqual "healthy, the declared width" Success =<< llamaCheck s
        assertEqual
            "a model of another width"
            (Failure "the model produces 384 dimensions, 768 declared")
            =<< llamaCheck s{lsDimension = 768}
        writeFile keyFile "another-key\n"
        assertEqual "the file's key is not the server's" (Failure "the api key was refused") =<< llamaCheck s
        writeFile keyFile "top-secret\n"
        writeIORef healthy False
        assertEqual "still loading" Unknown =<< llamaCheck s
    -- the server is gone: refused connections
    assertEqual "nobody home" (Failure "llama-server is not listening") =<< llamaCheck server{lsListen = Loopback 1}
