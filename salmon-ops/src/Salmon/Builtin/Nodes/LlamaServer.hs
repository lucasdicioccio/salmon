{-# LANGUAGE OverloadedStrings #-}

{- | @llama-server@ (llama.cpp) serving an __embedding model__, so text can be
turned into the vectors of a @vector(N)@ column ("Salmon.Builtin.Nodes.PgVector").
Text only; the image-and-text counterpart is a different builtin.

Nodes, in dependency order: 'llamaInstall' (a pinned release archive),
'llamaModel' (a GGUF file with a pinned sha256), then the server in one of
two run modes, and 'llamaReady' to wait for it.

What running build b11195 with a bge-small model showed, which the code is
shaped by:

* @GET \/health@ needs no key and answers @{"status":"ok"}@; refused
  connections are what one sees before it listens. @POST \/v1\/embeddings@
  wants the key when @--api-key-file@ is given (@401@ otherwise) and answers
  @data[0].embedding@, a list of as many numbers as the model has dimensions.
* @--pooling@ is passed explicitly rather than left to the model's default,
  because the vectors an index holds are only comparable with ones produced
  the same way.
* The release archive is a @tar.gz@ whose one top directory is
  @llama-\<build\>@ and whose binary finds its libraries through
  @RUNPATH=$ORIGIN@, so it runs from where it was extracted with no
  environment.

= The check is the dimension

Being up is not the question; producing vectors the column can hold is.
'llamaCheck' asks @\/health@, then embeds a fixed string and compares the
length of the answer with 'lsDimension'. A model swapped for one with another
width, which would make every later insert fail, is a 'Failure' naming both.
pgvector's indexes take at most 2000 dimensions for a @vector@ (4000 for
@halfvec@); a declared dimension above that is put in the node's notes.

= Two run modes

'llamaServerDaemon' is a process salmon owns (@Nodes/Daemon@): it comes up
only under @run serve@, and a one-shot @run up@ refuses it rather than
pretending. That is the mode for demos and tests. 'llamaServerSystemd' is a
unit for a host that should keep it, followed by 'llamaReady' (a model takes
seconds to load and @systemctl restart@ returns at once).

= Exposure

Loopback by default. The key is read from a file and never appears in argv
(nor in the @curl@ that checks the server: its configuration goes to @curl@
on stdin). A unix socket path is an option for owner-only access.
-}
module Salmon.Builtin.Nodes.LlamaServer (
    LlamaRelease (..),
    llamaInstall,
    llamaBinary,
    ModelFile (..),
    llamaModel,
    Pooling (..),
    Listen (..),
    LlamaServer (..),
    defaultLlamaServer,
    serverArgs,
    llamaServerDaemon,
    llamaServerSystemd,
    llamaReady,
    llamaCheck,

    -- * Pieces, exposed for tests
    interpretHealth,
    interpretEmbedding,
    dimensionNote,
    curlConfig,
    curlBase,
    sha256File,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.IO.Exception (ExitCode (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)
import Text.Printf (printf)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Daemon as Daemon
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | A pinned release: the build tag, the archive and the sha256 it must have
-- (GitHub shows it as the asset's digest), and where it is extracted.
data LlamaRelease = LlamaRelease
    { llamaBuild :: Text
    -- ^ @b11195@
    , llamaArchiveUrl :: Text
    , llamaArchiveSha256 :: Text
    , llamaInstallDir :: FilePath
    -- ^ the archive's @llama-\<build\>\/@ lands inside this
    }
    deriving (Eq, Show)

-- | Where the binary is after 'llamaInstall'.
llamaBinary :: LlamaRelease -> FilePath
llamaBinary rel = llamaInstallDir rel </> ("llama-" <> Text.unpack (llamaBuild rel)) </> "llama-server"

{- | Download, refuse unless the sha256 matches, extract. @check@ asks the
binary its build number; @down@ removes the extracted directory (which this
node made, and nothing else lives in).
-}
llamaInstall :: LlamaRelease -> Op
llamaInstall rel =
    op "llama-install" nodeps $ \actions ->
        actions
            { help = "installs llama.cpp " <> rel.llamaBuild
            , notes = ["pinned sha256: " <> rel.llamaArchiveSha256, "from " <> rel.llamaArchiveUrl]
            , ref = mkRef "llama-install" (rel.llamaBuild, rel.llamaInstallDir)
            , check = do
                there <- doesFileExist (llamaBinary rel)
                if not there
                    then pure (Failure ("no binary at " <> Text.pack (llamaBinary rel)))
                    else do
                        (code, out, _) <- readCreateProcessWithExitCode (proc (llamaBinary rel) ["--version"]) ""
                        let said = decode out
                        pure $ case code of
                            ExitSuccess | ("build " <> Text.drop 1 rel.llamaBuild <> ",") `Text.isInfixOf` said -> Success
                            _ -> Failure ("the binary is not " <> rel.llamaBuild)
            , up = do
                let archive = rel.llamaInstallDir </> ("llama-" <> Text.unpack rel.llamaBuild <> ".tar.gz.part")
                createDirectoryIfMissing True rel.llamaInstallDir
                _ <- run' "curl" ["-fsSL", "-o", archive, Text.unpack rel.llamaArchiveUrl]
                actual <- sha256File archive
                when (Text.toLower rel.llamaArchiveSha256 /= actual) $ do
                    removeFile archive
                    ioError (userError ("llama.cpp archive has sha256 " <> Text.unpack actual <> ", expected " <> Text.unpack rel.llamaArchiveSha256))
                _ <- run' "tar" ["xzf", archive, "-C", rel.llamaInstallDir]
                removeFile archive
            , down = removeDirectoryRecursive (takeDirectory (llamaBinary rel))
            }

-------------------------------------------------------------------------------

-- | A GGUF model with the sha256 it must have. Models are large, so this is
-- normally provisioned by somebody else; 'modelUrl' lets the node fetch it once.
data ModelFile = ModelFile
    { modelPath :: FilePath
    , modelSha256 :: Text
    , modelUrl :: Maybe Text
    }
    deriving (Eq, Show)

{- | @check@ hashes the file (streamed: a model is gigabytes). @up@ fetches to
a @.part@ file, verifies, and renames, or says the model has to be
provisioned. @down@ leaves it: a model is expensive to get back and is
not something this node made unless it fetched it.
-}
llamaModel :: ModelFile -> Op
llamaModel m =
    op "llama-model" nodeps $ \actions ->
        actions
            { help = "GGUF model at " <> Text.pack m.modelPath
            , notes = ["pinned sha256: " <> m.modelSha256, "down leaves the file"]
            , ref = mkRef "llama-model" m.modelPath
            , check = do
                there <- doesFileExist m.modelPath
                if not there
                    then pure (Failure ("missing: " <> Text.pack m.modelPath))
                    else do
                        actual <- sha256File m.modelPath
                        pure (if actual == Text.toLower m.modelSha256 then Success else Failure ("sha256 differs: " <> Text.pack m.modelPath))
            , up = case m.modelUrl of
                Nothing -> ioError (userError ("model " <> m.modelPath <> " is missing or is not the pinned one, and no url was given to fetch it"))
                Just url -> do
                    let part = m.modelPath <> ".part"
                    createDirectoryIfMissing True (takeDirectory m.modelPath)
                    _ <- run' "curl" ["-fsSL", "-o", part, Text.unpack url]
                    actual <- sha256File part
                    unless (actual == Text.toLower m.modelSha256) $ do
                        removeFile part
                        ioError (userError ("model has sha256 " <> Text.unpack actual <> ", expected " <> Text.unpack m.modelSha256))
                    renameFile part m.modelPath
            , down = pure ()
            }

-- | Streamed, lower-case hex.
sha256File :: FilePath -> IO Text
sha256File path = hex . SHA256.hashlazy <$> LByteString.readFile path

hex :: ByteString.ByteString -> Text
hex = Text.pack . concatMap (printf "%02x") . ByteString.unpack

-------------------------------------------------------------------------------

data Pooling = PoolNone | PoolMean | PoolCls | PoolLast | PoolRank
    deriving (Eq, Show)

poolingArg :: Pooling -> Text
poolingArg p = case p of
    PoolNone -> "none"
    PoolMean -> "mean"
    PoolCls -> "cls"
    PoolLast -> "last"
    PoolRank -> "rank"

data Listen
    = -- | 127.0.0.1 on this port
      Loopback Int
    | -- | another address; exposing it is the caller's decision
      Address Text Int
    | -- | a unix socket path (owner-only access)
      UnixSocket FilePath
    deriving (Eq, Show)

data LlamaServer = LlamaServer
    { lsName :: Text
    -- ^ identity of the node (and the unit's name)
    , lsRelease :: LlamaRelease
    , lsModel :: ModelFile
    , lsDimension :: Int
    -- ^ what the model must produce, i.e. the @vector(N)@ it feeds
    , lsPooling :: Pooling
    , lsListen :: Listen
    , lsApiKeyFile :: Maybe FilePath
    -- ^ keys, one per line; also what the check authenticates with
    , lsContext :: Maybe Int
    , lsThreads :: Maybe Int
    , lsUser :: Text
    -- ^ the systemd unit's @User=@ (unused by the daemon mode)
    }
    deriving (Eq, Show)

-- | Loopback on 8080, no key, the model's own context and thread count, run as root.
defaultLlamaServer :: Text -> LlamaRelease -> ModelFile -> Int -> Pooling -> LlamaServer
defaultLlamaServer name rel model dim pooling =
    LlamaServer name rel model dim pooling (Loopback 8080) Nothing Nothing Nothing "root"

-- | The arguments after the binary. The key's /path/ only.
serverArgs :: LlamaServer -> [Text]
serverArgs s =
    mconcat
        [ ["-m", Text.pack s.lsModel.modelPath, "--embedding", "--pooling", poolingArg s.lsPooling]
        , case s.lsListen of
            Loopback p -> ["--host", "127.0.0.1", "--port", tshow p]
            Address h p -> ["--host", h, "--port", tshow p]
            UnixSocket path -> ["--host", Text.pack path]
        , maybe [] (\f -> ["--api-key-file", Text.pack f]) s.lsApiKeyFile
        , maybe [] (\n -> ["-c", tshow n]) s.lsContext
        , maybe [] (\n -> ["-t", tshow n]) s.lsThreads
        ]

-- | Set when the dimension is beyond what pgvector indexes.
dimensionNote :: Int -> Maybe Text
dimensionNote n
    | n > 4000 = Just (tshow n <> " dimensions exceeds what pgvector can index at all (4000 for halfvec): truncate the vectors")
    | n > 2000 = Just (tshow n <> " dimensions exceeds the 2000 pgvector indexes for a vector: use halfvec, or truncate")
    | otherwise = Nothing

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-------------------------------------------------------------------------------

{- | A process salmon owns, kept running by @run serve@ (see
"Salmon.Builtin.Nodes.Daemon"). Its @check@ is 'llamaCheck', so the tending
loop notices a server that is up but wrong.
-}
llamaServerDaemon :: Reporter Daemon.Report -> LlamaServer -> Op
llamaServerDaemon r s =
    op "llama-server" (deps [llamaInstall s.lsRelease, llamaModel s.lsModel]) $ \actions ->
        actions
            { help = "keeps llama-server " <> s.lsName <> " running"
            , notes = maybe [] pure (dimensionNote s.lsDimension)
            , ref = mkRef "llama-server" s.lsName
            , managed = Just (Daemon.runDaemon r d)
            , check = llamaCheck s
            , up = throwIO (Daemon.NeedsSupervisor s.lsName)
            , down = pure ()
            }
  where
    d = Daemon.defaultDaemon ("llama-server-" <> s.lsName) (proc (llamaBinary s.lsRelease) (Text.unpack <$> serverArgs s))

{- | A systemd unit that keeps it, then 'llamaReady' on top: depend on the
returned node to depend on a server that produces vectors.
-}
llamaServerSystemd :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> LlamaServer -> Op
llamaServerSystemd r systemctl s =
    llamaReady s 120 `inject` Systemd.systemdService r systemctl trackConfig config
  where
    config :: Systemd.Config
    config =
        Systemd.Config
            Systemd.System
            "/etc/systemd/system"
            ("salmon-llama-server-" <> s.lsName <> ".service")
            (Systemd.Unit ("llama-server from Salmon (" <> s.lsName <> ")") "network-online.target")
            ( Systemd.Service
                Systemd.Simple
                s.lsUser
                s.lsUser
                "077"
                (Systemd.Start (llamaBinary s.lsRelease) (serverArgs s))
                Systemd.OnFailure
                Systemd.Process
                (llamaInstallDir s.lsRelease)
            )
            (Systemd.Install "multi-user.target")
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \_ ->
        op "llama-server-prerequisites" (deps [llamaInstall s.lsRelease, llamaModel s.lsModel]) $ \actions ->
            actions{ref = mkRef "llama-server-prerequisites" s.lsName}

{- | Waits (up to this many seconds) for 'llamaCheck' to pass. For after
something that starts the server and returns before it can answer.
-}
llamaReady :: LlamaServer -> Int -> Op
llamaReady s seconds =
    op "llama-server-ready" nodeps $ \actions ->
        actions
            { help = "llama-server " <> s.lsName <> " produces " <> tshow s.lsDimension <> "-dimensional vectors"
            , notes = maybe [] pure (dimensionNote s.lsDimension)
            , ref = mkRef "llama-server-ready" s.lsName
            , check = llamaCheck s
            , up = wait seconds
            }
  where
    wait n = do
        verdict <- llamaCheck s
        case verdict of
            Success -> pure ()
            Failure why | n <= 0 -> ioError (userError ("llama-server " <> Text.unpack s.lsName <> " is not producing vectors: " <> Text.unpack why))
            _ | n <= 0 -> ioError (userError ("llama-server " <> Text.unpack s.lsName <> " could not be checked in time"))
            _ -> threadDelay 1000000 >> wait (n - 1 :: Int)

-------------------------------------------------------------------------------

{- | @GET \/health@, then embed a fixed string and compare the vector's length
with the declared dimension.
-}
llamaCheck :: LlamaServer -> IO CheckResult
llamaCheck s = do
    key <- traverse readKey s.lsApiKeyFile
    (hcode, hout, _) <- curl (curlConfig s.lsListen "/health" Nothing key)
    let health = interpretHealth hcode (statusOf (decode hout))
    case health of
        Success -> do
            (ecode, eout, _) <- curl (curlConfig s.lsListen "/v1/embeddings" (Just "{\"input\":\"salmon\"}") key)
            pure $ case ecode of
                ExitSuccess -> let (body, status) = splitStatus (decode eout) in interpretEmbedding s.lsDimension status body
                ExitFailure _ -> Unknown
        other -> pure other
  where
    readKey f = Text.strip . Text.takeWhile (/= '\n') . decode <$> ByteString.readFile f
    curl cfg = readCreateProcessWithExitCode (proc "curl" curlBase) (Text.encodeUtf8 cfg)

-- | @curl@'s own arguments: everything else, the key included, is on stdin.
curlBase :: [String]
curlBase = ["-K", "-"]

{- | The @curl@ configuration (read from stdin) for one request. The key is
here and not in argv, where any user could read it off @ps@.
-}
curlConfig :: Listen -> Text -> Maybe Text -> Maybe Text -> Text
curlConfig listen path body key =
    Text.unlines . concat $
        [ ["silent", "max-time = 20", "write-out = \"\\n%{http_code}\""]
        , ["url = " <> quote url]
        , ["unix-socket = " <> quote (Text.pack sock) | UnixSocket sock <- [listen]]
        , ["header = " <> quote ("Authorization: Bearer " <> k) | Just k <- [key]]
        , concat [["header = \"Content-Type: application/json\"", "data = " <> quote b] | Just b <- [body]]
        ]
  where
    url = case listen of
        Loopback p -> "http://127.0.0.1:" <> tshow p <> path
        Address h p -> "http://" <> h <> ":" <> tshow p <> path
        UnixSocket _ -> "http://localhost" <> path
    quote t = "\"" <> Text.replace "\"" "\\\"" (Text.replace "\\" "\\\\" t) <> "\""

-- | The HTTP status from what @curl@ wrote: the body, a newline, the code.
splitStatus :: Text -> (Text, Int)
splitStatus out =
    case Text.breakOnEnd "\n" out of
        (body, code) | [(n, "")] <- reads (Text.unpack (Text.strip code)) -> (Text.dropWhileEnd (== '\n') body, n)
        _ -> (out, 0)

statusOf :: Text -> Int
statusOf = snd . splitStatus

{- | The verdict from @\/health@: refused (curl exit 7) is 'Failure', a
loading model (503) is 'Unknown', so a slow start is waited out rather than
restarted.
-}
interpretHealth :: ExitCode -> Int -> CheckResult
interpretHealth (ExitFailure 7) _ = Failure "llama-server is not listening"
interpretHealth (ExitFailure _) _ = Unknown
interpretHealth ExitSuccess 200 = Success
interpretHealth ExitSuccess 503 = Unknown
interpretHealth ExitSuccess n = Failure ("llama-server answers /health with " <> tshow n)

{- | The verdict from the embedding of a fixed string: the vector must be
as long as declared. A refused key is a 'Failure' too, since nothing else can
tell that the file this node reads is not the one the server was given.
-}
interpretEmbedding :: Int -> Int -> Text -> CheckResult
interpretEmbedding dim status body
    | status == 401 = Failure "the api key was refused"
    | status /= 200 = Unknown
    | otherwise = case Aeson.decodeStrict (Text.encodeUtf8 body) of
        Just (Aeson.Object o)
            | Just (Aeson.Array ds) <- KeyMap.lookup "data" o
            , (Aeson.Object d : _) <- foldr (:) [] ds
            , Just (Aeson.Array v) <- KeyMap.lookup "embedding" d ->
                let n = length v
                 in if n == dim then Success else Failure ("the model produces " <> tshow n <> " dimensions, " <> tshow dim <> " declared")
        _ -> Unknown

-------------------------------------------------------------------------------

decode :: ByteString.ByteString -> Text
decode = Text.decodeUtf8With TextError.lenientDecode

run' :: String -> [String] -> IO Text
run' cmd args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc cmd args) ""
    case code of
        ExitSuccess -> pure (decode out)
        ExitFailure n -> throwIO (Binary.CommandFailedSimple (cmd <> ": " <> take 500 (Text.unpack (decode err))) n)
