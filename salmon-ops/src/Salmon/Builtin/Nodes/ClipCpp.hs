{-# LANGUAGE OverloadedStrings #-}

{- | @clip.cpp@ (a ggml port of CLIP), so that __text and images__ are turned
into vectors of the same space and of one @vector(N)@ column
("Salmon.Builtin.Nodes.PgVector"): searching images by a sentence, or by
another image, is then a distance query. The text-only counterpart, with a
server, is "Salmon.Builtin.Nodes.LlamaServer".

Nodes, in dependency order: 'clipBuild' (a pinned commit, compiled), the
model ('clipModel', a GGUF file with a pinned sha256), and 'clipCpp' on top,
which is satisfied when the two together produce vectors of the declared
width. There is no server: clip.cpp is a command per input, and 'embed' is
the function a loader calls.

__Nothing here was compiled or run.__ What follows was read from the
upstream sources at 'knownCommit', and the code is shaped by it:

* There is no release binary. The build is @git clone --recurse-submodules@,
  @cmake -DCLIP_NATIVE=ON@, @make@, with the binaries in @build\/bin@. The
  ggml submodule is recorded in the commit, so pinning the commit pins it.
* The binary that writes vectors is @extract@. Its arguments are
  @-m \<model\>@, @-t \<threads\>@, @-v \<level\>@ and any number of
  @--text \<text\>@ and @--image \<path\>@. It has no @--version@.
* It does not print a vector: it writes @.\/text_vec_\<n\>.npy@ and
  @.\/img_vec_\<basename\>.npy@ __into its current directory__, as NumPy 1.0
  files holding a @(1, N)@ array of little-endian 32-bit floats.
* __It exits 0 when it could not read an image or tokenize a text__ (it
  prints and moves to the next input). So 'embed' never trusts the exit code:
  it runs @extract@ in a directory of its own with one input and wants the
  one file.
* The vectors are not normalised (@extract@ asks for none). Use a cosine
  distance (@\<=\>@, @vector_cosine_ops@), or normalise before storing.
* The README still says images are resized by linear interpolation; since
  June 2025 the source resamples bicubically and crops the centre. An
  embedding is therefore only comparable with ones made by the same commit
  and the same model, which is why both are pinned and why the build's
  'ClipSource' is part of 'ClipCpp'.
* @-DCLIP_NATIVE=ON@ is @-march=native@: the binary is for the machine that
  built it. Turning it off still leaves upstream's default @-mavx2@, @-mfma@
  and @-mf16c@ on x86.

= The checks

'clipBuild' cannot ask the binary which commit it is, so it leaves a stamp
next to it once the build has succeeded, naming the commit, the remote and
the options, and its @check@ compares that. A changed pin is a changed
stamp, and the build directory is removed before building again.

'clipCpp' embeds a fixed string, and an image when one is given (upstream
ships @tests\/white.jpg@), and compares both lengths with 'ccDimension', the
way the llama-server node does: a model of another width would make every
later insert fail.

= What is public

The text given to 'embed' is on @extract@'s command line, where any user of
the machine can read it. Do not embed a secret.
-}
module Salmon.Builtin.Nodes.ClipCpp (
    Report (..),

    -- * The build
    ClipSource (..),
    upstreamRemote,
    knownCommit,
    clipSourceAt,
    ClipTools (..),
    debianTools,
    clipBuild,
    clipBuildCheck,
    clipExtractBinary,

    -- * The model
    ModelFile (..),
    clipModel,
    laionViTB32,

    -- * Vectors
    ClipCpp (..),
    defaultClipCpp,
    clipCpp,
    clipCheck,
    Input (..),
    embed,
    pgvectorLiteral,

    -- * Pieces, exposed for tests
    validCommit,
    fetchArgs,
    configureArgs,
    compileArgs,
    buildStamp,
    stampPath,
    extractArgs,
    outputName,
    parseNpy,
    interpretVectors,
) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import GHC.IO.Exception (ExitCode (..))
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    makeAbsolute,
    removeDirectoryRecursive,
    renameFile,
 )
import System.FilePath (takeFileName, (</>))
import System.Posix.Temp (mkdtemp)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import Salmon.Builtin.Nodes.Filesystem (Directory (..), dir)
import Salmon.Builtin.Nodes.LlamaServer (ModelFile (..), dimensionNote, ggufModel)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = Fetching !ClipSource !Binary.Report
    | Building !ClipSource !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

{- | What is built, and where. 'clipDir' gets a @src@ (the checkout) and a
@build@ directory, both this node's own.
-}
data ClipSource = ClipSource
    { clipRemote :: Text
    , clipCommit :: Text
    -- ^ a full object name (40 or 64 hex digits). A branch or a tag is not a
    -- pin and is refused.
    , clipDir :: FilePath
    , clipNative :: Bool
    -- ^ @-DCLIP_NATIVE@, i.e. @-march=native@ (upstream's default is on)
    , clipJobs :: Maybe Int
    -- ^ parallel compile jobs; one at a time when 'Nothing'
    }
    deriving (Eq, Show)

upstreamRemote :: Text
upstreamRemote = "https://github.com/monatis/clip.cpp.git"

{- | The commit whose sources this module was written against (the tip of
upstream's @main@ in October 2026). Read, not built.
-}
knownCommit :: Text
knownCommit = "913458d5d1c9238380a0b0826cd1e71c8828a82d"

-- | 'knownCommit' from upstream, native, one job, in this directory.
clipSourceAt :: FilePath -> ClipSource
clipSourceAt d = ClipSource upstreamRemote knownCommit d True Nothing

{- | Where the tools come from. The compiler and @make@ are found by @cmake@
on its own, so they are a plain node and not a binary anything here calls.
-}
data ClipTools = ClipTools
    { toolGit :: Track' (Binary "git")
    , toolCmake :: Track' (Binary "cmake")
    , toolCompiler :: Op
    }

-- | @git@, @cmake@ and @build-essential@ from Debian's packages.
debianTools :: ClipTools
debianTools =
    ClipTools
        (Track $ \_ -> Debian.deb (Debian.Package "git"))
        (Track $ \_ -> Debian.deb (Debian.Package "cmake"))
        (Debian.deb (Debian.Package "build-essential"))

-- | Where @extract@ is after 'clipBuild'.
clipExtractBinary :: ClipSource -> FilePath
clipExtractBinary src = buildDir src </> "bin" </> "extract"

sourceDir, buildDir, stampPath :: ClipSource -> FilePath
sourceDir src = src.clipDir </> "src"
buildDir src = src.clipDir </> "build"
stampPath src = buildDir src </> "salmon-clip-cpp.stamp"

-- | A full object name: nothing a remote can move.
validCommit :: Text -> Bool
validCommit c = Text.length c `elem` [40, 64] && Text.all isHexDigit c

{- | The @git@ arguments that put the pinned commit, and the submodules it
records, in 'clipDir'@\/src@. Every step can be run again: @init@ on an
existing repository changes nothing, and the checkout is forced because the
directory is nobody's to edit. The commit is fetched by name with no history;
the submodules are fetched whole, since a shallow clone of one may not hold
the commit the superproject wants.
-}
fetchArgs :: ClipSource -> [[String]]
fetchArgs src =
    [ ["init", "--quiet", sourceDir src]
    , inSrc ["fetch", "--depth", "1", Text.unpack src.clipRemote, commit]
    , inSrc ["checkout", "--quiet", "--detach", "--force", commit]
    , inSrc ["submodule", "update", "--init", "--recursive", "--force"]
    ]
  where
    commit = Text.unpack src.clipCommit
    inSrc args = ["-C", sourceDir src] <> args

{- | @cmake@'s configure step. Upstream's tests (a benchmark over an image
set) are left out; the examples, where @extract@ is, are on by default.
-}
configureArgs :: ClipSource -> [String]
configureArgs src =
    [ "-S"
    , sourceDir src
    , "-B"
    , buildDir src
    , "-DCLIP_NATIVE=" <> (if src.clipNative then "ON" else "OFF")
    , "-DCLIP_BUILD_TESTS=OFF"
    ]

-- | Only the @extract@ target and what it needs.
compileArgs :: ClipSource -> [String]
compileArgs src =
    ["--build", buildDir src, "--target", "extract"]
        <> maybe [] (\n -> ["--parallel", show n]) src.clipJobs

-- | What the build leaves behind once it has succeeded, and what @check@ wants to find.
buildStamp :: ClipSource -> Text
buildStamp src =
    Text.unlines
        [ "clip.cpp " <> src.clipCommit
        , "from " <> src.clipRemote
        , Text.unwords (Text.pack <$> drop 4 (configureArgs src))
        ]

{- | Fetch the pinned commit and compile @extract@, as one node: the steps in
between are nobody's to depend on. @down@ removes the checkout and the build
directory, and leaves 'clipDir'.
-}
clipBuild :: Reporter Report -> ClipTools -> ClipSource -> Op
clipBuild r tools src =
    op "clip-cpp-build" (deps [dir (Directory src.clipDir), Binary.justInstall tools.toolGit, Binary.justInstall tools.toolCmake, tools.toolCompiler]) $ \actions ->
        actions
            { help = "builds clip.cpp " <> Text.take 12 src.clipCommit <> " in " <> Text.pack src.clipDir
            , notes = ["pinned commit: " <> src.clipCommit, "from " <> src.clipRemote] <> ["built for this machine's CPU (-march=native)" | src.clipNative]
            , ref = mkRef "clip-cpp-build" src.clipDir
            , check = clipBuildCheck src
            , up = do
                unless (validCommit src.clipCommit) $
                    ioError (userError ("clip.cpp must be pinned to a full commit id, not " <> show src.clipCommit))
                stamp <- readStamp src
                when (stamp /= Just (buildStamp src)) $ removeIfThere (buildDir src)
                forM_ (fetchArgs src) $ \args ->
                    Binary.untrackedExec gitCommand args "" (contramap (Fetching src) r)
                forM_ [configureArgs src, compileArgs src] $ \args ->
                    Binary.untrackedExecWith Binary.streamed cmakeCommand args "" (contramap (Building src) r)
                built <- doesFileExist (clipExtractBinary src)
                unless built $
                    ioError (userError ("the build succeeded and left no " <> clipExtractBinary src))
                let part = stampPath src <> ".part"
                ByteString.writeFile part (Text.encodeUtf8 (buildStamp src))
                renameFile part (stampPath src)
            , down = removeIfThere (buildDir src) >> removeIfThere (sourceDir src)
            }
  where
    removeIfThere d = do
        there <- doesDirectoryExist d
        when there (removeDirectoryRecursive d)

-- | The stamp is the one this declaration would leave, and the binary is there.
clipBuildCheck :: ClipSource -> IO CheckResult
clipBuildCheck src = do
    stamp <- readStamp src
    there <- doesFileExist (clipExtractBinary src)
    pure $ case (stamp, there) of
        (Nothing, _) -> Failure ("no build of clip.cpp in " <> Text.pack src.clipDir)
        (Just s, _) | s /= buildStamp src -> Failure "the build is of another commit or of other options"
        (_, False) -> Failure ("no binary at " <> Text.pack (clipExtractBinary src))
        _ -> Success

readStamp :: ClipSource -> IO (Maybe Text)
readStamp src = do
    there <- doesFileExist (stampPath src)
    if there then Just . decode <$> ByteString.readFile (stampPath src) else pure Nothing

gitCommand :: Binary.Command "git" [String]
gitCommand = Binary.Command (proc "git")

cmakeCommand :: Binary.Command "cmake" [String]
cmakeCommand = Binary.Command (proc "cmake")

-------------------------------------------------------------------------------

{- | A GGUF model with a pinned sha256: "Salmon.Builtin.Nodes.LlamaServer"'s
model node under a kind of its own. For text and images together it has to be
a two-tower file, not one of the @text-model@ or @vision-model@ ones.
-}
clipModel :: ModelFile -> Op
clipModel = ggufModel "clip-model"

{- | LAION's CLIP ViT-B\/32, 16-bit floats, two towers, as converted for
clip.cpp by the Hugging Face repository @mys\/ggml_CLIP-ViT-B-32-laion2B-s34B-b79K@
(tagged @clip-cpp-gguf@), at that repository's revision @26ebd3e1@. The
sha256 is the one the hub lists for the file (304 MB); __the file was never
downloaded, hashed or loaded here__, and that a file converted in September
2023 loads with 'knownCommit' is not verified. ViT-B\/32 projects to 512
dimensions.
-}
laionViTB32 :: FilePath -> ModelFile
laionViTB32 path =
    ModelFile
        path
        "8140b294ed52b960e4fe6c1f17dfc25bd36086b2744eaa242fd42ed98a4f6499"
        (Just "https://huggingface.co/mys/ggml_CLIP-ViT-B-32-laion2B-s34B-b79K/resolve/26ebd3e1648320e965df9e69ca01963d144cb380/CLIP-ViT-B-32-laion2B-s34B-b79K_ggml-model-f16.gguf")

-------------------------------------------------------------------------------

-- | A build and a model, and the width of what they produce together.
data ClipCpp = ClipCpp
    { ccSource :: ClipSource
    , ccModel :: ModelFile
    , ccDimension :: Int
    -- ^ what the model must produce, i.e. the @vector(N)@ it feeds
    , ccThreads :: Maybe Int
    -- ^ @-t@; upstream's default (4) when 'Nothing'
    , ccProbeImage :: Maybe FilePath
    -- ^ an image the check embeds, so the vision tower is checked too
    }
    deriving (Eq, Show)

-- | Upstream's thread count, and upstream's own @tests\/white.jpg@ as the image the check embeds.
defaultClipCpp :: ClipSource -> ModelFile -> Int -> ClipCpp
defaultClipCpp src model dim =
    ClipCpp src model dim Nothing (Just (sourceDir src </> "tests" </> "white.jpg"))

{- | The build and the model, producing vectors of the declared width: depend
on this to depend on something 'embed' can be called on. It applies nothing
of its own, so its @up@ is the check again, failing with what it found.
-}
clipCpp :: Reporter Report -> ClipTools -> ClipCpp -> Op
clipCpp r tools c =
    op "clip-cpp" (deps [clipBuild r tools c.ccSource, clipModel c.ccModel]) $ \actions ->
        actions
            { help = "clip.cpp produces " <> tshow c.ccDimension <> "-dimensional vectors for text and images"
            , notes = maybe [] pure (dimensionNote c.ccDimension)
            , ref = mkRef "clip-cpp" (c.ccSource.clipDir, c.ccModel.modelPath)
            , check = clipCheck c
            , up = do
                verdict <- clipCheck c
                case verdict of
                    Success -> pure ()
                    Failure why -> ioError (userError ("clip.cpp is not producing the declared vectors: " <> Text.unpack why))
                    _ -> ioError (userError "clip.cpp could not be checked")
            }

-- | Embed a fixed string, and the probe image if there is one, and compare the lengths with the declared one.
clipCheck :: ClipCpp -> IO CheckResult
clipCheck c = do
    text <- embed c (EmbedText "salmon")
    image <- traverse (embed c . EmbedImage) c.ccProbeImage
    pure (interpretVectors c.ccDimension text image)

{- | The verdict from the two probes. Either tower failing, or either having
another width than declared, is a 'Failure' naming it.
-}
interpretVectors :: Int -> Either Text [Float] -> Maybe (Either Text [Float]) -> CheckResult
interpretVectors dim text image =
    case (width "text" text, maybe (Right ()) (width "image") image) of
        (Left why, _) -> Failure why
        (_, Left why) -> Failure why
        _ -> Success
  where
    width what (Left why) = Left (what <> ": " <> why)
    width what (Right v)
        | length v == dim = Right ()
        | otherwise = Left ("the model produces " <> tshow (length v) <> " dimensions for " <> what <> ", " <> tshow dim <> " declared")

-------------------------------------------------------------------------------

data Input
    = -- | on the command line, hence public
      EmbedText Text
    | -- | jpg, png or gif, by path
      EmbedImage FilePath
    deriving (Eq, Show)

-- | The arguments after the binary, for one input.
extractArgs :: ClipCpp -> Input -> [String]
extractArgs c input =
    mconcat
        [ ["-m", c.ccModel.modelPath, "-v", "0"]
        , maybe [] (\n -> ["-t", show n]) c.ccThreads
        , case input of
            EmbedText t -> ["--text", Text.unpack t]
            EmbedImage p -> ["--image", p]
        ]

-- | The file @extract@ writes in its current directory for this input, given alone.
outputName :: Input -> FilePath
outputName (EmbedText _) = "text_vec_0.npy"
outputName (EmbedImage p) = "img_vec_" <> takeFileName p <> ".npy"

{- | The vector of one text or one image, or why there is none. @extract@ is
run in a fresh directory that is removed afterwards, with nothing on its
standard input, and the file it leaves there is the answer: a zero exit code
with no file is a failure. One process, and one load of the model, per call.
-}
embed :: ClipCpp -> Input -> IO (Either Text [Float])
embed c input0 = do
    input <- case input0 of
        EmbedImage p -> EmbedImage <$> makeAbsolute p
        t -> pure t
    binaryThere <- doesFileExist binary
    modelThere <- doesFileExist c.ccModel.modelPath
    case (binaryThere, modelThere) of
        (False, _) -> pure (Left ("no binary at " <> Text.pack binary))
        (_, False) -> pure (Left ("no model at " <> Text.pack c.ccModel.modelPath))
        _ -> do
            tmp <- getTemporaryDirectory
            bracket (mkdtemp (tmp </> "salmon-clip-cpp-")) removeDirectoryRecursive $ \scratch -> do
                (code, out, err) <- readCreateProcessWithExitCode ((proc binary (extractArgs c input)){cwd = Just scratch}) ""
                let said = Text.takeEnd 500 (Text.strip (decode out <> "\n" <> decode err))
                    vector = scratch </> outputName input
                written <- doesFileExist vector
                case code of
                    ExitFailure n -> pure (Left ("extract exited with " <> tshow n <> ": " <> said))
                    ExitSuccess
                        | not written -> pure (Left ("extract wrote no vector: " <> said))
                        | otherwise -> parseNpy <$> ByteString.readFile vector
  where
    binary = clipExtractBinary c.ccSource

{- | The one row of a NumPy 1.0 file as @extract@ writes it: the magic, the
version, a 16-bit header length, a header saying @\<f4@, C order and a
@(1, N)@ shape, then N little-endian floats. Anything else is refused, and so
is a value pgvector would refuse (an infinity, a NaN).
-}
parseNpy :: ByteString -> Either Text [Float]
parseNpy bytes = do
    unless (ByteString.take 6 bytes == "\x93NUMPY") (Left "not a NumPy file")
    unless (ByteString.length bytes >= 10) (Left "the NumPy header is cut short")
    unless (ByteString.index bytes 6 == 1 && ByteString.index bytes 7 == 0) (Left "not a version 1.0 NumPy file")
    let headerLen = fromIntegral (ByteString.index bytes 8) + 256 * fromIntegral (ByteString.index bytes 9)
        header = decode (ByteString.take headerLen (ByteString.drop 10 bytes))
        payload = ByteString.drop (10 + headerLen) bytes
    unless ("'descr': '<f4'" `Text.isInfixOf` header) (Left "not little-endian 32-bit floats")
    unless ("'fortran_order': False" `Text.isInfixOf` header) (Left "not in C order")
    n <- case Text.splitOn "," . Text.takeWhile (/= ')') . Text.drop 1 . snd . Text.breakOn "(" . snd . Text.breakOn "'shape':" $ header of
        [one, len]
            | [(1 :: Int, "")] <- reads (Text.unpack (Text.strip one))
            , [(k, "")] <- reads (Text.unpack (Text.strip len))
            , k > 0 ->
                Right k
        _ -> Left "not a (1, N) array"
    unless (ByteString.length payload == 4 * n) $
        Left ("the header announces " <> tshow n <> " floats and " <> tshow (ByteString.length payload) <> " bytes follow")
    let values = float <$> chunks payload
    unless (all finite values) (Left "the vector holds a value that is not finite")
    pure values
  where
    chunks b
        | ByteString.null b = []
        | otherwise = ByteString.take 4 b : chunks (ByteString.drop 4 b)
    float b = castWord32ToFloat (ByteString.foldr (\byte acc -> (acc `shiftL` 8) .|. fromIntegral byte) (0 :: Word32) b)
    finite x = not (isNaN x || isInfinite x)

-- | A vector as pgvector reads one: @[0.25,-1.0e-2]@.
pgvectorLiteral :: [Float] -> Text
pgvectorLiteral v = "[" <> Text.intercalate "," (tshow <$> v) <> "]"

-------------------------------------------------------------------------------

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

decode :: ByteString -> Text
decode = Text.decodeUtf8With TextError.lenientDecode
