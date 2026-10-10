{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for "Salmon.Builtin.Nodes.ClipCpp". __No clip.cpp was built and
no model was loaded__: everything here is Layer 0, plus 'embed' run against a
stub.

* The arguments are compared with what the upstream sources read (the build
  steps of the README, the option names of @CMakeLists.txt@, the flags of
  @examples\/common-clip.cpp@), which is not the same as having run them.
* 'fixture' is __hand-written__, following @writeNpyFile@ in
  @examples\/common-clip.cpp@ byte for byte. It is not a file a real
  @extract@ wrote.
* The stub is a shell script that behaves as @extract@ was read to behave:
  it writes its file into the current directory under upstream's names, and
  it exits 0 when it could not read an image.
-}
module Test.ClipCppSpec (tests) where

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Functor.Identity (runIdentity)
import Data.List (nub, sort)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.Query (pathedNodes)
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.ClipCpp
import Salmon.Builtin.Nodes.Debian.Package (Package (..))
import Salmon.Op.Eval (expand)
import Salmon.Reporter (silent)
import Test.Harness (requireExecutable, runUp, withTempDir)

source :: ClipSource
source = clipSourceAt "/opt/clip"

model :: ModelFile
model = ModelFile "/var/lib/models/clip.gguf" "def456" Nothing

clip :: ClipCpp
clip = defaultClipCpp source model 512

-- | No tool is installed by anybody.
noTools :: ClipTools
noTools = ClipTools ignoreTrack ignoreTrack realNoop

{- | Hand-written: a NumPy 1.0 file as upstream's @writeNpyFile@ lays it out
(magic, version 1.0, a header length of 118, the dictionary padded with
spaces to byte 127, a newline, then the floats), holding @[0.25, -1.5, 3.0]@.
-}
fixture :: ByteString
fixture = npy "(1, 3)" [[0x00, 0x00, 0x80, 0x3e], [0x00, 0x00, 0xc0, 0xbf], [0x00, 0x00, 0x40, 0x40]]

npy :: ByteString -> [[Int]] -> ByteString
npy shape floats =
    mconcat
        [ ByteString.pack [0x93]
        , "NUMPY"
        , ByteString.pack [1, 0, 118, 0]
        , dict
        , ByteString.replicate (128 - (10 + ByteString.length dict + 1)) 0x20
        , ByteString.pack [0x0a]
        , ByteString.pack (fromIntegral <$> concat floats)
        ]
  where
    dict = "{'descr': '<f4', 'fortran_order': False, 'shape': " <> shape <> ", }"

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.ClipCpp"
        [ testGroup
            "build arguments"
            [ testCase "the commit is fetched by name, checked out detached, then its submodules" $
                assertEqual
                    ""
                    [ ["init", "--quiet", "/opt/clip/src"]
                    , ["-C", "/opt/clip/src", "fetch", "--depth", "1", "https://github.com/monatis/clip.cpp.git", "913458d5d1c9238380a0b0826cd1e71c8828a82d"]
                    , ["-C", "/opt/clip/src", "checkout", "--quiet", "--detach", "--force", "913458d5d1c9238380a0b0826cd1e71c8828a82d"]
                    , ["-C", "/opt/clip/src", "submodule", "update", "--init", "--recursive", "--force"]
                    ]
                    (fetchArgs source)
            , testCase "configure: native as declared, upstream's benchmark left out" $ do
                assertEqual "" ["-S", "/opt/clip/src", "-B", "/opt/clip/build", "-DCLIP_NATIVE=ON", "-DCLIP_BUILD_TESTS=OFF"] (configureArgs source)
                assertEqual "" "-DCLIP_NATIVE=OFF" (configureArgs source{clipNative = False} !! 4)
            , testCase "only the extract target is compiled, one job unless told" $ do
                assertEqual "" ["--build", "/opt/clip/build", "--target", "extract"] (compileArgs source)
                assertEqual "" ["--parallel", "3"] (drop 4 (compileArgs source{clipJobs = Just 3}))
            , testCase "the binary is in the build directory's bin" $
                assertEqual "" "/opt/clip/build/bin/extract" (clipExtractBinary source)
            , testCase "only a full commit id is a pin" $ do
                assertBool "the known one" (validCommit knownCommit)
                assertBool "a branch" (not (validCommit "main"))
                assertBool "an abbreviation" (not (validCommit "913458d5"))
                assertBool "forty characters that are not hex" (not (validCommit (Text.replicate 40 "z")))
            , testCase "the stamp names the commit, the remote and the options" $ do
                assertEqual
                    ""
                    "clip.cpp 913458d5d1c9238380a0b0826cd1e71c8828a82d\nfrom https://github.com/monatis/clip.cpp.git\n-DCLIP_NATIVE=ON -DCLIP_BUILD_TESTS=OFF\n"
                    (buildStamp source)
                assertBool "another commit" (buildStamp source /= buildStamp source{clipCommit = Text.replicate 40 "a"})
                assertBool "other options" (buildStamp source /= buildStamp source{clipNative = False})
                assertEqual "more jobs is the same build" (buildStamp source) (buildStamp source{clipJobs = Just 8})
            ]
        , testGroup
            "the build's check and its refusals"
            [ testCase "nothing built, then a stamp without a binary, then both, then another pin" $ withTempDir $ \tmp -> do
                let src = clipSourceAt tmp
                assertEqual "" (Failure ("no build of clip.cpp in " <> Text.pack tmp)) =<< clipBuildCheck src
                createDirectoryIfMissing True (tmp </> "build" </> "bin")
                writeFile (stampPath src) (Text.unpack (buildStamp src))
                assertEqual "" (Failure ("no binary at " <> Text.pack (clipExtractBinary src))) =<< clipBuildCheck src
                writeFile (clipExtractBinary src) ""
                assertEqual "" Success =<< clipBuildCheck src
                assertEqual
                    ""
                    (Failure "the build is of another commit or of other options")
                    =<< clipBuildCheck src{clipCommit = Text.replicate 40 "a"}
            , testCase "a branch name fails the node before any command runs" $ withTempDir $ \tmp -> do
                ok <- runUp (clipBuild silent noTools (clipSourceAt tmp){clipCommit = "main"})
                assertBool "refused" (not ok)
                assertBool "nothing was fetched" . not =<< doesDirectoryExist (tmp </> "src")
            ]
        , testGroup
            "extract arguments"
            [ testCase "a text" $
                assertEqual "" ["-m", "/var/lib/models/clip.gguf", "-v", "0", "--text", "a red apple"] (extractArgs clip (EmbedText "a red apple"))
            , testCase "an image, and threads when given" $
                assertEqual
                    ""
                    ["-m", "/var/lib/models/clip.gguf", "-v", "0", "-t", "2", "--image", "/data/a.jpg"]
                    (extractArgs clip{ccThreads = Just 2} (EmbedImage "/data/a.jpg"))
            , testCase "a text that looks like an option is still the value of --text" $
                assertEqual "" ["--text", "--image"] (drop 4 (extractArgs clip (EmbedText "--image")))
            , testCase "the files upstream names" $ do
                assertEqual "" "text_vec_0.npy" (outputName (EmbedText "anything"))
                assertEqual "" "img_vec_a.jpg.npy" (outputName (EmbedImage "/data/a.jpg"))
            , testCase "the check's image is upstream's own, from the checkout" $
                assertEqual "" (Just "/opt/clip/src/tests/white.jpg") (ccProbeImage clip)
            ]
        , testGroup
            "the .npy file (hand-written fixture)"
            [ testCase "upstream's layout is 128 bytes of header" $
                assertEqual "" (128 + 12) (ByteString.length fixture)
            , testCase "the row is read" $
                assertEqual "" (Right [0.25, -1.5, 3.0]) (parseNpy fixture)
            , testCase "something else is refused" $
                assertEqual "" (Left "not a NumPy file") (parseNpy "Processing: 100.00%\n")
            , testCase "a file cut short is refused, with both counts" $
                assertEqual
                    ""
                    (Left "the header announces 3 floats and 8 bytes follow")
                    (parseNpy (ByteString.take (128 + 8) fixture))
            , testCase "more than one row is refused" $
                assertEqual "" (Left "not a (1, N) array") (parseNpy (npy "(2, 3)" (replicate 6 [0, 0, 0, 0])))
            , testCase "a NaN is refused, since pgvector would refuse it" $
                assertEqual
                    ""
                    (Left "the vector holds a value that is not finite")
                    (parseNpy (npy "(1, 1)" [[0x00, 0x00, 0xc0, 0x7f]]))
            , testCase "as pgvector reads it" $
                assertEqual "" "[0.25,-1.5,3.0]" (pgvectorLiteral [0.25, -1.5, 3.0])
            ]
        , testGroup
            "verdicts"
            [ testCase "both towers at the declared width" $
                assertEqual "" Success (interpretVectors 3 (Right [1, 2, 3]) (Just (Right [4, 5, 6])))
            , testCase "no probe image, text alone decides" $
                assertEqual "" Success (interpretVectors 3 (Right [1, 2, 3]) Nothing)
            , testCase "another width names both" $
                assertEqual
                    ""
                    (Failure "the model produces 3 dimensions for text, 512 declared")
                    (interpretVectors 512 (Right [1, 2, 3]) Nothing)
            , testCase "a vision tower of another width" $
                assertEqual
                    ""
                    (Failure "the model produces 2 dimensions for image, 3 declared")
                    (interpretVectors 3 (Right [1, 2, 3]) (Just (Right [1, 2])))
            , testCase "a tower that gave nothing" $
                assertEqual "" (Failure "image: extract wrote no vector: x") (interpretVectors 3 (Right [1, 2, 3]) (Just (Left "extract wrote no vector: x")))
            ]
        , testGroup
            "graph"
            [ testCase "the build, the model and the build's directory are under the node" $ do
                assertEqual
                    ""
                    [ "clip.cpp produces 512-dimensional vectors for text and images"
                    , "builds clip.cpp 913458d5d1c9 in /opt/clip"
                    , "ensures /opt/clip exists, including subdirs"
                    , "GGUF model at /var/lib/models/clip.gguf"
                    ]
                    (helps (clipCpp silent noTools clip))
            , testCase "debianTools brings git, cmake and a compiler, and nothing else" $
                assertEqual
                    ""
                    (sort [Package "build-essential", Package "cmake", Package "git"])
                    (sort (nub (packagesOf (clipCpp silent debianTools clip))))
            , testCase "with no tools there is no package in the graph" $
                assertEqual "" [] (packagesOf (clipCpp silent noTools clip))
            ]
        , testCase "embed, against a stub that behaves as extract was read to" stubbed
        ]
  where
    packagesOf :: Op -> [Package]
    packagesOf = concatMap snd . collectDynamics
    helps :: Op -> [Text.Text]
    helps o = [h | (_, _, h) <- pathedNodes (runIdentity (expand o))]

{- | A shell script in place of the binary. It logs its directory and
arguments, writes the fixture under the name upstream would use, and for an
image whose name holds @unreadable@ says so and exits 0, as upstream does.
-}
stubScript :: FilePath -> FilePath -> String
stubScript logFile fixtureFile =
    unlines
        [ "#!/bin/sh"
        , "printf '%s\\n' \"$PWD\" \"$@\" > '" <> logFile <> "'"
        , "out=''"
        , "while [ $# -gt 0 ]; do"
        , "  case \"$1\" in"
        , "    --text) out='text_vec_0.npy' ;;"
        , "    --image) case \"$2\" in"
        , "        *unreadable*) echo \"main: failed to load image from '$2'\" >&2 ;;"
        , "        *) out=\"img_vec_$(basename \"$2\").npy\" ;;"
        , "      esac ;;"
        , "  esac"
        , "  shift 2"
        , "done"
        , "if [ -n \"$out\" ]; then cp '" <> fixtureFile <> "' \"./$out\"; fi"
        , "exit 0"
        ]

stubbed :: IO ()
stubbed = requireExecutable "sh" $ withTempDir $ \tmp -> do
    let src = clipSourceAt (tmp </> "clip")
        logFile = tmp </> "argv.log"
        fixtureFile = tmp </> "fixture.npy"
        image = tmp </> "white.jpg"
        c = (defaultClipCpp src (ModelFile (tmp </> "model.gguf") "unused" Nothing) 3){ccProbeImage = Just image}
    createDirectoryIfMissing True (tmp </> "clip" </> "build" </> "bin")
    ByteString.writeFile fixtureFile fixture
    writeFile image ""
    writeFile (clipExtractBinary src) (stubScript logFile fixtureFile)
    setFileMode (clipExtractBinary src) 0o755

    assertEqual "no model yet" (Left ("no model at " <> Text.pack (tmp </> "model.gguf"))) =<< embed c (EmbedText "x")
    writeFile (tmp </> "model.gguf") ""

    assertEqual "a text" (Right [0.25, -1.5, 3.0]) =<< embed c (EmbedText "a red apple")
    logged <- lines <$> readFile logFile
    here <- getCurrentDirectory
    assertEqual "what it was given" ["-m", tmp </> "model.gguf", "-v", "0", "--text", "a red apple"] (drop 1 logged)
    assertBool "it ran in a directory of its own" (take 1 logged /= [here])
    assertBool "which is gone" . not =<< doesDirectoryExist (concat (take 1 logged))

    assertEqual "an image" (Right [0.25, -1.5, 3.0]) =<< embed c (EmbedImage image)
    assertEqual
        "exit 0 and no file is a failure"
        (Left ("extract wrote no vector: main: failed to load image from '" <> Text.pack (tmp </> "unreadable.jpg") <> "'"))
        =<< embed c (EmbedImage (tmp </> "unreadable.jpg"))

    assertEqual "both towers, the declared width" Success =<< clipCheck c
    assertEqual
        "another width declared"
        (Failure "the model produces 3 dimensions for text, 512 declared")
        =<< clipCheck c{ccDimension = 512}
    assertEqual
        "a vision tower that gives nothing"
        (Failure ("image: extract wrote no vector: main: failed to load image from '" <> Text.pack (tmp </> "unreadable.jpg") <> "'"))
        =<< clipCheck c{ccProbeImage = Just (tmp </> "unreadable.jpg")}
