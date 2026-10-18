{- | @salmon-ops@'s @http-server@ cabal flag: on by default, and off it must
still build, without warp, warp-tls or wai anywhere in the library's plan.

The default suite can only see the default build (this suite imports
"Salmon.Actions.Serve.Http" itself), so the flag-off side is opt-in: set
@SALMON_TEST_NO_HTTP_SERVER_BUILD=1@ and it builds the library with
@-http-server@ into a temporary @--builddir@, from the project root, and
reads the plan that build wrote. It costs a full compile of @salmon-ops@
(about two minutes when the store already has the dependencies), which is
why it is not in the default run. Without the variable it skips loudly.
-}
module Test.HttpServerFlagSpec (tests) where

import Control.Monad (unless)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Serve.Http as Http
import qualified Salmon.Actions.Serve.HttpBind as HttpBind
import qualified Salmon.Builtin.CommandLine as CLI

import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "salmon-ops http-server flag"
        [ testCase "the default build has the HTTP server" $
            assertBool "httpServerAvailable" CLI.httpServerAvailable
        , testCase "Serve.Http re-exports the listener configuration it moved out" $
            assertEqual "defaultSessionPolicy" HttpBind.defaultSessionPolicy Http.defaultSessionPolicy
        , testCase "the refusal names the flag and both options" $ do
            let msg = Text.pack CLI.httpServerUnavailable
            mapM_ (\w -> assertBool (Text.unpack w) (w `Text.isInfixOf` msg)) ["-http-server", "--http ", "--http-tcp"]
        , testCase "built with -http-server: no warp, warp-tls or wai in the library's plan (opt-in)" withoutHttpServer
        ]

optInEnv :: String
optInEnv = "SALMON_TEST_NO_HTTP_SERVER_BUILD"

withoutHttpServer :: IO ()
withoutHttpServer = do
    requested <- lookupEnv optInEnv
    case requested of
        Just v | not (null v) -> run
        _ -> hPutStrLn stderr ("SKIPPED: set " <> optInEnv <> "=1 to build salmon-ops with -http-server and check its plan")
  where
    run = do
        root <- projectRoot =<< getCurrentDirectory
        withTempDir $ \builddir -> do
            let args = ["build", "salmon-ops:lib:salmon-ops", "--constraint=salmon-ops -http-server", "--builddir=" <> builddir]
            (code, out, err) <- readCreateProcessWithExitCode (proc "cabal" args){cwd = Just root, close_fds = True} ""
            unless (code == ExitSuccess) $
                assertFailure ("cabal " <> unwords args <> " failed:\n" <> lastLines out <> lastLines err)
            plan <- Aeson.eitherDecodeFileStrict (builddir </> "cache" </> "plan.json")
            case plan of
                Left e -> assertFailure ("plan.json: " <> e)
                Right v -> do
                    let names = libraryClosure v
                    assertBool "the closure was found at all" (Set.member "salmon-core" names)
                    mapM_
                        (\p -> assertBool (Text.unpack p <> " is in salmon-ops -http-server's plan") (not (Set.member p names)))
                        ["warp", "warp-tls", "wai"]
    lastLines = unlines . reverse . take 40 . reverse . lines

-- | The directory holding @cabal.project@, from here upwards.
projectRoot :: FilePath -> IO FilePath
projectRoot dir = do
    found <- doesFileExist (dir </> "cabal.project")
    if found
        then pure dir
        else
            let up = takeDirectory dir
             in if up == dir then fail "no cabal.project above the test's directory" else projectRoot up

{- | Package names reachable from @salmon-ops@'s library in a cabal
@plan.json@: the whole project's plan lists every local package, some of
which (this suite) depend on warp themselves, so only the library's own
closure says anything.
-}
libraryClosure :: Value -> Set.Set Text
libraryClosure plan = go Set.empty roots
  where
    units = [u | Object o <- [plan], Just (Array us) <- [KeyMap.lookup "install-plan" o], Object u <- toList us]
    byId = Map.fromList [(i, u) | u <- units, Just (String i) <- [KeyMap.lookup "id" u]]
    str k u = case KeyMap.lookup (Key.fromText k) u of
        Just (String s) -> Just s
        _ -> Nothing
    depsOf u = case KeyMap.lookup "depends" u of
        Just (Array ds) -> [d | String d <- toList ds]
        _ -> case KeyMap.lookup "components" u of
            Just (Object cs) -> concat [[d | String d <- toList ds] | Object c <- KeyMap.elems cs, Just (Array ds) <- [KeyMap.lookup "depends" c]]
            _ -> []
    roots =
        concat
            [ depsOf u
            | u <- units
            , str "pkg-name" u == Just "salmon-ops"
            , str "component-name" u == Just "lib"
            ]
    go seen [] = Set.fromList [n | i <- Set.toList seen, Just u <- [Map.lookup i byId], Just n <- [str "pkg-name" u]]
    go seen (i : rest)
        | Set.member i seen = go seen rest
        | otherwise = go (Set.insert i seen) (maybe [] depsOf (Map.lookup i byId) <> rest)
