{- | The web UI's tail templates (feature 7fee51f2): the pure contract in
@salmon-ops\/ui\/tail-template.js@, evaluated with the vendored tramaj-js, is
checked by @salmon-ops\/ui-test\/tail-template.test.mjs@ (unit cases and
seeded random properties; @SALMON_UI_TEST_SEED@ fixes the seed). This runs
that script under @node@, skipped loudly when @node@ is not on PATH or the
source tree is not next to the suite.
-}
module Test.UiTailTemplateSpec (tests) where

import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Harness (requireExecutable)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "web UI tail templates (node)"
        [testCase "tail-template.js against tramaj-js: unit cases and properties" runNodeSuite]

runNodeSuite :: IO ()
runNodeSuite = do
    let script = "../salmon-ops/ui-test/tail-template.test.mjs"
    there <- doesFileExist script
    if not there
        then putStrLn "SKIPPED: salmon-ops/ui-test is not next to the test suite; the tail-template checks need the source tree"
        else requireExecutable "node" $ do
            -- stdin is "" (a pipe closed at once), never the suite's own
            (code, out, err) <- readCreateProcessWithExitCode (proc "node" [script]) ""
            unless (code == ExitSuccess) $
                assertFailure ("node " <> script <> " failed (" <> show code <> "):\n" <> out <> err)
