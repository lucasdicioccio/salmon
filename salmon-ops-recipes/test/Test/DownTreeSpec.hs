{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0/1 coverage for 'Salmon.Actions.UpDown.downTree''s teardown
ordering, in particular the shared-predecessor case that a naive top-down,
dedupe-by-'Ref' walk gets wrong: a dependency reached through two dependents
must be torn down only after /both/, not at whichever one happens to be
visited first.

We assert on order directly by having each node's 'down' append its name to a
shared log, rather than inferring it from a filesystem side effect.
-}
module Test.DownTreeSpec (tests) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (elemIndex, sort)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Builtin.Extension (Op, deps, down, nodeps, op, ref)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)

import Test.Harness (runDown)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.UpDown.downTree"
        [ testCase "a shared predecessor is torn down after all its dependents" sharedPredecessorLast
        , testCase "a diamond tears the apex down exactly once, last" diamondApexOnceLast
        , testCase "a failed teardown blocks its predecessors, not its siblings" failureBlocksPredecessors
        ]

{- | @root@ depends on @a@ and @b@, both of which depend on the one @shared@
node. Teardown must remove @root@, then @a@ and @b@ (in some order), and only
then @shared@ — never @shared@ while @a@ or @b@ still stands on it.
-}
sharedPredecessorLast :: IO ()
sharedPredecessorLast = do
    logRef <- newIORef []
    let rec name = modifyIORef' logRef (name :)
        shared = op "shared" nodeps $ \x -> x{ref = mkRef "leaf" ("shared" :: Text), down = rec "shared"}
        a = op "a" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("a" :: Text), down = rec "a"}
        b = op "b" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("b" :: Text), down = rec "b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" (), down = rec "root"}
    ok <- runDown root
    order <- reverse <$> readIORef logRef
    assertBool "everything torn down cleanly" ok
    assertEqual "each node torn down exactly once" (sort ["root", "a", "b", "shared"]) (sort order)
    before "root" "a" order
    before "root" "b" order
    before "a" "shared" order
    before "b" "shared" order

-- | Same shape reached two ways at once ('inject' + 'deps'): the apex is
-- still visited a single time and still last.
diamondApexOnceLast :: IO ()
diamondApexOnceLast = do
    logRef <- newIORef []
    let rec name = modifyIORef' logRef (name :)
        apex = op "apex" nodeps $ \x -> x{ref = mkRef "leaf" ("apex" :: Text), down = rec "apex"}
        left = op "left" (deps [apex]) $ \x -> x{ref = mkRef "mid" ("left" :: Text), down = rec "left"}
        -- reach apex a second, structurally different way (Connect, not Vertices)
        right = (op "right" nodeps $ \x -> x{ref = mkRef "mid" ("right" :: Text), down = rec "right"}) `inject` apex
        root = op "root" (deps [left, right]) $ \x -> x{ref = mkRef "root" (), down = rec "root"}
    ok <- runDown root
    order <- reverse <$> readIORef logRef
    assertBool "clean" ok
    assertEqual "apex torn down exactly once" 1 (length (filter (== "apex") order))
    assertEqual "apex torn down last" (Just (length order - 1)) (elemIndex "apex" order)

{- | If @a@'s @down@ throws, @a@ is still standing, so its predecessor
@shared@ must be 'Blocked' (not torn down) — but @b@, which does not depend on
@a@, is unaffected and comes down fine.
-}
failureBlocksPredecessors :: IO ()
failureBlocksPredecessors = do
    logRef <- newIORef []
    let rec name = modifyIORef' logRef (name :)
        shared = op "shared" nodeps $ \x -> x{ref = mkRef "leaf" ("shared" :: Text), down = rec "shared"}
        a = op "a" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("a" :: Text), down = rec "a" >> ioError (userError "boom")}
        b = op "b" nodeps $ \x -> x{ref = mkRef "mid" ("b" :: Text), down = rec "b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" (), down = rec "root"}
    ok <- runDown root
    order <- reverse <$> readIORef logRef
    assertBool "a failure makes the whole run report unclean" (not ok)
    assertBool "the failing node's own down still ran" ("a" `elem` order)
    assertBool "an unrelated sibling was still torn down" ("b" `elem` order)
    assertBool "the blocked predecessor was NOT torn down" ("shared" `notElem` order)

-------------------------------------------------------------------------------

before :: String -> String -> [String] -> IO ()
before x y order =
    assertBool
        (x <> " must come before " <> y <> " in " <> show order)
        (elemIndex x order < elemIndex y order)
