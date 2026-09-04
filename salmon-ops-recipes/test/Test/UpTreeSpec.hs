{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0/1 coverage for 'Salmon.Actions.UpDown.upTree''s side of the
walk: dependency ordering, one application per 'Ref' however many paths reach
it, and the failure containment that stops a node being evaluated against a
precondition that never arrived.

The mirror of "Test.DownTreeSpec", and deliberately so — since both drivers
now run the same walk over a 'Salmon.Op.Dag.Dag' in opposite directions, the
two suites are the check that the direction is the *only* difference.
-}
module Test.UpTreeSpec (tests) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (elemIndex, sort)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (Report (..))
import Salmon.Builtin.Extension (deps, dynamics, help, nodeps, notes, op, ref, up)
import Salmon.Op.Actions (Act (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)

import Test.Harness (runUpCapturing)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.UpDown.upTree"
        [ testCase "a predecessor is applied before its dependants" predecessorFirst
        , testCase "a shared node is applied exactly once" sharedNodeOnce
        , testCase "a failed node blocks its dependants, not its siblings" failureBlocksDependants
        , testCase "a blocked node blocks its own dependants in turn" blockingIsTransitive
        ]

-------------------------------------------------------------------------------

predecessorFirst :: IO ()
predecessorFirst = do
    logRef <- newIORef []
    let rec name = modifyIORef' logRef (name :)
        shared = op "shared" nodeps $ \x -> x{ref = mkRef "leaf" ("shared" :: Text), up = rec "shared"}
        a = op "a" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("a" :: Text), up = rec "a"}
        b = op "b" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("b" :: Text), up = rec "b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" (), up = rec "root"}
    _ <- runUpCapturing root
    order <- reverse <$> readIORef logRef
    assertEqual "each node applied exactly once" (sort ["root", "a", "b", "shared"]) (sort order)
    before "shared" "a" order
    before "shared" "b" order
    before "a" "root" order
    before "b" "root" order

{- | Same shape reached two ways at once ('inject' + 'deps'), which used to
produce one 'Eval' and one @Redundant@. The collapse to a
'Salmon.Op.Dag.Dag' makes it structurally one node, so there is one report
and nothing to dedupe at walk time.
-}
sharedNodeOnce :: IO ()
sharedNodeOnce = do
    logRef <- newIORef []
    let rec name = modifyIORef' logRef (name :)
        apex = op "apex" nodeps $ \x -> x{ref = mkRef "leaf" ("apex" :: Text), up = rec "apex"}
        left = op "left" (deps [apex]) $ \x -> x{ref = mkRef "mid" ("left" :: Text), up = rec "left"}
        right = (op "right" nodeps $ \x -> x{ref = mkRef "mid" ("right" :: Text), up = rec "right"}) `inject` apex
        root = op "root" (deps [left, right]) $ \x -> x{ref = mkRef "root" (), up = rec "root"}
    reports <- runUpCapturing root
    order <- reverse <$> readIORef logRef
    assertEqual "apex applied exactly once" 1 (length (filter (== "apex") order))
    assertEqual "apex applied first" (Just 0) (elemIndex "apex" order)
    assertEqual
        "and reported exactly once"
        1
        (length [() | Eval act <- reports, act.shorthand == "apex"])
    assertEqual "four nodes, four Evals" 4 (length [() | Eval _ <- reports])

{- | @a@'s @up@ throws, so @root@ (which depends on it) must not be evaluated
against a precondition that never arrived — but @b@, which does not depend on
@a@, is unaffected.
-}
failureBlocksDependants :: IO ()
failureBlocksDependants = do
    logRef <- newIORef []
    let rec name = modifyIORef' logRef (name :)
        a = op "a" nodeps $ \x -> x{ref = mkRef "mid" ("a" :: Text), up = rec "a" >> ioError (userError "boom")}
        b = op "b" nodeps $ \x -> x{ref = mkRef "mid" ("b" :: Text), up = rec "b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" (), up = rec "root"}
    reports <- runUpCapturing root
    order <- reverse <$> readIORef logRef
    assertBool "the failing node's own up still ran" ("a" `elem` order)
    assertBool "an unrelated sibling was still applied" ("b" `elem` order)
    assertBool "the blocked dependant was NOT applied" ("root" `notElem` order)
    assertEqual "and it is reported Blocked" ["root"] [act.shorthand | Blocked act <- reports]
    assertEqual "one node failed" ["a"] [act.shorthand | Failed act _ <- reports]

-- | Blocking is not one level deep: everything above a failure is contained.
blockingIsTransitive :: IO ()
blockingIsTransitive = do
    let a = op "a" nodeps $ \x -> x{ref = mkRef "mid" ("a" :: Text), up = ioError (userError "boom")}
        mid = op "mid" (deps [a]) $ \x -> x{ref = mkRef "mid" ("mid" :: Text)}
        root = op "root" (deps [mid]) $ \x -> x{ref = mkRef "root" ()}
    reports <- runUpCapturing root
    assertEqual
        "both levels above the failure are blocked"
        (sort ["mid", "root"])
        (sort [act.shorthand | Blocked act <- reports])

-------------------------------------------------------------------------------

before :: String -> String -> [String] -> IO ()
before x y order =
    assertBool
        (x <> " must come before " <> y <> " in " <> show order)
        (elemIndex x order < elemIndex y order)
