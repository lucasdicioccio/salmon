{- | Layer 0 coverage for "Salmon.Actions.Query" per @specs/advance-querying.md@:
pattern matching, selector resolution against a graph with a repeated
subtree (the same @Ref@ reachable at two paths — the "passwordless"/"chown"
shape the spec calls out), and 'Salmon.Actions.Query.forceSkip' actually
turning an excluded node into a 'Salmon.Actions.UpDown.Skip' at 'upTree'
time while leaving everything else unaffected.
-}
module Test.QuerySpec (tests) where

import Control.Monad.Identity (runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Salmon.Actions.Query as Query
import Salmon.Actions.UpDown (Report (..))
import Salmon.Builtin.Extension (Extension (..), Op, deps, nodeps, op, ref, up)
import Salmon.Op.Actions (extension)
import Salmon.Op.Eval (expand)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)

import Test.Harness (runUpCapturing)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Query"
        [ testCase "* matches exactly one segment, ** matches any depth including zero" patternMatching
        , testCase "resolveSelectors: a shared predecessor is one Ref, matched at both its paths" sharedPredecessorRefs
        , testCase "resolveSelectors: empty --select means everything, minus --exclude" selectDefaultsToEverything
        , testCase "forceSkip makes upTree report Skip for the excluded node, Eval for the rest" forceSkipSkipsOnlyExcluded
        ]

patternMatching :: IO ()
patternMatching = do
    assertBool "exact path matches itself" (Query.matchPattern (Query.parsePattern "/a/b/c") ["a", "b", "c"])
    assertBool "exact path does not match a different one" (not (Query.matchPattern (Query.parsePattern "/a/b/c") ["a", "b", "d"]))
    assertBool "* matches exactly one segment" (Query.matchPattern (Query.parsePattern "/a/*/c") ["a", "b", "c"])
    assertBool "* does not match zero segments" (not (Query.matchPattern (Query.parsePattern "/a/*/c") ["a", "c"]))
    assertBool "* does not match two segments" (not (Query.matchPattern (Query.parsePattern "/a/*/c") ["a", "b", "b2", "c"]))
    assertBool "** matches any depth" (Query.matchPattern (Query.parsePattern "/a/**") ["a", "b", "c", "d"])
    assertBool "** matches zero segments" (Query.matchPattern (Query.parsePattern "/a/**") ["a"])
    assertBool "** alone matches everything" (Query.matchPattern (Query.parsePattern "/**") ["a", "b", "c"])

-- | @root@ depends on @a@ and @b@, both of which depend on the one @shared@
-- node — same shape 'Test.DownTreeSpec.sharedPredecessorLast' uses.
sharedGraph :: Op
sharedGraph = root
  where
    shared = op "shared" nodeps $ \x -> x{ref = mkRef "leaf" ("shared" :: Text)}
    a = op "a" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("a" :: Text)}
    b = op "b" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("b" :: Text)}
    root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" ()}

sharedPredecessorRefs :: IO ()
sharedPredecessorRefs = do
    let cograph = runIdentity (expand sharedGraph)
        entries = Query.pathedRefs cograph
        sharedPaths = [path | (path, r) <- entries, r == mkRef "leaf" ("shared" :: Text)]
    assertEqual "the shared Ref appears at two distinct paths" 2 (length sharedPaths)
    assertBool "reachable through /root/a/shared" (["root", "a", "shared"] `elem` sharedPaths)
    assertBool "reachable through /root/b/shared" (["root", "b", "shared"] `elem` sharedPaths)
    let (selected, excluded) = Query.resolveSelectors cograph [] ["/root/a/**"]
    assertBool "excluding through one path excludes the shared Ref" (mkRef "leaf" ("shared" :: Text) `Set.member` excluded)
    assertBool "the shared Ref is therefore not selected either" (mkRef "leaf" ("shared" :: Text) `Set.notMember` selected)
    assertBool "'a' itself is excluded" (mkRef "mid" ("a" :: Text) `Set.member` excluded)
    assertBool "'b' is untouched" (mkRef "mid" ("b" :: Text) `Set.notMember` excluded)

selectDefaultsToEverything :: IO ()
selectDefaultsToEverything = do
    let cograph = runIdentity (expand sharedGraph)
        allRefs = Set.fromList (map snd (Query.pathedRefs cograph))
        (selectedAll, _) = Query.resolveSelectors cograph [] []
        (selectedSubtree, _) = Query.resolveSelectors cograph ["/root/a/**"] []
    assertEqual "no --select at all means every node is selected" allRefs selectedAll
    assertBool "a --select scopes down to a subtree" (mkRef "mid" ("a" :: Text) `Set.member` selectedSubtree)
    assertBool "a --select excludes what it doesn't match" (mkRef "mid" ("b" :: Text) `Set.notMember` selectedSubtree)

-- | Diamond shape reached two ways ('inject' + 'deps', like
-- 'Test.DownTreeSpec.diamondApexOnceLast'), so dedup-by-'Ref' at 'upTree'
-- time is exercised alongside the force-skip itself: 'apex' is excluded, and
-- must be 'Skip'ped both times it's walked (not just once, and not
-- 'Redundant' the second time as if it had genuinely run).
forceSkipSkipsOnlyExcluded :: IO ()
forceSkipSkipsOnlyExcluded = do
    ranRef <- newIORef []
    let rec name = modifyIORef' ranRef (name :)
        apex = op "apex" nodeps $ \x -> x{ref = mkRef "leaf" ("apex" :: Text), up = rec "apex"}
        left = op "left" (deps [apex]) $ \x -> x{ref = mkRef "mid" ("left" :: Text), up = rec "left"}
        right = (op "right" nodeps $ \x -> x{ref = mkRef "mid" ("right" :: Text), up = rec "right"}) `inject` apex
        root = op "root" (deps [left, right]) $ \x -> x{ref = mkRef "root" (), up = rec "root"}
        excluded = Set.singleton (mkRef "leaf" ("apex" :: Text))
        skipped = Query.forceSkip excluded root
    reports <- runUpCapturing skipped
    ran <- reverse <$> readIORef ranRef
    assertBool "apex's up never actually ran" ("apex" `notElem` ran)
    assertBool "everything else's up did run" (["left", "right", "root"] == ran || ["right", "left", "root"] == ran)
    let isApex act = ref (extension act) == mkRef "leaf" ("apex" :: Text)
    let skips = [() | Skip act <- reports, isApex act]
    let evals = [() | Eval act <- reports, isApex act]
    assertEqual "apex reported Skip exactly once (the other occurrence dedupes as Redundant)" 1 (length skips)
    assertEqual "apex never reported Eval" 0 (length evals)
