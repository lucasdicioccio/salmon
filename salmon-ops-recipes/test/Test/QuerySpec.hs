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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Query as Query
import Salmon.Actions.UpDown (Report (..))
import Salmon.Builtin.Extension (Extension (..), Op, deps, evalDeps, nodeps, op, ref, up)
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import Salmon.Op.Actions (extension)
import Salmon.Op.Eval (expand)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (Ref, mkRef)
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Reporter (silent)

import Test.Harness (runUpCapturing)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Query"
        [ testCase "* matches exactly one segment, ** matches any depth including zero" patternMatching
        , testCase "resolveSelectors: a shared predecessor is one Ref, matched at both its paths" sharedPredecessorRefs
        , testCase "resolveSelectors: empty --select means everything, minus --exclude" selectDefaultsToEverything
        , testCase "forceSkip makes upTree report Skip for the excluded node, Eval for the rest" forceSkipSkipsOnlyExcluded
        , testCase "pathedNodes carries each node's help text alongside its path/Ref" pathedNodesCarriesHelp
        , testCase "renderAnnotated tags same-path, distinct-Ref siblings with a stable shortRef so they aren't mistaken for duplicates" renderAnnotatedDisambiguatesSameTextSiblings
        , testCase "outline: selectors, first paths and shortest paths are what the full path listing gives" outlineAgreesWithTheListing
        , testCase "outline: a node declared again with one more dependency has that dependency" outlineMergesALaterOccurrence
        , testCase "outline: sixty diamonds resolve a selector and list paths without listing 2^60 of them" outlineOnADiamondChain
        , testCase "resolveRewrittenSelectors: a plain path selector behaves exactly as resolveSelectors" rewrittenPathSelectorUnchanged
        , testCase "resolveRewrittenSelectors: a #ref selector addresses a declared node directly" rewrittenRefSelectorAddressesDeclaredNode
        , testCase "resolveRewrittenSelectors: a #ref selector addressing a batch expands to its declared members" rewrittenRefSelectorExpandsABatch
        , testCase "resolveRewrittenSelectors: a path and a #ref selector combine" rewrittenPathAndRefSelectorsCombine
        , testCase "resolveRewrittenSelectors: an empty --select still means everything when --exclude is #ref-only" rewrittenEmptySelectStillMeansEverything
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
-- must be reported 'Skip'ped — once, because the collapse to a
-- 'Salmon.Op.Dag.Dag' makes it one node, and never 'Eval'ed.
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
    assertEqual "apex reported Skip exactly once, however many paths reach it" 1 (length skips)
    assertEqual "apex never reported Eval" 0 (length evals)

-- | 'query show --dedupe' collapses a shared node's repeated occurrences down
-- to its first-encountered path; 'query show --descriptions' needs each
-- node's help text alongside it, which is what 'pathedNodes' adds over
-- 'pathedRefs'.
pathedNodesCarriesHelp :: IO ()
pathedNodesCarriesHelp = do
    let shared = op "shared" nodeps $ \x -> x{ref = mkRef "leaf" ("shared" :: Text), help = "the shared leaf"}
        a = op "a" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("a" :: Text)}
        b = op "b" (deps [shared]) $ \x -> x{ref = mkRef "mid" ("b" :: Text)}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        entries = Query.pathedNodes cograph
        sharedEntries = [(path, h) | (path, r, h) <- entries, r == mkRef "leaf" ("shared" :: Text)]
    assertEqual "the shared Ref still occurs at both its paths" 2 (length sharedEntries)
    assertBool "each occurrence carries the node's help text" (all ((== "the shared leaf") . snd) sharedEntries)
    let dedupedRefs = go Set.empty [r | (_, r, _) <- entries]
        go _ [] = []
        go seen (r : rest)
            | r `Set.member` seen = go seen rest
            | otherwise = r : go (Set.insert r seen) rest
    assertEqual "dedupe-by-Ref keeps one occurrence per distinct node" 4 (length dedupedRefs)

-- | Two siblings built with the same 'ShortHand' (e.g. two migration files
-- both going through a "pg-script" builder) render identical path text but
-- carry distinct 'Ref's — 'query show's disambiguation tags every occurrence
-- of a colliding path with a stable, content-derived 'Query.shortRef' of its
-- own node (not an arbitrary, traversal-order-dependent counter), so the
-- lines don't look like an accidental exact duplicate and the tag doesn't
-- shift around if the graph is walked in a different order.
renderAnnotatedDisambiguatesSameTextSiblings :: IO ()
renderAnnotatedDisambiguatesSameTextSiblings = do
    let refA = mkRef "migration" ("a" :: Text)
        refB = mkRef "migration" ("b" :: Text)
        a = op "pg-script" nodeps $ \x -> x{ref = refA, help = "runs a"}
        b = op "pg-script" nodeps $ \x -> x{ref = refB, help = "runs b"}
        root = op "root" (deps [a, b]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        rendered = Query.renderAnnotated cograph Set.empty Set.empty True True
        lineA = "/root/pg-script #" <> Query.shortRef refA
        lineB = "/root/pg-script #" <> Query.shortRef refB
    assertBool "shortRef tags are distinct for distinct refs" (lineA /= lineB)
    assertBool "the first sibling's path is tagged with its own shortRef" (lineA `elem` rendered)
    assertBool "the second sibling's path is tagged with its own shortRef" (lineB `elem` rendered)
    assertBool "each sibling's own description follows its own tagged line" ("  # runs a" `elem` rendered && "  # runs b" `elem` rendered)
    assertEqual "no plain, untagged occurrence of the colliding path remains" 0 (length (Prelude.filter (== "/root/pg-script") rendered))
    assertBool "the non-colliding root path itself is left untagged" ("/root" `elem` rendered)

-------------------------------------------------------------------------------
-- the outline: one entry per Ref instead of one per path

{- | @n@ diamonds on top of one another: each @top@ stands on a @left@ and a
@right@ that both stand on the next @top@ down, and the last on @bottom@. The
number of paths to a node doubles with every diamond above it.
-}
diamondChain :: Int -> Op
diamondChain n = go n
  where
    go :: Int -> Op
    go 0 = op "bottom" nodeps $ \x -> x{ref = mkRef "diamond" ("bottom" :: Text)}
    go i =
        let below = go (i - 1)
            side name = op name (deps [below]) $ \x -> x{ref = mkRef "diamond" (name, i)}
         in op "top" (deps [side "left", side "right"]) $ \x -> x{ref = mkRef "diamond" ("top" :: Text, i)}

{- | On graphs small enough to list every path: a pattern selects exactly the
'Ref's found at a matching path, the first path per 'Ref' is the first in
the listing, and the paths kept are the head of the listing sorted.
-}
outlineAgreesWithTheListing :: IO ()
outlineAgreesWithTheListing =
    mapM_ agrees [("shared", sharedGraph), ("four diamonds", diamondChain 4)]
  where
    patterns =
        [ "/**"
        , "/root/a/**"
        , "/root/*/shared"
        , "/top"
        , "/top/**/bottom"
        , "/top/left/top/right/**"
        , "/**/right/*"
        , "/**/left/**/right/top/**"
        , "/*/*/*"
        , "/top/right/top/right/top/right/top/right/bottom"
        , "/nothing/**"
        , ""
        ]
    agrees :: (String, Op) -> IO ()
    agrees (name, graph) = do
        let cograph = runIdentity (expand graph)
            entries = Query.pathedRefs cograph
            o = Query.outline cograph
        assertEqual (name <> ": every node") (Set.fromList (map snd entries)) (Query.outlineRefs o)
        mapM_
            ( \pat ->
                assertEqual
                    (name <> ": " <> show pat)
                    (Set.fromList [r | (path, r) <- entries, Query.matchPattern (Query.parsePattern pat) path])
                    (Query.matchOutline (Query.parsePattern pat) o)
            )
            patterns
        assertEqual
            (name <> ": first paths")
            (Map.fromListWith (\_ old -> old) [(r, path) | (path, r) <- entries])
            (Map.fromList [(r, path) | (path, r, _) <- Query.outlineFirstPaths o])
        assertEqual (name <> ": one first path per node") (Set.size (Query.outlineRefs o)) (length (Query.outlineFirstPaths o))
        let width path = sum (map Text.length path) + length path
            listed = Map.map (take 3 . map snd . Set.toAscList) (Map.fromListWith Set.union [(r, Set.singleton (width path, path)) | (path, r) <- entries])
        assertEqual (name <> ": the three shortest paths per node") listed (Query.outlinePaths 3 o)

{- | The shape 'SreBox.PostgresPairPrereqs' relies on: a node named a second
time, same 'Ref', with one more dependency. The first occurrence is the one
walked; the second must still contribute what is new under it.
-}
outlineMergesALaterOccurrence :: IO ()
outlineMergesALaterOccurrence = do
    let extra = op "extra" nodeps $ \x -> x{ref = mkRef "leaf" ("extra" :: Text)}
        base = op "base" nodeps $ \x -> x{ref = mkRef "leaf" ("base" :: Text)}
        member ds = op "member" (deps ds) $ \x -> x{ref = mkRef "mid" ("member" :: Text)}
        root = op "root" (deps [member [base], member [base, extra]]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        (everything, _) = Query.resolveSelectors cograph [] []
        (selected, _) = Query.resolveSelectors cograph ["/root/member/extra"] []
    assertBool "the later occurrence's dependency is a node" (mkRef "leaf" ("extra" :: Text) `Set.member` everything)
    assertEqual "and is selected by its path" (Set.singleton (mkRef "leaf" ("extra" :: Text))) selected

{- | Sixty diamonds: 181 nodes, 2^60 paths to the bottom one. Listing them is
not an option, so everything below is only reachable if nothing lists them.
-}
outlineOnADiamondChain :: IO ()
outlineOnADiamondChain = do
    let cograph = runIdentity (expand (diamondChain 60))
        o = Query.outline cograph
        bottom = mkRef "diamond" ("bottom" :: Text)
        (everything, _) = Query.resolveSelectors cograph [] []
        (underTheFirstRight, _) = Query.resolveSelectors cograph ["/top/right/**"] []
        (byALongPath, _) = Query.resolveSelectors cograph [Text.concat (replicate 60 "/top/right") <> "/bottom"] []
        paths = Query.outlinePaths Query.pathLimit o
    assertEqual "every node, once" 181 (Set.size everything)
    assertEqual "everything but the first top and its left" 179 (Set.size underTheFirstRight)
    assertEqual "a path that is the longest of 2^60 still selects its node" (Set.singleton bottom) byALongPath
    assertEqual "a path per node at least" 181 (Map.size (Map.filter (not . null) paths))
    assertEqual "and no more than the limit" Query.pathLimit (maximum (map length (Map.elems paths)))
    assertEqual
        "the shortest path to the bottom goes left all the way"
        (Just (concat (replicate 60 ["top", "left"]) <> ["bottom"]))
        (fmap head (Map.lookup bottom paths))
    assertEqual "deduped rendering is one line per node" 181 (length (Query.renderAnnotated cograph Set.empty Set.empty True False))

-------------------------------------------------------------------------------
-- resolveRewrittenSelectors (R4)

pkg :: Text -> Op
pkg = Debian.deb . Debian.Package

pkgRef :: Text -> Ref
pkgRef name = mkRef "debian-deb" name

-- | The computed 'Rewritten' 'Salmon.Op.Rewrite.batchPackages' would produce
-- from this graph, everything desired, nothing ignored — the same 'Phase'
-- @run tree@\/@run dag@\/@query@ use for a whole-graph view.
computedFor :: [Rewrite.Rewrite Extension] -> Op -> Rewrite.Rewritten Extension
computedFor rewrites o =
    let dag = Dag.foldDag Dag.sameRepresentative (evalDeps o)
     in Rewrite.rewrite rewrites (Rewrite.wholeGraph dag) dag

onlyBatch :: Rewrite.Rewritten Extension -> IO Ref
onlyBatch c =
    -- the batch of packages, not the collected apt index node beside it
    case [r | (r, members) <- Map.toList (Rewrite.computedMembers c), pkgRef "curl" `Set.member` members] of
        [r] -> pure r
        rs -> assertFailure ("expected exactly one batch, got " <> show (length rs))

-- | Two independent packages, no rewrite registered: a plain-path selector
-- must resolve exactly as 'Query.resolveSelectors' already does, since
-- 'resolveRewrittenSelectors' must not change existing behaviour when no
-- '#'-pattern is involved.
rewrittenPathSelectorUnchanged :: IO ()
rewrittenPathSelectorUnchanged = do
    let root = op "root" (deps [pkg "curl", pkg "git"]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        computed = computedFor [] root
        (plainSel, plainExc) = Query.resolveSelectors cograph ["/root/deb"] []
        (rwSel, rwExc) = Query.resolveRewrittenSelectors cograph computed ["/root/deb"] []
    assertEqual "same selection with no '#' patterns involved" plainSel rwSel
    assertEqual "same exclusion with no '#' patterns involved" plainExc rwExc

-- | A '#' pattern matches a plain (un-batched) declared node by its own
-- 'Query.shortRef', the same text a tree\/dag render would show it as.
rewrittenRefSelectorAddressesDeclaredNode :: IO ()
rewrittenRefSelectorAddressesDeclaredNode = do
    let root = op "root" (deps [pkg "curl", pkg "git"]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        computed = computedFor [] root
        frag = Query.shortRef (pkgRef "curl")
        (selected, _) = Query.resolveRewrittenSelectors cograph computed ["#" <> frag] []
    assertEqual "exactly the matching declared node" (Set.singleton (pkgRef "curl")) selected

-- | A '#' pattern matching a rewrite-introduced (batch) node's own ref
-- expands, through 'Salmon.Op.Rewrite.membersOf', to every declared node the
-- batch stands in for — this is the fallback lookup a path glob cannot give,
-- since the batch was never declared and so has no path of its own.
rewrittenRefSelectorExpandsABatch :: IO ()
rewrittenRefSelectorExpandsABatch = do
    let root = op "root" (deps [pkg "curl", pkg "git"]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        computed = computedFor [Debian.batchPackages silent] root
    batchRef <- onlyBatch computed
    let frag = Query.shortRef batchRef
        (selected, _) = Query.resolveRewrittenSelectors cograph computed ["#" <> frag] []
    assertEqual
        "both declared packages the batch was built from, not the batch's own ref"
        (Set.fromList [pkgRef "curl", pkgRef "git"])
        selected

-- | The two kinds of selector union rather than override each other.
rewrittenPathAndRefSelectorsCombine :: IO ()
rewrittenPathAndRefSelectorsCombine = do
    let root = op "root" (deps [pkg "curl", pkg "git", pkg "vim"]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        computed = computedFor [] root
        frag = Query.shortRef (pkgRef "vim")
        (selected, _) = Query.resolveRewrittenSelectors cograph computed ["/root/**"] ["#" <> frag]
    assertBool "the root itself, matched by path" (mkRef "root" () `Set.member` selected)
    assertBool "curl and git, matched by path under root" (pkgRef "curl" `Set.member` selected && pkgRef "git" `Set.member` selected)
    assertBool "vim is excluded by its '#' pattern" (pkgRef "vim" `Set.notMember` selected)

-- | The bug this function's first draft had: 'resolveSelectors' treats an
-- empty select list as "everything", and that must still hold when the
-- overall select list is empty even though the exclude list is '#'-only —
-- checked against the *combined* pattern list, not just its path half.
rewrittenEmptySelectStillMeansEverything :: IO ()
rewrittenEmptySelectStillMeansEverything = do
    let root = op "root" (deps [pkg "curl", pkg "git"]) $ \x -> x{ref = mkRef "root" ()}
        cograph = runIdentity (expand root)
        computed = computedFor [] root
        frag = Query.shortRef (pkgRef "git")
        (selected, excluded) = Query.resolveRewrittenSelectors cograph computed [] ["#" <> frag]
        allRefs = Set.fromList (map snd (Query.pathedRefs cograph))
    assertEqual "everything but the excluded ref" (allRefs `Set.difference` Set.singleton (pkgRef "git")) selected
    assertEqual "exactly the excluded ref" (Set.singleton (pkgRef "git")) excluded
