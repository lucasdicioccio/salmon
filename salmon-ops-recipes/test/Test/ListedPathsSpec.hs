{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | The listed paths with a length cap ('Query.outlinePaths', 'Query.pathCap'),
against the uncapped listing they replaced ("Test.PathListReference") and
against every root-to-node path of the graph ('Query.pathedRefs'), on random
small graphs: the shapes of "Test.GraphFixture" with a random size and
seed, and random small graphs whose shorthands differ in length and repeat.

Every property lists every path ('pathedRefs') to have something to compare
with, so the graphs stay small (at most 'maxNodes' nodes); the caps drawn
are small too, so that the cap is crossed on most of them.
-}
module Test.ListedPathsSpec (tests) where

import Control.Monad.Identity (runIdentity)
import Data.List (isSuffixOf, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.QuickCheck (Gen, Property, choose, classify, conjoin, counterexample, elements, forAll, frequency, sublistOf, vectorOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.Help (pathText)
import qualified Salmon.Actions.Query as Query
import Salmon.Actions.Query (ListedPath (..))
import Salmon.Builtin.Extension (Op, deps, evalDeps, op, ref)
import Salmon.Op.Eval (expand)
import Salmon.Op.Ref (Ref, mkRef)

import Test.GraphFixture (Shape (..), generate, nodeRef)
import Test.PathListReference (outlinePathsUncapped, renderedWidth)

tests :: TestTree
tests =
    testGroup
        "listed paths with a length cap"
        [ testProperty "at or under the cap, the listing is the uncapped one, in the same order and positions" prop_underTheCapUnchanged
        , testProperty "with no cap reached, the listing is the uncapped one" prop_noCapReachedIsIdentical
        , testProperty "every listed path is a real path, or the real tail of one, and selects its node" prop_listedPathsAreReal
        , testProperty "the shortest path's length is always the first listed, and the rest are in length order" prop_shortestFirst
        , testProperty "at most the limit, no text twice, a path for every node the uncapped listing had" prop_bounded
        , testCase "a chain at the cap: spelled out up to it, a tail after it" chainAtTheCap
        , testCase "the rendering of a capped path is a pattern that selects the node" cappedRenderingSelects
        , testCase "query show is not capped: chain, diamond and tree print every path in full" queryShowIsNotCapped
        ]

-------------------------------------------------------------------------------
-- Graphs

maxNodes :: Int
maxNodes = 30

-- | A graph to test on, with what to call it in a counterexample.
data Case = Case
    { caseName :: String
    , caseGraph :: Op
    }

instance Show Case where
    show = caseName

genCase :: Gen Case
genCase = frequency [(3, genFixture), (2, genRandomNames)]

genFixture :: Gen Case
genFixture = do
    shape <- elements [Chain, Fan, Tree 2, Tree 3, Diamonds 2, Diamonds 3, Layered 3 2, Layered 2 2, RandomLayered 4 2]
    n <- choose (1, maxNodes)
    seed <- choose (0, 1000 :: Int)
    pure (Case (show shape <> " " <> show n <> " seed " <> show seed) (generate shape n (fromIntegral seed)))

{- | Nodes @0..n-1@, each depending on up to three nodes numbered above it,
with shorthands of differing lengths that repeat (distinct 'Ref's under
one name, as recipes reusing a builder have).
-}
genRandomNames :: Gen Case
genRandomNames = do
    n <- choose (1, 14)
    names <- vectorOf n (elements ["a", "bb", "ccc", "dddddd", "x", "yy"])
    edges <- traverse (\i -> take 3 <$> sublistOf [i + 1 .. n - 1]) [0 .. n - 1]
    let node :: Int -> Op
        node i = op (names !! i) (deps (map node (edges !! i))) (\x -> x{ref = mkRef "listed-paths" i})
        description = "random names " <> show (zip3 [0 :: Int ..] names edges)
    pure (Case description (node 0))

genCap :: Gen Int
genCap = frequency [(6, choose (1, 40)), (1, choose (40, 200)), (1, pure maxBound)]

genLimit :: Gen Int
genLimit = choose (1, 8)

-- | Everything a property needs about one graph.
data Listed = Listed
    { lOutline :: Query.Outline
    , lReal :: Map Ref (Set [Text])
    , lOld :: Map Ref [[Text]]
    , lNew :: Map Ref [ListedPath]
    }

listed :: Int -> Int -> Op -> Listed
listed limit cap graph =
    Listed
        { lOutline = o
        , lReal = Map.fromListWith Set.union [(r, Set.singleton p) | (p, r) <- Query.pathedRefs cograph]
        , lOld = outlinePathsUncapped limit o
        , lNew = Query.outlinePaths limit cap o
        }
  where
    cograph = runIdentity (expand graph)
    o = Query.outline cograph

withListed :: (Int -> Int -> Listed -> Property) -> Property
withListed k =
    forAll genCase $ \c ->
        forAll genLimit $ \limit ->
            forAll genCap $ \cap ->
                counterexample ("limit " <> show limit <> ", cap " <> show cap) (k limit cap (listed limit cap c.caseGraph))

perNode :: Listed -> (Ref -> [[Text]] -> [ListedPath] -> Property) -> Property
perNode l k =
    conjoin
        ( counterexample "the same nodes" (Map.keysSet l.lOld === Map.keysSet l.lNew)
            : [ counterexample (show r) (k r old (Map.findWithDefault [] r l.lNew))
              | (r, old) <- Map.toList l.lOld
              ]
        )

-------------------------------------------------------------------------------
-- Properties

prop_underTheCapUnchanged :: Property
prop_underTheCapUnchanged = withListed $ \_ cap l ->
    classify (any (any (.listedCapped)) (Map.elems l.lNew)) "some path capped" $
    conjoin
        [ counterexample "the same nodes" (Map.keysSet l.lOld === Map.keysSet l.lNew)
        , conjoin
            [ counterexample (show r) $
                let new = Map.findWithDefault [] r l.lNew
                    spelled = takeWhile (not . (.listedCapped)) new
                 in conjoin
                        [ map (.listedSegments) spelled === takeWhile ((<= cap) . renderedWidth) old
                        , counterexample "nothing spelled out after a capped path" (all (.listedCapped) (drop (length spelled) new))
                        , counterexample "spelled out iff at or under the cap" (all (\p -> p.listedCapped == (p.listedWidth > cap)) new)
                        ]
            | (r, old) <- Map.toList l.lOld
            ]
        ]

prop_noCapReachedIsIdentical :: Property
prop_noCapReachedIsIdentical =
    forAll genCase $ \c ->
        forAll genLimit $ \limit ->
            let o = Query.outline (runIdentity (expand c.caseGraph))
                old = outlinePathsUncapped limit o
                widest = maximum (0 : [renderedWidth p | ps <- Map.elems old, p <- ps])
                asOld = Map.map (map (\p -> (p.listedCapped, p.listedSegments)))
             in conjoin
                    [ counterexample "no cap at all" (asOld (Query.outlinePaths limit maxBound o) === Map.map (map (False,)) old)
                    , counterexample "the cap at the widest listed path" (asOld (Query.outlinePaths limit widest o) === Map.map (map (False,)) old)
                    , counterexample "the default cap on these small graphs" (asOld (Query.outlinePaths limit Query.pathCap o) === Map.map (map (False,)) old)
                    ]

prop_listedPathsAreReal :: Property
prop_listedPathsAreReal = withListed $ \_ cap l ->
    conjoin
        [ counterexample (show r <> " " <> show p) (realPath cap l r p)
        | (r, ps) <- Map.toList l.lNew
        , p <- ps
        ]

realPath :: Int -> Listed -> Ref -> ListedPath -> Property
realPath cap l r p
    | p.listedCapped =
        conjoin
            [ counterexample "over the cap" (p.listedWidth > cap)
            , counterexample "the longest tail within the cap of a real path of that length" (any (\q -> renderedWidth q == p.listedWidth && p.listedSegments `isSuffixOf` q && longest q) paths)
            , counterexample "a tail within the cap, or the node's own segment alone" (renderedWidth p.listedSegments <= cap || length p.listedSegments == 1)
            , counterexample "its rendering selects the node" selects
            ]
    | otherwise =
        conjoin
            [ counterexample "a real root-to-node path" (p.listedSegments `Set.member` reals)
            , counterexample "its length" (p.listedWidth === renderedWidth p.listedSegments)
            , counterexample "its rendering selects the node" selects
            ]
  where
    reals = Map.findWithDefault Set.empty r l.lReal
    paths = Set.toList reals
    selects = r `Set.member` Query.matchOutline (Query.parsePattern (Query.renderListedPath p)) l.lOutline
    -- one segment more of @q@ would not fit (or there is none).
    longest q =
        let k = length p.listedSegments
            more = drop (length q - k - 1) q
         in length q == k || renderedWidth more > cap

prop_shortestFirst :: Property
prop_shortestFirst = withListed $ \_ _ l ->
    perNode l $ \_ old new ->
        conjoin
            [ map (.listedWidth) (take 1 new) === map renderedWidth (take 1 old)
            , counterexample "in length order" (let ws = map (.listedWidth) new in and (zipWith (<=) ws (drop 1 ws)))
            ]

prop_bounded :: Property
prop_bounded = withListed $ \limit _ l ->
    perNode l $ \_ old new ->
        let texts = map Query.renderListedPath new
         in conjoin
                [ counterexample "at most the limit" (length new <= limit)
                , counterexample "no text twice" (texts === nub texts)
                , counterexample "a path where the uncapped listing had one" (null old === null new)
                ]

-------------------------------------------------------------------------------
-- Unit tests

-- | @n0@ to @n9@: the path to @nk@ renders in @3 (k + 1)@ characters.
chainAtTheCap :: IO ()
chainAtTheCap = do
    let o = Query.outline (evalDeps (generate Chain 10 0))
        paths = Query.outlinePaths Query.pathLimit 15 o
        at k = Map.findWithDefault [] (nodeRef k) paths
        names k = [Text.pack ('n' : show i) | i <- [0 .. k]]
    assertEqual "the root" [ListedPath 3 False ["n0"]] (at 0)
    assertEqual "n4, exactly at the cap: spelled out" [ListedPath 15 False (names 4)] (at 4)
    assertEqual "n5, one segment over: its tail within the cap" [ListedPath 18 True (drop 1 (names 5))] (at 5)
    assertEqual "rendered as a pattern" ["**/n1/n2/n3/n4/n5"] (map Query.renderListedPath (at 5))
    assertEqual "n9: its last five" [ListedPath 30 True (drop 5 (names 9))] (at 9)
    assertEqual "spelled out the way worldPaths always rendered" ["n0/n1/n2/n3/n4"] (map Query.renderListedPath (at 4))

-- | A capped path's rendering is a pattern, and it selects the node it was listed for.
cappedRenderingSelects :: IO ()
cappedRenderingSelects = do
    let cograph = evalDeps (generate (Diamonds 2) 31 0)
        o = Query.outline cograph
        paths = Query.outlinePaths Query.pathLimit 20 o
        capped = [(r, p) | (r, ps) <- Map.toList paths, p <- ps, p.listedCapped]
    assertBool "some paths are capped" (not (null capped))
    mapM_
        ( \(r, p) ->
            assertBool
                (show p <> " selects " <> show r)
                (r `Set.member` Query.matchOutline (Query.parsePattern (Query.renderListedPath p)) o)
        )
        capped

{- | @query show@ lists full paths by definition, so it is left uncapped:
on a chain whose deepest path is far over 'Query.pathCap', with and without
dedupe, and on a diamond chain and a tree (whose paths are short: the
boundary is not reachable on them at a size @--no-dedupe@ can list), every
line is a full path.
-}
queryShowIsNotCapped :: IO ()
queryShowIsNotCapped = do
    let chain = evalDeps (generate Chain 100 0)
        deepest = pathText [Text.pack ('n' : show i) | i <- [0 .. 99 :: Int]]
    assertBool "the chain's deepest path is over the cap" (Text.length deepest > Query.pathCap)
    mapM_
        ( \dedupe -> do
            let ls = Query.renderAnnotated chain Set.empty Set.empty dedupe False
            assertEqual ("one line per node, dedupe " <> show dedupe) 100 (length ls)
            assertEqual ("the last line is the deepest path in full, dedupe " <> show dedupe) (Just deepest) (lastMay ls)
            assertBool "nothing shown as a tail" (not (any ("**" `Text.isInfixOf`) ls))
        )
        [True, False]
    mapM_
        ( \(name, graph) -> do
            let cograph = evalDeps graph
                every = [pathText p | (p, _) <- Query.pathedRefs cograph]
                firsts = [pathText p | (p, _, _) <- Query.outlineFirstPaths (Query.outline cograph)]
            assertEqual (name <> ": --no-dedupe prints every path in full") every (Query.renderAnnotated cograph Set.empty Set.empty False False)
            assertEqual (name <> ": dedupe prints each node's first path in full") firsts (Query.renderAnnotated cograph Set.empty Set.empty True False)
        )
        [("diamonds", generate (Diamonds 2) 31 0), ("tree", generate (Tree 2) 63 0)]
  where
    lastMay [] = Nothing
    lastMay xs = Just (last xs)
