{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Test.GraphFixture" itself: the generator is only
useful to the suites that will lean on it if its closed forms are right, so
they are checked here three ways at small sizes: against a plain walk of the
unfold, against the expansion of the generated 'Op', and against its fold.

Nothing here measures anything. The one large case is opt-in (see
'largeEnv'); the default run stops at a few tens of thousands of nodes.
-}
module Test.GraphFixtureSpec (tests) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM_, unless)
import Control.Monad.Identity (runIdentity)
import Data.Dynamic (toDyn)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Text.Read (readMaybe)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension (Extension, Op, check, dynamics, evalDeps, getDynamics, help, managed, notes, opAct, ref)
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Graph (Graph (..))
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref)

import Test.GraphFixture

tests :: TestTree
tests =
    testGroup
        "Test.GraphFixture"
        [ testCase "the closed forms match a walk of the unfold" closedFormsMatchUnfold
        , testCase "the generated Op expands and folds to the closed forms" opMatchesClosedForms
        , testCase "a size is never exceeded, and chain, fan and tree hit it exactly" sizesAreRespected
        , testCase "the same seed gives the same graph, and only the random shape reads it" deterministic
        , testCase "a random layer keeps its own position and draws distinct others" randomLayerDraws
        , testCase "check, managed and dynamics are what the options say" nodeOptions
        , testCase "the edge form changes how edges are written, not the graph" edgeForms
        , testCase "the collision fraction yields exactly that many conflicting refs" collisions
        , testCase "describing a million nodes costs nothing until expanded" lazyDescription
        , testCase "every shape is walkable per node at tens of thousands of nodes" perNodeWalk
        , testCase ("a per-node walk at the size in " <> largeEnv <> " (opt-in)") largeWalk
        ]

-------------------------------------------------------------------------------

shapes :: [Shape]
shapes =
    [ Chain
    , Fan
    , Tree 1
    , Tree 2
    , Tree 3
    , Diamonds 1
    , Diamonds 2
    , Diamonds 3
    , Layered 1 1
    , Layered 3 1
    , Layered 4 2
    , Layered 3 3
    , RandomLayered 4 2
    , RandomLayered 5 3
    , RandomLayered 3 3
    ]

-- | Small enough that the shapes that share stay cheap to expand.
sizes :: [Int]
sizes = [0, 1, 2, 3, 4, 5, 7, 12, 20, 25]

seeds :: [Seed]
seeds = [0, 1, 42]

label :: Shape -> Int -> Seed -> String
label shape size seed = show shape <> " at " <> show size <> ", seed " <> show seed

{- | The fixture's counts obtained the slow way, from 'successors' alone,
together with the number of paths reaching each node.
-}
walkUnfold :: (Int -> [Int]) -> (Counts, Map.Map Int Integer, Set.Set (Int, Int))
walkUnfold next = (Counts nodeCount edgeCount occurrences leafPaths, paths, edges)
  where
    reachable = go Set.empty [0]
    go seen [] = seen
    go seen (i : rest)
        | Set.member i seen = go seen rest
        | otherwise = go (Set.insert i seen) (next i <> rest)

    edges = Set.fromList [(i, j) | i <- Set.toList reachable, j <- next i]

    -- a node's dependencies have larger numbers than it does, so ascending
    -- order has every path into a node counted before it is passed on.
    paths = foldl pass (Map.singleton 0 1) (Set.toAscList reachable)
    pass acc i =
        let here = Map.findWithDefault 0 i acc
         in foldl (\m j -> Map.insertWith (+) j here m) acc (next i)

    nodeCount = fromIntegral (Set.size reachable)
    edgeCount = fromIntegral (Set.size edges)
    occurrences = sum (Map.elems paths)
    leafPaths = sum [p | (i, p) <- Map.toList paths, null (next i)]

foldOf :: Op -> Dag.Dag Extension
foldOf = Dag.foldDag Dag.sameRepresentative . evalDeps

expansionLeaves :: Cofree Graph a -> Integer
expansionLeaves (_ :< g) =
    case toList g of
        [] -> 1
        below -> sum (fmap expansionLeaves below)

{- | The distinct 'Ref's of a graph, visiting each node once: what a consumer
that does not walk paths pays. An explicit stack, so depth costs heap rather
than stack.
-}
distinctRefs :: Op -> Int
distinctRefs root = go Set.empty [root]
  where
    go :: Set.Set Ref -> [Op] -> Int
    go seen [] = Set.size seen
    go seen (o : rest) =
        case opAct o of
            Just act
                | Set.member act.extension.ref seen -> go seen rest
                | otherwise -> go (Set.insert act.extension.ref seen) (below o <> rest)
            Nothing -> go seen (below o <> rest)

-- | What a node declares it depends on, one step down.
below :: Op -> [Op]
below o = toList (runIdentity o.predecessors)

-------------------------------------------------------------------------------

closedFormsMatchUnfold :: IO ()
closedFormsMatchUnfold =
    forM_ [(shape, size, seed) | shape <- shapes, size <- sizes, seed <- seeds] $ \(shape, size, seed) -> do
        let (walked, _, edges) = walkUnfold (successors shape size seed)
        assertEqual (label shape size seed) (counts shape size) walked
        assertBool
            (label shape size seed <> ": a dependency has a larger number than its dependant")
            (all (\(i, j) -> j > i) (Set.toList edges))

opMatchesClosedForms :: IO ()
opMatchesClosedForms =
    forM_ [(shape, size, seed) | shape <- shapes, size <- sizes, seed <- seeds] $ \(shape, size, seed) -> do
        let expected = counts shape size
            (_, _, edges) = walkUnfold (successors shape size seed)
            root = generate shape size seed
            expansion = evalDeps root
            dag = foldOf root
            at what = label shape size seed <> ": " <> what
        assertEqual (at "occurrences") expected.countOccurrences (fromIntegral (length expansion))
        assertEqual (at "leaf paths") expected.countLeafPaths (expansionLeaves expansion)
        assertEqual (at "nodes") expected.countNodes (fromIntegral (Map.size (Dag.dagNodes dag)))
        assertEqual (at "per-node walk") expected.countNodes (fromIntegral (distinctRefs root))
        -- the fold's edges are the unfold's, node for node: this is also
        -- what says the refs are distinct.
        assertEqual
            (at "edges")
            (Set.map (\(i, j) -> (nodeRef j, nodeRef i)) edges)
            (Dag.dagEdges dag)
        assertEqual (at "edge count") expected.countEdges (fromIntegral (Set.size (Dag.dagEdges dag)))
        assertEqual (at "the root is the only root") [nodeRef 0] (Dag.roots dag)
        assertEqual (at "no conflicts") 0 (length (Dag.dagConflicts dag))

sizesAreRespected :: IO ()
sizesAreRespected =
    forM_ [(shape, size) | shape <- shapes, size <- [1 .. 60]] $ \(shape, size) -> do
        let built = (counts shape size).countNodes
        assertBool (show shape <> " at " <> show size <> " built " <> show built) (built >= 1 && built <= fromIntegral size)
        case shape of
            Chain -> assertEqual "chain" (fromIntegral size) built
            Fan -> assertEqual "fan" (fromIntegral size) built
            Tree _ -> assertEqual "tree" (fromIntegral size) built
            _ -> pure ()

deterministic :: IO ()
deterministic = do
    let edgesOf shape size seed = Dag.dagEdges (foldOf (generate shape size seed))
    forM_ shapes $ \shape ->
        assertEqual (show shape <> " twice") (edgesOf shape 20 7) (edgesOf shape 20 7)
    forM_ [Chain, Fan, Tree 2, Diamonds 2, Layered 4 2] $ \shape ->
        assertEqual (show shape <> " ignores the seed") (edgesOf shape 20 1) (edgesOf shape 20 2)
    assertBool
        "the random shape differs between seeds"
        (edgesOf (RandomLayered 8 3) 41 1 /= edgesOf (RandomLayered 8 3) 41 2)
    assertBool
        "and differs from the regular one"
        (edgesOf (RandomLayered 8 3) 41 1 /= edgesOf (Layered 8 3) 41 1)

randomLayerDraws :: IO ()
randomLayerDraws = do
    let width = 8
        sharing = 3
        size = 1 + 6 * width
        next = successors (RandomLayered width sharing) size 99
    forM_ [1 .. size - 1 - width] $ \i -> do
        let picked = next i
        assertEqual ("node " <> show i <> " has its dependencies") sharing (length picked)
        assertEqual ("node " <> show i <> " draws distinct ones") sharing (Set.size (Set.fromList picked))
        assertEqual ("node " <> show i <> " keeps its own position") [i + width] (take 1 picked)
    forM_ [size - width .. size - 1] $ \i ->
        assertEqual ("node " <> show i <> " is in the last layer") [] (next i)

nodeOptions :: IO ()
nodeOptions = do
    let exts root = [act.extension | o <- toList (evalDeps root), Just act <- [opAct o]]
        plain = exts (generate (Tree 2) 7 0)
    assertEqual "seven nodes" 7 (length plain)
    verdicts <- mapM check plain
    assertEqual "no check by default" (replicate 7 Immaterial) verdicts
    assertBool "nothing managed by default" (all (isNothing . managed) plain)
    assertBool "no dynamics by default" (all (null . dynamics) plain)
    assertBool "no notes" (all (null . notes) plain)

    forM_ [Success, Completed, Failure "absent", Unknown] $ \verdict -> do
        answered <- mapM check (exts (generateWith defaultOptions{optCheck = verdict} (Tree 2) 7 0))
        assertEqual ("check answers " <> show verdict) (replicate 7 verdict) answered

    assertBool
        "every node managed"
        (all (isJust . managed) (exts (generateWith defaultOptions{optManaged = True} (Tree 2) 7 0)))

    let tagged = generateWith defaultOptions{optDynamics = \i -> [toDyn i]} (Tree 2) 7 0
    assertEqual
        "each node carries its own number"
        [0 .. 6 :: Int]
        (Set.toAscList (Set.fromList (concatMap getDynamics (toList (evalDeps tagged)))))

edgeForms :: IO ()
edgeForms = do
    let build form = generateWith defaultOptions{optEdges = form} (Layered 3 2) 10 0
        written form = runIdentity (build form).predecessors
    case written AsVertices of
        Vertices [_, _, _] -> pure ()
        other -> assertFailure ("AsVertices wrote " <> show (fmap (const ()) other))
    case written AsConnect of
        Connect (Vertices [_]) (Connect (Vertices [_]) (Connect (Vertices [_]) (Vertices []))) -> pure ()
        other -> assertFailure ("AsConnect wrote " <> show (fmap (const ()) other))
    case written AsOverlay of
        Overlay (Vertices [_]) (Overlay (Vertices [_]) (Overlay (Vertices [_]) (Vertices []))) -> pure ()
        other -> assertFailure ("AsOverlay wrote " <> show (fmap (const ()) other))
    forM_ [AsConnect, AsOverlay] $ \form -> do
        assertEqual (show form <> ": same edges") (Dag.dagEdges (foldOf (build AsVertices))) (Dag.dagEdges (foldOf (build form)))
        assertEqual (show form <> ": same order") (Dag.dagOrder (foldOf (build AsVertices))) (Dag.dagOrder (foldOf (build form)))

collisions :: IO ()
collisions =
    forM_ [(shape, fraction, seed) | shape <- shapes, fraction <- [0, 1 / 10, 1 / 3, 1], seed <- seeds] $ \(shape, fraction, seed) -> do
        let size = 20
            opts = defaultOptions{optCollisions = fraction}
            at what = label shape size seed <> ", colliding " <> show fraction <> ": " <> what
            expected = counts shape size
            wanted = colliderCount opts shape size
            (_, paths, _) = walkUnfold (successors shape size seed)
            chosen = filter (isCollider opts shape size seed) [-1 .. size + 1]
            root = generateWith opts shape size seed
            dag = foldOf root
            conflicting = Set.fromList (fmap (\c -> c.conflictRef) (Dag.dagConflicts dag))
        assertEqual (at "collider count") (floor (fraction * fromIntegral (expected.countNodes - 1))) wanted
        assertEqual (at "chosen nodes") wanted (fromIntegral (length chosen))
        assertEqual (at "conflicting refs") (Set.fromList (fmap nodeRef chosen)) conflicting
        assertEqual (at "nodes are unchanged") expected.countNodes (fromIntegral (Map.size (Dag.dagNodes dag)))
        assertEqual (at "edges are unchanged") expected.countEdges (fromIntegral (Set.size (Dag.dagEdges dag)))
        assertEqual
            (at "one more leaf per occurrence of a collider")
            (expected.countOccurrences + sum [paths Map.! c | c <- chosen])
            (fromIntegral (length (evalDeps root)))
        forM_ chosen $ \c ->
            assertEqual
                (at "the later declaration survives")
                (Just "graph fixture node (colliding declaration)")
                (fmap (\act -> act.extension.help) (Dag.representativeOf dag (nodeRef c)))

{- | The description is one closure: taking the root of a million-node graph,
and a few of its dependencies, must not build the rest. This is not a timing
assertion; a strict generator would show here as the suite stalling.
-}
lazyDescription :: IO ()
lazyDescription = do
    let million = 1000000
        refOf o = fmap (\act -> act.extension.ref) (opAct o)
    forM_ shapes $ \shape -> do
        let root = generate shape million 0
        assertEqual (show shape <> ": root") (Just (nodeRef 0)) (refOf root)
        case below root of
            first : _ -> assertEqual (show shape <> ": first dependency") (Just (nodeRef 1)) (refOf first)
            [] -> assertFailure (show shape <> ": a million-node graph whose root depends on nothing")
    assertEqual
        "a fan's root is over everything else"
        (fmap (Just . nodeRef) [1, 2, 3])
        (fmap refOf (take 3 (below (generate Fan million 0))))
    -- the closed forms are arithmetic, however large the expansion is.
    assertEqual "nodes of a million-node chain of diamonds" 1000000 (counts (Diamonds 2) million).countNodes
    assertBool
        "whose expansion could never be walked"
        ((counts (Diamonds 2) million).countOccurrences > 10 ^ (1000 :: Int))

perNodeWalk :: IO ()
perNodeWalk =
    forM_ shapes $ \shape -> do
        let size = 20000
        assertEqual (show shape) (counts shape size).countNodes (fromIntegral (distinctRefs (generate shape size 3)))

-- | Set to a node count (e.g. @1000000@) to run 'largeWalk'.
largeEnv :: String
largeEnv = "SALMON_TEST_GRAPH_FIXTURE_SIZE"

{- | Every shape at an operator-chosen size, visited once per node, and the
shapes without sharing expanded and folded as well. Opt-in: it is the one
case here whose cost is not negligible, and the host decides how much memory
it may take.
-}
largeWalk :: IO ()
largeWalk = do
    requested <- lookupEnv largeEnv
    case requested >>= readMaybe of
        Nothing ->
            hPutStrLn stderr ("SKIPPED: set " <> largeEnv <> " to a node count to walk every fixture shape at that size")
        Just size -> do
            forM_ shapes $ \shape ->
                assertEqual
                    (show shape <> ": per-node walk")
                    (counts shape size).countNodes
                    (fromIntegral (distinctRefs (generate shape size 3)))
            -- not the fan: the fold keeps each node's edges as a list it
            -- searches on every insert, so one node over everything else is
            -- quadratic there. That is a finding for whoever profiles, not
            -- something to wait on here.
            forM_ [Chain, Tree 2] $ \shape -> do
                let dag = foldOf (generate shape size 3)
                    expected = counts shape size
                unless (expected.countOccurrences == expected.countNodes) $
                    assertFailure (show shape <> " shares nodes")
                assertEqual (show shape <> ": folded nodes") expected.countNodes (fromIntegral (Map.size (Dag.dagNodes dag)))
                assertEqual (show shape <> ": folded edges") expected.countEdges (fromIntegral (Set.size (Dag.dagEdges dag)))
