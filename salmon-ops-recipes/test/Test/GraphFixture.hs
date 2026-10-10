{-# LANGUAGE OverloadedStrings #-}

{- | Synthetic 'Op' graphs of a chosen shape and size, for testing scale on
purpose instead of discovering it in production.

A fixture is a __recursive unfold__: 'successors' maps a node number to the
numbers of the nodes it depends on, and 'generateWith' turns that into an 'Op'
whose @predecessors@ are built on demand. Nothing is memoised and nothing is
retained, so describing a million-node graph costs one closure; the cost is
paid by whoever expands it, and only for the part they expand.

Every node is an in-process no-op (no 'IO' beyond a @pure@), carries its own
'Ref' ('nodeRef'), and is numbered so that a node's dependencies always have
larger numbers than it does: node 0 is the root, and the numbering is a
topological order.

= Per node versus per path

The shapes are chosen to separate the two costs a consumer can have:

* 'Chain', 'Fan' and 'Tree' have no sharing: the expanded tree
  ('Salmon.Builtin.Extension.evalDeps') has exactly one occurrence per node.
* 'Diamonds', 'Layered' and 'RandomLayered' share nodes, so the expanded tree
  is exponentially larger than the graph. 'countOccurrences' says by how
  much. Anything that walks the expansion ('Salmon.Op.Dag.foldDag' included)
  pays per occurrence, so pick the size with 'counts' in hand: a million-node
  chain of diamonds has more occurrences than can ever be visited.

= Sizes

The size is a __requested node count__. A shape realises the largest graph it
can that does not exceed it (and never fewer than the root alone), and
'counts' states what was actually built. 'Chain', 'Fan' and 'Tree' hit every
size exactly.

Layer 0 only: nothing here measures anything.
-}
module Test.GraphFixture (
    -- * Shapes
    Shape (..),
    Seed,

    -- * Generating
    generate,
    generateWith,
    Options (..),
    EdgeForm (..),
    defaultOptions,
    holdUntilCancelled,

    -- * What was generated, in closed form
    Counts (..),
    counts,
    colliderCount,

    -- * The unfold itself
    successors,
    isCollider,
    nodeRef,
    nodeName,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Identity (Identity (..))
import Data.Bits (shiftR, xor)
import Data.Dynamic (Dynamic)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.Exit (ExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension (Extension (..), Op, Output, nodeps, op)
import Salmon.Op.Graph (Graph (..))
import Salmon.Op.Ref (Ref, mkRef)

-------------------------------------------------------------------------------

{- | A family of graphs. The fields are what stays fixed as the size grows;
out-of-range values are clamped (a width or branching factor below 1 is 1, a
sharing factor is kept within @1..width@).
-}
data Shape
    = -- | Depth: each node depends on the next one.
      Chain
    | -- | Width: one root over @size - 1@ leaves.
      Fan
    | -- | A complete tree with the given branching factor: no sharing.
      Tree Int
    | -- | A chain of diamonds of the given width: a join node over @width@
      -- middle nodes that all depend on the next join. The smallest shape
      -- whose paths double (for width 2) at every step.
      Diamonds Int
    | -- | @Layered width sharing@: a root over a first layer of @width@
      -- nodes, each layer depending on the next. The node at position @p@
      -- depends on positions @p .. p + sharing - 1@ (wrapping) of the layer
      -- below, so @sharing = 1@ is @width@ independent chains and
      -- @sharing = width@ is a complete bipartite step.
      Layered Int Int
    | -- | As 'Layered', but which @sharing@ nodes of the layer below a node
      -- depends on is drawn from the seed. Position @p@ always depends on
      -- position @p@ below it (so every node stays reachable) and on
      -- @sharing - 1@ other distinct positions. Every node still has exactly
      -- @sharing@ dependencies, which is why the totals in 'counts' are the
      -- same as 'Layered' and do not depend on the seed; how the paths are
      -- spread over the nodes of a layer does.
      RandomLayered Int Int
    deriving (Show, Eq)

{- | Everything random about a fixture is a pure function of this: the same
shape, size, options and seed give the same graph. Only 'RandomLayered' and
the choice of colliding nodes ('optCollisions') read it.
-}
type Seed = Word64

-- | A shape with its size resolved to the parameters that were realised.
data Layout
    = -- | node count
      LChain !Int
    | -- | node count
      LFan !Int
    | -- | branching, node count
      LTree !Int !Int
    | -- | diamonds, width
      LDiamonds !Int !Int
    | -- | drawn from the seed?, layers, width, sharing
      LLayered !Bool !Int !Int !Int

layout :: Shape -> Int -> Layout
layout shape size =
    case shape of
        Chain -> LChain n
        Fan -> LFan n
        Tree b -> LTree (max 1 b) n
        Diamonds w ->
            let w' = max 1 w
             in LDiamonds ((n - 1) `div` (w' + 1)) w'
        Layered w k -> layered False w k
        RandomLayered w k -> layered True w k
  where
    n = max 1 size
    layered random w k =
        let w' = max 1 w
         in LLayered random ((n - 1) `div` w') w' (max 1 (min w' k))

-------------------------------------------------------------------------------

{- | What a fixture contains, as formulas rather than by walking it.

'Integer' because the last two outgrow a machine word quickly: that is the
point of the shapes that share.
-}
data Counts = Counts
    { countNodes :: !Integer
    -- ^ Distinct nodes, i.e. distinct 'Ref's: the size of the folded 'Salmon.Op.Dag.Dag'.
    , countEdges :: !Integer
    -- ^ Distinct dependency edges: the size of 'Salmon.Op.Dag.dagEdges'.
    , countOccurrences :: !Integer
    -- ^ Paths from the root to any node, the root's own empty path
    -- included: the number of nodes of the expanded tree
    -- ('Salmon.Builtin.Extension.evalDeps'). Equal to 'countNodes' exactly
    -- when nothing is shared.
    , countLeafPaths :: !Integer
    -- ^ Paths from the root to a node with no dependencies: the leaves of
    -- the expanded tree.
    }
    deriving (Show, Eq)

{- | The counts of @'generate' shape size seed@, for any seed.

With 'optCollisions' above zero the graph keeps these nodes and edges, and
each colliding node adds one leaf to the expansion per occurrence of it; see
'optCollisions'.
-}
counts :: Shape -> Int -> Counts
counts shape size =
    case layout shape size of
        LChain n -> Counts (int n) (int n - 1) (int n) 1
        LFan n -> Counts (int n) (int n - 1) (int n) (max 1 (int n - 1))
        LTree b n ->
            -- node i has children exactly when b * i + 1 < n
            let internal = (int n - 1 + int b - 1) `div` int b
             in Counts (int n) (int n - 1) (int n) (int n - internal)
        LDiamonds k w ->
            Counts
                { countNodes = 1 + int k * (int w + 1)
                , countEdges = 2 * int k * int w
                , -- join i is reached by w^i paths, and so is each of the w
                  -- middle nodes that depend on it
                  countOccurrences = 2 * geometric (int w) (int k + 1) - 1
                , countLeafPaths = int w ^ k
                }
        LLayered _ 0 _ _ -> Counts 1 0 1 1
        LLayered _ l w k ->
            Counts
                { countNodes = 1 + int l * int w
                , countEdges = int w + (int l - 1) * int w * int k
                , -- every node has k dependencies, so the paths reaching a
                  -- layer are k times those reaching the one above
                  countOccurrences = 1 + int w * geometric (int k) (int l)
                , countLeafPaths = int w * int k ^ (l - 1)
                }
  where
    int :: Int -> Integer
    int = fromIntegral

-- | @geometric r n = 1 + r + ... + r^(n-1)@.
geometric :: Integer -> Integer -> Integer
geometric r n
    | r == 1 = n
    | otherwise = (r ^ n - 1) `div` (r - 1)

-------------------------------------------------------------------------------

{- | The unfold: the nodes a node depends on, in declaration order. Total: a
number outside the graph depends on nothing.

Partially apply it to the shape, size and seed and keep the result: the
layout is resolved once.
-}
successors :: Shape -> Int -> Seed -> Int -> [Int]
successors shape size seed =
    case layout shape size of
        LChain n -> \i -> [i + 1 | i >= 0, i + 1 < n]
        LFan n -> \i -> if i == 0 then [1 .. n - 1] else []
        LTree b n -> \i -> if i < 0 then [] else takeWhile (< n) [b * i + 1 .. b * i + b]
        LDiamonds k w -> \i ->
            let (q, r) = i `divMod` (w + 1)
             in if i < 0 || q > k || (q == k && r /= 0)
                    then []
                    else
                        if r == 0
                            then (if q < k then [i + 1 .. i + w] else [])
                            else [(q + 1) * (w + 1)]
        LLayered random l w k -> \i ->
            if i == 0
                then (if l > 0 then [1 .. w] else [])
                else
                    let (layer, p) = (i - 1) `divMod` w
                        below q = 1 + (layer + 1) * w + q
                     in if i < 0 || layer + 1 >= l
                            then []
                            else
                                fmap below $
                                    if random
                                        then drawn seed w k layer p
                                        else [(p + j) `mod` w | j <- [0 .. k - 1]]

{- | Position @p@ itself, then @k - 1@ other distinct positions of a layer
@w@ wide, drawn (Floyd's sampling) from a stream keyed on the seed and the
node, so one node's draw never depends on having computed another's.
-}
drawn :: Seed -> Int -> Int -> Int -> Int -> [Int]
drawn seed w k layer p = p : [(p + o) `mod` w | o <- Set.toAscList offsets]
  where
    n = w - 1
    key = mix (mix (seed `xor` 0x243f6a8885a308d3) + fromIntegral layer) + fromIntegral p
    offsets = foldl pick Set.empty [n - (k - 1) + 1 .. n]
    pick acc j =
        let t = 1 + fromIntegral (mix (key + fromIntegral j * 0x9e3779b97f4a7c15) `mod` fromIntegral j)
         in if Set.member t acc then Set.insert j acc else Set.insert t acc

-- | The splitmix64 finaliser: a cheap bijective scrambler of a 'Word64'.
mix :: Word64 -> Word64
mix z0 =
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
     in z2 `xor` (z2 `shiftR` 31)

-------------------------------------------------------------------------------

-- | How a node's dependencies are written down.
data EdgeForm
    = -- | One @Vertices@ list: what 'Salmon.Builtin.Extension.deps' builds.
      AsVertices
    | -- | A @Connect@ per dependency: what repeated
      -- 'Salmon.Op.OpGraph.inject' builds.
      AsConnect
    | -- | An @Overlay@ per dependency: what repeated
      -- 'Salmon.Op.OpGraph.overlaid' builds.
      AsOverlay
    deriving (Show, Eq)

-- | What the profiles need to vary about the nodes. See 'defaultOptions'.
data Options = Options
    { optCheck :: CheckResult
    -- ^ The verdict every node's @check@ answers, without looking at anything.
    , optManaged :: Bool
    -- ^ Give every node a @managed@ action ('holdUntilCancelled').
    , optDynamics :: Int -> [Dynamic]
    -- ^ The @dynamics@ payload of a node, by number.
    , optEdges :: EdgeForm
    , optCollisions :: Rational
    -- ^ The fraction (clamped to @0..1@) of the nodes other than the root
    -- that are declared twice under one 'Ref' with differing @help@, which
    -- is what 'Salmon.Op.Dag.foldDag' reports as a conflict. Exactly
    -- 'colliderCount' nodes are; which ones is drawn from the seed.
    --
    -- The second declaration is a leaf written right after the real one,
    -- wherever the real one is depended upon. So the folded graph keeps its
    -- nodes and edges ('counts' still holds for those two), the second
    -- declaration is the surviving representative (last writer wins), and
    -- the expansion gains one leaf per occurrence of a colliding node.
    }

{- | What 'generate' uses: @check@ answers 'Immaterial' (what a node with no
@check@ says), nothing is @managed@, no @dynamics@, 'AsVertices', no
collisions.
-}
defaultOptions :: Options
defaultOptions =
    Options
        { optCheck = Immaterial
        , optManaged = False
        , optDynamics = const []
        , optEdges = AsVertices
        , optCollisions = 0
        }

{- | A @managed@ action with no effect: it holds its thread until that thread
is cancelled, which is how a driver stands a managed node down. It never
returns of its own accord, so it never looks like a process that exited.
-}
holdUntilCancelled :: Output -> IO ExitCode
holdUntilCancelled _ = forever (threadDelay 1000000000)

-- | The 'Ref' of a node, by number. Distinct numbers give distinct 'Ref's.
nodeRef :: Int -> Ref
nodeRef = mkRef "graph-fixture"

-- | The shorthand of a node, by number.
nodeName :: Int -> Text
nodeName i = "n" <> Text.pack (show i)

{- | How many nodes 'optCollisions' declares twice: the fraction of the
non-root nodes, rounded down.
-}
colliderCount :: Options -> Shape -> Int -> Integer
colliderCount opts shape size =
    floor (fraction opts * fromIntegral (countNodes (counts shape size) - 1))

fraction :: Options -> Rational
fraction opts = max 0 (min 1 opts.optCollisions)

{- | Whether a node is one of the 'colliderCount' declared twice. Partially
apply it and keep the result, as for 'successors'.

The non-root nodes are put through a permutation drawn from the seed (an
affine map modulo their number) and the first 'colliderCount' of the result
collide, so the count is exact and the choice needs no table.
-}
isCollider :: Options -> Shape -> Int -> Seed -> Int -> Bool
isCollider opts shape size seed
    | wanted <= 0 = const False
    | otherwise = \i -> i >= 1 && i <= others && ((a * (i - 1) + b) `mod` others) < wanted
  where
    others :: Int
    others = fromIntegral (countNodes (counts shape size)) - 1
    wanted :: Int
    wanted = fromIntegral (colliderCount opts shape size)
    draw :: Word64 -> Int
    draw salt = fromIntegral (mix (seed `xor` salt) `mod` fromIntegral others)
    b = draw 0x13198a2e03707344
    -- an affine map is a permutation exactly when its multiplier is coprime
    -- with the modulus; the search ends at the latest on a prime.
    a = head [c | c <- [max 1 (draw 0xa4093822299f31d0) ..], gcd c others == 1]

-- | 'generateWith' 'defaultOptions'.
generate :: Shape -> Int -> Seed -> Op
generate = generateWith defaultOptions

{- | The root of the fixture. Its predecessors are built when asked for and
not kept, so two expansions build the graph twice and neither holds it for
the other.
-}
generateWith :: Options -> Shape -> Int -> Seed -> Op
generateWith opts shape size seed = declared 0
  where
    next = successors shape size seed
    collides = isCollider opts shape size seed

    declared :: Int -> Op
    declared i = op (nodeName i) (Identity (written (concatMap declarations (next i)))) (dress i)

    declarations :: Int -> [Op]
    declarations j
        | collides j = [declared j, impostor j]
        | otherwise = [declared j]

    -- same 'Ref' and shorthand, differing @help@: a differing representative.
    impostor :: Int -> Op
    impostor j = op (nodeName j) nodeps (\x -> (dress j x){help = "graph fixture node (colliding declaration)"})

    dress :: Int -> Extension -> Extension
    dress i x =
        x
            { ref = nodeRef i
            , help = "graph fixture node"
            , check = pure opts.optCheck
            , managed = if opts.optManaged then Just holdUntilCancelled else Nothing
            , dynamics = opts.optDynamics i
            }

    written :: [Op] -> Graph Op
    written xs =
        case opts.optEdges of
            AsVertices -> Vertices xs
            AsConnect -> foldr (Connect . Vertices . pure) (Vertices []) xs
            AsOverlay -> foldr (Overlay . Vertices . pure) (Vertices []) xs
