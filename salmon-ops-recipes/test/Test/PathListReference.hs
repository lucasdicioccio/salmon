{- | 'Salmon.Actions.Query.outlinePaths' as it was before paths got a length
cap (the profiles' finding 4), kept as the reference "Test.ListedPathsSpec"
compares the current one with: at most @limit@ paths per node, every one in
full, shortest first by (rendered length, segments).

It holds nodes x depth segments on a deep graph: keep its inputs small.
Nothing outside the tests and the @graph-profiles@ benchmark uses it.

Written against the exported 'Query.Outline' functions: a node's shorthand
is the last segment of its first path, its children are 'outlineChildren',
and the walk visits the roots and children in the order the original did
(last met first), which only matters for an edge that closes a cycle.
-}
module Test.PathListReference (
    outlinePathsUncapped,
    renderedWidth,
) where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Salmon.Actions.Query as Query
import Salmon.Op.Ref (Ref)

-- | A path's rendered length as @\/a\/b@: what the listing sorts by first.
renderedWidth :: [Text] -> Int
renderedWidth p = sum (map Text.length p) + length p

outlinePathsUncapped :: Int -> Query.Outline -> Map Ref [[Text]]
outlinePathsUncapped limit o = List.foldl' place Map.empty topDown
  where
    shorthands :: Map Ref Text
    shorthands = Map.fromList [(ref, last path) | (path, ref, _) <- Query.outlineFirstPaths o, not (null path)]

    childrenRev ref = reverse (Query.outlineChildren o ref)

    rootsRev = reverse (Query.outlineRoots o)

    parents :: Map Ref [Ref]
    parents = Map.fromListWith (<>) [(c, [p]) | p <- Set.toList (Query.outlineRefs o), c <- childrenRev p]

    roots = Set.fromList rootsRev

    topDown :: [Ref]
    topDown = snd (List.foldl' visit (Set.empty, []) rootsRev)

    visit :: (Set Ref, [Ref]) -> Ref -> (Set Ref, [Ref])
    visit (seen, done) ref
        | ref `Set.member` seen = (seen, done)
        | otherwise =
            let (seen', done') = List.foldl' visit (Set.insert ref seen, done) (childrenRev ref)
             in (seen', ref : done')

    place :: Map Ref [[Text]] -> Ref -> Map Ref [[Text]]
    place acc ref = case Map.lookup ref shorthands of
        Nothing -> acc
        Just seg ->
            let own = [[seg] | ref `Set.member` roots]
                inherited =
                    [ p <> [seg]
                    | parent <- Map.findWithDefault [] ref parents
                    , p <- Map.findWithDefault [] parent acc
                    ]
             in Map.insert ref (shortest (own <> inherited)) acc

    shortest :: [[Text]] -> [[Text]]
    shortest = take limit . map snd . Set.toAscList . Set.fromList . map (\p -> (renderedWidth p, p))
