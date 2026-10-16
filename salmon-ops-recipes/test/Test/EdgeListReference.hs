{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | 'Salmon.Op.Dag.record', 'Salmon.Op.Dag.fromMagma',
'Salmon.Op.Dag.transposeOf', 'Salmon.Op.Dag.mergeDag' and
'Salmon.Actions.Query.outline' as they were before their edge lists stopped
searching and appending on the right (the profiles' finding 3), kept
verbatim as the reference "Test.EdgeListSpec" compares the current ones with.

Each is quadratic in the edges of one node: keep their inputs small. Nothing
outside the tests uses this module.
-}
module Test.EdgeListReference (
    recordAppending,
    fromMagmaAppending,
    transposeOfAppending,
    mergeDagAppending,
    OutlineAppending,
    outlineAppending,
    appendingRefs,
    appendingRoots,
    appendingChildren,
    appendingFirstPaths,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Salmon.Builtin.Extension (Extension (..), Op)
import Salmon.Op.Actions
import Salmon.Op.Dag (Conflict (..), Dag (..), dagOrder, dependenciesOf, emptyDag, representativeOf)
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref

-- | 'Salmon.Op.Dag.record', as it was.
recordAppending ::
    (Act ext -> Act ext -> Bool) ->
    Ref ->
    Act ext ->
    [Ref] ->
    Dag ext ->
    Dag ext
recordAppending same aref act predRefs dag =
    Dag
        { dagNodes = Map.insert aref act (dagNodes dag)
        , dagDependencies = foldl' addDependency deps0 predRefs
        , dagDependants = foldl' addDependant dependants0 predRefs
        , dagOrderRev = order'
        , dagConflicts = conflicts'
        }
  where
    previous = Map.lookup aref (dagNodes dag)

    conflicts'
        | Just old <- previous, not (same old act) =
            Conflict aref act old : dagConflicts dag
        | otherwise = dagConflicts dag

    order'
        | Map.member aref (dagNodes dag) = dagOrderRev dag
        | otherwise = aref : dagOrderRev dag

    deps0 = Map.insertWith (\_ old -> old) aref [] (dagDependencies dag)
    dependants0 = Map.insertWith (\_ old -> old) aref [] (dagDependants dag)

    addDependency m d = Map.insertWith snoc aref [d] (Map.insertWith (\_ old -> old) d [] m)
    addDependant m d = Map.insertWith snoc d [aref] m

    snoc [new] old
        | new `elem` old = old
        | otherwise = old <> [new]
    snoc new old = old <> filter (`notElem` old) new

-- | 'Salmon.Op.Dag.fromMagma', as it was.
fromMagmaAppending :: Map Ref (Act ext) -> Set (Ref, Ref) -> Dag ext
fromMagmaAppending magma edges = foldl' add emptyDag (Map.keys magma)
  where
    add dag r = recordAppending (\_ _ -> True) r (magma Map.! r) (deps r) dag

    deps :: Ref -> [Ref]
    deps r = Map.findWithDefault [] r incoming

    incoming :: Map Ref [Ref]
    incoming =
        Map.fromListWith
            (flip (<>))
            [ (dependant, [dependency])
            | (dependency, dependant) <- Set.toList edges
            , Map.member dependency magma
            , Map.member dependant magma
            ]

-- | 'Salmon.Op.Dag.transposeOf', as it was.
transposeOfAppending :: Map Ref [Ref] -> Map Ref [Ref]
transposeOfAppending deps =
    Map.union
        (Map.fromListWith (flip (<>)) [(d, [r]) | (r, ds) <- Map.toList deps, d <- ds])
        (Map.map (const []) deps)

-- | 'Salmon.Op.Dag.mergeDag', as it was.
mergeDagAppending :: (Act ext -> Act ext -> Bool) -> Dag ext -> Dag ext -> Dag ext
mergeDagAppending same into from = foldl' step into (dagOrder from)
  where
    step dag r =
        case representativeOf from r of
            Nothing -> dag
            Just act -> recordAppending same r act (dependenciesOf from r) dag

-------------------------------------------------------------------------------

-- | 'Salmon.Actions.Query.Outline', as it was, with its fields open.
data OutlineAppending = OutlineAppending
    { oaEntries :: !(Map Ref EntryAppending)
    , oaRootsRev :: ![Ref]
    , oaOrderRev :: ![Ref]
    }

data EntryAppending = EntryAppending
    { eaShorthand :: !Text
    , eaHelp :: !Text
    , eaFirstPath :: ![Text]
    , eaChildrenRev :: ![Ref]
    }

-- | 'Salmon.Actions.Query.outline', as it was.
outlineAppending :: Cofree Graph Op -> OutlineAppending
outlineAppending = go Nothing [] (OutlineAppending Map.empty [] [])
  where
    go :: Maybe Ref -> [Text] -> OutlineAppending -> Cofree Graph Op -> OutlineAppending
    go parent pfx acc (x :< g) =
        case x.node of
            Actionless -> List.foldl' (go parent pfx) acc (toList g)
            Actions act ->
                let ref' = act.extension.ref
                 in case Map.lookup ref' acc.oaEntries of
                        Nothing ->
                            let path = pfx <> [shorthand act]
                                entered =
                                    acc
                                        { oaEntries =
                                            Map.insert ref' (EntryAppending (shorthand act) act.extension.help path []) acc.oaEntries
                                        , oaOrderRev = ref' : acc.oaOrderRev
                                        }
                             in List.foldl' (go (Just ref') path) (link parent ref' entered) (toList g)
                        Just e ->
                            List.foldl' (peek ref' e.eaFirstPath) (link parent ref' acc) (toList g)

    peek :: Ref -> [Text] -> OutlineAppending -> Cofree Graph Op -> OutlineAppending
    peek ref' path acc c@(x :< g) =
        case x.node of
            Actionless -> List.foldl' (peek ref' path) acc (toList g)
            Actions act
                | linked (Just ref') act.extension.ref acc -> acc
                | otherwise -> go (Just ref') path acc c

    linked :: Maybe Ref -> Ref -> OutlineAppending -> Bool
    linked Nothing ref' acc = ref' `elem` acc.oaRootsRev
    linked (Just parent) ref' acc =
        maybe False (elem ref' . (.eaChildrenRev)) (Map.lookup parent acc.oaEntries)

    link :: Maybe Ref -> Ref -> OutlineAppending -> OutlineAppending
    link parent ref' acc
        | linked parent ref' acc = acc
        | otherwise = case parent of
            Nothing -> acc{oaRootsRev = ref' : acc.oaRootsRev}
            Just p ->
                acc{oaEntries = Map.adjust (\e -> e{eaChildrenRev = ref' : e.eaChildrenRev}) p acc.oaEntries}

-- | What 'Salmon.Actions.Query.outlineRefs' reads.
appendingRefs :: OutlineAppending -> Set Ref
appendingRefs = Map.keysSet . (.oaEntries)

-- | What 'Salmon.Actions.Query.outlineRoots' reads.
appendingRoots :: OutlineAppending -> [Ref]
appendingRoots o = reverse o.oaRootsRev

-- | What 'Salmon.Actions.Query.outlineChildren' reads.
appendingChildren :: OutlineAppending -> Ref -> [Ref]
appendingChildren o r = maybe [] (reverse . (.eaChildrenRev)) (Map.lookup r o.oaEntries)

-- | 'Salmon.Actions.Query.outlineFirstPaths', as it was.
appendingFirstPaths :: OutlineAppending -> [([Text], Ref, Text)]
appendingFirstPaths o =
    [ (e.eaFirstPath, r, e.eaHelp)
    | r <- reverse o.oaOrderRev
    , Just e <- [Map.lookup r o.oaEntries]
    ]
