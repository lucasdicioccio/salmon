{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Targeting 'Salmon.Actions.UpDown.upTree' (and 'run tree'/'run dag') at a
subset of an already-expanded graph, per @specs/advance-querying.md@.

A node is addressed by its tree /position/ (the same @\/initialize\/chown@
paths 'Salmon.Actions.Help.printHelpCograph' already prints), not by
identity: the same 'Ref' can occur at several paths (a shared predecessor,
e.g. a directory two files sit in). Resolving a selector is therefore always
"match paths, then take the 'Ref' at each match" — see 'resolveSelectors'.

The paths themselves are never listed to do that. The expanded graph is a
/tree/: a shared predecessor is a whole subtree again under each of its
parents, so the number of paths to a node doubles with every diamond above
it. Everything here that must answer on any graph goes through an 'Outline'
instead — the same graph with one entry per 'Ref' — and only 'pathedNodes',
which is asked for exactly that listing, still walks every occurrence.
-}
module Salmon.Actions.Query (
    -- * Patterns
    PatternSegment (..),
    parsePattern,
    matchPattern,

    -- * One entry per node
    Outline,
    outline,
    outlineRefs,
    outlineFirstPaths,
    matchOutline,
    pathLimit,
    outlinePaths,

    -- * Resolving selectors against an expanded graph
    pathedRefs,
    pathedNodes,
    resolveSelectors,
    resolveRewrittenSelectors,

    -- * Applying an exclusion set
    forceSkip,

    -- * Human-readable output
    shortRef, -- re-exported from "Salmon.Op.Ref", where it lives
    renderAnnotated,
    printAnnotated,

    -- * Plans
    Plan (..),
    digestBytes,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Foldable (toList, traverse_)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Numeric (showHex)

import Salmon.Actions.Help (pathText)
import Salmon.Builtin.Extension (Extension (..), Op)
import Salmon.Op.Actions
import Salmon.Op.Graph (Graph)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph
import Salmon.Op.Ref (Ref, shortRef, unRef)
import Salmon.Op.Rewrite (Rewritten)
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Actions.UpDown (CheckResult (Skipped))

-------------------------------------------------------------------------------

-- | One segment of a parsed selector pattern.
data PatternSegment
    = Lit Text
    | -- | @*@: exactly one segment.
      Star
    | -- | @**@: zero or more segments (any depth, including none).
      DoubleStar
    deriving (Show, Eq)

-- | Parses a @\/a\/b\/*\/**@-style pattern into 'PatternSegment's.
parsePattern :: Text -> [PatternSegment]
parsePattern raw =
    map toSegment $ filter (not . Text.null) $ Text.splitOn "/" raw
  where
    toSegment "*" = Star
    toSegment "**" = DoubleStar
    toSegment s = Lit s

-- | Does this parsed pattern match this node path (both as segment lists)?
matchPattern :: [PatternSegment] -> [Text] -> Bool
matchPattern [] [] = True
matchPattern [] (_ : _) = False
matchPattern (DoubleStar : ps) path =
    matchPattern ps path || case path of
        [] -> False
        (_ : rest) -> matchPattern (DoubleStar : ps) rest
matchPattern (_ : _) [] = False
matchPattern (Star : ps) (_ : rest) = matchPattern ps rest
matchPattern (Lit l : ps) (seg : rest) = l == seg && matchPattern ps rest

-------------------------------------------------------------------------------

{- | An expanded graph with one entry per 'Ref': what a node is called, the
nodes directly under it, and the path it was first met at.

Building it ('outline') descends into a 'Ref' the first time it is met and
not again, which is what keeps it linear in nodes and edges where the tree
it is read from is exponential in the number of diamonds. A later occurrence
is looked at one level deep: a node directly under it that was not under
this 'Ref' before is added and walked in turn, which is enough for a node
declared a second time with one more dependency. What it does not see is an
occurrence whose own dependencies are the ones already known but whose
dependencies' subtrees differ from their first occurrence; and a 'Ref' met
under two shorthands keeps the first.
-}
data Outline = Outline
    { outlineEntries :: !(Map Ref Entry)
    , outlineRootsRev :: ![Ref]
    -- ^ the nodes with no 'Ref'-carrying node above them, last met first
    , outlineOrderRev :: ![Ref]
    -- ^ every node, last met first
    }

data Entry = Entry
    { entryShorthand :: !Text
    , entryHelp :: !Text
    , entryFirstPath :: ![Text]
    , entryChildrenRev :: ![Ref]
    }

-- | See 'Outline'.
outline :: Cofree Graph Op -> Outline
outline = go Nothing [] (Outline Map.empty [] [])
  where
    -- the node above (if any), the path to it, and what has been seen so far.
    go :: Maybe Ref -> [Text] -> Outline -> Cofree Graph Op -> Outline
    go parent pfx acc (x :< g) =
        case x.node of
            Actionless -> List.foldl' (go parent pfx) acc (toList g)
            Actions act ->
                let ref = act.extension.ref
                 in case Map.lookup ref acc.outlineEntries of
                        Nothing ->
                            let path = pfx <> [shorthand act]
                                entered =
                                    acc
                                        { outlineEntries =
                                            Map.insert ref (Entry (shorthand act) act.extension.help path []) acc.outlineEntries
                                        , outlineOrderRev = ref : acc.outlineOrderRev
                                        }
                             in List.foldl' (go (Just ref) path) (link parent ref entered) (toList g)
                        Just e ->
                            List.foldl' (peek ref e.entryFirstPath) (link parent ref acc) (toList g)

    -- a later occurrence of @ref@: only what is new directly under it is walked.
    peek :: Ref -> [Text] -> Outline -> Cofree Graph Op -> Outline
    peek ref path acc c@(x :< g) =
        case x.node of
            Actionless -> List.foldl' (peek ref path) acc (toList g)
            Actions act
                | linked (Just ref) act.extension.ref acc -> acc
                | otherwise -> go (Just ref) path acc c

    linked :: Maybe Ref -> Ref -> Outline -> Bool
    linked Nothing ref acc = ref `elem` acc.outlineRootsRev
    linked (Just parent) ref acc =
        maybe False (elem ref . (.entryChildrenRev)) (Map.lookup parent acc.outlineEntries)

    link :: Maybe Ref -> Ref -> Outline -> Outline
    link parent ref acc
        | linked parent ref acc = acc
        | otherwise = case parent of
            Nothing -> acc{outlineRootsRev = ref : acc.outlineRootsRev}
            Just p ->
                acc{outlineEntries = Map.adjust (\e -> e{entryChildrenRev = ref : e.entryChildrenRev}) p acc.outlineEntries}

-- | Every node of the graph.
outlineRefs :: Outline -> Set Ref
outlineRefs = Map.keysSet . (.outlineEntries)

{- | Each node once, in the order and at the path a walk of the whole tree
first meets it, with its help text: 'pathedNodes' keeping the first entry
per 'Ref', without listing the others.
-}
outlineFirstPaths :: Outline -> [([Text], Ref, Text)]
outlineFirstPaths o =
    [ (e.entryFirstPath, ref, e.entryHelp)
    | ref <- reverse o.outlineOrderRev
    , Just e <- [Map.lookup ref o.outlineEntries]
    ]

{- | The nodes reached by at least one path the pattern matches: what
filtering 'pathedRefs' with 'matchPattern' gives, without the listing.

The pattern is read as an automaton whose states are its suffixes, and the
graph is searched for the (node, suffix) pairs that can occur, each visited
once — so the cost is nodes and edges times the pattern's length, however
many paths there are.
-}
matchOutline :: [PatternSegment] -> Outline -> Set Ref
matchOutline pat o = go Set.empty Set.empty [(r, i) | r <- o.outlineRootsRev, i <- closure 0]
  where
    n = length pat
    segments = Map.fromList (zip [0 :: Int ..] pat)

    -- a @**@ may match nothing, so being at one is also being past it.
    closure :: Int -> [Int]
    closure i = case Map.lookup i segments of
        Just DoubleStar -> i : closure (i + 1)
        _ -> [i]

    -- where reading one path segment at suffix @i@ leaves the pattern.
    consume :: Text -> Int -> [Int]
    consume seg i = case Map.lookup i segments of
        Just (Lit l) | l == seg -> closure (i + 1)
        Just Star -> closure (i + 1)
        Just DoubleStar -> closure i
        _ -> []

    go :: Set (Ref, Int) -> Set Ref -> [(Ref, Int)] -> Set Ref
    go _ matched [] = matched
    go visited matched (item@(ref, i) : rest)
        | item `Set.member` visited = go visited matched rest
        | otherwise = case Map.lookup ref o.outlineEntries of
            Nothing -> go (Set.insert item visited) matched rest
            Just e ->
                let after = consume e.entryShorthand i
                    matched' = if n `elem` after then Set.insert ref matched else matched
                    next = [(c, j) | c <- e.entryChildrenRev, j <- after, j < n]
                 in go (Set.insert item visited) matched' (next <> rest)

-- | How many paths per node the callers in this tree ask 'outlinePaths' for.
pathLimit :: Int
pathLimit = 8

{- | At most @limit@ paths to each node, shortest first (by rendered length,
then by segments): the head of what sorting 'pathedRefs' per 'Ref' gives.
A node with more paths than that is reachable by paths not listed here, and
a pattern built from one of those still selects it ('matchOutline').

Computed from the top down, each node's paths from those of the nodes above
it; the shortest paths to a node extend shortest paths to the node above, so
keeping @limit@ at each step loses none of the final ones.
-}
outlinePaths :: Int -> Outline -> Map Ref [[Text]]
outlinePaths limit o = List.foldl' place Map.empty topDown
  where
    childrenOf ref = maybe [] (.entryChildrenRev) (Map.lookup ref o.outlineEntries)

    parents :: Map Ref [Ref]
    parents = Map.fromListWith (<>) [(c, [p]) | (p, e) <- Map.toList o.outlineEntries, c <- e.entryChildrenRev]

    roots = Set.fromList o.outlineRootsRev

    -- every node after all the nodes above it (a depth-first post-order,
    -- reversed). An edge closing a cycle is one whose upper end comes later
    -- and has no paths yet, so it contributes none.
    topDown :: [Ref]
    topDown = snd (List.foldl' visit (Set.empty, []) o.outlineRootsRev)

    visit :: (Set Ref, [Ref]) -> Ref -> (Set Ref, [Ref])
    visit (seen, done) ref
        | ref `Set.member` seen = (seen, done)
        | otherwise =
            let (seen', done') = List.foldl' visit (Set.insert ref seen, done) (childrenOf ref)
             in (seen', ref : done')

    place :: Map Ref [[Text]] -> Ref -> Map Ref [[Text]]
    place acc ref = case Map.lookup ref o.outlineEntries of
        Nothing -> acc
        Just e ->
            let own = [[e.entryShorthand] | ref `Set.member` roots]
                inherited =
                    [ p <> [e.entryShorthand]
                    | parent <- Map.findWithDefault [] ref parents
                    , p <- Map.findWithDefault [] parent acc
                    ]
             in Map.insert ref (shortest (own <> inherited)) acc

    -- the @limit@ shortest of these paths, without repeats.
    shortest :: [[Text]] -> [[Text]]
    shortest = take limit . map snd . Set.toAscList . Set.fromList . map (\p -> (rendered p, p))

    rendered :: [Text] -> Int
    rendered p = sum (map Text.length p) + length p

-------------------------------------------------------------------------------

{- | Every node's path (as segments, root-to-node) paired with the 'Ref' found
there. One entry per /occurrence/, like 'pathedNodes'.
-}
pathedRefs :: Cofree Graph Op -> [([Text], Ref)]
pathedRefs = map (\(path, ref, _help) -> (path, ref)) . pathedNodes

{- | Like 'pathedRefs', but also carries each node's 'Salmon.Builtin.Extension.help' text.

One entry per occurrence in the expanded tree, so its length is exponential
in the number of shared dependencies stacked on one another: a listing for
whoever asked to see every path, never a step towards answering something
else. 'outline' is that step.
-}
pathedNodes :: Cofree Graph Op -> [([Text], Ref, Text)]
pathedNodes = go []
  where
    go :: [Text] -> Cofree Graph Op -> [([Text], Ref, Text)]
    go pfx (x :< g) =
        case x.node of
            Actionless -> concatMap (go pfx) (toList g)
            Actions act ->
                let path = pfx <> [shorthand act]
                 in (path, act.extension.ref, act.extension.help) : concatMap (go path) (toList g)

-------------------------------------------------------------------------------

-- | @resolveSelectors cograph selectPatterns excludePatterns@: @(selected, excluded)@.
resolveSelectors ::
    Cofree Graph Op ->
    [Text] ->
    [Text] ->
    (Set Ref, Set Ref)
resolveSelectors cograph selectPatterns excludePatterns =
    (selected, excluded)
  where
    o = outline cograph
    matches pats = Set.unions [matchOutline (parsePattern pat) o | pat <- pats]
    allRefs = outlineRefs o
    selectedBase = if null selectPatterns then allRefs else matches selectPatterns
    excluded = matches excludePatterns
    selected = selectedBase `Set.difference` excluded

{- | Like 'resolveSelectors', but rewrite-aware (see @specs\/advance-querying.md@
and (R4) in @specs\/per-node-state-machines-remaining.md@): a pattern is
resolved as a path glob against the /declared/ @cograph@ exactly as before,
__except__ one beginning with @#@, which instead matches by 'Ref' — either a
declared node's own, or (via 'Salmon.Op.Rewrite.membersOf') a
rewrite-introduced node's, expanded back to the declared nodes it stands in
for.

That fallback exists because a path glob fundamentally cannot address a
rewrite-introduced node (a package-install batch, say): such a node was
never declared, so it has no position in @cograph@ for a pattern to match —
it only exists in 'computed', produced after the fold. Its 'Ref' is the one
thing about it a pattern /can/ name, and it is exactly the text
'shortRef'\/'renderAnnotated' already print (the @#@ prefix mirrors
'renderAnnotated's own @" #" <> shortRef ref@ disambiguation suffix, so what
a render prints can be pasted straight back in as a selector). A fragment
matches as a prefix of either the short or the full 'Ref' text, so an
operator can paste the short form from a tree\/dag render or a longer,
disambiguating chunk of a full ref if a short one turns out ambiguous.

Every result is still a __declared__ 'Ref' set — this does not change what
'query plan'\/'query show' consume, since 'run up'/'run down''s
@phaseIgnored@ and 'Salmon.Op.Rewrite.collectDynamic' are both keyed on
declared refs. Addressing a batch by its own ref is therefore equivalent to
addressing every declared node it was built from — excluding \"the batch\"
/is/ excluding all 20 packages that went into it, which is the only
coherent meaning a plan (a set of declared exclusions consulted /before/ any
rewrite runs) can give it.
-}
resolveRewrittenSelectors ::
    Cofree Graph Op ->
    Rewritten Extension ->
    [Text] ->
    [Text] ->
    (Set Ref, Set Ref)
resolveRewrittenSelectors cograph computed selectPatterns excludePatterns =
    (selected, excluded)
  where
    o = outline cograph
    allRefs = outlineRefs o

    (selRefPats, selPathPats) = List.partition isRefPattern selectPatterns
    (excRefPats, excPathPats) = List.partition isRefPattern excludePatterns

    matchesOf :: [Text] -> [Text] -> Set Ref
    matchesOf pathPats refPats =
        Set.unions [matchOutline (parsePattern pat) o | pat <- pathPats]
            `Set.union` Set.unions (map matchRefPattern refPats)

    -- an empty select list still means "everything", exactly as
    -- 'resolveSelectors' — checked against the *combined* pattern list, not
    -- just its path half, or a select made of nothing but '#'-patterns
    -- would silently widen to "everything" instead of narrowing to what was
    -- actually asked for.
    selectedBase = if null selectPatterns then allRefs else matchesOf selPathPats selRefPats
    excluded = matchesOf excPathPats excRefPats
    selected = selectedBase `Set.difference` excluded

    isRefPattern :: Text -> Bool
    isRefPattern = Text.isPrefixOf "#"

    -- every declared 'Ref' a '#'-pattern resolves to: its direct matches
    -- among declared nodes, plus every declared member of a matching
    -- computed (rewrite-introduced) node.
    matchRefPattern :: Text -> Set Ref
    matchRefPattern pat = declaredHits `Set.union` viaComputed
      where
        fragment = Text.drop 1 pat
        declaredHits = Set.filter (matchesRefFragment fragment) allRefs
        computedHits = [cref | cref <- Map.keys (Dag.dagNodes computed.computedDag), matchesRefFragment fragment cref]
        viaComputed = Set.unions (map (Rewrite.membersOf computed) computedHits)

    matchesRefFragment :: Text -> Ref -> Bool
    matchesRefFragment fragment ref = fragment `Text.isPrefixOf` shortRef ref || fragment `Text.isPrefixOf` unRef ref

-------------------------------------------------------------------------------

{- | Rewrites every node whose 'Salmon.Op.Ref.Ref' is in the given set so its
'Salmon.Builtin.Extension.check' unconditionally reports 'Skipped',
leaving 'up'/'down'/'ref'/'dynamics' and the graph topology untouched.
This is the one producer of 'Skipped': it is a statement about a decision
made over the node, not about the node's effect.
Relies on 'OpGraph's derived 'Functor' recursing through the effectful
'predecessors' field (works because 'Op's @meval@ is 'Data.Functor.Identity',
itself a 'Functor') and on 'Actions'' own 'Functor' instance over its
extension type.
-}
forceSkip :: Set Ref -> Op -> Op
forceSkip refs = fmap (fmap rewrite)
  where
    rewrite :: Extension -> Extension
    rewrite ext
        | ext.ref `Set.member` refs = ext{check = pure Skipped}
        | otherwise = ext

-------------------------------------------------------------------------------

{- | Mirrors 'Salmon.Actions.Help.printHelpCograph', annotating matched paths.

@dedupe@: the same 'Ref' can occur at several paths (a shared predecessor); when
'True', only its first-encountered occurrence is printed instead of one line per
path. @showDescriptions@: when 'True', a node's 'Salmon.Builtin.Extension.help'
text (if non-empty) is printed on its own indented line right below the node's
path, prefixed with @"  # "@.

Path text alone doesn't always identify a node: sibling nodes built with the
same 'Salmon.Builtin.Extension.ShortHand' (e.g. several migration files each
going through the same @pg-script@ builder) render the exact same path text
while carrying distinct 'Ref's. Every occurrence of such a colliding path
(post-dedupe) is suffixed with @" #" <> 'shortRef' ref@ — stable across runs
and independent of traversal order, unlike an incrementing counter — so the
repeats are visibly distinguished instead of looking like accidental
duplicates.
-}
printAnnotated :: Cofree Graph Op -> Set Ref -> Set Ref -> Bool -> Bool -> IO ()
printAnnotated cograph selected excluded dedupe showDescriptions =
    traverse_ Text.putStrLn (renderAnnotated cograph selected excluded dedupe showDescriptions)

-- | Pure line-rendering behind 'printAnnotated' (kept separate so it's testable without IO capture).
renderAnnotated :: Cofree Graph Op -> Set Ref -> Set Ref -> Bool -> Bool -> [Text]
renderAnnotated cograph selected excluded dedupe showDescriptions =
    concatMap render entries
  where
    entries
        | dedupe = outlineFirstPaths (outline cograph)
        | otherwise = pathedNodes cograph

    -- how many (post-dedupe) entries render to this exact path text; >1 means it needs disambiguating.
    pathCounts :: Map Text Int
    pathCounts = Map.fromListWith (+) [(pathText path, 1 :: Int) | (path, _, _) <- entries]

    render :: ([Text], Ref, Text) -> [Text]
    render (path, ref, help) = line : descLine
      where
        key = pathText path
        collides = Map.findWithDefault 0 key pathCounts > 1
        line = key <> (if collides then " #" <> shortRef ref else "") <> annotation ref
        descLine = ["  # " <> help | showDescriptions && not (Text.null help)]

    annotation ref
        | ref `Set.member` excluded = " [excluded]"
        | ref `Set.member` selected = " [selected]"
        | otherwise = ""

-------------------------------------------------------------------------------

{- | An exclusion plan resolved against one specific directive.

'planDirective' is populated only when @query plan@ is run with
@--embed-directive@ — by default a plan is a small companion file that
travels alongside its directive (checked via 'planDirectiveDigest'), not a
copy of it. Embedding is for the case where you want the plan itself to be a
standalone, replayable artifact (e.g. archived for an audit trail); recover
the embedded bytes with @query extract-directive@ rather than reading this
field directly, so a caller never has to care whether a given plan carries
one.
-}
data Plan = Plan
    { planDirectiveDigest :: Text
    , planExcludedRefs :: [Ref]
    , planExcludedPatterns :: [Text]
    , planDirective :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Plan
instance FromJSON Plan

{- | sha256 of raw bytes, hex-encoded. Callers must hash the exact bytes read
off stdin, never a re-'Data.Aeson.encode'd value (aeson gives no
cross-invocation guarantee that decode-then-re-encode round-trips byte for
byte).
-}
digestBytes :: LByteString.ByteString -> Text
digestBytes = Text.concat . map hex . ByteString.unpack . SHA256.hashlazy
  where
    hex w =
        let s = showHex w ""
         in Text.pack (if length s == 1 then '0' : s else s)
