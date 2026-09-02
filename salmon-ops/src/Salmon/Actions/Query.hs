{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Targeting 'Salmon.Actions.UpDown.upTree' (and 'run tree'/'run dag') at a
subset of an already-expanded graph, per @specs/advance-querying.md@.

A node is addressed by its tree /position/ (the same @\/initialize\/chown@
paths 'Salmon.Actions.Help.printHelpCograph' already prints), not by
identity: the same 'Ref' can occur at several paths (a shared predecessor,
e.g. a directory two files sit in). Resolving a selector is therefore always
"match paths, then take the 'Ref' at each match" — see 'resolveSelectors'.
-}
module Salmon.Actions.Query (
    -- * Patterns
    PatternSegment (..),
    parsePattern,
    matchPattern,

    -- * Resolving selectors against an expanded graph
    pathedRefs,
    pathedNodes,
    resolveSelectors,

    -- * Applying an exclusion set
    forceSkip,

    -- * Human-readable output
    shortRef,
    renderAnnotated,
    printAnnotated,

    -- * Plans
    Plan (..),
    digestBytes,
) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Data.ByteString.Lazy as LByteString
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Foldable (toList, traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Numeric (showHex)

import Salmon.Actions.Help (pathText)
import Salmon.Builtin.Extension (Extension (..), Op)
import Salmon.Op.Actions
import Salmon.Op.Graph (Graph)
import Salmon.Op.OpGraph
import Salmon.Op.Ref (Ref, unRef)
import Salmon.Actions.UpDown (Requirement (Skippable))

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

-- | Every node's path (as segments, root-to-node) paired with the 'Ref' found there.
pathedRefs :: Cofree Graph Op -> [([Text], Ref)]
pathedRefs = map (\(path, ref, _help) -> (path, ref)) . pathedNodes

-- | Like 'pathedRefs', but also carries each node's 'Salmon.Builtin.Extension.help' text.
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
    entries = pathedRefs cograph
    matches pats = Set.fromList [ref | (path, ref) <- entries, pat <- map parsePattern pats, matchPattern pat path]
    allRefs = Set.fromList (map snd entries)
    selectedBase = if null selectPatterns then allRefs else matches selectPatterns
    excluded = matches excludePatterns
    selected = selectedBase `Set.difference` excluded

-------------------------------------------------------------------------------

{- | Rewrites every node whose 'Salmon.Op.Ref.Ref' is in the given set so its
'Salmon.Builtin.Extension.prelim' unconditionally reports 'Skippable',
leaving 'up'/'down'/'ref'/'dynamics' and the graph topology untouched.
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
        | ext.ref `Set.member` refs = ext{prelim = pure Skippable}
        | otherwise = ext

-------------------------------------------------------------------------------

{- | A short, stable, content-derived tag for a 'Ref' (the base64url encoding
of its own text, truncated to 8 characters) — the same "git abbreviated SHA"
idea, used to disambiguate colliding path text in 'renderAnnotated' without
resorting to an arbitrary, traversal-order-dependent counter.
-}
shortRef :: Ref -> Text
shortRef = Text.take 8 . Text.decodeUtf8 . Base64.URL.encode . Text.encodeUtf8 . unRef

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
        | dedupe = dedupeBy (\(_, ref, _) -> ref) (pathedNodes cograph)
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

dedupeBy :: (Ord b) => (a -> b) -> [a] -> [a]
dedupeBy f = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | f x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert (f x) seen) xs

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
