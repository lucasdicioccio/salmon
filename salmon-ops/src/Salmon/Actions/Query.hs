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
    resolveSelectors,

    -- * Applying an exclusion set
    forceSkip,

    -- * Human-readable output
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
import Salmon.Op.OpGraph
import Salmon.Op.Ref (Ref)
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
pathedRefs = go []
  where
    go :: [Text] -> Cofree Graph Op -> [([Text], Ref)]
    go pfx (x :< g) =
        case x.node of
            Actionless -> concatMap (go pfx) (toList g)
            Actions act ->
                let path = pfx <> [shorthand act]
                 in (path, act.extension.ref) : concatMap (go path) (toList g)

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

-- | Mirrors 'Salmon.Actions.Help.printHelpCograph', annotating matched paths.
printAnnotated :: Cofree Graph Op -> Set Ref -> Set Ref -> IO ()
printAnnotated cograph selected excluded =
    traverse_ Text.putStrLn [line path ref | (path, ref) <- pathedRefs cograph]
  where
    line path ref = pathText path <> annotation ref
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
