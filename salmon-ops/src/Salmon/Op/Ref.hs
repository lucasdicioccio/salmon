module Salmon.Op.Ref (
    Ref,
    unRef,
    shortRef,
    dotRef,
    mkRef,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Base64.URL as Base64.URL
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Printf (printf)

newtype Ref = Ref {unRef :: Text}
    deriving (Show, Eq, Ord)

{- | The bare text of the 'Ref'. This is what a 'Salmon.Actions.Query.Plan'
file carries and pastes back in, so it stays a plain string; the report
streams ("Salmon.Reporter.Tagged") render a 'Ref' as an object holding both
this and 'shortRef', through their own encoder rather than this instance.
-}
instance ToJSON Ref where
    toJSON = toJSON . unRef

instance FromJSON Ref where
    parseJSON = fmap Ref . parseJSON

instance Semigroup Ref where
    r1 <> r2 = dotRef $ unRef r1 <> unRef r2

{- | A short, stable, content-derived tag for a 'Ref' (the base64url encoding
of its own text, truncated to 8 characters) — the same "abbreviated SHA" idea.
'Salmon.Actions.Query.renderAnnotated' uses it to disambiguate colliding path
text without resorting to an arbitrary, traversal-order-dependent counter,
and a @#@-prefixed selector matches on it (see
'Salmon.Actions.Query.resolveRewrittenSelectors'). Lives here rather than in
"Salmon.Actions.Query" so that anything rendering a 'Ref' — the JSON report
encoding in particular — prints the same tag without importing the query
machinery.
-}
shortRef :: Ref -> Text
shortRef = Text.take 8 . Text.decodeUtf8 . Base64.URL.encode . Text.encodeUtf8 . unRef

{- | Build a 'Ref' from a "kind" tag and a structured 'Hashable' key that
identifies a node's identity within that kind — e.g. the node's own input
value (a 'FilePath', a tuple of fields, or a whole record), rather than a
hand-concatenated 'Text' string. Two calls with the same kind and equal keys
always produce the same 'Ref' (barring hash collisions, same caveat as
'dotRef'). Prefer this over 'dotRef' for new/touched call sites.
-}
mkRef :: (Hashable key) => Text -> key -> Ref
mkRef kind key = fromHash (hashWithSalt (hash kind) key)

{-# DEPRECATED dotRef "Prefer mkRef, which takes a kind tag plus a structured Hashable key instead of a hand-concatenated Text string." #-}
dotRef :: Text -> Ref
dotRef orig = fromHash (hash orig)

fromHash :: Int -> Ref
fromHash x =
    Ref $
        if x > 0
            then str x
            else "n" <> str (negate x)
  where
    str :: Int -> Text
    str = Text.pack . printf "%d"
