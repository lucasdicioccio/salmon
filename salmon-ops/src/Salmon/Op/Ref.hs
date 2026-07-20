module Salmon.Op.Ref (
    Ref,
    unRef,
    dotRef,
    mkRef,
) where

import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf (printf)

newtype Ref = Ref {unRef :: Text}
    deriving (Show, Eq, Ord)

instance Semigroup Ref where
    r1 <> r2 = dotRef $ unRef r1 <> unRef r2

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
