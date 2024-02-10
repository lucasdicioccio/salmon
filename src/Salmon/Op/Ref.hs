
module Salmon.Op.Ref
  ( Ref
  , unRef
  , dotRef
  ) where

import Data.Hashable (hash)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf (printf)

newtype Ref = Ref { unRef :: Text }
  deriving (Show, Eq, Ord)

instance Semigroup Ref where
  r1 <> r2 = dotRef $ unRef r1 <> unRef r2

dotRef :: Text -> Ref
dotRef orig = Ref $
  if x > 0
  then str x
  else "n" <> str (negate x)
  where
    str :: Int -> Text
    str = Text.pack . printf "%d"
    x :: Int
    x = hash orig
