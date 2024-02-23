{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds #-}
module Salmon.Op.Graph where

import GHC.Generics
import Data.Functor.Classes
import Data.Aeson (FromJSON, ToJSON)

-- from alga but with [a] for slightly more compact writing of the empty|vertex|unconnected-overlays
-- todo play with t instead of Graph, though could be recovered via a (Fix Graph)
data Graph a
  = Vertices [a]
  | Connect (Graph a) (Graph a)
  | Overlay (Graph a) (Graph a)
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic)

instance FromJSON a => FromJSON (Graph a)
instance ToJSON a => ToJSON (Graph a)

instance Show1 Graph where
  liftShowsPrec f g n gr =
    case gr of
      Vertices xs -> g xs
      Connect gr1 gr2 ->
        let
          s1 = liftShowsPrec f g n gr1
          s2 = liftShowsPrec f g n gr2
        in
        \sfx -> "(" ++ s1 ("->" ++ s2 (")" ++ sfx))
      Overlay gr1 gr2 ->
        let
          s1 = liftShowsPrec f g n gr1
          s2 = liftShowsPrec f g n gr2
        in
        \sfx -> "{" ++ s1 ("," ++ s2 ("}" ++ sfx))

