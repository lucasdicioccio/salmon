{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds #-}

module Salmon.Op.Graph where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Classes
import GHC.Generics

{- Graph is an algebraic graph from the alga paper.

   We depart from the alga paper by using a single constructor named `Vertices
[a]` which is isomorphic to the branches `empty | vertex | unconnected-overlay`
in Alga. This small digression allows us to be more compact.

-- todo: consider playing with t instead of Graph as recursion could be recovered via a (Fix GraphF)
-}
data Graph a
    = Vertices [a]
    | Connect (Graph a) (Graph a)
    | Overlay (Graph a) (Graph a)
    deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic)

instance (FromJSON a) => FromJSON (Graph a)
instance (ToJSON a) => ToJSON (Graph a)

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
