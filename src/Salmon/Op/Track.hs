
-- A contravariant functor to track dependencies in a "serializer" style:
-- you defined basic nodes and then compose them.
module Salmon.Op.Track where

import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Data.Functor.Contravariant (Contravariant(..),(>$<))
import Data.Functor.Contravariant.Divisible (Divisible(..), divided)

(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided

infixr 5 >*<

newtype Track m n a =
  Track { run :: a -> OpGraph m n }

instance Contravariant (Track m n) where
  contramap f s = Track (run s . f)

instance (Applicative m, Monoid n) => Divisible (Track m n) where
  conquer = Track (const $ OpGraph (pure $ Vertices []) mempty)
  divide f t1 t2 = Track $ \a ->  
    let
      (h,k) = f a
      x = run t1 h
      y = run t2 k
    in
    OpGraph (Vertices <$> pure [x,y]) mempty
