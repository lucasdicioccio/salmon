-- A contravariant functor to track dependencies in a "serializer" style:
-- you defined basic nodes and then compose them.
module Salmon.Op.Track where

import Data.Functor.Contravariant (Contravariant (..), (>$<))
import Data.Functor.Contravariant.Divisible (Divisible (..), divided)
import Salmon.Op.Graph
import Salmon.Op.OpGraph

(>*<) :: (Divisible f) => f a -> f b -> f (a, b)
(>*<) = divided

infixr 5 >*<

newtype Track m n a
    = Track {run :: a -> OpGraph m n}

instance Contravariant (Track m n) where
    contramap f s = Track (run s . f)

instance (Applicative m, Monoid n) => Divisible (Track m n) where
    conquer = Track (const $ OpGraph (pure $ Vertices []) mempty)
    divide f t1 t2 = Track $ \a ->
        let
            (h, k) = f a
            x = run t1 h
            y = run t2 k
         in
            OpGraph (Vertices <$> pure [x, y]) mempty

{- | A function to inject a dependency form a tracer when generating an OpGraph.
At first it looks like the we could just directly apply.
-}
tracking ::
    (Applicative m) =>
    Track m n z ->
    (a -> (b, z)) ->
    a ->
    (b -> OpGraph m n) ->
    OpGraph m n
tracking t f arg use =
    let (b, z) = f arg
     in use b `inject` run t z

data Tracked m n a
    = Tracked
    { track :: Track m n a
    , obj :: a
    }

-- | A pure tracked merely is the constructor with some initial trace.
pureTracked :: Track m n a -> a -> Tracked m n a
pureTracked = Tracked

-- | Eval the OpGraph of a Tracked object.
trackedGraph :: Tracked m n a -> OpGraph m n
trackedGraph t = run t.track t.obj

-- | a quasi-functor which records all dependencies at the point of mapping
mapTracked :: (a -> b) -> Tracked m n a -> Tracked m n b
mapTracked f t = Tracked (Track $ const $ trackedGraph t) (f t.obj)

-- | a quasi-applicative which records all combined dependencies at the point of mapping
apTracked :: (Applicative m) => Tracked m n (a -> b) -> Tracked m n a -> Tracked m n b
apTracked tf ta = Tracked (Track $ const $ trackedGraph tf `overlaid` trackedGraph ta) (tf.obj ta.obj)

-- | a quasi-monad which records previous dependencies as predecessors before binding
bindTracked :: (Applicative m) => Tracked m n a -> (a -> Tracked m n b) -> Tracked m n b
bindTracked ta f = Tracked (Track $ const $ trackedGraph tb `inject` trackedGraph ta) b
  where
    tb@(Tracked _ b) = f ta.obj

-- | A similar to `tracking` but for a Tracked object.
using :: (Applicative m) => Tracked m n a -> (a -> OpGraph m n) -> OpGraph m n
using tracked use =
    tracking tracked.track dup tracked.obj use
  where
    dup a = (a, a)
