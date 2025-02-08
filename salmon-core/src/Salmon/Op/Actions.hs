{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Salmon.Op.Actions where

import Data.Text (Text)

{- | Actions are bundles of named effects ongoing in a given monad.

A special action name Actionless is a noop and allows to define
a Monoid on a decorated bag of structures.

`Actions e` is isomorphic to `Maybe (Act e)`.
-}
data Actions e
    = -- | a true no-op action as in, it behaves as a mempty for the Monoid instance
      Actionless
    | -- | a proper set of actions
      Actions (Act e)
    deriving (Show, Functor, Foldable, Traversable)

-- | The monoidal actions revert the up and down.
instance (Semigroup e) => Semigroup (Actions e) where
    Actionless <> b = b
    a <> Actionless = a
    (Actions a) <> (Actions b) =
        Actions $
            Act
                (shorthand a <> "|" <> shorthand b)
                (extension a <> extension b)

-- | The mempty is Actionless
instance (Semigroup e) => Monoid (Actions e) where
    mempty = Actionless

-- | A name for something.
type ShortHand = Text

-- | Base action bag of function.
data Act extension = Act
    { shorthand :: ShortHand
    , extension :: extension
    }
    deriving (Show, Functor, Foldable, Traversable)
