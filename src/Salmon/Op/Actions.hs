{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Salmon.Op.Actions where

import Data.Text (Text)

-- | A name for something.
type ShortHand = Text

-- | Base action bag of function.
data Act extension = Act
  { shorthand    :: ShortHand
  , extension    :: extension
  } deriving (Show, Functor)

-- | Actions are bundles of effects ongoing in a given monad m.
--
-- A special action name Actionless is a noop and allows to define
-- a Monoid on a decorated bag of structures.
-- 
-- `Actions e` is isomorphic to `Maybe (Act e)`.
data Actions e
  = Actionless
  -- ^ a true no-op action as in, it behaves as a mempty for the Monoid instance
  | Actions (Act e)
  -- ^ a proper set of actions
  deriving (Show, Functor)

-- | A shorthand for when using no exension.
type ActionsN' = Actions ()

-- | The monoidal actions revert the up and down.
instance (Semigroup e) => Semigroup (Actions e) where
  Actionless <> b = b
  a <> Actionless = a
  (Actions a) <> (Actions b) =
     Actions
       $ Act
          (shorthand a <> "|" <> shorthand b)
          (extension a <> extension b)
  
-- | The mempty is Actionless
instance (Semigroup e) => Monoid (Actions e) where
  mempty = Actionless
