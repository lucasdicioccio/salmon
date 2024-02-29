{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Salmon.Builtin.Extension where

import Control.Monad.Identity
import Control.Comonad.Cofree
import Data.Maybe (catMaybes)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)

import Salmon.Op.Ref (Ref, dotRef)
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Eval
import Salmon.Op.Actions
import Salmon.Op.Track
import Salmon.Op.Configure
import Salmon.Actions.Dot (PlaceHolder(..))
import Salmon.Actions.UpDown (Requirement(..))

-- | Instanciate actions.
type Actions' = Actions Extension

-- | A short (one-liner) helper string.
type Help = Text

-- | An longer helper string.
type Note = Text

-- | Our demo extension.
data Extension = Extension {
    help         :: Help
  , notes        :: [Note]
  , ref          :: Ref
  , up           :: IO ()
  , prelim       :: IO Requirement
  , down         :: IO ()
  , check        :: IO ()
  , notify       :: IO ()
  , dynamics     :: [Dynamic]
}

instance Semigroup Extension where
  a <> b =
    Extension
      (help a <> "|" <> help b)
      (notes a <> notes b)
      (ref a <> ref b)
      (up a <> up b)
      (prelim a <> prelim b)
      (down b <> down a)
      (check a <> check b)
      (notify b <> notify a)
      (dynamics a <> dynamics b)

type Op = OpGraph Identity Actions'

type Track' a = Track Identity Actions' a

type Tracked' a = Tracked Identity Actions' a

evalDeps :: Op -> Cofree Graph Op
evalDeps = runIdentity . expand

nodeps :: Identity (Graph Op)
nodeps = pure $ Vertices []

deps :: [Op] -> Identity (Graph Op)
deps xs = pure $ Vertices  xs

realNoop :: Op
realNoop =
  OpGraph nodeps Actionless

ignoreTrack :: Track' a
ignoreTrack = Track (const realNoop)

noop :: ShortHand -> Op
noop short =
  OpGraph
    nodeps
    ( Actions
      $ Act
          short
          $ Extension
              noHelp
              noNotes
              ref
              skip
              (pure Required)
              skip
              skip
              skip
              noDynamics
     )
  where
    noHelp :: Help
    noHelp = ""

    noDynamics :: [Dynamic]
    noDynamics = []

    noNotes :: [Note]
    noNotes = []

    ref :: Ref
    ref = dotRef short

    skip :: IO ()
    skip = pure ()

op :: ShortHand -> Identity (Graph Op) -> (Extension -> Extension) -> Op
op short pred f =
  -- complicated implementation to say that we apply the modifier on Extension on top of a noop
  let baseOp = (noop short) { predecessors = pred }
      baseNode = node baseOp
  in baseOp { node = fmap f baseNode }

placeholder :: ShortHand -> Text -> Op
placeholder short t = op short nodeps $ \actions -> actions {
    dynamics = [ toDyn $ PlaceHolder t ]
  , ref = dotRef $ short <> t
  }

-- | Function to retrieve the dynamic objects of a given type.
getDynamics :: Typeable a => Op -> [a]
getDynamics o = catMaybes $ fmap fromDynamic $ concatMap dynamics exts
  where
    exts :: [Extension]
    exts = toList o.node -- uses the foldable instance of 'Actions' which is like a Maybe

-- | Collect all ops with a given dynamic type. This can be used to perform analyses on whole graphs.
collectDynamics :: Typeable a => Op -> [(Op, [a])]
collectDynamics root =
  let ops = toList (evalDeps root) in
  [(op, getDynamics op) | op <- ops]

-- Utility to partially apply type in opaque continuation setup in conjuction
-- with UpDown.upTree in defining a `up`.
newtype TrackedIO a = TrackedIO { unwrapTIO :: Tracked' (IO a) }
