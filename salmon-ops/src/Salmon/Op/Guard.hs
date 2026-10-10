{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | What a node that cannot be undone says about itself: the marker a driver
reads, and the two values its preconditions speak in.

The pattern itself (what goes in @check@, what goes in @up@, which
'Salmon.Op.Supervision.Supervision' it gets) is assembled by
"Salmon.Builtin.Guarded". This module is only the part the drivers and
"Salmon.Op.Dag" have to see, kept free of
'Salmon.Builtin.Extension.Extension' so that they can.

It rides @dynamics@ for the reason "Salmon.Op.Supervision" gives: nothing
changes for the many nodes with no opinion.
-}
module Salmon.Op.Guard (
    -- * Preconditions
    Precondition (..),
    PreconditionUnmet (..),

    -- * The marker
    Guard (..),
    guardOf,
    isGuarded,
) where

import Control.Exception (Exception (..))
import Data.Dynamic (Dynamic, fromDynamic)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Records (HasField, getField)

import Salmon.Op.Ref (Ref)

{- | Whether a risky operation may start now. The text is public (it ends up
in reports), so it says which condition is unmet and never quotes a secret.
-}
data Precondition
    = Met
    | Unmet !Text
    deriving (Show, Eq, Ord)

{- | What a guarded node's @up@ throws when its preconditions are unmet at the
moment it is asked to act.

A type of its own so that a driver can tell "refused to start" from "started
and failed": the first is not a failure of the operation and must not count
toward giving up on it. See "Salmon.Actions.Upkeep".
-}
newtype PreconditionUnmet = PreconditionUnmet Text
    deriving (Show, Eq)

instance Exception PreconditionUnmet where
    displayException (PreconditionUnmet why) =
        "guarded operation not started, precondition unmet: " <> Text.unpack why

{- | The marker a guarded node carries.

'guardHolds' is the hold set: the nodes that must stand still while this
one's @up@ runs. It is explicit, and not read off the dependency graph, on
purpose. An edge already keeps a /dependant/ from being brought up before
this node is, but it says nothing to a node that is already up: that node's
machine keeps checking its effect and puts it back when it goes away, which
is exactly the interference a restart or an upgrade of its neighbour causes.
And the nodes to hold are usually not dependants at all (the other members
of a cluster).

Rendered by value in "Salmon.Op.Dag" (@showDynamic@), so a re-declaration
that only changes the hold set is a changed representative.
-}
newtype Guard = Guard
    { guardHolds :: [Ref]
    }
    deriving (Show, Eq)

-- | The first 'Guard' a node declared, if any.
guardOf :: (HasField "dynamics" ext [Dynamic]) => ext -> Maybe Guard
guardOf ext = listToMaybe (mapMaybe cast (getField @"dynamics" ext))
  where
    cast :: Dynamic -> Maybe Guard
    cast = fromDynamic

isGuarded :: (HasField "dynamics" ext [Dynamic]) => ext -> Bool
isGuarded = isJust . guardOf
