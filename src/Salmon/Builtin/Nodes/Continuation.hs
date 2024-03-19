{- | for step processes where we need a special dance
the continuation itself is gonna be opaque
however the continuation setup itself may carry dependencies

it's basically dependency injection where the in-code dependency-injection
setup requires a concrete Operation

the implementation uses a Token that cannot be instanciated, hence forcing
to return a dependency-injected Op.
-}
module Salmon.Builtin.Nodes.Continuation (
    Continue (..),
    withContinuation,
) where

import GHC.TypeLits (Symbol)

import Salmon.Builtin.Extension
import Salmon.Op.OpGraph
import Salmon.Op.Track

data Token (sym :: Symbol) = Token

data Continue (sym :: Symbol) obj
    = Continue
    { tracked :: Track' (Token sym)
    , continue :: obj
    }

withContinuation :: forall a b. Continue a b -> (b -> Op) -> Op
withContinuation (Continue t cont) f =
    f cont `inject` run t (Token @a)
