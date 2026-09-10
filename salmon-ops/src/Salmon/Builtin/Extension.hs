{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salmon.Builtin.Extension where

import Control.Applicative ((<|>))
import Control.Comonad.Cofree
import Control.Monad.Identity
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode)

import Salmon.Actions.Dot (PlaceHolder (..))
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Op.Actions
import Salmon.Op.Configure
import Salmon.Op.Eval
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref (Ref, mkRef, unRef)
import Salmon.Op.Track

-- | Instanciate actions.
type Actions' = Actions Extension

-- | A short (one-liner) helper string.
type Help = Text

-- | An longer helper string.
type Note = Text

{- | Where a 'managed' action puts a line of its own output: the node's own
bounded ring (@Salmon.Op.Status.statusOutput@), which is also what tells a
watchdog the node is still making progress.
-}
type Output = Text -> IO ()

-- | Our demo extension.
data Extension = Extension
    { help :: Help
    , notes :: [Note]
    , ref :: Ref
    , up :: IO ()
    , -- | The long-running counterpart to 'up', for the one thing @up@
      -- cannot express: an effect that only exists for as long as something
      -- holds it. It blocks while the node is up and returns the reason it
      -- stopped, so the handle never has to escape — the node's own thread
      -- is in scope for the effect's entire lifetime, which is what a
      -- traversal (where @up :: IO ()@ ran and returned into nothing) could
      -- not offer. Teardown is cancelling that thread, so whatever bracket
      -- the action is built from is what does the killing; see
      -- "Salmon.Builtin.Nodes.Process".
      --
      -- 'Nothing' for every node whose effect persists on its own, which is
      -- almost all of them. Two fields rather than a
      -- @OneShot ... | Managed ...@ sum deliberately: the sum is the better
      -- type and would rewrite all 106 @up =@ sites in the tree for a
      -- feature a handful of nodes use. If a third lifecycle ever turns up,
      -- that is the moment to pay for it.
      --
      -- __Only a driver that can hold a running action honours this__ —
      -- "Salmon.Actions.Upkeep", i.e. @run serve@. The one-shot drivers call
      -- 'up', so a node that has no meaningful 'up' should say so by
      -- throwing from it rather than by no-oping.
      managed :: Maybe (Output -> IO ExitCode)
    , -- | "is my effect already in place": the merge of what used to be
      -- @prelim@ and a separate, unimplemented @check@. See 'CheckResult'.
      check :: IO CheckResult
    , down :: IO ()
    , dynamics :: [Dynamic]
    }

instance Show Extension where
    show ext =
        Text.unpack $
            Text.unwords
                [ "["
                , unRef ext.ref
                , ":"
                , ext.help
                , "]"
                ]

instance Semigroup Extension where
    a <> b =
        Extension
            (help a <> "|" <> help b)
            (notes a <> notes b)
            (ref a <> ref b)
            (up a <> up b)
            -- there is no combining two long-running actions: each is the
            -- effect's whole lifetime, and running both would mean one node
            -- owning two processes with one status. First one wins, which
            -- matches the magma's own last-writer-wins in spirit — take one,
            -- do not invent a third thing. Nothing on the execution path
            -- uses this instance.
            (managed a <|> managed b)
            (check a <> check b)
            (down b <> down a)
            (dynamics a <> dynamics b)

type Op = OpGraph Identity Actions'

type Track' a = Track Identity Actions' a

type Tracked' a = Tracked Identity Actions' a

evalDeps :: Op -> Cofree Graph Op
evalDeps = runIdentity . expand

nodeps :: Identity (Graph Op)
nodeps = pure $ Vertices []

deps :: [Op] -> Identity (Graph Op)
deps xs = pure $ Vertices xs

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
                -- nothing to hold: the default node's effect, whatever it
                -- turns out to be, persists without anybody watching it.
                Nothing
                -- a node that says nothing about its own effect is taken
                -- to be saying that asking would cost what applying costs,
                -- which 'requirement' reads as "run up" — the same
                -- behaviour the old @pure Required@ default had, and the
                -- reason the one-shot drivers cannot tell the difference.
                -- Under "Salmon.Actions.Upkeep" they part company: such a
                -- node parks instead of being polled forever for an answer
                -- it has already given.
                (pure Immaterial)
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
    ref = mkRef "noop" short

    skip :: IO ()
    skip = pure ()

op :: ShortHand -> Identity (Graph Op) -> (Extension -> Extension) -> Op
op short pred f =
    -- complicated implementation to say that we apply the modifier on Extension on top of a noop
    let baseOp = (noop short){predecessors = pred}
        baseNode = node baseOp
     in baseOp{node = fmap f baseNode}

placeholder :: ShortHand -> Text -> Op
placeholder short t = op short nodeps $ \actions ->
    actions
        { dynamics = [toDyn $ PlaceHolder t]
        , ref = mkRef short t
        }

-- | Function to retrieve the dynamic objects of a given type.
getDynamics :: (Typeable a) => Op -> [a]
getDynamics o = catMaybes $ fmap fromDynamic $ concatMap dynamics exts
  where
    exts :: [Extension]
    exts = toList o.node -- uses the foldable instance of 'Actions' which is like a Maybe

-- | Collect all ops with a given dynamic type. This can be used to perform analyses on whole graphs.
collectDynamics :: (Typeable a) => Op -> [(Op, [a])]
collectDynamics root =
    let ops = toList (evalDeps root)
     in [(op, getDynamics op) | op <- ops]

-- Utility to partially apply type in opaque continuation setup in conjuction
-- with UpDown.upTree in defining a `up`.
newtype TrackedIO a = TrackedIO {unwrapTIO :: Tracked' (IO a)}

type Act' = Act Extension

opAct :: Op -> Maybe (Act Extension)
opAct x =
    case x.node of
        Actionless -> Nothing
        Actions a -> Just a
