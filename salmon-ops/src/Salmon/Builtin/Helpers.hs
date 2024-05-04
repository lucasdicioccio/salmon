module Salmon.Builtin.Helpers where

import Control.Comonad.Cofree
import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension
import Salmon.Op.Actions
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Ref

-- | A helper to turn a migration graph into an Op.
collapse ::
    forall a.
    Text ->
    (a -> Op) ->
    Cofree Graph a ->
    Op
collapse opName toOp (m :< x) =
    current `inject` pred
  where
    current, pred :: Op
    current = toOp m
    pred = evalPred [] x
    currentref = case current.node of Actionless -> dotRef ""; (Actions (Act _ x)) -> x.ref
    gorec = collapse opName toOp
    setref lineage actions =
        actions{ref = dotRef $ unRef currentref <> "/" <> Text.concat lineage}

    -- the string in eval pred accumulates left/right branches choices to disambiguate noop nodes by ref
    evalPred :: [Text] -> Graph (Cofree Graph a) -> Op
    evalPred l (Vertices []) = realNoop
    evalPred l (Vertices zs) =
        op opName (deps $ fmap gorec zs) (setref l)
    evalPred l (Overlay g1 g2) =
        op opName (deps [evalPred ("l" : l) g1, evalPred ("r" : l) g2]) (setref l)
    evalPred l (Connect g1 g2) =
        op opName (deps [evalPred ("l" : l) g2 `inject` evalPred ("r" : l) g1]) (setref l)
