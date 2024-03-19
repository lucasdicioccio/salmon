module Salmon.Builtin.Nodes.Demo where

import Salmon.Actions.Dot
import Salmon.Builtin.Extension
import Salmon.Op.Ref

import Data.Text as Text

collatz :: [Int] -> Op
collatz ks =
    op "collatzs-orbits" (deps [segment k | k <- ks]) id
  where
    opname k = "cltz-" <> (Text.pack $ show k)
    useRef k = \actions -> actions{ref = dotRef $ opname k}
    depsAtLevel 1 = nodeps
    depsAtLevel n
        | n `mod` 2 == 0 = deps [segment (n `div` 2)]
        | otherwise = deps [segment (3 * n + 1)]
    segment k = op (opname k) (depsAtLevel k) (useRef k)
