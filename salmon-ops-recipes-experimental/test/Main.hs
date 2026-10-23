module Main (main) where

import System.Environment (lookupEnv, setEnv)
import Test.Tasty (defaultMain, testGroup)

import qualified Test.GeneratedSiteSpec as GeneratedSiteSpec
import qualified Test.NetbirdControlPlaneSpec as NetbirdControlPlaneSpec
import qualified Test.PgTurretSpec as PgTurretSpec

{- | The suite runs its tests one at a time unless told otherwise
(@TASTY_NUM_THREADS@, or @-j@ on the command line, which wins).

It is a fraction of a second of Layer 0 either way, and in parallel it hung:
with "Test.NetbirdControlPlaneSpec" in it, between one run in six and one in
two never finished under GHC 9.10.3, every test thread
@BlockedOnBlackHole@. The threads were each folding one shared
'Salmon.Builtin.Extension.Op' ('Salmon.Op.Dag.foldDag'); the one that owned
a node's @ref@ thunk was blocked in @mkRef@'s @printf@ on a black hole whose
owner was itself waiting for that @ref@. That is the shape of GHC's #26324
(lost wakeups in @threadPaused@ for threads blocked on black holes, fixed on
GHC's master in September 2025), which this compiler is assumed, not shown,
to have; nothing in the tests' own code takes a lock. One test at a time,
the same binary did not hang in the runs made.
-}
main :: IO ()
main = do
    threads <- lookupEnv "TASTY_NUM_THREADS"
    case threads of
        Nothing -> setEnv "TASTY_NUM_THREADS" "1"
        Just _ -> pure ()
    defaultMain (testGroup "salmon-ops-recipes-experimental" [PgTurretSpec.tests, NetbirdControlPlaneSpec.tests, GeneratedSiteSpec.tests])
