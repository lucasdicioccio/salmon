{- | Layer 0 coverage for 'amendNode': amending one node of an 'Op' leaves
its predecessors as they were, which an 'fmap' over the 'Op' does not.
-}
module Test.AmendNodeSpec (tests) where

import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Salmon.Builtin.Extension (Extension (..), Help, Op, amendNode, deps, evalDeps, nodeps, op, realNoop)
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (mkRef)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Extension.amendNode"
        [ testCase "amends the node and not its predecessor" amendsOnlyTheNode
        , testCase "an fmap over the Op reaches the predecessor (the hazard)" fmapReachesPredecessor
        , testCase "an Actionless node is returned unchanged" actionlessUnchanged
        ]

predecessor :: Op
predecessor = op "pre" nodeps $ \x -> x{help = "pre", ref = mkRef "amend-test" ("pre" :: Text)}

root :: Op
root = op "root" (deps [predecessor]) $ \x -> x{help = "root", ref = mkRef "amend-test" ("root" :: Text)}

-- | Every node's help, the root first, as the evaluator materializes them.
helps :: Op -> [Help]
helps o = [x.help | n <- toList (evalDeps o), x <- toList n.node]

amended :: Extension -> Extension
amended x = x{help = "amended"}

amendsOnlyTheNode :: IO ()
amendsOnlyTheNode = do
    assertEqual "before" ["root", "pre"] (helps root)
    assertEqual "after" ["amended", "pre"] (helps (amendNode amended root))

fmapReachesPredecessor :: IO ()
fmapReachesPredecessor =
    assertEqual "fmap" ["amended", "amended"] (helps (fmap (fmap amended) root))

actionlessUnchanged :: IO ()
actionlessUnchanged =
    assertEqual "actionless" [] (helps (amendNode amended realNoop))
