{- | Layer 0 coverage for a node's card template (feature 458c16c8,
"Salmon.Op.CardTemplate"): it is opt-in, presentation only, and does not
change what a declaration renders. A node without one is exactly what it
was; a node with one has the same representative, @ref@, @help@ and
@notes@ as without; two declarations differing only by their template
do not conflict.
-}
module Test.CardTemplateSpec (tests) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.QuickCheck (Gen, Property, arbitrary, choose, counterexample, elements, forAll, listOf, vectorOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Builtin.Extension (Extension (..), Op, amendNode, deps, evalDeps, nodeps, op, withCardTemplate)
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.CardTemplate (cardTemplateOf)
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Supervision (Supervision (..), defaultSupervision)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.CardTemplate"
        [ testCase "a node without a template has none" $
            assertEqual "" Nothing (cardTemplateOf (dynamicsOf (leaf "x" [])))
        , testCase "withCardTemplate replaces, the last one wins" $ do
            let o = amendNode (withCardTemplate "b") (amendNode (withCardTemplate "a") (leaf "x" []))
            assertEqual "the template" (Just "b") (cardTemplateOf (dynamicsOf o))
            assertEqual "only one is carried" 1 (length (dynamicsOf o))
        , testCase "amending the root with a template leaves its predecessor alone" $ do
            let pre = leaf "pre" []
                root = op "root" (deps [pre]) $ \x -> x{ref = mkRef "card-test" ("root" :: Text)}
                templates = [cardTemplateOf x.dynamics | n <- toList (evalDeps (amendNode (withCardTemplate "t") root)), x <- toList n.node]
            assertEqual "root first" [Just "t", Nothing] templates
        , testCase "two look-alikes differing only by template in one graph: no conflict, one template kept" $ do
            let a = amendNode (withCardTemplate "first") (leaf "same" ["n"])
                b = amendNode (withCardTemplate "second") (leaf "same" ["n"])
                top = op "top" (deps [a, b]) $ \x -> x{ref = mkRef "card-test" ("top" :: Text)}
                dag = Dag.foldDag Dag.sameRepresentative (evalDeps top)
            assertEqual "no conflict" 0 (length (Dag.dagConflicts dag))
            -- within one fold the look-alike skip keeps the one walked first
            -- (module notes, "Dag"); across declarations under serve the
            -- newest wins, which "Test.ServeHttpSpec" checks
            let kept = cardTemplateOf . (.extension.dynamics) =<< Map.lookup (refOf "same") (Dag.dagNodes dag)
            assertBool ("one of the two: " <> show kept) (kept `elem` [Just "first", Just "second"])
        , testProperty "a template changes neither the representative nor ref, help, notes" prop_presentationOnly
        , testProperty "other dynamics are kept, in order, and still compared" prop_otherDynamicsKept
        ]

refOf :: Text -> Ref
refOf k = mkRef "card-test" k

leaf :: Text -> [Text] -> Op
leaf k ns = op "leaf" nodeps $ \x -> x{ref = refOf k, help = "help " <> k, notes = ns}

dynamicsOf :: Op -> [Dynamic]
dynamicsOf o = concat [x.dynamics | x <- toList o.node]

actOf :: Op -> [Act Extension]
actOf o = case o.node of
    Actions a -> [a]
    Actionless -> []

genText :: Gen Text
genText = Text.pack <$> listOf (elements "ab {}()$.\"\\\n<>&é")

prop_presentationOnly :: Property
prop_presentationOnly =
    forAll genText $ \k ->
        forAll (listOf genText) $ \ns ->
            forAll genText $ \src ->
                let plain = leaf k ns
                    carded = amendNode (withCardTemplate src) plain
                 in counterexample (show (src, k, ns)) $
                        (fmap Dag.representative (actOf carded), fmap describe (actOf carded), cardTemplateOf (dynamicsOf carded))
                            === (fmap Dag.representative (actOf plain), fmap describe (actOf plain), Just src)
  where
    describe :: Act Extension -> (Text, Ref, Text, [Text])
    describe a =(a.shorthand, a.extension.ref, a.extension.help, a.extension.notes)

prop_otherDynamicsKept :: Property
prop_otherDynamicsKept =
    forAll (choose (0, 3 :: Int)) $ \n ->
        forAll (vectorOf n arbitrary) $ \reapplies ->
            forAll genText $ \src ->
                let sups = [defaultSupervision{supReapply = r} | r <- reapplies]
                    plain = amendNode (\x -> x{dynamics = fmap toDyn sups}) (leaf "d" [])
                    carded = amendNode (withCardTemplate src) plain
                 in fmap Dag.representative (actOf carded) === fmap Dag.representative (actOf plain)
