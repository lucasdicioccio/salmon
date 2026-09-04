{- | Layer 0 coverage for @check :: IO CheckResult@, the field that replaced
@prelim :: IO Requirement@ and the never-implemented @check :: IO ()@
(milestone 1 of @specs/per-node-state-machines.md@).

Two things are worth pinning here. First, that every 'CheckResult'
constructor lands on the right side of the "do I run 'up'" question, since
that mapping is what preserves the behaviour of 22 ported nodes. Second, the
one deliberate behaviour change of the merge: @prelim@ used to be evaluated
outside the @try@ that wraps 'up', so a @prelim@ that threw took the whole
traversal down with it. A 'check' that throws must now fail only its own
node, and that node must still be evaluated.
-}
module Test.CheckSpec (tests) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..), Report (..), Requirement (..), requirement)
import Salmon.Builtin.Extension (Extension (..), Op, deps, nodeps, op, ref, up)
import Salmon.Op.Actions (shorthand)
import Salmon.Op.Ref (mkRef)

import Test.Harness (runUp, runUpCapturing)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Extension.check"
        [ testCase "Success/Skipped/Completed are Skippable, Failure/Unknown are Required" requirementMapping
        , testCase "a satisfied check skips the node, an unsatisfied one evaluates it" checkDecidesEval
        , testCase "a node that implements no check is still evaluated" noCheckMeansEval
        , testCase "a check that throws fails only its own node, not the traversal" throwingCheckIsContained
        ]

requirementMapping :: IO ()
requirementMapping = do
    assertEqual "Success" Skippable (requirement Success)
    assertEqual "Skipped" Skippable (requirement Skipped)
    assertEqual "Completed" Skippable (requirement Completed)
    assertEqual "Failure" Required (requirement (Failure "not there"))
    assertEqual "Unknown" Required (requirement Unknown)

{- | One leaf per 'CheckResult', all under a common root, run in a single
traversal: the three satisfied answers must report 'Skip' and leave 'up'
alone, the two unsatisfied ones must report 'Eval' and run it.
-}
checkDecidesEval :: IO ()
checkDecidesEval = do
    ranRef <- newIORef []
    let rec name = modifyIORef' ranRef (name :)
        leaf name result =
            op name nodeps $ \x ->
                x
                    { ref = mkRef "check-leaf" (name :: Text)
                    , check = pure result
                    , up = rec name
                    }
        leaves =
            [ leaf "ok" Success
            , leaf "forced" Skipped
            , leaf "done" Completed
            , leaf "absent" (Failure "not there")
            , leaf "dunno" Unknown
            ]
        root = op "root" (deps leaves) $ \x -> x{ref = mkRef "check-root" ()}
    reports <- runUpCapturing root
    ran <- readIORef ranRef
    assertEqual
        "only the unsatisfied checks ran their up"
        ["absent", "dunno"]
        (sortNames ran)
    assertEqual
        "the satisfied checks were reported Skip"
        ["done", "forced", "ok"]
        (sortNames [shorthand act | Skip act <- reports])
    assertEqual
        "the unsatisfied checks were reported Eval"
        ["absent", "dunno", "root"]
        (sortNames [shorthand act | Eval act <- reports])

-- | The default 'check' is 'Unknown' ("I have no way to tell"), which has to
-- keep the pre-merge default's behaviour: run 'up'.
noCheckMeansEval :: IO ()
noCheckMeansEval = do
    ranRef <- newIORef []
    let lone = op "lone" nodeps $ \x -> x{ref = mkRef "check-leaf" ("lone" :: Text), up = modifyIORef' ranRef ("lone" :)}
    reports <- runUpCapturing lone
    ran <- readIORef ranRef
    assertEqual "up ran" ["lone"] ran
    assertEqual "reported Eval, not Skip" 1 (length [() | Eval _ <- reports])
    assertEqual "never reported Skip" 0 (length [() | Skip _ <- reports])

{- | @boom@'s check throws. Under the old @prelim@ this escaped 'upTree'
entirely and the sibling never ran. Now the throw is read as "could not
confirm the effect", so @boom@ is evaluated like any other unconfirmed node
and @quiet@ is untouched.
-}
throwingCheckIsContained :: IO ()
throwingCheckIsContained = do
    ranRef <- newIORef []
    let rec name = modifyIORef' ranRef (name :)
        boom =
            op "boom" nodeps $ \x ->
                x
                    { ref = mkRef "check-leaf" ("boom" :: Text)
                    , check = throwIO (ErrorCall "check blew up")
                    , up = rec "boom"
                    }
        quiet =
            op "quiet" nodeps $ \x ->
                x
                    { ref = mkRef "check-leaf" ("quiet" :: Text)
                    , check = pure Success
                    , up = rec "quiet"
                    }
        root = op "root" (deps [boom, quiet]) $ \x -> x{ref = mkRef "check-root" ()}
    ok <- runUp root
    ran <- readIORef ranRef
    assertBool "the traversal reported success: a thrown check is not a failed node" ok
    assertEqual "boom's up ran, quiet's was skipped by its own check" ["boom"] (sortNames ran)

-------------------------------------------------------------------------------

sortNames :: [Text] -> [Text]
sortNames = sort
