{- | 'Salmon.Client.Model.step' removes a node by lookup, and shows what it
showed when it filtered a list.

The model's display order used to be a list, and dropping a node (one
wanted @down@ whose @done@ arrives) filtered all of it: a teardown, one
such event per node, was quadratic (finding 5 of
@resources\/graph-profiles.md@). The order is now 'Model.Order'. What must
not have moved is anything a client can see, so the bar here is
observable equivalence with the fold as it was, which is kept, untouched,
as "Test.ClientModelReference":

  * __QuickCheck__, over generated scripts ('Script': a snapshot, then
    events and re-read snapshots): after every step the two folds show the
    same nodes in the same order with the same fields, and agree on the
    loop's part; the order is the last snapshot's with exactly the dropped
    nodes missing, holds no node the model does not have, and repeats none;
    a teardown in any order agrees step by step; and 'Model.Order' on its
    own is the list operations it replaces. A further property checks that
    the scripts do reach what they are for (removals, a removed node read
    again, events about a node that is not there).
  * __Unit cases__ for the positions a removal can be in (first, middle,
    last, absent, all of them), for a removed node coming back with a
    re-read snapshot, and for a snapshot that lists a ref twice.
  * __One scale case__: a teardown of a hundred thousand nodes within a
    minute, in the manner of "Test.GraphScaleSpec" (it tells per node from
    per node squared; it times nothing).

All of it is Layer 0: pure, in-process. __Not covered:__ the events are
generated from the kinds the fold names plus one it does not, not recorded
from a server; the scripts use a pool of eight refs and a ghost, so they
say nothing about size (the scale case does, for a teardown only); and
'Model.fromDag' is only given well-formed snapshots, apart from repeated
refs. The shared fixture @rs\/fixtures\/client-model.json@ is held to this
fold by "Test.ClientModelSpec", unchanged.
-}
module Test.ClientModelStepSpec (tests) where

import Control.Exception (evaluate)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.Timeout (timeout)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    QuickCheckTests (..),
    checkCoverage,
    choose,
    conjoin,
    counterexample,
    cover,
    elements,
    frequency,
    listOf,
    oneof,
    property,
    shrinkList,
    shuffle,
    sized,
    sublistOf,
    testProperty,
    (===),
 )

import qualified Salmon.Client.Model as Model
import Salmon.Client.Model (RefId (..))

import qualified Test.ClientModelReference as Reference

tests :: TestTree
tests =
    testGroup
        "Salmon.Client.Model.step: removal without filtering the order"
        [ testGroup
            "unit"
            [ testCase "removing the first, a middle and the last node keeps the others in place" removesEachPosition
            , testCase "a done about a node that is absent changes neither the nodes nor the order" absentNodeChangesNothing
            , testCase "a full teardown empties the model, first to last and last to first" fullTeardown
            , testCase "a removed node is shown again, in the snapshot's place, once a re-read snapshot has it" removedThenReread
            , testCase "a ref a snapshot lists twice is shown twice and removed from both places" repeatedRef
            , testCase "an Order reads as its list, and equality is what is read" orderUnit
            ]
        , localOption (QuickCheckTests cases) $
            testGroup
                "properties"
                [ testProperty "step shows what the reference step shows, after every event of a script" prop_stepMatchesReference
                , testProperty "the order is the last snapshot's less the dropped nodes: no removed node, no duplicate" prop_orderInvariant
                , testProperty "a teardown in any order agrees with the reference at every step and ends empty" prop_teardownInAnyOrder
                , testProperty "Order: toList . fromList is the list" prop_orderRoundTrips
                , testProperty "Order: removals are the filters they replace" prop_orderRemoveIsFilter
                , testProperty "Order: two orders are equal exactly when they read the same" prop_orderEquality
                ]
        , testProperty "the generated scripts reach removals, re-read removed nodes and absent nodes" prop_scriptsReachTheCases
        , testCase "a teardown of a hundred thousand nodes folds within a minute" teardownAtScale
        ]

-- | Cases per property (QuickCheck's default is a hundred): the folds are
-- pure and a script is a hundred-odd steps at most, so this stays at a few
-- seconds each (the comparison shows every node at every step).
cases :: Int
cases = 1000

-------------------------------------------------------------------------------
-- wire objects

refOf :: Text -> RefId
refOf short = RefId short ("full-" <> short)

refValue :: Text -> Value
refValue short = object ["short" .= short, "full" .= ("full-" <> short)]

nodeValue :: Text -> Text -> Text -> Value
nodeValue direction convergence short =
    object
        [ "ref" .= refValue short
        , "shorthand" .= ("node " <> short)
        , "help" .= ("" :: Text)
        , "notes" .= ([] :: [Text])
        , "direction" .= direction
        , "convergence" .= convergence
        , "status" .= Null
        , "paths" .= (["/" <> short] :: [Text])
        , "dynamics" .= ([] :: [Text])
        , "dependencies" .= ([] :: [Value])
        , "dependants" .= ([] :: [Value])
        ]

snapshotValue :: Word64 -> [Value] -> Value
snapshotValue seqNo nodes = object ["mode" .= ("interactive" :: Text), "seq" .= seqNo, "nodes" .= nodes]

-- | A snapshot of nodes all wanted down, in this order.
retiring :: Word64 -> [Text] -> Value
retiring seqNo shorts = snapshotValue seqNo (fmap (nodeValue "down" "pending") shorts)

updown :: Text -> Text -> Word64 -> Value
updown kind short seqNo =
    object ["stream" .= ("updown" :: Text), "kind" .= kind, "seq" .= seqNo, "ref" .= refValue short]

-------------------------------------------------------------------------------
-- the two folds, side by side

-- | One step of a script: an event off the stream, or a snapshot re-read
-- and joined to the model that has been folding ('Model.rebase').
data Action
    = Fold Value
    | Reread Value
    deriving (Show)

-- | The first snapshot, then what follows it.
data Script = Script Value [Action]
    deriving (Show)

startNew :: Value -> Model.Model
startNew = either (error . ("fromDag: " <>)) id . Model.fromDag

startOld :: Value -> Reference.Model
startOld = either (error . ("reference fromDag: " <>)) id . Reference.fromDag

applyNew :: Model.Model -> Action -> Model.Model
applyNew m (Fold v) = Model.step m (Model.eventOf v)
applyNew m (Reread s) = Model.rebase m (startNew s)

applyOld :: Reference.Model -> Action -> Reference.Model
applyOld m (Fold v) = Reference.step m (Reference.eventOf v)
applyOld m (Reread s) = Reference.rebase m (startOld s)

-- | Every model a script goes through, the first snapshot's included.
runNew :: Script -> [Model.Model]
runNew (Script s actions) = scanl applyNew (startNew s) actions

runOld :: Script -> [Reference.Model]
runOld (Script s actions) = scanl applyOld (startOld s) actions

{- | Everything a client reads off a model. The reference has types of its
own, with the constructors and field names of the real ones, so a node, a
pass and an event are compared by what they 'show': every field of every
node is in there.
-}
data Seen = Seen
    { seenInOrder :: [String]
    -- ^ the nodes as shown, first to last ('nodesInOrder')
    , seenOrder :: [(Text, Text)]
    -- ^ the order itself, which may name what the node map does not
    , seenNodes :: [(Text, Text)]
    -- ^ the refs of the node map (the nodes themselves are in 'seenInOrder')
    , seenCounts :: (Int, Int, Int)
    , seenMode :: Text
    , seenSeq :: Word64
    , seenLoopSeq :: Word64
    , seenPass :: String
    , seenSupervised :: Maybe Bool
    , seenLast :: String
    , seenResync :: Maybe Text
    }
    deriving (Eq, Show)

seenNew :: Model.Model -> Seen
seenNew m =
    Seen
        { seenInOrder = fmap show (Model.nodesInOrder m)
        , seenOrder = [(r.refShort, r.refFull) | r <- Model.orderToList m.modelOrder]
        , seenNodes = [(r.refShort, r.refFull) | r <- Map.keys m.modelNodes]
        , seenCounts = let c = Model.counts m in (c.countConverged, c.countErrored, c.countTotal)
        , seenMode = m.modelMode
        , seenSeq = m.modelSeq
        , seenLoopSeq = m.modelLoopSeq
        , seenPass = show m.modelPass
        , seenSupervised = m.modelSupervised
        , seenLast = show m.modelLast
        , seenResync = Model.modelResync m
        }

seenOld :: Reference.Model -> Seen
seenOld m =
    Seen
        { seenInOrder = fmap show (Reference.nodesInOrder m)
        , seenOrder = [(r.refShort, r.refFull) | r <- m.modelOrder]
        , seenNodes = [(r.refShort, r.refFull) | r <- Map.keys m.modelNodes]
        , seenCounts = let c = Reference.counts m in (c.countConverged, c.countErrored, c.countTotal)
        , seenMode = m.modelMode
        , seenSeq = m.modelSeq
        , seenLoopSeq = m.modelLoopSeq
        , seenPass = show m.modelPass
        , seenSupervised = m.modelSupervised
        , seenLast = show m.modelLast
        , seenResync = m.modelResyncReason
        }

-------------------------------------------------------------------------------
-- generators

-- | Few enough refs that a script comes back to the same node many times:
-- updated, removed, removed again, read again from a later snapshot.
pool :: [Text]
pool = ["n" <> Text.pack (show i) | i <- [0 .. 7 :: Int]]

-- | A ref no snapshot has.
ghost :: Text
ghost = "ghost"

genShort :: Gen Text
genShort = frequency [(12, elements pool), (1, pure ghost)]

{- | A snapshot at this number: mostly a shuffled part of the pool, each
ref once, as @\/dag@ answers; one time in ten any list over the pool, so
that refs repeat (not something a server sends, but the fold is total over
it and removal has to stay what it was there too). Three nodes in four are
wanted down, since those are the ones a @done@ removes.
-}
genSnapshot :: Word64 -> Gen Value
genSnapshot seqNo = do
    shorts <- frequency [(9, sublistOf pool >>= shuffle), (1, listOf (elements pool))]
    snapshotValue seqNo <$> traverse genNode shorts
  where
    genNode short = do
        direction <- frequency [(3, pure "down"), (1, pure "up")]
        convergence <- elements ["pending", "stale", "converged", "errored", "blocked"]
        pure (nodeValue direction convergence short)

{- | One event, numbered or not. Every stream and kind the fold names, a
kind it does not (@novel@), an event with no ref, and a shapeless object;
weighted towards @done@, the one that removes.
-}
genEvent :: Maybe Word64 -> Gen Value
genEvent seqNo =
    numbered
        <$> frequency
            [ (10, genUpdown >>= \(kind, rest) -> about "updown" kind rest <$> genShort)
            , (3, acted <$> genUpdown <*> genShort)
            , (2, about "upkeep" "next-look" <$> ((: []) . ("check" .=) <$> genCheck) <*> genShort)
            , (1, about "upkeep" <$> elements ["parked", "hung-up", "novel"] <*> pure [] <*> genShort)
            , (2, about "output" "output" <$> genLine <*> genShort)
            , (2, about "output" "log" <$> ((<>) <$> genLine <*> genChannel) <*> genShort)
            , (3, genServe)
            , (1, (\(n :: Int) -> ["stream" .= ("server" :: Text), "kind" .= ("gap" :: Text), "from" .= n]) <$> choose (0, 50))
            , (1, pure ["stream" .= ("updown" :: Text), "kind" .= ("done" :: Text)])
            , (1, pure [])
            ]
  where
    numbered :: [(Key.Key, Value)] -> Value
    numbered fields = object (maybe fields (\s -> ("seq" .= s) : fields) seqNo)

    about :: Text -> Text -> [(Key.Key, Value)] -> Text -> [(Key.Key, Value)]
    about stream kind rest short = ["stream" .= stream, "kind" .= kind, "ref" .= refValue short] ++ rest

    -- what a tending machine did, in the pass's vocabulary: the inner
    -- object carries the kind and the ref
    acted :: (Text, [(Key.Key, Value)]) -> Text -> [(Key.Key, Value)]
    acted (kind, rest) short =
        ["stream" .= ("upkeep" :: Text), "kind" .= ("acted" :: Text), "report" .= object (about "updown" kind rest short)]

    genUpdown :: Gen (Text, [(Key.Key, Value)])
    genUpdown =
        frequency
            [ (7, pure ("done", []))
            , (1, pure ("skip", []))
            , (1, pure ("failed", ["error" .= ("boom" :: Text)]))
            , (1, pure ("blocked", []))
            , (1, pure ("eval", []))
            , (1, pure ("novel", []))
            ]

    genCheck :: Gen Value
    genCheck = do
        verdict <- elements ["success", "failure", "unknown" :: Text]
        reason <- elements [[], ["reason" .= ("because" :: Text)]]
        pure (object (("verdict" .= verdict) : reason))

    genLine :: Gen [(Key.Key, Value)]
    genLine = oneof [pure [], (\(n :: Int) -> ["line" .= ("line " <> Text.pack (show n))]) <$> choose (0, 9)]

    genChannel :: Gen [(Key.Key, Value)]
    genChannel = elements [[], ["channel" .= ("stderr" :: Text)], ["channel" .= ("message" :: Text)]]

    genServe :: Gen [(Key.Key, Value)]
    genServe = do
        (a :: Int) <- choose (0, 9)
        (b :: Int) <- choose (0, 9)
        on <- elements [True, False]
        (("stream" .= ("serve" :: Text)) :)
            <$> elements
                [ ["kind" .= ("declared" :: Text), "epoch" .= a, "direction" .= ("down" :: Text)]
                , ["kind" .= ("cleared" :: Text)]
                , ["kind" .= ("converge-start" :: Text), "down" .= a, "up" .= b]
                , ["kind" .= ("converge-stop" :: Text), "ok" .= on, "remaining" .= a]
                , ["kind" .= ("supervised" :: Text), "on" .= on]
                , ["kind" .= ("novel" :: Text)]
                ]

{- | What follows a snapshot at @cursor@: mostly events numbered just past
the last (as the stream numbers them), some numbered at or below it (a
replay, which each part of the model drops by its own stamp), a few with
no number; and now and then a re-read snapshot, at the cursor or behind
it, which is how a node the fold removed comes back.
-}
genActions :: Word64 -> Int -> Gen [Action]
genActions _ 0 = pure []
genActions cursor k = do
    (action, cursor') <- frequency [(12, event), (1, reread)]
    (action :) <$> genActions cursor' (k - 1)
  where
    event = do
        seqNo <-
            frequency
                [ (8, Just . (cursor +) <$> choose (1, 2))
                , (2, Just <$> choose (0, cursor))
                , (1, pure Nothing)
                ]
        v <- genEvent seqNo
        pure (Fold v, maybe cursor (max cursor) seqNo)
    reread = do
        seqNo <- frequency [(3, pure cursor), (1, choose (0, cursor))]
        v <- genSnapshot seqNo
        pure (Reread v, cursor)

instance Arbitrary Script where
    arbitrary = sized $ \size -> do
        seqNo <- choose (0, 5)
        first <- genSnapshot seqNo
        k <- choose (0, 10 + size)
        Script first <$> genActions seqNo k

    -- drop steps; the events themselves are not shrunk
    shrink (Script first actions) = Script first <$> shrinkList (const []) actions

-- | A ref from the pool or the ghost, for the properties of 'Model.Order' alone.
newtype SmallRef = SmallRef RefId
    deriving (Show)

instance Arbitrary SmallRef where
    arbitrary = SmallRef . refOf <$> genShort

unsmall :: [SmallRef] -> [RefId]
unsmall xs = [r | SmallRef r <- xs]

-------------------------------------------------------------------------------
-- properties

-- | The bar: whatever the script, the model shows after each step what the
-- fold that filtered a list shows.
prop_stepMatchesReference :: Script -> Property
prop_stepMatchesReference script =
    conjoin
        [ counterexample ("after " <> show k <> " steps") (seenNew new === seenOld old)
        | (k, new, old) <- zip3 [0 :: Int ..] (runNew script) (runOld script)
        ]

{- | Without the reference: at every step the order is the refs of the
last snapshot read, in its order, less exactly the nodes the model no
longer has. So it never holds a removed node, it names every node the
model has, and it repeats a ref only if the snapshot did.
-}
prop_orderInvariant :: Script -> Property
prop_orderInvariant (Script first actions) =
    conjoin
        [ counterexample ("after " <> show k <> " steps") $
            conjoin
                [ counterexample "the order is the snapshot's less the dropped" (order === filter (`Map.member` m.modelNodes) base)
                , counterexample "every node is in the order" (Set.fromList order === Map.keysSet m.modelNodes)
                , counterexample "no duplicate the snapshot did not have" (property (nub base /= base || nub order == order))
                , counterexample "the size is the list's" (Model.orderSize m.modelOrder === length order)
                ]
        | (k, (base, m)) <- zip [0 :: Int ..] states
        , let order = Model.orderToList m.modelOrder
        ]
  where
    states = scanl advance (listed first, startNew first) actions
    advance (base, m) action = case action of
        Fold _ -> (base, applyNew m action)
        Reread s -> (listed s, applyNew m action)
    -- the refs a snapshot lists, in its order
    listed = Model.orderToList . Model.modelOrder . startNew

{- | A teardown: every node wanted down, one @done@ each, in an order of
its own (a pass brings nodes down in dependency order, tending machines in
any). Up to two hundred nodes; the order and the nodes shown are
compared after every removal, everything else at the end.
-}
data Teardown = Teardown [Text] [Text]
    deriving (Show)

instance Arbitrary Teardown where
    arbitrary = sized $ \size -> do
        n <- choose (0, 2 * size)
        let shorts = ["t" <> Text.pack (show i) | i <- [1 .. n]]
        Teardown shorts <$> shuffle shorts
    shrink (Teardown shorts gone) =
        [Teardown (filter (/= s) shorts) (filter (/= s) gone) | s <- shorts]

prop_teardownInAnyOrder :: Teardown -> Property
prop_teardownInAnyOrder (Teardown shorts gone) =
    conjoin
        [ conjoin
            [ counterexample ("after " <> show k <> " removals") $
                conjoin
                    [ Model.orderToList new.modelOrder === [refOf short | Reference.RefId short _ <- old.modelOrder]
                    , shown new === [n.nodeRef.refShort | n <- Reference.nodesInOrder old]
                    ]
            | (k, new, old) <- zip3 [0 :: Int ..] news olds
            ]
        , counterexample "the same model at the end" (seenNew (last news) === seenOld (last olds))
        , counterexample "nothing is left" (Model.orderToList (last news).modelOrder === [])
        , counterexample "no node is left" (Map.size (last news).modelNodes === 0)
        ]
  where
    script = Script (retiring 0 shorts) [Fold (updown "done" s i) | (i, s) <- zip [1 ..] gone]
    news = runNew script
    olds = runOld script

prop_orderRoundTrips :: [SmallRef] -> Property
prop_orderRoundTrips xs =
    conjoin
        [ Model.orderToList (Model.orderFromList refs) === refs
        , Model.orderSize (Model.orderFromList refs) === length refs
        ]
  where
    refs = unsmall xs

-- | Any run of removals (of refs shown once, shown twice, already removed
-- or never there) reads as the same run of filters over the list.
prop_orderRemoveIsFilter :: [SmallRef] -> [SmallRef] -> Property
prop_orderRemoveIsFilter xs gone =
    fmap Model.orderToList (scanl (flip Model.orderRemove) (Model.orderFromList refs) (unsmall gone))
        === scanl (\rs r -> filter (/= r) rs) refs (unsmall gone)
  where
    refs = unsmall xs

-- | Equality sees the reading and not the seats: an order with a ref
-- removed is the order built without it.
prop_orderEquality :: [SmallRef] -> [SmallRef] -> SmallRef -> Property
prop_orderEquality xs ys (SmallRef r) =
    conjoin
        [ (Model.orderFromList a == Model.orderFromList b) === (a == b)
        , Model.orderRemove r (Model.orderFromList a) === Model.orderFromList (filter (/= r) a)
        ]
  where
    a = unsmall xs
    b = unsmall ys

{- | The scripts are only worth the equivalence they are run for if they
get to the cases: a node removed by the fold, a removed node shown again
after a re-read, and a @done@ about a node the model does not have (the
ghost, or one already removed). 'checkCoverage' runs as many cases as it
takes to be sure of the shares below.
-}
prop_scriptsReachTheCases :: Script -> Property
prop_scriptsReachTheCases script@(Script _ actions) =
    checkCoverage $
        cover 50 (removals > 0) "a node was removed" $
            cover 10 (removals > 3) "more than three nodes were removed" $
                cover 25 cameBack "a removed node was read again" $
                    cover 50 strangers "a done about a node that is not there" $
                        property True
  where
    states = runNew script
    steps = zip3 states actions (drop 1 states)
    keys :: Model.Model -> Set.Set RefId
    keys = Map.keysSet . Model.modelNodes
    dropped = [keys before `Set.difference` keys after | (before, Fold _, after) <- steps]
    removals = sum (fmap Set.size dropped)
    strangers =
        or
            [ maybe False (`Set.notMember` keys before) e.eventRef
            | (before, Fold v, _) <- steps
            , let e = Model.eventOf v
            , e.eventStream == "updown" && e.eventKind == "done"
            ]
    -- (refs the fold has removed and no snapshot has brought back, seen one come back)
    cameBack = snd (foldl' track (Set.empty, False) steps)
    track (gone, seen) (before, action, after) = case action of
        Fold _ -> (gone `Set.union` (keys before `Set.difference` keys after), seen)
        Reread _ -> (gone `Set.difference` keys after, seen || not (Set.null (gone `Set.intersection` keys after)))

-------------------------------------------------------------------------------
-- unit cases

shown :: Model.Model -> [Text]
shown = fmap (refShort . Model.nodeRef) . Model.nodesInOrder

foldNew :: Model.Model -> [Value] -> Model.Model
foldNew = foldl' (\m v -> Model.step m (Model.eventOf v))

four :: Model.Model
four = startNew (retiring 0 ["a", "b", "c", "d"])

removesEachPosition :: IO ()
removesEachPosition = do
    assertEqual "the snapshot's order" ["a", "b", "c", "d"] (shown four)
    let without short = foldNew four [updown "done" short 1]
    assertEqual "the first" ["b", "c", "d"] (shown (without "a"))
    assertEqual "a middle one" ["a", "c", "d"] (shown (without "b"))
    assertEqual "the other middle one" ["a", "b", "d"] (shown (without "c"))
    assertEqual "the last" ["a", "b", "c"] (shown (without "d"))
    let m = without "b"
    assertEqual "the order is what is shown" [refOf "a", refOf "c", refOf "d"] (Model.orderToList m.modelOrder)
    assertEqual "and its size" 3 (Model.orderSize m.modelOrder)
    assertEqual "the node is gone from the map too" Nothing (Model.lookupNode (refOf "b") m)
    assertEqual "three counted" 3 (Model.countTotal (Model.counts m))
    -- one after another, the rest keep their places
    assertEqual "two removed" ["a", "d"] (shown (foldNew four [updown "done" "c" 1, updown "done" "b" 2]))
    -- an eval does not remove, nor a failed
    assertEqual "only a done removes" ["a", "b", "c", "d"] (shown (foldNew four [updown "eval" "b" 1, updown "failed" "c" 2]))

absentNodeChangesNothing :: IO ()
absentNodeChangesNothing = do
    let m = foldNew four [updown "done" "ghost" 1]
    assertEqual "the order" (Model.orderToList four.modelOrder) (Model.orderToList m.modelOrder)
    assertEqual "the nodes" four.modelNodes m.modelNodes
    assertEqual "the cursor still moves" 1 m.modelSeq
    -- and a node already removed is such a node: a second done, numbered
    -- later, finds nothing to remove
    let once = foldNew four [updown "done" "b" 1]
        twice = foldNew once [updown "done" "b" 2]
    assertEqual "removed once" ["a", "c", "d"] (shown twice)
    assertEqual "the nodes are as after the first" once.modelNodes twice.modelNodes
    assertEqual "the order too" once.modelOrder twice.modelOrder

fullTeardown :: IO ()
fullTeardown = do
    let shorts = ["a", "b", "c", "d"]
        forwards = scanl (\m (i, s) -> foldNew m [updown "done" s i]) four (zip [1 ..] shorts)
        backwards = scanl (\m (i, s) -> foldNew m [updown "done" s i]) four (zip [1 ..] (reverse shorts))
    assertEqual "first to last" [["a", "b", "c", "d"], ["b", "c", "d"], ["c", "d"], ["d"], []] (fmap shown forwards)
    assertEqual "last to first" [["a", "b", "c", "d"], ["a", "b", "c"], ["a", "b"], ["a"], []] (fmap shown backwards)
    let emptied = last forwards
    assertEqual "no order left" [] (Model.orderToList emptied.modelOrder)
    assertEqual "no node left" 0 (Map.size emptied.modelNodes)
    assertEqual "nothing counted" (Model.Counts 0 0 0) (Model.counts emptied)
    assertEqual "the cursor is the last done's" 4 emptied.modelSeq
    assertBool "the last event is kept" (isJust emptied.modelLast)

removedThenReread :: IO ()
removedThenReread = do
    let m = foldNew four [updown "done" "b" 1, updown "done" "d" 2]
    assertEqual "two gone" ["a", "c"] (shown m)
    -- the server still has b (declared again, say): the re-read snapshot
    -- lists it where the server orders it, and that is where it shows
    let again = Model.rebase m (startNew (retiring 5 ["c", "b", "a"]))
    assertEqual "b is back, in the fresh snapshot's order" ["c", "b", "a"] (shown again)
    -- and it can be removed again, by an event past the snapshot
    assertEqual "a replay of its old done is dropped" ["c", "b", "a"] (shown (foldNew again [updown "done" "b" 1]))
    assertEqual "a new done removes it" ["c", "a"] (shown (foldNew again [updown "done" "b" 6]))

repeatedRef :: IO ()
repeatedRef = do
    let m = startNew (retiring 0 ["a", "b", "a", "c"])
    assertEqual "shown twice" ["a", "b", "a", "c"] (shown m)
    assertEqual "one node for the two" 3 (Map.size m.modelNodes)
    assertEqual "removed from both places" ["b", "c"] (shown (foldNew m [updown "done" "a" 1]))
    assertEqual "the others keep theirs" ["a", "a", "c"] (shown (foldNew m [updown "done" "b" 1]))

orderUnit :: IO ()
orderUnit = do
    let refs = fmap refOf ["a", "b", "c"]
        order = Model.orderFromList refs
    assertEqual "reads as its list" refs (Model.orderToList order)
    assertEqual "size" 3 (Model.orderSize order)
    assertEqual "the empty order" [] (Model.orderToList (Model.orderFromList []))
    assertEqual "removing from the empty order" (Model.orderFromList []) (Model.orderRemove (refOf "a") (Model.orderFromList []))
    assertEqual "a removal equals the order built without the ref" (Model.orderFromList (fmap refOf ["a", "c"])) (Model.orderRemove (refOf "b") order)
    assertBool "the order of the refs matters" (Model.orderFromList (reverse refs) /= order)
    assertEqual "shown as the list it reads as" "orderFromList [RefId {refShort = \"a\", refFull = \"full-a\"}]" (show (Model.orderFromList [refOf "a"]))

-------------------------------------------------------------------------------
-- scale

{- | A hundred thousand nodes wanted down and a @done@ for each, in the
snapshot's order: the benchmark's teardown series, without the graph. The
fold that filtered the order took 1.5 to 2 seconds for ten thousand and
grew with the square (so minutes here); this one is expected well under a
second. The bound is as loose as "Test.GraphScaleSpec"'s, for the same
reason: the suite shares its process and its machine.
-}
teardownAtScale :: IO ()
teardownAtScale = do
    let size = 100000 :: Int
        shorts = ["s" <> Text.pack (show i) | i <- [1 .. size]]
        events = [Model.Event (Just i) "updown" "done" (Just (refOf s)) Nothing Null | (i, s) <- zip [1 ..] shorts]
    m0 <- evaluate (startNew (retiring 0 shorts))
    assertEqual "every node is in the snapshot" size (Model.orderSize m0.modelOrder)
    outcome <- timeout (60 * 1000000) (evaluate (foldl' Model.step m0 events))
    case outcome of
        Nothing -> assertFailure "still folding after 60s, which is what a pass over the order per removal looks like"
        Just m -> do
            assertEqual "nothing left in the order" 0 (Model.orderSize m.modelOrder)
            assertEqual "nor in the node map" 0 (Map.size m.modelNodes)
            assertEqual "the cursor is the last event's" (fromIntegral size) m.modelSeq
