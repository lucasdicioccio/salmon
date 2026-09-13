{-# LANGUAGE DeriveGeneric #-}

{- | Property-based coverage for "Salmon.Actions.Serve", per
@specs/serve-property-testing.md@: generate a random sequence of
declare\/converge commands, fold it through a small independent shadow model
of "which seeds are live, which nodes are up", and check the real
'Serve.serveWith' loop (driven in piped-script mode, exactly as
'Test.ServeSpec' drives it) agrees with the model both in its final
'Serve.World' and in exactly how many times each node's @up@\/@down@ ran.

Deliberately scoped to v1 per the spec: no @autoconverge@\/@force@\/@Rewrite@
commands (those need real idle time or batching, and stay example-based in
'Test.ServeSpec'), and every generated node's @up@\/@down@ always succeeds —
this suite is about the bookkeeping (invariants 1, 2, 3, 6), not about
failure/retry, which 'Test.ServeSpec' already covers by hand.
-}
module Test.ServeModelSpec (tests) where

import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO (Handle, IOMode (ReadMode), hClose, hPutStr, withFile)
import System.IO.Temp (withSystemTempFile)
import Test.QuickCheck (Gen, Property, choose, conjoin, counterexample, forAllShrink, frequency, ioProperty, shrinkList, sized, vectorOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Convergence (..), Direction (..), NodeState (..), World (..))
import Salmon.Builtin.Extension (Track', deps, nodeps, op, ref, up, down)
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Serve (property-based)"
        [ testProperty "convergence matches an independent shadow model" prop_convergesLikeModel
        , testProperty "clear settles the world to empty, whatever came before" prop_clearSettlesToEmpty
        ]

-------------------------------------------------------------------------------
-- The thing being served: a fixed, small alphabet of node names. Real seeds
-- overlap on purpose, mirroring 'Test.ServeSpec''s own fixture, so
-- shared-node teardown (invariant 3) actually gets exercised.

type NodeName = String

nodeNames :: [NodeName]
nodeNames = ["n1", "n2", "n3"]

-- | Three fixed seeds sharing nodes pairwise: (n1) / (n1,n2) / (n2,n3).
seedNodeNames :: [[NodeName]]
seedNodeNames = [["n1"], ["n1", "n2"], ["n2", "n3"]]

seedCount :: Int
seedCount = length seedNodeNames

newtype Spec = Spec {specNames :: [NodeName]}
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: [String] -> Either Text Spec
parseSpec [] = Left "expected at least one node name"
parseSpec args = Right (Spec args)

-- | A stub 'Track'' whose nodes touch nothing but two 'IORef' counters —
-- same shape as 'Test.ServeSpec''s @neverRuns@/@watched@, generalized to one
-- counter per node name rather than one global/two hand-picked ones.
spyProgram :: IORef (Map NodeName Int) -> IORef (Map NodeName Int) -> Track' Spec
spyProgram upsRef downsRef = Track $ \spec ->
    op "model-root" (deps (fmap nodeOp spec.specNames)) $ \actions ->
        actions{ref = mkRef "model-root" spec.specNames}
  where
    nodeOp name =
        op "model-node" nodeps $ \actions ->
            actions
                { ref = nodeRef name
                , up = bump upsRef name
                , down = bump downsRef name
                }
    bump r name = atomicModifyIORef' r (\m -> (Map.insertWith (+) name 1 m, ()))

-- | The 'Ref' a node named @name@ is given — computed the same way
-- 'spyProgram' computes it, so the test can look a node up in 'worldNodes'
-- without needing the real system to hand identities back out.
nodeRef :: NodeName -> Ref
nodeRef = mkRef "model-node"

-------------------------------------------------------------------------------
-- Commands and their shadow model.

data ServeCommand
    = CmdUp Int
    | CmdDown Int
    | CmdOnly Int
    | CmdClear
    | CmdConverge
    deriving (Show, Eq)

renderCommand :: ServeCommand -> String
renderCommand (CmdUp i) = "up " <> unwords (seedNodeNames !! i)
renderCommand (CmdDown i) = "down " <> unwords (seedNodeNames !! i)
renderCommand (CmdOnly i) = "only " <> unwords (seedNodeNames !! i)
renderCommand CmdClear = "clear"
renderCommand CmdConverge = "converge"

genCommand :: Gen ServeCommand
genCommand =
    frequency
        [ (3, CmdUp <$> seedIx)
        , (3, CmdDown <$> seedIx)
        , (2, CmdOnly <$> seedIx)
        , (1, pure CmdClear)
        , (2, pure CmdConverge)
        ]
  where
    seedIx = choose (0, seedCount - 1)

genCommands :: Gen [ServeCommand]
genCommands = sized $ \n -> do
    len <- choose (1, max 1 (min 30 (n + 5)))
    vectorOf len genCommand

shrinkCommand :: ServeCommand -> [ServeCommand]
shrinkCommand (CmdUp i) = [CmdUp i' | i' <- shrinkIx i]
shrinkCommand (CmdDown i) = [CmdDown i' | i' <- shrinkIx i]
shrinkCommand (CmdOnly i) = CmdUp i : [CmdOnly i' | i' <- shrinkIx i]
shrinkCommand CmdClear = []
shrinkCommand CmdConverge = []

shrinkIx :: Int -> [Int]
shrinkIx 0 = []
shrinkIx _ = [0]

shrinkCommands :: [ServeCommand] -> [[ServeCommand]]
shrinkCommands = shrinkList shrinkCommand

{- | The shadow model: an independent restatement of "which seeds are live,
which nodes 'Serve.worldNodes' currently still holds and in what state, and
how many times each node's up\/down should have fired so far" — written
against the domain (seeds, node names), not against 'Serve.hs''s own types.

Two subtleties below were found empirically (failing runs of this very
property, before this comment existed) rather than reasoned out in advance —
which is exactly the case this suite exists to catch automatically instead
of by hand:

1. A node's own history of having been up is irrelevant to whether @down@
   fires. Every builtin here has no @check@ (the ordinary 'Immaterial'
   default — see @CLAUDE.md@'s "check, not prelim" section), so a node is
   applied whenever it is freshly tracked in a direction, regardless of
   which direction that is. @down n1@ as the very first command ever to
   name @n1@ still calls @down@ once: @n1@ becomes tracked, wanted
   'TurnDown' since no live declaration claims it, starts untracked
   (equivalent to 'Pending'), and gets applied on that basis alone.
2. A node that reaches 'TurnDown'/converged is *dropped* from
   'Serve.worldNodes' (see its own haddock) — so a later declaration that
   names it again finds no trace of it and starts over from scratch, even if
   the direction it ends up wanting is the same 'TurnDown' as before. This
   is why @up n1@, @clear@, @down n1@ calls @down@ *twice*: once when
   @clear@ retires the live seed and @n1@ is torn down and dropped, and
   again when @down n1@ names it afresh — 'modelTracked' models exactly
   this by deleting a node's entry the moment it settles 'TurnDown', so a
   later mention of it has nothing to compare against and is applied
   unconditionally.
-}
data Model = Model
    { modelLive :: !(Set Int)
    -- ^ seeds currently declared up (via @up@\/@only@) and not yet retired.
    -- @down@ never adds to this; only removes, if present.
    , modelTracked :: !(Map NodeName (Direction, Bool))
    -- ^ nodes 'Serve.worldNodes' would currently still hold: each one's
    -- (direction, converged-in-that-direction). Absent entirely once it has
    -- settled 'TurnDown', exactly as 'Serve.worldNodes' drops it.
    , modelUpCount :: !(Map NodeName Int)
    , modelDownCount :: !(Map NodeName Int)
    }

emptyModel :: Model
emptyModel = Model Set.empty Map.empty Map.empty Map.empty

-- | Every command triggers a convergence pass in this suite (autoconverge
-- is left on throughout — see module haddock), so folding a command is:
-- update the live set, (re-)track any node names this command names, then
-- settle every currently-tracked node toward whether it is wanted by the
-- resulting live set.
stepModel :: Model -> ServeCommand -> Model
stepModel m cmd = settle (retrack (applyCommand cmd))
  where
    applyCommand (CmdUp i) = m{modelLive = Set.insert i (modelLive m)}
    applyCommand (CmdDown i) = m{modelLive = Set.delete i (modelLive m)}
    applyCommand (CmdOnly i) = m{modelLive = Set.singleton i}
    applyCommand CmdClear = m{modelLive = Set.empty}
    applyCommand CmdConverge = m

    mentionedNames = case cmd of
        CmdUp i -> seedNodeNames !! i
        CmdDown i -> seedNodeNames !! i
        CmdOnly i -> seedNodeNames !! i
        CmdClear -> []
        CmdConverge -> []

    -- | A declaration naming a node that isn't currently tracked (never
    -- seen, or previously torn down and dropped) puts it back with no
    -- verdict yet — 'settleNode' below treats that exactly like a brand
    -- new node.
    retrack m' = foldl' insertUntracked m' mentionedNames
      where
        insertUntracked acc name
            | Map.member name (modelTracked acc) = acc
            | otherwise = acc{modelTracked = Map.insert name (TurnDown, False) (modelTracked acc)}

    settle m' = foldl' settleNode m' (Map.keys (modelTracked m'))
      where
        desired = Set.fromList (concatMap (seedNodeNames !!) (Set.toList (modelLive m')))

        settleNode m'' name =
            let wantedDir = if Set.member name desired then TurnUp else TurnDown
                apply TurnUp acc =
                    acc
                        { modelTracked = Map.insert name (TurnUp, True) (modelTracked acc)
                        , modelUpCount = Map.insertWith (+) name 1 (modelUpCount acc)
                        }
                apply TurnDown acc =
                    acc
                        { modelTracked = Map.delete name (modelTracked acc) -- settled TurnDown: dropped, like 'Serve.worldNodes'
                        , modelDownCount = Map.insertWith (+) name 1 (modelDownCount acc)
                        }
             in case Map.lookup name (modelTracked m'') of
                    Just (dir, True) | dir == wantedDir -> m'' -- already converged this way: no-op
                    _ -> apply wantedDir m''

runModel :: [ServeCommand] -> Model
runModel = foldl' stepModel emptyModel

-------------------------------------------------------------------------------

prop_convergesLikeModel :: Property
prop_convergesLikeModel =
    forAllShrink genCommands shrinkCommands $ \cmds ->
        counterexample ("script:\n" <> unlines (fmap renderCommand cmds)) $
            runAgainstModel cmds

runAgainstModel :: [ServeCommand] -> Property
runAgainstModel cmds = ioProperty $ do
    upsRef <- newIORef Map.empty
    downsRef <- newIORef Map.empty
    w <- runServe (spyProgram upsRef downsRef) (fmap renderCommand cmds)
    ups <- readIORef upsRef
    downs <- readIORef downsRef
    let m = runModel cmds
    pure $
        counterexample ("model up counts:   " <> show (modelUpCount m)) $
            counterexample ("real  up counts:   " <> show ups) $
                counterexample ("model down counts: " <> show (modelDownCount m)) $
                    counterexample ("real  down counts: " <> show downs) $
                        conjoin
                            [ counterexample "up counts matched the model" (ups == modelUpCount m)
                            , counterexample "down counts matched the model" (downs == modelDownCount m)
                            , counterexample "final World state matched the model" (worldMatchesModel m w)
                            , counterexample "worldEpochs/worldLedger/worldMagma emptiness matched the model" (worldBookkeepingMatchesModel m w)
                            ]

{- | A dedicated property for invariant (6): whatever arbitrary history came
before, @clear@ (itself autoconverging, plus one extra @converge@ for
margin) must drive every one of 'worldNodes'\/'worldEpochs'\/'worldLedger'\/
'worldMagma' empty. Unlike 'prop_convergesLikeModel', which only happens to
land on that state when a random tail ends up there (rare, given
'CmdClear''s low generator weight), this one forces the terminal state
directly, so it shrinks straight to a short "history + clear" script whenever
something leaks.
-}
prop_clearSettlesToEmpty :: Property
prop_clearSettlesToEmpty =
    forAllShrink genCommands shrinkCommands $ \cmds ->
        let cmds' = cmds <> [CmdClear, CmdConverge]
         in counterexample ("script:\n" <> unlines (fmap renderCommand cmds')) $
                ioProperty $ do
                    upsRef <- newIORef Map.empty
                    downsRef <- newIORef Map.empty
                    w <- runServe (spyProgram upsRef downsRef) (fmap renderCommand cmds')
                    pure $
                        conjoin
                            [ counterexample "no node left to manage" (Map.null w.worldNodes)
                            , counterexample "no graph left to walk" (null w.worldEpochs)
                            , counterexample "no contribution left in the ledger" (Map.null w.worldLedger)
                            , counterexample "no representative left in the magma" (Map.null w.worldMagma)
                            ]

{- | 'modelTracked' only ever rests holding 'TurnUp'/'Converged' entries (see
its own haddock: a node settling 'TurnDown' is deleted in the same step), so
a node the model tracks should be present in 'worldNodes' as 'TurnUp'
\/'Converged', and a node the model doesn't track should be entirely absent
— 'Serve.World' drops a node once it has converged 'TurnDown'. Nothing here
ever fails an up\/down, so by the end of the script nothing should be left
mid-flight either side.
-}
worldMatchesModel :: Model -> World Spec Spec -> Bool
worldMatchesModel m w = all checkName nodeNames
  where
    checkName name =
        case (Map.lookup name (modelTracked m), Map.lookup (nodeRef name) w.worldNodes) of
            (Just (TurnUp, True), Just st) -> st.nodeDirection == TurnUp && st.nodeConvergence == Converged
            (Nothing, Nothing) -> True
            _ -> False

{- | Invariant (6), "settling is total", names 'worldEpochs'\/'worldLedger'\/
'worldMagma' explicitly, not just 'worldNodes' — so check them too. The model
has no independent notion of these (it only tracks nodes/seeds), but it does
know when it considers everything settled and gone: no seed live, no node
still tracked. Whenever that holds, the real world's other bookkeeping must
have collected everything as well, or something is leaking behind
'worldNodes''s back.
-}
worldBookkeepingMatchesModel :: Model -> World Spec Spec -> Bool
worldBookkeepingMatchesModel m w
    | Set.null (modelLive m) && Map.null (modelTracked m) =
        null w.worldEpochs && Map.null w.worldLedger && Map.null w.worldMagma
    | otherwise = True

-- | Drive the loop over a scripted stdin, piped-script mode (never idle, so
-- deterministic) — same shape as 'Test.ServeSpec.runServe'/'withScript',
-- inlined here since 'Test.ServeSpec' exports only its 'tests'.
runServe :: Track' Spec -> [String] -> IO (World Spec Spec)
runServe prog script =
    withScript script $ \h ->
        Serve.serveWith [] Nothing True silent silent parseSpec (Configure pure) prog h

withScript :: [String] -> (Handle -> IO a) -> IO a
withScript ls act =
    withSystemTempFile "salmon-serve-model-script" $ \path h -> do
        hPutStr h (unlines ls)
        hClose h
        withFile path ReadMode act
