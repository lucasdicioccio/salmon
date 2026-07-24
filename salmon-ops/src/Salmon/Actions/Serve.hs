{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A long-running convergence loop, fed a stream of seeds.

Where @run up@ is a one-shot "expand this one directive and walk it once",
'serve' keeps a 'World' around: a set of seeds that have been declared, the
graphs those seeds evaluated to, and — unified across all of them by 'Ref' —
a per-node 'NodeState' saying which 'Direction' that node is wanted in and
whether it has 'Converged' there yet.

The unit of input is a /declaration/: a seed, plus what to do with it (see
'ServeCommand'). Declaring a seed appends an 'Epoch' to an append-only
history (keeping the seed, the directive it configured to, and the graph it
evaluated to /at that moment/ — a later declaration of the same seed
re-evaluates and appends a fresh epoch rather than mutating the old one) and
updates the set of /active/ seeds. From the active set everything else is
derived:

  * a node in some active seed's graph is wanted 'TurnUp';
  * a node this world has ever seen and no active seed still asks for is
    wanted 'TurnDown' — this is why retired epochs' graphs are kept, they
    are the only remaining description of how to tear those nodes down;
  * flipping a node's direction resets it to 'Pending', so it gets applied
    again in the new direction.

Convergence then runs — after every declaration, and on demand via
@converge@ — as one teardown pass followed by one bring-up pass, each of
which is just "Salmon.Actions.UpDown".'UpDown.downTreeWith' /
'UpDown.upTreeWith' over the relevant graphs with a 'UpDown.Gate' that
filters down to the nodes wanted in that pass and not yet converged. The
dependency ordering, the dedup-by-'Ref', and the "a failed node blocks
whatever depended on it" containment therefore behave exactly as they do for
@run up@ / @run down@; the only thing this module adds on top is the memory
of what has already been done. Nodes that end a pass 'Errored' or 'Blocked'
stay non-converged and are retried by the next pass.
-}
module Salmon.Actions.Serve (
    -- * Running
    serve,

    -- * Input language
    ServeCommand (..),
    Declaration (..),
    parseServeCommand,
    tokenize,

    -- * World state
    World (..),
    emptyWorld,
    Epoch (..),
    EpochId (..),
    NodeState (..),
    Direction (..),
    Convergence (..),

    -- * Reporting
    Report (..),
    reportText,
    renderReport,
) where

import Control.Comonad.Cofree (Cofree)
import Control.Monad (when)
import Control.Monad.Identity (runIdentity)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Char (isSpace)
import Data.Foldable (toList, traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (Handle, hFlush, hGetLine, hIsEOF, stdout)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.UpDown (Requirement (..))
-- imported with their field selectors: OverloadedRecordDot only solves
-- HasField for fields that are in scope.
import Salmon.Builtin.Extension (Extension (..), Op, Track', deps, evalDeps, opAct)
import Salmon.Op.Actions (Act (..), Actions (..), ShortHand)
import Salmon.Op.Configure (Configure, gen)
import Salmon.Op.Graph (Graph)
import Salmon.Op.OpGraph (OpGraph (OpGraph))
import Salmon.Op.Ref (Ref, unRef)
import Salmon.Op.Track (run)
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | Which way a node is currently wanted.
data Direction
    = TurnUp
    | TurnDown
    deriving (Show, Eq, Ord)

-- | How far a node is from its wanted 'Direction'.
data Convergence
    = -- | never applied in the current direction (new node, or the
      -- direction just flipped under it)
      Pending
    | -- | applied in the current direction, or found to already be there
      Converged
    | -- | the last attempt threw; will be retried
      Errored
    | -- | the last attempt never ran because a neighbour failed; will be retried
      Blocked
    deriving (Show, Eq, Ord)

{- | What 'serve' remembers about one node of the unified graph. Keyed by
'Ref', so the same node reached through several seeds' graphs is one entry.
-}
data NodeState = NodeState
    { nodeShorthand :: !ShortHand
    , nodeHelp :: !Text
    , nodeDirection :: !Direction
    , nodeConvergence :: !Convergence
    , -- | most recent epoch whose graph contained this node
      nodeEpoch :: !EpochId
    }
    deriving (Show)

newtype EpochId = EpochId {unEpochId :: Int}
    deriving (Show, Eq, Ord)

{- | One declaration, and everything derived from it at the time it was made.
Kept forever in 'worldHistory' (the graph of a retired seed is what a
teardown is walked over), so this is deliberately a snapshot: re-declaring
the same seed appends a new epoch instead of updating this one.
-}
data Epoch seed directive = Epoch
    { epochId :: !EpochId
    , epochDeclaration :: !Declaration
    , epochDirection :: !Direction
    , -- | the argv this seed was declared with, kept for @history@
      epochTokens :: [String]
    , epochSeed :: seed
    , epochDirective :: directive
    , -- | identity of the seed for the active set: its encoded directive, so
      -- that two spellings of the same desired state are one active seed
      epochKey :: !ByteString
    , epochOp :: Op
    , -- | the graph as evaluated when the seed was declared
      epochGraph :: Cofree Graph Op
    , epochRefs :: Map Ref (ShortHand, Text)
    }

data World seed directive = World
    { worldNextId :: !Int
    , -- | append-only, newest first
      worldHistory :: [Epoch seed directive]
    , -- | seeds currently declared up, by 'epochKey'
      worldActive :: Map ByteString EpochId
    , -- | every node ever seen, unified by 'Ref'
      worldNodes :: Map Ref NodeState
    }

emptyWorld :: World seed directive
emptyWorld = World 0 [] Map.empty Map.empty

-------------------------------------------------------------------------------

-- | What declaring a seed does to the active set.
data Declaration
    = -- | @up@: add this seed to the active set
      Add
    | -- | @only@: make this seed the whole active set, retiring the others
      Replace
    | -- | @down@: retire this seed
      Remove
    deriving (Show, Eq, Ord)

data ServeCommand
    = Declare !Declaration ![String]
    | -- | @clear@: retire every seed (everything known goes down)
      Clear
    | -- | @converge@: re-attempt whatever has not converged
      Converge
    | Status
    | History
    | Quit
    | -- | blank line or comment
      Noop
    deriving (Show, Eq)

declarationDirection :: Declaration -> Direction
declarationDirection Add = TurnUp
declarationDirection Replace = TurnUp
declarationDirection Remove = TurnDown

{- | Parse one line of 'serve' input: a command word followed, for the
declaring commands, by the seed's own command-line arguments.
-}
parseServeCommand :: String -> Either Text ServeCommand
parseServeCommand line =
    case dropWhile isSpace line of
        [] -> Right Noop
        ('#' : _) -> Right Noop
        _ -> dispatch =<< tokenize line
  where
    dispatch toks =
        case toks of
            [] -> Right Noop
            (w : args) ->
                case w of
                    "up" -> Right (Declare Add args)
                    "only" -> Right (Declare Replace args)
                    "down" -> Right (Declare Remove args)
                    "clear" -> nullary w args Clear
                    "converge" -> nullary w args Converge
                    "status" -> nullary w args Status
                    "history" -> nullary w args History
                    "quit" -> nullary w args Quit
                    "exit" -> nullary w args Quit
                    _ -> Left ("unknown command: " <> Text.pack w)

    nullary w args cmd
        | null args = Right cmd
        | otherwise = Left (Text.pack w <> " takes no argument")

{- | Split a line into argv-style tokens, honouring single quotes, double
quotes and backslash escapes, so a seed can carry values with spaces in them.
-}
tokenize :: String -> Either Text [String]
tokenize = outside []
  where
    outside toks s =
        case s of
            [] -> Right (reverse toks)
            (c : cs)
                | isSpace c -> outside toks cs
                | otherwise -> word toks "" (c : cs)

    word toks cur s =
        case s of
            [] -> Right (reverse (reverse cur : toks))
            (c : cs)
                | isSpace c -> outside (reverse cur : toks) cs
                | c == '\\' -> escape (word toks) cur cs
                | c == '\'' -> quoted '\'' toks cur cs
                | c == '"' -> quoted '"' toks cur cs
                | otherwise -> word toks (c : cur) cs

    quoted q toks cur s =
        case s of
            [] -> Left "unterminated quote"
            (c : cs)
                | c == q -> word toks cur cs
                | c == '\\' && q == '"' -> escape (quoted q toks) cur cs
                | otherwise -> quoted q toks (c : cur) cs

    escape k cur s =
        case s of
            (d : ds) -> k (d : cur) ds
            [] -> Left "trailing backslash"

-------------------------------------------------------------------------------

data Report
    = Started
    | -- | input closed
      Stopped
    | BadCommand !Text
    | BadSeed !Text
    | -- | epoch, direction, nodes in its graph, active seeds afterwards
      Declared !EpochId !Direction !Int !Int
    | -- | number of seeds retired
      Cleared !Int
    | -- | nodes to turn down, nodes to turn up
      ConvergeStart !Int !Int
    | -- | everything applied cleanly, nodes still not converged
      ConvergeStop !Bool !Int
    | StatusReport ![(Ref, NodeState)]
    | -- | epoch, declaration, still active, argv
      HistoryReport ![(EpochId, Declaration, Bool, [String])]
    deriving (Show)

-- | Prints 'Report's in a human-readable, one-event-per-block form.
reportText :: Reporter Report
reportText = ReporterM $ \rep -> do
    traverse_ Text.putStrLn (renderReport rep)
    hFlush stdout

renderReport :: Report -> [Text]
renderReport rep =
    case rep of
        Started ->
            [ "serve: ready"
            , "serve: commands: (up|only|down) <seed args...> | clear | converge | status | history | quit"
            ]
        Stopped -> ["serve: input closed"]
        BadCommand err -> ["serve: " <> err]
        BadSeed err -> ("serve: cannot configure seed:") : Text.lines err
        Declared eid dir nnodes nactive ->
            [ Text.unwords
                [ "serve: epoch"
                , renderEpochId eid
                , renderDirection dir
                , "(" <> tshow nnodes <> " nodes,"
                , tshow nactive <> " active seed(s))"
                ]
            ]
        Cleared n -> ["serve: retired " <> tshow n <> " seed(s)"]
        ConvergeStart ndown nup ->
            ["serve: converging (" <> tshow ndown <> " down, " <> tshow nup <> " up)"]
        ConvergeStop ok remaining ->
            [ Text.unwords
                [ "serve:"
                , if ok then "converged" else "converge incomplete"
                , "(" <> tshow remaining <> " node(s) left)"
                ]
            ]
        StatusReport [] -> ["serve: no nodes"]
        StatusReport xs -> "serve: nodes:" : fmap renderNode (sortOn statusOrder xs)
        HistoryReport [] -> ["serve: no seed declared yet"]
        HistoryReport xs -> "serve: seeds:" : fmap renderEpochLine xs
  where
    statusOrder :: (Ref, NodeState) -> (Direction, Convergence, ShortHand, Text)
    statusOrder (r, st) = (st.nodeDirection, st.nodeConvergence, st.nodeShorthand, unRef r)

    renderNode :: (Ref, NodeState) -> Text
    renderNode (r, st) =
        Text.unwords
            [ " "
            , renderDirection st.nodeDirection
            , Text.justifyLeft 9 ' ' (tshow st.nodeConvergence)
            , Text.justifyLeft 10 ' ' (unRef r)
            , st.nodeShorthand
            ]

    renderEpochLine :: (EpochId, Declaration, Bool, [String]) -> Text
    renderEpochLine (eid, decl, active, toks) =
        Text.unwords
            [ " "
            , renderEpochId eid
            , Text.justifyLeft 8 ' ' (renderDeclaration decl)
            , if active then "[active]" else "[retired]"
            , Text.pack (unwords toks)
            ]

renderDirection :: Direction -> Text
renderDirection TurnUp = "up"
renderDirection TurnDown = "down"

renderDeclaration :: Declaration -> Text
renderDeclaration Add = "up"
renderDeclaration Replace = "only"
renderDeclaration Remove = "down"

renderEpochId :: EpochId -> Text
renderEpochId eid = "#" <> tshow eid.unEpochId

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-------------------------------------------------------------------------------

{- | Read declarations from a handle until EOF (or @quit@), converging after
each one, and hand back the 'World' as it stands when the loop ends. Never
tears anything down on its way out: exiting the loop leaves the machine as
the last convergence left it.
-}
serve ::
    forall seed directive.
    (ToJSON directive) =>
    -- | loop-level events
    Reporter Report ->
    -- | per-node events, same reporter @run up@ uses
    Reporter (UpDown.Report Extension) ->
    -- | parses a seed out of one declaration's arguments
    ([String] -> Either Text seed) ->
    Configure IO seed directive ->
    Track' directive ->
    Handle ->
    IO (World seed directive)
serve r nodeReporter parseSeed configure program h = do
    world <- newIORef emptyWorld
    runReporter r Started
    loop world
    readIORef world
  where
    nat = pure . runIdentity

    loop :: IORef (World seed directive) -> IO ()
    loop world = do
        eof <- hIsEOF h
        if eof
            then runReporter r Stopped
            else do
                line <- hGetLine h
                keepGoing <- step world line
                when keepGoing (loop world)

    step :: IORef (World seed directive) -> String -> IO Bool
    step world line =
        case parseServeCommand line of
            Left err -> do
                runReporter r (BadCommand err)
                pure True
            Right cmd ->
                case cmd of
                    Noop -> pure True
                    Quit -> pure False
                    Status -> do
                        w <- readIORef world
                        runReporter r (StatusReport (Map.toList w.worldNodes))
                        pure True
                    History -> do
                        w <- readIORef world
                        runReporter r (HistoryReport (historyLines w))
                        pure True
                    Converge -> do
                        converge world
                        pure True
                    Clear -> do
                        w <- readIORef world
                        writeIORef world (retune w{worldActive = Map.empty})
                        runReporter r (Cleared (Map.size w.worldActive))
                        converge world
                        pure True
                    Declare decl args -> do
                        declare world decl args
                        pure True

    declare :: IORef (World seed directive) -> Declaration -> [String] -> IO ()
    declare world decl args =
        case parseSeed args of
            Left err -> runReporter r (BadSeed err)
            Right seed -> do
                directive <- gen configure seed
                w0 <- readIORef world
                let o = run program directive
                let gr = evalDeps o
                let ep =
                        Epoch
                            { epochId = EpochId w0.worldNextId
                            , epochDeclaration = decl
                            , epochDirection = declarationDirection decl
                            , epochTokens = args
                            , epochSeed = seed
                            , epochDirective = directive
                            , epochKey = encode directive
                            , epochOp = o
                            , epochGraph = gr
                            , epochRefs = graphRefs gr
                            }
                let w1 = retune (record decl ep w0)
                writeIORef world w1
                runReporter r $
                    Declared
                        ep.epochId
                        ep.epochDirection
                        (Map.size ep.epochRefs)
                        (Map.size w1.worldActive)
                converge world

    converge :: IORef (World seed directive) -> IO ()
    converge world = do
        w <- readIORef world
        let (nup, ndown) = pendingCounts w
        runReporter r (ConvergeStart ndown nup)
        -- teardown first: a node being replaced by an incompatible one
        -- (different content, hence a different 'Ref') has to go before its
        -- successor is brought up.
        okDown <-
            if ndown == 0
                then pure True
                else
                    UpDown.downTreeWith
                        (gateFor world TurnDown)
                        (recorder world TurnDown)
                        nat
                        (forest (downOps w))
        okUp <-
            if nup == 0
                then pure True
                else
                    UpDown.upTreeWith
                        (gateFor world TurnUp)
                        (recorder world TurnUp)
                        nat
                        (forest (upOps w))
        w' <- readIORef world
        let (rup, rdown) = pendingCounts w'
        runReporter r (ConvergeStop (okDown && okUp) (rup + rdown))

    -- | Only touch what this pass is for: a node wanted the other way (it
    -- belongs to some other seed) or already converged is left alone.
    gateFor :: IORef (World seed directive) -> Direction -> UpDown.Gate Extension
    gateFor world dir = \act -> do
        w <- readIORef world
        pure $ case Map.lookup act.extension.ref w.worldNodes of
            Nothing -> Skippable
            Just st
                | st.nodeDirection /= dir -> Skippable
                | st.nodeConvergence == Converged -> Skippable
                | otherwise -> Required

    recorder :: IORef (World seed directive) -> Direction -> Reporter (UpDown.Report Extension)
    recorder world dir = reportBoth (stateWriter world dir) nodeReporter

    {- 'upTree'/'downTree' report an 'Eval' before running a node and a
    'Failed' after it only if it threw, and they run nodes one at a time —
    so recording 'Eval' as converged and letting the immediately following
    'Failed' overwrite it is exact, and avoids needing a success report. A
    'Skip' is either this pass's own gate (already converged, or not ours —
    both fine to record as converged, the direction check below drops the
    latter) or, on the way up, the node's own 'prelim' saying its effect is
    already in place, which is convergence too. -}
    stateWriter :: IORef (World seed directive) -> Direction -> Reporter (UpDown.Report Extension)
    stateWriter world dir = ReporterM $ \rep ->
        case rep of
            UpDown.Eval act -> mark act Converged
            UpDown.Skip act -> mark act Converged
            UpDown.Failed act _ -> mark act Errored
            UpDown.Blocked act -> mark act Blocked
            UpDown.Redundant _ -> pure ()
      where
        mark :: Act Extension -> Convergence -> IO ()
        mark act c = modifyIORef' world (setConvergence dir act.extension.ref c)

-------------------------------------------------------------------------------

-- | Appends the epoch to the history and applies it to the active set.
record :: Declaration -> Epoch seed directive -> World seed directive -> World seed directive
record decl ep w =
    w
        { worldNextId = w.worldNextId + 1
        , worldHistory = ep : w.worldHistory
        , worldActive = case decl of
            Add -> Map.insert ep.epochKey ep.epochId w.worldActive
            Replace -> Map.singleton ep.epochKey ep.epochId
            Remove -> Map.delete ep.epochKey w.worldActive
        }

{- | Re-derives every node's wanted 'Direction' from the active seeds. A node
whose direction is unchanged keeps its 'Convergence'; one that just flipped
goes back to 'Pending', because whatever was done to it was done the other way.
-}
retune :: World seed directive -> World seed directive
retune w =
    w{worldNodes = Map.mapWithKey adjust known}
  where
    activeIds :: Set EpochId
    activeIds = Set.fromList (Map.elems w.worldActive)

    desired :: Set Ref
    desired =
        Set.unions
            [ Map.keysSet ep.epochRefs
            | ep <- w.worldHistory
            , Set.member ep.epochId activeIds
            ]

    -- every node ever seen; history is newest-first and 'Map.unions' is
    -- left-biased, so a node's metadata comes from the latest graph it was in.
    known :: Map Ref (EpochId, ShortHand, Text)
    known =
        Map.unions
            [ fmap (\(sh, hlp) -> (ep.epochId, sh, hlp)) ep.epochRefs
            | ep <- w.worldHistory
            ]

    adjust r (eid, sh, hlp) =
        let dir = if Set.member r desired then TurnUp else TurnDown
         in case Map.lookup r w.worldNodes of
                Just st
                    | st.nodeDirection == dir ->
                        st{nodeShorthand = sh, nodeHelp = hlp, nodeEpoch = eid}
                _ -> NodeState sh hlp dir Pending eid

setConvergence :: Direction -> Ref -> Convergence -> World seed directive -> World seed directive
setConvergence dir r c w =
    w{worldNodes = Map.adjust upd r w.worldNodes}
  where
    upd st
        | st.nodeDirection == dir = st{nodeConvergence = c}
        | otherwise = st

-- | (nodes wanted up, nodes wanted down) that have not converged yet.
pendingCounts :: World seed directive -> (Int, Int)
pendingCounts w =
    (count TurnUp, count TurnDown)
  where
    count dir = length [() | st <- Map.elems w.worldNodes, st.nodeDirection == dir, st.nodeConvergence /= Converged]

-- | Graphs of the active seeds: what the up pass walks.
upOps :: World seed directive -> [Op]
upOps w =
    [ ep.epochOp
    | ep <- w.worldHistory
    , Set.member ep.epochId activeIds
    ]
  where
    activeIds = Set.fromList (Map.elems w.worldActive)

{- | Graphs holding at least one node still to be torn down: what the down
pass walks. Epochs whose nodes are all converged (or all still wanted up)
are left out, so a long history does not make every pass more expensive.
-}
downOps :: World seed directive -> [Op]
downOps w =
    [ ep.epochOp
    | ep <- w.worldHistory
    , any needsDown (Map.keys ep.epochRefs)
    ]
  where
    needsDown r =
        case Map.lookup r w.worldNodes of
            Just st -> st.nodeDirection == TurnDown && st.nodeConvergence /= Converged
            Nothing -> False

{- | Bundles several graphs under one 'Actionless' root, which 'upTree' and
'downTree' walk through without treating it as a node of its own.
-}
forest :: [Op] -> Op
forest ops = OpGraph (deps ops) Actionless

historyLines :: World seed directive -> [(EpochId, Declaration, Bool, [String])]
historyLines w =
    [ (ep.epochId, ep.epochDeclaration, Set.member ep.epochId activeIds, ep.epochTokens)
    | ep <- reverse w.worldHistory
    ]
  where
    activeIds = Set.fromList (Map.elems w.worldActive)

graphRefs :: Cofree Graph Op -> Map Ref (ShortHand, Text)
graphRefs gr =
    Map.fromList
        [ (a.extension.ref, (a.shorthand, a.extension.help))
        | o <- toList gr
        , Just a <- [opAct o]
        ]
