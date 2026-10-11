{-# LANGUAGE OverloadedStrings #-}

{- | "Salmon.Client.Model" as it was before its display order stopped being
a list: the reference 'step' that "Test.ClientModelStepSpec" holds the
current one to.

__This is a copy, on purpose, and it is not to be improved.__ It is the
text of @salmon-ops\/src\/Salmon\/Client\/Model.hs@ at the commit before
'modelOrder' became an @Order@ (types, 'fromDag', 'step', 'rebase',
'nodesInOrder', 'counts' and the JSON readers, unchanged; the module
header and the rendering functions, which the comparison does not need,
left out). 'step' here removes a node with @filter (\/= r) modelOrder@:
quadratic over a teardown, and plainly right, which is what a reference is
for.

Because it is a copy it has types of its own ('Model', 'Node', 'Event')
with the same constructors and field names as the real ones, so the spec
compares the two through what they 'show'.

A rule added to the real fold after this copy was taken (a new event kind,
a new field) will fail the comparison until it is added here too. That is
the price of a reference that does not share code with what it checks; if
it stops being worth paying, delete this module and the comparison with
it, and keep the properties that need no reference.
-}
module Test.ClientModelReference (
    Event (..),
    eventOf,
    RefId (..),
    Model (..),
    Node (..),
    Check (..),
    Pass (..),
    Counts (..),
    fromDag,
    step,
    rebase,
    nodesInOrder,
    counts,
) where

import Data.Aeson (Result (..), Value (..), fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

-------------------------------------------------------------------------------
-- events

-- | A node's identity on the wire: the short tag and the full text.
data RefId = RefId
    { refShort :: !Text
    , refFull :: !Text
    }
    deriving (Show, Eq, Ord)

{- | One event as the stream carried it. The fields are what every client
needs to route the object; 'eventValue' keeps the whole thing for whatever
else a report says.
-}
data Event = Event
    { eventSeq :: !(Maybe Word64)
    -- ^ 'Nothing' only for the @gap@ event, which the server sends without one
    , eventStream :: !Text
    , eventKind :: !Text
    , eventRef :: !(Maybe RefId)
    , eventOrigin :: !(Maybe Text)
    -- ^ the origin's name, for an event produced for a command
    , eventValue :: !Value
    }
    deriving (Show, Eq)

-- | Read an event out of its wire object. Total: a shapeless object is an
-- event of kind @?@ with no ref, which the model records and moves past.
eventOf :: Value -> Event
eventOf v =
    Event
        { eventSeq = numberAt ["seq"] v
        , eventStream = fromMaybe "?" (textAt ["stream"] v)
        , eventKind = fromMaybe "?" (textAt ["kind"] v)
        , eventRef = refAt ["ref"] v
        , eventOrigin = textAt ["origin", "name"] v
        , eventValue = v
        }

-------------------------------------------------------------------------------
-- the model

-- | A node's own last word on its effect, as @next-look@ and @\/dag@ carry it.
data Check = Check
    { checkVerdict :: !Text
    -- ^ @success@\/@skipped@\/@completed@\/@failure@\/@unknown@\/@immaterial@
    , checkReason :: !(Maybe Text)
    }
    deriving (Show, Eq)

data Node = Node
    { nodeRef :: !RefId
    , nodeShorthand :: !Text
    , nodeHelp :: !Text
    , nodeNotes :: ![Text]
    , nodeDirection :: !Text
    -- ^ @up@ or @down@
    , nodeConvergence :: !Text
    -- ^ @pending@\/@stale@\/@converged@\/@errored@\/@blocked@
    , nodeCheck :: !(Maybe Check)
    , nodeOutput :: ![Text]
    -- ^ the snapshot's output ring, then each @output@ line since, oldest first
    , nodeSaid :: ![Text]
    -- ^ the @log@ lines the stream carried (what the node said during a
    -- one-shot @up@), oldest first; a line from standard error is prefixed
    -- @err| @ and the node's own message @msg| @. In no snapshot.
    , nodeSaidSeq :: !Word64
    -- ^ the number 'nodeSaid' is current to; 0 until a line arrives
    , nodeError :: !(Maybe Text)
    -- ^ the last @failed@'s error, cleared by a later @done@\/@skip@
    , nodeLastKind :: !(Maybe Text)
    -- ^ the kind of the last event about this node, and its stream
    , nodeLastSeq :: !(Maybe Word64)
    , nodeSeq :: !Word64
    -- ^ the number this view is current to: the snapshot's, then each event's
    , nodeDependencies :: ![RefId]
    , nodeDependants :: ![RefId]
    , nodePaths :: ![Text]
    }
    deriving (Show, Eq)

-- | The loop's last convergence pass as the stream told it.
data Pass
    = -- | @converge-start@: nodes to turn down, nodes to turn up
      Converging !Int !Int
    | -- | @converge-stop@: everything applied cleanly, nodes left
      Stopped !Bool !Int
    deriving (Show, Eq)

data Model = Model
    { modelMode :: !Text
    -- ^ @interactive@\/@replay@\/@following@, from the snapshot's envelope
    , modelSeq :: !Word64
    -- ^ the highest sequence number seen: the snapshot's, then each
    -- event's; the cursor to resume @\/events@ from
    , modelLoopSeq :: !Word64
    -- ^ the number the loop-level fields are current to
    , modelOrder :: ![RefId]
    -- ^ the snapshot's dependency order ('Salmon.Op.Dag.dagOrder')
    , modelNodes :: !(Map RefId Node)
    , modelPass :: !(Maybe Pass)
    , modelSupervised :: !(Maybe Bool)
    -- ^ 'Nothing' until a @supervised@ event says
    , modelLast :: !(Maybe Event)
    -- ^ the last event folded, whatever it was about
    , modelResyncReason :: !(Maybe Text)
    -- ^ why the snapshot should be re-read; cleared by 'resolve' or 'fromDag'
    }
    deriving (Show, Eq)

-- | The snapshot should be re-read: a @gap@, or a declaration changed the
-- node set. The reason is human text for a status line.
modelResync :: Model -> Maybe Text
modelResync = modelResyncReason

-- | Forget the resync request (the snapshot is being re-read).
resolve :: Model -> Model
resolve m = m{modelResyncReason = Nothing}

{- | A re-read snapshot joining a model that has been folding: the nodes,
order and mode are the fresh snapshot's; the loop-level fields, their
stamp and the last event are the old model's; the cursor is the higher of
the two; and the resync request is answered. See the module header for
why the loop's part is not simply the snapshot's. What a node said
('nodeSaid') is the old model's too, for the nodes the fresh snapshot
still has: no snapshot carries it.
-}
rebase :: Model -> Model -> Model
rebase old fresh =
    fresh
        { modelNodes = Map.mapWithKey keepSaid fresh.modelNodes
        , modelSeq = max old.modelSeq fresh.modelSeq
        , modelLoopSeq = old.modelLoopSeq
        , modelPass = old.modelPass
        , modelSupervised = old.modelSupervised
        , modelLast = old.modelLast
        , modelResyncReason = Nothing
        }
  where
    keepSaid :: RefId -> Node -> Node
    keepSaid r n = case Map.lookup r old.modelNodes of
        Just o -> n{nodeSaid = o.nodeSaid, nodeSaidSeq = o.nodeSaidSeq}
        Nothing -> n

{- | A model from a @\/dag@ answer. 'Left' names what is missing; a @\/dag@
answer always has @nodes@ and @seq@, so a 'Left' is a wrong URL, not a
version skew.
-}
fromDag :: Value -> Either String Model
fromDag v = do
    nodes <- maybe (Left "/dag answer has no nodes array") Right (arrayAt ["nodes"] v)
    seqNo <- maybe (Left "/dag answer carries no seq") Right (numberAt ["seq"] v)
    parsed <- traverse (nodeOf seqNo) nodes
    pure
        Model
            { modelMode = fromMaybe "?" (textAt ["mode"] v)
            , modelSeq = seqNo
            , modelLoopSeq = 0
            , modelOrder = fmap nodeRef parsed
            , modelNodes = Map.fromList [(nodeRef n, n) | n <- parsed]
            , modelPass = Nothing
            , modelSupervised = Nothing
            , modelLast = Nothing
            , modelResyncReason = Nothing
            }
  where
    nodeOf :: Word64 -> Value -> Either String Node
    nodeOf seqNo n = do
        r <- maybe (Left ("a node without a ref: " <> show n)) Right (refAt ["ref"] n)
        pure
            Node
                { nodeRef = r
                , nodeShorthand = fromMaybe "?" (textAt ["shorthand"] n)
                , nodeHelp = fromMaybe "" (textAt ["help"] n)
                , nodeNotes = maybe [] (mapMaybe asText) (arrayAt ["notes"] n)
                , nodeDirection = fromMaybe "?" (textAt ["direction"] n)
                , nodeConvergence = fromMaybe "?" (textAt ["convergence"] n)
                , nodeCheck = checkAt ["status", "check"] n
                , nodeOutput = maybe [] (mapMaybe asText) (arrayAt ["status", "output"] n)
                , nodeSaid = []
                , nodeSaidSeq = 0
                , nodeError = Nothing
                , nodeLastKind = Nothing
                , nodeLastSeq = Nothing
                , nodeSeq = seqNo
                , nodeDependencies = maybe [] (mapMaybe (refAt [])) (arrayAt ["dependencies"] n)
                , nodeDependants = maybe [] (mapMaybe (refAt [])) (arrayAt ["dependants"] n)
                , nodePaths = maybe [] (mapMaybe asText) (arrayAt ["paths"] n)
                }

{- | Fold one event in. Total; an event already accounted for (numbered at
or below the stamp of the part it would change, see the module header)
leaves that part as it was. Otherwise the cursor moves to the event's
number, the node the event is about is updated, and the loop-level fields
follow the @serve@ stream. An event that changes nothing still becomes
'modelLast' if it is new to the cursor.
-}
step :: Model -> Event -> Model
step m0 e
    | Just s <- e.eventSeq, s <= m0.modelSeq = byStream m0
    | otherwise = byStream (m0{modelSeq = fromMaybe m0.modelSeq e.eventSeq, modelLast = Just e})
  where
    byStream m = case (e.eventStream, e.eventKind) of
        ("server", "gap") -> m{modelResyncReason = Just ("events " <> fromText (numberAt ["from"] e.eventValue) <> " fell off the ring")}
        ("server", _) -> m
        ("serve", "declared") ->
            onLoop m $ \l -> l{modelResyncReason = Just ("epoch " <> fromText (numberAt ["epoch"] e.eventValue) <> " declared " <> fromMaybe "?" (textAt ["direction"] e.eventValue))}
        ("serve", "cleared") -> onLoop m $ \l -> l{modelResyncReason = Just "every seed retired"}
        ("serve", "converge-start") ->
            onLoop m $ \l -> l{modelPass = Just (Converging (intAt ["down"] e.eventValue) (intAt ["up"] e.eventValue))}
        ("serve", "converge-stop") ->
            onLoop m $ \l -> l{modelPass = Just (Stopped (fromMaybe False (boolAt ["ok"] e.eventValue)) (intAt ["remaining"] e.eventValue))}
        ("serve", "supervised") -> onLoop m $ \l -> l{modelSupervised = boolAt ["on"] e.eventValue}
        ("serve", _) -> m
        ("updown", k) -> maybe m (\r -> onNode r (settle k e.eventValue) m) e.eventRef
        -- an @acted@ wraps what the tending machine did in the pass's
        -- vocabulary; the inner object carries the ref
        ("upkeep", "acted") ->
            case KeyMap.lookup "report" =<< asObject e.eventValue of
                Just inner | Just r <- refAt ["ref"] inner ->
                    let k = fromMaybe "?" (textAt ["kind"] inner)
                     in onNode r (updown k inner . touch ("acted " <> k)) m
                _ -> m
        ("upkeep", "next-look") ->
            maybe m (\r -> onNode r (\n -> (touch e.eventKind n){nodeCheck = checkAt ["check"] e.eventValue}) m) e.eventRef
        ("upkeep", k) -> maybe m (\r -> onNode r (touch k) m) e.eventRef
        -- a held action's line: in the ring, so under the node's stamp
        ("output", "output") ->
            case (e.eventRef, textAt ["line"] e.eventValue) of
                (Just r, Just line) -> onNode r (\n -> n{nodeOutput = keepLast (n.nodeOutput ++ [line])}) m
                _ -> m
        -- a line said during a one-shot up: in no snapshot, so its own stamp
        ("output", "log") ->
            case (e.eventRef, textAt ["line"] e.eventValue) of
                (Just r, Just line) -> said r (channelMark (textAt ["channel"] e.eventValue) <> line) m
                _ -> m
        _ -> m

    said :: RefId -> Text -> Model -> Model
    said r line m = case Map.lookup r m.modelNodes of
        Nothing -> m
        Just n
            | Just s <- e.eventSeq, s <= n.nodeSaidSeq -> m
            | otherwise ->
                let n' = n{nodeSaid = keepLast (n.nodeSaid ++ [line]), nodeSaidSeq = fromMaybe n.nodeSaidSeq e.eventSeq}
                 in m{modelNodes = Map.insert r n' m.modelNodes}

    keepLast :: [Text] -> [Text]
    keepLast xs = drop (length xs - outputKept) xs

    -- the loop-level fields, unless the event is at or below their stamp
    onLoop :: Model -> (Model -> Model) -> Model
    onLoop m f = case e.eventSeq of
        Just s | s <= m.modelLoopSeq -> m
        _ -> (f m){modelLoopSeq = fromMaybe m.modelLoopSeq e.eventSeq}

    -- update the node (unless the event is at or below its stamp), or
    -- drop it: a node wanted down that a pass has brought down is pruned
    -- by the loop after the pass, and /dag would no longer show it
    onNode :: RefId -> (Node -> Node) -> Model -> Model
    onNode r f m = case Map.lookup r m.modelNodes of
        Nothing -> m
        Just n
            | Just s <- e.eventSeq, s <= n.nodeSeq -> m
            | otherwise ->
                let n' = (f n){nodeSeq = fromMaybe n.nodeSeq e.eventSeq}
                 in if n'.nodeDirection == "down" && n'.nodeConvergence == "converged" && n'.nodeLastKind `elem` [Just "done", Just "acted done"]
                        then m{modelNodes = Map.delete r m.modelNodes, modelOrder = filter (/= r) m.modelOrder}
                        else m{modelNodes = Map.insert r n' m.modelNodes}

    touch :: Text -> Node -> Node
    touch k n = n{nodeLastKind = Just k, nodeLastSeq = e.eventSeq}

    -- the pass's verdict on a node, in /dag's convergence words
    settle :: Text -> Value -> Node -> Node
    settle k v n = updown k v (touch k n)

    updown :: Text -> Value -> Node -> Node
    updown k v n = case k of
        "done" -> n{nodeConvergence = "converged", nodeError = Nothing}
        "skip" -> n{nodeConvergence = "converged", nodeError = Nothing}
        "failed" -> n{nodeConvergence = "errored", nodeError = textAt ["error"] v}
        "blocked" -> n{nodeConvergence = "blocked"}
        _ -> n

-------------------------------------------------------------------------------
-- reading it

-- | The nodes in the snapshot's dependency order.
nodesInOrder :: Model -> [Node]
nodesInOrder m = mapMaybe (`Map.lookup` m.modelNodes) m.modelOrder

lookupNode :: RefId -> Model -> Maybe Node
lookupNode r m = Map.lookup r m.modelNodes

-- | How many lines of 'nodeOutput' and of 'nodeSaid' a node's view keeps.
outputKept :: Int
outputKept = 200

-- | How a @log@ line's channel shows in front of it: nothing for standard
-- output, the marks the text reporter and the web UI's tail use otherwise.
channelMark :: Maybe Text -> Text
channelMark c = case c of
    Just "stderr" -> "err| "
    Just "message" -> "msg| "
    _ -> ""

{- | The last @k@ lines a live tail of this node shows: what it said during
its one-shot @up@ when it said anything, its ring otherwise (a held
action's output, which is the only other thing that moves while one
watches).
-}
tailLines :: Int -> Node -> [Text]
tailLines k n = lastN (if null n.nodeSaid then n.nodeOutput else n.nodeSaid)
  where
    lastN xs = drop (length xs - k) xs

data Counts = Counts
    { countConverged :: !Int
    , countErrored :: !Int
    , countTotal :: !Int
    }
    deriving (Show, Eq)

counts :: Model -> Counts
counts m =
    foldl' tally (Counts 0 0 0) (Map.elems m.modelNodes)
  where
    tally :: Counts -> Node -> Counts
    tally (Counts c e t) n =
        Counts
            (c + fromEnum (n.nodeConvergence == "converged"))
            (e + fromEnum (n.nodeConvergence == "errored"))
            (t + 1)

-------------------------------------------------------------------------------
-- reading JSON, totally

asObject :: Value -> Maybe (KeyMap.KeyMap Value)
asObject (Object o) = Just o
asObject _ = Nothing

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

textAt :: [Text] -> Value -> Maybe Text
textAt ks v = at ks v >>= asText

numberAt :: [Text] -> Value -> Maybe Word64
numberAt ks v = case fromJSON <$> at ks v of
    Just (Success n) -> Just n
    _ -> Nothing

intAt :: [Text] -> Value -> Int
intAt ks v = maybe 0 fromIntegral (numberAt ks v)

boolAt :: [Text] -> Value -> Maybe Bool
boolAt ks v = case at ks v of
    Just (Bool b) -> Just b
    _ -> Nothing

arrayAt :: [Text] -> Value -> Maybe [Value]
arrayAt ks v = case at ks v of
    Just (Array xs) -> Just (toList xs)
    _ -> Nothing

refAt :: [Text] -> Value -> Maybe RefId
refAt ks v = RefId <$> textAt (ks ++ ["short"]) v <*> textAt (ks ++ ["full"]) v

checkAt :: [Text] -> Value -> Maybe Check
checkAt ks v = Check <$> textAt (ks ++ ["verdict"]) v <*> pure (textAt (ks ++ ["reason"]) v)

fromText :: Maybe Word64 -> Text
fromText = maybe "?" (Text.pack . show)
