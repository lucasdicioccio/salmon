{-# LANGUAGE OverloadedStrings #-}

{- | The client's read model: a @\/dag@ snapshot with the event stream folded
onto it, one view per node.

Milestone 6 of @specs\/generic-server.md@ says a client rebuilds the current
state as @dag ⊕ events since the dag's sequence number@. This module is
that ⊕, and nothing else: no socket, no terminal. "Salmon.Client.Http" is
what fetches the two inputs, and a terminal client (@salmon-tui@ in
@salmon-apps@) is a thin rendering of the 'Model' this produces. Keeping
the fold pure is what makes it testable against a recorded event sequence
('Test.ClientModelSpec'), and it is also what keeps the client honest about
the spec's design constraint — __the client holds no state the server does
not__: everything here is derived from @\/dag@ and @\/events@, so a restart
of the client is one @\/dag@ read, and a client that has fallen behind
('modelResync') re-reads rather than guessing.

= What is folded, and what is not

The inputs are the wire objects "Salmon.Reporter.Tagged" and
"Salmon.Actions.Serve.Events" describe, read as 'Aeson.Value': the model
takes every event as data (an 'Event' is what 'eventOf' can see in the
object — @seq@, @stream@, @kind@, @ref@, @origin@ — plus the object itself)
rather than decoding the four report sums back into Haskell. That is
deliberate: the client is generic over salmon binaries the way the server
is, and it should keep rendering a report kind it was not written for
(as its 'nodeLastKind') instead of failing to decode it.

What moves a node's view:

  * the @updown@ stream (and an @upkeep@ @acted@ wrapping one): @eval@ is
    the node being worked on, @done@ and @skip@ make it @converged@ in its
    direction, @failed@ makes it @errored@ with the error kept, @blocked@
    makes it @blocked@ — the same words @\/dag@ uses for @convergence@;
  * the @upkeep@ stream: @next-look@ carries the node's own last check
    verdict, which is the one thing the snapshot cannot keep fresh (a
    @\/dag@ read is at most one command old, and tending happens between
    commands); every other kind is recorded as the node's last event;
  * the @serve@ stream: @converge-start@\/@converge-stop@ for the loop's
    pass, @supervised@, and the two that change the __shape__ of the graph
    — @declared@ and @cleared@ — which set 'modelResync', because an event
    names nodes by 'Ref' and a declaration adds or retires nodes the model
    has never seen.

A node wanted @down@ whose @done@ arrives is dropped: the loop prunes it
after the pass, and @\/dag@ would not show it either.

= Replays are safe

Every step is a state update keyed on the node, never an increment, so
an event applied twice leaves the model where one application did. That
is what the snapshot's @seq@ ordering relies on ("Salmon.Actions.Serve.Http"
reads the number before the world so a racing event is replayed rather
than skipped), and it is what lets a client keep folding the live stream
while a re-read snapshot is on its way.
-}
module Salmon.Client.Model (
    -- * Events
    Event (..),
    eventOf,
    RefId (..),

    -- * The model
    Model (..),
    Node (..),
    Check (..),
    Pass (..),
    fromDag,
    step,
    modelResync,
    resolve,

    -- * Reading it
    nodesInOrder,
    Counts (..),
    counts,
    lookupNode,

    -- * Rendering pieces
    renderNodeRow,
    renderHeader,
    renderEventLine,
    describeEvent,
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
    -- ^ the snapshot's output ring, oldest first
    , nodeError :: !(Maybe Text)
    -- ^ the last @failed@'s error, cleared by a later @done@\/@skip@
    , nodeLastKind :: !(Maybe Text)
    -- ^ the kind of the last event about this node, and its stream
    , nodeLastSeq :: !(Maybe Word64)
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
    -- ^ the highest sequence number seen: the snapshot's, then each event's
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

{- | A model from a @\/dag@ answer. 'Left' names what is missing; a @\/dag@
answer always has @nodes@ and @seq@, so a 'Left' is a wrong URL, not a
version skew.
-}
fromDag :: Value -> Either String Model
fromDag v = do
    nodes <- maybe (Left "/dag answer has no nodes array") Right (arrayAt ["nodes"] v)
    seqNo <- maybe (Left "/dag answer carries no seq") Right (numberAt ["seq"] v)
    parsed <- traverse nodeOf nodes
    pure
        Model
            { modelMode = fromMaybe "?" (textAt ["mode"] v)
            , modelSeq = seqNo
            , modelOrder = fmap nodeRef parsed
            , modelNodes = Map.fromList [(nodeRef n, n) | n <- parsed]
            , modelPass = Nothing
            , modelSupervised = Nothing
            , modelLast = Nothing
            , modelResyncReason = Nothing
            }
  where
    nodeOf :: Value -> Either String Node
    nodeOf n = do
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
                , nodeError = Nothing
                , nodeLastKind = Nothing
                , nodeLastSeq = Nothing
                , nodeDependencies = maybe [] (mapMaybe (refAt [])) (arrayAt ["dependencies"] n)
                , nodeDependants = maybe [] (mapMaybe (refAt [])) (arrayAt ["dependants"] n)
                , nodePaths = maybe [] (mapMaybe asText) (arrayAt ["paths"] n)
                }

{- | Fold one event in. Total, and idempotent per event (see the module
header): the seq moves to the event's if higher, the node the event is
about is updated, and the loop-level fields follow the @serve@ stream.
-}
step :: Model -> Event -> Model
step m0 e = byStream (m0{modelSeq = maybe m0.modelSeq (max m0.modelSeq) e.eventSeq, modelLast = Just e})
  where
    byStream m = case (e.eventStream, e.eventKind) of
        ("server", "gap") -> m{modelResyncReason = Just ("events " <> fromText (numberAt ["from"] e.eventValue) <> " fell off the ring")}
        ("server", _) -> m
        ("serve", "declared") ->
            m{modelResyncReason = Just ("epoch " <> fromText (numberAt ["epoch"] e.eventValue) <> " declared " <> fromMaybe "?" (textAt ["direction"] e.eventValue))}
        ("serve", "cleared") -> m{modelResyncReason = Just "every seed retired"}
        ("serve", "converge-start") ->
            m{modelPass = Just (Converging (intAt ["down"] e.eventValue) (intAt ["up"] e.eventValue))}
        ("serve", "converge-stop") ->
            m{modelPass = Just (Stopped (fromMaybe False (boolAt ["ok"] e.eventValue)) (intAt ["remaining"] e.eventValue))}
        ("serve", "supervised") -> m{modelSupervised = boolAt ["on"] e.eventValue}
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
        _ -> m

    -- update the node, or drop it: a node wanted down that a pass has
    -- brought down is pruned by the loop after the pass, and /dag would
    -- no longer show it
    onNode :: RefId -> (Node -> Node) -> Model -> Model
    onNode r f m = case Map.lookup r m.modelNodes of
        Nothing -> m
        Just n ->
            let n' = f n
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
-- rendering pieces: the text a terminal shows, kept here so it is testable

{- | One line per node, the columns @salmon-tui@ shows: ref, shorthand,
direction, convergence, last check, last event. Widths are fixed for the
first four so the table reads as one; the last two are open-ended.
-}
renderNodeRow :: Node -> Text
renderNodeRow n =
    Text.unwords
        [ Text.justifyLeft 10 ' ' n.nodeRef.refShort
        , Text.justifyLeft 22 ' ' (Text.take 22 n.nodeShorthand)
        , Text.justifyLeft 4 ' ' n.nodeDirection
        , Text.justifyLeft 9 ' ' n.nodeConvergence
        , Text.justifyLeft 12 ' ' (maybe "-" renderCheck n.nodeCheck)
        , lastEvent
        ]
  where
    lastEvent = case (n.nodeLastKind, n.nodeLastSeq) of
        (Nothing, _) -> "-"
        (Just k, ms) -> k <> maybe "" (\s -> " #" <> Text.pack (show s)) ms <> maybe "" (": " <>) n.nodeError

renderCheck :: Check -> Text
renderCheck c = c.checkVerdict

-- | The header line: socket, mode, seq, the pass, and the counts.
renderHeader :: Text -> Model -> Text
renderHeader socket m =
    Text.unwords
        [ socket
        , "mode=" <> m.modelMode
        , "seq=" <> Text.pack (show m.modelSeq)
        , "converged=" <> tshow c.countConverged
        , "errored=" <> tshow c.countErrored
        , "total=" <> tshow c.countTotal
        , maybe "" renderPass m.modelPass
        , maybe "" (\on -> if on then "supervising" else "not supervising") m.modelSupervised
        ]
  where
    c = counts m
    tshow = Text.pack . show
    renderPass (Converging d u) = "converging(" <> tshow d <> " down, " <> tshow u <> " up)"
    renderPass (Stopped ok left) = (if left == 0 then "converged" else "incomplete(" <> tshow left <> " left)") <> (if ok then "" else "+failure")

-- | An event as one line: its number, stream, kind, and what it is about.
renderEventLine :: Event -> Text
renderEventLine e =
    Text.unwords
        [ maybe "#-" (\s -> "#" <> Text.pack (show s)) e.eventSeq
        , e.eventStream
        , e.eventKind
        , describeEvent e
        ]

-- | The words after the kind: the node's short ref, or the loop-level
-- fields worth a glance.
describeEvent :: Event -> Text
describeEvent e = case e.eventRef of
    Just r -> r.refShort <> maybe "" (" " <>) (textAt ["node", "shorthand"] e.eventValue)
    Nothing -> Text.unwords (mapMaybe (\k -> (\t -> k <> "=" <> t) <$> scalarAt [k] e.eventValue) ["line", "epoch", "direction", "nodes", "down", "up", "ok", "remaining", "from", "error", "on"])

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

scalarAt :: [Text] -> Value -> Maybe Text
scalarAt ks v = case at ks v of
    Just (String t) -> Just t
    Just (Number n) -> Just $ Text.pack $ case fromJSON (Number n) of
        Success (i :: Integer) -> show i
        _ -> show n
    Just (Bool b) -> Just (if b then "true" else "false")
    _ -> Nothing

fromText :: Maybe Word64 -> Text
fromText = maybe "?" (Text.pack . show)
