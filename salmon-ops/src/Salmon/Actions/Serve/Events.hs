{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The event stream behind @GET \/events@: one numbered, replayable record
of everything the loop and its machines report.

Milestone 4 of @specs\/generic-server.md@. "Salmon.Actions.Serve.Http"
mounts it; this module is the part with no HTTP in it — the counter, the
ring, the broadcast, and the reporter that feeds them — so that it can be
reasoned about (and tested) without a socket.

= One counter, one order

Every event carries a sequence number drawn from one counter per loop, and
so does every command @POST \/command@ queues (an @?async@ answer is that
number). The counter is taken, the ring appended and the broadcast written
in __one STM transaction__ ('publish'), which is what makes the number an
order rather than a label: the ring holds events in sequence order with no
holes, a client resuming from @?since=n@ gets exactly the events numbered
above @n@, and an @?async@ client waiting for "the reports of my command"
waits for events numbered above the one it was handed, stamped with its
origin.

The spec's open question asks that the number be taken under the same
'Control.Concurrent.MVar.MVar' the concurrent driver serialises
'runReporter' through, because 'Salmon.Actions.Upkeep' reports come from
machine threads while 'Salmon.Actions.Serve' and 'Salmon.Actions.UpDown'
reports come from the loop. That lock cannot be shared, as it turns out:
there is not one of it. "Salmon.Actions.Concurrent" makes a fresh
@reportLock@ inside every walk (two per convergence pass) and
"Salmon.Actions.Upkeep" makes its own in every supervisor, each local to
the function that made it. What each of them does hold while it calls
'runReporter' is /its/ lock, so a numbering reporter whose whole effect is
one transaction composes __under__ every one of them: within a driver,
report order and sequence order agree because the driver's lock serialises
its calls and each call's transaction is indivisible; across drivers and
threads, the transaction alone gives one total order. That is the answer
taken here — the numbering reporter is its own critical section (an STM
transaction, the moral equivalent of an 'Control.Concurrent.MVar.MVar' with
nothing else in it), and the drivers' locks compose over it.

= What is on the stream, and what is only here

Every 'Serve.Report' and 'UpDown.Report' the loop's reporters see, stamped
with the 'Origin' of the command being handled when there is one; and the
tending machines' 'Upkeep.Report's, which the loop hands its reporter
wrapped as 'Serve.Tended' between commands and which are unwrapped here to
the @upkeep@ stream they came from. __This stream is the only place a
client can see the machines at work__: a synchronous @POST \/command@ answers
with the reports stamped for that command, and tending happens exactly when
no command is being handled, so its reports have no origin and no request
to be answered to. A @\/dag@ or @\/status@ read is a snapshot that is at most
one command old (see "Salmon.Actions.Serve.Http"); the motion between
commands is here or nowhere.

Two events are the server's own rather than a report, under
@stream: "server"@: an @enqueued@ event for every command the HTTP surface
queues (which is what keeps the numbering dense — the number an @?async@
answer carries is an event in the ring like any other), and a @gap@ event,
sent first to a client whose @?since@ has fallen off the ring, so a
resumption never skips silently.

= The ring

A bounded 'Seq' of the newest events, oldest dropped first. It bounds
memory rather than promising history: a client that stays connected misses
nothing, a client that reconnects promptly misses nothing, and a client
that comes back after more events than the ring holds is told so. The size
is @run serve --events-ring N@.

= Per-node tails

Beside the ring, every event about one node ('Tagged.taggedNode') is kept in
that node's own tail, in the same transaction: the newest
'configTailOutput' of its @output@ stream (what a held action wrote, what a
node said while its @up@ ran) and, apart, the newest 'configTailReports' of
the rest (@updown@ and @upkeep@ reports), so a node polled every few seconds
does not push a build's output out with its @next-look@s. The ring is shared
by every node and turns over in minutes on a busy loop; a tail is what a
client opening (or reloading) a view of /one/ node is seeded from, and
@\/dag@ carries it per node ('snapshotTails'). Read with the sequence number
in one transaction, a tail holds exactly the events numbered up to that
number, so a client seeded from it and following @?since=@ that number sees
every line once. At most 'configTailNodes' nodes keep a tail; past that the
one quietest for longest is dropped, which bounds what a long-running loop
whose declarations keep minting new refs holds on to.
-}
module Salmon.Actions.Serve.Events (
    -- * The record
    Events,
    eventsConfig,
    Config (..),
    defaultConfig,
    newEvents,
    Event (..),
    Body (..),

    -- * Per-node tails
    Tails,
    emptyTails,
    Lane (..),
    laneOf,
    recordTail,
    tailOf,
    tailNodes,
    snapshotTails,

    -- * Feeding it
    publish,
    enqueued,
    eventsReporter,
    lastSequence,

    -- * Reading it
    Subscription (..),
    withSubscription,
    subscribers,
    Filter (..),
    noFilter,
    matches,
    streamOf,

    -- * The wire
    eventValue,
    gapValue,
    renderEvent,
    renderGap,
    keepAlive,
) where

import Control.Concurrent.STM (STM, TChan, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChanIO, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.Exception (bracket_)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)

import qualified Salmon.Actions.Upkeep as Upkeep
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), Origin)
import Salmon.Reporter
import Salmon.Op.Ref (Ref)
import Salmon.Reporter.Tagged (Tagged (..), originValue, taggedNode)

-------------------------------------------------------------------------------

data Config = Config
    { configRing :: !Int
    -- ^ how many events the ring keeps; at least one
    , configKeepAlive :: !Int
    -- ^ microseconds of silence before a subscriber is sent a comment
    -- line, so that proxies and clients with a read timeout do not drop
    -- an idle stream
    , configTailOutput :: !Int
    -- ^ per node, how many of its @output@-stream events its tail keeps
    , configTailReports :: !Int
    -- ^ per node, how many of its other events (@updown@, @upkeep@) its
    -- tail keeps
    , configTailNodes :: !Int
    -- ^ how many nodes keep a tail at all; see "Per-node tails"
    }
    deriving (Show, Eq)

-- | A ring in the low thousands, a keep-alive every fifteen seconds, and
-- per node the 256 lines of output a machine's own ring also keeps
-- ("Salmon.Op.Status") with 64 reports beside them.
defaultConfig :: Config
defaultConfig =
    Config
        { configRing = 2048
        , configKeepAlive = 15 * 1000000
        , configTailOutput = 256
        , configTailReports = 64
        , configTailNodes = 4096
        }

-- | What an event is about.
data Body
    = -- | a report from one of the three streams
      Reported !Tagged
    | -- | a command the HTTP surface queued: the line, as typed
      Enqueued !String
    deriving (Show)

data Event = Event
    { eventSeq :: !Word64
    , eventOrigin :: !(Maybe Origin)
    -- ^ the command this belongs to: the origin of the line being handled
    -- when it was reported, or the origin a command was queued under
    , eventBody :: !Body
    }
    deriving (Show)

-- | The record: counter, ring and broadcast, all written in one transaction.
data Events = Events
    { eventsConfig :: !Config
    , eventsLast :: !(TVar Word64)
    -- ^ the last number handed out; 0 before the first
    , eventsRing :: !(TVar (Seq Event))
    -- ^ the newest 'configRing' events, oldest first, contiguous
    , eventsChan :: !(TChan Event)
    -- ^ broadcast; a subscriber reads a 'dupTChan' of it
    , eventsSubscribers :: !(TVar Int)
    , eventsTails :: !(TVar Tails)
    -- ^ per node, the newest events about it; see "Per-node tails"
    }

newEvents :: Config -> IO Events
newEvents cfg =
    Events cfg{configRing = max 1 cfg.configRing}
        <$> newTVarIO 0
        <*> newTVarIO Seq.empty
        <*> newBroadcastTChanIO
        <*> newTVarIO 0
        <*> newTVarIO emptyTails

-------------------------------------------------------------------------------
-- per-node tails

-- | Which of a node's two bounded queues an event goes to.
data Lane
    = -- | the @output@ stream: lines
      OutputLane
    | -- | everything else about the node: reports
      ReportLane
    deriving (Show, Eq, Ord)

-- | The node an event is about and the lane it goes to, if it is about one.
laneOf :: Body -> Maybe (Ref, Lane)
laneOf body = case body of
    Reported tagged -> (\r -> (r, if streamOf body == "output" then OutputLane else ReportLane)) <$> taggedNode tagged
    Enqueued _ -> Nothing

data NodeTail = NodeTail
    { tailNewest :: !Word64
    -- ^ the number of the newest event kept, which eviction goes by
    , tailOutput :: !(Seq Event)
    , tailReports :: !(Seq Event)
    }

-- | Every node's tail. Pure, so that it can be checked without a loop.
newtype Tails = Tails (Map Ref NodeTail)

emptyTails :: Tails
emptyTails = Tails Map.empty

{- | Keep an event in a node's tail, under the limits a 'Config' sets: the
lane's oldest is dropped past its bound and, a node new to the tails
pushing them past 'configTailNodes', the node whose newest event is the
oldest goes. Events are expected in increasing number, as 'publish' hands
them out.
-}
recordTail :: Config -> Ref -> Lane -> Event -> Tails -> Tails
recordTail cfg r lane e (Tails m) = Tails (evict (Map.alter (Just . add) r m))
  where
    add :: Maybe NodeTail -> NodeTail
    add prior =
        let t = maybe (NodeTail 0 Seq.empty Seq.empty) id prior
         in case lane of
                OutputLane -> t{tailNewest = e.eventSeq, tailOutput = bounded cfg.configTailOutput (t.tailOutput |> e)}
                ReportLane -> t{tailNewest = e.eventSeq, tailReports = bounded cfg.configTailReports (t.tailReports |> e)}

    bounded :: Int -> Seq Event -> Seq Event
    bounded k q = Seq.drop (Seq.length q - max 0 k) q

    evict :: Map Ref NodeTail -> Map Ref NodeTail
    evict m'
        | Map.size m' <= max 1 cfg.configTailNodes = m'
        | otherwise = case Map.foldrWithKey quietest Nothing (Map.delete r m') of
            Just (victim, _) -> evict (Map.delete victim m')
            Nothing -> m'

    quietest :: Ref -> NodeTail -> Maybe (Ref, Word64) -> Maybe (Ref, Word64)
    quietest k t acc = case acc of
        Just (_, n) | n <= t.tailNewest -> acc
        _ -> Just (k, t.tailNewest)

-- | A node's tail, both lanes, oldest first.
tailOf :: Ref -> Tails -> [Event]
tailOf r (Tails m) = case Map.lookup r m of
    Nothing -> []
    Just t -> sortOn (.eventSeq) (toList t.tailOutput <> toList t.tailReports)

-- | The nodes that have a tail.
tailNodes :: Tails -> [Ref]
tailNodes (Tails m) = Map.keys m

{- | The last number handed out and every node's tail, read together: the
tails then hold exactly the events numbered up to it, so a client seeded
from them follows @?since=@ that number and sees each event once.
-}
snapshotTails :: Events -> IO (Word64, Tails)
snapshotTails ev = atomically ((,) <$> readTVar ev.eventsLast <*> readTVar ev.eventsTails)

-------------------------------------------------------------------------------
-- feeding

{- | Number a body, keep it, broadcast it: one transaction, see the module
header. Returns the number.
-}
publish :: Events -> Maybe Origin -> Body -> IO Word64
publish ev origin body = atomically $ do
    n <- (+ 1) <$> readTVar ev.eventsLast
    writeTVar ev.eventsLast n
    let e = Event n origin body
    modifyTVar' ev.eventsRing $ \ring ->
        let ring' = ring |> e
         in if Seq.length ring' > ev.eventsConfig.configRing then Seq.drop 1 ring' else ring'
    forM_ (laneOf body) $ \(r, lane) ->
        modifyTVar' ev.eventsTails (recordTail ev.eventsConfig r lane e)
    writeTChan ev.eventsChan e
    pure n

-- | The event for a command queued under an origin; returns its number.
enqueued :: Events -> Origin -> String -> IO Word64
enqueued ev origin line = publish ev (Just origin) (Enqueued line)

{- | The reporter to compose beside the loop's own. A 'Serve.Tended' report
is published as the 'Upkeep.Report' inside it: the wrapper exists so that
the loop can forward the machines' stream through its one reporter, and the
stream tag says the same thing on the wire.
-}
eventsReporter :: Events -> Reporter (Attributed Tagged)
eventsReporter ev = ReporterM $ \(Attributed origin tagged) ->
    void (publish ev origin (Reported (unwrap tagged)))
  where
    unwrap (FromServe (Serve.Tended inner)) = FromUpkeep inner
    unwrap t = t

{- | The last number handed out, for a snapshot to carry: everything that
happens after a read of this is numbered above it. A read that pairs this
with a snapshot takes this __first__, so that an event landing between the
two is replayed rather than skipped — a client applies it twice, which is
the safe direction for a state event, instead of never.
-}
lastSequence :: Events -> IO Word64
lastSequence ev = readTVarIO ev.eventsLast

-------------------------------------------------------------------------------
-- reading

-- | A replay and a live feed, taken in one transaction so nothing is
-- between them.
data Subscription = Subscription
    { subscriptionGap :: !(Maybe Word64)
    -- ^ 'Just' the oldest number still in the ring, when the events just
    -- after @since@ are no longer there
    , subscriptionReplay :: ![Event]
    -- ^ the events numbered above @since@ that the ring still has
    , subscriptionLive :: !(STM Event)
    -- ^ the next event after those; blocks
    }

{- | Subscribe from a point: 'Nothing' for live only, @'Just' n@ for
everything after @n@ (@0@ is the whole ring). The subscriber count is
kept for as long as the action runs, however it ends — a client
disconnecting is an exception out of its stream, and that is the cleanup.
-}
withSubscription :: Events -> Maybe Word64 -> (Subscription -> IO a) -> IO a
withSubscription ev since act =
    bracket_
        (atomically (modifyTVar' ev.eventsSubscribers (+ 1)))
        (atomically (modifyTVar' ev.eventsSubscribers (subtract 1)))
        (atomically subscribe >>= act)
  where
    subscribe :: STM Subscription
    subscribe = do
        chan <- dupTChan ev.eventsChan
        ring <- readTVar ev.eventsRing
        let (gap, replay) = case since of
                Nothing -> (Nothing, [])
                Just n ->
                    let after = toList (Seq.dropWhileL (\e -> e.eventSeq <= n) ring)
                     in case Seq.lookup 0 ring of
                            -- the ring is contiguous, so the first event
                            -- after `n` is missing iff the oldest one kept
                            -- is already past it
                            Just oldest | oldest.eventSeq > n + 1 -> (Just oldest.eventSeq, after)
                            _ -> (Nothing, after)
        pure
            Subscription
                { subscriptionGap = gap
                , subscriptionReplay = replay
                , subscriptionLive = readTChan chan
                }

-- | How many subscriptions are open right now.
subscribers :: Events -> IO Int
subscribers ev = readTVarIO ev.eventsSubscribers

{- | What a client asked to see: 'Nothing' is everything. Server-side, since
the spec's "clients filter" is right about who decides and wrong about who
pays — a terminal client over a slow link wants less on the wire.
-}
data Filter = Filter
    { filterStreams :: !(Maybe (Set Text))
    -- ^ @serve@, @updown@, @upkeep@, @server@
    , filterOrigins :: !(Maybe (Set Text))
    -- ^ by 'Serve.originName'
    }
    deriving (Show, Eq)

noFilter :: Filter
noFilter = Filter Nothing Nothing

matches :: Filter -> Event -> Bool
matches f e =
    maybe True (Set.member (streamOf e.eventBody)) f.filterStreams
        && maybe True (\os -> maybe False (\o -> Set.member (Serve.originName o) os) e.eventOrigin) f.filterOrigins

-- | The @stream@ an event is filed under: the 'Tagged' one, or @server@.
streamOf :: Body -> Text
streamOf body = case body of
    Reported (FromServe _) -> "serve"
    Reported (FromUpDown _) -> "updown"
    Reported (FromUpkeep (Upkeep.Output _ _)) -> "output"
    Reported (FromUpkeep _) -> "upkeep"
    Reported (FromFollow _) -> "follow"
    Reported (FromNode _) -> "output"
    Enqueued _ -> "server"

-------------------------------------------------------------------------------
-- the wire

{- | The 'Tagged' object with @seq@ added, and @origin@ (the object
@history@ entries use) when the event belongs to a command; an @enqueued@
event is @{"stream":"server","kind":"enqueued","line":...}@ with the same
two.
-}
eventValue :: Event -> Value
eventValue e = withSeq (withOrigin base)
  where
    base = case e.eventBody of
        Reported tagged -> toJSON tagged
        Enqueued line -> object ["stream" .= streamOf e.eventBody, "kind" .= ("enqueued" :: Text), "line" .= line]
    withSeq = insert "seq" (toJSON e.eventSeq)
    withOrigin v = maybe v (\o -> insert "origin" (originValue o) v) e.eventOrigin
    insert k v (Object o) = Object (KeyMap.insert k v o)
    insert k v other = object [k .= v, "report" .= other]

-- | The synthetic event a resumption that fell off the ring is sent first:
-- @from@ is the oldest number the replay then starts at.
gapValue :: Word64 -> Value
gapValue oldest = object ["kind" .= ("gap" :: Text), "from" .= oldest, "stream" .= ("server" :: Text)]

-- | One SSE event: its number as @id@, its object as @data@.
renderEvent :: Event -> Builder.Builder
renderEvent e =
    "id: " <> Builder.word64Dec e.eventSeq <> "\ndata: " <> Builder.lazyByteString (encode (eventValue e)) <> "\n\n"

-- | The gap event, with no @id@: a client resumes from the last real one.
renderGap :: Word64 -> Builder.Builder
renderGap oldest = "data: " <> Builder.lazyByteString (encode (gapValue oldest)) <> "\n\n"

-- | An SSE comment line, which a client ignores and a proxy sees as traffic.
keepAlive :: Builder.Builder
keepAlive = Builder.byteString ": keep-alive\n\n"
