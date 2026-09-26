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
import Control.Monad (void)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as Builder
import Data.Foldable (toList)
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
import Salmon.Reporter.Tagged (Tagged (..), originValue)

-------------------------------------------------------------------------------

data Config = Config
    { configRing :: !Int
    -- ^ how many events the ring keeps; at least one
    , configKeepAlive :: !Int
    -- ^ microseconds of silence before a subscriber is sent a comment
    -- line, so that proxies and clients with a read timeout do not drop
    -- an idle stream
    }
    deriving (Show, Eq)

-- | A ring in the low thousands, a keep-alive every fifteen seconds.
defaultConfig :: Config
defaultConfig = Config{configRing = 2048, configKeepAlive = 15 * 1000000}

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
    }

newEvents :: Config -> IO Events
newEvents cfg =
    Events cfg{configRing = max 1 cfg.configRing}
        <$> newTVarIO 0
        <*> newTVarIO Seq.empty
        <*> newBroadcastTChanIO
        <*> newTVarIO 0

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
