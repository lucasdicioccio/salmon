{-# LANGUAGE OverloadedStrings #-}

{- | The scheduler behind "Salmon.Actions.Follow": /when/ the fetcher asks the
registry, and /when/ what it found reaches the loop. Two jobs that point in
opposite directions, per @specs/pull-mode.md@.

__Toward the registry: a ladder.__ A round that succeeds — changed or not —
schedules the next one at 'schedBase'; a round that fails (the registry
threw, or answered with bytes that do not parse) schedules it at
@min cap (base · factor^(n-1))@ after @n@ consecutive failures, so the first
retry comes no later than a success's next round and every one after it is
slower, up to 'schedCap'. Every delay is jittered by up to 'schedJitter' of
itself, either way, so a fleet that rebooted together does not poll
together. There is no reason to slow down while quiet: an unchanged round is
a success.

__Toward the loop: a quiet window.__ A change is not injected at once. It
opens a window of 'schedDebounce'; a further change inside the window
restarts it (the pending batch is whatever the /latest/ document says,
diffed against the last one /applied/); the batch is injected once the
registry has been quiet for that long, or once 'schedMaxWait' has elapsed
since the first pending change, whichever comes first. This is what turns a
publisher writing three times in a row into one convergence pass, and what
keeps a half-published state from being applied — and it is the starvation
rule of "Salmon.Actions.Follow" restated as a rate: every injection stands
the supervisor's machines down, so how often that can happen is bounded by
the window, not by the poll.

__The @fetch@ command__ is the one place inbound events touch this: 'poked'
schedules a round now, forgets the failure count, and marks whatever is
pending (before or after that round) to be injected as soon as the round is
over — an operator who just published does not want to wait out either
ladder.

The whole thing is a pure step function over a small 'Sched' ('observed',
'poked', 'injected', and 'next' to ask what is due), which is what the
tests table-test, plus one 'IO' loop ('run') around it that takes its clock
from the caller, so that a test moves time rather than waiting for it.
Its randomness is a seeded 'Rng' for the same reason.

"Salmon.Actions.Upkeep" has a ladder of the same shape (double to a cap,
halve to a floor), but that one is a per-node question about /checks/; this
one is per-registry about /fetches/, and the two share nothing on purpose.
-}
module Salmon.Actions.Follow.Scheduler (
    -- * Configuration
    Micros,
    Config (..),
    defaultConfig,
    renderConfig,

    -- * The pure step
    Sched,
    schedFailures,
    schedNextFetch,
    schedPending,
    Pending (..),
    Outcome (..),
    Action (..),
    Due (..),
    start,
    observed,
    poked,
    injected,
    next,
    ladder,

    -- * Jitter
    Rng,
    mkRng,
    jittered,

    -- * The loop
    Clock (..),
    Wake (..),
    Poke,
    newPoke,
    poke,
    systemClock,
    Hooks (..),
    run,
) where

import Control.Concurrent.STM (TVar, atomically, check, newTVarIO, orElse, readTVar, registerDelay, writeTVar)
import Data.Bits (shiftR, xor)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-------------------------------------------------------------------------------
-- configuration

-- | Microseconds, the unit 'Control.Concurrent.threadDelay' takes.
type Micros = Int

data Config = Config
    { schedBase :: !Micros
    -- ^ the delay after a successful round, and the ladder's first rung
    , schedFactor :: !Double
    -- ^ how much slower each consecutive failure makes the next round
    , schedCap :: !Micros
    -- ^ the ladder's ceiling
    , schedJitter :: !Double
    -- ^ every delay is scaled by a uniform draw from @[1 - j, 1 + j]@
    , schedDebounce :: !Micros
    -- ^ how long the registry must be quiet after a change before it is injected
    , schedMaxWait :: !Micros
    -- ^ the longest a change waits, quiet or not
    }
    deriving (Show, Eq)

{- | The spec's defaults: tens of seconds toward the registry, a few seconds
toward the loop. -}
defaultConfig :: Config
defaultConfig =
    Config
        { schedBase = 30 * second
        , schedFactor = 2
        , schedCap = 10 * 60 * second
        , schedJitter = 0.2
        , schedDebounce = 5 * second
        , schedMaxWait = 60 * second
        }
  where
    second = 1000000

-- | One line, for a report.
renderConfig :: Config -> Text
renderConfig cfg =
    Text.concat
        [ "every "
        , secs cfg.schedBase
        , " (on failure x"
        , Text.pack (show cfg.schedFactor)
        , " up to "
        , secs cfg.schedCap
        , ", jitter "
        , Text.pack (show (round (cfg.schedJitter * 100) :: Int))
        , "%; a change waits "
        , secs cfg.schedDebounce
        , " of quiet, at most "
        , secs cfg.schedMaxWait
        , ")"
        ]
  where
    secs us = Text.pack (show (us `div` 1000000)) <> "s"

-------------------------------------------------------------------------------
-- jitter

{- | A splitmix64 generator, written out rather than pulled in as a
dependency: two lines of arithmetic are all a jitter needs, and a test that
wants the same draws twice seeds it with 'mkRng'. -}
newtype Rng = Rng Word64
    deriving (Show, Eq)

mkRng :: Word64 -> Rng
mkRng = Rng

nextWord :: Rng -> (Word64, Rng)
nextWord (Rng s) =
    let s' = s + 0x9e3779b97f4a7c15
        z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
     in (z2 `xor` (z2 `shiftR` 31), Rng s')

-- | A uniform draw from @[0, 1)@.
unit :: Rng -> (Double, Rng)
unit g =
    let (w, g') = nextWord g
     in (fromIntegral (w `shiftR` 11) / 9007199254740992, g')

-- | Scale a delay by a uniform draw from @[1 - j, 1 + j]@; the identity at @j = 0@.
jittered :: Config -> Rng -> Micros -> (Micros, Rng)
jittered cfg g us
    | cfg.schedJitter <= 0 = (us, g)
    | otherwise =
        let (u, g') = unit g
            scale = 1 - cfg.schedJitter + 2 * cfg.schedJitter * u
         in (max 0 (round (fromIntegral us * scale)), g')

-------------------------------------------------------------------------------
-- the pure step

-- | What one round of fetching every label amounted to.
data Outcome
    = -- | every label answered and none moved
      Unchanged
    | -- | every label answered and at least one moved: something is pending
      Changed
    | -- | at least one label could not be read
      Failed
    deriving (Show, Eq)

-- | A change waiting for its window: when it was first seen, and when last.
data Pending = Pending
    { pendingSince :: !Micros
    , pendingLast :: !Micros
    }
    deriving (Show, Eq)

data Sched = Sched
    { schedFailures :: !Int
    -- ^ consecutive failed rounds
    , schedNextFetch :: !Micros
    -- ^ when the next round is due
    , schedPending :: !(Maybe Pending)
    , schedFlush :: !(Maybe Micros)
    -- ^ a @fetch@ came in at this time: inject what is pending without
    -- waiting out the window (kept until an injection, or until a round
    -- ends with nothing pending)
    , schedLastInjection :: !(Maybe Micros)
    , schedRng :: !Rng
    }
    deriving (Show)

-- | What to do next, and when.
data Action = Fetch | Inject
    deriving (Show, Eq)

data Due = Due
    { dueAt :: !Micros
    , dueAction :: !Action
    }
    deriving (Show, Eq)

{- | A scheduler whose first round has just succeeded at @now@ — the
synchronous one "Salmon.Actions.Follow" runs at startup, whose result is
injected without any window (there is nothing to coalesce yet, and the first
convergence is meant to be deterministic). The next round is one base delay
away. -}
start :: Config -> Rng -> Micros -> Sched
start cfg g now =
    observed
        cfg
        now
        Unchanged
        Sched
            { schedFailures = 0
            , schedNextFetch = now
            , schedPending = Nothing
            , schedFlush = Nothing
            , schedLastInjection = Nothing
            , schedRng = g
            }

-- | The ladder's rung after this many consecutive failures, before jitter.
ladder :: Config -> Int -> Micros
ladder cfg n
    | n <= 1 = cfg.schedBase
    | otherwise =
        let raw = fromIntegral cfg.schedBase * cfg.schedFactor ^^ (n - 1) :: Double
         in if raw >= fromIntegral cfg.schedCap then cfg.schedCap else round raw

-- | A round just ended, at @now@, with this outcome.
observed :: Config -> Micros -> Outcome -> Sched -> Sched
observed cfg now outcome st =
    st
        { schedFailures = failures
        , schedNextFetch = now + delay
        , schedPending = pending
        , -- a flush with nothing behind it has nothing left to do
          schedFlush = if isJust pending then st.schedFlush else Nothing
        , schedRng = g'
        }
  where
    failures = case outcome of
        Failed -> st.schedFailures + 1
        _ -> 0
    pending = case outcome of
        Changed -> Just (maybe (Pending now now) (\p -> p{pendingLast = now}) st.schedPending)
        _ -> st.schedPending
    (delay, g') = jittered cfg st.schedRng (ladder cfg failures)

{- | A @fetch@ came in at @now@: the next round is due now, the ladder is
forgotten, and whatever is pending once that round is over goes in at once. -}
poked :: Micros -> Sched -> Sched
poked now st = st{schedFailures = 0, schedNextFetch = now, schedFlush = Just now}

-- | The pending batch was injected at @now@.
injected :: Micros -> Sched -> Sched
injected now st = st{schedPending = Nothing, schedFlush = Nothing, schedLastInjection = Just now}

{- | What is due next. A round and an injection due at the same instant go
round first: a @fetch@ makes both due now, and the point of it is to inject
what that round finds, not what was found before it. -}
next :: Config -> Sched -> Due
next cfg st =
    case injectionDue of
        Just at | at < st.schedNextFetch -> Due at Inject
        _ -> Due st.schedNextFetch Fetch
  where
    injectionDue = do
        p <- st.schedPending
        pure $ case st.schedFlush of
            Just at -> at
            Nothing -> min (p.pendingLast + cfg.schedDebounce) (p.pendingSince + cfg.schedMaxWait)

-------------------------------------------------------------------------------
-- the loop

-- | Why a wait ended.
data Wake = Elapsed | Poked
    deriving (Show, Eq)

{- | Where the loop gets its time. 'systemClock' is the real one; a test
supplies one whose 'clockWaitUntil' blocks until the test moves the clock. -}
data Clock = Clock
    { clockNow :: IO Micros
    , clockWaitUntil :: Micros -> IO Wake
    -- ^ return at the given time, or earlier if poked
    }

-- | The control channel a @fetch@ command pulls: one flag, set by 'poke'
-- and consumed by the next wait.
newtype Poke = Poke (TVar Bool)

newPoke :: IO Poke
newPoke = Poke <$> newTVarIO False

poke :: Poke -> IO ()
poke (Poke v) = atomically (writeTVar v True)

-- | The monotonic clock, sleeping in STM so a 'poke' can cut the sleep short.
systemClock :: Poke -> Clock
systemClock (Poke pokeVar) = Clock now waitUntil
  where
    now = fmap (\ns -> fromIntegral (ns `div` 1000)) getMonotonicTimeNSec
    waitUntil at = do
        t <- now
        if at <= t
            then pure Elapsed
            else do
                elapsedVar <- registerDelay (at - t)
                atomically $
                    (readTVar elapsedVar >>= check >> pure Elapsed)
                        `orElse` (readTVar pokeVar >>= check >> writeTVar pokeVar False >> pure Poked)

-- | What the loop does when something is due.
data Hooks = Hooks
    { hookFetch :: IO Outcome
    -- ^ one round: ask the registry for every label
    , hookInject :: IO ()
    -- ^ inject whatever is pending
    , hookBackoff :: Int -> Micros -> IO ()
    -- ^ a round failed: consecutive failures, and how long until the next one
    }

-- | Never returns; the caller kills the thread.
run :: Config -> Clock -> Hooks -> Sched -> IO ()
run cfg clock hooks = go
  where
    go st = do
        let due = next cfg st
        wake <- clockWaitUntil clock due.dueAt
        now <- clockNow clock
        st' <- case wake of
            Poked -> pure (poked now st)
            Elapsed -> case due.dueAction of
                Inject -> do
                    hookInject hooks
                    pure (injected now st)
                Fetch -> do
                    outcome <- hookFetch hooks
                    after <- clockNow clock
                    let st1 = observed cfg after outcome st
                    case outcome of
                        Failed -> hookBackoff hooks st1.schedFailures (st1.schedNextFetch - after)
                        _ -> pure ()
                    pure st1
        go st'
