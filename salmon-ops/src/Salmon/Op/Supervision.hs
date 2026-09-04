{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | What a node says about how it wants to be tended.

Two knobs, both optional, both authored on the node itself: how eagerly to
put it back when it stops being up ('Restart'), and how long its silence has
to last before somebody should worry ('supWatchdog').

= Why this rides 'Data.Dynamic.Dynamic' rather than a new field

'Salmon.Builtin.Extension.Extension' already has a channel for "a node states
something about itself that a later pass acts on": @dynamics@. It is what
@Package@ uses (@Nodes\/Debian\/Package.hs@) so that the post-fold collection
rewrite can find every package in the graph, and the argument for reusing it
here is the same one "Salmon.Op.Rewrite" makes — the information belongs to
the node, the decision belongs to something that sees more than the node.

Three things fall out of that choice:

* __Nothing changes for the many nodes with no opinion.__ There is no new
  field for every existing builtin to fill in with a default.
* __The default is structural.__ "A node that declares no watchdog is never
  considered wedged" is 'getDynamics' returning @[]@, not a 'Nothing' every
  author has to write. Silence is evidence only once somebody has said what
  silence would mean.
* __It is one line to add__, which was the bar: a watchdog is only as good as
  authors' willingness to set one.

The cost is that it is untyped and unenforced — nothing stops two conflicting
'Supervision' dynamics on one node. That is the same weakness the collection
rewrite already lives with, and it gets the same treatment as a conflicting
magma representative: take one, report the rest. Which one is arbitrary
(the first, here); what matters is that the loser is not silent.

= Time is 'Micros', not @DiffTime@

@specs\/per-node-state-machines.md@ writes @Maybe DiffTime@. salmon-ops has
no @time@ dependency and neither consumer of these values wants one:
'Control.Concurrent.threadDelay' takes microseconds and
'GHC.Clock.getMonotonicTimeNSec' hands back an integral nanosecond count.
An @Int@ of microseconds is what both ends already speak.
-}
module Salmon.Op.Supervision (
    -- * Policy
    Restart (..),
    Supervision (..),
    defaultSupervision,
    supervised,

    -- * Reading it back off a node
    supervisionOf,
    supervisionsOf,

    -- * Durations
    Micros (..),
    micros,
    millis,
    seconds,
    toNanos,
) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import GHC.Records (HasField, getField)

{- | When to put a node back after it has stopped being up.

Over the one-shot nodes this milestone covers — a @up :: IO ()@ that returns,
the only lifecycle 'Salmon.Builtin.Extension.Extension' can express today —
the policy is read against the node's own
'Salmon.Actions.UpDown.CheckResult' rather than against an exit code:

* 'OnFailure' (the default) re-runs @up@ when the check says the effect is
  gone ('Salmon.Actions.UpDown.Failure'), and only then.
* 'Never' leaves it alone: the node is reported as having fallen over and an
  operator decides. For a node whose @up@ is destructive to repeat, or whose
  failure means something worse happened upstream.
* 'Always' additionally re-runs it when the check says
  'Salmon.Actions.UpDown.Completed' — the one-shot reading of systemd's
  "restart a service that exits cleanly on reload". A job that means to run
  once should not be 'Always'.

'Salmon.Actions.UpDown.Unknown' never triggers a restart under any policy.
That is a deliberate departure from the one-shot drivers, where
'Salmon.Actions.UpDown.requirement' maps it to
'Salmon.Actions.UpDown.Required': erring toward acting is right for a single
pass over an idempotent action, and wrong for a loop, where it would spin a
node that simply has no check at its delay floor forever. "I could not look"
is not evidence the effect went away; only 'Salmon.Actions.UpDown.Failure'
is.

Exit codes reach this once nodes can own processes, which is the next
milestone; the shape of the type is chosen so that arrival is an added case
rather than a rewrite.
-}
data Restart
    = Always
    | OnFailure
    | Never
    deriving (Show, Eq, Ord)

data Supervision = Supervision
    { supRestart :: !Restart
    , supWatchdog :: !(Maybe Micros)
    -- ^ how long this node may go without doing anything observable before
    -- it should be called wedged. 'Nothing' — the default — means never.
    }
    deriving (Show, Eq)

-- | 'OnFailure', no watchdog: what a node that says nothing gets.
defaultSupervision :: Supervision
defaultSupervision = Supervision OnFailure Nothing

{- | State a supervision policy on a node, for a later pass to read back:

@
op "webserver" nodeps $ \\actions ->
    actions
        { ...
        , dynamics = [supervised (Supervision Always (Just (seconds 30)))]
        }
@
-}
supervised :: Supervision -> Dynamic
supervised = toDyn

{- | The policy this node is to be tended under, plus every other policy it
declared and lost.

An empty second component is the overwhelmingly common case (no declaration
at all, hence 'defaultSupervision'); a non-empty one is a node whose author
said two contradictory things, and the caller is expected to report it rather
than pick silently.
-}
supervisionOf ::
    (HasField "dynamics" ext [Dynamic]) =>
    ext ->
    (Supervision, [Supervision])
supervisionOf ext =
    case supervisionsOf ext of
        [] -> (defaultSupervision, [])
        (s : rest) -> (s, rest)

-- | Every 'Supervision' this node declared, in the order it declared them.
supervisionsOf ::
    (HasField "dynamics" ext [Dynamic]) =>
    ext ->
    [Supervision]
supervisionsOf ext = mapMaybe cast (getField @"dynamics" ext)
  where
    cast :: Dynamic -> Maybe Supervision
    cast = fromDynamic

-------------------------------------------------------------------------------

-- | Microseconds, the unit 'Control.Concurrent.threadDelay' takes.
newtype Micros = Micros {unMicros :: Int}
    deriving (Show, Eq, Ord)

micros :: Int -> Micros
micros = Micros

millis :: Int -> Micros
millis n = Micros (n * 1000)

seconds :: Int -> Micros
seconds n = Micros (n * 1000000)

-- | For comparing against 'GHC.Clock.getMonotonicTimeNSec'.
toNanos :: Micros -> Word64
toNanos (Micros n) = fromIntegral n * 1000
