{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | What a node says about how it wants to be tended.

A handful of knobs, all optional, all authored on the node itself: how
eagerly to put it back when it stops being up ('Restart'), how long its
silence has to last before somebody should worry ('supWatchdog'), and what
its going away means for the nodes standing on it ('Strategy').

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
    Strategy (..),
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

For a node that owns a process ('Salmon.Builtin.Extension.managed') the same
three answers are read against its 'System.Exit.ExitCode' instead, with one
ordering rule that matters: __the check is consulted before the policy.__ A
process that exits 0 because it daemonised is still up, and the check is the
only thing that can say so.
-}
data Restart
    = Always
    | OnFailure
    | Never
    deriving (Show, Eq, Ord)

{- | What a node leaving 'Salmon.Actions.Upkeep.Up' means for the nodes that
depend on it. Erlang's two supervision strategies, read along dependency
edges.

* 'OneForOne' (the default) is today's behaviour exactly: putting this node
  back is a statement about this node. A dependant that has already reached
  'Salmon.Actions.Upkeep.Up' is not disturbed.
* 'RestForOne' additionally sends every dependant back to
  'Salmon.Actions.Upkeep.WaitUp', to be brought up again on top of whatever
  this node turns into. A configuration file is the case that wants it: a
  service reading a config that has just been rewritten should be bounced,
  and only the config node knows that.

__This is authored on the node that goes away, not on the ones that get
bounced__, which is what makes it usable: the config file's author knows
their content is load-bearing, while the six services reading it would each
have to know, separately, that it might change under them.

Note the default is the opposite kind from 'supRestart''s. Restarting a node
that fell over is an active choice about that node, and 'OnFailure' makes it.
Bouncing a node's dependants is a decision about /other people's/ nodes, so
nothing happens until somebody says it should — which is also what makes this
safe to have added: a graph that names no strategy behaves as it did before.

The cascade is free rather than built: a demoted node is itself no longer up,
so a dependant of /it/ that also declares 'RestForOne' sees the same thing and
goes back too, all the way out to the edge of the opted-in cone.
-}
data Strategy
    = OneForOne
    | RestForOne
    deriving (Show, Eq, Ord)

data Supervision = Supervision
    { supRestart :: !Restart
    , supStrategy :: !Strategy
    -- ^ what this node leaving 'Salmon.Actions.Upkeep.Up' does to the nodes
    -- that depend on it. See 'Strategy'.
    , supReapply :: !Bool
    -- ^ for a node whose check answers
    -- 'Salmon.Actions.UpDown.Immaterial' (the default for a node with no
    -- @check@ at all): re-run @up@ on the tending loop instead of parking.
    --
    -- 'Salmon.Actions.UpDown.Immaterial' says "asking would cost what
    -- applying costs" — it does not say the effect can never go away, only
    -- that this node has no cheap way to tell. Most nodes that answer it
    -- should still park (see 'Salmon.Actions.Upkeep' @Rest@): re-running
    -- @up@ on a schedule is safe only if it is genuinely cheap /and/
    -- genuinely idempotent, a strictly stronger claim than @Immaterial@
    -- itself makes. 'Salmon.Builtin.Nodes.Filesystem.dir' is the case this
    -- exists for — @createDirectoryIfMissing@ costs about what
    -- @doesDirectoryExist@ would, so there is nothing to lose by preferring
    -- the former.
    --
    -- __Ignored for a node that holds a running action__
    -- ('Salmon.Builtin.Extension.managed'): such a node's @up@ throws by
    -- convention (see "Salmon.Builtin.Nodes.Daemon"), and re-running it on
    -- a schedule would crash-loop a service that is working fine. Such a
    -- node parks regardless of this field.
    --
    -- __Never demotes this node's own dependants.__ A scheduled re-apply
    -- does not go through 'Salmon.Actions.Upkeep.WaitUp' \/
    -- 'Salmon.Actions.Upkeep.Upping' and does not touch
    -- 'Salmon.Op.Status.statusEpoch', so a 'RestForOne' dependant watching
    -- this node is not told anything happened — nothing did, as far as that
    -- contract is concerned: the node never stopped being up. A re-apply
    -- that /fails/ is a different story and is folded back into the normal
    -- failure machinery ('supRestart', 'supGiveUpAfter'), which does have a
    -- way to say so.
    , supWatchdog :: !(Maybe Micros)
    -- ^ how long this node may go without doing anything observable before
    -- it should be called wedged. 'Nothing' — the default — means never.
    , supStableAfter :: !Micros
    -- ^ having been up this long counts as working: the backoff and the
    -- consecutive-failure count both reset.
    --
    -- This is what stops a service that falls over once a day from
    -- eventually being treated as a crash loop — only /consecutive quick/
    -- failures count. Without it, 'supGiveUpAfter' would latch off any
    -- long-lived node given enough days.
    , supDemoteEvery :: !Micros
    -- ^ 'RestForOne' rate limit: a node is demoted by a dependency at most
    -- once per this interval, so a dependency that is flapping cannot
    -- rebuild the whole cone behind it on every flap.
    --
    -- A separate field from 'supStableAfter' on purpose (see (I3) in
    -- @specs\/per-node-state-machines-remaining.md@) — "how long before a
    -- crash counts as a new one" and "how often may this node's dependants
    -- legitimately be rebuilt" are different questions with no reason to
    -- share a timescale. Defaults to 'supStableAfter''s value in
    -- 'defaultSupervision', so nothing changes for a node that has not
    -- thought about it.
    , supGiveUpAfter :: !(Maybe Int)
    -- ^ stop putting the node back after this many consecutive failures.
    -- 'Nothing' — the default — never gives up.
    --
    -- Right for a service whose repeated failure is information rather than
    -- an emergency; wrong for anything the machine cannot come back without,
    -- which is why the default is to keep trying. A node that has given up
    -- says so in its status and is not touched again until an operator
    -- forces it.
    }
    deriving (Show, Eq)

{- | 'OnFailure', 'OneForOne', never reapply on a schedule, no watchdog, ten
seconds of uptime counts as stable, never gives up: what a node that says
nothing gets.

Note the difference in kind between the two defaults that /do/ something.
'OnFailure' is an active choice — a node declared up that has stopped being
up is a convergence gap, and quietly accepting it would make this model
weaker than @run up@ already is (systemd's own default is the opposite, and
systemd is not converging a declared graph). Never giving up is the passive
choice: latching off is a decision only the node's author can justify.
-}
defaultSupervision :: Supervision
defaultSupervision = Supervision OnFailure OneForOne False Nothing (seconds 10) (seconds 10) Nothing

{- | State a supervision policy on a node, for a later pass to read back:

@
op "webserver" nodeps $ \\actions ->
    actions
        { ...
        , dynamics = [supervised defaultSupervision{supWatchdog = Just (seconds 30)}]
        }
@

Prefer amending 'defaultSupervision' to spelling out every field: the record
has grown once already and will again, and a node that only cares about its
watchdog should not have to have an opinion about giving up.
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
