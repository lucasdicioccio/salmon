# Per-node state machines: a supervised tree over a folded graph

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

This revision follows the `deptrack-devops` precedent
([`Devops/Graph.hs`](https://github.com/lucasdicioccio/deptrack-project/blob/master/deptrack-devops/src/Devops/Graph.hs)),
which already solved most of this, and refines its per-node *intent stream*
into a per-node *ledger*. It supersedes the first draft's five-state machine.
`Salmon.Builtin.Nodes.Supervised` — a first cut at supervision under the
current execution model — has since been removed in favour of this design;
`Serve.serveWith` still exists but has no consumer, and is subsumed here too.

## Problem

Execution in salmon is a **traversal**. `upTreeWith`
(`Actions/UpDown.hs:128`) walks an expanded `Cofree Graph` post-order;
`downTreeWith` (`:233`) collapses that `Cofree` into a `Ref`-level DAG and
releases nodes in reverse-dependency order. Both are one-shot: they start,
visit each node once, and return `IO Bool`.

Three consequences:

1. **Nothing is long-lived**, so "keep this running" needs apparatus bolted
   on the side. The removed `Supervised` carried a pid table, a polling reaper
   and an STM wakeup channel — 584 lines — for no reason other than that
   `Extension.up :: IO ()` (`Builtin/Extension.hs:39`) returns and forgets,
   leaving a handle nowhere to live.
2. **No dependency-aware restart.** A predecessor going away is not an event
   any dependent can observe; `serve`'s `converge`
   (`Actions/Serve.hs:1013`) gates on direction and convergence only.
3. **No parallelism.** Independent subtrees are walked one at a time, though
   the DAG already *is* the parallelism structure.

And one structural gap that (3) hides: the traversal only ever knows a node's
**predecessors**. Teardown ordering — "free once your *last* dependent is
done" — has to be recovered by collapsing the `Cofree` and counting
dependents (`Actions/UpDown.hs:266`–`:317`), which is where the "directory
not empty" bug CLAUDE.md records came from.

## The model: four structures, one control loop

Everything below is derived from folding declared graphs into state, rather
than from walking a graph:

| structure | type | what it is |
|---|---|---|
| **magma** | `HashMap Ref Node` | every node ever seen, one representative per `Ref` |
| **precedence** | `HashMap Ref [Ref]` ×2 | dependencies *and* dependants |
| **ledger** | `Map DirectiveDigest (Set Ref)` | which live declarations still want which nodes |
| **processes** | `HashMap Ref (Async (), TVar Status, TBQueue Instruction)` | the machine per node, its observable state, and its mailbox |

The magma and precedence come from the comonadic fold: `expand` already
produces the `Cofree`, and the collapse `downTreeWith` performs internally
(`:239`–`:259`) becomes a first-class reusable step that emits both adjacency
directions instead of one. Because it is keyed by `Ref`, folding a *second*
graph into the same structures is a merge, not a replacement — which is what
makes the graph dynamic.

Note what keying the magma by `Ref` buys: since `mkRef` (`Op/Ref.hs`) is
content-addressed, an equal `Ref` means an equal node, so a collision keeps
one representative rather than combining. In particular this does **not** need
`instance Semigroup Extension` (`Builtin/Extension.hs:58`), whose `up a <> up
b` would run the same action twice. The first draft worried about that
instance; in this model it simply is not involved.

### Storage is bounded by nodes, not by declarations

This is what makes the retention work already shipped (`worldEpochs`/`prune`,
`Actions/Serve.hs:1188`) unnecessary rather than merely smaller. Today a
retired epoch's whole `Cofree Graph Op` is kept because it is the only
remaining description of how to tear its nodes down. Here the magma holds each
node's `down` once, keyed by `Ref`, and the precedence graph holds the edges —
so nothing needs a per-declaration graph at all. A node leaves all four
structures when it has dropped out of `desired` *and* its machine has settled
in `Down`.

## Folding declarations into the ledger

**Decided: the ledger is a set, not a count.**

```haskell
type Ledger = Map DirectiveDigest (Set Ref)

desired :: Ledger -> Set Ref
desired = Set.unions . Map.elems
```

A declaration is a `(graph, direction)` pair, and folding it is an insert or a
delete keyed by the encoded directive — which is exactly what `worldActive`
(`Actions/Serve.hs`) already does, with the ref-set `LogEntry.logRefs` already
carries. A node is wanted up iff it appears in *some* live declaration's set.

Worked through the example — `g0` declares `{A,B}` up, `g1` retracts `g0`, `g2`
declares `{A}` up:

```
                       ledger                     desired
  up   g0 {A,B}        {g0: {A,B}}                {A, B}
  down g0              {}                         {}          A and B both go down
  up   g2 {A}          {g2: {A}}                  {A}         A back up, B stays down
```

Counting was the wrong instinct — mine as much as anyone's — and every
property it needed hand-maintaining, the set gets structurally:

| hazard under counting | under sets |
|---|---|
| a node reached by several paths in one graph double-counts | it is a `Set`; one membership |
| `down` of something never up drives the count negative | delete of an absent key is a no-op |
| re-declaring the same seed reaches 2, so one `down` strands it up | same digest, same key: insert replaces |
| two declarations wanting the same node must not cancel each other | union; the node leaves when the last set does |

The one thing counting bought was `O(1)` lookup per node. That is not worth
buying: declarations are rare (a human or a control plane types them), while
node state changes are the hot path and do not touch the ledger at all. So
recompute `desired` on declaration change, and memoise only if it ever shows
up in a profile.

Storage stays bounded by *live* declarations rather than by history, so the
append-only problem `worldEpochs` had does not come back.

## The node process

`deptrack` does not use one five-state machine. It uses **two**, and that is
better: an *upkeep* machine and a *downkeep* machine, three states each.

```haskell
data UpkeepState   = WaitUp   | Upping  | Up
data DownkeepState = WaitDown | Downing | Down
```

Plus the piece the first draft missed entirely — a node's observable state is
not just which state it is in, but **whether it has settled**:

```haskell
data Stability = Stable | Transient
data Status = Status
    { statusCheck      :: !CheckResult
    , statusDirection  :: !Direction
    , statusStability  :: !Stability
    , statusLastActive :: !Word64   -- monotonic ns; see below
    , statusOutput     :: !(Ring Text)
    }
```

### Progress, not just settledness

`deptrack` has exactly two stability values, and two is one short: a node that
has been `Transient` for four seconds because it is building, and a node that
has been `Transient` for four seconds because it is wedged, are the same value.
That is precisely the distinction a supervisor's restart decision needs.

**Decided: keep two states, and add a monotonic activity timestamp.** Anything
observable the node does bumps `statusLastActive` — a state transition, a check
completing, and in particular a line arriving in the output ring. "Wedged" is
then a derived predicate rather than a third state:

```haskell
wedged now st = statusStability st == Transient
             && now - statusLastActive st > watchdog
```

**Decided: `watchdog` is authored on the node, not guessed globally.** A
`cabal build` is legitimately silent for minutes and a web server's startup is
not, and no global or per-kind default can know the difference — the node
author does. This is `systemd`'s `WatchdogSec=` in all but name, and it lives
next to `Restart` as part of the same per-node policy. A node that declares no
watchdog is never considered wedged, which is the right default: silence is
only evidence when someone has said what silence would mean.

This keeps `Stable`/`Transient` as the thing `waitStability` blocks on (a
timestamp would make that condition unstable and wake dependants constantly)
while giving the supervisor what it needs to tell slow from stuck. It also
means a chatty build is *visibly* progressing for free, which is the common
case and the one an operator most wants to see.

### Output: a bounded ring per node

**Decided: each node keeps a bounded ring of its recent output.** Ownership
(below) makes capturing a `Managed` node's stdout/stderr possible for the first
time; a ring rather than a buffer is what stops a chatty service from being
quietly accumulated into the heap. It pays for itself three times over: it is
what an operator wants when a node is `Failure`, it is what feeds
`statusLastActive`, and it means a crash report can carry the last N lines that
preceded it rather than just an exit code.

Size is per-node and small (a few hundred lines); anything wanting real logs
should be shipping them somewhere, which is a node of its own.

### Ordering is STM, not messages

Dependency ordering is a blocking read of the neighbours' `TVar Status`:

```haskell
waitStability :: Direction -> Stability -> [TVar Status] -> STM ()
waitStability dir stab tvars = do
    sts <- traverse readTVar tvars
    if all (\s -> statusStability s == stab && statusDirection s == dir) sts
        then pure ()
        else retry
```

A node turning up waits on its *dependencies* being `Stable`/`TurnUp`; a node
turning down waits on its *dependants* being `Stable`/`TurnDown`. That single
inversion is the whole of the teardown-ordering problem the current
`downTreeWith` spends eighty lines on — and it is only expressible because the
fold kept both adjacency directions.

`retry` also means no polling, no wakeup channel and no scheduler: a node
blocks until a neighbour's state actually changes. `Serve.serveWith`'s
`STM (Set Ref)` plumbing exists only because nodes today have no state of
their own to block on.

### …but instructions are messages: one mailbox per node

Neighbour state is *pulled* with `retry`. Instructions have to be *pushed*,
because a node must be tellable things that are not derivable from its
neighbours at all:

- `Force` — run `up` even though `check` says `Skipped`; the operator knows
  something the check does not;
- `Skip` — treat as satisfied without acting (`Query.forceSkip`'s semantics);
- `Recheck` — collapse the adaptive delay to its floor and check now;
- `Pause`/`Resume` — stop the upkeep FSM without tearing the effect down.

**Decided: a bounded mailbox per node, rather than swapping the `Async` in the
magma.** Swapping has two problems. It cannot express a *transient*
instruction without killing and restarting the machine — for a `Managed` node
that means killing a healthy process to set a flag. And it is largely
redundant with content-addressing: a node whose definition really changed has
a different `Ref`, so it is already a different node with a different machine.

Decoration still has a place, but at **fold time** rather than run time. A
`Plan` is part of a declaration, so `Query.forceSkip` is applied as the graph
is folded into the magma: the node enters with its check pre-answered and no
running machine is disturbed. Declaration-time forcing is decoration; run-time
forcing is the mailbox. That split is what keeps `run up --plan` meaning the
same thing it does today while still allowing an operator to force one node in
a live `serve`.

The two inputs compose in STM, which is the point:

```haskell
atomically $
      (Left  <$> readTBQueue mailbox)
  <|> (Right <$> waitStability TurnUp Stable dependencies)
```

Bounded, because a control plane can outrun a node busy doing something slow,
and an unbounded mailbox turns a wedged node into a memory leak. Overflow
drops the *oldest* — these are statements about current intent, so stale ones
are the ones to lose — and a dropped instruction appears in the report stream,
or forcing a node becomes silently unreliable.

Provisional on purpose. Whether drop-oldest is right, or whether a coalescing
mailbox (at most one pending instruction of each kind) is better, depends on
how instructions actually get used, and there is no way to know that before
something is driving them.

### "Keep this running" is `check` plus re-`up`

The upkeep machine's steady state is not "hold a process handle". It is a
periodic check with an adaptive delay:

```haskell
fsm delay Up xyz = threadDelay delay >> do
    newStatus <- checkUpStatus xyz
    case statusCheck newStatus of
        Success -> fsm (increaseDelay delay) Up     xyz   -- ×2, capped 60s
        Skipped -> fsm (increaseDelay delay) Up     xyz
        _       -> fsm (decreaseDelay delay) Upping xyz   -- ÷2, floor 500ms
```

This is the most important idea to take from `deptrack`, and it is why
`Supervised` was deleted rather than ported. Supervision becomes:

- `up` — spawn it;
- `check` — is it alive?;
- `down` — kill it;

with the FSM's adaptive delay *being* the backoff, and no pid table, no
reaper, no `getProcessExitCode`, no wakeup channel.

**Decided: a check may be arbitrarily expensive.** Most should be cheap, but
there is no reason to forbid one that probes a remote endpoint or runs a real
query, and forbidding it would just push authors into writing a worse check.
Three consequences follow, and are the whole cost of the decision:

- the adaptive value is a *delay between* checks, not a period, so a slow
  check reduces its own frequency and the load is self-limiting;
- a check in flight must not hold the status `TVar` or block the mailbox, or a
  slow check makes the node unresponsive to `Force` and to teardown — and
  `cancel` must be able to interrupt one, which means checks must be
  interruptible `IO` rather than a long uninterruptible FFI call;
- a check in progress counts as activity for the watchdog above, or every slow
  check would look exactly like a wedge. It also generalises past
what `Supervised` can do: a node whose process salmon does not own — a systemd
unit, a container, something on another host — is supervised exactly the same
way, because the model never assumed ownership.

### `check` is the field nobody implements

`Extension` already has `check :: IO ()` (`Builtin/Extension.hs:42`). It is
assigned by **zero** nodes across `salmon-ops` and `salmon-ops-recipes`, and
`Salmon.Actions.Check.checkTree` has **zero callers** — it is not wired into
`CommandLine` at all. Meanwhile `prelim :: IO Requirement` (`:40`) is
implemented 23 times and answers very nearly the question the FSM needs:
`Skippable` means "the effect is already in place".

So the change is a merge, not an addition:

```haskell
data CheckResult = Success | Skipped | Failure !Text | Unknown

check :: IO CheckResult     -- replaces both `prelim` and today's dead `check`
```

`Requirement` derives from it (`Success`/`Skipped` ⇒ `Skippable`), so the 23
existing implementations port mechanically and `Salmon.Actions.Check` is
deleted rather than fixed.

`notify :: IO ()` (`:43`) is the same story and gets the same treatment:
zero implementations, and `Salmon.Actions.Notify.notifyTree` has zero callers.
**Decided: removed**, along with `Salmon.Actions.Notify`. Whatever it was
reaching for, `Broadcast`/`Reporter` is the thing that actually carries
per-node events here.

That covers every effect salmon does *not* own. For one it does own, `check`
is necessary but not sufficient — see the next section.

### Recovering process ownership

Polling `check` is the right answer for an effect salmon did not spawn: a
systemd unit, a container, a service on another host. It is a poor answer for
a process salmon started itself, and an earlier draft of this document gave
ownership up too readily. Three things go with it:

- **The exit status.** `check` answers alive-or-dead; `waitForProcess` answers
  `ExitFailure 137`. Without it there is no way to express
  `Restart=on-failure` — the common and correct policy of *not* restarting a
  service that exited cleanly.
- **Timeliness.** The adaptive delay backs *off* on success, to a 60s cap. A
  service that dies a second after a successful check stays dead for a minute.
  For anything user-facing that is the difference between supervision and a
  cron job.
- **Identity.** A pidfile plus `kill -0` cannot survive pid reuse, and cannot
  tell a live process from a zombie.

The per-node machine recovers all three, and it is precisely the machine that
makes it possible: **the handle never has to escape, because the node's own
thread is in scope for the effect's entire lifetime.** That was impossible
under the traversal, where `up :: IO ()` ran and returned into nothing.

```haskell
data Lifecycle
    = OneShot (IO ())        -- returns; the effect persists on its own
    | Managed (IO ExitCode)  -- blocks while the effect is up; returns when it stops
```

A `Managed` node's action *is* the process:

```haskell
Managed $ withProcess cp $ \ph -> waitForProcess ph
```

`Upping` runs the action. A `OneShot` node reaches `Up` when it returns; a
`Managed` node is `Up` *for as long as it is still running*, and the
`ExitCode` it eventually yields is the reason it stopped. Teardown is `cancel`
on the `Async`, and the `bracket` inside `withProcess` does the killing. This
is why no pid table appears anywhere in this design: the pid is a local
variable on the owning thread's stack.

So `Up` has two exits, and a node may have either or both: the action
returning (a managed process died, with its status) or a check failing (an
unowned effect went away). Concretely `Up` races the running action against
the adaptive check timer; a node supplying only `check` behaves exactly as the
previous section describes, and one supplying only `Managed` never polls.

Three consequences worth settling before this is built:

- **`-threaded` stops being a recommendation and becomes a requirement.**
  `waitForProcess` blocks the entire runtime on the non-threaded RTS, and this
  design has one such call per managed node. The test suite already met this:
  the (now removed) `Test.SupervisedSpec` needed `ghc-options: -threaded`
  before a blocking read stopped freezing the reaper along with everything
  else. That flag is still on the test suite for `serve`'s own reader thread.
- **`cancel` alone is not a stop.** `withCreateProcess`'s cleanup sends
  `SIGTERM` and waits; a service that ignores it wedges the teardown. The
  grace-then-`SIGKILL` escalation `Supervised.serviceDown` had is worth
  recovering into the bracket, as is `create_group = True` so the whole group
  goes. Both are in git history at `f9d7116` rather than lost.
- **This is `waitpid(pid)`, not `waitpid(-1)`.** `Supervised`
  polled `getProcessExitCode` partly to dodge a race with
  `Binary.untrackedExec` that, on reflection, never applied to it — waiting on
  a *specific* child cannot steal another child's status. That race is real
  only for a process obliged to reap orphans it never spawned, i.e. PID 1,
  which is exactly why `specs/salmon-as-init.md` keeps a Rust half. Owned
  children are waited on individually; orphans stay someone else's problem.

### The restart policy reads the exit code

**Decided: yes** — that is most of what ownership is *for*, and it is what
makes `Restart=on-failure` expressible at all.

```haskell
data Restart = Always | OnFailure | Never
```

With one ordering subtlety that falls out of having both mechanisms: **consult
`check` before the policy.** A process that exits 0 because it daemonised is
still up, and `check` is the only thing that can say so.

```
on exit with code c:
    check >>= \case
        Success -> Up              -- it forked; the effect is there regardless
        _       -> apply policy to c
```

That single line handles the double-fork case for free — the one shape
`Supervised` could not have handled at all, since a handle to a process that
has exited tells you nothing about the daemon it left behind.

Default `OnFailure`. A `Managed` node that exits cleanly finished on purpose,
and restarting it fights its own decision; it settles in a terminal
`Completed` condition that is visibly *not* `Up`, so `status` shows what
happened rather than a restart loop.

**Decided: `Completed` counts as converged.** It is a node that did what it
was asked and stopped, so the batch driver treats it as done and it does not
hold up quiescence; a job modelled as `Managed` therefore lets `run up`
terminate normally. The price is that `status` must distinguish `Up` from
`Completed` in its output rather than collapsing both to "fine" — a service
that quietly completed is exactly the thing an operator needs to see. `Always` is for services that exit 0 on reload;
`Never` for a one-shot job modelled as `Managed` only to capture its output.

Note this default differs from systemd's `Restart=no`, deliberately: a node
*declared up* that has stopped being up is a convergence gap, and quietly
accepting it would make this model weaker than `run up` is today.

## The supervision tree

Framing the node machines as Erlang processes under a supervisor gives two
things beyond "an `Async` per node".

**Let it crash.** A node machine that throws is not caught inline. The control
process monitors its children (`waitCatch` on the `Async`) and applies a
restart policy. That splits two kinds of failure the current design conflates:
*the managed effect stopped* (handled inside the upkeep FSM, `Up → Upping`)
versus *the machine managing it died* (handled by the supervisor). It is the
same two-level structure as `specs/salmon-as-init.md`'s PID-1 floor versus
supervisor policy, which is a good sign the shape is right.

**Restart strategies map onto the DAG.** Erlang's `one_for_one` is "restart
just this node"; `rest_for_one` is "restart this node and everything after
it". Along dependency edges `rest_for_one` *is* dependency-aware restart — a
node leaving `Up` demotes its dependants to `WaitUp`, and `waitStability`
already makes them block. It is also today's `Blocked` semantics
(`Actions/UpDown.hs:150`) expressed as a supervision policy rather than as a
`Bool` threaded through a walk. Making the strategy per-node is then a natural
knob: a config-file node probably wants `rest_for_one`, a log shipper probably
wants `one_for_one`.

## Two drivers over one node model

The first draft treated `run up`'s batch semantics as a hard problem needing a
quiescence predicate. `deptrack` shows the simpler answer: keep **both**
drivers over the same node definitions.

```haskell
syncTurnupGraph  :: Broadcast -> OpGraph -> IO ()                 -- topological, one-shot
asyncTurnupGraph :: Broadcast -> Statuses -> Intents -> OpGraph -> IO ()
upkeepGraph      :: ... -> UpkeepFSM -> DownkeepFSM -> IO ()      -- continuous
```

`run up`/`run down` keep the synchronous topological driver and therefore keep
returning `IO Bool` with today's exact failure containment; `serve` uses the
async supervised driver. No quiescence predicate is needed for the batch case
at all, because the batch driver still terminates by construction — and with
`Completed` counting as converged, a `Managed` node that finishes its work
does not hold it open either.
`Broadcast` — in `deptrack`, `(OpUniqueId, CheckResult, Stability, Direction)
-> IO ()` — is salmon's `Reporter`, so the report stream is where the two
drivers stay comparable.

**Decided: no separate `Blocked` status; `WaitUp` is it.** A node waiting on a
predecessor that will never arrive is in the same state as one waiting on a
predecessor that is merely slow — what differs is what the *driver* does about
it. The async driver keeps waiting, and correctly so: the predecessor may yet
be repaired, and the node should then proceed without anyone re-declaring
anything. The batch driver cannot wait forever, so it recognises "waiting on
something terminal" and reports `Blocked`, exactly as `upTreeWith` does today
(`Actions/UpDown.hs:150`).

So `Blocked` survives as a *report* rather than as a node state — which is
what keeps the existing suites meaningful, since they assert on the report
stream, while the node's own machine stays three-valued.

## Bounding concurrency

**Decided in two parts, and the first is not a concurrency limit at all.**

**(i) Collections.** The answer to "a graph with two hundred `apt-get install`
nodes" is that it should not have two hundred nodes. `Debian.Package` already
does this today: `installAllDebsAtOnceWith` (`Nodes/Debian/Package.hs`) is an
`Op -> Op` rewrite that harvests every `Package` from the graph's `dynamics`
and replaces the per-package nodes with a single batched `debs` node, while
`removeSinglePackages` prunes the originals.

Under this design that rewrite becomes *more* valuable, not less: the
collection is one node with one machine, so 199 `Async`s never exist to need
limiting. It also generalises past apt — anything whose underlying tool is
dramatically cheaper in bulk (package managers, `nft` rulesets, a batch of DNS
records) wants a collection rewrite rather than a semaphore.

**Decided: the rewrite runs *after* the fold, and is direction-aware.** This
is the opposite of what an earlier draft of this document guessed, and the
reason is decisive: a collection must not sweep together packages that are
being installed with packages that are being removed, and *nothing before the
fold knows which is which*. `desired` is what tells a `deb` node whether it is
wanted up or down, so `installAllDebsAtOnceWith` has to take that as an input
and partition on it, emitting an install batch and a removal batch rather than
one blind batch.

**The partition is conservative: still-required wins.** A node in `desired`
goes to the install batch; only a node *absent* from `desired` goes to the
removal batch. Two live declarations can disagree — one still wants `nginx`,
another was retracted and would have removed it — and the ledger has already
answered that by union. The rewrite inherits that answer rather than deriving
its own, so a package any live declaration still wants is never swept into a
removal. Erring the other way would let a retraction pull a package out from
under something still standing on it, which is the one failure mode here that
retrying does not recover.

**A batch reports failure for all of its members.** `apt-get install a b c`
exiting non-zero says the batch failed, not which package; narrowing it would
mean parsing apt's prose, which is a fragile thing to make a node's status
depend on. So every member goes to `Failure` with the batch's exit status, and
the batch's output ring is where the actual message lives. This is the
accepted trade — collections buy efficiency and pay in attribution. If it ever
bites, the fix is bisection rather than parsing: on a batch failure, fall back
to per-node execution for that batch on the next pass. A refinement, not a
requirement.

That settles the "do the ledger and the magma disagree about what exists?"
worry by drawing the line somewhere else than either draft did:

- the **ledger is declared intent**, and keeps the user's own per-package
  nodes. `status` still reports per package, which is what an operator wants;
- the **rewrite is an execution-plan detail**. The collection node exists only
  in the process layer, its lifetime derived from the set of package nodes it
  subsumes, and edges into a collected node redirect to it.

So they never disagree, because they are answering different questions. It
also means the rewrite has to redirect *precedence* edges, not just replace
nodes — anything that depended on `deb foo` now waits on the batch that
installs it.

### Why a recipe cannot do this itself

The ordering is not a scheduling convenience. It follows from a real
expressiveness limit in the pipeline, and naming that limit is the better
argument for "after the fold".

A recipe author supplies `Track' directive`, i.e. `directive -> Op`
(`Op/Track.hs`) — a function of **one directive in isolation**. It cannot see
the other live declarations, it cannot see their directions, and it cannot see
what was declared before. So "batch this package with the other packages that
are also currently wanted up" is not awkward to write in a recipe; it is
inexpressible there. Nothing in `directive -> Op` has the second argument.

That limit is why `dynamics :: [Dynamic]` (`Builtin/Extension.hs:44`) exists at
all. It is the escape hatch by which a node says *"I am a `Package`"* without
knowing what will be done about it, so that a later pass can collect the set
and act on it — which is exactly what `installAllDebsAtOnceWith` does. The
channel is already the right shape; it is only under-supplied, because today
that later pass runs over a single expanded graph and therefore knows no more
than the recipe did.

Folding is what enriches it. The same `dynamics` mechanism, read across the
whole magma with each node's `desired` direction in hand, gives the later pass
the two things the recipe structurally cannot have: **other declarations, and
directions**. "After the fold" is simply where that information first exists.

The tempting alternative is to widen the recipe instead — `Ledger -> directive
-> Op` — and it should be rejected on three counts:

- **expansion stops being deterministic in the directive.** A directive that
  expands differently depending on what else happens to be declared makes
  `run up` irreproducible;
- **it breaks the plan contract.** `Query.planDirectiveDigest` pins a plan to
  the digest of the directive it was computed from
  (`Builtin/CommandLine.hs`); if the graph is a function of ambient state too,
  the digest no longer identifies the graph;
- **it breaks retraction.** The ledger's `down` assumes what a declaration
  contributed is a property of that declaration. If expansion depended on the
  ledger, a declaration's contribution would depend on the order declarations
  arrived, and retracting it could not be computed at all.

So the split stands, and gets sharper: **recipes stay a pure function of their
own directive; cross-declaration knowledge lives in a post-fold pass.** The
practical consequence is that a rewrite stops being an `Op -> Op` an
application remembers to apply, and becomes a registered phase the control
process runs — roughly `Set Ref -> Magma -> Precedence -> (Magma,
Precedence)`, with `dynamics` as its input channel.

**Decided: `query` shows the computed graph by default, and the declared one
on request.** The default has to be what will actually execute, or `query`
describes a fiction and `run up --plan`'s excluded refs are written against
the wrong node set. The declared view stays available behind a flag, because
it is what the operator wrote and what they will edit.

One subtlety this exposes: for `run up` the computed graph is a function of a
single directive, so it is as reproducible as the directive itself and
`Query.planDirectiveDigest` stays meaningful. Under `serve` it depends on
*every* live declaration, so a plan captured against it is valid only while
the rest of the ledger holds still. Plans are a batch-mode affordance and
should probably stay one.

### The lock the two batches fight over

Splitting by direction creates one new problem: `apt-get install` and `apt-get
remove` both want the dpkg lock, and as two nodes they may run concurrently.
Retries do cover it, and are the fallback, but they are the weakest of the
available answers — a retry loop has to distinguish "could not acquire the
lock" from "no such package", and if it cannot, it retries real failures too.

Two better answers, both already available:

1. **An edge.** The rewrite emits both batches, so it can also emit the
   ordering between them, and the DAG then serialises them for free with no
   retry and no new mechanism. Removals before installs is the right order —
   it is what one would do by hand to clear conflicts, and it matches the
   existing `converge` sequence (`downTree` pass, then `upTree` pass).
2. **The bounding primitive of (ii).** This is precisely the case that
   motivates it: a named exclusive resource (`dpkg`) that several nodes
   declare they need. When that primitive exists, the edge becomes an
   unnecessary special case.

Recommend the edge now and the primitive later, with retries as the safety net
rather than the design.

**(ii) Bounding primitives, later.** Some resources are genuinely exclusive
and cannot be collected away: the `dpkg` lock, a single-writer migration, a
shared build directory. Those want an explicit primitive — a named bounded
resource a node declares it needs, with the control process holding the
semaphore — rather than a global thread cap. Deferred deliberately, because
(i) removes most of the pressure and because the right shape is much easier to
see once real graphs have run under this model.

The ordering matters: a global concurrency cap is the easy answer and the
wrong one, since it slows every unrelated node in the graph in order to
protect one contended resource.

## What this removes

| shipped | fate |
|---|---|
| `Nodes/Supervised.hs` (584 lines: pid table, reaper, wakeups, `Policy`) | already removed; `up`/`check`/`down` + adaptive-delay FSM replace it |
| `Serve.serveWith` + `STM (Set Ref)` wakeups | deleted; `waitStability` is the event source |
| `Serve`'s `worldEpochs`/`prune` retention | deleted; the magma is bounded by nodes |
| `Serve.Convergence` (`:146`) | replaced by `Status` (check + direction + stability) |
| `Extension.prelim`, `Extension.check`, `Actions/Check.hs` | merged into `check :: IO CheckResult` |
| `Extension.notify`, `Actions/Notify.hs` | deleted; 0 implementations, 0 callers |
| `upTree`/`downTree` | kept, re-expressed as the synchronous driver |

Nothing is lost by that removal. Process ownership — the one capability
`Supervised` had that a check-and-re-`up` loop does not — returns as
`Managed`, and stronger, because
the owning thread outlives the effect and so the handle never needs a table to
live in. `Supervised`'s genuinely useful parts are not lost either — they sit
in `f9d7116` waiting to be recovered:
`create_group = True`, the grace-then-`SIGKILL` escalation, and the
requested-exit-versus-crash distinction all move into the `Managed` bracket
and the `ExitCode` it returns. What goes is the bookkeeping that only existed
because `up :: IO ()` had nowhere to put a pid: the table, the reaper, the
wakeup channel, and the `run_stopping` flag.

## Suggested milestones

1. **`check :: IO CheckResult`**, absorbing `prelim`; delete
   `Salmon.Actions.Check` and `Salmon.Actions.Notify` with the `notify` field.
   Mechanical across 23 call sites, no behaviour change, and a prerequisite
   for everything else.
2. **`Salmon.Op.Dag`**: the comonadic fold to magma + both adjacency
   directions, lifted out of `downTreeWith`. Pure and unit-testable, and
   `downTree` keeps using it, so it is a refactor with no behaviour change.
3. **The ledger**: live-declaration ref-sets and `desired` as their union,
   replacing `worldEpochs`/`prune` in `serve`. Still no concurrency;
   `Test.ServeSpec` is the regression net.
4. **Sync drivers re-expressed** over the magma and ledger. `run up`/`run
   down` produce the same `Report` stream and the same `Bool`;
   `Test.DownTreeSpec`/`Test.QuerySpec` are the net.
5. **Rewrites as a registered post-fold phase**, taking `desired` and reading
   `dynamics` across the magma. `installAllDebsAtOnceWith` ports to it and
   becomes direction-aware: an install batch and a removal batch with an
   ordering edge between them, and precedence edges redirected onto them.
   Worth doing before parallelism rather than after, since it is what stops
   the first wide graph from wanting a concurrency limit — and it is the step
   where applications stop applying `Op -> Op` passes by hand.
6. **`TVar Status` per node and `waitStability`**, plus the async drivers and
   the per-node mailbox. Parallelism appears here.
7. **Upkeep/downkeep FSMs** with adaptive delay, over `OneShot` nodes only,
   plus the authored watchdog. Supervision of unowned effects appears here.
8. **`Managed` nodes**: `Up` races the running action against the check timer,
   `cancel` tears down through the bracket, and exit statuses reach the
   restart policy. This is the step that restores what removing `Supervised`
   gave up, so nothing supervises owned processes until it lands.
9. **`rest_for_one`**: a node leaving `Up` demotes its dependants. The payoff,
   and last because it is the only step that changes what a correct graph
   *does*.

## Non-goals (v1)

- Cross-machine supervision. `Nodes/Self.hs`'s remote-op flattening is
  unaffected.
- Replacing `Configure`/the seed→directive protocol; this is execution only.
- Persisting machine state across a `serve` restart — the pidfile convention
  above is what makes a restart recoverable, not a state file.
- A general actor framework. Two three-state FSMs and a supervisor.

## Open questions

Three rounds of these are now settled in the sections above — the ledger
shape, concurrency, `notify`, the plan machinery, captured output, exit codes,
`Transient`, where the rewrite runs and why, `Completed`, mailbox overflow,
check cost, collection conservatism, batch failure attribution, `Blocked`
versus `WaitUp`, and what `query` shows. One is left, and it is deliberately
left:

- **How is a node's watchdog authored?** It sits beside `Restart` as per-node
  policy, but `Extension` has no policy field at all today, and adding one is
  the first time a node would declare something about *how it is supervised*
  rather than what it does. That is a new kind of statement for a node to
  make, and the shape wants settling before milestone 7 rather than during it
  — a watchdog is only as good as the authors' willingness to set it, so if
  declaring one is awkward, nobody will and the mechanism is dead weight.
  Tracked in `todo`.

## Relationship to the other specs

- `specs/salmon-as-init.md` needs this and gets simpler for it: its PID-2
  supervisor is this control loop, its restart policy is the upkeep FSM, and
  its "PID 1 rate-limits, the supervisor decides" split is the same two-level
  supervision as above. The Rust PID 1 is unaffected — that boundary is about
  `waitpid(-1)`, not scheduling.
- `specs/multi-user-privilege-separation.md` composes unchanged: an `Invoker`
  decorates the `CreateProcess` a node's `up` spawns.
- `specs/advance-querying.md` is the open question above.
