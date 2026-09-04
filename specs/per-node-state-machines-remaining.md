# What is left of `specs/per-node-state-machines.md`

Status: living plan, update as work continues. Companion to
`specs/per-node-state-machines.md` (the design, whose milestone list is the
source of truth for 1–9) — this file is what remains, why each remaining
piece is worth doing, and what order I would do it in. Read the design first
if you need the "why" of the model; read this if you want to know what to
pick up next.

Written after milestone 7 landed (`git log --oneline` on `serve-supervision`:
`41f181f` back to `52ab4f8`).

## Headline: two milestones, and a residue that matters more than one of them

**Two of the nine milestones are unstarted:**

| # | what | size | why it is not done |
|---|---|---|---|
| 8 | **`Managed` nodes** — a node that owns a running process | medium | needs a new field on `Extension` and a real teardown; deliberately after 7 |
| 9 | **`rest_for_one`** — a node leaving `Up` demotes its dependants | small | last on purpose: the only step that changes what a *correct* graph does |

**And a residue no milestone covers.** Some of it is bookkeeping, but item
(R1) is not: it is the reason milestone 7 currently supervises almost
nothing, and it is worth more than milestone 9. The residue is §"Not in any
milestone" below.

The honest summary of where this design stands: **the execution model is
finished and the nodes have not caught up with it.** Seven milestones built a
per-node state machine, a ledger, a rewrite phase and two concurrent drivers,
all of which ask each node one question — "is your effect in place?" — that
roughly a quarter of nodes can answer.

## Where 1–7 got to, in one line each

So this file stands alone. Each is marked *landed* in the design, with its
deviations recorded in place there.

1. `check :: IO CheckResult` — `prelim` absorbed, `Actions/Check.hs` and
   `Actions/Notify.hs` deleted.
2. `Salmon.Op.Dag` — the `Cofree` collapse lifted out and made pure, both
   adjacency directions, last-writer-wins with a reported conflict.
3. `Salmon.Op.Ledger` — per-declaration `Contribution`s, nodes *and* edges,
   retiring rather than deleted.
4. Both synchronous drivers re-expressed over the magma and ledger; `serve`'s
   retired graphs gone; the cycle hole closed.
5. `Salmon.Op.Rewrite` — cross-declaration knowledge as a registered
   post-fold phase; `Debian.batchPackages`.
6. `Salmon.Op.Status`/`Mailbox` and `Salmon.Actions.Concurrent` — one thread
   per node, ordering by `waitStability`. `serve` converges through it, so
   convergence is parallel and unbounded.
7. `Salmon.Actions.Upkeep`/`Salmon.Op.Supervision` — the upkeep and downkeep
   FSMs, adaptive delay, authored watchdog. `serve` tends its nodes while
   idle. `serveWakingWith` deleted.

-------------------------------------------------------------------------------

## Milestone 8: `Managed` nodes

> `Up` races the running action against the check timer, `cancel` tears down
> through the bracket, and exit statuses reach the restart policy. This is the
> step that restores what removing `Supervised` gave up, so nothing supervises
> owned processes until it lands.

Everything milestone 7 built supervises effects salmon does **not** own: it
polls a `check` and re-runs `up`. That is the right answer for a systemd
unit, a container, or a service on another host, and a poor one for a process
salmon started itself, for the three reasons §"Recovering process ownership"
gives — no exit status, a 60s worst-case detection delay, and no identity
that survives pid reuse.

### 8.1 `Lifecycle` has to go on `Extension`, not on `dynamics`

`Supervision` rides `dynamics` and that was right: it is *policy*, optional,
and a node with no opinion needs to say nothing. `Lifecycle` is not policy —
it is *what the node's action is*, and there is already a field for that:

```haskell
data Lifecycle
    = OneShot (IO ())        -- returns; the effect persists on its own
    | Managed (IO ExitCode)  -- blocks while the effect is up
```

Two ways to land it, and the second is the one to take.

- **Replace `up :: IO ()` with `up :: Lifecycle`.** Honest, and touches every
  one of the ~91 `op` sites plus `noop`'s default, `instance Semigroup
  Extension` (whose `up a <> up b` has no meaning for two `Managed` halves),
  and `Actions/Dot.hs`. A large mechanical diff over the whole tree for a
  feature roughly two nodes will use.
- **Add `managed :: Maybe (IO ExitCode)` beside `up`.** `Nothing` is every
  existing node, unchanged and uninspected. The FSM reads `managed` first and
  falls back to `up`. Ugly in the sense that two fields encode one choice,
  and the ugliness is confined to `Extension` and one `case` in
  `Salmon.Actions.Upkeep.upping` rather than spread across 91 call sites.

Take the second, and say in `Extension`'s haddock that the two fields are one
sum type spelled as a product to avoid a tree-wide diff — a note, so the next
person does not have to rediscover the trade. If a third lifecycle ever
appears, that is the moment to pay for the sum.

### 8.2 `Up` becomes a race, and that is the whole state-machine change

Today `Salmon.Actions.Upkeep.resting` naps on `naptime` and then `look`s. For
a `Managed` node it has to wait on **three** things at once, which is one STM
choice plus one `Async`:

```
Up:  the action returning (the process died, with its ExitCode)
  |  the check timer expiring (an unowned effect went away)
  |  the mailbox (an operator said something)
```

`naptime`'s shape already composes: it is `halting ctx (listen ctx k)` over a
`registerDelay` `TVar`. Add `waitCatchSTM` on the action's `Async` as a
fourth branch and the existing `Wake` type grows one constructor. A node
supplying only `check` behaves exactly as it does now; one supplying only
`Managed` never polls, because its `Delay` is simply never consulted.

`Upping` changes less than it looks: it currently runs `up` inline and
catches. For `Managed` it `async`es the action and moves straight to `Up`,
because a `Managed` node **is** up for as long as its action is still
running. Which means `note status` gains a real writer for the first time —
`statusOutput` exists (milestone 6) and nothing writes to it but the
machine's own transitions.

### 8.3 The teardown is the part that is easy to get wrong

`cancel` alone is not a stop. §"Recovering process ownership" is explicit and
right: `withCreateProcess`'s cleanup sends `SIGTERM` and waits, and a service
that ignores `SIGTERM` wedges the teardown.

**This code already existed and was deleted, not lost.** `git show
f9d7116:salmon-ops/src/Salmon/Builtin/Nodes/Supervised.hs` has
`serviceDown`: flag it stopping *before* signalling (so whoever notices the
exit does not count it as a failure and schedule a restart), signal the
process **group**, `waitGone` for a grace period, then `sigKILL` and wait
again. Recover that into the bracket, with `create_group = True` on the
`CreateProcess` so the whole group goes. `System.Posix.Signals`
(`signalProcessGroup`) is already a dependency via `unix`.

The "flag it stopping first" detail is worth keeping precisely because
milestone 7 made it *more* necessary, not less: the upkeep FSM's whole job is
to put a stopped thing back, so a teardown that lets the machine observe the
exit it asked for is a teardown that resurrects what it just stopped.

### 8.4 `check` before the policy, and the double-fork it buys for free

```
on exit with code c:
    check >>= \case
        Success -> stay Up        -- it forked; the effect is there regardless
        _       -> apply policy to c
```

One line, and it handles the case a process handle fundamentally cannot: a
daemon that exits 0 having forked. `Salmon.Actions.Upkeep.look` is already
the function that runs the check and decides, so this is an extra entry into
it rather than new machinery.

### 8.5 The restart policy is currently too thin, and f9d7116 knew it

`Supervision.Restart = Always | OnFailure | Never` is what milestone 7
landed, and against a `CheckResult` that is enough. Against a **crash loop**
it is not, and the deleted module had the two fields that make the difference:

```haskell
policy_stableAfter :: Double   -- having run this long resets the escalation
policy_giveUpAfter :: Maybe Int -- stop after this many consecutive failures
```

`stableAfter` is what stops a service that crashes once a day from eventually
being treated as a crash loop — only *consecutive quick* failures count.
`giveUpAfter` is right for a service and wrong for anything the machine
cannot come back without (`neverGiveUp`, in the old module).

Milestone 7's adaptive delay is a backoff but it neither latches off nor
resets, so this is a real gap rather than a nicety — and it is a gap the
existing `Delay` cannot express, because a `Delay` has no memory of how it
got where it is. Adding it means `Supervision` grows two fields and the
machine carries a consecutive-failure count and a "running since"
timestamp. Do it *with* 8, not after: a `Managed` node that dies instantly
and forever is the first thing that will exercise it.

### 8.6 Three smaller things

- **`Restart` now collides.** `Salmon.Builtin.Nodes.Systemd.Restart`
  (rendered into a unit file's `Restart=` directive, one constructor:
  `OnFailure`) and `Salmon.Op.Supervision.Restart` (`Always`/`OnFailure`/
  `Never`) share both a name and a constructor. Nothing imports both today.
  This is the fourth collision of this kind in this work (`CheckResult`'s
  `Success`/`Failure` vs optparse, `Mailbox.Skip` vs `Report.Skip`, two
  `Direction`s — the last resolved by *merging* them, which is not available
  here). The two are genuinely different things: one is a string salmon
  writes into a file for systemd to read, the other is a decision salmon
  makes itself. Rename `Systemd.Restart` to `Systemd.RestartDirective` when
  something first needs both.
- **A replaced representative must restart its machine.** §"`Ref` is
  location-addressed" names this as the *one* case where swapping the `Async`
  is right: a `Managed` node whose command line changed but whose ref key did
  not is the same node, last-writer-wins replaces the magma entry, and the
  running process belongs to a machine started from the old one. Today
  `Serve.startTending` builds a whole new supervisor per idle period, so this
  is currently free — and will stop being free the moment a supervisor
  outlives a declaration, which milestone 8 makes tempting (killing a healthy
  process to restart its watcher is exactly what nobody wants). Decide it
  when it bites; do not pre-build it.
- **`-threaded` is already done** (milestone 6, all six executables plus the
  test suite), so the requirement §"Recovering process ownership" flags is
  met before it is needed.

### 8.7 What to test

`Test/UpkeepSpec.hs`'s pattern carries over unchanged — start a supervisor,
block on the report stream, poke, block again — and its 18 cases are the
regression net. New cases, all cheap:

- a `Managed` node is `Up` while its action runs, and the machine does not
  advance past it;
- its exit reaches the policy: `ExitSuccess` + `OnFailure` does not restart,
  `ExitFailure` does, `Always` restarts either;
- a check answering `Success` after a 0 exit keeps the node `Up` (the
  double-fork case) — assert with a `check` that ignores the action entirely;
- teardown kills a process that ignores `SIGTERM`, within the grace period
  plus slack, and the *group* goes (spawn a child that outlives its parent);
- an exit salmon asked for does not schedule a restart;
- `giveUpAfter` latches off, and `stableAfter` resets the count.

The last two want a fake clock or a very short `stableAfter`; prefer the
short value over a clock abstraction, as `watchdogFires` already does with
300ms.

-------------------------------------------------------------------------------

## Milestone 9: `rest_for_one`

> A node leaving `Up` demotes its dependants. The payoff, and last because it
> is the only step that changes what a correct graph *does*.

### 9.1 The exact gap, as the code stands

`Salmon.Actions.Upkeep.look` does half of this already. When a check says the
effect is gone it marks the node failed in the supervisor's
`TVar (Set Ref)` — so a dependant **still in `WaitUp`** holds off, which is
the one-shot drivers' `Blocked` containment expressed as a wait. What it does
not do is touch a dependant that has **already reached `Up`**: that node is
napping in `resting` and never looks at its dependencies again.

So the missing piece is small and precisely locatable: when a node leaves
`Up`, its dependants' machines have to go back to `WaitUp`.
`Salmon.Op.Status.unsettle` is already the function for that. What is missing
is a machine that *observes* it — `resting`'s STM choice would gain a branch
watching its own dependencies' statuses, which is `waitStability` inverted
("wake me when one of these stops being `Stable`/`TurnUp`").

That branch is also why this is last. Every node in a supervised `serve`
would then hold a live STM subscription to its dependencies for as long as it
is up, and a flapping leaf wakes its whole transitive cone. On a wide graph
that is the one part of this design with a plausible thundering-herd
behaviour, and it should land where it can be measured rather than early
where it cannot.

### 9.2 It has to be a per-node choice

Erlang's `one_for_one` is "restart just this node"; `rest_for_one` is "this
node and everything after it". §"The supervision tree" is right that the
strategy is a natural per-node knob — a config-file node probably wants
`rest_for_one` (a service reading a config that changed should be bounced), a
log shipper probably wants `one_for_one` (nothing downstream cares).

That is a third field on `Supervision`, which is already the per-node policy
channel and already optional:

```haskell
data Strategy = OneForOne | RestForOne
supStrategy :: !Strategy   -- default OneForOne
```

**Default `OneForOne`**, which is today's behaviour exactly — so this
milestone changes nothing until a node opts in, which is the property that
makes it safe to land at all. Note this is the opposite default from
`supRestart`'s (`OnFailure`, the active choice), and deliberately: restarting
a node that fell over is a statement about that node, while bouncing its
dependants is a statement about *other people's* nodes.

### 9.3 The hazard to design against

A node that flaps — check fails, check succeeds, check fails — with
`RestForOne` dependants demotes and re-runs its whole cone on every flap.
`stableAfter` from §8.5 is the mitigation and is another reason to do 8
first: demote dependants only once the node has been down long enough to
count, not on the first failed check.

### 9.4 What to test

- a dependant already `Up` is demoted when its dependency leaves `Up`, and
  comes back after it does;
- with `OneForOne` (the default) it is not demoted at all;
- a demoted dependant does not run `up` until the dependency is `Stable`
  again — i.e. the demotion goes through `WaitUp` and not straight to
  `Upping`;
- a node with no dependants demotes nothing and costs nothing.

-------------------------------------------------------------------------------

## Not in any milestone

The design's milestone list is about the *execution model*. These are things
the model now wants from the rest of the tree, plus the loose ends seven
milestones left behind. (R1) is the one that matters.

### R1. Nodes have no `check`, so almost nothing is actually supervised

**The single highest-value item in this document, milestones 8 and 9
included.**

`check` is the only thing in the model that can notice an effect going away.
Counting assignments across `salmon-ops/src/Salmon/Builtin/Nodes/` and
`salmon-ops-recipes/src/`: 21 sites in 12 files, against ~91 `op` nodes in
the builtins alone. And the misses are the *common* nodes —
`Filesystem.filecontents` and `Filesystem.dir` have none — the module's two
checks are `replaceDirectory`'s inner move and `destroyDirectory`, both
`skipIfDirectoryIsMissing` — nor does `Bash.run`, nor does
`Systemd.systemdService`. A node with no `check` answers `Unknown`, which
milestone 7 deliberately never acts on, so it is brought up once and
thereafter politely polled to no effect. The engine is real and tested; on
a real graph today it does nearly nothing.

This was already logged as an ordering question in §"Open questions"
("wants exercising on two or three real long-running nodes before milestone 7
hardens it. Tracked in `todo`"). Milestone 7 landing sharpens it: the
question is no longer "does this shape work" but "which nodes get a `check`".

Three candidates, in the order I would do them:

1. **`Systemd.systemdService`** — `systemctl is-active <unit>`, the same
   check-a-command's-output shape as `Netfilter.rule`'s
   `skipIfNftRuleExists`, which CLAUDE.md already holds up as the template.
   Highest value per line in the repo: these are the nodes that are actually
   long-running services, and the node currently runs `systemctl restart`
   unconditionally on every pass (`up = reload >> enable >> up`, where `Up`
   renders to `restart`), so a check makes it idempotent *and* supervisable
   in one change. Note the interaction to get right: a unit with `Restart=`
   is already supervised by systemd, so salmon's own `Supervision` for such a
   node should be `Never` or `OnFailure` and never `Always` — two supervisors
   fighting over one service is worse than one.
2. **`Filesystem.filecontents`** — compare the file's contents with what the
   node holds. Correct rather than approximate (`skipIfFileExists` would say
   `Success` for a file with the wrong bytes), and cheap in the only sense
   that matters here: the node's whole content is *already* in memory as
   `Text`, so reading it back costs what is already spent. An invalid-UTF-8
   read throws, which `runCheck` contains as `Failure`, which runs `up` —
   the safe direction.
3. **`Filesystem.dir`** — `doesDirectoryExist`. Trivial;
   `skipIfDirectoryIsMissing` is right there, inverted.

**Each of these changes what `run up` does for every existing caller**: a
node whose check says `Success` stops being re-applied. That is an
improvement (it is what the `check` convention is *for*, and CLAUDE.md's
idempotency section already asks for it) but it is a behaviour change on the
author's own infra, which is why milestone 7 deliberately did not smuggle any
of them in. Land them one at a time, each with its own commit and its own
Layer-1 test, so a regression is attributable.

### R2. An operator cannot address a node, so the mailbox is unreachable

`Salmon.Op.Mailbox` is built, `Upkeep.instruct` is built and tested, and
`Force`/`Satisfy`/`Recheck`/`Pause`/`Resume` all mean something to the FSM.
Nothing in the `serve` input language can name a node, so none of it is
reachable except from Haskell.

The route is already there and cheap: `parseSelection` /
`resolveWorldSelectors` turn `--select P`/`--exclude P` path globs into a
`Set Ref`, which is exactly `instruct`'s argument. So:

```
force   [--select P]... [--exclude P]...
recheck [--select P]... [--exclude P]...
pause   [--select P]... [--exclude P]...
resume  [--select P]... [--exclude P]...
```

One caveat to design for: `Serve.startTending` currently builds a fresh
supervisor per idle period, and posting to a mailbox that is about to be
discarded does nothing. Either these commands act on the *next* supervisor
(hold the instruction in the world and hand it to `startUpkeep`, which is
honest — the machines are stopped while a command is being handled, by
construction), or supervisors start outliving commands. The first is much the
smaller change and probably the right semantics anyway: "force this node next
time you look at it".

### R3. `statusOutput` has no reader

The bounded per-node ring is written (the machine narrates its transitions,
and milestone 8 gives it real process output) and nothing reads it. `status`
cannot: the supervisor is stopped while any command is handled, so the
`TVar`s are gone by the time it runs.

Two options, and the second is better. Have `stopTending` snapshot each
machine's final `Status` into the `World` before discarding it — which also
gives `status` a live `CheckResult` and a "last active" age per node, not
just `Pending`/`Converged`/`Errored`. That is a genuinely better `status`
output and it is the thing an operator wants when a node is `Errored`. (The
alternative, keeping supervisors alive across read-only commands, buys
liveness at the cost of the determinism §7 deliberately bought.)

### R4. `query`/`run tree`/`run dag` still print the *declared* graph

Known and recorded at milestone 5. Registered `Rewrite`s apply to `run
up`/`run down`/`run serve` but not to the three commands that *describe* a
graph, so `query` shows twenty `deb` nodes where `run up` will run one
`apt-get`. The obstacle is structural rather than an oversight: a rewritten
`Dag` has `Ref`s and edges and no **paths**, and `--select` matches path
globs (`Query.resolveSelectors` walks a `Cofree`). Printing the computed
graph needs either a renderer that does not exist or ref-addressed patterns.
Worth doing before anyone relies on `query` to predict a run; not worth doing
speculatively.

### R5. Supervisor-level restart — "let it crash" is only half wired

§"The supervision tree" wants two levels: the upkeep FSM handles *the managed
effect stopped*, and a supervisor handles *the machine managing it died*.
Milestone 7 has the monitoring (`stopUpkeep` does `waitCatch` on every
machine and reports `Escaped`) and not the restart — a machine that throws is
reported and gone until the next idle period rebuilds every machine anyway.
That is a tolerable accident of "supervisors are rebuilt per idle period" and
stops being tolerable the moment one outlives a command (see R2). A machine
throwing is a bug in `Salmon.Actions.Upkeep` rather than a node failure, so
the honest fix is to keep it loud rather than to make it survivable.

### R6. Bounding concurrency: still no primitive

§"Bounding concurrency" decided in two parts and shipped the first
(collections, milestone 5). The second — a bounding primitive for the case a
collection cannot express, e.g. two batches fighting over the dpkg lock
across *different* rewrites — is explicitly deferred and still is. Nothing
has needed it. Worth remembering that convergence has been parallel and
unbounded since milestone 6 and the only protection is an edge or a
collection.

### R7. Two dead bindings

- `Salmon.Op.GraphFold.postOrderM` (`salmon-core`) has no in-repo caller
  since milestone 4 moved both drivers onto `Dag`. Kept deliberately as a
  `salmon-core` primitive — it is a reasonable thing for a library to offer —
  but nothing in this repository exercises it any more, so it should be
  either used or dropped rather than left ambiguous.
- `Salmon.Actions.Serve.historyLines` is unused (`historyLinesMatching (const
  True)`; `history` goes through the `Matching` version). One line to delete.

-------------------------------------------------------------------------------

## The order I would do it in

Not the milestone order, because (R1) outranks milestone 9 and arguably
milestone 8.

1. **R1, one node at a time** — `Systemd.systemdService`, then
   `Filesystem.filecontents`, then `dir`. This is what turns milestone 7 from
   a tested engine into something that does anything, and it is the only item
   here whose value does not depend on another item landing. Three small
   commits, each with a Layer-1 test.
2. **R3** — snapshot `Status` into the `World` on `stopTending`. Small, and it
   is how you will *see* whether (R1) is working on a real graph. Do it early
   for that reason.
3. **Milestone 8**, with §8.5's `stableAfter`/`giveUpAfter` folded in rather
   than deferred. This is the biggest single piece left and the one that
   restores what deleting `Supervised` gave up.
4. **R2** — the four instruction commands. Cheap once 8 exists, and much more
   useful then: `pause` on a node that owns a process is a real operational
   verb.
5. **Milestone 9**, whose hazard (§9.3) wants `stableAfter` from step 3 and
   whose thundering-herd risk (§9.1) wants a real graph from step 1 to
   measure against.
6. **R4**, **R6**, **R7** as they become annoying. None is blocking anything.

## Relationship to the other specs

Unchanged from the design's own section, with one thing now firmer:
`specs/salmon-as-init.md` needs milestone 8 specifically, not this work in
general. Its PID-2 supervisor is the upkeep FSM (landed), but a supervisor
that cannot own a process cannot be an init system's — so init is gated on 8
and on R1, and not on 9. The Rust PID 1 remains unaffected: that boundary is
about `waitpid(-1)`, and everything here waits on specific children.

`specs/advance-querying.md` is R4. `specs/multi-user-privilege-separation.md`
still composes unchanged — an `Invoker` decorates the `CreateProcess` a node
spawns, which is as true of a `Managed` node's process as of a `OneShot`'s.
