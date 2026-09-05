# What is left of `specs/per-node-state-machines.md`

Status: living plan, update as work continues. Companion to
`specs/per-node-state-machines.md` (the design, whose milestone list is the
source of truth for 1–9) — this file is what remains, why each remaining
piece is worth doing, and what order I would do it in. Read the design first
if you need the "why" of the model; read this if you want to know what to
pick up next.

Written after milestone 9 landed (`git log --oneline` on `serve-supervision`).

## Headline: no milestones left, and a residue that matters more than they did

**All nine milestones have landed.** Each is marked *landed* in the design,
with its deviations recorded in place there.

**What remains is of two kinds.** A residue no milestone covers — some of it
bookkeeping, but item (R1) is not: it is the reason milestones 7, 8 and 9
supervise almost nothing on a real graph, and it was already worth more than
the last milestone. And, separately, five things that *did* land but were
shaped under one milestone's pressure and want a second pass now that the
whole thing exists; §"Landed, but wanting another iteration" is those, and
(I1) is the one that gets worse rather than better as (R1) proceeds.

The honest summary of where this design stands: **the execution model is
finished and the nodes have not caught up with it.** Eight milestones built a
per-node state machine, a ledger, a rewrite phase, two concurrent drivers and
a supervisor that can own a process, all of which ask each node one question
— "is your effect in place?" — that roughly a quarter of nodes can answer.

## Where 1–8 got to, in one line each

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
8. `Extension.managed` and `Salmon.Builtin.Nodes.Daemon` — a node can own a
   running process. `Up` races the action, exit codes reach the policy,
   teardown escalates through the action's own bracket to the process group,
   and a machine holding a process is `Kept` across commands rather than
   stopped with its supervisor.
9. `Supervision.supStrategy` — a node that declares `RestForOne` sends its
   dependants back to `WaitUp` when it stops being up. Opt-in, so it costs
   nothing until used; `Status.statusEpoch` is what makes a brief departure
   impossible to miss; `Upkeep.Under` is what an adopted machine is handed so
   that it keeps following a supervisor it did not start under.

-------------------------------------------------------------------------------

## Milestone 8: `Managed` nodes — landed

Landed as planned, with six departures recorded in place in
`specs/per-node-state-machines.md`. Three are worth knowing here because they
change what the remaining work looks like:

- **`Lifecycle` is a field, not a sum** (`managed` beside `up`), as this plan
  recommended. If a third lifecycle ever appears, that is when to pay for the
  sum.
- **A machine holding a process is `Kept` across commands.** This was not in
  the plan and is the largest thing milestone 8 added: `serve` stands its
  machines down before every command, so a supervisor that wound its
  processes down with it would restart every service on every `status`.
  Holding machines survive and the next supervisor adopts them, on exactly
  the condition §"`Ref` is location-addressed" named — still wanted up, and
  its representative unchanged.
- **The restart policy got `supStableAfter`/`supGiveUpAfter`**, folded in
  rather than deferred, as recommended. Which also means milestone 9's
  flapping hazard (§9.3) already has its mitigation available.

The `Restart` name collision the plan flagged is **still latent** — see (R8).

## Milestone 9: `rest_for_one` — landed

Landed with five departures, recorded in place in
`specs/per-node-state-machines.md`. Two of them are corrections to the
sketch below rather than choices, and both are worth knowing before touching
this code:

- **§9.1's "watch the dependencies' statuses" cannot work as written.** A
  level read of `Stability` misses every departure it is for — a dependency
  that fell over and recovered between two of a dependant's waits looks
  identical to one that never moved, and a rewritten config file is exactly
  that shape. `Status.statusEpoch` (monotonic, bumped when a settled node
  unsettles) is what the dependant compares against instead.
- **§9.3's use of `supStableAfter` as a settling delay would swallow the case
  the feature is for**, for the same reason: the config is back within
  milliseconds. It is a rate limit on *repeat* demotions instead — an
  isolated departure is always honoured, a second one inside the interval is
  dropped.

Two things the sketch got right and one it did not anticipate: the strategy
is per-node and authored on the node that goes away (§9.2), the cascade
needed no code, and the thundering herd (§9.1) does not arise at all, because
a machine with no opted-in dependency subscribes to nothing. What it did not
anticipate is that an adopted machine had to be *handed* its new supervisor's
state — see the design's fourth departure, and (R5) below, which this made
smaller.

The original analysis follows, for the reasoning behind the shape.

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
`supStableAfter` is the mitigation and it exists now (milestone 8): demote
dependants only once the node has been down long enough to count against the
tally, not on the first failed check. Milestone 9's job is to *use* it, not
to add it.

### 9.4 What to test

- a dependant already `Up` is demoted when its dependency leaves `Up`, and
  comes back after it does;
- with `OneForOne` (the default) it is not demoted at all;
- a demoted dependant does not run `up` until the dependency is `Stable`
  again — i.e. the demotion goes through `WaitUp` and not straight to
  `Upping`;
- a node with no dependants demotes nothing and costs nothing.

-------------------------------------------------------------------------------

## Landed, but wanting another iteration

These are not open work items in the sense (R1)–(R8) are: each one is
implemented, tested and shipped, and the code does something coherent today.
They are the places where the *shape* was decided under a single milestone's
pressure and a different answer was defensible — so they want a second pass
with the whole thing built, rather than a bug report. Ranked by how much the
answer changes.

(I1) is the one to settle first. It is the only one of the five where the
current behaviour is arguably wrong rather than merely one of two readings,
and it gets worse rather than better as (R1) lands.

### I1. A demoted node consults its own `check`, so a node that *can* answer is never bounced

`demoting` sends a node to `waitUp`, which comes back through
`attempt ... Consult` — and `Consult` asks the node's own `check` first. A
node whose check says `Success` therefore reports `Skip` and settles straight
back into `Up` without re-running anything (`Actions/Upkeep.hs`, `attempt`).

So `RestForOne` fires only for dependants that *cannot* tell whether they are
up. Today that is nearly every node, which is why the milestone's tests pass
and why it looks like it works. It is exactly backwards from where the tree
is going: the whole of (R1) is teaching nodes to answer that question, and
every node that learns to stops being bounceable. The flagship case is the
casualty — a service with a working health probe reads "still up" and ignores
the configuration that changed underneath it, which is the one thing this
milestone was for.

**The fork.** Either a demotion means *re-apply* — `attempt` entered with
`Regardless` rather than `Consult`, the way a restart from `look` already is
— or it means *re-evaluate*, which is what it means now. The case for the
current behaviour is that a spurious demotion then costs one check rather
than one `up`, and that a node's check is meant to be the authority on
whether work is needed. The case against is that it makes the feature
self-cancelling: the better a node's check, the less `RestForOne` can do to
it.

A middle answer exists and may be the right one: `Regardless` for a node with
a `managed` action (whose process this node has just torn down, so it is
certainly not up), `Consult` for one whose effect persists on its own. That
splits along the line milestone 8 already drew.

### I2. The strategy is authored on the dependency, not on the dependant

§9.2 says the strategy is a per-node knob without saying which end of the
edge it hangs off, and Erlang — where it is a property of the *supervisor* —
does not settle it either, because there is no supervisor here to put it on.
It went on the node that goes away: the config file declares that its going
away matters, and the services reading it say nothing.

The argument for that is real and is in the design doc: the file's author
knows the content is load-bearing, while six services would each have to know
separately that it might change. The argument against is equally real and is
not written down anywhere — **a node's own restarts are its own business**,
and this is the one policy in `Supervision` that lets one node's author
decide something about another node's behaviour. A `RestForOne` on a widely
shared node is a lever with a very long arm, and nothing warns the nodes on
the other end of it.

**The fork.** Keep it on the dependency; move it to the dependant ("bounce me
when anything I stand on moves"); or have both and require them to agree,
which is the conservative option and the expensive one.

### I3. `supStableAfter` now carries two unrelated meanings

It was "having been up this long forgets the earlier failures", read by
`countFailure`. Milestone 9 also made it "do not demote this node twice
inside this interval", read by `tooSoon`. One field, two jobs, and no
particular reason an author would want the same number for both: how long a
service has to run before a crash counts as a new crash rather than a
continuing one is a different question from how often its dependants may be
rebuilt behind it.

This happened because §9.3 named `supStableAfter` as the mitigation and
adding a field looked like exceeding the brief. It was the wrong instinct:
the record is *meant* to grow, `defaultSupervision` makes growing it free for
every node that does not care, and this is precisely the situation the "amend
`defaultSupervision`" convention exists to make cheap.

**The fork.** Split out `supDemoteEvery :: Micros` (defaulting to
`supStableAfter`'s value, so nothing changes for anybody), or accept the
overload and document it as one concept — "the timescale on which this node's
state is meaningful" — which is a defensible reading and is roughly the
justification the current doc gives.

### I4. The cascade needs opting in at every hop

A demoted node unsettles, so a dependant of *it* that also declared
`RestForOne` sees the same thing and goes back too. That is the whole
mechanism, and it means the cascade stops at the first node in the chain that
did not opt in: a `RestForOne` config, a plain service, and something
downstream of the service leaves the downstream node alone.

Erlang's `rest_for_one` restarts everything started after the failed child
regardless of what those children think. Ours is strictly more conservative,
which is the right default for a mechanism landing late — but it means the
name promises more than the behaviour, and an author reading "and everything
after it" will be surprised.

**The fork.** Leave it (and rename, or at least document the difference
loudly); or make the cascade transitive from the declaring node, which means
a demotion carries an "originating ref" out to the whole transitive cone
rather than only to the immediate dependants that opted in.

### I5. Adoption refreshes a machine's supervisor, but not its policy

`startUpkeep` writes a new `Under` into every machine it adopts, so an
adopted machine follows its current supervisor's statuses, failure set,
neighbour lists and halt flag. It does not rewrite `ctxPolicy`
(`Actions/Upkeep.hs`), so a `managed` node whose `Supervision` changes keeps
the old one for as long as it stays adopted — which under `serve` is
indefinitely.

Nor is the change detectable: `Dag.sameRepresentative` compares the
*rendering* of `dynamics`, and a `Dynamic` renders as its type alone, so a
node whose policy changed and whose ref did not is "unchanged" and is
adopted rather than replaced. This predates milestone 9 — but milestone 9 put
a second thing in `Supervision` that matters to other nodes, so a stale
policy now has reach beyond its own node.

**The fork.** Refresh the policy on adoption (cheap, but changing a running
machine's policy mid-flight has its own questions — a node that has given up
under an old `supGiveUpAfter` would need to be reconsidered); or make
`sameRepresentative` able to see it, which means `Supervision` rendering as a
value rather than as a `Dynamic`'s type name, and is the more honest fix
because it makes the change visible to the magma's conflict reporting too.

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

Milestone 8 narrows this in one respect and widens it in another. A node that
owns its process needs no `check` at all to be supervised — the action's exit
is the authority, which is most of what ownership was for — so
`Nodes/Daemon.hs` works today with nothing added. But it makes the gap
sharper for everything salmon does *not* own, which is every service already
under systemd: `Systemd.systemdService` still cannot notice its unit
stopping, and that is the largest single category of long-running effect in
this repository.

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

Milestone 8 raised the value of this considerably: `pause` on a node that
owns a process is a real operational verb (stop tending without killing the
service), and `force` on one means "restart it", which is the thing an
operator most often wants and currently cannot say.

One caveat to design for, now half-solved. A *holding* machine survives
commands, so posting to its mailbox works as-is. A one-shot machine does not:
`Serve.startTending` rebuilds it, and posting to a mailbox about to be
discarded does nothing. So either these commands act on the *next* supervisor
for one-shot nodes (hold the instruction in the world and hand it to
`startUpkeep`), or one-shot machines start being kept too. The first is
smaller and is probably the right semantics anyway: "force this node next
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

Milestone 8 made this worth more than it was: the ring now carries a managed
process's actual stdout and stderr, so a service that failed has its last
lines sitting in a structure nothing can print. That is the single most
useful thing an operator could be shown and it is currently write-only.
A holding machine's `Status` is also genuinely live between commands, so for
those nodes `status` could read the `TVar` directly rather than a snapshot.

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

Milestone 9 made this smaller without meaning to. Restarting a machine in
place means handing the replacement its supervisor's state rather than
whatever the dead one closed over, and `Upkeep.Under` is exactly that,
written into an adopted machine already.

### R6. Bounding concurrency: still no primitive

§"Bounding concurrency" decided in two parts and shipped the first
(collections, milestone 5). The second — a bounding primitive for the case a
collection cannot express, e.g. two batches fighting over the dpkg lock
across *different* rewrites — is explicitly deferred and still is. Nothing
has needed it. Worth remembering that convergence has been parallel and
unbounded since milestone 6 and the only protection is an edge or a
collection.

### R8. `Restart` means two different things

`Salmon.Builtin.Nodes.Systemd.Restart` (rendered into a unit file's
`Restart=` directive; one constructor, `OnFailure`) and
`Salmon.Op.Supervision.Restart` (`Always`/`OnFailure`/`Never`) share both a
name and a constructor. Nothing imports both today, so nothing is broken —
but this is the fourth collision of this kind in this work (`CheckResult`'s
`Success`/`Failure` vs optparse's `ParserResult`, `Mailbox.Skip` vs
`Report.Skip`, and two `Direction`s, the last resolved by *merging* them,
which is not available here).

The two are genuinely different things: one is a string salmon writes into a
file for systemd to read, the other is a decision salmon makes itself. Rename
`Systemd.Restart` to `Systemd.RestartDirective` when something first needs
both — which is likely to be soon, since a systemd service node with a
`check` (R1) is exactly the node that would want a `Supervision` too, and
would then have to decide whether salmon or systemd is supervising it.

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

(R1) is now the whole of what stands between this engine and its doing
anything on a real graph — with one thing to settle first, because (R1) is
what makes it bite.

0. **I1**, before or alongside the first node that gains a `check`. A demoted
   node consults its own check, so every node that learns to answer "am I up"
   stops being bounceable by the config it stands on. Doing (R1) without
   settling this quietly converts `RestForOne` from working-by-accident into
   not working, one node at a time, with nothing failing to say so.

1. **R1, one node at a time** — `Systemd.systemdService` first (it is the
   largest category of long-running effect in the repo and the one salmon
   does not own, so it depends entirely on a `check`), then
   `Filesystem.filecontents`, then `dir`. This is what turns nine landed
   milestones into something that does anything on a real graph, and it is
   the only item here whose value does not depend on another item landing.
   Three small commits, each with a Layer-1 test. Expect (R8) to come due
   while doing the first one. Milestone 9 sharpens the case for
   `filecontents` in particular: a config node with no `check` cannot notice
   its own file changing, so `RestForOne` on it can never fire.
2. **R3** — snapshot `Status` into the `World` on `stopTending`, and read the
   live `TVar` for a holding machine. Small, and it is how you will *see*
   whether (R1) is working. Milestone 8 also gave the output ring real
   content, so this is now the difference between having a failed service's
   last log lines and not.
3. **R2** — the four instruction commands. Cheap, and much more useful now
   that `pause` and `force` mean something to a node that owns a process.
4. **R4**, **R5**, **R6**, **R7** as they become annoying. None is blocking
   anything, and milestone 9 shrank (R5): the `Under` refresh it had to add
   is most of what a supervisor-level restart would have needed to hand a
   replacement machine.
5. **I2**–**I5** whenever there is an opinion to apply. None of them is
   urgent and none is a bug; (I3) is fifteen minutes, (I5) is worth doing
   with (R5) since both are about what an adopted or replaced machine is
   handed, and (I2) and (I4) are questions about what the feature *means*
   that are better answered after somebody has used it on a real graph.

## Relationship to the other specs

`specs/salmon-as-init.md` is **no longer gated on this work**. Its PID-2
supervisor is the upkeep FSM and its restart policy is `Supervision`, both
landed; a node that owns a process is `Nodes/Daemon.hs`, and "restart this
service and everything after it" is `supStrategy`. What it still needs from
here is (R1), for anything it does not own. The Rust PID 1 remains unaffected: that boundary is about
`waitpid(-1)`, and everything here waits on specific children — deliberately,
which is why the polling `getProcessExitCode` reaper the removed
`Supervised` module used was not recovered along with its teardown.

`specs/advance-querying.md` is R4. `specs/multi-user-privilege-separation.md`
still composes unchanged — an `Invoker` decorates the `CreateProcess` a node
spawns, which is as true of a `Managed` node's process as of a `OneShot`'s.
