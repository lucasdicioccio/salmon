# What is left of `specs/per-node-state-machines.md`

Status: living plan, update as work continues. Companion to
`specs/per-node-state-machines.md` (the design, whose milestone list is the
source of truth for 1–9) — this file is what remains, why each remaining
piece is worth doing, and what order I would do it in. Read the design first
if you need the "why" of the model; read this if you want to know where
things stand or what to pick up next.

-------------------------------------------------------------------------------

## Where this stands

Everything below is on branch `serve-supervision`, on top of `0bc3ed4`.

### Shipped

| # | milestone | commit |
|---|-----------|--------|
| 1 | `check :: IO CheckResult` — `prelim` absorbed, `Check.hs`/`Notify.hs` deleted | `52ab4f8` |
| 2 | `Salmon.Op.Dag` — the `Cofree` collapse, pure, both directions, conflicts reported | `042297e` |
| 3 | `Salmon.Op.Ledger` — per-declaration contributions, nodes *and* edges, retiring | `323f626` |
| 4 | both synchronous drivers over the magma and ledger; the cycle hole closed | `c12c32b` |
| 5 | `Salmon.Op.Rewrite` — cross-declaration knowledge as a registered post-fold phase | `39d773d` |
| 6 | `Op/Status` + `Op/Mailbox` + `Actions/Concurrent` — one thread per node | `14259cc` |
| 7 | `Actions/Upkeep` + `Op/Supervision` — nodes are *tended*, not applied once | `41f181f` |
| 8 | `Extension.managed` + `Nodes/Daemon` — a node can own its process | `ff3faaf` |
| 9 | `supStrategy` — a node's going away sends its dependants back to `WaitUp` | `f03afc2` |

And three things that are not milestones:

| what | commit |
|------|--------|
| (R1), first of three nodes: `Systemd.systemdService` has a `check` | `f7aec15` |
| (I1) fixed: a bounce is believed over a stale check | `f7aec15` |
| `salmon-ops-serve-fixture --daemon`, so 8 and 9 can be seen by hand | `12fb625` |
| (R1), the other half: `CheckResult.Immaterial`, and a node that answers it parks | `1a53d95` |
| (R1), second of three nodes: `Filesystem.filecontents` has a `check` | `126e0d4` |
| (R9): `supReapply`, and `Filesystem.dir` sets it — settles (R1)'s third node too | `72e1d55` |

156 tests pass, Layer 3 included. `cabal test salmon-ops-recipes --test-option=-j1`.
Each milestone is marked *landed* in the design, with its deviations recorded
in place there; this table is the index, not the record.

The honest summary of where this leaves things: **the execution model is
finished, and the nodes have started catching up with it, but only three
have.** Nine milestones built a per-node state machine, a ledger, a rewrite
phase, two concurrent drivers and a supervisor that can own a process and
bounce what stands on it — all of which ask each node one question, "is your
effect still in place, or is it cheap enough to just make sure?", that only
`systemdService`, `filecontents` and `dir` answer today, out of roughly
ninety builtins. That is (R1) properly closed rather than (R1) proven
worthwhile: the mechanism now visibly *works* on a real graph — the fixture
self-heals a removed directory with nobody typing anything — but most of
this repository's nodes still have no opinion about their own effect going
away, and giving them one remains exactly the per-node work it always was.

### Left

Nothing is blocking anything else. (R1) is done — every builtin that most
recipes actually declare (`systemdService`, `filecontents`, `dir`) now has
an opinion about its own effect going away, one way or another. What
remains is visibility and reach: seeing what the engine is doing (R3),
addressing it from the `serve` input language (R2), and the smaller,
independent items below.

| id | what | size | note |
|----|------|------|------|
| ~~**R1**~~ | ~~all three nodes done~~: `systemdService`, `filecontents` have a `check`; `dir` has `supReapply` instead | — | done; §R1 |
| ~~R9~~ | ~~a `Supervision` opt-in for "re-apply me on the loop, it is cheaper than asking"~~ | — | done; §R9 |
| **I6** | a re-declaration that changes a node's *content* does not re-apply it | medium | mostly closed by `filecontents`' check; §I6 |
| R3 | `statusOutput` has no reader — nobody can see a failed node's last lines | small | how you would *see* R1 working |
| R2 | no operator command addresses a node, so the mailbox is unreachable | small | `pause`/`force`/`recheck` mean something now |
| R4 | `query`/`tree`/`dag` print the declared graph, not the rewritten one | medium | = `specs/advance-querying.md` |
| R5 | supervisor-level restart is half wired (monitored, not restarted) | small | milestone 9's `Under` did most of it |
| R6 | no concurrency-bounding primitive; convergence is unbounded | medium | deliberate so far |
| R7 | two dead bindings (`postOrderM`, `historyLines`) | trivial | |
| R8 | `Restart` means two different things (`Systemd` vs `Supervision`) | trivial | did *not* bite doing R1's first node |
| I2 | `supStrategy` is authored on the dependency, not the dependant | — | taste; §I2 |
| I3 | `supStableAfter` carries two unrelated meanings | 15 min | taste; §I3 |
| I4 | the `RestForOne` cascade needs opting in at every hop | — | taste; §I4 |
| I5 | adoption refreshes a machine's supervisor but not its policy | small | do with R5 |

### The tradeoffs, in one place

Every milestone departed from the design somewhere; those are recorded in the
design's own milestone list, in place, so they are read next to what they
changed. The ones that are still *live decisions* — where a different answer
is defensible and reversing it is a real option — are the (I) items above and
in §"Landed, but wanting another iteration". The three worth knowing without
reading further:

- **`run up` no longer restarts a healthy systemd unit** (`f7aec15`). That is
  what giving a node a `check` costs, and it is the intended improvement, but
  it is a behaviour change on existing infra.
- **Supervision only runs while `serve` is idle** (milestone 7). A piped
  script is therefore never supervised, which keeps `serve < script`
  deterministic and makes the feature invisible to any scripted test.
- **`Unknown` never restarts anything** (milestone 7). Right, given nearly no
  node has a `check` — and the reason (R1) is worth more than any remaining
  milestone was.
- **A node with no `check` is no longer watched at all**, it is *parked*
  (`Immaterial`). Strictly speaking this removes something: before, such a
  node was woken once a minute. What it was woken to do was call `pure
  Unknown` and go back to sleep, so nothing is lost except the illusion that
  it was being looked after — which is the point. It makes the (R1) gap
  legible instead of hiding it behind a busy-looking loop.
- **One builtin, `Filesystem.dir`, now re-applies on a timer under
  supervision rather than sitting parked** (R9, `supReapply`). It is opt-in,
  narrow (an author-declared claim that `up` is cheap and idempotent), and
  read nowhere but `Actions/Upkeep`, so nothing about `run up`\/`run down`
  changed to land it — but it is a real behaviour change under `run serve`:
  a `dir` node that used to sit silent between commands now calls
  `createDirectoryIfMissing` again every time its delay elapses, for as
  long as the effect is stable that is at most once a minute, never zero.

-------------------------------------------------------------------------------

## Landed, but wanting another iteration

These are not open work items in the sense (R1)–(R8) are: each one is
implemented and shipped, and — (I1) excepted — the code does something
coherent today. They are the places where the *shape* was decided under a single milestone's
pressure and a different answer was defensible — so they want a second pass
with the whole thing built, rather than a bug report. (I1) turned out to be a
bug and is fixed; (I6) is the one left that is not a matter of taste.

(I1) is fixed — it lost a running process outright, which was not a matter of
taste. (I6) is now the one that matters most here: it is not a milestone-9
decision at all, and it says something uncomfortable about what a convergence
pass currently does.

### I1. A demoted node consults its own `check` — *fixed*

**This was a bug rather than a fork, and it is fixed; what follows is the
record.** `salmon-ops-serve-fixture --daemon --stale-check` gives a daemon
node a plausible health check ("my log file exists"); before the fix,
driving the loop and changing the config it stood on produced:

```
Signalling "web" 15
Reaped "web"
serve: daemon sent back to wait: ... stopped being up
```

...and nothing after it. The process was torn down and never restarted, and
the node settled into `Up` claiming its effect was in place.

`demoting` sends a node to `waitUp`, which comes back through
`attempt ... Consult` — and `Consult` asks the node's own `check` first. A
node whose check says `Success` therefore reports `Skip` and settles straight
back into `Up` without re-running anything (`Actions/Upkeep.hs`, `attempt`).
For a one-shot node that is merely a missed bounce; the effect really is
still there, and the check is right. For a `managed` node it is a lie the
supervisor tells about itself: the machine cancelled the action on its way
out of `watch`, so the process is *certainly* gone, and any check that says
otherwise is stale by construction.

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

**The middle answer was the one taken**, and writing it sharpened the rule
one step further: the discriminator is not "this node has a `managed`
action" but **"this machine was holding the effect when it was sent back"**.
A demotion out of `watch` re-applies (`Regardless`); a demotion out of
`resting` consults. The difference matters for exactly one shape — a managed
node whose action forked and exited, which `afterExit` then watches from
`resting` as an unowned effect. Re-applying *that* would start a second copy
of something already running, which is the case milestone 8 went out of its
way to avoid.

So the genuinely contestable half — whether a *one-shot* node's demotion
means re-apply or re-evaluate — is untouched and still open, decided on its
own merits rather than under the pressure of a bug. `Test/UpkeepSpec.hs`
pins both halves, and the first of the pair fails by timing out if the
`Regardless` is reverted.

Milestone 8's own ordering rule is the precedent and points the same way: it
consults the check before the policy because "a process that exits 0 because
it daemonised is still up, and the check is the only thing that can say so".
The demotion case is the exact opposite — *we* stopped the process, so the
check is the only thing that cannot say anything useful.

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

### I6. A re-declaration that changes what a node *is* does not re-apply it

Not a milestone-9 decision at all — it predates it, and milestone 9 is only
how it came to light. `Serve` records convergence per `Ref`, and a
re-declaration that changes a node's *content* leaves its `Ref` alone: the
magma's last-writer-wins swaps in the new representative, but the node stays
`Converged` and the gate skips it. The pass says `converging (0 down, 0 up)`
and does nothing at all.

Watchable in the fixture: `only --name web --daemon --greeting goodbye` after
an `up ... --greeting hello` produces an empty pass, and the new content only
lands afterwards, when the config node's own machine looks and finds the file
saying something other than what it should. **A node with no `check` — which
is very nearly all of them — keeps the old content indefinitely.** So today
`filecontents` with changed content is a no-op on re-declaration, and the
operator has no way to tell.

This is (R1) wearing a different hat, and it is the strongest argument for
(R1) so far: the checks are not only how drift is noticed, they are currently
the only way a *deliberate* change is applied at all.

**Mostly closed since, by (R1)'s second node.** `filecontents` now compares
its bytes, so a re-declaration that changes a config file's content *is*
picked up — by the tending machine rather than by the pass, which is the
second of the two paths described below and the one this document said to
lean on. Two things that leaves. The pass still reports `converging (0 down,
0 up)`, so an operator watching the pass still cannot tell that anything
changed; the change lands quietly, a moment later, when the machine looks.
And it still only works for a node with a `check` — `dir` is the remaining
one that has none, though for `dir` there is nothing content-bearing to
re-declare, so the residual case is narrow.

**The fork.** Reset a node's convergence when its representative changes,
which needs `sameRepresentative`'s comparison to be trusted for this purpose
(it compares shorthand, help, notes and the rendering of `dynamics` — not
`up`, which is where the content actually lives, so a `filecontents` whose
bytes changed compares *equal* and this does not work without giving nodes a
content-bearing identity). Or accept it and lean on (R1), making "a node
worth re-declaring with different content is a node that needs a `check`" an
explicit convention in the node-author docs.

-------------------------------------------------------------------------------

## Not in any milestone

The design's milestone list is about the *execution model*. These are things
the model now wants from the rest of the tree, plus the loose ends seven
milestones left behind. (R1) is the one that matters.

### R1. Nodes have no `check`, so almost nothing is actually supervised

**The single highest-value item in this document, milestones 8 and 9
included. One of the three candidates below is now done, and the framing has
changed underneath the other two — see "What `Immaterial` settled" below.**

`check` is the only thing in the model that can notice an effect going away.
Counting assignments across `salmon-ops/src/Salmon/Builtin/Nodes/` and
`salmon-ops-recipes/src/`: 21 sites in 12 files, against ~91 `op` nodes in
the builtins alone. And the misses are the *common* nodes —
`Filesystem.filecontents` and `Filesystem.dir` had none — the module's two
checks were `replaceDirectory`'s inner move and `destroyDirectory`, both
`skipIfDirectoryIsMissing` — nor does `Bash.run`, nor did
`Systemd.systemdService`. Two of those are now done; `dir` and `Bash.run`
are not. A node with no `check` answers `Immaterial` (it
answered `Unknown` until this change), which the upkeep FSM parks, so it is
brought up once and thereafter watched by nothing. The engine is real and
tested; on a real graph today it does nearly nothing.

This was already logged as an ordering question in §"Open questions"
("wants exercising on two or three real long-running nodes before milestone 7
hardens it. Tracked in `todo`"). Milestone 7 landing sharpens it: the
question is no longer "does this shape work" but "which nodes get a `check`".

Three candidates, in the order I would do them:

1. **`Systemd.systemdService`** — *done*. `Systemd.checkService` shells out
   once to `systemctl show --property=ActiveState --property=UnitFileState
   --property=NeedDaemonReload`, and `Systemd.interpretShow` (pure, tested in
   `Test/SystemdSpec.hs`) draws the verdict. Three departures from the
   one-line sketch above, each found by writing it:

   - **`is-active` alone is not enough, because this node's own dependency
     rewrites the unit file before the check ever runs.** Comparing the bytes
     on disk against what we would write can therefore only ever say "they
     match", and a changed unit would be rewritten and never restarted.
     `NeedDaemonReload` is systemd's own record of "the file changed since I
     loaded it" and is the only thing that still remembers.
   - **A transitional state is `Unknown`, not `Failure`.** `activating`,
     `deactivating` and `reloading` mean the service has not gone away, and
     treating them as gone is how a slow starter becomes a restart loop.
     This is the first place in the tree where `Unknown` is the *right*
     answer rather than the absence of one.
   - **`UnitFileState` earns its place** on its own: a unit somebody
     `systemctl disable`d is still running, so `ActiveState` says everything
     is fine right up until the next reboot.

   The behaviour change is the expected one and is documented on the
   function: a unit that is installed, enabled, loaded and running is now
   *skipped* by `run up` rather than reloaded-enabled-restarted every time.

   (R8) did **not** come due here, contrary to the prediction below: the
   collision only bites a module that needs both `Restart`s in scope, and
   this one never imports `Salmon.Op.Supervision` — the supervision policy
   for a systemd unit belongs on the caller's nodes, not on this one. The
   interaction still to get right when someone does write one: a unit with
   its own `Restart=` is already supervised by systemd, so salmon's
   `Supervision` for it should be `OnFailure` or `Never` and never `Always`
   — two supervisors fighting over one service is worse than one.
2. **`Filesystem.filecontents`** — *done*. `Filesystem.checkFileContents`
   compares the bytes on disk with the bytes the node would write. Correct
   rather than approximate (`skipIfFileExists` would say `Success` for a file
   with the wrong bytes), and cheap in the only sense that matters here: the
   node's content is *already* in hand, since `up` is about to encode it
   anyway. Comparing bytes rather than decoded text also sidesteps the
   invalid-UTF-8 question the sketch worried about, and covers the
   `ByteString`/`Aeson.Value` instances for free. Four things found in the
   writing:

   - **The size is compared first**, and a mismatch answers without reading.
     One `stat`, and it bounds what a node holding a few hundred bytes reads
     if something else has clobbered its path with something enormous.
   - **The reason must not quote the contents.** Failure text goes into
     reports, and this node writes `pgbouncer` userlists and `postgrest`
     configurations with signing keys in them. `Test/FilesystemSpec.hs` pins
     that.
   - **This is what made (R1)'s *first* node actually work.** `systemdService`
     writes its unit file through `filecontents` and then asks systemd
     whether the unit needs reloading — and systemd answers that from the
     file's mtime. Rewriting byte-identical contents on every pass therefore
     set `NeedDaemonReload=yes` on every pass, so `checkService` said
     `Failure` on every pass and reloaded-and-restarted a healthy service.
     The claim in `f7aec15` that a healthy unit is now *skipped* was true of
     `checkService` in isolation and false of the graph it sits in, until
     this landed. Verified against a real `systemctl --user` unit: rewriting
     identical bytes flips `NeedDaemonReload` to `yes`.
   - **The `EncodeFileContents (IO a)` instance is a hazard**, and the only
     one. The check runs the encoder, so a side-effecting generator runs once
     more per look and a non-deterministic one (a timestamp) makes the node
     rewrite its file on every pass. Safe direction, but documented on the
     function; such a node wants a stable encoder or a `check` of its own.
     Nothing in the tree uses that instance today.
3. **`Filesystem.dir`** — *settled, and not with a `check`*. (R9) landed
   and `dir` is what it was written for: it declares `supReapply` rather
   than comparing `doesDirectoryExist`, so under `run serve` it re-runs
   `createDirectoryIfMissing` on the tending loop instead of asking a
   question that would have cost the same `stat` for no extra information —
   `doesDirectoryExist` and `createDirectoryIfMissing` are within noise of
   each other, so there was nothing to buy by asking first. Nothing changes
   under a one-shot `run up`\/`run down`: the field is read only by
   `Actions/Upkeep`, and `dir`'s check still answers `Immaterial` either
   way. See (R9) for the mechanism and `Test/UpkeepSpec.hs`'s
   `dirSelfHeals` for the end-to-end case — a real `dir` node, a real
   `rmdir` behind salmon's back, put back with nobody re-declaring
   anything.

Milestone 8 narrows this in one respect and widens it in another. A node that
owns its process needs no `check` at all to be supervised — the action's exit
is the authority, which is most of what ownership was for — so
`Nodes/Daemon.hs` works today with nothing added. It made the gap sharper for
everything salmon does *not* own, which is every service already under
systemd, and that is the gap the first candidate above has now closed: a unit
that stops behind salmon's back is noticed and restarted, and one whose file
changed is reloaded.

Doing it also produced the first evidence that this list is in the right
order. Giving a real node a real check is what turned (I1) from a fork into a
demonstrated bug — a service node *has* a check, so it walked straight into
being torn down and left down — and the two landed together for that reason.

#### What `Immaterial` settled

The complaint above bundled two things that turn out to be separable, and
separating them is most of what this item needed.

The first is **coverage**: a node that can stop being true on its own, with
nothing in the model able to notice. That is unchanged, and it is what the
two remaining candidates fix.

The second was **a category error in the default**. "This node has no check"
and "this node's check ran and could not tell" were the same answer,
`Unknown`, and the FSM had to treat them the same way — which meant polling
78 of 100 builtins once a minute to call `pure Unknown`. `CheckResult` now
has a sixth constructor for the first case. `Immaterial` means *there is
nothing here worth asking about*: applying the effect costs about what
finding out would, which is exactly the property that makes those nodes
idempotent in the first place (`mkdir -p`, `ip route replace`, `ALTER SYSTEM
SET`, an append-if-missing). It is the default, so no node author writes it;
the one-shot drivers map it to `Required` and cannot tell it from `Unknown`,
so `run up` is byte-for-byte unchanged; and under `Actions/Upkeep` a node
that answers it **parks** — blocked on its mailbox, its demoting
dependencies and its own action, with no delay ladder — instead of polling.

Three things that buys, none of them the obvious one:

- **`Unknown` now means only what it says.** It was carrying two meanings,
  and every rule about it had to be written for the weaker one. The systemd
  check's transitional states are the case that wants the real `Unknown`,
  and they now have it to themselves.
- **The gap is legible.** A parked node is visibly not being watched.
  Before, a node nobody could supervise looked identical, from the outside,
  to one being supervised successfully — same `NextLook` line every 60s.
  Making the engine stop pretending is what turns (R1) from a note in a
  document into something an operator can see in `status`.
- **The cost of the default is now proportional to what it claims.** A node
  that says "don't ask, just apply me when something would have applied me"
  is asked exactly once, learns that, and stops. It costs one check per
  supervisor rather than one per minute.

What it does *not* do is make those nodes self-healing: a parked `dir` whose
directory somebody removed stays parked. Doing something about that is (R9),
and it was deliberately not part of this change — it is a second, independent
decision about whether re-applying on a loop is acceptable, and it belongs to
the node author rather than to `CheckResult`. (R9) has since landed and
`Filesystem.dir` now makes that decision; see §R9.

#### The behaviour change the remaining two carry

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

### R9. "Re-apply me on the loop; it is cheaper than asking" — *done*

The second half of the `Immaterial` design, deliberately not landed with the
first. `Immaterial` says *don't poll me*; it said nothing about what a
supervisor should do for a node whose effect is cheap to re-apply and can
still go away — `Filesystem.dir` was the whole argument. A `dir` that
somebody `rmdir`s was not noticed before `Immaterial`, was not noticed after
it either, and would not have been noticed by (R1) candidate 3 as a `check`
would have — at the cost of a `doesDirectoryExist` per node per minute,
which is the trade `Immaterial` exists to avoid making silently.

The shape landed as sketched: `Salmon.Op.Supervision.supReapply`, a `Bool`
field on `Supervision`, opt-in on the node, meaning "on the tending loop,
just run `up` again rather than asking". The node keeps the delay ladder it
would otherwise have parked out of, and the ladder's meaning inverts — it is
now a rate limit on re-application rather than on looking. `Filesystem.dir`
sets it; nothing else in the tree does.

The three things flagged as needing to be got right, and how each landed:

- **It re-runs `up` on a schedule, forever.** Still true, still not the
  default, still narrow: `supReapply` defaults to `False`, `defaultSupervision`
  sets it `False`, and it is documented on the field as sound only for an
  `up` that is genuinely cheap *and* genuinely idempotent. `Bash.run`,
  `cabal-build`, `git-repo`'s `clone >> pull` and `rsync:send-dir` remain
  `Immaterial` and none of them set it.
- **It interacts with `RestForOne`.** Landed by *not* going through
  `unsettle`\/`Upping` at all: a successful reapply calls neither, so
  `statusEpoch` never moves and a `RestForOne` watcher sees nothing — which
  is correct, since the node never stopped being up from a dependant's point
  of view. A reapply that *fails* still reaches a watching dependant, but
  through the existing `markFailed`\/failed-set path `crossing` already
  reads, not through the epoch — so no new mechanism was needed for the one
  case that does need to be seen. Pinned by
  `reapplyDoesNotDemoteDependants` (success, several times, nobody sent
  back) and `failingReapplyGivesUp` (failure, folded into the ordinary
  `supGiveUpAfter`\/backoff machinery via the same `failed` function a
  one-shot `up` failure uses).
- **The reporting has to distinguish it from a restart.** A new `Reapplying`
  report takes `NextLook`'s place for such a node — filtered from `serve`'s
  output the same way `Parked` is, visible in `status`. Distinguishing it
  from a real restart turned out to need no extra signal beyond that: a
  restart passes back through `Upkeep act Upping`, and a reapply never does
  — pinned by `reapplyStaysInUp`, which asserts `Upping` is reported exactly
  once (the original arrival) across several successful reapplies.

One thing not anticipated when this was written: a node holding a running
action (`managed`) had to be excluded explicitly. Such a node's `up` throws
by convention (see `Nodes/Daemon.hs`), so `supReapply` is read only by
`resting` (the non-holding loop); `watch` (the holding one) treats anything
other than `Poll` as `Park`, regardless of the field. Pinned by
`managedIgnoresSupReapply`.

`filecontents` was the evidence for the other half of the original argument:
it wanted a real check, because comparing bytes is *better* than
re-applying — it is exact, it is one `stat` in the common case, and
re-applying would have churned the mtime that `systemdService` reads. `dir`
has none of those properties: there is nothing to compare beyond existence,
and `createDirectoryIfMissing` costs about what `doesDirectoryExist` costs.
So `dir` got this field rather than a check, and nothing else in the tree
has both properties at once — the two R1 nodes and this one between them
cover the shapes that exist today; a fourth node wanting either treatment
should re-read this section's argument rather than copy whichever one is
closer.

## The order I would do it in

(R1) and (R9) are done. What is left is visibility and reach, not coverage.

0. ~~**I1**~~, ~~**R1**'s first node~~ and ~~**R1**'s default~~ — done.
   The first two together, because the node with a real check is what proved
   the bounce-over-stale-check question was a bug rather than a preference.
   Then `CheckResult.Immaterial`: the half of (R1) that is one decision
   rather than per-node work, and that makes the rest of it visible — a node
   nobody can supervise now says so instead of emitting a `NextLook` a minute.

1. ~~**`filecontents`**~~ — done, and it turned out to be the node that made
   (R1)'s first one work: `systemdService`'s unit file goes through it, and
   rewriting identical bytes was setting `NeedDaemonReload` on every pass.
   It also mostly closes (I6), and it let the fixture's config node drop its
   hand-rolled check for `checkFileContents`.

2. ~~**R9, and with it `dir`**~~ — done. `supReapply` landed as sketched: a
   `Bool` on `Supervision`, read only by the non-holding loop, deliberately
   outside `unsettle`\/`Upping` so a successful reapply cannot fire
   `RestForOne`, folded back into the ordinary failure machinery when it
   throws. `Filesystem.dir` sets it and is now the third (R1) node — closed
   without a check, which is the answer §R1 candidate 3 was undecided about.
3. **R3** — snapshot `Status` into the `World` on `stopTending`, and read the
   live `TVar` for a holding machine. Small, and it is how you will *see*
   whether (R1)\/(R9) are working. Milestone 8 also gave the output ring
   real content, so this is now the difference between having a failed
   service's last log lines and not.
4. **R2** — the four instruction commands. Cheap, and much more useful now
   that `pause` and `force` mean something to a node that owns a process.
5. **R4**, **R5**, **R6**, **R7** as they become annoying. None is blocking
   anything, and milestone 9 shrank (R5): the `Under` refresh it had to add
   is most of what a supervisor-level restart would have needed to hand a
   replacement machine.
6. **I2**–**I5** whenever there is an opinion to apply. None of them is
   urgent and none is a bug; (I3) is fifteen minutes, (I5) is worth doing
   with (R5) since both are about what an adopted or replaced machine is
   handed, and (I2) and (I4) are questions about what the feature *means*
   that are better answered after somebody has used it on a real graph.

(I6) has no place in that order because it is not a step, and it has largely
answered itself: giving `filecontents` a check made re-declared content land
(through the tending machine, not the pass), which is the second of the two
forks §I6 described. What is left of it is a reporting question — the pass
still says `converging (0 down, 0 up)` while something is in fact about to
change — and that belongs with (R3), not here.

## How milestones 8 and 9 actually went

Kept because both departed from the design in ways worth knowing before
touching that code, and because §9.1 and §9.3 below are the reasoning the
milestone-9 departures are corrections *to*.

### Milestone 8: `Managed` nodes

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

### Milestone 9: `rest_for_one`

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

#### 9.1 The exact gap, as the code stands

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

#### 9.2 It has to be a per-node choice

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

#### 9.3 The hazard to design against

A node that flaps — check fails, check succeeds, check fails — with
`RestForOne` dependants demotes and re-runs its whole cone on every flap.
`supStableAfter` is the mitigation and it exists now (milestone 8): demote
dependants only once the node has been down long enough to count against the
tally, not on the first failed check. Milestone 9's job is to *use* it, not
to add it.

#### 9.4 What to test

- a dependant already `Up` is demoted when its dependency leaves `Up`, and
  comes back after it does;
- with `OneForOne` (the default) it is not demoted at all;
- a demoted dependant does not run `up` until the dependency is `Stable`
  again — i.e. the demotion goes through `WaitUp` and not straight to
  `Upping`;
- a node with no dependants demotes nothing and costs nothing.

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
