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
| (R7): dropped `postOrderM` (dead since milestone 4), deleted unused `historyLines` | `69692a9`/`8a3dc9e` |
| (R3): `stopTending` snapshots every machine's `Status` onto its node; `status`\/`query` show it | `8a3dc9e` |
| (R2): `force`\/`recheck`\/`pause`\/`resume [--select P]...` reach the mailbox | `484c738` |
| (R4), half of it: `run tree`\/`run dag` print the *computed* `Dag`, not the declared graph | `d57f2a5` |

159 tests pass, Layer 3 included. `cabal test salmon-ops-recipes --test-option=-j1`.
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
an opinion about its own effect going away, one way or another; (R3) is
done — a node's last word about itself, and a failing one's last output, are
now visible in `status`/`query` rather than write-only; and (R2) is done —
`force`/`recheck`/`pause`/`resume` reach a node from the `serve` input
language. What remains is the smaller, independent items below.

| id | what | size | note |
|----|------|------|------|
| ~~**R1**~~ | ~~all three nodes done~~: `systemdService`, `filecontents` have a `check`; `dir` has `supReapply` instead | — | done; §R1 |
| ~~R9~~ | ~~a `Supervision` opt-in for "re-apply me on the loop, it is cheaper than asking"~~ | — | done; §R9 |
| ~~R3~~ | ~~`statusOutput` has no reader~~ — snapshotted onto `NodeState`, shown in `status`\/`query` | — | done; §R3 |
| ~~R7~~ | ~~two dead bindings~~ (`postOrderM`, `historyLines`) — dropped and deleted | — | done; §R7 |
| ~~R2~~ | ~~no operator command addresses a node~~ — `force`/`recheck`/`pause`/`resume` do now | — | done; §R2 |
| **I6** | a re-declaration that changes a node's *content* does not re-apply it | medium | mostly closed by `filecontents`' check; §I6 |
| ~~R4~~ | ~~`query` prints the declared graph, not the rewritten one~~ (`tree`/`dag` done; `query`'s selection is now rewrite-aware via a `#ref` fallback) | — | done; §R4; = `specs/advance-querying.md` |
| ~~R5~~ | ~~supervisor-level restart is half wired~~ — a crashing machine now restarts in place | — | done; §R5 |
| ~~R6~~ | ~~no concurrency-bounding primitive~~ — a global, optional cap now exists | — | done; §R6 |
| ~~R8~~ | ~~`Restart` means two different things~~ (`Systemd` vs `Supervision`) | — | done; §R8 |
| I2 | `supStrategy` is authored on the dependency, not the dependant | — | taste; §I2 |
| ~~I3~~ | ~~`supStableAfter` carries two unrelated meanings~~ — split into `supDemoteEvery` | — | done; §I3 |
| I4 | the `RestForOne` cascade needs opting in at every hop | — | taste; §I4 |
| ~~I5~~ | ~~adoption refreshes a machine's supervisor but not its policy~~ — a changed policy is now a differing representative, so it is not adopted at all | — | done; §I5 |

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

**A reframing that dissolves most of the "long lever" worry.** The argument
against assumed a bounce is a cost the dependant's author didn't sign up for.
But the tree already asks every node's `up` to be idempotent — safe to run
twice — as a base convention (see "Conventions for node authors" in
CLAUDE.md). Read `RestForOne` as *"go recheck yourself"* rather than
*"you are being torn down and rebuilt whether you like it or not"*, and an
unwanted bounce on a well-written node is just a wasted no-op `check`, not a
disruption. That reframes the shared-resource case in the worked example
above (§I2's `tlsCert`/A/B/C): service C being bounced unnecessarily is a
cheap re-verification, not an incident, *provided* C's own `up`/`check` are
actually idempotent — which is already the convention every node is supposed
to follow regardless of `RestForOne`.

What this does **not** cover: a node whose reapplication is genuinely
expensive or unsafe to repeat — slow warmup, an expensive connection pool
rebuild, a migration that isn't safely re-runnable. Such a node has no way
today to resist a demotion sent by an upstream `RestForOne`; `supStrategy`
only speaks from the dependency's side, there is no dependant-side veto.

**Conclusion for now: no implementation change to I2.** The current
dependency-side authoring is fine as long as the idempotency convention
holds. The real gap is a *future*, separate piece of flexibility: a
dependant-side mechanism for a node to declare "don't force-reapply me from
a `RestForOne` demotion" (or otherwise resist/absorb it), for the nodes that
are the exception rather than the rule. That belongs in `Extension`
alongside `up`/`check`/`down` — a per-node capability the node author
supplies, the same way `check` itself is — rather than as another top-level
field bolted onto `Supervision`. Not scoped further than that; noted here so
it isn't lost, not because it's next.

### I3. `supStableAfter` carried two unrelated meanings — *done*

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

Landed as the fork's first option: `Salmon.Op.Supervision.supDemoteEvery ::
Micros` is a new field read only by `tooSoon`; `countFailure` keeps reading
`supStableAfter`. `defaultSupervision` sets both to `seconds 10`, so nothing
changes for a node that has not thought about it — every existing
`defaultSupervision{...}` record update is untouched, since the constructor
call sites that needed updating were only `defaultSupervision` itself
(positional `Supervision` construction has no other caller in the tree).

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

### I5. Adoption refreshes a machine's supervisor, but not its policy — *done*

`startUpkeep` writes a new `Under` into every machine it adopts, so an
adopted machine follows its current supervisor's statuses, failure set,
neighbour lists and halt flag. It does not rewrite `ctxPolicy`
(`Actions/Upkeep.hs`), so a `managed` node whose `Supervision` changes keeps
the old one for as long as it stays adopted — which under `serve` is
indefinitely.

Nor was the change detectable: `Dag.sameRepresentative` compared the
*rendering* of `dynamics`, and a `Dynamic` renders as its type alone by
default, so a node whose policy changed and whose ref did not was
"unchanged" and got adopted rather than replaced. This predates milestone 9
— but milestone 9 put a second thing in `Supervision` that matters to other
nodes, so a stale policy has reach beyond its own node.

**The fork was resolved with the second option**: `sameRepresentative` now
sees a changed policy. `Dag.showDynamic` special-cases
`Salmon.Op.Supervision.Supervision` to render by value (via its own `Show`
instance) rather than by the `Dynamic` default of its type name alone; every
other `Dynamic` payload (`Package` and the rest) is untouched. A node
re-declared with a changed `Supervision` is therefore a genuine
`Representative` change: `startUpkeep`'s adoption test
(`Dag.sameRepresentative (machineAct m) act`) fails for it, so the old
machine is `Released` (its action cancelled through its bracket, tearing
down whatever it held) rather than adopted, and a fresh machine starts under
the new policy — no separate "refresh the policy in place" step was needed,
because a fresh machine already starts with the right one. A node
re-declared with an *unchanged* policy still compares equal and is adopted
exactly as before, which is the overwhelmingly common case this must not
regress.

This was the "more honest fix" the fork called out: the same comparison
feeds `dagConflicts`/`UpDown.Conflicting`, so a policy-only disagreement
between two live declarations sharing a `Ref` is now reported there too, not
just silently resolved by adoption's own logic.

Pinned two ways. `Test.DagSpec` (`changedSupervisionIsAConflict` /
`sameSupervisionIsNotAConflict`) checks the pure comparison directly: two
declarations of one `Ref` differing only in `supStrategy` are a
`dagConflicts` entry; two declarations with identical `Supervision` are not.
`Test.UpkeepSpec` (`changedPolicyIsNotAdopted` / `unchangedPolicyIsAdopted`,
group "adoption sees a changed Supervision policy (I5)") checks it end to
end through the real mechanism: a managed node's action never returns on its
own, so `startUpkeep` called a second time either releases it and starts a
fresh one — observed as a second spawn of the action — or adopts it and
does not, depending only on whether the policy differs between the two
calls. Verified the first of those two actually depends on the fix by
reverting `Dag.showDynamic` and confirming `changedPolicyIsNotAdopted` times
out (the stale machine is adopted and never re-runs).

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

### R2. An operator cannot address a node, so the mailbox is unreachable — *done*

`Salmon.Op.Mailbox` was built, `Upkeep.instruct` was built and tested, and
`Force`/`Satisfy`/`Recheck`/`Pause`/`Resume` all meant something to the FSM —
but nothing in the `serve` input language could name a node, so none of it
was reachable except from Haskell. Four commands close that:

```
force   [--select P]... [--exclude P]...
recheck [--select P]... [--exclude P]...
pause   [--select P]... [--exclude P]...
resume  [--select P]... [--exclude P]...
```

(`Satisfy` gets no command, matching this section's own "the four instruction
commands" — it is `Query.forceSkip`'s territory, decided at declare time, not
an operator's run-time say.) They reuse `parseSelection` /
`resolveWorldSelectors` exactly as sketched — the same `Set Ref` `status`\/
`query`\/`converge --select` already compute — and an empty selection means
every node, same as those three.

The caveat this section flagged landed as the smaller option it named: **the
instruction is queued, not posted.** `Tending` gained `tendingPending :: IORef
(Map Ref [Instruction])`; the command handler resolves the selection and
queues onto it (oldest first per node, so a `pause` then a `resume` is
delivered in that order) and immediately reports how many nodes matched
(`Instructed`). Nothing is posted into a mailbox at that moment — `loop`
already runs `stopTending` before every command, `status` included, so there
is never a live one to post into regardless of whether the target is a
one-shot or a holding machine. `startTending` drains the whole queue into
`Upkeep.instruct` the moment the next supervisor's machine table exists —
after adoption, so both a freshly-started machine and an adopted one see it —
and clears it. This is exactly "force this node next time you look at it",
and needed no change to `Upkeep.startUpkeep`'s signature: the queue is
delivered from the caller's side, after the call returns, not threaded
through it.

A selected node that no live machine ever answers to (excluded from every
active epoch, retired, or simply never reached by tending) silently drops the
instruction at delivery time, same as `Upkeep.instruct` already does for any
unknown `Ref` — there was nothing to queue it *for* once its target never
showed up. `Instructed`'s count is therefore a statement about the selection,
not a delivery receipt; the two can differ and that is not a bug.

Milestone 8's promise is now real: `pause` on a node that owns a process
stops tending it without touching the running service, and `force` on one is
how to restart something that is healthy and currently has no other way to
be told to. See `Test.ServeSpec`'s `forceOverridesASatisfiedCheck` (a second
`up` with the check never once unsatisfied — the only thing that can explain
it is `force` itself) and `pauseThenResume` (an effect allowed to vanish
while paused, and confirmed *not* put back until `resume`).

### R3. `statusOutput` has no reader — *done*

The bounded per-node ring is written (the machine narrates its transitions,
and milestone 8 gives it real process output) and nothing read it. `status`
couldn't: the supervisor is stopped while any command is handled, so the
`TVar`s were gone by the time it ran.

Landed as the first of the two options sketched here: `stopTending`
snapshots every machine's `Status` — read from the `Upkeep.Supervisor` via
`Upkeep.supervisorStatuses`, before `Upkeep.stopUpkeep` partitions it into
stopped and kept — onto a new `nodeStatus :: Maybe Status` field on
`NodeState`. `status`/`query` render it: a `[CheckResult]` on every node's
summary line, and — the thing milestone 8 made worth more than it was, since
the ring now carries a managed process's actual stdout/stderr — the tail of
a failing node's output ring underneath, capped at ten lines so one wedged
node cannot bury the rest of the listing.

Freshness needed no new mechanism: every command already runs `stopTending`
before it is handled (see `loop`), so a snapshot is never more than one
command old, and a holding machine is re-adopted (and so re-snapshotted)
into the next supervisor the next time tending starts — which happens
before every command too. The second option sketched here (read a holding
machine's `TVar` live rather than snapshotting it) turned out to buy nothing
extra given that rhythm, so it was not built.

Pinned by `Test.ServeSpec.statusShowsAFailingNodesOutput`: a node whose `up`
never stops throwing is declared, fails synchronously once, and is then
picked up by the idle tending loop (it is `Unsettled`, not yet `Converged`)
— which is what actually produces the `Failure` this test reads back
through `status`, not the declaring pass. Verified by hand too: chmod a
directory read-only, declare a `dir` under it, and `status` shows
`[Failure "...: permission denied"]` with the repeated `up` / error lines
underneath.

### R4. `query`/`run tree`/`run dag` still print the *declared* graph — done

Known and recorded at milestone 5. Registered `Rewrite`s apply to `run
up`/`run down`/`run serve` but not to the three commands that *describe* a
graph, so `query` shows twenty `deb` nodes where `run up` will run one
`apt-get`. The obstacle is structural rather than an oversight: a rewritten
`Dag` has `Ref`s and edges and no **paths**, and `--select` matches path
globs (`Query.resolveSelectors` walks a `Cofree`). Printing the computed
graph needs either a renderer that does not exist or ref-addressed patterns.

Landed for the half of this that has no `--select` to reconcile with paths in
the first place: `run tree`/`run dag` take no selection at all, so nothing
about them needed the fork above resolved before writing the renderer.
`CommandLine.hs`'s `Run RunTree`/`Run RunDAG` now fold and rewrite the graph
the same way `runUp`/`runDown` do (`computedTreeDag`, sharing
`Rewrite.wholeGraph`'s "everything desired, nothing ignored" `Phase`) and
print the resulting `Dag` through two new renderers: `Help.printDagTree` and
`Dot.printDagCograph`. Both are one line per `Ref` rather than one per path
— a node reached from several declarations is printed once, the way it is
walked once — with dependencies listed underneath (`printDagTree`) or as
plain edges (`printDagCograph`, which necessarily drops the
red/orange/gray `Connect`/`Overlay` distinction `Dot.printCograph` draws
from `Shape`, since a `Dag` has already collapsed both into "depends on").
With no rewrites registered this is the same nodes and edges as before,
just collapsed to one line/node instead of one per tree position — verified
by hand against the `salmon-ops-serve-fixture` binary (no rewrites
registered): `run tree`/`run dag` on a plain bundle directive show the same
four nodes either way, just without the duplicate positions a shared node
used to get.

`query` was the holdout, and the part that actually needed the fork this
section opened with resolved — it is the one of the three whose whole job is
resolving a `--select`/`--exclude` pattern, which only paths could do.

**Resolved with option (2)**: resolve a pattern against the declared graph
as before, and translate the result through `membersOf`. Landed as
`Query.resolveRewrittenSelectors` (`Salmon.Actions.Query`), a drop-in for
`resolveSelectors` that `CommandLine.hs`'s `QueryShow`/`QueryPlan` handlers
now call, passing the same whole-graph `Rewritten` `run tree`/`run dag`
already compute (`computedTreeDag` factored into `computedRewritten`, so the
`Rewritten` — not just its `computedDag` — is available to `query` too).
Ordinary path-glob patterns are untouched — `resolveRewrittenSelectors`
degrades exactly to `resolveSelectors`'s behaviour when no `#`-pattern is
given, checked by test.

**The fallback lookup**, added alongside rather than instead of (2): a
declared path genuinely cannot address a rewrite-introduced node (a
package-install batch, say) at all — such a node has no position in the
declared tree, since it was never declared, only ever produced after the
fold. So a pattern beginning with `#` matches by `Ref` instead of by path: a
prefix of either `shortRef` or the full ref text, checked against every
declared node *and* every computed (rewrite-introduced) node, with a
computed match expanded through `membersOf` back to the declared nodes it
stands in for. This deliberately mirrors `renderAnnotated`'s own `"
#" <> shortRef ref` disambiguation suffix (already printed today next to a
colliding path, and — since `run tree`/`run dag` moved to `Dag`-based
renderers — the same short-ref text that would identify a batch node there)
so that text a render prints can be pasted straight back in as a selector,
symmetric with how `git` short hashes work.

Both kinds of pattern union rather than override each other within one
`--select`/`--exclude`, and an empty `--select` list still means
"everything" when checked against the *combined* pattern list (not just its
path half) — the bug the first draft had, caught by
`rewrittenEmptySelectStillMeansEverything` before it shipped: an
exclude-only `--exclude '#...'` with no `--select` at all must still select
everything else, not silently narrow to nothing.

Every result is still a set of *declared* refs, deliberately: `query
plan`'s `phaseIgnored` and a rewrite's own `collectDynamic` are both keyed
on declared refs (`runUp`'s `Phase` is built and consumed before any
rewrite's batching decision), so this needed no change to what `run up`
consumes — addressing a batch by its ref and excluding it is exactly
equivalent to excluding every declared package that went into it, which is
the only coherent meaning available to a plan computed before any rewrite
runs. `query show`'s rendering is unchanged too (still the annotated
*declared* tree via `printAnnotated`) — only what a `--select`/`--exclude`
pattern can *match* changed; nothing needed the tree it annotates to become
the computed one, since the declared identities are still the more useful
thing to show next to `[selected]`/`[excluded]`.

See `Test/QuerySpec.hs`'s `resolveRewrittenSelectors`-prefixed cases, which
cover: plain path patterns unchanged; a `#ref` pattern addressing a plain
declared node directly; one addressing a batch and expanding to its
declared members; a path and a `#ref` pattern combining within one
selection; and the empty-select-still-means-everything edge case above.

### R5. Supervisor-level restart — *done*

§"The supervision tree" wants two levels: the upkeep FSM handles *the managed
effect stopped*, and a supervisor handles *the machine managing it died*.
Milestone 7 had the monitoring (`stopUpkeep` does `waitCatch` on every
machine and reports `Escaped`) and not the restart — a machine that threw was
reported and gone until the next idle period rebuilt every machine anyway.
That was a tolerable accident of "supervisors are rebuilt per idle period"
and stopped being tolerable the moment one outlives a command (see R2).

Landed as milestone 9 made it smaller: restarting a machine in place needs
nothing beyond handing the replacement its supervisor's current state, and
`Upkeep.Under` — a `TVar` a machine re-reads on every wait rather than
closing over — already *is* that, since it exists precisely so an adopted
machine sees a live supervisor rather than a dead one's maps. `startUpkeep`
now runs every machine through a new wrapper, `restarting`, instead of
`machine` directly: a crash is reported (`Escaped`, on every attempt — "the
honest fix is to keep it loud" turned out to mean *report each restart
loudly*, not *decline to restart*) and the machine restarts in place,
re-entering as `Unsettled` rather than wherever the dead one's closure
remembered. `Unsettled` is what makes the very next step a fresh `Consult`
rather than a blind `up` — nothing survived the crash, not even the
assumption that the effect is still there. A fixed `delayFloor` pause (not
the adaptive ladder, which is a policy about the node, not about this
module's own bugs) separates one restart attempt from the next, so a bug
that fires on every entry cannot spin a core.

One hazard found in the writing, not anticipated by the sketch above: the
restart wrapper's `try @SomeException` must not catch an *asynchronous*
exception. `releaseKept` tears a holding machine down by `cancel`ling its
thread — throwing `AsyncCancelled` into it — specifically because such a
machine ignores the halt flag and has no other way to be stopped; a wrapper
that treated that as a crash and restarted the machine would defeat the
teardown `releaseKept`'s caller is waiting on. `SomeAsyncException` is
matched via `fromException` and re-thrown untouched instead.

Named `restarting` rather than `supervised`, which was the obvious name and
already taken — `Salmon.Op.Supervision.supervised :: Supervision -> Dynamic`
is the smart constructor that attaches a policy to a node's `dynamics`, an
unrelated thing. Recorded so a future reader does not reach for the same
name a second time; see (R8) for the collision this project already has of
this shape.

### R6. Bounding concurrency — *done*

§"Bounding concurrency" decided in two parts and shipped the first
(collections, milestone 5). The second — a bounding primitive for the case a
collection cannot express, e.g. two batches fighting over the dpkg lock
across *different* rewrites — was explicitly deferred and still is: **no
per-resource primitive is added here**, deliberately. Two nodes contending
for one specific thing is still an edge or a collection's job, and nothing
in this item changes that.

What landed is the smaller, orthogonal thing this section used to only note:
a single global knob capping how many nodes are inside their own
`check`/`up`/`down` at once across one pass, for machines where unbounded
*width* itself is the problem (CPU/IO contention, an outbound connection
limit, file descriptors) rather than any particular pair of nodes fighting
over a particular resource. `Salmon.Op.Concurrency.ConcurrencyLimit` is a
thin wrapper over a `QSem`; `newConcurrencyLimit` builds one from a positive
`Int` (`error`s on `<= 0`, since a limit of zero would deadlock every gated
action rather than mean "run nothing" — that is what excluding every node
from the pass already says) and `withConcurrencyLimit` holds one slot for
the duration of an `IO` action, a no-op for `Nothing`.

`Salmon.Actions.Concurrent.upDagConcurrent`/`downDagConcurrent` (and the
`walkConcurrent` both share) take a `Maybe ConcurrencyLimit`; `Nothing`
reproduces every caller's behaviour from before this landed. The slot is
held only around `walkConcurrent`'s `apply` call — a node's own
`check`/`up`/`down` — never around the `STM` wait on its neighbours'
`waitStability`, which is what makes this safe to reason about without a
deadlock analysis: a node cannot even attempt to acquire a slot until every
node it depends on has settled and released its own, so two nodes never
hold a slot each while blocked on one another through this mechanism — the
only thing a wait through it can ever be for is a free slot, never another
node's turn.

`Salmon.Actions.Serve.serveWith` takes the same `Maybe ConcurrencyLimit` and
passes it to *both* halves of a convergence pass — the teardown walk and the
bring-up walk share one limit rather than getting one each, which is correct
because `converge` already awaits the first before starting the second, so
the two never contend for it at the same time. `serve` (no rewrites, for
callers that don't need them) passes `Nothing`, matching its signature
before this landed for anyone not opting in.

`run serve --max-concurrency N` is the CLI surface: `RunServe` gained a
`Maybe Int` (parsed with `optional (option auto (long "max-concurrency"
...))`, so omitting the flag is `Nothing`), and
`execCommandOrSeedWithRewrites` builds the `ConcurrencyLimit` from it right
before calling `serveWith`. `run up`/`run down` are untouched — they run
through the *sequential* drivers (`UpDown.upDag`/`downDag`), which are
already bounded to one node at a time by construction, so there was nothing
for this knob to do there.

One thing worth being explicit about: this bounds a **pass**, not the
tending loop `Actions/Upkeep.hs` runs between commands. A wide `serve`
declaration can still start as many supervised machines as it has nodes;
each one is normally idle (parked, or waiting out its own delay ladder)
rather than doing work, so the unbounded-width problem this item was written
for is specific to a convergence pass actually *doing* many things at once,
which is exactly what `--max-concurrency` now caps. Bounding the tending
loop itself was not asked for and is a different, larger question — the
loop's own steady-state cost is designed to be near zero per idle node
(see `Actions/Upkeep.hs`'s summary), so there is little evidence yet that it
needs one.

### R8. `Restart` means two different things — *done*

`Salmon.Builtin.Nodes.Systemd.Restart` (rendered into a unit file's
`Restart=` directive; one constructor, `OnFailure`) and
`Salmon.Op.Supervision.Restart` (`Always`/`OnFailure`/`Never`) shared both a
name and a constructor. Nothing imported both, so nothing was broken — but
this was the fourth collision of this kind in this work (`CheckResult`'s
`Success`/`Failure` vs optparse's `ParserResult`, `Mailbox.Skip` vs
`Report.Skip`, and two `Direction`s, the last resolved by *merging* them,
which was not available here).

The two are genuinely different things: one is a string salmon writes into a
file for systemd to read, the other is a decision salmon makes itself.
Renamed `Systemd.Restart` to `Systemd.RestartDirective` (constructor
`OnFailure` untouched, only the type name moved) rather than waiting for a
caller that needs both in scope, since the section's own prediction — a
systemd node with a `check` (R1) is exactly the node that would want a
`Supervision` too — is exactly the situation (R1) already created for
`systemdService`. Nothing outside `Systemd.hs` named the old type (checked
across both `cabal.project` and `cabal.perso.project`'s package sets), so
this was a same-module rename with no call-site fallout.

### R7. Two dead bindings — *done*

- `Salmon.Op.GraphFold.postOrderM` (`salmon-core`) had no in-repo caller
  since milestone 4 moved both drivers onto `Dag`, and its own module doc
  still named `upTree` as the caller it was written for — stale as well as
  dead. Dropped rather than kept speculative (it was untested, and this
  document's own "used or dropped" framing asked for a decision); recover it
  from history if a caller needs it again. `foldWithContext`, the module's
  other export, is not dead — `Actions/Dot.hs` uses it — and is untouched.
- `Salmon.Actions.Serve.historyLines` was unused and not even in the
  module's export list (`historyLinesMatching (const True)`; `history` goes
  through the `Matching` version directly). Deleted.

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

(R1), (R9) and (R3) are done. What is left is reach and a handful of
independent small items, not coverage or visibility.

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
3. ~~**R3**~~ — done. `stopTending` snapshots every machine's `Status` onto
   its `NodeState` before the `Upkeep.Supervisor` holding the live `TVar` is
   dropped; a holding machine gets a fresh snapshot too, since it is
   re-adopted before every command. `status`\/`query` show a `[CheckResult]`
   per node and a failing one's last output lines underneath — milestone 8's
   ring finally has a reader.
4. ~~**R7**~~ — done in passing: `postOrderM` dropped (dead since milestone
   4), `historyLines` deleted (dead since it was written). Both trivial,
   both cost nothing to do the moment they were noticed rather than later.
5. ~~**R2**~~ — done. `force`/`recheck`/`pause`/`resume [--select P]...
   [--exclude P]...` parse the same way `status`/`query`/`converge --select`
   already do, and land in a `Tending`-owned queue rather than a mailbox: the
   supervisor is always stopped by the time a command is handled, so there is
   never one to post into at parse time. `startTending` drains the queue into
   the next supervisor's machines — freshly started or adopted — the moment
   they exist.
6. ~~**R5**~~ — done. `startUpkeep` now runs every machine through
   `restarting` rather than `machine` directly: milestone 9's `Under` refresh
   turned out to be most of what a supervisor-level restart needed to hand a
   replacement machine, so the remaining work was the restart loop itself
   (re-entering `Unsettled`, reporting `Escaped` on every attempt) and making
   sure it lets an asynchronous exception — `releaseKept`'s `cancel` above
   all — through untouched rather than treating it as a crash.
7. ~~**R4**~~ — done. `run tree`/`run dag` print the computed `Dag`;
   `query`'s selection is now rewrite-aware too, via
   `Query.resolveRewrittenSelectors` (declared-path patterns, translated
   through `membersOf`, plus a `#ref` fallback for addressing a
   rewrite-introduced node directly). ~~**R6**~~ is done — a global, optional
   `ConcurrencyLimit` bounds a convergence pass's width, reachable as `run
   serve --max-concurrency N`; the per-resource primitive it was explicitly
   *not* about is still nothing more than an edge or a collection.
   ~~**R8**~~ is done — the rename cost nothing to do ahead of a caller
   needing both, once checked.
8. **I2** and **I4** whenever there is an opinion to apply. Neither is
   urgent and neither is a bug — both are questions about what the feature
   *means* that are better answered after somebody has used it on a real
   graph. ~~(I3)~~ and ~~(I5)~~ are done.

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
