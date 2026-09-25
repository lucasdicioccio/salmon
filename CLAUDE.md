# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Salmon is a Haskell library/toolkit for expressing infrastructure/provisioning/CI-CD operations
("xyz-dependencies") as DAGs of idempotent operations ("ops"), with uniform up/down/check
semantics regardless of whether a node is as small as "create a file" or as large as "turn a server up".

## Packages (cabal multi-package project)

- `salmon-core` — the core library: the `Graph`/`OpGraph` DAG representation and evaluation
  primitives. No IO-heavy deps; kept minimal on purpose.
- `salmon-ops` — IO-heavy primitives for provisioning/CI-CD tasks (files, systemd, debian
  packages, podman, postgres, wireguard, certificates, ssh, etc.), plus the CLI plumbing
  (`Salmon.Builtin.CommandLine`) that all salmon-based binaries use. Tries to stay light on
  cabal deps but allows heavier deps for genuinely deep tasks (e.g. cert generation/signing).
- `salmon-ops-recipes` — higher-level, opinionated "recipes" built out of `salmon-ops` builtins
  (e.g. `SreBox.PostgresMigrations`, `SreBox.PostgresPair`, `SreBox.MicroDNS`). This is where
  conventions get enforced (e.g. whether migrations ship and run locally vs. via a remote
  connstring). Kept deliberately free of heavy/unstable dependencies.
- `salmon-ops-recipes-experimental` — recipes that need heavier or less-stable dependencies:
  `SreBox.KitchenSinkBlog`/`SreBox.KitchenSinkMultiSites` (pull in the `kitchen-sink` library) and
  `SreBox.GeneratedSite` (builds/publishes kitchen-sink-generated sites via `SreBox.CabalBuilding`),
  plus `SreBox.CertSigning` and the `Salmon.Builtin.Nodes.Acme` builtin (pull in `acme-not-a-joke`).
  Split out of `salmon-ops-recipes` so that package's build stays fast; **not** part of the
  default `cabal.project` package set — it's only built via `cabal.perso.project` (see below).
- `salmon-apps` — blessed, project-useful binaries built from the above (e.g. `salmon-migrator`,
  see `Migrator.hs` / `MigratorApp.hs`; and `salmon-pgpair`, see `PgPair.hs`, whose directive is
  simply a `SreBox.PostgresPair.Pair` — moving a primary is then an edit to one word of the seed).
  `salmon-fleet` (`Fleet.hs`) is the odd one out: not a salmon binary in the seed → directive
  sense but the reader's side of `run serve --status-sink` — `salmon-fleet status DIR` folds a
  directory of status documents into one line per host (`Salmon.Actions.Fleet` is the fold; the
  binary only parses flags and prints). It never writes.
  `salmon-tui` (`Tui.hs`) is the other reader: a `brick` terminal over `Salmon.Client` (below)
  against `run serve --http PATH`'s socket, or `--http-tcp`'s listener given `https://HOST:PORT
  --token-file FILE [--cacert FILE]` — `salmon-tui PATH` reads `/dag` once, follows
  `/events`, and draws the node table with a `:` command line that is the only thing on the
  screen that touches the loop. `brick`/`vty` are dependencies of this package alone.
  `salmon-toy-qemu-pg-ha` (`QemuPgHaToy.hs`) is the same pair on three qemu guests it makes for
  itself, with a client that keeps writing while the primary moves: a demo of
  `specs/pg-switchover.md`, and the throwaway-validation counterpart to `salmon-gcp-toy`. Its two
  seeds split along the only line that matters — `prereqs` is the part that needs root
  (debootstrap, an initrd that can mount a 9p root, handing `/etc/ssh` to whoever runs the rest),
  and everything after it is an unprivileged user with two capabilities granted once. Splitting
  them is also what makes the demo a demo: `prereqs` is slow and rarely changes, `up` is the one
  you re-run, and re-running it with one word changed is the whole show.

Dependency direction is strictly `salmon-core` ← `salmon-ops` ← `salmon-ops-recipes` ←
`salmon-apps`, with `salmon-ops-recipes-experimental` branching off `salmon-ops-recipes` as an
alternate, heavier leaf (nothing in the default package set depends on it).

Some `source-repository-package` git dependencies in `cabal.project` point at the author's other
repos (`acme-not-a-joke`, `prodapi`, `prodapi-proxy`, `purescript-bridge`) pinned by commit hash —
these are not on Hackage. `cabal.perso.project` (untracked, gitignored, alongside its
`.perso.project.local`) carries an extra `kitchen-sink` source-repository-package entry and an
extra `salmon-personal-apps` package used only by the author's personal, non-public binaries.

## Build

```sh
cabal build all
cabal build salmon-core salmon-ops salmon-ops-recipes salmon-apps   # individual packages
cabal build salmon-migrator                                          # a single executable

# the experimental/personal packages aren't in cabal.project's package set; build them via:
cabal build --project-file=cabal.perso.project salmon-ops-recipes-experimental salmon-personal-apps
```

`salmon-ops-recipes` has a real test suite (`cabal test salmon-ops-recipes`); the other packages
don't. It mixes cheap in-process assertions on graph shape/traversal with slower "Layer 2" tests
that dogfood the project's own `Podman` builtins to run real recipes against disposable
containers (see `salmon-ops-recipes/test/Test/Harness.hs` and `Test.PostgresInitSpec` for the
pattern) — those need a working `podman` on the machine running the tests and are skipped loudly
if it's missing.

`cabal.project` carries `allow-newer` pins for `dhall-json` against `aeson`/`bytestring`/`text`;
don't remove these without checking the build still resolves.

## Core architecture (salmon-core)

Understanding these four modules (in `salmon-core/src/Salmon/Op/`) is a prerequisite for touching
anything in `salmon-ops*`:

- **`Graph`** (`Graph.hs`) — an algebraic graph (à la the Alga paper), with one simplification: a
  single `Vertices [a]` constructor instead of separate `empty`/`vertex`/`overlay` branches.
  Three constructors: `Vertices [a]`, `Connect g1 g2`, `Overlay g1 g2`.
- **`OpGraph`** (`OpGraph.hs`) — `OpGraph m node = OpGraph { predecessors :: m (Graph (OpGraph m node)), node :: node }`.
  A node centered on itself with an *effectful* recipe (`predecessors`, in monad `m`) for finding
  its dependency graph — this is what lets "create a file" and "turn a server up" be the same
  type despite wildly different dependency depth. `inject` adds a predecessor via `Connect`
  (ordering matters: "must happen before"); `overlaid` adds one via `Overlay` (co-occurring, no
  ordering implied).
- **`Track`** (`Track.hs`) — `Track m n a = Track { run :: a -> OpGraph m n }`, a contravariant/
  divisible functor: "given an `a`, I know how to produce the `OpGraph` that provisions it." This
  is the composition mechanism for builders — e.g. "if you need a database, here's a `Track` that
  turns a DB spec into the ops that create it." `Tracked` pairs a `Track` with a realized value so
  dependent ops can be built via `mapTracked`/`apTracked`/`bindTracked`/`using`.
- **`Eval`** (`Eval.hs`) — `expand :: OpGraph m node -> m (Cofree Graph (OpGraph m node))` walks
  the effectful `predecessors` recipes and materializes the full dependency graph.

`Actions`/`Act` (`Op/Actions.hs`) is the generic "bag of named effects" type; `Actionless` is the
monoidal no-op used so dependency-free ops still typecheck uniformly.

## salmon-ops layer

- **`Extension`** (`Salmon/Builtin/Extension.hs`) is the concrete payload every op in this repo
  carries: `help`, `notes`, a `ref` (dedup identity, see `Op/Ref.hs`), and IO actions `up`,
  `check` (an `IO CheckResult` answering "is my effect already in place", run before `up`),
  `down`, plus `dynamics :: [Dynamic]` for attaching arbitrary typed metadata that can be
  recovered later via `getDynamics`/`collectDynamics` (used e.g. to flatten "remote call" ops out
  of a graph). One more field, `managed :: Maybe (Output -> IO ExitCode)`, is the long-running
  counterpart to `up`: an action that blocks for as long as the node's effect is up and returns
  why it stopped. `Nothing` for nearly every node. It is two fields rather than an
  `OneShot ... | Managed ...` sum on purpose — the sum is the better type and would rewrite all
  106 `up =` sites in the tree for a feature a handful of nodes use. **Only `run serve` honours
  it** (see `Actions/Upkeep.hs`); the one-shot drivers call `up`, so a node with no meaningful
  `up` should throw from it rather than no-op. `Op = OpGraph Identity Actions'` is the type alias used everywhere in node/recipe
  code. Building a node normally goes through the `op :: ShortHand -> Identity (Graph Op) ->
  (Extension -> Extension) -> Op` helper, starting from `noop`/`nodeps`/`deps`.
- **`Nodes/Daemon.hs`** is the one builtin that fills in `managed`: a process salmon owns and
  keeps running, for where there is no systemd to hand the problem to (a container, a test
  harness, `specs/salmon-as-init.md`'s supervisor). `runDaemon` is the action, exposed so a node
  wanting more than `daemon` offers (a `check` of its own, dependencies, a richer `ref` key) can
  build on it rather than reimplement the teardown. That teardown is an **escalation, not a
  `cancel`**: signal the process *group*, wait `stop_grace`, then `SIGKILL` and wait again —
  recovered from the removed `Nodes/Supervised.hs` at `f9d7116`, because `withCreateProcess`
  sends `SIGTERM` and waits forever behind a service that ignores it, and because a service that
  forks workers has to take them with it (`create_group` is forced on regardless of what the
  caller's `CreateProcess` said). Its stdout/stderr are drained *while* it runs, not after, or a
  process that fills a pipe buffer blocks forever and looks wedged for a reason nobody can see.
  Its `up` **throws** `NeedsSupervisor` (a one-shot driver has nowhere to put an action that
  never returns, and a node that cannot be brought up should say so); its `down` is `pure ()`
  and that is not an inconsistency — under `serve` the process died when its machine was
  cancelled, and under `run down` this process never held one, so there is genuinely nothing to
  stop. See `Test/DaemonSpec.hs`, which covers exactly the parts a fake `IO ExitCode` cannot:
  signals, groups, and pipes.
- **Builtin nodes** live under `salmon-ops/src/Salmon/Builtin/Nodes/` — one module per concern
  (Filesystem, Systemd, Debian.Package, Podman, Postgres, WireGuard, Certificates, Ssh, Git,
  Netfilter, CronTask, Rsync, etc). Look at `Filesystem.hs` as the canonical small example of the
  `op` pattern (a value type like `Directory`/`FileContents`, a smart constructor returning `Op`
  that fills in `help`/`notes`/`ref`/`up`/`down`).
- **`Actions/UpDown.hs`** implements graph execution. Both directions run the same way: `expand`
  the `OpGraph` to a `Cofree Graph`, collapse that to a `Ref`-keyed DAG with `Salmon.Op.Dag`
  (below), then walk it. `upDag` walks it in dependency order — a node is applied once everything
  it depends on is done — evaluating `check` to decide `Skip` vs `Eval`+`up`. `downDag` walks the
  same structure the other way, running `down`. `upTree`/`downTree` are the expand-fold-walk
  wrappers over an `Op`. Both return `IO Bool`, `False` iff anything failed or was blocked.
  A node appears **once** whatever number of paths reach it, so there is no per-occurrence
  report; a `Ref` reached from two differently-described nodes is reported `Conflicting` (see
  `Op/Dag.hs`).
  Failure is contained in whichever direction the walk runs. Going up: `up` throwing is caught,
  reported `Failed`, and everything that (transitively) depends on it is `Blocked` instead of
  being evaluated against an unmet precondition — see "Conventions for node authors" below for
  what that means for how `up` must be written. Going down it is the mirror: a failed `down`
  leaves that node *still standing*, so every one of its predecessors is `Blocked` (unsafe to
  pull a dependency out from under a node that's still up), and a predecessor is blocked if
  *any* of its dependents was — one failure contains a whole still-standing sub-DAG.
  Walking the dependants direction is what teardown needs and is the reason the DAG carries both:
  a node must go down only after *every* node depending on it has, which for a predecessor shared
  by several dependents (the directory two files live in) a naive top-down walk gets wrong — it
  would remove that directory at the *first* dependent reached (a real "directory not empty" bug
  this used to have). A node's own `check` is never consulted for teardown (it answers "does my
  effect still need creating", which isn't the question a teardown asks), so there's no per-node
  "skip if already gone". If an edge set describes a **cycle**, the nodes on it never become
  ready; they're reported `Blocked` at the end of the walk rather than silently skipped.
  Both take an optional `Gate ext = Act ext -> IO Requirement` via `upTreeWith`/`downTreeWith`
  (or `upDag`/`downDag` directly) — a *caller*-supplied "does this traversal want to touch this
  node at all", asked before (and short-circuiting) the node's own `check`, and reported as a
  `Skip`. `upTree`/`downTree` are those with a gate that wants everything; the only real user is
  `Actions/Serve.hs` (below), which walks the union of several seeds' nodes and must leave other
  seeds' alone.
- **`Actions/Concurrent.hs`** is the same two walks with one thread per node. Each node gets a
  `TVar Status` (`Op/Status.hs`) and blocks on `waitStability` over its neighbours — dependencies
  going up, dependants coming down — so STM's `retry` does the scheduling: no counters, no
  ready-queue, no wakeup channel. `Status` also carries a monotonic `statusEpoch`, bumped when a
  settled node unsettles, because `Stability` only answers "where is it now" and something
  watching a neighbour for *departures* needs "did it move while I wasn't looking": a node that
  fell over and recovered between two readings is `Stable` at both of them, and STM keeps no
  queue of what happened in between. Same `Report` stream, same `IO Bool`, same failure containment
  as the sequential drivers. Three things it has to do that they don't: every `runReporter` goes
  through one `MVar` (the caller's reporter isn't assumed thread-safe, and interleaved multi-line
  reports are garbage); `Dag.stuck` is consulted **before** the walk, because a thread waiting on
  a node in a cycle never wakes rather than being noticed at the end; and failure containment
  lives in a `TVar (Set Ref)` beside the statuses, not in `Status`, with "record the failure" and
  "settle" in one transaction — `waitStability` deliberately can't see whether a neighbour
  succeeded, since the two drivers answer "proceed past a failure?" differently.
  **`serve` converges through this, so convergence is parallel and unbounded**; `run up`/`run
  down` stay sequential. The only protection against two nodes contending for one resource is an
  edge between them (or a collection rewrite that makes them one node) — there is no concurrency
  cap, deliberately. `Op/Mailbox.hs` is the push side: a bounded per-node queue of `Instruction`s
  (`Force`/`Satisfy`/`Recheck`/`Pause`/`Resume`) that drops the oldest on overflow and reports
  the drop. See milestone 6 and `Test/ConcurrentSpec.hs`.
- **`Actions/Upkeep.hs`** is the *continuous* driver: a node is not applied once, it is
  **tended**. Same ordering (`waitStability`), same `Report` vocabulary (wrapped in
  `Upkeep.Acted`), but it does not return — each node runs `WaitUp/Upping/Up` (or
  `WaitDown/Downing/Down`) and keeps asking whether its effect is still there. The steady
  state is a check on an adaptive delay: doubling to a 60s cap while the effect is there,
  halving to a 500ms floor when it is not. Five things to know before touching it.
  **`Down` is terminal and `Up` is not** — `check` answers "does my effect need creating",
  and nothing answers "is it still gone", so a downkeep machine that arrives exits while an
  upkeep machine that arrives has only started. **`Unknown` restarts nothing**: the one-shot
  drivers map it to `Required` (safe over one pass of an idempotent action), but a loop that
  did the same would re-run `up` at the delay floor forever. **`Immaterial` is not even
  polled**: it is the verdict from a node with no `check` — the default, and so nearly every
  node in the tree — and it says asking costs what applying costs, which leaves nothing
  cheaper to put on a timer. Such a node *parks*: one look to learn it (what a machine knows
  on the way in is that its `up` ran, not what a check would say), then it blocks on its
  mailbox, its demoting dependencies and its own action, with no delay ladder at all. It is
  still reachable by `Force`/`Recheck` and still bounced by a `RestForOne` dependency; it has
  only stopped asking a question nobody wrote an answer to. A node may opt out of parking with
  `supReapply`: instead of blocking, it re-runs `up` on the same ladder — inverted, now a rate
  limit on how often `up` reruns rather than on how often a check is asked. Sound only for an
  `up` that is genuinely cheap *and* genuinely idempotent (`Filesystem.dir` is the node it
  exists for; a build or a clone is not), ignored for a node holding a running action (whose
  `up` throws by convention), and deliberately outside the `unsettle`/`WaitUp` path so a
  successful reapply never bumps `statusEpoch` and never fires a `RestForOne` watcher — a
  reapply is not the node going away and coming back, so nothing should be told it did. A
  reapply that *fails* rejoins the ordinary failure machinery (`supRestart`, `supGiveUpAfter`)
  exactly as a one-shot `up` failure would. See (R9) and `Test/UpkeepSpec.hs`'s `supReapply`
  group. **A failing `up` backs off** (doubles) while a *vanished effect*
  tightens (halves); only the latter is evidence to look sooner. **Failure is waited out, not
  contained**: where a one-shot pass reports `Blocked` and ends, here the dependency's own
  machine is still retrying, so the dependant keeps waiting and proceeds the moment it
  recovers. And **`Standing`** — the caller says whether a node is already where it wants to
  be, because a supervisor is normally started right after something else did the work, and
  a node with no `check` would otherwise have that work done again immediately.
  `Recheck`/`Pause`/`Resume` finally mean something here. See milestone 7 and
  `Test/UpkeepSpec.hs`.
  A node with a `managed` action runs the same three states with one difference: `Up`
  additionally races the *action itself*, so the `ExitCode` it eventually yields is what the
  restart policy reads instead of a `CheckResult` (`Always`/`OnFailure`/`Never` mapped the
  systemd way, which is only expressible because something can tell `exit 0` from `exit 137`).
  Four things about that path are load-bearing. **The check is consulted before the policy** —
  a process that exits 0 because it daemonised is still up, and nothing else can say so; that
  one ordering handles double-forking for free. **The action runs under `withAsync`**, so
  cancelling the machine cancels the action and whatever bracket it is built from does the
  killing — which is the whole teardown story, and why `Nodes/Daemon.hs` needs no pid table.
  **A machine holding an action ignores the halt flag**: stopping a supervisor means "stop
  tending", so such a machine is *kept* (`Upkeep.Kept`) and adopted by the next supervisor
  rather than wound down — otherwise typing `status` would restart every service. And a
  `Settled` claim is **never** made about a managed node, because `Settled` means "the effect
  persists on its own" and a managed effect does not persist without its machine.
  One more ordering rule belongs to the same family and points the other way. When a node is
  sent back by a `RestForOne` dependency, **a machine that was holding the effect re-applies
  without consulting its check, and one that was not, consults**. Leaving `watch` cancels the
  action, so the effect is certainly gone and any check saying otherwise is stale by
  construction — a pidfile, a port something else holds, a log file that exists because the node
  ran earlier — and believing it would settle the node into `Up` holding nothing at all. The
  discriminator is *holding it right now*, not "has a `managed` action": a node whose action
  forked and exited is watched from `resting` as an unowned effect, and re-applying that one
  would start a second copy of something already running.
  Failure accounting lives in a per-machine `Tally` (consecutive failures, plus when the node
  last reached `Up`), which is what makes `supGiveUpAfter` usable: without `supStableAfter`
  resetting it, a service that falls over once a day reaches any finite limit eventually. A node
  that has given up is *parked*, not gone — `Force` or `Recheck` starts it over. See milestone 8.
  Lastly, **a node leaving `Up` can take its dependants with it**, though by default it does not.
  A dependency whose author wrote `supStrategy = RestForOne` sends every dependant that had
  reached `Up` back to `WaitUp`, to be brought up again on top of whatever it turns into —
  Erlang's strategy of the same name read along dependency edges, and a config file is the case
  for it. Four things make that affordable and are load-bearing. **It is opt-in on the node that
  goes away**, so a machine with no such dependency subscribes to no statuses at all and the
  whole watch is skipped rather than being a branch that never fires; the thundering herd simply
  does not arise. **The watch compares `statusEpoch`, not `Stability`**, or it would miss every
  departure short enough to matter — a rewritten config file is back in milliseconds. **It
  remembers which `TVar` that epoch came from**, since under `serve` a dependency gets a new
  machine on every command, and comparing across the two would read as a departure every time
  anyone typed anything. And **a dependency that has not been seen settled up yet cannot demote
  anybody**, which is what stops this undoing `Standing`. A second demotion inside the node's own
  `supStableAfter` is dropped (that is a flap, not a change); an isolated one is always honoured,
  whenever it comes. The cascade needed no code: a demoted node is itself no longer up, which is
  all a dependant of *it* that opted in has to see. See milestone 9.
  One structure exists only for adopted machines. `Upkeep.Under` is everything that belongs to a
  machine's *supervisor* rather than to its node — the statuses, the failure set, the neighbour
  lists, the halt flag — behind a `TVar` that `startUpkeep` rewrites on adoption. Without it an
  adopted machine watches state nobody maintains any more: it never sees a dependency move, it
  records its failures where no dependant reads them, and the first time it takes a path that
  heeds the halt flag (which, before `RestForOne`, it never did) it reads one that is
  permanently set and quietly exits, orphaning the process it holds.
- **`Op/Supervision.hs`** is the per-node policy the above reads: `Restart`
  (`Always`/`OnFailure`/`Never`, default `OnFailure`), a `Strategy`
  (`OneForOne`/`RestForOne`, default `OneForOne`) and an optional watchdog, carried on
  `dynamics` rather than in a new `Extension` field — the same channel, and for the same
  reason, as `Package` and the collection rewrite. Three things that buys: nothing changes
  for the many nodes with no opinion, "a node that declares no watchdog is never considered
  wedged" is just `getDynamics` returning `[]`, and it is one line to add. Untyped and
  unenforced, so two conflicting policies on one node get a magma-conflict's treatment: take
  the first, report the rest. The policy reads a `CheckResult` for a node whose effect persists
  on its own and an `ExitCode` for one that owns a process. Two more fields exist only for the
  latter's sake: `supStableAfter` (having been up this long forgets the previous failures) and
  `supGiveUpAfter` (stop after this many consecutive ones, default never). `supStrategy` is the
  one field authored for somebody else's benefit — it says what this node's *going away* does to
  the nodes standing on it, and it goes on the config file rather than on the six services
  reading it, because only the file's author knows the content is load-bearing. Its default is
  the opposite kind from `supRestart`'s, deliberately: putting a node back is an active choice
  about that node, while bouncing its dependants is a decision about other people's nodes, so
  nothing happens until somebody says it should. `supReapply` is the field that opts a node out
  of parking on `Immaterial` and into re-running `up` on the loop instead — see
  `Actions/Upkeep.hs`'s summary above for what it does and why it is narrow; `Filesystem.dir` is
  the one builtin that sets it. Prefer amending `defaultSupervision` to spelling out every
  field — the record has grown three times now and will again.
  The watchdog only ever *reports*: killing a wedged `up` would need a bracket the node does not
  necessarily have.
- **`Op/Dag.hs`** is that collapse, lifted out and made pure: `foldDag` turns an expanded
  `Cofree Graph (OpGraph m (Actions ext))` into a `Dag` — one representative per `Ref`
  (`dagNodes`, the *magma*), `dagDependencies` **and** `dagDependants` (the direction the
  `Cofree` cannot answer and a teardown needs), and first-seen `dagOrder`. Two things it does
  that the old inline version didn't. First, edges from *every* occurrence of a node accumulate
  rather than only the first one's, which is what makes folding a second graph into an existing
  `Dag` (`mergeDag`) a merge rather than a replacement. Second, because a `Ref` is
  *location-addressed* — `mkRef` hashes a kind tag plus an author-chosen identity key, so an
  equal `Ref` means "the same effect site", not an equal node — two declarations can collide on
  one node; **last writer wins**, and the representative that lost is recorded in `dagConflicts`
  and reported by `downTree` as `UpDown.Conflicting`. The comparison behind that is
  `sameRepresentative`, on the only fields that *are* comparable (`shorthand`/`help`/`notes`/the
  rendering of `dynamics` — `up`/`check`/`down` are functions, and `Dynamic` renders as its type
  alone by default). One exception: `Salmon.Op.Supervision.Supervision` is rendered by value
  (`Dag.showDynamic`), specifically so a re-declaration that only changes a node's supervision
  policy is a differing representative and is not silently adopted by
  `Salmon.Actions.Upkeep.startUpkeep` with its old policy still in force (see (I5) in
  `specs/per-node-state-machines-remaining.md`). It is still a heuristic — every other `Dynamic`
  payload is unaffected — but strictly more than the zero available before. See
  `specs/per-node-state-machines.md` milestone 2 and `Test/DagSpec.hs`.
- **`Op/Ledger.hs`** is the other half: who still *wants* which nodes. One `Contribution` per
  declaration — a `Set Ref` and a `Set (Ref, Ref)` of precedence edges, plus a `contribLive`
  flag — keyed by whatever identifies the declaration. `desired` is the union of the live ones'
  refs; `precedenceOf` is the union over **all** of them, live and retiring. Two decisions worth
  knowing before touching it. It's a **set, not a refcount**: counting breaks four ways (a
  diamond double-counts its apex, a `down` of something never up goes negative, a re-declared
  seed reaches 2 so one `down` strands it, and two declarations wanting one node cancel each
  other) that a set gets structurally. And a retraction **retires rather than deletes**
  (`contribLive = False`, then `collect` drops it once none of its nodes is still coming down),
  because retracting is exactly when a declaration's edges matter most: they're the only
  remaining statement of what order to tear its nodes down in. See milestone 3 and
  `Test/LedgerSpec.hs`.
- **`Op/Rewrite.hs`** is where cross-declaration knowledge is allowed to live, and the only
  place it can. A recipe supplies `Track' directive`, i.e. `directive -> Op` — a function of *one
  directive in isolation* — so "batch this package with the other packages currently wanted up"
  isn't awkward to write in a recipe, it's inexpressible there. A `Rewrite ext = Phase ->
  Rewritten ext -> Rewritten ext` runs *after* the fold, over the whole magma, and that's simply
  where the missing information first exists. `dynamics` is its input channel (`collectDynamic`),
  which is what that field was always for. `Phase` carries `phaseDesired` (what's wanted up —
  every node for `run up`, nothing for `run down`, the ledger's `desired` under `serve`) and
  `phaseIgnored` (a plan's excluded refs, or a `converge --select`'s complement — a rewrite must
  not batch those, or it runs work the operator asked to skip). A phase that introduces a node
  records what it stands in for via `introduce`; `membersOf` is how the drivers keep speaking in
  declared terms — a batch is worth touching iff some member is, and what happens to it happened
  to all of them. Register phases with `CommandLine.execCommandOrSeedWithRewrites` /
  `Serve.serveWith`; they apply to `run up`/`run down`/`run serve` and, as of (R4), `run
  tree`/`run dag` — both now print the *computed* `Dag` (`Help.printDagTree` /
  `Dot.printDagCograph`: one line/node per `Ref`, dependencies indented underneath, no
  red/orange/gray edge coloring since a `Dag` has already collapsed `Connect`/`Overlay` into
  plain "depends on"). `query` (`Actions/Query.hs`) still displays the *declared* graph — its
  whole job is resolving `--select`/`--exclude` against `pathedNodes`'s tree positions, and a
  rewritten `Dag` has refs and edges but no paths for a pattern to match — but (R4)'s resolved
  fork means a pattern *can* now reach a rewrite-introduced node with no declared position of its
  own: `resolveRewrittenSelectors` resolves ordinary patterns as path globs exactly as before, and
  a `#`-prefixed one by `Ref` instead (a prefix of `shortRef`/the full ref text — the same text
  `renderAnnotated`'s own disambiguation suffix already prints, so a render's output pastes back in
  as a selector), expanded through `membersOf` to the declared nodes it stands in for. `query
  plan`'s exclusion set is still declared refs throughout (`phaseIgnored` and `collectDynamic` are
  both keyed that way), so addressing a batch by ref is equivalent to excluding every declared node
  that went into it. See milestone 5, (R4) in
  `specs/per-node-state-machines-remaining.md`, and `Test/QuerySpec.hs`.
- **`Actions/Serve.hs`** is the long-running counterpart to the one-shot `upTree`: it keeps a
  `World` — a `worldLedger` of who's asked for what, a `worldMagma` of one representative per
  `Ref` (what each node *is*), a `NodeState` per node (a `Direction` it's wanted in plus whether
  it has `Converged` there), and `worldEpochs`, the declared seed / directive / graph, kept only
  for declarations that are still live. Everything else is derived: a node some live declaration
  asks for is wanted `TurnUp`, a node no live declaration still asks for is wanted `TurnDown`,
  and flipping a node's direction resets it to `Pending`. (I6): a re-declaration that keeps a
  node's `Ref` but changes what `Dag.sameRepresentative` can see about it (`help`/`notes`/
  `dynamics`) resets it to `Stale` instead of leaving it silently `Converged` — `record` compares
  the incoming declaration's representative against whatever was already in `worldMagma` for that
  `Ref`. `Stale` is read exactly like `Pending` by `gateFor` (anything but `Converged` gets a
  pass's attention, and the node's own `check` decides from there, same as ever) but is kept as
  its own constructor rather than folded into `Pending` so `status` can tell "never touched" from
  "was up, now re-verifying". This is `Dag.sameRepresentative`'s usual blind spot — content baked
  into `up`'s closure with nothing else about the declaration changed compares *equal* — which is
  why `filecontents` now puts a `contentFingerprint` into its `notes` (see `Nodes/Filesystem.hs`
  above): without that, a re-declared config file's content change is invisible to this check too,
  and only its own `check` running on the tending loop (unaffected by any of this) would ever
  notice. (R3): `NodeState` also carries a
  `nodeStatus` snapshot — the node's own last `CheckResult` and output ring, taken by
  `stopTending` from the live `TVar` the instant before the `Upkeep.Supervisor` holding it is
  dropped, since that `TVar` is otherwise unreachable once the machine has stood down. Freshness
  rides the loop's own rhythm rather than needing its own: every command runs `stopTending`
  first, so `status`'s snapshot is never more than one command old, and a holding machine is
  re-adopted (and re-snapshotted) into the next supervisor rather than frozen at whenever it
  first started holding. Converging is then one teardown pass
  and one bring-up pass (both concurrent, see `Actions/Concurrent.hs`) with a gate that filters
  to "wanted in this pass, not yet converged" — so
  ordering, dedup and failure containment are exactly `run up`/`run down`'s, and all this module
  adds is the memory. Nodes left `Errored`/`Blocked` are retried by the next pass.
  **Between commands the nodes are tended** (`Actions/Upkeep.hs`), which closes the gap a pass
  leaves: an effect that goes away on its own is otherwise unnoticed until somebody types
  `converge`. The machines run **only while the loop is idle** — they start when nothing is
  waiting in the input and stand down before any command is handled (stopping *waits for* an
  `up`/`down` in flight rather than cutting it). That is deliberate twice over: a piped script
  has every line, EOF included, already queued before the first pass ends, so `serve < script`
  is never supervised and stays a deterministic sequence of passes; and starting machines only
  to stop them because a command had been queued all along would make "was this node acted
  on?" depend on thread timing. A node's convergence becomes its `Upkeep.Standing` rather than
  a filter — `gateFor` demands a node has *not* converged, `tendOf` uses convergence to decide
  whether to act or merely watch. `supervise on|off` is the switch; note a restricted
  `converge --select` scopes the *pass*, not the standing watch, so `supervise off` first if a
  pass must be the only thing touching anything. This is what replaced `serveWakingWith`, a
  "these nodes want attention" hook nothing ever drove: it existed because a node had no state
  of its own to block on.
  **The input is one inbox filled by a list of `Producer`s** (`serveProducers`), each on a
  thread of its own pushing `Line`s tagged with the `Origin` that typed them; `serveWith` is
  the one-producer case, `stdinProducer` over a `Handle`, and is unchanged for every caller.
  "Idle" is still `isEmptyTChan` on that one inbox, whoever fills it, and every command still
  runs `stopTending` first. The one decision the list adds: **only the `Stdin` origin's `Eof`
  ends the loop**; another producer's `Eof` is not a command (nothing acts, so nothing stands
  down) and is read past — a socket client hanging up or a fetcher going quiet must not take
  the server with it, so a loop with no `Stdin` producer ends only on `quit`. Such a hang-up is
  reported as `HungUp origin` at the moment the loop reads its `Eof` — which, the inbox being
  one queue, is after every line that origin typed has been handled, and is what whoever holds
  a connection for it waits for before closing.   **Whose report is it** is the loop's knowledge and nobody else's, so `serveAttributed` is the
  entry point that says: it takes reporters over `Attributed a` (a `Maybe Origin` beside the
  report) and stamps every report — the loop's own and the per-node ones a pass emits — with
  the origin of the line being handled, `Nothing` outside a command (the tending machines'
  reports). The mechanism is one private `IORef (Maybe Origin)` written in `loop` after
  `stopTending` and cleared when `step` returns, applied through `Reporter.pulls`; it is
  private on purpose, so the API is two contravariant reporters and no mutable state.
  `serveProducers`/`serveWith` are the same loop with the stamp thrown away.
  **`Actions/Serve/Socket.hs`** is the first producer beyond stdin (milestone 2 of
  `specs/generic-server.md`): `withUnixListener` binds a unix socket (bind, then chmod 0600,
  then listen — race-free without a process-global umask, because a bound-but-unlistened
  socket refuses connections; a stale file is replaced only if a connect to it fails, a live one
  is `AlreadyListening`, a non-socket is `NotASocket`, a path too long for `sun_path` is
  `PathTooLong` before anything is touched, and `run serve` refuses such a `--listen`/`--http`
  path up front), `listenerProducer` accepts connections
  and reads each as lines under its own `Origin "PATH#n"`, and `listenerReporters` wraps the
  loop's one `Reporter Tagged` so that a report stamped with a connection's origin is also
  written to that connection as `reportJSONLines` — clients always get JSON, the loop's stdout
  stays whatever `--json` said. Two ordering facts are load-bearing: a connection is closed on
  the loop's `HungUp` for it and **not** when its reader hits EOF (a client that half-closes
  after typing is still owed its reports, which the loop may not have reached yet), and the
  producer's own teardown closes every connection so a client attached when `quit` ends the
  loop reads EOF. Under `--listen`, `CommandLine` hands stdin over as `Origin "stdin"` rather
  than `Stdin`: with a socket to talk to the process must outlive whatever started it
  (`< /dev/null &`), so stdin's EOF is a hang-up like any client's and only `quit` — from
  anywhere — ends the loop. `Test/ServeSocketSpec.hs` drives it with real connections.
  **`Actions/Serve/Http.hs`** is milestone 3: `run serve --http PATH` serves HTTP (warp) on a
  *second* unix socket, bound through the same `withUnixListener` (owner-only, live path
  refused) — its own path rather than protocol detection on `--listen`'s, because the line
  protocol reads through a `Handle` that cannot hand peeked bytes back, and sharing would have
  meant rewriting both over raw sockets plus a warp `Internal` shim for the price of one flag.
  **Reads bypass the inbox**: `GET /dag`, `/status`, `/history`, `/help/seed` read the loop's
  `World` cell through the accessor `serveObserved` hands its observer (`serveAttributed` is
  that with a no-op observer; the accessor is `readIORef`, nothing more), plus the (R3) tending
  snapshot already on each `NodeState` — so a read never runs `stopTending`, never wakes a
  machine, and answers while a node's `up` is still running in the loop; it is at most one
  command old. `/dag` is `worldDag` (magma plus the ledger's precedence, the structure a pass
  walks, *unrewritten*), one object per `Ref` in `dagOrder` with `dependencies`/`dependants`
  both ways and the `Act` projection — exactly the fields `Dag.sameRepresentative` compares
  (`shorthand`/`help`/`notes`/rendered `dynamics`, via `Dag.representative`) plus the loop's
  `direction`/`convergence`/`status` through `Tagged.nodeStatePairs`, one encoding not two. A
  node whose representative won a still-standing collision carries `conflict: {kept, replaced}`
  (`Tagged.representativeValue` of each side) — `Serve.Collision`, filled by `Serve.record` from
  both the fold's own `dagConflicts` and a differing representative that another *live*
  declaration still wants (which the loop now also reports `Conflicting` at declare time), kept
  while a holder of the losing side is live, dropped by `prune` otherwise. It
  exists from the first declaration on (every node `pending` under `autoconverge off`), and a
  retiring node is in it with `direction: down` until `prune` drops it. `/status` and
  `/history` are the objects `--json` prints for the commands (`history` folds the elided count
  in as a field); `/help/seed` is the seed parser's own `--help` (`CommandLine.seedHelpText`)
  and the command reference. **`POST /command`** is one line of the input language as one more
  `Producer`: the `Line` and its `Eof` are written in one STM transaction under an origin minted
  per request (`PATH#n`), so every arrival still runs `stopTending` first and nothing another
  producer types lands between the two. Sync (default) collects every report the loop stamps
  with that origin and answers, as a JSON array, on the loop's `HungUp` for it — Socket's
  closing rule, for the same reason; `?async` answers `202 {"seq": n}` at once. Report text is
  public, no redaction (spec decision). No token on the unix socket. `Test/ServeHttpSpec.hs`
  drives it with `http-client` over the socket and rebuilds `Help.dagLines` from `/dag`.
  **`GET /` and `/ui/*` are the web UI** (milestone 7, first two steps): `salmon-ops/ui/`'s
  `index.html`/`ui.js`/`ui.css`, embedded at build time with `file-embed` (`embedDir` under
  `makeRelativeToProject`, so the set is closed at compile time and a path outside it is the
  ordinary 404; listed in `extra-source-files` so an edit is a rebuild). Plain ES module, no
  bundler, no framework, no vendored library: `ui.js` draws `/dag` as a layered graph — its own
  longest-path layering plus four barycentre sweeps, dependencies above dependants, one box per
  `Ref` coloured by `convergence` and dashed for `direction: down` — then subscribes to
  `/events?since=<the snapshot's seq>` and applies `updown`/`upkeep` events to the boxes
  (`acted` and `tended` unwrapped to the inner report) and `converge-start`/`converge-stop` to
  the header. It holds no state the server does not: `declared`, `cleared`, `converge-stop`, a
  `gap`, or the stream dropping each mean fetch `/dag` again and resubscribe from its `seq` (it
  closes the `EventSource` on error rather than let the browser reconnect, since the browser
  resumes by `Last-Event-ID`, which the server does not read). A click opens a side panel from
  the node object alone. **Every write is `POST /command?async`**, never the sync form (a sync
  `up` holds the request for the whole pass): the panel's `force`/`recheck`/`pause`/`resume`
  (`--select #<short ref>`), the header's `converge`/`supervise`/`autoconverge`/`fetch`/`clear`,
  the seed form (`/help/seed`'s text, a field for the words, `up`/`only`/`down`, `/history`
  under it with a `down` per active row) and a raw command line. The outcome is read off
  `/events` by the request's origin — the events carrying it outline the nodes touched and fill
  the log under the command line until the loop's `hung-up` for that origin. No `quit` on the
  page, and the page never handles a token. A browser cannot open a unix socket, so it is
  reached on milestone 8's TCP listener (below), signing in at `/auth`, or through a forward
  of the unix socket (`ssh -L`); see `resources/serve-supervision.md` §14.
  **Milestone 8, the same HTTP over a network**: `run serve --http-tcp HOST:PORT --tls-cert
  FILE --tls-key FILE --token-file FILE` adds a warp-tls listener (`Http.withHttpServerOn`
  over a list of `Bind`s — `BindUnix PATH | BindTls TlsBind`, no plaintext constructor) running
  the *same* `application` on the *same* `Server` (one ring, one counter, one inbox), behind
  `requireToken`, a middleware on the TCP listener only that wants `Authorization: Bearer
  <token>` on every route, `/events` included, compared in constant time (`sameSecret`) and
  answering `401 {"error": ...}` otherwise. It is a middleware so a route added to
  `application` later is covered without knowing the token exists. A browser cannot send that
  header (not on a navigation, not from `EventSource`), so the same middleware owns `/auth`:
  `GET /` without a credential is a `303` there, a form posting the token, answered with a
  `__Host-salmon-session` cookie (`HttpOnly; Secure; SameSite=Strict`) that it then accepts
  wherever it accepts the header. The cookie is 32 random bytes per sign-in, kept as its
  SHA-256 in the listener's `Sessions` — never the token, and gone on restart. `POST
  /auth/logout` (the page's *sign out*, a plain form) revokes it and expires the cookie, and an
  `/events` stream opened with that session is cut there and then (`untilEnded` races the
  response body against the session leaving the set) rather than living on in another tab;
  `GET /auth/session` is how the page knows to show the button. A session also ends on its own
  (`SessionPolicy`, `--session-lifetime` default 12h from sign-in, `--session-idle` default 1h
  unused, `0` for none): the lifetime ends it however busy, cutting an open stream on time, while
  an open `/events` stream counts as use so a watched page does not idle out. Ended sessions are
  swept at every sign-in and dropped when next presented, which is what bounds the store to one
  lifetime's worth of sign-ins; the cookie carries the lifetime as `Max-Age`, and a stale cookie on
  `/` is sent to `/auth?ended`. The clock is a `SessionClock` so tests move it. `CommandLine.
  validateTcpOptions` is the pure refusal: `--http-tcp` without all three files exits 1 naming
  the missing ones, the files without `--http-tcp` are refused, `:PORT` with no host is
  refused (spell `0.0.0.0`), `[::1]:PORT` for IPv6; `Http.readTokenFile` refuses a
  world-readable or empty file. Exactly one line goes to stderr on startup (`serve: exposing
  HTTP on HOST:PORT with TLS, token from FILE`), and the listener's `onException` drops
  warp-tls's `InsecureConnectionDenied` (a plain-HTTP client, answered 426 first) and TLS-level
  connection errors (curl closes without close-notify on every request) so it stays the only
  line. Origins over TCP are the client's `ADDR:PORT#n` (`originFor`), the socket's `PATH#n`
  as before. The certificate and key are loaded before binding, so a bad file throws
  `BadCredentials` here rather than dying on warp's thread. `Test/ServeTlsSpec.hs` mints a
  certificate with `Certificates.certificateAuthority` (v3; `selfSign`/`caSign` write X.509 v1,
  which crypton's validation rejects as `LeafNotV3`) and drives it with `http-client-tls`
  pinning exactly that certificate. Not done: client certificates, a read-only token.
  **`Actions/Serve/Events.hs`** is milestone 4, `GET /events`: one numbered, replayable record
  of every report, as server-sent events (`id: N` / `data: {…}`, the `Tagged` object with `seq`
  added and `origin` — the object `history` entries use — when the report was stamped for a
  command). The `Events` value is the counter, a bounded ring (`Data.Sequence`, `--events-ring
  N`, default 2048) and a broadcast `TChan`, and `publish` writes all three in **one STM
  transaction**: that transaction is the critical section the spec's open question asks for.
  It could not be the concurrent driver's reporter `MVar`, because there is no single one —
  `Concurrent.walkConcurrent` makes a `reportLock` per walk and `Upkeep.startUpkeep` one per
  supervisor, each local to its function — but each of those is *held* while `runReporter` is
  called, so a numbering reporter whose whole effect is one transaction composes under all of
  them: within a driver, report order and sequence order agree; across drivers and machine
  threads, atomicity alone gives one total order. `Http.serverReporters` feeds it with
  `Events.eventsReporter` beside the loop's own reporter (stdout and `--listen` clients are
  unchanged), and unwraps `Serve.Tended` to the `upkeep` stream it came from. **The event
  stream is the only place a client sees the tending machines at work** — a sync `POST` answers
  with the reports stamped for its command, and tending happens exactly when no command is
  being handled. Numbering is dense: every `POST /command` publishes an `enqueued` event
  (`stream: "server"`, with the `line`) numbered from the same counter, and that number is the
  `?async` answer, so "the reports of my command" is "events above `n` with my origin". `/dag`
  and `/status` carry `seq`, the last number handed out, read *before* the world so that an
  event landing between the two reads is replayed rather than skipped. `?since=N` replays what
  the ring still holds above `N` then continues live; if `N+1` has fallen off, the first event
  is a synthetic `{"kind":"gap","from":<oldest>,"stream":"server"}` with no `id`, never a
  silent skip. `?stream=serve,updown,upkeep,server` and `?origin=NAME` filter server-side
  (the spec's "clients filter" is right about who decides, wrong about who pays). A comment
  line every `configKeepAlive` (15s) of silence keeps proxies and read timeouts from dropping
  an idle stream; a client hanging up is a failed write, which ends the stream and its
  subscription; the loop ending sets `serverStopped`, on which every open stream returns so
  warp's graceful shutdown is not held behind a subscriber. `Test/ServeEventsSpec.hs`: a
  seeded (`SALMON_EVENTS_SEED`) mid-pass disconnect-and-`?since=` equals an uninterrupted
  subscription; strictly increasing numbers across the three streams with `supervise on` and
  a node whose `check` always fails; ring overflow; `?async` then `?since=`; snapshot `seq`;
  filters; keep-alive and cleanup. One hazard it documents: the suite runs groups in parallel
  in one process and nothing in the tree passes `close_fds`, so a child spawned by another
  test inherits any fd not marked close-on-exec — `network`'s `socket` sets `SOCK_NONBLOCK`
  but not `SOCK_CLOEXEC` (its `accept` does), and `process`'s `createPipe` is plain — which
  showed up as a loop whose stdin never hit EOF and a hung-up client whose socket stayed open;
  the spec marks its own fds.
  **`Actions/Serve/StatusSink.hs`** is milestone 5 of `specs/pull-mode.md`: `run serve
  --status-sink PATH [--status-sink-interval S] [--status-sink-host NAME]` writes a JSON document
  about this host — `salmon-status: 1`, `host` (`--status-sink-host`, else `uname -n`), `written`, `mode`, `labels` (the document applied per
  followed label: id, sha256, when), `status` (the very object `status --json` prints) and `last`
  (the last `converge-stop` and the last follow-stream object, tagged, as `--json` prints them) —
  to a temp file renamed over `PATH`, so a reader never sees half of one. It is **a reporter and
  a timer, not a producer**: `sinkReporter` is composed beside the loop's `Reporter Tagged` with
  `reportBoth` and wakes the writer on `ConvergeStop` and `Follow.Injected`; `sinkObserver` is
  handed to `serveObserved` (sequenced after the HTTP server's) and reads the world through the
  same accessor `/status` does; a tick every interval rewrites it regardless. Nothing about it
  touches the inbox, so no write ever stands a machine down, and a host gone quiet is one whose
  `written` is old, not one whose file says all is well. A write that fails is `Serve.SinkFailed`
  — once per run of failures, re-armed by the next success, emitted from the sink's thread and
  attributed to nobody — and the loop keeps serving. The fetcher's `Applied` gained `appliedAt`
  and its cell is made by the caller (`Follow.newApplied`, beside `newMode`) so `Followed` can
  carry `followedApplied :: IO [AppliedDocument]`; the spec's "the sink is an op in the host's
  graph" was not done, because a node runs *in* a pass and the sink must write *after* it. The
  fetcher's reports are the fourth `Tagged` stream (`FromFollow`, `stream: "follow"`) as of the
  same change — before it they printed as text whatever `--json` said, and no sink could see
  them. **`Actions/Fleet.hs`** is the reader: `readStatusDir` (every `*.json` that parses as a
  sink document, the rest listed with why) and a pure `fold` — one `Row` per document in host
  order, `converged`/`errored`/total read off `status.nodes[].convergence`, the label filter,
  `rowStale` past `optStale` (a visible fact, not a decision: nothing decides a host is dead) —
  which `salmon-fleet status DIR [--label L] [--stale S] [--json]` in `salmon-apps` is a thin
  command line over. Two documents naming one host are two rows; the fold reports, it does not
  pick. See `Test/StatusSinkSpec.hs`.

  `Test/ServeModelSpec.hs`'s "input producers" group drives the loop from two lockstep
  in-memory producers and checks the world matches the one-script run.
  **`Actions/Follow.hs` is the second producer**, pull mode (`specs/pull-mode.md`, milestones
  2–3): `run serve --follow DIR --label L [--label L]... [--follow-base S ...]` fetches a JSON
  `Document` per label from a `Registry` (`directoryRegistry`: one `<dir>/<label>.json` per
  label) and injects the *diff* against that label's previously applied document — `up` for
  seeds newly present, `down` for seeds gone from it and from every other followed label's
  document, since the ledger keys a declaration by its directive and cannot tell one label's
  copy from another's. Three things there are load-bearing. **Change is detected before
  injection** (the registry's stamp — mtime and size — decides whether to read, the sha256
  digest whether anything changed, and the seed-set diff whether anything is declared): an
  unchanged round is invisible to the loop, because every inbox entry stands the machines down
  and a poller that injected on every tick would starve supervision (`Test/FollowSpec.hs`'s
  "rewriting the same bytes injects nothing" is that rule as a test). **A diff is one `Batch`
  inbox entry**, a `Line` constructor the loop runs with `autoconverge` held off, restores to
  whatever the operator had set, and follows with one `converge` — one entry so nothing
  another producer types lands in the middle of it, and restored by the loop because only the
  loop knows the setting. **The fetcher is a named actor in `history`**: `Origin` grew
  `Loaded path` and `Fetched Provenance` (registry, label, document id, digest), every
  `Epoch`/`LogEntry` carries the origin of the line that made it, and `history` renders it as
  a trailing `[fetched ... label=... id=... sha256=...]` — a typed line renders as before. The
  first round runs synchronously and stdin is held behind it (`Follow.gated`), so the first
  convergence is what the registry says, and what it found is injected at once.
  **When every later round runs, and when what it found reaches the loop, is
  `Actions/Follow/Scheduler.hs`** (milestone 3): a pure step over a small state (consecutive
  failures, the next round's deadline, a pending change and when it was first and last seen)
  and one `IO` loop around it that takes a `Clock` from the caller — the system's, or a test's
  that moves time (`Test/FollowSchedulerSpec.hs`). Toward the registry, a ladder: a successful
  round (changed or not) polls at `base`, a failed one — the registry threw, or its bytes do not
  parse; a label with no document is *not* a failure — at `min(cap, base·factor^(n-1))`, every
  delay jittered so a fleet does not poll in step. Toward the loop, a quiet window: a changed
  document is set aside as that label's latest `Seen`, and injected once the registry has been
  quiet for `debounce` or `max_wait` after the first pending change, diffed against the document
  last *applied* — so three writes inside one window are one diff and one pass, and a
  half-published state is never applied. A window can close over several labels at once, which
  is why `Batch` carries `[(Origin, ServeCommand)]`: one inbox entry, each declaration still
  naming its own document in `history`. `fetch` is the one place inbound events touch the
  scheduler — a round now, the ladder forgotten, whatever is pending injected the moment the
  round is over — and since the loop cannot call into a producer, `serveFollowing` takes the
  hook it pulls (`serveProducers` is that with none; without `--follow`, `fetch` says nothing is
  being followed) and `Scheduler.Poke` is the flag the fetcher's clock wakes on. A round and an
  injection due at the same instant go round first, deliberately: a poke makes both due now, and
  its point is to inject what that round finds. The flags are `--follow-base` (30s;
  `--follow-interval` is its older name), `--follow-factor` (2), `--follow-cap` (10m),
  `--follow-jitter` (0.2), `--follow-debounce` (5s; 0 injects at the round that saw the change)
  and `--follow-max-wait` (60s).
  **The last applied document survives a restart** (milestone 4): with `--follow-cache DIR`,
  `injectPending` writes each label's just-applied document — bytes, digest, id — to
  `DIR/<label>.applied.json` (a temp file and a rename, so a crash mid-write leaves the previous
  entry), and the startup round replays it for any label whose fetch *failed* (the registry
  threw, or its bytes did not parse — a label the registry answers `Absent` for is not replayed,
  the registry answered). A replayed document is that label's `Seen` with no stamp, so it goes
  through the same diff and batch as a fetched one, and its digest is compared exactly as an
  applied one's is: a registry coming back with the same bytes injects nothing, which is the
  starvation rule across restarts. `Serve.Mode` is what `status` says about it — `interactive`
  (no `--follow`), `following` (the world is what the registry last said), `replay` (at least one
  label came from the cache) — and it is only ever *entered* at startup, because the cache stands
  in for a world, not for a round: before the first round there is nothing else, and after a
  successful one the world already is the registry's last word, a later failure changes nothing
  about it, and the scheduler's `Backoff` is what says the registry is gone. `Replay` turns to
  `Following` at the first later round in which every label answers. The loop reads the mode
  through `Serve.Followed` — the fetch hook and an `IO Mode`, one record in the slot the hook
  had, `Nothing` meaning interactive — and `StatusReport` carries it as its first field, so the
  text render's first line is `serve: mode: ...` and the JSON object has `mode`. `Document` also
  gained an optional `published` (RFC 3339; a *malformed* one is a parse error, not ignored), and
  `--follow-refuse-older` refuses a fetched document published before the one already applied
  *or pending* for its label (`Stale`, not injected) — off by default, and without `published`
  on both sides the latest is whatever the registry says. A cache entry that cannot be read
  (`BadCache`) or written (`CacheFailed`) is reported and otherwise ignored; the cache never
  takes the loop down. `directoryRegistry` now *throws* when its directory is missing rather than
  answering `Absent`, since that is the difference between "the registry is unreachable" and "no
  document for this label", and the cache hinges on it. See `Test/FollowCacheSpec.hs`.
  **The other registries are `Actions/Follow/Registry.hs`** (milestone 6), one `Registry` value
  per backend, each owning the template that turns a label into an address, chosen by the shape
  of `--follow` (`parseAddress`) and opened by `open`; `follower` and the scheduler see none of
  it. `Registry/Git.hs` (`git+URL[#BRANCH[:SUBDIR]]`): cloned once into `--follow-workdir` (default
  `checkout` under the cache directory, else a temp directory named by the repository), then
  `git fetch` and `git reset --hard` onto the branch every round, all through `Binary` with
  `GIT_TERMINAL_PROMPT=0`; the document is `SUBDIR/<label>.json` in the checkout and the *stamp is
  the commit*, so a round that finds the same commit reads nothing. The subdirectory comes after
  the branch, not after the URL, because URLs have colons of their own. `Registry/Http.hs`
  (`http(s)://...`, `{label}` placed or `/<label>.json` appended): the stamp is the `ETag`, else
  `Last-Modified`, sent back as `If-None-Match`/`If-Modified-Since` so an unchanged document is a
  `304` and no body crosses; `404` is `Absent`, anything else throws (a failed round). One manager
  per registry, `--follow-timeout` on the whole response. `Registry/Dns.hs` (`dns:ZONE`): one
  `TXT` at `<label>.ZONE` reading `v=salmon1 url=<https url> sha256=<hex>`; **the record's digest
  is the stamp**, so a round is one lookup and the URL is fetched only when the announced digest
  moved, and a body that does not hash to what the record announces throws `IndexMismatch` —
  a failed round with that reason, never applied. The resolver is a `Resolver` record so tests
  stub it; the shipped one shells out to `dig +short` (nothing in the tree resolved DNS before,
  and one TXT lookup did not buy a resolver library). The bucket backends (`s3://`, `gs://`) are
  the HTTP one under a URL template (virtual-hosted S3, GCS's `storage.googleapis.com`, or
  path-style under `--follow-bucket-endpoint`): public or presigned objects only, no SDK, no
  credentials. **`followVerify`** is the verify-before-inject hook: `Digest -> ByteString -> IO
  (Either Text ByteString)`, run on the raw bytes after the digest comparison and before the
  parser, on every backend *and on a cache replay* (a cache file is as writable as a registry
  file); a `Left` is `Rejected label digest reason`, a failed round, never injected and never
  cached; a `Right` is *the bytes the loop parses*. `noVerifier` hands back what it got and is
  the default — **unsigned mode is the default** — and `Binary.untrackedExecOutput` is
  `untrackedExec` handing stdout back, for `git rev-parse` and `dig`. See
  `Test/FollowRegistrySpec.hs`: a bare repo, a `warp` server with `ETag`s, a stubbed resolver.
  **`Actions/Follow/Signature.hs` is the verifier that fills the hook** (E2, the spec's "Signed
  documents"): `run serve --follow ... --follow-key FILE` (repeatable, any one matching signature
  accepts) sets `followVerify = signedVerifier keys`, and a document must then arrive as a
  *signed envelope*, `{"salmon-signed": 1, "document": <the document as fetched>, "signatures":
  [{"key": <id>, "alg": "EdDSA", "sig": <base64>}]}`, signed over the canonical bytes of the
  `document` member. Canonical means `Data.Aeson.encode` of the parsed `Value` — sorted keys
  (aeson 2's `KeyMap` is a `Map` under its default flag; the plan pins aeson 2.2.5.1) and one
  spelling per scalar — so a registry, proxy or pretty-printer re-serialising the envelope leaves
  the signature valid, and both signer and verifier parse-then-encode with the same function
  rather than depending on a canonical-JSON library. The verifier hands the loop the *inner*
  document, so what `Document`'s parser sees is exactly what was signed; the digest kept
  everywhere (change detection, `Rejected`, `history`, the cache) is that of the bytes *as
  fetched* — the envelope — because the cache keeps those bytes and a replay goes through the
  verifier as a fetch did (`readCacheEntry` is the read without the parse). Keys are **JWK
  files** — the format `Nodes/Keys.hs` already writes with `jose` — and the algorithm is EdDSA
  on Ed25519 (`jose` on `crypton`, already dependencies; RSA/EC keys sign with `bestJWSAlg`);
  `none` and the HMACs are refused outright since a public key verifies neither; the key id is
  the RFC 7638 SHA-256 thumbprint. Refusals name their cause: unsigned under a key, an envelope
  that does not parse, no signatures, or every signature failing (which key, and why). A
  `--follow-key` that does not load exits 1 with the path before any loop starts. `salmon-fleet
  keygen --out FILE` (FILE 0600 and FILE.pub) and `salmon-fleet sign --key FILE < doc > signed`
  are the controller's half. Out of scope: rotation/revocation beyond several `--follow-key`s,
  signing inside a registry. See `Test/FollowSignatureSpec.hs`.
  `ServeCommand.DeclareInline` exists for a document's `{"directive": {...}}` entries and is
  never spelled by a line of the input language. And a `Configure` that throws is now a
  `BadSeed` report rather than the end of the loop, for typed and fetched lines alike —
  a document's author is not at the keyboard, and their typo must not cost a host its
  supervisor.
  (R2): `force`/`recheck`/`pause`/`resume [--select P]... [--exclude P]...` finally make
  `Op/Mailbox.hs`'s `Instruction`s reachable from the input language, reusing
  `parseSelection`/`resolveWorldSelectors` the same way `status`/`query`/`converge --select`
  already do. The one thing that isn't free: `stopTending` runs before *every* command, `status`
  included, so there is never a live mailbox to post into at the moment one of these is typed —
  a one-shot machine doesn't survive the command that named it the way a `Kept` one does. So the
  instruction is queued on `Tending` (`tendingPending`, a `Map Ref [Instruction]`) instead of
  posted, and `startTending` delivers the whole queue the moment the *next* supervisor's
  machines exist — both freshly started and adopted — then clears it: "force this node next
  time you look at it", the smaller of the two options and, per the node's own machine already
  treating a queued `Pause` followed by a `Resume` correctly in delivery order, the right one.
  A selected node that no live machine ever answers to (excluded, retired, never tended) drops
  the instruction silently, same as `Upkeep.instruct` always has; the operator already saw how
  many nodes matched at declare time (`Instructed`), which is a count of the selection, not a
  delivery receipt.
  A node with a `managed` action is handled differently again, and the difference is all about
  ordering. It is **invisible to both passes** (`gateFor` skips it, and `stateWriter` ignores
  that skip so the pass cannot claim it converged): there is nothing a one-shot `up` could do
  with it and nothing left for a one-shot `down` to do. Its machine is `Kept` across commands
  rather than stood down with the others. And `settleManaged` runs **before** a convergence
  pass, letting go of the machines for anything this world no longer wants up — that ordering is
  the point, because the down pass is what removes a daemon's config file and working directory,
  and it must not do so while the daemon is still running. Recording such a node down there is
  exact rather than optimistic: for an effect that only exists while something holds it,
  "nothing holds it" is what being down *is*. Leaving the loop leaves these processes running,
  which is what `quit`'s "changing nothing on the way out" has always promised. Neither pass
  touches a graph: `worldDag` rebuilds one walkable structure from the magma and the ledger's
  precedence via `Dag.fromMagma`, and both directions run over it. `epochGraph` survives for one
  reason only — `--select` resolves *path* patterns, and a `Dag` has `Ref`s and edges but no
  paths. Registered `Rewrite`s run once per convergence pass (not per declaration), because what
  they partition on is a property of the whole ledger at that moment.
- **`Client/Http.hs`** and **`Client/Model.hs`** are milestone 6 of `specs/generic-server.md`, the
  client's half, with no terminal in them. `Salmon.Client.Http` is a small typed client over
  `http-client` for the unix socket (`newUnixClient`) or an `--http-tcp` listener (`newTlsClient`:
  `https` only, a bearer token on every request, the server's certificate verified against
  `--cacert` alone when given and the system store otherwise, no switch to skip it): `dag`/`status`/`history`/
  `seedHelp` for the reads — which bypass the loop and never stand a machine down — `command`
  (sync, the reports) and `commandAsync` (the enqueue seq and origin), and `events`, which opens
  `/events` once with `?since=`/`?stream=`/`?origin=` and hands each event to a callback until it
  says stop or the stream ends; reconnecting is the caller's, with the last seq it saw.
  `Salmon.Client.Model` is the spec's `dag ⊕ events since the dag's seq`, pure: `fromDag` reads a
  `/dag` answer into one `Node` per ref in `dagOrder` (ref, shorthand, direction, convergence,
  last check, output ring, edges, paths), `step` folds one wire event — as *data*, never decoded
  back into the four report sums, so a kind the client was not written for still shows as a
  node's last event — and `rebase old fresh` joins a re-read snapshot to a model that has been
  folding. Two things are load-bearing. **Replays are dropped per stamp, and there are two
  stamps**: each node carries `nodeSeq` (the snapshot's, then each event's about it) and the
  loop-level fields (`modelPass`, `modelSupervised`, the resync request) carry `modelLoopSeq`,
  because a `/dag` snapshot says everything about the nodes and nothing about the loop — a client
  that re-reads after `declared` gets a seq past the whole pass, and one stamp would swallow the
  `converge-stop` of a pass whose start it had already shown. And **the model asks to be re-read
  rather than guessing** (`modelResync`, set by `declared`, `cleared` and `gap`): an event names
  nodes by ref and cannot describe a node the model has never seen, so the client holds no state
  the server does not, and a restart is one `/dag` read. `renderNodeRow`/`renderHeader` are the
  text a terminal shows, kept here so `Test/ClientModelSpec.hs` can assert on it: a recorded
  pass folded onto a snapshot, replay and rebase, the SSE parser against what `Events` renders,
  and, at Layer 1, the client itself against a real `withHttpServer` (`dag`, `commandAsync`,
  `events` from that seq, the model converges).
- **`Builtin/CommandLine.hs`** wires all of the above into the CLI every salmon binary shares:
  `execCommandOrSeed` implements the two-phase protocol described below.
- **`Op/Configure.hs`**: `Configure m seed a = Configure { gen :: seed -> m a }` — deliberately
  kept possibly-pure (non-IO) so the "turn a human-facing seed into a directive" step can be
  hermetically separated from the IO-heavy "turn a directive into ops and run them" step.

`Postgres.hs`'s `psql`-based admin commands (`database`, `user`, `group`, `grant`, `adminScript`,
`replicationUser`, `alterSystemSet`, `replicationSlot`, etc.) all take an explicit `Port` and pass
it as `-p` to every `sudo -u postgres psql ...` invocation — this is what makes it possible to
target a specific named, non-`"main"` cluster (see `createCluster`/`ClusterName` above) rather
than always silently hitting whichever cluster happens to be on the default port. Existing callers
that only ever manage `"main"` pass `Postgres.localServer.serverPort` (5432); a caller managing
multiple clusters on one box passes each cluster's own port.

Template databases: `Postgres.cloneDatabase` (a `CREATE DATABASE … TEMPLATE` node, whose
`Retention` says whether `down` drops it — `retainedClone` for an open PR's environment,
`disposableClone` for a test fixture or a merged PR) and
`SreBox.PostgresTemplate.template` (build, then lock with `IS_TEMPLATE`/`ALLOW_CONNECTIONS false`),
surfaced as `salmon-migrator config template …`. Two things about them are load-bearing. **The
template's build is an opaque nested walk inside its `up`**, not an ordinary dependency: a walk
applies dependencies before asking a dependant's check, so migrations as dependencies would run
against the locked template on every pass after the first. The check compares a fingerprint of the
inputs stamped in the database comment, and anything but a match is rebuilt *from nothing*, never
migrated in place. And **both can drop databases**, so each is marked in its comment
(`salmon-template:`/`salmon-clone:`) and every drop or adopt refuses an unmarked database: the name
is the caller's to choose and nothing else says who made it. `database`, `cloneDatabase` and
`template` share the `Ref` key `"pg-db" (port, name)`, because they are the same effect site.

Streaming replication (`primaryReplicationSetup`/`standbyReplicationSetup`, and the switchover
recipe sketched in `specs/pg-switchover.md` on top of them). **`standbyReplicationSetup` is the
one builtin in the tree whose `up` runs `rm -rf` on somebody's data**, so what guards it is the
point: the *system identifier*, read from the primary over a physical replication connection
(`IDENTIFY_SYSTEM`, which is `replication=true` — `replication=database` is the logical kind and
`pg_hba.conf` matches it against the database name, so the replication role's own line refuses
it) and locally from `pg_controldata`. Same identifier means the directory already belongs to
that cluster, promoted or not, and is left alone; a different one is cloned over only if that
cluster is pristine (nothing in `base/` at or above OID 16384) and otherwise refused. The guard
it replaced was `standby.signal`'s presence, which *promotion deletes* — so a promoted standby
read as "never cloned" and the next `run up` deleted the machine that had just become the
primary. `defaultReplicationTuning` turns on `wal_log_hints` (without it, or checksums,
`pg_rewind` can never rejoin an old primary, and it is restart-only so it has to be set before
there is data to lose) and caps `max_slot_wal_keep_size` (an uncapped slot lets a standby that
stays down fill the *primary's* disk). `startCluster`/`stopCluster` check `pg_lsclusters` rather
than running commands that exit non-zero when there is nothing to do, and
`restartClusterIfPending` — what `primaryReplicationSetup` uses — checks
`pg_settings.pending_restart`, the same "the file changed, the running thing is stale" shape as
systemd's `NeedDaemonReload`. `restartCluster` and `promoteCluster` stay unconditional: a
restart is not a state, and "not in recovery" is a fact about a *pair* of machines that one of
them cannot answer alone.

`SreBox.PostgresPair` is the pair above that cluster pair: two machines, one declared primary,
and `pairRole` — a node that *states where the primary is* rather than an action that moves it.
Its `check` asks both machines and every bouncer; its `up` takes steps until the declaration
holds, both over ssh from a **controlling** machine (never a member: the member that dies may be
the one running it). A pair is three kinds of node (`pairOp`): `member`, which makes a machine
able to be *either* half and says nothing about which — so a switchover edits one declaration,
the role node's — `bouncerSetup`, and the role node on top. `salmon-pgpair` is the binary,
`salmon-toy-qemu-pg-ha` the demo, `resources/postgres-pair.md` the guide, `specs/pg-switchover.md` the
argument. What follows is the list of things that are load-bearing; each was a defect first.

- **Nothing decides a machine is dead.** Salmon has no consensus, so a failover needs the
  operator to name, in `pair_may_discard`, the side whose un-replicated writes may go; without it
  the node refuses, and the same field is what allows one of two primaries to be rewound onto the
  other. A *crashed* peer is in that category for a reason worth knowing: `pg_controldata`'s
  checkpoint location is the end of the WAL only for a cluster that shut down cleanly (the last
  record is then a shutdown checkpoint), so for a crashed one "the standby has reached its
  checkpoint" reads as "the standby has everything" precisely when it is least likely to be true
  — hence `o_clean`, read from `Database cluster state`, with any value other than `shut
  down`/`shut down in recovery` counting as a crash.
- **A standby is asked two questions about its upstream**, where it *is* streaming from and where
  it is *told* to (`o_upstream` / `o_configured`), because a partition empties the first and
  leaves the second. Reading only the first, a standby that cannot reach its primary looks exactly
  like somebody else's standby — and the answer to that one is `Rejoin`, which stops it and then
  fails, so a broken link would take the standby down. Pointed here but not connected is
  `AwaitStreaming`: wait, then report `Unknown`.
- **The state is re-derived every turn** (`observe` → `nextStep` → act → observe), so an `up`
  killed mid-switchover is finished by the next one. There is no progress file that could disagree
  with the machines, and that property is what to protect when changing `nextStep`.
- **`Degraded` is `Unknown`**, the one verdict `Upkeep` acts on by continuing to look — "the
  primary is where it should be and the peer is unreachable" must not start anything.
- **The `ref` is keyed on the pair, never on the side**, so moving the primary changes that node
  rather than declaring a second one; the declared side rides in `notes`, where `serve` sees it as
  a change.
- **An old primary rejoins through `pg_rewind`** onto the new one's history, never a re-clone —
  which is what `wal_log_hints` above is for — and three things sit around that command. The node
  writes `primary_conninfo` itself, because `pg_rewind` may decide no rewind was needed and what
  it then does about `-R` is not worth betting a second primary on. It *deletes*
  `primary_slot_name`, because slots are not replicated and a standby naming a slot the new
  primary never heard of retries forever while looking healthy to every query but
  `pg_stat_wal_receiver`. And it completes a crashed target's recovery itself, in single-user mode
  with `-c config_file=` (pg_rewind's own attempt assumes Debian keeps postgresql.conf in the data
  directory, which it does not) and with `wal_keep_size` pinned to what `pg_wal` already holds —
  as `StopMember` pins it too, since any clean shutdown ends in a checkpoint and a checkpoint
  recycles the very WAL a rewind reads back to where the histories parted. The rejoin takes the
  pin off once it has been used.
- **The two system identifiers are compared before anything else is decided**, and a mismatch
  refuses whatever else is declared — `pair_may_discard` included, since it says whose *writes*
  may go and presumes one cluster, rather than licensing a pass to rewind a real cluster onto a
  stranger that happens to answer at the right address.
- **Two rules exist because a declaration can arrive at a bad moment.** The peer is stopped only
  once the declared primary is *streaming* from it (a clean stop hands the tail over through that
  connection, so stopping it without one strands whatever the standby lacks — an outage made out
  of a healthy pair by a declaration). And a declared primary that is behind a *stopped* peer
  starts that peer rather than waiting, since nothing arrives from a stopped machine however long
  anyone waits.
- **Each member streams with a slot the pair names** (`slotNameFor`, derived rather than declared
  so that a rejoining member computes the same name the member it rejoins would), which the rejoin
  creates on the peer over the replication connection and then verifies, and whose stale twin —
  the slot this member held while it was the primary — it drops, since a slot nobody consumes pins
  every segment behind it. The primary reports its slots' `wal_status`, because a slot that fell
  off `max_slot_wal_keep_size` is the one observation saying a standby can never catch up: that is
  `Degraded` naming the slot, not a `Rejoin`, since `pg_rewind` would succeed and change nothing,
  and re-seeding means wiping a machine — an operator's decision, like `pair_may_discard`.
- **Traffic moves through pgbouncer's admin console**: `PAUSE`, rewrite, `RELOAD`, `RESUME`, never
  a restart, since a restart drops the clients the bouncer is there to hold. That is why the
  routing lives in its own file pulled in by `%include` and deliberately *not* among
  `systemdServiceWatching`'s watched files: the ini has one writer (`bouncerSetup`, which renders
  it with `PgBouncer.renderIni` over ssh and restarts on change — `PgBouncer.setup` is the local
  equivalent, and watches the ini the same way) and the routing file has another (the role node, which does not), and one
  file with two writers is how a switchover becomes an outage.

`Test.PostgresPairSpec` is the whole table at Layer 0, refusals included;
`Test.PostgresSwitchoverSpec` moves a real primary between two VMs and back, stops a controller
after each step in turn to show a plain pass finishes what it left, kills a primary outright to
show the failover refused without the flag and rewound with it, and partitions the two machines —
once between themselves, where the right answer is to do nothing, and once hiding the old primary
from the controller as well, which is the only way to reach two primaries and the one case where
salmon knowingly discards acknowledged writes. Those partitions are `nft` rules built from
`Netfilter`'s own vocabulary and shipped over ssh, since the guests have no salmon on them; the
one that hides a machine from the controller is handed to the machine as cut-wait-heal, because
the command that would lift it would have to travel the path it cut. `Test.PostgresVms` holds what
those specs share, and the reason it exists is that **a rootfs is a host directory that outlives
its VM**: a spec that moved the primary leaves the next one starting from a standby, so each
normalizes on the way in (`ensurePrimary`, `resetCluster`) rather than assuming.

## Conventions for node authors

None of this is enforced by the type system — these are conventions every existing builtin
follows, and new ones should too.

**Idempotency.** `up` must be safe to run twice. Prefer, in rough order of how commonly they
apply:
- `replace` over `add` for anything that has it (e.g. `ip route replace`, used by
  `Salmon.Builtin.Nodes.Routes.route`; `ALTER SYSTEM SET`, which is a set rather than an insert).
- A `DO $$ IF NOT EXISTS (...) THEN ... END IF; END $$;` guard for a bare `CREATE` that has no
  `IF NOT EXISTS`/`CREATE OR REPLACE` form but *is* allowed to run inside one — e.g. `CREATE ROLE`
  (see `Postgres.CreateUser`/`CreateGroup`/`CreateReplicationUser`) or a physical replication slot
  (`Postgres.EnsurePhysicalReplicationSlot`).
- A shell-level check-then-act when even that isn't available — e.g. `CREATE DATABASE`, which
  Postgres refuses to run inside a transaction/DO block at all (`Postgres.CreateDB`: `psql -tAc
  "SELECT 1 FROM pg_database WHERE datname = '...'" | grep -q 1 || psql -c 'CREATE DATABASE
  ...'`).
- Append-if-missing for config file lines with no SQL/CLI equivalent at all (`grep -qxF ... ||
  echo ... >>`, see `Postgres.ensureHbaLineScript`, the `pg_hba.conf` case — there's no `ALTER
  SYSTEM` for that file).

`nft add rule` itself is *not* idempotent — reapplying the same graph would append a duplicate
rule every time instead of a no-op, since nft rule handles aren't content-addressed the way a
file path or a SQL role name is. `Netfilter.rule` instead uses `check` for this (rather than a
SQL/shell guard): `skipIfNftRuleExists` shells out to `nft list chain` and reports `Success` if
a line matching the rule's own rendered text is already there — the same "does the effect already
exist" shape as `Salmon.Actions.UpDown.skipIfFileExists`, just backed by a command's output
instead of the filesystem. Worth remembering as a template for any other node whose underlying
tool has no idempotent "set" verb at all (nothing to `replace`, no `IF NOT EXISTS`): check output,
skip via `check`, rather than trying to force the command itself to be idempotent.

**`check`, not `prelim`.** `Extension` used to carry both a `prelim :: IO Requirement`
(implemented by 22 nodes, consulted by `upTree`) and a never-implemented `check :: IO ()` with a
never-called `Actions/Check.hs` behind it, plus an equally dead `notify`. Those are merged: there
is one `check :: IO CheckResult`
(`Success`/`Skipped`/`Completed`/`Failure Text`/`Unknown`/`Immaterial`), `UpDown.requirement`
maps it to the old `Required`/`Skippable`, and `Actions/Check.hs` / `Actions/Notify.hs` are
gone. Four of the six describe the effect; the other two describe a decision somebody made
about the node. `Skipped` has exactly one producer — `Query.forceSkip` — and means "someone
decided to treat this as satisfied", as opposed to `Success` which is a statement about the
effect. `Immaterial` is the node *author's* equivalent: "there is nothing here worth asking
about", because applying the effect costs about what finding out would (`mkdir -p` against
`doesDirectoryExist`, `ip route replace` — the whole family whose idempotency comes from the
underlying tool having a "set" verb). It is the **default** for a node that sets no `check`,
which `Unknown` used to be, and it means `up` runs, matching the old `pure Required` default;
the one-shot drivers cannot tell the two apart, since running an idempotent `up` once is
precisely the cheap thing being claimed. They part company under `Actions/Upkeep.hs`, where
`Immaterial` parks a node instead of polling it — and the split leaves `Unknown` meaning only
what it says, a check that ran and could not tell, which it could not do while it doubled as
"nobody wrote a check". A `check` that *throws* is contained as a `Failure` rather than killing
the traversal, which `prelim` (evaluated outside `upTree`'s `try`) did not do. See
`specs/per-node-state-machines.md` milestone 1 and `Test/CheckSpec.hs`.

Two consequences worth naming now that `run serve` tends its nodes. **A node's `check` is the
only thing that can notice its effect going away**, and it is therefore also the only thing that
can fire a `RestForOne` — a config node with no `check` never notices its own file changing, so
nothing standing on it is ever bounced. A node with no `check` answers `Immaterial`, which the
upkeep FSM parks rather than polls (see `Actions/Upkeep.hs` above), so such a node is brought
up once and thereafter watched by nothing at all. That is not a loss of coverage relative to
before — a 60s poll that could only ever return "I cannot tell" was never coverage — but it
does make the shape of the gap explicit: **the only thing standing between a node and being
supervised is somebody writing its `check`**. Two builtins have had one written for them so
far, `Systemd.systemdService` and `Filesystem.filecontents`; most still don't, and
`Filesystem.dir` in particular means a directory removed behind salmon's back is not put back.
Adding one is per-node work and changes what `run up` does for existing callers (a node whose
check says `Success` stops being re-applied), so it is a deliberate decision rather than a
mechanical sweep. `Netfilter.rule`'s `skipIfNftRuleExists` is the template, and
`Op/Supervision.hs` is where a node states what should happen when its check says the effect
is gone.

`Systemd.systemdService` is the one that has been done, and it is the worked example of what
writing a real one costs — and of why the default is a claim rather than an absence: left
alone, a unit that can be stopped, crash, or be `systemctl disable`d behind salmon's back
would answer `Immaterial`, i.e. "nothing here worth asking about", which is simply false. `checkService` shells out once to `systemctl show` for three
properties and `interpretShow` (pure, so it is testable without a systemd — see
`Test/SystemdSpec.hs`) draws the verdict. Each property earns its place for a reason that only
shows up in the writing. **`ActiveState`** is the effect itself, but its *transitional* values
(`activating`/`deactivating`/`reloading`) map to `Unknown` rather than `Failure`: a service
part-way through starting has not gone away, and calling it gone is how a slow starter becomes a
restart loop — the first place in this tree where `Unknown` is the right answer rather than the
absence of one. **`UnitFileState`** catches a unit somebody `systemctl disable`d, which is still
running and so invisible to `ActiveState` right up until the next reboot. And
**`NeedDaemonReload`** is what makes a *changed* unit file take effect at all: this node's own
dependency rewrites the file before the check runs, so nothing on disk can still testify that
the running service is stale, and systemd's own record of "the file changed since I loaded it"
is the only thing that remembers. The behaviour change lands here too — a unit that is
installed, enabled, loaded and running is now **skipped** by `run up` rather than
reloaded-enabled-restarted every time.

`Filesystem.checkFileContents` is the second, and it is the one that made the first actually
work. It compares the bytes on disk with the bytes the node would write — not
`skipIfFileExists`, which would call a file holding the wrong thing satisfied, which is the
failure mode a config node most needs to catch. Reading them back costs nothing that isn't
already spent, since `up` is about to encode the same contents anyway; the file's *size* is
compared first so that a node holding a few hundred bytes doesn't read whatever enormous thing
has replaced its path. The reason text names the file and never quotes it, because failure text
goes into reports and these files include pgbouncer userlists and postgrest configs with
signing keys in them. The consequence worth knowing: **`systemdService` writes its unit file
through this node, and systemd decides `NeedDaemonReload` from that file's mtime.** Rewriting
byte-identical contents on every pass therefore reported a changed unit on every pass, which
made `checkService` say `Failure` on every pass and restart a healthy service — so the "a
healthy unit is now skipped" claim above was true of `checkService` alone and false of the
graph it sits in until this landed. One hazard, confined to the `EncodeFileContents (IO a)`
instance: the check runs the encoder, so a non-deterministic generator (a timestamp) makes the
node rewrite its file every pass. That is the safe direction, and nothing in the tree uses that
instance today. See `Test/FilesystemSpec.hs`.

`Systemd.systemdServiceWatching` is the same mechanism pointed at the *other* files a service
reads — a `pgbouncer.ini`, a `postgrest.conf`. `checkService` asks only after the unit file, so
a changed config file left the running process serving the old configuration forever: the unit
is active, enabled and loaded as written, so the node is skipped. Rather than a second check,
the watched files' contents are hashed into a comment at the end of the unit file, which makes
a config change a *unit* change, which is what `NeedDaemonReload` already notices. It is the
one caller of the `EncodeFileContents (IO a)` instance above, and so inherits its hazard
harmlessly (the encoder reads the watched files twice, and a file changing between those reads
is picked up next pass). `PgBouncer.setup` is the first user. Note what the reaction is: a
**restart**, which drops the connections a bouncer exists to hold — moving traffic gently is a
`PAUSE`/`RELOAD`/`RESUME` for the node orchestrating the move (`specs/pg-switchover.md`), not
something this node should try to do on its own.
(I6): `EncodeFileContents` also carries `contentFingerprint :: a -> Maybe Text`, a pure,
stable hash of the content an instance would write (`Nothing` by default — the `IO a` instance
keeps it, since its whole point is that content isn't known until the encoder runs).
`filecontents` puts the fingerprint into `notes` when its instance has one, which is what lets a
re-declaration that only changes a node's content register as a genuine
`Salmon.Op.Dag.Representative` change under `run serve` — see `Serve.record`'s `Stale` below —
rather than one indistinguishable from "nothing changed" at all. `Text`/`ByteString`/`String`/
`Aeson.Value` all have one; only the `IO a` hazard instance opts out.

**Failure must not be swallowed.** `Extension.up :: IO ()` has no way to signal failure in its
type — the only way a failure becomes visible to `upTree` (see above) is if `up` *throws*.
`Binary.untrackedExec`, which almost every builtin's `up` goes through via `withBinary`, does this
automatically: it checks the subprocess's exit code and throws `CommandFailed` on non-zero, so
most node authors don't need to think about this at all. The two places that do need explicit
handling:
- Anything built on the lower-level `withBinaryIO`/`CommandIO` (hands back a raw `ProcessHandle`
  instead of a checked result — used where stdin/stdout need redirecting, e.g.
  `WireGuard.privateKey`/`publicKey`) has to check `waitForProcess`'s `ExitCode` itself; use
  `Binary.checkExitCode label` (throws `CommandFailedSimple`).
- A node whose `up` recursively runs its own nested `upTree` (e.g.
  `PostgresMigrations.remoteMigrateOpaqueSetup`'s continuation) must check the returned `Bool` and
  `throwIO` if it's `False` — the outer traversal has no other way to learn the nested one failed.

## The seed → spec → ops CLI protocol

Every salmon binary (see `salmon-apps/src/Migrator.hs` as the worked example) exposes two
subcommands via `Salmon.Builtin.CommandLine.execCommandOrSeed`:

```sh
my-salmon config <seed-args...>          # seed (human/CLI-friendly) -> JSON-encoded directive on stdout
my-salmon run up|down|tree|dag           # reads a JSON directive on stdin, expands it into an Op graph, executes/prints it
my-salmon run serve                      # reads a stream of seed declarations on stdin, converges after each
my-salmon run serve --follow DIR --label L [--label L]... [--follow-base S] [--follow-debounce S] ...
                                         # the same loop, also fetching documents from DIR (pull mode)
my-salmon run serve --follow DIR --label L --follow-cache CACHE [--follow-refuse-older]
                                         # ... replaying CACHE's last applied document when DIR is unreachable at startup
my-salmon run serve --follow git+URL#BRANCH:SUBDIR | https://host/path | dns:ZONE | s3://B/P | gs://B/P --label L
                                         # the other registries (Salmon.Actions.Follow.Registry), chosen by the address's shape;
                                         # --follow-timeout S, --follow-workdir DIR, --follow-bucket-endpoint URL are theirs
my-salmon run serve --follow REG --label L --follow-key PUB.jwk [--follow-key PUB2.jwk]
                                         # ... requiring every document (cache replay included) to be a signed envelope one of
                                         # these keys signed (Salmon.Actions.Follow.Signature); without --follow-key, unsigned
my-salmon run serve --listen PATH        # the same, also accepting the line protocol on a unix socket at PATH
my-salmon run serve --http PATH          # the same, also serving HTTP on a unix socket at PATH:
                                         # GET /dag /status /history /help/seed, POST /command[?async],
                                         # GET /events[?since=N&stream=..&origin=..] (SSE; --events-ring N),
                                         # GET / (the web UI; forward the socket to a TCP port to open it)
my-salmon run serve --http-tcp HOST:PORT --tls-cert FILE --tls-key FILE --token-file FILE
                                         # the same HTTP over TCP with TLS, every request needing
                                         # `Authorization: Bearer <token>`; all three files or it refuses;
                                         # a browser signs in at /auth for a session lasting
                                         # --session-lifetime S (12h) / --session-idle S (1h, an open stream is use)
my-salmon run serve --status-sink PATH [--status-sink-interval S] [--status-sink-host NAME]
                                         # the same, also writing this host's status document to PATH
                                         # (atomically) after every pass and injection, and every S seconds;
                                         # `host` is NAME, else `uname -n`
salmon-fleet status DIR [--label L] [--stale S] [--json]
                                         # one line per host from a directory of such documents; reads only
salmon-tui PATH                          # a terminal over --http PATH: /dag once, /events live, `:` to type a command
salmon-tui https://HOST:PORT --token-file FILE [--cacert FILE]
                                         # the same over --http-tcp, pinning FILE's certificate when given
salmon-fleet keygen --out FILE           # an Ed25519 signing pair: FILE (JWK, 0600) and FILE.pub (for --follow-key)
salmon-fleet sign --key FILE < doc.json  # the document wrapped in a signed envelope, on stdout (or --out FILE)
```

Typical usage pipes them together: `my-salmon config 123 | my-salmon run up`. This split exists so
that config generation (which may be impure/human-parametrized) and execution (which must be
IO/hermetic and is meant to run unattended, e.g. on a remote box) are distinct, independently
inspectable steps — the JSON directive is the contract between them. `run tree` prints a
human-readable dependency tree (`Actions.Help`); `run dag` prints Graphviz dot output
(`Actions.Dot`); `run down` tears the directive's graph down (`downTree`).

`run up`/`run down`/`run serve` take `--json` (milestone 1 of `specs/generic-server.md`): the
binary's text reporters are replaced by one JSON object per line on stdout, flushed per report,
so `run up --json | jq` streams. `Salmon.Reporter.Tagged` is the whole of it — a `Tagged` sum of
the four report streams (`Serve.Report`, `UpDown.Report Extension`, `Upkeep.Report Extension`,
and, since the status sink needed to see it, `Follow.Report`)
tagged by `stream` (not `origin`, which names who typed a command), the four `ToJSON` instances (orphans, kept together there because the two
parametric streams are only encodable at `Extension`, which `UpDown` cannot import), and two
reporters over the sum: `reportTexts`, which dispatches back to the four text reporters
unchanged, and `reportJSONLines`. `CommandLine` builds exactly one `Reporter Tagged` per run and
`contramap`s it into the two the drivers take, so `--json` is a choice of reporter and not a
second reporting mechanism; the `Reporter` stays contravariant and text output is byte-identical
without the flag. Every object has a `kind` (constructor, kebab-cased), a `ref` as
`{short, full}` (`Op/Ref.hs`'s `shortRef`, moved there from `Query` for this) where the report is
about one node, and named fields; a nested report (`Upkeep.Acted`, `Serve.Tended`) reuses the
inner instance under `report`. Report text is public and encoded verbatim, per the spec's
decision. `Test/ReportJsonSpec.hs` holds a golden object per constructor of all four streams, and one for
the status sink document.
Not covered: a node's own `Binary.Report`s (handed a `reportPrint` by the recipe, printed as
text regardless), and sequence numbers (a later milestone).

`run serve` is the odd one out: it reads *seeds* (not a directive) as command lines, one
declaration per line, and keeps converging a `Salmon.Actions.Serve.World` across all of them —
see `Actions/Serve.hs` above for the state it maintains. Its input language is:

```
up <seed args...>      # declare this seed up (added to the active set)
only <seed args...>    # declare this seed up and retire every other one
down <seed args...>    # retire this seed (its nodes go down unless another seed still wants them)
clear                  # retire every seed
converge               # re-attempt whatever hasn't converged (e.g. after fixing what made it fail)
supervise on|off       # whether to tend nodes while the loop is idle (default on)
status | history       # dump the per-node state (first line: `mode: interactive|following|replay`) /
                       # the seed+graph history (each entry annotated [loaded <file>] or
                       # [fetched <registry> label=.. id=.. sha256=..] unless typed)
fetch                  # (--follow) fetch the followed documents now, ladder forgotten, and apply
                       # whatever is pending without waiting out the quiet window
quit                   # leave the loop, changing nothing on the way out
```

Seed args are parsed with the binary's own `ParseRecord seed` — the same words that would follow
`config` — so a seed is identified by the directive it configures to, not by its spelling.
`--listen PATH` (beside `--max-concurrency`/`--no-autoconverge`/`--json`) accepts the same lines
on a unix socket from any number of clients, each answered on its own connection as JSON lines;
see `Actions/Serve/Socket.hs` above and `resources/serve-supervision.md` §13. `--http PATH` serves
the same world over HTTP on a second socket — `curl --unix-socket PATH http://x/dag`, and
`POST /command` with a line as the body — see `Actions/Serve/Http.hs` above and §14.
`--http-tcp HOST:PORT` puts the same HTTP on a network, and only with `--tls-cert`,
`--tls-key` and `--token-file` all given — there is no plaintext option — see §14's "Reaching
it over the network".
`--status-sink PATH` writes the host's status document there after every pass and injection and
on a timer — see `Actions/Serve/StatusSink.hs` above and §12's "Status flows back" — and
`salmon-fleet status DIR` folds a directory of them. `salmon-tui PATH` is a terminal client of
`--http PATH` (and `salmon-tui https://HOST:PORT --token-file FILE [--cacert FILE]` of
`--http-tcp`) — see `Client/Http.hs`/`Client/Model.hs` above and §14's last paragraph.

To build one of these binaries: define a `seed` type, a `directive`/`Spec` type (`FromJSON`/
`ToJSON`), a `Configure IO seed Spec`, and a `Track' Spec` that turns a `Spec` into an `Op` by
composing builtin nodes and/or recipes.

## Naming/vocabulary from the salmon-core README

- **builtins**: mostly atomic nodes, DAGs with small diameter (this is `salmon-ops/.../Nodes/`).
- **recipes/apps**: combinations of builtins; many equivalent graphs may be valid.
- **configs**: user-provided choices, evaluated on the commanding machine.
- **setup**: machine-rationalized, evaluated on the local (target) machine.
- **prefs**: conventions parametrized by domain/service.

## Things to be careful about in this working tree

The working directory contains many *untracked* directories (`git-repos/`, `images/`, `secrets/`,
`certs/`, `tls/`, `jwk-keys/`, `ssh-keys/`, `tokens/`, `wg-tmp/`, `working/`, `acme/`, etc.) that
are scratch space, cloned dependency repos, or credential material for the author's personal
infra — not part of the `salmon` project itself. `git ls-files` is the source of truth for what's
actually part of this repository (currently just the four packages above plus root-level
`README.md`/`cabal.project*`). Don't read from or write into those directories
unless a task explicitly concerns them.
