# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Salmon is a Haskell library/toolkit for expressing infrastructure/provisioning/CI-CD operations
("xyz-dependencies") as DAGs of idempotent operations ("ops"), with uniform up/down/check/notify
semantics regardless of whether a node is as small as "create a file" or as large as "turn a server up".

## Packages (cabal multi-package project)

- `salmon-core` — the core library: the `Graph`/`OpGraph` DAG representation and evaluation
  primitives. No IO-heavy deps; kept minimal on purpose.
- `salmon-ops` — IO-heavy primitives for provisioning/CI-CD tasks (files, systemd, debian
  packages, podman, postgres, wireguard, certificates, ssh, etc.), plus the CLI plumbing
  (`Salmon.Builtin.CommandLine`) that all salmon-based binaries use. Tries to stay light on
  cabal deps but allows heavier deps for genuinely deep tasks (e.g. cert generation/signing).
- `salmon-ops-recipes` — higher-level, opinionated "recipes" built out of `salmon-ops` builtins
  (e.g. `SreBox.PostgresMigrations`, `SreBox.CertSigning`, `SreBox.MicroDNS`). This is where
  conventions get enforced (e.g. whether migrations ship and run locally vs. via a remote
  connstring). Kept deliberately free of heavy/unstable dependencies.
- `salmon-ops-recipes-experimental` — recipes that need heavier or less-stable dependencies:
  `SreBox.KitchenSinkBlog`/`SreBox.KitchenSinkMultiSites` (pull in the `kitchen-sink` library) and
  `SreBox.GeneratedSite` (builds/publishes kitchen-sink-generated sites via `SreBox.CabalBuilding`).
  Split out of `salmon-ops-recipes` so that package's build stays fast; **not** part of the
  default `cabal.project` package set — it's only built via `cabal.perso.project` (see below).
- `salmon-apps` — blessed, project-useful binaries built from the above (e.g. `salmon-migrator`,
  see `Migrator.hs` / `MigratorApp.hs`).

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
  only stopped asking a question nobody wrote an answer to. **A failing `up` backs off** (doubles) while a *vanished effect*
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
  nothing happens until somebody says it should. Prefer amending `defaultSupervision` to
  spelling out every field — the record has grown twice and will again.
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
  alone), so it is a heuristic; it is still strictly more than the zero available before. See
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
  `Serve.serveWith`; they apply to `run up`/`run down`/`run serve` but **not** to `run
  tree`/`run dag`/`query`, which still print the declared graph (a rewritten `Dag` has no paths
  for a `--select` pattern to match). See milestone 5 and `Test/RewriteSpec.hs`.
- **`Actions/Serve.hs`** is the long-running counterpart to the one-shot `upTree`: it keeps a
  `World` — a `worldLedger` of who's asked for what, a `worldMagma` of one representative per
  `Ref` (what each node *is*), a `NodeState` per node (a `Direction` it's wanted in plus whether
  it has `Converged` there), and `worldEpochs`, the declared seed / directive / graph, kept only
  for declarations that are still live. Everything else is derived: a node some live declaration
  asks for is wanted `TurnUp`, a node no live declaration still asks for is wanted `TurnDown`,
  and flipping a node's direction resets it to `Pending`. Converging is then one teardown pass
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
```

Typical usage pipes them together: `my-salmon config 123 | my-salmon run up`. This split exists so
that config generation (which may be impure/human-parametrized) and execution (which must be
IO/hermetic and is meant to run unattended, e.g. on a remote box) are distinct, independently
inspectable steps — the JSON directive is the contract between them. `run tree` prints a
human-readable dependency tree (`Actions.Help`); `run dag` prints Graphviz dot output
(`Actions.Dot`); `run down` tears the directive's graph down (`downTree`).

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
status | history       # dump the per-node state / the seed+graph history
quit                   # leave the loop, changing nothing on the way out
```

Seed args are parsed with the binary's own `ParseRecord seed` — the same words that would follow
`config` — so a seed is identified by the directive it configures to, not by its spelling.

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
`README.md`/`CHANGELOG.md`/`cabal.project*`). Don't read from or write into those directories
unless a task explicitly concerns them.
