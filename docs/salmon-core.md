# The salmon model

This document describes the general, domain-independent model that lives in
`salmon-core` — the algebraic graph, the `OpGraph`/`Track` abstractions, and
the traversal semantics. Everything infra-specific (files, systemd, postgres,
podman, ...) is built *on top of* this model in `salmon-ops` and above; none of
it is baked into `salmon-core` itself. If you want copy-pasteable recipes for
writing infra ops, see [`howto-ops.md`](howto-ops.md) instead — this document
is about the shape underneath, and where else that shape could be pointed.

## The problem this is solving

There's a recurring tension in provisioning/CI-CD/ops tooling:

- **One-off tools** that do one narrow thing (apply these DB migrations, sync
  this one directory) are highly tunable but don't compose with each other.
- **Do-everything tools** (turn up my whole dev environment, push to prod) are
  powerful but their behavior becomes inseparable from their configuration —
  changing "generate a secret then ssh in" to "ssh in then generate a secret"
  means editing the tool, not just its config.

Salmon's bet is that both needs are really the same problem at different
scales, *if* you have one representation that's equally comfortable expressing
"create a file" and "turn a server up" — because a "turn a server up" op is,
underneath, nothing but a big DAG of smaller "create a file"/"start a
service"/... ops. Get that representation right, and a one-off tool and a
do-everything tool are the same kind of object, just different-sized graphs.

## The four core pieces

### `Graph` — the algebraic graph

```haskell
data Graph a = Vertices [a] | Connect (Graph a) (Graph a) | Overlay (Graph a) (Graph a)
```

An algebraic graph in the style of the
[Alga paper](https://dl.acm.org/doi/10.1145/3122955.3122956), simplified to a
single `Vertices [a]` constructor instead of separate `empty`/`vertex`/
`overlay`. `Connect g1 g2` means "g1's nodes each precede g2's nodes" (an
ordering relationship); `Overlay g1 g2` means "these two subgraphs co-occur,
with no ordering implied between them." This is the substrate; everything else
in `salmon-core` is built on top of it.

### `OpGraph` — a self-centered node with an effectful neighbor-finder

```haskell
data OpGraph m node = OpGraph
    { predecessors :: m (Graph (OpGraph m node))
    , node :: node
    }
```

This is the key trick that makes "create a file" and "turn a server up" the
*same type* despite wildly different dependency depth: an `OpGraph` doesn't
carry its whole dependency tree up front — it carries a *recipe*
(`predecessors`, itself effectful in `m`) for producing its immediate
predecessors, on demand. "Create a file" and "turn a server up" both typecheck
as `OpGraph m node`; they just differ in how deep and effectful their
`predecessors` recipe turns out to be when it's actually run.

Two ways to compose two `OpGraph`s (from `Salmon.Op.OpGraph`):

- `` x `inject` y `` — add `y` as a `Connect`-ed predecessor of `x` ("`y` must
  happen before `x`").
- `` x `overlaid` y `` — add `y` as an `Overlay`-ed predecessor of `x`
  ("co-occurring with `x`'s existing predecessors, no ordering implied").

### `Track` — "given an `a`, I know how to build the graph for it"

```haskell
newtype Track m n a = Track { run :: a -> OpGraph m n }
```

A `Track` is a *contravariant, divisible functor* — a promise, parametrized by
input type, to produce an `OpGraph`. Concretely: "if you hand me a database
spec, I have a way to turn it into the ops that provision that database." This
is the composition mechanism for builders: it lets a piece of code that
*needs* something (a database, a signed certificate, a running binary) depend
on an abstract "here's how to get one" rather than a concrete provisioning
strategy — the caller supplies the `Track`, so the same consuming code can be
pointed at "provision it locally" vs. "assume it's already there"
(`ignoreTrack`) vs. "fetch it from elsewhere" without changing.

`Tracked` pairs a `Track` with an already-realized value, so *dependent*
values can be built compositionally: `mapTracked`/`apTracked`/`bindTracked`
give it quasi-Functor/Applicative/Monad shapes (each one recording the
accumulated dependency graph as it goes), and `using`/`using2`/`using3` let you
"open" one or more `Tracked` values to build a downstream `OpGraph` while
automatically wiring in everything they depended on.

### `Eval.expand` — materializing the graph

```haskell
expand :: (Monad m) => OpGraph m node -> m (Cofree Graph (OpGraph m node))
```

Walks the effectful `predecessors` recipes all the way down and produces a
fully materialized `Cofree Graph` — the actual, concrete dependency tree,
ready to be folded over (by a traversal like `upTree`/`downTree`, or by
anything else you want to write against a `Cofree Graph`). This is the one
place where "recipe for finding neighbors" becomes "the neighbors."

## Why a graph, not a plain sequence of steps

A flat ordered script conflates two different things: "must happen before"
and "happens to be listed first." A DAG keeps them separate — two independent
branches of provisioning can be *expressed* as unordered (`Overlay`) and a
traversal is free to run them concurrently, retry just one, or report exactly
which one failed, without the script author having had to think about
interleaving at authoring time. The DAG shape is also what makes **dedup**
possible: because nodes carry an explicit identity (`Ref`, at the `salmon-ops`
layer — see [`howto-ops.md`](howto-ops.md) §2.1), the same logical resource
reached via two different paths through the graph is recognized as one node,
not provisioned twice.

## `Actions`/`Act`/`Actionless` — the generic "bag of named effects"

`salmon-core`'s `Salmon.Op.Actions` module defines the generic wrapper that
turns a plain `node` payload into something a traversal can act on
uniformly:

```haskell
data Act ext = Act { shorthand :: ShortHand, extension :: ext }
data Actions ext = Actionless | Actions (Act ext)
```

`Actionless` is the monoidal identity — a node that exists purely for its
position in the graph (grouping/ordering) with genuinely nothing to run.
`salmon-core` itself is agnostic about what `ext` actually *is* — it's a type
parameter. `salmon-ops`'s `Extension` (see `howto-ops.md`) is one particular
choice of `ext`, tailored to shell-command-driven infrastructure provisioning
(`up`/`down`/`prelim`/`check`/`notify`, all `IO ()`-shaped). Nothing about
`OpGraph`/`Track`/`Eval` requires that choice.

## Generalizing beyond infrastructure

Everything above is agnostic to what an "op" actually does. The concrete
`salmon-ops` layer picks `IO ()`-shaped shell-command effects because that's
what provisioning needs, but the same `Graph`/`OpGraph`/`Track`/`Eval` core
would support a differently-shaped `ext`, as long as:

- individual units of work can be given a stable identity (a `Ref`-equivalent,
  for dedup),
- "did this succeed" can be observed (an exception, a return value, whatever
  fits the domain),
  and
- "what must happen before this" can be expressed as a graph rather than a
  flat list.

Concretely, this points at use cases beyond "provision a server," building on
the same up/down/check/notify shape:

- **CI/CD pipelines** — the existing use case in this repo (see
  `SreBox.CabalBuilding`, `SreBox.GeneratedSite`): a build/test/publish
  pipeline is itself a DAG of idempotent steps, and "did this artifact already
  get published" is exactly the kind of `prelim`-skippable check the model was
  built around.
- **Multi-machine/fleet orchestration** — `SreBox.PostgresMigrations`'s
  "upload self and re-invoke as a nested `upTree` on the remote machine"
  pattern (see `howto-ops.md` §5) is already a working instance of "a node's
  `up` is itself a whole graph traversal happening somewhere else"; the same
  shape generalizes to fleet-wide convergence (each machine's target state
  expressed as a graph, driven from a control machine).
- **Data pipelines** — a pipeline stage ("did this table/partition get
  computed for this date") is structurally the same triad as "did this file
  get written": a `Ref` keyed on (dataset, partition), an `up` that computes
  and writes it, a `prelim` that skips recomputation if the output already
  exists and its inputs haven't changed (the same shape as
  `skipIfFileExists`/`skipIfNftRuleExists` — see `howto-ops.md` §4), and a
  dependency graph that's *naturally* a DAG already (this aggregate depends on
  those upstream tables). Where salmon would differ from a typical DAG
  scheduler (Airflow-style) is that here the dependency graph can be computed
  effectfully at expansion time (`predecessors :: m (Graph ...)`) rather than
  only declared statically ahead of a run — useful when which upstream
  partitions a stage depends on isn't known until you've inspected what's
  actually available.
- **Business process modeling / process mining** — a business process (an
  order-fulfilment workflow, an approval chain, an onboarding sequence) is
  itself a graph of steps with real "must happen before" edges, "already
  done, skip it" idempotency (has this approval already been recorded?), and
  a genuine need to know when a step failed vs. was blocked by an upstream
  failure — exactly `upTree`'s `Failed`/`Blocked` distinction. Modeling a
  process this way rather than as an implicit control-flow graph buried in
  code has a second benefit specific to *process mining*: because the graph
  is a first-class, inspectable value (the same `Tree`/`Dot` rendering used
  for infra graphs — see `howto-ops.md` §9) rather than something that only
  exists as the emergent behavior of a program, the *reported* trace of a
  traversal (which nodes were `Eval`ed, `Skip`ped, `Redundant`, `Failed`,
  `Blocked`, and in what order — see `Salmon.Actions.UpDown.Report`) is
  already the kind of event log process-mining tooling wants to reconstruct
  a real process model from, without needing separate instrumentation
  bolted on after the fact.
- **Database/schema migrations as a DAG rather than a linear chain** — most
  migration tools force a strict total order; expressing migrations as an
  `OpGraph` would allow independent migrations (different schemas/tables) to
  be legitimately unordered relative to each other while still enforcing real
  dependencies (this migration needs that table to exist first) explicitly,
  and would get idempotent-rerun and partial-failure reporting for free from
  `upTree`.
- **Declarative, convergent configuration management generally** (the
  Puppet/Ansible/Terraform space) — the `up`-is-idempotent,
  `prelim`-skips-if-already-satisfied, `down`-tears-back-down shape is exactly
  a "declare desired state, converge to it, converge back" model; what salmon
  adds relative to most tools in that space is that the DAG is a first-class,
  inspectable value (`Tree`/`Dot` output — see `howto-ops.md` §9) rather than
  a topological order baked into engine internals, and that a node's
  dependencies can be *computed* (the `predecessors` recipe is effectful) as
  well as declared statically.
- **Anything with a "does this already exist" / "make it exist" / "tear it
  back down" triad and a real dependency structure between the things being
  managed** — the infra domain is simply the first and most fully-built-out
  instance of that triad in this codebase, not an assumption baked into
  `salmon-core`.

None of the above beyond CI/CD and fleet orchestration are implemented in this
repository today — they're recorded here as directions the core model
supports, not as existing code to go read.

## Vocabulary (from the original salmon-core README)

These terms recur throughout the codebase and its documentation:

- **builtins** — mostly atomic nodes, DAGs with small diameter (`salmon-ops`'s
  `Nodes/` modules: `Filesystem`, `Systemd`, `Postgres`, `Podman`, etc.).
- **recipes**/**apps** — combinations of builtins; many equivalent graphs may
  be valid for the same end state (`salmon-ops-recipes`'s `SreBox/` modules).
- **configs** — user-provided choices, evaluated on the commanding machine
  (the `seed`/`Configure` side of the CLI protocol — see `howto-ops.md` §9).
- **setup** — machine-rationalized state, evaluated on the local/target
  machine (the `Spec`/`Op` side of the same protocol).
- **prefs** — conventions parametrized by domain or service (e.g. "migrations
  always ship and run locally" vs. "via a remote connstring" — a convention
  `salmon-ops-recipes` deliberately enforces, per its own package description).

## Where to go next

- [`howto-ops.md`](howto-ops.md) — concrete, copy-pasteable patterns for
  writing and testing `salmon-ops` nodes on top of this model.
- `salmon-core/src/Salmon/Op/{Graph,OpGraph,Track,Eval}.hs` — the actual
  source, all four modules short enough to read end to end in one sitting.
- `salmon-ops/src/Salmon/Actions/UpDown.hs` — the reference traversal
  (`upTree`/`downTree`) built on `Eval.expand`; the closest thing to a
  "how do I fold over one of these graphs for real" example.
