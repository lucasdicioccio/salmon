# CLAUDE.md

Guidance for Claude Code in this repository. This file is the short version. The per-module
design notes (what is load-bearing, what was a defect first, what has never been run) are in
`resources/module-notes.md`: **grep it for the module's name before changing that module**, and
update it there, not here.

## What this is

Salmon is a Haskell toolkit for expressing provisioning/CI-CD operations as DAGs of idempotent
operations ("ops") with uniform up/down/check semantics, whether a node is "create a file" or
"turn a server up".

## Packages

Dependency direction is strictly `salmon-core` ← `salmon-ops` ← `salmon-ops-recipes` ← `salmon-apps`.

- `salmon-core`: the `Graph`/`OpGraph` DAG representation and evaluation. Minimal deps on purpose.
- `salmon-ops`: IO-heavy builtins (`Salmon/Builtin/Nodes/`), the drivers (`Salmon/Actions/`), the
  shared CLI (`Salmon.Builtin.CommandLine`), the `serve` loop and its HTTP/UI/client.
- `salmon-ops-recipes`: opinionated recipes built from builtins (`SreBox.*`). No heavy deps.
- `salmon-ops-recipes-experimental`: recipes needing heavy/unstable deps (`kitchen-sink`,
  `acme-not-a-joke`). **Not** in `cabal.project`; built only via `cabal.perso.project`.
- `salmon-apps`: binaries. `salmon-migrator`, `salmon-pgpair`, `salmon-report` (read-only
  capability/DNS probes), `salmon-fleet` (status fold, `keygen`, `sign`), `salmon-tui` (brick
  client of `serve --http`), `salmon-docs-sync`, `salmon-gcp-toy`, `salmon-toy-qemu-pg-ha`.

## Build and test

```sh
cabal build all
cabal build salmon-migrator
cabal test salmon-ops-recipes
cabal build --project-file=cabal.perso.project salmon-ops-recipes-experimental salmon-personal-apps
```

- GHC 9.10.3, no `source-repository-package` pins. `cabal.project.local` (tracked) carries
  `allow-newer` pins for `dhall-json`; do not remove without checking the build resolves.
  Building the experimental package needs `allow-newer: acme-not-a-joke:filepath`.
- Test layers: Layer 0 is pure/in-process (graph shape, rendered scripts, parsers); Layer 2 runs
  real recipes in disposable podman containers or user-scope systemd (skipped loudly when the
  tool is missing); Layer 3 is qemu VMs. See `salmon-ops-recipes/test/Test/Harness.hs`.
- The suite runs groups in parallel in one process and nothing passes `close_fds`: mark your own
  fds close-on-exec in tests that spawn children.
- Much of the GCP, quadlet system-scope, and pair-over-TLS code is **Layer 0 only**. When adding
  code, say in the notes what was actually run and what was not; do not claim more.

## Core architecture

In `salmon-core/src/Salmon/Op/`:

- `Graph`: algebraic graph with `Vertices [a]`, `Connect g1 g2`, `Overlay g1 g2`.
- `OpGraph m node`: a node plus an *effectful* `predecessors` recipe. `inject` adds a predecessor
  via `Connect` (must happen before); `overlaid` via `Overlay` (co-occurring, unordered).
- `Track m n a = a -> OpGraph m n`: contravariant builder composition; `Tracked` pairs one with a value.
- `Eval.expand`: materializes the dependency graph as a `Cofree Graph`.

The rest of `Salmon.Op.*` lives in `salmon-ops/src/Salmon/Op/`:

- `Dag.foldDag`: collapses that to one representative per `Ref`, with dependencies *and*
  dependants. A `Ref` is **location-addressed** (kind tag + author-chosen key: "the same effect
  site"), so two declarations can collide; last writer wins and the loser is reported
  `Conflicting`. `sameRepresentative` compares only `shorthand`/`help`/`notes`/rendered `dynamics`
  (functions are not comparable), so content must show in `notes` to be seen as a change.
- `Ledger`: who still wants which nodes. A set, not a refcount; a retraction retires, not deletes.
- `Rewrite`: the only place cross-declaration knowledge can live (e.g. batching packages); runs
  after the fold, reads `dynamics`.
- `Supervision`: per-node restart policy, carried on `dynamics`. `Window`: maintenance-window gate.
- `Configure m seed a`: seed → directive, kept possibly pure.

## salmon-ops layer

- `Extension` (`Builtin/Extension.hs`) is every op's payload: `help`, `notes`, `ref`, `up`,
  `check :: IO CheckResult`, `down`, `dynamics`, and `managed` (a long-running action only
  `run serve` honours). `Op = OpGraph Identity Actions'`. Build nodes with `op`, from
  `noop`/`nodeps`/`deps`. `Nodes/Filesystem.hs` is the canonical small example.
- Drivers: `Actions/UpDown.hs` (sequential, `run up`/`run down`), `Actions/Concurrent.hs` (one
  thread per node, STM scheduling, no concurrency cap: only an edge protects a shared resource),
  `Actions/Upkeep.hs` (continuous tending), `Actions/Serve.hs` (the long-running loop: a `World`
  of ledger + magma + per-node state).
- Failure containment: a failed `up` blocks its dependants; a failed `down` blocks its
  predecessors. A node appears once however many paths reach it. Cycles are reported `Blocked`.
- `serve`: every command runs `stopTending` first; machines tend only while the loop is idle.
  HTTP reads (`/dag`, `/status`, `/history`) bypass the inbox and never stand a machine down.
  A `managed` node is invisible to both passes and its machine is kept across commands. Moving a
  declaration is `up` new then `down` old, not `only`.
- Paths are matched, never listed (`Query.Outline`): the declared graph is a tree and paths
  double with every diamond.
- `/openapi.json` is hand-written with drift tests (`Test/ServeApiSpec.hs`): a new report
  constructor, renamed field or new route must be added to the document and the goldens
  (`Test/ReportJsonSpec.hs`).

## Conventions for node authors

None enforced by types; every builtin follows them.

- **Idempotent `up`.** Prefer `replace`/set verbs; else `IF NOT EXISTS` guards; else shell
  check-then-act; else append-if-missing. When the tool has no set verb (`nft add rule`), detect
  the effect in `check` instead (`Netfilter.rule`).
- **`up` must throw to fail.** `withBinary`/`untrackedExec` do this on non-zero exit. With
  `withBinaryIO` use `Binary.checkExitCode`; a nested `upTree` must have its `Bool` checked and thrown.
- **`check` semantics.** `Success`/`Completed`: effect in place, skip. `Failure`: apply. `Unknown`:
  ran and could not tell (use for transitional states; under `serve` it restarts nothing).
  `Immaterial`: the default with no `check`, meaning asking costs what applying costs; `up` runs
  each pass and under `serve` the node parks and is watched by nothing. Only a node's `check` can
  notice its effect going away. Adding a check changes `run up` for existing callers (a satisfied
  node stops being re-applied), so it is a deliberate decision.
- **Secrets.** Report text, `help`, `notes`, argv and failure text are public. Secret bytes are
  read at `up`/`check` time, never at graph build, and never appear in any of those, **nor does
  any digest of them** (a keyed HMAC where a fingerprint is needed, see Quadlet). Recipes take
  pre-provisioned paths and never choose a transport. A command whose output may hold a secret
  stays `captured` (`Binary.Routing` has no global streaming switch).
- **No child inherits stdin.** Under `serve` stdin is the command channel. Use
  `Binary.execDetached`/`detachedStdin`, never `callProcess` or a bare `createProcess`; gcloud
  runs with `--quiet`.
- **Nodes that destroy data are guarded by identity, not by a marker file**: Postgres system
  identifier before any `rm -rf`/rewind; `salmon-template:`/`salmon-clone:` comments before a
  drop; ownership markers before a GCP delete; never take over or delete what salmon cannot
  prove it made (exception: a DNS record set's name+type is the effect site and is taken over).
- **Nothing names a primary or decides a machine is dead.** Patroni/HAProxy nodes render
  members; `PostgresPair` needs the operator's `pair_may_discard`/`pair_reseed` to discard data.
- **Do not change what existing declarations render.** New options default to rendering nothing
  (same file, `ref`, `help`, `notes`), usually pinned by hash in the specs, because a changed
  representative or unit file restarts running services.
- Directories shared by many nodes are their own node, created and never removed.
- An `fmap` over an `Op` reaches its dependencies too; apply extras to the one node.
- Prefer amending `defaultSupervision` over spelling out every field.

## The seed → directive → ops CLI

Every binary uses `CommandLine.execCommandOrSeed` (`salmon-apps/src/Migrator.hs` is the worked example):

```sh
my-salmon config <seed-args...>     # seed -> JSON directive on stdout
my-salmon run up|down|tree|dag      # JSON directive on stdin
my-salmon run serve [--listen PATH] [--http PATH] [--http-tcp HOST:PORT --tls-cert F --tls-key F --token-file F]
                    [--follow REGISTRY --label L ...] [--status-sink PATH|URL] [--json]
```

`serve` reads seeds as lines: `up|only|down <seed args>`, `clear`, `converge`, `supervise on|off`,
`status`, `history`, `fetch`, `force|recheck|pause|resume [--select P]`, `quit`. `--json` swaps the
text reporters for one JSON object per line (`Salmon.Reporter.Tagged`). To build a binary: a
`seed` type, a JSON `Spec`, a `Configure IO seed Spec`, and a `Track' Spec`.

## Where things are

Details for each are in `resources/module-notes.md` under the same names.

| Area | Code | Spec / guide |
|---|---|---|
| Per-node state machines, upkeep, supervision | `Actions/{Concurrent,Upkeep,Serve}.hs`, `Op/{Status,Mailbox,Supervision}.hs` | `specs/per-node-state-machines*.md`, `resources/serve-supervision.md` |
| serve socket, HTTP, events, web UI, TLS | `Actions/Serve/{Socket,Http,Events,StatusSink}.hs`, `salmon-ops/ui/`, `Client/` | `specs/generic-server.md` |
| Pull mode (registries, scheduler, cache, signatures) | `Actions/Follow.hs`, `Actions/Follow/` | `specs/pull-mode.md` |
| Guarded risky operations (preconditions, park, hold siblings) | `Builtin/Guarded.hs`, `Op/Guard.hs` | `specs/pg-patroni.md` |
| Query, selectors, rewrites | `Actions/Query.hs`, `Op/Rewrite.hs` | `specs/advance-querying.md` |
| Processes, output routing, node logs, daemons | `Builtin/Nodes/{Binary,Daemon}.hs`, `Builtin/NodeLog.hs` | `specs/salmon-as-init.md` |
| Postgres, replication, templates | `Nodes/Postgres.hs`, `SreBox.PostgresTemplate` | |
| Postgres pair / switchover | `SreBox.PostgresPair`, `SreBox.PostgresPairPrereqs` | `specs/pg-switchover.md`, `resources/postgres-pair.md` |
| Patroni, pgBackRest, HAProxy | `Nodes/{Patroni,PgBackRest,Haproxy}.hs` | `specs/pg-patroni.md` |
| systemd services, jobs, timers | `Nodes/Systemd.hs`, `Nodes/Systemd/Job.hs` | |
| Podman, quadlets, registry logins | `Nodes/Podman.hs`, `Nodes/Podman/Quadlet.hs` | |
| GCP (compute, DNS, load balancer, storage, secrets) | `Nodes/Gcp/` | `specs/gcloud-support.md`, `resources/gcp-toy-validation.md` |
| Secrets delivery, deferred sub-graphs | `Nodes/{SecretDelivery,Deferred}.hs` | |
| Debian packages / apt index | `Nodes/Debian/Package.hs` | |
| WireGuard mesh, UPnP port mapping | `SreBox.WireGuardMesh`, `Nodes/PortMapping.hs` | `specs/wireguard-mesh.md` |
| qemu harness and live demo | `salmon-apps/src/QemuPgHaToy.hs`, `Test.PostgresVms` | `specs/qemu-test-vms.md` |

Vocabulary: **builtins** (near-atomic nodes), **recipes/apps** (combinations), **configs**
(evaluated on the commanding machine), **setup** (evaluated on the target), **prefs** (conventions).

## Working tree

Many *untracked* directories here (`git-repos/`, `images/`, `secrets/`, `certs/`, `tls/`,
`jwk-keys/`, `ssh-keys/`, `tokens/`, `wg-tmp/`, `acme/`, ...) are scratch space or credential
material for the author's personal infra. `git ls-files` is the source of truth. Do not read
from or write into them unless a task explicitly concerns them.
