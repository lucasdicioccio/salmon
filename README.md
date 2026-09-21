<p align="center">
  <img src="docs/logo001.png" alt="Salmon" width="220" />
</p>

<h1 align="center">Salmon</h1>

<p align="center">
  Yet another approach to xyz-dependencies: express provisioning, CI/CD, and
  general infrastructure operations as DAGs of idempotent operations
  ("ops"), with uniform up/down/check/notify semantics whether a node is as
  small as "create a file" or as large as "turn a server up."
</p>

## Why

Provisioning/CI-CD tooling tends to force a choice between one-off scripts
(highly tunable, don't compose) and do-everything tools (powerful, but their
behavior and their configuration become inseparable). Salmon's bet: both are
the same problem at different scales if operations are represented as an
inspectable dependency graph rather than a flat ordered script — with the
graph itself given real up/down/check/notify semantics, so it converges to a
target state (and back) instead of just running once. See
[`docs/salmon-core.md`](docs/salmon-core.md) for the full model and where
else it could be pointed.

## Packages

This is a cabal multi-package project, layered strictly bottom-to-top:

```
salmon-core  <-  salmon-ops  <-  salmon-ops-recipes  <-  salmon-apps
                                        ^
                                        |
                          salmon-ops-recipes-experimental
```

- **`salmon-core`** — the core library: the algebraic `Graph`, the
  `OpGraph`/`Track`/`Eval` abstractions that let "create a file" and "turn a
  server up" be the same type. No IO-heavy deps; kept minimal on purpose.
- **`salmon-ops`** — IO-heavy primitives for provisioning/CI-CD (files,
  systemd, Debian packages, podman, postgres, wireguard, certificates, ssh,
  netfilter, cron, rsync, ...), plus the CLI plumbing
  (`Salmon.Builtin.CommandLine`) every salmon-based binary shares.
- **`salmon-ops-recipes`** — higher-level, opinionated compositions of
  `salmon-ops` builtins, where project conventions get enforced (e.g. whether
  migrations ship and run locally vs. via a remote connstring). Has a real
  test suite (see [Testing](#testing) below).
- **`salmon-ops-recipes-experimental`** — recipes needing heavier/less-stable
  dependencies (e.g. `kitchen-sink`-based site generation); kept out of
  `salmon-ops-recipes` so that package stays fast to build. Not part of the
  default `cabal.project` package set.
- **`salmon-apps`** — blessed, project-useful binaries built from the above
  (e.g. `salmon-migrator`, `salmon-pgpair`, `salmon-gcp-toy`).

## Build

```sh
cabal build all
cabal build salmon-core salmon-ops salmon-ops-recipes salmon-apps   # individual packages
cabal build salmon-migrator                                          # a single executable

# experimental/personal packages aren't in cabal.project's package set:
cabal build --project-file=cabal.perso.project salmon-ops-recipes-experimental salmon-personal-apps
```

## Testing

```sh
cabal test salmon-ops-recipes
```

Tests are tiered by IO cost/blast-radius — cheap in-process graph-shape
assertions up through tests that dogfood the project's own `Podman` builtins
to run real recipes against disposable containers. See
[`docs/howto-ops.md`](docs/howto-ops.md#10-testing-ops) for the full
breakdown and how to write new ones.

## The model, in one paragraph

An `Op` (`OpGraph Identity Actions'`) is a graph node centered on itself, with
an *effectful* recipe for finding its own predecessors — this is what lets a
one-line filesystem op and a whole clustered-database-with-replication setup
type-check identically. Every op carries `up`/`down` (bring the effect into
being / undo it), `check` (a `CheckResult` answering "is my effect already in
place", which is what makes a node idempotent to re-run), and a `Ref` that
gives it a stable identity so the traversal can
dedupe a resource reached via multiple graph paths. `upTree`/`downTree`
(`Salmon.Actions.UpDown`) walk the materialized graph, catch and propagate
real failures (a thrown exception marks a node `Failed` and blocks everything
that depends on it, rather than being silently swallowed), and report exactly
what happened, node by node. Full details: [`docs/salmon-core.md`](docs/salmon-core.md)
(the general model) and [`docs/howto-ops.md`](docs/howto-ops.md) (concrete
patterns for writing and testing ops).

## Builtin nodes

`salmon-ops/src/Salmon/Builtin/Nodes/` — mostly atomic, small-diameter ops,
one module per concern:

| Module | Covers |
|---|---|
| `Acme` | ACME/Let's Encrypt certificate issuance |
| `Bash` | ad-hoc shell script ops |
| `Binary` | the shared subprocess-running plumbing (`untrackedExec`, exit-code-checked exec) every other builtin is built on |
| `Cabal` | building Haskell projects with cabal |
| `Certificates` | TLS certificate generation/signing |
| `Continuation` | chaining/sequencing op continuations |
| `CronTask` | cron job management |
| `Debian.Debootstrap`, `Debian.Package`, `Debian.OS` | Debian package installs and base-system setup |
| `Filesystem` | directories, file contents, copy/move/replace-directory (the canonical small example — see `docs/howto-ops.md`) |
| `Git` | git repository operations |
| `Keys` | key material management |
| `Netfilter` | `nft` firewall rules (with `check`-based idempotency, since `nft add rule` itself isn't idempotent) |
| `Nginx` | nginx site/config management |
| `Npm` | npm package operations |
| `PgBouncer` | PgBouncer connection-pooler configuration |
| `Podman` | container image/volume/network/env lifecycle |
| `Postgres` | cluster creation, users/groups/grants, WAL streaming replication, `pg_hba.conf` management |
| `Routes` | IP routing table entries |
| `Rsync` | file/secret transport over rsync |
| `Secrets` | secret material placement |
| `Self` | uploading and re-invoking this binary on a remote machine |
| `Spago` | PureScript/Spago builds |
| `Ssh` | SSH remote-machine plumbing |
| `Sysctl` | kernel parameter tuning |
| `Systemd` | systemd unit management |
| `Tar` | archive creation/extraction |
| `Upx` | binary compression |
| `User` | OS user/group accounts |
| `Web` | HTTP-facing ops |
| `WireGuard` | WireGuard VPN interfaces, keys, and routing |

## Recipes

`salmon-ops-recipes/src/SreBox/` — opinionated compositions of the builtins
above, where this project's own conventions get enforced:

| Module | Covers |
|---|---|
| `CabalBuilding` | building and publishing cabal-based binaries |
| `CertSigning` | certificate signing workflows |
| `DNSRegistration` | DNS record registration |
| `Environment` | environment/machine bootstrapping |
| `Initialize` | initial setup sequencing |
| `JWTSigning` | JWT signing key management |
| `MicroDNS` | a minimal DNS server setup |
| `PostgresInit` | database/user/group/grant setup for a Postgres cluster (locally or driven onto a remote machine via `Self`) |
| `PostgresMigrations` | shipping and running migrations, local or remote-connstring |
| `PostgresPair` | two machines, one Postgres cluster, and a *declared* primary: switchover, operator-decided failover, `pg_rewind` rejoins, and pgbouncer routing that follows — see [`docs/postgres-pair.md`](docs/postgres-pair.md) |
| `Postgrest` | PostgREST service configuration |
| `WireGuardVpn` | a full static-server/dynamic-client WireGuard VPN, transport-agnostic on key exchange |

`salmon-ops-recipes-experimental/src/SreBox/` (not part of the default
package set — see [Build](#build)): `KitchenSinkBlog`,
`KitchenSinkMultiSites`, `GeneratedSite`.

## Docs

- [`docs/salmon-core.md`](docs/salmon-core.md) — the general, domain-independent
  model (`Graph`/`OpGraph`/`Track`/`Eval`), why it's shaped this way, and
  putative use cases beyond infrastructure provisioning.
- [`docs/howto-ops.md`](docs/howto-ops.md) — a concrete, example-heavy
  cookbook for declaring and testing `salmon-ops` nodes, written to be usable
  as a reference even by less-context-heavy tooling.
- [`docs/serve-supervision.md`](docs/serve-supervision.md) — what `run serve`
  gives you for free (convergence, `status`/`force`/`pause`, self-healing on
  re-declaration), how to try it, and what to decorate a node with (`check`,
  `Supervision`, `managed`) to get more out of it.
- [`docs/postgres-pair.md`](docs/postgres-pair.md) — a Postgres primary and
  standby whose primary is a declaration rather than a discovery: what a
  switchover does step by step, when the recipe refuses and why, and how to
  watch a client keep writing across one with the `salmon-toy-qemu-pg-ha`
  demo.
- [`docs/gcp-toy-validation.md`](docs/gcp-toy-validation.md) — how to exercise
  the GCP builtins against a real, throwaway project with the `salmon-gcp-toy`
  binary: what it declares, how to read an `up`/`up`/`down` run, and what the
  automated (Layer 0) tests cannot tell you.
- [`CLAUDE.md`](CLAUDE.md) — repository-level conventions (package layering,
  idempotency conventions, failure-propagation rules) for anyone (human or
  AI) working in this codebase.
- [`CHANGELOG.md`](CHANGELOG.md) — release history.
