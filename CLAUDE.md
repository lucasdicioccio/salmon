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

There are no test-suite stanzas in any `.cabal` file currently — there is nothing to run with
`cabal test`.

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
  `prelim` (a `Requirement` check — `Required`/`Skippable` — run before `up`), `down`, `check`,
  `notify`, plus `dynamics :: [Dynamic]` for attaching arbitrary typed metadata that can be
  recovered later via `getDynamics`/`collectDynamics` (used e.g. to flatten "remote call" ops out
  of a graph). `Op = OpGraph Identity Actions'` is the type alias used everywhere in node/recipe
  code. Building a node normally goes through the `op :: ShortHand -> Identity (Graph Op) ->
  (Extension -> Extension) -> Op` helper, starting from `noop`/`nodeps`/`deps`.
- **Builtin nodes** live under `salmon-ops/src/Salmon/Builtin/Nodes/` — one module per concern
  (Filesystem, Systemd, Debian.Package, Podman, Postgres, WireGuard, Certificates, Ssh, Git,
  Netfilter, CronTask, Rsync, etc). Look at `Filesystem.hs` as the canonical small example of the
  `op` pattern (a value type like `Directory`/`FileContents`, a smart constructor returning `Op`
  that fills in `help`/`notes`/`ref`/`up`/`down`).
- **`Actions/UpDown.hs`** implements graph execution: `upTree` walks the expanded `Cofree Graph`
  bottom-up, dedupes by `Ref` (an already-visited ref is reported `Redundant` and skipped even if
  reachable via multiple paths), evaluates `prelim` to decide `Skip` vs `Eval`+`up`. `downTree` is
  the simpler top-level teardown pass.
- **`Builtin/CommandLine.hs`** wires all of the above into the CLI every salmon binary shares:
  `execCommandOrSeed` implements the two-phase protocol described below.
- **`Op/Configure.hs`**: `Configure m seed a = Configure { gen :: seed -> m a }` — deliberately
  kept possibly-pure (non-IO) so the "turn a human-facing seed into a directive" step can be
  hermetically separated from the IO-heavy "turn a directive into ops and run them" step.

## The seed → spec → ops CLI protocol

Every salmon binary (see `salmon-apps/src/Migrator.hs` as the worked example) exposes two
subcommands via `Salmon.Builtin.CommandLine.execCommandOrSeed`:

```sh
my-salmon config <seed-args...>   # seed (human/CLI-friendly) -> JSON-encoded directive on stdout
my-salmon run Up|Tree|DAG         # reads a JSON directive on stdin, expands it into an Op graph, executes/prints it
```

Typical usage pipes them together: `my-salmon config 123 | my-salmon run Up`. This split exists so
that config generation (which may be impure/human-parametrized) and execution (which must be
IO/hermetic and is meant to run unattended, e.g. on a remote box) are distinct, independently
inspectable steps — the JSON directive is the contract between them. `run tree` prints a
human-readable dependency tree (`Actions.Help`); `run dag` prints Graphviz dot output
(`Actions.Dot`).

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
