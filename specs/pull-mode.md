# Pull mode: a `serve` that fetches its own declarations

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan. It grew out of a fleet-management assessment; the companion
idea (a generic salmon server with web/terminal clients that render the live
`Dag`) is a separate sketch and is only referenced here where the two meet.

## Problem

Today's `run serve` only ever waits to be *pushed at*. Its whole input surface
is one `Handle`: `serveWith` forks a reader thread that `hGetLine`s into a
`TChan` and the loop reads commands off that channel
(`salmon-ops/src/Salmon/Actions/Serve.hs`, `serveWith`/`readInto`/`loop`).
The only way a declaration reaches a remote machine is a controller that
ssh-es in — and what it runs there is a one-shot `run up` with the directive
on stdin (`Salmon.Builtin.Nodes.Self.callSelf`), never a `serve`.

So the shape we have is hub-and-spoke push: a controller holds every
declaration, opens a connection per host per change, and learns an exit code
back. That has three limits that get worse with every host added:

- **Every host needs an inbound path from the controller** (ssh, a key, a
  firewall rule), and the controller must be up at the moment a change is
  wanted.
- **A `serve` on a remote host is unreachable once started.** `ssh host bin
  run serve` works, but nothing can then talk to it except that one ssh
  session's stdin.
- **There is no fleet-level state.** Each `serve` is an island; nothing
  answers "which hosts have converged to which declaration".

The missing mode is the inverse: a host that periodically *fetches* its
desired state from somewhere and converges on it — the kubelet / Puppet-agent
shape. It is the cheapest route to a fleet because it needs **no control
plane and no inbound port on any host**. Nothing in it requires consensus;
it requires a place to put documents.

```
controller ──writes──▶ dumb store (file / git / bucket / HTTP)
                            │ fetch                 ▲
              ┌─────────────┼─────────────┐         │ status
        host A: serve   host B: serve   host C: serve
          --follow        --follow        --follow
```

## What the loop already has

Most of a puller's semantics exist; what's missing is the transport and one
ordering rule.

- **`load <file>`** runs a file's lines through the command language, nested
  loads included, depth-capped (`loadFile`, `maxLoadDepth`). A puller is
  "`load` from somewhere else, on a trigger".
- **`only <seed>`** retires every other seed, **`up`/`down`** add/retire one,
  and **`clear`** retires all. Re-declaring an unchanged seed is a no-op
  because the ledger unifies by directive (`Salmon.Op.Ledger`, and
  `Serve.record`'s `Stale`-vs-unchanged comparison).
- **`autoconverge off` + `converge`** separates *recording* declarations
  from *acting* on them (`Serve.hs`, `AutoConverge`), so a fetched document
  can be applied as one atomic batch of declarations followed by a single
  pass, rather than N passes.
- **`up-directive <file>`** declares from a directive JSON rather than seed
  args, so a document can carry either spelling.
- **The reader is the only thing that assumes stdin.** `serveWith` already
  takes any `Handle`; the loop reads `Maybe String` off a `TChan`. Generalizing
  the reader to a *merged* source of lines is a small change.

## Design

### The fetched thing is a declarative document, not a command log

The document is the **desired set**: the seeds this host should have live.
It is *not* a sequence of `up`/`down` commands, and it is **JSON**, not seed
lines — JSON is what `up-directive` already speaks, what every tool that
might produce or inspect a document (CI, a web UI, `jq`, a controller written
in anything) speaks, and what a signature can be computed over
unambiguously. Seed-line spelling stays available *inside* it, as an array of
words, so a document can carry either a seed or a fully-configured directive:

```json
{
  "salmon": 1,
  "id": "web-api@2026-09-23T10:41:07Z",
  "seeds": [
    { "seed": ["base-packages"] },
    { "seed": ["app", "--version", "42"] },
    { "directive": { "...": "a directive JSON, as `up-directive` takes" } }
  ]
}
```

`salmon` is a format version; `id` is opaque, chosen by the publisher, and is
what `history` records (below). Anything else at the top level is ignored by
v1 so publishers can annotate.

Reason for a document rather than a log: a log needs a cursor, exactly-once
delivery, and a story for a host that missed the middle of it. A document is
idempotent — safe to re-fetch, safe to fetch twice, safe to fetch after a
month offline. The puller diffs the document against the live ledger and
emits, on the loop's inbox:

```
autoconverge off
up   <seed A>        # in document, not live
up   <seed B>        # in document, already live -> no-op by the ledger
down <seed C>        # live, not in document
autoconverge on      # (restore whatever it was)
converge
```

`only` is the tempting one-liner but it is the wrong primitive: it retires
everything not named, which conflicts with a host following several labels
(each document covers only its own label's seeds) and with an operator who
typed something interactively that the document shouldn't know about. The diff is explicit about what it
retires; `only` is not.

`force`/`recheck`/`pause`/`resume` are *not* state and do not belong in the
document. They stay on a push channel (stdin today; a socket later) or in a
separate, consumed-once side file if there's a real need.

### Labels are addresses into a registry

A **label** is not a selector inside one big fleet file; it is *syntax for
addressing the latest document* in a larger registry. A host started with
labels `web-api` and `canary` fetches two documents — "latest for `web-api`",
"latest for `canary`" — and its desired set is their union. A registry is
anything that can answer "latest document for `<label>`", and the label is
spliced into an address by a small template the registry backend owns:

| registry backend | how `<label>` becomes an address | latest / change detection |
|---|---|---|
| directory | `<dir>/<label>.json` | mtime + content hash, or inotify |
| git repo | `<repo>/<label>/latest.json` (a subdirectory per label; history is git's) | commit id |
| HTTP | `https://controller.example/seed/latest/<label>` | ETag / `If-None-Match`, else hash |
| bucket | `gs://<bucket>/<label>/latest.json` (`Gcp/Storage`) | object generation |
| DNS | `<label>.<zone>`, e.g. `web-api.controller.salmon.example` | see below |

The DNS backend is the "hack" worth spelling out because it is cheap to poll
and salmon already runs DNS (`SreBox.MicroDNS`, `SreBox.DNSRegistration`), so
a controller can *publish* records with nodes that exist today. Two shapes:

- **Index only.** A `TXT` record at `<label>.<zone>` carrying
  `v=salmon1 url=<where the JSON is> sha256=<digest>`. The host polls DNS
  (one UDP round-trip, cached by TTL, no connection to the controller) and
  fetches the payload only when the digest changes. DNS is the registry's
  *index*; HTTP/git/bucket is its *storage*. This is the recommended shape.
- **Inline.** For a document small enough, the `TXT` record *is* the
  document (base64, chunked at 255 bytes as TXT allows). Fine for a
  one-seed label; not the general case.

Either way the record's TTL is a natural, controller-chosen lower bound on
how fast a change propagates, and the zone's serial is a fleet-wide "did
anything change" bit for free.

Every backend above is already something salmon can *do* as an op. That is
the point of the second observation: **the agent's inbox can be a node in its
own graph** — "the latest document for `<label>` from `<registry>` is at this
local path" is a `Filesystem`/`Git`/`Storage`/`Web` node with the usual
`check`/`up`/failure reporting, and the puller reads a local file that this
node keeps fresh. That gets fetch failures into the same `Report` stream as
everything else instead of a separate log.

Labels are given at start (`--follow <registry> --label web-api --label
canary`); a label file the host re-reads on each round is a cheap extension
so that re-labelling doesn't need a restart, and is not v1.

Two labels whose documents disagree about one effect site are not a
registry-level error: both seeds are declared, and the `Dag` reports the
collision as `Conflicting` exactly as it would for two interactive `up`s. The
registry is not where that is resolved.

A directory registry ships first (it's also the test harness: write a file,
watch the world change), git second (the natural "desired state is a repo"
workflow), HTTP and DNS-index after, bucket last.

### Change detection happens *before* injection — the one rule that's easy to get wrong

Every line that arrives on the inbox stops tending: `loop` calls
`stopTending` before handling any command, by design (a command is about to
act on the nodes). A naive poller that injected on every tick — even "nothing
changed" — would therefore **starve the supervisor**: at a 30s poll the
machines would never reach their 60s check ceiling, and a `managed` node's
watch would be interrupted every tick.

So the fetcher hashes / ETags what it got and injects **only on change**. An
unchanged poll must be invisible to the loop. Corollary: the poll interval
and the tending loop are independent; polling can be aggressive without
costing supervision anything.

### A scheduler owns the rounds: backoff toward the registry, debounce toward the loop

Polling rounds must be decoupled from inbound events, because there will be
both: a round is not triggered by a line arriving on stdin or a socket, and a
line arriving does not reset the round clock. The scheduler is one thread per
followed registry producing "fetch now" ticks, and it has two jobs that point
in opposite directions.

**Toward the registry: exponential backoff on failure.** A round that fails
(unreachable, 5xx, unparseable, signature bad) schedules the next one at
`min(cap, base · factor^n)` with jitter; a round that succeeds resets `n`.
Configuration is the usual four numbers (`base`, `factor`, `cap`, `jitter`)
with defaults in the tens-of-seconds to minutes range. This protects the
registry from a fleet hammering it during its own outage, and the jitter
spreads a fleet that all rebooted at once. A successful round that observed
*no change* keeps the base interval; there is no reason to slow down while
quiet, and the base interval (plus DNS TTL, for that backend) is already the
propagation bound the operator chose.

**Toward the loop: debounce on change.** A change observed in a round does
not inject immediately. The scheduler opens a quiet window (`debounce`,
default a few seconds, configurable up to minutes) and injects the *latest*
document seen once no further change has been observed for that long, with
a `max_wait` after which it injects regardless. This coalesces a publisher
that writes three times in a row (a CI job pushing per-label documents one
after another, an operator saving a file twice), so the controlled system
sees one diff-batch and one convergence pass, not three — and so a
half-published state is never applied. It also bounds how often the
supervisor is stood down by document traffic, which is the starvation rule
above restated as a rate.

Both knobs live on the follow, not on the seed: `--follow <registry>
--poll 30s --backoff 5s..10m --debounce 5s`. A `fetch` command on the loop's
input language triggers a round out of schedule (an operator who just
published and doesn't want to wait), and is the one place inbound events
touch the scheduler.

`Upkeep` has a ladder of the same shape (double to a cap, halve to a floor),
but it is a per-node question about *checks*; this one is per-registry about
*fetches*. They should not share code beyond a small `Backoff` value type.

### Merged input, one inbox

Rather than a second loop, the reader becomes a *set* of producers into the
existing `TChan`:

- stdin (today's behaviour, unchanged, and still what a piped script uses);
- the fetcher, injecting a diff-batch on change;
- later, a socket (the generic-server sketch).

The loop does not care which producer a line came from. This preserves the
property the loop's own comment defends: a piped script has every line queued
before the first pass ends and is never supervised. A fetcher whose first
fetch happens *before* the loop starts (i.e. on startup, synchronously)
behaves the same way — deterministic first convergence — and only later
changes arrive "live", handled by the tending loop as any interactive command
would be. `status` should say which it is (see "what this doesn't solve").

### Groups, canaries and rollouts are registry writes

Because a label addresses a document, host groups, canaries and staged
rollouts are **writes to the registry**, with no service tracking
membership: publish `app --version 42` under `canary`, watch the status sink,
then publish it under `web-api`. A host in both groups gets the union, which
for two versions of one app is a `Conflicting` in its `Dag` — visible, and
the publisher's mistake to fix, not the registry's. This is also the first
meaningful use of a unified `Remote` type (today `Self`, `Ssh` and `Rsync`
each define their own `{user, host}` record): a host is a name plus its
labels.

### The fetcher is an actor in `history`

Every declaration the fetcher makes is recorded in `history` with its
provenance — registry, label, document `id`, digest — as a distinct actor
from an operator's typed line or a `load`ed file. `LogEntry` grows an origin
(`Typed | Loaded path | Fetched registry label id digest`), and `history`
prints it. Cheap, and it is the only way an operator can tell "I typed this"
from "the document said so" when a host does something surprising.

### Status flows back the same way

After every pass the host pushes its `status` snapshot — the same JSON the
generic-server sketch wants for its `Dag` endpoint — to a sink: a file, an
HTTP `POST`, or a bucket object keyed by host. Fleet status is then a **fold
over those objects**, computed by whoever reads the store (a script, the
web UI, a `salmon-fleet status` subcommand), not by a running service. The
store is the only shared dependency and it's a dumb one.

Same rule as fetching: pushing status is itself an op (a `filecontents`, a
`Storage` upload), so a sink that's down shows up as a `Failed` node, not a
silently stale dashboard.

### Signed documents

Pulling inverts trust: today a host trusts whoever holds an ssh key to it;
in pull mode it trusts *the source*. TLS to the store covers transport. For
the document itself, the tree already has JWT signing (`SreBox.JWTSigning`),
`Keys`, and `Certificates` — a document can carry a detached signature the
puller verifies against a key it was started with, before any line is
injected. A document that fails verification is reported and ignored; the
last good one stays in force. Not in v1, but the hook (verify-before-inject)
should be there from the start so it's a function to fill in, not a
restructuring.

### Bootstrap is the existing push pattern, once

`Self.uploadSelf`, then `ssh host bin run serve --follow <registry> --label …`
under a `Systemd.systemdService` unit (which gives restart-on-crash and
survives reboots). After that the controller **only writes documents** and
never ssh-es again. The push pattern isn't replaced; it's demoted to "day
zero".

## Interaction with the rest of the loop

- **`stopTending` before every command** — honoured unchanged; that's why
  change detection is load-bearing (above).
- **Persistence.** A host that can't reach the store keeps converging on its
  last fetched document — the right behaviour — *but only if that document
  survives a restart*. `World` is an `IORef` today; a puller that forgets on
  restart is worse than no puller (it comes up empty, tears nothing down,
  and looks converged). The fetcher should at minimum cache the last verified
  document on disk and replay it on start; the journal for `World` proper is
  its own item and should land first or alongside.
- **`supervise off`/`autoconverge off` typed interactively** should be
  respected by the fetcher — it must not silently re-enable either. The diff
  batch reads the current setting and restores it.
- **Rewrites** (`Salmon.Op.Rewrite`) already run once per convergence pass
  over the whole ledger; a document-driven batch is exactly one pass, so
  batching works as it does today with no change.
- **Two operators.** An interactive `up` for a seed the document doesn't
  mention is left alone by the diff (it only retires seeds it previously
  declared — the fetcher owns a *contribution* in the ledger's sense, and
  retires only its own). This is the ledger's set-not-refcount semantics
  doing its job.

## What this does not solve

- **Liveness / consensus.** Nothing decides a host is dead, same as today
  and deliberately (see `SreBox.PostgresPair`'s reasoning). A host that has
  stopped pushing status is *stale in the sink*, which is a visible fact, not
  a decision.
- **The store's availability** is one shared dependency. That is a storage
  problem (pick a durable store), not a control-plane problem.
- **Mid-pass arrivals.** A document change that arrives while the loop is
  idle is handled by the tending loop like any interactive command, not
  replayed deterministically. Correct, but `status` should report `mode:
  following` vs `mode: replay` so a test or an operator knows which
  guarantees apply.
- **Secrets in documents.** The document names seeds; seeds that need secret
  material should keep using pre-provisioned files (see the recipe
  key-exchange-agnostic convention), not inline them.

## Non-goals (v1)

- A server that *pushes* to hosts (that's the generic-server sketch, and a
  socket per host).
- Rollout orchestration (wait for A before B) beyond what labels + registry
  writes give.
- Any selector or query language: a label is an address, nothing more.
- Signing (hook only).
- A label file re-read at runtime (labels are start-time flags in v1).

## Decisions taken

- **JSON documents**, not seed lines, for tool support and for something a
  signature and a digest can be computed over; seed-line spelling survives
  as a word array inside them.
- **The fetcher is a first-class actor in `history`**, with registry, label,
  document id and digest.
- **A scheduler decouples rounds from inbound events**, with exponential
  backoff (base, factor, cap, jitter) toward the registry and a debounce
  window (plus `max_wait`) toward the loop — the first protects the
  registry, the second the controlled system.
- **Labels address documents in a registry**; the registry backend owns the
  template that turns a label into an address (a directory, a git
  subdirectory, an HTTP path, a bucket prefix, or a DNS name under a zone).

## Open questions

- DNS-index record format: one `TXT` with `url=` and `sha256=` as sketched,
  or a `URI` record plus a `TXT` digest? And whether the inline-document
  variant is worth having at all.
- Does the document `id` need to be ordered (so a host can refuse to move
  *backwards* if a registry serves a stale copy from a lagging replica), or
  is "latest is whatever the registry says" enough? Leaning: an optional
  `published` timestamp, refuse-older as a flag, off by default.
- Whether `debounce` should also apply to the *first* fetch at startup
  (probably not: startup wants the deterministic synchronous fetch, and
  there is nothing to coalesce yet).

## Suggested milestones

1. **Reader generalization.** `serveWith` takes a list of line producers
   instead of one `Handle`; stdin is one producer. No behaviour change;
   `Test.ServeSpec` still passes untouched.
2. **Document format + directory registry.** The JSON shape, `--follow
   <dir> --label <l>`, one document per label, union across labels,
   mtime+hash change detection, diff-batch injection, fetcher-owned
   contribution, fetcher origin in `history`. Layer 1 test: write the file,
   assert the world; rewrite identical content, assert *no* command was
   injected (the starvation rule, as a test).
3. **Scheduler.** Backoff with jitter toward the registry, debounce with
   `max_wait` toward the loop, the `fetch` command. Test: three writes
   inside the window yield one pass; a failing registry is polled on the
   ladder, not the base.
4. **Cached last document + `mode` in `status`.**
5. **Status sink** (file first), and a `salmon-fleet status` that folds a
   directory of them.
6. **Git registry**, then HTTP, then the DNS index over HTTP, then bucket.
   Verify-before-inject hook with a no-op verifier.
