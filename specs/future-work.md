# Future work: what a running world knows, and who gets to see it

Status: ideas, not a plan. Four notes taken after trying `run serve` with the
HTTP surface, the web UI and `salmon-tui` on `salmon-gcp-toy` (PR #8, #9),
each elaborated to the point where the next spec could start. They share one
theme: the loop now *has* a lot of knowledge about each node — what it found,
what it last said, where its effect lives, what a remote copy of it is doing —
and almost none of it is addressable from outside.

## 1. Facts a node discovers

**The itch.** `salmon-gcp-toy` tier 2 takes two passes because GCP picks the
address: the first pass reserves it, the driver reads it back with
`Compute.readAddress`, and feeds it in as `--vm-ip` for the second
(`docs/gcp-toy-validation.md`). The same shape recurs whenever `up` learns
something the declaration could not say: a project number, a Cloud Run
service URL, a bucket's generated name, a generated password's fingerprint, a
container's assigned port. Today that value lives, at best, in a node's output
ring (`NodeState.nodeStatus`, snapshotted by `stopTending`) or in prose in
`notes`, and the operator re-derives it with `gcloud`.

**The shape.** A node publishes *facts*: a small `Map Text Value` it is
willing to state after `up` or `check`. Two ways to get them out, and the
cheap one first:

- **As reports.** A `Fact !(Act ext) !Text !Value` constructor on
  `UpDown.Report` (or its own small stream in `Tagged`), emitted by `up` and
  `check` through the reporter a node already has. It then reaches everything
  for free: `--json`, the socket clients, `/events`, the status sink, the
  UI's per-node panel. Nothing changes in `Extension`; a node opts in by
  calling a helper.
- **As state.** `NodeState` gains `nodeFacts`, folded from those reports by
  the loop (last writer wins per key), so `/dag` and `status` show a node's
  current facts without replaying events, and a fleet fold can compare
  them across hosts (`salmon-fleet status --fact project-number`).

**Where it bites.** Feeding a fact *back into a declaration* — the
`--vm-ip` case — is the actual two-pass problem, and it is not solved by
either of the above. Options, roughly in order of ambition: a pull-mode
document that names a fact (`{"seed": [..., "--vm-ip", {"fact": "#addr/ip"}]}`,
resolved by the fetcher against the world before injection, so the second
pass is a document the controller can write generically); a `Rewrite` phase
that substitutes facts into dependants at expand time (closest to
`Rewrite.hs`'s "cross-declaration knowledge lives here" rule, but a
declaration whose words depend on a fact has a `Ref` that changes when the
fact does, which `Serve.record` would read as a re-declaration — perhaps
correctly). Start with facts-as-reports; the feedback loop is its own spec.

## 2. Structured, clickable notes

**The itch.** `notes :: [Text]` is prose. A GCP node's most useful note is a
console URL; a Cloud Run service's is its endpoint; a Postgres node's is a
connection string one should *not* paste into a log. The web UI renders notes
as text (`textContent`, deliberately: notes are untrusted), the TUI prints
them, and neither can offer a link.

**The shape.** Beside `notes`, an `Extension.links :: [Link]` with
`Link { linkLabel :: Text, linkKind :: LinkKind, linkHref :: Text }` and
`LinkKind = Console | Endpoint | Logs | Doc | Other Text`. Structured rather
than "notes that look like URLs" so the UI can render an anchor with a label,
the TUI can print `[console] https://...`, `/dag` can carry them typed, and a
fleet fold can list every host's endpoints. The builtins that know a URL
attach it: `Gcp.*` (console pages per resource kind), `CloudRun` (service
URL), `Storage` (bucket URL), `Systemd` (`journalctl -u` as a `Logs` link the
TUI can run), `Qemu` (the serial console socket).

**Where it bites.** `sameRepresentative` compares `notes`; whether `links`
count as identity (a changed console URL is not a changed effect) needs a
decision — probably not compared, like `dynamics` payloads other than
`Supervision`. And links are where secrets creep in (a presigned URL, a
connstring with a password): the sensitive-data story the generic-server
spec defers is a prerequisite for `Endpoint` links at least, or links must be
declared public by construction like notes are today.

## 3. A node's latest messages

**The itch.** After an `up`, the UI panel and the TUI say "never tended" or
show a stale check, because `/dag`'s per-node `status` is the snapshot
`stopTending` last filed (R3 in `Actions/Serve.hs`), and the tending machine's
live output ring is behind a `TVar` nobody outside `Upkeep` can reach. The
events ring has everything a node ever said, but only as one stream to scan.

**The shape.** Two reads, both cheap because the data already exists:

- `GET /node/<ref>`: the node's `/dag` projection plus its last *n* events
  from `Events`'s ring filtered by `ref` (the ring is a `Data.Sequence` of
  tagged objects; an index `Map Ref (Seq seq)` maintained at `publish` makes
  this O(n) in the answer, not the ring). `?since=` for a client that wants
  only what is new. The UI panel and the TUI's `enter` view get their "last
  events" from here instead of from what they happened to see live.
- **Live output ring.** `Upkeep.Supervisor` exposes a read of each machine's
  current `Status` (check verdict, reason, output ring, `statusEpoch`) and
  `serveObserved`'s accessor includes it, so `/node/<ref>` and `/dag` report
  the machine's *current* status when one is running and the snapshot only
  when none is. That is the freshness gap D1 and D2 both hit ("check reads
  `-` until a `next-look` arrives").

`salmon-tui` and the web UI are the consumers; `status --select` on the
socket could print the same thing in text.

## 4. Remote ops as a DAG with live events

**The itch.** `Self.callSelf` runs `ssh host bin run up` with a directive on
stdin and streams the remote's *text* output into the local node's output
ring. `run dag` can draw a remote subgraph through `injectRemoteSubgraphs`
and the `RemoteOp` dynamic, but `/dag` cannot (B3's finding: the magma has
no path to a remote subgraph), and a remote node's `eval`/`done`/`failed`
never become local events. So the UI shows one box, `remote-call`, going
from pending to converged after minutes, with everything interesting hidden
inside it.

**The shape**, in three steps that each stand alone:

1. **Parse what comes back.** `callSelf` invokes `run up --json` and decodes
   each line with the `Tagged` encoding (the wire format that item A1 made a
   contract), re-emitting every remote report through the local reporter with
   a `host` stamp — a `Remote host inner` constructor on `Tagged`, or a `host`
   field beside `stream`. Text stays available for a remote binary too old to
   speak JSON. The remote's `Fact`s (section 1) ride along unchanged.
2. **Draw the remote subgraph in the magma.** At expand time, a `RemoteOp`'s
   subgraph is folded into the `Dag` under its remote-call node with refs
   prefixed by host (so two hosts running the same recipe do not collide on
   one `Ref`, which `mkRef`'s location-addressing would otherwise make
   happen), marked `remote: host` in the `/dag` projection. The UI draws it
   as a nested box; the events from step 1 land on those nodes by ref.
   `Concurrent`'s failure containment is unaffected: the remote-call node is
   still the one that fails locally.
3. **The fleet DAG.** Once a remote host runs its own `serve --follow` (pull
   mode), the controller no longer *calls* it; it reads the host's `/dag`
   and `/events` (E1's TLS listener, `Salmon.Client.Http` from D1) and
   composes them into one view keyed by host — the aggregation `salmon-fleet
   status` lacks (C4's finding: nothing summarises by label or document id).
   That is the "fleet control" picture from the capabilities assessment, and
   it needs nothing more from the loop: it is a client.

Step 1 is small and immediately visible; step 2 is the one that changes
`Dag.hs`; step 3 is the one that makes pull mode and the server meet.

## Not here

Consensus, and any decision that a host is dead; those stay out on purpose
(`specs/pg-switchover.md`'s argument). The sensitive-data story deferred by
`specs/generic-server.md`, which sections 2 and 4 both lean on.
