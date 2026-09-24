# A generic salmon server: `serve` behind an API, with clients that show the DAG

Status: milestones 1 to 8 below are implemented (`--json` via
`Salmon.Reporter.Tagged`; `run serve --listen` via `Salmon.Actions.Serve.Socket`;
`run serve --http` via `Salmon.Actions.Serve.Http` with `/dag`, `/status`,
`/history`, `/help/seed`, `POST /command[?async]`; `GET /events` via
`Salmon.Actions.Serve.Events` and `--events-ring`; `mode` on `status`/`/dag`;
`salmon-tui` over `Salmon.Client.Http`/`Salmon.Client.Model`; the web UI under
`salmon-ops/ui/` served at `GET /`; and `--http-tcp` with `--tls-cert`,
`--tls-key`, `--token-file`), each with its deviations recorded in place in
the milestone list. Milestone 8's closing "Not done: the clients" has since
been done: `salmon-tui https://HOST:PORT --token-file FILE [--cacert FILE]`,
and a browser signs in at `/auth` for a session cookie (`--session-lifetime`,
`--session-idle`, sign-out at `/auth/logout`). Still open: the web UI's
"Not yet" items under milestone 7 (a `Conflicting` pair side by side, batch
and `RemoteOp` expansion, `history` as a timeline), client certificates and
a read-only token. Kept as the design record. Companion to `specs/pull-mode.md` (which is about a host
*fetching* its declarations); this one is about *talking to* a running
`serve` — a web UI, a terminal UI, tooling — and about finally showing the
DAG as what it is while it is being converged and tended.

## Problem

`run serve`'s entire surface is one `Handle` in and a text `Reporter` out
(`salmon-ops/src/Salmon/Actions/Serve.hs`, `serveWith`; the reader thread
`hGetLine`s into a `TChan`, `loop` reads it). Everything else follows from
that:

- **One operator, one terminal.** No second client can attach, no tool can
  drive a running supervisor, and there is no auth because there is nothing
  to authenticate to.
- **A remote `serve` cannot be addressed.** `Self.callSelf` only ever runs a
  one-shot `run up` over ssh. `ssh host bin run serve` works, but the only
  thing that can then speak to it is that ssh session's stdin.
- **The DAG is invisible while it runs.** `run tree`/`run dag` print a static
  picture *before* anything happens (`Help.printDagTree`,
  `Dot.printDagCograph`); `status` prints a flat list *after*. The one thing
  salmon is best at — a graph with per-node state, failure containment and
  `Conflicting`/`Blocked` distinctions — is never rendered as a graph while
  a pass or the tending loop is acting on it.

The author of the line protocol is the first to lament it. This sketch is
the replacement.

## What already exists as a value

None of this needs a new engine; it needs a transport and a read model over
values the loop already maintains.

- **`World`** (`Serve.hs`): the ledger, the magma (one representative per
  `Ref`), and a `NodeState` per node — `nodeShorthand`, `nodeHelp`,
  `nodeDirection`, `nodeConvergence`
  (`Pending`/`Stale`/`Converged`/`Errored`/`Blocked`) and, since (R3),
  `nodeStatus`: the node's own last `CheckResult`, when it last did anything
  observable, and its output ring.
- **`Dag`** (`Salmon.Op.Dag`): `dagNodes`, `dagDependencies`,
  `dagDependants`, `dagOrder`, `dagConflicts` — already the collapsed,
  rewrite-applied structure `run tree`/`run dag` print, with `membersOf` for
  a rewrite-introduced node standing in for declared ones.
- **Three report streams**, all plain sum types: `Serve.Report`
  (`Started`, `BadCommand`, `Loading`, ... the loop's own events),
  `UpDown.Report` (`Skip`/`Eval`/`Done`/`Failed`/`Blocked`/`Conflicting`/
  `Instructed`/...) and `Upkeep.Report` (`Acted`, `Upkeep`/`Downkeep` state
  changes, `NextLook`, `Wedged`/`Unwedged`, demotions). `Reporter` is
  contravariant and already has `encodeJSON`; what is missing is `ToJSON`
  instances on these types (most carry an `Act ext`, which needs a
  serializable projection — shorthand, ref, help, notes — not the closures).
- **Selectors**: `parseSelection`/`resolveWorldSelectors` resolve
  `--select`/`--exclude` path globs and `#ref` prefixes against the live
  world; `status` prints the paths a pattern can match.
- **`RemoteOp`** (`CommandLine.hs`): the dynamic that lets `run dag` draw a
  remote call's subgraph locally (`injectRemoteSubgraphs`). A live view can
  use the same thing to expand a remote node.
- **Mailbox instructions** (`Salmon.Op.Mailbox.Instruction`:
  `Force`/`Satisfy`/`Recheck`/`Pause`/`Resume`) and the `force`/`recheck`/
  `pause`/`resume` commands that queue them.

## Design

### The server is generic because the protocol never interprets a seed

Every salmon binary is a different program (`ParseRecord seed`, its own
`Configure`, its own `Track' directive`), so a server bolted onto `serve`
has to be binary-agnostic to be worth writing once. It is, as long as the
protocol carries seeds as **opaque word lists** — exactly what the line
parser already takes after `up`/`only`/`down` — and speaks about nodes only
in terms of `Ref`, path, shorthand, help, notes and state. A client that
speaks that drives any salmon binary; it never knows what `pgpair
primary=db1` means.

The one place this leaks: composing a seed. The server exposes each binary's
own `--help` text for `config` (it is `optparse-generic`'s, and already
exists), and optionally a JSON schema derived from the `ParseRecord`
instance, so a UI can offer a form instead of a free-text line. That is the
only non-generic surface and it is bounded.

### Three surfaces, one inbox

The loop stays as it is: a single consumer reading lines off one `TChan`,
calling `stopTending` before each. The server is another *producer* into
that channel (the same generalization `specs/pull-mode.md` milestone 1
asks for), plus a *reader* of `World` and the report streams. Concretely:

1. **Commands** — `POST /command` with `{"line": "up pgpair primary=db1"}`
   or a structured form `{"verb":"up","seed":[...]}`; the server writes the
   line to the inbox. **Both a sync and an async mode**: sync (the default)
   blocks and returns the reports that command produced, since the loop
   already knows when a command's reports end (`step` returns) — what
   `curl` and a CI step want; `?async` returns immediately with the
   sequence number at which the command was enqueued, and the client reads
   its reports off `/events` from there — what a UI wants. The existing
   grammar is the API, unchanged. `stdin` keeps working alongside.
2. **Reads** — `GET /dag` returns the computed `Dag` as JSON: one object per
   `Ref` with `ref`, `short`, `shorthand`, `help`, `notes`, `direction`,
   `convergence`, `check` (last `CheckResult`), `output` (tail), `paths`
   (the declared positions a selector can match), `members` (for a
   rewrite-introduced node), `remote` (a nested `Dag`, from `RemoteOp`), and
   `dependencies`/`dependants` as ref lists. It is built by **reading the
   magma** — `worldDag` from `worldMagma` plus the ledger's precedence,
   exactly as a convergence pass builds its own walkable structure — so it
   exists the moment anything has been *declared*, whether or not a pass
   has run yet (`autoconverge off` with declarations recorded is the
   ordinary case: every node `Pending`, edges in place). Plus `GET /status`,
   `GET /history`, `GET /help/seed`. Reads never touch the inbox and never
   stop tending: they read the `IORef World` and the tending snapshot.
3. **Events** — `GET /events`, server-sent events (one connection, text,
   proxies and `curl` understand it; WebSocket adds nothing here): **one
   stream**, every `Serve.Report`, `UpDown.Report` and `Upkeep.Report` as
   JSON, each tagged with its kind and the `Ref` it concerns and a monotonic
   sequence number so a client that reconnects can ask `?since=N`. The
   server is one more `Reporter` — the codebase's contravariant reporter
   type (`Salmon.Reporter`; the *tracer* word is taken by the ops
   themselves) — composed with `reportBoth` beside the text reporter the
   binary already has, `contramap`ped from each report type into one tagged
   sum. Nothing in the loop learns that a server exists; a client filters.
   This is the stream a live DAG view animates from.

Transport, in order: **a unix socket** carrying the *line protocol* first —
`serveWith` already takes any `Handle`, so this is nearly free, it makes a
remote `serve` addressable through `ssh -L`, and it lets a second client
attach today; then **HTTP on that socket or a TCP port** with the three
surfaces above; TLS and auth when it listens on anything but localhost or a
unix socket (see below).

### Reads and the tending loop

`stopTending` before every command exists because a command is about to
*act*. A read is not, so reads bypass the inbox and never stand the machines
down — which is exactly the property the loop's (R3) snapshot design was
built for: `nodeStatus` is snapshotted by `stopTending` and re-snapshotted
on the next, so a read sees a snapshot that is at most one command old. For
a *live* view that is not enough; the event stream is what carries the
between-commands changes (`Upkeep.Report`s are emitted by the running
machines, not by the loop). So: `GET /dag` for the picture, `/events` for
the motion, and a client rebuilds the current state as `dag ⊕ events since
the dag's sequence number`. The server records the sequence number at which
each snapshot was taken and returns it with the snapshot, so the client
knows where to resume.

### Serialization: what an `Act ext` becomes

`Act ext` holds closures (`up`/`check`/`down`/`managed`) and `Dynamic`s. The
wire projection is exactly the fields `Dag.sameRepresentative` compares —
`shorthand`, `help`, `notes`, the rendering of `dynamics` (with
`Supervision` by value, as `Dag.showDynamic` already does) — plus the `Ref`.
That is not a coincidence: it is the set of things that *are* comparable,
and a UI that shows exactly those is showing what `serve` itself can see.
`RemoteOp` is the one dynamic that gets special treatment (expanded to a
nested `Dag`), same as `run dag` does today.

### Modes, so a client knows which guarantees apply

`status` and `/dag` report the loop's mode: `replay` (a piped script or
startup replay — every line queued, never supervised, deterministic),
`interactive` (tending between commands), and, once pull mode exists,
`following` (documents arriving on the scheduler). A web client showing a
node as "supervised" while the loop is replaying a script would be lying.

### Clients

- **Web UI.** Renders `/dag` as a graph (a layered DAG layout — `dagre` or
  ELK — not force-directed; dependency direction *is* the information),
  colours nodes by `convergence`, overlays `check`, animates
  `Eval`/`Done`/`Failed`/`Blocked` as they arrive on `/events`, shows a
  `Conflicting` pair side by side, collapses a rewrite-introduced batch to
  its members on click, expands a `RemoteOp` node into its subgraph, and
  turns a click on a node into `force`/`recheck`/`pause`/`resume` with the
  `#ref` selector the server already accepts. `history` is a timeline. A
  `config` form built from `/help/seed` composes an `up`. `Dot` output stays
  as the export.
- **Terminal UI.** Same API, same read model, for the box with no browser:
  a tree view (the `run tree` shape) with live state, a report pane tailing
  `/events`, and a command line that is the existing grammar. This should
  be a *client of the socket*, not a mode of `serve`, so it works against
  a remote host over `ssh -L` unchanged.
- **Tooling.** `curl`, `jq`, a CI step asserting every node is `Converged`
  before proceeding; a `salmon-fleet status` that folds `/dag` from N hosts
  (or from the pull-mode status sink, which should emit the same JSON).

The web UI is a separate package (`salmon-web`, PureScript is already in
the tree via `Spago`/`purescript-bridge`; the bridge can generate the
client types from the Haskell ones, which is exactly what it is for). The
server itself lives in `salmon-ops` next to `Serve.hs` if it can stay light
(a small WAI app), else in its own package so `salmon-ops` does not grow a
`warp` dependency for every binary that never listens.

### Security

A unix socket inherits filesystem permissions and needs nothing else. For
TCP: TLS (the `Certificates` nodes can mint the cert; this is what they are
for), and a bearer token or client certificate — the tree already has JWT
signing (`SreBox.JWTSigning`). Commands and reads are the same privilege in
v1 (an `up` is as sensitive as reading the output ring of a node holding a
pgbouncer userlist); a read-only token is an obvious v2. **Default to
localhost or a unix socket; never listen on `0.0.0.0` without TLS and auth
configured** — a salmon server *is* root on the box, one `up` away.

**`notes`, `help`, output rings and report text are public** — the
convention already in force, since all of them end up in logs (this is why
`Filesystem.checkFileContents` names files and never quotes them, and why
`filecontents` puts a *fingerprint* in `notes`, never content). The server
inherits that convention rather than adding a redaction layer: it ships
exactly what the text reporter would print. A real story for sensitive data
(what a node may put where, and what a reporter may emit) is needed and is
**another spec**, not this one; until it exists, node authors should assume
anything they write into a report or an `Extension` text field is readable
by anyone who can read the logs.

## Interaction with pull mode

The two sketches meet at the inbox and at the status JSON. Pull mode adds a
producer (the scheduler) and a consumer of `/dag`-shaped JSON (the status
sink). A fleet view is then either a `salmon-fleet` that folds sink objects
(no server needed on any host) or a web UI that connects to N hosts'
`/events` (a server on every host). Both should work; the former is the
cheap one and lands first.

## Non-goals (v1)

- Multi-user access control, audit trails, read-only roles.
- A server that manages *other* hosts (that is pull mode plus a fleet
  fold; this server speaks for one `World`).
- Editing seeds or directives *in* the UI beyond composing an `up` line.
- WebSocket, gRPC, or any second wire format: SSE + JSON.
- Rendering the *declared* graph (the `query` tree with `Connect`/`Overlay`
  colouring); the live view is the computed `Dag`, the same as `run tree`/
  `run dag` since (R4). `query`'s tree stays a CLI concern.

## Decisions taken

- **`POST /command` has both modes**: sync by default, returning the
  command's reports; `?async` returning the enqueue sequence number for a
  client that reads `/events`.
- **`/dag` reads the magma.** It is `worldDag` over `worldMagma` and the
  ledger, the same structure a pass walks, and exists from the first
  declaration on — no pass required.
- **`notes` and every other report text are public.** No redaction in this
  server; a sensitive-data story is a separate spec.
- **One tagged event stream**, produced by composing a server `Reporter`
  beside the existing text one (`Salmon.Reporter`'s contravariant
  combinators), `contramap`ped into one sum. Clients filter.

## Open questions

- Sequence numbers: one counter for the whole loop, or per report kind? One
  (a client resuming wants a single cursor), but `Upkeep` reports are
  emitted from machine threads while `Serve`/`UpDown` reports come from the
  loop, so the counter has to be taken under the same `MVar` the concurrent
  driver already serialises `runReporter` through. *Answered in milestone 4:*
  one counter; the critical section is the numbering reporter's own STM
  transaction, which the drivers' (several, local) `MVar`s compose over.
- Whether `/dag` should include nodes only *retiring* declarations still
  describe (wanted `TurnDown`, not yet down). Yes, with `direction: down` —
  a teardown in progress is the most useful thing to watch — but the UI
  needs to draw them differently from live ones.

## Suggested milestones

1. **`ToJSON` for the three report streams and a `--json` reporter flag** on
   every shipped binary. No server yet; `run up | jq` works; every later
   client reuses the encoding. Test: golden JSON for each constructor.
2. **Unix socket carrying the line protocol** (`run serve --listen
   <path>`), as a second producer into the inbox, stdin unchanged. Test:
   two clients, interleaved commands, reports go to the client that typed
   them. **Shipped** (`Salmon.Actions.Serve.Socket`, `Test/ServeSocketSpec.hs`).
   Two deviations: "stdin unchanged" holds for what stdin *accepts*, not for
   what its end of input does — under `--listen` stdin EOF is a hang-up and
   only `quit` ends the loop, since a server started `< /dev/null &` must not
   exit at once; and a `HungUp` report was added to `Serve.Report`, because
   "every line this client typed has been handled" is a fact only the loop
   knows and the socket needs it to close the connection at the right moment.
3. **`/dag`, `/status`, `/history`, `/help/seed` as JSON** over HTTP on the
   socket. The `Act` projection lands here. Test: `/dag` equals what
   `Help.printDagTree` would print, structurally. **Shipped**
   (`Salmon.Actions.Serve.Http`, `run serve --http PATH`,
   `Test/ServeHttpSpec.hs`), with `POST /command` in both modes. Two
   deviations: it is a *second* unix socket beside `--listen`'s rather than
   HTTP detected on the same one (the line protocol reads through a
   `Handle` that cannot give peeked bytes back, so sharing meant rewriting
   both over raw sockets plus a warp `Internal` shim, for the price of one
   flag); and `/dag` is `worldDag` unrewritten — no `members`, no `remote`,
   no `output`/`paths` beyond what `status` already carries — since the
   loop's registered `Rewrite`s run per pass and a rewrite-introduced node
   has no `NodeState` to project. `/history` folds `history-elided`'s count
   in as an `elided` field rather than answering with two objects.
4. **`/events` (SSE) with sequence numbers and `?since=`.** Test: a client
   that reconnects mid-pass misses nothing. **Shipped**
   (`Salmon.Actions.Serve.Events`, `GET /events` in `Salmon.Actions.Serve.Http`,
   `--events-ring N`, `Test/ServeEventsSpec.hs`). The open question on
   sequence numbers is answered: one counter, and the critical section is
   an STM transaction owned by the numbering reporter (counter, ring and
   broadcast written together), not the concurrent driver's `MVar` — there
   is no single such `MVar` to take (one per walk, one per supervisor), but
   each is held while `runReporter` runs, so the transaction composes under
   all of them. Three deviations: every `POST /command` publishes an
   `enqueued` event numbered from the same counter (so the numbering is
   dense and the `?async` number is an event a client can see); a `Tended`
   report is delivered as its inner `Upkeep.Report` under `stream: "upkeep"`
   rather than as `{"kind":"tended"}`; and `?stream=`/`?origin=` filter
   server-side after all, since a terminal client over a slow link wants
   less on the wire. `/dag` and `/status` carry `seq`, read before the
   snapshot so a race replays rather than skips.
5. **`mode` in `status`/`/dag`.** **Shipped**: `status` and `/status`
   with `specs/pull-mode.md` milestone 4 (`Serve.Mode` on `StatusReport`),
   `/dag` as a top-level `mode` on the envelope, read from the server's
   accessor at the moment of the request; one `ToJSON Serve.Mode` in
   `Salmon.Reporter.Tagged` serves both. `/help/seed` does not carry it.
6. **Terminal client** against the socket. **Shipped** (`salmon-tui PATH` in
   `salmon-apps`, over `Salmon.Client.Http` and the pure `Salmon.Client.Model`
   in `salmon-ops`, `Test/ClientModelSpec.hs`): `/dag` once, `/events` from
   its `seq`, a node table with direction, state, last check and last
   event, `enter` for a node's help/notes/output, `:` for a command sent
   `?async` with its seq echoed, `r` to re-read, reconnect with `?since=`,
   re-read on `gap`. Three deviations from the "Clients" section. It is a
   *table* in `/dag`'s order (the `Dag`'s first-seen order, what `run tree`
   prints), not a tree view: with the edges in both directions on every
   node and a node appearing once whatever number of paths reach it, a tree
   would repeat nodes and a table with an expand does not, so the tree
   waits for the web UI's layered layout. There is no separate report pane
   tailing `/events`: the last event is one footer line and each node's row
   carries the last event about it, which is what a pane tailing the stream
   would mostly be showing; a scrollback of events is a place the client
   would hold state the server does not. And the model drops a replayed
   event *per stamp* (a node's own seq, the loop's own seq) rather than by
   one cursor, because a `/dag` snapshot carries the nodes' state and not
   the pass's, so a re-read after `declared` would otherwise swallow the
   `converge-stop` of a pass the client had already shown starting. Two
   things the spec did not say that the client needed: a snapshot must be
   *rebased* onto a folding model (`Model.rebase`), and `declared` must
   trigger a re-read, since an event names nodes by ref and no event
   describes a node the client has never seen.
7. **Web UI**: static graph from `/dag`, then live from `/events`, then
   actions, then the seed form. **Shipped, all four steps** (`GET /` and
   `/ui/*` in `Salmon.Actions.Serve.Http`, the files under `salmon-ops/ui/`
   embedded at build time with `file-embed`; `docs/serve-supervision.md`
   §14 "The web UI"). Three deviations from the sketch above. It is not a
   separate `salmon-web` package and not PureScript: three static files —
   one page, one ES module, one stylesheet, no bundler — served by the
   binary itself, since a UI that ships inside the thing it watches needs
   no deploy step and the read model is small enough that generated client
   types would cost more than they save; the bridge stays an option for the
   seed form. The layered layout is neither `dagre` nor ELK but a
   longest-path layering with barycentre ordering written in `ui.js`,
   because vendoring a bundle for a graph of tens of nodes is the wrong
   trade and the layout is a hundred lines. And a browser cannot open a
   unix socket, so the page is reached through a TCP forward (`socat`,
   `ssh -L`) until milestone 8, which added `--http-tcp` and the `/auth`
   sign-in, so a browser now reaches it directly. The
   actions and the seed form add three more deviations, all on the write
   side. Every write is `POST /command?async` and never the synchronous
   form — the page reads the outcome off `/events` by the request's
   origin, which is what marks the nodes a command touched and fills the
   log under its command line; the sketch's "what a UI wants" turned out
   to be the whole of it. The seed form is `/help/seed`'s text in a `<pre>`
   and a free-text field for the words, not a form derived from the
   `ParseRecord` — no schema is served, and the bridge stays unused. And
   "retire the seed behind this node" is not a per-node action: a node
   does not know its declaring seed and `/history` does not list an
   epoch's nodes, so the panel offers every live declaration's `down`
   instead and the operator picks. Also deliberate: no `quit` on the page
   (the page is served by the process it would stop), and no bearer token
   sent: over `--http-tcp` the browser signs in at `/auth` and carries a
   session cookie instead (milestone 8). Not yet: the
   `Conflicting` pair side by side, collapsing a batch to its members and
   expanding a `RemoteOp` (neither is on `/dag`, see milestone 3's
   deviations), and `history` as a timeline — it is a table under the
   seed form.
8. **TCP + TLS + token**, opt-in, with the loud default described above.
   **Shipped** (`run serve --http-tcp HOST:PORT --tls-cert FILE --tls-key
   FILE --token-file FILE`; `Http.withHttpServerOn`/`Http.Bind`/
   `Http.requireToken`, `CommandLine.validateTcpOptions`,
   `Test/ServeTlsSpec.hs`, `docs/serve-supervision.md` §14). The same
   `application` on a warp-tls listener beside the unix socket, one
   `Server` for both; a bearer token on every route of the TCP listener,
   compared in constant time; the unix socket unchanged and token-free.
   The loud default is stricter than the text above: not "never on
   `0.0.0.0` without TLS and auth" but never on *any* address without
   both — there is no plaintext TCP constructor or flag, `--http-tcp`
   without all three files exits 1 naming the missing ones, and the host
   is always spelled (`:8443` is refused, `0.0.0.0:8443` is how listening
   everywhere is written), so "default to localhost" is not a default but
   a choice the operator types. Four deviations. A bearer token from a
   file, not a JWT: `SreBox.JWTSigning` signs for *other* services and a
   verifier here would need a key store, an audience and a clock for what
   a `chmod 600` file already gives; and no client certificate, which is
   the v2 the text names. The token file must not be readable by others
   and must not be empty — two refusals the text did not ask for. Commands
   typed over TCP are attributed to the client's `ADDR:PORT#n`, not to the
   listener, so `history` says who. And the `Certificates` nodes *can*
   mint the certificate, as the text says, with one caveat found on the
   way: `selfSign`/`caSign` (`openssl x509 -req`) write X.509 v1
   certificates, which crypton's validation rejects (`LeafNotV3`) while
   OpenSSL-based clients accept; `certificateAuthority` (`req -x509`)
   writes v3, and is what the test pins. Not done: the clients —
   `salmon-tui` and the web UI need a `--token` and a TCP address to use
   this.
