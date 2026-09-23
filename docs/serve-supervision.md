# `run serve`: convergence and supervision

This is a companion to [`howto-ops.md`](howto-ops.md): where that doc covers
how to write one `Op`, this one covers what happens to a whole graph of them
once you run them through `run serve` instead of a one-shot `run up`. Read
`howto-ops.md` first if `Op`, `check`, `Track`, or the seed → spec → ops CLI
protocol are unfamiliar — this doc assumes all of them.

It's organized around the question a new user actually has: *what do I get
for nothing, how do I see it happening, and what do I have to write myself to
get more?*

## 1. The mental model in three sentences

`run up`/`run down` are one-shot: they walk a graph once and stop.
`run serve` is long-running: it reads seed declarations from stdin (one per
line — `up <seed args>`, `down <seed args>`, `only <seed args>`, and a
handful of operator commands), keeps a `World` of everything it's been told
to want, and **converges** that `World` after every command. Between
commands, while nothing is waiting on stdin, it also **tends** every node
it's converged — a second, independent mechanism that notices drift and
fixes it (or doesn't, depending on what the node tells it) without anybody
typing anything.

Those are the two halves of "supervision": a **convergence pass** (driven by
you, typing or piping commands) and a **tending loop** (driven by idle time).
Almost everything below is about what each one does for a node that says
nothing about itself, and what a node can say to get more out of either.

## 2. What you get for free

Take any existing salmon binary — built the ordinary way, via
`Salmon.Builtin.CommandLine.execCommandOrSeed`, with plain `Op`s that have no
`check`, no `Supervision` dynamic, no `managed` action — and point it at
`run serve` instead of `run up`. You get, with zero changes to your nodes:

- **Convergence bookkeeping across re-declarations.** Nodes unify by `Ref`
  across every seed that mentions them. Re-declaring an unchanged seed is a
  no-op (nothing pending, nothing re-run). Retiring a seed tears down exactly
  the nodes no other live seed still wants — a directory two files share
  survives until the last file using it is gone, never torn down out from
  under the other.
- **`status`/`history`/`query` introspection.** `status` lists every node
  this world still cares about, its wanted direction (up/down), and its
  convergence (`Pending`/`Stale`/`Converged`/`Errored`/`Blocked`) — plus, if
  the node has ever been tended, its own last `check` verdict and (for a
  failing node) the tail of its output, and (underneath) every path a
  currently-active seed's graph reaches it at — the exact text a
  `--select`/`--exclude` pattern matches, pasteable straight back in.
  Without this a pattern could only be *guessed*; `status` is where it comes
  from. `history` lists what was declared, when, and by whom — a typed line,
  a `load`ed file, or (§12) a fetched document. `query` annotates nodes
  `[selected]`/`[excluded]` against a `--select`/`--exclude` pattern without
  acting on anything — useful for checking a pattern before you `force`/
  `pause` with it for real.
- **Re-declaring with *different* content is noticed by the pass itself**,
  not just eventually by a background loop — as long as something about the
  declaration that changed is visible to `Salmon.Op.Dag.sameRepresentative`
  (shorthand, `help`, `notes`, most `dynamics`). A node goes `Stale` rather
  than staying silently `Converged`, and the very next convergence pass
  re-checks it. (§8 below is about making your own content-bearing nodes
  participate in this — the stock `Filesystem.filecontents` already does.)
- **`force`/`recheck`/`pause`/`resume`**, addressable by `--select`/`--exclude`
  path globs, reach any node regardless of whether it has a `check`:
  - `force` re-applies a node even if its own `check` currently calls it
    satisfied — the only way to tell a healthy-looking node "do it anyway".
  - `recheck` makes a tended node look *now* instead of waiting out its
    current delay.
  - `pause`/`resume` stop/restart tending a node without touching whatever
    effect it currently holds.
- **A managed node (one that owns a running process, see §7) is supervised
  with a sane default the moment it exists** — restarted `OnFailure`,
  `OneForOne` (demotes nobody else), kept running across every other `serve`
  command, even with a completely default `Supervision`. You don't have to
  opt in to get *some* policy; you only have to opt in to get a *different*
  one.
- **Idle cost is proportional to what a node claims.** A node with no
  `check` answers `Immaterial` ("asking would cost what applying costs") and
  is *parked* — looked at once, then left alone on its mailbox until
  something (a dependency moving, an operator's `force`/`recheck`) wakes it.
  It is not polled every minute doing nothing useful. This means an
  undecorated graph costs almost nothing to tend, but it also means it is
  not self-healing — see the gap this leaves in §4.
- **A piped script stays deterministic.** `run serve < script.txt` queues
  every line before the first pass can even finish, so there is never an
  idle moment for the tending loop to run in — what you get is exactly the
  sequence of convergence passes the script describes, nothing more. This is
  what makes `run serve` usable for CI/scripted setups, not only interactive
  sessions. Reach for `supervise off` if you want the same determinism in an
  interactive session, and `supervise on` (the default) to get it back.
- **Declaring and converging can be split apart.** By default every `up`/
  `only`/`down`/`clear` converges immediately, as if `converge` had been
  typed right after it. `autoconverge off` turns that off: declarations
  still record and are visible to `status`/`query` right away, but nothing
  is applied until you type an explicit `converge` (optionally restricted
  with `--select`/`--exclude`). Useful for stacking up several declarations
  — `up a`, `down b`, `up c` — and reviewing the combined result with
  `query`/`status` before anything actually runs. `run serve
  --no-autoconverge` starts a session already in that state, for a script
  or session that always wants to review before acting.

None of this requires writing a single `check` or `Supervision` dynamic.
What it does *not* give you for free: a node whose effect can be perturbed
from outside salmon (a config file hand-edited, a directory `rmdir`'d, a
service that crashes) is not put back unless that node has a `check` that
says so, or is a `managed` process. That's the next section.

## 3. Try it yourself, five minutes

The quickest way to see all of this without writing any code is the
fixture shipped with the repo, `salmon-ops-serve-fixture`
(`salmon-ops/fixtures/ServeFixture.hs` — read its own module haddock for the
full tour). But the same shape works on any salmon binary you already have;
substitute your own seed args for `<seed>` below.

```sh
$ my-salmon run serve
up <seed>
status
```

You should see your nodes listed, `TurnUp`/`Converged`. Now perturb something
underneath it from another terminal — if any of your nodes is a
`Filesystem.filecontents` or a `Filesystem.dir`, edit or delete the file/
directory by hand. Then:

```sh
status
```

If the perturbed node has a `check` (every `filecontents` does, and every
`dir` re-applies on a timer instead — see §5 and §6), it's back, without you
typing anything else in between — the tending loop already noticed and fixed
it while you were looking the other way. If it's a plain node with no
`check`, it stays broken until you `force` it:

```sh
force --select '/**'
```

Now try a re-declaration with different content (if your seed's args feed
into something content-bearing):

```sh
only <same seed, different content argument>
status
```

Watch for a line showing a node `Stale` rather than jumping straight back to
`Converged` with nothing having happened — that's the pass itself noticing
the change (§8), not the tending loop catching it later.

Finally:

```sh
pause --select '/**'
# perturb something again — it should stay broken
resume --select '/**'
# and now it's fixed again
quit
```

`help` at any point prints the full command reference; `help TOPIC` (e.g.
`help select`, `help force`) prints more about one command.

## 4. What's free vs. what needs decoration

| capability | free, zero decoration | needs |
|---|---|---|
| convergence bookkeeping, re-declare/retire semantics | ✅ | — |
| `status`/`history`/`query` | ✅ | — |
| `force`/`recheck`/`pause`/`resume` reachability | ✅ | — |
| discovering a node's `--select`/`--exclude` path | ✅ (`status`/`query` print it) | — |
| disambiguating two nodes that share one path (identical shorthand) | ✅ | a `#ref` selector (§9) |
| `Stale` on a re-declaration that changes `help`/`notes`/most `dynamics` | ✅ | — |
| `Stale` on a re-declaration that only changes content baked into `up` | ❌ | a `check`, or a content-derived `notes`/`dynamics` field (§8) |
| self-healing when an effect is perturbed from outside | ❌ | a `check` (§5) |
| a process salmon owns, restarted when it exits | ❌ (node must declare it) | `managed` (§7) |
| choosing *how* a managed node is restarted, or whether its going away bounces dependants | uses a sane default | a `Supervision` dynamic (§6) |
| a non-`managed` node re-applying itself on a timer instead of parking | ❌ | `supReapply` (§6), narrowly |
| addressing a batch/rewrite-introduced node that has no declared path | ❌ | a `#ref` selector (§9) |
| bounding how many nodes converge at once | ❌ (unbounded by default) | `--max-concurrency N` (§10) |
| reports a script can parse | ❌ (text by default) | `--json` (§11) |
| fetching declarations from a registry instead of typing them | ❌ (stdin only) | `--follow DIR --label L` (§12) |
| a second operator or a tool attached to a running `serve` | ❌ (stdin only by default) | `--listen PATH` (§13) |

## 5. Decorating nodes: `check`

This is the single highest-leverage thing you can add to a node, and it's
covered in full in `howto-ops.md` §4 for the one-shot angle (idempotency).
Here's what each `CheckResult` means specifically to the tending loop, which
is a second, independent reader of the same function:

- **`Immaterial` (the default, for a node with no `check` at all) parks the
  node.** It is looked at once on the way up, then left alone — not polled.
  This is right for a node whose `up` is already idempotent and cheap to ask
  about (`mkdir -p`, `ip route replace`) — there is nothing cheaper to put on
  a timer than what applying costs. It is *not* right for a node whose
  effect can drift without your say-so, because nothing will ever notice.
- **`Success`/`Failure text` are what let a node be supervised at all.** A
  check that can tell the effect is gone (`Failure`) is the only thing that
  can trigger a restart under `Supervision`'s `Restart` policy, or notice a
  re-declared node's new content (§8), or fire a `RestForOne` demotion (§6).
  Without a real check, a node is brought up once and then genuinely nobody
  is watching.
- **`Unknown`** (a check that ran and genuinely couldn't tell — e.g. a
  systemd unit mid-restart) never triggers anything under the tending loop;
  treating "I couldn't look" as evidence of anything would spin a node at
  its delay floor forever. It's different from `Immaterial`: `Unknown` means
  a check exists and sometimes can't say; `Immaterial` means there's no
  check worth having.
- **`Completed`** means the effect ran to completion and stopped on
  purpose — a job, not a service. Converged, not running. Only a `Restart =
  Always` policy (§6) re-applies on this.
- **`Skipped`** is not something your `check` should ever return itself —
  it's what `Query.forceSkip`/a `run up --plan` exclusion rewrites a check
  into, a statement that someone decided this node is satisfied, not a
  statement about the effect.

Worked template, following `Filesystem.checkFileContents`'s shape (compare
what's there against what you'd write, cheaply and without quoting secrets
into the failure text):

```haskell
myCheck :: MyConfig -> IO CheckResult
myCheck cfg = do
    there <- probeTheEffect cfg
    pure $ case there of
        Nothing -> Failure "missing: <identifying text, no secrets>"
        Just actual
            | actual == expected cfg -> Success
            | otherwise -> Failure "drifted: <identifying text>"
```

## 6. Decorating nodes: `Supervision`

`Salmon.Op.Supervision.Supervision` is a handful of optional fields, carried
on a node's `dynamics` (the same channel `Package` uses — see
`howto-ops.md` §7 on `dynamics` if this is unfamiliar):

```haskell
import Salmon.Op.Supervision (defaultSupervision, supervised, Strategy (..), Restart (..), seconds)

op "my-node" nodeps $ \actions -> actions
    { dynamics = [supervised defaultSupervision
        { supRestart = OnFailure       -- Always | OnFailure | Never (default OnFailure)
        , supStrategy = OneForOne      -- OneForOne | RestForOne      (default OneForOne)
        , supReapply = False           -- re-run `up` on a timer instead of parking (default False)
        , supWatchdog = Nothing        -- Maybe Micros: report if silent this long (default Nothing)
        , supStableAfter = seconds 10  -- how long up resets the failure tally (default 10s)
        , supDemoteEvery = seconds 10  -- RestForOne rate limit, see below     (default 10s)
        , supGiveUpAfter = Nothing     -- Maybe Int: stop retrying after N consecutive failures
        }]
    , ...
    }
```

Prefer amending `defaultSupervision` field-by-field, as above, rather than
writing out the constructor positionally — the record has grown several
times already.

A node that says nothing here behaves exactly as if none of this existed —
every field's default is chosen so an undecorated graph is unaffected.
What each buys you, beyond the default:

- **`supRestart = Never`** — for a node whose `up` is destructive to repeat,
  or whose failure means something worse happened upstream that an operator
  should look at rather than salmon silently retrying.
- **`supRestart = Always`** — the "restart a service that exits cleanly on
  reload" case. Don't set this on a job that's meant to run once.
- **`supStrategy = RestForOne`** — *this node's going away should bounce
  whatever depends on it.* The case this exists for is a config file: a
  service reading it should be re-verified (not necessarily restarted — see
  below) whenever the file changes underneath it. Authored on the node that
  goes away, not on its dependants, because only the config file's author
  knows its content is load-bearing.
  - **The safety condition**: this reframes a `RestForOne` bounce as "go
    re-verify yourself," which is cheap *only if the dependant's `up`/
    `check` are genuinely idempotent* — the same baseline convention every
    node in this tree is already supposed to follow. A dependant whose
    reapplication is genuinely expensive (a slow warmup, an unsafe-to-repeat
    migration) has no way today to resist a `RestForOne` demotion sent from
    upstream — don't put `RestForOne` on a node whose dependants might not
    be able to afford being asked to re-verify on every change.
  - `supDemoteEvery` is the rate limit: a node is demoted by a dependency at
    most once per this interval, so a flapping dependency can't rebuild the
    whole cone behind it on every flap. An isolated departure is always
    honoured whenever it comes, however soon.
- **`supGiveUpAfter = Just n`**: stop restarting after `n` *consecutive*
  failures (a service that crashes once a week never latches off, because
  `supStableAfter` forgets the streak once it's run that long between
  crashes). A node that's given up says so in `status`/a `force` starts it
  over; nothing else touches it until you do.
- **`supWatchdog = Just (seconds n)`**: report a node as possibly-wedged if
  it's gone this long without doing anything observable. Reports only —
  nothing kills a wedged `up`, since not every node has a bracket to kill it
  through.
- **`supReapply = True`**: for a node with no `check` at all (answers
  `Immaterial`), re-run `up` on the tending loop's delay ladder instead of
  parking. This is narrow — sound only for an `up` that is genuinely cheap
  *and* idempotent (`Filesystem.dir` is the one builtin that sets it:
  `createDirectoryIfMissing` costs about what asking first would). Don't
  reach for this as a substitute for writing a real `check` on anything
  whose `up` is a build, a clone, or otherwise not free to repeat.

## 7. Decorating nodes: owning a process (`managed`)

A node whose effect *is* a running process — not "create a file," but "keep
this running" — fills in `managed` instead of relying on `up` alone.
`Salmon.Builtin.Nodes.Daemon.daemon` is the builtin for this:

```haskell
import qualified Salmon.Builtin.Nodes.Daemon as Daemon
import System.Process (proc)

myService :: Op
myService = Daemon.daemon reporter $
    Daemon.defaultDaemon "my-service" (proc "/usr/bin/my-service" ["--config", path])
```

Three things that follow from `managed` existing at all, distinct from
everything above:

- **A one-shot `run up` cannot bring this node up at all** — its `up`
  deliberately throws (`NeedsSupervisor`) rather than silently no-op'ing.
  Only `run serve` can hold a running action.
- **`run serve` treats it specially.** It's invisible to the convergence
  pass in both directions (there's nothing for a one-shot up/down to do with
  it); instead its machine races the action itself, reads its `ExitCode`
  against `supRestart`, and is kept running across every other `serve`
  command rather than stood down and restarted on every `status`.
- **Decorate it with `Supervision` exactly as any other node** (§6) — the
  defaults already give you `OnFailure` restart and `OneForOne` (no
  bouncing of dependants). `RestForOne` on the *config file this process
  reads*, not on the process itself, is the combination the fixture's own
  `--daemon` walkthrough demonstrates end to end (`ServeFixture.hs`'s module
  haddock).

## 8. Making your own content-bearing nodes `Stale`-aware

§2 mentioned that a re-declaration with different content is noticed by the
pass, *if* something about it is visible to `Dag.sameRepresentative`
(shorthand/`help`/`notes`/most `dynamics` — deliberately not `up`/`check`,
which are functions and not comparable). `Filesystem.filecontents` already
does this for you: its `EncodeFileContents` instances carry a pure
`contentFingerprint`, and `filecontents` puts it into `notes` when one is
available. If you're writing your own content-bearing node — a hand-rolled
one, or a new `EncodeFileContents` instance — the same trick is available to
you:

```haskell
myConfigNode :: MyConfig -> Op
myConfigNode cfg =
    op "my-config" (deps [enclosingDir]) $ \actions -> actions
        { help = "writes " <> path
        , notes = ["content-hash: " <> hashOf cfg]   -- <-- this line is the whole trick
        , ref = mkRef "my-config" path
        , check = myCheck cfg                         -- still worth having regardless, per §5
        , up = writeTheFile path cfg
        , down = removeFile path
        }
  where
    hashOf = Text.take 12 . Text.decodeUtf8 . Base64.URL.encode . SHA256.hash . encode
```

Without this, a re-declaration that only changes `cfg` is indistinguishable
from one that changes nothing at all — the node stays `Converged`, and
you're relying entirely on `check` (if you wrote one) running on its own
timer to notice, which won't happen at all under a piped script (§2's last
bullet) and may take a while even interactively.

## 9. Addressing nodes precisely: `--select`/`--exclude`

Every command that takes `--select PATTERN`/`--exclude PATTERN` (`status`,
`history`, `query`, `converge`, `force`/`recheck`/`pause`/`resume`) resolves
it as a `/`-separated glob against each node's *declared tree position* —
`*` matches exactly one segment, `**` matches any depth including zero.
Patterns may repeat (union within each of `--select`/`--exclude`); omitting
`--select` entirely means everything. A few examples:

```
--select '/root/web/**'      everything under the web subtree
--select '/**' --exclude '/root/db/**'   everything except the db subtree
```

A path is built from op *kinds* (`shorthand`, e.g. `directory`,
`file-contents`), not from any identifying value a recipe passed in — two
nodes at the same tree position with the same shorthand (say, two files a
recipe declares in a loop) get the exact same path, and a path pattern
necessarily selects both together. `status`'s path line is where you'd
notice this: two nodes printing the identical path is the tell. When that
happens, address one of them directly instead with a `#`-prefixed pattern,
matching by `Ref` rather than by path — the same (short or full) hash text
`status`/`run tree`/`run dag`/`query show` already print next to it:

```
--select '#AbCd1234'    matches by a Ref fragment (short or full)
```

If your binary registers a `Rewrite` (e.g. `Debian.Package.batchPackages`,
which collapses every declared `deb` node into one `apt-get` batch — see
`howto-ops.md`/`CLAUDE.md`'s `Op/Rewrite.hs` section if this is new to you),
the rewrite-introduced node (the batch itself) was never declared and so has
no tree position — and no `status` line — of its own either; a `#`-match
against it expands to every declared node it stands in for, but only for
`run up`/`run down`/`query show`/`query plan`, which see the *computed*,
post-rewrite graph. `serve`'s own `status`/`query`/`converge --select` (this
section, otherwise) resolve against the *declared* graph and so cannot name
a batch this way at all — restrict by the declared nodes that feed it
instead.

## 10. Bounding concurrency

`run serve --max-concurrency N` caps how many nodes are inside their own
`check`/`up`/`down` at once, across one convergence pass. Useful on a
machine where unbounded *width* itself is the problem (CPU/IO contention, an
outbound connection limit) rather than two specific nodes fighting over one
resource — that case is still an edge (a dependency) or a collection's job,
not this flag's. Omit it for the old, unbounded behavior (the default).

## 11. Machine-readable reports: `--json`

Everything above prints text: `serve:`-prefixed lines for the loop itself,
`Show`n `UpDown.Report`s for what each node did. `run serve --json` (and
`run up --json`/`run down --json`, the same flag) replaces all of that with
**one JSON object per line on stdout**, flushed as each report happens, so
`my-salmon run up --json | jq` streams and a script can watch a `serve` for
`converge-stop` without parsing prose. The text output is unchanged when the
flag is absent.

Every object has a `kind` (the report's constructor, kebab-cased:
`declared`, `converge-start`, `done`, `failed`, `wedged`, ...), a `stream`
(`serve` for the loop's own reports, `updown` for what a node did; the
tending loop's reports arrive nested inside `serve`'s `tended`), a `ref`
whenever the report is about one node (`{"short": ..., "full": ...}`, the
same short tag `status`/`query show` print after `#`, so it pastes back in as
a selector), and the node's `shorthand`/`help`/`notes` under `node`. Report
text is public: `notes`, failure messages and a `status`'s output ring go
out verbatim, so keep secrets out of them (see the `filecontents` failure
text for the convention). `Salmon.Reporter.Tagged` is the encoding, and
`Test/ReportJsonSpec.hs` holds a golden object per constructor; there are no
sequence numbers yet.

Two things the flag does not cover. A node's *own* subprocess output — the
`Binary.Report`s a node's builder was handed a `reportPrint` for — is not one
of the three streams and still prints as text, so a binary whose nodes were
built with `reportPrint` (all of `salmon-apps` today) interleaves those lines
with the JSON ones; a consumer should skip lines that are not JSON. And
`run tree`/`run dag`/`query` are renderings of their own, not reports, and
are untouched.

## 12. Pull mode: `--follow`

`run serve --follow DIR --label L [--label L]... [--follow-base S] ...` makes
the loop fetch its own declarations instead of only waiting to be typed at.
`DIR` is a *registry*: one JSON document per label at `DIR/<label>.json`,
each the **desired set** of seeds for that label — not a log of commands:

```json
{
  "salmon": 1,
  "id": "web-api@2026-09-23T10:41:07Z",
  "seeds": [
    { "seed": ["--dir", "/tmp/play", "--name", "web", "--file", "index.html"] },
    { "directive": { "...": "a directive JSON, as `up-directive` takes" } }
  ]
}
```

`salmon` is the format version (only `1`), `id` is whatever the publisher
calls this revision, and anything else at the top level is ignored. A host
following several labels wants the union of their documents.

What happens on a change: the fetcher diffs the document against the one it
last applied *for that label* and injects one batch — `up` for each seed
newly present, `down` for each seed no longer present and not carried by any
other followed label either — which the loop runs with `autoconverge` held
off, restores, and converges once. Seeds you typed interactively are never in
that diff (unless you typed the exact seed a document then drops: the ledger
identifies a seed by its directive, not by who declared it).

What happens when nothing changed: **nothing**. The registry's mtime and size
say whether to read the file at all, the sha256 of the bytes says whether
anything changed, and an unchanged round is invisible to the loop. That rule
is load-bearing: every line reaching the loop stands the tending machines
down (§3), so a fetcher that injected on every poll would keep the supervisor
from ever reaching a steady state. Poll as often as you like.

`history` tells the fetcher's declarations from yours:

```
serve: seeds:
  #0 up       [active] --dir /tmp/play --name web --file index.html [fetched /srv/reg label=web-api id=web-api@2026-09-23T10:41:07Z sha256=32ea59311d97]
  #1 up       [active] --dir /tmp/play --name api --file openapi.json [fetched /srv/reg label=web-api id=web-api@2026-09-23T10:41:07Z sha256=32ea59311d97]
  #2 up       [active] --dir /tmp/play --name scratch --file notes
```

(`#2` was typed; a line run from `load <file>` says `[loaded <file>]`.)

The first fetch runs before standard input is read, so the first convergence
is deterministic — what the registry said at startup — and later changes
arrive live, handled like any typed command. A document that fails to parse,
a seed the binary cannot parse, or a seed whose `config` step throws, is
reported and skipped; the loop keeps serving and the last good document stays
in force.

### When rounds run, and when a change is applied

Two schedules, pointing in opposite directions, both on the `--follow-*`
flags (seconds unless said otherwise):

| flag | default | what it is |
|---|---|---|
| `--follow-base` | 30 | seconds between rounds while they succeed (`--follow-interval` is the older name for the same thing) |
| `--follow-factor` | 2 | how much slower each consecutive *failed* round makes the next one |
| `--follow-cap` | 600 | the longest a failing registry is left alone |
| `--follow-jitter` | 0.2 | every delay is scaled by a draw from `[1-j, 1+j]`, so a fleet does not poll in step |
| `--follow-debounce` | 5 | how long the registry must be quiet after a change before the change is applied; `0` applies at the round that saw it |
| `--follow-max-wait` | 60 | the longest a change waits while the registry keeps changing |
| `--follow-cache` | none | a directory to keep each label's last applied document in, replayed at startup if the registry cannot be reached (below) |
| `--follow-refuse-older` | off | refuse a document whose `published` is older than the one already applied for its label (below) |

**Toward the registry**: a round that succeeds — changed or not — schedules
the next one one base away; a round that fails (the registry threw, or the
bytes do not parse; a label with no document is *not* a failure, the
registry answered) climbs a ladder, `min(cap, base · factor^(n-1))` after
`n` failures in a row, and the first success steps off it. `follow: 3 failed
round(s) in a row; next in 120s` is what that looks like.

**Toward the loop**: a changed document is not applied at once. It is set
aside (`follow: web id=web@2 ...: changed, waiting for the registry to go
quiet`) and applied once no round has seen a further change for `debounce`,
or `max_wait` after the first pending one, whichever comes first — and what
is applied is the diff from the document the loop *last heard about* to the
*latest* one, so a publisher writing three times in a row is one batch and
one pass, and a half-published state is never applied. Several labels
changing inside one window are one batch too. The startup round is the
exception and applies at once: nothing to coalesce yet.

**`fetch`** cuts both short: a round now, the ladder forgotten, and whatever
is pending afterwards applied without waiting out the window — for the
operator who just published and does not want to wait. Without `--follow`
it only says nothing is being followed.

### Across a restart: `--follow-cache`

The world is in memory. Without more, a host restarted while its registry is
unreachable comes up empty, tears nothing down, and looks converged — worse
than no puller at all. `--follow-cache DIR` closes that: after every batch
the fetcher writes each label's just-applied document to
`DIR/<label>.applied.json` (bytes, sha256 and id; written to a temp file and
renamed, so a crash mid-write leaves the previous entry), and at startup a
label whose fetch *fails* — the registry directory is missing, or the file
does not parse — is replayed from there:

```
follow: fetching web failed: user error (registry directory does not exist: /srv/reg)
follow: web: registry unreachable; replaying the cached document id=web@1 sha256=8a8e1c180390
follow: web id=web@1 sha256=8a8e1c180390: 1 seed(s) up, 0 down
serve: epoch #0 up (4 nodes, 1 active seed(s))
```

A replayed document is treated exactly as a fetched one from then on — same
diff, same batch, same `[fetched ...]` in `history` — and its digest is what
the registry's answer is later compared against, so a registry that comes
back with the same bytes injects **nothing** (the starvation rule holds
across restarts) and one that comes back with a different document is
diffed against the replayed one, not applied from scratch. A label the
registry answers "no document" for is *not* replayed: the registry answered.
A cache entry that cannot be read is reported once and ignored, one that
cannot be written is reported and the batch goes in regardless; the cache
never takes the loop down. Without the flag nothing is cached, and a restart
against an unreachable registry declares nothing, as before.

### Which mode is this?

`status` now starts with which guarantees apply to the world:

```
serve: mode: replay
serve: nodes:
  ...
```

- `interactive` — nothing is followed; every declaration was typed, loaded
  or sent by a client.
- `following` — a fetcher is running and the world is what the registry
  last said.
- `replay` — the registry could not be reached at startup and at least one
  label's world is its cached document: the last thing this host knew, not
  necessarily what the registry says now.

`replay` turns into `following` at the first round in which every label
answers, changed or not. It is only ever *entered* at startup: after a
successful round the world already is the registry's last word, a round
failing later changes nothing about it (the last good document stays in
force), and `follow: N failed round(s) in a row` is what says the registry
is gone. Under `--json` the status object carries `"mode"`; the HTTP
surface's `/status` is the same object.

### Refusing to move backwards: `--follow-refuse-older`

A document may carry a `published` timestamp (RFC 3339) at its top level.
Nothing reads it unless `--follow-refuse-older` is given, under which a
fetched document published *before* the one already applied (or pending)
for its label is reported and left alone:

```
follow: web id=web@0: published before the document already applied; refused (--follow-refuse-older)
```

That is what a registry serving from a lagging replica would otherwise do to
a host. Off by default; a document without `published`, on either side, is
never refused. A `published` that does not parse is a malformed document,
not an ignored annotation.

Not there yet (`specs/pull-mode.md`, milestones 5 onwards): a status sink,
other registries (git, HTTP, DNS, bucket), and signatures.

## 13. A second way in: `--listen`

`run serve --listen PATH` binds a unix socket at `PATH` and accepts the
*same line protocol* on it — `up`, `status`, `force --select ...`, `quit`,
every command §3 typed on stdin — from any number of clients at once, while
stdin keeps working alongside. Milestone 2 of `specs/generic-server.md`;
`Salmon.Actions.Serve.Socket` is the implementation.

```sh
my-salmon run serve --listen /run/my-salmon.sock < /dev/null &
printf 'status\n' | socat - UNIX-CONNECT:/run/my-salmon.sock
ssh -L /tmp/remote.sock:/run/my-salmon.sock host   # then the same, locally
```

Four things to know:

- **Each client reads exactly the reports for its own lines**, as JSON
  lines in the §11 encoding, whatever the loop's own stdout is set to
  (text by default, JSON under `--json`; it sees everything either way).
  What another client typed, and what the tending loop says between
  commands, never reaches a client — the loop stamps every report with who
  typed the command it belongs to, and the socket only echoes the ones
  stamped for it. There is no per-client text mode.
- **A client hanging up is not `quit`.** It is reported on the loop's stdout
  (`serve: PATH#N hung up`) once every line that client typed has been
  handled, and the connection is closed then — so `printf 'status\n' |
  socat ...` gets its answer even though it half-closes immediately. `quit`
  from a client ends the loop exactly as it does from stdin.
- **Under `--listen`, stdin's end of input does not end the loop either.**
  With a socket to talk to, the process is expected to outlive whatever
  started it (`< /dev/null &`, a unit file), so stdin is one more source
  whose hang-up is reported and read past; only `quit` — typed anywhere —
  or a signal ends it. Without `--listen`, stdin closing ends the loop as
  it always has.
- **The socket is owner-only (mode 0600) and the path is checked before it
  is taken.** A stale socket file (its `serve` died without removing it) is
  replaced; one something still answers on is refused (`AlreadyListening`),
  as is a path holding something that is not a socket. Permissions are the
  whole access story: there is no authentication, and no TCP — see the
  spec's security section for why a salmon server must never listen on a
  network without both.

The commands are still one inbox: a line from a client stands the tending
machines down before it runs, same as a line from stdin, and two clients'
lines interleave at line granularity in arrival order.

## 14. HTTP on a socket: `--http`

`run serve --http PATH` binds a *second* unix socket and serves HTTP on it
— reads of the live world as JSON, and the same command language as
`POST`. Milestone 3 of `specs/generic-server.md`;
`Salmon.Actions.Serve.Http` is the implementation. It is its own path
rather than HTTP detected on `--listen`'s socket, so use both flags if you
want both; the socket file has the same owner-only mode and the same
live/stale checks as §13's.

```sh
my-salmon run serve --http /run/my-salmon.http < /dev/null &
C='curl -s --unix-socket /run/my-salmon.http'

$C http://x/dag | jq '.nodes[] | {shorthand, ref: .ref.short, direction, convergence,
                                  deps: [.dependencies[].short]}'
$C http://x/status | jq .        # the object `status` prints under --json
$C http://x/history | jq .       # likewise `history`, plus an `elided` count
$C http://x/help/seed | jq -r .seed   # this binary's own `config --help`

$C -X POST -d 'up --name web --file index.html' http://x/command      # sync
$C -X POST -d 'up --name api' 'http://x/command?async'                # {"seq": n}
$C -X POST -H 'content-type: application/json' -d '{"line": "status"}' http://x/command
```

What to know:

- **Reads never touch the inbox.** `/dag`, `/status`, `/history` and
  `/help/seed` read the loop's own `World` directly — they do not stand the
  tending machines down, do not wait behind a command, and answer while a
  node's `up` is still running. The price is that a read is at most one
  command old: each node's `status` is the snapshot the last command took
  (§11's `status` field, `null` for a node never tended). Motion between
  commands belongs to the event stream, a later milestone.
- **`/dag` is the graph a pass walks**, not the declared tree: one object
  per `Ref`, with `dependencies` and `dependants` as ref lists both ways,
  the node's `shorthand`/`help`/`notes`/`dynamics` (the fields §8's
  `Stale` detection compares), and its `direction`/`convergence`/`status`/
  `paths` as `status` lists them. It is populated the moment something is
  declared — under `autoconverge off` every node reads `pending` with its
  edges already in place — and a retired seed's nodes stay in it with
  `direction: "down"` until their teardown is done. A batch a `Rewrite`
  would introduce is not shown; the nodes it would stand in for are.
- **`POST /command` is one line of §3's language**, `text/plain` or
  `{"line": "..."}`, and it is handled like any other line: it stands the
  machines down first and takes its turn in the inbox. Synchronous by
  default, the response is a JSON array of exactly the reports that line
  produced (§11's objects), returned when the loop has finished with it —
  what a script or a CI step wants. `?async` returns `202 {"seq": n}` the
  moment the line is queued; `n` is the loop-wide sequence number the event
  stream will resume from. `quit` works from here too and answers `[]`.
- **Permissions are the whole access story.** No TLS, no token, no TCP;
  `notes`, `help` and report text are as public as the logs they already
  go to. Do not put this socket where an untrusted user can open it.

## 15. Gotchas

- **A piped script is never supervised.** If you're testing self-healing and
  piping a script in, you won't see it — there's no idle moment for the
  tending loop to occupy. Type interactively, or drive the loop from a fifo,
  to actually observe §3's perturb-and-watch-it-heal behaviour.
- **`Immaterial` means parked, not polled.** A node with no `check` looks
  exactly as "up" in `status` whether or not anything is actually watching
  it — the absence of supervision is deliberate and silent by design (the
  alternative, polling to ask a question with no useful answer, would cost
  something for nothing). If you want self-healing, write a `check`.
- **`sameRepresentative`'s blind spot is `up`/`check`/`down`.** Two
  declarations that differ only inside those functions compare *equal* for
  `Stale`-detection purposes (§8) — put something comparable in `notes` if
  you need a content-only change to register through the pass itself rather
  than the tending loop.
- **There are two different `query`s.** `my-salmon query show|plan|...` is a
  *top-level, one-shot* CLI command that inspects a directive on stdin
  before any `run up`/`run serve` even starts. `serve`'s own in-loop `query
  [--select]...` command inspects the live, already-converging `World`.
  They share selector syntax but operate on different things.
- **A node's `check` is the only thing that can notice it going away, and
  therefore the only thing that can fire a `RestForOne` demotion.** A config
  node with no `check` never notices its own file changing, and nothing
  standing on it is ever bounced, however that node is decorated otherwise.

## 16. Where to read more

- `docs/howto-ops.md` — writing and testing the `Op`s this doc assumes.
- `CLAUDE.md`'s "`salmon-ops` layer" section — the implementation-level
  summary of every module named above (`Actions/Upkeep.hs`,
  `Actions/Serve.hs`, `Op/Supervision.hs`, `Op/Dag.hs`, `Op/Rewrite.hs`),
  written for whoever is modifying salmon itself rather than using it.
- `specs/per-node-state-machines.md` and
  `specs/per-node-state-machines-remaining.md` — the original design and its
  running status; read these for the *why* behind a given tradeoff, or to
  see what's still explicitly left as "taste, not yet decided" (`supStrategy`
  living on the dependency rather than the dependant; the `RestForOne`
  cascade requiring opt-in at every hop).
- `salmon-ops/fixtures/ServeFixture.hs` — a runnable, hands-on tour of
  everything in §3, §6 and §7, including the `--daemon`/`--stale-check`
  flags that demonstrate `RestForOne` and what a health check is and isn't
  allowed to say about a process salmon just tore down.
