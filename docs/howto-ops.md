# How to write and test salmon ops

This is a cookbook. It is written to be unambiguous rather than elegant: copy the
patterns below, adapt the names/fields, and you will produce a valid `Op`. If you
want the conceptual model instead (why any of this exists), read
[`salmon-core.md`](salmon-core.md) first. This document assumes you already know
Haskell syntax but not necessarily this codebase.

Every code sample below is a paraphrase of real code in this repository. When in
doubt, grep for the cited file/function and copy its shape exactly.

## 1. The one type you need: `Op`

```haskell
type Op = OpGraph Identity Actions'
```

An `Op` is a single graph node plus everything needed to run it: how to bring it
up, how to tear it down, how to know if it's already up, and what it depends on.
You almost never construct one by hand — you call the `op` helper.

```haskell
op :: ShortHand -> Identity (Graph Op) -> (Extension -> Extension) -> Op
```

- `ShortHand` (a `Text`) — a short, human-readable kind name, e.g. `"directory"`,
  `"file-contents"`, `"pg-user"`. Used in `Ref` construction (see below) and in
  `Tree`/`Dot` output. Pick something stable — changing it changes the node's
  identity.
- `Identity (Graph Op)` — this node's dependencies. Almost always built with one
  of the two helpers below, never by hand:
  - `nodeps :: Identity (Graph Op)` — no dependencies.
  - `deps :: [Op] -> Identity (Graph Op)` — depends on (is preceded by) this list
    of ops, all co-occurring at the same level (no ordering is implied *between*
    them — see §3 for ordering).
- `(Extension -> Extension)` — a function that fills in the actual behavior on
  top of a blank no-op `Extension`. You almost always write this as a record
  update on the `actions` argument: `\actions -> actions { help = ..., up = ... }`.

## 2. The `Extension` record — what you fill in

```haskell
data Extension = Extension
    { help    :: Text            -- one-line description
    , notes   :: [Text]          -- longer free-form notes
    , ref     :: Ref             -- dedup identity (see §2.1)
    , up      :: IO ()           -- bring this node's own effect into being
    , managed :: Maybe (Output -> IO ExitCode)
                                 -- ...or *be* the effect, for as long as it runs (see §2.2)
    , check   :: IO CheckResult  -- is my effect already in place? (see §4)
    , down    :: IO ()           -- undo this node's own effect
    , dynamics :: [Dynamic]      -- arbitrary typed metadata, see §7
    }
```

You only ever need to touch `help`, `ref`, `up`, `down`, and — if the node
needs idempotency beyond what `up` itself can guarantee — `check`. `managed`
is `Nothing` for all but a handful of nodes; see §2.2 if yours is one of
them.

`check` answers one question about the node's own effect, with six possible
answers: `Success` (it is in place), `Skipped` (someone decided to treat it as
satisfied — only `Query.forceSkip` produces this), `Completed` (it ran to
completion and stopped on purpose), `Failure reason` (it is not in place —
the *ordinary* answer on a first run, not an error report), `Unknown` (a check
ran and could not tell), and `Immaterial` (there is nothing here worth asking
about — applying the effect costs about what finding out would; this is the
**default** when you write no `check`). `upTree` skips the first three and runs
`up` for the last three.

`check` earns its keep twice, and the second time is easy to miss. In a one-shot
`run up` it is an optimisation: it saves an `up` you didn't need. Under `run
serve`, which *tends* its nodes between commands (`Salmon.Actions.Upkeep`), it
is the **only** thing that can notice your effect going away — nothing else in
the model looks. A node with no `check` answers `Immaterial`, and the upkeep
FSM *parks* it: brought up once, then blocked on its mailbox rather than woken
on a timer to be told the same thing. (An explicit `Unknown` is different: the
FSM keeps asking, and keeps not acting on it — a loop that treated "I could not
tell" as "so run `up`" would spin.) So if your node is something that can stop
being true on its own — a service, a mount, a firewall rule, a file something
else might clobber — write a `check`; it is the difference between a node that
gets *applied* and a node that gets *supervised*. `Netfilter.rule`'s
`skipIfNftRuleExists` is the template, and `Systemd.checkService` is the worked
example of what a real one costs.

`up` and `down` default to `pure ()` (a no-op) if you don't set them — this is
useful for pure "grouping" nodes (see §3) that only exist to bundle
dependencies, with no effect of their own.

### 2.1 `ref`: dedup identity

```haskell
ref = mkRef "directory" path
```

`mkRef :: (Hashable key) => Text -> key -> Ref` builds a `Ref` from a kind tag
plus anything `Hashable` that uniquely identifies *this* instance of the node
— usually the primary key of whatever you're describing (a file path, a
database name, a `(src, tgt)` pair for a copy). Two `Op`s with the same `Ref`
are treated as the *same node* by the graph traversal (`upTree`/`downTree`):
reachable via two different paths or not, it is one node, applied once.
**Getting `ref` right is the
single most important thing when writing a new node** — get it wrong (too
coarse, e.g. reusing one `Ref` for two different files) and you silently skip
work; get it wrong the other way (varying per-call for what should be the same
resource) and you silently do the work twice.

Note what the identity key is *not*: the node's behaviour. `filecontents` keys
on the path alone, so an equal `Ref` means "the same effect site", not an equal
node — two `Op`s writing different bytes to one path are one node, and only one
of them wins. Both traversals go through `Salmon.Op.Dag`, so the rule is the
same in both directions: the **last** representative wins, and the loser is
reported as `Conflicting` so the collision is at least visible.
Tightening a key (putting a content digest in it, say) is a legitimate per-node
fix, but think about it per node: it is right for a file and wrong for a
long-running service, where every config tweak would become a different node.

### 2.2 `managed`: when the node *is* a running process

`up :: IO ()` describes an effect that persists once it has been made: the
file stays written, the route stays installed. A long-running process does
not — nothing keeps it alive but something watching it. That is what
`managed` is for:

```haskell
managed :: Maybe (Output -> IO ExitCode)
```

It blocks for as long as the node is up and returns the reason it stopped,
which means the node's own thread is in scope for the process's entire
lifetime and the handle never has to escape. Three things follow that a
`check` alone cannot give you: an exit *status* (so "don't restart a service
that exited cleanly" is expressible), promptness (a death is noticed at once
rather than at the end of a check delay that may be a minute), and identity
that survives pid reuse.

Don't write one from scratch — use `Salmon.Builtin.Nodes.Daemon`:

```haskell
Daemon.daemon reportPrint (Daemon.defaultDaemon "webserver" (proc "nginx" ["-g", "daemon off;"]))
```

or, if your node needs dependencies or a liveness `check` of its own, build
it around `Daemon.runDaemon`, which is the same action without the node
wrapped round it. Either way you get the teardown: `SIGTERM` to the process
*group*, a grace period, then `SIGKILL`. Writing that yourself is easy to get
subtly wrong.

Three things to know before reaching for this:

- **only `run serve` honours it.** `run up`/`run down` call `up`, and a
  one-pass driver has nowhere to put an action that never returns. So a node
  like this should `throwIO` from `up` (that's what `Daemon.daemon` does)
  rather than no-op into a world that then believes it is up.
- **prefer systemd where there is systemd.** `Systemd.systemdService` hands
  the whole problem to an init system that is better at it and survives
  salmon exiting. `managed` is for where that is not available: a container,
  a test harness, or salmon-as-init itself.
- **the `Output` argument is where your process's own lines go** — into the
  node's bounded ring, which is what an operator reads when it has failed and
  what tells a watchdog it is still making progress. `Daemon.runDaemon`
  handles the piping.

## 3. The canonical example: `Filesystem.hs`

Read `salmon-ops/src/Salmon/Builtin/Nodes/Filesystem.hs` end to end — it is the
smallest complete example of every pattern in this document. Key excerpts:

A node with no dependencies:

```haskell
newtype Directory = Directory {directoryPath :: FilePath}

dir :: Directory -> Op
dir directory =
    op "directory" nodeps $ \actions ->
        actions
            { help = Text.pack $ "ensures " <> path <> " exists, including subdirs"
            , ref = mkRef "directory" path
            , up = createDirectoryIfMissing True path
            , down = removeDirectory path
            }
  where
    path = directory.directoryPath
```

A node that depends on another node it builds internally:

```haskell
data FileContents a = FileContents {filePath :: FilePath, contents :: a}

filecontents :: (EncodeFileContents a) => FileContents a -> Op
filecontents fcontents =
    op "file-contents" (deps [enclosingdir]) $ \actions ->
        actions
            { help = Text.pack $ "writes " <> path <> " with some contents"
            , ref = mkRef "file-contents" path
            , up = ByteString.writeFile path =<< encodeFileContents fcontents.contents
            , down = removeFile path
            }
  where
    enclosingdir = dir (Directory $ takeDirectory path)
    path = fcontents.filePath
```

Note the pattern: `filecontents` doesn't take a `Directory` as an argument — it
*derives* the directory it needs (`takeDirectory path`) and constructs that
dependency itself, inline, in a `where` clause. This is idiomatic: a node
should make its own prerequisites, not expect a caller to remember to also
build them. As long as the derived `dir` call produces the same `Ref` every
time it's reachable, it dedups correctly no matter how many other nodes also
depend on "the same" directory.

A node combining several dependencies with explicit ordering
(`replaceDirectory` in the same file):

```haskell
replaceDirectory :: FilePath -> FilePath -> FilePath -> Op
replaceDirectory src tgt trash =
    op "replace-dir" (deps [delete3 `inject` move2 `inject` move1]) $ \actions ->
        actions
            { help = Text.pack $ "replace " <> src <> " " <> tgt
            , ref = mkRef "replace-dir" (src, tgt)
            }
  where
    move1 = moveDirectory tgt trash $ \actions -> actions{check = skipIfDirectoryIsMissing tgt}
    move2 = moveDirectory src tgt id
    delete3 = destroyDirectory trash
```

This node has **no `up`/`down` of its own** (they default to no-ops) — it's a
pure orchestration node whose entire job is wiring three other ops together in
order. This is a completely normal and common pattern: not every `op` call
needs to *do* anything itself.

### 3.1 `deps` vs `inject` vs `overlaid`

- `deps [a, b, c]` — this node is preceded by `a`, `b`, and `c`, each also
  preceded by whatever *they* depend on. No ordering is implied among `a`,
  `b`, `c` themselves.
- `` a `inject` b `` (from `Salmon.Op.OpGraph`) — "`b` must happen before `a`":
  adds `b` as a predecessor of `a` via an ordered graph connection. Chain it to
  express a strict sequence: `` c `inject` b `inject` a `` means run `a`, then
  `b`, then `c`.
- `` a `overlaid` b `` — "`a` and `b` co-occur, no ordering implied between
  them" (the unordered counterpart to `inject`).

Use `deps [...]` for the normal "these are my prerequisites" case. Reach for
`inject` only when two ops need a specific order that the dependency structure
alone wouldn't otherwise guarantee (e.g. "delete the trash dir" must happen
*after* "move src into place", not just alongside it).

## 4. Idempotency: making `up` safe to run twice

`upTree` may run the same `Op` graph repeatedly (that's the point — it's meant
to converge a system to a target state, not run once and be thrown away).
**`up` must be safe to call when the target state already holds.** In rough
order of preference:

1. **Use a `replace`-shaped command if the tool has one.** E.g. `ip route
   replace` instead of `ip route add` (see `Salmon.Builtin.Nodes.Routes`), or
   SQL `ALTER SYSTEM SET` instead of an insert.
2. **Use a `CREATE ... IF NOT EXISTS` / `CREATE OR REPLACE` form if it exists**
   for what you're creating.
3. **Guard a bare `CREATE` with a conditional inside a `DO` block**, when the
   tool allows a `CREATE` with no `IF NOT EXISTS` form to run inside one — e.g.
   Postgres `CREATE ROLE` has no `IF NOT EXISTS`, but can run inside:
   ```sql
   DO $$
   BEGIN
     IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'myuser') THEN
       CREATE ROLE myuser;
     END IF;
   END $$;
   ```
   (see `Postgres.CreateUser`/`CreateGroup` in
   `salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs`).
4. **Shell-level check-then-act**, when even that isn't available — e.g.
   Postgres refuses to run `CREATE DATABASE` inside a transaction/`DO` block at
   all, so `Postgres.CreateDB` does:
   ```sh
   psql -tAc "SELECT 1 FROM pg_database WHERE datname = 'mydb'" | grep -q 1 \
     || psql -c 'CREATE DATABASE mydb'
   ```
5. **Append-if-missing**, for config file lines with no SQL/CLI equivalent at
   all: `grep -qxF '<line>' file || echo '<line>' >> file` (see
   `Postgres.ensureHbaLineScript`, for `pg_hba.conf`).
6. **A `check`-based skip check**, when the underlying tool has *no* idempotent
   verb at all — nothing to `replace`, no `IF NOT EXISTS` — so `up` itself
   cannot be made safe to re-run. Instead, check *before* running whether the
   effect already exists, and report `Success` if so:
   ```haskell
   skipIfNftRuleExists :: Chain -> Rule -> IO CheckResult
   skipIfNftRuleExists c rule = do
       (code, out, _err) <- readCreateProcessWithExitCode (proc "nft" ["list", "chain", ...]) ""
       pure $
           if renderedRuleText rule `isInfixOf` out
               then Success
               else Failure "no such rule in the chain"
   ```
   ```haskell
   op "netfilter-rule" (deps [...]) $ \actions ->
       actions
           { check = skipIfNftRuleExists chain rule
           , up = addRule ...
           , ...
           }
   ```
   This is the same shape as the built-in `skipIfFileExists`/
   `skipIfDirectoryIsMissing`/`skipIfNetworkExists` helpers in
   `Salmon.Actions.UpDown`/`Salmon.Builtin.Nodes.Podman` — **when you add a new
   node wrapping a command with no idempotent "set" verb (e.g. `nft add rule`,
   `podman network create`), this is the template to copy**: write a
   `skipIfXExists :: ... -> IO CheckResult` that shells out to a read-only
   "does X exist" check, and wire it into `check`.

`check` defaults to `pure Immaterial` — "asking would cost what applying costs",
which `upTree` reads as "run `up`" — if you don't set it, so options 1–5 above
need no `check` at all; only option 6 does. That default is a claim, not an
absence, and for options 1–5 it is a true one: the whole reason those nodes are
idempotent is that re-applying them is cheap. It is *false* for anything that
can stop being true on its own, which is why such a node needs option 6. Note also that a `check` that *throws* is
contained: it is read as `Failure`, so the node is evaluated and the rest of
the traversal is unaffected.

## 5. Failure: how `up`/`down` report errors

`up :: IO ()` and `down :: IO ()` have no return value to signal failure — the
**only** way a failure becomes visible to the graph traversal (`upTree`/
`downTree` in `Salmon.Actions.UpDown`) is if `up`/`down` **throws**. A thrown
exception is caught by the traversal, the node is reported `Failed`, and every
node that (transitively) depends on it is reported `Blocked` and *not*
evaluated (for `upTree`; `downTree` blocks the opposite direction — a node's
own predecessors — since teardown walks top-to-bottom). Both `upTree` and
`downTree` return `IO Bool`: `True` iff nothing was `Failed`/`Blocked`.

**Getting this right is mostly automatic** if you build `up` on top of
`Salmon.Builtin.Nodes.Binary.untrackedExec` (which almost every builtin does
via `withBinary`) — it already checks the subprocess exit code and throws
`CommandFailed` on non-zero, so a normal `up = someCommand r' ...` needs no
extra handling.

Two cases *do* need you to add explicit handling:

- If your node is built on the lower-level `withBinaryIO`/`CommandIO` (used
  when you need to redirect a subprocess's stdin/stdout yourself, e.g.
  `WireGuard.privateKey`/`publicKey`), you get a raw `ProcessHandle` back
  instead of a checked result — call `Binary.checkExitCode label` on it
  yourself (it throws `CommandFailedSimple` on non-zero).
- If your node's `up` recursively runs its *own* nested `upTree` (e.g. to
  drive a remote machine, see `PostgresMigrations.remoteMigrateOpaqueSetup`),
  you must check the returned `Bool` yourself and `throwIO` if it's `False` —
  the outer traversal has no other way to learn the nested one failed:
  ```haskell
  up = do
      ok <- upTree r nat nestedGraph
      unless ok (throwIO (userError "nested upTree failed"))
  ```

Never swallow an exception inside `up`/`down` (e.g. via `catch`/`handle`) just
to "keep going" — that defeats `Failed`/`Blocked` propagation and makes a real
failure look like success to the traversal and to anything scripting around
this binary's exit code.

## 6. Composing with `Track`/`Tracked` — when a node needs a value from elsewhere

Sometimes one node's construction genuinely needs a *value* that another part
of the graph produces or that the caller controls (not just an ordering
dependency) — e.g. "grant these rights to *this* database owner role, which
some other part of the setup created." That's what `Track`/`Tracked` are for:

```haskell
newtype Track m n a = Track { run :: a -> OpGraph m n }
```

A `Track' a` (i.e. `Track Identity Actions' a`) is "given an `a`, I know how to
build the `Op` that provisions it." You'll see this passed around as a
parameter so callers can plug in different strategies for "how do I get an
`a`" without the node itself caring — e.g. `Postgres.database` takes a
`Track' (Binary "psql")` so callers can point it at a locally-installed
`psql` binary or a differently-provisioned one.

`ignoreTrack :: Track' a` is a real, always-available `Track` that discards its
input and produces a no-op `Op` — use it when you have a value already in hand
and don't need the `Track` machinery to *produce* anything more, only to
satisfy a function signature that expects one (this is extremely common; grep
existing recipes for `ignoreTrack` to see the pattern used dozens of times).

You do not need `Track`/`Tracked` for the vast majority of new nodes — only
reach for it when you're deliberately decoupling "how do I build a value" from
"what do I do with it," the same way `Postgres.hs` and the recipes in
`salmon-ops-recipes/src/SreBox/` do it. See [`salmon-core.md`](salmon-core.md)
§`Track` for the type-level reasoning.

## 7. `dynamics` — attaching arbitrary metadata to a node

`dynamics :: [Dynamic]` lets you stash arbitrary typed metadata on a node that
some other part of the codebase can later recover by type, without changing
`Extension` itself. Two real uses:

- `placeholder` (in `Salmon.Builtin.Extension`) stashes a `PlaceHolder Text` so
  dot-graph rendering can show a label without the node needing real `up`/
  `down` behavior.
- `getDynamics`/`collectDynamics` walk a graph and pull out every `Dynamic` of
  a chosen type — used e.g. to flatten "which of these ops are actually remote
  calls" out of a graph for special handling.
- `Debian.Package.deb` stashes a `Package`, which the `batchPackages` **rewrite**
  (`Salmon.Op.Rewrite`) collects across the whole graph into one `apt-get`
  invocation.
- A `Salmon.Op.Supervision.Supervision` states how the node wants to be tended:
  whether to put it back when its `check` says the effect has gone (`Always`/
  `OnFailure`/`Never`, defaulting to `OnFailure`), how long its silence may
  last before somebody should worry, and — `supStrategy` — what its *going
  away* means for the nodes standing on it. `Salmon.Actions.Upkeep` reads it
  back with the same `getDynamics`. One line, and a node with no opinion needs
  none:

  ```haskell
  dynamics = [supervised defaultSupervision{supWatchdog = Just (seconds 30)}]
  ```

  Amend `defaultSupervision` rather than spelling out every field: the record
  has grown twice and will again, and a node that only cares about its
  watchdog should not have to have an opinion about giving up.

  `supStrategy` is the one field authored for somebody else's benefit. The
  default, `OneForOne`, is that putting this node back is a statement about
  this node and nothing downstream is disturbed. `RestForOne` sends every
  dependant back to `WaitUp`, to be brought up again on top of whatever this
  node turns into — Erlang's strategy of the same name, read along dependency
  edges. A configuration file is the case for it:

  ```haskell
  -- the services reading this file are bounced when it is rewritten
  dynamics = [supervised defaultSupervision{supStrategy = RestForOne}]
  ```

  Note it goes on the config node, not on the services: the file's author
  knows their content is load-bearing, while each service reading it would
  otherwise have to know, separately, that it might change underneath. Only
  `run serve` acts on it (a one-shot pass has no "already up" to send back
  from), and it costs nothing at all until a node opts in.

That last one is what the field is really for, and it's worth understanding
the shape. A node says *"I am a `Package`"* without knowing what will be done
about it, and a later pass collects the set and acts on it. The later pass is
the only place that *can* act: your `Track' directive` is a function of one
directive in isolation, so it can't see the other declarations `run serve`
currently holds, or which way each of their nodes is wanted. A `Rewrite` runs
after the graph has been folded to a `Ref`-keyed DAG, where both of those
exist — so it can batch across declarations and split an install batch from a
removal batch, neither of which a recipe could express. Register one with
`CommandLine.execCommandOrSeedWithRewrites` rather than applying an `Op -> Op`
inside your `Track'`.

You will rarely need to add a *new* dynamic type; if you find yourself wanting
one, search for existing `toDyn`/`fromDynamic`/`getDynamics` usages first. The
question to ask is whether some *later, wider* pass needs to know this about
your node — if the answer is "only this node cares", it's a field on your own
value type, not a dynamic.

## 8. Writing a new builtin node — a worked template

Put a new node under `salmon-ops/src/Salmon/Builtin/Nodes/<Concern>.hs` (one
module per concern — see the existing list: `Filesystem`, `Systemd`,
`Debian.Package`, `Podman`, `Postgres`, `Netfilter`, `Ssh`, `WireGuard`, etc.).
Minimal template:

```haskell
module Salmon.Builtin.Nodes.Widget where

import qualified Data.Text as Text
import Salmon.Builtin.Extension
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter

-- | Whatever this node needs to do its job.
data Report = CreatedWidget !WidgetName | RemovedWidget !WidgetName
    deriving (Show)

newtype WidgetName = WidgetName Text
    deriving (Eq, Ord, Show)

widget :: Reporter Report -> WidgetName -> Op
widget r name =
    op "widget" nodeps $ \actions ->
        actions
            { help = "creates a widget named " <> getWidgetName name
            , ref = mkRef "widget" name
            , up = createWidget name >> runReporter r (CreatedWidget name)
            , down = removeWidget name >> runReporter r (RemovedWidget name)
            }
```

Points worth calling out explicitly since they're easy to get subtly wrong:

- **`Reporter r`** is the standard way a node emits structured events (not
  `print`/logging directly) — every builtin module defines its own `Report`
  sum type and takes a `Reporter Report` as its first argument, then
  `contramap`s it into sub-calls (`contramap SomeConstructor r`) when composing
  with other nodes' reporters. Look at any existing module (e.g. `PostgresInit
  .hs`'s `Report` type, which wraps `Postgres.Report`/`Self.Report`/etc.) to
  copy the pattern for a module that composes several builtins.
- **Don't forget idempotency** (§4) before calling it done — this is the most
  commonly-missed step for a first-draft node.
- **Real subprocess calls should go through `Salmon.Builtin.Nodes.Binary`**
  (`withBinary`/`untrackedExec`) rather than raw `System.Process` calls, so
  failure propagation (§5) is automatic.

## 9. Wiring a node into a CLI binary (seed → spec → ops)

If you're building a whole binary (not just adding one node to an existing
recipe), every salmon binary follows the same two-subcommand shape via
`Salmon.Builtin.CommandLine.execCommandOrSeed`:

```sh
my-salmon config <seed-args...>   # seed (CLI-friendly) -> JSON directive on stdout
my-salmon run Up|Tree|DAG         # reads a JSON directive on stdin, executes/prints it
```

You need three things (see `salmon-apps/src/Migrator.hs` as the worked,
complete example):

1. A `Seed` type — parsed from CLI args (`Options.Generic`/`optparse-applicative`).
2. A `Spec` type — `FromJSON`/`ToJSON`, the thing that actually gets piped over
   stdin/stdout between `config` and `run`.
3. A `Configure IO Seed Spec` (`Configure { gen :: Seed -> IO Spec }`) plus a
   `Track' Spec` (i.e. `program :: Track' Spec`, `program = Track $ \spec -> ...
   build an Op from spec ...`) that turns the `Spec` into the actual `Op` graph.

```haskell
main :: IO ()
main = do
    cmd <- execParser (info parseRecord ...)
    CLI.execCommandOrSeed reportPrint configure program cmd

program :: Track' Spec
program = Track $ \spec -> op "program" (deps [...]) id

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> ... build a Spec from seed ...
```

This split exists so config generation (impure, human-parametrized, runs on
the commanding machine) and execution (must be IO/hermetic, meant to run
unattended, e.g. piped to a remote box over ssh) stay separate, independently
inspectable steps — the JSON `Spec` is the contract between them.

## 10. Testing ops

Tests for `salmon-ops-recipes` live under `salmon-ops-recipes/test/Test/` and
run via `cabal test salmon-ops-recipes`. All the plumbing you need is in
`Test.Harness` (`salmon-ops-recipes/test/Test/Harness.hs`) — **read that file**;
this section is a guide to it, not a replacement for it.

Tests are organized into three tiers by IO cost/blast-radius. Pick the
*cheapest* tier that actually exercises what you changed.

### Layer 0 — structural, no side effects

Assert on the shape of the graph itself, not on any effect. Use
`Salmon.Builtin.Extension.evalDeps :: Op -> Cofree Graph Op` to walk the graph
and check what's in it (e.g. "does this graph contain a node with this
`Ref`", "how many nodes does it have"). No IO happens at all. Good for
checking wiring/composition logic (did I actually connect the deps I meant
to) without needing any real environment.

### Layer 1 — sandboxed IO, no external services

Run the *real* `up`/`down` for real, but only against something throwaway and
local — a temp directory, an in-process value — nothing that needs a running
service. Use:

```haskell
runUp   :: Op -> IO Bool   -- runs upTree, returns True iff everything succeeded
runDown :: Op -> IO Bool   -- runs downTree, same contract
withTempDir :: (FilePath -> IO a) -> IO a  -- auto-cleaned scratch dir
runUpCapturing :: Op -> IO [UpDown.Report Extension]  -- full trace, for asserting Skip/Eval/Blocked directly
```

Example shape (see `JWTSigningSpec.hs` for a real one):

```haskell
testCase "writes the file" $ withTempDir $ \dir -> do
    let op = someNodeThatWritesInto dir
    ok <- runUp op
    assertBool "expected up to succeed" ok
    contents <- readFile (dir </> "expected-file")
    contents @?= "expected contents"
```

**Always check the `Bool` `runUp`/`runDown` return** (via `assertBool`) in
addition to any postcondition check — a postcondition alone can't tell you
whether the traversal itself reported `Failed`/`Blocked` somewhere it
shouldn't have.

### Layer 2 — real system services via disposable podman containers

For anything that needs a real service (postgres, a systemd unit, apt) to
verify against, dogfood the project's own `Podman` builtins as the sandbox
provisioner — do not hand-roll `podman run`/`podman rm` shell-outs. Guard the
whole test with `requireExecutable "podman"` so CI/dev machines without
podman skip loudly instead of failing:

```haskell
testCase "creates a real database" $ requireExecutable "podman" $
    withContainer (Podman.Image "debian:bookworm")
                  (Podman.PortMapping "15432" "5432" Podman.TCPPort) $ \cid -> do
        -- one-time sandbox prep that isn't part of the recipe under test:
        podmanExec_ cid ["apt-get", "update", "-qq"]

        withShimmedPath cid ["apt-get", "sudo", "bash"] $ do
            let op = MyRecipe.setupSomething ...
            ok <- runUp op
            assertBool "recipe should fully succeed" ok

        -- postcondition: check it for real, inside the container
        (code, out, err) <- podmanExecCapture cid ["sudo", "-u", "postgres", "psql", "-lqt"]
        assertBool ("expected db in: " <> out) ("mydb" `isInfixOf` out)
```

Key pieces, all from `Test.Harness`:

- `withContainer :: Podman.Image -> Podman.PortMapping -> (String -> IO a) -> IO a`
  — pulls the image, starts a uniquely-named container, hands you its
  container id, and guarantees teardown (via the real `Podman` `down` action,
  not a manual `podman rm`) even on exception.
- `withShimmedPath :: String -> [String] -> IO a -> IO a` — for recipes that
  shell out to real binaries by name (`apt-get`, `sudo`, `psql`, ...) with no
  indirection to mock: writes lookalike wrapper scripts for each named command
  that forward into the container via `podman exec`, and prepends them to
  `PATH` for the duration of the action. This lets you run the recipe's
  *actual, unmodified* command construction and graph wiring against the
  sandbox, instead of testing a mocked stand-in. Figure out which commands to
  shim by reading which binaries the recipe under test actually invokes (see
  `PostgresInitSpec.hs`'s `shimmedCommands` for a worked example and comment
  explaining the reasoning).
- `podmanExec_ :: String -> [String] -> IO ()` — run a setup command inside the
  container, discard output, `error` on non-zero exit. For sandbox prep only
  (e.g. `apt-get install sudo`), not for the thing under test.
- `podmanExecCapture :: String -> [String] -> IO (ExitCode, String, String)` —
  same, but for postcondition checks: hands back everything instead of
  throwing.

A real bug this pattern caught (documented at the top of
`PostgresInitSpec.hs`): a builtin used to hardcode a Postgres cluster version
that didn't match the container's actual installed version, so the recipe
silently failed to start the cluster — a Layer 0 test would never have caught
this, because the graph *shape* was perfectly correct; only running it for
real against a real (if disposable) Debian container did.

### Choosing a layer: a quick decision guide

- Changed how ops are wired together (deps/ordering/ref dedup)? → **Layer 0**
  is probably enough.
- Changed what a node actually writes/does, but it's pure filesystem/local
  state? → **Layer 1**.
- Changed anything that shells out to a real service's CLI (`psql`,
  `pg_ctlcluster`, `nft`, `systemctl`, `apt-get`, `podman`, ...) or depends on
  that service's actual behavior? → **Layer 2** — a Layer 0/1 test would pass
  even if the actual command is wrong, wrong-ordered, or the tool's real
  idempotency behavior doesn't match what you assumed.

### Running the tests

```sh
cabal test salmon-ops-recipes
```

Layer 2 tests are skipped loudly (not failed) if `podman` isn't on `PATH` —
check the test output for `SKIPPED:` lines if you expect Layer 2 coverage and
don't see it exercised.
