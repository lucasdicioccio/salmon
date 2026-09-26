# Multi-user / privilege separation: running parts of a graph as a lesser identity

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem

One `upTree`/`downTree` is one Haskell process, and a process has exactly one
uid. Today that uid has to be the *maximum* of every privilege any node in the
graph needs — in practice root, because somewhere in the graph there is an
`apt-get install`, an `nft add rule`, or a `systemctl daemon-reload`. Every
other node in the graph then also runs as root, whether or not it needs to.

That produces three distinct problems, and it is worth separating them because
they have different fixes:

1. **No way to drop privilege for a node that doesn't need it.** A
   `Cabal.build`, a `Git.repo` clone, an `Npm`/`Spago` build, a
   `Web.download` — none of these need root, and running them as root means an
   arbitrary `build.sh`/`postinstall` script in a third-party dependency runs
   as root. There is no vocabulary in the codebase to say "this node runs as
   `builder`".

2. **Identity-switching is hardcoded, per node, in the wrong place.**
   `Salmon.Builtin.Nodes.Postgres.psqlAdminRun_Sudo`
   (`salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs:398`) bakes
   `sudo -u postgres` into the `CreateProcess` of all twelve of its admin
   commands. That module already carries the todo this spec is answering
   (`Postgres.hs:394`):

   > todo: workaround chmod and sudo hack with some calling preference
   > - we'll need to request more than a `Track' (Binary "psql")` but some more
   >   complex logic with sudo, the user, and the right binary

   The consequences of hardcoding are concrete: the choice of mechanism
   (`sudo` vs `runuser` vs `setpriv`) is not a caller's to make, `sudo`
   becomes an undeclared dependency (nothing in the graph installs it or
   configures sudoers), and a caller that *already runs as* `postgres` still
   pays for a `sudo` hop.

3. **Files created by the root process are unreadable by the lesser identity,
   and the workaround is a security hole.** `Filesystem.filecontents`
   (`Filesystem.hs:51`) is `ByteString.writeFile` under the process umask:
   root-owned, `0644`. So `Postgres.adminScript` (`Postgres.hs:342`) copies the
   migration script into `/opt/salmon/postgres/migrations/admin/` and then runs
   `chmod a+r` on it (`ChmodAdminScript`, `Postgres.hs:404`) purely so that the
   `sudo -u postgres psql -f ...` on the next line can read it. That makes
   every admin migration script world-readable on the box. The same shape would
   be far worse applied to `Secrets.sharedSecretFile` or `Keys`.

The ask is: what vocabulary should salmon grow — ops, decorators, or something
else — to express this?

## The structural finding: the decoration cannot live on `Op`

The instinctive answer is an `Op -> Op` decorator:

```haskell
runAs :: User -> Op -> Op          -- does not work
```

It cannot work, and understanding why determines the whole design.
`Extension.up` has type `IO ()`. By the time you hold an `Op`, the command has
already been rendered: `withBinary` (`Binary.hs:80`) called `prepare` to build
a `CreateProcess`, closed over it in `untrackedExec`, and handed the node
author an opaque `Reporter Report -> IO ()`. An `Op -> Op` decorator receives
that closure and has no way to look inside it, let alone rewrite its `cmdspec`.
`up`'s type erases everything.

So an `Op`-level decorator has exactly one implementation available to it:
wrap the whole `IO ()` in a `forkProcess`+`setuid` (Layer 4 below), which
brings in the threaded-RTS hazards and semantic cliffs discussed there.

**The privilege decision has to be made upstream of `Extension.up` — at the
`Command`/`withBinary` boundary, where the `CreateProcess` still exists as a
value.** That is the good news: it is exactly one chokepoint, and ~100 of the
call sites in the repo go through it.

## What already exists to build on

The repo is not starting from nothing here; several pieces are already the
right shape.

| Existing | What it gives us |
|---|---|
| `Binary.Command { prepare :: arg -> CreateProcess }` (`Binary.hs:72`) | A pure `arg -> CreateProcess`, i.e. a value that can be *rewritten* by a combinator before `withBinary` closes over it. |
| `Track'`/`tracking`/`inject` | The mechanism to make "and `sudo` must be installed, and the user must exist" real graph predecessors rather than ambient assumptions. |
| `User.User`/`Group`/`Owner`/`chown` (`User.hs`) | The identity vocabulary already exists as typed values, plus a `Track' Group`/`Track' User` convention for provisioning them. |
| `Capabilities.grantCapabilities` (`Capabilities.hs`) | Precedent for "grant a binary less than root instead of running the caller as root" — the fine-grained end of this same spectrum. |
| `Systemd.Service.service_user`/`service_group` | Identity separation already solved for the *runtime* of a service; this spec is the same idea for the *provisioning* of it. |
| `Self.callSelfAsSudo` (`Self.hs:99`) | Precedent for re-invoking the salmon binary itself under a different identity with a JSON directive on stdin — currently only over SSH, but the local case is the same trick. |
| `Query.Plan` + `run up --plan` + `Query.forceSkip` | Machinery to execute a *subset* of a graph, digest-pinned to a directive. This is what makes Layer 3's partitioning possible with almost no new code. |
| `Extension.dynamics :: [Dynamic]` + `collectDynamics` | The established way to attach out-of-band typed metadata to a node and recover it by a whole-graph analysis — the natural carrier for a privilege-domain tag. |
| `UpDown.Gate` | A caller-supplied per-node "does this traversal want this node" — already the hook a domain-filtered pass would use. |

## Proposed layering

Four layers, deliberately independent. Each is useful alone; none requires the
next.

| Layer | What it segments | New code | Recommendation |
|---|---|---|---|
| **L0** | Nothing — partition by hand with existing `query plan` + `sudo -u self run up --plan` | none | Document it now; it is available today |
| **L1** | Individual **binary calls** (`RunAs` + `Invoker`) | small, in `Binary.hs` | **Build this first.** Answers the `Postgres.hs:394` todo directly |
| **L2** | **File ownership/mode at creation** | small, in `Filesystem.hs` | Build second. Without it L1 is half a solution |
| **L3** | Whole **subgraphs**, by declared domain, via re-exec of self | medium | Design now, build if L1+L2 prove insufficient |
| **L4** | Individual **ops**, via `forkProcess`+`setuid` | small but hazardous | Document why not; keep as escape hatch |

The user's framing — "if it's too complicated to segment ACLs/user around an
Op, then at least segment the binary calls" — maps onto L4-vs-L1. The
recommendation is that L1 is not the consolation prize: it is the *better*
answer, because it puts the decision where the information still exists,
whereas L4 puts it where the type has already thrown the information away.

---

## L0: partition by hand, today

Worth writing down because it needs no code and it validates the L3 model
before anyone builds L3. The querying work already landed, so:

```sh
my-salmon config ... > directive.json

# everything the unprivileged builder owns
my-salmon query plan --select '/**/cabal-build/**' --select '/**/git-repo/**' \
  < directive.json > builder.plan
# everything else
my-salmon query plan --exclude '/**/cabal-build/**' --exclude '/**/git-repo/**' \
  < directive.json > root.plan

sudo -u builder my-salmon run up --plan builder.plan < directive.json
sudo             my-salmon run up --plan root.plan    < directive.json
```

The `Plan`'s directive digest is what keeps this honest: both passes are pinned
to the same `directive.json`, so a plan computed against one graph cannot be
silently applied to another.

Two real limitations, which are also the argument for L1/L3:

- **Ordering across the partition boundary is lost.** Each invocation walks its
  own subset with correct internal ordering, but if a root node must run
  *between* two builder nodes, no ordering of the two passes expresses that.
  The partition has to be a topological cut, and nothing checks that it is.
- **Addressing is by path glob**, so a recipe refactor that renames a
  shorthand silently changes which nodes land in which privilege domain.
  Silently mis-partitioning privilege is a bad failure mode. L3 fixes this by
  moving the tag into the recipe.

---

## L1: `RunAs` + `Invoker` — segmenting the binary calls

### The types

```haskell
-- in Salmon.Builtin.Nodes.Binary (or a new Salmon.Builtin.Nodes.Privilege)

-- | Whose identity a command runs under.
data RunAs
  = -- | Inherit the salmon process's own identity. The default; today's
    -- behaviour for every node that doesn't hardcode sudo.
    RunAsSelf
  | -- | Drop (or raise) to another account, by a chosen mechanism.
    RunAsUser !Mechanism !User.User
  deriving (Show, Eq, Ord, Generic)

instance Hashable RunAs

-- | How the switch is performed. Pluggable on purpose: which one is correct
-- depends on what the salmon process already is, and on what the target box
-- has configured.
data Mechanism
  = -- | @sudo -u \<user\> -- \<cmd\> \<args\>@. Works from a non-root caller,
    -- but needs a sudoers rule (and NOPASSWD, since there is no tty).
    ViaSudo
  | -- | @runuser -u \<user\> -- \<cmd\> \<args\>@. Needs the caller to already
    -- be root, but needs no sudoers configuration and cannot prompt.
    ViaRunuser
  | -- | @setpriv --reuid \<uid\> --regid \<gid\> --init-groups -- \<cmd\>@.
    -- Lowest-level; no PAM session at all.
    ViaSetpriv
  deriving (Show, Eq, Ord, Generic)
```

**Recommended default: `ViaRunuser`, not `ViaSudo`.** Salmon's whole premise is
that the process starts privileged. From root, `runuser` is strictly simpler
than `sudo`: no sudoers file to provision (which is itself an unmodelled
dependency today), no PAM password path that could block a headless run
forever, no `env_reset` surprises. `sudo` stays available for the case where
salmon is *not* root and needs to go up. `setpriv` for when even a PAM session
is unwanted.

Note the `--` in every rendering: it terminates option parsing, so a command
whose first argument happens to start with `-` cannot be reinterpreted as a
flag of the wrapper.

### The rewrite

```haskell
-- | Rewrites a rendered command to run under another identity.
applyRunAs :: RunAs -> CreateProcess -> CreateProcess
```

It rewrites `cmdspec` only. `RawCommand path args` becomes
`RawCommand "runuser" (["-u", user, "--", path] <> args)`; a `ShellCommand`
becomes the wrapper invoking a shell with `-c` (and is worth discouraging —
see "Gotchas").

### The carrier: `Invoker`

A `RunAs` alone is not enough, because switching identity has *dependencies*:
the wrapper binary must be installed, and the target account must exist. Those
are graph edges, and salmon already has the vocabulary for graph edges. So the
thing a node asks for is not a bare `Track' (Binary x)` but:

```haskell
-- | Everything needed to invoke a binary: where the binary comes from, whose
-- identity it runs under, and how that identity is provisioned.
data Invoker (x :: Symbol)
  = Invoker
  { invoker_binary :: Track' (Binary x)
  , invoker_runAs :: RunAs
  , invoker_identity :: Track' RunAs
    -- ^ provisions the wrapper binary + the target account; 'ignoreTrack'
    -- when both are pre-existing.
  }

-- | Lift today's signature unchanged: same binary, no identity switch.
asSelf :: Track' (Binary x) -> Invoker x
asSelf t = Invoker t RunAsSelf ignoreTrack

-- | The parallel of 'withBinary', threading the identity through.
withInvoker ::
  Invoker x -> Command x arg -> arg -> ((Reporter Report -> IO ()) -> Op) -> Op
```

`withInvoker` is `withBinaryStdin` with two changes: it applies `applyRunAs` to
the `CreateProcess` before closing over it, and it `inject`s
`run invoker_identity invoker_runAs` alongside the existing binary track. That
second half is the part that earns its keep — it makes "this op needs `sudo`
installed and the `postgres` account to exist" visible in `run tree`/`run dag`
and orderable by `upTree`, instead of being an assumption that fails at
runtime.

`invoker_identity` being a `Track'` (rather than a hardcoded
`Debian.sudo`) is the same convention as the existing rule that recipes must
not bake in a secret-transport mechanism: take the provisioning as a parameter,
let the caller pass `ignoreTrack` when the account and wrapper are already
provisioned out of band. A recipe that hardcodes `Debian.sudo` would be making
the same mistake `psqlAdminRun_Sudo` makes today, just one level up.

### Two different consumers, one type

It is worth being explicit that L1 serves two cases that look similar and are
not:

- **Node-author-fixed identity.** "psql admin commands run as the `postgres`
  account" is intrinsic to the node; no caller should override it. Here the
  node keeps constructing its own `RunAs` internally, and the *only* thing that
  changes versus today is that the mechanism and the dependencies become
  values rather than a string literal in `prepare`.
- **Caller-chosen identity.** "build this cabal target as `builder`" is a
  deployment choice. Here the smart constructor's signature has to change from
  `Track' (Binary "cabal")` to `Invoker "cabal"`.

Only the second forces signature churn, and only on the nodes that want it.

### Migration

There are ~100 `withBinary`/`withBinaryStdin` call sites and ~89
`Track' (Binary …)` parameters in `salmon-ops`. A big-bang migration is neither
necessary nor desirable:

1. Add `RunAs`/`Mechanism`/`applyRunAs`/`Invoker`/`asSelf`/`withInvoker`.
   Nothing changes for anyone; `withBinary` keeps working unchanged and is
   redefined as `withBinary t = withInvoker (asSelf t)`.
2. Fix `Postgres`: delete `psqlAdminRun_Sudo`, keep a plain `psqlAdminRun` that
   renders bare `psql`, and have the admin nodes build
   `Invoker psql (RunAsUser ViaRunuser "postgres") …`. This resolves
   `Postgres.hs:394` and is a good single-module proof of the design.
3. Convert nodes to `Invoker` **only when a caller actually wants to choose** —
   `Cabal`, `Git`, `Npm`, `Spago`, `Web`, `Tar` are the plausible first set
   (the "build stuff" cluster, which is where running as root is least
   defensible). Call sites become `asSelf debianCabal` mechanically.

### `Ref` identity — a correctness requirement, not a nicety

If a node's identity is caller-chosen, **its `mkRef` key must include the
`RunAs`**, otherwise `upTree`'s dedup collapses "clone this repo as `builder`"
and "clone this repo as `ci`" into one node and silently drops the second. This
is easy to get wrong because it only bites in graphs that actually use two
identities for the same command. Rule to state in CLAUDE.md's "Conventions for
node authors": *any node taking an `Invoker` includes its `RunAs` in the `Ref`
key.* Cost is zero when the identity is fixed, so there is no reason to make it
conditional. This is why `RunAs` needs `Hashable`.

### `CommandIO` too

`withBinaryIO`/`CommandIO` (`Binary.hs:163`, used by `WireGuard.privateKey`/
`publicKey` where stdin/stdout need redirecting) builds its `CreateProcess` in
`IO`. `applyRunAs` applies just as well there — it is still a
`CreateProcess -> CreateProcess` at the end. Worth doing at the same time so
the two paths don't diverge, even though no current `CommandIO` user needs an
identity switch.

---

## L2: ownership and mode at file-creation time

L1 lets `psql` run as `postgres`; it does not let `postgres` *read the script*.
Without L2, every L1 identity switch grows its own `chmod a+r`, and the repo
ends up with more instances of the exact hack this spec is trying to remove.

Today `Filesystem.dir`/`filecontents` create paths under the process umask, and
`User.chown` (`User.hs:185`) is a *separate* op whose own haddock says it "does
not itself create the path". Composing them means create-then-chown, which has
a window where the file exists with the wrong owner and mode. Irrelevant for a
migration script; not irrelevant for `Secrets.sharedSecretFile` or `Keys`.

Proposal: an optional ownership/mode decoration applied *at* creation.

```haskell
data Ownership
  = Ownership
  { ownership_owner :: Maybe User.Owner   -- ^ user:group; needs root
  , ownership_mode  :: Maybe FileMode     -- ^ e.g. 0o640
  }

dirWith          :: Ownership -> Directory -> Op
filecontentsWith :: (EncodeFileContents a) => Ownership -> FileContents a -> Op
```

Implemented in-process with `System.Posix.Files`
(`setFileMode`/`setOwnerAndGroup`, or `openFd` with the mode up front) rather
than by shelling out — no subprocess, no `chmod`/`chown` binary dependency, and
the mode can be correct from the moment the inode exists. `dir`/`filecontents`
stay as they are, defined as the `Ownership Nothing Nothing` case.

The `Postgres.adminScript` fix then reads: write the script `0640`
`root:postgres` into the admin dir, drop `ChmodAdminScript` from `PsqlAdmin`
entirely, and the world-readable window disappears along with it.

Open question worth deciding early: should `Ownership` be a *field* on
`FileContents`/`Directory` rather than a separate `…With` constructor? A field
is tidier and forces every call site to consider it; a separate constructor
keeps the ~40 existing `filecontents` call sites untouched. Leaning towards the
separate constructor for the same "additive, no churn" reason as `asSelf`.

---

## L3: declared privilege domains + partitioned execution

L1 handles "this command runs as X". It does not handle "this whole subtree —
including its in-process file writes, its nested `upTree`, its non-subprocess
`up`s — runs as X". If that turns out to be needed, the answer is not to make
`up` polymorphic in identity; it is to run *a second salmon process* under that
identity, and salmon already knows how to do that.

### The tag

Reuse `Extension.dynamics`, which exists for exactly this kind of out-of-band
annotation:

```haskell
newtype PrivilegeDomain = PrivilegeDomain Text   -- e.g. "builder", "root"

inDomain :: PrivilegeDomain -> Op -> Op   -- attaches via dynamics
```

Unlike L0's path globs, this survives recipe refactors: the tag travels with
the node, and `collectDynamics` recovers the partition from an expanded graph.

### The execution

A `--domain` selector on `query plan` resolves the tag to a `Ref` set, giving
exactly the `Plan` files L0 built by hand — but derived from the recipe rather
than from glob spelling. Then either the operator runs the passes (as in L0),
or a driver command does it:

```
my-salmon run up --partitioned < directive.json
```

which, for each domain in topological order, re-execs `/proc/self/exe run up
--plan <domain>.plan` under that domain's `RunAs`, with the directive on stdin.
This is `Self.callSelfAsSudo` (`Self.hs:99`) with the SSH hop removed — the
same "serialize a directive, re-invoke myself under another identity, check the
child's exit code" pattern that is already load-bearing for remote provisioning.

The hard part is not the mechanism, it is the **cut**: the partition must be
topologically consistent, i.e. there must exist an ordering of domains such
that no node in an earlier domain depends on a node in a later one. This is
checkable on the expanded graph and *must* be checked — an unchecked partition
fails as "provisioning silently ran in the wrong order", which is much worse
than a refusal. `run up --partitioned` should refuse to run an inconsistent
partition and print the offending edge.

Note this is strictly more expressive than L1 for the sandboxing question but
strictly *less* fine-grained in ordering: within-domain ordering is exact,
cross-domain ordering is coarse. L1 preserves the exact DAG. That is the real
tradeoff between the two, and it is why L1 should come first.

---

## L4: `forkProcess` + `setuid` per op

The literal `Op -> Op` decorator, for completeness:

```haskell
runOpAs :: RunAs -> Op -> Op   -- wraps `up`/`down` in fork + setgid/setuid
```

The child calls `setGroups`/`setGroupID`/`setUserID` (in that order — dropping
the group after the user is a classic bug, as is forgetting supplementary
groups), runs the original `up`, and `_exit`s; the parent `getProcessStatus`es
and rethrows a non-zero status as an exception so `upTree` sees a `Failed`.

Why it should not be the default:

- **`forkProcess` under the threaded RTS is hazardous.** Only the calling
  thread survives into the child; any lock held by another capability at fork
  time is held forever in the child. Salmon's own `Reporter` machinery and any
  node doing concurrent IO are exactly the things that make this bite. It is
  not "usually fine" so much as "usually fine until a report is being flushed".
- **In-memory effects are lost.** `up`s that mutate an `IORef`, populate a
  `Tracked` value, or run a nested `upTree` (e.g.
  `PostgresMigrations.remoteMigrateOpaqueSetup`'s continuation) do that work in
  a process that then exits. The parent sees only an exit code.
- **It is one-way.** The parent must be root; a node cannot regain privilege.
  Fine as a constraint, but it means the ambient uid still has to be the
  maximum over the graph, which is the thing we set out to avoid.

Keep it documented as an escape hatch for a node whose `up` is genuinely
in-process and genuinely needs another uid. Do not build recipes on it.

---

## Cross-cutting concerns

**Teardown.** At L1 this is free: `down` closes over the same rewritten
`CreateProcess`, so a node torn down runs under the identity that brought it
up. At L0/L3 it is a real gap — `run down --plan` does not exist, and
`specs/advance-querying.md` explicitly listed down-with-plan as a non-goal
because `downTree` has no `prelim`-equivalent. Partitioned teardown therefore
needs its own design; it is not just "same thing, reversed", because the
domain ordering also reverses and the "a failed `down` blocks its
predecessors" containment has to survive being split across processes. Another
reason to prefer L1.

**`serve`.** L1 composes with `Serve` without any thought: the loop stays one
root process and the identity switch happens per subprocess. L3 does *not* — 
`serve` holds the `World` in memory across declarations, and a design that
re-execs a fresh process per domain has nowhere to put that state. If
partitioned execution and `serve` are both wanted, that is a genuine open
design question, not an implementation detail.

**Reporting.** `Binary.Report`'s `CommandStart`/`CommandStopped` carry the
`CreateProcess`, so the rewritten (wrapper-prefixed) command shows up in
reports automatically — which is what we want for auditability: the report
should say what actually ran. The debuggability cost is that a `CommandFailed`
now carries the *wrapper's* stderr, so "user not in sudoers" and "psql syntax
error" arrive through the same channel and look similar. Worth a distinct
report constructor, or at least making `applyRunAs` record the original
`cmdspec` alongside the rewritten one.

**Environment and working directory.** `applyRunAs` rewrites `cmdspec` and
leaves `CreateProcess`'s `env`/`cwd` alone, but the *semantics* of both change
under a wrapper. `sudo` resets the environment by default (`env_reset`), so an
`env` set by `prepare` may not survive; `runuser -` versus `runuser` differ on
whether a login shell is set up. Worse, `cwd` is applied by the parent before
exec, so a `cwd` the root process can enter but the target user cannot (the
`Cabal.build` case — `Cabal.hs` sets `cwd` to the project dir) fails in a
confusing way. Rule for node authors: **a node that sets `cwd` and takes an
`Invoker` must ensure the directory is reachable by the target identity** —
which is a direct dependency on L2 landing.

**Testing.** `Test.PostgresInitSpec` shims `["apt-get", "sudo", "bash",
"chmod"]` inside its container. Recommending `runuser` as the default mechanism
means adding a `runuser` shim; dropping `ChmodAdminScript` at L2 means the
`chmod` shim can eventually go. Both are cheap, but the shim list is a
reminder that mechanism choice is observable by the test harness and should be
settled before the Postgres migration, not after. Beyond that, L1 is unusually
testable without containers: `applyRunAs` is a pure
`CreateProcess -> CreateProcess`, so its renderings (including the `--`
boundary) belong in the cheap in-process test layer.

**Security.** Three things deserve stating rather than assuming:
- The `--` argument boundary is not optional; without it a command argument
  beginning with `-` is parsed by the wrapper.
- `ShellCommand` composed with an identity switch is a quoting hazard —
  `applyRunAs` should either refuse `ShellCommand` or be very explicit about
  what it does with it. `Postgres.CreateDB`'s existing `bash -c` string is the
  one place this already matters.
- Dropping privilege is only meaningful if the lesser user cannot trivially
  regain it. A `builder` account that owns a file the root pass later executes,
  or that can write into a directory root reads from, has not actually been
  contained. L2 is what makes L1's containment real, which is the main argument
  for treating them as one piece of work rather than two.

## Open questions

- Naming: `Invoker`/`RunAs`, or something closer to the todo's own vocabulary
  ("calling preference")? `RunAs` reads well at call sites
  (`RunAsUser ViaRunuser "postgres"`); `Invoker` is the weaker of the two names.
  **Decided (owner, 2026-09-26):** `RunAs`.
- Should `Invoker` also carry the `cwd`/`env` policy (i.e. "this identity
  always runs with this env"), or is that overloading it?
  **Decided (owner, 2026-09-26):** no `cwd`/`env` policy on the `Invoker` in v1.
- Is `Mechanism` per-`Invoker`, or a single deployment-wide default with
  per-node override? A box configures sudoers once; carrying the choice on
  every invoker may be more knob than anyone wants.
  **Decided (owner, 2026-09-26):** a deployment-wide default that an `Invoker`
  may override. The default mechanism is `runuser`.
- Does the `PrivilegeDomain` tag (L3) subsume `RunAs` (L1), i.e. should an
  `Invoker` name a *domain* and let a top-level table map domains to
  identities? That would make L1→L3 a smooth progression rather than two
  mechanisms, at the cost of indirection in the common single-identity case.

## Suggested order of work

1. `RunAs`/`Mechanism`/`applyRunAs` + pure tests on the rendering. No call site
   changes.
2. `Invoker`/`asSelf`/`withInvoker`; redefine `withBinary` in terms of it.
3. L2 `Ownership` on `Filesystem`.
4. Migrate `Postgres`: plain `psqlAdminRun` + an `Invoker`, delete
   `ChmodAdminScript`, closing `Postgres.hs:394`. This is the end-to-end proof
   that L1+L2 solve a real case that exists today.
5. Add the `Ref`-includes-`RunAs` rule to CLAUDE.md's node-author conventions.
6. Convert the build cluster (`Cabal`, `Git`, `Npm`, `Spago`) to `Invoker`, and
   see whether "builds run as `builder`" is now expressible without L3.
7. Only then decide on L3. **Decided (owner, 2026-09-26):** deferred; revisit
   after step 6.
