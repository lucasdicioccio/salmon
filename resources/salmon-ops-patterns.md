# Salmon ops patterns

This is a companion to [`howto-ops.md`](howto-ops.md): where that doc is a
per-primitive cookbook (how to write one node, one `Op`, one CLI binary),
this one collects recurring *shapes* worth reusing across recipes — patterns
that combine several of those primitives to solve a problem that comes up
more than once. Read `howto-ops.md` first if a term here (`Op`, `Track`,
`seed`/`Spec`, `check`) is unfamiliar.

## Pattern: one-time privileged bootstrap, then unprivileged forever after

**Problem.** A recipe needs some action that only root can perform —
granting a Linux capability (`setcap`), installing an OS package, `chown`-ing
a path to a different user — but you don't want every *routine* invocation
of the binary to require root just because *one* op deep in its graph does.
Requiring `sudo` for everything is both a security smell (broader blast
radius than needed) and an ergonomics problem (can't run unattended as a
normal user, breaks non-interactive/CI invocations).

**Shape.** Split the privileged, one-time setup from the routine, repeated
work as two different seeds of the *same* binary, using the existing
seed → spec → ops CLI protocol (`howto-ops.md` §9):

```sh
sudo my-salmon config bootstrap | sudo my-salmon run up   # once per machine
my-salmon config <routine-seed> | my-salmon run up        # every other time, no sudo
```

- The `bootstrap` seed's `Op` graph contains *only* the privileged,
  machine-wide setup: capability grants, package installs, ownership fixes.
  Nothing routine (booting a VM, writing a recipe's actual state) belongs in
  it.
- Every other seed's graph assumes that setup already happened and never
  needs privilege itself.
- **This only works if every op in the bootstrap graph is genuinely
  idempotent** (`howto-ops.md` §4) — re-running `bootstrap` under `sudo`
  later (e.g. after a package upgrade wipes a capability) must be a safe,
  cheap no-op via `check`, not a hazard. If you can't make the bootstrap
  step idempotent, this pattern isn't safe to recommend to users as "run it
  whenever" — treat it as a real migration instead.
- The bootstrap seed usually needs to know *which* unprivileged user/group
  future invocations will run as (to `chown`/grant-to the right identity) —
  take that as an explicit seed argument rather than inferring it from
  `$SUDO_USER`/similar, so `sudo my-salmon config bootstrap --for alice` is
  unambiguous about who it's provisioning for.

**Worked example.** `Salmon.Builtin.Nodes.Capabilities.grantCapabilities`
(`salmon-ops/src/Salmon/Builtin/Nodes/Capabilities.hs`) is exactly this
kind of bootstrap-only op: it needs `CAP_SETFCAP` (in practice, root) to
run, but its `check` (`getcap`-based) makes every subsequent run a
no-op — see its haddock. The qemu test tier
(`specs/qemu-test-vms-progress.md` §0.2) combines it with
`Salmon.Builtin.Nodes.User.chown` into one bootstrap graph, currently
exposed as a standalone fixture binary
(`salmon-ops/fixtures/QemuHostSetupFixture.hs`) rather than a real
`bootstrap` seed on a unified CLI binary — folding it into the latter
shape (a proper `Seed = Bootstrap User | BootVm VmSpec | ...`) is the
natural next step if/when this tier grows a real production binary instead
of remaining test-only support code. `salmon-toy-qemu-pg-ha`
(`salmon-apps/src/QemuPgHaToy.hs`) already has the two-seed shape — a
`prereqs` seed run once under `sudo` (root filesystems, and `/etc/ssh`
handed to the unprivileged user), then `up`/`client` without it — though
its two capability grants are still a documented manual `setcap`, not a
node in `prereqs`.

**Related, but not this pattern:** if the privileged step *isn't* safely
re-runnable (e.g. a real schema migration, a one-shot data backfill), don't
reach for "bootstrap seed" — that's ordinary migration territory
(`Salmon.Builtin.Migrations` reads migration files into a graph;
`SreBox.PostgresMigrations.migrate` runs each as a psql script). Be aware
that salmon records nothing about which migrations were applied: there is no
once-ever guarantee, and every `run up` runs every migration again. So write
each migration to be idempotent (`CREATE ... IF NOT EXISTS`, `ADD COLUMN IF
NOT EXISTS`, a guarded `DO $$ ... $$` block, a backfill with a `WHERE` that
selects only unfinished rows), like any other `up`.

## Pattern: partition by hand, one plan per privilege level

**Problem.** One directive contains nodes that need different identities —
say a build that must run as an unprivileged `builder` user, and the rest
(packages, systemd units) that need root — and you do not want the whole
`run up` under `sudo`. There is no per-node `RunAs` in the tree yet
(`specs/multi-user-privilege-separation.md` sketches it, L1 to L4); until
then you can cut the graph yourself with what `query plan` already offers.

**Recipe.** Compute two `Plan`s from the same directive, one selecting the
builder's nodes and one excluding them, and run each under its own identity:

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

(`my-salmon query tree` shows the declared paths the globs are matched
against.) A plan carries the digest of the directive it was computed from,
so both passes are pinned to the same `directive.json`: a plan computed
against one graph cannot be silently applied to another. Run the pass whose
nodes are depended upon first.

**Limitations.** Both are silent, which is why this is a stopgap and not a
feature:

- **The cut must be topological, and nothing checks it.** Each invocation
  walks only its own subset, with correct ordering inside it. If a root node
  must run *between* two builder nodes, no ordering of the two passes
  expresses that, and salmon will not tell you; check by hand that no node in
  the first pass depends on a node in the second.
- **Addressing is by path glob.** A recipe refactor that renames a shorthand
  silently changes which nodes land in which privilege domain, and a
  mis-partitioned privilege is a bad failure mode. Re-run `query plan` and
  look at what each plan selects after any change to the recipe.

Also note `run down --plan` does not exist, so teardown cannot be partitioned
this way. The tagged-domain design in
`specs/multi-user-privilege-separation.md` (L3) removes the glob and checks
the cut.

## Pattern: secrets are files somebody else put there

Recipes take a secret as a path to a file that already exists, and never as
a value: a directive is printed in reports and piped on standard input, and
a recipe that shipped its own secrets would have picked a transport for
everyone using it. What puts the file there is a separate node the caller
opts into, and there are two.

**From the controlling machine, over ssh** —
`Salmon.Builtin.Nodes.SecretDelivery.uploadSecretFile`:

```haskell
SecretDelivery.uploadSecretFile clientOpts reporter Debian.ssh
    SecretDelivery.SecretUpload
        { uploadSource = "secrets/app/db-password"        -- local, read at up time
        , uploadRemote = Ssh.Remote "salmon" "10.0.0.2"
        , uploadPlacement = SecretDelivery.Placement "/etc/app/db-password" "app" "app" "0600"
        , uploadElevation = SecretDelivery.WithSudo        -- any owner but the login user
        }
```

It belongs wherever the hand-off to the machine is declared; for
`SreBox.Gcp.VmProvision` that is `vmp_beforeCall`, which hands over the
`Ssh.ClientOpts` to use. Inject the node that generates the local file
(`Secrets.sharedSecretFile`, a certificate, ...) so both happen in one pass.

**On the machine itself, from Secret Manager** —
`Salmon.Builtin.Nodes.Gcp.SecretManager.secretFile`, declared in the graph
that runs on the instance, which reads the secret as the instance's own
service account. Only the secret's *name* is in the directive. The instance
needs `roles/secretmanager.secretAccessor` on the secret, the
`cloud-platform` scope and a `gcloud`; the enclosing directory is a
dependency you declare.

What both guarantee, and what a third transport should too:

- the bytes are read when `up`/`check` runs, never when the graph is built,
  so they are not in the directive, `ref`, `help` or `notes`;
- they travel on standard input, never in an argv;
- no report or failure text carries them — **nor a digest of them**. A
  fingerprint in `notes` is what would let `run serve` mark a re-declared
  secret `Stale`, and it is left out deliberately: a digest of a password is
  an offline guessing oracle for whoever can read a report. A changed secret
  is noticed by the node's `check`, which compares the bytes themselves;
- the file is written to a `0600` temporary beside the destination, then
  chowned, chmoded and renamed over it, and a mode granting anything to
  *others* is refused.

`down` removes the delivered file. An upload's `down` treats a machine it
cannot reach as having nothing left to remove, since failing there would
block the teardown of the machine itself.
