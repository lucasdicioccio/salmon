# Salmon ops patterns

This is a companion to [`howto-ops.md`](howto-ops.md): where that doc is a
per-primitive cookbook (how to write one node, one `Op`, one CLI binary),
this one collects recurring *shapes* worth reusing across recipes — patterns
that combine several of those primitives to solve a problem that comes up
more than once. Read `howto-ops.md` first if a term here (`Op`, `Track`,
`seed`/`Spec`, `prelim`) is unfamiliar.

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
  cheap no-op via `prelim`, not a hazard. If you can't make the bootstrap
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
run, but its `prelim` (`getcap`-based) makes every subsequent run a
no-op — see its haddock. The qemu test tier
(`specs/qemu-test-vms-progress.md` §0.2) combines it with
`Salmon.Builtin.Nodes.User.chown` into one bootstrap graph, currently
exposed as a standalone fixture binary
(`salmon-ops/fixtures/QemuHostSetupFixture.hs`) rather than a real
`bootstrap` seed on a unified CLI binary — folding it into the latter
shape (a proper `Seed = Bootstrap User | BootVm VmSpec | ...`) is the
natural next step if/when this tier grows a real production binary instead
of remaining test-only support code.

**Related, but not this pattern:** if the privileged step *isn't* safely
re-runnable (e.g. a real schema migration, a one-shot data backfill), don't
reach for "bootstrap seed" — that's ordinary migration territory
(`Salmon.Builtin.Migrations`), which has its own once-ever semantics instead
of `prelim`-based idempotency.
