# Salmon as PID 1: an init system whose unit graph is a real DAG

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem / goal

An init system that boots a Linux VM, converges an `Op` graph, and then stays
up forever supervising whatever that graph declared — reading a small seed from
`/etc/salmon-init.json` for the handful of per-machine parameters.

Scoping this to **VMs rather than physical machines** is what makes it
tractable, and the repo is already set up for it: `Salmon.Builtin.Nodes.Qemu`
direct-boots a kernel with `-kernel`/`-initrd`, `root=vroot rootfstype=9p`,
`net.ifnames=0` (`Qemu.kernelCmdline`), against a
`Debian.Debootstrap.rootTree` chroot. Hardware is then *known*: virtio
devices, one serial console, no firmware quirks, no disk enumeration race, no
initramfs, no udev rule engine, no network-interface naming ambiguity. And
`VmConfig.vm_extra_kernel_args` already exists as the place `init=/sbin/salmon-init`
would go, which means the qemu test tier from `specs/qemu-test-vms.md` is
also the test bed for this. Physical hardware is explicitly out of scope.

### The build model: a cabal-built init, not a generic one

This is the framing decision that simplifies everything downstream. The
supervisor is **not** a general-purpose init that interprets an arbitrary
machine description at runtime. It is a binary you `cabal build` for a
particular machine role — a seed type, a directive type, and a `Track' Spec`
composing builtins and recipes — exactly like `salmon-migrator` in
`salmon-apps`. The boot graph is *Haskell code*, compiled in and static.

`/etc/salmon-init.json` then carries only what genuinely varies between two
machines of the same role: hostname, addresses, paths to key material, a
service count. Small, and typed by that binary's own seed type.

Consequences worth stating up front, because each removes a whole subsystem
from the design:

- **No generic seed language, no plugin/unit-file loading, no runtime recipe
  discovery.** Changing what a machine runs means rebuilding and redeploying
  the binary, which is a deployment story this repo already has
  (`Self.uploadSelf`, `SreBox.CabalBuilding`).
- **The graph is inspectable at build time**, from a laptop, with the very
  same binary: `run tree` / `run dag` / `query plan` against a candidate seed.
  This is the payoff of `Configure` being a separate hermetic step, and it is
  what lets the "boot order is what I reviewed" test in the Testing section
  exist at all.
- **Reconfiguration-without-reboot stops being load-bearing.** It stays
  achievable (see SIGHUP below) but it is a convenience, not the mechanism by
  which machines change. A machine changes by getting a new binary.

Being "generally static afterwards" also means the supervisor can be built
with as few runtime moving parts as possible — ideally statically linked, so
that PID 1 and the supervisor are the entire userspace trusted base at boot.

## Why this is genuinely attractive (the pitch)

Every init system has a dependency graph. systemd's is `After=`/`Before=`/
`Wants=`/`Requires=` scattered across unit files, and there is no good way to
see it, test it, or reason about it before boot. Salmon's `Op` graph *is* that
graph, as a first-class value, and salmon already has:

- **`upTree`** — topological ordering, dedup by `Ref`, and failure containment
  (a failed node's dependents are `Blocked`, not run against an unmet
  precondition). That is precisely correct boot semantics, already implemented.
- **`downTree`** — teardown in reverse dependency order, where a node becomes
  free only once its *last* dependent is gone. That is precisely correct
  shutdown semantics, already implemented, including the shared-predecessor
  case that a naive walk gets wrong.
- **`run tree` / `run dag`** — you can print and review the boot order, as a
  tree or as Graphviz, *from your laptop*, before booting anything.
- **`query plan --select/--exclude`** — boot a subset. "Boot everything except
  the database" becomes a plan file, not a rescue-shell adventure.
- **`Serve`'s `World`** — a per-node `Direction` + `Convergence` state machine
  across a set of active seeds, with retry of `Errored`/`Blocked` nodes on the
  next pass. That is a supervisor's bookkeeping, already written.

So a surprising amount of this spec is not "build an init system" but "notice
that the init system is mostly already here, and identify the handful of
things that genuinely are not."

## The three things that genuinely are not here

### 1. Salmon has no vocabulary for "keep this running"

`Extension.up :: IO ()` is a one-shot idempotent action that *returns*. An init
system's central job is owning processes that never return. The closest thing
in the repo, `Systemd.systemdService`, does not actually solve this — its `up`
is `systemctl restart`, which returns immediately, and the entire supervision
problem is delegated to systemd. When salmon *is* PID 1 there is nothing left
to delegate to.

**The proposed answer: don't add a new execution model — add an event source.**
Supervision decomposes into exactly the pieces the convergence loop already
has, if the loop is woken by signals instead of by stdin lines:

| Supervision concern | Existing salmon mechanism |
|---|---|
| "is this service running?" | `prelim :: IO Requirement` — `Skippable` if the pid is alive, `Required` if not |
| "start it" | `up` — spawn, record the pid |
| "stop it" | `down` — signal the process group, wait, escalate |
| "it died, restart it" | SIGCHLD marks that node non-`Converged`; the next `converge` pass re-runs `up` |
| "don't restart in a tight loop" | `prelim` returns `Skippable` while inside the backoff window |
| "start things in the right order" | `upTree`'s existing topological walk |
| "a dependency failed" | `Blocked`, existing |

That last-but-one row is the neat one: **restart backoff needs no new control
flow at all**, because `Skippable` already means "not now" and `Serve` already
retries non-converged nodes on the next pass. What it does need is somewhere to
put the counters (see "State that `World` doesn't have" below).

So the delta is: a `Salmon.Builtin.Nodes.SalmonInit.service` node, shaped
*exactly* like `Systemd.systemdService` (same `Config`-ish record: user, group,
umask, exec, restart policy, kill mode, working dir — `Systemd.Service` is
already the right vocabulary and should be reused or generalized rather than
re-invented), whose `up`/`down`/`prelim` talk to the local supervisor instead
of to `systemctl`. **The recipe layer then does not change conceptually at
all** — a recipe ports from systemd to salmon-init by swapping one node.

This is not only an init-system concern, and it should not wait for one:
supervision is a gap in `run serve` today, for the same reason and with the
same fix, and building it there — where it can be exercised interactively
from a shell — is the right order. This spec then consumes it rather than
introducing it.

That work has since moved: a first cut landed as
`Salmon.Builtin.Nodes.Supervised` and was removed again in favour of
`specs/per-node-state-machines.md`, which supplies supervision as a property
every node has rather than as one special node type. Read the paragraphs
below about `prelim`-alive and `Skippable` as describing the shape of the
answer; that spec is where it now lives, and it changes one thing that
matters here — a supervised process's handle lives on its own node's thread,
so the Haskell side never needs a pid table and never needs `waitpid(-1)`,
which is the constraint the two-process split exists to satisfy.

### 2. PID 1 has duties that are not convergence at all

None of these are expressible as `Op`s, and all are non-negotiable:

- **Reaping.** PID 1 inherits every orphan on the machine and must
  `waitpid(-1)` them or the process table fills with zombies.
- **Signals.** PID 1 gets *no default signal dispositions* — the kernel
  discards any signal for which PID 1 has not explicitly installed a handler.
  So it cannot be accidentally killed, but every signal it wants must be
  handled explicitly: SIGTERM/SIGUSR1/SIGUSR2 for the shutdown/reboot
  conventions, SIGINT for ctrl-alt-del (after `reboot(RB_DISABLE_CAD)`),
  SIGCHLD as the supervision event source, SIGHUP for "re-read the seed".
- **Never exiting.** If PID 1 returns, the kernel panics (`Attempted to kill
  init!`). For a program on a managed runtime this is a severe constraint: an
  uncaught exception, or heap exhaustion, is a kernel panic. This is the
  constraint that ends up deciding PID 1's implementation language.
- **Early boot.** Mounting `/proc`, `/sys`, `/dev` (devtmpfs), `/dev/pts`,
  `/run`; `hostname`; loopback up; entropy seed.
- **Shutdown.** SIGTERM to everything, grace period, SIGKILL, `sync`,
  unmount, `reboot(2)` with the right command.

### 3. The bootstrap paradox

A convergence engine cannot converge the preconditions of its own execution.
Salmon's engine needs `/dev/null` (every `CreateProcess` redirect),
`/proc/self/exe` (`Self.readSelfPath_linux`), and a readable, writable root
before it can run `Configure` on `/etc/salmon-init.json` at all. So there is a
**stage 0** that is hardcoded imperative code, deliberately *not* part of the
graph, and it must be kept as small as possible because nothing in it is
inspectable with `run tree`. Drawing that line precisely is a design decision,
not an implementation detail; the proposal is to keep stage 0 to exactly:
mounts, `RB_DISABLE_CAD`, signal handlers, the reaper, and the console.

## The hard engineering problem: `waitpid(-1)` versus the GHC runtime

This deserves its own section because it is the thing most likely to sink a
naive implementation, and it is not obvious.

`Binary.untrackedExec` — which essentially every builtin's `up` goes through —
uses `readCreateProcessWithExitCode`, which ends in `waitForProcess`, which
calls `waitpid` on **one specific pid**. Meanwhile PID 1 must run a reaper that
calls `waitpid(-1, …)` to collect orphans. If the reaper wins the race and
reaps a child that `waitForProcess` is waiting for, the specific-pid `waitpid`
returns `ECHILD` and `untrackedExec` throws — which `upTree` faithfully reports
as a failed node. The result is a provisioning engine that fails randomly under
load, with an error that points at the wrong thing entirely.

The alternative — one unified reaper, where every child (services *and*
`untrackedExec` subprocesses) registers in a `Map ProcessID (MVar ExitCode)`
and a single thread owns `waitpid(-1, WNOHANG)` — would require
`Binary.untrackedExec` to stop using `System.Process`'s own waiting, i.e. an
"exec backend" seam in the most load-bearing module in `salmon-ops`, existing
solely to serve PID 1. Rejected.

## Architecture: a Rust PID 1, a Haskell PID 2

```
PID 1  salmon-init        Rust. Tiny, boring, cannot panic:
       (Rust, static)     stage-0 mounts, signal handlers, THE reaper,
                          spawns/kills/setuids service processes,
                          control socket, reboot(2)
                            |
                            | spawn requests / exit events over a socketpair
                            v
PID 2  <role>-supervisor   Haskell, cabal-built per machine role: reads the
       (Haskell, static)   seed, runs Configure, expands the graph, runs the
                           Serve-style World + supervision state, uses
                           System.Process normally
```

**Why the split.** It is not primarily about language:

- **The waitpid conflict disappears.** PID 1's children are the supervisor,
  the getty, and the services. The supervisor's own `untrackedExec`
  subprocesses are *its* children, reaped by `System.Process` as usual. Two
  reapers, disjoint sets, no race.
- **A crash in the complicated half is no longer a kernel panic.** The
  convergence engine is where all the intricate code lives (JSON, graph
  expansion, subprocess management, arbitrary recipe `up` actions) and
  therefore where the bugs live. If it dies, PID 1 restarts it.
- **The supervisor is restartable and upgradable in place.** Because
  **services are children of PID 1, not of the supervisor**, the supervisor can
  crash, be restarted, or be swapped for a new binary without any running
  service being orphaned or killed. This is what makes redeploying a rebuilt
  supervisor a non-event, which matters a lot given the build model above. It
  is also the reason spawning must live in PID 1 rather than in the
  supervisor, even though that is the less obvious place to put it.

**Why Rust for PID 1.** The split makes PID 1 so small and so tightly
constrained — must never exit, must never block indefinitely, must not depend
on a heap it can exhaust — that a managed runtime buys nothing there and
costs the two failure modes that matter most: an uncaught exception and heap
exhaustion are both kernel panics in PID 1. Rust removes both by construction:
no GC, no runtime to fail to initialize before `main`, `#![deny(panic)]`-style
discipline enforceable in review, and a genuinely static binary
(`x86_64-unknown-linux-musl`) with no loader dependency at a point in boot
where the dynamic loader's own assumptions are shakiest. C would also work;
Rust is preferred, and the safety argument is mostly about the reaper and
signal-handling code — the exact code where C's classic PID-1 bugs
(`EINTR` handling, signal-unsafe calls in handlers, races on the pid table)
live.

The Rust half is deliberately *not* extensible: it knows how to mount, reap,
spawn with a uid/gid/pgid, signal, rate-limit respawns, and reboot. Every
decision about *what* to run and *in what order* is on the Haskell side. If a
change requires touching the Rust binary, that is a signal the boundary is in
the wrong place.

The one deliberate exception is restart backoff, which lives on **both** sides —
see the next section, since it is the one place the boundary is crossed on
purpose and therefore the one place it can go wrong.

**The cost is a protocol** between the two: `Spawn`/`Signal`/`Query` requests
and `Exited pid status` events. It is small, it must be stable (a supervisor
restart must not require a PID 1 restart), and it is worth noting it is *the
same shape* as the `SalmonInit.service` node's `up`/`down`/`prelim` — so the
node can talk to the control socket directly and the protocol only has to be
designed once. Length-prefixed JSON over a `SOCK_SEQPACKET` socketpair is
almost certainly enough; the temptation to make it clever should be resisted,
because every feature in this protocol is a feature the Rust half has to grow.

## Restart backoff: PID 1 rate-limits, the supervisor decides

Backoff lives on both sides, with different jobs. Getting this division wrong
is how you end up with multiplicative delays and a machine that takes twenty
minutes to bring a service back, so it is worth being precise:

- **PID 1's backoff is a safety property, not a policy.** It is a per-slot
  minimum interval between respawns of the same thing, applied to *everything*
  PID 1 spawns. Its purpose is to keep the machine alive and loggable when
  whatever is above it is broken or absent — including the case that has no
  other answer at all: **a crash-looping supervisor**, where there is no
  Haskell side left to consult. It never decides *whether* something should
  run.
- **The supervisor's backoff is policy.** Per-service, dependency-aware, with
  windows and a give-up latch, and expressible in the graph.

### How they compose instead of fighting

The rule is that **PID 1 defers, it never refuses**, and the supervisor
*asks* rather than duplicating the timer.

When a spawn request arrives sooner than the slot's minimum interval allows,
PID 1 schedules it rather than rejecting it, and answers the request with
`Deferred { until }`. A `Query` on that slot then answers "not running, spawn
deferred until T" — which maps exactly onto `Skippable` in the existing
`prelim` vocabulary. So the `SalmonInit.service` node's `prelim` returns
`Skippable` while PID 1 says deferred, the convergence pass leaves the node
alone, and the next pass picks it up once the deferral expires. The two
backoffs compose into `max(pid1_floor, supervisor_policy)` rather than summing,
and the supervisor needs no timer of its own for this case.

Deferring rather than refusing matters: a refusal that a buggy supervisor drops
on the floor means the service never comes back, whereas a deferral is
self-healing. The cost is that PID 1 holds a small timer queue — bounded by the
number of slots, which is bounded by the config file, so it is not an unbounded
allocation.

### The supervisor slot is special

If the supervisor itself crash-loops, PID 1 must **cap the backoff, never give
up**. A give-up latch is right for a service and catastrophic for the
supervisor — a machine whose supervisor has permanently stopped being restarted
is a machine with no way back. So: exponential growth up to a ceiling, then a
steady retry at the ceiling forever, and after N failures in a window PID 1
writes prominently to the console (the getty is already up from stage 0, so
there is somewhere to write to). Visible and still trying, never silent and
stopped.

### The config file

PID 1 reads `/etc/salmon-init.conf` in stage 0. This is a *different* file from
`/etc/salmon-init.json` — the latter is the supervisor's seed, typed by that
role's Haskell seed type; this one is PID 1's own knobs and is read by the
Rust half only. Keeping them separate keeps the Rust half from ever needing to
understand a role's schema.

Non-negotiable properties, all of which follow from "PID 1 must always boot":

- **Missing or unparseable is not an error.** Compiled-in defaults apply; a bad
  line is reported to the console and skipped; a bad file is ignored wholesale.
  PID 1 must never fail to boot because of its own config.
- **The format is boring on purpose.** Flat `key = value` lines with `#`
  comments and a `[slot.<name>]` grouping for per-slot overrides. Not TOML, not
  JSON, not YAML: every parser is panic surface inside the trusted base, and
  this file has maybe a dozen keys. A hand-written line parser is a feature.
- **Kernel-cmdline override.** PID 1 reads `/proc/cmdline` and honours e.g.
  `salmon_init.backoff=off`, so a config that makes the machine effectively
  unbootable (an enormous ceiling, say) can be escaped from the bootloader or
  the qemu `-append` without editing a filesystem you may not be able to reach.
  Cheap to implement, and the kind of escape hatch you only regret not having
  once.
- **Re-read on SIGHUP**, keeping the previous values if the new file does not
  parse.

The knob set should stay small — every knob is Rust surface:

```
# defaults for every slot
initial_delay   = 100ms
multiplier      = 2.0
max_delay       = 30s
stable_after    = 10s     # ran this long => reset backoff to initial_delay
report_after    = 5       # failures in a window before shouting to the console

[slot.supervisor]
max_delay       = 5s      # come back fast; never give up
give_up         = never
```

`give_up = never` being expressible — and being the supervisor's default —
is the config-level statement of the previous section's rule.

### Clock

Backoff must use `CLOCK_MONOTONIC`. At early boot the wall clock is whatever
the RTC said, and it will jump when time sync happens; a backoff computed
against wall time can silently become a multi-hour deferral the first time NTP
corrects a skewed guest clock. This is a small detail with a very confusing
failure mode, which is why it belongs in the spec rather than in the code
review.

## Repo consequences

This puts a non-Haskell component in a cabal
multi-package project. The `salmon-init` Rust crate does not belong in the
`cabal.project` package set; the plausible shape is a sibling directory with
its own `Cargo.toml`, built independently, with the Haskell side depending on
it only at *image assembly* time (the `Debootstrap` chroot gets a
`/sbin/salmon-init` binary). That keeps `cabal build all` unaffected — which
matters, given the project already splits packages specifically to keep build
times down.

## The calling convention

The kernel execs init with argv derived from the kernel cmdline; unrecognized
cmdline words are passed through as argv/env. That is the reason none of this
can be a fourth `Command` constructor: `execCommandOrSeed`'s entire contract is
argv parsing, and argv is exactly what is unavailable here. These binaries are
separate-enough things and should not pretend otherwise.

- **PID 1 (Rust) takes no arguments and parses none.** It refuses to run unless
  `getpid() == 1` (behind an explicit `--pretend` for development), because
  doing this by accident on a workstation would be memorable.
- **The supervisor (Haskell) has its own entry point**, not
  `execCommandOrSeed`. It is exec'd by PID 1 with a fixed argv, and reads its
  seed from a path, not from flags.
- **The same supervisor binary is also the client** when run from a shell:
  `<role>ctl status`, `converge`, `reboot`, `up <seed args>` — talking to the
  control socket, exactly as `systemctl` relates to `systemd`. This is worth
  keeping in one binary because the client is the thing that needs to *know the
  graph* in order to resolve `--select` patterns and print sensible node names,
  and that knowledge is compiled in.
- **Two sockets, not one.** PID 1's socket speaks only the small mechanical
  protocol (spawn/signal/query/reboot). The `serve`-language socket is the
  supervisor's own. Keeping them separate is what allows `reboot` and
  `poweroff` to work even when the supervisor is wedged or restarting — which
  is precisely when you need them.

Usefully, the *same* cabal-built binary still supports the ordinary
`config`/`run tree`/`run dag`/`query plan` surface when invoked normally on a
developer machine — that is how the boot graph gets reviewed before it is ever
booted. So the binary has three personalities (init supervisor, control client,
ordinary salmon CLI) but only the first is entered without argv.

### The control socket speaks the `serve` language

`Serve.parseServeCommand` already defines a line-oriented language —
`up`/`only`/`down`/`up-directive`/`clear`/`converge`/`status`/`history`/
`query`/`load`/`help`/`quit` — with `--select`/`--exclude` on the query-ish
ones. That is a remarkably good fit for an init control interface, and reusing
it verbatim means the interactive story is done. Additions needed:
`reboot`, `poweroff`, `reload` (re-read `/etc/salmon-init.json`), and a
`logs`-ish affordance. `quit` obviously has to mean something different (or
be rejected) when quitting is a kernel panic.

## Boot sequence

```
kernel → exec /sbin/salmon-init (pid 1, Rust)
  stage 0 (hardcoded, small, cannot panic)
    reboot(RB_DISABLE_CAD); install signal handlers; start reaper
    mount /proc /sys /dev(devtmpfs) /dev/pts /run
    read /etc/salmon-init.conf  ──failure──> compiled-in defaults, carry on
    read /proc/cmdline for salmon_init.* overrides
    open /dev/console; lo up
    spawn a getty on ttyS0            <- unconditional, before anything can fail
    open the PID-1 control socket
    fork/exec the supervisor          <- restarted by pid 1, with backoff,
                                         capped, never giving up
  supervisor (pid 2, Haskell)
    read /etc/salmon-init.json  ──failure──> report + leave the getty running
    Configure IO seed directive ──failure──> report + leave the getty running
    expand to Op graph, seed the World with it, converge
    open the serve-language socket
    then: block on { exit events from pid 1, control socket, timers }
          → converge again
```

Note `sethostname` is *not* in stage 0: it is a per-machine parameter, so it
comes from the seed and belongs in the graph. The rule for what stays in stage
0 is "things the engine needs in order to run at all", not "things that happen
early".

Two properties worth calling out:

- **The getty comes up before anything that can fail.** If the seed is missing,
  malformed, or configures to a graph that fails to converge, the machine must
  still be a machine you can log into. systemd's `emergency.target` exists for
  this reason and is worth copying. "Rescue" here is not a mode — it is just
  "the console got spawned in stage 0 and convergence didn't work", which
  requires no extra machinery.
- **Re-reading the seed on SIGHUP is reconfiguration without reboot**, and it
  is nearly free: it is `Serve`'s `only <new seed>` path, which retires the old
  seed and converges the difference — teardown of what is no longer wanted,
  bring-up of what is newly wanted, using `downTree`/`upTree`'s existing
  ordering. Getting that property essentially for free is a good sign the
  `Serve` model is the right foundation. Given the build model, this only ever
  re-reads *parameters*, never a new graph shape; a new graph shape arrives as
  a new supervisor binary, and PID 1 restarting the supervisor is the same
  code path as PID 1 restarting a crashed one.

## Why the VM assumption buys so much

Spelled out, because each of these is a subsystem that does *not* have to be
written:

| Physical-machine problem | Why it disappears in the target VM |
|---|---|
| initramfs, early module loading | `-kernel`/`-initrd` direct boot with virtio built in; `Qemu.kernelCmdline` already does `root=vroot rootfstype=9p rootflags=trans=virtio rw` |
| udev, device naming rules | devtmpfs gives the kernel-created nodes; the device set is fixed and known |
| network interface naming | `net.ifnames=0 biosdevname=0`, already in `kernelCmdline` |
| disk enumeration, fsck, LVM, LUKS, mount ordering | 9p root, no block devices to speak of |
| firmware/ACPI quirks, suspend/resume | not modelled at all |
| console detection | always `console=ttyS0`, already in `kernelCmdline` |
| verifying a reboot actually happened | `VmConfig.vm_monitor_socket` gives an out-of-band channel to observe and to force-reset a wedged guest |

The last row is the one that makes this *testable* rather than merely
buildable, and it is why the qemu tier is the natural home for the tests.

## State that `World` doesn't have

`Serve.NodeState` carries `nodeDirection`/`nodeConvergence`/`nodeEpoch`.
Supervision needs more, and it should go in a **separate supervisor table keyed
by `Ref`** rather than by widening `NodeState` (which is shared with the
one-shot `serve` path that has no notion of a running process):

- the live `ProcessID` and its process-group id,
- restart count and the window it is counted over,
- next-eligible-restart time (the backoff that `prelim` consults),
- a "gave up" latch, so a service that crash-loops stops being retried and is
  reported instead of consuming the machine.

Under the init system the third of these is partly answered by PID 1 rather
than stored: a `Query` can come back "deferred until T", which `prelim` turns
straight into `Skippable`. The supervisor still keeps its own next-eligible
time, because its policy is usually the longer of the two and because plain
`run serve` (no PID 1 underneath) has nobody to ask. See "Restart backoff"
above for how the two compose.

**Also a real problem, now fixed: `World.worldHistory` was append-only
forever.** Correct and cheap for a `serve` session measured in minutes; for a
supervisor on a box that stays up for months it was an unbounded leak, since
it retained a whole `Cofree Graph Op` per epoch. It was replaced (`885d9f0`)
by a retention policy: `worldEpochs` keeps only the graphs a future pass could
still walk — the active ones, plus retired ones that still describe a node to
turn down — and `worldLog` keeps the small per-declaration lines `history`
prints, so the record outlives the graphs. `specs/per-node-state-machines.md`
would replace even that, since a magma keyed by `Ref` needs no per-declaration
graph at all.

Both of these were `Serve` problems before they were init problems. The
retention fix in particular landed on its own merits; a long-running `serve`
had the same leak, it was just less likely to be noticed. Note that the split architecture softens the consequence — heap
exhaustion in the supervisor is a supervisor restart, not a kernel panic — but
a supervisor that restarts every few weeks and loses its convergence state is
not a system anyone should ship.

## Process lifecycle details worth deciding early

- **Process groups.** Each service gets its own session (`setsid`) so it can be
  killed as a group. This is `Systemd.KillMode`'s `Process` vs a
  `control-group` equivalent; without cgroups, the pgid is the available
  approximation and is good enough for the VM case. cgroup v2 (for real
  containment and for resource limits) is a plausible v2, not a v1.
- **Dropping privilege at spawn.** `Systemd.Service` already carries
  `service_user`/`service_group`/`service_umask`. Under salmon-init these stop
  being rendered into a unit file and travel over the spawn protocol, becoming
  actual `setgroups`/`setgid`/`setuid` calls made by PID 1 in the forked child
  before `exec` — in that order, since dropping the group after the user is a
  classic privilege-escalation bug, as is forgetting supplementary groups.
  This is the same operation `specs/multi-user-privilege-separation.md`
  proposes as `applyRunAs`, applied at spawn rather than by wrapping a command.
  The two specs share the `RunAs` *vocabulary* on the Haskell side, but not the
  implementation: here the fork-and-setuid that spec cautions against is the
  correct approach, because it is a fresh fork in a non-GC'd runtime whose only
  job is to exec — none of the hazards that make it a bad idea inside the
  Haskell engine apply.
- **Stdout/stderr.** Services need somewhere to write. v1: a per-service file
  under `/run/salmon-init/log/<name>`, opened by PID 1 before exec. A journal
  is not v1.
- **Readiness.** `Type=simple` only (as `Systemd.ServiceType` already is):
  "spawned" means "started". Notify/readiness protocols are a v2 concern, and
  the honest v1 story for ordering against readiness is "the dependent node's
  `prelim`/`up` retries until the dependency answers" — which the convergence
  loop already does, and which is arguably more robust than a readiness
  protocol anyway.
- **Shutdown ordering.** `downTree` gives correct reverse-dependency teardown,
  but a real shutdown also needs a global deadline: converge-down with a
  timeout, then SIGTERM everything remaining, then SIGKILL, then `sync` and
  `reboot(2)`. The deadline cannot live in the graph.

## Testing

This is unusually testable for something this invasive, because the harness
already exists:

1. `Debootstrap.rootTree` + `ensureVm9pBoot` produce the chroot; add the Rust
   `salmon-init` at `/sbin/salmon-init`, the cabal-built role supervisor, and a
   `/etc/salmon-init.json`. Image assembly is the one place the two build
   systems meet, so it is worth making it an `Op` like everything else rather
   than a shell script beside the tests.
2. Boot it with `vm_extra_kernel_args = ["init=/sbin/salmon-init"]` — note
   `Systemd.render_service`'s `quoteArg` already handles the `-append` quoting
   trap that bit `Qemu` once (documented in `specs/qemu-test-vms-progress.md`).
3. Assert over the serial console and over SSH: services running, boot order as
   `run tree` predicted, `salmon-init status` agrees, a killed service comes
   back, a crash-looping service gives up rather than spinning, SIGHUP with a
   new seed converges the difference, `poweroff` actually powers off (observable
   on the monitor socket).

The "boot order matches what `run tree` printed on the developer's laptop" test
is the one that justifies the whole design, and it is only possible *because*
config generation and execution are already separate hermetic steps.

## Non-goals (v1)

Physical hardware; initramfs generation; udev; socket/dbus activation; cgroup
resource control; timers (`CronTask` exists but needs cron, and a timer
subsystem is its own spec); a journal; user sessions/logind; SELinux/AppArmor;
`/dev/initctl` compatibility; being a drop-in systemd replacement for an
existing distro's unit corpus; and — per the build model — any form of runtime
extensibility: no unit-file directory, no plugin path, no way to add a service
without rebuilding the supervisor. The target is a machine whose entire
configuration is one cabal-built binary plus a small parameter file, not a
machine that also has to run somebody else's units.

## Open questions

- **Is the supervisor's own restart policy declarable?** Services are declared
  *by* the graph but supervised by PID 1, which also supervises the supervisor.
  So the one process whose restart behaviour cannot be expressed in the graph
  is the one that owns the graph. Probably fine and unavoidable; worth being
  deliberate rather than discovering it.
- **How small can stage 0 actually get?** Every line in it is a line that
  `run tree` cannot show you. Is there a defensible way to express the mounts
  as ops that run under a degraded engine, or is hardcoding them honest?
- **Seed vs directive on disk.** `/etc/salmon-init.json` holding the *seed*
  means `Configure` runs in IO at boot — flexible, but it can fail at boot in a
  way a pre-computed directive could not. Supporting both (seed, plus an
  optional cached directive alongside) hedges this and mirrors `serve`'s
  existing `up` vs `up-directive` distinction. Note the seed is JSON here, so
  it needs `FromJSON seed`, not the `ParseRecord seed` that `serve` uses.
- **How static is "static"?** A statically-linked GHC binary is achievable but
  not free, and it interacts with what the recipes actually shell out to: a
  supervisor with no dynamic loader still needs `psql`, `ip`, `nft` and friends
  present in the image. Worth deciding whether the goal is a static supervisor
  or a *minimal, known* image — they are different targets, and the second is
  probably the real one.
- **Interaction with `specs/multi-user-privilege-separation.md`.** That spec's
  L1 (`RunAs`/`Invoker`) and this one's service-spawn privilege drop want to be
  the same vocabulary — except the drop happens in Rust here, so what is shared
  is the *type* on the Haskell side and its serialization into the spawn
  protocol, not the implementation. Doing that spec first makes this one
  smaller.

## Suggested milestones

Each is independently useful and independently testable, which matters a lot
for something whose failure mode is "the VM does not boot".

**In `Serve`, ahead of any of this** (both worth doing on their own merits):

0a. **Supervision in `run serve`** — exercised interactively from a shell,
    where a wedged loop costs nothing, before anything depends on it to boot.
    This is milestone 4 below, built somewhere debuggable first. Superseded in
    approach by `specs/per-node-state-machines.md`: a first cut shipped as
    `Nodes/Supervised` and was removed (`579f435`) in favour of it.

0b. **`worldHistory` retention** — done (`885d9f0`); see above.

**Then the init system itself:**

1. **`salmon-init` (Rust) as a dumb-init.** Stage 0 + reaper + signals + getty
   + `reboot`/`poweroff`, no convergence at all, boots to a shell in a qemu VM.
   Proves the PID-1 mechanics and the test harness end to end, and establishes
   the Rust build/packaging path into the `Debootstrap` image.
2. **The spawn protocol, `/etc/salmon-init.conf`, and PID-1 backoff**, with a
   hardcoded service list and the supervisor slot. Proves the process topology
   — "kill the supervisor, services survive, supervisor comes back and
   re-adopts", which everything else leans on — and, separately, that a binary
   that exits immediately gets rate-limited to the configured ceiling instead
   of spinning, that a missing/corrupt conf still boots on defaults, and that
   `salmon_init.backoff=off` on the kernel cmdline overrides the file. That
   last set is cheap to test and is exactly the behaviour nobody exercises
   until the day it matters.
3. **`SalmonInit.service` node + seed reading + one converge pass.** First real
   boot of a cabal-built role supervisor from `/etc/salmon-init.json`.
4. **Supervision proper in the supervisor**: exit-event-driven convergence,
   restart policy, backoff, give-up latch — i.e. 0a wired to the real event
   source.
5. **Reconfiguration**: SIGHUP re-read of parameters, and supervisor binary
   replacement without dropping services.
6. **Ordered shutdown**: `downTree` with a global deadline, then `reboot(2)`.
