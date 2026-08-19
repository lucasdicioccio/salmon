# Lighter-weight process isolation for services: systemd hardening, bubblewrap, firejail

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem

Every service recipe in this repo today (`PgBouncer.setup`, `Nginx.setup`,
`SreBox.Postgrest.setupPostgrest`, and the "budgetz"-style app instances
sketched in `specs/pg-ha-control-plane.md`) runs as a plain
`Systemd.systemdService` unit: a `[Service]` stanza with `User`/`Group`/
`WorkingDirectory`/`ExecStart` and nothing else — `Systemd.Service`
(`Salmon.Builtin.Nodes.Systemd`) has no isolation directives at all today.
The only *heavier* isolation option this project has is
`Salmon.Builtin.Nodes.Podman` — a full container (own image, own network
namespace by default, its own lifecycle/pull/build machinery).

That leaves a wide gap on the isolation spectrum for exactly the case the
[[pg-ha control plane]] spec's app-instances (`budgetz.a.0`, `postgrest.b`,
...) sit in: several independent, mutually-untrusting-ish app instances
sharing a machine (the "shared tier" from that spec), each with its own
pg-user/secret/connstring, where "plain systemd service, wide open to the
whole filesystem" is more privilege than any of them need, but "full podman
container per instance" is real overhead (image management, a container
network to wire into the bouncer/LB story, slower iteration) for something
that's really just one Haskell/whatever binary reading one config file and
talking to one upstream port.

## What "containerization mode" actually means here — three tiers, not one

Firejail and bubblewrap are both namespace-based sandboxing tools, but
they're worth evaluating against a third option that isn't a separate tool
at all: **systemd's own per-unit sandboxing directives**, which every
recipe in this repo already goes through `Systemd.systemdService` for.

| Tier | Mechanism | New binary needed? | Isolation granularity |
|---|---|---|---|
| A. systemd hardening | `[Service]` directives (`ProtectSystem`, `PrivateTmp`, `NoNewPrivileges`, `CapabilityBoundingSet`, ...) | No — systemd already runs every service | Coarse-to-medium: named policy levels + path allow-lists, no arbitrary bind-mount graph |
| B. bubblewrap (`bwrap`) | Unprivileged (user namespaces), wraps the command in an argv of explicit `--bind`/`--ro-bind`/`--unshare-*` flags | Yes, `bubblewrap` | Fine-grained: explicit bind-mount list, explicit namespace unshares, nothing implicit |
| C. firejail | Wraps the command via a setuid-root helper, driven by profile files (built-in per-app profiles or custom `.profile`s) | Yes, `firejail` | Coarse (profile-driven) unless a custom profile is authored |

Recommendation: **build A first, B second, treat C as documented-but-not-
default**. Reasoning:

- **A costs nothing new.** Every existing service recipe already routes
  through `Systemd.systemdService` — adding hardening fields to
  `Systemd.Service` benefits `Nginx`/`PgBouncer`/`Postgrest`/budgetz
  immediately, with no new dependency to install, detect, or version-pin.
  For a same-machine "shared tier" deployment where the main worry is one
  app instance reading another's secret file or binding another's port,
  `ProtectSystem=strict` + `ProtectHome=yes` + a `ReadWritePaths=`
  allow-list + `NoNewPrivileges=yes` already closes most of that gap.
- **B is the natural next step when A's directive vocabulary isn't
  expressive enough** — e.g. a precise, arbitrary bind-mount graph (not
  just "read-only except these paths"), or wanting the same sandboxing
  outside a systemd context at all (ad hoc test tooling, the qemu test
  harness in `specs/qemu-test-vms.md`). `bwrap` has no daemon, no setuid
  binary (modern kernels: unprivileged user namespaces), no profile files
  to keep in sync with a recipe's actual needs — every flag is explicit on
  the command line, which matches this project's "everything explicit,
  typed" philosophy better than firejail's profile-file model.
- **C (firejail) is real but has a worse security-model fit and a worse
  fit for this codebase's conventions**: it's a setuid-root binary with a
  meaningful CVE history specifically in that setuid helper (privilege-
  escalation bugs recur because the design *requires* root to set up the
  sandbox before dropping it) — a bigger trust footprint than bwrap's
  unprivileged-by-default model for a project that otherwise runs
  everything as an explicit, auditable command. Its profile-file model
  (`/etc/firejail/*.profile`, whitelist/blacklist directives in their own
  DSL) also doesn't compose with typed Haskell values the way this
  project's other builtins do — a `Firejail` node would either shell out to
  an existing hand-authored `.profile` (opaque to salmon, un-typed) or
  reimplement enough of firejail's DSL as Haskell types to be worth it,
  neither of which is attractive. Worth supporting *if* there's a concrete
  reason (an existing firejail profile someone already relies on, a
  specific feature bwrap lacks), but not the default recommendation.

## Kernel namespaces in play

Both bwrap and systemd's hardening directives are front-ends over the same
underlying kernel mechanism (`unshare(2)`/`clone(2)` with `CLONE_NEW*`
flags) — worth being explicit about each namespace on its own, since "turn
on sandboxing" is really several independent, separately-costed decisions,
not one knob. `ip netns`/`Salmon.Builtin.Nodes.LinuxBridge` (the qemu spec)
is the same network-namespace machinery again, one more data point that
this project already relies on namespaces elsewhere.

| Namespace | Isolates | bwrap flag | systemd directive | Relevant to an app instance here? |
|---|---|---|---|---|
| Mount (`CLONE_NEWNS`) | The process's view of the filesystem tree | Implicit — bwrap's whole `--bind`/`--ro-bind` model *is* a mount namespace; there's no "don't use one" | `ProtectSystem=`, `ProtectHome=`, `PrivateTmp=`, `ReadWritePaths=`/`ReadOnlyPaths=`/`InaccessiblePaths=` (all mount-ns-backed) | **Yes, the main one.** This is what stops one app instance from reading another's secret file — the whole point of §Tier A/B. |
| PID (`CLONE_NEWPID`) | Process ID space; can't see or signal processes outside the namespace | `--unshare-pid` | `RestrictNamespaces=`+`PrivatePIDs=` isn't a real systemd directive — systemd doesn't PID-namespace a unit's main process by default and has no single directive that does; would need pairing with bwrap or a raw `unshare --pid` wrapper | Mild value (can't `kill -9` a sibling instance, can't read its `/proc/<pid>/environ`), but **note the systemd interaction**: systemd's own process supervision (knowing whether the service is still alive, restart-on-failure) tracks the unit via **cgroups**, not PID visibility, so PID-namespacing a systemd-managed process doesn't break `systemctl status`/restart semantics the way it might naively seem to. Still, this is a place where Tier A alone can't reach — a reason Tier B exists. |
| Network (`CLONE_NEWNET`) | Network interfaces, routing table, port bind space | `--unshare-net` | `PrivateNetwork=yes` | **The one with a real tradeoff here** — see below. |
| UTS (`CLONE_NEWUTS`) | Hostname/domainname | `--unshare-uts` (pair with `--hostname`) | `ProtectHostname=yes` | Cosmetic mostly; low priority, cheap to turn on (no functional downside for a service that doesn't self-report its hostname anywhere meaningful). |
| IPC (`CLONE_NEWIPC`) | System V IPC objects, POSIX message queues | `--unshare-ipc` | `PrivateIPC=yes` (systemd ≥247) | Low relevance for a TCP-speaking app instance; cheap to turn on, no known downside for the recipes in scope. |
| User (`CLONE_NEWUSER`) | UID/GID mapping — what makes *unprivileged* bwrap possible at all (root inside the namespace maps to an unprivileged UID outside) | `--unshare-user` (bwrap uses this internally by default on modern kernels even without the flag spelled out, to be able to do the rest unprivileged) | `PrivateUsers=yes` (systemd ≥232+, with caveats around filesystem UID mapping) | This is the mechanism, not really an independent policy choice for Tier B — it's *why* bwrap doesn't need setuid. Worth calling out precisely because it's the structural reason Tier B was recommended over Tier C (§ above): firejail's setuid helper does its privilege drop a different, riskier way. |
| Cgroup (`CLONE_NEWCGROUP`) | View of the cgroup hierarchy | `--unshare-cgroup` | Not directly; `ProtectControlGroups=yes` covers the read-only-ness of `/sys/fs/cgroup` via the mount namespace instead | Minor hardening (hides host cgroup layout); low priority. |
| Time (`CLONE_NEWTIME`, Linux ≥5.6) | Per-namespace offsets for `CLOCK_MONOTONIC`/`CLOCK_BOOTTIME` | `--unshare-all` pulls it in on new-enough bwrap; no dedicated flag on most bwrap versions in the wild yet | No dedicated directive | Essentially never relevant to anything in this repo (it exists for container-migration/checkpoint use cases). Not worth spending design effort on. |

### The network-namespace tradeoff, specifically

This is the one namespace decision that actually changes behavior for the
app-instance use case, so it's worth its own paragraph rather than just a
table row. `--unshare-net`/`PrivateNetwork=yes` gives the sandboxed process
*only* a loopback interface with no route out — no connectivity to the
bouncer, the LB, or anything else, until something explicitly re-provides
it:

- **Don't unshare the network at all** (this spec's leaning, restated from
  the open question below): rely on Tier A's `RestrictAddressFamilies=`
  (still lets a process talk `AF_INET`/`AF_INET6`/`AF_UNIX` freely, just
  can't open e.g. `AF_PACKET` raw sockets) plus ordinary firewall rules
  (`Salmon.Builtin.Nodes.Netfilter`) if per-instance port-level restriction
  is wanted. Simplest, and correct for the diagram's shape — every app
  instance's only real network need is "reach my bouncer's port" — but it
  means app instances share the host's network namespace and can, in
  principle, reach each other or anything else the host can reach; the
  actual boundary is the mount namespace (can't read each other's secrets)
  plus whatever the bouncer/Postgres side enforces via its own
  authentication.
- **Do unshare the network per instance**: real isolation (an instance
  literally cannot open a socket to anything but what's explicitly wired
  in), but needs one of: a veth pair into a bridge per instance (the same
  `LinuxBridge` primitives the qemu spec introduces, reused for a very
  different purpose — process sandboxes instead of VMs), or `slirp4netns`-
  style userspace NAT (another new dependency). Meaningfully more
  plumbing, and — the concrete cost — DNS resolution and any outbound
  calls an app instance makes beyond its bouncer (a JWKS fetch, an
  external webhook) would need that plumbing to actually work, not just
  the bouncer connection.

No change to the recommendation already in the open questions section
below: start without network namespacing, revisit only if a concrete case
needs it (e.g. a genuinely hostile/untrusted app instance sharing a
"shared tier" box with others, where "can't even attempt to reach a
neighbor's port" becomes a real requirement rather than defense in depth).

## Design goals / non-goals

Goals:
- Tier A available to every existing `Systemd.systemdService`-based recipe
  by construction, not as an opt-in rewrite each recipe author has to
  remember.
- Tier B (`Bwrap`) usable both as a `Systemd.Start`-wrapper (for services)
  and standalone (for ad hoc sandboxed command execution, e.g. from test
  tooling), since bwrap's value isn't systemd-specific.
- Every new field/type has a safe, explicit default — no recipe's behavior
  changes just from this landing; hardening is opt-in per recipe/service,
  matching CLAUDE.md's "don't add validation/hardening for scenarios that
  can't happen" spirit turned the other way: don't force isolation
  decisions onto recipes that haven't asked for them yet.

Non-goals (v1):
- A firejail builtin (§ above — documented as an option, not built, unless
  a concrete need shows up).
- Rootless bubblewrap **without** unprivileged user namespaces available
  (some hardened kernels disable `CLONE_NEWUSER` for unprivileged
  processes) — detecting and falling back is a real concern but not solved
  here; see open questions.
- Seccomp/syscall-filter authoring (`SystemCallFilter=` in tier A,
  `--seccomp` in bwrap) — both mechanisms support it, but hand-authoring a
  correct filter per service is its own project or template pipeline;
  starting with namespace/filesystem/capability isolation only.
- A per-recipe "which tier" policy/decision engine — the tier a given
  service uses is picked by whoever writes the seed/recipe wiring, not
  inferred from a `Tier`/`HardwareProfile` value automatically (though see
  the pg-ha spec's open questions — this may become relevant there later).

## Proposed design

### Tier A: `Systemd.Service` hardening fields

Extend `Salmon.Builtin.Nodes.Systemd`'s `Service`/`render_service` (the
only change needed to an existing module — everything else here is new
modules):

```haskell
data Hardening
    = Hardening
    { harden_protect_system :: Maybe ProtectSystemLevel  -- ProtectSystem=
    , harden_protect_home :: Bool                         -- ProtectHome=yes|no
    , harden_private_tmp :: Bool                           -- PrivateTmp=
    , harden_no_new_privileges :: Bool                     -- NoNewPrivileges=
    , harden_read_write_paths :: [FilePath]                -- ReadWritePaths=
    , harden_capability_bounding_set :: [Text]              -- CapabilityBoundingSet= (empty list = "" = drop all)
    , harden_restrict_address_families :: [Text]            -- RestrictAddressFamilies=, e.g. ["AF_INET", "AF_UNIX"]
    , harden_protect_hostname :: Bool                        -- ProtectHostname= (UTS namespace; cheap, see "Kernel namespaces in play")
    , harden_private_ipc :: Bool                              -- PrivateIPC= (systemd >=247; IPC namespace)
    , harden_private_network :: Bool                          -- PrivateNetwork= (network namespace) -- see the network-namespace tradeoff discussion; False by default for every recipe in scope today
    }

data ProtectSystemLevel = ProtectStrict | ProtectFull | ProtectReadOnly

noHardening :: Hardening   -- every field off/empty; today's exact behavior

data Service
    = Service
    { -- ...existing fields unchanged...
    , service_hardening :: Hardening   -- new field, defaults to 'noHardening' everywhere existing code constructs a 'Service'
    }
```

`render_service` grows the corresponding `[Service]` lines only for
non-default `Hardening` fields (e.g. omit `ReadWritePaths=` entirely if the
list is empty, rather than emitting an empty directive). Because `Service`
already has every field spelled out positionally at each of its four
current call sites (`Nginx.setup`, `PgBouncer.setup`,
`SreBox.Postgrest`'s systemd wiring), this is a real, mechanical edit to
each — not a source-breaking change hidden behind a default, since Haskell
records don't have optional-with-default construction without deriving
extra machinery this codebase doesn't currently use. Worth doing as its
own small PR (touch the 3–4 call sites, thread `noHardening` through)
before building anything Tier-A-consuming on top.

A concrete "shared tier app instance" profile, for the pg-ha spec's
budgetz-instance recipe to use once it exists:

```haskell
appInstanceHardening :: Hardening
appInstanceHardening =
    noHardening
        { harden_protect_system = Just ProtectStrict
        , harden_protect_home = True
        , harden_private_tmp = True
        , harden_no_new_privileges = True
        , harden_read_write_paths = []  -- nothing; app instances are stateless besides their own connstring/secret, both read-only
        , harden_capability_bounding_set = []  -- drop all
        , harden_restrict_address_families = ["AF_INET", "AF_INET6"]  -- talks to its bouncer over TCP, nothing else
        }
```

### Tier B: `Salmon.Builtin.Nodes.Bwrap`

A new builtin, in the same shape as `Salmon.Builtin.Nodes.Netfilter`/
`Salmon.Builtin.Nodes.LinuxBridge` (renders an explicit argv, no persistent
state of its own to be idempotent about beyond "the wrapped process is
running" — which is exactly what wrapping a `Systemd.Start` already handles
for free):

```haskell
data BindMount = BindMount { bindHostPath :: FilePath, bindGuestPath :: FilePath, bindReadOnly :: Bool }

data BwrapConfig
    = BwrapConfig
    { bwrap_binds :: [BindMount]           -- explicit allow-list; --ro-bind or --bind per entry
    , bwrap_unshare_net :: Bool             -- --unshare-net; see "the network-namespace tradeoff" -- False by default
    , bwrap_unshare_pid :: Bool             -- --unshare-pid
    , bwrap_unshare_uts :: Bool              -- --unshare-uts
    , bwrap_unshare_ipc :: Bool              -- --unshare-ipc
    , bwrap_unshare_cgroup :: Bool           -- --unshare-cgroup
    , bwrap_die_with_parent :: Bool         -- --die-with-parent (avoid orphaned sandboxed processes)
    , bwrap_new_session :: Bool             -- --new-session
    , bwrap_hostname :: Maybe Text          -- --hostname, only meaningful if bwrap_unshare_uts is set
    }

-- Mount and user namespaces aren't separate boolean fields here: mount-
-- namespacing is inherent to bwrap's whole bind-mount model, and user-
-- namespacing is the privilege-drop mechanism bwrap always relies on to run
-- unprivileged in the first place -- see "Kernel namespaces in play" for why
-- these two aren't optional the way the others are.

-- | Wraps a systemd Start's command+args in a bwrap invocation.
wrapStart :: Track' (Binary "bwrap") -> BwrapConfig -> Systemd.Start -> Systemd.Start
wrapStart bin cfg (Systemd.Start path args) =
    Systemd.Start "/usr/bin/bwrap" (bwrapArgs cfg <> [Text.pack path] <> args)
```

Every `bwrap` invocation needs `/` itself bound (typically `--ro-bind / /`
as the base, then the explicit allow-list layers read-write exceptions on
top) — `bwrapArgs` renders that base plus one `--ro-bind`/`--bind` pair per
`BindMount`, `--unshare-net`/`--unshare-pid` if requested, `--proc /proc
--dev /dev` (bwrap needs these explicitly, unlike a full container
runtime's defaults). This is the part that most benefits from hand-testing
against a real service before finalizing exact flags — sketched here at
the "what fields does the type need" level, not "the exact argv," per this
project's practice of validating shell-invocation shapes for real (see how
`specs/qemu-test-vms.md`'s phased plan puts a hand-validated boot before
wrapping in a node — same discipline applies here).

`wrapStart` composes with `Systemd.systemdService` (or `Qemu`-style bare
process execution — see `specs/qemu-test-vms.md`) — `PgBouncer.setup`/
`Nginx.setup`/`Postgrest`'s systemd wiring would build their `Start` as
today, then apply `wrapStart bwrapBin appInstanceBwrapConfig` before
handing it to `Systemd.Config`. No change needed to `Systemd.hs` itself for
this tier, unlike Tier A.

Also usable standalone (not just for services) — e.g.
`Bwrap.runSandboxed :: Track' (Binary "bwrap") -> BwrapConfig -> Command sym arg -> Command sym arg` as a `Command`-level wrapper, for anywhere else in
this codebase that shells out to something that could benefit from the same
treatment without going through `Systemd` at all.

### Tier C: firejail (documented, not built)

If a concrete need shows up: a `Firejail` builtin would look like `Bwrap`'s
`wrapStart` (prefix the command with `firejail --profile=<path> --` or an
explicit `--noprofile` + flag list), but this project's convention is typed
values driving rendered config, not shelling out to an externally-authored
profile file salmon can't inspect — so if this ever gets built, prefer
rendering an explicit firejail flag list (mirroring `BwrapConfig`'s shape)
over accepting a `.profile` path, keeping the same "everything explicit"
property Tier B has. Not scheduled; revisit only if Tier A+B turn out
insufficient for some concrete case.

## Open questions

- **Network isolation vs. the bouncer**: see "the network-namespace
  tradeoff" above — leaning towards *not* unsharing net by default, only
  revisited if a concrete "shared tier" case needs stronger-than-mount-
  namespace isolation between instances.
- **Capability set specifics per existing recipe**: `Nginx`/`PgBouncer` may
  need to bind low ports (`CAP_NET_BIND_SERVICE`) if configured to listen
  below 1024 — `appInstanceHardening`'s "drop all capabilities" sketch
  above needs per-recipe review, not a single one-size-fits-all profile.
- **Detecting bwrap availability / unprivileged userns support**: some
  hardened kernels (grsecurity-influenced sysctls, some container hosts)
  disable unprivileged `CLONE_NEWUSER`. Should `Bwrap`'s `Op` have a
  `prelim` that checks this and reports something actionable, or is "let
  `bwrap` itself fail loudly and let `Binary.untrackedExec`'s existing
  non-zero-exit-throws behavior surface it" (CLAUDE.md's existing
  "failure must not be swallowed" convention) good enough? Leaning towards
  the latter — consistent with how every other builtin in this codebase
  handles a missing precondition, no new mechanism needed.
- **Does Tier A's `Hardening` threading break existing test fixtures**?
  `Test.PostgresInitSpec`/`Test.PodmanSpec` construct `Systemd.Service`
  values directly in a few places (worth grep-confirming before landing
  the `service_hardening` field) — a mechanical `noHardening` addition at
  each existing call site, but worth doing carefully in one pass rather
  than piecemeal to avoid a half-migrated `Service` type.

## Phased plan

1. Tier A: add `Hardening`/`ProtectSystemLevel`/`noHardening` to
   `Systemd.hs`, thread `service_hardening` through `Service`'s existing
   4-ish call sites with `noHardening` (behavior-preserving), confirm
   `cabal build`+existing tests still pass.
2. Write `appInstanceHardening` (or similar) and apply it by hand to one
   real service (e.g. `PgBouncer.setup`'s call site, or once it exists, the
   budgetz-instance recipe from `specs/pg-ha-control-plane.md`) — hand-test
   that the service still starts and does its job with `ProtectSystem=
   strict`/`ReadWritePaths=`/etc. actually applied, not just that it
   compiles.
3. `Salmon.Builtin.Nodes.Bwrap` (Tier B): `BwrapConfig`, `wrapStart`,
   `bwrapArgs`. Hand-validate the rendered argv against a trivial command
   first (`bwrap --ro-bind / / --unshare-net -- echo hi`-shaped), the same
   "hand-validate before wrapping in a node" discipline as the qemu spec.
4. Apply `wrapStart` to one real service, confirm it still works under the
   combination of Tier A *and* Tier B (they're not mutually exclusive —
   systemd's own directives plus a bwrap-wrapped `ExecStart` compose, since
   systemd applies its sandboxing to whatever process `ExecStart` names,
   which by then is `bwrap` itself wrapping the real binary).
5. Firejail (Tier C), only if a concrete need shows up.

## Future work

- Seccomp filter authoring/templates (both tiers support the underlying
  mechanism; authoring correct filters is future work).
- A `Tier`/`HardwareProfile`-driven policy (from `specs/pg-ha-control-plane.md`)
  that picks a sandboxing tier automatically based on shared-vs-dedicated
  deployment, once real usage shows what the right default actually is.
- Rootless-bwrap-unavailable fallback (detect, degrade to Tier A only, warn).
