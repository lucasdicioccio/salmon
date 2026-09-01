# Pinning an application's traffic to a chosen interface via dedicated network namespaces

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem

Sometimes one particular app/service's traffic needs to go out a specific
interface — e.g. only one app instance's egress should go through a
WireGuard tunnel while everything else on the box uses the normal uplink,
or (on a multi-homed box) a specific service needs to be pinned to a
secondary physical NIC. This is occasional, per-service, not a host-wide
routing policy change.

This is exactly the open edge `specs/service-sandboxing.md` left dangling:
its "network-namespace tradeoff" section flagged that Tier B's
`--unshare-net` gives a sandboxed process *zero* connectivity until
something wires it back up, and deferred solving that. This spec is that
"something" — and it turns out solving it well also directly answers "how
do I pin an app to an interface," because **the namespace boundary and the
interface-selection mechanism are the same mechanism**: whatever
interface(s) live inside a given network namespace are the *only* ones a
process joined to it can use, full stop. No `ip rule`/fwmark policy routing
needed, no risk of a routing-table rule silently not matching what you
expected — the kernel simply doesn't hand that process any other device.

## Why a *persistent, named* namespace instead of bwrap's `--unshare-net`

`specs/service-sandboxing.md`'s Tier B creates its network namespace
*ephemerally*, as a side effect of the sandboxed process starting
(`bwrap --unshare-net ...`). That's fine for "isolate this process from the
network," useless for "wire this process to a specific interface": there's
no namespace to attach a veth peer or move a device into *before* the
process exists, and no stable name to reference it by afterwards.

Linux (via `ip netns`) also supports **persistent, named** network
namespaces, independent of any process — created with `ip netns add
<name>`, they show up as bind-mounted files under `/run/netns/<name>` and
stick around until explicitly deleted. This is the right primitive here:
the host can fully wire a namespace's networking (create/move interfaces,
assign addresses, set routes) *before* anything joins it, and systemd has
a native way to join a unit's process to one: **`NetworkNamespacePath=`**
(systemd ≥245). This composes cleanly with everything in
`specs/service-sandboxing.md`'s Tier A — network-namespace selection and
mount/capability hardening are orthogonal `Systemd.Service` fields, not
competing mechanisms.

Recommendation: **for anything network-namespace-related, prefer a
persistent named `ip netns` + `NetworkNamespacePath=` over bwrap's
`--unshare-net`.** Tier B (bwrap) remains valuable for the *other*
namespaces (mount, in particular) it's already recommended for; this spec
effectively narrows bwrap's job back to "everything except networking" and
gives networking its own, more host-controllable mechanism.

## Proposed design

### 1. `Salmon.Builtin.Nodes.NetNamespace` (new)

Same shape as `Salmon.Builtin.Nodes.LinuxBridge` (prelim-based idempotency,
since neither `ip netns add` nor `ip link set ... netns ...` is idempotent
on its own):

```haskell
newtype NetNs = NetNs {netnsName :: Text}
    deriving (Eq, Ord, Show)

-- | Creates a persistent, named network namespace.
netns :: Reporter Report -> Track' (Binary "ip") -> NetNs -> Op

-- | Moves an already-existing interface into a namespace. Depends on both
-- the namespace and (by 'Ref', not a hardcoded call) whatever created the
-- interface in the first place -- callers pass that Op in explicitly since
-- this module has no idea whether it's a WireGuard iface, a veth end, or
-- anything else.
moveInterface :: Reporter Report -> Track' (Binary "ip") -> NetNs -> LinuxBridge.DevName -> Op -> Op

-- | A veth pair: one end moved into the namespace, the other attached to a
-- 'LinuxBridge.Bridge' (reusing the qemu spec's bridge primitive, here for
-- a very different purpose -- wiring a process sandbox to an uplink instead
-- of a VM).
data VethPair = VethPair { veth_inside_name :: LinuxBridge.DevName, veth_outside_name :: LinuxBridge.DevName, veth_ns :: NetNs, veth_bridge :: LinuxBridge.Bridge }
vethIntoBridge :: Reporter Report -> Track' (Binary "ip") -> VethPair -> Op
```

`prelim` for `netns`: `ip netns list` (grep for the name) — same "does the
effect already exist" shape as every other `ip`-backed idempotency check in
this codebase. `moveInterface`'s `prelim`: `ip netns exec <ns> ip link
show <dev>` succeeding means it's already there. `down` for `netns`: `ip
netns delete <name>` — same teardown-ordering concern
`Salmon.Actions.UpDown.downTree`'s module-level docs already describe for
any predecessor shared by several dependents (a namespace shouldn't be
deleted while a device still sits in it, or while a systemd unit is still
joined to it) — nothing new to design here, `downTree`'s existing
reverse-dependency-order teardown already handles this as long as the
`Op` graph's edges reflect the real dependency (interface-in-namespace
depends on namespace existing, exactly as sketched above).

### 2. Setting up routing *inside* the namespace

Once an interface is moved in, configuring anything about it — including
the namespace's own default route — has to run *inside* that namespace,
not the host's. `Routes.route`/`Netfilter`'s existing ops build plain
`proc "ip" [...]`/`proc "nft" [...]` `CreateProcess` values; rather than
duplicating those modules with namespace-aware copies, a small generic
combinator lets every existing `Command`-shaped builtin in this codebase
be reused unmodified:

```haskell
-- | Rewrites any CreateProcess to run inside a namespace via `ip netns exec`.
inNetNamespace :: NetNs -> CreateProcess -> CreateProcess
inNetNamespace ns cp = case cmdspec cp of
    RawCommand path args -> cp{cmdspec = RawCommand "ip" (["netns", "exec", Text.unpack ns.netnsName, path] <> args)}
    ShellCommand s -> cp{cmdspec = RawCommand "ip" ["netns", "exec", Text.unpack ns.netnsName, "sh", "-c", s]}

-- | Same, at the Command level -- wrap once, every Op built from the
-- wrapped Command runs inside the namespace, no changes needed to
-- Routes.hs/Netfilter.hs/WireGuard.hs/anything else.
wrapCommand :: NetNs -> Command sym arg -> Command sym arg
wrapCommand ns (Command prepare) = Command (inNetNamespace ns . prepare)
```

This is the one genuinely new idea worth calling out: **no existing
builtin needs to change.** `Routes.route reporter (wrapCommand ns
Routes.ipcommand ... )` — wait, `Routes.route`'s current signature takes a
`Track' (Binary "ip")`, not a `Command` directly, so the wrapping has to
happen one layer up, at whichever `Command` value a builtin's smart
constructor closes over internally. **This is a real, small prerequisite
change**: `Routes.route`/`WireGuard.iface`/etc. would need their internal
`ipcommand`/`wgcommand` value exposed as a parameter (or a namespace-aware
variant added) rather than hardcoded in the function body, mirroring how
`Systemd.systemdService` already takes its `Track' (Binary "systemctl")`
as a parameter instead of assuming one binary track globally. Small,
mechanical, per-module change — flagged rather than hidden, same spirit as
`specs/service-sandboxing.md`'s "not a source-breaking change hidden
behind a default" note about threading `Hardening` through `Service`.

### 3. Joining a systemd unit to the namespace

Extend `Systemd.Service` (the same record `specs/service-sandboxing.md`
is already extending with `service_hardening`) with one more optional
field:

```haskell
data Service
    = Service
    { -- ...existing + service_hardening from specs/service-sandboxing.md...
    , service_network_namespace :: Maybe FilePath   -- NetworkNamespacePath=/run/netns/<name>, or Nothing (today's behavior: host's default netns)
    }
```

`render_service` emits `NetworkNamespacePath=<path>` only when `Just`. The
namespace itself, and everything inside it, is provisioned by an ordinary
`Op` dependency (§1/§2 above) the same way `PgBouncer.setup`'s config files
are a dependency of its systemd unit — `deps [netns r ip ns, ...]` at the
service's own `op` construction, so the namespace and its interface exist
before `systemctl start` ever runs.

### 4. Worked example: pin one app instance's egress through WireGuard

```haskell
appNs :: NetNs
appNs = NetNs "ns-internaltool-a-0"

wgIfaceOp :: Op
wgIfaceOp = WireGuard.iface reporter ipTrack "wg-internaltool-a-0" (Ipv4Cidr "10.66.0.2" 32)
    -- ...peer/key setup as any other WireGuardVpn.hs-style recipe already does...

pinnedOp :: Op
pinnedOp =
    Systemd.systemdService reporter systemctl trackConfig cfg
        `inject` NetNamespace.moveInterface reporter ipTrack appNs "wg-internaltool-a-0" wgIfaceOp
        `inject` defaultRouteInsideNs
  where
    defaultRouteInsideNs =
        Routes.route reporter (wrapCommand appNs Routes.ipcommand |> asTrack) (Routes.Route Routes.Default "wg-internaltool-a-0" Nothing)
        -- ^ pseudocode: see §2's note that Routes.route needs a small parameter
        -- change before this composes as cleanly as sketched
    cfg = /* Systemd.Config with service_network_namespace = Just "/run/netns/ns-internaltool-a-0" */
```

The app instance's own binary needs no awareness of any of this — it just
opens sockets normally; the kernel only ever shows it `wg-internaltool-a-0` (plus
loopback). No policy-routing rule to get subtly wrong, no fwmark to leak
across a recipe boundary.

### 5. Worked example: pin traffic to a specific physical uplink

Same shape, but §4's WireGuard interface is replaced by §1's `VethPair`
into a `LinuxBridge.Bridge` that's itself attached to (or routes toward) the
secondary physical NIC — reuses the qemu spec's bridge primitive for a
third purpose now (VM networking, then test-VM networking, now process
traffic pinning), which is a good sign the primitive is at the right level
of abstraction rather than over-fit to one caller.

## Open questions

- **DNS resolution inside the namespace**: `ip netns exec` bind-mounts
  `/etc/netns/<name>/*` over `/etc/*` if present — so a pinned app needing
  working DNS resolution needs `/etc/netns/ns-internaltool-a-0/resolv.conf`
  written (a plain `FS.filecontents` op, nothing new needed) pointing at a
  resolver actually reachable from inside the namespace (not necessarily
  the host's own `/etc/resolv.conf` contents, if that resolver is only
  reachable via the interface being deliberately excluded).
- **Is this actually static-per-restart, and is that acceptable?**
  `NetworkNamespacePath=` is read at unit start; changing which namespace
  (or what's inside it) takes effect on the next restart, not live. Given
  the "sometimes" framing in the original ask, confirm that "toggle by
  redeploying/restarting the service" is the right granularity, as opposed
  to something that needs to flip while the process keeps running (which
  would need an entirely different mechanism — e.g. runtime policy routing
  after all).
- **The `Command`-parameterization prerequisite (§2)**: how many existing
  builtins actually need this before the worked examples stop being
  pseudocode — `Routes.hs` for sure (default route inside the namespace);
  `Netfilter.hs` possibly, if per-namespace firewall rules are ever wanted
  inside a pinned namespace too. Worth doing as its own small prerequisite
  pass (mirroring `specs/service-sandboxing.md`'s Tier A prerequisite of
  threading `Hardening` through `Service`'s call sites) rather than
  ad hoc per-recipe.
- **`ip link add ... netns <ns>` direct-create vs. create-then-move**:
  modern iproute2 can create a link directly inside a target namespace in
  one command; §1 sketches the safer, universally-supported
  create-in-host-namespace-then-move sequence instead (lets
  `WireGuard.iface` stay completely unmodified). Worth revisiting once
  this is actually run against the target iproute2 version — may simplify
  the sequencing, doesn't change the design.
- **Relevance to the pg-ha control plane vs. WireGuardVpn**: is the actual
  motivating case an app-instance (from `specs/pg-ha-control-plane.md`)
  needing to reach something only over a VPN, or is this more about the
  control-plane's *own* management traffic, or something outside either
  spec entirely (the original ask didn't specify)? Affects which recipe
  gets the first real integration in the phased plan below.

## Phased plan

1. `Salmon.Builtin.Nodes.NetNamespace`: `netns`, `moveInterface`,
   `inNetNamespace`/`wrapCommand`. Hand-validate against a throwaway
   interface (a dummy/veth, not WireGuard yet) before anything real depends
   on it.
2. The `Command`-parameterization prerequisite (open question above) for
   `Routes.hs` specifically — smallest slice that makes §4's worked example
   real instead of pseudocode.
3. `Systemd.Service`'s `service_network_namespace` field +
   `NetworkNamespacePath=` rendering, threaded through existing call sites
   with `Nothing` (behavior-preserving), same discipline as
   `specs/service-sandboxing.md`'s Tier A rollout.
4. End-to-end worked example (§4), against a real WireGuard tunnel, on
   whichever recipe the answer to the last open question points at.
5. `VethPair`/§5 (physical-uplink pinning), only once §4 is proven and if a
   concrete need for the non-VPN case shows up.

## Future work

- `/etc/netns/<name>/` resolv.conf management as its own small helper if
  this pattern gets reused often enough to be worth a smart constructor
  rather than a one-off `FS.filecontents` call each time.
- Revisiting whether a live-reconfigurable mechanism (policy routing after
  all, or some other dynamic scheme) is ever actually needed, depending on
  the answer to the "static-per-restart" open question above.
