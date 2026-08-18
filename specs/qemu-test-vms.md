# Local qemu VMs + tap/bridge networking for recipe testing

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem

`salmon-ops-recipes/test/Test/Harness.hs` already tiers tests by IO cost/
blast-radius, and its Layer 2 (`podmanTrack`/`withContainer`) dogfoods
`Podman.pullImage`/`Podman.runContainer` to get a real, disposable sandbox
for recipes that need a real service (Postgres, in `Test.PostgresInitSpec`).
That works well for anything that fits in a container, but several things
this project increasingly needs to test do not:

- systemd units as PID 1 actually manages them (`Systemd.hs` is used all
  over `salmon-ops`/`SreBox`, but a container's systemd, if present at all,
  behaves differently from a real boot),
- real network interfaces/routing (`WireGuard.hs`, `Routes.hs`,
  `Netfilter.hs`), where a podman container's network namespace doesn't
  exercise the same code paths as a genuine host interface,
- multi-machine topologies where "machine" needs to mean something closer
  to a real boot (kernel, init, network stack) — exactly what the [[pg-ha
  control plane spec]] (`specs/pg-ha-control-plane.md`) needs to test the
  diagonal replication pair, bouncer failover, etc. against something more
  realistic than two podman containers.

There's an unfinished start at this: `Debian.Debootstrap.rootTree` builds a
debootstrapped root filesystem at a path (idempotent via `skipIfFileExists`
on `etc/issue`) but nothing turns that into something bootable, and there's
no qemu or bridge/tap networking builtin at all — `Netfilter.hs` literally
has `-- TODO: Ip, Ip6, Arp, Bridge, NetDev` and stops there.

## Goal

A Layer 3 addition to the same test-harness tiering: real qemu VMs, on a
local bridge, reachable over SSH, disposable the same way Layer 2's podman
containers are — dogfooding new salmon builtins (bridge/tap setup, qemu
VM lifecycle) as the sandbox provisioner, same philosophy as Layer 2.

## Design goals / non-goals

Goals:
- New builtins follow every existing convention: `op`/`Track'`-shaped,
  idempotent `up` (see CLAUDE.md's "Conventions for node authors"),
  `down` implemented (this project's `downTree` machinery assumes it, and
  disposable-sandbox teardown is the whole point here).
- Reuse `Systemd.hs` for the VM process lifecycle instead of inventing a
  new "manage a long-running process" mechanism — a qemu VM is just another
  systemd unit from the host's point of view (see §2).
- Reuse `Ssh.hs` for reaching into a running VM, exactly like Layer 2 uses
  `podman exec`.
- Finish `Debootstrap` enough to produce something qemu can boot, without
  inventing a whole image-building subsystem.

Non-goals (v1):
- Production VM hosting (this is a *test* sandbox mechanism, not a new
  "salmon runs your workload in a VM" feature — no live migration, no
  resize, no snapshots).
- libvirt/virsh — plain `qemu-system-x86_64` + a monitor socket is enough
  for scripted start/stop; libvirt's XML/daemon layer adds nothing v1 needs.
- Multi-host bridging (VXLAN, etc.) — a single-host Linux bridge is enough
  for "several VMs on one test machine talk to each other and to podman
  containers if needed."

## Proposed builtins

### 1. `Salmon.Builtin.Nodes.LinuxBridge` (new)

The bridge + tap primitives `Netfilter.hs`'s TODO never got to:

```haskell
data Bridge = Bridge { bridge_name :: Text }

data Tap = Tap { tap_name :: Text, tap_bridge :: Bridge, tap_owner :: Maybe User.User }

bridge :: Reporter Report -> Track' (Binary "ip") -> Bridge -> Op
tap    :: Reporter Report -> Track' (Binary "ip") -> Tap -> Op
```

`up`: `ip link add name <br> type bridge && ip link set <br> up`;
`ip tuntap add dev <tap> mode tap [user <owner>] && ip link set <tap> master
<br> && ip link set <tap> up`. Neither `ip link add` nor `ip tuntap add` is
idempotent (both fail with "File exists" on retry) — same shape as
`Netfilter.rule`'s problem, so use the same fix already established as this
project's convention: `prelim` checks `ip link show <name>` and reports
`Skippable` if it's already there, rather than trying to force the `ip`
invocation itself to be idempotent. `down`: `ip link delete <name>`.

### 2. `Salmon.Builtin.Nodes.Qemu` (new)

A VM as a systemd unit, mirroring `Nginx.setup`/`PgBouncer.setup`'s exact
shape (render a start command, hand it to `Systemd.systemdService`):

```haskell
data VmConfig
    = VmConfig
    { vm_name :: Text
    , vm_memory_mb :: Int
    , vm_smp :: Int
    , vm_disk :: FilePath          -- see §3, the boot image
    , vm_tap :: LinuxBridge.Tap    -- depends on §1
    , vm_mac :: Text               -- stable MAC so the host can predict/reserve a DHCP lease if needed
    , vm_monitor_socket :: FilePath
    , vm_extra_args :: [Text]
    }

setup :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> Track' (Binary "qemu-system-x86_64") -> VmConfig -> Op
```

`up`/`down` are exactly "start/stop the systemd unit" (free, via
`Systemd.hs`) — no new process-management code. Command line:

```
qemu-system-x86_64 -name <vm_name> -m <memory_mb> -smp <smp>
  -drive file=<disk>,if=virtio,format=raw
  -netdev tap,id=net0,ifname=<tap>,script=no,downscript=no
  -device virtio-net-pci,netdev=net0,mac=<mac>
  -monitor unix:<monitor_socket>,server,nowait
  -nographic -serial mon:stdio
  -enable-kvm   -- if /dev/kvm exists; fall back to TCG otherwise (slow but portable, worth keeping as a fallback for CI boxes without nested virt)
```

Graceful shutdown on `down` ideally goes through the monitor socket
(`system_powerdown`) rather than `systemctl stop` sending SIGTERM straight
to qemu — worth a small `Qemu.shutdown` helper that writes to the monitor
socket and polls for the process to exit before falling back to a hard
stop, so the guest gets a real ACPI shutdown instead of losing an in-flight
`up`/write. Exact mechanism (raw socket write vs `qemu-system-x86_64`'s own
`-monitor` command tooling, if any exists on the host) is an implementation
detail to work out against a real qemu version, not a design blocker.

### 3. Finishing `Debootstrap`: from chroot dir to bootable disk

Two options, both worth having eventually but starting with the first:

**a. 9p virtfs passthrough (recommended v1 default)** — skip image-building
entirely; boot the existing `RootTree` directory straight off the host
filesystem via qemu's `virtfs`:

```
-fsdev local,id=root,path=<rootTree.path>,security_model=mapped
-device virtio-9p-pci,fsdev=root,mount_tag=/dev/root
-kernel <rootTree.path>/boot/vmlinuz-*  -initrd <rootTree.path>/boot/initrd.img-*
-append "root=/dev/root rootfstype=9p rootflags=trans=virtio rw console=ttyS0"
```

No mkfs/loop-mount step, no separate image artifact to keep in sync with
the chroot, fast to rebuild (`debootstrap` again just overwrites the dir,
same idempotency the node already has). Tradeoff: 9p root is nonstandard
enough that a few recipes' assumptions (real block device semantics,
`fsync` behavior a Postgres data directory cares about) might not transfer
1:1 to production behavior — acceptable for "does the recipe's `up`/`down`/
`check` logic run correctly," not for filesystem-performance testing.

**b. Raw disk image (future work, if 9p's divergence bites)** — extend
`Debootstrap` with a variant that targets a loop-mounted `.raw`/`.img` file
instead of a plain directory (`losetup`, `mkfs.ext4`, mount, run
`debootstrap` against the mountpoint, install a bootloader or keep using
direct `-kernel`/`-initrd` boot to skip GRUB entirely), producing a real
block-device-backed VM. More moving parts (loop device idempotency/cleanup
needs its own care — a stale loop device from a crashed previous run is
exactly the kind of thing `down` needs to handle), so deferred until 9p
proves insufficient.

Either way, `RootTree.includes` needs to grow to cover what a bootable VM
needs that a plain chroot doesn't: a kernel package (`linux-image-<arch>`),
`openssh-server` (so §4's SSH-based test harness can reach in), and enough
of an init to reach multi-user (Debian's default `systemd-sysv` — already
implied by `debootstrap` unless `--variant=minbase` was used, worth
confirming `RootTree` isn't passing that).

### 4. Test harness: Layer 3

```haskell
-- Test.Harness additions, mirroring podmanTrack/withContainer/podmanExec_
qemuTrack :: Track' (Binary "qemu-system-x86_64")
withVm :: VmConfig -> (Ssh.Remote -> IO a) -> IO a   -- boots, polls SSH readiness, runs action, tears down
vmExec_ :: Ssh.Remote -> Text -> IO ()
```

`withVm` runs `Qemu.setup`'s `Op` through `runUp`/`runDown` exactly like
`withContainer` does for podman — real IO, no mocking, dogfooding the new
builtins as their own test infrastructure. "Boot readiness" is poll-SSH-
until-it-answers (a VM takes real seconds to boot, unlike a podman
container being "up"), with a timeout that fails loudly rather than hanging
a test suite — same "skip loudly, don't hang" spirit as `requireExecutable`.

`requireExecutable "qemu-system-x86_64"` (already-generic) gates the whole
tier the same way Layer 2 gates on `podman`, so a machine without qemu
skips these tests instead of failing.

## Open questions

- **KVM availability in CI/dev containers**: nested virtualization may not
  be available everywhere this test suite runs. `-enable-kvm` needs a
  `/dev/kvm`-exists fallback to TCG (§2 already notes this) — worth
  confirming up front whether TCG boot times are tolerable for a test
  suite before committing to "VMs boot fast enough to be a normal test
  tier" as an assumption.
- **Bridge lifecycle scope**: one shared bridge reused across test runs
  (persistent, created once, VMs' taps attach/detach per test), or a fresh
  bridge per test run (fully disposable, more `sudo ip`-shaped setup/teardown
  noise per test)? Leaning towards one persistent bridge (named distinctly,
  e.g. `salmontest0`) with per-test taps, mirroring how Layer 2 doesn't
  recreate podman's network each test either.
- **Privilege**: `ip link add`/`tuntap add` and (for KVM) `/dev/kvm` access
  typically need root or specific capabilities/group membership
  (`CAP_NET_ADMIN`, the `kvm` group). Does the test harness assume the
  invoking user already has these (documented prerequisite, same as
  `podman` needing to be installed/usable), or does it need a sudo-wrapped
  path? Recommend the former (documented prerequisite) to match how Layer 2
  already assumes a working, usable `podman` rather than trying to grant
  privileges itself.
- **Where do prebuilt kernel/initrd come from**: `debootstrap` installs
  `/boot/vmlinuz-*`/`initrd.img-*` only if a kernel package is in
  `includes` (§3) — confirm the target suite's kernel package name
  (`linux-image-amd64` on Debian stable) and that `update-initramfs` runs
  automatically as part of package postinst inside the chroot (it should,
  via the chroot's own dpkg triggers) rather than needing an explicit step.

## Phased plan

1. `LinuxBridge` (§1): bridge + tap nodes, prelim-based idempotency, `down`.
   Test by hand (`ip link show`) before anything qemu-shaped depends on it.
2. Extend `Debootstrap`'s `includes`/confirm kernel+ssh presence (§3a); by
   hand, boot the resulting chroot directly with a one-off qemu command
   line (no salmon `Op` yet) to validate the 9p+`-kernel` approach works at
   all before wrapping it in a node.
3. `Qemu` node (§2) wrapping the now-validated command line as a
   `Systemd.systemdService`, against the §1 bridge.
4. `Test.Harness` Layer 3 (§4): `withVm`, SSH-readiness polling, one smoke
   test that boots a VM and runs a trivial command over SSH.
5. Pick one existing recipe that Layer 2 can't exercise well (a `Systemd`-
   or `WireGuard`-dependent one) and add its first Layer 3 test, proving
   the tier end to end.
6. Raw disk image variant (§3b), only if 9p's divergence from a real block
   device turns out to matter for something concrete.

## Future work

- Multi-VM topologies on the same bridge for testing the [[pg-ha control
  plane]]'s diagonal replication pair against two real VMs instead of two
  podman containers, once Layer 3 itself is proven out.
- Snapshot/clone support (qemu `-snapshot` or backing-file qcow2 images) to
  make repeated test runs cheaper once the raw-image variant (§3b) exists.
