# Implementation progress: `specs/qemu-test-vms.md`

Status: living doc, update as work continues. Companion to
`specs/qemu-test-vms.md` (the design) — this file tracks *what's actually
built*, what's still untested, and exactly what to do next. Read the spec
first if you need the "why."

**Nothing in this branch is committed yet** — everything below is working
tree changes only (`git status --short` at the top level shows the four
files listed in §1 as modified/untracked; `specs/` itself is untracked,
matching `advance-querying.md`'s existing precedent).

## 1. Files touched so far

| File | State | What |
|---|---|---|
| `salmon-ops/src/Salmon/Builtin/Nodes/LinuxBridge.hs` | new | `bridge`, `tap`, `bridgeAddr` ops. Builds clean. |
| `salmon-ops/src/Salmon/Builtin/Nodes/Qemu.hs` | new | `VmConfig`, `resolveKernelInitrd`, `qemuArgs`, `setup`. Builds clean. |
| `salmon-ops/src/Salmon/Builtin/Nodes/Debian/Debootstrap.hs` | modified | added `vmEssentials :: Includes` (kernel + openssh-server). Builds clean. |
| `salmon-ops/salmon-ops.cabal` | modified | registered `LinuxBridge` and `Qemu` in `exposed-modules`. |
| `salmon-ops-recipes/test/Test/Harness.hs` | modified | Layer 3 section: `testBridge`/`testBridgeCidr`/`testVmAddr`/`ensureTestBridge`/`withVm`, plus `bridgeAddr`'s use. Builds clean (`cabal build salmon-ops-recipes:test:salmon-ops-recipes-test`). |

Everything above compiles (`cabal build salmon-ops salmon-ops-recipes
salmon-ops-recipes:test:salmon-ops-recipes-test`, confirmed today) but
**nothing has been run** — no debootstrap, no qemu boot, no test execution.
This is phases 1–4 of the spec's phased plan, code-only.

## 2. Environment state (this machine, checked today)

- `iproute2` (`ip`): already installed.
- `qemu-system-x86` (provides `qemu-system-x86_64`): **installed today** by
  the user (apt package name is `qemu-system-x86`, not
  `qemu-system-x86_64` — that confused the first install attempt).
- `debootstrap`: **not confirmed installed** — check with `dpkg -l
  debootstrap` before phase-2 hand-validation.
- `openssh-server`: not needed on the host — needs to land *inside* the
  guest rootfs via `Debootstrap.vmEssentials`, already wired.
- **No KVM on this machine**: `/dev/kvm` doesn't exist, `/proc/cpuinfo` has
  zero `vmx`/`svm` flags despite this being a real 13th-gen Intel Core CPU.
  User was going to check BIOS (`Intel VT-x`, under
  `Advanced`/`CPU Configuration` or `Security`) — **unknown whether that
  happened or fixed it**. Until confirmed: `Qemu.vm_enable_kvm` must be
  `False` (as `Test.Harness.withVm` already sets it) and expect slow TCG
  boots. Recheck with `egrep -c '(vmx|svm)' /proc/cpuinfo` and `ls
  /dev/kvm` before assuming otherwise.
- User is on a slow/metered connection as of this writing — debootstrap
  (network-heavy, pulls ~150–300MB) was deliberately deferred, not
  forgotten.

## 3. What's unverified / first-cut and needs checking on the next real boot

These are called out in code comments already (search for "unverified" /
"first cut" in `Test/Harness.hs` and `Qemu.hs`), collected here for
convenience:

1. **Guest NIC naming**: `Test.Harness.withVm` hardcodes `ip=...::eth0:off`
   on the kernel cmdline, assuming the single virtio-net device comes up as
   `eth0`. Debian's default udev predictable-naming rules might instead
   name it something like `enp0s2` / `ens3`, in which case the VM boots but
   never gets an address and SSH polling times out. **First thing to check
   if `withVm` hangs then fails with "never answered SSH."** Fix, if
   needed: either pass `net.ifnames=0 biosdevname=0` on the kernel cmdline
   to force classic naming, or read the actual interface name from the
   guest's boot log over the serial console.
2. **9p boot actually working at all**: the whole `-fsdev
   local,...,security_model=mapped` + `-kernel`/`-initrd` + `rootfstype=9p
   rootflags=trans=virtio` combination in `Qemu.qemuArgs` is written from
   the design in `specs/qemu-test-vms.md` §3a, not yet booted once. Kernel
   needs `9P_FS`/`VIRTIO` support built in or as initrd-loadable modules —
   Debian's stock `linux-image-amd64` should have these as modules, which
   the initrd should already handle, but this is exactly the kind of thing
   that only becomes certain by trying it.
3. **`openssh-server` starting on boot unattended**: `vmEssentials` installs
   it, but nothing yet confirms sshd is enabled/started automatically by
   debootstrap's default package postinst (should be, via `systemd-sysv`,
   but unverified).
4. **Root login over SSH**: `withVm`'s haddock assumes
   `root/.ssh/authorized_keys` is pre-populated in the rootfs by the
   caller. Nothing does that yet — see §4 step 2 below.
5. **`resolveKernelInitrd`'s prefix match**: assumes exactly one
   `vmlinuz-*`/`initrd.img-*` pair under `<rootfs>/boot`. Should hold for a
   fresh single-kernel debootstrap, unverified in practice.

## 4. Exact next steps (in order)

1. Confirm `debootstrap` is installed (`dpkg -l debootstrap`); install via
   `sudo apt install debootstrap` if not — flag to the user first given the
   metered-connection concern (it's small, but still a network op).
2. Hand-run debootstrap once, by hand (no salmon `Op` yet — this is
   `specs/qemu-test-vms.md`'s phase 2, still not done):
   ```sh
   sudo debootstrap --include=linux-image-amd64,openssh-server stable /var/lib/salmon-test-vms/smoke/root
   ```
   Then, before booting: drop an authorized_keys file in so SSH login works
   (`sudo mkdir -p .../root/root/.ssh && sudo cp ~/.ssh/id_ed25519.pub
   .../root/root/.ssh/authorized_keys`), matching the "caller pre-provisions
   secrets" assumption in `withVm`'s haddock.
3. Hand-craft one raw `qemu-system-x86_64` command line (see
   `Qemu.qemuArgs`'s shape for the exact flags, or just call `Qemu.qemuArgs`
   from a `ghci` session against a `VmConfig` to print it) and boot it
   directly, watching the serial console, *before* going through
   `Test.Harness.withVm` — this is where issues #1–#3 in §3 get resolved.
4. Once a manual boot + SSH login works, run `Test.Harness.withVm` for real
   (a tiny ad hoc test, or wire it into an existing spec file) and confirm
   the polling loop actually succeeds end to end.
5. Only then: phase 5 of the spec — pick a real recipe Layer 2 can't
   exercise well (something `Systemd`- or `WireGuard`-shaped) and write its
   first Layer 3 test.

## 5. Design decisions made while implementing (not equally emphasized in the spec)

- `bridgeAddr`/`Cidr` in `LinuxBridge.hs` — not in the original spec
  write-up, added because the host side of the test bridge needs an
  address for SSH to route through. Same idempotency shape as `bridge`/
  `tap` (`ip addr show dev` grep via `prelim`).
  - Test subnet: `10.99.0.0/24`, host (bridge) `10.99.0.1`, guest fixed at
    `10.99.0.2` (`Test.Harness.testBridgeCidr`/`testVmAddr`) — single-VM-
    at-a-time assumption for v1, matching the spec's "prove the tier end to
    end" framing before anything like an address pool.
  - Bridge name: `salmontest0`, left standing across test runs (persistent,
    matching the spec's leaning in its bridge-lifecycle open question).
- `Qemu.setup`'s systemd unit runs qemu as `root:root`
  (`Test.Harness.withVm` sets `vm_user`/`vm_group = "root"`) rather than
  threading through `LinuxBridge.tap`'s `tapOwner` for an unprivileged
  user — simplest given this whole tier already assumes privileged
  execution (documented prerequisite, per the spec's privilege open
  question), avoids a second permissions mechanism to get right on the
  first pass.
- Graceful VM shutdown via the qemu monitor socket (spec §2) is **not
  implemented** — `down` goes through plain `systemctl stop`, i.e. SIGTERM.
  Explicitly called out as acceptable for v1's disposable-VM use case in
  `Qemu.hs`'s module haddock; revisit if abrupt termination ever causes a
  real problem (e.g. corrupting guest filesystem state between runs).
