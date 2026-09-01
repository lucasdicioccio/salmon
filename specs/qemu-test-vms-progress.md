# Implementation progress: `specs/qemu-test-vms.md`

Status: living doc, update as work continues. Companion to
`specs/qemu-test-vms.md` (the design) — this file tracks *what's actually
built*, what's still untested, and exactly what to do next. Read the spec
first if you need the "why."

Work on this branch is being committed incrementally as it lands (see
`git log`); this doc may still lag the latest working-tree state by a
change or two at any given moment.

## 0.2 Headline: the whole tier runs unprivileged now, no `sudo` at all (2026-09-01/08)

Following up on §4's privilege open question: dropped the requirement that
the whole test binary run as root. `Test.Harness.hasVmPrivileges` now
accepts either real root or a one-time capability grant; `withVmAt` runs
qemu as the invoking user (via `LinuxBridge.Tap`'s `tapOwner` and
`Qemu.VmConfig`'s `vm_user`/`vm_group`) instead of `root:root`. New
`Salmon.Builtin.Nodes.Capabilities` (`setcap`/`getcap`, idempotent) plus a
`salmon-qemu-host-setup-fixture` executable do the one-time host grant as
a real `Op` graph instead of a shell snippet — see its own haddock for the
exact commands. Confirmed passing fully unprivileged: `QemuSmokeSpec`
(20s) and `PostgresReplicationSpec` (69s, two VMs) both green with no
`sudo` anywhere in the test invocation.

Two real, boot-validated bugs found getting there, neither guessable from
code review:

1. **Granting `cap_net_admin` to `ip` itself does not work.** `strace` on
   a failing unprivileged `ip link add ... type bridge` showed `ip`
   unconditionally calling `capset({...}, {effective=0, permitted=0,
   inheritable=0})` at startup — iproute2 drops its entire capability set
   on exec and only trusts the *ambient* set afterwards, which a plain
   file-capability grant can never populate (the kernel zeroes ambient for
   any exec of a "privileged" file, by design). Confirmed via a clean
   control test first: `ping` (file-cap `cap_net_raw`) works fine
   unprivileged, proving the capability mechanism itself was never the
   problem. Fix: grant the capability to `capsh` instead, and have `ip`
   invocations go through `capsh --inh=cap_net_admin --addamb=cap_net_admin
   -- -c "ip ...args..."` (`Salmon.Builtin.Nodes.LinuxBridge.ipLinkCommand`)
   — ambient capabilities do propagate across exec and are what iproute2
   actually honors. Raising ambient itself needs the capability in *both*
   the process's permitted *and* inheritable sets (`--inh=` first) since
   exec does not carry a file's inheritable bit into the new process's own
   inheritable set. Works identically for real root (whose permitted set
   is already full) and for an unprivileged user with the grant on
   `capsh`, so the wrapping is unconditional now, not privilege-mode
   -specific.
2. **A qemu VM's systemd unit can't live under `/etc/systemd/system`
   unprivileged** (plain permission denied writing there). Fix: added
   `Systemd.Scope` (`System`/`User`) to `Salmon.Builtin.Nodes.Systemd`;
   `Qemu.VmConfig` gained `vm_systemd_scope`/`vm_unit_dir`, and
   `Test.Harness.withVmAt` now uses `Systemd.User` against a resolved
   `~/.config/systemd/user`, with `systemctl --user` and `default.target`
   swapped in for `multi-user.target`/`network-online.target` (which don't
   exist in the user manager). Systemd also rejects `User=`/`Group=` in a
   user-manager unit, so `render_service` omits them for `User` scope.
   `PgBouncer`/`Postgrest`/`MicroDNS`'s existing `Systemd.Config` call
   sites were updated to explicit `System`/`/etc/systemd/system` — no
   behavior change for them.

Not yet chased down: the Postgres replication VM test flaked once when run
as part of the full 20-test suite (`cabal test salmon-ops-recipes`) —
`walsender process due to replication timeout` inside the guest, a
stale-pidfile postgres restart loop — while passing cleanly twice
standalone. Smells like timing/resource contention from running
back-to-back with everything else rather than a regression from the
privilege changes above (this project already has one documented
unrelated Layer 2 podman flake under load), but not confirmed either way.

## 0.1 Headline: Phase 5's real recipe test now passes for real (2026-08-21)

`Test.PostgresReplicationSpec` (see §1) now passes under `sudo` against real
`pg-primary`/`pg-standby` rootfses — closes §3 item 4. Getting from "builds
clean, never run" to a real pass took four more bugs, none guessable without
actually booting the VMs:

1. **`resolveFixtureBinary` took the first line of `cabal list-bin`'s
   stdout**, not the last: under `sudo` (root, no prior cabal config),
   `cabal` prints a one-line notice ("Config file path source is default
   config file.") to stdout *before* the actual bin path, so the scp target
   became that notice string instead of a path. Fixed to take the last
   non-blank line.
2. **`postgresql-client` isn't pulled in by the `postgresql` meta-package**
   the way `postgresql-client-17` is — the fixture's `Debian.psql`/
   `Debian.pg_ctl` map to the generic `postgresql-client` package name, which
   wasn't in the rootfs's `--include` list, so the guest (no network route
   past boot, by design) failed trying to fetch it. Fixed by chroot-installing
   it into the master rootfs (host has network) before copying out — see §2.
3. **The cluster ended up on port 5433, not 5432**: `pg_createcluster`
   (during the `postgresql` package's postinst, run inside a `chroot` on the
   *host*) auto-picks the next free port by probing the host's own running
   processes — since it shares the host's network stack, it saw something
   already on 5432 and picked 5433. The fixture hardcodes `primaryPort =
   5432`. Fixed by forcing `port = 5432` in the rootfs's
   `postgresql.conf` directly.
4. **`Test.Harness.sshToVm` silently mis-delivers any argument containing
   embedded whitespace.** `ssh` joins every argument after the destination
   with a single space and ships the result as *one string* for the remote
   shell to tokenize — exactly like typing the words by hand at a terminal.
   So an `args` element that's a whole SQL statement or a `cmd 2>&1`
   redirection doesn't arrive as one remote token: the remote shell re-splits
   it on spaces along with everything else. E.g. `["psql", "-tAc", "SELECT
   state FROM pg_stat_replication;"]` arrived remotely as `psql -tAc SELECT
   state FROM pg_stat_replication;` — `-tAc` only captured `SELECT`, and
   `waitForStreaming`'s query silently malformed on every single poll,
   meaning it could never have detected real streaming state regardless of
   whether replication actually worked. Same class of bug as the
   `Systemd.render_start` `ExecStart=` quoting fix in §0 item 4 below — just
   in the test harness instead of production code. Fixed by adding
   `Test.Harness.quoteForRemoteShell` (single-quotes a string so it survives
   ssh's space-join as one token) and applying it at each call site in
   `Test.PostgresReplicationSpec` that needs one. Deliberately **not** made
   automatic inside `sshToVm` itself: `Test.QemuSmokeSpec`'s existing
   `sshToVm access ["echo smoke-ok"]` relies on the remote shell's own
   re-splitting to turn one Haskell string into two remote words — quoting
   every argument unconditionally would instead hand the remote shell one
   literal token `"echo smoke-ok"` (a program name with a space in it) and
   break that passing test.

Confirmed passing standalone (`--pattern 'Postgres replication'`, ~100s) and
with the rest of the non-root suite around it unaffected (Podman's Layer 2
container test failed in one non-root sanity run, but that's an unrelated,
pre-existing environmental flake — `Salmon.Builtin.Nodes.Podman` wasn't
touched by any of this work).

## 0. Headline: the Layer 3 tier now works end to end (2026-08-20)

`Test.QemuSmokeSpec` (new) boots a real qemu VM via `Test.Harness.withVm`
against a hand-built debootstrap rootfs and SSHes into it for real —
confirmed passing under `sudo` (root is required, see §2):

```sh
cabal build salmon-ops salmon-ops-recipes:test:salmon-ops-recipes-test
sudo PATH="$PATH" dist-newstyle/build/x86_64-linux/ghc-9.8.2/salmon-ops-recipes-0.1.0.0/t/salmon-ops-recipes-test/build/salmon-ops-recipes-test/salmon-ops-recipes-test --pattern Qemu
# Qemu (Layer 3, real VM boot via withVm)
#   boots the smoke rootfs and answers SSH: OK
```

Getting there took **four real production bugs**, each hand-validated by
booting an actual VM and reading its serial console — none of these were
guessable from code review alone:

1. **NIC naming** (predicted in §3.1 below, confirmed for real): the
   virtio-net device came up as `ens4`, not `eth0`, breaking the `ip=`
   kernel arg silently. Fixed by adding `net.ifnames=0 biosdevname=0` to
   `Qemu.kernelCmdline` unconditionally (this whole tier already assumes a
   single, always-`eth0` NIC).
2. **9p root never mounts**: the stock debootstrap initrd never even
   attempts a 9p mount of its own root (`9pnet`/`9pnet_virtio`/`9p` are
   kernel *modules*, not builtin, and nothing loads them) — panics with
   `/dev/root does not exist`. Fixed with a new op,
   `Debootstrap.ensureVm9pBoot`, that appends those modules to
   `/etc/initramfs-tools/modules` and regenerates the initrd via a chroot.
   Also required renaming the 9p mount tag from `/dev/root` to `vroot`
   (and `root=vroot` on the cmdline): `initramfs-tools`'s
   `local_device_setup` only skips its udev block-device wait for a `ROOT`
   that neither starts with `/dev` nor contains `=` — anything else, 9p
   tags included, it waits forever since 9p never produces a udev block
   device.
3. **`security_model=mapped` breaks `/sbin/init`**: `run-init: /sbin/init:
   Too many symbolic links encountered` — `mapped` doesn't round-trip
   Debian's `/bin -> usr/bin`-style symlinks faithfully. Since qemu (and
   this whole tier) already runs as root on the host, switched to
   `security_model=passthrough` (real symlinks/ownership preserved, no
   uid remapping) in `Qemu.qemuArgs`.
4. **`Systemd.render_start` never quotes `ExecStart=` args** (found only
   once #1–#3 above were fixed and the VM booted but SSH still never
   answered): it joins args with a bare `Text.unwords`. `-append`'s value
   is one multi-word string; unquoted in the unit file, systemd's own
   `ExecStart=` parser splits it back into several separate qemu
   arguments, so the kernel cmdline silently never arrives intact. Fixed
   by quoting any arg containing whitespace/shell metacharacters in
   `Systemd.hs`. This is a general `Systemd.hs` bug, not qemu-specific —
   just never exercised before since nothing else here passes a
   multi-word single `ExecStart=` argument.

A fifth issue was in the *test* setup, not production code: the original
plan (§4 step 2, now superseded) had a human manually copying their own
`~/.ssh/id_ed25519.pub` into the rootfs's `authorized_keys`. Running the
privileged tier under `sudo` doesn't forward the invoking user's
ssh-agent, so pubkey auth via a personal key silently never succeeds and
`waitForSsh` just times out. Fixed by having `withVm` generate its own
ephemeral SSH CA + signed client key per boot (`Test.Harness.
ensureVmSshAccess`, using the existing but previously-unused
`Keys.sshKey`/`Keys.signKey` CA-signing primitives) and provision the
guest's `sshd` to trust it (`TrustedUserCAKeys` + `PasswordAuthentication
no` drop-in) — no manual key step needed any more. This surfaced one more
bug along the way: `Keys.signKey` never passed `-n <principal>` to
`ssh-keygen -s`, and modern OpenSSH (checked against 9.6p1) hard-rejects a
certificate with an empty principal list (`Certificate lacks principal
list`) — contrary to older folklore that an empty list means "valid for
any principal." Fixed by adding a `[Principal]` parameter to `signKey`
(safe: grep confirmed nothing else in the codebase called it yet).

## 1. Files touched so far

| File | State | What |
|---|---|---|
| `salmon-ops/src/Salmon/Builtin/Nodes/LinuxBridge.hs` | new | `bridge`, `tap`, `bridgeAddr` ops. |
| `salmon-ops/src/Salmon/Builtin/Nodes/Qemu.hs` | new | `VmConfig`, `resolveKernelInitrd`, `qemuArgs`, `setup`. Boot-validated fixes: `security_model=passthrough`, `mount_tag=vroot`/`root=vroot`, `net.ifnames=0 biosdevname=0` baked into `kernelCmdline`, `-cpu host` paired with `-enable-kvm`. |
| `salmon-ops/src/Salmon/Builtin/Nodes/Debian/Debootstrap.hs` | modified | `vmEssentials :: Includes` (kernel + openssh-server), plus new `ensureVm9pBoot` op (9p initramfs modules + `update-initramfs` via chroot). |
| `salmon-ops/src/Salmon/Builtin/Nodes/Systemd.hs` | modified | `render_start` now quotes `ExecStart=` args containing whitespace/shell metacharacters — real bug fix, not qemu-specific. |
| `salmon-ops/src/Salmon/Builtin/Nodes/Keys.hs` | modified | `signKey` gained a required `[Principal]` parameter (`-n` to `ssh-keygen -s`) — modern OpenSSH rejects principal-less certs. |
| `salmon-ops/salmon-ops.cabal` | modified | registered `LinuxBridge` and `Qemu` in `exposed-modules`. |
| `salmon-ops-recipes/test/Test/Harness.hs` | modified | Layer 3 section: `testBridge`/`testBridgeCidr`/`testVmAddr`/`testVmAddr2`/`ensureTestBridge`/`withVm`/`withVmAt`, `VmAccess`/`sshToVm`/`scpToVm` (replaces the old bare-`Ssh.Remote` interface — see §0 item 5), `ensureVmSshAccess`. `withVm` is now `withVmAt testVmAddr`; `withVmAt` takes the guest address as a parameter so more than one VM can be up at once on the shared test bridge (nested calls, one address each) — added for §3 item 4. `quoteForRemoteShell` added 2026-08-21 (see §0.1 item 4) — single-quotes a caller's `sshToVm` argument so it survives ssh's own space-join as one remote token; not applied automatically inside `sshToVm` itself (would break `Test.QemuSmokeSpec`'s existing `["echo smoke-ok"]` call, which relies on the old join-then-resplit behavior). |
| `salmon-ops-recipes/test/Test/QemuSmokeSpec.hs` | new | Layer 3 smoke test: boots the smoke rootfs via `withVm`, asserts SSH answers. Skips loudly (not fail) without root, without `qemu-system-x86_64`, or without a pre-built rootfs at `/var/lib/salmon-test-vms/smoke/root`. |
| `salmon-ops-recipes/test/Test/DebootstrapSpec.hs` | new | Layer 3: runs `Debootstrap.rootTree` `inject` `Debootstrap.ensureVm9pBoot` as real `Op`s (not the equivalent hand-run chroot script) against `/var/lib/salmon-test-vms/debootstrap-op-smoke/root`, checks the 9p modules got written, then reruns once more to confirm idempotency. Closes §3 item 2. Gated behind root/`debootstrap`-on-PATH (skip loudly) plus, for the first (network-heavy) run only, the opt-in env var `SALMON_TEST_RUN_DEBOOTSTRAP=1` — deliberately does *not* wipe the rootfs between runs, so once debootstrapped once, every later run is offline/seconds-long via the two ops' own `prelim`s. Confirmed passing under `sudo` (`1562.57s` first run, network-bound). |
| `salmon-ops-recipes/test/Test/PostgresReplicationSpec.hs` | new | Layer 3 port of the hand-run `salmon-ops/fixtures/PostgresReplicationFixture.hs`: boots a primary VM (`testVmAddr`) and a standby VM (`testVmAddr2`) via `withVmAt`, `scpToVm`s the already-built fixture binary (resolved via `cabal list-bin`, no hardcoded path) onto each, drives it over SSH exactly like the old fixture's manual `podman exec` steps, then — unlike the old fixture, which just told a human to eyeball `psql` — polls `pg_stat_replication` for `streaming`, inserts a row on the primary, and polls the standby until that row actually shows up. **Closes §3 item 4 (2026-08-21)** — confirmed passing under `sudo` for real (~100s), after the four bugs in §0.1. On a failed fixture run or a `waitForStreaming` timeout, dumps `pg_lsclusters`/postgres logs/`pg_stat_replication`/`pg_stat_wal_receiver`/a standby→primary ping into the failure message (failure-path only, no extra SSH round-trips on the success path). |
| `salmon-ops-recipes/test/Test/QemuResolveKernelSpec.hs` | new | Layer 1, no root/VM needed: builds a scratch `boot/` dir via `temporary`'s `withSystemTempDirectory` and checks `Qemu.resolveKernelInitrd`'s three cases (one match resolves, zero throws, two — a held-over old kernel — throws `"ambiguous"` rather than silently picking one). Closes §3 item 3 (2026-08-21). |
| `salmon-ops-recipes/test/Main.hs`, `salmon-ops-recipes.cabal` | modified | wired `Test.QemuSmokeSpec`, `Test.DebootstrapSpec`, `Test.PostgresReplicationSpec`, `Test.QemuResolveKernelSpec` in; added `unix` to test-suite `build-depends` (for `getEffectiveUserID`). |

`cabal build salmon-ops salmon-ops-recipes salmon-ops-recipes:test:salmon-ops-recipes-test`
and `cabal test salmon-ops-recipes` (non-root; all Layer-3-needing tests
vacuously skip, everything else including a real Layer 2 podman test
passes — 17 tests, all green) both clean as of 2026-08-20. The `sudo`-run
Qemu and Debootstrap tests are the real, non-vacuous confirmations (see
§0, §3 item 2); the Postgres replication test is written and building but
not yet run for real (see its row above).

## 2. Environment state (this machine, checked 2026-08-20)

- `iproute2` (`ip`), `qemu-system-x86` (provides `qemu-system-x86_64`),
  `debootstrap` (`1.0.134ubuntu2`): all installed.
- **KVM available and now exercised**: `/dev/kvm` exists, 40 `vmx`/`svm`
  flags in `/proc/cpuinfo`. `Test.Harness.withVm` now defaults to
  `vm_enable_kvm = True` with `-cpu host` (see §3.1 for the fix history);
  confirmed passing under `sudo`, boot+SSH completing in single-digit
  seconds in the two runs measured so far, vs. ~80–140s for the earlier
  TCG-only runs.
- The smoke rootfs lives at `/var/lib/salmon-test-vms/smoke/root`, built
  via (see §0 item 2 for why `ensureVm9pBoot`'s initramfs fix also needs
  applying — the rootfs on disk currently has that fix hand-applied via
  chroot, *not* yet re-derived by actually running the new
  `Debootstrap.ensureVm9pBoot` op against it):
  ```sh
  sudo debootstrap --include=linux-image-amd64,openssh-server stable /var/lib/salmon-test-vms/smoke/root
  ```
  No `authorized_keys` provisioning needed any more (§0 item 5) —
  `withVm` handles its own access.
- A second rootfs, `/var/lib/salmon-test-vms/debootstrap-op-smoke/root`,
  built *by `Debootstrap.rootTree`/`Debootstrap.ensureVm9pBoot` themselves*
  (via `Test.DebootstrapSpec`, see §1) rather than by hand — this is the
  one that actually proves those ops work, as opposed to the smoke rootfs
  above which still carries a hand-applied 9p fix.
- `/var/lib/salmon-test-vms/pg-primary/root` and
  `/var/lib/salmon-test-vms/pg-standby/root` (§1) **built 2026-08-21** —
  postgres is baked in at debootstrap time rather than apt-installed inside
  the guest at test time, since the test bridge has no NAT/internet route out
  of a guest past boot. Rather than debootstrapping each separately (they
  need the identical package set), built once as a `pg-master` rootfs and
  copied out twice:
  ```sh
  sudo debootstrap --include=linux-image-amd64,openssh-server,postgresql,sudo stable /var/lib/salmon-test-vms/pg-master/root
  # apply Debootstrap.ensureVm9pBoot's fix (see §0 item 2) to pg-master once
  # chroot-install postgresql-client into pg-master (see §0.1 item 2 — not
  # pulled in by the `postgresql` meta-package the way postgresql-client-17 is)
  # force `port = 5432` in pg-master's postgresql.conf (see §0.1 item 3 —
  # pg_createcluster auto-picked 5433 since the chroot install shares the
  # host's own network stack/port-in-use probing)
  sudo rsync -aHAX --numeric-ids /var/lib/salmon-test-vms/pg-master/root/ /var/lib/salmon-test-vms/pg-primary/root/
  sudo rsync -aHAX --numeric-ids /var/lib/salmon-test-vms/pg-master/root/ /var/lib/salmon-test-vms/pg-standby/root/
  ```
  Safe here specifically because `Test.Harness.sshToVm` uses
  `StrictHostKeyChecking=no`/`UserKnownHostsFile=/dev/null` (no host-key
  verification, so duplicate host keys across the two copies don't matter),
  and `postgresql`'s postinst doesn't start the service or write
  instance-specific state during a chroot debootstrap (services don't
  autostart in a chroot) — the copied data directories are just the vanilla
  package-created default cluster; primary vs. standby role is entirely a
  runtime distinction made by the fixture, not baked into the rootfs.

## 3. What's still open

1. ~~KVM never actually exercised end to end.~~ **Done (2026-08-20).**
   Flipped `Test.Harness.withVm`'s `Qemu.vm_enable_kvm` to `True` and
   reran `QemuSmokeSpec` under `sudo` — passed. First KVM pass was
   noticeably *slower* than the proven TCG baseline; root cause: `-enable-
   kvm` was passed without `-cpu host`, so the guest still ran the generic
   emulated `qemu64` CPU model — KVM only pays off once the guest actually
   gets a KVM-aware CPU model. Fixed in `Qemu.qemuArgs`:
   `if cfg.vm_enable_kvm then ["-enable-kvm", "-cpu", "host"] else []`.
   Rerun after the fix passed in ~5.5s (one earlier run hit ~3.75s) —
   both far faster than the ~80–140s TCG figure recorded in §2, though
   with only two data points this could partly be host-side caching
   rather than a clean KVM-vs-TCG comparison; not worth chasing further
   unless boot time becomes a problem again. `vm_enable_kvm = True` is now
   the harness default.
2. ~~`Debootstrap.ensureVm9pBoot` itself is untested as a salmon `Op`.~~
   **Done (2026-08-20).** `Test.DebootstrapSpec` runs `Debootstrap.rootTree`
   `inject` `Debootstrap.ensureVm9pBoot` for real against a fresh rootfs,
   checks the 9p modules land, and reruns once more to confirm both ops'
   `prelim`s report `Skippable` the second time — passed under `sudo`
   (`1562.57s`, almost entirely `debootstrap`'s own package downloads).
   Gated behind `SALMON_TEST_RUN_DEBOOTSTRAP=1` for that first real run and
   does not wipe the rootfs between runs, so it doesn't silently re-fetch
   ~100+ packages (and burn a metered connection) on every suite run — see
   its row in §1.
3. ~~`resolveKernelInitrd`'s prefix match~~ **Done (2026-08-21).** Added
   `Test.QemuResolveKernelSpec` — a Layer 1 (pure filesystem, no
   root/VM/debootstrap) test that builds a scratch `boot/` dir via
   `withSystemTempDirectory` and checks all three cases:
   exactly-one-match resolves, zero matches throws `"no vmlinuz-*"`, and
   two matches (simulating a held-over old kernel) throws `"ambiguous
   vmlinuz-*"` rather than silently picking one. All three pass. No
   rootfs/VM bugs found — `resolveKernelInitrd`'s existing ambiguity
   handling was already correct, just previously unverified.
4. ~~Phase 5 of the spec: pick a real recipe Layer 2 can't exercise well and
   write its first real Layer 3 test.~~ **Done (2026-08-21).**
   `Test.PostgresReplicationSpec` (§1) ports
   `salmon-ops/fixtures/PostgresReplicationFixture.hs`'s hand-run,
   eyeballed podman flow onto two real qemu VMs with actual pass/fail
   assertions (`pg_stat_replication` reaches `streaming`, a row written on
   the primary shows up on the standby) — confirmed passing under `sudo`
   for real (~100s), after the four bugs in §0.1 (a `cabal list-bin`
   output-parsing bug, a missing `postgresql-client` package, a wrong
   postgres port, and a silent `sshToVm` argument-quoting bug affecting
   every multi-word remote command this test ran). Needed generalizing
   `withVm` into `withVmAt` (a caller-chosen guest address) so two VMs can
   be up at once on the shared test bridge, plus a new `scpToVm` to get
   the compiled fixture binary onto each guest.

## 4. Design decisions made while implementing (not equally emphasized in the spec)

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
  first pass. This also motivated the `security_model=passthrough` choice
  in §0 item 3.
- Graceful VM shutdown via the qemu monitor socket (spec §2) is **not
  implemented** — `down` goes through plain `systemctl stop`, i.e. SIGTERM.
  Explicitly called out as acceptable for v1's disposable-VM use case in
  `Qemu.hs`'s module haddock; revisit if abrupt termination ever causes a
  real problem (e.g. corrupting guest filesystem state between runs).
- Test SSH access is now entirely `withVm`'s own responsibility (a fresh
  per-boot CA + signed key, see §0 item 5) rather than something the
  caller pre-provisions — a deliberate narrowing from the original "caller
  supplies `authorized_keys`" plan once the sudo/agent problem showed up
  in practice. Production recipes (e.g. a real CA-backed service) still
  follow the project's key-exchange-agnostic convention; this only changes
  how the *test harness* itself authenticates to its own disposable,
  harness-owned VM.
