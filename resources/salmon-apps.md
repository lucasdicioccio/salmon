# The salmon-apps binaries

`salmon-apps` is the package of shipped binaries: each one is a small `Main`
over a recipe or a reader, and this page has one entry per binary, saying what
it is for, what its seed looks like, the shortest command line that does
something, and what of it has actually been run.

Most of them speak the protocol every salmon binary shares
(`Salmon.Builtin.CommandLine`): `config <seed args>` prints a JSON directive,
and `run up|down|tree|dag|serve` reads one on stdin.

```sh
salmon-X config <seed args> | salmon-X run tree   # what would be declared
salmon-X config <seed args> | salmon-X run up     # converge it
salmon-X config <seed args> | salmon-X run down   # tear it down
salmon-X run serve --http PATH                    # keep converging; seeds arrive as lines
```

Three do not, because they converge nothing: `salmon-fleet` and `salmon-tui`
read what other binaries report, and `salmon-report` has a driver of its own
that only ever runs `check`s.

## Getting them

`salmon-migrator`, `salmon-pgpair`, `salmon-fleet` and `salmon-tui` are
published as prebuilt, stripped binaries on
[GitHub Releases](https://github.com/lucasdicioccio/salmon/releases),
linux-x86_64 only:

```sh
curl -fsSL https://github.com/lucasdicioccio/salmon/releases/latest/download/salmon-pgpair-linux-x86_64 -o salmon-pgpair && chmod +x salmon-pgpair
```

Every other binary here is build-from-source only, being a demo, a test
harness or an internal tool rather than something to depend on:

```sh
cabal build salmon-apps            # all of them
cabal list-bin salmon-gcp-toy      # where one of them landed
```

## How "what has been run" is meant

The repository's tests come in layers (see `salmon-ops-recipes/test/Test/Harness.hs`):
**Layer 0** is pure or in-process (the graph a seed declares, rendered scripts
and argument lists, parsers); **Layer 2** runs real recipes in disposable
podman containers or user-scope systemd; **Layer 3** boots qemu VMs. A binary
said to be "Layer 0 only" has had its declarations checked and nothing it
declares actually applied by a test. Where something was run by hand, the
entry says so, and says against what.

## At a glance

| Binary | For | Released | Exercised |
|---|---|---|---|
| `salmon-migrator` | Postgres migrations, template databases, clones | yes | `template` in a VM (Layer 3); template/clone nodes in a container (Layer 2) |
| `salmon-pgpair` | a Postgres primary/standby pair whose primary is declared | yes | switchovers under a writing client in VMs (Layer 3); TLS modes by hand in containers |
| `salmon-pg-backup` | a `pg_dump` now, or the cron job that keeps taking one, here or over ssh | no | `dump --over` and `schedule` in a VM (Layer 3) |
| `salmon-init-locally` | the salmon user, its sudoers entry, an authorized key | no | no test of its own |
| `salmon-fleet` | the controller's side of pull mode: fold status documents, make and use signing keys | yes | Layer 0 |
| `salmon-tui` | a terminal client of `run serve --http` / `--http-tcp` | yes | its client library against a real in-process server; the screen has no test |
| `salmon-report` | a read-only report of what a network can do (DNS, ports, NAT, reachability) | no | end to end on loopback; no real NAT or gateway |
| `salmon-gcp-toy` | a tiered, throwaway exercise of the GCP builtins | no | tiers 0 and 1 against a real project; several later options not yet |
| `salmon-cloudrun` | Cloud Run services, secrets and a load balancer from one JSON file | no | Layer 0 only |
| `salmon-toy-qemu-pg-ha` | the `salmon-pgpair` demo on three qemu guests it makes itself | no | the one-shot demo; the `run serve` session not with guests booted |
| `salmon-patroni-rootfs` | the root filesystems of the three-VM Patroni test harness | no | builds what that harness boots; no test of its own |
| `salmon-docs-sync` | rebuild, commit and push this website when its sources change | no | runs on this repository |

## Postgres

### salmon-migrator

Applies SQL migrations to a Postgres database on a Debian-like machine, in two
sets: *superuser* migrations, run first as the `postgres` system user, and
*owner* migrations, run as the database's owner role with a password read
from a file. Three subcommands share that shape:

- `migrate` moves a live database forward in place;
- `template` builds a template database from the same migrations: created
  from nothing, migrated, then locked (`IS_TEMPLATE`, no connections), skipped
  while the migration files are unchanged and dropped and rebuilt when any of
  them changes; it refuses to replace a database salmon did not build as a
  template;
- `clone` copies a database from such a template (one per preview branch,
  say); `--retain` keeps it on `down`, without it `down` drops it; it refuses
  to touch a database salmon did not itself clone.

The `apt-get` installs are batched by a registered rewrite, so under
`run serve` they are collected across every declaration the server holds.

```sh
salmon-migrator config migrate --db app --db-owner app --db-passfile /etc/app/db.pass \
  --superuser-root migrations/superuser --owner-root migrations/owner \
  | salmon-migrator run up
salmon-migrator config template --db app_template --db-owner app --db-passfile /etc/app/db.pass | salmon-migrator run up
salmon-migrator config clone --db preview_my_branch --template app_template --retain | salmon-migrator run up
```

**Run:** `config template` is driven through the real binary on a real
Postgres in a qemu VM (`Test/MigratorTemplateSpec.hs`, Layer 3): build, skip
when unchanged, rebuild on a changed migration, and plain-SQL clones of the
result. The template and clone nodes underneath are run against a container
(`Test/PostgresTemplateSpec.hs`, Layer 2). No test drives `config migrate` or
`config clone` through the binary itself. Code:
[`Migrator.hs`](../salmon-apps/src/Migrator.hs).

### salmon-pgpair

A two-machine Postgres pair with a *declared* primary, and a pgbouncer that
follows it. Moving the primary is an edit to one word of the seed, not a
procedure: a pass makes the machines agree with it (pause the bouncer, stop the
old primary cleanly, promote the other, repoint the bouncer, rejoin the old one
through `pg_rewind`). A failover with a lost member is refused unless
`--may-discard` names the side whose writes may be lost; `--seed` builds one
side from the other the first time; `--reseed` allows re-cloning a standby that
has fallen too far behind. Run it from a machine that is neither member.

It assumes what it does not provision, because a recipe that ships secrets
picks a transport for everybody: a cluster on both machines, pgbouncer and its
`userlist.txt` on the bouncer, and the `.pgpass` files it names on all three
(`SreBox.PostgresPairPrereqs` declares that layer for a binary composing the
pair as a library; this one does not).

```sh
salmon-pgpair config --primary A --a HOST_A --b HOST_B --bouncer HOST_C | salmon-pgpair run up
salmon-pgpair config --primary B --a HOST_A --b HOST_B --bouncer HOST_C | salmon-pgpair run up   # the switchover
```

`--conn-security plain|tls-scram|tls-cert` (with `--tls-ca`,
`--tls-verify-full`, `--tls-server-cert`/`--tls-server-key`) sets what the
replication and rewind connections between the two machines must be.

**Run:** the binary is driven in qemu VMs with a client writing through the
bouncer while the primary moves twice (`Test/PgPairDemoSpec.hs`, Layer 3).
The `--conn-security` modes were exercised by hand on two Debian trixie
containers (Postgres 17), not in the qemu tier. The full story, including what
it refuses and why, is the [Postgres pair guide](postgres-pair.md). Code:
[`PgPair.hs`](../salmon-apps/src/PgPair.hs).

### salmon-pg-backup

Takes a Postgres dump, or installs the `cron.d` job that keeps taking one
(and takes one immediately, so a broken credential shows now rather than at
03:17 tomorrow). With `--over USER@HOST` it uploads *itself* to that machine,
runs there with the same directive, and with `--fetch-into DIR` pulls the dump
back; the target needs nothing beyond `rsync`, `bash` and a Postgres client.
The dump's timestamp is frozen in the directive on the controlling machine, so
both ends name the same file and re-running the same directive is a no-op.

Credentials never go on a command line: `--pgpass`, `--pg-service-file` with
`--pg-service`, or `--client-cert`/`--client-key`/`--client-ca` become libpq
environment variables, and the default is peer authentication over the local
socket. The binary creates none of those files.

```sh
salmon-pg-backup config --database app | salmon-pg-backup run up                         # dump here, now
salmon-pg-backup config --database app --action schedule | salmon-pg-backup run up       # daily, at 03:17
salmon-pg-backup config --database app --over root@db1 --fetch-into ./dumps | salmon-pg-backup run up
```

**Run:** `--action dump --over` (upload, dump, fetch, the row found in the
fetched file) and `--action schedule` (the `cron.d` entry) against a real
Postgres in a qemu VM (`Test/PgBackupSpec.hs`, Layer 3); the generated script
and the freshness verdict are Layer 0 (`Test/PostgresBackupSpec.hs`). The
`--gcs-bucket` copy and the non-peer credential modes are not in those tests.
Code: [`PgBackup.hs`](../salmon-apps/src/PgBackup.hs).

## Machines and fleets

### salmon-init-locally

The local-machine salmon setup: a `salmon` user in the `sudo` group with no
password, `/etc/sudoers.d/salmon` granting it passwordless sudo, its home
directory, and, with `--authorized-key-file`, one public key appended to its
`authorized_keys` (the file is read on the commanding machine at `config`
time; the recipe only sees the key it is handed).

```sh
salmon-init-locally config --authorized-key-file ~/.ssh/id_ed25519.pub | sudo salmon-init-locally run up
```

**Run:** no test exercises it. Code:
[`Initializer.hs`](../salmon-apps/src/Initializer.hs),
over `SreBox.Initialize`.

### salmon-fleet

The controller's side of pull mode (see the
[`run serve` guide](serve-supervision.md)). It writes nothing a host reads.

- `status DIR [--label L] [--stale SECONDS] [--json]` folds the status
  documents that each host's `run serve --status-sink` writes into `DIR`, one
  line per host. Tab-separated when piped; a bordered table on a terminal
  (`--pretty`/`--no-pretty` force either).
- `keygen --out FILE` writes an Ed25519 key pair as JWK files: `FILE` (mode
  0600) and `FILE.pub`, which hosts are given as `--follow-key`. It refuses to
  overwrite an existing key.
- `sign --key FILE [--label L] [--out FILE]` wraps the document on stdin in a
  signed envelope; a host following another label refuses it.
- `describe` and `run DIR` expose `status` alone as a one-shot JSON tool for
  other programs.

```sh
salmon-fleet status /srv/salmon-status
salmon-fleet keygen --out fleet.jwk
salmon-fleet sign --key fleet.jwk --label web < directive.json > signed.json
```

**Run:** Layer 0: the table and the tab-separated output are pinned by goldens
(`salmon-apps/test/Test/FleetSpec.hs`), and the fold and the signature format
are tested in-process with the status sink and pull mode
(`Test/StatusSinkSpec.hs`, `Test/FollowSignatureSpec.hs`). Code:
[`Fleet.hs`](../salmon-apps/src/Fleet.hs).

### salmon-tui

A terminal client of a running `run serve`: over the unix socket given to
`--http PATH`, or over `--http-tcp`'s TLS listener with a bearer token read
from a file (refused if others can read it) and, optionally, a pinned
certificate. It reads `/dag` once, follows `/events`, and draws the node table;
`enter` expands a node (help, notes, edges, last check, a live tail of its
output), `r` re-reads, `q` quits. Only a line typed after `:` touches the loop,
sent as a serve command; every other read bypasses it.

```sh
salmon-tui $XDG_RUNTIME_DIR/my.http
salmon-tui https://host:8443 --token-file token --cacert server.crt
```

**Run:** the client underneath (`Salmon.Client.Model`, `Salmon.Client.Http`)
is tested: QuickCheck properties of the model against a reference, and the HTTP
client against a real in-process server over TLS (`Test/ServeTlsSpec.hs`). The
`brick` screen itself has no automated test. Code:
[`Tui.hs`](../salmon-apps/src/Tui.hs).

### salmon-report

A read-only report of what a network can do, built from probes: each probe is
an ordinary node whose `check` gathers one fact, and the binary's own driver
runs every `check` concurrently with a per-probe timeout and never calls `up`.
Nothing is converged, and no third-party service is contacted unless declared:
there is no default target, server or port list.

- `--name`, `--tcp HOST:PORT`, `--echo URL`, and UPnP/NAT-PMP discovery of the
  gateway (`--no-upnp`, `--no-natpmp` to skip);
- `--domain D` with `--ns`, `--zone PROJECT/ZONE` (read with `gcloud`),
  `--record` and `--resolver`: is the domain delegated to the servers meant to
  serve it, do they agree, do declared names resolve as the zone says;
- `--nmap HOST:PORTS`: one unprivileged connect scan of one declared host;
- `--stun HOST:PORT`: the mapped address each declared STUN server sees, and
  whether the NAT's mapping depends on the destination;
- `--inbound HOST:PORT[@LISTEN]` with `--vantage-ssh [USER@]HOST`: does a
  connection from a second, declared machine arrive;
- `--hairpin [ADDR:]PORT[@LISTEN]`: does a connection to the gateway's
  external address on an already-mapped port come back in.

```sh
salmon-report config --name example.org --tcp example.org:443 | salmon-report run report
salmon-report config --nmap example.org:22,443 | salmon-report run report --json
```

**Run:** end to end on loopback (nmap scans, a stand-in STUN responder, a
stand-in `ssh` for the inbound probes, hairpin with discovery turned off), with
parsers on captured or standard fixtures. **Not run:** a real STUN server,
NAT or gateway, a real second machine over `ssh`, `upnpc` itself, and the
`--zone` read through `gcloud`. The filtering half of the NAT type is not done.
The per-probe detail is in the [module notes](module-notes.md). Code:
[`Report.hs`](../salmon-apps/src/Report.hs).

## Google Cloud

### salmon-gcp-toy

A throwaway, tiered exercise of the `Gcp.*` builtins against a real project,
in a sandbox meant to be deleted. Tiers are cumulative and ordered by cost:
**0** a project (created by default), billing, APIs, a bucket, a service
account, IAM, an Artifact Registry repository; **1** an image built with
podman, pushed, and deployed to Cloud Run; **2** a VM trusting a
salmon-generated SSH CA, onto which the binary uploads and runs itself;
**3** a regional external Application Load Balancer in front of that VM.
When the toy created the project, `run down` removes every resource one by one
and then deletes the project, which sweeps whatever a buggy `down` left.

```sh
salmon-apps/scripts/gcp-toy-validate.sh -- \
  --project salmon-toy-$(date +%s) --organization ORG_ID --billing-account BILLING_ID --tier 0
```

The script drives `up` → `up` → `down` and reports what each pass did;
`salmon-apps/scripts/gcp-toy-serve.sh` keeps the same binary under `run serve`.

**Run:** tiers 0 and 1 against a real project; those runs found the
eventual-consistency retries and the authfile fix the
[playbook](gcp-toy-validation.md) records. **Not run against a real project
yet:** the one-pass tier 2, the peer, the secret delivery, tier 3's container,
the DNS zone and the HTTPS balancer; the playbook lists each gap and what was
run instead. Code: [`GcpToy.hs`](../salmon-apps/src/GcpToy.hs).

### salmon-cloudrun

A Cloud Run turnup from one JSON file: the services (deployed from images
already in a registry), the account each runs as (`{"create": ID}` or
`{"email": ADDRESS}`), its plain environment, the Secret Manager secrets bound
to it as variables or files, the secrets the turnup itself makes (optionally
uploaded from a `source_file` path), and optionally a regional external
Application Load Balancer in front. An unknown key in the file is refused, and
`config` collects every problem it finds before printing anything. No secret
byte is in the file, the directive, a node's text or a command line.

`run down` removes only what is provably this turnup's and holds no data: the
services, which carry a `salmon-turnup=NAME` label, and the balancer's
resources. Secrets, service accounts, grants, the proxy-only subnet and the
APIs are kept, and their nodes say so.

```sh
salmon-cloudrun config --file turnup.json | salmon-cloudrun run tree
salmon-cloudrun config --file turnup.json | salmon-cloudrun run up
```

**Layer 0 only.** The parser, the refusals, the folded graph and the deploy's
argument list are tested (`salmon-apps/test/Test/CloudrunSpec.hs`), and
`config` and `run tree` were run by hand on the documented example. No node's
`up`, `check` or `down` has been run, and nothing has been run against a real
project. The example file is in the module header of
[`Cloudrun.hs`](../salmon-apps/src/Cloudrun.hs).

## Test harnesses and demos

### salmon-toy-qemu-pg-ha

The `salmon-pgpair` demo, on three qemu guests the binary makes for itself (two
Postgres members and a bouncer), with a client that keeps writing while the
primary moves and says at the end how many inserts were acknowledged, failed,
or went missing. `prereqs` is the only part that needs root (debootstrapped
root filesystems); everything after it runs as an unprivileged user with two
capabilities granted once and membership of the `kvm` group.

```sh
t=$(cabal list-bin salmon-toy-qemu-pg-ha)
sudo $t config prereqs | sudo $t run up               # once: three root filesystems
$t config up --primary A --seed B | $t run up         # once: the pair, B built from A
$t config client --seconds 120 | $t run up &          # a client, writing
$t config up --primary B | $t run up                  # the switchover, under the client
```

Under `run serve` the guests stay booted and the demo is driven one line at a
time, with extra seeds for that: `guests`, `writer` (a client the server
holds), and faults as declarations, `frozen --machine M` and
`partition --machine M --from N` (`up` injects, `down` heals);
`salmon-apps/scripts/qemu-pg-ha-serve.sh` is those lines with names.

**Run:** the one-shot demo above (the module header quotes a run: 460 inserts
acknowledged, none failed, none missing); the seeds' graphs are Layer 0
(`salmon-apps/test/Test/QemuPgHaToySpec.hs`), and pausing and resuming a real
qemu process is tested without a guest. **Not run with guests booted:** the
`run serve` session, the partition route inside a guest, and the held writer
against a live pair. Its secrets are constants in the source, on purpose: it is
a demo to read, not a deployment. See the [Postgres pair guide](postgres-pair.md)
("Driving it live"). Code: [`QemuPgHaToy.hs`](../salmon-apps/src/QemuPgHaToy.hs).

### salmon-patroni-rootfs

The root filesystems of the three-VM Patroni test harness: one Debian rootfs
per guest under `/var/lib/salmon-test-vms/patroni-{1,2,3}/root`, each with
`postgresql`, `patroni`, `python3-etcd`, `etcd-server`, `haproxy` and
`pgbackrest` baked in (the guests have no network past boot), bootable over
9p, and with `/etc/ssh` handed to the user who typed `sudo` so the tests can
write each guest's SSH trust into it.

```sh
t=$(cabal list-bin salmon-patroni-rootfs)
sudo $t config prereqs | sudo $t run up
```

**Run:** it has no test of its own; what it builds is what the Layer 3
Patroni harness (`Test/PatroniVms.hs`) boots. Code:
[`PatroniRootfs.hs`](../salmon-apps/src/PatroniRootfs.hs).

## Repository tooling

### salmon-docs-sync

Salmon on its own repository: keeps this website (`docs/`, what GitHub Pages
serves) in step with what it is generated from. Its `check` fingerprints every
tracked file under `resources/`, `specs/`, `website/src/` and
`website/scripts/`, plus `README.md`, and compares the result with the stamp
committed as `docs/.docs-sync-stamp`; its `up` runs
`website/scripts/build-site.sh`, writes the stamp, commits `docs/` as
`docs: regenerate site`, and pushes with the repository's own authentication.
It touches nothing outside `docs/`. Run it once, from cron or a post-merge
hook, or keep it under `run serve` so a change is noticed without anybody
asking.

```sh
salmon-docs-sync config --repo . --no-push | salmon-docs-sync run up
```

**Run:** on this repository; its commits are in the history of `docs/`. It
has no test in the suite. Code: [`DocsSync.hs`](../salmon-apps/src/DocsSync.hs).
