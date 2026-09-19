# Postgres with automatic failover: salmon provisions, Patroni decides

Status: draft / not implemented. A design sketch to react to, not a committed
plan.

Companion: `pg-switchover.md` is the two-node, operator-driven design for
test harnesses and low SLAs. This one is for when nobody should have to be
awake for a failover.

## Position

**Salmon does not decide when to fail over, and does not know who the
primary is.** Safe automatic failover needs consensus, so that a split
network cannot elect two primaries, and a leader that demotes itself when it
loses the quorum, which is Patroni's form of fencing. `run serve` is one
process per machine with no coordination between them. Building consensus
into salmon would mean writing a worse etcd.

So the split of work is:

| Salmon | Patroni |
|---|---|
| installs and configures etcd, Patroni, routing | runs Postgres: initdb, start, stop, restart, promote, demote |
| supervises the Patroni and etcd systemd units | supervises Postgres |
| states the cluster-wide config it wants | applies it, restarting members when a setting needs it |
| creates databases, roles, templates, clones, runs migrations, **through the leader** | holds the leader key in etcd, i.e. the only answer to "who is primary" |
| backups and archiving | uses the archive to rebuild replicas |

The chain of ownership is salmon → systemd → Patroni → Postgres. Salmon's
supervision (`Actions/Upkeep.hs`) must stop at the Patroni unit: a salmon
node that restarts Postgres is fighting its owner.

## The consequence for everything that exists

**The primary's location never appears in a directive, a `ref`, `help` or
`notes`.** This is the opposite of `pg-switchover.md`, where the operator's
declaration *is* the truth. Here, a declared primary would be stale after the
first automatic failover, and the next `run up` would try to "correct" it.

Existing builtins that must **not** run on a Patroni member:

| Builtin | Replaced by |
|---|---|
| `createCluster`, `pgLocalCluster` (initdb, start) | Patroni's bootstrap on the first member, and its replica creation on the others. Debian's `postgresql@` units must be disabled or masked, or two supervisors race for the data directory. |
| `startCluster` / `stopCluster` / `restartCluster` / `promoteCluster` | nothing; `patronictl` if an operator must |
| `alterSystemSet`, `reloadConf` | the DCS config node (below). `ALTER SYSTEM` still "works", but Patroni can overwrite what it sets and it does not reach the other members |
| `hbaLine`, `allowReplicationFrom`, `allowClientCertFrom` | `postgresql.pg_hba` in `patroni.yml`, or the DCS config |
| `primaryReplicationSetup`, `standbyReplicationSetup`, `replicationSlot` | Patroni: it creates replicas and manages physical slots for members |

Builtins that still apply, **as long as they reach the leader:** `database`,
`user`, `group`, `grant`, `adminScript`, templates and clones,
`SreBox.PostgresMigrations`. Every one of them runs
`sudo -u postgres psql -p <port>` on the local machine today. On a replica,
`CREATE DATABASE` fails with "cannot execute ... in a read-only transaction".
See "Reaching the leader".

## New builtins

### `Etcd`

- Renders the config and a systemd unit; the `check` is
  `etcdctl endpoint health` against the member itself.
- **The trap is bootstrap.** `initial-cluster-state` is `new` exactly once in
  a cluster's life. After that, a new member is added by a runtime call
  (`etcdctl member add`) followed by starting it with `existing`, and it
  cannot be expressed as config alone. A node for "etcd cluster of these
  three" therefore has a seed phase (all start with `new` when no member
  answers) and a join phase (member add, then start). Its `check` compares
  the member list, and does not look at a config file.
- TLS: peer and client certificates come in pre-provisioned, per the recipes'
  secret-transport rule. `Certificates` can mint them in a recipe that owns
  that choice; the builtin only takes paths.
- Several Patroni clusters can share one etcd (each has its own `scope`
  and `namespace`). This is how `pg-ha-control-plane.md`'s cross-replicated
  pair becomes safe: two Postgres machines plus a small third one running
  only etcd, with three votes between them.

### `Patroni`

- Renders `patroni.yml` (an `EncodeFileContents` value, so its content
  fingerprint reaches `notes` and a re-declaration is `Stale` under
  `run serve`), plus a systemd unit.
- The `check` reads the local REST API on port 8008:
  - `GET /health`: is Postgres running under Patroni;
  - `GET /patroni`: state, role, `pending_restart`, and whether the member is
    in the cluster at all.
  - Role is reported, never judged: a replica is as healthy as a leader.
- Debian's `patroni` package ships its own integration with the
  `postgresql-common` layout (`pg_createconfig_patroni` and a per-cluster
  `patroni@` unit). Whether to build on it or render our own is an open
  question below.

### The cluster-wide config node

This is `ALTER SYSTEM`'s replacement. Patroni keeps
`postgresql.parameters` and friends in etcd, applied on every member.
- `check`: `GET /config`, comparing only the keys this node declares, so that
  other declarations or operators can own other keys.
- `up`: `PATCH /config` with those keys.
- A second `check` condition, the same move as `checkService` reading
  `NeedDaemonReload`: after a restart-only setting changes, `/patroni` on each
  member says `pending_restart`. The node is not `Success` until nothing is
  pending, and its `up` can trigger `patronictl restart --pending`, which
  restarts replicas before the leader. Whether salmon should restart members
  at all, or only report, is an open question.

### Routing to the leader

Three options, from least to most machinery. The recipe should offer the
first and one of the others:

1. **libpq multi-host connection strings:**
   `host=a,b,c target_session_attrs=read-write`. No new process; works only
   for libpq clients and only at connect time.
2. **`vip-manager`:** a floating IP that follows the leader key in etcd.
   Needs one L2 segment.
3. **HAProxy** with `option httpchk` on `GET /primary` (port 8008): the
   standard Patroni setup. One port for the leader, one for replicas
   (`GET /replica`). It sits in front of, or replaces the upstream of, the
   existing `PgBouncer`.

A `Haproxy` builtin would have the same shape as `Nginx`: a config value, a
renderer, and a systemd unit. Unlike pgbouncer, HAProxy needs no reload on
failover: its health checks move the traffic.

## Reaching the leader

Admin nodes need a way to reach whichever member is the leader right now,
which is not known when the directive is written.

The recommendation is to **address the leader by its routed endpoint** (the
HAProxy primary port, or the VIP) with a connection string, not by
`sudo -u postgres` on a machine. That means connection-string variants of the
admin builtins. `SreBox.PostgresMigrations.remoteMigrateOpaqueSetup` already
works this way; `database`, `user`, templates and clones do not. This is the
largest single piece of work in this spec, and it is independently useful:
anything managed (Cloud SQL, see `Gcp/PostgrestCloudRun`) has the same "no
local superuser" shape.

The alternative, running the admin nodes on every member with a `check` that
answers `Skipped` when this member is not the leader, is smaller but racy:
- the leader can change between the check and the `up`;
- templates would be built by whichever member was the leader at the time.

Refused.

## Nodes that must run on exactly one member

Backups (from a replica, by preference) and scheduled jobs (`CronTask`)
would otherwise run on every member.
- **Backups:** a Patroni tag such as `nofailover` or a custom one marks the
  member dedicated to them. The scheduled command gates itself with
  `curl -sf localhost:8008/replica`, skipping when the member is not a
  replica.
- **PITR:** `SreBox.PostgresBackup` is `pg_dump`, which has no point-in-time
  recovery and no use to Patroni. A continuous archive (pgBackRest or WAL-G,
  both packaged by Debian) earns its place twice: as the disaster recovery
  story, and as Patroni's `create_replica_methods`, which rebuilds a replica
  from the archive instead of loading the leader with a base backup.

## Switchover, under Patroni

`pg-switchover.md`'s role node has a counterpart here, and it is opt-in: a
**preferred leader**.
- `check`: `GET /leader` names the preferred member.
- `up`: `POST /switchover`.

The default is *no preference*. With a preference declared and supervision
on, salmon would move the leader back after every automatic failover, as
soon as the preferred member is healthy. That is sometimes what is wanted
(a preferred site or a bigger machine) and sometimes a flap. It must be a
choice, never the default.

## Disaster scenarios

These reuse `pg-switchover.md`'s scenario catalogue where the cause is the
same, with different expected outcomes, plus the ones only consensus makes
meaningful.

**Machines:** three VMs, each with `postgresql`, `patroni`, `etcd-server` and
`haproxy` in the rootfs (the guests cannot reach the network past boot,
see `Test.PostgresReplicationSpec`). The addresses are
`testVmAddr`/`testVmAddr2`/`testVmAddr3`. HAProxy and the client loop run on
one of them, or on a fourth VM.

| # | Scenario | Assert |
|---|---|---|
| T1 | Kill the leader's machine | a new leader within the TTL (about 30s by default); writes through HAProxy resume; the old leader rejoins as a replica when it comes back |
| T2 | Partition the leader from the other two | the old leader **demotes itself** once its lease expires; at no moment do two members accept writes (the client loop writes through each member's own port and records which accepted) |
| T3 | **After T1, rerun `run up` on every member and on the controller** | **nothing changes**: no restart, no config write, no role change. This is the salmon-specific assertion: salmon does not fight Patroni |
| T4 | Change a restart-only parameter through the config node | members restart replicas first; `pending_restart` clears; the node's check reaches `Success` |
| T5 | Lose etcd quorum (stop two of three) | with `failsafe_mode` off, the leader goes read-only; with it on, the leader keeps writing while it can reach all members. Pins down which mode the recipe defaults to |
| T6 | Create a database and a clone through the routed endpoint, fail over, repeat | both work on each leader; no admin node ever ran against a replica |
| T7 | Add a fourth member | etcd's join phase runs, not a bootstrap; Patroni builds the replica from the archive if one is configured |

T3 is the test the whole design rests on, and the reason "the primary never
appears in a directive" is a rule rather than a guideline.

## Phased plan

1. **Connection-string admin builtins** (`database`, `user`, grants,
   templates, clones). Useful today for managed Postgres, and required here.
2. **`Etcd`,** with the seed and join phases; Layer 3, three VMs, including
   a restart of one member.
3. **`Patroni`** plus the cluster-wide config node; T1, T3, T4.
4. **Routing:** multi-host connection strings first, then HAProxy; T2, T6.
5. **Archive and PITR** (pgBackRest or WAL-G), and backups on one member; T7.
6. **The opt-in preferred leader.**

## Open questions

- **Debian's `pg_createconfig_patroni` and `patroni@` units, or our own?**
  Theirs fits `postgresql-common`'s layout, which the rest of `Postgres.hs`
  assumes (`pg_lsclusters`, `/var/lib/postgresql/$version/$cluster`). Ours is
  simpler to reason about, and portable to non-Debian hosts.
- **Should the config node restart members** (`patronictl restart
  --pending`), or report `pending_restart` and leave the restart to an
  operator? Restarting is what `run up` means everywhere else. A restart of
  the leader under load is also a small outage.
- **`failsafe_mode` default** (see T5).
- **REST API authentication:** Patroni's unsafe endpoints (`/switchover`,
  `PATCH /config`, `/restart`) need `restapi.authentication`. The
  credentials come in as a pre-provisioned file.
- **Which etcd API:** Patroni's `etcd3:` section, the v3 API, is the only
  sensible one with current etcd. Say so in the builtin and do not offer v2.
