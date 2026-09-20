# Two-node Postgres with salmon-driven switchover and failover

Status: partly implemented, and the rest is still a sketch to react to.

Done: the prerequisites P1-P5; the state table and the probe
(`SreBox.PostgresPair`, phase 2), covered at Layer 0 including every
refusal; the role node over ssh (phase 3), covered by a Layer 3 test that
moves a real primary between two VMs and back (S1, minus the client
assertions).

Not done: symmetric member nodes and the seeding clone, so a pair is still
built by hand, as `Test.PostgresSwitchoverSpec` does; bouncer routing (phase
4), so `PauseBouncers`/`RepointBouncers` are in the table with nothing behind
them; `pair_may_discard`'s failover path, written but tested only at Layer 0
(phase 5); the slot budget and re-seeding (phase 6). Scenarios S2-S8 are
unwritten.

Companion: `pg-patroni.md` covers the other end of the range, with automatic
failover and three voters. `pg-ha-control-plane.md` is the wider
bouncer/app/LB picture that either one plugs into.

## What this is for

A primary and a streaming standby on two machines. Salmon moves the primary
between them when an operator changes a declaration. Nothing decides on its
own that a machine is dead.

Two audiences:

1. **Disaster tests in the harness.** The declared primary is a knob a test
   turns. The same node that runs a switchover can also bring a scenario
   back to a known state, and every failure mode below is something a
   Layer 3 test should be able to cause on purpose and then assert about.
   See "Disaster scenarios", which is as much the point of this spec as the
   recipe itself.
2. **Services with a low SLA,** where a few minutes of downtime and a human
   deciding "fail over now" are acceptable, and two machines are what the
   budget allows.

It is **not** for anything that needs to survive an unattended failure at
3am. That takes consensus, and consensus takes three voters. See
`pg-patroni.md`.

What it promises:

| Event | Data lost | Time to recover |
|---|---|---|
| Planned switchover | none: the old primary is stopped cleanly and the standby is checked to have its last record before promotion | seconds; with pgbouncer `PAUSE`, clients see a delay rather than errors |
| Unplanned failover (the primary died) | whatever had not reached the standby, i.e. the replication lag at the moment of death | however long the operator takes to decide, plus seconds |
| Network partition, no operator action | nothing: salmon refuses to act | none: the primary stays up and the standby falls behind |

An optional `pair_synchronous` could take the unplanned row's data loss to
zero, at the usual two-node price: when the standby is down, every commit
blocks. See "Open questions".

## Where the layout question landed

Two machines are fine for this design, and bad for automatic failover. The
costs of the cross-replicated shape (each machine primary for one cluster
and standby for the other):

- each machine must be sized to carry both primaries, since that is what
  happens when one dies; spreading them buys headroom, not smaller boxes;
- losing one machine takes away the redundancy of both clusters at once;
- maintaining a machine means switching both clusters over first.

**The primary's location is a per-pair field in the directive**, so "A is
primary for everything" and the cross-replicated shape are the same recipe
with different values. Start with the former. Nothing below depends on the
choice.

## Prerequisites: fixes to what exists, each worth doing on its own

These are defects in `Salmon.Builtin.Nodes.Postgres` and `PgBouncer` as they
stand. The rest of the design assumes they are fixed.

**P1. `standbyReplicationSetup` can wipe a promoted standby.**
`cloneFromPrimaryScript`'s guard is "does `standby.signal` exist". Promotion
deletes that file, so the next `run up` with an unchanged directive runs
`rm -rf "$datadir"` and clones from the old primary again. If the old primary
is down, which is the usual reason for promoting, the clone fails *after* the
delete. The guard should compare system identifiers instead: the local
`pg_controldata`'s "Database system identifier" against the primary's
`SELECT system_identifier FROM pg_control_system()`.
- **Equal:** this data directory already belongs to the cluster, as a
  standby or as a promoted one. Never touch it.
- **Different:** it is foreign. Clone only if it holds nothing but what
  `pg_createcluster` made (no databases beyond `postgres`/`template0`/
  `template1`), and otherwise refuse with the reason.

**P2. `pg_rewind` is impossible on these clusters today.** It needs
`wal_log_hints = on` or data checksums, and nothing in `salmon-ops/src` sets
either. Without it, rejoining an old primary means cloning it again from
scratch. Add `wal_log_hints = on` to `primaryReplicationSetup`'s settings. It
is restart-only, so it has to be in place before there is data worth
protecting. Add `max_slot_wal_keep_size` too (see scenario S6): without a
cap, a standby that is down for long enough fills the primary's disk through
its slot.

**P3. `primaryReplicationSetup` restarts the primary on every `run up`**,
because `restartCluster` (via `clusterCtl`) has no `check`. The natural check
for a restart node is "is any setting waiting for one":
`SELECT count(*) FROM pg_settings WHERE pending_restart`. That is `Success`
at zero, which is the same move as `checkService` reading
`NeedDaemonReload`. `startCluster`'s named-cluster path has the same
non-idempotent start that `detectVersionAndStartMainCluster` had before the
templates PR.

**P4. A changed `pgbouncer.ini` is never applied by `run up`.**
`PgBouncer.setup` goes through `Systemd.systemdService`, whose `checkService`
says `Success` for an active, enabled unit whose *unit file* has not changed.
`configFiles` rewrites the ini as a dependency, but nothing reads the ini's
change, so the service is skipped. Routing a switchover through pgbouncer
needs a `RELOAD`. Do it over the admin console rather than with a restart,
which would drop every client connection. That also needs an admin user in
`BouncerConfig`.

**P5. The replication password travels inside a `bash -c` string.**
`cloneFromPrimaryScript` builds `PGPASSWORD=... pg_basebackup` in the script
text. Use a passfile pre-provisioned on the machine, per the recipes'
secret-transport rule.

## Shape of the recipe

A new `SreBox.PostgresPair`, three kinds of node.

### 1. Members are symmetric

Both machines get the same configuration, whichever is primary today:
- `wal_level`, `max_wal_senders`, `max_replication_slots`, `hot_standby`,
  `wal_log_hints`, `max_slot_wal_keep_size`;
- `pg_hba` lines for replication *from the peer*, and for the rewind role
  (below) from the peer;
- the replication role and the rewind role. Roles live in the catalog, so
  they are created on whichever member is primary and replicate to the other.

**No member node mentions a role in its `ref`, `help` or `notes`.** A
switchover then changes exactly one node's declaration, the role node's, and
under `run serve` (I6) the members are not marked `Stale` by it. This replaces
the `primaryReplicationSetup`/`standbyReplicationSetup` asymmetry for this
recipe. Those two stay for the existing fixture and
`Test.PostgresReplicationSpec`.

The rewind role needs a plain login, not a replication one, with `EXECUTE` on
`pg_ls_dir(text, boolean, boolean)`, `pg_stat_file(text, boolean)`,
`pg_read_binary_file(text)` and `pg_read_binary_file(text, bigint, bigint,
boolean)`. That is the documented minimum for `pg_rewind --source-server`
without a superuser.

### 2. Seeding the standby is a one-time clone

This is the `pg_basebackup` step, with P1's guard. It runs once in a pair's
life, and again only if a standby is lost beyond repair (S6). It is
deliberately **not** how an old primary rejoins after a switchover; that is
`pg_rewind`, inside the role node.

### 3. The role node: "this pair's primary is on B"

```haskell
data Side = A | B

data Member = Member
    { member_remote :: Ssh.Remote       -- how the controller reaches it
    , member_host :: Postgres.Host      -- how the peer and the bouncers reach it
    , member_cluster :: Postgres.ClusterName
    , member_port :: Postgres.Port
    }

data Pair = Pair
    { pair_name :: Text
    , pair_a, pair_b :: Member
    , pair_primary :: Side
    , pair_may_discard :: Maybe Side   -- see "Failover and split brain"
    , pair_repl_role, pair_rewind_role :: Postgres.RoleName
    , pair_passfile :: FilePath        -- on each member, pre-provisioned
    , pair_bouncers :: [Bouncer]       -- admin-console endpoints to PAUSE/RELOAD/RESUME
    , pair_step_timeout :: Int         -- seconds to wait for the standby to catch up
    }
```

`ref = mkRef "pg-pair-role" pair_name`: keyed on the pair, never on who is
primary, so switching primaries changes that one node rather than creating
another. `notes` carry the declared primary and `pair_may_discard`, so a
re-declaration is a visible change (I6).

The node runs on a **controlling machine** and reaches both members over
`Ssh.callWith` (or `Nodes/Self.hs`'s `callSelf` for anything bigger than a
script). It cannot run on a member: the member that dies might be the one
running it.

#### Observation, then a pure verdict

One ssh round trip per member returns a small `key=value` report:
- cluster status from `pg_lsclusters`;
- if it is running: `pg_is_in_recovery()`, the timeline, the current or
  replay/receive LSN, the upstream host from `pg_stat_wal_receiver`, and
  `system_identifier`;
- if it is stopped: `pg_controldata`'s latest checkpoint location, timeline
  and system identifier;
- from the bouncers, whether each one is paused (`SHOW DATABASES` has a
  `paused` column).

```haskell
data Observed
    = Unreachable Text
    | Stopped { o_sysid :: Word64, o_timeline :: Int, o_checkpoint :: Lsn }
    | Primary { o_sysid :: Word64, o_timeline :: Int, o_lsn :: Lsn }
    | Standby { o_sysid :: Word64, o_timeline :: Int, o_upstream :: Maybe Text, o_received, o_replayed :: Lsn }

data Step
    = Done
    | Degraded Text           -- the declaration holds, but the pair is not redundant
    | PauseBouncers
    | StopMember Side          -- a clean, fast shutdown
    | AwaitCatchUp Side Lsn    -- the standby must have received past this
    | Promote Side
    | Rejoin Side              -- pg_rewind -R against the primary, then start
    | StartMember Side
    | RepointBouncers Side     -- rewrite ini, RELOAD, RESUME
    | Refuse Text

nextStep :: Pair -> Observed -> Observed -> [BouncerState] -> Step
```

`parseObserved` and `nextStep` are pure, in the `Systemd.interpretShow` /
`Postgres.interpretTemplateRow` pattern. Layer 0 can then cover the whole
table below, including every refusal, without a database.

- **`check`** is `observe >>= nextStep`: `Done` is `Success`, `Degraded` is
  `Unknown`, and anything else is a `Failure` naming the step.
- **`up`** loops: observe, compute the step, perform it, and repeat until the
  step is `Done` or `Degraded`, throwing on `Refuse`. There is a bound on
  iterations so that a fault in the table cannot spin forever.

Because the state is re-derived from observation on every turn, **`up` can
resume from any point.** A controller killed between "stop A" and
"promote B" leaves a pair the next run recognises and finishes. There is no
progress file to lose or to disagree with reality.

`Degraded` maps to `Unknown` on purpose. Under `run serve`, `Unknown`
"restarts nothing" (see `Actions/Upkeep.hs`). "The primary is where it should
be, and the peer is unreachable" is exactly a case where supervision should
keep looking and must not act.

#### The table (declared primary: B, peer: A)

Checked in order, first match wins:

| Observed A | Observed B | Step |
|---|---|---|
| any sysid ≠ B's sysid | | `Refuse "not the same cluster"` |
| Standby streaming from B | Primary | bouncers point at B and are unpaused? `Done` : `RepointBouncers B` |
| Standby, upstream not B | Primary | `Rejoin A` (the rewind is a no-op if nothing diverged; `-R` repoints it) |
| Stopped | Primary | `Rejoin A` |
| Unreachable | Primary | `RepointBouncers B` if needed, else `Degraded` |
| Primary, not `may_discard` | Primary | `Refuse "two primaries"` |
| Primary, `may_discard = A` | Primary | `StopMember A`, then `Rejoin A`: A's divergent writes are lost, as declared |
| Primary | Standby of A | `PauseBouncers`, then `StopMember A` |
| Stopped (checkpoint c) | Standby, received < c | `AwaitCatchUp B c` (on timeout: `StartMember A`, resume, `Refuse`) |
| Stopped (checkpoint c) | Standby, received ≥ c | `Promote B` |
| Unreachable, not `may_discard` | Standby | `Refuse "cannot confirm A is stopped"` |
| Unreachable, `may_discard = A` | Standby | `PauseBouncers`, then `Promote B` |
| Standby | Standby | promote B only if B has received at least as much as A, else `Refuse` |
| Stopped | Stopped | `StartMember B` |

Paused bouncers are a state of their own. If `up` dies with them paused,
every client is stuck, so any bouncer found paused makes the check fail, and
every terminal step, `Refuse` included, resumes the bouncers salmon paused.

`Rejoin` is `pg_rewind -R --source-server=<B>` as the rewind role, then a
start. `-R` writes `standby.signal` and `primary_conninfo`. It also:
- creates the physical slot for A on B, which must happen before A starts
  streaming, since slots are not replicated;
- drops A's now-stale slot for B, which on a standby would keep WAL forever.

Whether `pg_rewind -R` writes the recovery configuration when it reports "no
rewind required", which is the normal case after a clean switchover, needs
checking during implementation. If not, write it directly.

## Failover and split brain: `pair_may_discard`

The operator's escape hatch is one field, and its name states what it costs:
"I accept losing writes on this side that the other side does not have."

The same flag covers two cases:
- **Failover:** A is unreachable and B is promoted without proof that A has
  stopped.
- **Split-brain resolution:** both sides are primaries and A is rewound onto
  B's history.

At `configure` time it must name the side that is *not* the declared
primary; any other value is rejected there.

Salmon cannot fence a machine it cannot reach. What makes promoting B safe
enough for a low SLA is **routing**, not fencing: if `pg_hba` only admits
application traffic from the bouncers' hosts, then repointing the bouncers is
the fence. A reachable A that comes back as a primary is stopped and rewound
by the next pass. Its writes in the gap are the loss the flag already
declared. The recipe should say this plainly in its haddock and in the check's
`Failure` text, instead of implying a fence it does not have.

## Routing

The bouncers' upstream is always the *declared* primary. Admin nodes that
must run on the primary (`database`, `user`, templates, clones, migrations)
also target the declared primary, so the `Track' Postgres.Server` they take
is simply B's. This is where the design is simpler than Patroni's, where the
primary has to be discovered at run time.

A switchover is `PAUSE` on every bouncer, the Postgres steps, a rewritten ini,
`RELOAD`, then `RESUME`. With `pool_mode = transaction`, `PAUSE` waits for
transactions in flight, so clients see latency rather than errors. That makes
scenario S1's "no client errors" a real assertion.

## Disaster scenarios

A catalogue of what to cause and what to assert, meant to run as Layer 3
tests. Each is a scenario description plus the assertions it must pass; if the
Patroni spec reuses them against its own backend, the expected outcomes differ
and the causes do not.

**Machines:** three VMs:
- `pg-primary` and `pg-standby` (rootfses that already exist, plus
  `pgbouncer`);
- a third VM for the bouncer and a client loop.

The controller is the test process, reaching the guests with the harness's
SSH CA key, which should map onto `Ssh.ClientOpts`' identity and
known-hosts fields.

**Ways to cause trouble**, all things the tree already has:
- `sshToVm` with `systemctl kill -s KILL postgresql@...` for a crash;
- `Netfilter.rule` dropping traffic between the two members for a partition,
  which dogfoods the builtin;
- a controller interrupted after step *k*, done by running `up` with the
  action wrapped to throw after *k* steps.

| # | Scenario | Assert |
|---|---|---|
| S1 | Switch A→B, then B→A, with a client inserting through the bouncer the whole time | every acknowledged insert is present; the client saw zero errors; a rerun of `run up` changes nothing |
| S2 | S1, with the controller killed after each step *k* in turn | a plain rerun finishes; no bouncer is left paused; the result is the same as S1 |
| S3 | Crash A; declare B with `may_discard = A`; restart A | B serves writes; A rejoins as B's standby through `pg_rewind`, not a re-clone; rows acknowledged before the last replicated LSN survive |
| S4 | Partition A from B, with no change to the declaration | the check is `Unknown`/`Degraded`; nothing is promoted; after the partition heals, B catches up |
| S5 | Partition, fail over to B with `may_discard = A` while A still accepts direct writes, then heal | the first pass after healing sees two primaries and rewinds A; A's gap writes are gone, as declared; without the flag, it refuses |
| S6 | Stop B; write past `max_slot_wal_keep_size` on A | A's disk is bounded; the slot reports `wal_status = 'lost'`; the pair is `Degraded`, and re-seeding B (the only fix) is an explicit re-clone, not a silent one |
| S7 | Stop both; declare B | B starts first; A rejoins |
| S8 | A standby from a *different* cluster (fresh `initdb`) where A should be | `Refuse "not the same cluster"`, and nothing is deleted |

S2 and S8 matter most. S2 proves resumability, which is the design's main
claim. S8 proves the refusal that P1 is about.

## Phased plan

1. **P1–P5,** each with its own test. P1 gets a Layer 3 case in
   `PostgresReplicationSpec`: promote the standby, rerun `run up`, and assert
   the data survives.
2. **`Observed`, `parseObserved`, `nextStep`,** and Layer 0 tests over the
   whole table.
3. **Symmetric members, the seed clone and the role node,** without bouncers;
   S1 (minus the client assertions), S2, S7, S8.
4. **Bouncer routing,** PAUSE/RELOAD/RESUME; the rest of S1.
5. **`pair_may_discard`,** failover and split brain; S3, S4, S5.
6. **The slot budget and re-seeding;** S6.

## Open questions

- **`pair_synchronous`.** Synchronous replication gives zero loss on
  failover, but with two nodes it blocks every commit while the standby is
  down. If it is offered, `Promote` must also clear `synchronous_standby_names`
  on the new primary, or the survivor blocks waiting for the dead member.
  Leaning towards leaving it out of v1.
- **Minimum Postgres version: 13.** It brings `max_slot_wal_keep_size`,
  `wal_status`, `pg_rewind -R` and its automatic crash recovery, and a
  reloadable `primary_conninfo`. The templates already need 13 for
  `DROP DATABASE ... WITH (FORCE)`.
- **Should the symmetric members replace `primaryReplicationSetup` /
  `standbyReplicationSetup`,** once the fixture and
  `PostgresReplicationSpec` are ported? They are strictly less capable, and
  they are where P1 and P3 live.
- **Controller placement for real deployments.** In the harness it is the test
  process. For a low-SLA service it is whatever machine runs `run up` or
  `run serve` for the pair. Is it worth also stating that it must not be one
  of the two members?
