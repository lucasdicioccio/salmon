# Two-node Postgres with salmon-driven switchover and failover

Status: partly implemented, and the rest is still a sketch to react to.

Done: the prerequisites P1-P5; the state table and the probe
(`SreBox.PostgresPair`, phase 2), covered at Layer 0 including every
refusal; the role node over ssh (phase 3) and `pair_may_discard`'s failover
and split-brain paths (phase 5), covered by Layer 3 tests that move a real
primary between two VMs and back (S1, minus the client assertions), stop a
controller after each step in turn and let an ordinary pass finish it (S2),
kill a primary outright and fail over onto its standby (S3), cut the two
machines off from each other and change nothing (S4), and fail over across a
partition that hides the old primary from the controller too, then heal it
and rewind the loser (S5).

Also done: the slot budget (phase 6), in that each member streams with a slot
the pair names and the rejoin creates, and a slot that falls off the budget
is reported rather than rewound at (S6). The *re-seeding* half of phase 6 is
not: there is no seeding node, so "re-seed it" is still something an operator
does by hand -- which is why the check can only name the problem.

The disaster catalogue S1-S8 is written, and running it is what most of the
design above was decided by.

Phases 3 and 4 are now whole: `member` configures a machine to be either half
of the pair without naming a side, `bouncerSetup` stands a pgbouncer up in
front, `pairOp` is the three of them as one declaration, and
`salmon-pgpair` is a binary an operator can type at. `PauseBouncers` and
`RepointBouncers` have commands behind them, and `decide` asks the bouncers
rather than assuming there are none.

Also done: the seeding clone (`seedMember`, declared through `pair_seed`),
and S1's client assertions -- 460 inserts through pgbouncer across a
switchover, no errors, nothing lost.

There are two ways to watch it. `Test.PgPairDemoSpec` asserts it on three
VMs the harness boots. `salmon-toy-qemu-pg-ha` is the same thing as a
binary that makes its own guests, where moving the primary is a command you
type -- and where the three steps that used to be a README (debootstrap,
`ensureVm9pBoot`, handing `/etc/ssh` to the unprivileged user) are a `prereqs`
seed that declares them.

Not done: nothing in this spec, though `pg-patroni.md` is still a sketch.

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

Built, as `member` / `memberScript`. It runs over ssh from the controller
like everything else here, which is why it renders SQL rather than reusing
the nodes in `Nodes/Postgres.hs`: those are ops that run *on* the machine
they configure, and nothing in this recipe does. What it does reuse is that
module's opinion about which settings replication needs
(`replicationSettings`), so one place decides. Role passwords are read on the
member out of the `.pgpass` files the pair already names, and fed to `psql`
on standard input — never in the script, never on a command line, never in a
report.


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

Not built yet, and S6 is what says how it should behave when it is: a lost
slot leaves the pair in a state only a re-seed fixes, and the role node
deliberately does not fix it. Wiping a machine's data directory is a decision
about losing whatever is on it, which is the same class of decision as
`pair_may_discard` and belongs to the same place — an operator saying so —
rather than to a pass that runs unattended. So the seeding node should be
*separately declared*, and the role node's job is to name the problem
precisely enough that the operator knows which machine to declare it for.

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
  replay/receive LSN, the upstream host from `pg_stat_wal_receiver`, the
  *configured* upstream from `primary_conninfo`, and `system_identifier`;
- if it is stopped: `pg_controldata`'s cluster state, latest checkpoint
  location, minimum recovery point, timeline and system identifier;
- if it is running: the replication slots it holds and each one's
  `wal_status`, which is the only place the fate of the *other* member's
  catching-up is written down;
- from the bouncers, whether each one is paused (`SHOW DATABASES` has a
  `paused` column).

```haskell
data Observed
    = Unreachable Text
    | Stopped { o_sysid :: Word64, o_timeline :: Int, o_checkpoint :: Lsn, o_clean :: Bool }
    | Primary { o_sysid :: Word64, o_timeline :: Int, o_lsn :: Lsn, o_slots :: [(Text, Text)] }
    | Standby { o_sysid :: Word64, o_timeline :: Int, o_upstream, o_configured :: Maybe Text, o_received, o_replayed :: Lsn }

data Step
    = Done
    | Degraded Text           -- the declaration holds, but the pair is not redundant
    | PauseBouncers
    | StopMember Side          -- a clean, fast shutdown
    | AwaitCatchUp Side Lsn    -- the standby must have received past this
    | AwaitStreaming Side      -- pointed here, not connected: wait, do not rewind
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

A standby is asked two questions about its upstream, and the difference
between them is what keeps a partition from becoming an outage. `o_upstream`
is where it *is* streaming from, which is empty the instant the connection
drops; `o_configured` is where `primary_conninfo` *tells* it to stream from,
which it keeps through a partition. Reading only the first, a standby that
cannot reach its primary is indistinguishable from a standby that belongs to
somebody else -- and the answer to the second of those is `Rejoin`, which
stops the standby and then fails, because whatever keeps it from streaming
keeps `pg_rewind` from reading too. A partition would take the standby down.
Reading both, the answer is `AwaitStreaming`: wait, and if the waiting runs
out say so as `Unknown` rather than as a failure, because "it came back a
second ago" and "it has been cut off for an hour" are the same observation.

Two of those positions are not the field whose name they carry, and writing
S3 is what found it. A **stopped** cluster's position is
`max(latest checkpoint, minimum recovery point)`: a standby that was stopped
replayed past its last checkpoint, and only the second field says so. A
**standby's** is `max(received, replayed)`, because
`pg_last_wal_receive_lsn()` is `NULL` — not a position — in a server that has
received nothing since it started, which is the state of every standby whose
primary has just died, i.e. exactly when a failover needs the number.

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
| Standby or Stopped, and B's slot for A is `lost` | Primary | `Degraded "the slot is lost"`: the WAL it needs has been recycled, so waiting produces nothing and a rewind changes nothing. Only a re-seed helps, and that is an operator's call |
| Standby, pointed at B, not streaming | Primary | `AwaitStreaming A`, and `Unknown` once the waiting runs out |
| Standby, pointed anywhere else | Primary | `Rejoin A` (the rewind is a no-op if nothing diverged) |
| Stopped | Primary | `Rejoin A` |
| Unreachable | Primary | `RepointBouncers B` if needed, else `Degraded` |
| Primary, not `may_discard` | Primary | `Refuse "two primaries"` |
| Primary, `may_discard = A` | Primary | `StopMember A`, then `Rejoin A`: A's divergent writes are lost, as declared |
| Primary | Standby of A, streaming | `PauseBouncers`, then `StopMember A` |
| Primary | Standby of A, not streaming | `AwaitStreaming B`: a clean stop hands the tail over through that connection, so stopping A without one strands whatever B has not got |
| Stopped, `may_discard = A` | Standby | `Promote B`: the loss is already accepted, and waiting on a machine that will send nothing more is a slower way to the same place |
| Stopped, crashed | Standby | `Refuse "A did not stop cleanly"` |
| Stopped cleanly at c | Standby, received < c, still streaming | `AwaitCatchUp B c`: the tail is in flight |
| Stopped cleanly at c | Standby, received < c, not streaming | `StartMember A`: nothing will arrive from a stopped machine, so start the one that holds the records and let B catch up from it |
| Stopped cleanly at c | Standby, received ≥ c | `Promote B` |
| Unreachable, not `may_discard` | Standby | `Refuse "cannot confirm A is stopped"` |
| Unreachable, `may_discard = A` | Standby | `PauseBouncers`, then `Promote B` |
| Standby | Standby | promote B only if B has received at least as much as A, else `Refuse` |
| Stopped | Stopped | `StartMember B` |

Paused bouncers are a state of their own. If `up` dies with them paused,
every client is stuck, so any bouncer found paused makes the check fail, and
every terminal step, `Refuse` included, resumes the bouncers salmon paused.

`Rejoin` is `pg_rewind -R --source-server=<B>` as the rewind role, then a
start — with three things around it that the bare command does not do, each
of which was a failure before it was a line of script.

**The recovery configuration is written here, not by `-R`.** `pg_rewind` is
entitled to decide no rewind was needed at all, which is the ordinary case
after a clean switchover, and what it then does about `-R` is not worth
betting a second primary on. So `primary_conninfo` is rewritten every time.
So is `primary_slot_name` — *deleted*, and for a sharper reason: slots are
not replicated, so a member that comes back naming the slot it used to stream
with names it on a machine that has never heard of it, and a standby whose
slot is missing does not fall back to streaming without one. It retries
forever (`replication slot "..." does not exist`) while looking, to every
other query, like a healthy standby: `pg_is_in_recovery()` is true, the
timeline is right, only `pg_stat_wal_receiver` is empty. Streaming with no
slot costs WAL retention, which is what phase 6's slot budget is for.
Streaming with a slot that is not there costs everything.

**A crashed target is recovered before the rewind, by us.** `pg_rewind`
refuses a target that was not shut down cleanly, and since 13 it fixes that
itself by running `postgres --single -D <datadir>` — which takes the
configuration to be *in* the data directory. On Debian it is in
`/etc/postgresql`, so that step fails, and it fails in exactly the case a
failover is about: the machine that died. The rejoin therefore reads
`pg_controldata`'s cluster state and, for anything but a clean stop, runs the
single-user recovery itself with `-c config_file=`. Single-user, never a
start: a server that listens is a second primary, and this one still believes
it is the primary.

**Neither that recovery nor the stop before it may throw away what the
rewind is for.** A clean shutdown ends in a checkpoint, and a checkpoint
recycles the WAL before it — which is the WAL `pg_rewind` then reads, walking
back to the last checkpoint the two machines share. So both `StopMember` and
the recovery pin `wal_keep_size` to what `pg_wal` already holds, and the
rejoin takes the pin off again once it has been used. Without it the rewind
fails with `could not open file .../pg_wal/...` immediately after the step
that was supposed to enable it. In an ordinary switchover nothing older than
the shutdown checkpoint is ever wanted, so this never shows; after a split
brain the histories parted long before, and stopping the loser is precisely
what destroys the record of how.

**And the slot it will stream with is made here too.** `-R` does not create
one, and slots are not replicated, so a rejoining member creates its own on
the machine it is about to stream from — over the replication connection,
which is the one path the pair already requires — and then *checks that it is
there*, because a standby naming a slot the primary does not have retries
forever while looking healthy to every query but `pg_stat_wal_receiver`. The
name is derived from the pair and the side (`salmon_pair_<name>_<side>`,
lower-cased, anything else an underscore, 63 characters) rather than declared,
so that a member computes the same name the member it rejoins would and a slot
nobody can name is not a slot nobody can drop. The same step drops the slot
this member held for its *peer* back when it was the primary: nothing consumes
it here, and a slot nobody consumes goes on pinning every segment behind it.

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

### What a stopped peer proves

A peer that is not running is read off `pg_controldata`, and what that file
is worth depends on how it stopped:

- **Shut down cleanly.** The last record written is a shutdown checkpoint, so
  the checkpoint location *is* the end of its WAL: a standby that has reached
  it has everything. This is the ordinary switchover, and it needs no
  declaration from anybody.
- **Crashed.** The checkpoint location is wherever the last periodic
  checkpoint happened to land, and any amount of acknowledged WAL may follow
  it, with nothing on disk to say how much. Comparing the standby against
  that number would read as "the standby has everything" precisely when it is
  least likely to be true.

So a crashed peer is a refusal until `pair_may_discard` names the side. It is
the same rule as the unreachable peer's, arrived at by a different road: in
both, promoting means losing writes nobody can count. `Database cluster
state` is the field that tells them apart, and anything other than
`shut down` / `shut down in recovery` — including a value this parser does
not recognise — counts as a crash, which is the direction that refuses.

Salmon cannot fence a machine it cannot reach. What makes promoting B safe
enough for a low SLA is **routing**, not fencing: if `pg_hba` only admits
application traffic from the bouncers' hosts, then repointing the bouncers is
the fence. A reachable A that comes back as a primary is stopped and rewound
by the next pass. Its writes in the gap are the loss the flag already
declared. The recipe should say this plainly in its haddock and in the check's
`Failure` text, instead of implying a fence it does not have.

## Routing

Implemented; what follows is what was built and why.

The bouncers' upstream is always the *declared* primary. Admin nodes that
must run on the primary (`database`, `user`, templates, clones, migrations)
also target the declared primary, so the `Track' Postgres.Server` they take
is simply B's. This is where the design is simpler than Patroni's, where the
primary has to be discovered at run time.

A switchover is `PAUSE` on every bouncer, the Postgres steps, a rewritten
routing file, `RELOAD`, then `RESUME`. With `pool_mode = transaction`,
`PAUSE` waits for transactions in flight, so clients see latency rather than
errors. That makes scenario S1's "no client errors" a real assertion.

**The routing lives in its own file, not in the ini**, pulled in with
`%include`. That is the seam between two nodes that would otherwise fight
over one file. `PgBouncer.setup` owns the ini and *watches* it, so a change
there is noticed and applied — by a restart, which drops every client the
bouncer exists to hold. The role node owns the routing file, which is not
watched, and applies a change the gentle way. Each file has one writer, and
the one that moves traffic never restarts anything.

`bouncerSetup` writes the routing file only when it is **missing**, for the
same reason: after that it is the role node's, and a pass that rewrote it
would move clients without pausing them first.

Reading a bouncer is `SHOW DATABASES` on the admin console, by column
*name* — the columns have changed between pgbouncer versions, and counting
them is a way to read the wrong one. A bouncer that cannot be reached reads
as sending clients nowhere, which is not `Done`: a pair whose clients are
going somewhere unknown has not arrived.

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

**Ways to cause trouble**, all things the tree already has. Two of the three
are written, in `Test.PostgresVms` and `SreBox.PostgresPair`:
- **a crash:** `crashCluster` — `systemctl kill -s KILL postgresql@...` for the
  unit's whole cgroup, then a wait for the cluster to actually be gone;
- **a controller killed part-way:** `convergeUpTo`, a step budget, which from
  the machines' side is indistinguishable from the controller dying after
  step *k*;
- **a partition:** `partitionFrom` / `partitionFromEverythingFor`, `nft`
  rules built out of `Netfilter`'s own `Table`/`Chain`/`Rule` vocabulary and
  rendered by its own command, so the rule text is the tree's; only the
  *running* of it differs, since the guests have no salmon on them and the
  argv goes over ssh. A partition that hides a machine from the controller
  cannot be lifted by the controller — the command would have to travel the
  path it cut — so that one is handed to the machine as cut, wait, heal, and
  left to run detached.

Alongside them, what a scenario asserts *with*: `dataDirectoryIdentity` (the
inodes a rewind keeps and a re-clone cannot), `assertPrimaryIs` /
`assertInRecovery`, and `waitFor`, since every one of these waits is on a
machine doing something in its own time.

| # | Scenario | Assert |
|---|---|---|
| S1 | Switch A→B, then B→A, with a client inserting through the bouncer the whole time | every acknowledged insert is present; the client saw zero errors; a rerun of `run up` changes nothing |
| S2 | S1, with the controller killed after each step *k* in turn | a plain rerun finishes; no bouncer is left paused; the result is the same as S1 |
| S3 | Crash A while it holds writes the standby never got; declare B, first without `may_discard` and then with it | the first pass refuses, and promotes nothing; the second promotes B, which serves writes; A rejoins as B's standby through `pg_rewind`, not a re-clone (its data directory is never unlinked); the replicated rows survive and the un-replicated ones are gone, which is what the flag said |
| S4 | Partition A from B, with no change to the declaration | the check is `Unknown`; nothing is promoted, and in particular the standby is not stopped or rewound; after the partition heals, B catches up on its own and the pair is `Done` |
| S5 | Partition A from B *and from the controller*, fail over to B with `may_discard = A` while A still holds writes B never got, then let it heal | without the flag the pass refuses, twice: once while A cannot be reached, and again once both machines call themselves primaries; with it, B is promoted, then A is stopped and rewound onto B's history; A's writes behind the partition are gone, as declared |
| S6 | Stop the standby; write past `max_slot_wal_keep_size` on the primary | the primary's `pg_wal` is bounded rather than following the standby down; the slot reports `wal_status = 'lost'`; the check is `Unknown` with the slot named in it; a pass does nothing at all, and in particular the standby's data directory is not unlinked — re-seeding is an operator's decision about throwing data away, not a step |
| S7 | Stop both, in either order, and declare the side that stopped *first* | the pair converges on the declaration without losing what the other machine wrote after it: B starts, and if it is behind, A is started again so B can catch up from it before the ordinary switchover runs |
| S8 | A stranger's cluster (fresh `initdb`) where a member should be: same address, same cluster name, same port | `Refuse`, in both directions and with `may_discard` naming either side — the flag says whose *writes* may go, which presumes one cluster, and is not a licence to wipe a machine that was never in the pair. Neither data directory is touched, and the stranger's own databases are still there |

S2 and S8 matter most. S2 proves resumability, which is the design's main
claim. S8 proves the refusal that P1 is about, one level up: P1 guards the
clone against cloning over a stranger, and S8 guards every *step* against
being applied to one.

All eight are written, in `Test.PostgresSwitchoverSpec`, and between them
they cost the recipe twelve defects. Where those came from is the argument
for writing a catalogue of *causes* rather than of features: not one of the
twelve is on the happy path. S1, the scenario in which nothing goes wrong,
found nothing, and so did S8, whose guard was written first and stayed right.
Every other defect needed a machine that stopped, or was cut off, without
being asked to.

Writing S3 alone
turned up six defects: the two positions that were not what their names said
(see "Observation, then a pure verdict"), the crashed peer treated as a clean
one, and the three things the rejoin now does around `pg_rewind`. None of them
is reachable from the happy path, because every one needs a machine that
stopped without being asked to -- which is the argument for the rest of this
catalogue, and the reason the scenarios are written as a list of *causes*
rather than of features.

S4 and S5 turned up three more, in the same spirit. A partitioned standby
read as somebody else's standby and would have been stopped and rewound, so a
broken link between the two machines would have taken the standby down with
it. Deciding that a member is unreachable took minutes, because nothing
bounded ssh's own retrying — on the one failure this recipe exists for. And
the clean stop of a divergent primary recycled the WAL the rewind of it then
needed.

S6 turned up the state the pair had no answer for at all: once slots are in
use, a standby that falls off the budget can never catch up, and every
earlier version of the table would have gone on rewinding at it. S7 turned up
the two worst, both of them one declaration away -- stopping a primary while
the standby was not connected to receive its tail, and then waiting out a
budget for records a stopped machine was never going to send. Between them
they could take a healthy pair to one machine stopped and the other
unpromotable.

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
