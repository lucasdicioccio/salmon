# A Postgres pair whose primary is declared, not discovered

Two machines, one Postgres cluster, and a sentence that says which machine is
the primary today. Changing that sentence moves the primary; nothing else
does. This is `SreBox.PostgresPair`, the `salmon-pgpair` binary, and
`salmon-toy-qemu-pg-ha`, a demo that builds its own machines to show it.

If you want the design argument rather than the instructions, read
[`specs/pg-switchover.md`](../specs/pg-switchover.md); this file is about
using what it describes.

## What it is for, and what it is not

**It is for** a planned switchover (maintenance, a machine you want to drain)
and an *operator-decided* failover, on two machines, where a few minutes of
somebody's attention is an acceptable price for the machine you did not buy.
It is also, deliberately, a thing to break on purpose: every failure mode
below is something the test suite causes and then asserts about.

**It is not** automatic failover. Deciding that a machine is dead needs
consensus, consensus needs three voters, and salmon has neither — so this
recipe never promotes on a hunch. When it cannot prove a promotion is safe it
*refuses*, and the operator's answer to a refusal is the one field that
accepts a loss by name:

```
--may-discard A     "I accept losing the writes on A that B does not have"
```

For the unattended-at-3am version, [`specs/pg-patroni.md`](../specs/pg-patroni.md)
sketches the Patroni-backed shape, where salmon provisions and supervises and
Patroni decides.

## The shape

Three kinds of node, and only one of them mentions a role:

| Node | Says |
|---|---|
| `member` (one per machine) | how to be *either* half of the pair: replication settings, `pg_hba` lines for the peer, the replication and rewind roles. Nothing about which half it is. |
| `bouncerSetup` (one per bouncer) | pgbouncer's own configuration, and a routing file it includes but does not own |
| `pairRole` | *"this pair's primary is on B"* — the only declaration an operator edits |

That split is the whole ergonomic claim: a switchover changes one node's
declaration, so under `run serve` the machines underneath are not marked
stale by it, and an operator reading a diff sees one word.

`pairRole`'s `check` asks both machines and every bouncer what they are; its
`up` takes one step at a time until the declaration holds. **The state is
re-derived every pass** — observe, decide, act, observe again — so an `up`
killed half-way through a switchover is finished by the next one. There is no
progress file that could disagree with the machines.

## Using it

```
salmon-pgpair config --primary A --a 10.0.0.2 --b 10.0.0.3 --bouncer 10.0.0.4 --seed B \
  | salmon-pgpair run up

salmon-pgpair config --primary B --a 10.0.0.2 --b 10.0.0.3 --bouncer 10.0.0.4 \
  | salmon-pgpair run up          # the switchover
```

`run tree` prints what the first of those declares before anything runs:

```
pg-pair-role (n2504805732311083189) primary of app is on A
  <- pg-pair-bouncer
  <- pg-pair-seed
  <- pg-pair-member
  <- pg-pair-member
pg-pair-bouncer (n671955809932212377) pgbouncer 10.0.0.4 in front of app
pg-pair-seed (7089073737010868211) seeds 10.0.0.3 from 10.0.0.2
  <- pg-pair-member
  <- pg-pair-member
pg-pair-member (n6118828710077022853) member of app on 10.0.0.3
pg-pair-member (2659614637461449593) member of app on 10.0.0.2
```

What it assumes was done before it ever ran, because a recipe that ships
secrets has chosen a transport for everyone who uses it: both machines have a
Postgres cluster and the two `.pgpass` files the pair names, and the bouncer
has pgbouncer, a `userlist.txt` and the `.pgpass` for its admin console. The recipe is given paths.

`--seed B` is the first clone of a pair's life. It is safe to leave declared —
the clone does nothing once the two sides share a system identifier, and
refuses a machine holding a cluster it does not recognise. It is *not* the way
back from a standby that has fallen too far behind (see the slot budget
below): that is `--reseed`.

## What a switchover actually does

```
PauseBouncers -> StopMember A -> Promote B -> RepointBouncers B -> Rejoin A -> Done
```

- **PauseBouncers** holds the clients rather than dropping them. With
  `pool_mode = transaction`, `PAUSE` waits for transactions in flight and
  queues what comes after, so a client sees latency where it would otherwise
  see an error.
- **StopMember** is a *clean* stop, which hands the standby everything it has
  not got, including the shutdown checkpoint the promotion waits for. It also
  pins `wal_keep_size` first, because a clean shutdown ends in a checkpoint
  and a checkpoint recycles the WAL a later rewind reads.
- **Promote** waits for the server to say it promoted.
- **RepointBouncers** rewrites the routing file, `RELOAD`s, and `RESUME`s.
  Never a restart: a restart drops every client the bouncer is there to hold.
- **Rejoin** brings the old primary back as a standby through `pg_rewind`,
  onto the new primary's history — same machine, same data, only the records
  that diverged replaced. Not a re-clone.

Traffic moves through pgbouncer's admin console, and the routing lives in its
own file pulled in with `%include`. That is a seam between two writers:
`bouncerSetup` owns the ini, so a change there is applied by a restart; the
role node owns the routing file (`bouncerSetup` writes it only when it is
missing), and applies a change gently. One file with two writers is how a switchover becomes
an outage.

## When it refuses, and why

A refusal is the recipe saying it cannot prove the next step is safe:

| It says | Because |
|---|---|
| the two machines hold different clusters | their system identifiers differ, so every step below would be applied to somebody else's data. Checked before anything else, and `--may-discard` does not override it |
| both machines are primaries | resolving a split brain means rewinding one onto the other, which is a loss. Name the side |
| the peer did not stop cleanly | a crashed cluster's `pg_controldata` records its last *checkpoint*, not the end of its WAL: the standby may be missing anything written after it |
| cannot confirm the peer has stopped | it is unreachable, and promoting without fencing is how two primaries happen |
| the peer standby is ahead | promoting the declared side would lose the difference |

And two verdicts that are *not* failures, and act on nothing:

- **`AwaitStreaming`** — the peer is pointed here and not connected: a
  partition, or a standby that came back a second ago. Waiting is right;
  rewinding at it would stop it and then fail, because whatever keeps it from
  streaming keeps `pg_rewind` from reading too.
- **`Degraded`** — the declaration holds but the pair is one machine short.
  Reported as `Unknown`, the one verdict `run serve` acts on by continuing to
  look.

## The slot budget

Each member streams with a replication slot the pair names, created by the
member that rejoins. A slot is a promise to keep WAL until the standby has it,
and an unbounded promise is how a machine that is merely *down* takes the
machine that is *up* with it — so `max_slot_wal_keep_size` caps it. Past the
cap the slot is invalidated, the WAL is recycled, and the standby can never
catch up.

That is a good trade and a terrible surprise, so the pair says it out loud:
the check reports the lost slot by name and **does nothing**. `pg_rewind`
would succeed and change nothing; the only way back is a re-seed, and wiping a
machine is an operator's decision, declared with `--reseed B` (the side that is
not `--primary`). The declaration acts only on that one diagnosis — the
primary reporting that side's slot lost — so it is safe to leave in place: a
healthy or merely lagging standby is never wiped. The pass stops the machine,
removes its data directory (only if it is this pair's own cluster or empty;
a stranger's is refused by name), clones it again from the primary, drops the
lost slot and lets the machine make its own. A pass killed half-way is
finished by the next one.

## Watching it happen

`salmon-toy-qemu-pg-ha` builds three qemu guests, puts the pair on two of them
and pgbouncer on the third, and writes through the bouncer while the primary
moves:

```
t=$(cabal list-bin salmon-toy-qemu-pg-ha)

sudo $t config prereqs             | sudo $t run up   # once: three rootfses
$t config up --primary A --seed B  | $t run up        # once: the pair
$t config client --seconds 120     | $t run up &      # a client, writing
$t config up --primary B           | $t run up        # the demo
```

The client prints three numbers when it stops. On the machine this was written
on:

```
  inserts acknowledged through the bouncer: 460
  inserts that came back an error:          0
  acknowledged rows missing afterwards:     0
```

`prereqs` is the only part that needs root — debootstrapping a root
filesystem, regenerating an initrd that can mount a 9p root, and handing
`/etc/ssh` to whoever runs the rest. Everything after it is an unprivileged
user with two capabilities granted once (`capsh` needs `cap_net_admin`, qemu
needs `cap_dac_override,cap_chown,cap_fowner`; see
[`specs/qemu-test-vms-progress.md`](../specs/qemu-test-vms-progress.md)).

After the demo B is the primary: pause machine B's guest, declare
`up --primary A`, and it refuses; add `--may-discard B` and the refusal turns
into a failover.

## What is tested, and what that is worth

`Test.PostgresPairSpec` covers the decision table at Layer 0 — every state two
machines can be found in, including every refusal — without a database.

`Test.PostgresSwitchoverSpec` and `Test.PgPairDemoSpec` cause the failures on
real machines:

| | |
|---|---|
| S1 | switch A→B→A with a client attached: every acknowledged insert present, zero client errors |
| S2 | the controller killed after each step in turn: a plain rerun finishes it |
| S3 | the primary crashed with un-replicated writes: refused without the flag, rewound with it |
| S4 | the two machines partitioned: the check is `Unknown`, and the standby is not touched |
| S5 | a failover across a partition that hides the primary from the controller too: two primaries, then the loser rewound |
| S6 | a standby that falls off the slot budget: said, not silently re-seeded |
| S7 | both machines stopped, in either order, and the stale one declared primary |
| S8 | a stranger's cluster where a member should be: refused, nothing deleted |

Between them these cost the recipe twelve defects, and not one was on the
happy path: S1 and S8 found nothing, and every other defect needed a machine
that stopped, or was cut off, without being asked to. That is the argument for
writing the list as *causes* rather than as features.
