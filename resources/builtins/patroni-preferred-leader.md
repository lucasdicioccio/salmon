# Patroni preferred leader

Opt-in: moves a Patroni cluster's leader back to a preferred member after a failover.

```haskell
PreferredLeader.preferredLeader cfg
```

Defined in [`Salmon.Builtin.Nodes.Patroni.PreferredLeader`](../../salmon-ops/src/Salmon/Builtin/Nodes/Patroni/PreferredLeader.hs).

## Effect site

The node's `ref` is `patroni-preferred-leader`, keyed by the scope alone, so two declarations preferring different members collide. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Asks the REST API. The preferred member leads: in place. Another leads and the preferred one is eligible: `Failure`. No leader, or the preferred member not eligible (with `synchronous_mode`, only a sync standby is): `Unknown`.

## up

`POST /switchover` to the preferred member, then waits up to the settle time for it to lead.

## down

Nothing: the leader stays where it is.

## What it never shows

The credentials file's contents. The card shows the scope, the preferred member, its REST address, the credentials file's path and the settle time. The REST API is plain HTTP, so it is not linked.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (HaCards.preferredLeader cfg) (PreferredLeader.preferredLeader cfg)
```

The card is in [`Salmon.Builtin.Cards.Patroni`](../../salmon-ops/src/Salmon/Builtin/Cards/Patroni.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
