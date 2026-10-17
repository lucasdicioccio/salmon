# HAProxy router

HAProxy in front of a Patroni cluster, as a systemd service: one port for the leader, one over the replicas.

```haskell
Haproxy.setup reporter systemctl haproxy cfg  -- e.g. cfg = Haproxy.patroniRouter members
```

Defined in [`Salmon.Builtin.Nodes.Haproxy`](../../salmon-ops/src/Salmon/Builtin/Nodes/Haproxy.hs).

## Effect site

The node's `ref` is `systemd-unit`, keyed by `haproxy.service`. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Asks systemd whether the unit is active, enabled and loaded as written. In transition, or no answer: `Unknown`, and nothing restarts on that.

## up

The unit, restarted when `haproxy.cfg` changes. A changed member list drops the connections in flight; a failover changes nothing here, since HAProxy's own health checks against each member's REST API move the traffic.

## down

`systemctl stop`; the configuration file stays.

## What it never shows

Which member leads: it is not in the configuration either. The card shows the configuration file, each listener (address, port and the REST path it asks), the members and the stats address.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (HaCards.haproxy cfg) (Haproxy.setup reporter systemctl haproxy cfg)
```

The card is in [`Salmon.Builtin.Cards.Patroni`](../../salmon-ops/src/Salmon/Builtin/Cards/Patroni.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
