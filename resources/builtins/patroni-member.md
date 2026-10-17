# Patroni member

One Patroni cluster member: `patroni.yml`, the credentials file's ownership, Debian's cluster unit masked, the unit, and a node that waits for the member to be healthy.

```haskell
Patroni.patroniMember reporter systemctl patroni cfg
```

Defined in [`Salmon.Builtin.Nodes.Patroni`](../../salmon-ops/src/Salmon/Builtin/Nodes/Patroni.hs).

## Effect site

The node's `ref` is `patroni-member`, keyed by the scope and the member's name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Asks the member's REST API (`/health`, `/patroni`). Running and healthy in the declared scope: in place. Not answering, unhealthy or in another scope: `Failure`. Starting, or a transitional state: `Unknown`, and nothing restarts on that. The member's role is reported, never judged.

## up

Waits, up to the ready timeout, for the check to pass. The configuration and the unit are the nodes under it.

## down

Nothing of its own: the unit under it is stopped, and the data directory is left alone.

## What it never shows

The credentials file's contents, and which member leads. The card shows the scope, the member, the REST and Postgres addresses, the Postgres version, the etcd hosts, the configuration directory and the credentials file's path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (HaCards.patroniMember cfg) (Patroni.patroniMember reporter systemctl patroni cfg)
```

The card is in [`Salmon.Builtin.Cards.Patroni`](../../salmon-ops/src/Salmon/Builtin/Cards/Patroni.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
