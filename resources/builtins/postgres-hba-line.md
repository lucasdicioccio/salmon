# Postgres pg_hba.conf line

Appends one line to a cluster's `pg_hba.conf` and reloads.

```haskell
Postgres.hbaLine reporter pgCtlCluster cluster line
```

Defined in [`Salmon.Builtin.Nodes.Postgres`](../../salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs).

## Effect site

The node's `ref` is `pg-hba-line`, keyed by the cluster and the line. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

No check: `run up` applies it every time (it is idempotent). Under `serve` it parks and nothing watches it.

## up

Appends the line to `pg_hba.conf` unless it is there verbatim, then reloads the cluster.

## down

Nothing: the line stays. A changed declaration leaves the old line in place.

## What it never shows

Nothing secret is involved. The card shows the cluster and the line.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (PgCards.hbaLine cluster line) (Postgres.hbaLine reporter pgCtlCluster cluster line)
```

The card is in [`Salmon.Builtin.Cards.Postgres`](../../salmon-ops/src/Salmon/Builtin/Cards/Postgres.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
