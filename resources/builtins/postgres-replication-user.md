# Postgres replication role

Creates a role with `REPLICATION LOGIN` and sets its password.

```haskell
Postgres.replicationUser reporter server psql port (User role) password
```

Defined in [`Salmon.Builtin.Nodes.Postgres`](../../salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs).

## Effect site

The node's `ref` is `pg-replication-user`, keyed by the role. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

No check: `run up` applies it every time (it is idempotent). Under `serve` it parks and nothing watches it.

## up

Creates the role with `REPLICATION LOGIN` if missing, then sets its password (either way).

## down

Nothing: this node never drops a role.

## What it never shows

The password, and any digest of it. The card takes only the role and the port.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (PgCards.replicationUser port role) (Postgres.replicationUser reporter server psql port role password)
```

The card is in [`Salmon.Builtin.Cards.Postgres`](../../salmon-ops/src/Salmon/Builtin/Cards/Postgres.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
