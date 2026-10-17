# Postgres database

Creates a database on a local Postgres server.

```haskell
Postgres.database reporter server psql port (Database name)
```

Defined in [`Salmon.Builtin.Nodes.Postgres`](../../salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs).

## Effect site

The node's `ref` is `pg-db`, keyed by the port and the database name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

No check: `run up` applies it every time (a no-op when the database exists). Under `serve` it parks and nothing watches it.

## up

`CREATE DATABASE` as the postgres user, on the local server at this port, unless `pg_database` already has it.

## down

Nothing: this node never drops a database.

## What it never shows

Nothing secret is involved. The card shows the database and the port.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (Cards.pgDatabase port db) (Postgres.database reporter server psql port db)
```

The card is in [`Salmon.Builtin.Cards`](../../salmon-ops/src/Salmon/Builtin/Cards.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
