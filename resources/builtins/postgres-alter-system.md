# Postgres ALTER SYSTEM setting

Writes one server setting with `ALTER SYSTEM SET`.

```haskell
Postgres.alterSystemSet reporter psql port param value
```

Defined in [`Salmon.Builtin.Nodes.Postgres`](../../salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs).

## Effect site

The node's `ref` is `pg-alter-system`, keyed by the port and the setting name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

No check: `run up` applies it every time (it is idempotent). Under `serve` it parks and nothing watches it.

## up

`ALTER SYSTEM SET`, written to `postgresql.auto.conf`. It takes effect on a reload or, for some settings, a restart.

## down

Nothing: the setting stays.

## What it never shows

The setting and its value are public: do not use this node for a value that is a secret.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (PgCards.alterSystem port param value) (Postgres.alterSystemSet reporter psql port param value)
```

The card is in [`Salmon.Builtin.Cards.Postgres`](../../salmon-ops/src/Salmon/Builtin/Cards/Postgres.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
