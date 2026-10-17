# Postgres login role, password from a file

Keeps a login role whose password is read from a pre-provisioned file.

```haskell
Postgres.userPassFile reporter server psql port passfile (User role)
```

Defined in [`Salmon.Builtin.Nodes.Postgres`](../../salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs).

## Effect site

The node's `ref` is `pg-user`, keyed by the role (the same effect site as the plain login role). Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when the role exists, may log in, and its password is the file's. Missing, not allowed to log in, or holding another password: `up` sets it. `Unknown` when the cluster cannot be asked or the file cannot be read.

## up

Creates the login role if missing, then sets its password from the file, read at that moment.

## down

Nothing: this node never drops a role.

## What it never shows

The file's contents and any digest of them. The card shows the role, the port and the file's path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (PgCards.userPassFile port path role) (Postgres.userPassFile reporter server psql port passfile role)
```

The card is in [`Salmon.Builtin.Cards.Postgres`](../../salmon-ops/src/Salmon/Builtin/Cards/Postgres.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

The recipe chooses how the file gets there; this node only reads it at `up` and `check` time.
