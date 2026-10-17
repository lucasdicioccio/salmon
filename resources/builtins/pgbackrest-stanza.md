# pgBackRest stanza

The archive holds the stanza.

```haskell
PgBackRest.stanza reporter pgbackrest cfg
```

Defined in [`Salmon.Builtin.Nodes.PgBackRest`](../../salmon-ops/src/Salmon/Builtin/Nodes/PgBackRest.hs).

## Effect site

The node's `ref` is `pgbackrest-stanza`, keyed by the stanza. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Reads the repository (`pgbackrest info`), from any member. No stanza: `up` creates it.

## up

`pgbackrest stanza-create`, idempotent. It needs the cluster running and this member to be the primary; elsewhere it fails until the leader has done it.

## down

Nothing: the stanza and its archive stay.

## What it never shows

The secrets fragment's contents, and an S3 archive's location. The card shows the stanza, the data directory, the repository's kind and the configuration file.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (HaCards.pgBackRestStanza cfg) (PgBackRest.stanza reporter pgbackrest cfg)
```

The card is in [`Salmon.Builtin.Cards.Patroni`](../../salmon-ops/src/Salmon/Builtin/Cards/Patroni.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
