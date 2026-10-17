# pgBackRest first backup

The archive holds at least one backup.

```haskell
PgBackRest.firstBackup reporter pgbackrest cfg
```

Defined in [`Salmon.Builtin.Nodes.PgBackRest`](../../salmon-ops/src/Salmon/Builtin/Nodes/PgBackRest.hs).

## Effect site

The node's `ref` is `pgbackrest-first-backup`, keyed by the stanza. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Reads the repository. No backup: `up` takes a full one.

## up

`pgbackrest backup --type=full`, from the primary only. Later backups are a schedule's (`scheduledBackup`), not a state.

## down

Nothing: backups stay.

## What it never shows

The secrets fragment's contents, and an S3 archive's location. The card shows the stanza, the data directory, the repository's kind, the configuration file and how many full backups are kept.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (HaCards.pgBackRestFirstBackup cfg) (PgBackRest.firstBackup reporter pgbackrest cfg)
```

The card is in [`Salmon.Builtin.Cards.Patroni`](../../salmon-ops/src/Salmon/Builtin/Cards/Patroni.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
