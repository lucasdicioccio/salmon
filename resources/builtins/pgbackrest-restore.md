# pgBackRest restore

A data directory restored from the archive, to a target. For a cluster salmon runs itself, not a Patroni member.

```haskell
PgBackRest.restoredCluster reporter pgbackrest cfg target
```

Defined in [`Salmon.Builtin.Nodes.PgBackRest`](../../salmon-ops/src/Salmon/Builtin/Nodes/PgBackRest.hs).

## Effect site

The node's `ref` is `pgbackrest-restore`, keyed by the data directory. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the data directory holds a cluster (`PG_VERSION`). One that does is left alone, whatever it is.

## up

`pgbackrest restore` without `--delta` or `--force`, so a directory holding anything is refused. Starting the cluster is the next node's.

## down

Nothing: a restored cluster is data.

## What it never shows

The secrets fragment's contents, and an S3 archive's location. The card shows the stanza, the data directory, the repository's kind, the configuration file and the target.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (HaCards.pgBackRestRestore cfg target) (PgBackRest.restoredCluster reporter pgbackrest cfg target)
```

The card is in [`Salmon.Builtin.Cards.Patroni`](../../salmon-ops/src/Salmon/Builtin/Cards/Patroni.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
