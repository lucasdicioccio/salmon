# pgBackRest configuration

The pgBackRest binary, `pgbackrest.conf`, the log directory, a posix repository's directory and the secrets fragment's ownership.

```haskell
PgBackRest.configuration pgbackrest cfg
```

Defined in [`Salmon.Builtin.Nodes.PgBackRest`](../../salmon-ops/src/Salmon/Builtin/Nodes/PgBackRest.hs).

## Effect site

The node's `ref` is `pgbackrest-config`, keyed by the configuration file's path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

None of its own: the nodes under it are checked.

## up

Nothing of its own. A missing secrets fragment fails: writing it is not this node's job.

## down

Nothing of its own.

## What it never shows

The secrets fragment's contents, and an S3 archive's bucket, endpoint or path (the card says `S3`). The card shows the stanza, the data directory, the repository's kind (and a posix repository's directory), the configuration file and the fragment's path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (HaCards.pgBackRestConfiguration cfg) (PgBackRest.configuration pgbackrest cfg)
```

The card is in [`Salmon.Builtin.Cards.Patroni`](../../salmon-ops/src/Salmon/Builtin/Cards/Patroni.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
