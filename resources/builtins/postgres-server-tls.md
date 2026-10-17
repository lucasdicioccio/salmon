# Postgres server TLS

Turns TLS on for a local server, from three pre-provisioned files.

```haskell
Postgres.serverTls reporter psql port tls  -- or serverTlsAfter
```

Defined in [`Salmon.Builtin.Nodes.Postgres`](../../salmon-ops/src/Salmon/Builtin/Nodes/Postgres.hs).

## Effect site

The node's `ref` is `pg-server-tls`, keyed by the port and the certificate path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

No check of its own: the settings and the reload under it run on each pass. A broken certificate is not noticed until a client connects.

## up

Nothing of its own: under it, `ssl` and the three file settings are set, then the configuration is reloaded.

## down

Nothing: TLS stays configured.

## What it never shows

What is in the three files, the key above all. The card shows the port and the three paths.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (PgCards.serverTls port tls) (Postgres.serverTls reporter psql port tls)
```

The card is in [`Salmon.Builtin.Cards.Postgres`](../../salmon-ops/src/Salmon/Builtin/Cards/Postgres.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
