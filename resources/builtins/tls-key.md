# TLS private key

Generates an RSA private key, once.

```haskell
Certs.tlsKey reporter openssl (Certs.Key Certs.RSA2048 dir name)
```

Defined in [`Salmon.Builtin.Nodes.Certificates`](../../salmon-ops/src/Salmon/Builtin/Nodes/Certificates.hs).

## Effect site

The node's `ref` is `openssl`, keyed by the key's path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the key file is there. Missing: `up` generates it.

## up

`openssl genrsa`. An existing key is never regenerated.

## down

Nothing: the key stays. Its directory is a retained one: on teardown it is renamed with a timestamp, not deleted.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows the key type and its path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.tlsKey key) (Certs.tlsKey reporter openssl key)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
