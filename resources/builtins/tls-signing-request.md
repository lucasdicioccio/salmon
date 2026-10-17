# Certificate signing request

Writes a certificate signing request for a CN, with its key, as PEM and as DER.

```haskell
Certs.signingRequest reporter openssl req
```

Defined in [`Salmon.Builtin.Nodes.Certificates`](../../salmon-ops/src/Salmon/Builtin/Nodes/Certificates.hs).

## Effect site

The node's `ref` is `openssl-csr`, keyed by the request's path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

None: `run up` rewrites the request every time. Under `serve` it parks and nothing watches it.

## up

`openssl req -new` for the CN with the key (a node under it), then the same request as DER.

## down

Nothing: the request stays.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows the CN, the request's path and the key's path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.signingRequest req) (Certs.signingRequest reporter openssl req)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
