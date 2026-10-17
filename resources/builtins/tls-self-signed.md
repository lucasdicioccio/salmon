# Self-signed certificate

Signs a certificate signing request with its own key.

```haskell
Certs.selfSign reporter openssl selfSigned
```

Defined in [`Salmon.Builtin.Nodes.Certificates`](../../salmon-ops/src/Salmon/Builtin/Nodes/Certificates.hs).

## Effect site

The node's `ref` is `openssl-selfsign`, keyed by the certificate's path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the certificate is there and valid for more than a day (`openssl x509 -checkend 86400`). Missing, expired or expiring: `up` signs again.

## up

`openssl x509 -req`, signed with the request's own key. The request is a node under it.

## down

Nothing: the certificate stays.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows the certificate's path, the CN, the request's path and the key's path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.selfSigned s) (Certs.selfSign reporter openssl s)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
