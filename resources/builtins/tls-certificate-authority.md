# Certificate authority

A certificate authority salmon owns: its key and its self-signed CA certificate.

```haskell
Certs.certificateAuthority reporter openssl ca
```

Defined in [`Salmon.Builtin.Nodes.Certificates`](../../salmon-ops/src/Salmon/Builtin/Nodes/Certificates.hs).

## Effect site

The node's `ref` is `openssl-ca`, keyed by the CA certificate's path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the CA certificate is there and valid for more than a day. Missing, expired or expiring: `up` self-signs a new one with the same key.

## up

`openssl req -x509` with the CA key (a node under it), which marks the certificate as a CA, for the declared number of days.

## down

Nothing: the CA stays. Losing its key means nothing can ever be issued to the fleet that trusts it.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows the CN, the certificate's path, the key's path and the validity.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.certificateAuthority ca) (Certs.certificateAuthority reporter openssl ca)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
