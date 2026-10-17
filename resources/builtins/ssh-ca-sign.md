# SSH certificate from a CA

Signs an SSH public key with an SSH certificate authority's key, for the principals given.

```haskell
Keys.signKey reporter sshKeygen ca identifier principals key
```

Defined in [`Salmon.Builtin.Nodes.Keys`](../../salmon-ops/src/Salmon/Builtin/Nodes/Keys.hs).

## Effect site

The node's `ref` is `ssh-ca-sign`, keyed by the CA and the key identifier. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the certificate (`<key>-cert.pub`) is there. Missing: `up` signs the key.

## up

`ssh-keygen -s` with the CA's private key, the identifier and the principals (which must not be empty). Both key pairs are nodes under it. An existing certificate is not renewed.

## down

Nothing: the certificate is kept.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows the identifier, the principals, the certificate's path, the signed key's path and the CA key's path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.sshCaSign ca identifier principals key) (Keys.signKey reporter sshKeygen ca identifier principals key)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
