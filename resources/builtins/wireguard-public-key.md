# WireGuard public key

Derives a WireGuard public key from a private key file, once.

```haskell
WireGuard.publicKey wg mkPrivate privatePath publicPath
```

Defined in [`Salmon.Builtin.Nodes.WireGuard`](../../salmon-ops/src/Salmon/Builtin/Nodes/WireGuard.hs).

## Effect site

The node's `ref` is `wg-write-public-pk`, keyed by the public key's path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the public key file is there. Missing: `up` derives it.

## up

`wg pubkey` from the private key. A later change of the private key is not followed.

## down

Nothing: the key stays.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows both paths.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.wgPublicKey privatePath publicPath) (WireGuard.publicKey wg mkPrivate privatePath publicPath)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
