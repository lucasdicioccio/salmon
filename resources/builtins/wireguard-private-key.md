# WireGuard private key

Generates a WireGuard private key into a file, once.

```haskell
WireGuard.privateKey wg path
```

Defined in [`Salmon.Builtin.Nodes.WireGuard`](../../salmon-ops/src/Salmon/Builtin/Nodes/WireGuard.hs).

## Effect site

The node's `ref` is `wg-write-pk`, keyed by the path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the file is there. Missing: `up` generates the key.

## up

`wg genkey` into the file. An existing key is never regenerated. Its directory is a node of its own.

## down

Nothing: the key stays.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows the path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.wgPrivateKey path) (WireGuard.privateKey wg path)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
