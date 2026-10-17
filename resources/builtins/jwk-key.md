# JWK key

Generates a JSON Web Key, once.

```haskell
Keys.jwkKey (Keys.JWKKeyPair Keys.ED25519 dir name)
```

Defined in [`Salmon.Builtin.Nodes.Keys`](../../salmon-ops/src/Salmon/Builtin/Nodes/Keys.hs).

## Effect site

The node's `ref` is `jwk`, keyed by the key's directory and name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Whether the key file is there. Missing: `up` generates it.

## up

Generates the key in-process and writes it as JSON. An existing key is never regenerated.

## down

Nothing: keys are kept.

## What it never shows

The card shows paths only. It never reads the file, so it never shows a key, a fingerprint or any digest of one; whether the file is there is the node's own check. The card shows the key type and its path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.jwkKey key) (Keys.jwkKey key)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
