# Directory

Makes sure a directory exists.

```haskell
dir (Directory path)
```

Defined in [`Salmon.Builtin.Nodes.Filesystem`](../../salmon-ops/src/Salmon/Builtin/Nodes/Filesystem.hs).

## Effect site

The node's `ref` is `directory`, keyed by the path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

No check: asking costs what creating costs, so `up` re-applies it each pass (under `serve`, on the supervision delay).

## up

Creates the directory and its parents. It never touches what is inside.

## down

Removes the directory only when it is empty. A non-empty one fails, on purpose.

## What it never shows

Nothing the declaration does not already publish: the card shows the path.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (Cards.directory path) (dir (Directory path))
```

The card is in [`Salmon.Builtin.Cards`](../../salmon-ops/src/Salmon/Builtin/Cards.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

A directory shared by many nodes should be its own node, created and never removed (see the variants in the same module).
