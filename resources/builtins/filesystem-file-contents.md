# File contents

Keeps one file's whole contents as declared.

```haskell
filecontents (FileContents path body)
```

Defined in [`Salmon.Builtin.Nodes.Filesystem`](../../salmon-ops/src/Salmon/Builtin/Nodes/Filesystem.hs).

## Effect site

The node's `ref` is `file-contents`, keyed by the path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Reads the file and compares it with the declaration. In place: skipped. Missing or different: `up` writes it.

## up

Writes the whole file. There is no merge with what is there.

## down

Removes the file. A file already absent is fine. The directory stays (it is its own node).

## What it never shows

The contents, and any digest of them. The card shows the path and its directory. A `content-hash` note, when the builtin has one, is still among the notes.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (Cards.fileContents path) (filecontents (FileContents path body))
```

The card is in [`Salmon.Builtin.Cards`](../../salmon-ops/src/Salmon/Builtin/Cards.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
