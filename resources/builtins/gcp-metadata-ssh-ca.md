# SSH CA key in project metadata

Installs an SSH CA public key into a GCP project's metadata, so that instances trust it.

```haskell
SshAccess.installMetadataCaKey reporter gcloud (SshAccess.MetadataSshCa project publicKeyPath)
```

Defined in [`Salmon.Builtin.Nodes.Gcp.SshAccess`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/SshAccess.hs).

## Effect site

The node's `ref` is `gcp-metadata-ssh-ca`, keyed by the project id and the public key's path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

None: `run up` adds it every time (`gcloud` replaces the metadata key). Under `serve` it parks and nothing watches it.

## up

`gcloud compute project-info add-metadata --metadata-from-file ssh-ca=<path>`.

## down

Nothing: the metadata key stays.

## What it never shows

The public key's contents. The card shows the project and the path, and links to the project's metadata page in the Cloud console, built from the project id alone.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.metadataSshCa cfg) (SshAccess.installMetadataCaKey reporter gcloud cfg)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
