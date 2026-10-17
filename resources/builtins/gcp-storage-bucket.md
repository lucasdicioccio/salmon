# GCP Cloud Storage bucket

Creates one Cloud Storage bucket.

```haskell
Storage.bucket reporter gcloud bkt
```

Defined in [`Salmon.Builtin.Nodes.Gcp.Storage`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/Storage.hs).

## Effect site

The node's `ref` is `gcp-bucket`, keyed by the bucket name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when the bucket is there. Absent: `up` creates it.

## up

Creates the bucket.

## down

Deletes the bucket if it is there.

## What it never shows

Nothing secret is involved. The card shows the bucket, project, location and whether uniform bucket-level access is on.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.bucket bkt) (Storage.bucket reporter gcloud bkt)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

Contents, IAM bindings, lifecycle and website settings are their own nodes in the same module. Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
