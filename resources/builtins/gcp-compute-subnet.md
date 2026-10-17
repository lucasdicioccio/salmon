# GCP subnetwork

Creates one regional subnetwork on a VPC network.

```haskell
Compute.subnet reporter gcloud net
```

Defined in [`Salmon.Builtin.Nodes.Gcp.Compute`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/Compute.hs).

## Effect site

The node's `ref` is `gcp-subnet`, keyed by project, region and subnet name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when the subnet exists with the declared purpose. Absent: `up` creates it. Of another purpose: `up` does not change that, and the reason names both.

## up

Creates the subnetwork. The range is not compared with an existing one.

## down

Deletes the subnetwork if it is there.

## What it never shows

Nothing secret is involved. The card shows the name, project, region, network, range and purpose.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.subnet net) (Compute.subnet reporter gcloud net)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
