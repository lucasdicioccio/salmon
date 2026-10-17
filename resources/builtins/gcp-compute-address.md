# GCP reserved IP address

Reserves a regional IP address, external or internal.

```haskell
Compute.address reporter gcloud addr
```

Defined in [`Salmon.Builtin.Nodes.Gcp.Compute`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/Compute.hs).

## Effect site

The node's `ref` is `gcp-address`, keyed by project, region and address name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when reserved as declared. Not reserved, or holding another address than the declared one: `up` reserves it.

## up

Reserves the address, retrying while a freshly enabled API settles.

## down

Releases the address if it is reserved.

## What it never shows

Nothing secret is involved. The card shows the name, project, region and kind (with the subnet and literal for an internal one).

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.address addr) (Compute.address reporter gcloud addr)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

An address has no console page of its own, so the card links to the list of addresses. Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
