# GCP Cloud DNS managed zone

Creates a public Cloud DNS zone for one domain.

```haskell
Dns.managedZone reporter gcloud zone
```

Defined in [`Salmon.Builtin.Nodes.Gcp.CloudDns`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/CloudDns.hs).

## Effect site

The node's `ref` is `gcp-dns-zone`, keyed by project and zone name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when the zone exists and serves the declared domain. Absent: `up` creates it. A zone of that name serving another domain makes `up` fail rather than take it over.

## up

Creates the public managed zone.

## down

Deletes the zone if it is there and serves the declared domain. Cloud DNS refuses while records other than the apex NS and SOA remain.

## What it never shows

Nothing secret is involved. The card shows the zone, project and domain.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.managedZone zone) (Dns.managedZone reporter gcloud zone)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
