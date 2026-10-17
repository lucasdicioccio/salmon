# GCP Cloud DNS record set

Keeps one record set (a name and a type) holding exactly the declared data and TTL.

```haskell
Dns.recordSet reporter gcloud rs  -- or Dns.resolvedRecordSet
```

Defined in [`Salmon.Builtin.Nodes.Gcp.CloudDns`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/CloudDns.hs).

## Effect site

The node's `ref` is `gcp-dns-record`, keyed by the zone, the name and the type. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when it holds exactly the declared TTL and data. Absent, or holding other data or TTL: `up` writes the declared set. For a resolved record set, data not readable yet is `Unknown`.

## up

Creates the record set, or updates it in place. The name and type are the effect site, so one made by hand is taken over (the one documented exception to "never take over what salmon did not make").

## down

Deletes the record set at that name and type if there is one, whatever it holds.

## What it never shows

Nothing secret is involved. The card shows the name, type, TTL, data ("resolved when applied" for a resolved one), zone and project.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.recordSet False rs) (Dns.recordSet reporter gcloud rs)
withCard (GcpCards.recordSet True rs) (Dns.resolvedRecordSet reporter gcloud rs resolve)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

A record set has no console page of its own, so the card links to its zone. Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
