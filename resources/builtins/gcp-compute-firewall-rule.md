# GCP firewall rule

Creates one ingress firewall rule on a VPC network.

```haskell
Compute.firewallRule reporter gcloud fw
```

Defined in [`Salmon.Builtin.Nodes.Gcp.Compute`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/Compute.hs).

## Effect site

The node's `ref` is `gcp-firewall-rule`, keyed by project and rule name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when a rule of that name exists and holds what is declared: the protocols and ports it allows, its source ranges and its target tags, compared as sets (source ranges only when some are declared), on the declared network, as an ingress allow rule. Absent, or any of those differing: a failure naming both sides. A describe that failed for a reason of its own, or a rule that could not be read: unknown.

## up

Creates the ingress rule when absent. An existing rule whose ports, source ranges or target tags differ is updated to the declared ones (`gcloud compute firewall-rules update`). A rule on another network, an egress rule or a deny rule cannot be updated into the declared one: `up` fails naming the difference and changes nothing.

## down

Deletes the rule if it is there.

## What it never shows

Nothing secret is involved. The card shows the rule, project, network, what it allows, the source ranges and target tags.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.firewallRule fw) (Compute.firewallRule reporter gcloud fw)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project. The comparison and the update have only been tested on a describe written from the API's resource shape, never against a real project.
