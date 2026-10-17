# GCP firewall rule

Creates one ingress firewall rule on a VPC network.

```haskell
Compute.firewallRule reporter gcloud fw
```

Defined in [`Salmon.Builtin.Nodes.Gcp.Compute`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/Compute.hs).

## Effect site

The node's `ref` is `gcp-firewall-rule`, keyed by project and rule name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when a rule of that name exists. Absent: `up` creates it.

## up

Creates the ingress rule. An existing rule of that name is neither compared nor changed.

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

Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
