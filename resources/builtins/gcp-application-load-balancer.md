# GCP application load balancer

The root of a regional application (HTTP/HTTPS) load balancer: its parts are its dependencies.

```haskell
Lb.applicationLoadBalancer reporter gcloud alb  -- or the After/With variants
```

Defined in [`Salmon.Builtin.Nodes.Gcp.LoadBalancing`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/LoadBalancing.hs).

## Effect site

The node's `ref` is `gcp-application-lb`, keyed by project, region and balancer name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when every instance-group backend reports healthy. A backend not healthy, or an invalid declaration: the reason says which.

## up

Nothing of its own: its parts (address, health check, services, URL map, certificates, proxies, forwarding rules) are its dependencies.

## down

Nothing of its own: each part's node deletes its resource.

## What it never shows

Nothing secret is involved. The card shows the balancer, project, region and whether it serves HTTPS.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.applicationLoadBalancer alb) (Lb.applicationLoadBalancer reporter gcloud alb)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

A regional balancer has no stable console page of its own, so the card links to the list of balancers. Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
