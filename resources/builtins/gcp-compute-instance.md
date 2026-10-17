# GCP Compute Engine instance

Keeps one Compute Engine VM present, in the declared power state and machine type.

```haskell
Compute.gceInstance reporter gcloud inst
```

Defined in [`Salmon.Builtin.Nodes.Gcp.Compute`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/Compute.hs).

## Effect site

The node's `ref` is `gcp-instance`, keyed by project, zone and instance name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when the instance is there in the declared power state, machine type and scopes. Absent or not as declared: `up` creates, starts, stops or resizes it, as permitted (the reason says what differs).

## up

Creates the instance if absent, then starts, resumes or stops it to the declared power. A different machine type is changed only when the declaration permits a stop.

## down

Deletes the instance if it is there, whatever its state.

## What it never shows

The instance metadata and its files: a startup script may carry anything. The card shows the name, project, zone, machine type and declared power.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.instance_ inst) (Compute.gceInstance reporter gcloud inst)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
