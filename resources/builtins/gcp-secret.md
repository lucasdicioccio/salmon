# GCP Secret Manager secret

Creates a Secret Manager secret, with no version.

```haskell
Secrets.secret reporter gcloud sec
```

Defined in [`Salmon.Builtin.Nodes.Gcp.SecretManager`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/SecretManager.hs).

## Effect site

The node's `ref` is `gcp-secret`, keyed by project and secret name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when the secret is present. Missing: `up` creates it, empty (versions are their own nodes).

## up

Creates the secret, with no version.

## down

Deletes the secret if it is there, and with it every version.

## What it never shows

Any version's value, and any digest of it. The card shows the name, project and replication policy.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.secret sec) (Secrets.secret reporter gcloud sec)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
