# GCP secret delivered to a file

Writes one Secret Manager version to a file on this machine, with an owner and a mode.

```haskell
Secrets.secretFile reporter gcloud file
```

Defined in [`Salmon.Builtin.Nodes.Gcp.SecretManager`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/SecretManager.hs).

## Effect site

The node's `ref` is `secret-file`, keyed by the destination path. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

In place when the file is present, as the secret holds it. Missing, different, or the secret not readable: `up` writes it.

## up

Reads the secret version as this machine's identity and writes it to the path, atomically. The bytes are never reported.

## down

Removes the file.

## What it never shows

The secret's bytes and any digest of them. The card shows the path, the secret, the version, the project, the owner and the mode; whether the file is there comes from the node's own check.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (GcpCards.secretFile file) (Secrets.secretFile reporter gcloud file)
```

The card is in [`Salmon.Builtin.Cards.Gcp`](../../salmon-ops/src/Salmon/Builtin/Cards/Gcp.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

Every GCP card links to the resource's Cloud console page, built from the declaration's public names. Those console links have not been followed against a real project.
