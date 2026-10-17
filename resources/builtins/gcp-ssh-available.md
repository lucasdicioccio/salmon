# SSH reachable

A gate: SSH answers on a host, typically a VM created moments earlier.

```haskell
SshAccess.sshAvailable reporter endpoint  -- or sshAvailableWithin policy
```

Defined in [`Salmon.Builtin.Nodes.Gcp.SshAccess`](../../salmon-ops/src/Salmon/Builtin/Nodes/Gcp/SshAccess.hs).

## Effect site

The node's `ref` is `gcp-ssh-available`, keyed by the user, host and port. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Probes the host over ssh in batch mode. A new host key is accepted, a changed one refused.

## up

Probes again on the probe policy (30 attempts, 10s apart by default) and fails if the host never answers, so its dependants are blocked.

## down

Nothing: it only observes.

## What it never shows

The client key's contents. The card shows the host, the port and the user.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (KeyCards.sshAvailable endpoint) (SshAccess.sshAvailable reporter endpoint)
```

The card is in [`Salmon.Builtin.Cards.Keys`](../../salmon-ops/src/Salmon/Builtin/Cards/Keys.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.
