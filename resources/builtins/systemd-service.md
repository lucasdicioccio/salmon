# systemd service

Keeps a systemd service enabled and running from the unit it is given.

```haskell
systemdService reporter systemctl unitFile cfg
```

Defined in [`Salmon.Builtin.Nodes.Systemd`](../../salmon-ops/src/Salmon/Builtin/Nodes/Systemd.hs).

## Effect site

The node's `ref` is `systemd-unit`, keyed by the unit name. Two declarations with the same `ref` are the same effect site. If they differ, the last one wins and the other is reported `Conflicting`.

## check

Asks systemd whether the unit is active, enabled and loaded as written. Not as declared: `up` reloads, enables and restarts it. In transition, or no answer from `systemctl`: `Unknown`, and nothing restarts on that.

## up

`systemctl daemon-reload`, `enable`, then `restart`.

## down

`systemctl stop`. The unit file is its own node.

## What it never shows

The unit file's body. The card shows the unit, its scope (system or user) and its unit directory.

## Its web UI card

The card is opt-in, attached per node. It is presentation only: it is left out of the node's representative, so adding it or dropping it restarts nothing.

```haskell
withCard (Cards.systemdService cfg) (systemdService reporter systemctl unitFile cfg)
```

The card is in [`Salmon.Builtin.Cards`](../../salmon-ops/src/Salmon/Builtin/Cards.hs). It shows the facts listed above, the last check in the node's own words, and what `up` and `down` do. It also links back to this page.

## Notes

A service whose *config file* changed is not restarted by this node alone: the check looks at the unit file. Use `systemdServiceWatching` with the files the service reads at start.
