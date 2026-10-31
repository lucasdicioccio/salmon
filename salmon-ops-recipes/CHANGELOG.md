# Revision history for salmon-ops-recipes

## 0.3.0.0 -- 2026-10-31

No change to the library's own code. Requires `salmon-ops ^>=0.3.0.0`, whose exports changed.

* Every external dependency now carries a PVP upper bound.
* Tests for the ClickHouse, PeerDB and `run serve` start-up/`--once`/`--json-log` additions of
  `salmon-ops` (Layer 0; Layer 2 under podman, PeerDB opt-in with `SALMON_TEST_PEERDB=1`).

## 0.2.0.0 -- 2026-10-23

Breaking: `SreBox.PostgresPair`'s records and some exported functions changed. Requires
`salmon-ops ^>=0.2.0.0`.

* `SreBox.PostgresPair`: one bouncer routing several databases, `hostssl` and SCRAM for
  replication and rewind, a member's SSH address separate from its replication address, every
  script under one `sudo` when the SSH login is not root.
* New `SreBox.PostgresPairPrereqs`: what a pair assumed was done first, as nodes.
* New `SreBox.PgVectorDemo`: index a directory of text and images into pgvector with a
  llama-server, clip.cpp or a model-free stand-in embedder, and search it by cosine distance.
* New `SreBox.WireGuardMesh`: declared peers, groups and policies unfolded per host.
* New `SreBox.Gcp.PlakarGcsBackup`; `SreBox.Gcp.VmProvision` exports the startup script that makes
  an instance trust the SSH CA.

## 0.1.0.0 -- 2026-10-01

* First release.
