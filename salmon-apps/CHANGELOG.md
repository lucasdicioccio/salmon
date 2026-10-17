# Revision history for salmon-apps

## 0.2.0.0 -- unreleased

Breaking: exported functions of `GcpToy`, `PgPair` and `QemuPgHaToy` changed. Requires
`salmon-ops ^>=0.2.0.0` and `salmon-ops-recipes ^>=0.2.0.0`.

* New `salmon-cloudrun`: a Cloud Run turnup from one declarative file.
* New `salmon-pgvector-demo`: `SreBox.PgVectorDemo` as a binary, with a read-only `search`.
* New `salmon-report`: read-only capability probes (DNS setup, STUN mapping type, hairpin NAT,
  inbound reachability from a second vantage point, nmap on declared targets).
* New `salmon-docs-sync`: detect `docs/` drift, regenerate, commit and push.
* `salmon-tui`: a live tail of what a node writes.
* `salmon-gcp-toy`: load balancer, Cloud DNS and a quadlet-served tier; instance run limit.
* `salmon-toy-qemu-pg-ha`: a VM harness driven live through `run serve --http`.

## 0.1.0.0 -- 2026-10-01

* First release.
