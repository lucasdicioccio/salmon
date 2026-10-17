# Revision history for salmon-ops

## 0.2.0.0 -- unreleased

Breaking: several exported records gained or changed fields (e.g. `Gcp.Compute`'s instance,
`Client.Model`'s model) and some exported functions changed signature. Requires
`salmon-core ^>=0.1.0.0` (unchanged).

New builtins:

* `Podman.Quadlet`: containers as systemd services (bind volumes, `UserNS=keep-id`, readiness
  wait, registry login from a GCE instance, container jobs); `Systemd.Job`: oneshot units, timers
  and a node reading how the last run went.
* Postgres high availability: `Patroni`, `PgBackRest` (archive, PITR, replica method),
  `Haproxy` routing to the leader by health check, `Etcd` join phase.
* `PgTextSearch`, `ClipCpp` (embeddings for pgvector), `Plakar` integrations and GCS store.
* `SecretDelivery` (a pre-provisioned secret onto a machine), `Deferred` (a sub-graph built at
  `up` from a value read then), `Netbird` peer enrolment, `PortMapping` (UPnP-IGD via `upnpc`),
  `Rsync.receiveDir`.
* GCP: `Gcp.CloudDns` (managed zones; A, AAAA, CNAME, TXT, MX record sets); the load balancer
  as one node per resource (HTTPS proxy, managed and replaceable certificates, host/path rules,
  backend buckets, redirects, path rewrites, removal of what is no longer declared);
  `Gcp.Storage` bucket IAM, lifecycle, website settings and published directories;
  `Gcp.Compute` pinned internal address, access scopes, machine-type resize, run limit;
  `Gcp.Core` declared gcloud account; Cloud Run min instances and always-allocated CPU.
* `Salmon.Builtin.Guarded` / `Salmon.Op.Guard`: guarded risky operations (preconditions in
  `check`, park on failure, hold siblings).

Behaviour:

* Processes started for a node no longer inherit the pass's standard input.
* `Binary`: choose where a command's stdout/stderr go, with per-node log lines
  (`Salmon.Builtin.NodeLog`); `Ssh`/`Self` follow a remote call's output and reports.
* `Dag.foldDag` visits each node once instead of each occurrence.
* `Query`/`serve`: paths are matched, never listed; a retraction redescribes nothing.
* Web UI: compact DAG layout, `/history` timeline, `Conflicting` pairs side by side.
* Web UI: an optional Tramaj card template per node (`withCardTemplate`), served on `/dag` as
  `template` and drawn in place of the default card; presentation only, not part of the
  representative.
* `Builtin.Cards`: opt-in card templates for `filecontents`, `dir`, `systemdService` and
  Postgres `database` (`withCard (Cards.fileContents path) op`); no builtin attaches one itself.

## 0.1.0.0 -- 2026-10-01

* First release.
