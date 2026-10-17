# Builtin node pages

One page per builtin node, each linked from that builtin's web UI card
(`Salmon.Builtin.Cards`). Each page covers what the node manages, its effect
site (`ref`), what `check`, `up` and `down` do, what it never shows, and how
to attach its card.

The pages are written by hand. `website/scripts/sync-repo-docs.sh` mirrors
`resources/builtins/<slug>.md` to `builtin-<slug>.html` on the docs site, and
a card links to `Cards.docsBaseUrl <> "/builtin-<slug>.html"` (`Cards.docsLink`).
`Test.BuiltinCardsSpec` checks that every card names a page and that its
source is here. To add a page for a builtin, write `<slug>.md` here and set
`cardDocs = Just "<slug>"` on its card. Keep the page in step with the card's
sentences. The link lives only in the card template, which is not part of the
representative, so adding one restarts nothing.

## Filesystem and systemd

- [File contents](filesystem-file-contents.md)
- [Directory](filesystem-directory.md)
- [systemd service](systemd-service.md)

## Postgres

- [Database](postgres-database.md)
- [Login role](postgres-user.md)
- [Login role, password from a file](postgres-user-passfile.md)
- [Replication role](postgres-replication-user.md)
- [Replication slot](postgres-replication-slot.md)
- [pg_hba.conf line](postgres-hba-line.md)
- [ALTER SYSTEM setting](postgres-alter-system.md)
- [Server TLS](postgres-server-tls.md)

## GCP

- [Compute Engine instance](gcp-compute-instance.md)
- [Reserved IP address](gcp-compute-address.md)
- [Firewall rule](gcp-compute-firewall-rule.md)
- [Subnetwork](gcp-compute-subnet.md)
- [Cloud DNS managed zone](gcp-dns-managed-zone.md)
- [Cloud DNS record set](gcp-dns-record-set.md)
- [Cloud Storage bucket](gcp-storage-bucket.md)
- [Secret Manager secret](gcp-secret.md)
- [Secret delivered to a file](gcp-secret-file.md)
- [Application load balancer](gcp-application-load-balancer.md)
