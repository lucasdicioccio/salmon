# Validating the GCP builtins against a real project

This is the playbook for running salmon's `Salmon.Builtin.Nodes.Gcp.*` builtins
against real GCP, in a sandbox you are willing to destroy. It exists because the
GCP nodes' automated tests are all Layer 0: they check the *pure* verdict
functions (`interpretInstanceStatus`, `interpretBillingDescribe`, …) and the
rendered `gcloud` argument lists. Nothing in `cabal test` ever talks to Google,
so "does an `up` actually converge, and is it idempotent, and does `down` take
it all away" is a question only a real project can answer.

Three pieces do that:

- `salmon-apps`'s **`salmon-gcp-toy`** binary (`salmon-apps/src/GcpToy.hs`), a
  tiered, throwaway stack built out of the Gcp builtins, driven through the
  usual seed → directive → ops protocol.
- **`salmon-apps/scripts/gcp-toy-validate.sh`**, which runs that binary through
  `up` → `up` → `down` and reports what each pass did.
- **`salmon-apps/scripts/gcp-toy-serve.sh`**, which keeps that binary up as a
  `run serve` server instead — the HTTP surface, the web UI, `salmon-tui` —
  and changes what it wants one word at a time (`tier0`, `tier1`, `tag v2`,
  `down`), with tier 2's two passes driven through pull mode by editing a
  registry document. Use it once tier 0 is clean under the validator.

The blast radius is bounded by the project: by default the toy *creates* the
project it works in, which makes the project the deepest node of the graph, so
`run down` tears every resource down individually first (that is the part being
validated) and then deletes the project, which sweeps anything a buggy `down`
left behind.

## What the toy declares

Tiers are cumulative, ordered by cost. Pick one with `--tier`.

**Tier 0** (≈ free — nothing here is billed beyond negligible storage):

| Node | Resource |
|---|---|
| `Gcp.Core.applicationDefaultCredentials` | validates ADC before anything else runs |
| `Gcp.Core.declaredAccount` | with `--account EMAIL`: refuses unless gcloud's active account is that one |
| `Gcp.ResourceManager.project` | the project itself (skipped with `--existing-project`) |
| `Gcp.Billing.linkBillingAccount` | links the billing account |
| `Gcp.ServiceUsage.enableService` | `storage`, `iam`, `artifactregistry` (and `run` at tier 1, `monitoring` with `--alert-email`) |
| `Gcp.Storage.bucket` | `<project>-<prefix>` , uniform bucket-level access |
| `Gcp.Iam.serviceAccount` | `<prefix>-sa@<project>.iam.gserviceaccount.com` |
| `Gcp.ArtifactRegistry.artifactRepository` | `<prefix>-repo`, docker format |
| `Gcp.Iam.iamBinding` ×2 | `storage.objectViewer` on the bucket, `artifactregistry.reader` on the repo |

With `--dns-zone DNS_NAME`, tier 0 also enables `dns.googleapis.com` and
declares `Gcp.CloudDns.managedZone` — a public zone `<prefix>-zone` for that
domain — with a toy node on top that reads back the name servers Cloud DNS
assigned (`Gcp.CloudDns.readNameServers`) and prints them:

    name servers for example.org.: ns-cloud-c1.googledomains.com. ns-cloud-c2.googledomains.com. ...

Those four are what goes to the registrar to delegate the domain. Creating a
zone needs no proof that you own the name, and nothing resolves through it
until the registrar points there, so any name will do for a validation run. A
zone is billed per month, pro rata (cents). `down` deletes it.

**Tier 1** (cents) adds `SreBox.Gcp.CloudRunDeploy.buildPushDeploy`: podman
builds an image, logs in to `<region>-docker.pkg.dev` with an isolated
authfile, pushes, and `Gcp.CloudRun.cloudRunService` deploys it as
`<prefix>-hello` running as the tier-0 service account, `--max-instances 1`.

The image is either `FROM --base-image` (Google's `cloudrun/container/hello`
sample by default, plus a label carrying `--image-tag` so each tag really is a
distinct image) or your own `--containerfile PATH`, built with that file's
directory as the podman build context. Whatever you deploy must serve HTTP on
`$PORT` and be linux/amd64, or the revision never becomes ready. The service is
deployed *without* `--allow-unauthenticated`: the toy asserts the deploy
happened and runs the expected image, it never issues an HTTP request to it.

With `--alert-email ADDRESS`, tier 1 also declares
`SreBox.Gcp.CloudRunAlerts.standardAlerts` on the service: one
`Gcp.Monitoring.notificationChannel` (`<prefix> alerts`, an email channel to
that address) and four `Gcp.Monitoring.alertPolicy` nodes — `<prefix>-hello:
5xx ratio`, `p99 latency`, `memory` and, since the service has
`--max-instances 1`, `instances at max`. Both resources are addressed by
display name (Cloud Monitoring assigns the ids), and a policy carries a
`salmon-fingerprint` user label of what it was rendered from: a second pass
skips the four, editing one in the console makes the next pass update it
back, and `run down` deletes what a lookup by name finds. Alerting is free;
what this tier exercises is create, skip, update and delete against the real
API, which the Layer 0 tests on the rendered `gcloud` argv and policy JSON
cannot.

**Tier 2** (an `e2-micro`'s hourly rate, plus a reserved IP) is
`specs/gcloud-support.md` §6's "objective": a VM salmon boots, trusts and
then provisions with *this same binary*.

| Node | Resource |
|---|---|
| `Gcp.Compute.address` | a reserved regional external IP, `<prefix>-ip` |
| `Gcp.Compute.firewallRule` | `tcp:22` from `--ssh-source-range` to instances tagged `<prefix>-ssh` |
| `SreBox.Gcp.VmProvision.caTrustStartupScriptFile` (a `Filesystem.filecontents`) | the startup script, passed as `--metadata-from-file` |
| `Keys.sshKey` ×2, `Keys.signKey` | a CA and a client key, and a certificate for `--vm-user` |
| `Gcp.SshAccess.installMetadataCaKey` | the CA's public key, into project metadata |
| `Gcp.Compute.gceInstance` | the VM, claiming the address and carrying the tag |
| `Gcp.SshAccess.sshAvailable` | waits for sshd to answer *as that user, with that certificate* |
| `Secrets.sharedSecretFile`, `SecretDelivery.uploadSecretFile` (a `vmp_beforeCall` node) | a secret generated in the workdir, put on the VM as `/etc/salmon-toy/secret`, `root:root 0600`, before the binary runs |
| `Self.uploadAndCallSelfAsSudoWith` (via `SreBox.Gcp.VmProvision.provisionedVm`) | rsyncs this binary over and runs `run up` on it there |

The startup script is what closes the gap `installMetadataCaKey` leaves:
nothing on a GCE instance reads that metadata key by itself. It is not the
toy's own: `SreBox.Gcp.VmProvision` exports it as `caTrustStartupScript`
(text, given the login user), with `caTrustStartupScriptFile` to write it as
a node and `withStartupScriptFile` to name it in an `Instance`'s metadata, so
a consumer does not carry a copy. It waits for the key to be visible (a 404
under `set -e` would otherwise leave a machine nobody can log into), fetches the
CA from the metadata server into `/etc/ssh/salmon_ca.pub`, points sshd's
`TrustedUserCAKeys` at it, creates the login user the certificate names as
its principal (with no OS Login, a principal must be a local account), gives
it passwordless sudo, and makes sure `rsync` is there for the upload.

**Tier 2 provisions in the pass that creates the machine.** GCP picks the
address, and an `Op` naming the host has to be built before any `up` runs, so
the nodes that name it (the ssh probe, the secret upload, the remote call)
cannot be declared. Without `--vm-ip` the toy uses
`VmProvision.provisionedVmReadingHost`: the instance, the CA and the signed
key are ordinary nodes, and the hand-off is one `Salmon.Builtin.Nodes.Deferred`
node whose `up` reads the address (`Compute.readAddress`), builds those nodes
from it and walks them. `run tree` shows that node and not what is inside it.

The script still drives two passes: after the first it reads the IP itself
(`gcloud compute addresses describe`) and re-issues the directive with
`--vm-ip`, which declares the hand-off as ordinary nodes. The second pass is
then expected to skip or re-check what the first already did, rather than to
do the provisioning. **The one-pass path has not been run against a real
project**; before this change the first pass stopped at the instance, so a
first pass that fails at `deferred` is this code, not the infrastructure.
Under `run serve`, moving from a seed without `--vm-ip` to one with it
retires the deferred node, whose `down` removes the uploaded secret before the
declared upload puts it back.

What proves it worked is the file the uploaded binary writes on the VM,
`/var/lib/salmon-toy/provisioned`; the script reads it back over ssh. The
binary runs there with the same directive, tagged `OnVm`, which is why the
payload is declared in the same `Track'` as everything else.

**Tier 2 can also declare a peer** (a second `e2-micro`), with
`--vm-internal-ip A --peer-internal-ip B`: two free addresses in the region's
`default` subnet (`gcloud compute networks subnets describe default --region
REGION --format='value(ipCidrRange)'` says which range; `10.132.0.0/20` in
`europe-west1`).

| Node | Resource |
|---|---|
| `Gcp.Compute.address` ×2 | reserved *internal* addresses `<prefix>-vm-internal` and `<prefix>-peer-internal`, at A and B |
| `Gcp.Compute.gceInstance` | the VM again, now also pinned to A (`--private-network-ip`) |
| `Gcp.Compute.gceInstance` | `<prefix>-peer`: pinned to B, and with no external address (`--no-address`) |
| `Gcp.Compute.firewallRule` | `tcp:8081` from `A/32` to instances tagged `<prefix>-peer` |
| `gcp-toy-peer-reached` (on the VM) | fetches `http://B:8081/` and keeps the answer in `/var/lib/salmon-toy/peer-reached` |

The point is the contrast with the paragraph above: an internal address is
the caller's choice, so it is known when the graph is declared, and the
firewall rule naming the VM and the URL naming the peer are written before
either machine exists. That is what a recipe naming its peers by address
(`SreBox.PostgresPair`: a `/32` in `pg_hba.conf`, a `primary_conninfo`) needs
to run on GCE without replicating over public addresses. The peer has no way
out of the VPC — the toy declares no Cloud NAT — so its startup script uses
only what the image ships. An instance's addresses are fixed when it is
created: adding the two flags to a run whose VM already exists changes
nothing about that VM.

**Tier 3** (a forwarding rule's hourly rate on top of tier 2) puts a
*regional external* Application Load Balancer in front of that VM.

| Node | Resource |
|---|---|
| `Gcp.Compute.subnet` | the proxy-only subnet, `<prefix>-proxy`, `REGIONAL_MANAGED_PROXY`/`ACTIVE` |
| `Gcp.Compute.instanceGroup` | an unmanaged, zonal group, `<prefix>-ig` |
| `Gcp.Compute.instanceGroupMember` | the tier-2 VM, put in it |
| `Gcp.Compute.firewallRule` | `tcp:<--lb-port>` from the proxy range **and** the health-check ranges, to instances tagged `<prefix>-lb` |
| `Gcp.LoadBalancing.applicationLoadBalancer` | health check, backend service, named ports, URL map, target proxy, forwarding rule (a node each, under one root) |
| `Podman.buildImage`, `Podman.pushLoggingIn` | `<region>-docker.pkg.dev/<project>/<prefix>-repo/page:<--image-tag>`, an `nginx:alpine` with the page copied in, built in `<workdir>/page` and pushed before the instance is created |
| `Gcp.Compute.gceInstance` (the tier-2 VM, changed) | created as `<prefix>-sa` with the `cloud-platform` scope; tier 0's `roles/artifactregistry.reader` grant and the account are now its prerequisites |
| `Debian.deb` (on the VM) | `podman` |
| `Gcp.ArtifactRegistry.instanceLogin` (on the VM) | `/var/lib/salmon-toy/registry-auth.json`, the metadata server's token as `oauth2accesstoken` |
| `Podman.Quadlet.quadletContainer` (on the VM) | `/etc/containers/systemd/salmon-toy-page.container`, so `salmon-toy-page.service`: the image's port 80 on `--lb-port` |

With `--dns-zone DNS_NAME`, tier 3 also declares
`Gcp.CloudDns.resolvedRecordSet`: an `A` record `lb.DNS_NAME` (TTL 300) at
the address GCP gave the forwarding rule. That address does not exist until
the balancer's `up` has run, so the record's data is read at `up`
(`Gcp.LoadBalancing.readAddress`) instead of being declared — no extra pass.
Its `check` describes the record set and compares it with the address read
again, so a second `up` skips it. The script compares what the zone holds
with the forwarding rule's address; it does not resolve the name, since
nothing resolves through a zone the registrar does not delegate to. `down`
removes the record before the zone, which Cloud DNS would otherwise refuse
to delete.

With `--lb-https` as well (it needs `--dns-zone`), the same balancer also
serves `lb.DNS_NAME` over HTTPS, and the tier puts everything else
`Gcp.LoadBalancing` can express through the project once:

| Declared | Resource |
|---|---|
| `ManagedCertificate` | a regional Certificate Manager certificate `<prefix>-lb-cert` for `lb.DNS_NAME`, and one DNS authorization `<prefix>-lb-cert-lb-<DNS_NAME, dots as dashes>` |
| (the certificate list being non-empty) | a reserved address `<prefix>-lb-ip`, a target HTTPS proxy `<prefix>-lb-https-proxy`, a second forwarding rule `<prefix>-lb-https-fw` on `:443` — both rules on that one address |
| `BackendService "slow"` | a second backend service `<prefix>-lb-slow-backend` over the same instance group, with a 120-second timeout |
| `HostRule` / `PathRule` | the URL map imported whole: `lb.DNS_NAME` to the default service, `/slow/*` to the second one |
| a toy node | the `CNAME` the authorization asks for, published in the zone |

A *regional* balancer cannot use the classic Google-managed
`compute ssl-certificates` (those are global only, and are the ones that
provision once DNS points at the balancer). What it takes is a regional
Certificate Manager certificate issued against a **DNS authorization**: a
`CNAME` whose name and target GCP picks, which the zone has to carry. So the
order is the reverse of the classic one — the certificate can be issued
before anything points at the balancer, but **only if `DNS_NAME` is really
delegated to the zone's name servers** (the ones tier 0 prints). Without the
delegation everything is created, the certificate stays `PROVISIONING`, the
balancer's check says `Unknown`, and `:443` does not complete a handshake.

The VM serves one page, so `/slow/*` answers `404` through the balancer; the
path rule and the timeout are read back from the URL map and the backend
service rather than observed in a response.

Three of those exist only because a regional external ALB is an Envoy fleet
rather than a Google frontend, and that is what the tier is really testing:
the proxies run *inside* the VPC, in a proxy-only subnet that must already
exist in the region; they reach the backends **from that subnet's range**, so
the backend firewall has to allow it — as does the separate
`35.191.0.0/16` + `130.211.0.0/22` pair the *health checks* come from, which
is a different source entirely and the usual reason a balancer that came up
cleanly still answers `502`; and a VM is not a backend, an instance group is.

The web server is declared on the **VM side**, by the tier-2 payload. That is
deliberate: a forwarding rule that merely exists proves nothing, so what the
script checks is a `200` carrying the project id, and the only thing that can
put that body there is salmon running on the machine. A green tier 3 is
therefore a second, independent proof that the tier-2 hand-off worked — this
time through the front door.

The page is not a file salmon wrote on the VM. It is baked into an image the
control side builds and pushes to the toy's own repository, and the VM runs
that image as a quadlet in **system scope**, having logged podman in to the
registry with the token the metadata server gives the instance's service
account. So the body (`served by salmon-gcp-toy from <project>, in a
container pulled from <image>`) is also the evidence for three things that
were written from documentation: `quadletContainer` as root under the system
manager, `instanceLogin` on a real instance, and a pull through the auth file
that login wrote. The script's verdict line says `from the VM's container`
only when the body names the image. **None of this has been run against a
real project yet** (see the gaps below for what was run instead).

Until this change tier 3 served the page from an authored unit,
`salmon-toy-web.service` (`python3 -m http.server`). A VM provisioned by that
toy is not converted: its account and scopes were fixed when it was created
(the instance node then fails on the scopes and changes nothing), and its old
unit would still hold the port. Take the toy down and up again.

## Step 1 — dry run, no GCP calls

`run tree` only expands the graph; it never shells out to `gcloud`. Do this
first, with a throwaway billing id, to see what a given set of flags declares:

```sh
cd <repo>
cabal build salmon-gcp-toy
TOY=$(cabal list-bin salmon-gcp-toy)

$TOY config --project salmon-toy-$(date +%s) \
      --billing-account 000000-000000-000000 --tier 0 \
  | $TOY run tree
```

`config` also validates the flags (project id shape, prefix length, that
`--containerfile` exists, that a created project has a billing account), so a
typo fails here rather than half-way through an `up`.

## Step 2 — find the real ids

```sh
gcloud billing accounts list   # ACCOUNT_ID -> --billing-account
gcloud organizations list      # ID         -> --organization (omit if you have none)
```

The billing account must be one the *active account* can see in that listing
**and** `OPEN: True` — a closed account cannot be linked, and an id you cannot
see fails with a permission error that reads as if the id might not exist. The
script pre-flights this (`gcloud billing accounts describe`) before it declares
anything, because linking is the first step that touches something the caller
may not own, and by then a project has already been created.

## Step 3 — point gcloud at the sandbox

The toy needs credentials twice over: the `gcp-adc` node checks *application
default credentials*, while every `gcloud` invocation uses the *active account*.
They are separate logins, and a stale one is the most common cause of a
first-run failure.

```sh
gcloud config configurations create salmon-sandbox   # leaves other configs alone
gcloud auth login
gcloud auth application-default login
gcloud config get-value account                      # confirm the sandbox identity
```

Pass `--account you@example.com` in the seed to have the graph check this
rather than you: `Gcp.Core.declaredAccount` reads `gcloud config get-value
account` and, on any other answer, fails before a single resource is created,
blocking everything declared on top of it. It never switches the account
itself. Without the flag nothing is asserted, as before.

The script prints the active account and the ambient project when it starts.
The ambient project should not matter: every node passes `--project`
explicitly. If a run only works when the ambient project happens to be right,
that is a bug in a node, and worth reporting.

## Step 4 — the real run

```sh
salmon-apps/scripts/gcp-toy-validate.sh -- \
  --project salmon-toy-$(date +%s) \
  --organization YOUR_ORG_ID \
  --billing-account YOUR_BILLING_ACCOUNT \
  --tier 0
```

Then, once tier 0 is clean, the same with `--tier 1` (needs `podman`), and/or
`--containerfile ./myapp/Containerfile` to deploy your own app. Tier 2 adds
`--vm-zone` (default `<region>-b`), `--vm-machine-type` (`e2-micro`),
`--vm-image-family`/`--vm-image-project` (Ubuntu 24.04 LTS), `--vm-user`
(`salmon`) and `--ssh-source-range` (`0.0.0.0/0` — narrow it to your own
address if the sandbox is not disposable). Tier 3 adds `--lb-proxy-range`
(`192.168.100.0/24`) and `--lb-port` (`8080`), and with `--dns-zone` takes
`--lb-https`.

### The HTTPS run

Nothing below has been run against a real project yet; this is the run that
would close the gap. It needs a domain you control, and the full verdict
needs that domain (or a subdomain of it) delegated to the zone the toy
creates, which takes two steps because the name servers are only known once
the zone exists.

```sh
# 1. tier 0 with the zone, kept: prints the name servers Cloud DNS assigned
salmon-apps/scripts/gcp-toy-validate.sh --keep -- \
  --project YOUR_TOY_PROJECT \
  --organization YOUR_ORG_ID \
  --billing-account YOUR_BILLING_ACCOUNT \
  --dns-zone toy.example.org \
  --tier 0

# 2. at the registrar (or in the parent zone): NS records for
#    toy.example.org at those four name servers. Then wait for
dig +short NS toy.example.org        # to answer with them

# 3. the same project, tier 3 with HTTPS (the script drives both passes of a
#    tier >= 2 itself: reserve the VM address, then re-issue with --vm-ip)
salmon-apps/scripts/gcp-toy-validate.sh -- \
  --project YOUR_TOY_PROJECT \
  --existing-project \
  --dns-zone toy.example.org \
  --tier 3 --lb-https
```

Adjust step 3's project flags to whatever step 1 used; the point is that the
zone, and so its name servers, must be the same one. `CERT_WAIT_ATTEMPTS`
(default 30, twenty seconds apart) bounds how long the script waits for the
certificate.

A green run prints, beside the tier-3 lines:

- `https: :443 and :80 share the reserved address`
- `rules: the URL map routes lb.toy.example.org, and /slow/* to the second backend service`
- `timeout: the second backend service waits 120s`
- `authorization: the zone holds the CNAME Certificate Manager asked for`
- `certificate: <prefix>-lb-cert is ACTIVE`
- `https: the balancer served the VM's page over HTTPS, certificate verified for lb.toy.example.org`

Without the delegation the first four can still be green; the certificate
line then says so and HTTPS is not probed. To check by hand what the script
checks:

```sh
gcloud compute url-maps describe <prefix>-lb-url-map --region REGION --project P
gcloud compute backend-services describe <prefix>-lb-slow-backend --region REGION --project P --format='value(timeoutSec)'
gcloud certificate-manager dns-authorizations describe <prefix>-lb-cert-lb-toy-example-org --location REGION --project P
gcloud certificate-manager certificates describe <prefix>-lb-cert --location REGION --project P --format='value(managed.state)'
curl --resolve lb.toy.example.org:443:LB_IP https://lb.toy.example.org/
```

A second `run up` on the directive should skip the balancer (its check is
`Success` once the backends are healthy and the certificate is `ACTIVE`),
though by design it would re-run two "set" calls if it did not: the URL map
import and the timeout update.

**Turning HTTPS on for a balancer that already exists** leaves its `:80`
rule on the ephemeral address it was created with — a forwarding rule's
address cannot be changed, and the script only creates what is missing. The
`:443` rule gets the reserved one, and `readAddress` reads that. Tear the
balancer down first if the two must agree.

Script options, before the `--`: `-y` skips the confirmation prompt, `--keep`
skips teardown (it then prints the `run down` command to finish up later).
Everything after the `--` goes verbatim to `salmon-gcp-toy config`. `OUT=<dir>`
and `SALMON_GCP_TOY=<binary>` override the log directory and the binary.

**Use a fresh project id every run.** A deleted project sits in
`DELETE_REQUESTED` for ~30 days and nobody can reuse its id in that window —
`$(date +%s)` in the id is there for exactly this. `Gcp.ResourceManager`'s check
reports that state with its own message rather than as a plain "not found",
because the `up` that follows is going to fail on the id, not on credentials.

**Tier 2 also delivers a secret.** The control side generates 32 random
bytes into `<workdir>/secrets/toy-secret` and
`Salmon.Builtin.Nodes.SecretDelivery.uploadSecretFile` writes them to
`/etc/salmon-toy/secret` on the VM over the connection the provisioning
already uses, before the uploaded binary is called. The VM-side directive
knows only the path: its `gcp-toy-secret-read` node reads the file, fails if
it is absent or not 64 bytes long, and leaves
`/var/lib/salmon-toy/secret-read` saying how many bytes it read. The script
reads that marker and the file's owner and mode back, never its contents. On
the second pass the upload should be a `Skip` (its check compares the remote
file with the local one), which the script notes.

## What the passes mean

| Pass | What a clean result looks like | What a dirty one tells you |
|---|---|---|
| `up` #1 | converges first time | a failure triggers **one** retry; "converged ONLY ON RETRY" means something needed time — IAM propagation after creating a service account, or a just-enabled API — that no node waits out yet |
| `up` #2 | every node with a real `check` reports `Skip` | a node listed as "RE-APPLIED DESPITE A CHECK" has a `check` that never says `Success`, i.e. it is not idempotent under `run up` |
| `down` | succeeds, then the project is `DELETE_REQUESTED` (or, with `--existing-project`, every resource fails to `describe`) | a failed `down` leaves that node standing and `Blocked`s everything it depends on — including, deliberately, the project delete, so the leftovers are still there to look at |

Nodes that legitimately have no `check` today re-apply on every pass; the
script lists them separately rather than counting them as findings. Tier 2
adds the expensive members of that list: `rsync:sendfile` re-uploads the
binary and `ssh:call` re-runs the remote directive every pass. The remote run
is itself idempotent — it streams its own report back over ssh, and on the
second pass it reports `Skip` for its file node, which the script surfaces.

Two nodes also cannot go *down* on a workstation, by design rather than by
accident, and the script says so instead of calling the teardown failed:
`deb` tears down with `apt-get remove`, which needs root and would uninstall
a system package salmon did not put there; and `directory` refuses a
non-empty directory, which the ssh key dir always is, because `Keys.sshKey`
deliberately "keeps keys around". The logs
(`gcp-toy-runs/<timestamp>/up-1.log`, `up-2.log`, `down.log`, plus `tree.txt`
and `directive.json`) hold the full `UpDown` report stream.

The script exits non-zero on any failure, retry, unexpected re-apply, or
leftover.

## Things that will bite

- **Org policies.** A freshly created organization enforces several policies by
  default. None of the tier 0/1 resources needs a service-account *key*, which
  is the usual casualty, but an unexpected `up` failure mentioning
  `constraints/...` is a policy, not a salmon bug.
- **`gcloud` prompts.** The script exports `CLOUDSDK_CORE_DISABLE_PROMPTS=1`; a
  prompt in a non-interactive `up` would otherwise hang forever.
- **Bucket names are global.** The toy scopes them by project id, so two
  sandboxes cannot collide, but a name you pick by hand can collide with the
  whole world.
- **Permissions.** Creating a project and linking billing need
  `resourcemanager.projects.create` on the parent and
  `billing.resourceAssociations.create` on the billing account (i.e.
  `roles/billing.user`, which only a billing administrator can grant — being
  able to *see* an account, or to create projects, does not imply it).
  `--existing-project` avoids both if you would rather have someone else create
  the project and attach billing.
- **A failed run is resumable.** Nothing is torn down when `up` fails twice, and
  re-running with the *same* `--project` reuses the project rather than burning
  a new id: its check reports `ACTIVE` and the node is skipped. Fix the cause
  (or the flag), re-run the same command line, or `run down < directive.json`
  to drop what was built.

- **`--workdir` is deleted by `down`.** The Containerfile and the podman
  authfile live there, and the directory node that holds them removes the
  directory on teardown — so point it at a scratch path, not at a directory
  with anything else in it. (Tier 1's first real run failed here: `podman
  logout` empties the authfile but leaves it, and the leftover file kept the
  directory from being removed. `Podman.login`'s `down` now deletes the file it
  caused to exist.)
- **Eventual consistency is real, and nodes now ride it out.** Two cases were
  found by running this toy: a create issued seconds after its API was enabled
  is denied for up to a minute (`PERMISSION_DENIED ... (or it may not exist)`,
  even for a project owner), and a binding naming a just-created service
  account is rejected by the service owning the resource. `Gcp.Core.retryingIO`
  is the shared remedy: bucket/repository/Cloud Run creates retry ~6×10s,
  `Iam.iamBinding` 5×3s, and `Iam.serviceAccount`'s `up` additionally waits for
  its own `describe` to answer. Retries show up in the logs as repeated
  `CommandStart` reports for one node, which is how to tell "needed the retry"
  from "worked first time".

- **Tier 2 needs a local glibc no newer than the VM's.** The binary is
  rsynced and run as-is, so the image family has to be at least as new as the
  machine running the toy. Ubuntu 24.04 (glibc 2.39) is the default because
  that is what this was developed against; an older image will fail at
  exec time with a version error, not at build time.
- **Tier 2's `--ssh-source-range` defaults to the whole internet.** A
  throwaway VM reachable on 22 by anybody, trusting only a certificate, is an
  acceptable risk for an hour; narrow it anyway when the project is not
  disposable.

- **Tier 3's proxy range must avoid `10.128.0.0/9`.** The `default` network is
  an *auto mode* VPC, and that whole block belongs to the subnets GCP creates
  per region on its own — including for regions that do not exist yet. Hence
  the `192.168.100.0/24` default. It also has to be `/26` or larger, and only
  one `ACTIVE` proxy-only subnet may exist per network per region, so a second
  concurrent tier-3 run in the *same* project (not the same organization) will
  collide.
- **Tier 3 builds and pushes an image, so it needs `podman` on the commanding
  machine** (tier 1 already does) and pulls `docker.io/library/nginx:alpine`
  there once. The VM pulls only from the toy's repository; what it fetches
  from elsewhere is the `podman` package, over the external address it has.
- **Tier 3 creates the instance as `<prefix>-sa`.** Whoever runs the toy needs
  `iam.serviceAccounts.actAs` on that account (a project owner has it), and
  the grant that lets the account read the repository is made minutes before
  the VM pulls: an IAM binding that has not propagated yet shows as a failed
  `podman-quadlet-image` on the VM (`denied`), which the script's retry pass
  is there for.
- **A tier-3 backend is `UNHEALTHY` for a minute or two after `up`.** The
  balancer answers `502` until the first health checks pass, which is why the
  script waits up to five minutes for the page rather than fetching once. If
  it never arrives, the script prints the backend's health, because the
  cause is nearly always a firewall rule rather than the balancer.

## Gaps this does not cover

- **The DNS zone has not been run against a real project yet.** The `gcloud
  dns managed-zones` argv and the reading of `describe --format json` are
  covered by Layer 0 tests, on a description written from the API's
  `ManagedZone` resource rather than captured; that the zone is created, its
  name servers printed, the second pass skips it and `down` removes it is
  what the first run with `--dns-zone` will show.
- **The peer has not been run against a real project yet.** The `gcloud`
  argv for a pinned internal address, a reserved internal address and an
  instance with no external address is covered by Layer 0 tests; that GCP
  accepts `--private-network-ip` on an address already reserved by a
  `compute addresses create --subnet … --addresses …` in the same pass, and
  that the VM then reaches the peer, is what the first run with
  `--vm-internal-ip`/`--peer-internal-ip` will show.
- **The secret delivery has not been run against a real VM yet.** The remote
  scripts are covered by Layer 0 tests that run them under a local `sh`, and
  the ssh command line by its rendering; `sudo -n sh -c` reading the secret
  from ssh's standard input on a real guest is what the first tier-2 run
  will show.
- **`Gcp.SecretManager.secretFile` is not exercised by the toy at all.** It is
  the on-machine transport (the instance reads Secret Manager as its own
  service account); the toy's VM runs as `<prefix>-sa` at tier 3 only, which
  is granted nothing on a secret, and only the `gcloud` argv and the local
  placement are tested.
- **Tier 3's container has not been run against a real project yet.** What
  was run: Layer 0 tests of both graphs (`Test/GcpToySpec.hs`: the same image
  reference on both sides, login before pull before the unit, the instance
  standing on the push, the account and the grant, tier 2 and the peer
  unchanged); podman 4.9.3's *system* generator in dry-run on the rendered
  `salmon-toy-page.container`, which it accepts; and the page's Containerfile
  built and run by hand with rootless podman, answering with the page. What
  the first tier-3 run will show: that `gcloud compute instances create
  --service-account ... --scopes cloud-platform` is accepted, that the
  metadata server's token logs podman in to Artifact Registry
  (`instanceLogin` has only ever met a stand-in), that the pull through that
  auth file works, that `quadletContainer` behaves as root under the system
  manager as it does in user scope (`/etc/containers/systemd`, the key-less
  label check, `daemon-reload` and restart), and that a second pass skips the
  login (its expiry stamp), the pull and the unit. The quadlet declares no
  `containerReady`, so `up` is done when the container exists; whether it
  serves is what the balancer's health check and the script's fetch say.
- **An instance's addresses are not checked for drift.** `gceInstance`'s
  check reads the instance's status only, so an instance that exists on
  another internal address than the declared one is reported satisfied.

- **The VM's own teardown is the project delete.** `down` removes the
  instance, the address and the firewall rule as declared nodes, but nothing
  checks that the guest was left in any particular state.
- **Host identity is trust-on-first-use, per recipe.** The user is
  authenticated by certificate, but the *host* is not: `VmProvision` keeps a
  known-hosts file next to the client key and accepts a new host on sight.
  Signing host certificates with the same CA would close that, and needs
  `ssh-keygen -s -h` support in `Keys` plus a way to get each VM's host key
  signed at boot.
- **The serverless-NEG backend is still unexercised.** Tier 3 drives
  `Gcp.LoadBalancing`'s `InstanceGroupBackend`; the `CloudRunBackend` branch
  (a serverless NEG in front of tier 1's Cloud Run service) renders but has
  never been run. Mixing the two in one balancer is not an option — a backend
  service holds one kind of backend — so exercising it means a second
  balancer.
- **HTTPS, host and path rules, several backend services, timeouts: declared
  by `--lb-https`, never run.** `Gcp.LoadBalancing` renders all of it and the
  toy declares it, but no `certificate-manager` command, no `url-maps import`,
  no `target-https-proxies` call and no `--address`-carrying forwarding rule
  has met a real project. The Layer 0 tests run the scripts against a
  stand-in `gcloud` written from the same assumptions, so they show the
  scripts' own logic (guards, order, what a second run does) and nothing
  about the API. Specifically unverified: that a regional DNS authorization
  takes `--type=PER_PROJECT_RECORD`; that `target-https-proxies create
  --region` accepts `--certificate-manager-certificates` by bare name; that
  `url-maps import` reads JSON on stdin and replaces an existing map under
  `--quiet`; how `value(hostRules[].hosts)`, `value(timeoutSec)`,
  `value(managed.state)` and the three-field `dnsResourceRecord` read print;
  that a reserved regional address in the default tier is accepted by an
  `EXTERNAL_MANAGED` forwarding rule; that one instance group can back
  two backend services of one balancer; and the named-port handling that
  makes that useful -- each backend service sends to a named port of its own
  (`--port-name`, its resource name), and a group's named ports are set once
  per group, merged with what `instance-groups get-named-ports
  --format='value(name,port)'` prints -- of which the two-column output and
  `value(portName)` are assumed, not recorded. "The HTTPS run" above is the
  run that settles these.
- **The balancer's check does not see path rules**, nor which service a host
  is sent to: only that each declared host is in the map, and each declared
  timeout on its service. A path rule changed behind salmon is put back by
  the next `up` that runs for another reason, not noticed.
- **No client-facing TLS policy, no self-managed certificate upload.**
  `ComputeCertificate` names a regional certificate somebody else made.
- **A backend bucket route on a regional balancer took every service host
  down, and is now refused.** Not run by the toy: observed on a downstream
  deployment's live regional external Application Load Balancer
  (europe-west1, 2026-10-05, 14:36 to 15:28 UTC). What was seen: with one
  path matcher on a regional backend bucket in the URL map (`albBuckets` and
  a host rule to `NamedBucket`), every matcher on a backend service answered
  503 (`failed_to_pick_backend`) while the backends were `HEALTHY`, and the
  bucket's own host served. What restored it: `url-maps import` of the same
  map minus that one host rule and its matcher; the six service hosts served
  again 80 seconds later. What is cleared: a matcher that only redirects,
  which stayed in the map. What it implies about the calls, on that
  deployment's account and with no output recorded here: a regional
  `backend-buckets` resource was created and a regional URL map accepted it,
  since the bucket's host served. What is not known: why (the service rules are
  rendered identically with and without the bucket, so it is not in what
  salmon writes about them; no cause is claimed), whether it happens on
  every regional balancer or needed something else this one had, and whether
  a *global* balancer mixes the two — this module does not make one and
  nobody here has run one. So `albProblems` refuses a rule routing to a
  backend bucket (every node's check a `Failure`, every `up` a throw, no
  call made), naming the above. An `albBuckets` entry no rule names is
  still accepted. For a balancer already in that state the repair is the
  declaration without the rule, and one pass: the map's check finds the
  host no rule declares and its `up` imports the map without it (at
  `2582343`, the commit that deployment ran, it did not: the check did not
  see a removed host). That path is tested against the stand-in `gcloud`
  only. The backend bucket dropped from the declaration as well is deleted
  after the import, if a pass ever marked it as the balancer's; one that
  never was is left:

  ```sh
  gcloud compute backend-buckets delete NAME-N-bucket --project P --region R
  ```

  `albBucketRoutes = AllowBucketRoutesKnownToHaveBrokenALiveBalancer` is
  there for whoever wants to show a mixed map working, on a balancer that
  serves nothing that matters.
- **A path rewrite on a path rule: not declared by the toy, never run.**
  `PathRule`'s `pathRuleRewrite = RewritePrefix P` is rendered as
  `routeAction: {urlRewrite: {pathPrefixRewrite: P}}` beside the rule's
  `service` in the imported map, from GCP's URL map reference. Unverified:
  that a *regional external* balancer's `url-maps import` accepts a
  `routeAction` on a `pathRules` entry that also names a `service`, and one
  that names a backend bucket; that an exact pattern (`/`) counts as wholly
  matched, so that `/` rewritten to `/index.html` asks the backend for
  `/index.html`; and that `/static/*` matches up to and including the last
  slash. The Layer 0 tests assert the JSON and the fingerprint, nothing
  about the API.
- **Redirects and the HTTP listener option (and what the scripts assume of
  backend buckets): not declared by the toy, never run by it.** A rule may
  answer with a redirect (`RedirectTo`) or, under the opt-in above, send to
  a backend bucket (`albBuckets`, `NamedBucket`), and `albHttp` says
  what port 80 does: by default what it always did (`:80` serves the same map
  as `:443`, through a proxy created once and never set again), or
  `ServeHttp`, `RedirectToHttps code`, `NoHttp`. All of it is exercised
  against the stand-in `gcloud` only. What the scripts assume and nobody
  recorded: that `compute backend-buckets create --region` with
  `--load-balancing-scheme=EXTERNAL_MANAGED` makes something a regional URL
  map accepts (the flags are in gcloud 573's `--help`; the deployment above
  had a bucket host serving, so it does, but nobody recorded the call), and
  that such a map names it as
  `.../regions/R/backendBuckets/NAME`; how `value(bucketName)` reads; that
  `url-maps import` takes `urlRedirect`/`defaultUrlRedirect` with
  `redirectResponseCode` as spelled, a path matcher with a redirect and no
  default service, and a map with nothing but `defaultUrlRedirect`; that
  `description` survives an import and reads back through
  `value(description)` (the check of a map with a bucket or a redirect
  requires it, so if it does not, that map reads as never in place; a map
  of backend services only is now written with one too, but its check only
  refuses a *different* `salmon:` description, so there a description that
  does not survive costs the detection of a host moved to another service
  and nothing else); that `value(hostRules[].hosts)` prints nothing for a
  map with no host rule and every host otherwise (the check now compares
  hosts as a set, and the rule-less map's `up` imports over a map for which
  it prints anything); that `url-maps import` accepts a map of `name` and
  `defaultService` alone and takes the rules off the one there; that a
  regional proxy prints its map under `value(urlMap)` as a URL ending in the
  map's name; that `target-http-proxies update --url-map` moves a proxy in
  service. On the GCP side, also unknown: whether a regional backend bucket
  serves `/` as `index.html` or a custom 404 page, and whether it answers
  `HEAD`. A throwaway balancer with one redirect and `RedirectToHttps`
  settles the rest; one with a bucket route as well (the opt-in) is the only
  way to learn more about the outage above.
- **Replacing a certificate on a live balancer has never met a real
  project.** A `DomainSetCertificate` is named after its domain set, so a
  changed set is a new certificate beside the old one; the HTTPS proxy is
  moved to it (`target-https-proxies update`) only once it is `ACTIVE`, and
  the superseded one is deleted after. While the new one is being issued a
  pass exits 0: the proxy's `up` prints `PENDING certificate swap` and leaves
  the proxy alone, the cleanup skips the certificate still served, and both
  checks read `Unknown`; only a `FAILED` or absent certificate fails the
  pass. So a release script cannot read "swap done" off the exit status, and
  a pass has to be run again (or `serve` left tending) after issuance. The
  records of the new authorizations belong on the `gcp-lb-dns-authorization`
  nodes, not on the balancer's root. All of that is exercised against the
  stand-in `gcloud` only, and the toy still declares a `ManagedCertificate`
  (fixed name, one host). What the scripts assume and nobody recorded: that
  a regional proxy lists Certificate Manager certificates under
  `sslCertificates` (by path, last segment the name); that `update` takes
  `--certificate-manager-certificates`; that `value(managed.domains)`,
  `value(managed.authorizationAttemptInfo[].state)` and `value(expireTime)`
  read as they are parsed; that `certificates list --format='value(name)'`
  ends each line with the certificate's name; that one DNS authorization can
  back two certificates at once (the swap relies on it for the names both
  sets cover); and that GCP refuses to delete a certificate a proxy still
  serves (the scripts look first, and do not rely on it). If
  `managed.domains` reads differently from what is assumed, a
  `ManagedCertificate` whose list never changed would be reported as
  covering other names: that is the first thing to look at on a real run.
- **The removal of what a balancer no longer declares has never met a real
  project**, and it deletes. The balancer's last node (`gcp-lb-leftovers`)
  removes a backend bucket, a Certificate Manager certificate, a DNS
  authorization or the redirect-only URL map that this balancer made and the
  declaration no longer names, once nothing uses it. "Made by this balancer"
  is a `description` of `salmon:lb:<balancer>` (for the redirect map, the
  fingerprint the module writes into it), and that node writes it on the
  declared resources that have no description. So **the first pass after an
  upgrade runs one `update --description` per declared backend bucket,
  managed certificate and DNS authorization, and deletes nothing**; a
  resource that left the declaration before it was marked is never removed,
  and is removed by hand:

  ```sh
  gcloud compute backend-buckets delete NAME-N-bucket --project P --region R
  gcloud certificate-manager certificates delete CERT --project P --location R
  gcloud certificate-manager dns-authorizations delete AUTHZ --project P --location R
  gcloud compute url-maps delete NAME-http-redirect-url-map --project P --region R
  ```

  (GCP refuses each while something still names it; the published `CNAME` of
  a deleted authorization is still in the zone.) A delete or a marking that
  fails fails that node and the pass, after every resource that serves
  traffic was applied. What the scripts assume and nobody recorded: that
  `compute backend-buckets update --region`, `certificate-manager
  certificates update` and `dns-authorizations update` take `--description`
  alone and change nothing else (if one does not, the first pass after the
  upgrade fails on that node, on every balancer that has such a resource:
  the first thing to look at); that the three `list
  --format='value(name,description)'` print the name (or a path ending in
  it), a tab and the description, and an empty description as nothing; that
  `compute backend-buckets list` without a region flag includes regional
  ones (each candidate is read again with `describe --region` before
  anything is concluded); that `compute url-maps list --format=json` holds
  every map of the project and names a regional backend bucket as
  `/regions/R/backendBuckets/NAME"`; that `target-https-proxies list
  --format='value(sslCertificates,certificateManagerCertificates)'` shows
  the certificates of every proxy by path, and `certificates list
  --format='value(managed.dnsAuthorizations)'` the authorizations in use;
  that `target-http-proxies list` and `target-https-proxies list` print
  `value(urlMap)` as a URL containing `/regions/R/urlMaps/NAME`; and that
  `certificate-manager ... list` simply fails, without prompting, in a
  project where the API is off. Not removed at all: anything at teardown, a
  backend service, a health check, a NEG, or the HTTPS proxy, rule and
  address of a balancer that stops declaring certificates.
- **The `--network`-carrying form of the forwarding rule** is still
  rendered-but-unrun.
- **Two credentials; only the one that acts is pinned.** `gcp-adc` validates
  ADC, which no node here uses: every `gcloud` call and
  `Core.printAccessToken` (the registry login password) act as the active
  account. `--account` (`Core.declaredAccount`) asserts who that is; nothing
  compares the ADC identity with it, and the assertion is made once per pass
  rather than passed as `--account` on each call, so an account switched
  mid-pass is not caught.
- **`CloudRun`'s check is a substring match** on the image, so `img:1` matches
  `img:10`, and a changed env var or service account is not noticed.
- **`CloudRunOptions`' scaling knobs have never met a real service.**
  `croMinInstances` (`--min-instances`) and `croCpuAlwaysAllocated`
  (`--no-cpu-throttling`) — what a service with a background loop needs to
  stay up and keep its CPU between requests — render, and the check compares
  them, when declared, against the template annotations
  `autoscaling.knative.dev/minScale` and `run.googleapis.com/cpu-throttling`.
  Those two names are from Cloud Run's documented service YAML, not from a
  `describe` the toy captured: if gcloud reports them otherwise, a service
  declaring either knob redeploys on every pass.
