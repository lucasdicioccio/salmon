# Validating the GCP builtins against a real project

This is the playbook for running salmon's `Salmon.Builtin.Nodes.Gcp.*` builtins
against real GCP, in a sandbox you are willing to destroy. It exists because the
GCP nodes' automated tests are all Layer 0: they check the *pure* verdict
functions (`interpretInstanceStatus`, `interpretBillingDescribe`, …) and the
rendered `gcloud` argument lists. Nothing in `cabal test` ever talks to Google,
so "does an `up` actually converge, and is it idempotent, and does `down` take
it all away" is a question only a real project can answer.

Two pieces do that:

- `salmon-apps`'s **`salmon-gcp-toy`** binary (`salmon-apps/src/GcpToy.hs`), a
  tiered, throwaway stack built out of the Gcp builtins, driven through the
  usual seed → directive → ops protocol.
- **`salmon-apps/scripts/gcp-toy-validate.sh`**, which runs that binary through
  `up` → `up` → `down` and reports what each pass did.

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
| `Gcp.ResourceManager.project` | the project itself (skipped with `--existing-project`) |
| `Gcp.Billing.linkBillingAccount` | links the billing account |
| `Gcp.ServiceUsage.enableService` | `storage`, `iam`, `artifactregistry` (and `run` at tier 1) |
| `Gcp.Storage.bucket` | `<project>-<prefix>` , uniform bucket-level access |
| `Gcp.Iam.serviceAccount` | `<prefix>-sa@<project>.iam.gserviceaccount.com` |
| `Gcp.ArtifactRegistry.artifactRepository` | `<prefix>-repo`, docker format |
| `Gcp.Iam.iamBinding` ×2 | `storage.objectViewer` on the bucket, `artifactregistry.reader` on the repo |

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

**Tier 2** (an `e2-micro`'s hourly rate, plus a reserved IP) is
`specs/gcloud-support.md` §6's "objective": a VM salmon boots, trusts and
then provisions with *this same binary*.

| Node | Resource |
|---|---|
| `Gcp.Compute.address` | a reserved regional external IP, `<prefix>-ip` |
| `Gcp.Compute.firewallRule` | `tcp:22` from `--ssh-source-range` to instances tagged `<prefix>-ssh` |
| `Filesystem.filecontents` | the startup script, passed as `--metadata-from-file` |
| `Keys.sshKey` ×2, `Keys.signKey` | a CA and a client key, and a certificate for `--vm-user` |
| `Gcp.SshAccess.installMetadataCaKey` | the CA's public key, into project metadata |
| `Gcp.Compute.gceInstance` | the VM, claiming the address and carrying the tag |
| `Gcp.SshAccess.sshAvailable` | waits for sshd to answer *as that user, with that certificate* |
| `Self.uploadAndCallSelfAsSudoWithIdentity` | rsyncs this binary over and runs `run up` on it there |

The startup script is what closes the gap `installMetadataCaKey` leaves:
nothing on a GCE instance reads that metadata key by itself. It fetches the
CA from the metadata server into `/etc/ssh/salmon_ca.pub`, points sshd's
`TrustedUserCAKeys` at it, creates the login user the certificate names as
its principal (with no OS Login, a principal must be a local account), gives
it passwordless sudo, and makes sure `rsync` is there for the upload.

**Tier 2 runs in two passes, and the script drives both.** GCP picks the
address, so the first pass reserves it and stops; the driver then reads the
IP (`Compute.readAddress`) and re-issues the directive with `--vm-ip`, and
the second pass declares the same graph plus the provisioning step. That is
not a wart of the toy: an `Op` naming the host has to be built before any
`up` runs, so *something* outside the graph has to carry the address across.

What proves it worked is the file the uploaded binary writes on the VM,
`/var/lib/salmon-toy/provisioned`; the script reads it back over ssh. The
binary runs there with the same directive, tagged `OnVm`, which is why the
payload is declared in the same `Track'` as everything else.

**Tier 3** (a forwarding rule's hourly rate on top of tier 2) puts a
*regional external* Application Load Balancer in front of that VM.

| Node | Resource |
|---|---|
| `Gcp.Compute.subnet` | the proxy-only subnet, `<prefix>-proxy`, `REGIONAL_MANAGED_PROXY`/`ACTIVE` |
| `Gcp.Compute.instanceGroup` | an unmanaged, zonal group, `<prefix>-ig` |
| `Gcp.Compute.instanceGroupMember` | the tier-2 VM, put in it |
| `Gcp.Compute.firewallRule` | `tcp:<--lb-port>` from the proxy range **and** the health-check ranges, to instances tagged `<prefix>-lb` |
| `Gcp.LoadBalancing.applicationLoadBalancer` | health check, backend service, named ports, URL map, target proxy, forwarding rule |
| `Systemd.systemdService` (on the VM) | `salmon-toy-web.service`, a `python3 -m http.server` over a page salmon wrote |

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
(`192.168.100.0/24`) and `--lb-port` (`8080`).

Script options, before the `--`: `-y` skips the confirmation prompt, `--keep`
skips teardown (it then prints the `run down` command to finish up later).
Everything after the `--` goes verbatim to `salmon-gcp-toy config`. `OUT=<dir>`
and `SALMON_GCP_TOY=<binary>` override the log directory and the binary.

**Use a fresh project id every run.** A deleted project sits in
`DELETE_REQUESTED` for ~30 days and nobody can reuse its id in that window —
`$(date +%s)` in the id is there for exactly this. `Gcp.ResourceManager`'s check
reports that state with its own message rather than as a plain "not found",
because the `up` that follows is going to fail on the id, not on credentials.

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
- **A tier-3 backend is `UNHEALTHY` for a minute or two after `up`.** The
  balancer answers `502` until the first health checks pass, which is why the
  script waits up to five minutes for the page rather than fetching once. If
  it never arrives, the script prints the backend's health, because the
  cause is nearly always a firewall rule rather than the balancer.

## Gaps this does not cover

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
- **HTTPS, and anything past the default route.** The URL map has one default
  service and the forwarding rule is plain `:80`; managed certificates, host
  and path rules, and the `--network`-carrying form of the forwarding rule are
  all rendered-but-unrun.
- **Two credentials, one identity assumed.** `gcp-adc` validates ADC;
  `Core.printAccessToken` (the registry login password) uses the active account.
  Log both in as the same identity.
- **`CloudRun`'s check is a substring match** on the image, so `img:1` matches
  `img:10`, and a changed env var or service account is not noticed.
