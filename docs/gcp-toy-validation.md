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

There is no VM tier yet — see [Gaps](#gaps-this-does-not-cover) below.

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
`--containerfile ./myapp/Containerfile` to deploy your own app.

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

Nodes that legitimately have no `check` today (`gcloud`, the `gcp-toy` root,
`podman-build`/`login`/`push`, `directory`) re-apply on every pass; the script
lists them separately rather than counting them as findings. The logs
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

## Gaps this does not cover

- **VMs and SSH.** `Gcp.Compute` and `SreBox.Gcp.VmProvision` are not exercised.
  `SshAccess.installMetadataCaKey` writes an `ssh-ca` project-metadata key, but
  nothing on the instance reads it: trusting the CA needs a startup script
  writing sshd's `TrustedUserCAKeys`. A VM tier also needs a static address (the
  recipe takes the SSH host as an input, and an ephemeral address is not known
  until after `up`), a firewall rule for port 22, and a self binary that runs on
  the guest's libc.
- **Load balancing.** `Gcp.LoadBalancing` renders a regional external ALB, and
  GCP only accepts one in a network that already has a proxy-only subnet, which
  nothing here creates. Unexercised.
- **Two credentials, one identity assumed.** `gcp-adc` validates ADC;
  `Core.printAccessToken` (the registry login password) uses the active account.
  Log both in as the same identity.
- **`CloudRun`'s check is a substring match** on the image, so `img:1` matches
  `img:10`, and a changed env var or service account is not noticed.
