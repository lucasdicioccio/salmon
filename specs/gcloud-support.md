# GCP Support Plan for Salmon

Status: phase 1 (gcloud-first) is implemented through all ten steps of §15:
`Salmon.Builtin.Nodes.Gcp.{Core,Storage,Compute,SshAccess,Iam,ArtifactRegistry,CloudRun,LoadBalancing}`
(instance-group and serverless-NEG backends), plus modules the plan did not
list (`ResourceManager`, `Billing`, `ServiceUsage`, `SecretManager`), the
recipes `SreBox.Gcp.{VmProvision,CloudRunDeploy,PostgrestCloudRun,PreviewEnvironment}`,
`salmon-gcp-toy` and `Test.GcpSpec`. Not done: phase 2 (direct REST calls),
the `SreBox.Gcp.WebStack` recipe of §2, CloudRun job executions, and a
configurable `gcloud` path (both deferred by §17). Kept as the design record.

This document proposes adding Google Cloud Platform (GCP) resource support to Salmon. The goal is to enable Salmon DAGs that turn up VMs, load balancers, Artifact Registry repositories, Cloud Storage buckets, and CloudRun services, while provisioning the VMs over SSH using an SSH-CA trust model.

The design stays within Salmon's existing patterns: resources are modelled as `Op` nodes, GCP tools are wrapped via `Salmon.Builtin.Nodes.Binary`, and VM provisioning reuses the existing `Self`, `Ssh`, `Keys`, and `Rsync` machinery.

---

## 1. Strategy: gcloud-first, API-second

For the same reason Salmon wraps `systemctl`, `psql`, `podman`, and `nft` rather than linking their native APIs, the fastest path is to model GCP resources as nodes that shell out to `gcloud`.

| Phase | Approach |
|-------|----------|
| **Phase 1** | Wrap `gcloud` CLI commands via `Binary` nodes. |
| **Phase 2** | Replace hot or latency-sensitive paths (VM status polling, operation waiting) with direct REST calls via `http-client` if needed. |
| **Auth** | Use Application Default Credentials (ADC). A single `Gcp.Core` validation node runs before any resource node. |

This gives Salmon's existing execution model — sequential, concurrent, and supervised (`run serve`) drivers — for free.

---

## 2. Package Layout

### New modules under `salmon-ops/src/Salmon/Builtin/Nodes/Gcp/`

```text
```text
salmon-ops/src/Salmon/Builtin/Nodes/Gcp/
  Core.hs              -- project, zone, region, auth, common CLI wrappers
  Compute.hs           -- GCE instances, instance groups, templates
  LoadBalancing.hs     -- backend services, URL maps, forwarding rules, health checks, NEGs
  ArtifactRegistry.hs  -- repositories, IAM, docker/podman auth
  Storage.hs           -- GCS buckets, IAM, lifecycle
  Iam.hs               -- service accounts, role bindings
  CloudRun.hs          -- CloudRun services and revisions
  SshAccess.hs         -- OS Login / metadata keys / SSH-CA glue
```

### New recipes under `salmon-ops-recipes/src/SreBox/Gcp/`

```text
  WebStack.hs          -- VM + LB + bucket + CloudRun service
  VmProvision.hs       -- network + vm + ssh access + self-provisioning
  WebStack.hs          -- VM + LB + bucket + CloudRun job
```

---

## 3. Shared Core: `Gcp.Core`

```haskell
module Salmon.Builtin.Nodes.Gcp.Core where

data Project = Project { projectId :: Text }
  deriving (Eq, Ord, Show)

data Zone = Zone { zoneName :: Text }
  deriving (Eq, Ord, Show)

data Region = Region { regionName :: Text }
  deriving (Eq, Ord, Show)

data GcpError = GcpCliError Text Int Text
  deriving (Exception, Show)

-- | Track for the gcloud binary.
gcloud :: Track' (Binary "gcloud")

-- | Validates Application Default Credentials.
-- Almost every other GCP op depends on this.
applicationDefaultCredentials :: Op
```

The ADC node should run `gcloud auth application-default print-access-token` and succeed only if a token is returned. This catches misconfigured environments before any resource operation.

---

## 4. Compute Engine VMs: `Gcp.Compute`

### Types

```haskell
data MachineType
  = E2Medium
  | E2Standard2
  | N2Standard4
  | Custom Text
  deriving (Eq, Show)

data BootDisk = BootDisk
  { bootDiskSizeGb :: Int
  , bootDiskImage  :: Text
  }
  deriving (Eq, Show)

data Instance = Instance
  { instanceName         :: Text
  , instanceProject      :: Project
  , instanceZone         :: Zone
  , instanceMachineType  :: MachineType
  , instanceBootDisk     :: BootDisk
  , instanceNetwork      :: Text
  , instanceSubnet       :: Text
  , instanceServiceAccount :: Maybe Text
  , instanceMetadata     :: Map Text Text
  , instanceTags         :: [Text]
  }
  deriving (Eq, Show)
```

### Op

```haskell
instance :: Reporter Report -> Track' (Binary "gcloud") -> Instance -> Op
```

- **`up`**: `gcloud compute instances create ...`
- **`down`**: `gcloud compute instances delete --quiet ...`
- **`check`**: `gcloud compute instances describe --format='value(status)'`
  - `RUNNING` → `Success`
  - `TERMINATED` or absent → `Failure`
  - `PROVISIONING`, `STAGING`, `STOPPING` → `Unknown`
  - *SSH readiness is checked separately by `Gcp.SshAccess.sshAvailable`, not by the VM node itself.*

### External networking

Keep VMs private by default. If outbound internet is required, depend on an existing Cloud NAT or add a `Gcp.Compute.CloudNat` node. For provisioning access, use IAP tunneling or the SSH access model described below.


### SSH availability check

Because the VM node only reports GCE-level `RUNNING`, add a separate support node for verifying that SSH is actually reachable. This is especially useful before `Self.uploadAndCallSelf` runs.

```haskell
data SshEndpoint = SshEndpoint
  { sshHost     :: Text
  , sshPort     :: Int
  , sshIdentity :: FilePath
  }

sshAvailable :: Reporter Report -> SshEndpoint -> Op
```

- **`up`**: no-op (or a connect probe)
- **`check`**: `ssh -o ConnectTimeout=5 -o BatchMode=yes -i <identity> <host> true` exits 0 → `Success`; else `Failure`
- **`down`**: no-op

Recipes that provision a VM should depend on `sshAvailable` before invoking `Self.uploadAndCallSelf`.

---

## 5. SSH Access Model

The objective is to create a VM and then SSH into it to run a Salmon binary for local provisioning. We prefer SSH-CA over long-lived per-instance keys.

### Option A: OS Login with SSH-CA (preferred long-term)

1. Enable OS Login at project or organization level.
2. Upload the SSH CA public key to Google Cloud Identity via the OS Login API.
3. Salmon signs short-lived user certificates using existing `Keys` primitives.
4. Instances get metadata `enable-oslogin=TRUE`.
5. Users connect with signed certificates; Google validates the CA.

```haskell
data OsLoginConfig = OsLoginConfig
  { osLoginProject     :: Project
  , osLoginCaPublicKey :: FilePath
  }

enableOsLogin      :: OsLoginConfig -> Op
uploadOsLoginCaKey :: Text -> FilePath -> Op
```

**Caveat**: uploading a CA to Cloud Identity requires domain-wide delegation or admin credentials. This is cleanest if you control the Google Workspace / Cloud Identity domain.

### Option B: Project metadata SSH keys + Salmon CA (recommended starting point)

1. Salmon generates an SSH CA key pair (`Keys.SSHKeyPair`).
2. Salmon injects the CA **public** key into project or instance metadata.
3. Instances trust that CA.
4. Salmon signs user/host certificates as needed.

```haskell
data MetadataSshCa = MetadataSshCa
  { sshCaProject   :: Project
  , sshCaPublicKey :: FilePath
  }

installMetadataCaKey :: MetadataSshCa -> Op
```

This avoids Cloud Identity Admin SDK complexity and is self-contained within a project.

### Option C: OS Login with Google-managed keys

Use `gcloud compute ssh`. This is the simplest but loses the SSH-CA model.

### Recommendation

Start with **Option B** (metadata SSH-CA) for a self-contained Salmon setup. Migrate to **Option A** once organization-wide trust and Cloud Identity admin automation are in place.

---

## 6. VM Self-Provisioning

Once SSH access works, reuse Salmon's existing remote-provisioning machinery:

- `Salmon.Builtin.Nodes.Self.uploadAndCallSelf`
- `Salmon.Builtin.Nodes.Self.uploadAndCallSelfAsSudo`
- `Salmon.Builtin.Nodes.Ssh.preExistingRemoteMachine`
- `Salmon.Builtin.Nodes.Rsync.sendFile`

A recipe composes the pieces:

```haskell
module SreBox.Gcp.VmProvision where

provisionedVm ::
  Reporter Report ->
  Instance ->
  Self.SelfPath ->
  Ssh.Remote ->
  Track' directive ->
  directive ->
  Op
provisionedVm r inst selfpath remote runRemote spec =
  let vm        = Gcp.Compute.instance r gcloud inst
      sshAccess = Gcp.SshAccess.metadataCaAccess r gcloud inst
      provision = Self.uploadAndCallSelfAsSudo
                    ... selfpath remote Ssh.preExistingRemoteMachine runRemote CLI.Up spec
  in
    provision
    `inject` sshAccess
    `inject` vm
```

This mirrors the existing `SreBox.PostgresMigrations.remoteMigrateOpaqueSetup` pattern.

---

## 7. Load Balancers: `Gcp.LoadBalancing`

GCP L7 load balancers consist of many small resources. Expose a single high-level recipe rather than forcing users to wire every component manually.

```haskell
data Backend
  = InstanceGroupBackend InstanceGroup [Int]        -- ports for named ports
  | CloudRunBackend Text                            -- CloudRun service name
  deriving (Eq, Show)

data ApplicationLoadBalancer = ApplicationLoadBalancer
  { albName        :: Text
  , albProject     :: Project
  , albRegion      :: Region
  , albNetwork     :: Maybe Text                    -- required for instance groups; optional for serverless
  , albBackends    :: [Backend]
  , albHealthCheck :: Maybe HealthCheck             -- required for instance groups; omitted for CloudRun
  }

applicationLoadBalancer :: Reporter Report -> ApplicationLoadBalancer -> Op
```

Internally creates:

1. Health check (for instance-group backends only)
2. Instance group + named ports, **or** serverless NEG for each CloudRun backend
3. Backend service (one per backend, or a single service with multiple backends)
4. URL map
5. HTTP(S) target proxy
6. SSL certificate (managed or self-provided)
7. Forwarding rule + external/global IP

**Check**: verify every sub-resource exists and the backend service reports healthy backends (for instance groups) or that the NEG points to a deployed CloudRun service.

The recipe must reject mixing instance-group and CloudRun backends in a single backend service if GCP does not allow it; otherwise create separate backend services and route by URL map path rules.

---

## 8. Artifact Registry: `Gcp.ArtifactRegistry`

```haskell
data RepoFormat = Docker | Maven | Npm | Python | Apt | Yum

data ArtifactRepo = ArtifactRepo
  { repoName     :: Text
  , repoProject  :: Project
  , repoLocation :: Region
  , repoFormat   :: RepoFormat
  }

artifactRepository :: ArtifactRepo -> Op
```

- **`up`**: `gcloud artifacts repositories create ...`
- **`check`**: `gcloud artifacts repositories describe ...`
- IAM nodes grant `roles/artifactregistry.reader` / `writer`.

Then reuse existing `Salmon.Builtin.Nodes.Podman` with image names like:

```text
{region}-docker.pkg.dev/{project}/{repo}/{image}:{tag}
```

Add a helper to configure local auth:

```haskell
configureDockerAuth :: Project -> Region -> Op
-- runs: gcloud auth configure-docker {region}-docker.pkg.dev
```

---

## 9. Cloud Storage: `Gcp.Storage`

```haskell
data Bucket = Bucket
  { bucketName   :: Text
  , bucketProject :: Project
  , bucketLocation :: Region
  , bucketUniformBucketLevelAccess :: Bool
  }

bucket :: Bucket -> Op
```

- **`up`**: `gcloud storage buckets create gs://... --location=...`
- **`check`**: `gcloud storage buckets describe gs://...`
- IAM nodes for access grants.
- Optional lifecycle and CORS nodes if needed.

Use a pre-check before create to make `up` idempotent.

---

## 10. IAM: `Gcp.Iam`

A generic IAM node is useful for creating and attaching principals to resources. It can create service accounts and grant roles on projects or individual resources.

```haskell
data Principal
  = ServiceAccount Text              -- account ID
  | User Text
  | Group Text

data IamBinding = IamBinding
  { iamPrincipal :: Principal
  , iamRole      :: Text              -- e.g. "roles/storage.objectViewer"
  , iamResource  :: Text              -- project ID or resource URI
  }

serviceAccount :: Project -> Text -> Op       -- create service account
iamBinding     :: IamBinding -> Op            -- grant role on resource
```

- **`serviceAccount up`**: `gcloud iam service-accounts create <id> --project=<project>`
- **`serviceAccount check`**: `gcloud iam service-accounts describe <id>@<project>.iam.gserviceaccount.com`
- **`iamBinding up`**: `gcloud <resource-type> add-iam-policy-binding <resource> --member=<member> --role=<role>`
- **`iamBinding check`**: `gcloud <resource-type> get-iam-policy <resource>` and verify the binding exists
- **`down`**: remove the binding or delete the service account

Use this for granting CloudRun service accounts access to buckets, Artifact Registry, and secrets.

---

## 11. CloudRun Services: `Gcp.CloudRun`

For now we model **CloudRun services and revisions**, not job executions. The image comes from the Artifact Registry repository provisioned earlier, so a CloudRun service node depends on both the repository and a pushed image.

```haskell
data CloudRunService = CloudRunService
  { crsName           :: Text
  , crsProject        :: Project
  , crsRegion         :: Region
  , crsImage          :: Text           -- full Artifact Registry URL
  , crsEnv            :: Map Text Text
  , crsServiceAccount :: Text
  , crsIngress        :: IngressSetting
  , crsMaxInstances   :: Maybe Int
  }

data IngressSetting = All | Internal | InternalAndLoadBalancing

cloudRunService :: CloudRunService -> Op
```

- **`up`**: `gcloud run deploy <name> --image=<image> --region=<region> ...`
- **`check`**: `gcloud run services describe <name> --region=<region>` and verify the active revision points at `crsImage`.
- **`down`**: `gcloud run services delete <name> --region=<region> --quiet`

**Image lifecycle**: a CloudRun service node does not build or push images. It depends on an upstream node that pushes a Podman-built image to Artifact Registry. The image URL is part of the service spec, so changing the image triggers a new revision on `up`.

**Job executions**: deferred to a later phase. When needed, add `cloudRunJob` and `cloudRunJobExec` nodes separately.

---

## 12. Example DAG: Tenant Stack

```haskell
tenantStack :: TenantId -> Op
tenantStack tenant =
  let bkt    = Gcp.Storage.bucket    (tenantBucket tenant)
      repo   = Gcp.ArtifactRegistry.artifactRepository (tenantRepo tenant)
      vm     = Gcp.Compute.instance    ... (tenantVm tenant)
      sshCa  = Gcp.SshAccess.metadataCaAccess ... tenant
      remote = Ssh.Remote "salmon"   (vmIp tenant)
      provision = provisionedVm ... vm sshCa remote tenantSpec
  in
    provision
    `inject` repo
    `inject` bkt
```

Dependencies ensure the bucket and repository exist before the VM starts, and SSH access is in place before the remote Salmon binary runs.

---

## 13. Idempotency and Check Mapping

| Resource | Check command | Result mapping |
|----------|---------------|----------------|
| ADC | `gcloud auth application-default print-access-token` | exit 0 → `Success`; else `Failure` |
| VM | `gcloud compute instances describe --format=value(status)` | `RUNNING` → `Success`; `TERMINATED`/absent → `Failure`; transitional → `Unknown` |
| SSH available | `ssh -o ConnectTimeout=5 -o BatchMode=yes ... true` | exit 0 → `Success`; else `Failure` |
| OS Login CA | list user's SSH keys and compare CA fingerprint | match → `Success`; else `Failure` |
| Metadata CA | `gcloud compute project-info describe` metadata | key present → `Success`; else `Failure` |
| Load balancer | describe all sub-resources | all present and healthy → `Success`; else `Failure` or `Unknown` |
| Artifact Registry | `gcloud artifacts repositories describe` | exists → `Success`; else `Failure` |
| GCS bucket | `gcloud storage buckets describe gs://...` | exists → `Success`; else `Failure` |
| IAM binding | `gcloud <resource> get-iam-policy <name>` | binding present → `Success`; else `Failure` |
| CloudRun service | `gcloud run services describe` | exists and active revision matches image → `Success`; else `Failure` |

For resources with an `update` operation, prefer `describe → update-or-create` so that `up` remains idempotent even when parameters change.

---

## 14. Secrets Handling

Use existing `Salmon.Builtin.Nodes.Secrets` for:

- SSH CA private keys.
- Service account JSON files, if ADC cannot be used.

Never commit service account keys into directives. Pass them as file paths generated by `Secrets.sharedSecretFile`.

---

## 15. Implementation Order

1. **`Gcp.Core`** + ADC validation node.
2. **`Gcp.Storage.bucket`** — simplest resource; good proving ground.
3. **`Gcp.Compute.instance`** + status-based `check`.
4. **`Gcp.SshAccess`** — choose metadata CA or OS Login and implement key injection.
5. **`Gcp.SshAccess.sshAvailable`** — verify SSH is reachable before remote provisioning.
6. **`SreBox.Gcp.VmProvision`** — VM + SSH + `Self.uploadAndCallSelf`.
7. **`Gcp.Iam`** — service accounts and role bindings.
8. **`Gcp.ArtifactRegistry`** repository + docker auth.
9. **`Gcp.CloudRun`** services and revisions from pushed images.
10. **`Gcp.LoadBalancing`** recipe last, because it has the most moving parts and depends on VM/instance-group or CloudRun nodes.

---

## 16. Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| **Async operations** | Poll inside `up` for `RUNNING` / ready state, or model intermediate states as explicit nodes. |
| **API rate limits** | Salmon's concurrent driver is unbounded; add dependency edges between GCP ops or introduce a rewrite that batches `gcloud` calls. |
| **Output fragility** | Prefer `--format=value(...)` or JSON output parsed with Aeson over human-readable text. |
| **IAM propagation delay** | Retry on auth failures; do not trust `check` alone in the first seconds after a grant. |
| **gcloud not installed** | Add a `Binary "gcloud"` provider node that fails early with a clear message. |
| **Network access to private VMs** | Use IAP tunneling, a bastion, or OS Login with Identity-Aware Proxy. |

---

## 17. Decisions

1. **VM `check`**: reports only GCE-level `RUNNING` status. SSH readiness is verified by a separate `Gcp.SshAccess.sshAvailable` support node.
2. **Load balancer backends**: support both GCE instance groups and serverless NEGs for CloudRun.
3. **CloudRun**: model services and revisions first, deployed from Podman images pushed to Artifact Registry. Job executions are deferred to a later phase.
4. **IAM**: add a generic `Gcp.Iam` module for creating service accounts and granting roles on projects or resources.
5. **gcloud binary**: support `gcloud` on `PATH` only for now. Configurable binary paths are deferred.
