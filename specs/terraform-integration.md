# Terraform integration: consuming (and optionally driving) existing Terraform-managed infra

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem

Salmon's own job stops at "given a machine that already exists, converge it
to a declared state" — nothing in `salmon-core`/`salmon-ops` creates
compute/network/DNS-zone resources at a cloud provider, and nothing in the
repo talks to Terraform today (confirmed: no `terraform`/`.tf`/HCL
references anywhere in `salmon-*`). Meanwhile there may already be
Terraform state describing the machines/networks/DNS zones this project
needs to target — the [[pg-ha control plane]] spec's "Machines" leaf in
particular ("control-plane seeds must be unfoldable to Machines...") has to
come from *somewhere*, and if that somewhere is already Terraform, salmon
shouldn't duplicate or fight it.

## What "integration" could mean — two different things

It's worth naming these as separate models up front, because they have very
different blast radii and this spec recommends starting with only one of
them.

**Model A — read-only consumption.** Terraform (run by hand, by CI, by
whatever already runs it) is the source of truth for resource existence;
salmon only ever *reads* its outputs (machine IPs, DNS zone ids, generated
credentials Terraform provisioned) to build seeds/directives. Salmon never
calls `terraform apply`/`destroy`.

**Model B — salmon-driven apply.** Salmon shells out to `terraform` itself,
treating a Terraform root module as one more `Op` in the graph (`up` runs
`apply`, `down` runs `destroy`), so a single `salmon-x run up` can bring up
both the Terraform-managed layer and everything salmon layers on top in one
traversal.

Recommendation: **build Model A first, treat Model B as optional/future**.
Model A composes with "we may have already-existing terraform usage"
directly — it's non-invasive by construction, since salmon never mutates
anything Terraform owns. Model B requires deciding who owns lifecycle
(salmon `down` calling `terraform destroy` against existing hand-managed
infra is a real footgun — see §3's caveats) and isn't needed to unblock the
pg-ha control-plane work, which only needs machine inventory as an input.

## Design goals / non-goals

Goals:
- No new core (`salmon-core`) mechanism — reuse the same insight as the
  previous-seed-state design in `specs/pg-ha-control-plane.md`:
  `Configure`'s `gen :: seed -> m a` is already impure, so "read Terraform
  outputs" is just another impure read at seed-to-directive time, exactly
  like reading a previous-state JSON file.
- Salmon never becomes a Terraform state owner. It reads `terraform output
  -json` (or, for Model B, drives `apply`/`destroy` against a root module
  it's pointed at) — it does not generate, template, or hand-edit `.tf`
  files. Whoever wrote the existing Terraform config keeps owning it.
- Salmon never manages cloud credentials itself. Both models assume
  `terraform`/its providers are already configured to run ambiently
  (env vars, an assumed role, a configured backend) exactly as if a human
  ran `terraform apply` at that path — salmon just shells out to the
  `terraform` binary the same way `Binary.withBinary` already wraps every
  other external tool in this codebase.

Non-goals (v1):
- Generating/templating Terraform HCL from salmon seeds (the inverse
  direction — "salmon describes infra, emits `.tf`"). A materially
  different, much larger feature; not needed to consume existing usage.
- Remote state backend management (S3 bucket + DynamoDB lock table, TFC
  workspace creation, etc.) — assumed to already exist if it exists at all.
- Import of unmanaged resources into Terraform state — out of scope; if
  something needs importing, that's a one-time `terraform import` a human
  runs, upstream of anything salmon touches.

## Proposed design

### 1. Reading outputs into a seed (Model A)

A small new module, e.g. `SreBox.TerraformState` (recipes-level, not
core — this is derivation logic, not an IO primitive with its own `up`/
`down`):

```haskell
-- shells out to `terraform output -json`, or reads a cached copy of that
-- JSON from a file (useful for offline/repeatable seed generation, and
-- avoids requiring the `terraform` binary + provider credentials just to
-- build a directive)
readOutputs :: FilePath -> IO (Either String Aeson.Value)  -- workdir, or a captured -json file
```

A seed carries either a working directory (to shell out live) or a path to
a previously-captured `terraform output -json` file — same "carry a
`Maybe FilePath`" shape the previous-seed-state design already established:

```haskell
data TerraformSource
    = LiveWorkdir FilePath (Maybe Text)   -- terraform root dir, optional workspace name
    | CapturedOutputs FilePath             -- a `terraform output -json > file` snapshot
```

`gen` then parses the specific outputs a given recipe needs (e.g.
`machine_ab_0_ip`, `machine_ab_1_ip`, `dns_zone_id`) out of the JSON into
strongly-typed seed fields, the same way any other `Configure` step turns
loosely-typed input into a typed directive — failure to find an expected
output key is a `gen`-time error (surfaced before any `Op` runs), not
something recipes downstream ever need to handle.

This directly answers the pg-ha control-plane spec's open question of
"where do machine addresses come from": `ControlPlaneSeed`'s machine
fields become optional-override-else-read-from-Terraform, i.e. the seed
carries a `TerraformSource` and the two `pair_machine_a`/`pair_machine_b`
addresses are populated from it during `gen`, rather than being typed in by
hand every time.

### 2. Recommended workflow shape

```sh
# Terraform, run however it already is (CI, human, whatever) — salmon doesn't touch this
terraform -chdir=infra/prod apply

# capture outputs once (or every time before a converge, cheap either way)
terraform -chdir=infra/prod output -json > tf-outputs.json

# salmon reads the capture, same two-phase protocol as everything else
salmon-x config --terraform-outputs tf-outputs.json ... | salmon-x run up
```

This keeps the existing hermetic seed→directive→ops boundary completely
intact (per CLAUDE.md's description of why that split exists) — Terraform
becomes just one more impure input `gen` reads, alongside the previous-
state file from the pg-ha spec. Nothing about `Actions/Serve.hs`'s `World`/
`Epoch` model needs to change either: a `serve` loop re-reads whatever
`tf-outputs.json` currently says on every seed declaration, same as it
would re-read any other input file.

### 3. Optional: `Salmon.Builtin.Nodes.Terraform` as an `Op` (Model B)

If/when salmon-driven apply is actually wanted, it fits the existing node
shape cleanly — unlike `nft`/`ip link`, `terraform apply` *is* naturally
idempotent (a no-op plan applies as a no-op), so this is a "prefer
replace"-bucket node per CLAUDE.md's conventions, not a `prelim`-skip one:

```haskell
data TerraformRoot
    = TerraformRoot
    { tf_workdir :: FilePath
    , tf_workspace :: Maybe Text
    , tf_var_file :: Maybe FilePath
    }

apply :: Reporter Report -> Track' (Binary "terraform") -> TerraformRoot -> Op
```

`up`: `terraform -chdir=<workdir> [workspace select <ws>] apply -auto-approve
[-var-file=<f>]`, via the ordinary `withBinary`/`untrackedExec` path (which
already throws on non-zero exit, satisfying CLAUDE.md's "failure must not
be swallowed" convention for free — no special handling needed here).

`down`: `terraform -chdir=<workdir> destroy -auto-approve` — **deliberately
not wired to run automatically from a generic `downTree` walk in v1.**
`destroy` against a root module that predates salmon's involvement, or that
other tooling/humans also touch, is exactly the kind of hard-to-reverse,
shared-state action this project's own operating conventions (see the
"Executing actions with care" guidance salmon is developed under) say needs
an explicit, deliberate trigger — not something that happens as a side
effect of some unrelated node's teardown pulling in a shared predecessor.
Concretely: expose `apply` as an `Op` usable in `up`/`tree`/`dag`, but ship
its `down` as `Actionless`/no-op by default, with a separate, explicitly-
named `destroy` value (not wired into the same `Op`) that a caller has to
opt into deliberately if they really want `salmon-x run down` to reach it.

### 4. Reading Terraform-generated secrets

Terraform providers commonly generate credentials (a random DB password, a
provider-issued API key) as sensitive outputs. Treat these exactly like
every other secret in this codebase per the existing convention ([[recipe
key exchange agnostic]]): `readOutputs` surfaces them as plain values at
`gen` time (already local, already trusted — no new transport invented),
and downstream recipes take them as ordinary pre-provisioned secret values,
same as they'd take a secret read from any other file today. No new secret-
handling machinery needed.

## Open questions

- **Does "already-existing Terraform usage" mean one root module or
  several** (e.g. separate network/compute/DNS roots, possibly separate
  workspaces per tier/environment)? Determines whether `TerraformSource`
  needs to be a list (read/merge outputs from multiple roots) rather than
  one workdir — leaning towards supporting a list from the start since
  "one big root module" vs "several small ones" is a common enough split
  not to special-case away.
- **Output naming contract**: does salmon assume specific output names
  (`machine_ab_0_ip`, etc.) that the existing `.tf` files would need to
  expose (possibly requiring someone to add outputs to already-existing
  config), or does `gen` need a mapping/config layer between "whatever
  outputs already exist" and "what the seed needs"? Depends entirely on
  what the existing Terraform code currently outputs — worth looking at
  before finalizing the parsing shape in §1.
- **Model B's `destroy` exposure**: even as an explicit opt-in value (not
  wired to `down`), should it require something stronger than "a Haskell
  value the caller chooses to reference" — e.g. a separate CLI subcommand
  gated behind its own confirmation prompt/flag — given the blast radius?
  Leaning yes, but the concrete UX depends on how `destroy` would actually
  be invoked in practice (interactively vs. from CI).
- **Version/binary pinning**: does the environment already pin a Terraform
  version (a `.terraform-version`/`required_version` in existing config),
  and should the new `Track' (Binary "terraform")` check/require it, or
  just shell out to whatever `terraform` is on `PATH` like every other
  `Binary` track in this codebase does today (recommend the latter —
  consistent with existing conventions, and version mismatches surface as
  ordinary `terraform` errors rather than needing salmon-side detection)?

## Phased plan

1. `SreBox.TerraformState.readOutputs` (§1) against a captured
   `tf-outputs.json` file only — no live `terraform output` shell-out yet,
   no `Op` involved at all, just JSON parsing into typed values. Cheapest
   possible slice, immediately unblocks the pg-ha control-plane spec's
   "where do machine addresses come from" question.
2. Wire a `TerraformSource` into `ControlPlaneSeed` (from the pg-ha spec)
   as an alternative to hand-typed machine addresses.
3. Live shell-out variant (`terraform output -json`, not just a captured
   file), once the captured-file path has been exercised for real.
4. Model B (`Terraform.apply`, §3), only if/when there's a concrete need
   for salmon to drive `apply` itself rather than assuming it already ran.

## Future work

- HCL generation/templating from salmon seeds (the inverse direction),
  if the "already-existing usage" turns out to be small enough that salmon
  owning it outright becomes more attractive than treating it as a fixed
  external input.
- A `destroy`-with-plan confirmation flow analogous to `advance-querying.md`'s
  `Plan`/digest mechanism, if Model B's opt-in destroy needs more ceremony
  than a bare CLI flag.
