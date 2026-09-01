# HA Postgres + bouncer + app-instance + LB control plane

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem

We want to host a service (multiple services, eventually) on top of:

- a replicated Postgres pair per logical cluster, WAL-streamed both ways
  across two machines so each machine holds one primary and one standby
  ("diagonal" replication — machine.ab.0 is primary for `pg.a`/standby for
  `pg.b`, machine.ab.1 is the mirror image),
- pgbouncer in front of each cluster, pointed at whichever side currently
  holds the primary role,
- app instances (PostgREST-style, plus the user's own "internaltool" service) that
  need their own pg users/roles/secrets/CORS/rate-limit settings, sat behind
  the bouncers,
- a front load balancer (or a small HA pair of them) fronting the bouncers
  and app instances, with DNS pointed at it,
- monitoring/alerting across all of the above,
- multiple *sizing tiers* of this whole shape — shared multi-tenant
  deployments and dedicated ones on different hardware profiles.

Sketch (as given):

```
 ┌────────────────────┐        ┌────────────────────┐
 │ pg.a   pg.b         │  wal   │ pg.a   pg.b         │
 │ (primary) (standby) │◄──────►│ (standby) (primary) │
 │ machine.ab.0         │        │ machine.ab.1         │
 └──────────┬──────────┘        └──────────┬──────────┘
      bouncer.a                       bouncer.b
            │  (crossed: every app instance can reach either bouncer)
   internaltool.a.0  internaltool.b.0  internaltool.a.1  internaltool.b.1  postgrest.b  control-plane
            └──────────────────────┬──────────────────────┘
                                   LB ── DNS
```

Sticky note requirement: **control-plane seeds must be unfoldable to**
machines, pg clusters (certs, users), bouncers, app-instances (pg-users,
secrets, roles, cors-settings, rate-limit-settings), load-balancers (DNS
configs), PostgREST configs (secrets, pg-users), and monitoring/alerting
configs.

## What already exists (inventory)

Salmon already has most of the *leaf* building blocks this needs; nothing
below is new work:

| Diagram box | Existing code |
|---|---|
| pg primary/standby, WAL streaming | `Salmon.Builtin.Nodes.Postgres` (`primaryReplicationSetup`/`standbyReplicationSetup`/`replicationUser`/`EnsurePhysicalReplicationSlot`) — exercised today only by the hand-run `salmon-postgres-replication-fixture`, not wired into a real recipe |
| bouncer.a / bouncer.b | `Salmon.Builtin.Nodes.PgBouncer` — takes plain host/port/dbname/user/password, deliberately decoupled from how the upstream cluster was provisioned |
| LB | `Salmon.Builtin.Nodes.Nginx` — vhost list → upstream group, reverse proxy. Nothing HA (keepalived/VRRP) for "load-balancer(s)" plural yet |
| DNS | `SreBox.MicroDNS`, `SreBox.DNSRegistration` |
| app instance (internaltool/postgrest shape: pg-user, secret, connstring, systemd unit, pushed to a remote box) | `SreBox.Postgrest` is the exact template — build the "internaltool" recipe by mirroring its `PostgrestSetup`/`setupPostgrest` shape, not by generalizing it prematurely (see `[[recipe_key_exchange_agnostic]]`-style convention: pass in pre-provisioned secrets, don't invent a transport) |
| seed → directive → ops, long-running convergence | `Salmon.Builtin.CommandLine.execCommandOrSeed`, `Salmon.Actions.Serve` (`World`/`Epoch`/`NodeState`) |
| declaring "which pg cluster/user/db" | `SreBox.PostgresInit`, `Postgres.CreateDB`/`CreateUser`/etc. |

What's genuinely missing:

1. A recipe that composes the **diagonal replication pair** into one
   reusable unit (today `Postgres.hs` only has the two standalone
   primary/standby roles; nothing ties "these two machines are each other's
   primary and standby" together, or derives each bouncer's upstream from
   whichever side is *currently* primary).
2. A way for a **seed to carry forward previously-converged state** — most
   importantly "which side is primary right now" — across separate `config`
   invocations. Nothing like this exists yet.
3. **Sizing tiers / hardware profiles** (shared vs. dedicated) as a
   first-class concept threaded through machine/cluster seeds.
4. A **control-plane seed** that unfolds into the sub-seeds for
   machines/clusters/bouncers/app-instances/LB/DNS/monitoring, per the
   sticky note.
5. **Monitoring/alerting nodes** — zero Prometheus/alerting builtins exist
   in the repo today.
6. An **HA front LB** story — today's `Nginx.hs` is a single reverse proxy
   config renderer; "load balancer(s)" plural in the diagram implies either
   DNS-level multi-A-record fanout (already partially available via
   MicroDNS) or a keepalived/VRRP-style active/standby pair, neither of
   which has a builtin yet.

## Design goals / non-goals

Goals:
- Reuse every existing builtin/recipe listed above unchanged; new code
  should be composition, not rewrites.
- Keep the "recipes are key-exchange/secret-transport agnostic" convention:
  new recipes take pre-provisioned secrets/certs/files, never invent a way
  to move them (matches the existing constraint on `salmon-ops-recipes`
  modules).
- Make "which side is primary" an explicit, operator-declared fact carried
  through the seed, not something salmon infers or automates — salmon's
  `up`/`down`/`check` model is declarative convergence, not a control loop;
  automatic failover detection is out of scope for salmon itself (it can be
  *fed by* an external health-checker, see open questions).
- Sizing tiers should be data (a profile value in the seed), not a code
  fork — a shared-tier deployment and a dedicated-tier deployment should go
  through the same recipes with different profile values.

Non-goals (v1):
- Automatic failover orchestration (deciding *when* to promote a standby).
  Salmon's job is to converge the system to whatever the seed currently
  declares, including "pg.a's primary is now machine.ab.1" — the decision
  to flip that declaration is an operator (or external tool) action.
- A generic multi-service control plane. Build this for the pg/bouncer/app/
  LB shape in the diagram; generalize later only if a second, differently-
  shaped service shows the abstraction is right.
- Monitoring dashboards/alert rules content — v1 is just "the nodes exist to
  install and configure an agent," not a curated set of alerts.

## Proposed architecture

### 1. `PgClusterPair`: the diagonal replication unit

A new `SreBox.PostgresHA` (or similar) recipe, one level above
`Postgres.primaryReplicationSetup`/`standbyReplicationSetup`, that takes:

```haskell
data Side = SideA | SideB
    deriving (Eq, Show, Generic)

data PgClusterPair
    = PgClusterPair
    { pair_machine_a :: Text          -- host/address of machine.ab.0
    , pair_machine_b :: Text          -- host/address of machine.ab.1
    , pair_cluster_a :: Postgres.ClusterName   -- "pg.a"
    , pair_cluster_b :: Postgres.ClusterName   -- "pg.b"
    , pair_primary_side :: Side       -- carried forward, see state section below
    , pair_repl_role :: Postgres.RoleName
    , pair_repl_password :: Postgres.Password  -- pre-provisioned, not generated here
    , pair_repl_slot :: Postgres.ReplicationSlotName
    }
```

`up` for this seeded value produces the four `Postgres` ops (primary-a,
standby-a on the other box, primary-b, standby-b), each exactly the existing
`Postgres.primaryReplicationSetup`/`standbyReplicationSetup` calls the
fixture already demonstrates — the new code is just "call these four times
with swapped machine/cluster arguments," plus:

```haskell
bouncerUpstream :: PgClusterPair -> Postgres.ClusterName -> PgBouncer.UpstreamDb
```

which looks at `pair_primary_side` to decide whether `pg.a`'s bouncer
upstream is `pair_machine_a` or `pair_machine_b`. This is the function that
makes bouncer config "dynamic" per the ask — the bouncer recipe itself
doesn't change at all, only which host/port `setupPostgrest`/`PgBouncer`-
config-building code plugs in.

Promoting a standby (flipping `pair_primary_side`) is **not** this recipe's
job — see previous-seed section. This recipe only ever converges to
whatever `pair_primary_side` currently says; a real failover still needs an
operator (or a health-check tool) to run `pg_ctl promote`/equivalent on the
actual standby out of band, then update the declared state so the next
`salmon-x config ...` reflects reality. Modeling *that* promotion as a
salmon `Op` is future work (see below) — v1 treats it as an external fact.

### 2. Previous-seed state: no new core mechanism needed

The key realization: `Configure m seed a = Configure { gen :: seed -> m a }`
(`Salmon.Op.Configure`) already allows `gen` to be arbitrarily impure. There
is no need for a new "previous-seed" abstraction in `salmon-core` — a seed
can simply carry the *path to a small state file*, and `gen` reads it (if
present) as part of building the directive:

```haskell
data ClusterSeed
    = ClusterSeed
    { seed_machine_a :: Text
    , seed_machine_b :: Text
    , seed_previous_state :: Maybe FilePath   -- e.g. state/pg-ab.json
    , ...
    }

data PairState = PairState { state_primary_side :: Side }
    deriving (Generic)
instance FromJSON PairState
instance ToJSON PairState

configure :: Configure IO ClusterSeed PgClusterPair
configure = Configure $ \seed -> do
    prev <- maybe (pure Nothing) readPairState seed.seed_previous_state
    let side = maybe SideA state_primary_side prev
    pure PgClusterPair { pair_primary_side = side, ... }
```

`run serve`'s `status`/`history` output (`Salmon.Actions.Serve`) is already
a superset of what a state file needs, so an even lighter option is: after
every converge, a tiny script/CLI extracts `{"primary_side": ...}` from
`serve status` and writes it to the state file the *next* `config`
invocation reads — no changes to `Serve.hs` at all. Whether that extraction
lives in a shell wrapper or a new `run status --extract pg-pair` subcommand
is an open question below.

This generalizes beyond pg-pair: any seed that wants "remember what I last
converged to" (stable secrets, stable port allocations, stable primary/
standby role) uses the same `Maybe FilePath` + small JSON state pattern.
Worth documenting as a convention once the first instance (pg-pair) proves
it out, rather than building a generic "stateful seed" typeclass speculatively.

### 3. Sizing tiers / hardware profiles

A `Tier` value threaded through the control-plane seed, data not code:

```haskell
data Tier
    = Shared     -- multiple logical clusters/instances per machine
    | Dedicated  -- one logical deployment owns the machine
    deriving (Eq, Show, Generic)

data HardwareProfile
    = HardwareProfile
    { profile_tier :: Tier
    , profile_pg_shared_buffers :: Text   -- e.g. postgresql.conf tuning knobs
    , profile_bouncer_max_client_conn :: Int
    , profile_app_instance_count :: Int
    -- extend as real constraints show up; resist modeling resource limits
    -- (cgroups/systemd slices) until a concrete need forces it — nothing in
    -- salmon-ops does resource-limiting today
    }
```

`Postgres.defaultReplicationTuning` and `PgBouncer.BouncerConfig`'s
size-shaped fields (`bouncer_max_client_conn`, `bouncer_default_pool_size`)
already exist as plain values a `HardwareProfile` can feed — no changes
needed to those modules, just don't hardcode the numbers in the new
control-plane recipe.

### 4. Control-plane seed unfolding

Mirror the existing `Migrator.Seed` → `Migrator.Spec` → `Migrator.Ops`
three-file split (`salmon-apps/src/Migrator/`), one level deeper, so the
control-plane binary follows the same seed→spec→ops shape every other
salmon binary already uses (per CLAUDE.md's "seed → spec → ops CLI
protocol" section) instead of inventing a new pattern:

```haskell
-- ControlPlane/Seed.hs — human/CLI-facing, ParseRecord
data ControlPlaneSeed
    = ControlPlaneSeed
    { cp_tier :: Tier
    , cp_pair :: ClusterSeed              -- previous-state-aware, see above
    , cp_app_instances :: [AppInstanceSeed]  -- internaltool.a.0, internaltool.b.0, ...
    , cp_postgrest :: [PostgrestSeed]
    , cp_lb :: LbSeed
    , cp_dns :: DnsSeed
    , cp_monitoring :: MonitoringSeed
    }

-- ControlPlane/Spec.hs — FromJSON/ToJSON directive, output of `config`
data ControlPlaneSpec
    = ControlPlaneSpec
    { cps_pair :: PgClusterPair
    , cps_bouncers :: [PgBouncer.BouncerConfig]
    , cps_app_instances :: [AppInstanceSetup]     -- mirrors PostgrestSetup
    , cps_postgrest :: [PostgrestSetup]
    , cps_lb :: Nginx.NginxConfig
    , cps_dns :: ...
    , cps_monitoring :: ...
    }

-- ControlPlane/Ops.hs — Track' ControlPlaneSpec -> Op, composing the above
-- recipes exactly as SreBox.Postgrest/SreBox.Initialize already do
```

`gen :: ControlPlaneSeed -> IO ControlPlaneSpec` is where all the "unfold
one seed into machines/clusters/bouncers/..." fan-out happens — e.g.
deriving each `internaltool.X.N` instance's pg-user name from `X`/`N`
deterministically, deriving bouncer upstream lists from `cps_pair` +
`bouncerUpstream` above, deriving the LB's vhost upstream list from the
concrete app-instance ports. This step is pure fan-out/derivation logic,
independently testable without touching any `Op`/IO machinery (same
argument `advance-querying.md` makes for why directive-shape is worth
pinning down separately from execution).

Whether one `ControlPlaneSeed` should directly enumerate every app instance
(as sketched above) or itself be built from a smaller "how many of each
tier" description is an open question below — start with explicit
enumeration (simplest, matches how `Migrator.Seed` already just lists
fields) and only introduce a generator if the enumeration gets unwieldy.

### 5. Monitoring/alerting — new builtin, deliberately thin in v1

New `Salmon.Builtin.Nodes.Monitoring` (naming TBD) module, following the
`PgBouncer.hs`/`Nginx.hs` shape exactly (a config value type, a render
function, `justInstall` + `Systemd.systemdService`/`restartService`). v1
scope: install and configure a metrics agent (node_exporter-style) per
machine and a scrape-target list on whatever central collector exists —
*not* alert rule content, dashboards, or the collector itself, all of which
need a stack decision first (see open questions).

### 6. HA front LB

Two independently-shippable pieces, not one:
- **Multiple LB instances**: `Nginx.setup` already renders a full config
  from a value — running it on two boxes is just calling it twice with the
  same `NginxConfig`. No new code.
- **Active/standby or DNS-fanout in front of them**: either extend
  `SreBox.DNSRegistration`/`MicroDNS` to publish multiple A records
  (client-side failover/round-robin, simplest, no new node), or add a new
  `Keepalived`/VRRP builtin for a floating VIP (real HA, meaningfully more
  work: needs its own idempotency story per CLAUDE.md's conventions section
  since `keepalived.conf` has no natural "replace" verb, would likely follow
  the `Netfilter.rule`-style `prelim`-based skip). Recommend starting with
  DNS fanout and only building VRRP support if it's a hard requirement.

## Open questions

- **Failover signal**: is "which side is primary" ever going to be
  automatically detected (an external health-checker writes the state
  file), or always an explicit operator action (`salmon-x config ... 
  --primary-side b`)? Changes whether the state-file convention in §2 needs
  to be racy-write-safe (atomic rename) from day one.
- **`run status --extract` vs. a wrapper script**: does the previous-state
  read belong as a first-class `Serve`/CLI feature, or is a small external
  script (parse `serve status` JSON, write the state file) enough for v1?
  Leaning toward the external script — avoids growing `Serve.hs` for a
  need that isn't proven out yet.
- **Monitoring stack**: Prometheus + node_exporter + something for alerts
  (Alertmanager? a hosted service?) — needs a decision before §5 can be
  more than a stub.
- **"Internaltool" vs. generic app-instance**: is `SreBox.Postgrest` close enough
  to fork/mirror directly, or does internaltool need meaningfully different
  shape (its own migrations, non-PostgREST HTTP surface, different secret
  set)? Affects whether §4's `AppInstanceSeed` is its own new module or a
  thin renaming of `PostgrestSeed`.
- **Cross-wiring in the diagram** (every `internaltool.X.N` reaching *both*
  bouncers, not just its "own" one): intentional (read replicas via the
  standby-side bouncer, or just redundancy), or a simplification in the
  sketch? Determines whether `AppInstanceSetup` takes one connstring or a
  primary/replica pair.
- **Enumeration vs. generator for `ControlPlaneSeed`** (§4): revisit once
  a first real deployment shows how many app instances/tiers actually need
  representing.

## Phased plan

1. `SreBox.PostgresHA` (§1) as a standalone recipe + fixture, no
   control-plane wiring yet — proves out the diagonal-pair composition and
   `bouncerUpstream` derivation against real (podman) machines, same style
   as the existing replication fixture.
2. Previous-seed state convention (§2), proven out against the §1 fixture:
   manually flip `pair_primary_side` via a state file, confirm bouncer
   config follows.
3. `AppInstanceSeed`/internaltool recipe (§4, mirroring `SreBox.Postgrest`),
   wired to a single `PgClusterPair` — no LB/DNS/monitoring yet.
4. LB + DNS wiring (§6, DNS-fanout option first).
5. `ControlPlaneSeed` (§4) tying 1–4 together end to end for one tier.
6. `HardwareProfile`/`Tier` (§3) threaded through, second tier added.
7. Monitoring (§5), stub scope.

## Future work

- Modeling promotion itself (`pg_ctl promote` + updating the declared state)
  as a salmon `Op`, once the manual-declaration workflow in the phased plan
  is proven out and its pain points are known.
- VRRP/keepalived-based LB HA (§6), if DNS fanout turns out insufficient.
- Generalizing the control-plane seed pattern beyond this one service shape,
  if/when a second differently-shaped service needs the same treatment.
