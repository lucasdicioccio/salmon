# Salmon next to Ansible, Terraform, Patroni and repmgr

What salmon does that the tools you already know do not, what they do that
salmon does not, and when to choose one of them over it. Two kinds of tool
are compared: general provisioners (Ansible, Terraform) and tools built only
for Postgres high availability (Patroni, repmgr). Salmon overlaps with each,
but it is not a replacement for either.

Statements about the other tools describe their usual, documented use, not
every plugin or wrapper built around them. Statements about salmon say what
has been run and what has not, as the rest of this repository does.

## The one-paragraph version

Salmon is a library for building **one kind of graph**: every node has a
`check` ("is this already in place?"), an `up` ("make it so"), a `down`
("undo it"), and edges that say what must come first. A node can be as small
as one line in a file or as large as a Postgres switchover. One binary can run
the graph once (`run up`, `run down`), or keep it converged with `run serve`,
which asks every node's `check` again between commands and re-applies what has
gone away. Ansible and Terraform each give you part of this. Patroni and
repmgr give you a better version of one node of it. The rest of this page
goes through the details.

## Against a general provisioner (Ansible, Terraform)

| | Ansible | Terraform | Salmon |
|---|---|---|---|
| What you write | YAML task lists (playbooks, roles) | HCL resources | Haskell values: a `Track` from a JSON spec to an `Op` graph |
| Ordering | playbook order, plus handlers | a dependency graph inferred from references | a dependency graph you declare (`deps`, `inject`) and can print (`run tree`, `run dag`) |
| "Already done?" | each module decides, by its own convention; `--check` mode depends on module support | a refresh compares the state file with the provider's API | every node can have a `check` that asks the world directly; there is no state file to drift from |
| Undo | write the opposite tasks (`state: absent`) by hand | `terraform destroy`, for every resource | `run down`, for every node: each node carries its own `down`, run in reverse dependency order |
| Scope of one node | a task on a host | a resource behind a provider API | any IO: a file, a unit, a package, a cloud resource, a database role, a process to keep running, a Postgres switchover |
| Keeping it true afterwards | runs once; `ansible-pull` on a timer is the usual pattern for repeating it | runs once; drift is found at the next `plan` | `run serve` stays up: it tends nodes between commands under a per-node restart policy, holds long-running processes (`managed`), serves an HTTP API, an event stream and a web UI, and can pull signed declarations from a registry |
| Failure | the play stops on that host (unless told to ignore it) | apply stops; partial state is recorded | a failed `up` blocks only its dependants, and the rest of the graph keeps converging |
| Ecosystem | very large | very large | small: the builtins listed in the README |

What this means in practice:

- **Same semantics at every size.** In Ansible, "write this config file" and
  "move this database's primary" are different kinds of thing: a module, and
  then a role full of conditionals. In Terraform the second does not fit the
  resource model. In salmon both are an `Op` with a `check` and an `up`, so
  they compose, print and supervise the same way (`SreBox.PostgresPair`'s role
  node is an ordinary node).
- **Undo comes with the node.** `run down` is defined for every graph,
  including the parts Terraform's providers would not cover, such as files,
  units and database objects on a machine you already have.
- **No state file.** A node's `check` asks the machine, so a change made by
  hand is seen at the next pass, not at the next `plan`. The price is that
  `check` has to be written, and a node without one runs its `up` every pass
  (see `Immaterial` in [the cookbook](howto-ops.md)).
- **One long-running mode.** `run serve` is the same graph kept alive, not a
  second product: see [`run serve`](serve-supervision.md).

When to choose one of them instead:

- **You need a provider salmon does not have.** Terraform's provider catalogue
  and Ansible's module collections are far larger than salmon's builtins.
  Salmon's GCP builtins exist, but much of that code is tested at Layer 0 only
  (see [the GCP playbook](gcp-toy-validation.md)), and there is no AWS or Azure
  support.
- **Your team works in YAML or HCL, not Haskell.** Writing a salmon graph means
  writing Haskell. Operating one does not (`config | run up` takes plain
  arguments and JSON), but someone has to write it.
- **Terraform already owns your infrastructure.** Salmon does not read
  Terraform state today; [`specs/terraform-integration.md`](../specs/terraform-integration.md)
  is a design sketch, not an implementation. The natural split is Terraform for
  machines and salmon for what runs on them.

## Against a Postgres HA tool (Patroni, repmgr)

| | Patroni | repmgr | Salmon's pair (`SreBox.PostgresPair`) |
|---|---|---|---|
| Who decides the primary | a leader lock in a consensus store (etcd, Consul, ZooKeeper, Kubernetes) | the operator, or the `repmgrd` daemon voting among nodes (optionally with a witness) | **the operator, always**: the primary is a field in a declaration |
| Automatic failover | yes | yes, with `repmgrd` | **no, never**: an unproven promotion is refused, and the operator accepts a named loss with `--may-discard` |
| Machines needed | three or more consensus voters (they can sit beside Postgres) | two or more, plus a witness to break ties | two |
| Planned switchover | `patronictl switchover` | `repmgr standby switchover` | change one word (`--primary B`): the next pass pauses the bouncer, stops the old primary cleanly, promotes, repoints, rejoins with `pg_rewind` |
| An interrupted switchover | Patroni's loop re-evaluates | rerun or repair by hand | state is re-derived every pass: the next `run up` finishes it (scenario S2) |
| Everything else on the machine | out of scope | out of scope | the same graph: packages, units, `pg_hba`, roles, certificates, pgbouncer, the application |

The difference that matters: **Patroni and repmgr are HA tools, not
provisioners.** Something still has to install Postgres, write their
configuration, create the replication roles, set up the bouncer and supervise
the daemons. Salmon is one graph that does all of that, and its pair recipe is
one more node in it.

What salmon's pair is good for:

- **Two machines and a person available.** A planned switchover loses nothing
  and, with pgbouncer `PAUSE`, clients see latency rather than errors.
  A failover is a decision someone makes and writes down (`--may-discard A`),
  not a timeout.
- **Testing failure on purpose.** The declared primary is a setting a test can
  change. The disaster catalogue S1 to S8 (crashed primaries, partitions,
  killed controllers, a stranger's cluster where a member should be) is run
  against real VMs by `Test.PostgresSwitchoverSpec` and `Test.PgPairDemoSpec`.
  See [the Postgres pair guide](postgres-pair.md#what-is-tested-and-what-that-is-worth).

When to choose Patroni (or repmgr) instead:

- **Nobody will be awake at 3am.** Deciding that a machine is dead needs
  consensus, which needs three voters. Salmon's pair does neither, by design.
  This is Patroni's job.
- You can also **use both**: salmon has a `Patroni` builtin (`patroniMember`,
  run against a real Patroni and etcd in containers) plus `Etcd`, `PgBackRest`
  and `Haproxy` builtins. In that layout salmon provisions and supervises and
  Patroni decides. Those nodes exist; the recipe that combines them into a
  cluster is still a design sketch ([`specs/pg-patroni.md`](../specs/pg-patroni.md)),
  and the HAProxy node is tested at Layer 0 only.

## See it rather than read it

`salmon-toy-qemu-pg-ha` builds three qemu guests, runs the pair on two and
pgbouncer on the third, and keeps a client writing while the primary moves.
On the machine where the guide was written, one declared switchover ended with:

```
  inserts acknowledged through the bouncer: 460
  inserts that came back an error:          0
  acknowledged rows missing afterwards:     0
```

The commands, the root-only `prereqs` step, and what has and has not been run
are in [the Postgres pair guide](postgres-pair.md#watching-it-happen).
