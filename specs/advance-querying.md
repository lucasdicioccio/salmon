# Advanced querying: targeting `run` at a subset of nodes

Status: draft / not implemented. This is a design sketch to react to, not a committed plan.

## Problem

Today `execCommandOrSeed` (`Salmon.Builtin.CommandLine`) only offers whole-graph
operations: `run Up` executes every node, `run Tree`/`run DAG` describe every
node. There is no way to say "run everything except this node" or "show me
just this subtree" without hand-editing the recipe. As graphs grow (a single
`initialize` already nests user/group/chown/ssh-key subtrees), operators want
to:

- inspect a subtree or subgraph in isolation (debugging, code review of a
  recipe change, onboarding),
- run `Up` while skipping a known-bad or intentionally-deferred node, without
  losing the rest of the DAG's ordering/dedup/failure-propagation behavior.

## Design goals / non-goals

Goals:
- A query surface to talk about "this subtree", "this subgraph", "all of X
  except Y" against an already-expanded graph.
- A `run`-compatible execution mode that skips excluded nodes but otherwise
  preserves exact DAG semantics (ordering, `Ref` dedup, `Failed`/`Blocked`
  propagation).
- Safe by construction against the seed/config/query/run split already being
  four independent process invocations: nothing should let an exclusion list
  computed against one graph get silently applied to a different one.

Non-goals (v1):
- Running *only* a subtree in isolation (as opposed to "everything, minus
  some exclusions") — interesting, but a distinct feature; see "Future work".
- A general boolean query language (intersection, negation of a whole
  expression, etc.) — v1 covers union-of-selections minus union-of-exclusions,
  which is what the motivating use case needs.
- Down/teardown-with-plan — `downTree` has no `prelim`-equivalent today (see
  CLAUDE.md), so "skip on down" needs its own design; out of scope here.

## Why the addressing/digest scheme has to start from the directive, not the seed

Recall the existing two-phase protocol (`Salmon.Builtin.CommandLine`):

```
my-salmon config <seed-args...>   # seed -> directive JSON  (Configure IO seed directive: impure)
my-salmon run Up|Tree|DAG         # directive JSON -> Op -> execute            (Track' directive: pure)
```

`Configure`'s own haddock is explicit that this split exists so the "turn a
seed into a directive" step can be impure (reads files, env vars, whatever)
while "turn a directive into ops and run them" is meant to be hermetic. In
every existing recipe, `Track' directive` builds an `Op = OpGraph Identity
Actions'` — the `Identity` means `expand` is pure. **Given the same directive
JSON, the DAG shape (nodes, `Ref`s, edges) is fully deterministic.** All the
non-determinism in the whole pipeline lives in the seed → directive step.

That has one immediate consequence for this design: a `query` command must
consume the **directive** (the same JSON `run` already reads from stdin), not
the seed. If `query` instead re-ran `Configure` from a seed, two separate
invocations of an impure `Configure` (one for `query`, one later for `run`)
could silently produce two different directives — and therefore two
different graphs — while looking like "the same seed" to a human. Consuming
the directive sidesteps that: it's already the hermetic boundary the project
chose on purpose.

This also gives us the digest mechanism for free: hashing the directive's
canonical JSON bytes pins the exact DAG shape a plan was computed against, and
`run` can cheaply re-check that hash against whatever directive it's handed
before trusting a plan.

## Node addressing

Reuse the path format `run tree` (`Salmon.Actions.Help`) already prints —
`/initialize/chown/deb` etc. — as the human-facing selector syntax, since
operators already read that format when debugging today.

```
/initialize/chown/deb            -- exact path
/initialize/chown/*               -- one segment wildcard (direct children)
/initialize/chown/**               -- subtree wildcard (any depth)
```

Important subtlety: a path is a *position* in the expanded tree, not a node
identity — the same `Ref` can appear at multiple paths (that's exactly the
repeated-subtree phenomenon from the `passwordless`/`chown` example earlier
in this project). So resolving a selector is a two-step process:

1. Match the pattern against every path in the expanded `Cofree Graph` (same
   traversal `Help.printHelpCograph` and `Dot.printCograph` already do),
   collecting the `Ref` at each matching path.
2. Selections and exclusions are ultimately **sets of `Ref`s** — this is also
   what `upTree`'s dedup-by-`Ref` already keys on, so "exclude this `Ref`"
   composes cleanly with "the second occurrence of this node is `Redundant`
   anyway": whichever occurrence is walked first is the one whose `prelim`
   gets forced.

## Query language (v1)

Repeated flags, applied as one union-then-difference, no operator precedence
to think about:

```
--select PATTERN     -- may repeat; union. Omitted entirely = "everything".
--exclude PATTERN     -- may repeat; union, then subtracted from the selection.
```

`resolvedRefs = union(matches(select_i)) \ union(matches(exclude_j))`

This is enough to express both motivating cases:
- "show me this subtree": `--select '/initialize/chown/**'`
- "run everything except this node": `--exclude '/initialize/passwordless'`

## New CLI surface

Add a `query` subcommand alongside `config`/`run`, and let `run up` accept an
optional plan.

```haskell
data Command seed
    = Config seed
    | Query QueryCommand
    | Run BaseCommand

data QueryCommand
    = QueryCommand
    { queryMode :: QueryMode
    , querySelect :: [Text]   -- PATTERN, repeatable
    , queryExclude :: [Text]  -- PATTERN, repeatable
    }

data QueryMode
    = ShowMatches   -- human-readable, like `run tree` but annotated
    | EmitPlan       -- JSON `Plan` to stdout

data BaseCommand
    = Up
    | UpWithPlan FilePath   -- new: run Up, but honor an emitted Plan
    | Tree
    | DAG
```

Both `Query` and `UpWithPlan` read the directive from stdin exactly like
`Run` does today — no new input channel, same hermetic boundary.

```sh
# inspect a subtree
my-salmon config <seed-args> | my-salmon query show --select '/initialize/chown/**'

# build an exclusion plan
my-salmon config <seed-args> | my-salmon query plan --exclude '/initialize/passwordless' > plan.json

# apply it
my-salmon config <seed-args> | my-salmon run up --plan plan.json
```

## The `Plan` artifact

```haskell
data Plan = Plan
    { planDirectiveDigest :: Text        -- sha256 of the canonical directive JSON
    , planExcludedRefs :: [Ref]          -- resolved at `query plan` time
    , planExcludedPatterns :: [Text]     -- kept for humans re-reading the file
    }
```

`query plan` emits this after resolving patterns against the directive it was
handed. `run up --plan plan.json`:

1. Reads the directive from stdin (unchanged).
2. Recomputes `sha256` of the same canonical encoding and compares it to
   `planDirectiveDigest`. Mismatch → refuse to run (non-zero exit, loud
   error naming both digests), unless `--force-stale-plan` is passed.
   This is the guard against the two-invocation race: if anything upstream
   (a re-run of `config` with different seed args, a change to the recipe
   binary between builds, ...) produced a different directive than the one
   `query plan` saw, the plan is refused rather than silently mis-applied.
3. Expands the directive into the `Op` graph as usual.
4. Applies `forceSkip planExcludedRefs` (see below).
5. Runs `upTree` unchanged.

### `forceSkip`

```haskell
forceSkip :: Set Ref -> Op -> Op
```

Walks the graph and, for every node whose `ref` is in the given set, replaces
`prelim` with `pure Skippable`, leaving `up`, `down`, `ref`, `dynamics`, and
the graph topology completely untouched. `OpGraph`'s derived
`Functor`/`Traversable` (`salmon-core/src/Salmon/Op/OpGraph.hs`) already maps
a function over every `node` in the structure including through the effectful
`predecessors`, which is the natural place to hang this rewrite.

Net effect on `upTree` (see `Salmon.Actions.UpDown`): a forced-skip node is
walked exactly like today, dedup-by-`Ref` still applies, but it reports `Skip`
unconditionally instead of running its own `prelim`/`up`. Everything that
depends on it runs exactly as if it had genuinely been up-to-date — this is
the "keep the original DAG ordering" property asked for: nothing is cut out
of the graph, so a dependent that (incorrectly) assumed the excluded node's
effect doesn't silently get reordered or vanish, it just sees a no-op
predecessor.

Caveat worth documenting prominently: this can absolutely produce a broken
system if a dependent's correctness genuinely required the excluded node's
`up` to have run (e.g. excluding user creation but not the things that log in
as that user). The tool enforces graph-shape consistency (the digest check);
it cannot and should not try to enforce semantic safety of an operator's
chosen exclusion set.

## `query show` output sketch

Mirrors `run tree`'s indentation, annotating matched lines:

```
/initialize
/initialize/file-contents writes /etc/sudoers.d/salmon with some contents
/initialize/file-contents/directory ensures /etc/sudoers.d exists, including subdirs
/initialize/passwordless removes a system user's password          [excluded]
/initialize/passwordless/user creates a system user
...
/initialize/chown sets ownership of /home/salmon to salmon:salmon   [selected]
```

## Open questions

- Should `query plan`'s resolved `Ref`s be re-validated at `run` time against
  the freshly-expanded graph (i.e. warn/fail if an excluded `Ref` from the
  plan no longer appears anywhere), separately from the digest check? Given
  the digest already pins the whole directive, this may be redundant, but a
  friendlier error message ("this plan excludes a node that doesn't exist in
  this graph") might be worth the extra check.
- `--force-stale-plan`: allow proceeding on a digest mismatch by re-resolving
  `planExcludedPatterns` (not `planExcludedRefs`) against the new directive?
  That would make the plan resilient to some classes of directive drift
  (e.g. an unrelated field changed) at the cost of re-introducing the exact
  risk the digest was meant to close. Leaning towards: no, keep the escape
  hatch dumb (skip the check entirely, operator's responsibility) rather than
  clever (silently re-resolve and hope the patterns still mean the same
  thing).
- Tag/dynamics-based addressing (nodes opting into stable labels via the
  existing `dynamics` field, independent of tree position) would be more
  refactor-resistant than path globs, but requires node authors to annotate
  their ops. Worth a v2 if path-glob addressing turns out too fragile once
  recipes change shape.

## Future work

- Selection-only execution ("run just this subtree, treating its own
  external dependencies as already-satisfied") rather than
  everything-minus-exclusions. Same `forceSkip` machinery, inverted:
  force-skip everything *not* reachable from the selection's roots, then run
  the selection's own roots normally. Deferred because it changes the
  "predecessor's a real dependency" invariant in a way that needs its own
  correctness argument (a truly-required predecessor outside the selection
  would need to be forced skippable, which is a much easier way to shoot
  yourself in the foot than the exclusion case).
- The same `Plan`/digest idea applied to `downTree`, once teardown gets a
  `prelim`-equivalent.
