# How the graph operations scale

What each graph operation costs as the graph grows, per shape: measured, with the growth fitted,
by the `graph-profiles` benchmark in `salmon-ops-recipes` over the synthetic graphs of
`Test.GraphFixture`. The numbers are **indicative**: one run, on a shared laptop-class machine that
other work was using. Read the growth and the order of magnitude, not the third digit.

This document reports. The super-linear findings are listed with what was measured and,
separately, what reading the code suggests is the cause. Five have been fixed since: finding 4
(the listed paths, see "After the path cap"), finding 5
(the client model's teardown) and finding 6 (what a tended node retains), both under "Since
fixed" below, finding 1 (`foldDag` per path: see
"After the per-node fold", which gives the fold, `query plan` and the `serve` loop before and
after, and what else changed), and finding 3 except the concurrent walk's wake-ups (edge lists:
see "After the edge lists"). The others stand as measured, and the findings and the two big
tables below are the 2026-10-10 run, left as measured.

## Running it

```sh
cabal bench salmon-ops-recipes:graph-profiles \
  --benchmark-options='--max-size 100000 --budget 6 --heap 4g --out profiles.tsv'
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--summarise profiles.tsv'
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--list'
```

It is a `benchmark` stanza, so `cabal build all` and `cabal test` neither build nor run it.
`--max-size` is the size cap (default 10 000 nodes), `--only TEXT` and `--shape NAME` narrow the
run, `--out` appends tab-separated rows, `--summarise` turns such a file into the tables below.
The full run behind this document took about 45 minutes.

A cheap guard runs in the ordinary suite (`Test.GraphScaleSpec`): the outline, a selector, the
listed paths, `fromMagma`, `mergeDag`, `stuck` and the walks over the three shapes that share, at
600 nodes (`2^200` paths for the chain of diamonds), each within a minute. It tells "per node"
from "per path"; it does not time anything.

## What was measured, and on what

- **Machine:** 13th Gen Intel Core i7-1370P (20 hardware threads), 32 GB, Linux 7.0. Shared: the
  load average was between 2 and 5 throughout, from other work.
- **Compiler and flags:** GHC 9.10.3, cabal's default optimisation (`-O1`), `-threaded` with the
  default single capability (how the binaries in `salmon-apps` are built), `+RTS -T -M4g`.
- **Date:** 2026-10-10, at the commit that added the benchmark.
- **Runs:** `resources/graph-profiles/100k.tsv` is every operation over every shape up to 100 000
  nodes with a budget of 6 s. `resources/graph-profiles/1m.tsv` is a second, narrower run up to
  1 000 000 nodes with a budget of 60 s, for the operations that were still cheap at 100 000.
- **A row** is one operation at one size: wall-clock time of the operation alone (its inputs are
  built and forced first, and a major collection runs before the clock starts) and the bytes
  allocated meanwhile on every thread. A step under a quarter of a second is run three times and
  the fastest kept; a longer one is run once.
- **Nodes** are in-process no-ops whose `check` answers `Immaterial`, so an up pass runs every
  `up`. One node in ten carries a `Dynamic` marker for the rewrites to collect. Reporters are
  silent. So the times are salmon's own bookkeeping and nothing else.
- **Each series runs in a process of its own.** In a first attempt everything ran in one process,
  and every series after the first abandoned step (which leaves a supervisor's machines or a loop
  mid-pass behind) came out hundreds of times slower than it is. Those numbers were discarded.

### Shapes

| name | fixture | nodes : edges | what it stresses |
|---|---|---|---|
| chain | `Chain` | n : n-1 | depth |
| fan | `Fan` | n : n-1 | one node with n-1 dependencies |
| tree2 | `Tree 2` | n : n-1 | neither: the friendly case |
| diamonds2 | `Diamonds 2` | 3k+1 : 4k | sharing; paths double per diamond; depth 2n/3 |
| layered8x2 | `Layered 8 2` | 8l+1 : ~2n | sharing; depth n/8 |
| random8x3 | `RandomLayered 8 3` | 8l+1 : ~3n | sharing, drawn from a seed; depth n/8 |
| dense32 | `Layered 32 32` | 32l+1 : ~32n | many edges per node |

### Two size axes

An operation that reads the **expanded** graph pays per *occurrence*: per path from the root to a
node. On the four shapes that share, that number is exponential in the node count, so those series
are sized by occurrences (up to 3 x 10^7) and reach a few dozen to a few hundred nodes. They are
marked `occurrences` in the tables, and the node count is given beside the size.

An operation that reads a `Dag` pays per node. Those series are given a `Dag` built by
`Test.GraphFixture.dagOf`, which visits each node once and does not go through `foldDag`, so they
can be measured at node counts the fold could never produce (at the time; the fold is per node
now, and `dagOf` is kept because it does not go through `Dag.record`). `dagOf` is checked against
the fold at small sizes by `Test.GraphScaleSpec`.

In the 2026-10-10 tables `expand + foldDag`, `query plan` and the `serve` loop are sized by
occurrences. The benchmark now sizes them by nodes, and has a separate
`expand + foldDagPerOccurrence` series (the old fold, kept as `Test.DagReference`) sized by
occurrences.

### Reading a cell

`n^1.3, 100.0k: 1.2 s, 690 MB (cap)` is: fitted growth, the largest size the series completed,
the time and the allocation there, and why the series ended.

- **Growth** is the slope of log time against log size over the last four steps that took at
  least a millisecond. Operations that are linear in principle come out between `n^1.0` and
  `n^1.4` here (maps keyed by `Ref`, and collections over a growing heap), so read **up to about
  1.4 as "per node", and 1.8 and above as quadratic**. `too fast to fit` means fewer than three
  steps reached a millisecond.
- **Why it ended:** `cap` (the size cap), `budget` (the step took over a third of the budget, and
  the next is three times larger), `timeout at N` (the step at size N was abandoned at three times
  the budget), `heap at N` or `process: exit 251` (the 4 GB bound was hit at size N), `inputs at N`
  (building the operation's inputs at size N took over two budgets: for the renders and the client
  model that input is the listed paths, which the query group times on their own).

## Findings

Every number here is in the tables below. "Read from the code" marks an explanation that was
not itself profiled.

1. **`foldDag` pays per path, and everything that declares or prints a graph goes through it.**
   Confirmed and sized: 1.8 to 2.5 µs and about 6.4 kB allocated per occurrence. A chain
   of diamonds of **58 nodes** (2.1 M occurrences) takes 3.8 s to fold; each further diamond
   doubles it, which puts 61 nodes at about 7.5 s (extrapolated, consistent with the 7 s noted
   when the path listing was fixed). The same wall is hit by a declaration in `serve` (4.4 s at 58
   nodes) and by `query plan` (4.5 s). The outline and the selectors do **not** hit it: they are
   per node on the same shapes (100 000-node chain of diamonds outlined in 0.7 s).
2. **`Dag.stuck` is quadratic in depth, and every concurrent pass and every `startUpkeep` calls
   it.** (Fixed since: see "After `stuck` in one pass".) Chain: 2.0 s at 3 000 nodes (`n^2.2`). Diamonds: 13.6 s at 10 000. The
   concurrent walk on a chain costs what `stuck` costs (1.9 s at 3 000), so does `startUpkeep`
   (1.5 s), and so does a `serve` pass (1.9 s up, 1.8 s down, at 3 000). The sequential walk over
   the same chain is 1.0 s at 100 000. On a shallow shape the concurrent walk is fine: tree2 is
   3.3 s at 100 000 and 123 s at 1 000 000 (one thread and one `TVar` per node does fit in 4 GB
   there). *Read from the code:* `stuck` peels one layer of ready nodes per round with a full
   filter of the remaining set, so it costs depth x nodes.
3. **A node with many edges is quadratic in its edge count, in several places.** (Fixed since
   for `record`, the fold, `fromMagma`, `mergeDag`, `transposeOf` and the outline, see "After the
   edge lists"; the concurrent walk over a fan is not.)
   Fan: `foldDag` 1.3 s at 10 000 and abandoned at 30 000; `fromMagma` 2.8 s and 7.1 GB allocated
   at 10 000; `mergeDag` 2.0 s at 10 000; `Query.outline` 15 s at 30 000. A rewrite collapsing a
   tenth of the nodes into one batch: `n^1.6` to `n^1.9` (fan excepted), 1.8 s and 4.5 GB allocated at 100 000 on
   a chain, 9.0 s and 34 GB on random8x3. `upDagConcurrent` over a fan: `n^1.9`, 11 s at 30 000
   (the teardown direction is per node: 2.1 s at 100 000). *Read from the code:* `Dag.record`
   appends to a node's edge list and searches it on every insert; `fromMagma` and `transposeOf`
   build edge lists by appending on the right; `outline` tests membership in a node's child list
   with `elem`; and a node waiting on k neighbours re-reads all k `TVar`s each time one of them
   settles.
4. **The listed paths are quadratic in depth, in time and in memory, and `status`, `/dag`,
   `/status` and the status sink all carry them.** Paths per node (what `worldPaths` computes):
   chain 7.5 s and 8.1 GB allocated at 10 000; diamonds 2.9 s at 3 000; layered8x2 11 s at
   10 000. `status` inside the loop on a chain: 0.6 s at 3 000, `n^2.2`. The renders themselves
   are cheap once the paths exist (`/dag` 1.3 s at 100 000 on tree2), which is why their series
   end as `inputs`. `query show --dedupe` has the same shape (10 s at 10 000 on a chain). *Read
   from the code:* a path is as long as the node is deep and each of up to eight per node is a
   fresh list, so a deep graph holds nodes x depth segments.
5. **`Client.Model.step` was quadratic over a teardown (fixed since, see "Since fixed").** One `done` event per node wanted down:
   1.5 to 2.0 s and 4.3 GB allocated at 10 000 nodes on the five shapes that got there, abandoned
   at 30 000 on fan and tree2. Folding
   the same number of events for a bring-up is per node (0.3 s at 100 000). *Read from the code:*
   each removal filters the whole `modelOrder` list.
6. **A supervisor does not fit 100 000 nodes in 4 GB.** `startUpkeep` over nodes already up: 1.6 s
   and 1.0 GB allocated at 30 000 (fan), 2.0 s at 30 000 (tree2); at 100 000 the heap bound was
   hit on both. Over managed nodes on a fan it is abandoned at 3 000. A `serve` loop over tree2
   hit the heap bound at 100 000 as well (260 ms to record and 1.1 s for the first pass at
   30 000). **Explained and fixed since** (32 KiB of thread stack a machine, from registering
   its nap timer): see "Since fixed".
7. **Per node, where nothing above applies:** the sequential walks cost about 8 to 10 µs a node at 100 000
   (25 s at 1 000 000), `fromMagma` and `mergeDag` 10 to 14 µs, expansion alone 0.3 to 1.5 µs an
   occurrence, the ledger reads well under a microsecond a node. A rewrite of sixteen batches
   costs sixteen collapses of the whole graph (5 to 9 s at 100 000).

### Candidate follow-ups

One per finding worth fixing, in the order the numbers suggest. The first, second, third, fifth and sixth are done.

1. `foldDag` per node instead of per occurrence (finding 1): it bounds `run up`/`down`/`tree`/
   `dag`, `query plan` and every `serve` declaration at a few dozen nodes of diamond chain.
   **Done since, under a precondition and with two accepted behaviour changes: see "After the
   per-node fold".**
2. `Dag.stuck` in one pass (finding 2): it bounds every concurrent pass and every tending start
   on a deep graph at a few thousand nodes. **Done**: see "After `stuck` in one pass".
3. Edge lists that do not search or append on the right, in `Dag.record`, `fromMagma`,
   `transposeOf` and `Query.outline` (finding 3). **Done**: see "After the edge lists". The
   wake-up cost of a node waiting on many neighbours is a separate question, not touched.
4. Listed paths that do not cost depth per node (finding 4): shared prefixes, a length cap, or
   listing on request. **Done, as a length cap: see "After the path cap".**
5. `Client.Model.step` removing a node without filtering the order list (finding 5). **Done.**
6. What a tended node retains (finding 6). **Done**: see "Since fixed".

### Since fixed

**Finding 5, `Client.Model.step` over a teardown** (2026-10-11). The model's display order was a
list and each removal filtered it; it is now `Model.Order` (seat number to ref, and each ref's
seats), so a removal is a lookup and the refs that stay keep their place. The same change was
made to the Rust port (`rs/salmon-serve-client`, `Vec::retain` before). The web UI's fold
(`salmon-ops/ui/ui.js`) never had the cost: it does not remove a node on an event at all, its
order is only rebuilt by a `/dag` read.

The two `Model.step` series were run again, before and after, the same day on the same machine
(one run each side; load average about 2 for the first and 3.5 to 5 for the second, so as
indicative as the rest), budget 6 s:

| teardown, one `done` per node wanted down | before | after |
|---|---|---|
| chain, 3 000 | 159 ms, 355 MB | 5 ms, 6 MB |
| chain, 10 000 | 2.2 s, 4.3 GB | 22 ms, 21 MB |
| fan, 10 000 | 2.1 s, 4.3 GB | 27 ms, 21 MB |
| tree2, 10 000 | 2.2 s, 4.3 GB | 26 ms, 21 MB |
| tree2, 30 000 | not reached (budget) | 102 ms, 67 MB |
| tree2, 100 000 | not reached (budget) | 507 ms, 247 MB |

Fitted growth went from `n^2.0`-`n^2.3` to `n^1.2`-`n^1.3` where there were enough steps to fit,
which is "per node" as this document reads it; the allocation, which the load does not move, went
from 4.3 GB to 21 MB at 10 000. A teardown now costs about what folding the same number of events
for a bring-up costs (422 ms and 158 MB at 100 000 on tree2 in the same run), plus half again in
allocation. The rows are `resources/graph-profiles/client-step-before.tsv` and
`client-step-after.tsv`; the tables further down are still the original full run and still show
the old teardown row.

What this does not say:

- The other shapes stop at 3 000 or 10 000 nodes after the change as before it, because
  *building their input* (the listed paths, finding 4) is what runs out, not the fold. How far a
  series gets on that account moved with the load between runs.
- Only the Haskell fold was timed. The Rust one was not, before or after.
- `Model.fromDag` now seats the order as well as keying the nodes. Its series was run after the
  change only (`client-fromdag-after.tsv`), so the comparison is with the original run of the day
  before under another load: allocation is up by 11 to 15 % (27 to 31 MB at 10 000 on a chain, 289
  to 322 MB at 100 000 on tree2), and the times (25 to 33 ms, 470 to 802 ms) are not comparable.

**Finding 6, what a tended node retains** (2026-10-15). A heap profile by closure type (`+RTS
-hT`) of `startUpkeep` over 30 000 settled nodes of tree2 put 949 MB of the 1.05 GB live in
`STACK` objects: 31.6 kB a machine, the runtime's 32 KiB stack chunk (`+RTS -kc`). Cloning the
machines' stacks (`GHC.Stack.CloneStack`, `ghc-heap`'s `decodeStack`) showed a machine at rest
is 15 frames, inside the 1 KiB chunk a thread starts with; yet from about a thousand nodes up
nearly every machine's only chunk was a 4 093-word one. The cause is `registerDelay`, which each
machine calls for every nap: it inserts into the runtime's timer queue *on the calling thread*,
deeper the more timeouts are outstanding. A machine overflowing its first chunk there is moved,
frames and all, to a 32 KiB chunk, and never returns past those frames, so it keeps it. (A bare
program of threads that each `registerDelay` and block shows the same: 2 % of them at 10 000,
and 100 000 of them exhaust a 1 GB heap.) The nap's timeout is now registered on a short-lived
thread of its own (`Upkeep.napTimer`, one `forkIO` a nap); the machine keeps its 1 KiB chunk.
After, at 30 000: 115 MB live, `STACK` 27 MB, about 3.9 kB a node, the largest remaining item
being closures (`FUN`, 24 MB).

Run, before (master `8184fb1`) and after, same machine as above, 2026-10-15, load average 4 to 19
from other work (the first, wider run was during the higher load), budget 6 s, heap 4 GB:

```sh
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--max-size 100000 --budget 6 \
  --heap 4g --only "tend nodes" --only "bring nodes" --shape tree2 --out rows.tsv'
```

twice each side, alternating (`resources/graph-profiles/upkeep-retention-{before,after}.tsv`), and
once more over `upkeep` and `loop` on chain, fan and tree2 (not checked in). Peak memory is the
series process's own `max_mem_in_use_bytes`.

| tree2 | before | after |
|---|---|---|
| settled, `startUpkeep` at 30 000 | 5.9 s / 4.2 s, 1.3 / 1.1 GB allocated | 2.1 s / 1.8 s, 322 MB allocated |
| settled, largest size reached | 30 000 (budget, both runs); heap bound at 100 000 in the wide run | 100 000: 8.8 s, 1.1 GB allocated |
| settled, peak memory in use | 1 144 MiB at 30 000 | 250 MiB at 30 000, 1 016 MiB at 100 000 |
| unsettled, until every machine is stable at 30 000 | 4.7 s / 4.1 s, 1.0 / 1.1 GB allocated | 1.5 s / 1.4 s, 195 MB allocated |
| unsettled, largest size reached | 30 000 (budget); heap bound at 100 000 in the wide run | 30 000 (budget once, timeout at 100 000 once) |

Fan, settled, in the wide run: 30 000 and 1 147 MiB before, 100 000 and 1 008 MiB after. The
`serve` loop over tree2 no longer hits the heap bound at 100 000 (`exit 251` before); it stops
on the budget at 30 000, its first pass taking 2.5 s.

What this does not say:

- **Times are noisy; the memory is not.** The load moved by a factor of four between runs and
  some "after" times in the wide run are slower than "before" for series the change cannot
  touch (`loop: clear`, `record a declaration`). Read the allocation and the peak memory.
- **Unsettled at 100 000 is still not reached**, now by time rather than memory. Not
  investigated.
- **The managed series and chain are unchanged in regime**: chain is bounded by `Dag.stuck`
  (finding 2) at a few thousand nodes before memory matters, and a managed node holds its
  action's thread too.
- *Not run:* real nodes, a failing or restarting node, `+RTS -N`, anything above 100 000.

## After the per-node fold

`Dag.foldDag` no longer visits every occurrence. It recognises an occurrence by its `Ref`, its
description (as far as `sameRepresentative` can tell) and the `Ref`s it depends on, and does not
walk one it has already walked, nor anything below it. The precondition ("coherent
declarations"), the two accepted differences (one `Conflicting` per declaration that lost
instead of one per flip; a difference below two declarations that look alike is not seen) and
the tests are in `resources/module-notes.md` under `Op/Dag.hs`.

**What was run.** The three operations finding 1 names, before (master `61e84b6`) and after, on
the same machine as above, 2026-10-11, load average 2 to 7 from other work (the "before" run
started at a load of about 6 and came out about twice as slow as the 2026-10-10 run on the same
code: 7.4 s where that run had 3.8 s; the "after" run ended at about 2). One run each, budget 6 s, cap 100 000 nodes:

```sh
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--max-size 100000 --budget 6 \
  --heap 4g --only "expand + foldDag" --only "query plan" --only "loop" --out rows.tsv'
```

The rows are `resources/graph-profiles/folddag-per-node-before.tsv` and
`folddag-per-node-after.tsv`. **Indicative, like everything here:** what they are worth is the
change of regime (a series that stopped at a few dozen nodes now reaches the cap, with a fitted
growth of n^1.1 to n^1.3), not the ratios between two cells, which the load moves by a factor of
two. "Before" is sized by occurrences, "after" by nodes, so the cells are the largest size each
reached.

| operation | | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|
| expand + foldDag | before | 58 nodes: 7.4 s, 13.4 GB | 145 nodes: 7.1 s, 14.0 GB | 89 nodes: 2.5 s, 4.5 GB | 129 nodes: 5.0 s, 7.5 GB |
| | after | n^1.2, 100.0k: 2.1 s, 1.6 GB | n^1.2, 100.0k: 2.0 s, 2.6 GB | n^1.1, 100.0k: 3.3 s, 5.0 GB | n^1.4, 3.0k: 7.3 s, 12.5 GB |
| query plan | before | 52 nodes: 2.1 s, 3.3 GB | 129 nodes: 2.1 s, 3.4 GB | 89 nodes: 3.0 s, 4.5 GB | 129 nodes: 6.0 s, 7.5 GB |
| | after | n^1.2, 100.0k: 3.8 s, 2.3 GB | n^1.2, 100.0k: 3.8 s, 3.5 GB | n^1.2, 100.0k: 6.1 s, 6.1 GB | n^1.5, 993: 2.5 s, 4.2 GB |
| serve: record a declaration | before | 58 nodes: 7.9 s, 13.4 GB | 129 nodes: 2.6 s, 3.4 GB | 89 nodes: 3.2 s, 4.5 GB | 129 nodes: 5.5 s, 7.5 GB |
| | after | 3.0k: 30 ms, 46 MB | 10.0k: 201 ms, 265 MB | 3.0k: 75 ms, 146 MB | 3.0k: 6.0 s, 12.6 GB |

What the numbers say, and what they do not:

- **The wall of finding 1 is gone on the shapes that share.** A chain of diamonds folds at
  100 000 nodes in about 2 s (it stopped at 58), and `query plan` with it. The `serve` series
  now end on the *other* findings, not on the declaration: the passes (`stuck`, finding 2) and
  `status` (the listed paths, finding 4: 3.8 s and 3.9 GB allocated at 3 000 nodes of diamond
  chain, which could not be reached before). Those are not touched here.
- **Shapes with no sharing pay a little more.** In the "after" run, the same process measured
  the old fold beside the new one: chain at 100 000, 0.71 s and 922 MB old against 1.0 s and
  1.0 GB new; tree2 0.79 s against 1.1 s. So roughly 15 to 40 % slower and 10 % more
  allocation where there is nothing to skip (the table of walked occurrences), within a run
  whose noise is of the same order.
- **dense32 is per node but expensive: about 2 ms and 4 MB a node.** An occurrence can only be
  recognised after reading its predecessors' `Ref`s, so a node costs its dependencies times
  their dependencies (32 x 32 here), and in this fixture reading a `Ref` builds the node. It
  went from 129 nodes to 3 000 in the same time, and it is never more than the old fold paid
  (every occurrence looked at now was visited then), but it is not cheap. *Read from the code,
  not profiled.*
- **fan is unchanged** (`n^2.0`, 30 000 in 9 to 15 s): that is `Dag.record`'s edge list, finding 3.
- **Not measured:** the 1 000 000-node run was not repeated; `optCollisions` (the backward walk
  that settles contested `Ref`s only runs when there are some, and no benchmark series has
  any); the full benchmark (only the three operations above); a quiet machine.

## After the edge lists

`Dag.record`, `mergeDag`, `foldDag` and `fromMagma` build edge lists through a set beside each
touched list (an insert is a `Set.member` and a cons, the lists are reversed back once at the
end); `fromMagma`'s incoming lists and `transposeOf` cons and reverse instead of appending on the
right; `Query.outline` asks a set, not its child list, whether a node is already under another.
The lists, their order, and everything rendered from them are unchanged (`Test.EdgeListSpec`
compares each with the appending version, kept as `Test.EdgeListReference`; see
`resources/module-notes.md` under `Op/Dag.hs`, "Edge lists").

**What was run.** The operations finding 3 names, before (master `0c9cecf`) and after, on the
same machine as above, 2026-10-16, one run each, budget 6 s, cap 100 000 nodes, load average 2 to
4 from other work in both runs (no build of this change ran alongside either):

```sh
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--max-size 100000 --budget 6 \
  --heap 4g --only "expand + foldDag" --only "fromMagma" --only "mergeDag into an empty Dag" \
  --only "mergeDag onto itself" --only "one batch of a tenth" --only "outline" --out rows.tsv'
```

The rows are `resources/graph-profiles/edge-lists-before.tsv` and `edge-lists-after.tsv` (the
`--only` texts are substrings, so they also hold `foldDagPerOccurrence` and "paths per node"
rows). **Indicative**, as everything here: one run each on a shared machine.

| operation | | chain | fan | tree2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|
| expand + foldDag | before | n^1.3, 100.0k: 993 ms, 1.0 GB | n^2.1, 30.0k: 12.6 s, 25.5 GB | n^1.3, 100.0k: 1.6 s, 1.0 GB | n^1.1, 100.0k: 5.0 s, 5.0 GB | n^1.4, 3.0k: 6.8 s, 12.5 GB |
| | after | n^1.3, 100.0k: 1.1 s, 920 MB | n^1.3, 100.0k: 1.4 s, 1.1 GB | n^1.3, 100.0k: 1.3 s, 926 MB | n^1.2, 100.0k: 3.6 s, 4.6 GB | n^1.3, 3.0k: 5.3 s, 12.3 GB |
| fromMagma | before | n^1.3, 100.0k: 693 ms, 690 MB | n^2.0, 10.0k: 2.1 s, 7.1 GB | n^1.3, 100.0k: 1.5 s, 688 MB | n^1.3, 100.0k: 3.3 s, 1.4 GB | n^1.3, 30.0k: 6.6 s, 5.4 GB |
| | after | n^1.3, 100.0k: 740 ms, 552 MB | n^1.2, 100.0k: 424 ms, 602 MB | n^1.3, 100.0k: 790 ms, 546 MB | n^1.3, 100.0k: 2.2 s, 983 MB | n^1.3, 30.0k: 4.9 s, 2.3 GB |
| mergeDag into an empty Dag | before | n^1.3, 100.0k: 684 ms, 516 MB | n^2.0, 10.0k: 1.4 s, 2.8 GB (timeout at 30.0k) | n^1.4, 100.0k: 1.4 s, 521 MB | n^1.3, 100.0k: 1.9 s, 1.0 GB | n^1.0, 30.0k: 1.8 s, 3.8 GB (timeout at 100.0k) |
| | after | n^1.4, 100.0k: 814 ms, 399 MB | n^1.3, 100.0k: 1.0 s, 464 MB | n^1.3, 100.0k: 950 ms, 402 MB | n^1.3, 100.0k: 1.4 s, 634 MB | n^1.1, 100.0k: 6.5 s, 4.7 GB |
| mergeDag onto itself | before | n^1.3, 100.0k: 669 ms, 549 MB | n^1.8, 10.0k: 2.1 s, 45 MB | n^1.3, 100.0k: 1.1 s, 551 MB | n^1.3, 100.0k: 1.9 s, 1.0 GB | n^1.2, 30.0k: 1.6 s, 2.1 GB (timeout at 100.0k) |
| | after | n^1.4, 100.0k: 953 ms, 387 MB | n^1.4, 100.0k: 1.2 s, 444 MB | n^1.4, 100.0k: 1.1 s, 390 MB | n^1.3, 100.0k: 1.6 s, 612 MB | n^1.2, 30.0k: 1.7 s, 1.3 GB (timeout at 100.0k) |
| rewrite: one batch of a tenth | before | n^1.6, 100.0k: 1.1 s, 4.5 GB | n^1.4, 100.0k: 1.1 s, 275 MB | n^1.6, 100.0k: 2.4 s, 4.5 GB | n^1.9, 30.0k: 2.4 s, 3.1 GB | n^1.6, 30.0k: 8.7 s, 33.7 GB |
| | after | n^1.4, 100.0k: 396 ms, 230 MB | n^1.3, 100.0k: 437 ms, 281 MB | n^1.3, 100.0k: 435 ms, 231 MB | n^1.4, 100.0k: 955 ms, 475 MB | n^1.3, 30.0k: 1.6 s, 1.2 GB (timeout at 100.0k) |
| outline | before | n^1.2, 100.0k: 352 ms, 626 MB | n^2.0, 30.0k: 15.0 s, 164 MB | n^1.3, 100.0k: 967 ms, 634 MB | n^1.1, 100.0k: 3.0 s, 3.9 GB | n^1.4, 3.0k: 3.0 s, 11.1 GB |
| | after | n^1.2, 100.0k: 370 ms, 637 MB | n^1.3, 100.0k: 531 ms, 633 MB | n^1.2, 100.0k: 485 ms, 647 MB | n^1.1, 100.0k: 2.1 s, 4.0 GB | n^1.4, 3.0k: 2.7 s, 11.1 GB |

What the numbers say, and what they do not:

- **The fan is per node in every operation finding 3 named.** The fold went from 30 000 nodes in
  12.6 s (`n^2.1`) to the cap in 1.4 s, `fromMagma` from 10 000 in 2.1 s and 7.1 GB to the cap
  in 0.4 s, `mergeDag` from a timeout at 30 000 to the cap in about 1 s, the outline from 30 000
  in 15 s to the cap in 0.5 s. The paths per node over a fan, which build an outline first, went
  from 30 000 in 8.0 s to the cap in 1.3 s; on the other shapes that series is still finding 4
  (the listed paths), untouched.
- **The rewrite gained the most elsewhere.** A collapse transposes the whole dependency map once
  (`transposeOf`), which appended on the right: chain at 100 000 from 1.1 s and 4.5 GB to 0.4 s
  and 230 MB; random8x3 from 30 000 (`n^1.9`) to the cap in under a second; dense32 at 30 000 from
  8.7 s and 33.7 GB to 1.6 s and 1.2 GB. Sixteen batches still cost sixteen collapses (not
  re-measured).
- **dense32's `mergeDag` into an empty `Dag` now reaches 100 000** (6.5 s, 4.7 GB) where it timed
  out; onto itself it still times out there.
- **Where nothing was quadratic, within the noise.** Chain and tree2 differ by -40 % to +40 %
  between the two runs, both ways, with allocation the same or lower (`fromMagma` and `mergeDag`
  allocate 20 to 25 % less). A single `record` call (not a series here) now goes through the
  set-backed form and back, so one call on a node with many edges pays a `Set.fromList` of its
  list where it used to pay one `elem`: no driver calls it in a loop any more (only
  `Test.DagReference` does).
- **Not measured:** the 1 000 000-node run; the concurrent walk over a fan (the wake-ups,
  outside this change); a quiet machine.

## After `stuck` in one pass

`Dag.stuck` keeps a count of outstanding waits per node in an unboxed array and releases a
node's waiters once, when it becomes ready, instead of re-filtering every remaining node once per
level of depth. Same answer (`Test.DagStuckSpec` compares it with the rounds, kept as
`Test.DagReference.stuckRounds`; see `resources/module-notes.md` under `Op/Dag.hs`, "`stuck` in
one pass").

**What was run.** One run, 2026-10-16, same machine as above, the rounds and the one pass in the
same run (the benchmark has a `stuckRounds` series for this), budget 10 s, cap 100 000 nodes,
load average 2.6 to 4 from other work; then the concurrent walks and `startUpkeep` on two deep
shapes, budget 6 s, load average 7 to 11 (something else ran meanwhile, so those times are
pessimistic):

```sh
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--max-size 100000 --budget 10 \
  --only stuck --out rows.tsv'
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--max-size 100000 --budget 6 \
  --only DagConcurrent --only "tend nodes already up" --shape chain --shape diamonds2 --out rows.tsv'
```

The rows are `resources/graph-profiles/stuck-one-pass.tsv`. **Indicative**: one run on a shared
machine.

| operation | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|
| stuck, rounds (before) | n^2.3, 3.0k: 2.1 s (timeout at 10.0k) | n^1.2, 100.0k: 154 ms | n^1.3, 100.0k: 464 ms | n^2.3, 10.0k: 20.7 s | n^2.2, 10.0k: 4.5 s | n^2.2, 10.0k: 4.6 s | n^2.1, 30.0k: 20.9 s |
| stuck, one pass, up | n^1.3, 100.0k: 183 ms, 59 MB | n^1.2, 100.0k: 153 ms, 100 MB | n^1.3, 100.0k: 169 ms, 61 MB | n^1.3, 100.0k: 213 ms, 69 MB | n^1.3, 100.0k: 278 ms, 87 MB | n^1.3, 100.0k: 497 ms, 119 MB | n^1.4, 100.0k: 7.5 s, 1.3 GB |
| stuck, one pass, down | n^1.3, 100.0k: 176 ms, 59 MB | n^1.1, 100.0k: 72 ms, 59 MB | n^1.3, 100.0k: 164 ms, 59 MB | n^1.3, 100.0k: 222 ms, 69 MB | n^1.3, 100.0k: 265 ms, 87 MB | n^1.4, 100.0k: 506 ms, 120 MB | n^1.3, 100.0k: 6.0 s, 1.3 GB |

By depth, waiting on dependencies (seconds):

| nodes | chain, rounds | chain, one pass | diamonds2, rounds | diamonds2, one pass | dense32, rounds | dense32, one pass |
|---|---|---|---|---|---|---|
| 1 000 | 0.167 | 0.0005 | 0.112 | 0.0006 | 0.014 | 0.013 |
| 3 000 | 2.08 | 0.0020 | 1.37 | 0.0023 | 0.117 | 0.057 |
| 10 000 | (timeout) | 0.0086 | 20.7 | 0.011 | 1.60 | 0.21 |
| 30 000 | | 0.038 | | 0.053 | 20.9 | 0.81 |
| 100 000 | | 0.18 | | 0.21 | | 7.5 |

What the drivers gained (before: the "Up to 100 000 nodes" tables):

| operation | chain, before | chain, after | diamonds2, before | diamonds2, after |
|---|---|---|---|---|
| upDagConcurrent | 3.0k: 1.9 s (timeout at 10.0k) | 3.0k: 82 ms; 100.0k: 8.8 s | 10.0k: 13.5 s | 10.0k: 0.55 s; 30.0k: 2.7 s |
| downDagConcurrent | 3.0k: 1.8 s (timeout at 10.0k) | 3.0k: 73 ms; 30.0k: 2.2 s | 10.0k: 12.4 s | 10.0k: 0.75 s; 100.0k: 9.6 s |
| startUpkeep (settled) | 3.0k: 1.5 s | 3.0k: 0.38 s; 30.0k: 4.2 s | | 10.0k: 1.2 s; 30.0k: 3.2 s |

What the numbers say, and what they do not:

- **Depth no longer matters.** The one pass is `n^1.1` to `n^1.4` on every shape (the exponent
  above 1 is the `log n` of numbering the `Ref`s and the collector, as for every other per-node
  series here), and the chain and the chain of diamonds, which stopped at 3 000 and 10 000, reach
  the cap in about 0.2 s. dense32 has 32 edges per node, so its 100 000 nodes are 3.2 million
  waits: 7.5 s is about 2 µs per wait.
- **Where the rounds were already cheap, no worse.** On a fan (one round) the two are equal going
  up and the one pass is three times faster coming down; tree2 (seventeen rounds) is three times
  faster.
- **The concurrent walks and `startUpkeep` are now bounded by something else.** The concurrent
  walk on a chain went from 1.9 s at 3 000 to 82 ms; it still grows faster than linearly (one
  thread and one `TVar` per node, finding 3's wake-ups on a node with many neighbours), and its
  series end on the budget between 30 000 and 100 000. Those times were taken at a high load
  average.
- **Not measured:** the `serve` passes (they go through the concurrent walk and were not re-run);
  the 1 000 000-node run; a quiet machine.

## After the path cap

`Query.outlinePaths` keeps a path in full only while it renders in at most `Query.pathCap` (256)
characters; a longer one keeps its length and the tail that fits, and `serve` shows it as
`**/tail`. Paths at or under the cap are listed as before (same set, order and positions:
`Test.ListedPathsSpec` against `Test.PathListReference`, the function as it was). `query show` is
not capped. See `resources/module-notes.md`, "Listed paths have a length cap", for the choices.

**What was run.** One run, 2026-10-16, same machine, budget 6 s, cap 100 000 nodes, load average
2 to 5 from other work:

```sh
cabal bench salmon-ops-recipes:graph-profiles --benchmark-options='--max-size 100000 --budget 6 \
  --heap 4g --only "paths per node" --only "GET /dag" --only "GET /status" \
  --only "status sink document" --only "loop" --out rows.tsv'
```

The rows are `resources/graph-profiles/path-cap.tsv`. "Before" for the paths per node is the
uncapped reference measured in the same process run (a new row, `paths per node, uncapped
reference`); for `status` in the loop it is the 2026-10-10 table above (another day, another
load). **Indicative**, as everything here.

| operation | | chain | diamonds2 | layered8x2 | random8x3 |
|---|---|---|---|---|---|
| paths per node | uncapped | n^2.2, 10.0k: 15.4 s, 10.9 GB | n^2.1, 3.0k: 3.9 s, 4.1 GB | n^2.1, 10.0k: 17.9 s, 10.5 GB | n^1.8, 3.0k: 2.7 s, 1.3 GB |
| | capped | n^1.2, 100.0k: 4.5 s, 1.8 GB | n^1.0, 30.0k: 2.4 s, 3.2 GB | n^1.1, 30.0k: 3.7 s, 4.5 GB | n^1.2, 10.0k: 2.3 s, 2.3 GB |
| loop: status | 2026-10-10 | n^2.2, 3.0k: 595 ms, 730 MB | 58 nodes (fold) | 145 nodes (fold) | 89 nodes (fold) |
| | capped | n^1.2, 3.0k: 48 ms, 44 MB | n^1.1, 3.0k: 210 ms, 340 MB | n^1.3, 10.0k: 1.8 s, 1.5 GB | n^1.5, 3.0k: 558 ms, 583 MB |

What the numbers say, and what they do not:

- **The listed paths are per node on the deep shapes.** The chain went from 10 000 nodes in 15 s
  and 11 GB (`n^2.2`) to 100 000 in 4.5 s (`n^1.2`); layered8x2 from 10 000 in 18 s to 30 000 in
  3.7 s. fan and tree2 are shallow and were already per node; dense32 stops at about 1 000 on
  both, on its outline (finding 3's note on dense32), not on the paths.
- **`status` inside the loop is no longer the wall on a chain**: 48 ms at 3 000 where it was
  0.6 s; the loop series now end on the passes (`stuck`, finding 2) and the declaration timeout.
- **`/dag`, `/status` and the status sink reach the cap on chain and diamonds2** (1.9 s, 0.5 s
  and 0.6 s at 100 000 on a chain); on layered8x2, random8x3 and dense32 they still end as
  `inputs`, building the outline and the paths, not rendering them.
- **Still costs depth per node:** `query show --dedupe` (`outlineFirstPaths`, not capped by
  choice) was not re-measured.
- **Not measured:** the 1 000 000-node run; a quiet machine.

## Not measured

- `Help.printTree`, `Dot.printCograph` and `Query.pathedRefs` on their own: none is on a command
  path any more except `query show` without `--dedupe`, which is measured.
- Colliding `Ref`s (`optCollisions`) and the `Connect`/`Overlay` edge forms of the fixture.
- `Model.step` for the `upkeep` and `output` event kinds; `Model.rebase`.
- The HTTP server itself, the event ring, the socket listener, TLS, pull mode: only the pure
  renders behind `/dag` and `/status` and the status sink's document were timed.
- A supervisor that is tending: only start, settle and stop (and, for finding 6, a heap profile
  of the settled series at 30 000). No node here ever fails, restarts,
  or is watched by a real `check`.
- More than one capability (`+RTS -N`), profiling builds, and any real node (no process is
  spawned, no file is written).
- Above 100 000 nodes, only the series in the last table.

## Up to 100 000 nodes

Budget 6 s. From `resources/graph-profiles/100k.tsv`.

### fold

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| expand (evalDeps, every occurrence visited) | occurrences | n^1.3, 100.0k: 82 ms, 112 MB (cap) | n^1.0, 100.0k: 26 ms, 116 MB (cap) | n^1.2, 100.0k: 26 ms, 118 MB (cap) | n^1.0, 16.8M (67 nodes): 4.0 s, 19.5 GB (budget) | n^1.0, 16.8M (169 nodes): 3.9 s, 21.7 GB (budget) | n^1.0, 19.1M (113 nodes): 5.8 s, 25.5 GB (budget) | too fast to fit, 1.1M (129 nodes): 179 ms, 1.4 GB (cap) |
| expand + foldDag | occurrences | n^1.3, 100.0k: 1.3 s, 922 MB (cap) | n^1.8, 10.0k: 1.3 s, 2.9 GB (timeout at 30.0k) | n^1.3, 100.0k: 1.0 s, 928 MB (cap) | n^1.0, 2.1M (58 nodes): 3.8 s, 13.4 GB (budget) | n^1.0, 2.1M (145 nodes): 4.1 s, 14.0 GB (budget) | n^1.0, 2.1M (97 nodes): 4.2 s, 13.9 GB (budget) | n^0.9, 1.1M (129 nodes): 2.8 s, 7.5 GB (budget) |
| dagOf (the fixture's per-node build, for comparison) | nodes | n^1.3, 100.0k: 1.4 s, 825 MB (cap) | n^1.3, 100.0k: 2.0 s, 906 MB (cap) | n^1.3, 100.0k: 912 ms, 834 MB (cap) | n^1.3, 100.0k: 1.1 s, 980 MB (cap) | n^1.3, 100.0k: 1.2 s, 1.3 GB (cap) | n^1.2, 100.0k: 1.5 s, 1.8 GB (cap) | n^1.1, 30.0k: 2.6 s, 4.4 GB (budget) |

### dag

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| fromMagma | nodes | n^1.3, 100.0k: 1.2 s, 690 MB (cap) | n^2.0, 10.0k: 2.8 s, 7.1 GB (budget) | n^1.4, 100.0k: 1.1 s, 688 MB (cap) | n^1.3, 100.0k: 1.0 s, 803 MB (cap) | n^1.3, 100.0k: 1.4 s, 1.0 GB (cap) | n^1.3, 100.0k: 2.2 s, 1.4 GB (budget) | n^1.3, 30.0k: 6.2 s, 5.4 GB (budget) |
| mergeDag into an empty Dag | nodes | n^1.4, 100.0k: 1.1 s, 516 MB (cap) | n^2.0, 10.0k: 2.0 s, 2.8 GB (timeout at 30.0k) | n^1.3, 100.0k: 952 ms, 521 MB (cap) | n^1.3, 100.0k: 1.0 s, 599 MB (cap) | n^1.3, 100.0k: 997 ms, 766 MB (cap) | n^1.3, 100.0k: 1.3 s, 1.0 GB (cap) | n^1.1, 30.0k: 2.0 s, 3.8 GB (budget) |
| mergeDag onto itself | nodes | n^1.3, 100.0k: 1.1 s, 549 MB (cap) | n^1.9, 30.0k: 7.9 s, 152 MB (budget) | n^1.3, 100.0k: 792 ms, 551 MB (cap) | n^1.4, 100.0k: 1.0 s, 630 MB (cap) | n^1.3, 100.0k: 1.0 s, 787 MB (cap) | n^1.4, 100.0k: 2.0 s, 1.0 GB (budget) | n^1.2, 30.0k: 1.9 s, 2.1 GB (timeout at 100.0k) |
| dagEdges | nodes | n^1.4, 100.0k: 188 ms, 88 MB (cap) | n^1.4, 100.0k: 168 ms, 88 MB (cap) | n^1.3, 100.0k: 119 ms, 88 MB (cap) | n^1.4, 100.0k: 183 ms, 116 MB (cap) | n^1.4, 100.0k: 319 ms, 172 MB (cap) | n^1.3, 100.0k: 595 ms, 262 MB (cap) | n^1.5, 30.0k: 2.4 s, 910 MB (budget) |
| roots + leaves | nodes | n^1.3, 100.0k: 161 ms, 5 MB (cap) | n^1.2, 100.0k: 105 ms, 10 MB (cap) | n^1.3, 100.0k: 97 ms, 8 MB (cap) | n^1.3, 100.0k: 95 ms, 5 MB (cap) | n^1.4, 100.0k: 100 ms, 5 MB (cap) | n^1.3, 100.0k: 106 ms, 5 MB (cap) | n^1.3, 100.0k: 125 ms, 5 MB (cap) |
| stuck (waiting on dependencies) | nodes | n^2.2, 3.0k: 2.0 s, 5 MB (budget) | n^1.3, 100.0k: 153 ms, 72 MB (cap) | n^1.3, 100.0k: 316 ms, 95 MB (cap) | n^2.2, 10.0k: 13.6 s, 19 MB (budget) | n^2.1, 10.0k: 2.5 s, 18 MB (budget) | n^2.2, 10.0k: 2.4 s, 18 MB (budget) | n^2.2, 30.0k: 10.3 s, 56 MB (budget) |
| stuck (waiting on dependants) | nodes | n^2.2, 3.0k: 1.9 s, 5 MB (timeout at 10.0k) | n^1.3, 100.0k: 216 ms, 72 MB (cap) | n^1.4, 100.0k: 1.8 s, 100 MB (cap) | n^2.3, 10.0k: 13.7 s, 19 MB (budget) | n^2.2, 10.0k: 2.4 s, 18 MB (budget) | n^2.2, 10.0k: 2.3 s, 18 MB (budget) | n^2.0, 30.0k: 8.9 s, 56 MB (budget) |

### walk

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| upDag (sequential) | nodes | n^1.3, 100.0k: 1.0 s, 292 MB (cap) | n^1.3, 100.0k: 824 ms, 219 MB (cap) | n^1.4, 100.0k: 819 ms, 291 MB (cap) | n^1.4, 100.0k: 814 ms, 319 MB (cap) | n^1.3, 100.0k: 785 ms, 380 MB (cap) | n^1.3, 100.0k: 995 ms, 468 MB (cap) | n^1.2, 30.0k: 791 ms, 828 MB (timeout at 100.0k) |
| downDag (sequential) | nodes | n^1.4, 100.0k: 1.0 s, 274 MB (cap) | n^1.3, 100.0k: 895 ms, 268 MB (cap) | n^1.3, 100.0k: 764 ms, 270 MB (cap) | n^1.4, 100.0k: 848 ms, 301 MB (cap) | n^1.4, 100.0k: 907 ms, 361 MB (cap) | n^1.4, 100.0k: 2.0 s, 449 MB (budget) | n^1.3, 30.0k: 1.3 s, 822 MB (timeout at 100.0k) |
| upDagConcurrent | nodes | n^2.1, 3.0k: 1.9 s, 24 MB (timeout at 10.0k) | n^1.9, 30.0k: 11.0 s, 259 MB (budget) | n^1.4, 100.0k: 3.3 s, 743 MB (budget) | n^2.2, 10.0k: 13.5 s, 83 MB (budget) | n^2.0, 10.0k: 2.8 s, 80 MB (budget) | n^1.9, 10.0k: 4.3 s, 91 MB (budget) | n^1.7, 30.0k: 13.9 s, 1.6 GB (budget) |
| downDagConcurrent | nodes | n^2.1, 3.0k: 1.8 s, 22 MB (timeout at 10.0k) | n^1.2, 100.0k: 2.1 s, 640 MB (budget) | n^1.4, 100.0k: 4.2 s, 742 MB (budget) | n^2.2, 10.0k: 12.4 s, 82 MB (budget) | n^1.9, 10.0k: 3.1 s, 83 MB (budget) | n^2.0, 10.0k: 3.9 s, 86 MB (budget) | n^1.4, 10.0k: 1.6 s, 493 MB (timeout at 30.0k) |

### upkeep

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| tend nodes already up (settled): startUpkeep | nodes | n^1.9, 3.0k: 1.5 s, 113 MB (timeout at 10.0k) | n^1.3, 30.0k: 1.6 s, 994 MB (heap at 100.0k) | n^1.3, 30.0k: 2.0 s, 1.3 GB (heap at 100.0k) | n^2.1, 10.0k: 13.0 s, 413 MB (budget) | n^1.7, 10.0k: 3.0 s, 411 MB (budget) | n^1.8, 10.0k: 4.1 s, 411 MB (budget) | n^1.5, 10.0k: 1.7 s, 412 MB (timeout at 30.0k) |
| tend nodes already up (settled): until every machine is stable | nodes | too fast to fit, 3.0k: 432 us, 626 kB (timeout at 10.0k) | too fast to fit, 30.0k: 17 ms, 8 MB (heap at 100.0k) | too fast to fit, 30.0k: 13 ms, 7 MB (heap at 100.0k) | too fast to fit, 10.0k: 2 ms, 2 MB (budget) | too fast to fit, 10.0k: 2 ms, 2 MB (budget) | too fast to fit, 10.0k: 3 ms, 2 MB (budget) | too fast to fit, 10.0k: 2 ms, 2 MB (timeout at 30.0k) |
| tend nodes already up (settled): stopUpkeep | nodes | too fast to fit, 3.0k: 3 ms, 1 MB (timeout at 10.0k) | n^2.1, 30.0k: 387 ms, 320 MB (heap at 100.0k) | n^1.2, 30.0k: 56 ms, 13 MB (heap at 100.0k) | too fast to fit, 10.0k: 13 ms, 4 MB (budget) | too fast to fit, 10.0k: 9 ms, 4 MB (budget) | too fast to fit, 10.0k: 16 ms, 4 MB (budget) | too fast to fit, 10.0k: 12 ms, 4 MB (timeout at 30.0k) |
| bring nodes up (unsettled): startUpkeep | nodes | n^2.0, 3.0k: 1.9 s, 34 MB (timeout at 10.0k) | n^2.1, 3.0k: 3.8 s, 470 MB (budget) | n^1.1, 30.0k: 1.1 s, 594 MB (heap at 100.0k) | n^2.1, 10.0k: 12.7 s, 144 MB (budget) | n^1.6, 10.0k: 2.8 s, 166 MB (budget) | n^1.9, 10.0k: 3.9 s, 235 MB (budget) | n^1.4, 10.0k: 2.0 s, 529 MB (budget) |
| bring nodes up (unsettled): until every machine is stable | nodes | n^1.6, 3.0k: 58 ms, 85 MB (timeout at 10.0k) | too fast to fit, 3.0k: 6 ms, 1 MB (budget) | n^1.4, 30.0k: 1.7 s, 814 MB (heap at 100.0k) | n^1.5, 10.0k: 256 ms, 290 MB (budget) | too fast to fit, 10.0k: 263 ms, 286 MB (budget) | too fast to fit, 10.0k: 420 ms, 228 MB (budget) | too fast to fit, 10.0k: 9.3 s, 1.7 GB (budget) |
| bring nodes up (unsettled): stopUpkeep | nodes | too fast to fit, 3.0k: 3 ms, 1 MB (timeout at 10.0k) | too fast to fit, 3.0k: 4 ms, 15 MB (budget) | n^1.5, 30.0k: 69 ms, 13 MB (heap at 100.0k) | too fast to fit, 10.0k: 18 ms, 4 MB (budget) | too fast to fit, 10.0k: 9 ms, 4 MB (budget) | too fast to fit, 10.0k: 19 ms, 4 MB (budget) | too fast to fit, 10.0k: 11 ms, 4 MB (budget) |
| managed nodes: hold, hand over, adopt: startUpkeep | nodes | n^2.0, 3.0k: 1.3 s, 39 MB (timeout at 10.0k) | n^2.0, 1.0k: 243 ms, 66 MB (timeout at 3.0k) | n^1.2, 30.0k: 882 ms, 332 MB (budget) | n^1.9, 3.0k: 891 ms, 34 MB (timeout at 10.0k) | n^1.9, 10.0k: 2.7 s, 120 MB (budget) | n^1.9, 10.0k: 3.6 s, 122 MB (budget) | n^1.3, 10.0k: 2.3 s, 530 MB (budget) |
| managed nodes: hold, hand over, adopt: until every machine is stable | nodes | n^1.1, 3.0k: 64 ms, 111 MB (timeout at 10.0k) | n^2.1, 1.0k: 458 ms, 116 MB (timeout at 3.0k) | n^1.5, 30.0k: 2.9 s, 1.3 GB (budget) | n^1.2, 3.0k: 67 ms, 118 MB (timeout at 10.0k) | n^1.3, 10.0k: 436 ms, 413 MB (budget) | n^1.4, 10.0k: 698 ms, 429 MB (budget) | n^1.4, 10.0k: 12.3 s, 1.9 GB (budget) |
| managed nodes: hold, hand over, adopt: stopUpkeep (machines kept) | nodes | too fast to fit, 3.0k: 1 ms, 2 MB (timeout at 10.0k) | too fast to fit, 1.0k: 6 ms, 26 MB (timeout at 3.0k) | n^1.6, 30.0k: 61 ms, 24 MB (budget) | too fast to fit, 3.0k: 1 ms, 2 MB (timeout at 10.0k) | too fast to fit, 10.0k: 8 ms, 8 MB (budget) | too fast to fit, 10.0k: 10 ms, 8 MB (budget) | too fast to fit, 10.0k: 9 ms, 8 MB (budget) |
| managed nodes: hold, hand over, adopt: startUpkeep (adopting) | nodes | n^2.2, 3.0k: 1.4 s, 16 MB (timeout at 10.0k) | too fast to fit, 1.0k: 3 ms, 3 MB (timeout at 3.0k) | n^1.6, 30.0k: 660 ms, 147 MB (budget) | n^2.3, 3.0k: 983 ms, 15 MB (timeout at 10.0k) | n^2.2, 10.0k: 3.2 s, 52 MB (budget) | n^2.1, 10.0k: 3.7 s, 52 MB (budget) | n^1.9, 10.0k: 1.2 s, 42 MB (budget) |
| managed nodes: hold, hand over, adopt: stopUpkeep + releaseKept | nodes | n^1.0, 3.0k: 51 ms, 63 MB (timeout at 10.0k) | n^0.8, 1.0k: 20 ms, 18 MB (timeout at 3.0k) | n^1.5, 30.0k: 2.0 s, 865 MB (budget) | n^1.1, 3.0k: 44 ms, 62 MB (timeout at 10.0k) | n^1.3, 10.0k: 240 ms, 275 MB (budget) | n^1.2, 10.0k: 290 ms, 274 MB (budget) | n^1.2, 10.0k: 477 ms, 254 MB (budget) |

### rewrite

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| no phase registered (wholeGraph + rewrite) | nodes | too fast to fit, 100.0k: 5 ms, 16 MB (cap) | too fast to fit, 100.0k: 6 ms, 16 MB (cap) | too fast to fit, 100.0k: 6 ms, 16 MB (cap) | too fast to fit, 100.0k: 5 ms, 16 MB (cap) | too fast to fit, 100.0k: 5 ms, 16 MB (cap) | too fast to fit, 100.0k: 9 ms, 16 MB (cap) | n^1.4, 100.0k: 415 ms, 16 MB (cap) |
| one batch of a tenth of the nodes | nodes | n^1.7, 100.0k: 1.8 s, 4.5 GB (cap) | n^1.3, 100.0k: 562 ms, 275 MB (cap) | n^1.6, 100.0k: 1.3 s, 4.5 GB (cap) | n^1.7, 100.0k: 1.9 s, 8.0 GB (cap) | n^1.9, 100.0k: 5.4 s, 17.9 GB (budget) | n^1.9, 100.0k: 9.0 s, 34.5 GB (budget) | n^1.7, 30.0k: 12.2 s, 33.7 GB (budget) |
| sixteen batches of a tenth of the nodes | nodes | n^1.4, 100.0k: 7.3 s, 3.1 GB (budget) | n^1.4, 100.0k: 9.3 s, 3.9 GB (budget) | n^1.4, 100.0k: 5.3 s, 3.1 GB (budget) | n^1.4, 100.0k: 6.2 s, 3.9 GB (budget) | n^1.3, 30.0k: 2.2 s, 1.5 GB (budget) | n^1.4, 30.0k: 3.3 s, 2.1 GB (budget) | n^1.3, 10.0k: 8.8 s, 8.5 GB (budget) |

### ledger

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| eight overlapping declarations: contribution (of one Dag) | nodes | n^1.3, 100.0k: 174 ms, 92 MB (cap) | n^1.3, 100.0k: 171 ms, 92 MB (cap) | n^1.3, 100.0k: 144 ms, 92 MB (cap) | n^1.3, 100.0k: 179 ms, 120 MB (cap) | n^1.4, 100.0k: 434 ms, 176 MB (cap) | n^1.3, 100.0k: 675 ms, 266 MB (cap) | n^1.4, 30.0k: 3.1 s, 911 MB (budget) |
| eight overlapping declarations: desired | nodes | n^1.1, 100.0k: 33 ms, 18 MB (cap) | n^1.1, 100.0k: 35 ms, 18 MB (cap) | n^1.1, 100.0k: 30 ms, 18 MB (cap) | n^1.2, 100.0k: 32 ms, 18 MB (cap) | n^1.2, 100.0k: 43 ms, 18 MB (cap) | n^1.2, 100.0k: 45 ms, 18 MB (cap) | too fast to fit, 30.0k: 14 ms, 5 MB (budget) |
| eight overlapping declarations: precedenceOf | nodes | n^1.1, 100.0k: 57 ms, 28 MB (cap) | n^1.1, 100.0k: 35 ms, 16 MB (cap) | n^1.2, 100.0k: 51 ms, 28 MB (cap) | n^1.2, 100.0k: 67 ms, 37 MB (cap) | n^1.2, 100.0k: 127 ms, 49 MB (cap) | n^1.1, 100.0k: 214 ms, 76 MB (cap) | n^1.2, 30.0k: 484 ms, 171 MB (budget) |
| eight overlapping declarations: knownRefs | nodes | n^1.3, 100.0k: 46 ms, 18 MB (cap) | n^1.1, 100.0k: 36 ms, 18 MB (cap) | n^1.1, 100.0k: 31 ms, 18 MB (cap) | n^1.2, 100.0k: 30 ms, 18 MB (cap) | n^1.2, 100.0k: 44 ms, 18 MB (cap) | n^1.2, 100.0k: 42 ms, 18 MB (cap) | too fast to fit, 30.0k: 12 ms, 5 MB (budget) |
| eight overlapping declarations: retractAll + collect, nothing standing | nodes | too fast to fit, 100.0k: 10 ms, 2 kB (cap) | too fast to fit, 100.0k: 10 ms, 2 kB (cap) | too fast to fit, 100.0k: 9 ms, 2 kB (cap) | too fast to fit, 100.0k: 7 ms, 2 kB (cap) | too fast to fit, 100.0k: 10 ms, 2 kB (cap) | too fast to fit, 100.0k: 8 ms, 2 kB (cap) | too fast to fit, 30.0k: 3 ms, 2 kB (budget) |

### query

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| outline | nodes | n^1.2, 100.0k: 515 ms, 626 MB (cap) | n^2.2, 30.0k: 15.0 s, 164 MB (budget) | n^1.2, 100.0k: 509 ms, 634 MB (cap) | n^1.2, 100.0k: 699 ms, 1.0 GB (cap) | n^1.1, 100.0k: 1.5 s, 1.9 GB (cap) | n^1.1, 100.0k: 2.5 s, 3.9 GB (budget) | n^1.3, 3.0k: 4.0 s, 11.1 GB (budget) |
| paths per node (outline + outlinePaths, as worldPaths) | nodes | n^2.2, 10.0k: 7.5 s, 8.1 GB (budget) | n^2.0, 10.0k: 2.4 s, 86 MB (budget) | n^1.3, 100.0k: 1.7 s, 1.2 GB (cap) | n^2.0, 3.0k: 2.9 s, 3.9 GB (budget) | n^2.0, 10.0k: 11.0 s, 10.2 GB (budget) | n^2.0, 10.0k: 16.0 s, 13.2 GB (budget) | n^1.5, 3.0k: 9.0 s, 13.7 GB (budget) |
| resolveSelectors (select **, exclude **/n1/**) | nodes | n^1.3, 100.0k: 2.4 s, 1.2 GB (budget) | n^1.7, 30.0k: 14.8 s, 273 MB (budget) | n^1.3, 100.0k: 1.7 s, 1.2 GB (cap) | n^1.3, 100.0k: 2.5 s, 1.6 GB (budget) | n^1.3, 100.0k: 3.8 s, 2.5 GB (budget) | n^1.2, 100.0k: 4.4 s, 4.6 GB (budget) | n^1.4, 3.0k: 3.9 s, 11.2 GB (budget) |
| query show --dedupe (renderAnnotated) | nodes | n^2.1, 10.0k: 10.1 s, 17.9 GB (budget) | n^2.0, 30.0k: 15.1 s, 232 MB (budget) | n^1.2, 100.0k: 940 ms, 1.4 GB (cap) | n^2.1, 10.0k: 5.1 s, 12.0 GB (budget) | n^1.9, 30.0k: 11.1 s, 20.7 GB (budget) | n^1.9, 30.0k: 13.7 s, 21.3 GB (budget) | n^1.4, 3.0k: 3.8 s, 11.2 GB (budget) |
| query show, every path (renderAnnotated) | occurrences | n^2.2, 10.0k: 14.7 s, 22.1 GB (budget) | n^1.1, 100.0k: 663 ms, 673 MB (cap) | n^1.1, 100.0k: 505 ms, 1.2 GB (cap) | n^1.1, 524.3k (52 nodes): 3.8 s, 10.1 GB (budget) | n^1.1, 524.3k (129 nodes): 4.3 s, 6.6 GB (budget) | n^1.0, 708.6k (89 nodes): 4.0 s, 7.6 GB (budget) | n^1.1, 1.1M (129 nodes): 4.9 s, 8.9 GB (budget) |
| query plan (expand, fold, resolve, encode) | occurrences | n^1.3, 100.0k: 3.4 s, 1.6 GB (budget) | n^1.9, 10.0k: 4.9 s, 2.9 GB (budget) | n^1.3, 100.0k: 2.4 s, 1.5 GB (budget) | n^1.0, 2.1M (58 nodes): 4.5 s, 13.4 GB (budget) | n^1.0, 524.3k (129 nodes): 2.4 s, 3.4 GB (budget) | n^1.0, 2.1M (97 nodes): 5.7 s, 13.9 GB (budget) | n^1.0, 1.1M (129 nodes): 4.5 s, 7.5 GB (budget) |
| run tree (dagLines) | nodes | n^1.3, 100.0k: 204 ms, 66 MB (cap) | n^1.3, 100.0k: 494 ms, 103 MB (cap) | n^1.2, 100.0k: 154 ms, 66 MB (cap) | n^1.3, 100.0k: 177 ms, 72 MB (cap) | n^1.2, 100.0k: 379 ms, 119 MB (cap) | n^1.4, 100.0k: 398 ms, 136 MB (cap) | n^1.1, 100.0k: 1.1 s, 623 MB (cap) |
| run dag (printDagCograph, to /dev/null) | nodes | n^1.1, 100.0k: 217 ms, 673 MB (cap) | n^1.1, 100.0k: 345 ms, 714 MB (cap) | n^1.1, 100.0k: 178 ms, 673 MB (cap) | n^1.1, 100.0k: 175 ms, 785 MB (cap) | n^1.1, 100.0k: 325 ms, 1.0 GB (cap) | n^1.0, 100.0k: 300 ms, 1.4 GB (cap) | n^1.0, 100.0k: 1.5 s, 11.1 GB (cap) |

### serve

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| loop: record a declaration (expand, fold, ledger) | occurrences | n^1.0, 3.0k: 19 ms, 28 MB (timeout at 10.0k) | n^1.4, 3.0k: 265 ms, 280 MB (timeout at 10.0k) | n^1.2, 30.0k: 260 ms, 317 MB (process: exit 251) | n^1.0, 2.1M (58 nodes): 4.4 s, 13.4 GB (budget) | n^1.0, 2.1M (145 nodes): 6.6 s, 14.0 GB (budget) | n^1.1, 708.6k (89 nodes): 2.2 s, 4.5 GB (budget) | n^0.9, 1.1M (129 nodes): 3.1 s, 7.5 GB (budget) |
| loop: before the first pass (rewrite, worldDag) | occurrences | too fast to fit, 3.0k: 254 us, 938 kB (timeout at 10.0k) | too fast to fit, 3.0k: 540 us, 938 kB (timeout at 10.0k) | too fast to fit, 30.0k: 3 ms, 9 MB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 7 us, 20 kB (budget) | too fast to fit, 2.1M (145 nodes): 15 us, 47 kB (budget) | too fast to fit, 708.6k (89 nodes): 10 us, 29 kB (budget) | too fast to fit, 1.1M (129 nodes): 12 us, 42 kB (budget) |
| loop: first pass, every node up | occurrences | n^1.9, 3.0k: 1.9 s, 138 MB (timeout at 10.0k) | n^1.6, 3.0k: 475 ms, 736 MB (timeout at 10.0k) | n^1.2, 30.0k: 1.1 s, 1.1 GB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 880 us, 713 kB (budget) | n^0.1, 2.1M (145 nodes): 2 ms, 5 MB (budget) | n^0.2, 708.6k (89 nodes): 2 ms, 2 MB (budget) | n^0.0, 1.1M (129 nodes): 8 ms, 21 MB (budget) |
| loop: status | occurrences | n^2.2, 3.0k: 595 ms, 730 MB (timeout at 10.0k) | n^1.9, 3.0k: 158 ms, 14 MB (timeout at 10.0k) | n^1.4, 30.0k: 353 ms, 232 MB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 790 us, 2 MB (budget) | n^0.1, 2.1M (145 nodes): 2 ms, 4 MB (budget) | n^0.1, 708.6k (89 nodes): 1 ms, 3 MB (budget) | n^0.5, 1.1M (129 nodes): 34 ms, 27 MB (budget) |
| loop: before the idle pass | occurrences | too fast to fit, 3.0k: 349 us, 843 kB (timeout at 10.0k) | too fast to fit, 3.0k: 447 us, 843 kB (timeout at 10.0k) | too fast to fit, 30.0k: 5 ms, 8 MB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 7 us, 24 kB (budget) | too fast to fit, 2.1M (145 nodes): 16 us, 48 kB (budget) | too fast to fit, 708.6k (89 nodes): 9 us, 28 kB (budget) | too fast to fit, 1.1M (129 nodes): 19 us, 40 kB (budget) |
| loop: idle pass, nothing pending | occurrences | too fast to fit, 3.0k: 2 ms, 2 MB (timeout at 10.0k) | too fast to fit, 3.0k: 3 ms, 2 MB (timeout at 10.0k) | n^1.3, 30.0k: 39 ms, 23 MB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 26 us, 47 kB (budget) | too fast to fit, 2.1M (145 nodes): 86 us, 115 kB (budget) | too fast to fit, 708.6k (89 nodes): 44 us, 73 kB (budget) | too fast to fit, 1.1M (129 nodes): 63 us, 100 kB (budget) |
| loop: clear | occurrences | too fast to fit, 3.0k: 5 us, 3 kB (timeout at 10.0k) | too fast to fit, 3.0k: 10 us, 3 kB (timeout at 10.0k) | too fast to fit, 30.0k: 8 us, 3 kB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 2 us, 3 kB (budget) | too fast to fit, 2.1M (145 nodes): 3 us, 3 kB (budget) | too fast to fit, 708.6k (89 nodes): 2 us, 3 kB (budget) | too fast to fit, 1.1M (129 nodes): 2 us, 3 kB (budget) |
| loop: before the teardown pass | occurrences | too fast to fit, 3.0k: 2 ms, 3 MB (timeout at 10.0k) | too fast to fit, 3.0k: 3 ms, 3 MB (timeout at 10.0k) | n^1.4, 30.0k: 37 ms, 27 MB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 20 us, 54 kB (budget) | too fast to fit, 2.1M (145 nodes): 68 us, 132 kB (budget) | too fast to fit, 708.6k (89 nodes): 36 us, 80 kB (budget) | too fast to fit, 1.1M (129 nodes): 46 us, 115 kB (budget) |
| loop: teardown pass, every node down | occurrences | n^2.1, 3.0k: 1.8 s, 135 MB (timeout at 10.0k) | n^1.5, 3.0k: 344 ms, 640 MB (timeout at 10.0k) | n^1.4, 30.0k: 1.7 s, 905 MB (process: exit 251) | too fast to fit, 2.1M (58 nodes): 754 us, 693 kB (budget) | n^0.1, 2.1M (145 nodes): 2 ms, 2 MB (budget) | n^0.0, 708.6k (89 nodes): 1 ms, 1 MB (budget) | n^0.1, 1.1M (129 nodes): 8 ms, 21 MB (budget) |

### render

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| GET /dag (dagValue + encode) | nodes | n^1.4, 10.0k: 441 ms, 525 MB (inputs at 30.0k) | n^1.0, 30.0k: 415 ms, 660 MB (inputs at 100.0k) | n^1.2, 100.0k: 1.3 s, 2.2 GB (cap) | n^1.7, 3.0k: 120 ms, 219 MB (inputs at 10.0k) | n^1.4, 10.0k: 445 ms, 622 MB (inputs at 30.0k) | n^1.3, 10.0k: 350 ms, 682 MB (inputs at 30.0k) | n^1.1, 3.0k: 157 ms, 629 MB (inputs at 10.0k) |
| GET /status (status report + encode) | nodes | n^1.8, 10.0k: 353 ms, 410 MB (inputs at 30.0k) | n^1.0, 30.0k: 95 ms, 319 MB (inputs at 100.0k) | n^1.1, 100.0k: 396 ms, 1.1 GB (cap) | n^1.8, 3.0k: 109 ms, 178 MB (inputs at 10.0k) | n^1.6, 10.0k: 316 ms, 447 MB (inputs at 30.0k) | n^1.4, 3.0k: 30 ms, 68 MB (inputs at 10.0k) | too fast to fit, 3.0k: 14 ms, 49 MB (inputs at 10.0k) |
| status sink document (encode) | nodes | n^1.6, 10.0k: 306 ms, 411 MB (inputs at 30.0k) | n^1.0, 30.0k: 94 ms, 319 MB (inputs at 100.0k) | n^1.1, 100.0k: 391 ms, 1.1 GB (cap) | n^1.8, 3.0k: 108 ms, 178 MB (inputs at 10.0k) | n^1.6, 10.0k: 376 ms, 447 MB (inputs at 30.0k) | n^1.3, 3.0k: 31 ms, 68 MB (inputs at 10.0k) | too fast to fit, 3.0k: 14 ms, 49 MB (inputs at 10.0k) |

### client

| operation | sized by | chain | fan | tree2 | diamonds2 | layered8x2 | random8x3 | dense32 |
|---|---|---|---|---|---|---|---|---|
| Model.fromDag | nodes | n^1.3, 10.0k: 25 ms, 27 MB (inputs at 30.0k) | n^1.2, 30.0k: 81 ms, 84 MB (inputs at 100.0k) | n^1.3, 100.0k: 470 ms, 289 MB (cap) | too fast to fit, 3.0k: 6 ms, 11 MB (inputs at 10.0k) | n^1.4, 10.0k: 41 ms, 41 MB (inputs at 30.0k) | n^1.4, 10.0k: 37 ms, 45 MB (inputs at 30.0k) | n^1.4, 3.0k: 25 ms, 50 MB (inputs at 10.0k) |
| Model.step, one done event per node | nodes | too fast to fit, 10.0k: 18 ms, 14 MB (inputs at 30.0k) | n^1.3, 30.0k: 59 ms, 45 MB (inputs at 100.0k) | n^1.4, 100.0k: 299 ms, 158 MB (cap) | too fast to fit, 3.0k: 3 ms, 4 MB (inputs at 10.0k) | too fast to fit, 10.0k: 14 ms, 14 MB (inputs at 30.0k) | too fast to fit, 3.0k: 3 ms, 4 MB (inputs at 10.0k) | too fast to fit, 3.0k: 10 ms, 4 MB (inputs at 10.0k) |
| Model.step, a teardown: one done event per node wanted down | nodes | n^2.4, 10.0k: 2.0 s, 4.3 GB (inputs at 30.0k) | n^2.0, 10.0k: 1.5 s, 4.3 GB (timeout at 30.0k) | n^2.2, 10.0k: 1.6 s, 4.3 GB (timeout at 30.0k) | too fast to fit, 3.0k: 96 ms, 355 MB (inputs at 10.0k) | n^2.1, 10.0k: 1.6 s, 4.3 GB (inputs at 30.0k) | n^2.3, 10.0k: 1.6 s, 4.3 GB (inputs at 30.0k) | n^2.1, 3.0k: 153 ms, 349 MB (inputs at 10.0k) |

## Up to 1 000 000 nodes

Budget 60 s, three shapes, the series that were still cheap at 100 000. From
`resources/graph-profiles/1m.tsv`. The other four shapes were not run at this size.

### fold

| operation | sized by | chain | tree2 | layered8x2  |
|---|---|---|---|---|
| expand (evalDeps, every occurrence visited) | occurrences | n^1.1, 1.0M: 1.5 s, 1.1 GB (cap) | n^1.0, 1.0M: 356 ms, 1.2 GB (cap) | n^1.0, 16.8M (169 nodes): 6.6 s, 21.7 GB (cap)  |
| expand + foldDag | occurrences | n^1.2, 1.0M: 20.0 s, 10.2 GB (budget) | n^1.2, 1.0M: 24.3 s, 10.3 GB (budget) | n^1.1, 8.4M (161 nodes): 29.4 s, 55.5 GB (budget)  |
| dagOf (the fixture's per-node build, for comparison) | nodes | n^1.4, 1.0M: 42.3 s, 9.1 GB (budget) | n^1.3, 1.0M: 29.8 s, 9.2 GB (budget) | n^1.2, 1.0M: 33.5 s, 13.9 GB (budget)  |

### dag

| operation | sized by | chain | tree2 | layered8x2  |
|---|---|---|---|---|
| fromMagma | nodes | n^1.3, 1.0M: 29.1 s, 8.2 GB (budget) | n^1.3, 1.0M: 26.8 s, 8.2 GB (budget) | n^1.3, 1.0M: 51.3 s, 12.3 GB (budget)  |

### walk

| operation | sized by | chain | tree2 | layered8x2  |
|---|---|---|---|---|
| upDag (sequential) | nodes | n^1.3, 1.0M: 25.2 s, 3.4 GB (budget) | n^1.2, 1.0M: 25.5 s, 3.4 GB (budget) | n^1.4, 1.0M: 36.4 s, 4.5 GB (budget)  |
| upDagConcurrent | nodes | n^2.2, 10.0k: 32.5 s, 83 MB (budget) | n^1.4, 1.0M: 123.1 s, 7.7 GB (budget) | n^2.2, 30.0k: 61.3 s, 252 MB (budget)  |

### query

| operation | sized by | chain | tree2 | layered8x2  |
|---|---|---|---|---|
| query plan (expand, fold, resolve, encode) | occurrences | n^1.2, 1.0M: 63.3 s, 18.1 GB (budget) | n^1.2, 1.0M: 50.0 s, 16.9 GB (budget) | n^1.0, 8.4M (161 nodes): 40.9 s, 55.5 GB (budget)  |

