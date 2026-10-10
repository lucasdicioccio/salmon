# How the graph operations scale

What each graph operation costs as the graph grows, per shape: measured, with the growth fitted,
by the `graph-profiles` benchmark in `salmon-ops-recipes` over the synthetic graphs of
`Test.GraphFixture`. The numbers are **indicative**: one run, on a shared laptop-class machine that
other work was using. Read the growth and the order of magnitude, not the third digit.

This document reports; it fixes nothing. The super-linear findings are listed with what was
measured and, separately, what reading the code suggests is the cause.

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
can be measured at node counts the fold could never produce. `dagOf` is checked against the fold
at small sizes by `Test.GraphScaleSpec`.

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
   it.** Chain: 2.0 s at 3 000 nodes (`n^2.2`). Diamonds: 13.6 s at 10 000. The
   concurrent walk on a chain costs what `stuck` costs (1.9 s at 3 000), so does `startUpkeep`
   (1.5 s), and so does a `serve` pass (1.9 s up, 1.8 s down, at 3 000). The sequential walk over
   the same chain is 1.0 s at 100 000. On a shallow shape the concurrent walk is fine: tree2 is
   3.3 s at 100 000 and 123 s at 1 000 000 (one thread and one `TVar` per node does fit in 4 GB
   there). *Read from the code:* `stuck` peels one layer of ready nodes per round with a full
   filter of the remaining set, so it costs depth x nodes.
3. **A node with many edges is quadratic in its edge count, in several places.**
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
5. **`Client.Model.step` is quadratic over a teardown.** One `done` event per node wanted down:
   1.5 to 2.0 s and 4.3 GB allocated at 10 000 nodes on the five shapes that got there, abandoned
   at 30 000 on fan and tree2. Folding
   the same number of events for a bring-up is per node (0.3 s at 100 000). *Read from the code:*
   each removal filters the whole `modelOrder` list.
6. **A supervisor does not fit 100 000 nodes in 4 GB.** `startUpkeep` over nodes already up: 1.6 s
   and 1.0 GB allocated at 30 000 (fan), 2.0 s at 30 000 (tree2); at 100 000 the heap bound was
   hit on both. Over managed nodes on a fan it is abandoned at 3 000. A `serve` loop over tree2
   hit the heap bound at 100 000 as well (260 ms to record and 1.1 s for the first pass at
   30 000).
7. **Per node, where nothing above applies:** the sequential walks cost about 8 to 10 µs a node at 100 000
   (25 s at 1 000 000), `fromMagma` and `mergeDag` 10 to 14 µs, expansion alone 0.3 to 1.5 µs an
   occurrence, the ledger reads well under a microsecond a node. A rewrite of sixteen batches
   costs sixteen collapses of the whole graph (5 to 9 s at 100 000).

### Candidate follow-ups

One per finding worth fixing, in the order the numbers suggest; none is done here.

1. `foldDag` per node instead of per occurrence (finding 1): it bounds `run up`/`down`/`tree`/
   `dag`, `query plan` and every `serve` declaration at a few dozen nodes of diamond chain.
2. `Dag.stuck` in one pass (finding 2): it bounds every concurrent pass and every tending start
   on a deep graph at a few thousand nodes.
3. Edge lists that do not search or append on the right, in `Dag.record`, `fromMagma`,
   `transposeOf` and `Query.outline` (finding 3). The wake-up cost of a node waiting on many
   neighbours is a separate question.
4. Listed paths that do not cost depth per node (finding 4): shared prefixes, a length cap, or
   listing on request.
5. `Client.Model.step` removing a node without filtering the order list (finding 5).
6. What a tended node retains (finding 6): not investigated beyond the heap bound being hit.

## Not measured

- `Help.printTree`, `Dot.printCograph` and `Query.pathedRefs` on their own: none is on a command
  path any more except `query show` without `--dedupe`, which is measured.
- Colliding `Ref`s (`optCollisions`) and the `Connect`/`Overlay` edge forms of the fixture.
- `Model.step` for the `upkeep` and `output` event kinds; `Model.rebase`.
- The HTTP server itself, the event ring, the socket listener, TLS, pull mode: only the pure
  renders behind `/dag` and `/status` and the status sink's document were timed.
- A supervisor that is tending: only start, settle and stop. No node here ever fails, restarts,
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

