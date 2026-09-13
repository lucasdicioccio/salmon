# Property-based testing for `run serve`

Status: draft / not implemented. This is a design sketch to react to, not a
committed plan.

## Problem

`Test.ServeSpec` (see `CLAUDE.md`'s note on `Salmon.Actions.Serve`) is entirely
example-based: each test picks one specific command sequence by hand and
asserts on the outcome. That style is good at pinning a known regression down
precisely (see the `autoconverge off` tests added on `serve-supervision`), but
it only ever checks the sequences somebody thought to type. The bug this spec
is a reaction to — `autoconverge off` failing to also stop the idle tending
loop, and later `force` having nowhere to deliver its instruction once tending
was correctly stopped — was found by hand, once, in an interactive session. A
property test stating "no IO happens on any node while autoconverge is off,
whatever the preceding history" would have caught both for free, and would
keep catching the next variant of the same mistake without anyone having to
think of the exact scenario again.

`Serve.hs`'s own model is unusually well-suited to this: a `World` is a pure
fold over a sequence of commands (`record`/`retract`/`converge`/etc.), and the
whole point of the module (per its own haddock) is that "everything else is
derived" from that fold. That's exactly the shape property-based testing
wants: generate a sequence, fold it, check an invariant of the result — not
"guess an interesting sequence and hand-write it."

## Design goals / non-goals

Goals:
- A handful of invariants, checked against **randomly generated command
  sequences**, over the exact `Serve.serveWith` entry point the example tests
  already drive (no separate model of the implementation to keep in sync —
  the shadow model is of the *domain*, e.g. "which seeds are live", not of
  `Serve.hs`'s internals).
- Shrinking that produces a short, readable failing command sequence, since
  that is most of the value of property testing over examples — a hand-picked
  regression test is only as good as the report that led to it.
- Reuse of the existing test fixtures/plumbing (`runServeWith`, the `Spec`
  seed type, `withSession`) rather than a parallel test harness.

Non-goals (v1):
- Properties that need real idle time (the tending loop, `withSession`'s
  forked-thread sessions). Real threads and real delays make shrinking
  fight the scheduler instead of the command sequence; the hand-written
  `withSession` examples already cover that territory and should stay
  example-based. v1 is scoped to **piped-script mode** (`runServeWith`), which
  is a pure function of the command list — no idle moment ever occurs, so
  there is nothing nondeterministic to shrink around (see `CLAUDE.md`'s note
  that a piped script is never supervised).
- `Rewrite`-registered batching. Interesting, but it's a second axis of
  complexity on top of plain declare/converge; a v2 concern once the plain
  case has a harness worth extending.
- Testing the tending FSM itself (`Test.UpkeepSpec` already does that, at the
  right level — a single machine's state transitions, not a whole `serve`
  session).

## Invariants

Numbered for reference, not priority — see "Suggested starting set" below.

1. **Convergence does what it says.** After any `converge` (or an
   autoconverging declaration), every node whose *final* resolved direction
   is `TurnUp` and was not already `Converged` had `up` called on it at least
   once since the previous convergence; dually, `TurnDown` nodes had `down`
   called. This generalizes the motivating example ("if the latest event is
   `up node`, we observe `up` called by the end") to the state at the end of
   an arbitrary history rather than just after one command.

2. **No spurious re-application.** A node that was already `Converged` in
   some direction, whose direction and content did not change, gets *zero*
   additional `up`/`down` calls from a later `converge`. The idempotence half
   of (1) — easy to eyeball as "it converged", easy to miss "it converged
   *again* for no reason" in a hand-read transcript.

3. **Shared-node teardown safety.** If two live declarations both want a
   node (the shared-directory case `retireMultiFileBundle`/`onlySupersedes`
   already cover by hand), retiring one must never call `down` on it while
   the other is still live. Generalizes those two fixed examples across
   arbitrary interleavings of `up`/`down`/`only`/`clear`.

4. **`autoconverge off` is a strict no-op on IO.** From `autoconverge off`
   until either `converge` or `autoconverge on`, no `up`/`down` fires for any
   node, regardless of how many `up`/`down`/`clear`/`only` commands happen in
   between. This is the bug this spec exists because of; see "Non-goals"
   above for why it stays example-based (`withSession`) rather than becoming
   a v1 property despite being the original motivation — the *interesting*
   failure mode (idle tending applying a deferred node) is precisely the one
   piped-script mode cannot exercise at all.
5. **`force`/`recheck` are scoped.** Forcing node A never causes IO on node
   B, no matter what else is pending. Same real-idle-time caveat as (4) — the
   instruction only does anything once a machine exists to receive it, which
   piped-script mode never starts. Stays example-based in v1 for the same
   reason as (4).
6. **Settling is total.** `clear` followed by enough `converge`s drives
   `worldNodes`/`worldEpochs`/`worldLedger`/`worldMagma` all empty, whatever
   the preceding history was — generalizes `assertWorldSettled` off its one
   fixed sequence.
7. **Ref stability under reordering.** Declaring the same seed args always
   resolves to the same `Ref`, independent of what else was declared
   before/after/around it — `mkRef`'s content-addressing should not depend on
   history shape. Cheap to check as a side-assertion inside (1)/(3)/(6) rather
   than its own property: whenever the model says "seed X is up", the real
   node's `Ref` should be the one first seen for seed X, ever.

### Suggested starting set for v1

(1), (2), (3), (6) — all piped-script-only, all checkable against the exact
harness sketched below with no new infrastructure beyond a generator and a
shadow model. (4) and (5) are the ones the bug this spec reacts to actually
lived in, but per "Non-goals" they need real idle time to be meaningful, so
they stay as the `withSession` examples already on `serve-supervision`
(`autoConvergeOffAlsoStopsIdleTending`, `autoConvergeOffForceStillReachesANamedNode`,
etc.) rather than becoming property tests in this first pass. (7) is cheap
enough to fold into whichever of (1)/(3)/(6) lands first rather than write
standalone.

## Harness sketch

**Universe.** A small *fixed* set of seed ids and node names — 2–3 seeds
sharing 1–2 node names on purpose (mirroring `Test.ServeSpec`'s existing
`program`/`Spec` fixture: a seed is a list of file names under a shared
directory). A bigger universe dilutes exactly the shared-node interactions
these invariants are about; a bigger *history length* is where the
interesting coverage should come from, not a bigger alphabet.

**Command generator.** `Gen [ServeCommand]`-shaped, restricted to
`Up`/`Down`/`Only`/`Clear`/`Converge` over that fixed universe (no
`AutoConverge`/`Force`/`Instruct` in v1 per "Non-goals"). Ordinary list
shrinking (shrink the list, then shrink individual seed choices toward the
first seed id) should already produce short, readable counterexamples,
since QuickCheck/Hedgehog both shrink lists well out of the box.

**Spy.** A stub `Track' Spec` (reusing the existing `Spec`/`program`
approach) whose `up`/`down` each bump a per-node counter in one
`IORef (Map Ref Int)` (or `Map NodeName Int` if working in terms of the
model's own naming rather than the real content-addressed `Ref`) rather than
touching the filesystem — matching `neverRuns`/`neverRunsNamed`'s existing
pattern in `Test.ServeSpec`, extended to record *counts per node* rather than
one global counter or two hand-picked ones.

**Shadow model.** Something close to what `Ledger`/`worldNodes` already
compute, but written independently and at the *seed* level rather than
mirrored from the implementation:
```haskell
data Model = Model
  { modelLive :: Set SeedId          -- declared and not yet retired
  , modelUpCount :: Map NodeName Int -- expected cumulative `up` calls
  , modelDownCount :: Map NodeName Int
  }
```
Folding a command into a `Model` should be short enough to visibly not share
logic with `Serve.hs` — the whole point is an independent restatement of
"what should be true", not a shrunk copy of `Ledger.hs`.

**Running it.** Feed the generated `[ServeCommand]` as lines to
`runServeWith` (piped-script mode, deterministic, one convergence pass per
autoconverging command — see `CLAUDE.md`). Fold the same command list through
the shadow model. Compare: final `World`'s per-node `Direction`/`Convergence`
against the model's `modelLive`-derived expectation (invariant set (1)/(2)/(3)/(6)),
and the spy's counters against the model's expected call counts.

## Decisions needed before writing code

- **Library.** Neither `salmon-ops-recipes.cabal`'s test-suite nor any other
  package in this tree currently depends on `QuickCheck`/`tasty-quickcheck`
  (or `hedgehog`/`tasty-hedgehog`). This is a new, if standard and light,
  test-only dependency to add consciously rather than as a side effect of
  the first property test — `tasty-quickcheck` is the smaller addition given
  `tasty`/`tasty-hunit` are already in use, but worth a deliberate choice
  over `hedgehog`'s (arguably nicer) generator/shrinker story.
- **Where the shadow model lives.** As its own small internal module
  (`Test.ServeModelSpec` or similar) versus inline in `Test.ServeSpec` — the
  existing file is already large; a model-based property suite is a distinct
  enough concern (and reusable enough, if `Rewrite`-aware properties get
  added later) to justify its own module from the start.
- **Number of commands per generated sequence.** Long enough to hit
  multi-seed overlap reliably, short enough that a first failing run is
  already close to minimal before shrinking does its work — needs a bit of
  experimentation once the harness exists rather than a guess up front.
