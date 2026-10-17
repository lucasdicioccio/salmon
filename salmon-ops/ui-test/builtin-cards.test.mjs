// The builtin card templates (Salmon.Builtin.Cards, feature 91138a85) against
// the card contract (ui/card-template.js) and the vendored tramaj-js.
// The templates are rendered by Haskell, so this script takes them from a
// JSON file the Haskell test writes (Test/BuiltinCardsSpec.hs):
//   node salmon-ops/ui-test/builtin-cards.test.mjs CARDS.json
// with CARDS.json = {cards: [{name, source, facts, verdicts}], quotes: [{text, source}]}.
// SALMON_UI_TEST_SEED fixes the random contexts' seed; it is printed either way.

import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";

const ct = await import(new URL("../ui/card-template.js", import.meta.url));
const tt = await import(new URL("../ui/tail-template.js", import.meta.url));
const tramaj = await import(new URL("../ui/vendor/tramaj-js/index.js", import.meta.url));

const input = JSON.parse(readFileSync(process.argv[2], "utf8"));

const SEED = Number(process.env.SALMON_UI_TEST_SEED ?? Math.floor(Math.random() * 2 ** 31));
console.log(`# SALMON_UI_TEST_SEED=${SEED}`);

function rng(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const R = rng(SEED);
const int = (n) => Math.floor(R() * n);
const pick = (xs) => xs[int(xs.length)];
const ALPHABET = ["a", "Z", "0", " ", "<", ">", "&", '"', "`", "$", "\\", "\n", "é"];
const randomText = (max = 20) => Array.from({ length: int(max) }, () => pick(ALPHABET)).join("");
const maybe = (f) => (int(3) === 0 ? undefined : f());
const refOf = () => ({ full: "f".repeat(8) + randomText(8), short: randomText(8) });
const VERDICTS = [null, "success", "completed", "skipped", "failure", "unknown", "immaterial"];

function randomNode() {
  return {
    ref: maybe(refOf),
    shorthand: maybe(() => randomText()),
    help: maybe(() => randomText()),
    notes: maybe(() => Array.from({ length: int(4) }, () => randomText())),
    dynamics: maybe(() => Array.from({ length: int(3) }, () => randomText())),
    direction: maybe(() => pick(["up", "down"])),
    convergence: maybe(() => pick(["pending", "converged", "stale", "errored", "blocked"])),
    paths: maybe(() => Array.from({ length: int(3) }, () => randomText())),
    dependencies: maybe(() => Array.from({ length: int(3) }, refOf)),
    dependants: maybe(() => Array.from({ length: int(3) }, refOf)),
    status: maybe(() => (int(2) ? null : { check: maybe(() => ({ verdict: pick(VERDICTS), reason: maybe(() => randomText()) })), output: maybe(() => Array.from({ length: int(4) }, () => randomText())) })),
    conflict: maybe(() => ({ kept: {}, replaced: {} })),
    machine: maybe(() => pick([null, "watching", "applying"])),
  };
}

// the sentence a card should say for this verdict
function sentenceFor(verdicts, v) {
  switch (v) {
    case "success":
    case "completed":
    case "skipped":
      return verdicts.inPlace;
    case "failure":
      return verdicts.toApply;
    case "unknown":
      return verdicts.unsure;
    case "immaterial":
      return verdicts.noCheck;
    default:
      return verdicts.unchecked;
  }
}

function draw(program, node) {
  const r = ct.foldCard(tramaj, program, ct.cardContext(node));
  assert.equal(r.error, undefined, `error: ${r.error}`);
  assert.equal(r.fallback, undefined, "a builtin card never falls back to the default card");
  return tt.specText(r.spec);
}

assert.ok(input.cards.length > 0, "no cards given");

for (const card of input.cards) {
  test(`${card.name}: parses`, () => {
    tramaj.parseProgram(card.source);
  });

  test(`${card.name}: each verdict draws its own sentence, the facts, the reason`, () => {
    const program = tramaj.parseProgram(card.source);
    for (const verdict of VERDICTS) {
      for (const conflict of [false, true]) {
        const reason = verdict === "failure" ? "the reason <b>&`$x`" : undefined;
        const text = draw(program, {
          help: "the help",
          notes: ["note one", "note two"],
          dynamics: ["dyn"],
          status: verdict === null ? null : { check: { verdict, reason }, output: ["line 1", "line 2"] },
          conflict: conflict ? { kept: {}, replaced: {} } : undefined,
        });
        const want = sentenceFor(card.verdicts, verdict);
        assert.ok(text.includes(want), `${verdict}: wants "${want}" in ${JSON.stringify(text)}`);
        for (const other of VERDICTS) {
          const s = sentenceFor(card.verdicts, other);
          if (s !== want) assert.ok(!text.includes(s), `${verdict}: says "${s}" too`);
        }
        for (const [k, v] of card.facts) {
          assert.ok(text.includes(k) && text.includes(v), `fact ${k}=${v} missing`);
        }
        assert.ok(text.includes("the help"));
        assert.ok(text.includes("note one") && text.includes("note two"));
        if (reason) assert.ok(text.includes(reason), "the failure's reason, verbatim");
        assert.equal(text.includes("disagree"), conflict, "the conflict warning, only on a conflict");
      }
    }
  });

  test(`${card.name}: any node draws without error (seeded)`, () => {
    const program = tramaj.parseProgram(card.source);
    for (let i = 0; i < 200; i++) draw(program, randomNode());
  });
}

test("tramajString: every quoted text reads back as itself", () => {
  assert.ok(input.quotes.length > 0, "no quotes given");
  for (const [i, q] of input.quotes.entries()) {
    let program;
    try {
      program = tramaj.parseProgram(q.source);
    } catch (e) {
      assert.fail(`quote ${i} (${JSON.stringify(q.text)}) does not parse: ${tt.messageOf(e)}`);
    }
    const r = ct.foldCard(tramaj, program, ct.cardContext({}));
    assert.equal(r.error, undefined, `quote ${i}: ${r.error}`);
    assert.equal(tt.specText(r.spec), q.text, `quote ${i}`);
  }
});
