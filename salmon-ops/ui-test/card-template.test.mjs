// The card-template contract (ui/card-template.js) against the vendored
// tramaj-js, under node: unit cases and seeded random properties.
// Run: node salmon-ops/ui-test/card-template.test.mjs
// (Test/UiTailTemplateSpec.hs runs it when `node` is on PATH.)
// SALMON_UI_TEST_SEED fixes the properties' seed; it is printed either way.

import { test } from "node:test";
import assert from "node:assert/strict";

const ct = await import(new URL("../ui/card-template.js", import.meta.url));
const tt = await import(new URL("../ui/tail-template.js", import.meta.url));
const tramaj = await import(new URL("../ui/vendor/tramaj-js/index.js", import.meta.url));

const SEED = Number(process.env.SALMON_UI_TEST_SEED ?? Math.floor(Math.random() * 2 ** 31));
console.log(`# SALMON_UI_TEST_SEED=${SEED}`);
const CASES = 200;

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
const ALPHABET = ["a", "Z", "0", " ", "\t", "<", ">", "&", '"', "'", "{", "}", "=", ":", "-", "é", "日", "\\", "`", "$", "."];
const randomText = (max = 30) => Array.from({ length: int(max) }, () => pick(ALPHABET)).join("");
const maybe = (f) => (int(3) === 0 ? undefined : f());
const refOf = () => ({ full: "f".repeat(8) + randomText(8), short: randomText(8) });

const KEYS = [
  "check",
  "conflict",
  "convergence",
  "dependants",
  "dependencies",
  "direction",
  "dynamics",
  "help",
  "last",
  "lastReason",
  "machine",
  "notes",
  "output",
  "paths",
  "ref",
  "shorthand",
].sort();

// a node as ui.js keeps it: /dag's object plus the live overlay, any of it missing
function randomNode() {
  return {
    ref: maybe(refOf),
    shorthand: maybe(() => randomText()),
    help: maybe(() => randomText()),
    notes: maybe(() => Array.from({ length: int(3) }, () => randomText())),
    dynamics: maybe(() => Array.from({ length: int(2) }, () => randomText())),
    direction: maybe(() => pick(["up", "down"])),
    convergence: maybe(() => pick(["pending", "converged", "stale", "errored", "blocked"])),
    paths: maybe(() => Array.from({ length: int(3) }, () => randomText())),
    dependencies: maybe(() => Array.from({ length: int(3) }, refOf)),
    dependants: maybe(() => Array.from({ length: int(3) }, refOf)),
    status: maybe(() => (int(2) ? null : { check: maybe(() => ({ verdict: pick(["success", "failure"]), reason: maybe(() => randomText()) })), output: maybe(() => [randomText()]) })),
    conflict: maybe(() => ({ kept: {}, replaced: {} })),
    machine: maybe(() => pick([null, "watching", "applying"])),
    last: maybe(() => pick([null, "up-done"])),
    lastReason: maybe(() => pick([null, randomText()])),
    template: maybe(() => randomText()),
  };
}

const node = {
  ref: { full: "a".repeat(64), short: "aaaaaaaa" },
  shorthand: "file /tmp/x",
  help: "a file",
  notes: ["one", "two"],
  dynamics: [],
  direction: "up",
  convergence: "converged",
  status: { check: { verdict: "success", reason: null }, output: ["ok"] },
  paths: ["root/file /tmp/x"],
  dependencies: [{ full: "b".repeat(64), short: "bbbbbbbb" }],
  dependants: [],
  machine: "watching",
  last: null,
  lastReason: null,
  template: ".div($ctx.help)",
};
const run = (src, n) => ct.foldCard(tramaj, tramaj.parseProgram(src), ct.cardContext(n));

test("the context carries every key, and only what /dag served", () => {
  const ctx = ct.cardContext(node);
  assert.deepEqual(Object.keys(ctx).sort(), KEYS);
  assert.deepEqual(ctx.ref, { full: "a".repeat(64), short: "aaaaaaaa" });
  assert.deepEqual(ctx.dependencies, ["bbbbbbbb"]);
  assert.deepEqual(ctx.check, { verdict: "success", reason: null });
  assert.equal(ctx.conflict, false);
  assert.equal("template" in ctx, false, "the template does not see its own source");
  const bare = ct.cardContext({});
  assert.deepEqual(Object.keys(bare).sort(), KEYS);
  assert.equal(bare.help, "");
  assert.deepEqual(bare.notes, []);
  assert.deepEqual(bare.check, { verdict: null, reason: null });
  // a template reading any documented key evaluates on a bare node
  for (const k of KEYS) assert.ok(!run(`str($ctx.${k})`, {}).error, k);
  assert.ok(!run("str($ctx.ref.short)", {}).error);
  assert.deepEqual(run('branch($ctx.check.verdict, eq($ctx.check.verdict, null), "never checked")', {}), { spec: "never checked" });
});

test("a card draws block tags; class and title only, actions ignored", () => {
  const r = run('.div(class: "t-ok", title: "t", style: "x", action("click", "k", {}), .h4($ctx.shorthand), .ul(map($ctx.notes, (x) => .li($x))))', node);
  assert.deepEqual(r.spec, {
    tag: "div",
    cls: "t-ok",
    title: "t",
    children: [
      { tag: "h4", cls: null, title: null, children: ["file /tmp/x"] },
      {
        tag: "ul",
        cls: null,
        title: null,
        children: [
          { tag: "li", cls: null, title: null, children: ["one"] },
          { tag: "li", cls: null, title: null, children: ["two"] },
        ],
      },
    ],
  });
  const t = run('.table(.tr(.th("state"), .td($ctx.convergence)))', node);
  assert.equal(tt.specText(t.spec), "stateconverged");
});

test("null asks for the default card; a scalar is text", () => {
  const p = 'branch(null, eq($ctx.convergence, "errored"), .p(class: "t-err", "broken"))';
  assert.deepEqual(run(p, node), { fallback: true });
  assert.deepEqual(run(p, { ...node, convergence: "errored" }), { spec: { tag: "p", cls: "t-err", title: null, children: ["broken"] } });
  assert.deepEqual(run("$ctx.help", node), { spec: "a file" });
});

test("what is not drawn in a card is an error, never markup", () => {
  for (const tag of ["script", "a", "img", "iframe", "style", "form", "input", "object"]) {
    assert.match(run(`.${tag}("x")`, node).error, /is not drawn in a card/, tag);
  }
  assert.match(run("$ctx.nope", node).error, /PathNotFound/);
});

test("the tail's allowlist is unchanged by the card's", () => {
  const ctx = tt.tailContext({ line: "x", text: "x" }, undefined);
  assert.match(tt.foldLine(tramaj, tramaj.parseProgram('.div("x")'), ctx).error, /is not drawn in a tail/);
  for (const tag of tt.TEMPLATE_TAGS) assert.ok(ct.CARD_TAGS.has(tag), tag);
  assert.ok(!tt.TEMPLATE_TAGS.has("div"));
});

test("property: every key is present and the context is plain JSON on any node", () => {
  for (let i = 0; i < CASES; i++) {
    const n = randomNode();
    const ctx = ct.cardContext(n);
    assert.deepEqual(Object.keys(ctx).sort(), KEYS);
    // no undefined anywhere: a JSON round-trip is the identity
    assert.deepEqual(JSON.parse(JSON.stringify(ctx)), ctx);
    assert.ok(["notes", "dynamics", "paths", "dependencies", "dependants", "output"].every((k) => Array.isArray(ctx[k]) && ctx[k].every((s) => typeof s === "string")));
    assert.equal(typeof ctx.help, "string");
    assert.equal(typeof ctx.conflict, "boolean");
    assert.equal("template" in ctx, false);
  }
});

test("property: the help template shows the help unchanged, the notes in order", () => {
  const help = tramaj.parseProgram(".p($ctx.help)");
  const notes = tramaj.parseProgram(".ul(map($ctx.notes, (x) => .li($x)))");
  for (let i = 0; i < CASES; i++) {
    const n = { help: randomText(), notes: Array.from({ length: int(5) }, () => randomText()) };
    const ctx = ct.cardContext(n);
    assert.equal(tt.specText(ct.foldCard(tramaj, help, ctx).spec), n.help);
    assert.deepEqual(ct.foldCard(tramaj, notes, ctx).spec.children.map(tt.specText), n.notes);
  }
});

// random normative Node JSON, allowed in a card and not
const TAGS = [...ct.CARD_TAGS, "script", "a", "img", "iframe"];
function randomDoc(depth) {
  const r = int(depth > 3 ? 2 : 4);
  if (r === 0) return { type: "text", value: pick([randomText(8), 3, true, null]), annotations: {} };
  if (r === 1) return { type: "text", value: randomText(8), annotations: {} };
  if (r === 2) return { type: "fragment", children: Array.from({ length: int(3) }, () => randomDoc(depth + 1)), annotations: {} };
  const tag = int(10) === 0 ? pick(TAGS.slice(-4)) : pick(TAGS);
  const attributes = Array.from({ length: int(3) }, () => ({ kind: "attribute", name: pick(["class", "title", "onclick", "href"]), value: randomText(5) }));
  const children = tag === "br" || tag === "hr" ? [] : Array.from({ length: int(3) }, () => randomDoc(depth + 1));
  return { type: "element", tag, attributes, value: null, children, annotations: {} };
}
const forbidden = (n) => (n.type === "element" && !ct.CARD_TAGS.has(n.tag)) || (n.children || []).some(forbidden);
const tags = (s) => (typeof s === "string" ? [] : [...(s.tag ? [s.tag] : []), ...s.children.flatMap(tags)]);

test("property: a card document draws iff every tag is a card tag, and draws only card tags", () => {
  for (let i = 0; i < CASES * 2; i++) {
    const d = randomDoc(0);
    if (forbidden(d)) {
      assert.throws(() => tt.specOf(tramaj, d, ct.CARD_ALLOW), tt.TemplateError);
    } else {
      const s = tt.specOf(tramaj, d, ct.CARD_ALLOW);
      assert.ok(tags(s).every((t) => ct.CARD_TAGS.has(t)));
    }
  }
});
