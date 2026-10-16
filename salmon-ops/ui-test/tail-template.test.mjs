// The tail-template contract (ui/tail-template.js) against the vendored
// tramaj-js, under node: unit cases and seeded random properties.
// Run: node salmon-ops/ui-test/tail-template.test.mjs
// (Test/UiTailTemplateSpec.hs runs it when `node` is on PATH.)
// SALMON_UI_TEST_SEED fixes the properties' seed; it is printed either way.

import { test } from "node:test";
import assert from "node:assert/strict";

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
const ALPHABET = ["a", "Z", "0", " ", " ", "\t", "<", ">", "&", '"', "'", "{", "}", "=", ":", "-", "é", "日", "\\", "`", "$", "."];
const randomText = (max = 40) => Array.from({ length: int(max) }, () => pick(ALPHABET)).join("");

const node = { ref: { full: "f".repeat(64), short: "ffffffff" }, shorthand: "file /tmp/x", convergence: "converged" };
const entry = (over = {}) => ({ line: "err| boom", text: "boom", stream: "output", kind: "log", channel: "stderr", seq: 7, ...over });
const run = (src, ctx) => tt.foldLine(tramaj, tramaj.parseProgram(src), ctx);

test("the context carries every key, null when unknown", () => {
  const ctx = tt.tailContext(entry(), node);
  assert.deepEqual(Object.keys(ctx).sort(), ["channel", "fields", "kind", "line", "node", "seq", "stream", "text", "words"]);
  assert.deepEqual(ctx.node, { ref: "f".repeat(64), short: "ffffffff", shorthand: "file /tmp/x", convergence: "converged" });
  const bare = tt.tailContext({ line: "x" }, undefined);
  assert.equal(bare.text, "x");
  assert.equal(bare.seq, null);
  assert.equal(bare.channel, null);
  assert.deepEqual(bare.node, { ref: null, short: null, shorthand: null, convergence: null });
  // a template reading any documented key evaluates on a bare line
  for (const k of ["line", "text", "stream", "kind", "channel", "seq", "words", "fields", "node.ref", "node.convergence"]) {
    assert.ok(!run(`str($ctx.${k})`, bare).error, k);
  }
});

test("fields: a JSON object, else key=value words, else nothing", () => {
  assert.deepEqual(tt.fieldsOf(' {"level":"warn","n":3} '), { level: "warn", n: 3 });
  assert.deepEqual(tt.fieldsOf("ts=1 level=error msg=disk full"), { ts: "1", level: "error", msg: "disk" });
  assert.deepEqual(tt.fieldsOf("[1,2] a=b"), { a: "b" });
  assert.deepEqual(tt.fieldsOf("plain words"), {});
  assert.deepEqual(tt.fieldsOf("{not json"), {});
});

test("a document draws as allowed elements; class and title only, actions ignored", () => {
  const r = run('.span(class: "t-err", title: "why", style: "x", action("click", "k", {}), $ctx.text, " #", $ctx.seq)', tt.tailContext(entry(), node));
  assert.deepEqual(r, { spec: { tag: "span", cls: "t-err", title: "why", children: ["boom", " #", "7"] } });
  assert.equal(tt.specText(r.spec), "boom #7");
});

test("null drops the line; a scalar is text", () => {
  const keepStderr = 'branch(null, eq($ctx.channel, "stderr"), $ctx.line)';
  assert.deepEqual(run(keepStderr, tt.tailContext(entry({ channel: "stdout" }), node)), { drop: true });
  assert.deepEqual(run(keepStderr, tt.tailContext(entry(), node)), { spec: "err| boom" });
  assert.deepEqual(run("$ctx.seq", tt.tailContext(entry(), node)), { spec: "7" });
  assert.deepEqual(run("1.0", tt.tailContext(entry(), node)), { spec: "1.0" });
});

test("structured fields are readable", () => {
  const ctx = tt.tailContext(entry({ text: '{"level":"warn","msg":"slow"}' }), node);
  const r = run('.span(class: lookup($ctx.fields, "level", "none"), $ctx.fields.msg)', ctx);
  assert.deepEqual(r.spec, { tag: "span", cls: "warn", title: null, children: ["slow"] });
});

test("what is not drawn is an error, never markup", () => {
  for (const tag of ["script", "a", "img", "div", "iframe", "style"]) {
    const r = run(`.${tag}("x")`, tt.tailContext(entry(), node));
    assert.match(r.error, /is not drawn/, tag);
  }
  assert.match(run("$ctx.nope", tt.tailContext(entry(), node)).error, /PathNotFound/);
  assert.throws(() => tramaj.parseProgram(".span("), /ParseError|expected/);
  const many = tt.tailContext(entry({ text: JSON.stringify({ xs: Array.from({ length: 600 }, (_, i) => i) }) }), node);
  assert.match(run(".span(map($ctx.fields.xs, (x) => .b($x)))", many).error, /more than/);
});

test("property: the identity template shows the line unchanged", () => {
  const asDoc = tramaj.parseProgram(".span($ctx.line)");
  const asExpr = tramaj.parseProgram("$ctx.line");
  for (let i = 0; i < CASES; i++) {
    const line = randomText();
    const ctx = tt.tailContext({ line, text: line, stream: "output", kind: "output", channel: null, seq: i }, node);
    assert.equal(tt.specText(tt.foldLine(tramaj, asDoc, ctx).spec), line);
    assert.equal(tt.specText(tt.foldLine(tramaj, asExpr, ctx).spec), line);
  }
});

test("property: words is the text split on whitespace", () => {
  for (let i = 0; i < CASES; i++) {
    const text = randomText(120);
    const ws = tt.tailContext({ line: text, text }, node).words;
    const expected = text.split(/\s+/).filter(Boolean);
    assert.deepEqual(ws, expected.slice(0, tt.TEMPLATE_MAX_WORDS));
    assert.ok(ws.every((w) => w.length > 0 && !/\s/.test(w)));
  }
});

test("property: a first-word filter keeps exactly the lines a JS filter keeps", () => {
  const p = tramaj.parseProgram('branch(null, eq(lookup($ctx.words, 0, ""), "ERROR"), $ctx.line)');
  for (let i = 0; i < CASES; i++) {
    const text = pick(["ERROR ", "error ", "WARN ", " ERROR ", "ERRORS ", ""]) + randomText(20);
    const r = tt.foldLine(tramaj, p, tt.tailContext({ line: text, text }, node));
    const keep = text.split(/\s+/).filter(Boolean)[0] === "ERROR";
    assert.equal(Boolean(r.drop), !keep, JSON.stringify(text));
    if (keep) assert.equal(r.spec, text);
  }
});

// random normative Node JSON, allowed and not
const TAGS = [...tt.TEMPLATE_TAGS, "script", "a", "div", "img"];
const ATTRS = ["class", "title", "onclick", "href", "style", "class"];
function randomNode(depth) {
  const r = int(depth > 3 ? 2 : 4);
  if (r === 0) return { type: "text", value: pick([randomText(8), 3, true, null]), annotations: {} };
  if (r === 1) return { type: "text", value: randomText(8), annotations: {} };
  if (r === 2) return { type: "fragment", children: Array.from({ length: int(3) }, () => randomNode(depth + 1)), annotations: {} };
  const tag = int(10) === 0 ? pick(TAGS.slice(-4)) : pick(TAGS);
  const attributes = Array.from({ length: int(4) }, () =>
    int(4) === 0 ? { kind: "action", event: "click", key: "k", payload: {} } : { kind: "attribute", name: pick(ATTRS), value: pick([randomText(6), 1]) },
  );
  const children = tag === "br" ? [] : Array.from({ length: int(3) }, () => randomNode(depth + 1));
  return { type: "element", tag, attributes, value: null, children, annotations: {} };
}
const hasForbidden = (n) => (n.type === "element" && !tt.TEMPLATE_TAGS.has(n.tag)) || (n.children || []).some(hasForbidden);
const leafText = (n) =>
  n.type === "text" ? (typeof n.value === "string" ? n.value : n.value === null ? "" : tramaj.stringify(n.value)) : n.tag === "br" ? "\n" : n.children.map(leafText).join("");
function lastAttr(n, name) {
  let v = null;
  for (const a of n.attributes) if (a.kind === "attribute" && a.name === name && typeof a.value === "string") v = a.value;
  return v;
}
function checkSpec(n, s) {
  if (n.type === "text") return assert.equal(typeof s, "string");
  if (n.type === "fragment") assert.equal(s.tag, null);
  else {
    assert.ok(tt.TEMPLATE_TAGS.has(s.tag));
    assert.equal(s.cls, lastAttr(n, "class"));
    assert.equal(s.title, lastAttr(n, "title"));
  }
  assert.deepEqual(Object.keys(s).sort(), ["children", "cls", "tag", "title"]);
  n.children.forEach((c, i) => checkSpec(c, s.children[i]));
}

test("property: a document draws iff every tag is allowed, keeping its text and only class/title", () => {
  for (let i = 0; i < CASES * 2; i++) {
    const n = randomNode(0);
    if (hasForbidden(n)) {
      assert.throws(() => tt.specOf(tramaj, n), tt.TemplateError);
    } else {
      const s = tt.specOf(tramaj, n);
      checkSpec(n, s);
      assert.equal(tt.specText(s), leafText(n));
    }
  }
});
