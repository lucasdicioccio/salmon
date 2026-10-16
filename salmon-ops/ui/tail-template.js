// Tail templates: the pure half of shaping how a live-tail window renders its
// lines with a Tramaj template (feature 7fee51f2). Opt-in: `ui.js` imports
// this module, and the vendored tramaj-js under `vendor/tramaj-js/`, only
// once a window has a template, so a page with none loads neither.
//
// No DOM and no import of tramaj here: the evaluator is passed in, so the
// contract below can be checked under node (`salmon-ops/ui-test/`).
//
// The contract a template is written against, and which must stay stable:
//
// - It is a Tramaj program evaluated once per line, in concrete mode, with
//   no libraries, against `$ctx` = `tailContext(entry, node)`:
//     line     the text the window shows without a template (with its
//              `[updown] `, `[upkeep] `, `err| ` or `msg| ` mark)
//     text     the same without the mark
//     stream   "output", "updown" or "upkeep"
//     kind     the event's kind ("output", "log", "eval", "failed", ...)
//     channel  "stdout", "stderr" or "message" for a `log` line, else null
//     seq      the event's number, or null for a line from `status.output`
//     words    `text` split on whitespace (at most TEMPLATE_MAX_WORDS)
//     fields   `text` read as a JSON object, else its `key=value` words, else {}
//     node     {ref, short, shorthand, convergence} of the tailed node
//   Every key is always present (null when unknown): Tramaj refuses a path
//   that is not there, so a template never has to guard a key with `has`.
//   Tramaj has no string builtins; `words` and `fields` are how a template
//   picks a line out by its content (`eq(lookup($ctx.words, 0, ""), "ERROR")`,
//   `lookup($ctx.fields, "level", null)`).
// - Its result decides the line: `null` drops it (a filter); a string or
//   other scalar is shown as text; a document is shown as elements.
// - Only the phrasing tags in TEMPLATE_TAGS are drawn, and of attributes
//   only `class` and `title` (string-valued). A tag outside the list makes
//   the line an error; any other attribute, and every action, is ignored.
//   Nothing is ever parsed as HTML. `ui.css` styles the classes t-err,
//   t-warn, t-ok, t-muted and t-hl inside a tail.
// - A line whose evaluation fails is shown raw; the window says the first
//   error once. A template that does not parse leaves every line as text.
// - A line is evaluated once per template, with `node` as it was then: a
//   later change of the node's convergence does not redraw old lines.

export const TEMPLATE_TAGS = new Set(["span", "b", "strong", "i", "em", "u", "s", "code", "mark", "small", "sub", "sup", "br"]);
export const TEMPLATE_ATTRS = new Set(["class", "title"]);
export const TEMPLATE_MAX_WORDS = 64;
// a line's document is cut off past these, as an error rather than a page
// that stalls drawing a template's runaway `map`
export const TEMPLATE_MAX_NODES = 512;
export const TEMPLATE_MAX_DEPTH = 32;

export class TemplateError extends Error {
  name = "TemplateError";
}

// The `$ctx` of one line. `entry` is a window's line as `ui.js` keeps it
// ({line, text, stream, kind, channel, seq}); `node` the tailed node from
// /dag, or undefined once it is gone.
export function tailContext(entry, node) {
  const text = typeof entry.text === "string" ? entry.text : String(entry.line ?? "");
  return {
    line: String(entry.line ?? ""),
    text,
    stream: entry.stream ?? null,
    kind: entry.kind ?? null,
    channel: entry.channel ?? null,
    seq: typeof entry.seq === "number" ? entry.seq : null,
    words: wordsOf(text),
    fields: fieldsOf(text),
    node: node
      ? {
          ref: (node.ref && node.ref.full) ?? null,
          short: (node.ref && node.ref.short) ?? null,
          shorthand: node.shorthand ?? null,
          convergence: node.convergence ?? null,
        }
      : { ref: null, short: null, shorthand: null, convergence: null },
  };
}

export function wordsOf(text) {
  return text.split(/\s+/).filter((w) => w.length > 0).slice(0, TEMPLATE_MAX_WORDS);
}

// A structured log line's fields: the line itself if it is a JSON object,
// else its `key=value` words (logfmt without quoting), else {}.
export function fieldsOf(text) {
  const s = text.trim();
  if (s.startsWith("{")) {
    try {
      const v = JSON.parse(s);
      if (v !== null && typeof v === "object" && !Array.isArray(v)) return v;
    } catch {
      // not JSON; try the words
    }
  }
  const out = {};
  for (const w of wordsOf(s)) {
    const m = /^([A-Za-z_][A-Za-z0-9_.-]*)=(.*)$/.exec(w);
    if (m) out[m[1]] = m[2];
  }
  return out;
}

// One line through a parsed template: {drop: true}, {spec}, or {error}.
// `tramaj` is the tramaj-js module (runProgram, stringify).
export function foldLine(tramaj, program, ctx) {
  let out;
  try {
    out = tramaj.runProgram("concrete", new Map(), ctx, program);
  } catch (e) {
    return { error: messageOf(e) };
  }
  if (out === null) return { drop: true };
  try {
    return { spec: specOf(tramaj, out) };
  } catch (e) {
    return { error: messageOf(e) };
  }
}

// A template's result (Tramaj's normative Node JSON, or a scalar) as the
// tree `ui.js` draws: a string, or {tag, cls, title, children} where a null
// tag is a fragment. Throws TemplateError for what is not drawn.
export function specOf(tramaj, out) {
  const budget = { nodes: 0 };
  return specAt(tramaj, out, 0, budget);
}

function specAt(tramaj, v, depth, budget) {
  if (++budget.nodes > TEMPLATE_MAX_NODES) throw new TemplateError(`more than ${TEMPLATE_MAX_NODES} nodes in one line`);
  if (depth > TEMPLATE_MAX_DEPTH) throw new TemplateError(`nested deeper than ${TEMPLATE_MAX_DEPTH}`);
  if (v === null || typeof v !== "object" || typeof v.type !== "string") return scalarText(tramaj, v);
  switch (v.type) {
    case "text":
      return scalarText(tramaj, v.value);
    case "fragment":
      return { tag: null, cls: null, title: null, children: childrenOf(tramaj, v, depth, budget) };
    case "element": {
      if (!TEMPLATE_TAGS.has(v.tag)) throw new TemplateError(`tag "${v.tag}" is not drawn in a tail (allowed: ${[...TEMPLATE_TAGS].join(" ")})`);
      let cls = null;
      let title = null;
      for (const a of Array.isArray(v.attributes) ? v.attributes : []) {
        if (a.kind !== "attribute" || !TEMPLATE_ATTRS.has(a.name) || typeof a.value !== "string") continue;
        if (a.name === "class") cls = a.value;
        else title = a.value;
      }
      return { tag: v.tag, cls, title, children: childrenOf(tramaj, v, depth, budget) };
    }
    default:
      return scalarText(tramaj, v);
  }
}

function childrenOf(tramaj, v, depth, budget) {
  return (Array.isArray(v.children) ? v.children : []).map((c) => specAt(tramaj, c, depth + 1, budget));
}

// A text leaf keeps its Tramaj value; a string is itself, null nothing, and
// anything else Tramaj's own JSON text (so a whole float reads `3.0`).
function scalarText(tramaj, v) {
  if (typeof v === "string") return v;
  if (v === null || v === undefined) return "";
  try {
    return tramaj.stringify(v);
  } catch {
    return String(v);
  }
}

// The characters a spec shows, `br` as a newline.
export function specText(spec) {
  if (typeof spec === "string") return spec;
  if (spec.tag === "br") return "\n";
  return spec.children.map(specText).join("");
}

export function messageOf(e) {
  if (e && typeof e.message === "string" && e.message) return `${e.name || "Error"}: ${e.message}`;
  return String(e);
}
