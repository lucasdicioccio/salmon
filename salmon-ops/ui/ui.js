// The web UI: `/dag` drawn as a layered graph, kept live from `/events`.
//
// Holds no state the server does not. The picture is the last `/dag`
// snapshot; what moves between snapshots is applied from the event stream
// subscribed at that snapshot's `seq`, and anything that changes the node
// set or settles convergence (`declared`, `cleared`, `converge-stop`, a
// `gap`, a lost stream) is answered by fetching `/dag` again and
// resubscribing from its `seq`. Reload is the same thing by hand.
//
// Every write is `POST /command?async`: the line is queued, the answer is
// the sequence number it was queued at and the origin it was queued under,
// and what it did is read off `/events` like everything else — the reports
// carrying that origin are the command's, which is what lights the nodes it
// touched, fills the log under the command line and says when it is done
// (`hung-up`). Never the synchronous form: a sync `up` holds the request for
// the whole pass, and this page is the thing that would be waiting.
//
// The seed form is `/help/seed` (the binary's own `config --help`) in a
// `<pre>`, a text field for the words, and the three declaring verbs; the
// history under it is `/history`, fetched again on `declared`/`cleared`.
// `quit` is deliberately not offered: a page cannot answer for the socket it
// is served on, and leaving the loop is not something to click.

const $ = (id) => document.getElementById(id);
const SVG = "http://www.w3.org/2000/svg";

// ---------------------------------------------------------------------------
// state

const state = {
  nodes: new Map(), // full ref -> node object from /dag, plus live overlay
  order: [], // full refs in dagOrder
  seq: 0,
  mode: "–",
  readOnly: false, // GET /access said read-only (serve --http-read-only)
  converge: null, // last converge-stop {ok, remaining}
  selected: null, // full ref
  source: null, // the EventSource
  reloadTimer: null,
  retryDelay: 1000,
  requests: new Map(), // origin name -> {seq, origin, line, reports, done, li}
  unclaimed: [], // events with an origin no request has claimed yet
  history: null, // last /history: {seeds: [...], elided}
  seedHelpLoaded: false,
  tails: new Map(), // full ref -> {lines, paused, held, pre, ...} for each pinned live tail
  templating: null, // the promise of {tt, tramaj} once a tail has a template (lazy: tail-template.js + vendored tramaj-js)
  templatingLib: null, // what that promise resolved to
  cardPrograms: new Map(), // a node's card template source -> {program} or {error}, parsed once
  defaultCards: new Set(), // full refs whose card the operator switched back to the default one
  cardLoadError: null, // why the evaluator did not load for a card, so the panel does not retry in a loop
  zoom: { scale: 1, tx: 0, ty: 0 }, // the #viewport transform; world units are the raw layout() pixels
  zoomInit: false, // true once the first fit-to-view has run
  worldSize: null, // the current layout()'s {width, height}, for fitView
  view: "graph", // "graph" (the default) or "rows", the opt-in list view
  rowFilter: "", // a stateClass the rows view shows only, or "" for all
  rowsFrame: null, // a pending requestAnimationFrame re-render of the rows
  rowsTimer: null, // the rows view's once-a-second clock for its "since" cells
  loadedOnce: false, // a node first seen in a later /dag was declared while watching
};

const MIN_ZOOM = 0.1;
const MAX_ZOOM = 4;

// The dock holds a few windows, not one per node: a tail is for the node
// being watched right now.
const TAIL_MAX_WINDOWS = 4;
const TAIL_MAX_LINES = 500;
const TAIL_STORE = "salmon-tails";
// ref -> a tail window's Tramaj template source, kept across unpinning
const TAIL_TEMPLATE_STORE = "salmon-tail-templates";
const TAIL_TEMPLATE_KEEP = 64;

// How long a node stays marked as touched once the command that touched it
// has been handled.
const TOUCH_LINGER_MS = 4000;
const TOAST_MS = 6000;

// ---------------------------------------------------------------------------
// fetching and subscribing

async function loadDag() {
  clearTimeout(state.reloadTimer);
  state.reloadTimer = null;
  closeStream();
  let dag;
  try {
    const r = await fetch("dag", { cache: "no-store" });
    if (r.status === 401) return signedOut();
    if (!r.ok) throw new Error(`/dag answered ${r.status}`);
    dag = await r.json();
  } catch (err) {
    setStream("lost", String(err.message || err));
    state.retryDelay = Math.min(state.retryDelay * 2, 15000);
    state.reloadTimer = setTimeout(loadDag, state.retryDelay);
    return;
  }
  state.retryDelay = 1000;
  state.seq = dag.seq;
  state.mode = dag.mode;
  const before = state.nodes;
  state.nodes = new Map();
  state.order = [];
  for (const n of dag.nodes) {
    n.last = null;
    n.lastReason = null;
    n.machine = null;
    // a snapshot is fetched on `declared` and `converge-stop`, mid-command,
    // and which command touched a node is this page's knowledge, not /dag's
    n.touched = before.has(n.ref.full) ? before.get(n.ref.full).touched : null;
    carryClock(n, before.get(n.ref.full));
    state.nodes.set(n.ref.full, n);
    state.order.push(n.ref.full);
  }
  if (state.selected && !state.nodes.has(state.selected)) state.selected = null;
  state.loadedOnce = true;
  reconcileTails();
  render();
  subscribe(dag.seq);
}

// Coalesce: several events in a row that each want a fresh snapshot cost
// one fetch.
function scheduleReload() {
  if (state.reloadTimer) return;
  state.reloadTimer = setTimeout(loadDag, 250);
}

function closeStream() {
  if (state.source) {
    state.source.close();
    state.source = null;
  }
}

function subscribe(since) {
  const es = new EventSource(`events?since=${since}`);
  state.source = es;
  es.onopen = () => setStream("live");
  es.onmessage = (m) => {
    let e;
    try {
      e = JSON.parse(m.data);
    } catch {
      return;
    }
    if (typeof e.seq === "number") state.seq = e.seq;
    applyEvent(e);
    attribute(e);
    renderHeader();
  };
  es.onerror = () => {
    // EventSource would reconnect on its own, but with Last-Event-ID, which
    // the server does not read; resuming is `/dag` then `?since=` its seq.
    if (state.source !== es) return;
    closeStream();
    setStream("lost");
    scheduleReload();
  };
}

function setStream(cls, detail) {
  const el = $("h-stream");
  el.className = cls;
  el.textContent = cls === "live" ? "live" : detail ? `lost: ${detail}` : "reconnecting";
}

// ---------------------------------------------------------------------------
// events onto nodes

function applyEvent(e) {
  switch (e.stream) {
    case "server":
      if (e.kind === "gap") scheduleReload();
      return;
    case "serve":
      applyServe(e);
      return;
    case "updown":
      applyUpDown(e);
      return;
    case "upkeep":
      applyUpkeep(e);
      return;
    case "output":
      applyOutput(e);
      return;
    default:
      return;
  }
}

// ---------------------------------------------------------------------------
// live tails: a dock of pinned mini terminals, one per node. Besides a
// managed action's own `output` lines, a tailed node's updown/upkeep reports
// are formatted into the same window (prefixed by stream) — those exist for
// every node, so a one-shot builtin's tail is not silent just because it has
// no `managed` action to produce raw output.

function storedPins() {
  try {
    const v = JSON.parse(localStorage.getItem(TAIL_STORE) || "[]");
    return Array.isArray(v) ? v.filter((x) => typeof x === "string") : [];
  } catch {
    return [];
  }
}

function storePins() {
  try {
    localStorage.setItem(TAIL_STORE, JSON.stringify([...state.tails.keys()]));
  } catch {
    // page storage may be unavailable; windows just do not survive a reload
  }
}

// After every /dag: a window for a node that is no longer there closes (the
// node was retired and pruned), and the pins remembered from a previous page
// load are opened for the nodes that are. Every open window is then topped
// up from the node's `tail` (the newest events about it, as /events sent
// them, none above the snapshot's seq): a window only takes the events
// numbered above the last one it shows, so a reload fills a fresh window and
// a resubscription fills whatever the old stream missed, each line once.
function reconcileTails() {
  for (const ref of storedPins()) {
    if (state.nodes.has(ref) && !state.tails.has(ref) && state.tails.size < TAIL_MAX_WINDOWS) {
      state.tails.set(ref, newTail(ref));
    }
  }
  for (const ref of [...state.tails.keys()]) {
    const n = state.nodes.get(ref);
    if (!n) {
      state.tails.delete(ref);
      continue;
    }
    seedTail(state.tails.get(ref), n);
  }
  storePins();
  renderDock();
}

// `seq` is the number of the newest event the window shows. `lines` and
// `held` are entries ({line, text, stream, kind, channel, seq}, see
// `tailEntryOf`); `line` is what the window shows without a template.
function newTail(ref) {
  const t = { lines: [], paused: false, held: [], pre: null, seq: 0, ref };
  resetTemplate(t, storedTemplate(ref));
  return t;
}

function seedTail(t, n) {
  if (Array.isArray(n.tail)) {
    for (const e of n.tail) {
      if (typeof e.seq !== "number" || e.seq <= t.seq) continue;
      const entry = tailEntryOf(e);
      if (entry === null) continue;
      t.lines.push(entry);
      t.seq = e.seq;
    }
    if (t.lines.length > TAIL_MAX_LINES) t.lines.splice(0, t.lines.length - TAIL_MAX_LINES);
  } else if (t.lines.length === 0 && n.status && Array.isArray(n.status.output)) {
    // a server from before /dag carried `tail`: the held action's ring
    t.lines = n.status.output.slice(-TAIL_MAX_LINES).map((line) => plainEntry(String(line)));
  }
}

function toggleTail(ref) {
  if (state.tails.has(ref)) {
    state.tails.delete(ref);
  } else {
    // the oldest pin makes room
    if (state.tails.size >= TAIL_MAX_WINDOWS) state.tails.delete(state.tails.keys().next().value);
    const t = newTail(ref);
    const n = state.nodes.get(ref);
    if (n) seedTail(t, n);
    state.tails.set(ref, t);
  }
  storePins();
  renderDock();
  renderPanel();
}

// What a window shows for one event about its node, live or from a `tail`:
// the same entry either way. Its `line` is the text shown without a
// template; the rest is what a tail template reads besides: the line without
// its mark, and the event's stream, kind, channel and number. null for an
// event a window does not show.
function tailEntryOf(e) {
  switch (e.stream) {
    case "output":
      return outputEntry(e);
    case "updown":
      return markedEntry("updown", e.kind, "[updown] ", formatUpDownLine(e), e.seq);
    case "upkeep":
      return e.kind === "acted" && e.report
        ? markedEntry("updown", e.report.kind, "[updown] ", formatUpDownLine(e.report), e.seq)
        : markedEntry("upkeep", e.kind, "[upkeep] ", formatUpkeepLine(e), e.seq);
    default:
      return null;
  }
}

function markedEntry(stream, kind, mark, text, seq) {
  return { line: mark + text, text, stream, kind: kind ?? null, channel: null, seq: typeof seq === "number" ? seq : null };
}

// a line from a server too old for `tail`: the held action's ring
function plainEntry(line) {
  return { line, text: line, stream: "output", kind: "output", channel: null, seq: null };
}

// The `output` stream carries two kinds: `output`, a line a held (`managed`)
// action wrote, and `log`, a line any node said while its `up` was running:
// a streamed command's stdout or stderr, or a progress message of its own.
function outputEntry(e) {
  if (typeof e.line !== "string") return null;
  const mark = e.kind === "log" && e.channel === "stderr" ? "err| " : e.kind === "log" && e.channel === "message" ? "msg| " : "";
  return {
    line: mark + e.line,
    text: e.line,
    stream: "output",
    kind: e.kind ?? null,
    channel: e.kind === "log" ? (e.channel ?? null) : null,
    seq: typeof e.seq === "number" ? e.seq : null,
  };
}

function applyOutput(e) {
  if (!e.ref) return;
  if (isProgress(e)) {
    const n = nodeOf(e);
    if (n) {
      n.progress = e.line;
      paintRow(n);
    }
  }
  const entry = outputEntry(e);
  if (entry !== null) pushTailLine(e.ref.full, entry, e.seq);
}

// Shared by a managed action's raw output and by the formatted updown/upkeep
// report lines below; a no-op if that ref has no open tail window, or if the
// window already shows the event (seeded from a `tail` that had it).
function pushTailLine(ref, entry, seq) {
  const t = state.tails.get(ref);
  if (!t) return;
  if (typeof seq === "number") {
    if (seq <= t.seq) return;
    t.seq = seq;
  }
  // paused holds the view still; what arrives meanwhile is kept for resume
  const into = t.paused ? t.held : t.lines;
  into.push(entry);
  if (into.length > TAIL_MAX_LINES) into.splice(0, into.length - TAIL_MAX_LINES);
  if (!t.paused) paintTail(t);
}

function formatUpDownLine(e) {
  switch (e.kind) {
    case "failed":
      return `failed: ${e.error}`;
    case "conflicting":
      return `conflicting: kept ${e.kept && e.kept.shorthand}, replaced ${e.replaced && e.replaced.shorthand}`;
    case "instructed":
      return `instructed: ${e.instruction}`;
    case "dropped-instructions":
      return `dropped ${e.dropped} instruction(s)`;
    default:
      return e.kind;
  }
}

function formatUpkeepLine(e) {
  switch (e.kind) {
    case "next-look":
      return `next-look: ${(e.check && e.check.kind) || "?"}${e.delay_us != null ? ` (retry in ${Math.round(e.delay_us / 1e6)}s)` : ""}`;
    case "upkeep":
      return `upkeep: ${e.state}`;
    case "downkeep":
      return `downkeep: ${e.state}`;
    case "demoted":
      return `demoted by ${e.dependency && e.dependency.short}`;
    case "gave-up":
      return `gave up after ${e.failures} failure(s)`;
    case "wedged":
      return `wedged (${Math.round(e.silent_us / 1e6)}s silent)`;
    case "reapplying":
      return `reapplying (retry in ${Math.round(e.delay_us / 1e6)}s)`;
    default:
      return e.kind;
  }
}

function paintTail(t) {
  if (!t.pre) return;
  const stick = t.pre.scrollTop + t.pre.clientHeight >= t.pre.scrollHeight - 4;
  fillTailBody(t);
  if (stick) t.pre.scrollTop = t.pre.scrollHeight;
}

// Without a template (or until it is parsed), the lines as text, as before.
// With one, each line through it: drawn once per template and kept on the
// entry, so a new line does not re-evaluate the other 499.
function fillTailBody(t) {
  if (!t.program || !state.templatingLib) {
    t.pre.textContent = t.lines.map((l) => l.line).join("\n");
    paintTemplateError(t);
    return;
  }
  const { tt, tramaj } = state.templatingLib;
  const node = state.nodes.get(t.ref);
  const rows = [];
  for (const entry of t.lines) {
    if (entry.drawnFor !== t.program) {
      entry.drawnFor = t.program;
      const r = tt.foldLine(tramaj, t.program, tt.tailContext(entry, node));
      if (r.error && !t.templateError) t.templateError = `${r.error}; such a line is shown raw (first: ${entry.line.slice(0, 120)})`;
      entry.drawn = r.drop ? null : lineElement(r.error ? entry.line : r.spec, Boolean(r.error));
    }
    if (entry.drawn) rows.push(entry.drawn);
  }
  t.pre.replaceChildren(...rows);
  paintTemplateError(t);
}

function lineElement(spec, raw) {
  const row = document.createElement("span");
  row.className = raw ? "tail-line tail-raw" : "tail-line";
  row.appendChild(specElement(spec));
  return row;
}

// A spec from tail-template.js's `specOf`: only allowlisted tags, `class`
// and `title`, text as text. Never innerHTML.
function specElement(spec) {
  if (typeof spec === "string") return document.createTextNode(spec);
  const el = spec.tag === null ? document.createDocumentFragment() : document.createElement(spec.tag);
  if (spec.tag !== null) {
    if (spec.cls) el.className = spec.cls;
    if (spec.title) el.title = spec.title;
  }
  for (const c of spec.children) el.appendChild(specElement(c));
  return el;
}

function paintTemplateError(t) {
  if (!t.errorLine) return;
  t.errorLine.hidden = !t.templateError;
  t.errorLine.textContent = t.templateError ? `template: ${t.templateError}` : "";
}

// ---------------------------------------------------------------------------
// tail templates (opt-in): a window may carry a Tramaj template that shapes
// how each of its lines renders. The contract is in tail-template.js; this
// half keeps the sources, loads the evaluator on first use, and draws.

function storedTemplates() {
  try {
    const v = JSON.parse(localStorage.getItem(TAIL_TEMPLATE_STORE) || "{}");
    return v && typeof v === "object" && !Array.isArray(v) ? v : {};
  } catch {
    return {};
  }
}

function storedTemplate(ref) {
  const v = storedTemplates()[ref];
  return typeof v === "string" && v.trim() !== "" ? v : null;
}

function storeTemplate(ref, source) {
  try {
    const all = storedTemplates();
    delete all[ref];
    if (source !== null) all[ref] = source;
    // newest last; the oldest go past the cap
    const keys = Object.keys(all);
    for (const k of keys.slice(0, Math.max(0, keys.length - TAIL_TEMPLATE_KEEP))) delete all[k];
    localStorage.setItem(TAIL_TEMPLATE_STORE, JSON.stringify(all));
  } catch {
    // page storage may be unavailable; the template lasts as long as the page
  }
}

// Set a window's template source (null for none) and parse it, loading the
// evaluator first if this is the first template on the page.
function resetTemplate(t, source) {
  t.template = source;
  t.program = null;
  t.templateError = null;
  if (t.draft === undefined) t.draft = source ?? "";
  if (source === null) return;
  if (state.templatingLib) {
    parseTemplate(t);
    return;
  }
  loadTemplating().then(
    () => {
      if (t.template !== source) return;
      parseTemplate(t);
      paintTail(t);
    },
    (e) => {
      t.templateError = `the template evaluator did not load: ${e && e.message ? e.message : e}`;
      paintTail(t);
    },
  );
}

function parseTemplate(t) {
  const { tt, tramaj } = state.templatingLib;
  try {
    t.program = tramaj.parseProgram(t.template);
  } catch (e) {
    t.program = null;
    t.templateError = `does not parse, lines shown as text: ${tt.messageOf(e)}`;
  }
}

// Same-origin, embedded in the binary: no third-party fetch.
function loadTemplating() {
  if (!state.templating) {
    state.templating = Promise.all([import("./tail-template.js"), import("./vendor/tramaj-js/index.js"), import("./card-template.js")]).then(([tt, tramaj, ct]) => {
      state.templatingLib = { tt, tramaj, ct };
      return state.templatingLib;
    });
    state.templating.catch(() => {
      state.templating = null; // a later template tries again
    });
  }
  return state.templating;
}

const TEMPLATE_HINT =
  "A Tramaj template per line. $ctx: line, text, stream, kind, channel, seq, words, fields, node {ref, short, shorthand, convergence}. " +
  "null drops the line; tags: span b strong i em u s code mark small sub sup br; attributes: class, title " +
  "(classes t-err t-warn t-ok t-muted t-hl are styled). " +
  'e.g. branch(.span($ctx.line), eq($ctx.channel, "stderr"), .span(class: "t-err", $ctx.line))';

function templateEditor(t) {
  const box = document.createElement("div");
  box.className = "tail-template";
  const area = document.createElement("textarea");
  area.rows = 4;
  area.spellcheck = false;
  area.placeholder = TEMPLATE_HINT;
  area.title = TEMPLATE_HINT;
  area.value = t.draft ?? "";
  area.addEventListener("input", () => {
    t.draft = area.value;
  });
  const apply = document.createElement("button");
  apply.type = "button";
  apply.textContent = "apply";
  apply.title = "render this window's lines through the template (kept in this browser)";
  apply.addEventListener("click", () => {
    const source = area.value.trim() === "" ? null : area.value;
    storeTemplate(t.ref, source);
    resetTemplate(t, source);
    renderDock();
  });
  const remove = document.createElement("button");
  remove.type = "button";
  remove.textContent = "remove";
  remove.title = "show the lines as plain text again";
  remove.addEventListener("click", () => {
    t.draft = "";
    storeTemplate(t.ref, null);
    resetTemplate(t, null);
    t.editing = false;
    renderDock();
  });
  box.append(area, apply, remove);
  return box;
}

function renderDock() {
  const dock = $("dock");
  dock.replaceChildren();
  dock.hidden = state.tails.size === 0;
  for (const [ref, t] of state.tails) {
    const n = state.nodes.get(ref);
    const win = document.createElement("section");
    win.className = "tail";
    const bar = document.createElement("header");
    const title = document.createElement("span");
    title.className = "tail-title";
    title.textContent = n ? n.shorthand : ref.slice(0, 8);
    const st = document.createElement("span");
    st.className = `badge ${n ? stateClass(n) : ""}`;
    st.textContent = n ? n.convergence : "gone";
    bar.append(title, " ", st);
    const button = (label, hint, fn) => {
      const b = document.createElement("button");
      b.type = "button";
      b.textContent = label;
      b.title = hint;
      b.addEventListener("click", fn);
      return b;
    };
    const pause = button(t.paused ? "resume" : "pause", "hold the window still; lines keep arriving", () => {
      t.paused = !t.paused;
      if (!t.paused) {
        t.lines.push(...t.held);
        t.held = [];
        if (t.lines.length > TAIL_MAX_LINES) t.lines.splice(0, t.lines.length - TAIL_MAX_LINES);
        paintTail(t);
      }
      pause.textContent = t.paused ? "resume" : "pause";
    });
    bar.append(
      pause,
      button("clear", "empty this window (the node's own ring is untouched)", () => {
        t.lines = [];
        t.held = [];
        paintTail(t);
      }),
      button(t.template ? "template*" : "template", "shape how this window renders its lines with a Tramaj template", () => {
        t.editing = !t.editing;
        renderDock();
      }),
      button("unpin", "close this window", () => toggleTail(ref)),
    );
    const pre = document.createElement("pre");
    pre.className = "tail-body";
    t.pre = pre;
    const err = document.createElement("div");
    err.className = "tail-template-error";
    err.hidden = true;
    t.errorLine = err;
    win.append(bar);
    if (t.editing) win.append(templateEditor(t));
    win.append(err, pre);
    dock.appendChild(win);
    fillTailBody(t);
    pre.scrollTop = pre.scrollHeight;
  }
}

function applyServe(e) {
  switch (e.kind) {
    case "declared":
    case "cleared":
      scheduleReload();
      if (state.history) loadHistory();
      break;
    case "converge-start":
      state.converge = { running: true, down: e.down, up: e.up };
      break;
    case "converge-stop":
      state.converge = { ok: e.ok, remaining: e.remaining };
      scheduleReload();
      break;
    case "tended":
      if (e.report) applyUpkeep({ ...e.report, stream: "upkeep", seq: e.seq });
      break;
    default:
      break;
  }
}

function nodeOf(e) {
  return e.ref && state.nodes.get(e.ref.full);
}

function applyUpDown(e) {
  if (e.ref) pushTailLine(e.ref.full, markedEntry("updown", e.kind, "[updown] ", formatUpDownLine(e), e.seq), e.seq);
  const n = nodeOf(e);
  if (!n) return;
  n.last = e.kind;
  switch (e.kind) {
    case "eval":
      n.lastReason = null;
      startRun(n);
      pulse(n);
      break;
    case "done":
      n.convergence = "converged";
      endRun(n, true);
      pulse(n);
      break;
    case "skip":
      n.convergence = "converged";
      endRun(n, false);
      break;
    case "failed":
      n.convergence = "errored";
      n.lastReason = e.error;
      endRun(n, true);
      pulse(n);
      break;
    case "blocked":
      n.convergence = "blocked";
      endRun(n, false);
      break;
    case "conflicting":
      n.lastReason = `conflicting: kept ${e.kept && e.kept.shorthand}, replaced ${e.replaced && e.replaced.shorthand}`;
      break;
    default:
      break;
  }
  paintNode(n);
}

function applyUpkeep(e) {
  if (e.kind === "acted" && e.report) {
    applyUpDown({ ...e.report, stream: "updown", seq: e.seq });
    return;
  }
  if (e.ref) pushTailLine(e.ref.full, markedEntry("upkeep", e.kind, "[upkeep] ", formatUpkeepLine(e), e.seq), e.seq);
  const n = nodeOf(e);
  if (!n) return;
  n.last = e.kind;
  switch (e.kind) {
    case "next-look":
      if (!n.status) n.status = {};
      n.status.check = e.check;
      n.lastReason = e.check && e.check.reason ? e.check.reason : null;
      break;
    case "upkeep":
    case "downkeep":
      n.machine = e.state;
      break;
    case "demoted":
      n.lastReason = `sent back by ${e.dependency && e.dependency.short}`;
      break;
    case "gave-up":
      n.lastReason = `gave up after ${e.failures} failure(s)`;
      break;
    case "escaped":
      n.lastReason = e.error;
      break;
    case "wedged":
      n.lastReason = `silent for ${Math.round(e.silent_us / 1e6)}s`;
      break;
    case "unwedged":
      n.lastReason = null;
      break;
    default:
      break;
  }
  paintNode(n);
}

function pulse(n) {
  n.pulse = true;
}

// ---------------------------------------------------------------------------
// layout: longest-path layering, then barycentre ordering (Sugiyama-lite)

const BOX_W = 176;
const BOX_H = 66;
const GAP_X = 24;
const GAP_Y = 72;
const PAD = 12;

function layout() {
  const ids = state.order;
  const depsOf = (id) => (state.nodes.get(id).dependencies || []).map((r) => r.full).filter((d) => state.nodes.has(d));
  const dependantsOf = (id) => (state.nodes.get(id).dependants || []).map((r) => r.full).filter((d) => state.nodes.has(d));

  // layer = longest path from a node with no dependencies; a cycle (which
  // the drivers report Blocked) is cut wherever it is first re-entered.
  const layer = new Map();
  const visiting = new Set();
  const layerOf = (id) => {
    if (layer.has(id)) return layer.get(id);
    if (visiting.has(id)) return 0;
    visiting.add(id);
    let l = 0;
    for (const d of depsOf(id)) l = Math.max(l, layerOf(d) + 1);
    visiting.delete(id);
    layer.set(id, l);
    return l;
  };
  ids.forEach(layerOf);

  const layers = [];
  for (const id of ids) {
    const l = layer.get(id);
    (layers[l] ||= []).push(id);
  }

  // order within a layer by the mean position of neighbours in the layers
  // already ordered: down sweeps look at dependencies, up sweeps at
  // dependants; a node with none keeps its place.
  const pos = new Map();
  const place = () => layers.forEach((row) => row.forEach((id, i) => pos.set(id, i)));
  place();
  const bary = (id, neigh) => {
    const ps = neigh(id).map((d) => pos.get(d)).filter((p) => p !== undefined);
    return ps.length ? ps.reduce((a, b) => a + b, 0) / ps.length : pos.get(id);
  };
  const sortRow = (row, neigh) => {
    const keyed = row.map((id) => [bary(id, neigh), pos.get(id), id]);
    keyed.sort((a, b) => a[0] - b[0] || a[1] - b[1]);
    return keyed.map((k) => k[2]);
  };
  for (let sweep = 0; sweep < 4; sweep++) {
    for (let l = 1; l < layers.length; l++) layers[l] = sortRow(layers[l], depsOf);
    place();
    for (let l = layers.length - 2; l >= 0; l--) layers[l] = sortRow(layers[l], dependantsOf);
    place();
  }

  // coordinates: rows are not centred on the widest one. Each node starts
  // packed to the left, then alternating sweeps pull it towards the mean x
  // of its neighbours (dependencies going down, dependants going up), each
  // row being re-packed by isotonic regression (pool-adjacent-violators) so
  // order is kept, boxes never overlap, and the squared distance to the
  // targets is as small as the separation allows. Parents end up above
  // their children and chains run straight; narrow layers hug their
  // neighbours instead of floating in the middle of the widest row.
  const STEP = BOX_W + GAP_X;
  const xs = new Map();
  layers.forEach((row) => row.forEach((id, i) => xs.set(id, i * STEP)));
  const settle = (row, neigh) => {
    // y_i = x_i - i*STEP must be non-decreasing; targets shifted likewise
    const blocks = [];
    row.forEach((id, i) => {
      const ns = neigh(id).map((d) => xs.get(d)).filter((x) => x !== undefined);
      const t = (ns.length ? ns.reduce((p, q) => p + q, 0) / ns.length : xs.get(id)) - i * STEP;
      blocks.push({ sum: t, n: 1 });
      while (blocks.length > 1) {
        const last = blocks[blocks.length - 1];
        const prev = blocks[blocks.length - 2];
        if (prev.sum / prev.n <= last.sum / last.n) break;
        blocks.pop();
        prev.sum += last.sum;
        prev.n += last.n;
      }
    });
    let i = 0;
    for (const bl of blocks) {
      for (let k = 0; k < bl.n; k++, i++) xs.set(row[i], bl.sum / bl.n + i * STEP);
    }
  };
  for (let sweep = 0; sweep < 6; sweep++) {
    for (let l = 1; l < layers.length; l++) settle(layers[l], depsOf);
    for (let l = layers.length - 2; l >= 0; l--) settle(layers[l], dependantsOf);
  }
  let minX = Infinity;
  let maxX = -Infinity;
  for (const x of xs.values()) {
    minX = Math.min(minX, x);
    maxX = Math.max(maxX, x);
  }
  const width = maxX - minX + BOX_W + 2 * PAD;
  const coords = new Map();
  layers.forEach((row, l) => {
    row.forEach((id) => coords.set(id, { x: PAD + xs.get(id) - minX, y: PAD + l * (BOX_H + GAP_Y) }));
  });
  const height = layers.length * BOX_H + (layers.length - 1) * GAP_Y + 2 * PAD;
  return { coords, width, height };
}

// ---------------------------------------------------------------------------
// rendering

function stateClass(n) {
  return n.direction === "down" ? "retiring" : n.convergence;
}

function stateLine(n) {
  const parts = [n.direction, n.convergence];
  if (n.machine) parts.push(n.machine);
  return parts.join(" · ");
}

function lastLine(n) {
  const parts = [];
  if (n.last) parts.push(n.last);
  const check = n.status && n.status.check;
  if (check && check.verdict) parts.push(`check: ${check.verdict}`);
  return parts.join("  ");
}

function render() {
  renderHeader();
  const empty = state.order.length === 0;
  $("empty").hidden = !empty;
  const rows = state.view === "rows";
  $("graph-viewport").style.display = empty || rows ? "none" : "";
  $("rows").hidden = empty || !rows;
  document.body.classList.toggle("view-rows", rows);
  // the hidden view is not drawn: switching views calls render() again
  if (rows) renderRows();
  else renderGraph();
  renderList();
  renderPanel();
}

function renderHeader() {
  $("h-mode").textContent = state.mode;
  $("h-seq").textContent = String(state.seq);
  let converged = 0;
  let errored = 0;
  for (const n of state.nodes.values()) {
    if (n.convergence === "converged") converged++;
    if (n.convergence === "errored" || n.convergence === "blocked") errored++;
  }
  $("h-counts").textContent = `${converged} converged / ${errored} errored / ${state.nodes.size}`;
  const c = state.converge;
  $("h-converge").textContent = !c
    ? "–"
    : c.running
      ? `running (${c.down} down, ${c.up} up)`
      : `${c.ok ? "ok" : "failed"}, ${c.remaining} remaining`;
}

function renderGraph() {
  const edges = $("edges");
  const nodes = $("nodes");
  edges.replaceChildren();
  nodes.replaceChildren();
  if (state.order.length === 0) return;
  const { coords, width, height } = layout();
  state.worldSize = { width, height };

  for (const id of state.order) {
    const n = state.nodes.get(id);
    const to = coords.get(id);
    for (const d of n.dependencies || []) {
      const from = coords.get(d.full);
      if (!from) continue;
      const x1 = from.x + BOX_W / 2;
      const y1 = from.y + BOX_H;
      const x2 = to.x + BOX_W / 2;
      const y2 = to.y;
      const bend = Math.max(20, (y2 - y1) / 2);
      const p = document.createElementNS(SVG, "path");
      p.setAttribute("d", `M ${x1} ${y1} C ${x1} ${y1 + bend}, ${x2} ${y2 - bend}, ${x2} ${y2}`);
      p.dataset.from = d.full;
      p.dataset.to = id;
      edges.appendChild(p);
    }
  }

  for (const id of state.order) {
    const n = state.nodes.get(id);
    const c = coords.get(id);
    const g = document.createElementNS(SVG, "g");
    g.setAttribute("transform", `translate(${c.x} ${c.y})`);
    g.dataset.ref = id;
    const rect = document.createElementNS(SVG, "rect");
    rect.setAttribute("width", BOX_W);
    rect.setAttribute("height", BOX_H);
    g.appendChild(rect);
    g.appendChild(text("ref", 8, 15, `#${n.ref.short}`));
    g.appendChild(text("shorthand", 8, 32, n.shorthand));
    g.appendChild(text("state", 8, 47, ""));
    g.appendChild(text("last", 8, 60, ""));
    const title = document.createElementNS(SVG, "title");
    title.textContent = n.help;
    g.appendChild(title);
    // Selection is handled by the delegated, coordinate-based hit test
    // below (pointer capture retargets click's own bubble path away from
    // this element), not a listener here.
    g.addEventListener("animationend", () => g.classList.remove("pulse"));
    nodes.appendChild(g);
    n.el = g;
    paintNode(n);
  }
  highlightEdges();
  if (state.zoomInit) applyZoom();
  else fitView();
}

// ---------------------------------------------------------------------------
// pan/zoom: a transform on #viewport, world units = layout()'s raw pixels.
// The SVG itself has no viewBox (so 1 user unit = 1 CSS pixel of the
// rendered element), which is what keeps the wheel/drag math below in plain
// screen pixels instead of also tracking a separate content scale.

function applyZoom() {
  const z = state.zoom;
  $("viewport").setAttribute("transform", `translate(${z.tx} ${z.ty}) scale(${z.scale})`);
}

// Fits the whole graph in the viewport, centred. Called once on first load
// and from the "fit" button; a later re-render (a live update) keeps
// whatever the operator has already panned/zoomed to.
function fitView() {
  const vp = $("graph-viewport");
  // a hidden viewport (the rows view, an empty world) has no size to fit:
  // leave the first fit for when it is shown
  if (!state.zoomInit && vp.clientWidth === 0) return;
  const w = vp.clientWidth || 800;
  const h = vp.clientHeight || 500;
  const world = state.worldSize || { width: w, height: h };
  const raw = Math.min((w - 24) / world.width, (h - 24) / world.height) || 1;
  const scale = Math.max(MIN_ZOOM, Math.min(MAX_ZOOM, raw));
  state.zoom = {
    scale,
    tx: (w - world.width * scale) / 2,
    ty: (h - world.height * scale) / 2,
  };
  state.zoomInit = true;
  applyZoom();
}

// Zooms by `factor`, keeping the point at (cx, cy) — viewport-relative
// screen pixels — fixed under the cursor.
function zoomAt(cx, cy, factor) {
  const z = state.zoom;
  const newScale = Math.max(MIN_ZOOM, Math.min(MAX_ZOOM, z.scale * factor));
  const localX = (cx - z.tx) / z.scale;
  const localY = (cy - z.ty) / z.scale;
  z.scale = newScale;
  z.tx = cx - localX * newScale;
  z.ty = cy - localY * newScale;
  applyZoom();
}

function text(cls, x, y, content) {
  const t = document.createElementNS(SVG, "text");
  t.setAttribute("class", cls);
  t.setAttribute("x", x);
  t.setAttribute("y", y);
  t.textContent = clip(content, cls === "shorthand" ? 22 : 26);
  return t;
}

function clip(s, n) {
  s = String(s ?? "");
  return s.length > n ? s.slice(0, n - 1) + "…" : s;
}

// Repaint one box (and its list row) from the node's current fields.
function paintNode(n) {
  const cls = stateClass(n);
  stampState(n, cls);
  if (n.el) {
    const g = n.el;
    g.setAttribute("class", `node ${cls}${n.touched ? " touched" : ""}${n.conflict ? " conflict" : ""}${state.selected === n.ref.full ? " selected" : ""}`);
    g.querySelector(".state").textContent = clip(stateLine(n), 26);
    g.querySelector(".last").textContent = clip(lastLine(n), 30);
    if (n.pulse) {
      n.pulse = false;
      g.classList.remove("pulse");
      void g.getBoundingClientRect();
      g.classList.add("pulse");
    }
  }
  if (n.li) {
    n.li.className = `${cls}${n.touched ? " touched" : ""}${state.selected === n.ref.full ? " selected" : ""}`;
    n.li.querySelector(".state").textContent = stateLine(n) + (lastLine(n) ? ` — ${lastLine(n)}` : "");
  }
  paintRow(n);
  if (state.selected === n.ref.full) renderPanel();
}

function renderList() {
  const list = $("list");
  list.replaceChildren();
  for (const id of state.order) {
    const n = state.nodes.get(id);
    const li = document.createElement("li");
    const ref = document.createElement("div");
    ref.className = "ref mono";
    ref.textContent = `#${n.ref.short}`;
    const sh = document.createElement("div");
    sh.textContent = n.shorthand;
    sh.style.fontWeight = "600";
    const st = document.createElement("div");
    st.className = "state";
    const deps = document.createElement("div");
    deps.className = "deps";
    deps.textContent = (n.dependencies || []).length ? `depends on ${n.dependencies.map((d) => "#" + d.short).join(", ")}` : "no dependencies";
    li.append(ref, sh, st, deps);
    li.addEventListener("click", () => select(id));
    list.appendChild(li);
    n.li = li;
    paintNode(n);
  }
}

function highlightEdges() {
  for (const p of $("edges").querySelectorAll("path")) {
    p.classList.toggle("hi", state.selected !== null && (p.dataset.from === state.selected || p.dataset.to === state.selected));
  }
}

// ---------------------------------------------------------------------------
// the rows view: opt-in, never the default. The same nodes as the graph, one
// row each in a topological order (every node below all its dependencies,
// ties kept in /dag's order), with the dependency edges drawn in a gutter on
// the left the way `git log --graph` draws history: a node's lane runs down
// from its row to its last dependant's, forking into each dependant on the
// way, and a node with several dependencies is where their lanes merge.
//
// The "since" and "took" cells are this page's clock, not the server's:
// /dag and /events carry no timestamps, so a state is stamped when the page
// first sees it (a node already in the first /dag has no "since"), and a
// run is timed from its `eval` to its `done`/`failed` as they arrive. A
// reload of the page loses them; a re-fetch of /dag keeps them. "progress"
// is the last message the node said (a `log` line on the `msg` channel)
// since its last `eval`, seeded from /dag's `tail`.

const ROW_H = 30;
const LANE_W = 14;
const GUTTER_MAX = 280; // past this many pixels of lanes, lanes narrow (down to 4px) instead
const GUTTER_PAD = 6;
const VIEW_STORE = "salmon-view";
const STATE_CLASSES = ["pending", "stale", "converged", "errored", "blocked", "retiring"];

function storedView() {
  try {
    return localStorage.getItem(VIEW_STORE) === "rows" ? "rows" : "graph";
  } catch {
    return "graph";
  }
}

function setView(view, initial) {
  state.view = view === "rows" ? "rows" : "graph";
  const b = $("view-toggle");
  b.setAttribute("aria-pressed", String(state.view === "rows"));
  b.textContent = state.view === "rows" ? "graph" : "rows";
  try {
    if (state.view === "rows") localStorage.setItem(VIEW_STORE, "rows");
    else localStorage.removeItem(VIEW_STORE);
  } catch {
    // a remembered view is a convenience
  }
  clearInterval(state.rowsTimer);
  state.rowsTimer = state.view === "rows" ? setInterval(tickRows, 1000) : null;
  if (state.view !== "rows") for (const n of state.nodes.values()) n.row = null;
  if (!initial) render();
}

// --- the page's clock

function carryClock(n, old) {
  if (old) {
    n.since = old.since;
    n.sinceState = old.sinceState;
    n.startedAt = old.startedAt;
    n.startApprox = old.startApprox;
    n.tookMs = old.tookMs;
    n.tookApprox = old.tookApprox;
    n.progress = old.progress;
  } else {
    // in the first /dag: the state's start is not known; later: just declared
    n.since = state.loadedOnce ? Date.now() : null;
    n.sinceState = stateClass(n);
    // a run the tail says is under way began before this page heard of it
    // (its `eval` came ahead of the /dag that has the node): timed from now,
    // shown as a lower bound
    const t = tailRun(n.tail);
    n.startedAt = t.running ? Date.now() : null;
    n.startApprox = t.running;
    n.tookMs = null;
    n.tookApprox = false;
    n.progress = t.progress;
  }
}

function stampState(n, cls) {
  if (n.sinceState === cls) return;
  n.sinceState = cls;
  n.since = Date.now();
}

function startRun(n) {
  n.startedAt = Date.now();
  n.startApprox = false;
  n.tookMs = null;
  n.progress = null;
}

// `timed`: the run ended by doing something (done, failed), as opposed to
// skipping it or being blocked
function endRun(n, timed) {
  if (timed && n.startedAt) {
    n.tookMs = Date.now() - n.startedAt;
    n.tookApprox = n.startApprox;
  }
  n.startedAt = null;
}

function isProgress(e) {
  return e.stream === "output" && e.kind === "log" && e.channel === "message" && typeof e.line === "string";
}

// From a /dag `tail`: the last message said since the last `eval`, and
// whether that `eval` is still waiting for its outcome.
function tailRun(tail) {
  let progress = null;
  let running = false;
  if (!Array.isArray(tail)) return { progress, running };
  for (const e of tail) {
    const kind = e.stream === "updown" ? e.kind : e.stream === "upkeep" && e.kind === "acted" && e.report ? e.report.kind : null;
    if (kind === "eval") {
      progress = null;
      running = true;
    } else if (kind === "done" || kind === "failed" || kind === "skip" || kind === "blocked") {
      running = false;
    } else if (isProgress(e)) {
      progress = e.line;
    }
  }
  return { progress, running };
}

function duration(ms) {
  const s = Math.max(0, Math.round(ms / 1000));
  if (s < 60) return ms < 10000 ? `${(Math.max(0, ms) / 1000).toFixed(1)}s` : `${s}s`;
  const m = Math.floor(s / 60);
  if (m < 60) return `${m}m${String(s % 60).padStart(2, "0")}s`;
  const h = Math.floor(m / 60);
  return `${h}h${String(m % 60).padStart(2, "0")}m`;
}

function clockText(n, now) {
  if (n.startedAt) return `running ${n.startApprox ? "≥ " : ""}${duration(now - n.startedAt)}`;
  const parts = n.since ? [`${duration(now - n.since)} ago`] : [];
  if (n.tookMs !== null && n.tookMs !== undefined) parts.push(`took ${n.tookApprox ? "≥ " : ""}${duration(n.tookMs)}`);
  return parts.length ? parts.join(" · ") : "–";
}

function clockTitle(n) {
  const at = (t) => new Date(t).toLocaleTimeString();
  if (n.startedAt) return n.startApprox ? `started before ${at(n.startedAt)}, when this page first saw it` : `started at ${at(n.startedAt)}`;
  if (!n.since) return "in this state since before the page loaded";
  return `${n.sinceState} since ${at(n.since)}`;
}

function tickRows() {
  const now = Date.now();
  for (const n of state.nodes.values()) {
    if (!n.row) continue;
    n.row.querySelector(".since").textContent = clockText(n, now);
  }
}

// --- order and gutter (pure: ids and a dependency function in, rows out)

// Kahn's algorithm, ready nodes taken in `ids` order; whatever a cycle keeps
// from ever being ready is appended in `ids` order (the drivers report it
// Blocked).
function topoOrder(ids, depsOf) {
  const index = new Map(ids.map((id, i) => [id, i]));
  const waiting = new Map();
  const dependants = new Map();
  for (const id of ids) {
    const ds = depsOf(id).filter((d) => index.has(d) && d !== id);
    waiting.set(id, ds.length);
    for (const d of ds) (dependants.get(d) || dependants.set(d, []).get(d)).push(id);
  }
  // a binary heap of indices: the earliest ready node in `ids` comes first
  const heap = [];
  const push = (i) => {
    heap.push(i);
    let k = heap.length - 1;
    while (k > 0) {
      const p = (k - 1) >> 1;
      if (heap[p] <= heap[k]) break;
      [heap[p], heap[k]] = [heap[k], heap[p]];
      k = p;
    }
  };
  const pop = () => {
    const top = heap[0];
    const last = heap.pop();
    if (heap.length) {
      heap[0] = last;
      let k = 0;
      for (;;) {
        const l = 2 * k + 1;
        const r = l + 1;
        let m = k;
        if (l < heap.length && heap[l] < heap[m]) m = l;
        if (r < heap.length && heap[r] < heap[m]) m = r;
        if (m === k) break;
        [heap[m], heap[k]] = [heap[k], heap[m]];
        k = m;
      }
    }
    return top;
  };
  ids.forEach((id, i) => {
    if (waiting.get(id) === 0) push(i);
  });
  const out = [];
  const done = new Set();
  while (heap.length) {
    const id = ids[pop()];
    out.push(id);
    done.add(id);
    for (const t of dependants.get(id) || []) {
      const w = waiting.get(t) - 1;
      waiting.set(t, w);
      if (w === 0) push(index.get(t));
    }
  }
  for (const id of ids) if (!done.has(id)) out.push(id);
  return out;
}

// The dependencies a shown row draws: its own, and for one that is filtered
// out, the nearest shown nodes beneath it (`direct: false`, drawn dashed),
// so filtering keeps "this needs that" visible through the hidden rows.
function shownDeps(order, shown, depsOf) {
  const reach = new Map(); // hidden id -> Set of shown ids beneath it
  const visiting = new Set();
  const beneath = (id) => {
    if (reach.has(id)) return reach.get(id);
    const acc = new Set();
    if (visiting.has(id)) return acc;
    visiting.add(id);
    for (const d of depsOf(id)) {
      if (shown.has(d)) acc.add(d);
      else for (const x of beneath(d)) acc.add(x);
    }
    visiting.delete(id);
    reach.set(id, acc);
    return acc;
  };
  const out = new Map();
  for (const id of order) {
    if (!shown.has(id)) continue;
    const ds = new Map();
    for (const d of depsOf(id)) {
      if (shown.has(d)) ds.set(d, true);
      else for (const x of beneath(d)) if (!ds.has(x)) ds.set(x, false);
    }
    ds.delete(id);
    out.set(id, ds);
  }
  return out;
}

// rows: shown ids, top to bottom; deps: id -> Map(dep -> direct). A node's
// lane is held from its row to its last dependant's row; a node takes the
// lane of a dependency that ends on its row when there is one (so a chain
// is one straight line), else the lowest lane free at its row.
function gutterLayout(rows, deps) {
  const rowOf = new Map(rows.map((id, i) => [id, i]));
  const lastRow = new Map();
  rows.forEach((id, r) => {
    for (const d of deps.get(id).keys()) {
      const rd = rowOf.get(d);
      if (rd !== undefined && rd < r) lastRow.set(d, Math.max(lastRow.get(d) ?? rd, r));
    }
  });
  const laneEnd = []; // lane -> last row it is held to
  const laneOf = new Map();
  rows.forEach((id, r) => {
    let lane = -1;
    for (const d of deps.get(id).keys()) {
      const l = laneOf.get(d);
      if (l !== undefined && laneEnd[l] === r && lastRow.get(d) === r) {
        lane = l;
        break;
      }
    }
    if (lane < 0) {
      lane = laneEnd.findIndex((end) => end <= r);
      if (lane < 0) lane = laneEnd.length;
    }
    laneEnd[lane] = Math.max(r, lastRow.get(id) ?? r);
    laneOf.set(id, lane);
  });
  const edges = [];
  rows.forEach((id, r) => {
    for (const [d, direct] of deps.get(id)) {
      const rd = rowOf.get(d);
      if (rd === undefined || rd >= r) continue; // a cycle's back edge
      edges.push({ from: d, to: id, fromRow: rd, toRow: r, fromLane: laneOf.get(d), toLane: laneOf.get(id), direct });
    }
  });
  return { laneOf, lanes: Math.max(1, laneEnd.length), edges };
}

// --- drawing

function rowsDepsOf(id) {
  const n = state.nodes.get(id);
  return n ? (n.dependencies || []).map((r) => r.full).filter((d) => state.nodes.has(d)) : [];
}

// a state change can move a row in or out of the filter: one re-render per frame
function scheduleRows() {
  if (state.rowsFrame !== null) return;
  state.rowsFrame = requestAnimationFrame(() => {
    state.rowsFrame = null;
    if (state.view === "rows") renderRows();
  });
}

function renderFilterOptions() {
  const sel = $("rows-filter");
  const counts = new Map(STATE_CLASSES.map((c) => [c, 0]));
  for (const n of state.nodes.values()) counts.set(stateClass(n), (counts.get(stateClass(n)) || 0) + 1);
  const opts = [["", `all (${state.nodes.size})`], ...STATE_CLASSES.map((c) => [c, `${c} (${counts.get(c) || 0})`])];
  if (sel.options.length !== opts.length) {
    sel.replaceChildren(...opts.map(([v]) => Object.assign(document.createElement("option"), { value: v })));
  }
  opts.forEach(([v, label], i) => {
    sel.options[i].value = v;
    sel.options[i].textContent = label;
  });
  sel.value = state.rowFilter;
}

function renderRows() {
  const list = $("rows-list");
  const gutter = $("rows-gutter");
  list.replaceChildren();
  gutter.replaceChildren();
  for (const n of state.nodes.values()) n.row = null;
  renderFilterOptions();
  const order = topoOrder(state.order, rowsDepsOf);
  const shown = new Set(order.filter((id) => !state.rowFilter || stateClass(state.nodes.get(id)) === state.rowFilter));
  const rows = order.filter((id) => shown.has(id));
  const deps = shownDeps(order, shown, rowsDepsOf);
  const { laneOf, lanes, edges } = gutterLayout(rows, deps);
  $("rows-count").textContent = state.rowFilter ? `${rows.length} of ${order.length} shown` : `${rows.length} nodes`;

  const laneW = Math.max(4, Math.min(LANE_W, GUTTER_MAX / lanes));
  const width = Math.ceil(lanes * laneW + 2 * GUTTER_PAD);
  $("rows").style.setProperty("--gutter", `${width}px`);
  gutter.setAttribute("width", width);
  gutter.setAttribute("height", rows.length * ROW_H);
  const x = (lane) => GUTTER_PAD + lane * laneW + laneW / 2;
  const y = (row) => row * ROW_H + ROW_H / 2;
  for (const e of edges) {
    const x1 = x(e.fromLane);
    const x2 = x(e.toLane);
    const y1 = y(e.fromRow);
    const y2 = y(e.toRow);
    let d;
    if (x1 === x2) {
      d = `M ${x1} ${y1} L ${x2} ${y2}`;
    } else {
      // down the dependency's lane, then bend into the dependant on its row
      const yb = Math.max(y1, y2 - ROW_H);
      d = `M ${x1} ${y1} L ${x1} ${yb} C ${x1} ${y2 - ROW_H / 3}, ${x2} ${y2 - ROW_H / 2}, ${x2} ${y2}`;
    }
    const p = document.createElementNS(SVG, "path");
    p.setAttribute("d", d);
    if (!e.direct) p.setAttribute("class", "indirect");
    p.dataset.from = e.from;
    p.dataset.to = e.to;
    gutter.appendChild(p);
  }
  const now = Date.now();
  rows.forEach((id, r) => {
    const n = state.nodes.get(id);
    const dot = document.createElementNS(SVG, "circle");
    dot.setAttribute("cx", x(laneOf.get(id)));
    dot.setAttribute("cy", y(r));
    dot.setAttribute("r", Math.min(4.5, Math.max(2, laneW / 2 - 0.5)));
    gutter.appendChild(dot);

    const li = document.createElement("li");
    li.tabIndex = 0;
    li.dataset.ref = id;
    const gap = document.createElement("span");
    const st = document.createElement("span");
    st.className = "state";
    const name = document.createElement("span");
    name.className = "name";
    const ref = document.createElement("span");
    ref.className = "ref mono";
    ref.textContent = `#${n.ref.short}`;
    const sh = document.createElement("span");
    sh.className = "shorthand";
    sh.textContent = n.shorthand;
    name.title = n.help || "";
    name.append(ref, " ", sh);
    const since = document.createElement("span");
    since.className = "since mono";
    since.textContent = clockText(n, now);
    const progress = document.createElement("span");
    progress.className = "progress";
    li.append(gap, st, name, since, progress);
    li.addEventListener("click", () => select(id));
    li.addEventListener("keydown", (ev) => {
      if (ev.key === "Enter" || ev.key === " ") {
        ev.preventDefault();
        select(id);
      }
    });
    list.appendChild(li);
    n.row = li;
    n.dot = dot;
    paintRow(n);
  });
  highlightRowEdges();
}

function paintRow(n) {
  if (!n.row) {
    // a node the filter hides that now matches it
    if (state.view === "rows" && state.rowFilter && stateClass(n) === state.rowFilter) scheduleRows();
    return;
  }
  const cls = stateClass(n);
  if (state.rowFilter && cls !== state.rowFilter) {
    scheduleRows();
    return;
  }
  const li = n.row;
  li.className = `row ${cls}${n.touched ? " touched" : ""}${state.selected === n.ref.full ? " selected" : ""}${n.startedAt ? " running" : ""}`;
  const st = li.querySelector(".state");
  st.textContent = n.machine ? `${cls} · ${n.machine}` : cls;
  st.title = stateLine(n) + (lastLine(n) ? ` — ${lastLine(n)}` : "");
  const since = li.querySelector(".since");
  since.textContent = clockText(n, Date.now());
  since.title = clockTitle(n);
  const progress = li.querySelector(".progress");
  const text = n.progress || (n.lastReason ? n.lastReason : "");
  progress.textContent = text;
  progress.title = text;
  progress.classList.toggle("muted", !n.progress);
  if (n.dot) n.dot.setAttribute("class", cls);
}

function highlightRowEdges() {
  for (const p of $("rows-gutter").querySelectorAll("path")) {
    p.classList.toggle("hi", state.selected !== null && (p.dataset.from === state.selected || p.dataset.to === state.selected));
  }
}

// ---------------------------------------------------------------------------
// the side panel

function select(id) {
  const prev = state.selected;
  state.selected = state.selected === id ? null : id;
  if (prev && state.nodes.has(prev)) paintNode(state.nodes.get(prev));
  if (state.selected) paintNode(state.nodes.get(state.selected));
  highlightEdges();
  highlightRowEdges();
  renderPanel();
}

function renderPanel() {
  const panel = $("panel");
  const n = state.selected && state.nodes.get(state.selected);
  if (!n) {
    panel.hidden = true;
    return;
  }
  panel.hidden = false;
  const body = $("panel-body");
  body.replaceChildren();
  const h = document.createElement("h2");
  h.textContent = n.shorthand;
  body.appendChild(h);
  body.appendChild(para("mono", `#${n.ref.short}`));

  const badges = document.createElement("p");
  for (const [cls, label] of [[stateClass(n), n.convergence], ["", `wanted ${n.direction}`], ...(n.machine ? [["", n.machine]] : [])]) {
    const b = document.createElement("span");
    b.className = `badge ${cls}`;
    b.textContent = label;
    badges.appendChild(b);
    badges.appendChild(document.createTextNode(" "));
  }
  body.appendChild(badges);

  // the four mailbox instructions, addressed by ref: the `#`-prefixed
  // selector is a prefix of the short ref, the same text the box prints.
  const actions = document.createElement("p");
  actions.className = "node-actions";
  for (const verb of ["force", "recheck", "pause", "resume"]) {
    const b = document.createElement("button");
    b.type = "button";
    b.className = "write";
    b.textContent = verb;
    b.title = `${verb} --select #${n.ref.short}`;
    b.addEventListener("click", () => post(`${verb} --select ${quote("#" + n.ref.short)}`));
    actions.appendChild(b);
  }
  const tail = document.createElement("button");
  tail.type = "button";
  tail.textContent = state.tails.has(n.ref.full) ? "untail" : "tail";
  tail.title = "pin a live window of this node's output at the bottom of the page";
  tail.addEventListener("click", () => toggleTail(n.ref.full));
  actions.appendChild(tail);
  body.appendChild(actions);

  if (n.conflict) conflictView(body, n.conflict);
  // a card template replaces the descriptive sections, both halves; the
  // default card keeps today's order, with paths and refs in between
  const templated = cardFromTemplate(body, n);
  if (!templated) describeNode(body, n);
  if (n.paths && n.paths.length) list(body, "paths", n.paths, "mono");
  refList(body, "dependencies", n.dependencies);
  refList(body, "dependants", n.dependants);
  if (!templated) nodeActivity(body, n);

  body.appendChild(sectionTitle("full ref"));
  const full = document.createElement("pre");
  full.textContent = n.ref.full;
  body.appendChild(full);
  seedList(body);
}

// The default card's two halves: what the node is, and what it last did.
// A node's card template replaces both.
function describeNode(body, n) {
  section(body, "help", n.help);
  section(body, "notes", n.notes);
  if (n.dynamics && n.dynamics.length) list(body, "dynamics", n.dynamics.map(String));
}

function nodeActivity(body, n) {
  const check = n.status && n.status.check;
  const h3 = document.createElement("h3");
  h3.textContent = "last check";
  body.appendChild(h3);
  if (check) {
    body.appendChild(para("", check.verdict + (check.reason ? `: ${check.reason}` : "")));
  } else {
    body.appendChild(para("", "never tended"));
  }
  if (n.last) section(body, "last event", n.last + (n.lastReason ? `: ${n.lastReason}` : ""));
  if (n.status && n.status.output && n.status.output.length) {
    const h4 = document.createElement("h3");
    h4.textContent = "output";
    body.appendChild(h4);
    const pre = document.createElement("pre");
    pre.className = "output";
    pre.textContent = n.status.output.join("\n");
    body.appendChild(pre);
  }
}

// A node that carries a card template (/dag's `template`, see
// card-template.js) has its card drawn from it, with a switch to the default
// one. Answers false when the default card is to be drawn instead: no
// template, the operator's choice, the template answered null, or the
// evaluator is still loading. A template that does not parse or fails says
// so, and the default card follows.
function cardFromTemplate(body, n) {
  if (typeof n.template !== "string") return false;
  const ref = n.ref.full;
  const showDefault = state.defaultCards.has(ref);
  const bar = document.createElement("p");
  bar.className = "card-switch muted";
  const b = document.createElement("button");
  b.type = "button";
  b.textContent = showDefault ? "templated card" : "default card";
  b.title = "this node carries a card template; switch between it and the default card";
  b.addEventListener("click", () => {
    if (state.defaultCards.has(ref)) state.defaultCards.delete(ref);
    else state.defaultCards.add(ref);
    renderPanel();
  });
  bar.appendChild(b);
  body.appendChild(bar);
  if (showDefault) return false;
  if (!state.templatingLib) {
    if (state.cardLoadError) {
      // said once; a reload of the page tries again
      body.appendChild(para("template-error", `card template: ${state.cardLoadError}; the default card follows`));
      return false;
    }
    bar.appendChild(document.createTextNode(" loading the template…"));
    loadTemplating().then(
      () => {
        if (state.selected === ref) renderPanel();
      },
      (e) => {
        state.cardLoadError = `the template evaluator did not load: ${e && e.message ? e.message : e}`;
        if (state.selected === ref) renderPanel();
      },
    );
    return false;
  }
  const { ct, tt, tramaj } = state.templatingLib;
  let parsed = state.cardPrograms.get(n.template);
  if (!parsed) {
    try {
      parsed = { program: tramaj.parseProgram(n.template) };
    } catch (e) {
      parsed = { error: `does not parse: ${tt.messageOf(e)}` };
    }
    state.cardPrograms.set(n.template, parsed);
  }
  const r = parsed.error ? { error: parsed.error } : ct.foldCard(tramaj, parsed.program, ct.cardContext(n));
  if (r.error) {
    body.appendChild(para("template-error", `card template: ${r.error}; the default card follows`));
    return false;
  }
  if (r.fallback) return false;
  const card = document.createElement("div");
  card.className = "card-template";
  card.appendChild(specElement(r.spec));
  body.appendChild(card);
  return true;
}

function seedList(body) {
  // A node does not know which seed declared it, and /history does not say
  // which nodes an epoch declared, so the choice is the operator's: every
  // live declaration, each with its `down`.
  body.appendChild(sectionTitle("retire a seed"));
  if (!state.history) {
    const a = document.createElement("a");
    a.textContent = "load the history";
    a.addEventListener("click", () => loadHistory().then(renderPanel));
    body.appendChild(para("", "")).appendChild(a);
    return;
  }
  const live = state.history.seeds.filter((h) => h.active);
  if (live.length === 0) {
    body.appendChild(para("", "no live declaration"));
    return;
  }
  body.appendChild(para("muted", "the server does not say which of these declared this node"));
  for (const h of live) {
    const row = document.createElement("p");
    row.className = "seed-row";
    const words = document.createElement("span");
    words.className = "words mono";
    words.textContent = `${h.declaration} ${h.args.map(quote).join(" ")}`;
    const b = document.createElement("button");
    b.type = "button";
    b.textContent = "down";
    b.addEventListener("click", () => post(`down ${h.args.map(quote).join(" ")}`));
    row.append(words, b);
    body.appendChild(row);
  }
}

// Two declarations collided on one ref: kept (the last writer) and replaced,
// each {shorthand, help, notes, dynamics}, side by side, differing rows marked.
function conflictView(body, c) {
  body.appendChild(sectionTitle("conflict: kept vs replaced"));
  const grid = document.createElement("div");
  grid.className = "conflict";
  const show = (v) => (v == null || v === "" ? "–" : Array.isArray(v) ? (v.length ? v.map(String).join("\n") : "–") : String(v));
  for (const t of ["", "kept", "replaced"]) {
    const h = document.createElement("div");
    h.className = "chead";
    h.textContent = t;
    grid.appendChild(h);
  }
  for (const f of ["shorthand", "help", "notes", "dynamics"]) {
    const a = show((c.kept || {})[f]);
    const b = show((c.replaced || {})[f]);
    const l = document.createElement("div");
    l.className = "clabel";
    l.textContent = f;
    grid.appendChild(l);
    for (const v of [a, b]) {
      const pre = document.createElement("pre");
      if (a !== b) pre.className = "differs";
      pre.textContent = v;
      grid.appendChild(pre);
    }
  }
  body.appendChild(grid);
}

function sectionTitle(t) {
  const h = document.createElement("h3");
  h.textContent = t;
  return h;
}

function para(cls, t) {
  const p = document.createElement("p");
  if (cls) p.className = cls;
  p.textContent = t;
  return p;
}

function section(body, title, content) {
  body.appendChild(sectionTitle(title));
  const pre = document.createElement("pre");
  pre.textContent = content == null || content === "" ? "–" : String(content);
  body.appendChild(pre);
}

function list(body, title, items, cls) {
  body.appendChild(sectionTitle(title));
  const ul = document.createElement("ul");
  for (const it of items) {
    const li = document.createElement("li");
    if (cls) li.className = cls;
    li.textContent = it;
    ul.appendChild(li);
  }
  body.appendChild(ul);
}

function refList(body, title, refs) {
  body.appendChild(sectionTitle(title));
  if (!refs || refs.length === 0) {
    body.appendChild(para("", "none"));
    return;
  }
  const ul = document.createElement("ul");
  for (const r of refs) {
    const li = document.createElement("li");
    const a = document.createElement("a");
    const target = state.nodes.get(r.full);
    a.textContent = `#${r.short}${target ? ` ${target.shorthand}` : ""}`;
    a.addEventListener("click", () => select(r.full));
    li.appendChild(a);
    ul.appendChild(li);
  }
  body.appendChild(ul);
}

// ---------------------------------------------------------------------------
// commands: one POST /command?async, then the events carrying its origin

// A word of the input language, quoted for `Serve.tokenize` when it needs it.
function quote(w) {
  w = String(w);
  return /^[A-Za-z0-9_@%+=:,./#-]+$/.test(w) ? w : `'${w.replace(/\\/g, "\\\\").replace(/'/g, "\\'")}'`;
}

async function post(line) {
  line = String(line).trim();
  if (!line) return null;
  if (state.readOnly) {
    toast(`read-only view: not sent — ${line}`, "bad");
    return null;
  }
  let r;
  let body;
  try {
    r = await fetch("command?async", { method: "POST", headers: { "content-type": "text/plain" }, body: line });
    if (r.status === 401) return signedOut();
    body = await r.json();
  } catch (err) {
    toast(`not sent: ${err.message || err}`, "bad");
    return null;
  }
  if (!r.ok) {
    toast(`${r.status}: ${body && body.error ? body.error : "refused"} — ${line}`, "bad");
    return null;
  }
  const req = { seq: body.seq, origin: body.origin, line, reports: [], done: false, li: null };
  state.requests.set(req.origin, req);
  logRequest(req);
  toast(`queued at seq ${body.seq}: ${line}`);
  // its reports may already have arrived: the stream does not wait for the
  // POST's answer to be read
  const early = state.unclaimed.filter((e) => claimant(e) === req.origin);
  state.unclaimed = state.unclaimed.filter((e) => !early.includes(e));
  early.forEach((e) => reportFor(req, e));
  return req;
}

// The request an event belongs to, by the origin it carries; `hung-up`
// names the origin it is about rather than being stamped with it.
function claimant(e) {
  if (e.kind === "hung-up" && e.from) return e.from;
  return e.origin && e.origin.kind === "other" ? e.origin.name : null;
}

function attribute(e) {
  const name = claimant(e);
  if (!name) return;
  const req = state.requests.get(name);
  if (req) {
    reportFor(req, e);
    return;
  }
  state.unclaimed.push(e);
  if (state.unclaimed.length > 500) state.unclaimed.shift();
}

function reportFor(req, e) {
  if (req.done) return;
  req.reports.push(e);
  if (e.kind === "enqueued") return;
  logLine(req, e);
  const n = nodeOf(e);
  if (n) {
    n.touched = req.origin;
    paintNode(n);
  }
  if (e.stream !== "serve") return;
  switch (e.kind) {
    case "hung-up":
      req.done = true;
      req.li.classList.add("done");
      setTimeout(() => untouch(req.origin), TOUCH_LINGER_MS);
      break;
    case "instructed":
      toast(`${e.instruction}: ${e.nodes} node(s)`, "ok");
      break;
    case "declared":
      toast(`declared ${e.direction}: epoch ${e.epoch}, ${e.nodes} node(s), ${e.active_seeds} live seed(s)`, "ok");
      break;
    case "cleared":
      toast(`cleared: ${e.retired} seed(s) retired`, "ok");
      break;
    case "converge-stop":
      toast(`converge: ${e.ok ? "ok" : "failed"}, ${e.remaining} remaining`, e.ok ? "ok" : "bad");
      break;
    case "fetch-requested":
      toast(e.following ? "fetch requested" : "nothing is being followed", e.following ? "ok" : "bad");
      break;
    default:
      if (e.kind.startsWith("bad-")) toast(`${errText(e.error)} — ${req.line}`, "bad");
      break;
  }
}

function untouch(origin) {
  for (const n of state.nodes.values()) {
    if (n.touched === origin) {
      n.touched = null;
      paintNode(n);
    }
  }
}

// ---------------------------------------------------------------------------
// toasts

function toast(text, cls) {
  const el = document.createElement("div");
  el.className = `toast${cls ? ` ${cls}` : ""}`;
  el.textContent = text;
  el.addEventListener("click", () => el.remove());
  $("toasts").appendChild(el);
  setTimeout(() => el.remove(), TOAST_MS);
}

// ---------------------------------------------------------------------------
// the log under the command line: one entry per request, its reports under it

function logRequest(req) {
  const li = document.createElement("li");
  const head = document.createElement("div");
  head.className = "request";
  head.textContent = `${req.seq} ${req.line} `;
  const origin = document.createElement("span");
  origin.className = "origin";
  origin.textContent = req.origin;
  head.appendChild(origin);
  const ul = document.createElement("ul");
  li.append(head, ul);
  req.li = li;
  const list = $("cli-log-list");
  list.appendChild(li);
  $("cli-log-count").textContent = `(${state.requests.size})`;
  list.scrollTop = list.scrollHeight;
}

function logLine(req, e) {
  const li = document.createElement("li");
  li.textContent = summarize(e);
  if (e.error || e.kind === "failed" || e.kind === "blocked") li.className = "bad";
  req.li.querySelector("ul").appendChild(li);
  if ($("cli-log").open) {
    const list = $("cli-log-list");
    list.scrollTop = list.scrollHeight;
  }
}

function summarize(e) {
  const parts = [String(e.seq ?? ""), `${e.stream}/${e.kind}`];
  if (e.ref && e.ref.short) parts.push(`#${e.ref.short}`);
  if (e.node && e.node.shorthand) parts.push(e.node.shorthand);
  switch (e.kind) {
    case "declared":
      parts.push(`epoch ${e.epoch} ${e.direction}, ${e.nodes} node(s), ${e.active_seeds} live`);
      break;
    case "cleared":
      parts.push(`${e.retired} retired`);
      break;
    case "converge-start":
      parts.push(`${e.down} down, ${e.up} up`);
      break;
    case "converge-stop":
      parts.push(`${e.ok ? "ok" : "failed"}, ${e.remaining} remaining`);
      break;
    case "instructed":
      parts.push(e.instruction, e.nodes !== undefined ? `${e.nodes} node(s)` : "");
      break;
    case "supervised":
    case "auto-converged":
      parts.push(e.on ? "on" : "off");
      break;
    case "hung-up":
      parts.push("done");
      break;
    case "help":
      parts.push(`${(e.lines || []).length} line(s)`);
      break;
    case "status":
    case "query":
      parts.push(`${(e.nodes || []).length} node(s)`);
      break;
    case "history":
      parts.push(`${(e.seeds || []).length} seed(s)`);
      break;
    default:
      break;
  }
  if (e.error) parts.push(errText(e.error));
  return parts.filter(Boolean).join(" ");
}

function errText(x) {
  return typeof x === "string" ? x : JSON.stringify(x);
}

// ---------------------------------------------------------------------------
// the seed panel: /help/seed, the form, /history

async function loadSeedHelp() {
  if (state.seedHelpLoaded) return;
  try {
    const r = await fetch("help/seed", { cache: "no-store" });
    if (!r.ok) throw new Error(`/help/seed answered ${r.status}`);
    const h = await r.json();
    $("seed-help").textContent = h.seed || "–";
    $("seed-commands").textContent = (h.commands || []).join("\n");
    state.seedHelpLoaded = true;
  } catch (err) {
    $("seed-help").textContent = `could not load: ${err.message || err}`;
  }
}

async function loadHistory() {
  let h;
  try {
    const r = await fetch("history", { cache: "no-store" });
    if (!r.ok) throw new Error(`/history answered ${r.status}`);
    h = await r.json();
  } catch (err) {
    toast(`history: ${err.message || err}`, "bad");
    return;
  }
  state.history = { seeds: h.seeds || [], elided: h.elided || 0 };
  renderHistory();
  if (state.selected) renderPanel();
}

function originText(o) {
  if (!o) return "–";
  switch (o.kind) {
    case "stdin":
      return "stdin";
    case "other":
      return o.name;
    case "loaded":
      return `loaded ${o.path}`;
    case "fetched":
      return `fetched ${o.label}`;
    default:
      return o.kind;
  }
}

function renderHistory() {
  const list = $("history");
  list.replaceChildren();
  const h = state.history;
  if (!h) return;
  $("history-elided").textContent = "";
  $("history-empty").hidden = h.seeds.length > 0;
  list.hidden = h.seeds.length === 0;
  // a timeline, newest first; the elided count is its tail
  for (const s of [...h.seeds].reverse()) {
    const li = document.createElement("li");
    li.className = `tl-item ${s.active ? "active" : "retired"}`;
    const head = document.createElement("div");
    head.className = "tl-head";
    const ep = document.createElement("span");
    ep.className = "tl-epoch";
    ep.textContent = `#${s.epoch}`;
    const decl = document.createElement("span");
    decl.className = "muted";
    decl.textContent = s.declaration;
    const badge = document.createElement("span");
    badge.className = `badge${s.active ? " active" : ""}`;
    badge.textContent = s.active ? "active" : "retired";
    head.append(ep, decl, badge);
    li.appendChild(head);
    const words = s.args.map(quote).join(" ");
    const w = document.createElement("div");
    w.className = "words";
    w.textContent = words;
    w.title = "put these words in the form";
    w.addEventListener("click", () => {
      $("seed-words").value = words;
      $("seed-words").focus();
    });
    li.appendChild(w);
    const foot = document.createElement("div");
    foot.className = "tl-foot muted";
    const o = document.createElement("span");
    o.textContent = originText(s.origin);
    foot.appendChild(o);
    if (s.active) {
      const b = document.createElement("button");
      b.type = "button";
      b.className = "write";
      b.textContent = "down";
      b.title = `down ${words}`;
      b.addEventListener("click", () => post(`down ${words}`));
      foot.appendChild(b);
    }
    li.appendChild(foot);
    list.appendChild(li);
  }
  if (h.elided) {
    const li = document.createElement("li");
    li.className = "tl-item muted";
    li.textContent = `${h.elided} older entries elided`;
    list.appendChild(li);
  }
}

function toggleSeeds(open) {
  const panel = $("seeds");
  const want = open === undefined ? panel.hidden : open;
  panel.hidden = !want;
  $("seeds-toggle").setAttribute("aria-expanded", String(want));
  if (want) {
    loadSeedHelp();
    loadHistory();
    $("seed-words").focus();
  }
}

function declare(verb) {
  const words = $("seed-words").value.trim();
  if (!words) {
    toast("no seed words", "bad");
    $("seed-words").focus();
    return;
  }
  post(`${verb} ${words}`);
}

// ---------------------------------------------------------------------------
// wiring

$("panel-close").addEventListener("click", () => select(state.selected));
$("reload").addEventListener("click", loadDag);

// wheel to zoom (centred on the cursor), drag to pan; a drag that actually
// moved suppresses the click it ends with, so panning never also selects
// whatever node the pointer happened to end up over.
{
  const svg = $("graph");
  const vp = $("graph-viewport");
  let dragging = false;
  let justPanned = false;
  let start = null;

  svg.addEventListener(
    "wheel",
    (ev) => {
      ev.preventDefault();
      const rect = svg.getBoundingClientRect();
      zoomAt(ev.clientX - rect.left, ev.clientY - rect.top, ev.deltaY < 0 ? 1.15 : 1 / 1.15);
    },
    { passive: false },
  );

  svg.addEventListener("pointerdown", (ev) => {
    if (ev.button !== 0) return;
    dragging = true;
    justPanned = false;
    start = { x: ev.clientX, y: ev.clientY, tx: state.zoom.tx, ty: state.zoom.ty };
    svg.setPointerCapture(ev.pointerId);
    svg.classList.add("panning");
  });
  svg.addEventListener("pointermove", (ev) => {
    if (!dragging) return;
    const dx = ev.clientX - start.x;
    const dy = ev.clientY - start.y;
    if (Math.hypot(dx, dy) > 3) justPanned = true;
    state.zoom.tx = start.tx + dx;
    state.zoom.ty = start.ty + dy;
    applyZoom();
  });
  const endDrag = (ev) => {
    if (!dragging) return;
    dragging = false;
    svg.classList.remove("panning");
    if (ev && ev.pointerId !== undefined) {
      try {
        svg.releasePointerCapture(ev.pointerId);
      } catch {
        // already released
      }
    }
  };
  svg.addEventListener("pointerup", endDrag);
  svg.addEventListener("pointercancel", endDrag);
  // Pointer capture is set on every pointerdown (above), which means the
  // browser retargets the whole click's mouse-compat sequence to `svg`
  // itself rather than whatever node is under the cursor — so a node `g`'s
  // own click listener never sees it. Hit-test from coordinates instead of
  // relying on the (retargeted) event target/bubble path.
  svg.addEventListener(
    "click",
    (ev) => {
      if (justPanned) {
        justPanned = false;
        ev.stopPropagation();
        ev.preventDefault();
        return;
      }
      const hit = document.elementFromPoint(ev.clientX, ev.clientY);
      const nodeEl = hit && hit.closest ? hit.closest("[data-ref]") : null;
      if (nodeEl) select(nodeEl.dataset.ref);
    },
    true,
  );

  $("zoom-in").addEventListener("click", () => zoomAt(vp.clientWidth / 2, vp.clientHeight / 2, 1.3));
  $("zoom-out").addEventListener("click", () => zoomAt(vp.clientWidth / 2, vp.clientHeight / 2, 1 / 1.3));
  $("zoom-reset").addEventListener("click", fitView);
}

$("view-toggle").addEventListener("click", () => setView(state.view === "rows" ? "graph" : "rows"));
$("rows-filter").addEventListener("change", (ev) => {
  state.rowFilter = ev.target.value;
  renderRows();
});

for (const b of document.querySelectorAll("#actions button[data-line]")) {
  b.addEventListener("click", () => {
    if (b.dataset.confirm && !window.confirm(b.dataset.confirm)) return;
    post(b.dataset.line);
  });
}

$("seeds-toggle").addEventListener("click", () => toggleSeeds());
$("seed-form").addEventListener("submit", (ev) => {
  ev.preventDefault();
  declare("up");
});
for (const b of document.querySelectorAll("#seed-form button[data-verb]")) {
  if (b.type === "submit") continue;
  b.addEventListener("click", () => declare(b.dataset.verb));
}

$("cli-form").addEventListener("submit", (ev) => {
  ev.preventDefault();
  const input = $("cli-line");
  const line = input.value.trim();
  if (!line) return;
  input.value = "";
  post(line);
});

// `:` focuses the command line, as in `vi`/`less`; Esc leaves it.
document.addEventListener("keydown", (ev) => {
  const t = ev.target;
  const typing = t && (t.tagName === "INPUT" || t.tagName === "TEXTAREA");
  if (ev.key === ":" && !typing && !ev.ctrlKey && !ev.metaKey && !ev.altKey) {
    ev.preventDefault();
    $("cli-line").focus();
  } else if (ev.key === "Escape" && typing) {
    t.blur();
  }
});

// Over --http-tcp the page is signed in with a session cookie it cannot
// read (HttpOnly), so it asks; on the unix socket the answer is false and
// the button stays hidden.
async function offerSignOut() {
  try {
    const r = await fetch("auth/session", { cache: "no-store" });
    if (r.ok && (await r.json()).session === true) $("signout").hidden = false;
  } catch {
    // no button is the safe way to be wrong
  }
}

// Under serve --http-read-only every POST /command is refused, so the page
// hides whatever would send one (`.write`). Not knowing leaves them shown:
// the server refuses either way, and this is only about not offering it.
async function checkAccess() {
  try {
    const r = await fetch("access", { cache: "no-store" });
    if (r.status === 401) return signedOut();
    if (r.ok && (await r.json()).access === "read-only") {
      state.readOnly = true;
      document.body.classList.add("spectate");
    }
  } catch {
    // shown buttons, refused by the server, is the worst case
  }
  return null;
}

// A 401 means the session ended — signed out in another tab, expired, or
// the server restarted — and the sign-in page is the only thing that fixes it.
function signedOut() {
  closeStream();
  location.assign("auth?ended");
  return null;
}

offerSignOut();
checkAccess();
setView(storedView(), true);
loadDag();
