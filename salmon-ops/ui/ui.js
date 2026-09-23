// The web UI: `/dag` drawn as a layered graph, kept live from `/events`.
//
// Holds no state the server does not. The picture is the last `/dag`
// snapshot; what moves between snapshots is applied from the event stream
// subscribed at that snapshot's `seq`, and anything that changes the node
// set or settles convergence (`declared`, `cleared`, `converge-stop`, a
// `gap`, a lost stream) is answered by fetching `/dag` again and
// resubscribing from its `seq`. Reload is the same thing by hand.
//
// Reads only: nothing here POSTs. Actions and the seed form are the next
// step (specs/generic-server.md, milestone 7).

const $ = (id) => document.getElementById(id);
const SVG = "http://www.w3.org/2000/svg";

// ---------------------------------------------------------------------------
// state

const state = {
  nodes: new Map(), // full ref -> node object from /dag, plus live overlay
  order: [], // full refs in dagOrder
  seq: 0,
  mode: "–",
  converge: null, // last converge-stop {ok, remaining}
  selected: null, // full ref
  source: null, // the EventSource
  reloadTimer: null,
  retryDelay: 1000,
};

// ---------------------------------------------------------------------------
// fetching and subscribing

async function loadDag() {
  clearTimeout(state.reloadTimer);
  state.reloadTimer = null;
  closeStream();
  let dag;
  try {
    const r = await fetch("dag", { cache: "no-store" });
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
  state.nodes = new Map();
  state.order = [];
  for (const n of dag.nodes) {
    n.last = null;
    n.lastReason = null;
    n.machine = null;
    state.nodes.set(n.ref.full, n);
    state.order.push(n.ref.full);
  }
  if (state.selected && !state.nodes.has(state.selected)) state.selected = null;
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
    default:
      return;
  }
}

function applyServe(e) {
  switch (e.kind) {
    case "declared":
    case "cleared":
      scheduleReload();
      break;
    case "converge-start":
      state.converge = { running: true, down: e.down, up: e.up };
      break;
    case "converge-stop":
      state.converge = { ok: e.ok, remaining: e.remaining };
      scheduleReload();
      break;
    case "tended":
      if (e.report) applyUpkeep({ ...e.report, stream: "upkeep" });
      break;
    default:
      break;
  }
}

function nodeOf(e) {
  return e.ref && state.nodes.get(e.ref.full);
}

function applyUpDown(e) {
  const n = nodeOf(e);
  if (!n) return;
  n.last = e.kind;
  switch (e.kind) {
    case "eval":
      n.lastReason = null;
      pulse(n);
      break;
    case "done":
      n.convergence = "converged";
      pulse(n);
      break;
    case "skip":
      n.convergence = "converged";
      break;
    case "failed":
      n.convergence = "errored";
      n.lastReason = e.error;
      pulse(n);
      break;
    case "blocked":
      n.convergence = "blocked";
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
    applyUpDown({ ...e.report, stream: "updown" });
    return;
  }
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
const GAP_X = 28;
const GAP_Y = 64;
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

  // coordinates: each layer a row, centred on the widest one
  const widest = Math.max(...layers.map((r) => r.length));
  const width = widest * BOX_W + (widest - 1) * GAP_X + 2 * PAD;
  const coords = new Map();
  layers.forEach((row, l) => {
    const rowWidth = row.length * BOX_W + (row.length - 1) * GAP_X;
    const x0 = PAD + (width - 2 * PAD - rowWidth) / 2;
    row.forEach((id, i) => {
      coords.set(id, { x: x0 + i * (BOX_W + GAP_X), y: PAD + l * (BOX_H + GAP_Y) });
    });
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
  $("graph").style.display = empty ? "none" : "";
  renderGraph();
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
  const svg = $("graph");
  const edges = $("edges");
  const nodes = $("nodes");
  edges.replaceChildren();
  nodes.replaceChildren();
  if (state.order.length === 0) return;
  const { coords, width, height } = layout();
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
  svg.setAttribute("width", width);
  svg.setAttribute("height", height);

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
    g.addEventListener("click", () => select(id));
    g.addEventListener("animationend", () => g.classList.remove("pulse"));
    nodes.appendChild(g);
    n.el = g;
    paintNode(n);
  }
  highlightEdges();
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
  if (n.el) {
    const g = n.el;
    g.setAttribute("class", `node ${cls}${state.selected === n.ref.full ? " selected" : ""}`);
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
    n.li.className = `${cls}${state.selected === n.ref.full ? " selected" : ""}`;
    n.li.querySelector(".state").textContent = stateLine(n) + (lastLine(n) ? ` — ${lastLine(n)}` : "");
  }
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
// the side panel

function select(id) {
  const prev = state.selected;
  state.selected = state.selected === id ? null : id;
  if (prev && state.nodes.has(prev)) paintNode(state.nodes.get(prev));
  if (state.selected) paintNode(state.nodes.get(state.selected));
  highlightEdges();
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

  section(body, "help", n.help);
  section(body, "notes", n.notes);
  if (n.dynamics && n.dynamics.length) list(body, "dynamics", n.dynamics.map(String));
  if (n.paths && n.paths.length) list(body, "paths", n.paths, "mono");
  refList(body, "dependencies", n.dependencies);
  refList(body, "dependants", n.dependants);

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
  body.appendChild(sectionTitle("full ref"));
  const full = document.createElement("pre");
  full.textContent = n.ref.full;
  body.appendChild(full);
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

$("panel-close").addEventListener("click", () => select(state.selected));
$("reload").addEventListener("click", loadDag);
loadDag();
