// Card templates: the pure half of drawing a node's card in the panel from
// a Tramaj template the node itself carries (feature 458c16c8). Opt-in: a
// node declares one with `withCardTemplate` (Salmon.Builtin.Extension), and
// `/dag` then carries it as the node's `template`; a node without one has no
// `template` key and its card is the default one. `ui.js` imports this
// module, with `tail-template.js` and the vendored tramaj-js, only once a
// selected node has a template (or a tail window does).
//
// No DOM here, and the evaluator is passed in, so the contract can be checked
// under node (`salmon-ops/ui-test/card-template.test.mjs`).
//
// The contract a card template is written against, and which must stay
// stable:
//
// - It is a Tramaj program evaluated in concrete mode, with no libraries,
//   against `$ctx` = `cardContext(node)`, every time the card is drawn:
//     ref           {full, short}
//     shorthand     the node's shorthand
//     help          its help text ("" when none)
//     notes         its notes (a list of strings)
//     dynamics      its rendered dynamics (a list of strings)
//     direction     "up" or "down": what the loop wants of it
//     convergence   "pending", "converged", "stale", ...
//     machine       the machine state the page last heard of, or null
//     paths         up to 8 declared paths to it, shortest first
//     dependencies  the short refs it depends on
//     dependants    the short refs that depend on it
//     check         {verdict, reason} of its last check; both null when it
//                   was never checked (an object either way, since Tramaj's
//                   `branch` evaluates both arms: `$ctx.check.verdict` is
//                   always a valid path)
//     last          the last event the page saw about it, or null
//     lastReason    that event's reason, or null
//     output        its tending machine's last output lines (a list)
//     conflict      true while two live declarations disagree about it
//   Every key is always present (null or empty when unknown): Tramaj refuses
//   a missing path. All of it is what `/dag` already serves to the page,
//   so a template sees nothing that is not already public; the template's
//   own source is public too (it is served on `/dag`).
// - Its result is the card: `null` means "draw the default card" (so a
//   template can opt out per state); a string or other scalar is text; a
//   document is drawn as elements.
// - Only the tags in CARD_TAGS are drawn (the tail's phrasing tags plus a
//   few block ones, and `a`), and of attributes only `class` and `title`,
//   plus `href` on an `a`. A tag outside the list is an error; another
//   attribute, and every action, is ignored. Nothing is ever parsed as HTML.
// - An `a` is a link out of the page (a cloud console page, say). Its `href`
//   is kept only when it is an absolute `https:` URL with no user name or
//   password in it (`safeHref`); anything else (`javascript:`, `data:`, a
//   relative path, plain `http:`) leaves the `a` without an href, drawn as
//   its text. `ui.js` opens a kept link in a new tab, with
//   `rel="noopener noreferrer"` and no referrer. At most TEMPLATE_MAX_NODES
//   nodes and TEMPLATE_MAX_DEPTH deep, as in a tail.
// - A template that does not parse, or a card whose evaluation fails, is
//   drawn as the default card with the error above it.
// - The card replaces the descriptive sections of the panel (help, notes,
//   dynamics, last check, last event, output). The title, ref, state badges,
//   the command buttons, paths, dependencies, dependants, the full ref and
//   the seed list stay as they are: they are how the page is driven.

import { TEMPLATE_ATTRS, TEMPLATE_TAGS, foldLine } from "./tail-template.js";

export const CARD_TAGS = new Set([
  ...TEMPLATE_TAGS,
  "div",
  "p",
  "pre",
  "ul",
  "ol",
  "li",
  "dl",
  "dt",
  "dd",
  "table",
  "thead",
  "tbody",
  "tr",
  "th",
  "td",
  "h4",
  "h5",
  "hr",
  "details",
  "summary",
  "a",
]);

// An `a`'s href as drawn, or null: only an absolute https URL without
// credentials, normalized by the URL parser.
export function safeHref(v) {
  if (typeof v !== "string") return null;
  let u;
  try {
    u = new URL(v);
  } catch {
    return null;
  }
  if (u.protocol !== "https:" || u.username !== "" || u.password !== "" || u.hostname === "") return null;
  return u.href;
}

export const CARD_ALLOW = { tags: CARD_TAGS, attrs: TEMPLATE_ATTRS, href: safeHref, where: "a card" };

const strings = (v) => (Array.isArray(v) ? v.map((x) => String(x)) : []);
const orNull = (v) => (v === undefined ? null : v);
const shortOf = (r) => (r && typeof r.short === "string" ? r.short : null);

// The `$ctx` of a node as `ui.js` keeps it: the /dag object plus the live
// overlay (`machine`, `last`, `lastReason`).
export function cardContext(n) {
  const check = n.status && n.status.check;
  return {
    ref: { full: orNull(n.ref && n.ref.full), short: orNull(n.ref && n.ref.short) },
    shorthand: orNull(n.shorthand),
    help: typeof n.help === "string" ? n.help : "",
    notes: strings(n.notes),
    dynamics: strings(n.dynamics),
    direction: orNull(n.direction),
    convergence: orNull(n.convergence),
    machine: orNull(n.machine),
    paths: strings(n.paths),
    dependencies: (Array.isArray(n.dependencies) ? n.dependencies : []).map(shortOf).filter((s) => s !== null),
    dependants: (Array.isArray(n.dependants) ? n.dependants : []).map(shortOf).filter((s) => s !== null),
    check: { verdict: orNull(check && check.verdict), reason: orNull(check && check.reason) },
    last: orNull(n.last),
    lastReason: orNull(n.lastReason),
    output: strings(n.status && n.status.output),
    conflict: Boolean(n.conflict),
  };
}

// A node's card through a parsed template: {fallback: true} (null: draw the
// default card), {spec}, or {error}.
export function foldCard(tramaj, program, ctx) {
  const r = foldLine(tramaj, program, ctx, CARD_ALLOW);
  return r.drop ? { fallback: true } : r;
}
