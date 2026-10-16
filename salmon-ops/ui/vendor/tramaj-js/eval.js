/**
 * `Value`, the environment, and the evaluator — concrete and symbolic modes
 * (`specs/reference.md` §6, §7, §10; `specs/v3-symbols.md` §4-6;
 * `specs/v4-types.md` §7-8). Mirrors `tramaj-rs/src/eval.rs`.
 */
import { arithmeticNames, symbolSites } from "./analysis.js";
import { adaptKey, isHiddenName, unlets, } from "./ast.js";
import { classifyNumber, compactJson, compareStrings, displayString, float, getField, hasField, inIntegerRange, isJsonObject, jsonEqual, jsonObjectFrom, JsonNumberError, normalizeNumber, ownKeys, } from "./json.js";
import { mapActions, noAnnotations, nodeToJson, } from "./node.js";
import { canonicalId, deepTypeConstraints, eraseTypes, programTypeRoots, typeClosure, TypeError as TramajTypeError, } from "./types.js";
export const defaultOptions = { mode: "concrete", arithmetic: false };
/**
 * Error kinds from `specs/reference.md` §12, plus v3's symbol kinds and the
 * one v4 static failure. `message` always leads with the bare kind, which is
 * what the corpus harness matches on.
 */
export class EvalError extends Error {
    name = "EvalError";
    kind;
    detail;
    cause;
    constructor(kind, detail, cause) {
        super(detail === "" ? kind : `${kind} ${detail}`);
        this.kind = kind;
        this.detail = detail;
        this.cause = cause;
    }
    static unboundName(name) {
        return new EvalError("UnboundName", name);
    }
    static pathNotFound(path) {
        return new EvalError("PathNotFound", JSON.stringify(path));
    }
    static typeMismatch(m) {
        return new EvalError("TypeMismatch", m);
    }
    static concatMismatch(m) {
        return new EvalError("ConcatMismatch", m);
    }
    static unknownLibrary(name) {
        return new EvalError("UnknownLibrary", name);
    }
    static importCycle(name) {
        return new EvalError("ImportCycle", name);
    }
    static inLibrary(name, e) {
        return new EvalError("InLibrary", `${name} (${e.message})`, e);
    }
    static symbolsUnavailable(m) {
        return new EvalError("SymbolsUnavailable", m);
    }
    static notConcrete(who) {
        return new EvalError("NotConcrete", who);
    }
    static allocationInLibrary(name) {
        return new EvalError("AllocationInLibrary", name);
    }
    static notRepresentable(m) {
        return new EvalError("NotRepresentable", m);
    }
    static typeErr(e) {
        return new EvalError("TypeErr", e.message);
    }
}
function enterLibrary(name, ctx) {
    const inProgress = new Set(ctx.inProgress);
    inProgress.add(name);
    return { ...ctx, inProgress, isRoot: false };
}
/** Evaluates in the given mode, without the arithmetic profile. */
export function evalProgram(mode, libs, ctx, program) {
    return evalProgramWith({ mode }, libs, ctx, program);
}
/** As `evalProgram`, with the mode and the arithmetic profile chosen by `options`. */
export function evalProgramWith(options, libs, ctx, program) {
    return evalProgramWithEmissions({ ...defaultOptions, ...options }, libs, ctx, program).output;
}
function evalProgramWithEmissions(options, libs, ctx, program) {
    const { value: v, emissions } = evalProgramRaw(options, libs, ctx, program);
    const output = v.t === "node" ? { t: "node", node: v.node } : { t: "value", value: toJson(v) };
    return { output, emissions: dedupe(emissions) };
}
/** The value of a program and everything it emitted, in evaluation order and before `dedupe`. */
function evalProgramRaw(options, libs, ctx, program) {
    const { mode, arithmetic } = options;
    const erased = withTypeErrors(() => eraseTypes(libs, program));
    const ctxVal = checkedFromJson(options, ctx);
    const emissions = { constraints: [], symbols: [] };
    const evalCtx = {
        libs,
        inProgress: new Set(),
        mode,
        isRoot: true,
        arithmetic,
        emissions,
    };
    const env = initialEnv(arithmetic, ctxVal);
    return { value: evalExpr(evalCtx, env, erased.root), emissions };
}
/**
 * How many constraints an evaluation emitted, counted before equal ones are
 * made one. No program and no output can tell this number: it is here for
 * tests, as the one way to count how many times a function was applied, which
 * `reference.md` §11 fixes for the key function of a sort.
 */
export function emittedConstraintCount(options, libs, ctx, program) {
    return evalProgramRaw({ ...defaultOptions, ...options }, libs, ctx, program).emissions.constraints.length;
}
function withTypeErrors(f) {
    try {
        return f();
    }
    catch (e) {
        if (e instanceof TramajTypeError)
            throw EvalError.typeErr(e);
        throw e;
    }
}
/**
 * Two constraints with the same name and equal arguments are one constraint,
 * and two symbol-table entries with the same id are one entry — each kept at
 * the position of the first (`v3-symbols.md` §4).
 */
function dedupe(e) {
    const constraints = [];
    for (const c of e.constraints) {
        if (!constraints.some((x) => constraintEq(x, c)))
            constraints.push(c);
    }
    const symbols = [];
    for (const s of e.symbols) {
        if (!symbols.some((x) => x.id === s.id))
            symbols.push(s);
    }
    return { constraints, symbols };
}
function constraintEq(a, b) {
    if (a.t !== "constraint" || b.t !== "constraint")
        return false;
    if (a.name !== b.name || a.args.length !== b.args.length)
        return false;
    return a.args.every((x, i) => {
        try {
            return jsonEqual(toJson(x), toJson(b.args[i]));
        }
        catch {
            return false;
        }
    });
}
/**
 * Evaluates and serializes to the wire JSON a host compares against
 * `expected.json`, in the given mode and without the arithmetic profile. The
 * result is in canonical form (`json.ts`): write it with `stringify`, since
 * `JSON.stringify` writes the float `3.0` as `3`.
 */
export function runProgram(mode, libs, ctx, program) {
    return runProgramWith({ mode }, libs, ctx, program);
}
/** As `runProgram`, with the mode and the arithmetic profile chosen by `options`. */
export function runProgramWith(options, libs, ctx, program) {
    const full = { ...defaultOptions, ...options };
    const { mode } = full;
    const { output, emissions } = evalProgramWithEmissions(full, libs, ctx, program);
    if (mode === "concrete") {
        return output.t === "node" ? nodeToJson(output.node) : output.value;
    }
    const kind = output.t === "node" ? "document" : "expression";
    const root = output.t === "node" ? nodeToJson(output.node) : output.value;
    const { types, typeConstraints } = withTypeErrors(() => buildTypesInfo(libs, program));
    const sortedTypes = [...types.entries()].sort((a, b) => compareStrings(a[0], b[0]));
    return {
        format: "tramaj/symbolic/1",
        kind,
        root,
        symbols: emissions.symbols.map(symbolEntryToJson),
        constraints: emissions.constraints.map(constraintToJson),
        types: sortedTypes.map(([id, rt]) => ({ id, definition: resolvedTypeToJson(rt) })),
        "type-constraints": typeConstraints.map(typeConstraintToJson),
    };
}
function buildTypesInfo(libs, prog) {
    const roots = programTypeRoots(libs, prog);
    return {
        types: typeClosure(libs, prog, roots),
        typeConstraints: deepTypeConstraints(libs, prog),
    };
}
/** A `ResolvedType`'s `"definition"` shape (`v4-types.md` §8). */
function resolvedTypeToJson(rt) {
    switch (rt.t) {
        case "Prim":
            return { kind: "prim", name: rt.name };
        case "Array":
            return { kind: "array", element: resolvedTypeToJson(rt.element) };
        case "Record":
            return {
                kind: "record",
                fields: rt.fields.map(([name, t]) => ({ name, type: resolvedTypeToJson(t) })),
            };
        case "Union":
            return {
                kind: "union",
                arms: rt.arms.map(([name, payload]) => payload === null ? { name } : { name, payload: resolvedTypeToJson(payload) }),
            };
        // A `Ref` renders as a pointer only — its definition is a separate entry
        // in the table, which is §3's "stop at declaration boundaries" clause.
        case "Ref":
            return { kind: "ref", id: canonicalId(rt) };
        case "Var":
            return { kind: "var", path: [...rt.path] };
    }
}
function typeConstraintToJson([name, args]) {
    return {
        name,
        arguments: args.map((a) => {
            switch (a.t) {
                case "Type":
                    return { $type: canonicalId(a.type) };
                case "ScalarStr":
                    return a.value;
                case "ScalarInt":
                    return a.value;
                case "ScalarFloat":
                    return float(a.value);
                case "ScalarBool":
                    return a.value;
                case "ScalarNull":
                    return null;
            }
        }),
    };
}
function constraintToJson(v) {
    if (v.t !== "constraint")
        return tryToJson(v);
    return { name: v.name, arguments: v.args.map(tryToJson) };
}
function tryToJson(v) {
    try {
        return toJson(v);
    }
    catch {
        return null;
    }
}
function symbolEntryToJson(e) {
    const origin = e.origin.t === "alloc"
        ? { kind: "alloc", site: e.origin.site, key: e.origin.key }
        : { kind: "demand", path: [...e.origin.path] };
    return { id: e.id, origin, binding: e.binding };
}
const BUILTIN_NAMES = [
    "cardinality",
    "count",
    "str",
    "not",
    "and",
    "or",
    "eq",
    "lt",
    "lte",
    "gt",
    "gte",
    "has",
    "lookup",
    "format-number",
    "concat",
    "append",
];
/**
 * `$ctx` and the builtins. The arithmetic names are bound only with the
 * arithmetic profile on (`reference.md` §11); without it they are unbound,
 * like any other name nobody bound.
 */
function initialEnv(arithmetic, ctxVal) {
    const env = new Map();
    for (const n of BUILTIN_NAMES)
        env.set(n, { t: "builtin", name: n });
    if (arithmetic)
        for (const n of arithmeticNames)
            env.set(n, { t: "builtin", name: n });
    env.set("ctx", ctxVal);
    return env;
}
// Core evaluation --------------------------------------------------------
function evalExpr(ctx, env, e) {
    switch (e.t) {
        case "Path": {
            const v = env.get(e.root);
            if (v === undefined)
                throw EvalError.unboundName(e.root);
            return walkFields(ctx, [e.root, ...e.fields], v, e.fields);
        }
        case "FieldAccess":
            return walkFields(ctx, e.fields, evalExpr(ctx, env, e.target), e.fields);
        case "Call": {
            const fnVal = evalExpr(ctx, env, e.fn);
            const argVals = e.args.map((a) => evalExpr(ctx, env, a));
            return apply(ctx, describeCallee(e.fn), fnVal, argVals);
        }
        case "Lambda":
            return { t: "closure", params: e.params, body: e.body, env: new Map(env) };
        // `@name=?(k)` or `@name=?ctx.path` binds the allocation/demand directly to
        // a name, which is what the symbol table's `"binding"` field reports.
        case "Let": {
            const v = evalBindable(ctx, env, isHiddenName(e.name) ? null : e.name, e.value);
            const env2 = new Map(env);
            env2.set(e.name, v);
            return evalExpr(ctx, env2, e.body);
        }
        case "StringLit":
            return { t: "str", value: e.value };
        case "IntLit":
        case "FloatLit":
            return { t: e.t === "IntLit" ? "int" : "float", value: e.value };
        case "BoolLit":
            return { t: "bool", value: e.value };
        case "NullLit":
            return { t: "null" };
        case "ArrayLit":
            return { t: "array", items: e.elements.map((x) => evalExpr(ctx, env, x)) };
        case "ObjectLit": {
            const fields = new Map();
            for (const [k, sub] of e.fields)
                fields.set(k, evalExpr(ctx, env, sub));
            return { t: "object", fields };
        }
        case "Element": {
            const attributes = e.attributes.map((a) => evalAttribute(ctx, env, a));
            const value = toJson(evalExpr(ctx, env, e.value));
            const children = evalChildren(ctx, env, e.children);
            return {
                t: "node",
                node: {
                    t: "element",
                    tag: e.tag,
                    attributes,
                    value,
                    children,
                    annotations: noAnnotations(),
                },
            };
        }
        case "Fragment":
            return {
                t: "node",
                node: { t: "fragment", children: evalChildren(ctx, env, e.children), annotations: noAnnotations() },
            };
        case "Branch": {
            const cond = requireBool("a branch condition", evalExpr(ctx, env, e.condition));
            return evalExpr(ctx, env, cond ? e.then : e.else);
        }
        case "Map": {
            const items = evalCollection(ctx, env, "map", e.collection);
            const fnVal = evalExpr(ctx, env, e.fn);
            return { t: "array", items: items.map((item) => apply(ctx, "map", fnVal, [item])) };
        }
        case "Filter": {
            const items = evalCollection(ctx, env, "filter", e.collection);
            const fnVal = evalExpr(ctx, env, e.fn);
            const kept = [];
            for (const item of items) {
                if (requireBool("a filter predicate", apply(ctx, "filter", fnVal, [item])))
                    kept.push(item);
            }
            return { t: "array", items: kept };
        }
        case "Scan": {
            const items = evalCollection(ctx, env, "scan", e.collection);
            let acc = evalExpr(ctx, env, e.initial);
            const fnVal = evalExpr(ctx, env, e.fn);
            const out = [acc];
            for (const item of items) {
                acc = apply(ctx, "scan", fnVal, [acc, item]);
                out.push(acc);
            }
            return { t: "array", items: out };
        }
        case "Fold": {
            const items = evalCollection(ctx, env, "fold", e.collection);
            let acc = evalExpr(ctx, env, e.initial);
            const fnVal = evalExpr(ctx, env, e.fn);
            for (const item of items)
                acc = apply(ctx, "fold", fnVal, [acc, item]);
            return acc;
        }
        case "SortBy": {
            const who = e.descending ? "sort-by-descending" : "sort-by";
            const items = evalCollection(ctx, env, who, e.collection);
            const fnVal = evalExpr(ctx, env, e.fn);
            return { t: "array", items: sortByKey(ctx, who, e.descending, fnVal, items) };
        }
        case "Concat":
            return concatValues(evalExpr(ctx, env, e.left), evalExpr(ctx, env, e.right));
        case "Import": {
            const params = new Map();
            for (const [k, p] of e.params) {
                if (p.t === "PExpr") {
                    params.set(k, evalExpr(ctx, env, p.expr));
                }
                else if (p.t === "PFromContext") {
                    const ctxVal = env.get("ctx");
                    if (ctxVal === undefined)
                        throw EvalError.unboundName("ctx");
                    params.set(k, walkFields(ctx, ["ctx", ...p.path], ctxVal, p.path));
                }
                // A `%`-marked entry (`v4-types.md` §2) supplies a type, not a value,
                // so it never reaches the library's own `$ctx`.
            }
            return { t: "import", pending: { name: e.name, params, queued: [] } };
        }
        case "AdaptActions": {
            const target = evalExpr(ctx, env, e.target);
            const fnVal = e.fn === null ? null : evalExpr(ctx, env, e.fn);
            return adaptValue(ctx, e.adaptation, fnVal, target);
        }
        // Each argument must be able to cross a JSON boundary, checked here rather
        // than deferred: a constraint carrying a closure would otherwise sit
        // unnoticed until whatever `!` eventually reaches it.
        case "Constrain": {
            const args = e.args.map((a) => evalExpr(ctx, env, a));
            for (const a of args)
                toJson(a);
            return { t: "constraint", name: e.name, args };
        }
        case "Emit": {
            const cv = evalExpr(ctx, env, e.constraint);
            ctx.emissions.constraints.push(...collectConstraints(cv));
            return evalExpr(ctx, env, e.body);
        }
        case "Alloc":
        case "Demand":
            return evalBindable(ctx, env, null, e);
        // A type declaration means nothing to evaluation (`v4-types.md` §1.1).
        case "TypeDecl":
            return evalExpr(ctx, env, e.body);
        // Erasure rewrites every `TypeAnnotate` before evaluation, so this case is
        // not the normal path; kept total, matching what erasure would produce
        // minus the emission.
        case "TypeAnnotate": {
            const v = evalBindable(ctx, env, e.name, e.value);
            const env2 = new Map(env);
            env2.set(e.name, v);
            return evalExpr(ctx, env2, e.body);
        }
        case "TypeEmit":
            return evalExpr(ctx, env, e.body);
    }
}
/**
 * `Alloc` and `Demand` are the two forms whose symbol-table entry records the
 * name they were bound to, or `null` when used inline (`v3-symbols.md` §5.2).
 */
function evalBindable(ctx, env, binding, e) {
    if (e.t === "Alloc")
        return evalAlloc(ctx, env, binding, e.site, e.key);
    if (e.t === "Demand")
        return evalDemand(ctx, env, binding, e.path);
    return evalExpr(ctx, env, e);
}
/** `?(key)` (`v3-symbols.md` §1.2, §4): the key evaluates in both modes; only minting differs. */
function evalAlloc(ctx, env, binding, site, keyExpr) {
    const keyJson = requireConcrete("?(...)", evalExpr(ctx, env, keyExpr));
    if (ctx.mode === "concrete") {
        throw EvalError.symbolsUnavailable("?(...) would have to allocate a symbol, which concrete mode cannot represent");
    }
    const id = `#${site}:${compactJson(keyJson)}`;
    ctx.emissions.symbols.push({ id, origin: { t: "alloc", site, key: keyJson }, binding });
    return { t: "symbol", id, path: [] };
}
/**
 * `?ctx.a.b` (`v3-symbols.md` §1.3): reads exactly as `$ctx.a.b` would when
 * supplied. Unsupplied, it allocates at the root (mode permitting) and is an
 * ordinary unsupplied read anywhere else.
 */
function evalDemand(ctx, env, binding, path) {
    const snapshot = {
        constraints: ctx.emissions.constraints.length,
        symbols: ctx.emissions.symbols.length,
    };
    try {
        return evalExpr(ctx, env, { t: "Path", root: "ctx", fields: path });
    }
    catch (err) {
        if (!(err instanceof EvalError))
            throw err;
        // Anything the failed sub-evaluation emitted along the way is discarded
        // with it.
        ctx.emissions.constraints.length = snapshot.constraints;
        ctx.emissions.symbols.length = snapshot.symbols;
        if (err.kind !== "PathNotFound" || !ctx.isRoot)
            throw err;
        if (ctx.mode === "concrete") {
            throw EvalError.symbolsUnavailable("?ctx....  unsupplied at the root would have to allocate a symbol, which concrete mode cannot represent");
        }
        const id = `#ctx${path.map((p) => `.${p}`).join("")}`;
        ctx.emissions.symbols.push({ id, origin: { t: "demand", path: [...path] }, binding });
        return { t: "symbol", id, path: [] };
    }
}
/** The coercion table a `!` collects by (`v3-symbols.md` §2.2). */
function collectConstraints(v) {
    if (v.t === "constraint")
        return [v];
    if (v.t === "array")
        return v.items.flatMap(collectConstraints);
    throw EvalError.typeMismatch(`! expects a constraint or an array of them, got ${describeValue(v)}`);
}
function describeCallee(e) {
    return e.t === "Path" ? [e.root, ...e.fields].join(".") : "a call";
}
// Documents ----------------------------------------------------------------
function evalAttribute(ctx, env, a) {
    if (a.t === "Attr") {
        return { t: "attribute", name: a.name, value: toJson(evalExpr(ctx, env, a.value)) };
    }
    return {
        t: "action",
        event: a.event,
        key: a.key,
        payload: toJson(evalExpr(ctx, env, a.payload)),
    };
}
function evalChildren(ctx, env, children) {
    return children.flatMap((e) => childNodes(evalExpr(ctx, env, e)));
}
function childNodes(v) {
    if (v.t === "node")
        return [v.node];
    if (v.t === "array")
        return v.items.flatMap(childNodes);
    return [{ t: "text", value: toJson(v), annotations: noAnnotations() }];
}
// Application ----------------------------------------------------------------
function apply(ctx, who, f, args) {
    switch (f.t) {
        case "closure": {
            if (f.params.length !== args.length) {
                throw EvalError.typeMismatch(`closure expects ${f.params.length} argument(s), got ${args.length}`);
            }
            const env2 = new Map(f.env);
            f.params.forEach((p, i) => env2.set(p, args[i]));
            return evalExpr(ctx, env2, f.body);
        }
        case "builtin":
            return evalBuiltin(f.name, args);
        case "import": {
            const only = args.length === 1 ? args[0] : undefined;
            if (only === undefined) {
                throw EvalError.typeMismatch(`${who}: the import of ${JSON.stringify(f.pending.name)} expects exactly 1 argument, the parameters to add`);
            }
            if (only.t !== "object") {
                throw EvalError.typeMismatch(`${who}: the import of ${JSON.stringify(f.pending.name)} takes an object of parameters, got ${describeValue(only)}`);
            }
            const params = new Map(f.pending.params);
            for (const [k, v] of only.fields)
                params.set(k, v);
            return { t: "import", pending: { ...f.pending, params } };
        }
        default:
            throw EvalError.typeMismatch(`${who} is not callable: ${describeValue(f)}`);
    }
}
function evalCollection(ctx, env, who, e) {
    const v = evalExpr(ctx, env, e);
    if (v.t === "array")
        return v.items;
    if (isSymbolic(v))
        throw EvalError.notConcrete(who);
    throw EvalError.typeMismatch(`${who} expects an array as its first argument, got ${describeValue(v)}`);
}
/**
 * The elements ordered by the key `fn` gives each (`reference.md` §11,
 * *Sorting*). The checks come in the order the reference gives them: element
 * by element in index order, the application of the key function and then the
 * key it gave, so the first element whose application or key fails decides
 * the error. Only then is anything reordered, so the key function runs
 * exactly once per element, and not at all for an empty list.
 *
 * Element `i` precedes element `j` when its key is smaller (larger, when
 * descending), or when the keys are equal and `i < j`. The comparison ends on
 * the index, so no two elements compare equal: there is one result whatever
 * algorithm `Array.prototype.sort` runs, and the descending sort is not the
 * reversal of the ascending one.
 */
function sortByKey(ctx, who, descending, fnVal, items) {
    const keyed = [];
    items.forEach((item, index) => {
        const key = sortKey(who, apply(ctx, who, fnVal, [item]));
        const first = keyed[0]?.key ?? key;
        if (key.t !== first.t) {
            throw EvalError.typeMismatch(`${who} expects keys of one type, got ${describeValue(first)} and then ${describeValue(key)}`);
        }
        keyed.push({ key, item, index });
    });
    const direction = descending ? -1 : 1;
    keyed.sort((a, b) => direction * compareSortKeys(a.key, b.key) || a.index - b.index);
    return keyed.map((k) => k.item);
}
/** A key is an integer, a float or a string; a symbol or a term is `NotConcrete`. */
function sortKey(who, v) {
    if (v.t === "int" || v.t === "float" || v.t === "str")
        return v;
    if (isSymbolic(v))
        throw EvalError.notConcrete(who);
    throw EvalError.typeMismatch(`${who} expects a key that is an integer, a float or a string, got ${describeValue(v)}`);
}
/** Two keys of the same type, which `sortByKey` has checked. A float is never a NaN, so the order is total. */
function compareSortKeys(a, b) {
    if (a.t === "str")
        return compareCodePoints(a.value, b.value);
    const x = a.value;
    const y = b.value;
    return x < y ? -1 : x > y ? 1 : 0;
}
/**
 * Lexicographic order by Unicode code point, a proper prefix first. `<` on
 * two strings compares UTF-16 code units, which puts U+1F600 (the surrogates
 * `D83D DE00`) before U+FF5E; by code point it comes after.
 *
 * Two well-formed strings differ first at a code unit, and the code points
 * holding those two units compare as the units do unless exactly one of the
 * units is a surrogate: a surrogate belongs to a code point above U+FFFF,
 * which is larger than any code point a unit outside the surrogate range can
 * be. When both are surrogates they are of the same kind, the leading ones
 * of two code points or the trailing ones after equal leading ones, and
 * their order is that of the code points.
 */
function compareCodePoints(a, b) {
    const n = Math.min(a.length, b.length);
    for (let i = 0; i < n; i += 1) {
        const x = a.charCodeAt(i);
        const y = b.charCodeAt(i);
        if (x === y)
            continue;
        const xs = isSurrogate(x);
        if (xs !== isSurrogate(y))
            return xs ? 1 : -1;
        return x < y ? -1 : 1;
    }
    return a.length - b.length;
}
function isSurrogate(unit) {
    return unit >= 0xd800 && unit <= 0xdfff;
}
// Concat -----------------------------------------------------------------
/**
 * The monoid operation over the three types that have one. Either side being a
 * symbol is `NotConcrete` (`v3-symbols.md` §1.5), ahead of the generic
 * mismatch: the spine of a concatenation must be known.
 */
function concatValues(l, r) {
    if (isSymbolic(l) || isSymbolic(r))
        throw EvalError.notConcrete("<>");
    if (l.t === "str" && r.t === "str")
        return { t: "str", value: l.value + r.value };
    if (l.t === "array" && r.t === "array")
        return { t: "array", items: [...l.items, ...r.items] };
    if (l.t === "object" && r.t === "object") {
        const fields = new Map(l.fields);
        for (const [k, v] of r.fields)
            fields.set(k, v);
        return { t: "object", fields };
    }
    throw EvalError.concatMismatch(`${describeValue(l)} and ${describeValue(r)}`);
}
// Imports --------------------------------------------------------------------
function forceImport(ctx, pending) {
    const result = runLibrary(ctx, pending.name, { t: "object", fields: new Map(pending.params) });
    return applyQueued(ctx, pending.queued, result);
}
function runLibrary(ctx, name, ctxVal) {
    if (ctx.inProgress.has(name))
        throw EvalError.importCycle(name);
    const rawProg = ctx.libs.get(name);
    if (rawProg === undefined)
        throw EvalError.unknownLibrary(name);
    // The static counterpart of `AllocationInLibrary` (`v3-symbols.md` §6),
    // checked lexically before evaluating anything and deliberately *not*
    // wrapped in `InLibrary`: this is a property of the library's own source.
    if (symbolSites(rawProg).length !== 0)
        throw EvalError.allocationInLibrary(name);
    const prog = withTypeErrors(() => eraseTypes(ctx.libs, rawProg));
    const ctx2 = enterLibrary(name, ctx);
    try {
        const { statements, root } = unlets(prog.root);
        const libEnv = initialEnv(ctx.arithmetic, ctxVal);
        const bindingNames = [];
        for (const stmt of statements) {
            switch (stmt.t) {
                case "Let":
                // Erasure has already turned an annotated binding into a `Let` plus
                // `Emit` by the time a library's chain gets here; this branch, like
                // `TypeDecl`'s, only guards totality.
                case "Annotate":
                    libEnv.set(stmt.name, evalExpr(ctx2, libEnv, stmt.value));
                    if (!isHiddenName(stmt.name))
                        bindingNames.push(stmt.name);
                    break;
                // A library's own emissions are collected when a field is read off its
                // import (`v3-symbols.md` §2.3) — which is exactly when this runs.
                case "Emit":
                    ctx2.emissions.constraints.push(...collectConstraints(evalExpr(ctx2, libEnv, stmt.constraint)));
                    break;
                case "TypeDecl":
                case "TypeEmit":
                    break;
            }
        }
        const rendered = evalExpr(ctx2, libEnv, root);
        const vals = new Map();
        for (const n of bindingNames)
            vals.set(n, libEnv.get(n));
        const out = new Map();
        out.set("rendered", rendered);
        out.set("vals", { t: "importResult", fields: vals });
        return { t: "importResult", fields: out };
    }
    catch (e) {
        if (e instanceof EvalError)
            throw EvalError.inLibrary(name, e);
        throw e;
    }
}
// Action adaptation -----------------------------------------------------------
function adaptValue(ctx, adaptation, fnVal, v) {
    switch (v.t) {
        case "node":
            return {
                t: "node",
                node: mapActions(v.node, (event, key, payload) => adaptAction(ctx, adaptation, fnVal, event, key, payload)),
            };
        case "importResult": {
            const fields = new Map();
            for (const [k, x] of v.fields)
                fields.set(k, adaptValue(ctx, adaptation, fnVal, x));
            return { t: "importResult", fields };
        }
        case "array":
            return { t: "array", items: v.items.map((x) => adaptValue(ctx, adaptation, fnVal, x)) };
        case "object": {
            const fields = new Map();
            for (const [k, x] of v.fields)
                fields.set(k, adaptValue(ctx, adaptation, fnVal, x));
            return { t: "object", fields };
        }
        case "import":
            return {
                t: "import",
                pending: { ...v.pending, queued: [...v.pending.queued, { adaptation, fn: fnVal }] },
            };
        default:
            return v;
    }
}
function applyQueued(ctx, queued, v0) {
    let v = v0;
    for (const q of queued)
        v = adaptValue(ctx, q.adaptation, q.fn, v);
    return v;
}
function adaptAction(ctx, adaptation, fnVal, event, key, payload) {
    const key2 = adaptKey(adaptation, key);
    if (fnVal === null)
        return { t: "action", event, key: key2, payload };
    const actionObj = new Map();
    actionObj.set("eventType", { t: "str", value: event });
    actionObj.set("key", { t: "str", value: key2 });
    actionObj.set("payload", fromJson(payload));
    const result = toJson(apply(ctx, "adapt-actions", fnVal, [{ t: "object", fields: actionObj }]));
    if (!isJsonObject(result)) {
        throw EvalError.typeMismatch("adapt-actions: the function must return an object with an eventType field");
    }
    const event2 = getField(result, "eventType");
    if (typeof event2 !== "string") {
        throw EvalError.typeMismatch('adapt-actions: the function\'s result needs a string "eventType" field');
    }
    const payload2 = hasField(result, "payload") ? result["payload"] : null;
    return { t: "action", event: event2, key: key2, payload: payload2 };
}
// Paths and fields -----------------------------------------------------------
function walkFields(ctx, context, v, fields) {
    if (fields.length === 0)
        return v;
    const field = fields[0];
    const rest = fields.slice(1);
    switch (v.t) {
        case "object":
        case "importResult": {
            const next = v.fields.get(field);
            if (next === undefined)
                throw EvalError.pathNotFound(context);
            return walkFields(ctx, context, next, rest);
        }
        case "import":
            return walkFields(ctx, context, forceImport(ctx, v.pending), fields);
        // Projection (`v3-symbols.md` §1.6): reads nothing, and is never rejected.
        // Consumes every remaining segment at once, since a projection just
        // extends the path.
        case "symbol":
            return { t: "symbol", id: v.id, path: [...v.path, ...fields] };
        default:
            throw EvalError.typeMismatch(`cannot read field ${JSON.stringify(field)} of ${describeValue(v)} in path ${JSON.stringify(context.join("."))}`);
    }
}
// Conversion ------------------------------------------------------------------
/** Down to JSON, at the boundaries where only JSON is meaningful. */
export function toJson(v) {
    switch (v.t) {
        case "null":
            return null;
        case "bool":
            return v.value;
        case "int":
            return v.value;
        // Canonical form (`json.ts`): a whole-valued float is a `Float`, so that
        // it is still a float once it has left the evaluator.
        case "float":
            return float(v.value);
        case "str":
            return v.value;
        case "array":
            return v.items.map(toJson);
        case "object":
            return jsonObjectFrom([...v.fields].map(([k, x]) => [k, toJson(x)]));
        case "node":
            throw EvalError.typeMismatch("a document node is not a plain value -- nest it as a child rather than using it where a value is expected");
        case "closure":
            throw EvalError.typeMismatch("expected a value, got a function -- call it first, e.g. $my-fn(...)");
        case "builtin":
            throw EvalError.typeMismatch(`expected a value, got the builtin ${JSON.stringify(v.name)} -- call it first`);
        case "importResult":
            throw EvalError.typeMismatch("expected a value, got an import result -- read .rendered, .vals, or a binding name from it first");
        case "import":
            throw EvalError.typeMismatch(`expected a value, got the import of ${JSON.stringify(v.pending.name)} -- read .rendered or .vals from it to run it first`);
        // A symbol may sit in an attribute, a payload, a value slot or a text
        // child (`v3-symbols.md` §1.5), so this always succeeds; `requireConcrete`
        // is what refuses one. §5.3's tagged shape: both fields required.
        case "symbol":
            return { $sym: v.id, path: [...v.path] };
        // A term crosses wherever a symbol does, as §5.3's other tagged shape.
        // Its operands are numbers, symbols and terms, so they always cross.
        case "term":
            return { $term: v.op, arguments: v.operands.map(toJson) };
        case "constraint":
            throw EvalError.typeMismatch(`a constraint (${JSON.stringify(v.name)}) cannot cross a JSON boundary -- only "!" may consume it`);
    }
}
/**
 * Whether a value is itself a symbol or a term (`v3-symbols.md` §1.9), which
 * is what the operations that inspect one value refuse.
 */
function isSymbolic(v) {
    return v.t === "symbol" || v.t === "term";
}
/**
 * Whether a value is, or contains, a symbol or a term (`v3-symbols.md` §1.5,
 * §1.7, §1.9): a structure built of concrete pieces is itself concrete.
 */
function containsSymbol(v) {
    if (isSymbolic(v))
        return true;
    if (v.t === "array")
        return v.items.some(containsSymbol);
    if (v.t === "object")
        return [...v.fields.values()].some(containsSymbol);
    return false;
}
/**
 * Requires a value with no symbol anywhere in it, for the operations §1.5
 * lists as needing to *know* something about their argument rather than merely
 * carry it: `str`, `eq`, and an allocation key.
 */
function requireConcrete(who, v) {
    if (containsSymbol(v))
        throw EvalError.notConcrete(who);
    return toJson(v);
}
/**
 * A JSON number as a value, by `json.ts`'s classification, or `null` for
 * anything that is not a number. One the value domain does not hold (an
 * integer outside the range, a float that is not finite) is refused with a
 * `JsonNumberError`, and a negative zero is read as zero.
 */
function numberFromJson(v) {
    const n = classifyNumber(v);
    if (n === null)
        return null;
    const value = Number(normalizeNumber(n.type, n.value));
    return { t: n.type === "integer" ? "int" : "float", value };
}
/** A JSON value the evaluator itself produced (an action payload), back to a value. */
function fromJson(v) {
    if (v === null)
        return { t: "null" };
    if (typeof v === "boolean")
        return { t: "bool", value: v };
    if (typeof v === "string")
        return { t: "str", value: v };
    const n = numberFromJson(v);
    if (n !== null)
        return n;
    if (Array.isArray(v))
        return { t: "array", items: v.map(fromJson) };
    const o = v;
    const fields = new Map();
    for (const k of ownKeys(o))
        fields.set(k, fromJson(o[k]));
    return { t: "object", fields };
}
/**
 * The input context's boundary, decoded whole before evaluation starts.
 *
 * Numbers (`reference.md` §3): a number is typed by `json.ts`'s
 * classification, which for JSON text read by `parseJson` is the type of the
 * literal of the same text. One the value domain does not hold is a
 * `TypeMismatch`.
 *
 * Reserved keys (`v3-symbols.md` §5.3): `"$sym"`, `"$type"` and `"$term"` are
 * recursively refused as ordinary object keys, in every profile. In concrete
 * mode each is refused unconditionally. In symbolic mode, seeding (§5.4)
 * accepts a well-formed `{"$sym": ..., "path": [...]}` back as an actual
 * symbol and, with the arithmetic profile on, a well-formed term back as an
 * actual term.
 */
function checkedFromJson(options, v) {
    if (v === null)
        return { t: "null" };
    if (typeof v === "boolean")
        return { t: "bool", value: v };
    if (typeof v === "string")
        return { t: "str", value: v };
    let n;
    try {
        n = numberFromJson(v);
    }
    catch (e) {
        if (!(e instanceof JsonNumberError))
            throw e;
        throw EvalError.typeMismatch(`the context holds a number that is not a value: ${e.message}`);
    }
    if (n !== null)
        return n;
    if (Array.isArray(v))
        return { t: "array", items: v.map((x) => checkedFromJson(options, x)) };
    const o = v;
    if (hasField(o, "$type")) {
        throw EvalError.typeMismatch('the context carries the reserved key "$type", which only a typed envelope may use');
    }
    if (hasField(o, "$sym")) {
        if (options.mode === "concrete") {
            throw EvalError.typeMismatch('the context carries the reserved key "$sym", which only a symbolic envelope may use');
        }
        return decodeSymbolRef(o["$sym"], o);
    }
    if (hasField(o, "$term")) {
        if (options.mode === "concrete") {
            throw EvalError.typeMismatch('the context carries the reserved key "$term", which only a symbolic envelope may use');
        }
        return decodeTerm(options, o["$term"], o);
    }
    const fields = new Map();
    for (const k of ownKeys(o))
        fields.set(k, checkedFromJson(options, o[k]));
    return { t: "object", fields };
}
/**
 * Decodes a `{"$term": <op>, "arguments": [...]}` object back into a term
 * (`v3-symbols.md` §5.3, §5.4). A well-formed term is one a call could have
 * built, so this is the call's own check, `arithmeticOperands`, on arguments
 * already decoded, which holds a nested term to the same rule. Two things a
 * call accepts are refused first: an array, since a term holds its operands
 * already flattened, and operands that are all numbers, since the call would
 * have computed. Without the arithmetic profile no `op` is known, so every
 * term is refused.
 */
function decodeTerm(options, opVal, obj) {
    const argsJson = getField(obj, "arguments");
    if (typeof opVal !== "string" || ownKeys(obj).length !== 2 || argsJson === undefined) {
        throw EvalError.typeMismatch('a "$term" object must be exactly {"$term": <op>, "arguments": [<argument>, ...]}');
    }
    if (!Array.isArray(argsJson)) {
        throw EvalError.typeMismatch(`a term's "arguments" must be an array`);
    }
    const args = argsJson.map((x) => checkedFromJson(options, x));
    const op = JSON.stringify(opVal);
    if (!options.arithmetic) {
        throw EvalError.typeMismatch(`the context carries a term (${op}), which needs the arithmetic profile`);
    }
    if (!arithmeticNames.includes(opVal)) {
        throw EvalError.typeMismatch(`a term names an unknown operation: ${op}`);
    }
    if (args.some((a) => a.t === "array")) {
        throw EvalError.typeMismatch(`a term (${op}) holds its operands flattened, not in an array`);
    }
    const operands = arithmeticOperands(opVal, args);
    if (!operands.some(isSymbolic)) {
        throw EvalError.typeMismatch(`a term (${op}) must hold a symbol or a term among its arguments`);
    }
    return { t: "term", op: opVal, operands };
}
/**
 * Decodes a `{"$sym": <id>, "path": [<segment>, ...]}` object back into a
 * symbol (`v3-symbols.md` §5.4). Both fields are required, and no other key
 * may be present.
 */
function decodeSymbolRef(symVal, obj) {
    const badShape = () => EvalError.typeMismatch('a "$sym" object must be exactly {"$sym": <id>, "path": [<segment>, ...]}');
    if (typeof symVal !== "string")
        throw badShape();
    if (ownKeys(obj).length !== 2)
        throw badShape();
    const pathArr = getField(obj, "path");
    if (!Array.isArray(pathArr))
        throw badShape();
    const path = pathArr.map((x) => {
        if (typeof x !== "string") {
            throw EvalError.typeMismatch('a symbol reference\'s "path" must be an array of strings');
        }
        return x;
    });
    return { t: "symbol", id: symVal, path };
}
function describeValue(v) {
    switch (v.t) {
        case "null":
            return "null";
        case "bool":
            return "a boolean";
        case "int":
            return "an integer";
        case "float":
            return "a float";
        case "str":
            return "a string";
        case "array":
            return "an array";
        case "object":
            return "an object";
        case "node":
            return "a document node";
        case "closure":
            return "a function";
        case "builtin":
            return `the builtin ${JSON.stringify(v.name)}`;
        case "importResult":
            return "an import result";
        case "import":
            return `the not-yet-run import of ${JSON.stringify(v.pending.name)}`;
        case "symbol":
            return "a symbol";
        case "term":
            return `a term (${JSON.stringify(v.op)})`;
        case "constraint":
            return `a constraint (${JSON.stringify(v.name)})`;
    }
}
/** Control flow must be concrete (`v3-symbols.md` §1.5). */
function requireBool(who, v) {
    if (v.t === "bool")
        return v.value;
    if (isSymbolic(v))
        throw EvalError.notConcrete(who);
    throw EvalError.typeMismatch(`${who} must be a boolean, got ${describeValue(v)}`);
}
// Builtins ------------------------------------------------------------------
function evalBuiltin(name, args) {
    const arityErr = (n) => EvalError.typeMismatch(`${name} expects exactly ${n} argument(s), got ${args.length}`);
    const arg = (i) => args[i];
    switch (name) {
        case "cardinality":
        case "count": {
            if (args.length !== 1)
                throw arityErr(1);
            const a = arg(0);
            if (a.t === "array")
                return { t: "int", value: a.items.length };
            if (a.t === "object")
                return { t: "int", value: a.fields.size };
            if (isSymbolic(a))
                throw EvalError.notConcrete(name);
            throw EvalError.typeMismatch(`${name} expects an array or object, got ${describeValue(a)}`);
        }
        case "str": {
            if (args.length !== 1)
                throw arityErr(1);
            return { t: "str", value: displayString(requireConcrete(name, arg(0))) };
        }
        case "not":
            if (args.length !== 1)
                throw arityErr(1);
            return { t: "bool", value: !asBool(name, arg(0)) };
        case "and": {
            let acc = true;
            for (const a of args)
                acc = acc && asBool(name, a);
            return { t: "bool", value: acc };
        }
        case "or": {
            let acc = false;
            for (const a of args)
                acc = acc || asBool(name, a);
            return { t: "bool", value: acc };
        }
        case "eq": {
            if (args.length !== 2)
                throw arityErr(2);
            const a = requireConcrete(name, arg(0));
            const b = requireConcrete(name, arg(1));
            return { t: "bool", value: jsonEqual(a, b) };
        }
        case "lt":
        case "lte":
        case "gt":
        case "gte": {
            if (args.length !== 2)
                throw arityErr(2);
            // Two integers or two floats (`reference.md` §6). A mixed pair is a
            // `TypeMismatch` like any other pair of two types: nothing is promoted,
            // so `gt(1.5, 0)` is written `gt(1.5, 0.0)`.
            const na = asNumber(name, arg(0));
            const nb = asNumber(name, arg(1));
            if (na.t !== nb.t) {
                throw EvalError.typeMismatch(`${name} expects two integers or two floats, got ${describeValue(na)} and ${describeValue(nb)}`);
            }
            const a = na.value;
            const b = nb.value;
            const r = name === "lt" ? a < b : name === "lte" ? a <= b : name === "gt" ? a > b : a >= b;
            return { t: "bool", value: r };
        }
        case "has":
            if (args.length !== 2)
                throw arityErr(2);
            return { t: "bool", value: hasImpl(name, arg(0), arg(1)) };
        case "lookup":
            if (args.length !== 3)
                throw arityErr(3);
            return lookupImpl(name, arg(0), arg(1), arg(2));
        case "format-number":
            if (args.length !== 3)
                throw arityErr(3);
            return { t: "str", value: formatNumberImpl(name, arg(0), arg(1), arg(2)) };
        case "concat":
            return { t: "array", items: args.flatMap((a) => asArray(name, a)) };
        case "append": {
            if (args.length !== 2)
                throw arityErr(2);
            return { t: "array", items: [...asArray(name, arg(0)), arg(1)] };
        }
        default:
            if (arithmeticNames.includes(name))
                return arithmetic(name, args);
            throw EvalError.unboundName(name);
    }
}
function asBool(name, v) {
    if (v.t === "bool")
        return v.value;
    throw EvalError.typeMismatch(`${name} expects a boolean argument, got ${describeValue(v)}`);
}
/**
 * A number operand, returned as it is so that its type is still there to
 * check. A symbol or a term is `NotConcrete` (`v3-symbols.md` §1.5).
 */
function asNumber(name, v) {
    if (v.t === "int" || v.t === "float")
        return v;
    if (isSymbolic(v))
        throw EvalError.notConcrete(name);
    throw EvalError.typeMismatch(`${name} expects a number argument, got ${describeValue(v)}`);
}
function asArray(name, v) {
    if (v.t === "array")
        return v.items;
    throw EvalError.typeMismatch(`${name} expects an array argument, got ${describeValue(v)}`);
}
/**
 * An index is a non-negative integer: `-1` is not one, and neither is a
 * float, `1.0` included, since nothing converts a float into an integer here.
 * The caller has checked the type.
 */
function asIndex(n) {
    return n >= 0 ? n : null;
}
/**
 * Deliberately tolerant: a missing key, an out-of-range index, or a container
 * of the wrong shape all answer `false` — except a symbolic container or key, which
 * is `NotConcrete` rather than a lie (`v3-symbols.md` §1.5).
 */
function hasImpl(name, container, key) {
    if (isSymbolic(container) || isSymbolic(key))
        throw EvalError.notConcrete(name);
    if (container.t === "object" && key.t === "str")
        return container.fields.has(key.value);
    if (container.t === "array" && key.t === "int") {
        const i = asIndex(key.value);
        return i !== null && i < container.items.length;
    }
    return false;
}
function lookupImpl(name, container, key, fallback) {
    if (isSymbolic(container) || isSymbolic(key))
        throw EvalError.notConcrete(name);
    if (container.t === "object" && key.t === "str") {
        return container.fields.get(key.value) ?? fallback;
    }
    if (container.t === "array" && key.t === "int") {
        const i = asIndex(key.value);
        if (i === null)
            return fallback;
        return container.items[i] ?? fallback;
    }
    return fallback;
}
function isNumberValue(v) {
    return v.t === "int" || v.t === "float";
}
/**
 * One of the arithmetic builtins, applied. Operands that are all numbers
 * compute; if one is a symbol or a term the result is a term holding the
 * flattened operands exactly as written (`v3-symbols.md` §1.9). Every operand
 * is checked before either happens, so a `TypeMismatch` takes precedence over
 * a `NotRepresentable`.
 */
function arithmetic(name, args) {
    const operands = arithmeticOperands(name, args);
    if (operands.some(isSymbolic))
        return { t: "term", op: name, operands };
    return compute(name, operands);
}
function flattenOperands(args) {
    return args.flatMap((a) => (a.t === "array" ? flattenOperands(a.items) : [a]));
}
/**
 * The operands of a call, checked as far as they can be without knowing what
 * a symbol stands for; every refusal is a `TypeMismatch`.
 *
 * - `sum` and `product` flatten their arguments by the rule children use: an
 *   array contributes each of its elements, recursively, in order. They need
 *   at least one operand afterwards. The others take a fixed count and do not
 *   flatten, so an array given to one is refused whatever it holds.
 * - Each operand is a number, a symbol or a term. A symbol or a term stands
 *   for one number of either type and is not looked into.
 * - The operands that are numbers agree with each other in type and with what
 *   the builtin accepts. Nothing is converted or promoted.
 */
function arithmeticOperands(name, args) {
    const variadic = name === "sum" || name === "product";
    let operands;
    if (variadic) {
        operands = flattenOperands(args);
        if (operands.length === 0) {
            throw EvalError.typeMismatch(`${name} expects at least one operand: seed it with the zero or the one of the intended type`);
        }
    }
    else {
        const n = name === "quotient" || name === "floor-quotient" || name === "modulo" ? 2 : 1;
        if (args.length !== n) {
            throw EvalError.typeMismatch(`${name} expects exactly ${n} argument(s), got ${args.length}`);
        }
        operands = args;
    }
    for (const v of operands) {
        if (!isNumberValue(v) && !isSymbolic(v)) {
            throw EvalError.typeMismatch(`${name} expects number operands, got ${describeValue(v)}`);
        }
    }
    const numbers = operands.filter(isNumberValue);
    const all = (t) => numbers.every((v) => v.t === t);
    let accepted;
    let wanted;
    switch (name) {
        case "quotient":
            [accepted, wanted] = [all("float"), "two floats"];
            break;
        case "inverse":
            [accepted, wanted] = [all("float"), "a float"];
            break;
        case "floor-quotient":
        case "modulo":
            [accepted, wanted] = [all("int"), "two integers"];
            break;
        case "sum":
        case "product":
            [accepted, wanted] = [all("int") || all("float"), "all integers or all floats"];
            break;
        // `negate`, `floor`, `real` and `round` take a number of either type.
        default:
            [accepted, wanted] = [true, "a number"];
    }
    if (!accepted) {
        throw EvalError.typeMismatch(`${name} expects ${wanted}, got ${numbers.map(describeValue).join(", ")}`);
    }
    return operands;
}
/** An integer result, or `NotRepresentable` outside the integer range. */
function integerResult(name, n) {
    if (!inIntegerRange(n)) {
        throw EvalError.notRepresentable(`${name}: the result is outside the integer range, -(2^53 - 1) to 2^53 - 1`);
    }
    // There is no negative zero, of either type.
    return { t: "int", value: n === 0 ? 0 : n };
}
/** A float result, or `NotRepresentable` when it is not finite: overflow and a zero divisor alike. */
function floatResult(name, n) {
    if (!Number.isFinite(n)) {
        throw EvalError.notRepresentable(`${name}: the result is not a finite float`);
    }
    return { t: "float", value: n === 0 ? 0 : n };
}
/**
 * `floor-quotient` and `modulo` together: `a` is `b * quotient + remainder`,
 * the quotient rounded toward negative infinity and the remainder zero or of
 * the sign of `b`.
 *
 * `a / b` is not used, since its rounding can land on the integer above the
 * true quotient. `%` is exact and truncates; `a - r` is then a multiple of
 * `b` no larger in magnitude than `a`, so its division is exact too, and one
 * step moves the truncated pair to the floored one.
 */
function floorDivision(name, a, b) {
    if (b === 0)
        throw EvalError.notRepresentable(`${name}: the divisor is zero`);
    const r = a % b;
    const q = (a - r) / b;
    const adjust = r !== 0 && r < 0 !== b < 0;
    return { quotient: adjust ? q - 1 : q, remainder: adjust ? r + b : r };
}
/**
 * The concrete rules (`reference.md` §11, *Semantics*), over operands
 * `arithmeticOperands` accepted and that are all numbers.
 *
 * Both number types hold a double. Float arithmetic is therefore the host's,
 * one correctly rounded operation at a time, and nothing here can be
 * contracted into a fused multiply-add. Integer arithmetic is exact as long
 * as each result is checked: the sum or the product of two integers of the
 * guaranteed range that leaves it rounds to a double of magnitude at least
 * `2^53`, which `inIntegerRange` refuses.
 */
function compute(name, operands) {
    const first = operands[0];
    const second = operands[1];
    const result = first.t === "int" ? integerResult : floatResult;
    switch (name) {
        case "sum":
        case "product": {
            // A left fold from the first operand, each step checked: an integer
            // step out of range is an error although the total would be in range.
            let acc = result(name, first.value);
            for (const v of operands.slice(1)) {
                acc = result(name, name === "sum" ? acc.value + v.value : acc.value * v.value);
            }
            return acc;
        }
        case "negate":
            return result(name, -first.value);
        case "quotient":
            return floatResult(name, first.value / second.value);
        case "inverse":
            return floatResult(name, 1 / first.value);
        case "floor-quotient":
            return integerResult(name, floorDivision(name, first.value, second.value).quotient);
        case "modulo":
            return integerResult(name, floorDivision(name, first.value, second.value).remainder);
        case "floor":
            return first.t === "int" ? first : integerResult(name, Math.floor(first.value));
        case "real":
            return { t: "float", value: first.value };
        // The integer nearest to the exact value of the float, a tie going away
        // from zero: the rule `format-number` applies with no decimals. A
        // magnitude beyond the integer range converts to a double of at least
        // `2^53`, which `integerResult` refuses.
        case "round": {
            if (first.t === "int")
                return first;
            const magnitude = Number(roundedMagnitude(first.value, 0));
            return integerResult(name, first.value < 0 ? -magnitude : magnitude);
        }
        default:
            throw EvalError.unboundName(name);
    }
}
// Number formatting (reference.md §11) -------------------------------------------
/**
 * The magnitude of a finite double as `mantissa * 2^exponent`, exactly: a
 * double is a binary fraction, and these are its two fields. A subnormal has
 * no implicit leading bit and the smallest exponent.
 */
function exactMagnitude(x) {
    const view = new DataView(new ArrayBuffer(8));
    view.setFloat64(0, Math.abs(x));
    const bits = view.getBigUint64(0);
    const biased = Number(bits >> 52n);
    const fraction = bits & ((1n << 52n) - 1n);
    return biased === 0
        ? { mantissa: fraction, exponent: -1074 }
        : { mantissa: fraction | (1n << 52n), exponent: biased - 1075 };
}
/**
 * `|x| * 10^places` rounded to the nearest integer; when two are equally
 * near, the larger, which is the one farther from zero once the sign is put
 * back. This is the one rounding rule of `round` and `format-number`
 * (`reference.md` §11), computed on the exact value of the double: neither
 * `toFixed`, which stops at 1e21 and at 100 digits, nor `Math.round`, which
 * takes a negative tie toward zero, follows it.
 *
 * `mantissa * 10^places` is an integer and the exponent a power of two, so a
 * non-negative exponent leaves nothing to round and a negative one is a
 * right shift: the bit just below the units says whether the part shifted
 * out is at least a half.
 */
function roundedMagnitude(x, places) {
    const { mantissa, exponent } = exactMagnitude(x);
    const scaled = mantissa * 10n ** BigInt(places);
    if (exponent >= 0)
        return scaled << BigInt(exponent);
    const shift = BigInt(-exponent);
    return (scaled >> shift) + ((scaled >> (shift - 1n)) & 1n);
}
/**
 * `format-number(x, decimals, group)` (`reference.md` §11, *Number
 * formatting*). The arguments are examined left to right and the first that
 * is not acceptable decides the error: a symbol or a term is `NotConcrete`,
 * anything else of the wrong type a `TypeMismatch`.
 */
function formatNumberImpl(name, x, decimals, group) {
    const refuse = (wanted, got) => isSymbolic(got)
        ? EvalError.notConcrete(name)
        : EvalError.typeMismatch(`${name} expects ${wanted}, got ${describeValue(got)}`);
    if (!isNumberValue(x))
        throw refuse("a number as its first argument", x);
    if (decimals.t !== "int")
        throw refuse("an integer number of decimals", decimals);
    if (decimals.value < 0 || decimals.value > 20) {
        throw EvalError.typeMismatch(`${name} expects a number of decimals from 0 to 20, got ${decimals.value}`);
    }
    if (group.t !== "str")
        throw refuse("a string as its separator", group);
    return formatNumber(x.value, decimals.value, group.value);
}
/**
 * A number in positional decimal notation: an optional `-`, the integer part
 * with the separator between its groups of three digits counted from the
 * point leftward, and, when there are decimals, a `.` and exactly that many
 * digits. Never an exponent, and no sign on a result whose digits are all
 * zero. An integer is a double it holds exactly, so both types go the same
 * way.
 */
function formatNumber(x, places, separator) {
    const magnitude = roundedMagnitude(x, places);
    // At least one digit before the point: `0.50`, never `.50`.
    const digits = magnitude.toString().padStart(places + 1, "0");
    const integerPart = digits.slice(0, digits.length - places);
    const groups = [];
    for (let end = integerPart.length; end > 0; end -= 3) {
        groups.unshift(integerPart.slice(Math.max(0, end - 3), end));
    }
    const sign = x < 0 && magnitude !== 0n ? "-" : "";
    const grouped = separator === "" ? integerPart : groups.join(separator);
    return sign + grouped + (places === 0 ? "" : `.${digits.slice(digits.length - places)}`);
}
