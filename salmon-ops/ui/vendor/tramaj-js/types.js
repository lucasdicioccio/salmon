/**
 * v4-types resolution, normalisation, canonical identity, and the closure and
 * constraint machinery output needs (`specs/v4-types.md`). A static pass over
 * parsed programs, evaluating nothing.
 *
 * **Canonical ids and the library segment.** §3 renders a `Ref` as
 * `library ":" name` and requires injectivity. A `name` is always a colon-free
 * identifier, so splitting on the last colon is unambiguous however many
 * colons `library` contains — but the program being resolved itself needs some
 * token for "no library", and no plain word is safe, since a library can
 * legally be named `"root"`. `renderLibrary` wraps every real library key in a
 * literal pair of quotes and reserves the bare word `root`.
 */
import { transitiveImportNames, typeExprsIn, typeParamCollisions, typeParams } from "./analysis.js";
import { subExprs, typeDecls, unlets, } from "./ast.js";
import { compareStrings } from "./json.js";
/** `v4-types.md` §10's analysis errors. `message` leads with the bare kind. */
export class TypeError extends Error {
    name = "TramajTypeError";
    kind;
    constructor(kind, detail) {
        super(detail === "" ? kind : `${kind} ${detail}`);
        this.kind = kind;
    }
    static unresolvedType(n) {
        return new TypeError("UnresolvedType", n);
    }
    static notStaticallyResolvable(n) {
        return new TypeError("NotStaticallyResolvable", n);
    }
    static partialType(id, path) {
        return new TypeError("PartialType", `${id} ${JSON.stringify(path)}`);
    }
    static typeParamCollision(k) {
        return new TypeError("TypeParamCollision", k);
    }
    static typeCycle(k) {
        return new TypeError("TypeCycle", k);
    }
}
/** Just the type declarations at the top of a program's own statement chain. */
export function programTypeDecls(prog) {
    return new Map(typeDecls(unlets(prog.root).statements));
}
/**
 * Resolves a `TypeExpr` written in `prog`'s own statement chain against
 * `prog`'s own scope: its own declarations for a bare `Name`, and the
 * libraries in `libs` reached through `prog`'s own direct `import(...)`
 * bindings for a `LibRef`. No library is evaluated.
 */
export function resolveTypeExpr(libs, prog, t) {
    return resolveWith(libs, prog, new Map(), new Set(), t);
}
function resolveWith(libs, prog, subst, visiting, t) {
    switch (t.t) {
        case "Prim":
            return { t: "Prim", name: t.name };
        case "Array":
            return { t: "Array", element: resolveWith(libs, prog, subst, visiting, t.element) };
        case "Record": {
            const fields = t.fields.map(([n, ft]) => [n, resolveWith(libs, prog, subst, visiting, ft)]);
            fields.sort((a, b) => compareStrings(a[0], b[0]));
            return { t: "Record", fields };
        }
        case "Union": {
            const arms = t.arms.map(([n, at]) => [n, at === null ? null : resolveWith(libs, prog, subst, visiting, at)]);
            arms.sort((a, b) => compareStrings(a[0], b[0]));
            return { t: "Union", arms };
        }
        // Only a single-segment path is ever substituted: v4-types has no notion of
        // projecting into a type parameter, so a longer path stays a `Var`.
        case "Var": {
            if (t.path.length === 1) {
                const rt = subst.get(t.path[0]);
                if (rt !== undefined)
                    return rt;
            }
            return { t: "Var", path: [...t.path] };
        }
        case "Name":
            if (programTypeDecls(prog).has(t.name))
                return { t: "Ref", lib: null, name: t.name, args: [] };
            throw TypeError.unresolvedType(t.name);
        case "LibRef": {
            const stmts = unlets(prog.root).statements;
            const { key, libProg, params } = resolveLibBinding(libs, stmts, t.lib);
            if (!programTypeDecls(libProg).has(t.name))
                throw TypeError.unresolvedType(t.name);
            if (visiting.has(key))
                throw TypeError.typeCycle(key);
            const visiting2 = new Set(visiting);
            visiting2.add(key);
            // Every type parameter the target library has, not only the ones this
            // import supplies: an unsupplied one still needs a slot in `args`.
            const wanted = new Set(typeParams(libProg).map((p) => p[0] ?? ""));
            const suppliedTypes = new Map(params.flatMap(([k, v]) => (v.t === "PType" ? [[k, v.type]] : [])));
            const args = [];
            for (const k of [...wanted].sort(compareStrings)) {
                const te = suppliedTypes.get(k);
                args.push([
                    k,
                    te === undefined
                        ? { t: "Var", path: [k] }
                        : resolveWith(libs, prog, subst, visiting2, te),
                ]);
            }
            return { t: "Ref", lib: key, name: t.name, args };
        }
    }
}
/**
 * `lib` must be bound, in `stmts`, directly to an `import("key", ...)` — not to
 * anything that merely evaluates to one — and `key` must be present in `libs`.
 * Takes the last such binding if `lib` is somehow bound more than once.
 */
function resolveLibBinding(libs, stmts, lib) {
    let found = null;
    for (const s of stmts) {
        if (s.t === "Let" && s.value.t === "Import" && s.name === lib) {
            found = { key: s.value.name, params: s.value.params };
        }
    }
    if (found === null)
        throw TypeError.notStaticallyResolvable(lib);
    const libProg = libs.get(found.key);
    if (libProg === undefined)
        throw TypeError.notStaticallyResolvable(lib);
    return { key: found.key, libProg, params: found.params };
}
/**
 * `v4-types.md` §3's grammar, rendered. Assumes its argument is already
 * normalised the way `resolveTypeExpr` produces it — this renders the order it
 * is given, it does not sort.
 */
export function canonicalId(rt) {
    switch (rt.t) {
        case "Prim":
            return rt.name;
        case "Array":
            return `[${canonicalId(rt.element)}]`;
        case "Record":
            return `{${rt.fields.map(([n, t]) => `${n}:${canonicalId(t)}`).join(",")}}`;
        case "Union":
            return rt.arms.map(([n, t]) => (t === null ? `|${n}` : `|${n} ${canonicalId(t)}`)).join("");
        case "Ref": {
            const head = `${renderLibrary(rt.lib)}:${rt.name}`;
            if (rt.args.length === 0)
                return head;
            return `${head}[${rt.args.map(([k, t]) => `${k}=${canonicalId(t)}`).join(",")}]`;
        }
        // `Var`'s path excludes the leading `ctx` segment it is always rooted at,
        // so it is reinserted here to render §3's `var ::= "%" path`.
        case "Var":
            return `%ctx${rt.path.map((p) => `.${p}`).join("")}`;
    }
}
function renderLibrary(lib) {
    return lib === null ? "root" : `"${lib}"`;
}
export function resolvedTypeEqual(a, b) {
    if (a.t !== b.t)
        return false;
    switch (a.t) {
        case "Prim":
            return a.name === b.name;
        case "Array":
            return resolvedTypeEqual(a.element, b.element);
        case "Record": {
            const bb = b;
            return (a.fields.length === bb.fields.length &&
                a.fields.every(([n, t], i) => {
                    const other = bb.fields[i];
                    return n === other[0] && resolvedTypeEqual(t, other[1]);
                }));
        }
        case "Union": {
            const bb = b;
            return (a.arms.length === bb.arms.length &&
                a.arms.every(([n, t], i) => {
                    const other = bb.arms[i];
                    if (n !== other[0])
                        return false;
                    if (t === null || other[1] === null)
                        return t === other[1];
                    return resolvedTypeEqual(t, other[1]);
                }));
        }
        case "Ref": {
            const bb = b;
            return (a.lib === bb.lib &&
                a.name === bb.name &&
                a.args.length === bb.args.length &&
                a.args.every(([k, t], i) => {
                    const other = bb.args[i];
                    return k === other[0] && resolvedTypeEqual(t, other[1]);
                }));
        }
        case "Var": {
            const bb = b;
            return a.path.length === bb.path.length && a.path.every((p, i) => p === bb.path[i]);
        }
    }
}
/** Whether a normal form still contains a `Var`, a `Ref`'s own arguments included. */
function containsVar(rt) {
    switch (rt.t) {
        case "Var":
            return true;
        case "Array":
            return containsVar(rt.element);
        case "Record":
            return rt.fields.some(([, t]) => containsVar(t));
        case "Union":
            return rt.arms.some(([, t]) => t !== null && containsVar(t));
        case "Ref":
            return rt.args.some(([, t]) => containsVar(t));
        case "Prim":
            return false;
    }
}
/** The path of the first `Var` found, left-to-right. */
function firstVarPath(rt) {
    switch (rt.t) {
        case "Var":
            return [...rt.path];
        case "Array":
            return firstVarPath(rt.element);
        case "Record": {
            const hit = rt.fields.find(([, t]) => containsVar(t));
            return hit === undefined ? [] : firstVarPath(hit[1]);
        }
        case "Union": {
            const hit = rt.arms.find(([, t]) => t !== null && containsVar(t));
            return hit === undefined || hit[1] === null ? [] : firstVarPath(hit[1]);
        }
        case "Ref": {
            const hit = rt.args.find(([, t]) => containsVar(t));
            return hit === undefined ? [] : firstVarPath(hit[1]);
        }
        case "Prim":
            return [];
    }
}
/** `v4-types.md` §4: a type reaching the root must be closed. */
export function requireClosed(rt) {
    if (containsVar(rt))
        throw TypeError.partialType(canonicalId(rt), firstVarPath(rt));
    return rt;
}
/** `v4-types.md` §10's `TypeParamCollision`, surfaced from `analysis`'s plain set. */
export function checkTypeParamCollisions(prog) {
    const xs = [...typeParamCollisions(prog)].sort(compareStrings);
    const first = xs[0];
    if (first !== undefined)
        throw TypeError.typeParamCollision(first);
}
// Output: the transitive closure of referenced types (v4-types §8) --------
function collectRefs(rt) {
    switch (rt.t) {
        case "Ref":
            return [rt, ...rt.args.flatMap(([, a]) => collectRefs(a))];
        case "Array":
            return collectRefs(rt.element);
        case "Record":
            return rt.fields.flatMap(([, t]) => collectRefs(t));
        case "Union":
            return rt.arms.flatMap(([, t]) => (t === null ? [] : collectRefs(t)));
        case "Prim":
        case "Var":
            return [];
    }
}
function lookupDecl(libs, prog, libKey, name) {
    if (libKey === null) {
        const body = programTypeDecls(prog).get(name);
        if (body === undefined)
            throw TypeError.unresolvedType(name);
        return { declProg: prog, body };
    }
    const libProg = libs.get(libKey);
    if (libProg === undefined)
        throw TypeError.notStaticallyResolvable(libKey);
    const body = programTypeDecls(libProg).get(name);
    if (body === undefined)
        throw TypeError.unresolvedType(name);
    return { declProg: libProg, body };
}
/**
 * The transitive closure of every type referenced from `roots` (`v4-types.md`
 * §8), cut by canonical id so a cycle terminates. Keyed by id rather than by
 * `(library, name)`: two applications of the same generic library at different
 * arguments are two different entries.
 */
export function typeClosure(libs, prog, roots) {
    const acc = new Map();
    const queue = roots.flatMap(collectRefs);
    for (;;) {
        const r = queue.pop();
        if (r === undefined)
            return acc;
        if (r.t !== "Ref")
            continue;
        const cid = canonicalId(r);
        if (acc.has(cid))
            continue;
        const { declProg, body } = lookupDecl(libs, prog, r.lib, r.name);
        const subst = new Map(r.args);
        const def = resolveWith(libs, declProg, subst, new Set(), body);
        queue.push(...collectRefs(def));
        acc.set(cid, def);
    }
}
// Erasure (v4-types §7) ----------------------------------------------------
/**
 * Rewrites every `TypeAnnotate` into the `Let` plus `Emit` §7 specifies,
 * resolving and closing its type first. A `TypeDecl` is left in place rather
 * than stripped, since it is already inert to the evaluator; a `TypeEmit` is
 * dropped. Run once, up front, by every evaluation entry point, which is what
 * keeps `Value` free of a type constructor.
 */
export function eraseTypes(libs, prog) {
    return { kind: prog.kind, root: eraseExpr(libs, prog, prog.root) };
}
function eraseExpr(libs, prog, e) {
    const go = (x) => eraseExpr(libs, prog, x);
    switch (e.t) {
        case "Path":
        case "StringLit":
        case "IntLit":
        case "FloatLit":
        case "BoolLit":
        case "NullLit":
        case "Demand":
            return e;
        case "FieldAccess":
            return { t: "FieldAccess", target: go(e.target), fields: e.fields };
        case "Call":
            return { t: "Call", fn: go(e.fn), args: e.args.map(go) };
        case "Lambda":
            return { t: "Lambda", params: e.params, body: go(e.body) };
        case "Let":
            return { t: "Let", name: e.name, value: go(e.value), body: go(e.body) };
        case "ArrayLit":
            return { t: "ArrayLit", elements: e.elements.map(go) };
        case "ObjectLit":
            return { t: "ObjectLit", fields: e.fields.map(([k, v]) => [k, go(v)]) };
        case "Element":
            return {
                t: "Element",
                tag: e.tag,
                attributes: e.attributes.map((a) => eraseAttr(libs, prog, a)),
                value: go(e.value),
                children: e.children.map(go),
            };
        case "Fragment":
            return { t: "Fragment", children: e.children.map(go) };
        case "Branch":
            return { t: "Branch", condition: go(e.condition), then: go(e.then), else: go(e.else) };
        case "Map":
            return { t: "Map", collection: go(e.collection), fn: go(e.fn) };
        case "Filter":
            return { t: "Filter", collection: go(e.collection), fn: go(e.fn) };
        case "SortBy":
            return { t: "SortBy", descending: e.descending, collection: go(e.collection), fn: go(e.fn) };
        case "Scan":
            return { t: "Scan", collection: go(e.collection), initial: go(e.initial), fn: go(e.fn) };
        case "Fold":
            return { t: "Fold", collection: go(e.collection), initial: go(e.initial), fn: go(e.fn) };
        case "Concat":
            return { t: "Concat", left: go(e.left), right: go(e.right) };
        case "Import":
            return {
                t: "Import",
                name: e.name,
                params: e.params.map(([k, p]) => [k, p.t === "PExpr" ? { t: "PExpr", expr: go(p.expr) } : p]),
            };
        case "AdaptActions":
            return {
                t: "AdaptActions",
                target: go(e.target),
                adaptation: e.adaptation,
                fn: e.fn === null ? null : go(e.fn),
            };
        case "Constrain":
            return { t: "Constrain", name: e.name, args: e.args.map(go) };
        case "Emit":
            return { t: "Emit", constraint: go(e.constraint), body: go(e.body) };
        case "Alloc":
            return { t: "Alloc", site: e.site, key: go(e.key) };
        case "TypeDecl":
            return { t: "TypeDecl", name: e.name, type: e.type, body: go(e.body) };
        case "TypeAnnotate": {
            const rt = requireClosed(resolveTypeExpr(libs, prog, e.type));
            const hasType = {
                t: "Constrain",
                name: "has-type",
                args: [
                    { t: "Path", root: e.name, fields: [] },
                    { t: "ObjectLit", fields: [["$type", { t: "StringLit", value: canonicalId(rt) }]] },
                ],
            };
            return {
                t: "Let",
                name: e.name,
                value: go(e.value),
                body: { t: "Emit", constraint: hasType, body: go(e.body) },
            };
        }
        case "TypeEmit":
            return go(e.body);
    }
}
function eraseAttr(libs, prog, a) {
    return a.t === "Attr"
        ? { t: "Attr", name: a.name, value: eraseExpr(libs, prog, a.value) }
        : { t: "ActionAttr", event: a.event, key: a.key, payload: eraseExpr(libs, prog, a.payload) };
}
function resolveConstraintArg(libs, prog, a) {
    return a.t === "Type" ? { t: "Type", type: resolveTypeExpr(libs, prog, a.type) } : a;
}
function constraintArgEqual(a, b) {
    if (a.t !== b.t)
        return false;
    if (a.t === "Type")
        return resolvedTypeEqual(a.type, b.type);
    if (a.t === "ScalarNull")
        return true;
    return a.value === b.value;
}
function collectTypeEmits(e) {
    const own = e.t === "TypeEmit" ? [[e.name, e.args]] : [];
    return [...own, ...subExprs(e).flatMap(collectTypeEmits)];
}
function dedupeFirst(xs) {
    const out = [];
    for (const x of xs) {
        const seen = out.some(([n, args]) => n === x[0] &&
            args.length === x[1].length &&
            args.every((a, i) => constraintArgEqual(a, x[1][i])));
        if (!seen)
            out.push(x);
    }
    return out;
}
/** Every `!type-constraint` this program's own chain collects (`v4-types.md` §5). */
export function typeConstraints(libs, prog) {
    return dedupeFirst(collectTypeEmits(prog.root).map(([name, args]) => [name, args.map((a) => resolveConstraintArg(libs, prog, a))]));
}
/** `typeConstraints`, over-approximated by following every transitively imported library. */
export function deepTypeConstraints(libs, prog) {
    const out = typeConstraints(libs, prog);
    for (const name of transitiveImportNames(libs, prog)) {
        const p = libs.get(name);
        if (p !== undefined)
            out.push(...typeConstraints(libs, p));
    }
    return dedupeFirst(out);
}
// Type references (v4-types §9) --------------------------------------------
/** Every `TypeExpr` in a type-bearing position of `prog`'s own syntax, resolved. */
export function programTypeRoots(libs, prog) {
    const everywhere = (e) => [
        ...typeExprsIn(e),
        ...subExprs(e).flatMap(everywhere),
    ];
    return everywhere(prog.root).map((t) => resolveTypeExpr(libs, prog, t));
}
/** Every type this program's own statement chain refers to, as canonical ids. */
export function typeReferences(libs, prog) {
    return [...new Set(programTypeRoots(libs, prog).map(canonicalId))].sort(compareStrings);
}
/** `typeReferences`, following every transitively imported library. */
export function deepTypeReferences(libs, prog) {
    const out = new Set(typeReferences(libs, prog));
    for (const name of transitiveImportNames(libs, prog)) {
        const p = libs.get(name);
        if (p !== undefined)
            for (const id of typeReferences(libs, p))
                out.add(id);
    }
    return [...out].sort(compareStrings);
}
