/**
 * Static analysis over the AST alone (`specs/reference.md` §9,
 * `specs/v3-symbols.md` §7, `specs/v4-types.md` §9). Answers from the AST
 * alone: no context, no library evaluation, no host code.
 *
 * Every set-valued answer is returned as a sorted array, so two runs over the
 * same program agree on order as well as membership.
 */
import { adaptKey, subExprs, typeDecls, unlets, } from "./ast.js";
import { compareStrings } from "./json.js";
function sortedStrings(xs) {
    return [...new Set(xs)].sort(compareStrings);
}
function pathKey(path) {
    return JSON.stringify(path);
}
function sortedPaths(xs) {
    const seen = new Map();
    for (const p of xs)
        seen.set(pathKey(p), p);
    return [...seen.entries()].sort((a, b) => compareStrings(a[0], b[0])).map(([, p]) => p);
}
/** Collects from an expression and every expression inside it. */
function everywhere(f, e) {
    return [...f(e), ...subExprs(e).flatMap((sub) => everywhere(f, sub))];
}
// Imports ---------------------------------------------------------------------
/** Every library this program imports directly. */
export function staticImportNames(prog) {
    return sortedStrings(everywhere((e) => (e.t === "Import" ? [e.name] : []), prog.root));
}
/**
 * Every library reachable from this program, directly or through another
 * import. A missing name is still reported; a cycle terminates.
 */
export function transitiveImportNames(libs, prog) {
    const seen = new Set();
    const frontier = [...staticImportNames(prog)];
    for (;;) {
        const name = frontier.pop();
        if (name === undefined)
            break;
        if (seen.has(name))
            continue;
        seen.add(name);
        const p = libs.get(name);
        if (p === undefined)
            continue;
        for (const next of staticImportNames(p))
            if (!seen.has(next))
                frontier.push(next);
    }
    return sortedStrings(seen);
}
// Actions -----------------------------------------------------------------------
function ownKeysOf(e) {
    return e.t === "Element"
        ? e.attributes.flatMap((a) => (a.t === "ActionAttr" ? [a.key] : []))
        : [];
}
function actionKeysIn(e) {
    if (e.t === "AdaptActions") {
        const out = actionKeysIn(e.target).map((k) => adaptKey(e.adaptation, k));
        return e.fn === null ? out : [...out, ...actionKeysIn(e.fn)];
    }
    return [...ownKeysOf(e), ...subExprs(e).flatMap(actionKeysIn)];
}
/** Every action key this program can emit from its own AST; adaptation is applied, not ignored. */
export function staticActionKeys(prog) {
    return sortedStrings(actionKeysIn(prog.root));
}
/**
 * Every action key this program can emit, following imports. An
 * over-approximation: keys under a `Branch` arm that will never be selected
 * are still reported.
 */
export function deepActionKeys(libs, prog) {
    const go = (seen, e) => {
        if (e.t === "AdaptActions") {
            const out = go(seen, e.target).map((k) => adaptKey(e.adaptation, k));
            return e.fn === null ? out : [...out, ...go(seen, e.fn)];
        }
        if (e.t === "Import") {
            const out = e.params.flatMap(([, p]) => (p.t === "PExpr" ? go(seen, p.expr) : []));
            if (!seen.has(e.name)) {
                seen.add(e.name);
                const p = libs.get(e.name);
                if (p !== undefined)
                    out.push(...go(seen, p.root));
            }
            return out;
        }
        return [...ownKeysOf(e), ...subExprs(e).flatMap((sub) => go(seen, sub))];
    };
    return sortedStrings(go(new Set(), prog.root));
}
// Context holes ---------------------------------------------------------------------
function ownContextHoles(e) {
    return e.t === "Import"
        ? e.params.flatMap(([, p]) => (p.t === "PFromContext" ? [p.path] : []))
        : [];
}
/** Every path this program declares as a hole with `ctx(path)`. */
export function contextHoles(prog) {
    return sortedPaths(everywhere(ownContextHoles, prog.root));
}
/** Context holes of this program and of every library it imports. */
export function deepContextHoles(libs, prog) {
    const out = [...contextHoles(prog)];
    for (const name of transitiveImportNames(libs, prog)) {
        const p = libs.get(name);
        if (p !== undefined)
            out.push(...contextHoles(p));
    }
    return sortedPaths(out);
}
/** Every path this program reads out of its own context, however written. */
export function contextReads(prog) {
    return sortedPaths(everywhere((e) => (e.t === "Path" && e.root === "ctx" ? [e.fields] : ownContextHoles(e)), prog.root));
}
/**
 * For each import in this program, the paths its library reads from its context
 * that the import does not supply. Attributed by the read's first segment.
 */
export function unsuppliedParams(libs, prog) {
    const out = [];
    const collect = (e) => {
        if (e.t === "Import") {
            const supplied = new Set(e.params.map(([k]) => k));
            const p = libs.get(e.name);
            const reads = p === undefined ? [] : contextReads(p);
            out.push([
                e.name,
                reads.filter((path) => path.length > 0 && !supplied.has(path[0])),
            ]);
        }
        for (const sub of subExprs(e))
            collect(sub);
    };
    collect(prog.root);
    return out;
}
// Constraints -------------------------------------------------------------------
/** Every constraint name this program can emit from its own AST (`v3-symbols.md` §7). */
export function constraintKinds(prog) {
    return sortedStrings(everywhere((e) => (e.t === "Constrain" ? [e.name] : []), prog.root));
}
/** Every constraint name this program can emit, following imports. Over-approximates. */
export function deepConstraintKinds(libs, prog) {
    const go = (seen, e) => {
        if (e.t === "Import") {
            const out = e.params.flatMap(([, p]) => (p.t === "PExpr" ? go(seen, p.expr) : []));
            if (!seen.has(e.name)) {
                seen.add(e.name);
                const p = libs.get(e.name);
                if (p !== undefined)
                    out.push(...go(seen, p.root));
            }
            return out;
        }
        const own = e.t === "Constrain" ? [e.name] : [];
        return [...own, ...subExprs(e).flatMap((sub) => go(seen, sub))];
    };
    return sortedStrings(go(new Set(), prog.root));
}
// Symbols -------------------------------------------------------------------
/**
 * The `?(k)` allocation sites this program contains (`v3-symbols.md` §7) —
 * sites, not keys. Also the static counterpart of `AllocationInLibrary` (§6):
 * a program with a non-empty `symbolSites` cannot serve as a library.
 */
export function symbolSites(prog) {
    const sites = everywhere((e) => (e.t === "Alloc" ? [e.site] : []), prog.root);
    return [...new Set(sites)].sort((a, b) => a - b);
}
/** Every context path this program declares symbolic with `?ctx.…` (§7), directly. */
export function symbolDemands(prog) {
    return sortedPaths(everywhere((e) => (e.t === "Demand" ? [e.path] : []), prog.root));
}
/** Demands bubbled up through every library this program imports. */
export function deepSymbolDemands(libs, prog) {
    const out = [...symbolDemands(prog)];
    for (const name of transitiveImportNames(libs, prog)) {
        const p = libs.get(name);
        if (p !== undefined)
            out.push(...symbolDemands(p));
    }
    return sortedPaths(out);
}
// Arithmetic --------------------------------------------------------------------
/**
 * The ten names of the arithmetic profile (`specs/reference.md` §11), `round`
 * being the tenth (`specs/decisions.md` §20). They are names and not syntax: an evaluation with the profile on binds them in the initial
 * environment, and a program may bind any of them itself.
 */
export const arithmeticNames = [
    "sum",
    "product",
    "negate",
    "inverse",
    "quotient",
    "floor-quotient",
    "modulo",
    "floor",
    "real",
    "round",
];
/**
 * Which of `arithmeticNames` this program references free
 * (`specs/reference.md` §9), called (`sum(1, 2)`) or passed by reference
 * (`fold($xs, 0, $sum)`) alike, since both are a `Path` rooted at the name.
 *
 * Scope-aware: a name bound by a binding, a lambda parameter or a pattern
 * name is not reported where that binding is in scope. A pattern is lowered
 * to plain bindings by the parser, so it needs no case here. A binding's own
 * right-hand side is outside its scope, so `@sum = sum(1, 2)` reports `sum`.
 *
 * Over-approximates like every analysis here: a name used only under a
 * `Branch` arm no context will select is still reported.
 */
export function arithmeticOps(prog) {
    const go = (bound, e) => {
        switch (e.t) {
            case "Path":
                return arithmeticNames.includes(e.root) && !bound.has(e.root) ? [e.root] : [];
            case "Let":
            case "TypeAnnotate":
                return [...go(bound, e.value), ...go(new Set([...bound, e.name]), e.body)];
            case "Lambda":
                return go(new Set([...bound, ...e.params]), e.body);
            default:
                return subExprs(e).flatMap((sub) => go(bound, sub));
        }
    };
    return sortedStrings(go(new Set(), prog.root));
}
/**
 * `arithmeticOps` of this program and of every library it imports, directly
 * or not. A library has its own scope, so a binding in the importing program
 * shadows nothing there. This is what a host that leaves the arithmetic
 * profile off checks before running a program: non-empty means the program
 * would fail with `UnboundName`.
 */
export function deepArithmeticOps(libs, prog) {
    const out = [...arithmeticOps(prog)];
    for (const name of transitiveImportNames(libs, prog)) {
        const p = libs.get(name);
        if (p !== undefined)
            out.push(...arithmeticOps(p));
    }
    return sortedStrings(out);
}
// Types -----------------------------------------------------------------------
/** Every name this program declares with `type ... = ...` (`v4-types.md` §9). */
export function typeDeclarations(prog) {
    return sortedStrings(typeDecls(unlets(prog.root).statements).map(([n]) => n));
}
/**
 * The `%ctx.*` paths one `TypeExpr` mentions directly — `Var` is a leaf, the
 * way `types.resolveTypeExpr` must treat it: a reference's own parameters are
 * not its referent's.
 */
function typeParamsIn(t) {
    switch (t.t) {
        case "Array":
            return typeParamsIn(t.element);
        case "Record":
            return t.fields.flatMap(([, ft]) => typeParamsIn(ft));
        case "Union":
            return t.arms.flatMap(([, at]) => (at === null ? [] : typeParamsIn(at)));
        case "Var":
            return [t.path];
        case "Prim":
        case "Name":
        case "LibRef":
            return [];
    }
}
/**
 * Every `TypeExpr` sitting in one expression's own syntax, not recursing into
 * subexpressions — a declaration's body, an annotation's type, a type
 * constraint's type-marked arguments, and an import parameter's `%`-marked
 * value are the four positions a `TypeExpr` can occur in at all.
 */
export function typeExprsIn(e) {
    switch (e.t) {
        case "TypeDecl":
        case "TypeAnnotate":
            return [e.type];
        case "TypeEmit":
            return e.args.flatMap((a) => (a.t === "Type" ? [a.type] : []));
        case "Import":
            return e.params.flatMap(([, p]) => (p.t === "PType" ? [p.type] : []));
        default:
            return [];
    }
}
/**
 * Every `%ctx.*` path this program mentions in a type-bearing position
 * (`v4-types.md` §9) — its type-level parameter list, the way `contextReads` is
 * for values.
 */
export function typeParams(prog) {
    return sortedPaths(everywhere((e) => typeExprsIn(e).flatMap(typeParamsIn), prog.root));
}
/**
 * For each import in this program, the type params its library needs that the
 * import's `%`-marked entries do not supply.
 */
export function unsuppliedTypeParams(libs, prog) {
    const out = [];
    const go = (e) => {
        if (e.t === "Import") {
            const supplied = new Set(e.params.flatMap(([k, p]) => (p.t === "PType" ? [k] : [])));
            const p = libs.get(e.name);
            const wanted = p === undefined ? [] : typeParams(p);
            out.push([
                e.name,
                wanted.filter((path) => path.length > 0 && !supplied.has(path[0])),
            ]);
        }
        for (const sub of subExprs(e))
            go(sub);
    };
    go(prog.root);
    return out;
}
/**
 * Every params key read both as a value context path (`$ctx.k`/`ctx(k)`) and as
 * a type hole (`%ctx.k`) — `v4-types.md` §10's `TypeParamCollision`, keyed by
 * first segment.
 */
export function typeParamCollisions(prog) {
    const reads = new Set(contextReads(prog).flatMap((p) => (p.length > 0 ? [p[0]] : [])));
    const tparams = typeParams(prog).flatMap((p) => (p.length > 0 ? [p[0]] : []));
    return sortedStrings(tparams.filter((k) => reads.has(k)));
}
export function programKind(prog) {
    return prog.kind === "document" ? "document" : "value";
}
/**
 * A one-glance summary of a program's static interface, composed from the
 * primitives above rather than adding a new AST walk. `requires` is
 * deliberately the shallow, root-only `contextReads`: a card describes what
 * running *this* program needs from its own caller.
 */
export function programCard(libs, prog) {
    return {
        produces: programKind(prog),
        requires: contextReads(prog),
        imports: transitiveImportNames(libs, prog),
        emits: deepActionKeys(libs, prog),
        unsupplied: unsuppliedParams(libs, prog),
    };
}
