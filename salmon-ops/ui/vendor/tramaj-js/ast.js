/**
 * Core AST (`specs/reference.md` §2, `specs/v3-symbols.md` §3,
 * `specs/v4-types.md` §1). Mirrors `tramaj-rs/src/ast.rs` and
 * `tramaj-hs/src/Tramaj/Ast.hs`.
 */
export function documentProgram(root) {
    return { kind: "document", root };
}
export function expressionProgram(root) {
    return { kind: "expression", root };
}
export function adaptKey(adaptation, key) {
    return adaptation.t === "Identity" ? key : adaptation.prefix + key;
}
function attributeExprs(attrs) {
    return attrs.map((a) => (a.t === "Attr" ? a.value : a.payload));
}
/** The immediately-contained expressions of an expression, in source order. */
export function subExprs(e) {
    switch (e.t) {
        case "Path":
        case "StringLit":
        case "IntLit":
        case "FloatLit":
        case "BoolLit":
        case "NullLit":
        case "Demand":
            return [];
        case "FieldAccess":
            return [e.target];
        case "Call":
            return [e.fn, ...e.args];
        case "Lambda":
            return [e.body];
        case "Let":
            return [e.value, e.body];
        case "ArrayLit":
            return e.elements;
        case "ObjectLit":
            return e.fields.map(([, v]) => v);
        case "Element":
            return [...attributeExprs(e.attributes), e.value, ...e.children];
        case "Fragment":
            return e.children;
        case "Branch":
            return [e.condition, e.then, e.else];
        case "Map":
        case "Filter":
        case "SortBy":
            return [e.collection, e.fn];
        case "Scan":
        case "Fold":
            return [e.collection, e.initial, e.fn];
        case "Concat":
            return [e.left, e.right];
        case "Import":
            return e.params.flatMap(([, p]) => (p.t === "PExpr" ? [p.expr] : []));
        case "AdaptActions":
            return e.fn === null ? [e.target] : [e.target, e.fn];
        case "Constrain":
            return e.args;
        case "Emit":
            return [e.constraint, e.body];
        case "Alloc":
            return [e.key];
        case "TypeDecl":
            return [e.body];
        case "TypeAnnotate":
            return [e.value, e.body];
        case "TypeEmit":
            return [e.body];
    }
}
/**
 * Assigns each `Alloc` the index of its `?(...)` among all of them, in source
 * order (`v3-symbols.md` §1.4): a plain pre-order, left-to-right walk over the
 * freshly parsed tree. Done as a rewrite after parsing so two independently
 * written parsers agree on site numbers by construction.
 */
export function numberAllocs(e) {
    let n = 0;
    const go = (x) => {
        if (x.t === "Alloc") {
            x.site = n;
            n += 1;
            go(x.key);
            return;
        }
        for (const sub of subExprs(x))
            go(sub);
    };
    go(e);
}
/**
 * Peels the outermost `Let`/`Emit`/`TypeDecl`/`TypeAnnotate`/`TypeEmit` chain
 * back off, stopping at the first non-statement constructor.
 */
export function unlets(e) {
    const statements = [];
    let cur = e;
    for (;;) {
        switch (cur.t) {
            case "Let":
                statements.push({ t: "Let", name: cur.name, value: cur.value });
                cur = cur.body;
                continue;
            case "Emit":
                statements.push({ t: "Emit", constraint: cur.constraint });
                cur = cur.body;
                continue;
            case "TypeDecl":
                statements.push({ t: "TypeDecl", name: cur.name, type: cur.type });
                cur = cur.body;
                continue;
            case "TypeAnnotate":
                statements.push({ t: "Annotate", name: cur.name, type: cur.type, value: cur.value });
                cur = cur.body;
                continue;
            case "TypeEmit":
                statements.push({ t: "TypeEmit", name: cur.name, args: cur.args });
                cur = cur.body;
                continue;
            default:
                return { statements, root: cur };
        }
    }
}
export function typeDecls(stmts) {
    return stmts.flatMap((s) => (s.t === "TypeDecl" ? [[s.name, s.type]] : []));
}
/**
 * A name the parser invents when lowering a binding pattern (decisions §17):
 * it starts with `#`, which no surface name can. Such a binding is a lowering
 * detail, so it is never reported as a symbol's `"binding"` and never exposed
 * through a library's `.vals`.
 */
export function isHiddenName(n) {
    return n.startsWith("#");
}
/**
 * Just the named bindings of a statement block, in order — what an import
 * exposes as `.vals`. An annotated binding still binds its name.
 */
export function letBindings(stmts) {
    return stmts.flatMap((s) => s.t === "Let" || s.t === "Annotate" ? [[s.name, s.value]] : []);
}
