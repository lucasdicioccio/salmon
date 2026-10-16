/**
 * Hand-written recursive-descent lexer + parser for the surface syntax
 * (`specs/reference.md` §5, §8; `specs/v3-symbols.md` §1-2; `specs/v4-types.md`
 * §1, §5, §7). Mirrors `tramaj-rs/src/parser.rs`'s grammar, including its
 * "name recognition backtracks; the shape does not" rule for the special forms
 * and the static-position (no-interpolation) restrictions.
 */
import { documentProgram, expressionProgram, numberAllocs, } from "./ast.js";
export class ParseError extends Error {
    name = "ParseError";
}
const SPECIAL_FORM_NAMES = new Set([
    "map",
    "filter",
    "scan",
    "fold",
    "sort-by",
    "sort-by-descending",
    "branch",
    "import",
    "adapt-actions",
    "constraint",
]);
/** The five value-domain shapes `v4-types.md` §1 reserves as type primitives. */
const PRIM_NAMES = new Set(["string", "int", "float", "bool", "null", "document"]);
const ALPHABETIC = /\p{Alphabetic}/u;
const ALPHANUMERIC = /[\p{Alphabetic}\p{N}]/u;
const WHITESPACE = /\p{White_Space}/u;
const ASCII_DIGIT = /[0-9]/;
const ASCII_HEXDIGIT = /[0-9a-fA-F]/;
const FAIL = Symbol("parse-fail");
class P {
    chars;
    pos = 0;
    constructor(src) {
        this.chars = Array.from(src);
    }
    err(msg) {
        return new ParseError(`at char ${this.pos}: ${msg}`);
    }
    eof() {
        return this.pos >= this.chars.length;
    }
    peek() {
        return this.chars[this.pos];
    }
    peekAt(off) {
        return this.chars[this.pos + off];
    }
    advance() {
        const c = this.peek();
        if (c === undefined)
            return undefined;
        this.pos += 1;
        return c;
    }
    /** Full save/restore backtracking around one alternative. */
    attempt(f) {
        const save = this.pos;
        try {
            return f();
        }
        catch (e) {
            if (e instanceof ParseError) {
                this.pos = save;
                return FAIL;
            }
            throw e;
        }
    }
    succeeded(v) {
        return v !== FAIL;
    }
    // Lexing ---------------------------------------------------------------
    skipSpaces() {
        for (;;) {
            const c = this.peek();
            if (c !== undefined && WHITESPACE.test(c)) {
                this.advance();
            }
            else if (c === "-" && this.peekAt(1) === "-") {
                while (this.peek() !== undefined && this.peek() !== "\n")
                    this.advance();
            }
            else {
                return;
            }
        }
    }
    lexeme(f) {
        const v = f();
        this.skipSpaces();
        return v;
    }
    literal(s) {
        const save = this.pos;
        for (const expected of s) {
            if (this.advance() !== expected) {
                this.pos = save;
                throw this.err(`expected ${JSON.stringify(s)}`);
            }
        }
    }
    symbol(s) {
        this.lexeme(() => this.literal(s));
    }
    charLit(c) {
        if (this.peek() === c) {
            this.advance();
            return c;
        }
        throw this.err(`expected ${JSON.stringify(c)}`);
    }
    rawIdent() {
        const c0 = this.peek();
        if (c0 === undefined || !ALPHABETIC.test(c0))
            throw this.err("expected an identifier");
        this.advance();
        let s = c0;
        for (;;) {
            const c = this.peek();
            if (c !== undefined && (ALPHANUMERIC.test(c) || c === "_")) {
                s += c;
                this.advance();
            }
            else if (c === "-") {
                const next = this.peekAt(1);
                if (next !== undefined && (ALPHANUMERIC.test(next) || next === "_")) {
                    s += "-";
                    this.advance();
                }
                else {
                    return s;
                }
            }
            else {
                return s;
            }
        }
    }
    identifier() {
        return this.lexeme(() => this.rawIdent());
    }
    pathTail() {
        const root = this.rawIdent();
        return [root, this.dottedPathTail()];
    }
    dottedPathTail() {
        const segs = [];
        for (;;) {
            const save = this.pos;
            if (this.peek() !== ".")
                return segs;
            this.advance();
            const seg = this.attempt(() => this.rawIdent());
            if (!this.succeeded(seg)) {
                this.pos = save;
                return segs;
            }
            segs.push(seg);
        }
    }
    fieldAccessSuffix() {
        return this.dottedPathTail();
    }
    static applyFieldAccess(base, segs) {
        return segs.length === 0 ? base : { t: "FieldAccess", target: base, fields: segs };
    }
    // Static strings ---------------------------------------------------------
    staticString() {
        return this.lexeme(() => {
            this.charLit('"');
            let s = "";
            for (;;) {
                const c = this.peek();
                if (c === undefined || c === '"' || c === "`")
                    break;
                s += c;
                this.advance();
            }
            if (this.peek() !== '"') {
                throw this.err("this position must be a literal string, so it cannot contain an interpolation");
            }
            this.advance();
            return s;
        });
    }
    // String literals ---------------------------------------------------------
    stringLit() {
        return this.lexeme(() => {
            this.charLit('"');
            const parts = [];
            for (;;) {
                const c = this.peek();
                if (c === undefined || c === '"')
                    break;
                if (c === "`") {
                    this.advance();
                    const e = this.expr();
                    this.charLit("`");
                    parts.push({ t: "interp", value: e });
                }
                else {
                    parts.push({ t: "lit", value: this.litChunk() });
                }
            }
            this.charLit('"');
            return desugarString(parts);
        });
    }
    litChunk() {
        let s = "";
        for (;;) {
            const c = this.peek();
            if (c === "\\") {
                this.advance();
                s += this.escapeSeq();
            }
            else if (c !== undefined && c !== '"' && c !== "`") {
                s += c;
                this.advance();
            }
            else {
                break;
            }
        }
        if (s === "")
            throw this.err("expected string content");
        return s;
    }
    escapeSeq() {
        const c = this.advance();
        if (c === undefined)
            throw this.err("unterminated escape sequence");
        switch (c) {
            case "n":
                return "\n";
            case "t":
                return "\t";
            case "r":
                return "\r";
            case "\\":
                return "\\";
            case '"':
                return '"';
            case "`":
                return "`";
            case "0":
                return "\0";
            case "u": {
                this.charLit("{");
                let digits = "";
                for (;;) {
                    const d = this.peek();
                    if (d !== undefined && ASCII_HEXDIGIT.test(d)) {
                        digits += d;
                        this.advance();
                    }
                    else {
                        break;
                    }
                }
                if (digits === "")
                    throw this.err("invalid unicode escape");
                this.charLit("}");
                const n = Number.parseInt(digits, 16);
                // Rust's `char::from_u32` rejects surrogates as well as out-of-range
                // code points, and a lone surrogate must not parse here either.
                if (!Number.isFinite(n) || n > 0x10ffff || (n >= 0xd800 && n <= 0xdfff)) {
                    throw this.err("invalid unicode escape");
                }
                return String.fromCodePoint(n);
            }
            default:
                throw this.err(`unknown escape sequence: \\${c}`);
        }
    }
    // Literals ------------------------------------------------------------
    /**
     * `["-"] digits ["." digits] [("e"|"E") ["+"|"-"] digits]`, where a `_` may
     * sit between two digits (reference.md §5, *Number literals*). The fraction
     * and exponent are taken only when complete, so a stray `.`, `e` or `_` is
     * left for the caller to reject. An overflow to infinity is refused and a
     * zero is normalized so `-0` never escapes.
     */
    numberLit() {
        return this.lexeme(() => {
            let full = "";
            if (this.peek() === "-") {
                this.advance();
                full = "-";
            }
            const intPart = this.digits();
            if (intPart === "")
                throw this.err("expected a digit");
            full += intPart;
            let save = this.pos;
            if (this.peek() === ".") {
                this.advance();
                const frac = this.digits();
                if (frac === "")
                    this.pos = save;
                else
                    full += `.${frac}`;
            }
            save = this.pos;
            const e = this.peek();
            if (e === "e" || e === "E") {
                this.advance();
                let exp = "e";
                const sign = this.peek();
                if (sign === "+" || sign === "-") {
                    this.advance();
                    exp += sign;
                }
                const expDigits = this.digits();
                if (expDigits === "")
                    this.pos = save;
                else
                    full += exp + expDigits;
            }
            const n = Number(full);
            if (Number.isNaN(n))
                throw this.err("invalid number literal");
            // A fraction or an exponent makes a float; neither, an integer
            // (reference.md §5). `full` holds a `.` or an `e` only in the first case.
            if (/[.e]/.test(full)) {
                if (!Number.isFinite(n))
                    throw this.err("float literal out of range");
                return { t: "FloatLit", value: n === 0 ? 0 : n };
            }
            if (!Number.isSafeInteger(n)) {
                throw this.err("integer literal outside the integer range, -(2^53 - 1) to 2^53 - 1");
            }
            return { t: "IntLit", value: n === 0 ? 0 : n };
        });
    }
    /** `digit { ["_"] digit }` with the underscores dropped; `""` (consuming nothing) when no digit is next. */
    digits() {
        let out = "";
        const first = this.peek();
        if (first === undefined || !ASCII_DIGIT.test(first))
            return out;
        for (;;) {
            const c = this.peek();
            if (c !== undefined && ASCII_DIGIT.test(c)) {
                out += c;
                this.advance();
                continue;
            }
            const next = this.peekAt(1);
            if (c === "_" && next !== undefined && ASCII_DIGIT.test(next)) {
                this.advance();
                continue;
            }
            return out;
        }
    }
    keywordLit() {
        const name = this.identifier();
        switch (name) {
            case "true":
                return { t: "BoolLit", value: true };
            case "false":
                return { t: "BoolLit", value: false };
            case "null":
                return { t: "NullLit" };
            default:
                throw this.err("not a literal keyword");
        }
    }
    arrayLit() {
        this.symbol("[");
        const elements = this.sepEndBy(",", () => this.expr());
        this.symbol("]");
        return { t: "ArrayLit", elements };
    }
    objectLit() {
        this.symbol("{");
        const fields = this.sepEndBy(",", () => this.objEntry());
        this.symbol("}");
        return { t: "ObjectLit", fields };
    }
    objEntry() {
        const explicit = this.attempt(() => this.explicitEntry());
        if (this.succeeded(explicit))
            return explicit;
        const k = this.identifier();
        return [k, { t: "Path", root: k, fields: [] }];
    }
    explicitEntry() {
        const k = this.objectKey();
        this.refuseReservedKey(k);
        this.symbol(":");
        return [k, this.expr()];
    }
    refuseReservedKey(k) {
        if (k === "$sym" || k === "$type" || k === "$term") {
            throw this.err(`"${k}" is a reserved key and cannot be used as an object key`);
        }
    }
    objectKey() {
        const s = this.attempt(() => this.staticString());
        return this.succeeded(s) ? s : this.identifier();
    }
    // Sep-end-by helper -----------------------------------------------------
    sepEndBy(sep, item) {
        const out = [];
        const first = this.attempt(item);
        if (!this.succeeded(first))
            return out;
        out.push(first);
        for (;;) {
            const save = this.pos;
            const sepOk = this.attempt(() => this.symbol(sep));
            if (!this.succeeded(sepOk)) {
                this.pos = save;
                return out;
            }
            const next = this.attempt(item);
            if (!this.succeeded(next)) {
                this.pos = save;
                return out;
            }
            out.push(next);
        }
    }
    // Expressions -----------------------------------------------------------
    pathExpr() {
        return this.lexeme(() => {
            this.charLit("$");
            const [root, fields] = this.pathTail();
            return { t: "Path", root, fields };
        });
    }
    /** `?(key)` (`v3-symbols.md` §1.2); `numberAllocs` assigns the real site later. */
    allocExpr() {
        this.charLit("?");
        this.symbol("(");
        const key = this.expr();
        this.charLit(")");
        const segs = this.fieldAccessSuffix();
        this.skipSpaces();
        return P.applyFieldAccess({ t: "Alloc", site: 0, key }, segs);
    }
    /** `?ctx.a.b` (`v3-symbols.md` §1.3): the path MUST be rooted at `ctx`. */
    demandExpr() {
        return this.lexeme(() => {
            this.charLit("?");
            const root = this.rawIdent();
            if (root !== "ctx")
                throw this.err("a demand must be rooted at ctx, as in ?ctx.path");
            return { t: "Demand", path: this.dottedPathTail() };
        });
    }
    callExpr() {
        this.attempt(() => this.charLit("$"));
        const [root, fields] = this.pathTail();
        this.symbol("(");
        const args = this.sepEndBy(",", () => this.expr());
        this.charLit(")");
        const segs = this.fieldAccessSuffix();
        this.skipSpaces();
        return P.applyFieldAccess({ t: "Call", fn: { t: "Path", root, fields }, args }, segs);
    }
    lambdaExpr() {
        this.symbol("(");
        const params = this.sepEndBy(",", () => this.pattern());
        this.symbol(")");
        this.symbol("=>");
        return lowerLambda(params, this.expr());
    }
    // Binding patterns (decisions §17) ------------------------------------------
    /**
     * A name, or an object pattern `{a, b: c, d: {e}}`. Object patterns only;
     * defaults (`{a = 1}`), rest (`{...r}`) and array patterns are not in the
     * grammar, so they are parse errors.
     */
    pattern() {
        const obj = this.attempt(() => this.objectPattern());
        return this.succeeded(obj) ? obj : { t: "PName", name: this.identifier() };
    }
    objectPattern() {
        this.symbol("{");
        const fields = this.sepEndBy(",", () => this.patternField());
        if (fields.length === 0)
            throw this.err("expected a field in a binding pattern");
        this.symbol("}");
        const pat = { t: "PObject", fields };
        const names = patternNames(pat);
        if (new Set(names).size !== names.length) {
            throw this.err("duplicate name in a binding pattern");
        }
        return pat;
    }
    /** `name` binds the field to itself; `name: p` binds or destructures it as `p`. */
    patternField() {
        const k = this.identifier();
        const sub = this.attempt(() => {
            this.symbol(":");
            return this.pattern();
        });
        return [k, this.succeeded(sub) ? sub : { t: "PName", name: k }];
    }
    parenExpr() {
        this.symbol("(");
        const e = this.expr();
        this.symbol(")");
        return e;
    }
    /**
     * Soft recognition only: consumes the name (and trailing whitespace) iff it
     * is one of the recognized special forms; otherwise fully restores.
     */
    trySpecialFormName() {
        const save = this.pos;
        this.attempt(() => this.charLit("$"));
        const n = this.attempt(() => this.identifier());
        if (this.succeeded(n) && SPECIAL_FORM_NAMES.has(n))
            return n;
        this.pos = save;
        return null;
    }
    /**
     * Once the name is recognized the shape does not backtrack: any error here
     * is a hard parse error (`reference.md` §5).
     */
    specialFormShape(name) {
        let base;
        switch (name) {
            case "map": {
                const [collection, fn] = this.binaryShape();
                base = { t: "Map", collection, fn };
                break;
            }
            case "filter": {
                const [collection, fn] = this.binaryShape();
                base = { t: "Filter", collection, fn };
                break;
            }
            case "scan": {
                const [collection, initial, fn] = this.ternaryShape();
                base = { t: "Scan", collection, initial, fn };
                break;
            }
            case "fold": {
                const [collection, initial, fn] = this.ternaryShape();
                base = { t: "Fold", collection, initial, fn };
                break;
            }
            case "sort-by":
            case "sort-by-descending": {
                const [collection, fn] = this.binaryShape();
                base = { t: "SortBy", descending: name === "sort-by-descending", collection, fn };
                break;
            }
            case "branch":
                base = this.branchShape();
                break;
            case "import":
                base = this.importShape();
                break;
            case "adapt-actions":
                base = this.adaptActionsShape();
                break;
            case "constraint":
                base = this.constraintShape();
                break;
            default:
                throw this.err("unrecognized special form");
        }
        const segs = this.fieldAccessSuffix();
        this.skipSpaces();
        return P.applyFieldAccess(base, segs);
    }
    binaryShape() {
        this.symbol("(");
        const a = this.expr();
        this.symbol(",");
        const b = this.expr();
        this.charLit(")");
        return [a, b];
    }
    ternaryShape() {
        this.symbol("(");
        const a = this.expr();
        this.symbol(",");
        const b = this.expr();
        this.symbol(",");
        const c = this.expr();
        this.charLit(")");
        return [a, b, c];
    }
    branchShape() {
        this.symbol("(");
        const fallback = this.expr();
        const arms = [];
        for (;;) {
            const save = this.pos;
            const sepOk = this.attempt(() => this.symbol(","));
            if (!this.succeeded(sepOk)) {
                this.pos = save;
                break;
            }
            const arm = this.attempt(() => this.branchArm());
            if (!this.succeeded(arm)) {
                this.pos = save;
                break;
            }
            arms.push(arm);
        }
        this.attempt(() => this.symbol(","));
        this.charLit(")");
        let acc = fallback;
        for (let i = arms.length - 1; i >= 0; i -= 1) {
            const [condition, then] = arms[i];
            acc = { t: "Branch", condition, then, else: acc };
        }
        return acc;
    }
    branchArm() {
        const p = this.expr();
        this.symbol(",");
        return [p, this.expr()];
    }
    importShape() {
        this.symbol("(");
        const name = this.staticString();
        this.symbol(",");
        const params = this.importParams();
        this.charLit(")");
        return { t: "Import", name, params };
    }
    importParams() {
        this.symbol("{");
        const entries = this.sepEndBy(",", () => this.paramEntry());
        this.symbol("}");
        return entries;
    }
    paramEntry() {
        const explicit = this.attempt(() => this.explicitParam());
        if (this.succeeded(explicit))
            return explicit;
        const k = this.identifier();
        return [k, { t: "PExpr", expr: { t: "Path", root: k, fields: [] } }];
    }
    explicitParam() {
        const k = this.objectKey();
        this.symbol(":");
        return [k, this.paramValue()];
    }
    paramValue() {
        const marked = this.attempt(() => this.markedTypeExpr());
        if (this.succeeded(marked))
            return { t: "PType", type: marked };
        const fromCtx = this.attempt(() => this.ctxParam());
        if (this.succeeded(fromCtx))
            return fromCtx;
        return { t: "PExpr", expr: this.expr() };
    }
    // Types ---------------------------------------------------------------
    typeExpr() {
        const v = this.attempt(() => this.typeVar());
        if (this.succeeded(v))
            return v;
        const u = this.attempt(() => this.typeUnion());
        if (this.succeeded(u))
            return u;
        const a = this.attempt(() => this.typeArray());
        if (this.succeeded(a))
            return a;
        const r = this.attempt(() => this.typeRecord());
        if (this.succeeded(r))
            return r;
        return this.typePrimOrRef();
    }
    /**
     * Deliberately narrower than `typeExpr`: a bare name is excluded because
     * nothing marks where a nullary arm ends (`| Dev | Staging` is two arms).
     */
    typeExprPayload() {
        const v = this.attempt(() => this.typeVar());
        if (this.succeeded(v))
            return v;
        const a = this.attempt(() => this.typeArray());
        if (this.succeeded(a))
            return a;
        return this.typeRecord();
    }
    typeVar() {
        return this.lexeme(() => {
            this.charLit("%");
            const root = this.rawIdent();
            if (root !== "ctx")
                throw this.err("a type hole must be rooted at ctx, as in %ctx.path");
            return { t: "Var", path: this.dottedPathTail() };
        });
    }
    /**
     * A `%`-marked type argument (`v4-types.md` §2, §5). Unlike `typeVar` the
     * leading `%` does not require what follows to be `ctx`.
     */
    markedTypeExpr() {
        this.charLit("%");
        const f = this.attempt(() => this.ctxForward());
        if (this.succeeded(f))
            return f;
        const u = this.attempt(() => this.typeUnion());
        if (this.succeeded(u))
            return u;
        const a = this.attempt(() => this.typeArray());
        if (this.succeeded(a))
            return a;
        const r = this.attempt(() => this.typeRecord());
        if (this.succeeded(r))
            return r;
        return this.typePrimOrRef();
    }
    ctxForward() {
        return this.lexeme(() => {
            const root = this.rawIdent();
            if (root !== "ctx")
                throw this.err("a %-marked value must be ctx.path or a type expression");
            return { t: "Var", path: this.dottedPathTail() };
        });
    }
    typeArray() {
        this.symbol("[");
        const element = this.typeExpr();
        this.symbol("]");
        return { t: "Array", element };
    }
    typeRecord() {
        this.symbol("{");
        const fields = this.sepEndBy(",", () => this.typeField());
        this.symbol("}");
        return { t: "Record", fields };
    }
    /**
     * A field name here is always a bare identifier, never a quoted string —
     * what keeps `types.canonicalId` injective, since the id grammar's own
     * delimiters can never appear in one.
     */
    typeField() {
        const k = this.identifier();
        this.symbol(":");
        return [k, this.typeExpr()];
    }
    typeUnion() {
        this.symbol("|");
        const arms = [this.unionArm()];
        for (;;) {
            const save = this.pos;
            const barOk = this.attempt(() => this.symbol("|"));
            if (!this.succeeded(barOk)) {
                this.pos = save;
                break;
            }
            arms.push(this.unionArm());
        }
        return { t: "Union", arms };
    }
    unionArm() {
        const name = this.identifier();
        const payload = this.attempt(() => this.typeExprPayload());
        return [name, this.succeeded(payload) ? payload : null];
    }
    typePrimOrRef() {
        const r = this.attempt(() => this.typeLibRef());
        if (this.succeeded(r))
            return r;
        return this.typeNameOrPrim();
    }
    typeLibRef() {
        return this.lexeme(() => {
            this.charLit("$");
            const lib = this.rawIdent();
            this.charLit(".");
            this.literal("types");
            this.charLit(".");
            const name = this.rawIdent();
            return { t: "LibRef", lib, name };
        });
    }
    typeNameOrPrim() {
        const name = this.identifier();
        return PRIM_NAMES.has(name) ? { t: "Prim", name } : { t: "Name", name };
    }
    typeConstraintArg() {
        const marked = this.attempt(() => this.markedTypeExpr());
        if (this.succeeded(marked))
            return { t: "Type", type: marked };
        return this.scalarArg();
    }
    scalarArg() {
        const s = this.attempt(() => this.staticString());
        if (this.succeeded(s))
            return { t: "ScalarStr", value: s };
        const n = this.attempt(() => this.numberLit());
        if (this.succeeded(n))
            return asScalarArg(n);
        const k = this.attempt(() => this.keywordLit());
        if (this.succeeded(k))
            return asScalarArg(k);
        throw this.err("expected a type-constraint argument");
    }
    /**
     * `!type-constraint(name, args...)` (`v4-types.md` §5): tried before the
     * general `!expr` emission, since both share the `!` leader.
     */
    typeEmission() {
        this.charLit("!");
        const kw = this.identifier();
        if (kw !== "type-constraint")
            throw this.err("not a !type-constraint");
        this.symbol("(");
        const name = this.staticString();
        const args = [];
        for (;;) {
            const save = this.pos;
            const sepOk = this.attempt(() => this.symbol(","));
            if (!this.succeeded(sepOk)) {
                this.pos = save;
                break;
            }
            const a = this.attempt(() => this.typeConstraintArg());
            if (!this.succeeded(a)) {
                this.pos = save;
                break;
            }
            args.push(a);
        }
        this.attempt(() => this.symbol(","));
        this.charLit(")");
        this.skipSpaces();
        return { t: "TypeEmit", name, args };
    }
    /** `type Name = TypeExpr` (`v4-types.md` §1.1): a keyword leader, not a character. */
    typeDeclStmt() {
        const kw = this.identifier();
        if (kw !== "type")
            throw this.err("not a type declaration");
        const name = this.identifier();
        this.symbol("=");
        return { t: "TypeDecl", name, type: this.typeExpr() };
    }
    ctxParam() {
        const save = this.pos;
        const n = this.identifier();
        if (n !== "ctx" || this.peek() !== "(") {
            this.pos = save;
            throw this.err("not a ctx(...) parameter");
        }
        this.symbol("(");
        const [root, fields] = this.pathTail();
        this.skipSpaces();
        this.charLit(")");
        this.skipSpaces();
        return { t: "PFromContext", path: [root, ...fields] };
    }
    /** `constraint(name, args...)` (`v3-symbols.md` §2.1): a static name, then any number of expressions. */
    constraintShape() {
        this.symbol("(");
        const name = this.staticString();
        const args = [];
        for (;;) {
            const save = this.pos;
            const sepOk = this.attempt(() => this.symbol(","));
            if (!this.succeeded(sepOk)) {
                this.pos = save;
                break;
            }
            const a = this.attempt(() => this.expr());
            if (!this.succeeded(a)) {
                this.pos = save;
                break;
            }
            args.push(a);
        }
        this.attempt(() => this.symbol(","));
        this.charLit(")");
        return { t: "Constrain", name, args };
    }
    adaptActionsShape() {
        this.symbol("(");
        const target = this.expr();
        this.symbol(",");
        const adaptation = this.adaptationShape();
        const fn = this.attempt(() => {
            this.symbol(",");
            return this.expr();
        });
        this.attempt(() => this.symbol(","));
        this.charLit(")");
        return { t: "AdaptActions", target, adaptation, fn: this.succeeded(fn) ? fn : null };
    }
    adaptationShape() {
        const name = this.identifier();
        if (name === "identity")
            return { t: "Identity" };
        if (name === "prefix") {
            this.symbol("(");
            const prefix = this.staticString();
            this.charLit(")");
            this.skipSpaces();
            return { t: "Prefix", prefix };
        }
        throw this.err('an action adaptation must be identity or prefix("...")');
    }
    // Documents ---------------------------------------------------------------
    documentExpr() {
        this.charLit(".");
        const frag = this.attempt(() => this.fragmentShape());
        if (this.succeeded(frag))
            return frag;
        return this.elementShape();
    }
    fragmentShape() {
        this.symbol("(");
        const children = this.sepEndBy(",", () => this.expr());
        this.symbol(")");
        return { t: "Fragment", children };
    }
    elementShape() {
        const tag = this.rawIdent();
        this.skipSpaces();
        this.symbol("(");
        const args = this.sepEndBy(",", () => this.elementArg());
        this.symbol(")");
        return this.buildElement(tag, args);
    }
    elementArg() {
        const positional = this.tryAttributePositionArg();
        if (positional !== null)
            return positional;
        const named = this.attempt(() => this.namedArg());
        if (this.succeeded(named))
            return { t: "attr", value: named };
        return { t: "child", value: this.expr() };
    }
    /** `action(...)`/`value(...)`: soft name+lookahead recognition, then a hard-committed shape. */
    tryAttributePositionArg() {
        const save = this.pos;
        const n = this.attempt(() => this.identifier());
        if (!this.succeeded(n)) {
            this.pos = save;
            return null;
        }
        if (this.peek() !== "(" || (n !== "action" && n !== "value")) {
            this.pos = save;
            return null;
        }
        return n === "action"
            ? { t: "attr", value: this.actionShape() }
            : { t: "value", value: this.valueShape() };
    }
    actionShape() {
        this.symbol("(");
        const event = this.staticString();
        this.symbol(",");
        const key = this.staticString();
        this.symbol(",");
        const payload = this.expr();
        this.symbol(")");
        return { t: "ActionAttr", event, key, payload };
    }
    valueShape() {
        this.symbol("(");
        const v = this.expr();
        this.symbol(")");
        return v;
    }
    namedArg() {
        const name = this.objectKey();
        this.symbol(":");
        return { t: "Attr", name, value: this.expr() };
    }
    buildElement(tag, args) {
        let seenChild = false;
        for (const arg of args) {
            if (arg.t === "child")
                seenChild = true;
            else if (seenChild) {
                throw this.err("attributes, action(...) and value(...) must all come before an element's children");
            }
        }
        const values = args.flatMap((a) => (a.t === "value" ? [a.value] : []));
        const attributes = args.flatMap((a) => (a.t === "attr" ? [a.value] : []));
        const children = args.flatMap((a) => (a.t === "child" ? [a.value] : []));
        if (values.length > 1)
            throw this.err("an element can have at most one value(...)");
        const value = values.length === 1 ? values[0] : { t: "NullLit" };
        return { t: "Element", tag, attributes, value, children };
    }
    // Precedence ---------------------------------------------------------------
    expr() {
        let acc = this.operand();
        for (;;) {
            const save = this.pos;
            const rhs = this.attempt(() => {
                this.symbol("<>");
                return this.operand();
            });
            if (!this.succeeded(rhs)) {
                this.pos = save;
                return acc;
            }
            acc = { t: "Concat", left: acc, right: rhs };
        }
    }
    operand() {
        const alternatives = [
            () => this.keywordLit(),
            () => this.lambdaExpr(),
            () => this.parenExpr(),
        ];
        for (const alt of alternatives) {
            const v = this.attempt(alt);
            if (this.succeeded(v))
                return v;
        }
        const special = this.trySpecialFormName();
        if (special !== null)
            return this.specialFormShape(special);
        const rest = [
            () => this.callExpr(),
            () => this.pathExpr(),
            () => this.allocExpr(),
            () => this.demandExpr(),
            () => this.documentExpr(),
            () => this.stringLit(),
            () => this.numberLit(),
            () => this.arrayLit(),
            () => this.objectLit(),
        ];
        for (const alt of rest) {
            const v = this.attempt(alt);
            if (this.succeeded(v))
                return v;
        }
        throw this.err("expected an expression");
    }
    // Programs -----------------------------------------------------------------
    /**
     * `@name=expr`, `@name : T = expr` (`v4-types.md` §7) or `@{pattern} = expr`
     * (decisions §17), which lowers to several plain bindings. A pattern takes no
     * annotation.
     */
    binding() {
        this.charLit("@");
        const pat = this.pattern();
        if (pat.t === "PObject") {
            this.symbol("=");
            return bindPattern(pat, this.expr());
        }
        const name = pat.name;
        const annot = this.attempt(() => {
            this.symbol(":");
            return this.typeExpr();
        });
        this.symbol("=");
        const value = this.expr();
        return [
            this.succeeded(annot) ? { t: "Annotate", name, type: annot, value } : { t: "Let", name, value },
        ];
    }
    /** `!expr` (`v3-symbols.md` §2.2): a statement position only. */
    emissionStmt() {
        this.charLit("!");
        return this.expr();
    }
    program() {
        this.skipSpaces();
        const statements = [];
        for (;;) {
            const b = this.attempt(() => this.binding());
            if (this.succeeded(b)) {
                statements.push(...b);
                continue;
            }
            const te = this.attempt(() => this.typeEmission());
            if (this.succeeded(te)) {
                statements.push(te);
                continue;
            }
            const em = this.attempt(() => this.emissionStmt());
            if (this.succeeded(em)) {
                statements.push({ t: "Emit", constraint: em });
                continue;
            }
            const td = this.attempt(() => this.typeDeclStmt());
            if (this.succeeded(td)) {
                statements.push(td);
                continue;
            }
            break;
        }
        const root = this.expr();
        this.skipSpaces();
        if (!this.eof())
            throw this.err("unexpected trailing input");
        const isDocument = root.t === "Element" || root.t === "Fragment";
        let body = root;
        for (let i = statements.length - 1; i >= 0; i -= 1) {
            const stmt = statements[i];
            switch (stmt.t) {
                case "Let":
                    body = { t: "Let", name: stmt.name, value: stmt.value, body };
                    break;
                case "Emit":
                    body = { t: "Emit", constraint: stmt.constraint, body };
                    break;
                case "TypeDecl":
                    body = { t: "TypeDecl", name: stmt.name, type: stmt.type, body };
                    break;
                case "Annotate":
                    body = { t: "TypeAnnotate", name: stmt.name, type: stmt.type, value: stmt.value, body };
                    break;
                case "TypeEmit":
                    body = { t: "TypeEmit", name: stmt.name, args: stmt.args, body };
                    break;
            }
        }
        numberAllocs(body);
        return isDocument ? documentProgram(body) : expressionProgram(body);
    }
}
function patternNames(p) {
    return p.t === "PName" ? [p.name] : p.fields.flatMap(([, sub]) => patternNames(sub));
}
/**
 * Lowers `pattern = source` to plain bindings, in written order. A source that
 * is a path is read directly (`$ctx.item.a`), so errors and static analyses see
 * the reads a hand-written program would make; anything else is bound once to
 * the hidden name `#src` first (`isHiddenName`).
 */
function bindPattern(p, source) {
    if (p.t === "PName")
        return [{ t: "Let", name: p.name, value: source }];
    const readFields = (src) => p.fields.flatMap(([k, sub]) => bindPattern(sub, readField(src, k)));
    if (source.t === "Path")
        return readFields(source);
    return [
        { t: "Let", name: "#src", value: source },
        ...readFields({ t: "Path", root: "#src", fields: [] }),
    ];
}
function readField(src, k) {
    return src.t === "Path"
        ? { t: "Path", root: src.root, fields: [...src.fields, k] }
        : { t: "FieldAccess", target: src, fields: [k] };
}
/**
 * A pattern parameter becomes a hidden parameter `#argN`; the body is wrapped in
 * the bindings that read the pattern's names out of it.
 */
function lowerLambda(params, body) {
    const lets = params.flatMap((p, i) => p.t === "PObject" ? bindPattern(p, { t: "Path", root: `#arg${i}`, fields: [] }) : []);
    let wrapped = body;
    for (let i = lets.length - 1; i >= 0; i -= 1) {
        const l = lets[i];
        wrapped = { t: "Let", name: l.name, value: l.value, body: wrapped };
    }
    return {
        t: "Lambda",
        params: params.map((p, i) => (p.t === "PName" ? p.name : `#arg${i}`)),
        body: wrapped,
    };
}
function asScalarArg(e) {
    switch (e.t) {
        case "IntLit":
            return { t: "ScalarInt", value: e.value };
        case "FloatLit":
            return { t: "ScalarFloat", value: e.value };
        case "BoolLit":
            return { t: "ScalarBool", value: e.value };
        case "StringLit":
            return { t: "ScalarStr", value: e.value };
        default:
            return { t: "ScalarNull" };
    }
}
function desugarString(parts) {
    const coalesced = coalesce(parts);
    const first = coalesced[0];
    if (first === undefined)
        return { t: "StringLit", value: "" };
    let acc = partExpr(first);
    for (let i = 1; i < coalesced.length; i += 1) {
        acc = { t: "Concat", left: acc, right: partExpr(coalesced[i]) };
    }
    return acc;
}
function partExpr(p) {
    return p.t === "lit"
        ? { t: "StringLit", value: p.value }
        : { t: "Call", fn: { t: "Path", root: "str", fields: [] }, args: [p.value] };
}
function coalesce(parts) {
    const out = [];
    for (const part of parts) {
        const last = out[out.length - 1];
        if (last !== undefined && last.t === "lit" && part.t === "lit") {
            out[out.length - 1] = { t: "lit", value: last.value + part.value };
        }
        else {
            out.push(part);
        }
    }
    return out;
}
export function parseProgram(src) {
    return new P(src).program();
}
/** `parseProgram`, returning the error rather than throwing it. */
export function tryParseProgram(src) {
    try {
        return { ok: true, program: parseProgram(src) };
    }
    catch (e) {
        if (e instanceof ParseError)
            return { ok: false, error: e };
        throw e;
    }
}
