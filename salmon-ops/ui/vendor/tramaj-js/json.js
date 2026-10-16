/**
 * JSON as Tramaj reads and writes it (`specs/node-json.md`): like any JSON
 * value, except that a number is an integer or a float, decided by its text
 * (`specs/reference.md` §3, `specs/decisions.md` §18). Plus the handful of
 * operations the rest of the port needs over it: structural equality, and
 * `specs/reference.md` §6's normative `str` rendering.
 *
 * **How a JavaScript number is classified.** A host `number` has no type of
 * its own, and `JSON.parse` reads `3` and `3.0` as the same value, so this
 * port's binding is:
 *
 * - a `number` that is a safe integer (`Number.isSafeInteger`) is an
 *   **integer**; any other `number` (`1.5`, `1e21`) is a **float**;
 * - a `Float` is a **float** whatever it holds, which is the only way to
 *   write the float `3.0`;
 * - a `bigint` is an **integer**. It is how an integer-form JSON number
 *   beyond the range is carried, digits intact, to the decoder that refuses
 *   it.
 *
 * What the evaluator hands back is in **canonical form**: an integer is a
 * plain `number`, and a float is a plain `number` unless its value is a safe
 * integer, in which case it is a `Float`. A host that never meets a
 * whole-valued float therefore never meets a `Float`.
 *
 * **Integer range.** This port has the guaranteed range only,
 * `-(2^53 - 1)` to `2^53 - 1` (`specs/reference.md` §13, the corpus's
 * `int53`): an integer is held in a double, which is exact there and nowhere
 * beyond.
 *
 * `JSON.parse` and `JSON.stringify` lose the type (`3.0` becomes `3`, in
 * both directions); `parseJson` and `stringify` keep it.
 */
/**
 * A float, stated as one. `new Float(3)` is the float `3.0`, where the plain
 * number `3` is the integer. Prefer `float(n)`, which wraps only when the
 * plain number would be read as an integer.
 */
export class Float {
    value;
    constructor(value) {
        this.value = value;
    }
    /** So that host arithmetic and comparisons see the number. */
    valueOf() {
        return this.value;
    }
    /** The text `stringify` writes: `3.0`, never `3`. */
    toString() {
        return formatFloat(this.value);
    }
    /**
     * `JSON.stringify` cannot write `3.0`, so through it a `Float` degrades to
     * its plain number and the type is lost. `stringify` keeps it.
     */
    toJSON() {
        return this.value;
    }
}
/** The float of that value, in canonical form: wrapped only when a plain number would be an integer. */
export function float(n) {
    return Number.isSafeInteger(n) ? new Float(n) : n;
}
/** Whether a double is an integer this port holds exactly: a whole number in the guaranteed range. */
export function inIntegerRange(n) {
    return Number.isSafeInteger(n);
}
/**
 * A JSON number as its type and its value, or `null` for anything else. A
 * `bigint` outside the integer range has no exact double: its `value` is the
 * nearest one, which `inIntegerRange` refuses.
 */
export function classifyNumber(v) {
    if (typeof v === "number")
        return { type: Number.isSafeInteger(v) ? "integer" : "float", value: v };
    if (typeof v === "bigint")
        return { type: "integer", value: Number(v) };
    if (v instanceof Float)
        return { type: "float", value: v.value };
    return null;
}
export function isInteger(v) {
    return classifyNumber(v)?.type === "integer";
}
export function isFloat(v) {
    return classifyNumber(v)?.type === "float";
}
/** The value of a number of either type, or `undefined` for anything else. */
export function numberValue(v) {
    return classifyNumber(v)?.value;
}
/** A number outside the value domain (`specs/reference.md` §3). */
export class JsonNumberError extends Error {
    name = "JsonNumberError";
}
/**
 * The numbers of a JSON value as Tramaj values, at any depth and in canonical
 * form, or a `JsonNumberError` for one that cannot be a value. This is the
 * one place that decides what a number outside the value domain becomes
 * (`specs/reference.md` §3, `specs/node-json.md` *Decoding*):
 *
 * - an integer outside the integer range is refused, not rounded;
 * - a float that is not finite (`1e400` read from text, a host `NaN`) is
 *   refused;
 * - a negative zero is zero: `-0.0` is `0.0` and `-0` is `0`.
 */
export function normalizeNumbers(v) {
    if (v === null || typeof v === "boolean" || typeof v === "string")
        return v;
    const n = classifyNumber(v);
    if (n !== null)
        return normalizeNumber(n.type, n.value);
    if (Array.isArray(v))
        return v.map(normalizeNumbers);
    const o = v;
    return jsonObjectFrom(ownKeys(o).map((k) => [k, normalizeNumbers(o[k])]));
}
/** One number, as `normalizeNumbers` treats it. */
export function normalizeNumber(type, value) {
    if (type === "integer") {
        if (!inIntegerRange(value)) {
            throw new JsonNumberError("an integer is outside the integer range, -(2^53 - 1) to 2^53 - 1");
        }
        return value === 0 ? 0 : value;
    }
    if (!Number.isFinite(value)) {
        throw new JsonNumberError("a float is not finite: it is too large for a double");
    }
    return float(value === 0 ? 0 : value);
}
/** A JSON object, which a `Float` is not. */
export function isJsonObject(v) {
    return typeof v === "object" && v !== null && !Array.isArray(v) && !(v instanceof Float);
}
/** Own-property read, so a JSON object's inherited `constructor`/`toString` never answers. */
export function getField(o, key) {
    return Object.prototype.hasOwnProperty.call(o, key) ? o[key] : undefined;
}
export function hasField(o, key) {
    return Object.prototype.hasOwnProperty.call(o, key);
}
export function ownKeys(o) {
    return Object.keys(o);
}
/**
 * Builds an object from dynamic keys without `__proto__` reaching the
 * prototype setter, which a plain assignment would.
 */
export function jsonObjectFrom(entries) {
    const out = {};
    for (const [k, v] of entries) {
        Object.defineProperty(out, k, { value: v, enumerable: true, writable: true, configurable: true });
    }
    return out;
}
/**
 * Structural, with object keys unordered. An integer and a float are never
 * equal, whatever they hold: `3` and `3.0` are two values.
 */
export function jsonEqual(a, b) {
    const na = classifyNumber(a);
    const nb = classifyNumber(b);
    if (na !== null || nb !== null) {
        if (na === null || nb === null || na.type !== nb.type)
            return false;
        // Two integers beyond the range are compared by their digits.
        if (typeof a === "bigint" && typeof b === "bigint")
            return a === b;
        return na.value === nb.value;
    }
    if (a === b)
        return true;
    if (Array.isArray(a) && Array.isArray(b)) {
        if (a.length !== b.length)
            return false;
        return a.every((x, i) => jsonEqual(x, b[i]));
    }
    if (isJsonObject(a) && isJsonObject(b)) {
        const ka = ownKeys(a);
        const kb = ownKeys(b);
        if (ka.length !== kb.length)
            return false;
        return ka.every((k) => hasField(b, k) && jsonEqual(a[k], b[k]));
    }
    return false;
}
/**
 * An integer as its decimal digits. ECMAScript's `Number::toString` writes
 * every whole number below `1e21` that way, which covers the integer range.
 */
export function formatInteger(n) {
    return String(n);
}
/**
 * A float as the shortest round-trip text of ECMAScript's `Number::toString`,
 * with `.0` appended when that text has neither a fraction nor an exponent:
 * `1.0`, `0.1`, `100000000000.0`, `1e+21` (`specs/node-json.md` *Numbers*).
 *
 * A value that is not finite has no JSON text and is never a Tramaj value. It
 * is written as a float no double holds, which `parseJson` reads back as one
 * the decoder refuses.
 */
export function formatFloat(n) {
    if (!Number.isFinite(n))
        return n < 0 ? "-1e400" : "1e400";
    const text = String(n);
    return /^-?[0-9]+$/.test(text) ? `${text}.0` : text;
}
function isJsonNumber(v) {
    return typeof v === "number" || typeof v === "bigint" || v instanceof Float;
}
/** A number of either type as JSON text that keeps the type. */
function formatJsonNumber(v) {
    if (typeof v === "bigint")
        return formatInteger(v);
    if (v instanceof Float)
        return formatFloat(v.value);
    return Number.isSafeInteger(v) ? formatInteger(v) : formatFloat(v);
}
export function quoteString(s) {
    return JSON.stringify(s);
}
/** Compact JSON with object keys sorted and numbers by their type (`specs/reference.md` §6). */
export function compactJson(v) {
    if (v === null)
        return "null";
    if (typeof v === "boolean")
        return v ? "true" : "false";
    if (isJsonNumber(v))
        return formatJsonNumber(v);
    if (typeof v === "string")
        return quoteString(v);
    if (Array.isArray(v))
        return `[${v.map(compactJson).join(",")}]`;
    const keys = ownKeys(v).sort(compareStrings);
    return `{${keys.map((k) => `${quoteString(k)}:${compactJson(v[k])}`).join(",")}}`;
}
/** `str`'s rendering (`specs/reference.md` §6): raw at the top level, compact below it. */
export function displayString(v) {
    if (v === null)
        return "";
    if (typeof v === "boolean")
        return v ? "true" : "false";
    if (isJsonNumber(v))
        return formatJsonNumber(v);
    if (typeof v === "string")
        return v;
    return compactJson(v);
}
export function compareStrings(a, b) {
    return a < b ? -1 : a > b ? 1 : 0;
}
// Writing ---------------------------------------------------------------------
/**
 * JSON text that keeps the type of each number, object keys in the order the
 * object holds them. Compact, or with `indent` spaces per level when given;
 * an empty array or object stays on one line either way.
 */
export function stringify(v, indent = 0) {
    const step = " ".repeat(Math.max(0, indent));
    const block = (open, close, pad, items) => {
        if (items.length === 0)
            return open + close;
        if (step === "")
            return open + items.join(",") + close;
        return `${open}\n${items.map((i) => pad + step + i).join(",\n")}\n${pad}${close}`;
    };
    const go = (x, pad) => {
        if (x === null)
            return "null";
        if (typeof x === "boolean")
            return x ? "true" : "false";
        if (isJsonNumber(x))
            return formatJsonNumber(x);
        if (typeof x === "string")
            return quoteString(x);
        const inner = pad + step;
        if (Array.isArray(x))
            return block("[", "]", pad, x.map((i) => go(i, inner)));
        const sep = step === "" ? ":" : ": ";
        return block("{", "}", pad, ownKeys(x).map((k) => quoteString(k) + sep + go(x[k], inner)));
    };
    return go(v, "");
}
// Reading ---------------------------------------------------------------------
export class JsonParseError extends Error {
    name = "JsonParseError";
}
const NUMBER = /-?(?:0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/y;
const STRING = /"(?:[^"\\]|\\.)*"/y;
const WORDS = [
    ["null", null],
    ["true", true],
    ["false", false],
];
/**
 * Parses JSON text (RFC 8259), typing each number by its text: one with
 * neither a fraction nor an exponent is an integer, one with either is a
 * float, so `3` is an integer and `3.0` and `3e0` are floats.
 *
 * No number is refused here; refusing is left to whoever decodes the value
 * (`normalizeNumbers`, which the evaluator applies to a context and
 * `nodeFromJson` to a node). An integer-form number beyond the integer range
 * is read as a `bigint`, digits intact, and a float too large for a double as
 * a `Float` holding an infinity. Of two equal keys in one object, the last
 * wins.
 */
export function parseJson(text) {
    let pos = 0;
    const fail = (what) => new JsonParseError(`${what} at offset ${pos}`);
    const ws = () => {
        for (;;) {
            const c = text[pos];
            if (c === " " || c === "\n" || c === "\r" || c === "\t")
                pos += 1;
            else
                return;
        }
    };
    const string = () => {
        STRING.lastIndex = pos;
        const m = STRING.exec(text);
        if (m === null)
            throw fail("expected a string");
        let s;
        try {
            // The host's decoder owns the escapes and refuses a raw control character.
            s = JSON.parse(m[0]);
        }
        catch {
            throw fail("invalid string");
        }
        pos += m[0].length;
        return s;
    };
    const number = () => {
        NUMBER.lastIndex = pos;
        const m = NUMBER.exec(text);
        if (m === null)
            throw fail("unexpected character");
        pos += m[0].length;
        const n = Number(m[0]);
        if (m[1] !== undefined || m[2] !== undefined)
            return Number.isFinite(n) ? float(n) : new Float(n);
        return Number.isSafeInteger(n) ? n : BigInt(m[0]);
    };
    const value = () => {
        ws();
        const c = text[pos];
        if (c === undefined)
            throw fail("unexpected end of input");
        if (c === '"')
            return string();
        if (c === "[") {
            pos += 1;
            const items = [];
            ws();
            if (text[pos] === "]") {
                pos += 1;
                return items;
            }
            for (;;) {
                items.push(value());
                ws();
                if (text[pos] === ",")
                    pos += 1;
                else if (text[pos] === "]")
                    break;
                else
                    throw fail("expected , or ]");
            }
            pos += 1;
            return items;
        }
        if (c === "{") {
            pos += 1;
            const entries = [];
            ws();
            if (text[pos] === "}") {
                pos += 1;
                return jsonObjectFrom(entries);
            }
            for (;;) {
                ws();
                const k = string();
                ws();
                if (text[pos] !== ":")
                    throw fail("expected :");
                pos += 1;
                entries.push([k, value()]);
                ws();
                if (text[pos] === ",")
                    pos += 1;
                else if (text[pos] === "}")
                    break;
                else
                    throw fail("expected , or }");
            }
            pos += 1;
            return jsonObjectFrom(entries);
        }
        for (const [word, v] of WORDS) {
            if (text.startsWith(word, pos)) {
                pos += word.length;
                return v;
            }
        }
        return number();
    };
    const v = value();
    ws();
    if (pos !== text.length)
        throw fail("unexpected trailing text");
    return v;
}
