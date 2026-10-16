/**
 * `Node`/`NodeAttribute` and the strict `specs/node-json.md` encode/decode,
 * hand-written rather than derived so the "a decoder MUST reject ... MUST NOT
 * infer a missing field from a default" rules are enforced exactly.
 */
import { hasField, isJsonObject, jsonObjectFrom, JsonNumberError, normalizeNumbers, } from "./json.js";
export function noAnnotations() {
    return {};
}
export function textNode(value, annotations = noAnnotations()) {
    return { t: "text", value, annotations };
}
export function elementNode(tag, attributes, value, children, annotations = noAnnotations()) {
    return { t: "element", tag, attributes, value, children, annotations };
}
export function fragmentNode(children, annotations = noAnnotations()) {
    return { t: "fragment", children, annotations };
}
function annotationsToJson(anns) {
    return jsonObjectFrom(Object.keys(anns).map((k) => [k, anns[k]]));
}
export function nodeToJson(n) {
    switch (n.t) {
        case "text":
            return { type: "text", value: n.value, annotations: annotationsToJson(n.annotations) };
        case "element":
            return {
                type: "element",
                tag: n.tag,
                attributes: n.attributes.map(nodeAttributeToJson),
                value: n.value,
                children: n.children.map(nodeToJson),
                annotations: annotationsToJson(n.annotations),
            };
        case "fragment":
            return {
                type: "fragment",
                children: n.children.map(nodeToJson),
                annotations: annotationsToJson(n.annotations),
            };
    }
}
export function nodeAttributeToJson(a) {
    return a.t === "attribute"
        ? { kind: "attribute", name: a.name, value: a.value }
        : { kind: "action", event: a.event, key: a.key, payload: a.payload };
}
export class NodeDecodeError extends Error {
    name = "NodeDecodeError";
}
function req(what, field, obj) {
    if (!hasField(obj, field)) {
        throw new NodeDecodeError(`${what}: missing required field ${JSON.stringify(field)}`);
    }
    return obj[field];
}
/**
 * A `Value` field (`specs/node-json.md` *Numbers*, *Decoding*): its numbers
 * are typed by `json.ts`'s classification and brought to canonical form, and
 * one outside the value domain is rejected rather than rounded.
 */
function reqValue(what, field, obj) {
    const v = req(what, field, obj);
    try {
        return normalizeNumbers(v);
    }
    catch (e) {
        if (!(e instanceof JsonNumberError))
            throw e;
        throw new NodeDecodeError(`${what}: field ${JSON.stringify(field)}: ${e.message}`);
    }
}
function reqString(what, field, obj) {
    const v = req(what, field, obj);
    if (typeof v !== "string") {
        throw new NodeDecodeError(`${what}: field ${JSON.stringify(field)} must be a string`);
    }
    return v;
}
function reqArray(what, field, obj) {
    const v = req(what, field, obj);
    if (!Array.isArray(v)) {
        throw new NodeDecodeError(`${what}: field ${JSON.stringify(field)} must be an array`);
    }
    return v;
}
function reqAnnotations(obj) {
    const v = req("node", "annotations", obj);
    if (!isJsonObject(v)) {
        throw new NodeDecodeError('node: field "annotations" must be an object');
    }
    return jsonObjectFrom(Object.keys(v).map((k) => [k, v[k]]));
}
export function nodeFromJson(v) {
    if (!isJsonObject(v))
        throw new NodeDecodeError("expected a JSON object for a node");
    const type = reqString("node", "type", v);
    switch (type) {
        case "text":
            return { t: "text", value: reqValue("text node", "value", v), annotations: reqAnnotations(v) };
        case "element":
            return {
                t: "element",
                tag: reqString("element node", "tag", v),
                attributes: reqArray("element node", "attributes", v).map(nodeAttributeFromJson),
                value: reqValue("element node", "value", v),
                children: reqArray("element node", "children", v).map(nodeFromJson),
                annotations: reqAnnotations(v),
            };
        case "fragment":
            return {
                t: "fragment",
                children: reqArray("fragment node", "children", v).map(nodeFromJson),
                annotations: reqAnnotations(v),
            };
        default:
            throw new NodeDecodeError(`unknown node type: ${JSON.stringify(type)}`);
    }
}
export function nodeAttributeFromJson(v) {
    if (!isJsonObject(v))
        throw new NodeDecodeError("expected a JSON object for a node attribute");
    const kind = reqString("node attribute", "kind", v);
    switch (kind) {
        case "attribute":
            return {
                t: "attribute",
                name: reqString("attribute", "name", v),
                value: reqValue("attribute", "value", v),
            };
        case "action":
            return {
                t: "action",
                event: reqString("action", "event", v),
                key: reqString("action", "key", v),
                payload: reqValue("action", "payload", v),
            };
        default:
            throw new NodeDecodeError(`unknown node attribute kind: ${JSON.stringify(kind)}`);
    }
}
/** Rewrites every action reachable in a tree, leaving everything else untouched. */
export function mapActions(n, f) {
    switch (n.t) {
        case "text":
            return n;
        case "element":
            return {
                ...n,
                attributes: n.attributes.map((a) => a.t === "action" ? f(a.event, a.key, a.payload) : a),
                children: n.children.map((c) => mapActions(c, f)),
            };
        case "fragment":
            return { ...n, children: n.children.map((c) => mapActions(c, f)) };
    }
}
