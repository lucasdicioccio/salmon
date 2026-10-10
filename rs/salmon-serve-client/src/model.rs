//! The client's read model: a `/dag` snapshot with the event stream folded
//! onto it, one view per node. A port of `Salmon.Client.Model`; that module's
//! header is the reference for why each rule is what it is.
//!
//! Events are taken as data (`serde_json::Value`), never decoded into the
//! server's report types: the client is generic over salmon binaries the way
//! the server is, and a report kind it was not written for still shows as a
//! node's last event.
//!
//! Two rules are load-bearing, and both are tested:
//!
//! * **Replays are dropped per stamp, and there are two stamps.** Each node
//!   remembers the number it is current to ([`Node::seq`]), and the
//!   loop-level fields remember theirs ([`Model::loop_seq`]). A snapshot says
//!   everything about the nodes and nothing about the loop, so a re-read
//!   snapshot must not swallow the `converge-stop` of a pass whose start was
//!   already shown: [`Model::rebase`] carries the loop's part over.
//! * **The model asks to be re-read rather than guessing**
//!   ([`Model::resync`], set by `declared`, `cleared` and `gap`): an event
//!   names nodes by ref and cannot describe a node the model has never seen.

use std::collections::BTreeMap;

use serde_json::Value;

/// A node's identity on the wire: the short tag and the full text.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefId {
    pub short: String,
    pub full: String,
}

/// One event as the stream carried it.
#[derive(Debug, Clone, PartialEq)]
pub struct Event {
    /// `None` only for the `gap` event, which the server sends without one.
    pub seq: Option<u64>,
    pub stream: String,
    pub kind: String,
    pub node: Option<RefId>,
    /// The origin's name, for an event produced for a command.
    pub origin: Option<String>,
    /// The whole object, for whatever else a report says.
    pub value: Value,
}

impl Event {
    /// Read an event out of its wire object. Total: a shapeless object is an
    /// event of kind `?` with no ref, which the model records and moves past.
    pub fn of(value: Value) -> Event {
        Event {
            seq: number_at(&value, &["seq"]),
            stream: text_at(&value, &["stream"]).unwrap_or_else(|| "?".into()),
            kind: text_at(&value, &["kind"]).unwrap_or_else(|| "?".into()),
            node: ref_at(&value, &["ref"]),
            origin: text_at(&value, &["origin", "name"]),
            value,
        }
    }

    /// The event as one line: its number, stream, kind, and what it is about.
    pub fn render_line(&self) -> String {
        [
            self.seq.map_or("#-".to_string(), |s| format!("#{s}")),
            self.stream.clone(),
            self.kind.clone(),
            self.describe(),
        ]
        .join(" ")
    }

    /// The words after the kind: the node's short ref, or the loop-level
    /// fields worth a glance.
    pub fn describe(&self) -> String {
        match &self.node {
            Some(r) => match text_at(&self.value, &["node", "shorthand"]) {
                Some(shorthand) => format!("{} {}", r.short, shorthand),
                None => r.short.clone(),
            },
            None => [
                "line",
                "epoch",
                "direction",
                "nodes",
                "down",
                "up",
                "ok",
                "remaining",
                "from",
                "error",
                "on",
            ]
            .iter()
            .filter_map(|k| scalar_at(&self.value, &[k]).map(|t| format!("{k}={t}")))
            .collect::<Vec<_>>()
            .join(" "),
        }
    }
}

/// A node's own last word on its effect, as `next-look` and `/dag` carry it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Check {
    /// `success`/`skipped`/`completed`/`failure`/`unknown`/`immaterial`
    pub verdict: String,
    pub reason: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub id: RefId,
    pub shorthand: String,
    pub help: String,
    pub notes: Vec<String>,
    /// `up` or `down`
    pub direction: String,
    /// `pending`/`stale`/`converged`/`errored`/`blocked`
    pub convergence: String,
    pub check: Option<Check>,
    /// The snapshot's output ring, oldest first.
    pub output: Vec<String>,
    /// The last `failed`'s error, cleared by a later `done`/`skip`.
    pub error: Option<String>,
    /// The kind of the last event about this node.
    pub last_kind: Option<String>,
    pub last_seq: Option<u64>,
    /// The number this view is current to: the snapshot's, then each event's.
    pub seq: u64,
    pub dependencies: Vec<RefId>,
    pub dependants: Vec<RefId>,
    pub paths: Vec<String>,
}

impl Node {
    /// The six cells of a node's row: ref, shorthand, direction, convergence,
    /// last check, last event. [`Node::render_row`] is these, padded.
    pub fn cells(&self) -> [String; 6] {
        let last_event = match (&self.last_kind, self.last_seq) {
            (None, _) => "-".to_string(),
            (Some(kind), seq) => {
                let mut text = kind.clone();
                if let Some(s) = seq {
                    text.push_str(&format!(" #{s}"));
                }
                if let Some(e) = &self.error {
                    text.push_str(&format!(": {e}"));
                }
                text
            }
        };
        [
            self.id.short.clone(),
            self.shorthand.clone(),
            self.direction.clone(),
            self.convergence.clone(),
            self.check
                .as_ref()
                .map_or("-".to_string(), |c| c.verdict.clone()),
            last_event,
        ]
    }

    /// One line per node, the columns `salmon-tui` shows. Widths are fixed
    /// for the first five so the table reads as one; the last is open-ended.
    pub fn render_row(&self) -> String {
        let [short, shorthand, direction, convergence, check, last_event] = self.cells();
        let shorthand: String = shorthand.chars().take(22).collect();
        format!(
            "{short:<10} {shorthand:<22} {direction:<4} {convergence:<9} {check:<12} {last_event}"
        )
    }
}

/// The loop's last convergence pass as the stream told it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pass {
    /// `converge-start`: nodes to turn down, nodes to turn up.
    Converging { down: u64, up: u64 },
    /// `converge-stop`: everything applied cleanly, nodes left.
    Stopped { ok: bool, remaining: u64 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Counts {
    pub converged: usize,
    pub errored: usize,
    pub total: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Model {
    /// `interactive`/`replay`/`following`, from the snapshot's envelope.
    pub mode: String,
    /// The highest sequence number seen: the snapshot's, then each event's.
    /// The cursor to resume `/events` from.
    pub seq: u64,
    /// The number the loop-level fields are current to.
    pub loop_seq: u64,
    /// The snapshot's dependency order.
    pub order: Vec<RefId>,
    pub nodes: BTreeMap<RefId, Node>,
    pub pass: Option<Pass>,
    /// `None` until a `supervised` event says.
    pub supervised: Option<bool>,
    /// The last event folded, whatever it was about.
    pub last: Option<Event>,
    /// Why the snapshot should be re-read: a `gap`, or a declaration changed
    /// the node set. Human text for a status line. Cleared by
    /// [`Model::resolve`], [`Model::rebase`] or a fresh [`Model::from_dag`].
    pub resync: Option<String>,
}

impl Model {
    /// A model from a `/dag` answer. The error names what is missing; a
    /// `/dag` answer always has `nodes` and `seq`, so an error is a wrong
    /// URL, not a version skew.
    pub fn from_dag(dag: &Value) -> Result<Model, String> {
        let nodes = array_at(dag, &["nodes"]).ok_or("/dag answer has no nodes array")?;
        let seq = number_at(dag, &["seq"]).ok_or("/dag answer carries no seq")?;
        let mut order = Vec::with_capacity(nodes.len());
        let mut by_ref = BTreeMap::new();
        for n in nodes {
            let id = ref_at(n, &["ref"]).ok_or_else(|| format!("a node without a ref: {n}"))?;
            let node = Node {
                id: id.clone(),
                shorthand: text_at(n, &["shorthand"]).unwrap_or_else(|| "?".into()),
                help: text_at(n, &["help"]).unwrap_or_default(),
                notes: texts_at(n, &["notes"]),
                direction: text_at(n, &["direction"]).unwrap_or_else(|| "?".into()),
                convergence: text_at(n, &["convergence"]).unwrap_or_else(|| "?".into()),
                check: check_at(n, &["status", "check"]),
                output: texts_at(n, &["status", "output"]),
                error: None,
                last_kind: None,
                last_seq: None,
                seq,
                dependencies: refs_at(n, &["dependencies"]),
                dependants: refs_at(n, &["dependants"]),
                paths: texts_at(n, &["paths"]),
            };
            order.push(id.clone());
            by_ref.insert(id, node);
        }
        Ok(Model {
            mode: text_at(dag, &["mode"]).unwrap_or_else(|| "?".into()),
            seq,
            loop_seq: 0,
            order,
            nodes: by_ref,
            pass: None,
            supervised: None,
            last: None,
            resync: None,
        })
    }

    /// Forget the resync request (the snapshot is being re-read).
    pub fn resolve(&mut self) {
        self.resync = None;
    }

    /// A re-read snapshot joining a model that has been folding: the nodes,
    /// order and mode are the fresh snapshot's; the loop-level fields, their
    /// stamp and the last event are the old model's; the cursor is the
    /// higher of the two; and the resync request is answered.
    pub fn rebase(old: &Model, fresh: Model) -> Model {
        Model {
            seq: old.seq.max(fresh.seq),
            loop_seq: old.loop_seq,
            pass: old.pass.clone(),
            supervised: old.supervised,
            last: old.last.clone(),
            resync: None,
            ..fresh
        }
    }

    /// Fold one event in. Total; an event already accounted for (numbered at
    /// or below the stamp of the part it would change) leaves that part as
    /// it was. Otherwise the cursor moves to the event's number, the node
    /// the event is about is updated, and the loop-level fields follow the
    /// `serve` stream. An event that changes nothing still becomes
    /// [`Model::last`] if it is new to the cursor.
    pub fn step(&mut self, e: &Event) {
        let replayed = matches!(e.seq, Some(s) if s <= self.seq);
        if !replayed {
            if let Some(s) = e.seq {
                self.seq = s;
            }
            self.last = Some(e.clone());
        }
        match (e.stream.as_str(), e.kind.as_str()) {
            ("server", "gap") => {
                self.resync = Some(format!(
                    "events {} fell off the ring",
                    shown(number_at(&e.value, &["from"]))
                ));
            }
            ("server", _) => {}
            ("serve", "declared") => {
                let reason = format!(
                    "epoch {} declared {}",
                    shown(number_at(&e.value, &["epoch"])),
                    text_at(&e.value, &["direction"]).unwrap_or_else(|| "?".into())
                );
                self.on_loop(e, |m| m.resync = Some(reason));
            }
            ("serve", "cleared") => {
                self.on_loop(e, |m| m.resync = Some("every seed retired".into()));
            }
            ("serve", "converge-start") => {
                let pass = Pass::Converging {
                    down: number_at(&e.value, &["down"]).unwrap_or(0),
                    up: number_at(&e.value, &["up"]).unwrap_or(0),
                };
                self.on_loop(e, |m| m.pass = Some(pass));
            }
            ("serve", "converge-stop") => {
                let pass = Pass::Stopped {
                    ok: bool_at(&e.value, &["ok"]).unwrap_or(false),
                    remaining: number_at(&e.value, &["remaining"]).unwrap_or(0),
                };
                self.on_loop(e, |m| m.pass = Some(pass));
            }
            ("serve", "supervised") => {
                let on = bool_at(&e.value, &["on"]);
                self.on_loop(e, |m| m.supervised = on);
            }
            ("serve", _) => {}
            ("updown", kind) => {
                if let Some(r) = &e.node {
                    self.on_node(e, r, |n| {
                        touch(n, kind, e.seq);
                        settle(n, kind, &e.value);
                    });
                }
            }
            // an `acted` wraps what the tending machine did in the pass's
            // vocabulary; the inner object carries the ref
            ("upkeep", "acted") => {
                if let Some(inner) = e.value.get("report") {
                    if let Some(r) = ref_at(inner, &["ref"]) {
                        let kind = text_at(inner, &["kind"]).unwrap_or_else(|| "?".into());
                        self.on_node(e, &r, |n| {
                            touch(n, &format!("acted {kind}"), e.seq);
                            settle(n, &kind, inner);
                        });
                    }
                }
            }
            ("upkeep", "next-look") => {
                if let Some(r) = &e.node {
                    self.on_node(e, r, |n| {
                        touch(n, "next-look", e.seq);
                        n.check = check_at(&e.value, &["check"]);
                    });
                }
            }
            ("upkeep", kind) => {
                if let Some(r) = &e.node {
                    self.on_node(e, r, |n| touch(n, kind, e.seq));
                }
            }
            _ => {}
        }
    }

    /// The loop-level fields, unless the event is at or below their stamp.
    fn on_loop(&mut self, e: &Event, f: impl FnOnce(&mut Model)) {
        if matches!(e.seq, Some(s) if s <= self.loop_seq) {
            return;
        }
        f(self);
        if let Some(s) = e.seq {
            self.loop_seq = s;
        }
    }

    /// Update the node (unless the event is at or below its stamp), or drop
    /// it: a node wanted down that a pass has brought down is pruned by the
    /// loop after the pass, and `/dag` would no longer show it.
    fn on_node(&mut self, e: &Event, r: &RefId, f: impl FnOnce(&mut Node)) {
        let Some(n) = self.nodes.get_mut(r) else {
            return;
        };
        if matches!(e.seq, Some(s) if s <= n.seq) {
            return;
        }
        f(n);
        if let Some(s) = e.seq {
            n.seq = s;
        }
        let brought_down = n.direction == "down"
            && n.convergence == "converged"
            && matches!(n.last_kind.as_deref(), Some("done") | Some("acted done"));
        if brought_down {
            self.nodes.remove(r);
            self.order.retain(|o| o != r);
        }
    }

    /// The nodes in the snapshot's dependency order.
    pub fn nodes_in_order(&self) -> Vec<&Node> {
        self.order
            .iter()
            .filter_map(|r| self.nodes.get(r))
            .collect()
    }

    pub fn counts(&self) -> Counts {
        Counts {
            converged: self
                .nodes
                .values()
                .filter(|n| n.convergence == "converged")
                .count(),
            errored: self
                .nodes
                .values()
                .filter(|n| n.convergence == "errored")
                .count(),
            total: self.nodes.len(),
        }
    }

    /// The header line: target, mode, seq, the counts, the pass, supervision.
    /// Word for word what `renderHeader` gives, trailing blanks included.
    pub fn render_header(&self, target: &str) -> String {
        let c = self.counts();
        let pass = match &self.pass {
            None => String::new(),
            Some(Pass::Converging { down, up }) => format!("converging({down} down, {up} up)"),
            Some(Pass::Stopped { ok, remaining }) => {
                let left = if *remaining == 0 {
                    "converged".to_string()
                } else {
                    format!("incomplete({remaining} left)")
                };
                format!("{left}{}", if *ok { "" } else { "+failure" })
            }
        };
        let supervised = match self.supervised {
            None => "",
            Some(true) => "supervising",
            Some(false) => "not supervising",
        };
        [
            target.to_string(),
            format!("mode={}", self.mode),
            format!("seq={}", self.seq),
            format!("converged={}", c.converged),
            format!("errored={}", c.errored),
            format!("total={}", c.total),
            pass,
            supervised.to_string(),
        ]
        .join(" ")
    }
}

fn touch(n: &mut Node, kind: &str, seq: Option<u64>) {
    n.last_kind = Some(kind.to_string());
    n.last_seq = seq;
}

/// The pass's verdict on a node, in `/dag`'s convergence words.
fn settle(n: &mut Node, kind: &str, v: &Value) {
    match kind {
        "done" | "skip" => {
            n.convergence = "converged".into();
            n.error = None;
        }
        "failed" => {
            n.convergence = "errored".into();
            n.error = text_at(v, &["error"]);
        }
        "blocked" => n.convergence = "blocked".into(),
        _ => {}
    }
}

// --- reading JSON, totally --------------------------------------------------

fn at<'a>(v: &'a Value, keys: &[&str]) -> Option<&'a Value> {
    keys.iter().try_fold(v, |v, k| v.as_object()?.get(*k))
}

fn text_at(v: &Value, keys: &[&str]) -> Option<String> {
    at(v, keys)?.as_str().map(str::to_string)
}

fn number_at(v: &Value, keys: &[&str]) -> Option<u64> {
    at(v, keys)?.as_u64()
}

fn bool_at(v: &Value, keys: &[&str]) -> Option<bool> {
    at(v, keys)?.as_bool()
}

fn array_at<'a>(v: &'a Value, keys: &[&str]) -> Option<&'a Vec<Value>> {
    at(v, keys)?.as_array()
}

fn texts_at(v: &Value, keys: &[&str]) -> Vec<String> {
    array_at(v, keys)
        .map(|xs| {
            xs.iter()
                .filter_map(|x| x.as_str().map(str::to_string))
                .collect()
        })
        .unwrap_or_default()
}

fn ref_at(v: &Value, keys: &[&str]) -> Option<RefId> {
    let r = at(v, keys)?;
    Some(RefId {
        short: text_at(r, &["short"])?,
        full: text_at(r, &["full"])?,
    })
}

fn refs_at(v: &Value, keys: &[&str]) -> Vec<RefId> {
    array_at(v, keys)
        .map(|xs| xs.iter().filter_map(|x| ref_at(x, &[])).collect())
        .unwrap_or_default()
}

fn check_at(v: &Value, keys: &[&str]) -> Option<Check> {
    let c = at(v, keys)?;
    Some(Check {
        verdict: text_at(c, &["verdict"])?,
        reason: text_at(c, &["reason"]),
    })
}

fn scalar_at(v: &Value, keys: &[&str]) -> Option<String> {
    match at(v, keys)? {
        Value::String(t) => Some(t.clone()),
        Value::Number(n) => Some(n.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}

fn shown(n: Option<u64>) -> String {
    n.map_or("?".to_string(), |n| n.to_string())
}
