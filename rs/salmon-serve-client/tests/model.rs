//! The model against the recorded pass the Haskell fold is tested on.
//!
//! `rs/fixtures/client-model.json` holds one snapshot, one event sequence
//! and the text expected after folding; `Test/ClientModelSpec.hs` checks the
//! same file against `Salmon.Client.Model`. The cases below are that spec's
//! Layer 0 cases, one for one.

use salmon_serve_client::follow::join;
use salmon_serve_client::model::{Check, Counts, Event, Model, Pass, RefId};
use serde_json::{json, Value};

fn fixture() -> Value {
    serde_json::from_str(include_str!("../../fixtures/client-model.json"))
        .expect("the fixture is JSON")
}

fn recorded() -> Vec<Value> {
    fixture()["recorded"].as_array().expect("recorded").clone()
}

fn snapshot_at(seq: u64) -> Model {
    let mut snapshot = fixture()["snapshot"].clone();
    snapshot["seq"] = json!(seq);
    Model::from_dag(&snapshot).expect("snapshot")
}

fn start() -> Model {
    Model::from_dag(&fixture()["snapshot"]).expect("snapshot")
}

fn fold(mut model: Model, events: &[Value]) -> Model {
    for v in events {
        model.step(&Event::of(v.clone()));
    }
    model
}

fn id(short: &str) -> RefId {
    RefId {
        short: short.into(),
        full: format!("full-{short}"),
    }
}

fn about(stream: &str, kind: &str, short: &str, seq: u64) -> Value {
    json!({
        "stream": stream, "kind": kind, "seq": seq,
        "ref": {"short": short, "full": format!("full-{short}")},
        "node": {"shorthand": short, "help": "", "notes": []},
    })
}

#[test]
fn a_dag_snapshot_folded_over_a_recorded_pass_gives_the_per_node_view() {
    let m0 = start();
    assert_eq!(m0.seq, 5);
    assert_eq!(m0.order, vec![id("n1"), id("n2"), id("root")]);
    assert_eq!(m0.mode, "interactive");
    assert_eq!(m0.nodes[&id("root")].dependencies, vec![id("n1"), id("n2")]);
    assert_eq!(m0.nodes[&id("n1")].paths, vec!["/n1"]);
    assert_eq!(m0.nodes[&id("n1")].notes, vec!["a note"]);
    // the declaration asks for a re-read, and the fold goes on past it
    let recorded = recorded();
    let mut after_declared = fold(m0, &recorded[..1]);
    assert_eq!(
        after_declared.resync.as_deref(),
        Some("epoch 0 declared up")
    );
    after_declared.resolve();
    let m = fold(after_declared, &recorded[1..]);
    assert_eq!(m.seq, 16, "seq is the highest seen (the gap has none)");
    assert_eq!(m.resync.as_deref(), fixture()["expect"]["resync"].as_str());
    assert_eq!(
        m.pass,
        Some(Pass::Stopped {
            ok: false,
            remaining: 2
        })
    );
    let n1 = &m.nodes[&id("n1")];
    assert_eq!(n1.convergence, "converged");
    assert_eq!(
        n1.check,
        Some(Check {
            verdict: "success".into(),
            reason: None
        })
    );
    assert_eq!(
        (n1.last_kind.as_deref(), n1.last_seq),
        (Some("next-look"), Some(15))
    );
    assert_eq!(n1.error, None);
    let n2 = &m.nodes[&id("n2")];
    assert_eq!(n2.convergence, "errored");
    assert_eq!(n2.error.as_deref(), Some("boom"));
    assert_eq!(
        (n2.last_kind.as_deref(), n2.last_seq),
        (Some("failed"), Some(11))
    );
    let root = &m.nodes[&id("root")];
    assert_eq!(root.convergence, "blocked");
    assert_eq!(
        (root.last_kind.as_deref(), root.last_seq),
        (Some("blocked"), Some(12))
    );
    assert_eq!(
        m.counts(),
        Counts {
            converged: 1,
            errored: 1,
            total: 3
        }
    );
    let order: Vec<RefId> = m.nodes_in_order().iter().map(|n| n.id.clone()).collect();
    assert_eq!(order, vec![id("n1"), id("n2"), id("root")]);
    assert_eq!(m.last.as_ref().map(|e| e.kind.as_str()), Some("gap"));
}

#[test]
fn folding_an_event_twice_is_folding_it_once() {
    let recorded = recorded();
    let once = fold(start(), &recorded);
    assert_eq!(
        fold(once.clone(), &recorded),
        once,
        "the whole sequence twice"
    );
    // every numbered prefix replayed onto the whole leaves it where it was
    let numbered: Vec<Value> = recorded
        .iter()
        .filter(|v| v.get("seq").is_some())
        .cloned()
        .collect();
    for k in 0..=numbered.len() {
        assert_eq!(
            fold(once.clone(), &numbered[..k]),
            once,
            "prefix {k} replayed"
        );
    }
    // a snapshot taken after a racing event drops that event's replay too
    let later = fold(snapshot_at(9), &recorded);
    let n2 = &later.nodes[&id("n2")];
    assert_eq!(
        (n2.last_kind.as_deref(), n2.last_seq),
        (Some("failed"), Some(11)),
        "events above the snapshot's seq land"
    );
    let n1 = &later.nodes[&id("n1")];
    assert_eq!(
        (n1.convergence.as_str(), n1.last_kind.as_deref()),
        ("pending", Some("next-look")),
        "n1's done (9) was not re-applied: the snapshot already had its say"
    );
    // but the loop's part has its own stamp: a fresh snapshot read after the
    // pass, rebased onto a model that saw it start, still takes the stop
    let started = fold(start(), &recorded[..2]);
    assert_eq!(started.pass, Some(Pass::Converging { down: 0, up: 3 }));
    let rebased = Model::rebase(&started, snapshot_at(20));
    assert_eq!(
        rebased.pass,
        Some(Pass::Converging { down: 0, up: 3 }),
        "the pass carried over"
    );
    assert_eq!(rebased.seq, 20, "the cursor is the snapshot's");
    assert_eq!(rebased.resync, None, "the resync is answered");
    let ended = fold(rebased, &recorded[2..]);
    assert_eq!(
        ended.pass,
        Some(Pass::Stopped {
            ok: false,
            remaining: 2
        }),
        "the stop (13 < 20) still lands on the loop's part"
    );
    assert_eq!(
        ended.nodes[&id("n2")].convergence,
        "pending",
        "n2's failed (11) is below the snapshot's stamp"
    );
}

#[test]
fn an_upkeep_acted_is_unwrapped_to_the_pass_vocabulary() {
    let acted = json!({"stream": "upkeep", "kind": "acted", "seq": 20, "report": about("updown", "done", "n2", 0)});
    let m = fold(start(), &[acted]);
    let n2 = &m.nodes[&id("n2")];
    assert_eq!(n2.convergence, "converged");
    assert_eq!(
        (n2.last_kind.as_deref(), n2.last_seq),
        (Some("acted done"), Some(20))
    );
}

#[test]
fn a_node_wanted_down_is_dropped_by_its_done() {
    let mut retiring = fixture()["snapshot"].clone();
    retiring["seq"] = json!(1);
    retiring["nodes"].as_array_mut().unwrap().truncate(2);
    retiring["nodes"][0]["direction"] = json!("down");
    let m0 = Model::from_dag(&retiring).unwrap();
    assert_eq!(m0.counts().total, 2);
    let m = fold(
        m0.clone(),
        &[
            about("updown", "eval", "n1", 2),
            about("updown", "done", "n1", 3),
        ],
    );
    assert!(!m.nodes.contains_key(&id("n1")), "n1 is gone once down");
    assert_eq!(m.order, vec![id("n2")]);
    // while a node wanted up that is done stays
    let m = fold(m0, &[about("updown", "done", "n2", 4)]);
    assert_eq!(m.nodes[&id("n2")].convergence, "converged");
}

#[test]
fn the_rendered_rows_header_and_event_lines() {
    let fixture = fixture();
    let expect = &fixture["expect"];
    let m = fold(start(), &recorded());
    let rows: Vec<Value> = m
        .nodes_in_order()
        .iter()
        .map(|n| json!(n.render_row()))
        .collect();
    assert_eq!(json!(rows), expect["rows"]);
    assert_eq!(
        json!(m.render_header(expect["target"].as_str().unwrap())),
        expect["header"]
    );
    let lines: Vec<Value> = recorded()
        .into_iter()
        .map(|v| json!(Event::of(v).render_line()))
        .collect();
    assert_eq!(json!(lines), expect["eventLines"]);
}

#[test]
fn the_other_header_words() {
    let m = fold(
        start(),
        &[
            json!({"stream": "serve", "kind": "supervised", "seq": 6, "on": true}),
            json!({"stream": "serve", "kind": "converge-start", "seq": 7, "down": 1, "up": 2}),
        ],
    );
    assert_eq!(m.render_header("x"), "x mode=interactive seq=7 converged=0 errored=0 total=3 converging(1 down, 2 up) supervising");
    let m = fold(
        m,
        &[
            json!({"stream": "serve", "kind": "converge-stop", "seq": 8, "ok": true, "remaining": 0}),
        ],
    );
    assert_eq!(
        m.render_header("x"),
        "x mode=interactive seq=8 converged=0 errored=0 total=3 converged supervising"
    );
}

#[test]
fn a_shapeless_object_and_an_unknown_kind_are_recorded_and_passed() {
    let m = fold(
        start(),
        &[
            json!({"seq": 6}),
            about("updown", "something-new", "n1", 7),
            about("upkeep", "x", "nobody", 8),
        ],
    );
    assert_eq!(m.seq, 8);
    let n1 = &m.nodes[&id("n1")];
    assert_eq!(
        (n1.convergence.as_str(), n1.last_kind.as_deref()),
        ("pending", Some("something-new"))
    );
    assert_eq!(
        m.counts().total,
        3,
        "an event about a node the model has never seen adds nothing"
    );
    let shapeless = Event::of(json!([1, 2]));
    assert_eq!(
        (
            shapeless.stream.as_str(),
            shapeless.kind.as_str(),
            shapeless.seq
        ),
        ("?", "?", None)
    );
}

#[test]
fn a_dag_answer_that_is_not_one_is_refused_in_words() {
    assert_eq!(
        Model::from_dag(&json!({"seq": 1})).unwrap_err(),
        "/dag answer has no nodes array"
    );
    assert_eq!(
        Model::from_dag(&json!({"nodes": []})).unwrap_err(),
        "/dag answer carries no seq"
    );
    assert!(Model::from_dag(&json!({"seq": 1, "nodes": [{}]}))
        .unwrap_err()
        .starts_with("a node without a ref"));
}

#[test]
fn a_snapshot_read_on_reconnecting_keeps_the_loop_part_unless_the_server_started_over() {
    let folded = fold(start(), &recorded()[..2]);
    // same server, later snapshot: the pass shown is kept
    let joined = join(Some(&folded), snapshot_at(30));
    assert_eq!(joined.pass, Some(Pass::Converging { down: 0, up: 3 }));
    assert_eq!(joined.seq, 30);
    // a snapshot numbered below the cursor is another process: start over,
    // or its events (numbered from 1 again) would all be dropped as replays
    let restarted = join(Some(&folded), snapshot_at(2));
    assert_eq!(
        (restarted.seq, restarted.loop_seq, restarted.pass.clone()),
        (2, 0, None)
    );
    let m = fold(
        restarted,
        &[json!({"stream": "serve", "kind": "converge-start", "seq": 3, "down": 0, "up": 1})],
    );
    assert_eq!(m.pass, Some(Pass::Converging { down: 0, up: 1 }));
    assert_eq!(join(None, snapshot_at(4)).seq, 4);
}
