//! The display order (`model::Order`) against the `Vec` it replaced.
//!
//! Removing a node used to be `order.retain(|o| o != r)`: one pass over the
//! whole order per removal, so a teardown was quadratic. `Order` removes by
//! lookup, and what it must keep is what a reader sees: the same refs, in
//! the same order, as the `Vec` would have held. The cases below say that
//! for the positions that matter and for scripts drawn from a fixed seed;
//! `Test/ClientModelStepSpec.hs` is the QuickCheck statement of the same on
//! the Haskell side, against the unchanged pre-change fold. There is no
//! property-testing crate in this workspace, so the scripts here are drawn
//! by a small generator of this file's own and the seeds are fixed.

use salmon_serve_client::model::{Event, Model, Order, RefId};
use serde_json::{json, Value};

fn id(short: &str) -> RefId {
    RefId {
        short: short.into(),
        full: format!("full-{short}"),
    }
}

fn ids(shorts: &[&str]) -> Vec<RefId> {
    shorts.iter().map(|s| id(s)).collect()
}

fn order_of(shorts: &[&str]) -> Order {
    ids(shorts).into_iter().collect()
}

#[test]
fn an_order_reads_as_the_list_it_was_built_from() {
    let order = order_of(&["a", "b", "c"]);
    assert_eq!(order.to_vec(), ids(&["a", "b", "c"]));
    assert_eq!(order.len(), 3);
    assert!(!order.is_empty());
    assert!(Order::default().is_empty());
    assert_eq!(Order::default(), Vec::new());
}

#[test]
fn removing_the_first_a_middle_and_the_last_keeps_the_others_in_place() {
    for (gone, left) in [
        ("a", ["b", "c", "d"]),
        ("b", ["a", "c", "d"]),
        ("c", ["a", "b", "d"]),
        ("d", ["a", "b", "c"]),
    ] {
        let mut order = order_of(&["a", "b", "c", "d"]);
        order.remove(&id(gone));
        assert_eq!(order, ids(&left), "without {gone}");
        assert_eq!(order.len(), 3);
    }
}

#[test]
fn removing_a_ref_that_is_not_shown_changes_nothing() {
    let mut order = order_of(&["a", "b"]);
    order.remove(&id("z"));
    assert_eq!(order, ids(&["a", "b"]));
    // and twice is once
    order.remove(&id("a"));
    order.remove(&id("a"));
    assert_eq!(order, ids(&["b"]));
}

#[test]
fn a_ref_listed_twice_is_shown_twice_and_removed_from_both_places() {
    let mut order = order_of(&["a", "b", "a", "c"]);
    assert_eq!(order, ids(&["a", "b", "a", "c"]));
    order.remove(&id("a"));
    assert_eq!(order, ids(&["b", "c"]));
}

#[test]
fn a_push_after_removals_lands_last_and_equality_is_what_is_read() {
    let mut order = order_of(&["a", "b", "c"]);
    order.remove(&id("c"));
    order.remove(&id("a"));
    order.push(id("a"));
    assert_eq!(order, ids(&["b", "a"]));
    // the seats differ from a fresh order's, the reading does not
    assert_eq!(order, order_of(&["b", "a"]));
    assert_ne!(order, order_of(&["a", "b"]));
}

#[test]
fn a_whole_teardown_empties_it_in_any_direction() {
    let shorts: Vec<String> = (0..200).map(|i| format!("n{i}")).collect();
    let refs: Vec<RefId> = shorts.iter().map(|s| id(s)).collect();
    let mut forwards: Order = refs.iter().cloned().collect();
    let mut backwards = forwards.clone();
    for (i, r) in refs.iter().enumerate() {
        forwards.remove(r);
        assert_eq!(forwards, refs[i + 1..].to_vec());
    }
    for (i, r) in refs.iter().enumerate().rev() {
        backwards.remove(r);
        assert_eq!(backwards, refs[..i].to_vec());
    }
    assert!(forwards.is_empty() && backwards.is_empty());
}

/// A linear congruential generator: enough to draw scripts from a seed that
/// a failure message can name.
struct Draw(u64);

impl Draw {
    fn below(&mut self, n: u64) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (self.0 >> 33) % n
    }
}

/// Pushes and removals over a small pool (so that refs repeat, are removed
/// twice, and are pushed again after a removal), checked after every step
/// against a `Vec` doing what the model did before.
#[test]
fn drawn_pushes_and_removals_read_as_the_vec_they_replace() {
    for seed in 0..300u64 {
        let mut draw = Draw(seed);
        let pool = 1 + draw.below(9);
        let mut order = Order::default();
        let mut reference: Vec<RefId> = Vec::new();
        for step in 0..120 {
            let r = id(&format!("n{}", draw.below(pool)));
            if draw.below(3) == 0 {
                order.push(r.clone());
                reference.push(r);
            } else {
                order.remove(&r);
                reference.retain(|o| o != &r);
            }
            assert_eq!(order, reference, "seed {seed}, step {step}");
            assert_eq!(order.len(), reference.len(), "seed {seed}, step {step}");
        }
    }
}

fn node(short: &str, direction: &str) -> Value {
    json!({
        "ref": {"short": short, "full": format!("full-{short}")},
        "shorthand": short, "help": "", "notes": [],
        "direction": direction, "convergence": "pending", "status": null,
        "paths": [], "dynamics": [], "dependencies": [], "dependants": [],
    })
}

fn event(kind: &str, short: &str, seq: u64) -> Event {
    Event::of(json!({
        "stream": "updown", "kind": kind, "seq": seq,
        "ref": {"short": short, "full": format!("full-{short}")},
    }))
}

/// The fold itself, on drawn snapshots and events: after every event the
/// order is the order before it less exactly the refs the event took out of
/// the node map (what `retain` computed), every ref shown has a node, and
/// no ref is shown twice. The events are `done`, `eval` and `failed` about
/// nodes wanted up or down, about nodes already dropped, and about a ref no
/// snapshot had; every third script re-reads a snapshot half way.
#[test]
fn drawn_folds_keep_the_order_the_retain_kept() {
    for seed in 0..200u64 {
        let mut draw = Draw(seed ^ 0x5eed);
        let size = 1 + draw.below(12);
        let snapshot = |draw: &mut Draw, seq: u64| -> Model {
            let mut nodes: Vec<Value> = Vec::new();
            for i in 0..size {
                // four nodes in five are in a given snapshot
                if draw.below(5) != 0 {
                    let direction = if draw.below(3) == 0 { "up" } else { "down" };
                    nodes.push(node(&format!("n{i}"), direction));
                }
            }
            Model::from_dag(&json!({"mode": "interactive", "seq": seq, "nodes": nodes}))
                .expect("snapshot")
        };
        let mut m = snapshot(&mut draw, 0);
        let mut seq = 0;
        for step in 0..80 {
            if seed % 3 == 0 && step == 40 {
                let fresh = snapshot(&mut draw, seq);
                m = Model::rebase(&m, fresh);
            }
            let before = m.order.to_vec();
            // mostly forwards, sometimes a replay
            seq = if draw.below(6) == 0 { draw.below(seq + 1) } else { seq + 1 };
            let kind = ["done", "done", "done", "eval", "failed"][draw.below(5) as usize];
            // one past the pool: a ref no snapshot had
            let short = format!("n{}", draw.below(size + 1));
            m.step(&event(kind, &short, seq));
            let expected: Vec<RefId> = before
                .into_iter()
                .filter(|r| m.nodes.contains_key(r))
                .collect();
            assert_eq!(m.order, expected, "seed {seed}, step {step}");
            assert_eq!(m.order.len(), m.nodes.len(), "seed {seed}, step {step}");
            let shown: Vec<&RefId> = m.nodes_in_order().iter().map(|n| &n.id).collect();
            assert_eq!(shown, m.order.iter().collect::<Vec<_>>(), "seed {seed}, step {step}");
        }
    }
}

/// A teardown of fifty thousand nodes, first to last. With `retain` this
/// was 1.25 x 10^9 comparisons of two strings each; it is here so that the
/// suite would visibly stall if the removal went back to a scan. It times
/// nothing.
#[test]
fn a_teardown_of_fifty_thousand_nodes_folds() {
    let size = 50_000u64;
    let nodes: Vec<Value> = (0..size).map(|i| node(&format!("n{i}"), "down")).collect();
    let mut m = Model::from_dag(&json!({"mode": "interactive", "seq": 0, "nodes": nodes}))
        .expect("snapshot");
    assert_eq!(m.order.len(), size as usize);
    for i in 0..size {
        m.step(&event("done", &format!("n{i}"), i + 1));
    }
    assert!(m.order.is_empty());
    assert!(m.nodes.is_empty());
    assert_eq!(m.seq, size);
}
