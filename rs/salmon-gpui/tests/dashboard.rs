//! The window, headless: the recorded pass both folds are tested on, fed
//! the way the feed thread feeds it, read back out of the real table.
//!
//! This renders the real components in gpui-kit's test window. It does not
//! look at pixels, and no pointer is involved: selection is set on the
//! table's state, which is what a click ends in.

use gpui_kit::test::TestWindowExt;
use gpui_kit::{
    px, size, AnyWindowHandle, AppContext as _, Bounds, Entity, Point, TestAppContext,
    WindowBounds, WindowOptions,
};
use salmon_gpui::dashboard::{Dashboard, COLUMNS};
use salmon_gpui::feed::{Connection, Update};
use salmon_serve_client::model::{Event, Model};
use serde_json::Value;

fn fixture() -> Value {
    serde_json::from_str(include_str!("../../fixtures/client-model.json"))
        .expect("the fixture is JSON")
}

fn snapshot(fixture: &Value) -> Model {
    Model::from_dag(&fixture["snapshot"]).expect("snapshot")
}

/// The fixture's snapshot with only these nodes, in this order.
fn snapshot_of(fixture: &Value, shorts: &[&str]) -> Model {
    let mut dag = fixture["snapshot"].clone();
    let all = dag["nodes"].as_array().unwrap().clone();
    let pick = |short: &&str| {
        all.iter()
            .find(|n| n["ref"]["short"] == **short)
            .unwrap()
            .clone()
    };
    dag["nodes"] = Value::Array(shorts.iter().map(pick).collect());
    dag["seq"] = Value::from(50);
    Model::from_dag(&dag).expect("snapshot")
}

fn open(cx: &mut TestAppContext) -> (AnyWindowHandle, Entity<Dashboard>) {
    cx.update(gpui_kit::init);
    cx.update(|cx| {
        let options = WindowOptions {
            window_bounds: Some(WindowBounds::Windowed(Bounds {
                origin: Point::default(),
                size: size(px(1400.), px(600.)),
            })),
            ..Default::default()
        };
        gpui_kit::open_window(options, cx, |window, cx| {
            cx.new(|cx| Dashboard::new("/tmp/x.http", window, cx))
        })
        .expect("open the test window")
    })
}

fn feed(
    cx: &mut TestAppContext,
    window: AnyWindowHandle,
    dashboard: &Entity<Dashboard>,
    updates: Vec<Update>,
) {
    cx.update_window(window, |_, window, cx| {
        dashboard.update(cx, |dashboard, cx| dashboard.apply(updates, cx));
        window.render_frame(cx);
    })
    .expect("the window is open");
}

/// A row of cells padded the way `salmon-tui` pads its line.
fn padded(cells: &[String]) -> String {
    format!(
        "{:<10} {:<22} {:<4} {:<9} {:<12} {}",
        cells[0], cells[1], cells[2], cells[3], cells[4], cells[5]
    )
}

#[gpui_kit::test]
fn the_table_shows_the_recorded_pass_as_the_terminal_does(cx: &mut TestAppContext) {
    let fixture = fixture();
    let (window, dashboard) = open(cx);

    feed(
        cx,
        window,
        &dashboard,
        vec![Update::Connection(Connection::Connecting)],
    );
    cx.update(|cx| {
        assert_eq!(dashboard.read(cx).header(), "/tmp/x.http [connecting]");
        let (headers, rows) = dashboard.read(cx).table().read(cx).dump(cx);
        assert_eq!(headers, COLUMNS.map(|(_, name, _)| name.to_string()));
        assert!(rows.is_empty());
    });

    // the snapshot, then the events one at a time, as the feed sends them
    feed(
        cx,
        window,
        &dashboard,
        vec![Update::Snapshot(snapshot(&fixture))],
    );
    for event in fixture["recorded"].as_array().unwrap() {
        feed(
            cx,
            window,
            &dashboard,
            vec![Update::Event(Event::of(event.clone()))],
        );
    }
    cx.update(|cx| {
        let (_, rows) = dashboard.read(cx).table().read(cx).dump(cx);
        let shown: Vec<Value> = rows
            .iter()
            .map(|cells| Value::from(padded(cells)))
            .collect();
        assert_eq!(
            Value::Array(shown),
            fixture["expect"]["rows"],
            "the cells are the terminal's row"
        );
        let header = fixture["expect"]["header"].as_str().unwrap().trim_end();
        assert_eq!(dashboard.read(cx).header(), format!("{header} [live]"));
        assert!(
            dashboard.read(cx).detail().is_empty(),
            "nothing is selected yet"
        );
    });

    // the same updates in one batch end in the same table
    let (window_2, batched) = open(cx);
    let mut all = vec![Update::Snapshot(snapshot(&fixture))];
    all.extend(
        fixture["recorded"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| Update::Event(Event::of(e.clone()))),
    );
    feed(cx, window_2, &batched, all);
    cx.update(|cx| {
        assert_eq!(
            batched.read(cx).table().read(cx).dump(cx),
            dashboard.read(cx).table().read(cx).dump(cx)
        );
    });
}

#[gpui_kit::test]
fn the_panel_follows_the_selected_node_wherever_its_row_goes(cx: &mut TestAppContext) {
    let fixture = fixture();
    let (window, dashboard) = open(cx);
    let mut all = vec![Update::Snapshot(snapshot(&fixture))];
    all.extend(
        fixture["recorded"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| Update::Event(Event::of(e.clone()))),
    );
    feed(cx, window, &dashboard, all);

    // select the second row: n2, the one that failed
    cx.update_window(window, |_, window, cx| {
        let table = dashboard.read(cx).table().clone();
        table.update(cx, |table, cx| table.set_selected_row(1, cx));
        window.render_frame(cx);
    })
    .unwrap();
    cx.update(|cx| {
        let detail = dashboard.read(cx).detail();
        let section = |name: &str| {
            detail
                .iter()
                .find(|(heading, _)| *heading == name)
                .map(|(_, lines)| lines.clone())
        };
        assert_eq!(
            section("Node"),
            Some(vec!["node-two".to_string(), "full-n2".to_string()])
        );
        assert_eq!(
            section("State"),
            Some(vec!["wanted up, errored".to_string()])
        );
        assert_eq!(section("Error"), Some(vec!["boom".to_string()]));
        assert_eq!(
            section("Last event"),
            Some(vec!["failed #11: boom".to_string()])
        );
        assert_eq!(section("Last check"), Some(vec!["-".to_string()]));
        assert_eq!(section("Paths"), Some(vec!["/n2".to_string()]));
        assert_eq!(
            section("Output"),
            None,
            "a section with nothing in it is not drawn"
        );
    });

    // an event about the selected node shows in the panel
    let healed = serde_json::json!({
        "stream": "updown", "kind": "done", "seq": 30,
        "ref": {"short": "n2", "full": "full-n2"},
    });
    feed(
        cx,
        window,
        &dashboard,
        vec![Update::Event(Event::of(healed))],
    );
    cx.update(|cx| {
        let detail = dashboard.read(cx).detail();
        assert!(detail.contains(&("State", vec!["wanted up, converged".to_string()])));
        assert!(detail.iter().all(|(heading, _)| *heading != "Error"));
    });

    // a re-read snapshot that moves the node moves the selection with it
    feed(
        cx,
        window,
        &dashboard,
        vec![Update::Snapshot(snapshot_of(
            &fixture,
            &["n2", "n1", "root"],
        ))],
    );
    cx.update(|cx| {
        assert_eq!(dashboard.read(cx).table().read(cx).selected_row(), Some(0));
        assert_eq!(
            dashboard.read(cx).selected().map(|n| n.id.short.as_str()),
            Some("n2")
        );
    });

    // and one that no longer has it leaves nothing selected
    feed(
        cx,
        window,
        &dashboard,
        vec![Update::Snapshot(snapshot_of(&fixture, &["n1", "root"]))],
    );
    cx.update(|cx| {
        assert_eq!(dashboard.read(cx).table().read(cx).selected_row(), None);
        assert!(dashboard.read(cx).detail().is_empty());
    });

    // a lost stream keeps what was last read on screen, and says so
    feed(
        cx,
        window,
        &dashboard,
        vec![Update::Connection(Connection::Lost(
            "the stream ended".into(),
        ))],
    );
    cx.update(|cx| {
        let header = dashboard.read(cx).header();
        assert!(
            header.ends_with("[reconnecting (the stream ended)]"),
            "{header}"
        );
        assert_eq!(dashboard.read(cx).table().read(cx).dump(cx).1.len(), 2);
    });
}
