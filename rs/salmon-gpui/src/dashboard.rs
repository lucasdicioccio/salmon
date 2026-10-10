//! The window: a header line, the node table, and a panel for the selected
//! node.
//!
//! What is shown is the text `salmon-tui` shows, cell for cell: the table's
//! columns are [`Node::cells`] and the header is [`Model::render_header`],
//! so the fixture the two folds are tested on is also what the headless
//! window test reads back out of the table.

use gpui_kit::component::table::{Column, DataTable, TableDelegate, TableEvent, TableState};
use gpui_kit::component::{h_flex, v_flex, ActiveTheme as _};
use gpui_kit::{
    div, prelude::*, px, App, Context, Entity, Hsla, SharedString, Subscription, Window,
};
use salmon_serve_client::model::{Model, Node, RefId};

use crate::feed::{Connection, Update};

/// The table's columns: a key, the heading, and a width.
pub const COLUMNS: [(&str, &str, f32); 6] = [
    ("ref", "Ref", 110.),
    ("shorthand", "Node", 240.),
    ("direction", "Wanted", 80.),
    ("convergence", "Convergence", 120.),
    ("check", "Last check", 110.),
    ("event", "Last event", 320.),
];

const CONVERGENCE: usize = 3;
const CHECK: usize = 4;

/// The rows of the table: the model's nodes in the snapshot's order.
pub struct NodeRows {
    nodes: Vec<Node>,
}

impl NodeRows {
    fn index_of(&self, id: &RefId) -> Option<usize> {
        self.nodes.iter().position(|n| &n.id == id)
    }
}

impl TableDelegate for NodeRows {
    fn columns_count(&self, _: &App) -> usize {
        COLUMNS.len()
    }

    fn rows_count(&self, _: &App) -> usize {
        self.nodes.len()
    }

    fn column(&self, col_ix: usize, _: &App) -> Column {
        let (key, name, width) = COLUMNS[col_ix];
        Column::new(key, name).width(px(width))
    }

    fn render_td(
        &mut self,
        row_ix: usize,
        col_ix: usize,
        _: &mut Window,
        cx: &mut Context<TableState<Self>>,
    ) -> impl IntoElement {
        let text = self.cell_text(row_ix, col_ix, cx);
        let color = match col_ix {
            CONVERGENCE => convergence_color(&text, cx),
            CHECK => check_color(&text, cx),
            _ => None,
        };
        div()
            .when_some(color, |cell, color| cell.text_color(color))
            .child(text)
    }

    fn cell_text(&self, row_ix: usize, col_ix: usize, _: &App) -> String {
        self.nodes
            .get(row_ix)
            .map(|node| node.cells()[col_ix].clone())
            .unwrap_or_default()
    }
}

fn convergence_color(word: &str, cx: &App) -> Option<Hsla> {
    match word {
        "converged" => Some(cx.theme().success),
        "errored" => Some(cx.theme().danger),
        "blocked" | "stale" => Some(cx.theme().warning),
        _ => Some(cx.theme().muted_foreground),
    }
}

fn check_color(word: &str, cx: &App) -> Option<Hsla> {
    match word {
        "success" | "completed" | "skipped" => Some(cx.theme().success),
        "failure" => Some(cx.theme().danger),
        "unknown" => Some(cx.theme().warning),
        _ => Some(cx.theme().muted_foreground),
    }
}

pub struct Dashboard {
    target: SharedString,
    connection: Connection,
    model: Option<Model>,
    /// The selection is a node, not a row: rows move when a node is dropped
    /// or a snapshot is re-read.
    selected: Option<RefId>,
    table: Entity<TableState<NodeRows>>,
    _table_events: Subscription,
}

impl Dashboard {
    pub fn new(
        target: impl Into<SharedString>,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Self {
        let table = cx.new(|cx| {
            TableState::new(NodeRows { nodes: Vec::new() }, window, cx)
                .row_selectable(true)
                .col_selectable(false)
                .col_movable(false)
                .sortable(false)
        });
        let _table_events = cx.subscribe(
            &table,
            |this: &mut Self, table, event: &TableEvent, cx| match event {
                TableEvent::SelectRow(row_ix) => {
                    this.selected = table
                        .read(cx)
                        .delegate()
                        .nodes
                        .get(*row_ix)
                        .map(|n| n.id.clone());
                    cx.notify();
                }
                TableEvent::ClearSelection => {
                    this.selected = None;
                    cx.notify();
                }
                _ => {}
            },
        );
        Dashboard {
            target: target.into(),
            connection: Connection::Connecting,
            model: None,
            selected: None,
            table,
            _table_events,
        }
    }

    /// Take what the feed read, in order, and redraw once.
    pub fn apply(&mut self, updates: impl IntoIterator<Item = Update>, cx: &mut Context<Self>) {
        for update in updates {
            match update {
                Update::Snapshot(model) => {
                    self.model = Some(model);
                    self.connection = Connection::Live;
                }
                Update::Event(event) => {
                    if let Some(model) = &mut self.model {
                        model.step(&event);
                    }
                }
                Update::Connection(connection) => self.connection = connection,
            }
        }
        let nodes: Vec<Node> = self
            .model
            .as_ref()
            .map(|m| m.nodes_in_order().into_iter().cloned().collect())
            .unwrap_or_default();
        let selected = self.selected.clone();
        self.table.update(cx, |table, cx| {
            table.delegate_mut().nodes = nodes;
            // keep the selection on its node, wherever its row went
            let row = selected
                .as_ref()
                .and_then(|id| table.delegate().index_of(id));
            match (row, table.selected_row()) {
                (Some(row), Some(now)) if row == now => {}
                (Some(row), _) => table.set_selected_row(row, cx),
                (None, Some(_)) => table.clear_selection(cx),
                (None, None) => {}
            }
            cx.notify();
        });
        cx.notify();
    }

    pub fn table(&self) -> &Entity<TableState<NodeRows>> {
        &self.table
    }

    pub fn selected(&self) -> Option<&Node> {
        let model = self.model.as_ref()?;
        model.nodes.get(self.selected.as_ref()?)
    }

    /// The header line: what `salmon-tui` puts there, then the connection.
    pub fn header(&self) -> String {
        let model = match &self.model {
            Some(model) => model.render_header(&self.target),
            None => self.target.to_string(),
        };
        let connection = match &self.connection {
            Connection::Connecting => "connecting".to_string(),
            Connection::Live => "live".to_string(),
            Connection::Lost(why) => format!("reconnecting ({why})"),
        };
        format!("{} [{connection}]", model.trim_end())
    }

    /// The selected node as headed sections of lines, in the order drawn.
    /// Kept apart from the drawing so that it can be read in a test.
    pub fn detail(&self) -> Vec<(&'static str, Vec<String>)> {
        let (Some(model), Some(node)) = (self.model.as_ref(), self.selected()) else {
            return Vec::new();
        };
        let named = |ids: &[RefId]| -> Vec<String> {
            ids.iter()
                .map(|id| match model.nodes.get(id) {
                    Some(other) => {
                        format!("{} {} ({})", id.short, other.shorthand, other.convergence)
                    }
                    None => id.short.clone(),
                })
                .collect()
        };
        let check = match &node.check {
            None => vec!["-".to_string()],
            Some(check) => match &check.reason {
                Some(reason) => vec![format!("{}: {reason}", check.verdict)],
                None => vec![check.verdict.clone()],
            },
        };
        let [_, _, _, _, _, last_event] = node.cells();
        let sections = vec![
            ("Node", vec![node.shorthand.clone(), node.id.full.clone()]),
            ("Help", vec![node.help.clone()]),
            (
                "State",
                vec![format!("wanted {}, {}", node.direction, node.convergence)],
            ),
            ("Last check", check),
            ("Last event", vec![last_event]),
            ("Error", node.error.iter().cloned().collect()),
            ("Notes", node.notes.clone()),
            ("Depends on", named(&node.dependencies)),
            ("Needed by", named(&node.dependants)),
            ("Paths", node.paths.clone()),
            ("Output", node.output.clone()),
        ];
        sections
            .into_iter()
            .filter(|(_, lines)| lines.iter().any(|line| !line.is_empty()))
            .collect()
    }
}

impl Render for Dashboard {
    fn render(&mut self, _: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let theme = cx.theme();
        let (border, muted, mono) = (
            theme.border,
            theme.muted_foreground,
            theme.mono_font_family.clone(),
        );
        let lost = matches!(self.connection, Connection::Lost(_));
        let warning = theme.warning;
        let detail = self.detail();

        let panel = v_flex()
            .id("detail")
            .w(px(400.))
            .h_full()
            .flex_shrink_0()
            .overflow_y_scroll()
            .border_l_1()
            .border_color(border)
            .p_3()
            .gap_3()
            .when(detail.is_empty(), |panel| {
                panel.child(
                    div()
                        .text_color(muted)
                        .child("Select a node to see what it is and what it last said."),
                )
            })
            .children(detail.into_iter().map(|(heading, lines)| {
                let output = heading == "Output" || heading == "Paths";
                v_flex()
                    .gap_1()
                    .child(div().text_xs().text_color(muted).child(heading))
                    .children(lines.into_iter().map(|line| {
                        div()
                            .text_sm()
                            .when(output, |line| line.font_family(mono.clone()))
                            .child(line)
                    }))
            }));

        v_flex()
            .size_full()
            .child(
                div()
                    .id("header")
                    .w_full()
                    .px_3()
                    .py_2()
                    .border_b_1()
                    .border_color(border)
                    .text_sm()
                    .font_family(mono.clone())
                    .when(lost, |header| header.text_color(warning))
                    .child(self.header()),
            )
            .child(
                h_flex()
                    .flex_1()
                    .min_h_0()
                    .w_full()
                    .items_start()
                    .child(
                        div()
                            .flex_1()
                            .min_w_0()
                            .h_full()
                            .child(DataTable::new(&self.table).stripe(true).bordered(false)),
                    )
                    .child(panel),
            )
    }
}
