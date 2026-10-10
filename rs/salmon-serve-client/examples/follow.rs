//! The client with no window: print the snapshot, then every event with the
//! row it changed. Reads only; it never types a command.
//!
//! ```sh
//! cargo run --example follow -- PATH [--once]
//! cargo run --example follow -- https://HOST:PORT --token-file FILE [--cacert FILE] [--once]
//! ```
//!
//! `--once` prints the snapshot and exits, which is also a way to check an
//! address, a token file and a certificate before opening a window on them.

use std::process::ExitCode;

use salmon_serve_client::follow::{follow, Change};
use salmon_serve_client::http::{Client, USAGE};
use salmon_serve_client::model::Model;

fn main() -> ExitCode {
    let mut args: Vec<String> = std::env::args().skip(1).collect();
    let once = args.iter().any(|a| a == "--once");
    args.retain(|a| a != "--once");
    let client = match Client::from_args(&args) {
        Ok(client) => client,
        Err(why) => {
            eprintln!("usage: follow {USAGE}\n{why}");
            return ExitCode::from(2);
        }
    };
    let print_snapshot = |model: &Model| {
        println!("{}", model.render_header(client.target()));
        for node in model.nodes_in_order() {
            println!("  {}", node.render_row());
        }
    };
    let followed = follow(&client, None, |model, change| {
        match change {
            Change::Snapshot(None) => print_snapshot(model),
            Change::Snapshot(Some(why)) => {
                println!("re-read ({why})");
                print_snapshot(model);
            }
            Change::Event(event) => {
                println!("{}", event.render_line());
                if let Some(node) = event.node.as_ref().and_then(|r| model.nodes.get(r)) {
                    println!("  {}", node.render_row());
                }
            }
        }
        !once
    });
    match followed {
        Ok(_) if once => ExitCode::SUCCESS,
        Ok(_) => {
            println!("the stream ended");
            ExitCode::SUCCESS
        }
        Err((_, why)) => {
            eprintln!("{}: {why}", client.target());
            ExitCode::FAILURE
        }
    }
}
