//! The client on a thread of its own, feeding the window.
//!
//! The client is blocking I/O and the window is GPUI's foreground executor,
//! so the two meet on a channel. The thread sends the snapshot when one is
//! read and each event as it arrives; the window keeps its own copy of the
//! model and folds the events itself ([`crate::dashboard::Dashboard::apply`]),
//! so an event costs the same however many nodes there are, and a burst is
//! one redraw.

use std::thread;
use std::time::Duration;

use async_channel::Sender;
use salmon_serve_client::follow::{follow, Change};
use salmon_serve_client::http::Client;
use salmon_serve_client::model::{Event, Model};

/// How the window stands with the server.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Connection {
    Connecting,
    /// A snapshot was read and the stream is being followed.
    Live,
    /// The stream ended or a request failed, with why; retrying.
    Lost(String),
}

/// What the thread hands the window.
#[derive(Debug, Clone)]
pub enum Update {
    /// A snapshot was read (the first, or a re-read): the model to show.
    Snapshot(Model),
    /// An event to fold onto the model shown.
    Event(Event),
    Connection(Connection),
}

const RETRY: Duration = Duration::from_secs(1);

/// Follow the server for as long as someone listens on the channel,
/// reconnecting a second after the stream ends or fails, from the last
/// number seen.
pub fn spawn(client: Client, updates: Sender<Update>) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut resume = None;
        loop {
            if updates
                .send_blocking(Update::Connection(Connection::Connecting))
                .is_err()
            {
                return;
            }
            let followed = follow(&client, resume.take(), |model, change| {
                let update = match change {
                    Change::Snapshot(_) => Update::Snapshot(model.clone()),
                    Change::Event(event) => Update::Event(event.clone()),
                };
                updates.send_blocking(update).is_ok()
            });
            let why = match followed {
                Ok(model) => {
                    resume = Some(model);
                    "the stream ended".to_string()
                }
                Err((model, why)) => {
                    resume = model;
                    why.to_string()
                }
            };
            if updates
                .send_blocking(Update::Connection(Connection::Lost(why)))
                .is_err()
            {
                return;
            }
            thread::sleep(RETRY);
        }
    })
}
