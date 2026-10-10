//! The loop every reader runs: read `/dag`, follow `/events` from that
//! snapshot's `seq`, fold each event, and re-read the snapshot when the
//! model asks (a `declared`, a `cleared`, a `gap`).
//!
//! It is what `salmon-tui` does around its terminal, minus the terminal: the
//! caller gets the model after every change and decides what to draw.

use crate::http::{Client, ClientError};
use crate::model::{Event, Model};

/// Why the observer is being called.
#[derive(Debug, Clone, PartialEq)]
pub enum Change<'a> {
    /// A snapshot was read: the first one, or a re-read the model asked for
    /// (with the reason it gave).
    Snapshot(Option<&'a str>),
    /// An event was folded in.
    Event(&'a Event),
}

/// Follow one server until the observer answers `false`, the server ends the
/// stream, or a request fails.
///
/// `resume` is a model from an earlier call, if any: the stream is resumed
/// from its cursor and its loop-level fields are kept, the way a re-read
/// snapshot joins a model that has been folding. Returns the model as it
/// stood when the stream ended, to be passed back in when reconnecting.
pub fn follow(
    client: &Client,
    resume: Option<Model>,
    mut observe: impl FnMut(&Model, Change<'_>) -> bool,
) -> Result<Model, (Option<Model>, ClientError)> {
    let fresh = match read(client) {
        Ok(fresh) => fresh,
        Err(e) => return Err((resume, e)),
    };
    let mut model = join(resume.as_ref(), fresh);
    if !observe(&model, Change::Snapshot(None)) {
        return Ok(model);
    }
    let mut failure = None;
    let since = model.seq;
    let streamed = client.events(Some(since), |event| {
        model.step(&event);
        if !observe(&model, Change::Event(&event)) {
            return false;
        }
        if let Some(reason) = model.resync.clone() {
            match read(client) {
                Ok(fresh) => model = Model::rebase(&model, fresh),
                Err(e) => {
                    failure = Some(e);
                    return false;
                }
            }
            return observe(&model, Change::Snapshot(Some(&reason)));
        }
        true
    });
    match (failure, streamed) {
        (Some(e), _) | (None, Err(e)) => Err((Some(model), e)),
        (None, Ok(())) => Ok(model),
    }
}

/// A snapshot read on reconnecting, joined to the model from before.
///
/// A snapshot numbered below the old cursor is a server that started over
/// (the counter is per process), and nothing of the old model applies to it:
/// keeping the old cursor would drop every event of the new server as a
/// replay. So the old model is kept only when the snapshot is at or past it.
pub fn join(old: Option<&Model>, fresh: Model) -> Model {
    match old {
        Some(old) if fresh.seq >= old.seq => Model::rebase(old, fresh),
        _ => fresh,
    }
}

fn read(client: &Client) -> Result<Model, ClientError> {
    let dag = client.dag()?;
    Model::from_dag(&dag).map_err(ClientError::Undecodable)
}
