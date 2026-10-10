//! A read-only client of salmon's `run serve --http PATH` (or `--http-tcp
//! HOST:PORT`), with no window in it.
//!
//! This is a port of the two Haskell modules a terminal client is built on,
//! `Salmon.Client.Model` and the read half of `Salmon.Client.Http`:
//!
//! * [`model`] is the pure part: a `/dag` snapshot with the `/events` stream
//!   folded onto it, one view per node. It is tested against the same wire
//!   data as the Haskell fold (`rs/fixtures/client-model.json`, which
//!   `Test/ClientModelSpec.hs` checks against its own recorded pass).
//! * [`sse`] splits the event stream into blocks.
//! * [`http`] fetches the two inputs, over the unix socket or over TLS with a
//!   bearer token. It only ever sends `GET`: there is no command here.
//! * [`follow`] ties them together: read `/dag`, follow `/events` from that
//!   snapshot's `seq`, re-read when the model asks.
//!
//! The design constraint is the Haskell client's: **the client holds no state
//! the server does not**. Everything is derived from `/dag` and `/events`, so
//! a restart is one `/dag` read, and a client that has fallen behind re-reads
//! rather than guessing.

pub mod follow;
pub mod http;
pub mod model;
pub mod sse;
