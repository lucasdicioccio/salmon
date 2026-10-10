//! Experimental: a native window on salmon's `run serve --http`, built with
//! gpui-kit. A third client of the endpoints the web UI and `salmon-tui`
//! already read, and **read-only**: it issues `GET /dag` and `GET /events`
//! and nothing else, so opening it never stands a tending machine down.
//!
//! Everything about the protocol is in `salmon-serve-client`; this crate is
//! the drawing. [`feed`] runs the client on a thread of its own and hands
//! what it reads to the window; [`dashboard`] is the window: the node table,
//! and a panel for the selected node.
//!
//! Not here, on purpose, each a step of its own: the layered DAG drawing,
//! and anything that writes (`force`/`recheck`/`pause`/`resume`, `converge`,
//! a command line).

pub mod dashboard;
pub mod feed;
