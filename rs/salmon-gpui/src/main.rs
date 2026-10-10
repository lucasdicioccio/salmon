//! `salmon-gpui PATH`, or `salmon-gpui https://HOST:PORT --token-file FILE
//! [--cacert FILE]`: the arguments `salmon-tui` takes, for the same servers.

use std::process::ExitCode;

use gpui_kit::{px, size, AppContext as _, Bounds, WindowBounds, WindowOptions};
use salmon_gpui::dashboard::Dashboard;
use salmon_gpui::feed;
use salmon_serve_client::http::{Client, USAGE};

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    // refused before a window opens: a wrong address, a token file others
    // can read, a certificate file with no certificate in it
    let client = match Client::from_args(&args) {
        Ok(client) => client,
        Err(why) => {
            eprintln!("usage: salmon-gpui {USAGE}\n{why}");
            return ExitCode::from(2);
        }
    };
    gpui_kit::application().run(move |cx| {
        gpui_kit::init(cx);
        let target = client.target().to_string();
        let options = WindowOptions {
            window_bounds: Some(WindowBounds::Windowed(Bounds::centered(
                None,
                size(px(1400.), px(800.)),
                cx,
            ))),
            ..Default::default()
        };
        let (_, dashboard) = gpui_kit::open_window(options, cx, |window, cx| {
            cx.new(|cx| Dashboard::new(target, window, cx))
        })
        .expect("failed to open the window");
        // one window, and the process is the window
        cx.on_window_closed(|cx, _| cx.quit()).detach();

        let (send, receive) = async_channel::unbounded();
        feed::spawn(client, send);
        cx.spawn(async move |cx| {
            while let Ok(first) = receive.recv().await {
                // whatever else has arrived goes in the same redraw
                let mut updates = vec![first];
                while let Ok(more) = receive.try_recv() {
                    updates.push(more);
                }
                dashboard.update(cx, |dashboard, cx| dashboard.apply(updates, cx));
            }
        })
        .detach();
    });
    ExitCode::SUCCESS
}
