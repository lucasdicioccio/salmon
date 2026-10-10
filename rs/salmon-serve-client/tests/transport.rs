//! The transport against a canned server in this process: a unix socket,
//! and a TLS listener with a throwaway certificate.
//!
//! The server here is a stand-in that answers the way `run serve --http`
//! frames its answers (a JSON body with a length, a chunked event stream).
//! It is not salmon: what a real server sends is covered by running the
//! `follow` example against one, which the module notes record.

use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpListener;
use std::os::unix::net::UnixListener;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::thread;

use salmon_serve_client::follow::{follow, Change};
use salmon_serve_client::http::{Client, ClientError};
use salmon_serve_client::model::Pass;
use serde_json::{json, Value};

const TOKEN: &str = "s3cr3t-token-for-the-test";

fn fixture() -> Value {
    serde_json::from_str(include_str!("../../fixtures/client-model.json"))
        .expect("the fixture is JSON")
}

/// A directory of this test's own, removed when dropped.
struct Scratch(PathBuf);

impl Scratch {
    fn new(name: &str) -> Scratch {
        let dir =
            std::env::temp_dir().join(format!("salmon-serve-client-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("scratch directory");
        Scratch(dir)
    }

    fn file(&self, name: &str, content: &str) -> PathBuf {
        let path = self.0.join(name);
        std::fs::write(&path, content).expect("scratch file");
        path
    }
}

impl Scratch {
    /// A file only its owner can read, as a token file must be.
    fn secret(&self, name: &str, content: &str) -> PathBuf {
        use std::os::unix::fs::PermissionsExt;
        let path = self.file(name, content);
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).expect("chmod");
        path
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// What the canned server saw: one entry per request, the request line
/// first, then its headers.
type Seen = Arc<Mutex<Vec<Vec<String>>>>;

/// Answer one request on the stream. `/dag` answers the fixture's snapshot
/// (at `seq` 20 once the stream has been opened, as a re-read after a
/// `declared` would be); `/events` answers the fixture's events above `since`, chunked.
fn answer(mut stream: impl Read + Write, seen: &Seen, require_token: bool) {
    let mut reader = BufReader::new(&mut stream);
    let mut request = Vec::new();
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).unwrap_or(0) == 0 || line.trim().is_empty() {
            break;
        }
        request.push(line.trim().to_string());
    }
    drop(reader);
    if request.is_empty() {
        return;
    }
    let route = request[0].split(' ').nth(1).unwrap_or("").to_string();
    let authorized = request
        .iter()
        .any(|h| h == &format!("Authorization: Bearer {TOKEN}"));
    let streaming = {
        let mut seen = seen.lock().unwrap();
        seen.push(request);
        seen.iter().any(|r| r[0].starts_with("GET /events"))
    };
    let fixture = fixture();
    if require_token && !authorized {
        let body = r#"{"error":"a bearer token is required"}"#;
        let _ = write!(stream, "HTTP/1.1 401 Unauthorized\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{body}", body.len());
    } else if route == "/dag" {
        let mut snapshot = fixture["snapshot"].clone();
        if streaming {
            snapshot["seq"] = json!(20);
        }
        let body = snapshot.to_string();
        let _ = write!(
            stream,
            "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{body}",
            body.len()
        );
    } else if let Some(since) = route.strip_prefix("/events?since=") {
        let since: u64 = since.parse().expect("since is a number");
        let _ = write!(stream, "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\n\r\n");
        let mut blocks = vec![": keep-alive\n\n".to_string()];
        for event in fixture["recorded"].as_array().unwrap() {
            match event["seq"].as_u64() {
                Some(seq) if seq > since => blocks.push(format!("id: {seq}\ndata: {event}\n\n")),
                Some(_) => {}
                None => {} // the gap is not part of this pass
            }
        }
        // split one block across two chunks, as a network would
        for block in blocks {
            let (a, b) = block.split_at(block.len() / 2);
            for piece in [a, b] {
                let _ = write!(stream, "{:x}\r\n{piece}\r\n", piece.len());
                let _ = stream.flush();
            }
        }
        let _ = write!(stream, "0\r\n\r\n");
    } else {
        let body = r#"{"error":"no such route"}"#;
        let _ = write!(
            stream,
            "HTTP/1.1 404 Not Found\r\nContent-Length: {}\r\n\r\n{body}",
            body.len()
        );
    }
    let _ = stream.flush();
}

fn unix_server(path: &Path) -> Seen {
    let listener = UnixListener::bind(path).expect("bind the test socket");
    let seen: Seen = Arc::default();
    let theirs = seen.clone();
    thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            let seen = theirs.clone();
            thread::spawn(move || answer(stream, &seen, false));
        }
    });
    seen
}

#[test]
fn over_the_unix_socket_the_snapshot_then_the_stream_from_its_seq() {
    let scratch = Scratch::new("unix");
    let path = scratch.0.join("serve.http");
    let seen = unix_server(&path);
    let client = Client::unix(&path);
    assert_eq!(client.target(), path.display().to_string());

    let dag = client.dag().expect("/dag");
    assert_eq!(dag["seq"], json!(5));

    let mut changes = Vec::new();
    let model = follow(&client, None, |_, change| {
        changes.push(match change {
            Change::Snapshot(None) => "snapshot".to_string(),
            Change::Snapshot(Some(why)) => format!("re-read: {why}"),
            Change::Event(e) => e.kind.clone(),
        });
        true
    })
    .unwrap_or_else(|(_, e)| panic!("follow: {e}"));

    // the declared (6) asked for a re-read, which answered at seq 20: the
    // node events of the pass (8..15) are below the new snapshot's stamp and
    // fall away, while the loop's own part still takes the stop (13)
    assert_eq!(
        changes,
        [
            "snapshot",
            "declared",
            "re-read: epoch 0 declared up",
            "converge-start",
            "eval",
            "done",
            "eval",
            "failed",
            "blocked",
            "converge-stop",
            "parked",
            "next-look",
            "hung-up"
        ]
    );
    assert_eq!(model.seq, 20);
    assert_eq!(
        model.pass,
        Some(Pass::Stopped {
            ok: false,
            remaining: 2
        })
    );
    assert!(model.nodes.values().all(|n| n.convergence == "pending"));

    let seen = seen.lock().unwrap();
    let lines: Vec<&str> = seen.iter().map(|r| r[0].as_str()).collect();
    assert_eq!(
        lines,
        [
            "GET /dag HTTP/1.1",
            "GET /dag HTTP/1.1",
            "GET /events?since=5 HTTP/1.1",
            "GET /dag HTTP/1.1"
        ]
    );
    assert!(
        seen.iter()
            .flatten()
            .all(|h| !h.to_ascii_lowercase().starts_with("authorization")),
        "nothing but the request goes on the unix socket"
    );
}

#[test]
fn a_socket_nobody_listens_on_and_a_route_that_is_refused_are_errors_in_words() {
    let scratch = Scratch::new("refused");
    let nobody = Client::unix(scratch.0.join("nobody.http"));
    assert!(matches!(nobody.dag(), Err(ClientError::Io(_))));
    let failed = follow(&nobody, None, |_, _| true);
    assert!(matches!(failed, Err((None, ClientError::Io(_)))));
}

// --- TLS -----------------------------------------------------------------------

struct TlsServer {
    url: String,
    cert_pem: String,
    seen: Seen,
}

fn tls_server(is_ca: bool) -> TlsServer {
    let key = rcgen::KeyPair::generate().expect("a key");
    let mut params = rcgen::CertificateParams::new(vec!["localhost".to_string()]).expect("params");
    if is_ca {
        // what `openssl req -x509` makes by default: a self-signed
        // certificate that also says it is a CA
        params.is_ca = rcgen::IsCa::Ca(rcgen::BasicConstraints::Unconstrained);
    }
    let cert = params.self_signed(&key).expect("a certificate");
    let provider = Arc::new(rustls::crypto::ring::default_provider());
    let config = rustls::ServerConfig::builder_with_provider(provider)
        .with_safe_default_protocol_versions()
        .unwrap()
        .with_no_client_auth()
        .with_single_cert(
            vec![cert.der().clone()],
            rustls::pki_types::PrivateKeyDer::Pkcs8(key.serialize_der().into()),
        )
        .expect("server config");
    let config = Arc::new(config);
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
    let port = listener.local_addr().unwrap().port();
    let seen: Seen = Arc::default();
    let theirs = seen.clone();
    thread::spawn(move || {
        for tcp in listener.incoming().flatten() {
            let (config, seen) = (config.clone(), theirs.clone());
            thread::spawn(move || {
                let connection = rustls::ServerConnection::new(config).expect("server connection");
                let mut tls = rustls::StreamOwned::new(connection, tcp);
                answer(&mut tls, &seen, true);
                tls.conn.send_close_notify();
                let _ = tls.flush();
            });
        }
    });
    TlsServer {
        url: format!("https://localhost:{port}"),
        cert_pem: cert.pem(),
        seen,
    }
}

#[test]
fn over_tls_the_token_goes_to_the_pinned_certificate_and_to_nobody_else() {
    let scratch = Scratch::new("tls");
    let token_file = scratch.secret("token", &format!("{TOKEN}\n"));
    let server = tls_server(false);
    let cacert = scratch.file("server.pem", &server.cert_pem);

    // the server's own certificate is the only one trusted: it answers
    let client = Client::tls(&server.url, &token_file, Some(&cacert)).expect("a client");
    assert_eq!(client.target(), server.url);
    assert!(
        !format!("{client:?}").contains(TOKEN),
        "the token is not in the client's Debug"
    );
    assert_eq!(client.dag().expect("/dag over TLS")["seq"], json!(5));
    let model =
        follow(&client, None, |_, _| true).unwrap_or_else(|(_, e)| panic!("follow over TLS: {e}"));
    assert_eq!(
        model.pass,
        Some(Pass::Stopped {
            ok: false,
            remaining: 2
        })
    );
    {
        let seen = server.seen.lock().unwrap();
        assert!(!seen.is_empty());
        assert!(
            seen.iter()
                .all(|r| r.contains(&format!("Authorization: Bearer {TOKEN}"))),
            "every request carries the token, /events included"
        );
    }

    // another certificate pinned: the handshake fails, and no request (so
    // no token) was sent
    let other = tls_server(false);
    let before = other.seen.lock().unwrap().len();
    let wrong = Client::tls(&other.url, &token_file, Some(&cacert)).expect("a client");
    let refused = wrong
        .dag()
        .expect_err("a certificate that is not the pinned one");
    assert!(matches!(refused, ClientError::Tls(_)), "{refused:?}");
    assert!(!refused.to_string().contains(TOKEN));
    assert_eq!(
        other.seen.lock().unwrap().len(),
        before,
        "nothing was sent to it"
    );

    // no --cacert: the system's store does not know a self-signed certificate
    let system = Client::tls(&server.url, &token_file, None).expect("a client");
    assert!(matches!(system.dag(), Err(ClientError::Tls(_))));

    // a wrong token is the server's 401, with its words
    let wrong_token = scratch.secret("wrong-token", "not-the-token");
    let unauthorized = Client::tls(&server.url, &wrong_token, Some(&cacert))
        .expect("a client")
        .dag();
    match unauthorized {
        Err(ClientError::Refused(401, text)) => assert_eq!(text, "a bearer token is required"),
        other => panic!("expected a 401, got {other:?}"),
    }
}

#[test]
fn a_self_signed_certificate_that_says_it_is_a_ca_can_be_pinned_too() {
    let scratch = Scratch::new("tls-ca");
    let token_file = scratch.secret("token", TOKEN);
    let server = tls_server(true);
    let cacert = scratch.file("server.pem", &server.cert_pem);
    let client = Client::tls(&server.url, &token_file, Some(&cacert)).expect("a client");
    assert_eq!(client.dag().expect("/dag over TLS")["seq"], json!(5));

    // pinned is not trusted in general: another server holding another such
    // certificate is refused, and so is this one under a name it does not carry
    let other = tls_server(true);
    let wrong = Client::tls(&other.url, &token_file, Some(&cacert)).expect("a client");
    assert!(matches!(wrong.dag(), Err(ClientError::Tls(_))));
    assert!(
        other.seen.lock().unwrap().is_empty(),
        "nothing was sent to it"
    );
    let by_address = server.url.replace("localhost", "127.0.0.1");
    let misnamed = Client::tls(&by_address, &token_file, Some(&cacert)).expect("a client");
    assert!(matches!(misnamed.dag(), Err(ClientError::Tls(_))));
    assert_eq!(
        server.seen.lock().unwrap().len(),
        1,
        "the misnamed request was never sent"
    );
}

#[test]
fn what_is_not_a_tls_target_is_refused_before_anything_is_sent() {
    let scratch = Scratch::new("bad-target");
    let token_file = scratch.secret("token", TOKEN);
    let refused = |r: Result<Client, ClientError>| match r {
        Err(ClientError::BadTarget(why)) => {
            assert!(!why.contains(TOKEN));
            why
        }
        other => panic!("expected a refusal, got {other:?}"),
    };
    assert!(
        refused(Client::tls("http://localhost:1", &token_file, None))
            .contains("not an https://HOST:PORT address")
    );
    assert!(refused(Client::tls(
        "https://localhost:1",
        &scratch.0.join("absent"),
        None
    ))
    .contains("cannot read the token file"));
    assert!(refused(Client::tls(
        "https://localhost:1",
        &scratch.secret("empty", "\n"),
        None
    ))
    .contains("is empty"));
    assert!(refused(Client::tls(
        "https://localhost:1",
        &scratch.secret("two", "a\nb\n"),
        None
    ))
    .contains("more than one word"));
    let for_all = scratch.file("for-all", TOKEN);
    std::fs::set_permissions(
        &for_all,
        std::os::unix::fs::PermissionsExt::from_mode(0o644),
    )
    .unwrap();
    assert!(
        refused(Client::tls("https://localhost:1", &for_all, None)).contains("readable by others")
    );
    let not_a_cert = scratch.file("not-a-cert.pem", "hello\n");
    assert!(refused(Client::tls(
        "https://localhost:1",
        &token_file,
        Some(&not_a_cert)
    ))
    .contains("no certificate in"));
}
