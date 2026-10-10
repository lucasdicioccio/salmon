//! Fetching the two inputs of the model: `GET /dag` and `GET /events`.
//!
//! Two ways to reach a server, and no third, as in `Salmon.Client.Http`:
//!
//! * [`Client::unix`] for `--http PATH`, where the socket's permissions are
//!   the whole access story and nothing is sent but the request;
//! * [`Client::tls`] for `--http-tcp HOST:PORT`, which is only ever TLS with
//!   a bearer token. There is no constructor for plain HTTP over TCP. The
//!   server's certificate is verified against the certificates in `--cacert`
//!   alone when given, against the system's store otherwise, and there is no
//!   switch that turns verification off: the token is sent on every request,
//!   and a client that would send it to anyone is how it leaks.
//!
//! **Pinning a self-signed certificate.** The certificate the tree mints for
//! a listener (`Certificates.certificateAuthority`, `openssl req -x509`) is
//! self-signed *and* says it is a CA, and the verifier this library uses
//! refuses a CA presented as a server (`CaUsedAsEndEntity`). So a server
//! certificate that is byte for byte one of those in `--cacert` is accepted
//! as the pin it is (`Pinned`): the name is still checked and the handshake
//! still proves the server holds its key, but its validity dates are not
//! looked at, which the Haskell client does. Any other certificate goes
//! through the ordinary verification with `--cacert` as the only roots.
//!
//! **This client only reads.** Both routes bypass the loop's inbox and never
//! stand a tending machine down; there is no `POST /command` here.
//!
//! **The token is never printed.** It lives in a type whose `Debug` is
//! redacted, it is read from a file and never from the command line, and no
//! error this module builds contains it.
//!
//! The HTTP here is deliberately small: one request per connection
//! (`Connection: close`), blocking I/O, a body framed by `Content-Length`,
//! by chunks, or by the connection ending. Sockets are close-on-exec, which
//! is what the standard library does for every descriptor it opens.

use std::fmt;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use rustls::client::danger::{HandshakeSignatureValid, ServerCertVerified, ServerCertVerifier};
use rustls::client::WebPkiServerVerifier;
use rustls::pki_types::pem::PemObject;
use rustls::pki_types::{CertificateDer, ServerName, UnixTime};
use serde_json::Value;

use crate::model::Event;
use crate::sse::{self, Block};

/// The arguments [`Client::from_args`] reads.
pub const USAGE: &str = "PATH   (`run serve --http PATH`)\n       https://HOST:PORT --token-file FILE [--cacert FILE]   (`run serve --http-tcp HOST:PORT`)";

const CONNECT_TIMEOUT: Duration = Duration::from_secs(10);
/// For a read that is one answer. The stream has no timeout: it is open for
/// as long as the loop runs, and the server's keep-alive is what shows it is.
const READ_TIMEOUT: Duration = Duration::from_secs(30);

/// What went wrong, in words fit for a status line. Never contains the token.
#[derive(Debug)]
pub enum ClientError {
    /// Something this client will not connect or send a token to: not an
    /// `https://HOST:PORT` address, a CA file with no certificate in it, a
    /// token file that others can read, is empty, or holds more than a word.
    BadTarget(String),
    /// The connection could not be made, or broke.
    Io(io::Error),
    /// The TLS handshake was refused, the certificate included.
    Tls(String),
    /// A non-2xx status, with the `error` text the server put in the body.
    Refused(u16, String),
    /// An answer that was not the HTTP or the JSON expected.
    Undecodable(String),
}

impl fmt::Display for ClientError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClientError::BadTarget(why) => write!(f, "{why}"),
            ClientError::Io(e) => write!(f, "{e}"),
            ClientError::Tls(why) => write!(f, "TLS: {why}"),
            ClientError::Refused(code, text) => write!(f, "the server answered {code}: {text}"),
            ClientError::Undecodable(why) => write!(f, "{why}"),
        }
    }
}

impl std::error::Error for ClientError {}

impl From<io::Error> for ClientError {
    fn from(e: io::Error) -> Self {
        // rustls reports a failed handshake through the stream's I/O error
        match e
            .get_ref()
            .and_then(|inner| inner.downcast_ref::<rustls::Error>())
        {
            Some(tls) => ClientError::Tls(tls.to_string()),
            None => ClientError::Io(e),
        }
    }
}

/// The content of the server's `--token-file`. Its `Debug` says nothing.
#[derive(Clone)]
struct Token(String);

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<token>")
    }
}

#[derive(Debug, Clone)]
enum Target {
    Unix(PathBuf),
    Tls {
        host: String,
        port: u16,
        token: Token,
        config: Arc<rustls::ClientConfig>,
    },
}

/// A connection factory for one server.
#[derive(Debug, Clone)]
pub struct Client {
    target: Target,
    label: String,
}

impl Client {
    /// A client for the unix socket at the path. Nothing is checked until
    /// the first request.
    pub fn unix(path: impl Into<PathBuf>) -> Client {
        let path = path.into();
        Client {
            label: path.display().to_string(),
            target: Target::Unix(path),
        }
    }

    /// A client for an `--http-tcp` listener. Refuses a URL that is not
    /// `https://HOST:PORT` rather than sending a token in the clear, a CA
    /// file with no certificate in it, and a token file that could not be a
    /// token. The token goes in an `Authorization: Bearer` header on every
    /// request, `/events` included.
    pub fn tls(url: &str, token_file: &Path, cacert: Option<&Path>) -> Result<Client, ClientError> {
        let (host, port) = parse_https(url)?;
        let token = read_token(token_file)?;
        let provider = Arc::new(rustls::crypto::ring::default_provider());
        let mut roots = rustls::RootCertStore::empty();
        let mut pins = Vec::new();
        match cacert {
            Some(file) => {
                let unreadable = |e: rustls::pki_types::pem::Error| {
                    ClientError::BadTarget(format!("cannot read {}: {e}", file.display()))
                };
                for cert in CertificateDer::pem_file_iter(file).map_err(unreadable)? {
                    let cert = cert.map_err(unreadable)?;
                    roots.add(cert.clone()).map_err(|e| {
                        ClientError::BadTarget(format!("a certificate in {}: {e}", file.display()))
                    })?;
                    pins.push(cert);
                }
                if roots.is_empty() {
                    return Err(ClientError::BadTarget(format!(
                        "no certificate in {}",
                        file.display()
                    )));
                }
            }
            None => {
                // certificates the system has that this library cannot read
                // are skipped; with none left there is nothing to verify with
                let (_added, _skipped) =
                    roots.add_parsable_certificates(rustls_native_certs::load_native_certs().certs);
                if roots.is_empty() {
                    return Err(ClientError::BadTarget(
                        "the system has no certificate store to verify the server with; name one with --cacert".to_string(),
                    ));
                }
            }
        }
        let ordinary =
            WebPkiServerVerifier::builder_with_provider(Arc::new(roots), provider.clone())
                .build()
                .map_err(|e| ClientError::Tls(e.to_string()))?;
        let config = rustls::ClientConfig::builder_with_provider(provider)
            .with_safe_default_protocol_versions()
            .map_err(|e| ClientError::Tls(e.to_string()))?
            .dangerous()
            .with_custom_certificate_verifier(Arc::new(Pinned { pins, ordinary }))
            .with_no_client_auth();
        Ok(Client {
            label: format!("https://{}", host_port(&host, port)),
            target: Target::Tls {
                host,
                port,
                token,
                config: Arc::new(config),
            },
        })
    }

    /// A client from a command line, the one `salmon-tui` takes:
    /// `PATH`, or `https://HOST:PORT --token-file FILE [--cacert FILE]`.
    /// The error is a message for the person who typed it. The token is only
    /// ever named by its file: an argument is visible to every process on
    /// the machine.
    pub fn from_args(args: &[String]) -> Result<Client, String> {
        let mut target = None;
        let mut token_file = None;
        let mut cacert = None;
        let mut rest = args.iter();
        while let Some(arg) = rest.next() {
            match arg.as_str() {
                "--token-file" => {
                    token_file = Some(rest.next().ok_or("--token-file needs a FILE")?)
                }
                "--cacert" => cacert = Some(rest.next().ok_or("--cacert needs a FILE")?),
                flag if flag.starts_with("--") => return Err(format!("unknown option {flag}")),
                _ if target.is_some() => {
                    return Err(format!("one server at a time: what is {arg}?"))
                }
                _ => target = Some(arg),
            }
        }
        let target =
            target.ok_or("name a server: a socket path, or an https://HOST:PORT address")?;
        if target.contains("://") {
            let token_file = token_file
                .ok_or_else(|| format!("{target} needs --token-file FILE: the listener answers nothing without the token"))?;
            Client::tls(target, Path::new(token_file), cacert.map(Path::new))
                .map_err(|e| e.to_string())
        } else if token_file.is_some() || cacert.is_some() {
            Err("--token-file and --cacert go with an https://HOST:PORT address: nothing is sent on a unix socket but the request".to_string())
        } else {
            Ok(Client::unix(target))
        }
    }

    /// What the client points at, for messages: the socket path or the URL.
    pub fn target(&self) -> &str {
        &self.label
    }

    /// `GET /dag`: the envelope, with `mode`, `seq` and `nodes`.
    pub fn dag(&self) -> Result<Value, ClientError> {
        let mut answer = self.get("/dag", Some(READ_TIMEOUT))?;
        let mut body = Vec::new();
        answer.body.read_to_end(&mut body)?;
        if !(200..300).contains(&answer.status) {
            return Err(refused(answer.status, &body));
        }
        serde_json::from_slice(&body)
            .map_err(|e| ClientError::Undecodable(format!("/dag did not answer with JSON: {e}")))
    }

    /// Open `/events` and hand every event to the callback until it answers
    /// `false`. Returns `Ok` when the callback stops it or the server ends
    /// the stream (the loop quit). `since` is `None` for live only, `Some(n)`
    /// for everything after `n` the server's ring still holds; the `gap`
    /// event arrives like any other, with no `seq`. A keep-alive is consumed
    /// here. Reconnecting is the caller's, with the last `seq` it saw.
    pub fn events(
        &self,
        since: Option<u64>,
        mut on_event: impl FnMut(Event) -> bool,
    ) -> Result<(), ClientError> {
        let route = match since {
            Some(n) => format!("/events?since={n}"),
            None => "/events".to_string(),
        };
        let mut answer = self.get(&route, None)?;
        if answer.status != 200 {
            let mut body = Vec::new();
            answer.body.read_to_end(&mut body)?;
            return Err(refused(answer.status, &body));
        }
        let mut buffer = Vec::new();
        let mut chunk = [0u8; 4096];
        loop {
            let n = match answer.body.read(&mut chunk) {
                Ok(n) => n,
                // a server that goes away mid-stream without saying so is
                // the stream ending, as far as a reader is concerned
                Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => 0,
                Err(e) => return Err(e.into()),
            };
            if n == 0 {
                return Ok(());
            }
            buffer.extend_from_slice(&chunk[..n]);
            for block in sse::split_blocks(&mut buffer) {
                if let Some(Block::Event(_, value)) = sse::parse_block(&block) {
                    if !on_event(Event::of(value)) {
                        return Ok(());
                    }
                }
            }
        }
    }

    fn get(&self, route: &str, read_timeout: Option<Duration>) -> Result<Answer, ClientError> {
        let (mut stream, host, authorization): (Box<dyn ReadWrite>, String, Option<String>) =
            match &self.target {
                Target::Unix(path) => {
                    let stream = UnixStream::connect(path)?;
                    stream.set_read_timeout(read_timeout)?;
                    (Box::new(stream), "salmon".to_string(), None)
                }
                Target::Tls {
                    host,
                    port,
                    token,
                    config,
                } => {
                    let name = ServerName::try_from(host.clone()).map_err(|e| {
                        ClientError::BadTarget(format!(
                            "{host} is not a name a certificate can carry: {e}"
                        ))
                    })?;
                    let tcp = connect_tcp(host, *port)?;
                    tcp.set_read_timeout(read_timeout)?;
                    let connection = rustls::ClientConnection::new(config.clone(), name)
                        .map_err(|e| ClientError::Tls(e.to_string()))?;
                    (
                        Box::new(rustls::StreamOwned::new(connection, tcp)),
                        host_port(host, *port),
                        Some(format!("Authorization: Bearer {}\r\n", token.0)),
                    )
                }
            };
        let request = format!(
            "GET {route} HTTP/1.1\r\nHost: {host}\r\nAccept: */*\r\nConnection: close\r\n{}\r\n",
            authorization.as_deref().unwrap_or("")
        );
        stream.write_all(request.as_bytes())?;
        stream.flush()?;
        read_answer(BufReader::new(stream))
    }
}

/// The ordinary verification, except for a server certificate that is
/// exactly one of the pinned ones. See the module header.
#[derive(Debug)]
struct Pinned {
    /// The certificates of `--cacert`; empty when the system's store decides.
    pins: Vec<CertificateDer<'static>>,
    ordinary: Arc<WebPkiServerVerifier>,
}

impl ServerCertVerifier for Pinned {
    fn verify_server_cert(
        &self,
        end_entity: &CertificateDer<'_>,
        intermediates: &[CertificateDer<'_>],
        server_name: &ServerName<'_>,
        ocsp_response: &[u8],
        now: UnixTime,
    ) -> Result<ServerCertVerified, rustls::Error> {
        if self
            .pins
            .iter()
            .any(|pin| pin.as_ref() == end_entity.as_ref())
        {
            let parsed = rustls::server::ParsedCertificate::try_from(end_entity)?;
            rustls::client::verify_server_name(&parsed, server_name)?;
            return Ok(ServerCertVerified::assertion());
        }
        self.ordinary
            .verify_server_cert(end_entity, intermediates, server_name, ocsp_response, now)
    }

    // the handshake's proof that the server holds the certificate's key is
    // never skipped, pinned or not
    fn verify_tls12_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &rustls::DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, rustls::Error> {
        self.ordinary.verify_tls12_signature(message, cert, dss)
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &rustls::DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, rustls::Error> {
        self.ordinary.verify_tls13_signature(message, cert, dss)
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        self.ordinary.supported_verify_schemes()
    }
}

trait ReadWrite: Read + Write + Send {}
impl<T: Read + Write + Send> ReadWrite for T {}

fn connect_tcp(host: &str, port: u16) -> Result<TcpStream, ClientError> {
    let mut last = None;
    for address in (host, port).to_socket_addrs()? {
        match TcpStream::connect_timeout(&address, CONNECT_TIMEOUT) {
            Ok(stream) => return Ok(stream),
            Err(e) => last = Some(e),
        }
    }
    Err(ClientError::Io(last.unwrap_or_else(|| {
        io::Error::new(
            io::ErrorKind::NotFound,
            format!("{host} resolves to no address"),
        )
    })))
}

fn host_port(host: &str, port: u16) -> String {
    if host.contains(':') {
        format!("[{host}]:{port}")
    } else {
        format!("{host}:{port}")
    }
}

/// `https://HOST[:PORT]` with trailing slashes allowed, and nothing else: no
/// other scheme, no path, no query, no credentials in the address.
fn parse_https(url: &str) -> Result<(String, u16), ClientError> {
    let bad = || ClientError::BadTarget(format!("not an https://HOST:PORT address: {url}"));
    let rest = url.strip_prefix("https://").ok_or_else(bad)?;
    let authority = rest.trim_end_matches('/');
    if authority.is_empty() || authority.contains(['/', '?', '#', '@', ' ']) {
        return Err(bad());
    }
    let (host, port) = if let Some(bracketed) = authority.strip_prefix('[') {
        // an IPv6 literal: [::1]:8443
        let (host, after) = bracketed.split_once(']').ok_or_else(bad)?;
        match after.strip_prefix(':') {
            Some(port) => (host, Some(port)),
            None if after.is_empty() => (host, None),
            None => return Err(bad()),
        }
    } else {
        match authority.rsplit_once(':') {
            Some((host, port)) => (host, Some(port)),
            None => (authority, None),
        }
    };
    let port = match port {
        Some(text) => text.parse().map_err(|_| bad())?,
        None => 443,
    };
    if host.is_empty() || host.contains(':') && !authority.starts_with('[') {
        return Err(bad());
    }
    Ok((host.to_string(), port))
}

/// The token file's content, trimmed. Refused when others can read the file,
/// as the server refuses its own (a token anyone on the box can read is not
/// one), and unless it is one word of printable characters: the token goes
/// into a header verbatim.
fn read_token(file: &Path) -> Result<Token, ClientError> {
    use std::os::unix::fs::PermissionsExt;
    let unreadable = |e: io::Error| {
        ClientError::BadTarget(format!(
            "cannot read the token file {}: {e}",
            file.display()
        ))
    };
    let mode = std::fs::metadata(file)
        .map_err(unreadable)?
        .permissions()
        .mode();
    if mode & 0o004 != 0 {
        return Err(ClientError::BadTarget(format!(
            "the token file {} is readable by others; a token anyone on the box can read is not one (chmod 600 it)",
            file.display()
        )));
    }
    let raw = std::fs::read_to_string(file).map_err(unreadable)?;
    let token = raw.trim();
    if token.is_empty() {
        return Err(ClientError::BadTarget(format!(
            "the token file {} is empty",
            file.display()
        )));
    }
    if token.chars().any(|c| c.is_control() || c.is_whitespace()) {
        return Err(ClientError::BadTarget(format!(
            "the token file {} holds more than one word",
            file.display()
        )));
    }
    Ok(Token(token.to_string()))
}

/// The server's own `{"error": ...}` text when the body is one.
fn refused(status: u16, body: &[u8]) -> ClientError {
    let text = serde_json::from_slice::<Value>(body)
        .ok()
        .and_then(|v| v.get("error").and_then(Value::as_str).map(str::to_string))
        .unwrap_or_else(|| String::from_utf8_lossy(body).trim().to_string());
    ClientError::Refused(status, text)
}

// --- the answer ---------------------------------------------------------------

struct Answer {
    status: u16,
    body: Body<BufReader<Box<dyn ReadWrite>>>,
}

fn read_answer(mut reader: BufReader<Box<dyn ReadWrite>>) -> Result<Answer, ClientError> {
    let status_line = read_line(&mut reader)?;
    let status = status_line
        .strip_prefix("HTTP/1.")
        .and_then(|rest| rest.split(' ').nth(1))
        .and_then(|code| code.parse::<u16>().ok())
        .ok_or_else(|| ClientError::Undecodable("the answer is not HTTP/1.x".to_string()))?;
    let mut framing = Framing::UntilClose;
    loop {
        let line = read_line(&mut reader)?;
        if line.is_empty() {
            break;
        }
        let Some((name, value)) = line.split_once(':') else {
            continue;
        };
        let value = value.trim();
        if name.eq_ignore_ascii_case("transfer-encoding")
            && value.to_ascii_lowercase().contains("chunked")
        {
            framing = Framing::Chunked {
                left: 0,
                first: true,
                done: false,
            };
        } else if name.eq_ignore_ascii_case("content-length")
            && matches!(framing, Framing::UntilClose)
        {
            let length = value.parse().map_err(|_| {
                ClientError::Undecodable("a Content-Length that is not a number".to_string())
            })?;
            framing = Framing::Length(length);
        }
    }
    Ok(Answer {
        status,
        body: Body { reader, framing },
    })
}

/// One header line, without its line ending. Bounded, so that something that
/// is not an HTTP server cannot make the client buffer without end.
fn read_line(reader: &mut impl BufRead) -> Result<String, ClientError> {
    let mut line = Vec::new();
    let n = reader.take(16 * 1024).read_until(b'\n', &mut line)?;
    if n == 0 {
        return Err(ClientError::Undecodable(
            "the connection ended before an answer".to_string(),
        ));
    }
    if line.last() != Some(&b'\n') {
        return Err(ClientError::Undecodable(
            "a header line with no end".to_string(),
        ));
    }
    line.pop();
    if line.last() == Some(&b'\r') {
        line.pop();
    }
    String::from_utf8(line)
        .map_err(|_| ClientError::Undecodable("a header line that is not text".to_string()))
}

enum Framing {
    Length(u64),
    Chunked { left: u64, first: bool, done: bool },
    UntilClose,
}

struct Body<R> {
    reader: R,
    framing: Framing,
}

impl<R: BufRead> Read for Body<R> {
    fn read(&mut self, out: &mut [u8]) -> io::Result<usize> {
        if out.is_empty() {
            return Ok(0);
        }
        match &mut self.framing {
            Framing::UntilClose => self.reader.read(out),
            Framing::Length(left) => {
                if *left == 0 {
                    return Ok(0);
                }
                let want = out.len().min(usize::try_from(*left).unwrap_or(usize::MAX));
                let n = self.reader.read(&mut out[..want])?;
                if n == 0 {
                    return Err(io::ErrorKind::UnexpectedEof.into());
                }
                *left -= n as u64;
                Ok(n)
            }
            Framing::Chunked { left, first, done } => {
                if *done {
                    return Ok(0);
                }
                if *left == 0 {
                    // the line ending after the previous chunk's data, then the next size
                    if !*first {
                        chunk_line(&mut self.reader)?;
                    }
                    *first = false;
                    let size_line = chunk_line(&mut self.reader)?;
                    let size = size_line.split(';').next().unwrap_or("").trim();
                    *left = u64::from_str_radix(size, 16).map_err(|_| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "a chunk size that is not a number",
                        )
                    })?;
                    if *left == 0 {
                        // trailers, up to the blank line
                        while !chunk_line(&mut self.reader)?.is_empty() {}
                        *done = true;
                        return Ok(0);
                    }
                }
                let want = out.len().min(usize::try_from(*left).unwrap_or(usize::MAX));
                let n = self.reader.read(&mut out[..want])?;
                if n == 0 {
                    return Err(io::ErrorKind::UnexpectedEof.into());
                }
                *left -= n as u64;
                Ok(n)
            }
        }
    }
}

fn chunk_line(reader: &mut impl BufRead) -> io::Result<String> {
    let mut line = Vec::new();
    let n = reader.take(1024).read_until(b'\n', &mut line)?;
    if n == 0 || line.last() != Some(&b'\n') {
        return Err(io::ErrorKind::UnexpectedEof.into());
    }
    line.pop();
    if line.last() == Some(&b'\r') {
        line.pop();
    }
    String::from_utf8(line)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "a chunk line that is not text"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_https_host_port_is_a_tls_target() {
        assert_eq!(
            parse_https("https://box.example:8443").unwrap(),
            ("box.example".to_string(), 8443)
        );
        assert_eq!(
            parse_https("https://box.example:8443//").unwrap(),
            ("box.example".to_string(), 8443)
        );
        assert_eq!(
            parse_https("https://box.example").unwrap(),
            ("box.example".to_string(), 443)
        );
        assert_eq!(
            parse_https("https://[::1]:8443").unwrap(),
            ("::1".to_string(), 8443)
        );
        for bad in [
            "http://box.example:8443",
            "box.example:8443",
            "https://",
            "https://box.example:8443/dag",
            "https://box.example:8443?x=1",
            "https://user@box.example:8443",
            "https://box.example:port",
            "https://::1:8443",
        ] {
            assert!(
                matches!(parse_https(bad), Err(ClientError::BadTarget(_))),
                "{bad} should be refused"
            );
        }
    }

    #[test]
    fn the_command_line_is_a_path_or_an_address_with_its_token_file() {
        let args = |words: &[&str]| words.iter().map(|w| w.to_string()).collect::<Vec<_>>();
        assert_eq!(
            Client::from_args(&args(&["/run/salmon.http"]))
                .unwrap()
                .target(),
            "/run/salmon.http"
        );
        let refused = |words: &[&str]| Client::from_args(&args(words)).unwrap_err();
        assert!(refused(&[]).contains("name a server"));
        assert!(refused(&["https://box.example:8443"]).contains("needs --token-file FILE"));
        assert!(
            refused(&["http://box.example:8443", "--token-file", "/nowhere"])
                .contains("not an https://HOST:PORT address")
        );
        assert!(refused(&["/run/salmon.http", "--token-file", "/nowhere"])
            .contains("nothing is sent on a unix socket"));
        assert!(refused(&["/run/a.http", "/run/b.http"]).contains("one server at a time"));
        assert!(refused(&["/run/a.http", "--token", "abc"]).contains("unknown option --token"));
    }

    fn body(framing: Framing, wire: &[u8]) -> io::Result<Vec<u8>> {
        let mut out = Vec::new();
        Body {
            reader: BufReader::new(wire),
            framing,
        }
        .read_to_end(&mut out)?;
        Ok(out)
    }

    #[test]
    fn a_body_is_framed_by_length_by_chunks_or_by_the_end() {
        assert_eq!(
            body(Framing::Length(5), b"hello, and more").unwrap(),
            b"hello"
        );
        assert_eq!(body(Framing::UntilClose, b"hello").unwrap(), b"hello");
        let chunked = Framing::Chunked {
            left: 0,
            first: true,
            done: false,
        };
        assert_eq!(
            body(
                chunked,
                b"5\r\nhello\r\n7;ext=1\r\n, world\r\n0\r\nTrailer: x\r\n\r\nnot read"
            )
            .unwrap(),
            b"hello, world"
        );
        // an answer cut short is an error, not a short answer
        assert!(body(Framing::Length(9), b"hello").is_err());
        let chunked = Framing::Chunked {
            left: 0,
            first: true,
            done: false,
        };
        assert!(body(chunked, b"5\r\nhel").is_err());
    }
}
