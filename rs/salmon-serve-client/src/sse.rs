//! Splitting the `/events` stream into blocks, as `Salmon.Client.Http`'s
//! `splitBlocks`/`parseBlock` do. The server ends every block with a blank
//! line, sends `id:` and `data:` lines for an event (no `id:` on a `gap`),
//! and a comment line as a keep-alive.

use serde_json::Value;

/// One block of the stream.
#[derive(Debug, Clone, PartialEq)]
pub enum Block {
    /// An event with its `id` (absent on a `gap`).
    Event(Option<u64>, Value),
    /// A keep-alive: every line of the block is a comment.
    Comment,
}

/// Take the complete blocks (ended by a blank line) off the front of the
/// buffer, leaving what is left of an unfinished one.
pub fn split_blocks(buffer: &mut Vec<u8>) -> Vec<Vec<u8>> {
    let mut blocks = Vec::new();
    let mut start = 0;
    while let Some(at) = find(&buffer[start..], b"\n\n") {
        blocks.push(buffer[start..start + at].to_vec());
        start += at + 2;
    }
    buffer.drain(..start);
    blocks
}

/// One block. A block whose every line is a comment is [`Block::Comment`];
/// one with a `data:` line that is JSON is an event; anything else (an empty
/// block, a `data:` line that is not JSON) is dropped, since the server
/// never sends one and a client has nothing to do with it.
pub fn parse_block(block: &[u8]) -> Option<Block> {
    let lines: Vec<&[u8]> = block
        .split(|b| *b == b'\n')
        .filter(|l| !l.is_empty())
        .collect();
    if lines.is_empty() {
        return None;
    }
    if lines.iter().all(|l| l.starts_with(b":")) {
        return Some(Block::Comment);
    }
    let data = lines.iter().find_map(|l| field(l, b"data:"))?;
    let value = serde_json::from_slice(data).ok()?;
    let id = lines
        .iter()
        .find_map(|l| field(l, b"id:"))
        .and_then(|raw| std::str::from_utf8(raw).ok())
        .and_then(|text| text.trim().parse().ok());
    Some(Block::Event(id, value))
}

fn field<'a>(line: &'a [u8], name: &[u8]) -> Option<&'a [u8]> {
    let rest = line.strip_prefix(name)?;
    let blanks = rest.iter().take_while(|b| **b == b' ').count();
    Some(&rest[blanks..])
}

fn find(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack.windows(needle.len()).position(|w| w == needle)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn a_gap_two_events_and_a_keep_alive() {
        // what `Events.renderGap`, `renderEvent` and `keepAlive` put on the wire
        let mut wire = Vec::new();
        wire.extend_from_slice(b"data: {\"stream\":\"server\",\"kind\":\"gap\",\"from\":3}\n\n");
        wire.extend_from_slice(
            b"id: 7\ndata: {\"stream\":\"server\",\"kind\":\"enqueued\",\"seq\":7}\n\n",
        );
        wire.extend_from_slice(b": keep-alive\n\n");
        wire.extend_from_slice(
            b"id: 8\ndata: {\"stream\":\"serve\",\"kind\":\"started\",\"seq\":8}\n\n",
        );
        wire.extend_from_slice(b"id: 9\ndata: {\"partial");
        let blocks = split_blocks(&mut wire);
        assert_eq!(
            wire, b"id: 9\ndata: {\"partial",
            "the partial block is left over"
        );
        let parsed: Vec<Block> = blocks.iter().filter_map(|b| parse_block(b)).collect();
        assert_eq!(
            parsed,
            vec![
                Block::Event(None, json!({"stream": "server", "kind": "gap", "from": 3})),
                Block::Event(
                    Some(7),
                    json!({"stream": "server", "kind": "enqueued", "seq": 7})
                ),
                Block::Comment,
                Block::Event(
                    Some(8),
                    json!({"stream": "serve", "kind": "started", "seq": 8})
                ),
            ]
        );
    }

    #[test]
    fn a_block_arriving_in_pieces_is_one_block() {
        let mut buffer = b"id: 1\ndata: {\"seq\"".to_vec();
        assert!(split_blocks(&mut buffer).is_empty());
        buffer.extend_from_slice(b":1}\n");
        assert!(
            split_blocks(&mut buffer).is_empty(),
            "one newline is not the end"
        );
        buffer.extend_from_slice(b"\n");
        let blocks = split_blocks(&mut buffer);
        assert_eq!(blocks.len(), 1);
        assert_eq!(
            parse_block(&blocks[0]),
            Some(Block::Event(Some(1), json!({"seq": 1})))
        );
        assert!(buffer.is_empty());
    }

    #[test]
    fn what_is_not_an_event_is_dropped() {
        assert_eq!(parse_block(b""), None);
        assert_eq!(parse_block(b"data: not json"), None);
        assert_eq!(parse_block(b"event: something"), None);
    }
}
