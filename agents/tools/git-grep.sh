#!/bin/bash
# git-grep.sh - Search project source code using git grep, with paginated results.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if [ "${1:-}" == "describe" ]; then
    cat <<'EOF'
{
  "slug": "gitgrep",
  "description": "Search project source code using git grep, with paginated results",
  "args": [
    {
      "name": "query",
      "description": "Search pattern passed to git grep",
      "type": "string",
      "backing_type": "string",
      "arity": "single",
      "mode": "dashdashspace"
    },
    {
      "name": "path",
      "description": "Optional path filter (directory or glob)",
      "type": "string",
      "backing_type": "string",
      "arity": "optional",
      "mode": "dashdashspace"
    },
    {
      "name": "limit",
      "description": "Maximum hits per page (default 50, max 200)",
      "type": "number",
      "backing_type": "string",
      "arity": "optional",
      "mode": "dashdashspace"
    },
    {
      "name": "offset",
      "description": "Pagination offset (default 0)",
      "type": "number",
      "backing_type": "string",
      "arity": "optional",
      "mode": "dashdashspace"
    }
  ],
  "empty-result": { "tag": "AddMessage", "contents": "No matches found" }
}
EOF
    exit 0
fi

if [ "${1:-}" != "run" ]; then
    echo "Usage: $0 <describe|run>" >&2
    exit 1
fi

shift

QUERY=""
PATH_FILTER=""
LIMIT="50"
OFFSET="0"

# Parse arguments passed as --name value
while [[ $# -gt 0 ]]; do
    case $1 in
        --query)
            QUERY="${2:-}"
            shift 2
            ;;
        --path)
            PATH_FILTER="${2:-}"
            shift 2
            ;;
        --limit)
            LIMIT="${2:-50}"
            shift 2
            ;;
        --offset)
            OFFSET="${2:-0}"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

if ! git -C "$PROJECT_ROOT" rev-parse --git-dir >/dev/null 2>&1; then
    echo "Error: not inside a git repository" >&2
    exit 1
fi

if [[ -z "$QUERY" ]]; then
    echo "Error: missing required argument 'query'" >&2
    exit 1
fi

# Normalize and cap limit.
if ! [[ "$LIMIT" =~ ^[0-9]+$ ]] || (( LIMIT < 1 )); then
    LIMIT=50
fi
MAX_LIMIT=200
if (( LIMIT > MAX_LIMIT )); then
    LIMIT=$MAX_LIMIT
fi

# Normalize offset.
if ! [[ "$OFFSET" =~ ^[0-9]+$ ]]; then
    OFFSET=0
fi

GIT_ARGS=(-n -e "$QUERY" --)
if [[ -n "$PATH_FILTER" ]]; then
    GIT_ARGS+=("$PATH_FILTER")
fi

ALL_MATCHES="$(git -C "$PROJECT_ROOT" grep "${GIT_ARGS[@]}" 2>/dev/null || true)"
TOTAL=0
MATCHES=""
if [[ -n "$ALL_MATCHES" ]]; then
    TOTAL=$(printf '%s\n' "$ALL_MATCHES" | wc -l | tr -d ' ')
    START=$((OFFSET + 1))
    END=$((OFFSET + LIMIT))
    MATCHES=$(printf '%s\n' "$ALL_MATCHES" | sed -n "${START},${END}p")
fi

MATCH_COUNT=0
if [[ -n "$MATCHES" ]]; then
    MATCH_COUNT=$(printf '%s\n' "$MATCHES" | wc -l | tr -d ' ')
fi

printf 'Git grep results for: %s' "$QUERY"
[[ -n "$PATH_FILTER" ]] && printf ' (path: %s)' "$PATH_FILTER"
printf '\n'

if (( TOTAL == 0 )); then
    echo "No matches found."
else
    echo "Showing $MATCH_COUNT of $TOTAL hit(s) (offset=$OFFSET, limit=$LIMIT)"
    echo
    echo "$MATCHES"
    if (( OFFSET + LIMIT < TOTAL )); then
        NEXT_OFFSET=$((OFFSET + LIMIT))
        echo
        echo "(more results available; use offset=$NEXT_OFFSET to continue)"
    fi
fi
