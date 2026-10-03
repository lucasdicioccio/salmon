#!/usr/bin/env bash
# Drive salmon-toy-qemu-pg-ha through `run serve --http`: three qemu guests
# booted once and kept, a Postgres pair on them, and a session in which the
# primary is moved, a machine is paused or cut off, and a client keeps writing
# -- one line at a time, by a person or by a program holding the socket.
#
# The one-shot form of the same toy (`config ... | run up`, see the header of
# salmon-apps/src/QemuPgHaToy.hs) starts from nothing on every line. This one
# keeps a server up; every subcommand below is one POST to it, and everything
# it does can be watched on /events, in salmon-tui or in the web UI.
#
# Usage (from anywhere: it cd's to the repo root itself):
#   salmon-apps/scripts/qemu-pg-ha-serve.sh serve              # start the server (+ web UI forward if socat is there)
#   salmon-apps/scripts/qemu-pg-ha-serve.sh guests             # boot the bridge and the three guests, wait for ssh
#   salmon-apps/scripts/qemu-pg-ha-serve.sh pair A --seed B    # the pair on top, A the primary, B cloned from it
#   salmon-apps/scripts/qemu-pg-ha-serve.sh writer             # a client the server holds, writing through the bouncer
#   salmon-apps/scripts/qemu-pg-ha-serve.sh tail               # the writer's lines, live (the `output` stream)
#   salmon-apps/scripts/qemu-pg-ha-serve.sh pair B             # move the primary: one word changed
#   salmon-apps/scripts/qemu-pg-ha-serve.sh freeze b           # a fault: pause B's guest ...
#   salmon-apps/scripts/qemu-pg-ha-serve.sh pair A             # ... a failover is refused ...
#   salmon-apps/scripts/qemu-pg-ha-serve.sh pair A --may-discard B   # ... until somebody says what may be lost
#   salmon-apps/scripts/qemu-pg-ha-serve.sh thaw b             # resume it: it rejoins through pg_rewind
#   salmon-apps/scripts/qemu-pg-ha-serve.sh cut a b            # a fault: A sends nothing to B (heal a b undoes it)
#   salmon-apps/scripts/qemu-pg-ha-serve.sh cmd 'recheck --select #REF'   # any line of the serve language, sync (REF: first column of status)
#   salmon-apps/scripts/qemu-pg-ha-serve.sh status | dag | events | tui
#   salmon-apps/scripts/qemu-pg-ha-serve.sh unpair             # retire the pair, keep the guests booted
#   salmon-apps/scripts/qemu-pg-ha-serve.sh down               # clear every seed (guests stopped), then quit
#
# Needs what the toy needs: `prereqs` run once as root, the two capabilities
# and the kvm group (see QemuPgHaToy.hs), plus curl and jq here.
# See resources/postgres-pair.md, "Driving it live".
set -euo pipefail

ROOT=${ROOT:-/var/lib/salmon-toy-pg-ha}
RUN=${XDG_RUNTIME_DIR:-/tmp}
SOCK=${SOCK:-$RUN/salmon-toy.http}     # unix socket paths are limited to 108 bytes
STATE=$RUN/salmon-toy.pair             # the words of the pair declaration in force
UI_PORT=${UI_PORT:-9081}

cd "$(dirname "$0")/../.."
TOY=$(cabal list-bin salmon-toy-qemu-pg-ha)

post() { curl -s --unix-socket "$SOCK" -X POST --data-binary "$1" "http://x/command${2:-}"; }
get() { curl -s --unix-socket "$SOCK" "http://x$1"; }
# what a pass did, one line per report, without the node descriptions
brief() { jq -c '.[] | {kind, ref: (.ref.short? // null), node: (.node.shorthand? // null), ok, remaining, error, reason} | with_entries(select(.value != null))'; }
side() {
    case "${1:-}" in
    A | a) echo A ;;
    B | b) echo B ;;
    *) echo "name a side: A or B" >&2; exit 1 ;;
    esac
}

case "${1:-}" in
serve)
    cabal build salmon-toy-qemu-pg-ha salmon-tui
    rm -f "$STATE"
    "$TOY" run serve --http "$SOCK" < /dev/null &
    server=$!
    sleep 1
    echo "server on $SOCK (pid $server)"
    if command -v socat > /dev/null; then
        socat "TCP-LISTEN:$UI_PORT,bind=127.0.0.1,reuseaddr,fork" "UNIX-CONNECT:$SOCK" &
        echo "web UI at http://127.0.0.1:$UI_PORT/"
    fi
    echo "next:  $0 guests"
    wait "$server"
    ;;
guests)
    post "up guests --root $ROOT" | brief
    ;;
pair)
    primary=$(side "${2:-}")
    shift 2
    words="up --root $ROOT --primary $primary $*"
    words=${words% }
    previous=$(cat "$STATE" 2> /dev/null || true)
    if [[ $words == "$previous" ]]; then
        post converge | brief
        exit 0
    fi
    # The new declaration first, then the one it replaces. Both name the same
    # nodes, so retiring the old one takes nothing down; in between, the role
    # node is reported `conflicting` (two live declarations describe it), the
    # newer one wins, and the pass that follows is the switchover.
    post "up $words" | brief
    echo "$words" > "$STATE"
    if [[ -n $previous ]]; then
        post "down $previous" | brief
    fi
    ;;
unpair)
    previous=$(cat "$STATE" 2> /dev/null || true)
    [[ -n $previous ]] || { echo "no pair declared from here" >&2; exit 1; }
    post "down $previous" | brief
    rm -f "$STATE"
    ;;
writer)
    post "up writer --root $ROOT" | brief
    echo "its lines:  $0 tail"
    ;;
tail)
    # held actions only: a pass's own narration is on the other streams
    curl -s -N --unix-socket "$SOCK" "http://x/events?since=${2:-0}&stream=output" |
        sed -un 's/^data: //p' | jq -r --unbuffered '"\(.seq)\t\(.node.shorthand)\t\(.line)"'
    ;;
events)
    curl -s -N --unix-socket "$SOCK" "http://x/events?since=${2:-0}" | sed -un 's/^data: //p' |
        jq -c --unbuffered '{seq, stream, kind, origin, node: (.node.shorthand? // .report.node.shorthand? // null), line, error, reason} | with_entries(select(.value != null))'
    ;;
freeze)
    post "up frozen --root $ROOT --machine ${2:?machine: a, b or bouncer}" | brief
    ;;
thaw)
    post "down frozen --root $ROOT --machine ${2:?machine: a, b or bouncer}" | brief
    ;;
cut)
    post "up partition --root $ROOT --machine ${2:?machine} --from ${3:?the machine it stops answering}" | brief
    ;;
heal)
    post "down partition --root $ROOT --machine ${2:?machine} --from ${3:?the machine it stopped answering}" | brief
    ;;
cmd)
    post "${2:?a line of the serve language}" | brief
    ;;
status)
    get /status | jq -r '.nodes[] | "\(.ref.short)\t\(.direction)\t\(.convergence)\t\(.status.check.verdict // "-")\t\(.shorthand)\t\(.help)"' | column -t -s "$(printf '\t')"
    ;;
dag)
    get /dag
    ;;
tui)
    exec "$(cabal list-bin salmon-tui)" "$SOCK"
    ;;
down)
    # every seed retired: faults healed and the writer let go first, then the
    # pair, then the guests (an ACPI power-off each), dependants before what
    # they stand on. Sync, so this waits and prints the reports.
    post clear | brief
    post quit > /dev/null
    rm -f "$STATE"
    echo "guests stopped; the root filesystems under $ROOT are left as they are"
    ;;
*)
    sed -n '2,/^set -e/p' "$0" | head -n -1
    ;;
esac
