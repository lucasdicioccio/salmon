#!/usr/bin/env bash
# Drive salmon-gcp-toy through `run serve`: the HTTP surface, the web UI, the
# TUI, and tier 2 through pull mode. The companion of gcp-toy-validate.sh,
# which drives the same binary through one-shot up -> up -> down passes; this
# one keeps a server up and changes what it wants one word at a time.
#
# Usage (from anywhere: it cd's to the repo root itself):
#   salmon-apps/scripts/gcp-toy-serve.sh serve                       # start the toy as a server + web UI forward
#   salmon-apps/scripts/gcp-toy-serve.sh tier0                       # declare tier 0 (async), watch it converge
#   salmon-apps/scripts/gcp-toy-serve.sh tier1                       # `only` tier 1: shared nodes skipped, new ones added
#   salmon-apps/scripts/gcp-toy-serve.sh tag v2                      # bump the image tag: a redeploy
#   salmon-apps/scripts/gcp-toy-serve.sh cmd 'status'                # any line of the serve language, sync
#   salmon-apps/scripts/gcp-toy-serve.sh tui                         # the terminal client
#   salmon-apps/scripts/gcp-toy-serve.sh follow                      # tier 2 through pull mode (two passes, see below)
#   salmon-apps/scripts/gcp-toy-serve.sh vm-ip                       # pull mode second pass: add --vm-ip to the document
#   salmon-apps/scripts/gcp-toy-serve.sh down                        # clear every seed (tears everything down), then quit
#
# Fill in ORG and BILLING below (or export them). Use a fresh PROJECT per run:
# a deleted project keeps its id for ~30 days. See resources/gcp-toy-validation.md.
set -euo pipefail

ORG=${ORG:-YOUR_ORG_ID}
BILLING=${BILLING:-YOUR_BILLING_ACCOUNT}
PROJECT=${PROJECT:-salmon-toy-$(date +%s)}
REGION=${REGION:-europe-west1}

SOCK=/tmp/gcp-toy.http           # unix socket paths are limited to 108 bytes
UI_PORT=${UI_PORT:-9080}
SINKS=/tmp/gcp-sinks
REG=/tmp/gcp-reg                  # pull-mode registry (one <label>.json per label)
CACHE=/tmp/gcp-cache
WORKDIR=$PWD/gcp-toy-work         # absolute: the server's cwd is where it was started
STATE=/tmp/gcp-toy.project        # remembers PROJECT across invocations

cd "$(dirname "$0")/../.."
TOY=$(cabal list-bin salmon-gcp-toy)
TUI=$(cabal list-bin salmon-tui)

post() { curl -s --unix-socket "$SOCK" -X POST --data-binary "$1" "http://x/command$2"; }
seed() { echo "--project $PROJECT --organization $ORG --billing-account $BILLING --region $REGION --workdir $WORKDIR $*"; }

need_ids() {
    if [[ $ORG == YOUR_ORG_ID || $BILLING == YOUR_BILLING_ACCOUNT ]]; then
        echo "set ORG and BILLING first (gcloud organizations list; gcloud billing accounts list)" >&2
        exit 1
    fi
}

case "${1:-}" in
serve)
    cabal build salmon-gcp-toy salmon-tui
    mkdir -p "$SINKS"
    echo "$PROJECT" > "$STATE"
    "$TOY" run serve --http "$SOCK" --status-sink "$SINKS/toy.json" < /dev/null &
    sleep 1
    socat "TCP-LISTEN:$UI_PORT,bind=127.0.0.1,reuseaddr,fork" "UNIX-CONNECT:$SOCK" &
    echo "server on $SOCK, web UI at http://127.0.0.1:$UI_PORT/ (project id: $PROJECT)"
    echo "the tending loop calls gcloud for every node's check between passes;"
    echo "  salmon-apps/scripts/gcp-toy-serve.sh cmd 'supervise off'   keeps it quiet if you prefer"
    wait
    ;;
tier0)
    need_ids; PROJECT=$(cat "$STATE")
    post "up $(seed --tier 0)" '?async'; echo
    echo "watch it converge in the browser, the TUI, or:  salmon-apps/scripts/gcp-toy-serve.sh cmd status"
    ;;
tier1)
    need_ids; PROJECT=$(cat "$STATE")
    # `only` retires the previous seed and declares this one: shared nodes are
    # skipped, only the image build/push and the Cloud Run service are new.
    post "only $(seed --tier 1)" '?async'; echo
    ;;
tag)
    need_ids; PROJECT=$(cat "$STATE")
    post "only $(seed --tier 1 --image-tag "${2:?tag}")" '?async'; echo
    ;;
cmd)
    post "${2:?line}" '' | jq -c '.[] | {kind, epoch, nodes, ok, remaining, error} | with_entries(select(.value != null))'
    ;;
tui)
    exec "$TUI" "$SOCK"
    ;;
follow)
    # Tier 2 is two passes because GCP picks the address: exactly the shape a
    # registry document handles. Start a separate server that follows $REG.
    need_ids; PROJECT=$(cat "$STATE" 2>/dev/null || echo "$PROJECT"); echo "$PROJECT" > "$STATE"
    mkdir -p "$REG" "$CACHE" "$SINKS"
    words=$(seed --tier 2 | sed 's/ /","/g')
    printf '{"salmon":1,"id":"toy@1","seeds":[{"seed":["%s"]}]}\n' "$words" > "$REG/toy.json"
    echo "document: $REG/toy.json"
    "$TOY" run serve --http "$SOCK" --status-sink "$SINKS/toy.json" \
        --follow "$REG" --label toy --follow-cache "$CACHE" \
        --follow-base 10 --follow-debounce 3 < /dev/null &
    sleep 1
    socat "TCP-LISTEN:$UI_PORT,bind=127.0.0.1,reuseaddr,fork" "UNIX-CONNECT:$SOCK" &
    echo "pass one runs from the document; once the address node is up, run:  salmon-apps/scripts/gcp-toy-serve.sh vm-ip"
    wait
    ;;
vm-ip)
    need_ids; PROJECT=$(cat "$STATE")
    ip=$(gcloud compute addresses list --project "$PROJECT" --format='value(address)' | head -1)
    [[ -n $ip ]] || { echo "no reserved address yet in $PROJECT" >&2; exit 1; }
    words=$(seed --tier 2 --vm-ip "$ip" | sed 's/ /","/g')
    printf '{"salmon":1,"id":"toy@2","seeds":[{"seed":["%s"]}]}\n' "$words" > "$REG/toy.json"
    echo "document now toy@2 with --vm-ip $ip; the fetcher injects the diff within the debounce window"
    echo "  salmon-apps/scripts/gcp-toy-serve.sh cmd fetch     forces a round now"
    ;;
down)
    # every seed retired: the whole graph goes down dependants-first, the
    # project last; sync, so this waits and prints the reports.
    post clear '' | jq -c '.[] | {kind, ok, remaining} | with_entries(select(.value != null))'
    post quit '' > /dev/null
    echo "teardown done; confirm with: gcloud projects describe $(cat "$STATE") --format='value(lifecycleState)'"
    ;;
*)
    sed -n '2,/^set -e/p' "$0" | head -n -1
    ;;
esac
