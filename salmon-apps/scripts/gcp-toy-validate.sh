#!/usr/bin/env bash
# Drives `salmon-gcp-toy` through up -> up -> down against a sandbox GCP
# project and reports what each pass did. Everything after the options is
# passed verbatim to `salmon-gcp-toy config`.
#
#   salmon-apps/scripts/gcp-toy-validate.sh [-y] [--keep] -- \
#       --project salmon-toy-$(date +%s) --organization ORG_ID \
#       --billing-account XXXXXX-XXXXXX-XXXXXX [--tier 1]
#
# Tier 1 deploys `FROM --base-image` (Google's hello sample by default) or,
# with `--containerfile PATH`, your own app built from PATH's directory; it
# must serve HTTP on $PORT. The two flags are mutually exclusive.
#
# Tier 2 boots a VM and provisions it over an SSH CA with this same binary. It
# takes two passes and this script drives both: the first reserves the address
# (GCP picks the IP, so nothing can name the machine before it exists), then
# the IP is read back and fed to `config --vm-ip` for the pass that provisions.
#
# Tier 3 puts a regional external load balancer in front of that VM and checks
# it by fetching a page the VM only serves because the tier-2 hand-off
# installed a systemd unit there. A balancer that exists proves nothing (one
# in front of no server answers 502 just as well), so the verdict is a 200
# carrying the project id -- allow a few minutes for the backend to pass its
# first health checks.
#
# What it checks, pass by pass:
#   up #1   everything comes up. If it fails, one retry is attempted and the
#           run is flagged: converging only on a retry usually means eventual
#           consistency (IAM propagation, a just-enabled API) that a node
#           does not wait out yet.
#   up #2   idempotency: every node with a real `check` must be skipped.
#           Nodes known to have no check are listed separately; anything
#           else re-applied is reported as unexpected.
#   down    teardown succeeds, then the resources are confirmed gone (a
#           created project must be DELETE_REQUESTED; resources in an
#           --existing-project must no longer describe).
#
# Logs and the directive land in $OUT (default gcp-toy-runs/<timestamp>).
# Environment: SALMON_GCP_TOY overrides the binary (default: cabal list-bin).
set -euo pipefail

YES=0
KEEP=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        -y|--yes) YES=1; shift ;;
        --keep) KEEP=1; shift ;;
        --) shift; break ;;
        -h|--help) sed -n '2,40p' "$0"; exit 0 ;;
        *) break ;;
    esac
done

# A refusal must not read like a quiet, successful finish: every early exit
# goes through this, on stdout (where the rest of the run's output goes) with
# a banner in the same == style as a pass.
abort() {
    local headline=$1
    shift
    echo
    echo "=============================================================================="
    echo "== ABORTED: $headline"
    for line in "$@"; do
        echo "$line" | tr '|' '\n' | sed 's/^/==   /'
    done
    echo "=============================================================================="
    exit 1
}

CONFIG_ARGS=("$@")
BIN=${SALMON_GCP_TOY:-$(cabal list-bin salmon-gcp-toy)}
OUT=${OUT:-gcp-toy-runs/$(date +%Y%m%dT%H%M%S)}
mkdir -p "$OUT"
DIRECTIVE="$OUT/directive.json"

# a prompt from gcloud would hang a non-interactive `up` forever
export CLOUDSDK_CORE_DISABLE_PROMPTS=1

"$BIN" config "$@" > "$DIRECTIVE" || abort "salmon-gcp-toy config rejected these flags" "nothing was created; see the error above"
"$BIN" run tree < "$DIRECTIVE" > "$OUT/tree.txt"

field() { sed -E "s/.*\"$1\":(\"([^\"]*)\"|([^,}]*)).*/\2\3/" "$DIRECTIVE"; }
PROJECT=$(field project)
PREFIX=$(field prefix)
REGION=$(field region)
TIER=$(field tier)
WORKDIR=$(field workDir)
CREATES_PROJECT=1
grep -q '"createProjectUnder":null' "$DIRECTIVE" && CREATES_PROJECT=0

echo "== salmon-gcp-toy validation"
echo "   project:         $PROJECT (created by this run: $([[ $CREATES_PROJECT == 1 ]] && echo yes || echo no))"
echo "   tier:            $TIER, region $REGION, prefix $PREFIX"
echo "   gcloud account:  $(gcloud config get-value account 2>/dev/null)"
echo "   ambient project: $(gcloud config get-value project 2>/dev/null) (must not matter)"
echo "   logs:            $OUT"
echo "   graph:           $(grep -c '^[a-z]' "$OUT/tree.txt") nodes, see $OUT/tree.txt"
# Pre-flight the billing account: linking is the first thing that touches
# something the caller may not own, and a run that gets that far has already
# created a project. Cheaper to refuse here.
BILLING=$(field billingAccount)
if [[ -n $BILLING && $BILLING != null ]]; then
    billing_open=$(gcloud billing accounts describe "$BILLING" --format='value(open)' 2>/dev/null || true)
    case "$billing_open" in
        True|true) echo "   billing account:  $BILLING (open)" ;;
        False|false)
            abort "billing account $BILLING is CLOSED" \
                "a closed account cannot be linked; reopen it or pick another" \
                "nothing was created" ;;
        *)
            abort "billing account $BILLING is not visible to $(gcloud config get-value account 2>/dev/null)" \
                "open accounts this identity can see:" \
                "$(gcloud billing accounts list --filter=open=true --format='value(name,displayName)' 2>/dev/null | sed 's/^/  /' | tr '\n' '|' | sed 's/|$//')" \
                "nothing was created" ;;
    esac
fi

if [[ $YES != 1 ]]; then
    read -r -p "This creates billable resources in $PROJECT. Continue? [y/N] " answer
    [[ $answer == y || $answer == Y ]] || abort "not confirmed" "nothing was created" 
fi

# `run up`/`run down` print one UpDown report per line: Skip/Eval/Done/Failed/Blocked (Act {shorthand = "...", ...
node_of() { sed -E 's/^[A-Za-z]+ \(Act \{shorthand = "([^"]*)", extension = \[ [^ ]* : (.*) \]\}\).*/\1: \2/'; }

summarize() {
    local log=$1
    printf '   skip=%s eval=%s done=%s failed=%s blocked=%s\n' \
        "$(grep -c '^Skip ' "$log" || true)" "$(grep -c '^Eval ' "$log" || true)" \
        "$(grep -c '^Done ' "$log" || true)" "$(grep -c '^Failed ' "$log" || true)" \
        "$(grep -c '^Blocked ' "$log" || true)"
    grep '^Failed ' "$log" | node_of | sed 's/^/   FAILED  /' || true
    grep '^Failed ' "$log" | sed -E 's/.*\]\}\) //' | sed 's/^/           /' | cut -c1-400 || true
    # gcloud inherits stderr, so its own diagnosis is in the log: without this
    # the summary says which command failed but never says why.
    if grep -q '^Failed ' "$log"; then
        grep -h '^ERROR:' "$log" | sort -u | head -5 | cut -c1-400 | sed 's/^/           /' || true
        grep -hE '^ +(permission|reason|constraint):' "$log" | sort -u | head -5 | sed 's/^ */           /' || true
    fi
    grep '^Blocked ' "$log" | node_of | sed 's/^/   blocked /' || true
}

run_pass() {
    local name=$1 verb=$2
    echo "== $name"
    local rc=0
    "$BIN" run "$verb" < "$DIRECTIVE" > "$OUT/$name.log" 2>&1 || rc=$?
    summarize "$OUT/$name.log"
    return $rc
}

VERDICT=()

# Tier 2, pass one: bring up the address (and the rest of the infrastructure),
# then read the IP GCP picked and re-issue the directive with it. Only then can
# a graph name the machine it is about to ssh into.
if [[ $TIER -ge 2 ]]; then
    VM_USER=$(field vmUser)
    if ! run_pass up-infra up; then
        abort "the tier-2 infrastructure pass failed (see $OUT/up-infra.log)" \
            "whatever came up is STILL UP; nothing was torn down" \
            "inspect, then: $BIN run down < $DIRECTIVE"
    fi
    VM_IP=$(gcloud compute addresses describe "$PREFIX-ip" --region "$REGION" --project "$PROJECT" --format='value(address)' 2>/dev/null || true)
    [[ -n $VM_IP ]] || abort "could not read the reserved address $PREFIX-ip" "the VM exists but nothing can name it; $BIN run down < $DIRECTIVE"
    echo "   reserved IP:     $VM_IP (re-issuing the directive with --vm-ip)"
    "$BIN" config "${CONFIG_ARGS[@]}" --vm-ip "$VM_IP" > "$DIRECTIVE" \
        || abort "salmon-gcp-toy config rejected --vm-ip $VM_IP" "the infrastructure is up: $BIN run down < $DIRECTIVE"
fi

if run_pass up-1 up; then
    VERDICT+=("up: converged on the first pass")
elif run_pass up-retry up; then
    VERDICT+=("up: converged ONLY ON RETRY (see $OUT/up-1.log)")
else
    abort "up failed twice (see $OUT/up-retry.log)" \
        "whatever came up is STILL UP; nothing was torn down" \
        "inspect, then: $BIN run down < $DIRECTIVE" 
fi

# Nodes with no `check` today, so re-applying them is expected. The tier-2
# ones are the expensive half of this list: rsync:sendfile re-uploads the
# binary and ssh:call re-runs the remote directive on every pass. That is
# salmon's behaviour today, not a defect of the toy -- and the remote pass is
# itself idempotent, which is what the marker check below reads.
NO_CHECK='^(gcloud|gcp-toy|gcp-toy-vm-infra|gcp-toy-on-vm|gcp-cloudrun-deploy|gcp-vm-provision|gcp-metadata-ssh-ca|podman-build|podman-login|podman-push|directory|deb|pre-existing-file|remote|self-call|copy-oneself|ssh:call|rsync:sendfile): '
run_pass up-2 up || VERDICT+=("idempotency pass: FAILED")
UNEXPECTED=$(grep '^Eval ' "$OUT/up-2.log" | node_of | grep -Ev "$NO_CHECK" || true)
EXPECTED=$(grep '^Eval ' "$OUT/up-2.log" | node_of | grep -E "$NO_CHECK" | cut -d: -f1 | sort | uniq -c | tr '\n' ' ' || true)
echo "   re-applied (no check, expected): ${EXPECTED:-none}"
if [[ -n $UNEXPECTED ]]; then
    echo "$UNEXPECTED" | sed 's/^/   RE-APPLIED DESPITE A CHECK: /'
    VERDICT+=("idempotency: $(echo "$UNEXPECTED" | wc -l) node(s) with a check were re-applied")
else
    VERDICT+=("idempotency: every checked node was skipped")
fi

# The VM half is only really proven by what the uploaded binary left behind.
if [[ $TIER -ge 2 ]]; then
    echo "== verify the VM was provisioned by the uploaded binary"
    marker=$(ssh -i "$WORKDIR/ssh/toy-client" -o BatchMode=yes -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10 \
        "$VM_USER@$VM_IP" cat /var/lib/salmon-toy/provisioned 2>&1 | tail -1)
    echo "   /var/lib/salmon-toy/provisioned: $marker"
    # The remote `run up` streams its own report back over ssh, so the second
    # pass shows whether the VM side is idempotent too.
    if grep -qF 'Skip (Act {shorthand = \"file-contents\"' "$OUT/up-2.log"; then
        echo "   the remote pass skipped its own file node (the VM side is idempotent too)"
    fi
    if [[ $marker == *"$PROJECT"* ]]; then
        VERDICT+=("vm: the uploaded binary ran on the VM over the salmon CA")
    else
        VERDICT+=("vm: MARKER NOT FOUND on the VM ($marker)")
    fi
fi

# Tier 3 is only validated by what comes back *through* the balancer: a
# forwarding rule that exists in front of nothing answers 502 just as readily
# as a working one answers 200.
if [[ $TIER -ge 3 ]]; then
    echo "== verify the load balancer serves the VM"
    LB_IP=$(gcloud compute forwarding-rules describe "$PREFIX-lb-fw" --region "$REGION" --project "$PROJECT" --format='value(IPAddress)' 2>/dev/null || true)
    if [[ -z $LB_IP ]]; then
        VERDICT+=("lb: NO FORWARDING RULE ($PREFIX-lb-fw has no address)")
    else
        echo "   forwarding rule $PREFIX-lb-fw: http://$LB_IP/"
        # A backend is UNHEALTHY until it has passed its first health checks,
        # so this is a wait, not a probe: up to ~5 minutes.
        body=""
        for attempt in $(seq 1 30); do
            body=$(curl -sS --max-time 10 "http://$LB_IP/" 2>&1 || true)
            [[ $body == *"$PROJECT"* ]] && break
            [[ $attempt == 1 || $((attempt % 6)) == 0 ]] && echo "   still waiting for a healthy backend (attempt $attempt): ${body:0:80}"
            sleep 10
        done
        if [[ $body == *"$PROJECT"* ]]; then
            VERDICT+=("lb: the balancer served the VM's page")
        else
            # The usual cause is a firewall rule, and the health of the
            # backend says so more precisely than the response body does.
            gcloud compute backend-services get-health "$PREFIX-lb-backend" --region "$REGION" --project "$PROJECT" \
                --format='value(status.healthStatus[].healthState)' 2>&1 | sed 's/^/   backend health: /' || true
            VERDICT+=("lb: NOT SERVING (last response: ${body:0:120})")
        fi
    fi
fi

if [[ $KEEP == 1 ]]; then
    VERDICT+=("down: skipped (--keep); later: $BIN run down < $DIRECTIVE")
else
    if run_pass down down; then
        VERDICT+=("down: succeeded")
    else
        # Two node kinds cannot go down on a workstation, by design rather
        # than by accident: `deb` tears down with `apt-get remove`, which
        # needs root and would uninstall a system package salmon did not put
        # there; and `directory` refuses a non-empty directory, which the ssh
        # key dir always is because Keys.sshKey deliberately "keeps keys
        # around". Failing is the safe outcome for both.
        real=$(grep '^Failed ' "$OUT/down.log" | node_of | grep -Ev '^(deb|directory): ' || true)
        if [[ -z $real ]]; then
            VERDICT+=("down: succeeded except where it should not (apt-get remove needs root; ssh keys are kept on purpose)")
        else
            VERDICT+=("down: FAILED (see $OUT/down.log)")
        fi
    fi

    echo "== verify teardown"
    gone() { if "$@" >/dev/null 2>&1; then echo "   STILL PRESENT: ${*:2:4}"; return 1; fi; }
    leftovers=0
    if [[ $CREATES_PROJECT == 1 ]]; then
        state=$(gcloud projects describe "$PROJECT" --format='value(lifecycleState)' 2>/dev/null || echo ABSENT)
        echo "   project lifecycleState: $state"
        [[ $state == DELETE_REQUESTED || $state == ABSENT ]] || leftovers=1
    else
        gone gcloud storage buckets describe "gs://$PROJECT-$PREFIX" --project "$PROJECT" || leftovers=1
        gone gcloud iam service-accounts describe "$PREFIX-sa@$PROJECT.iam.gserviceaccount.com" --project "$PROJECT" || leftovers=1
        gone gcloud artifacts repositories describe "$PREFIX-repo" --location "$REGION" --project "$PROJECT" || leftovers=1
        if [[ $TIER -ge 2 ]]; then
            gone gcloud compute instances describe "$PREFIX-vm" --zone "$(field vmZone)" --project "$PROJECT" || leftovers=1
            gone gcloud compute addresses describe "$PREFIX-ip" --region "$REGION" --project "$PROJECT" || leftovers=1
            gone gcloud compute firewall-rules describe "$PREFIX-ssh" --project "$PROJECT" || leftovers=1
        fi
        if [[ $TIER -ge 3 ]]; then
            gone gcloud compute forwarding-rules describe "$PREFIX-lb-fw" --region "$REGION" --project "$PROJECT" || leftovers=1
            gone gcloud compute url-maps describe "$PREFIX-lb-url-map" --region "$REGION" --project "$PROJECT" || leftovers=1
            gone gcloud compute backend-services describe "$PREFIX-lb-backend" --region "$REGION" --project "$PROJECT" || leftovers=1
            gone gcloud compute instance-groups unmanaged describe "$PREFIX-ig" --zone "$(field vmZone)" --project "$PROJECT" || leftovers=1
            gone gcloud compute networks subnets describe "$PREFIX-proxy" --region "$REGION" --project "$PROJECT" || leftovers=1
        fi
        if [[ $TIER -ge 1 ]]; then
            gone gcloud run services describe "$PREFIX-hello" --region "$REGION" --project "$PROJECT" || leftovers=1
        fi
    fi
    if [[ $leftovers == 0 ]]; then VERDICT+=("teardown verified"); else VERDICT+=("teardown: LEFTOVERS FOUND"); fi
fi

echo "== verdict"
printf '   %s\n' "${VERDICT[@]}"
printf '%s\n' "${VERDICT[@]}" | grep -qE 'FAILED|RETRY|LEFTOVERS|NOT FOUND|NOT SERVING|NO FORWARDING RULE|re-applied$' && exit 1 || exit 0
