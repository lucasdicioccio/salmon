#!/usr/bin/env bash
# Release the salmon libraries to Hackage: candidates first, publish on request.
# See RELEASING.md. Never pushes anything; the final tag is local.
set -euo pipefail

PACKAGES=(salmon-core salmon-ops salmon-ops-recipes salmon-apps)   # dependency order

usage() {
  cat <<USAGE
usage: scripts/release-hackage.sh [options]

  --dry-run          do everything except the upload (sdist, build, test); no network upload
  --publish          upload as published releases instead of candidates, then tag locally
  --token-file FILE  file holding the Hackage API token (else \$HACKAGE_TOKEN; else cabal's own credentials)
  --set-bounds       rewrite internal dependencies to ^>=VERSION in the .cabal files, then exit
  --skip-tests       build the unpacked tarballs but do not run their test suites
  --heavy-tests      also run the container/VM tier (qemu, podman; needs root-ish prerequisites, shared host)
  --keep             keep the work directory
  --yes              do not ask before uploading
  -h, --help
USAGE
}

DRY=0 PUBLISH=0 TOKEN_FILE="" SET_BOUNDS=0 SKIP_TESTS=0 HEAVY=0 KEEP=0 YES=0
while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run) DRY=1 ;;
    --publish) PUBLISH=1 ;;
    --token-file) TOKEN_FILE="${2:?--token-file needs a file}"; shift ;;
    --set-bounds) SET_BOUNDS=1 ;;
    --skip-tests) SKIP_TESTS=1 ;;
    --heavy-tests) HEAVY=1 ;;
    --keep) KEEP=1 ;;
    --yes) YES=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

cd "$(git rev-parse --show-toplevel)"
say() { printf '\n== %s\n' "$*"; }
die() { printf 'release-hackage: %s\n' "$*" >&2; exit 1; }

version_of() { awk '/^version:/ {print $2; exit}' "$1/$1.cabal"; }

# ---- 1. versions agree ------------------------------------------------------
VERSION="$(version_of "${PACKAGES[0]}")"
[ -n "$VERSION" ] || die "cannot read version of ${PACKAGES[0]}"
for p in "${PACKAGES[@]}"; do
  v="$(version_of "$p")"
  [ "$v" = "$VERSION" ] || die "version mismatch: $p is $v, ${PACKAGES[0]} is $VERSION"
done
say "all packages at $VERSION"

# ---- 2. internal bounds -----------------------------------------------------
# An internal dependency line is "    , salmon-core" (bare) or "    , salmon-ops ^>=X".
INTERNAL='^[[:space:]]*,?[[:space:]]*salmon-(core|ops-recipes|ops)([[:space:]]|,|$)'
if [ "$SET_BOUNDS" = 1 ]; then
  for p in "${PACKAGES[@]}"; do
    sed -E -i "s/^([[:space:]]*,?[[:space:]]*salmon-(core|ops-recipes|ops))[[:space:]]*\$/\\1 ^>=$VERSION/" "$p/$p.cabal"
  done
  echo "internal bounds set to ^>=$VERSION; review and commit the change, then re-run."
  exit 0
fi
bad=0
for p in "${PACKAGES[@]}"; do
  f="$p/$p.cabal"
  while IFS= read -r line; do
    case "$line" in
      *"^>=$VERSION"*) ;;
      *) printf '  %s: %s\n' "$f" "$line" >&2; bad=1 ;;
    esac
  done < <(grep -E "$INTERNAL" "$f" || true)
done
[ "$bad" = 0 ] || die "internal dependencies above lack ^>=$VERSION (run with --set-bounds, review, commit)"

# ---- 3. clean tree ----------------------------------------------------------
if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  git status --short --untracked-files=no >&2
  die "working tree has uncommitted changes to tracked files"
fi
COMMIT="$(git rev-parse HEAD)"
say "releasing $COMMIT"

# ---- 4. cabal check + sdist -------------------------------------------------
WORK="$(mktemp -d -t salmon-release.XXXXXX)"
cleanup() { if [ "$KEEP" = 1 ]; then echo "kept $WORK"; else rm -rf "$WORK"; fi; }
trap cleanup EXIT
SDIST="$WORK/sdist"; mkdir -p "$SDIST"
CHECK_FAILED=0

for p in "${PACKAGES[@]}"; do
  say "cabal check $p"
  # warnings exit 0; errors ("Hackage would reject this package") fail the release,
  # except in a dry run, which reports them and carries on so the rest can be exercised.
  if ! (cd "$p" && cabal check); then
    if [ "$DRY" = 1 ]; then
      echo "release-hackage: WOULD BLOCK A REAL RELEASE: cabal check failed for $p" >&2
      CHECK_FAILED=1
    else
      die "cabal check failed for $p"
    fi
  fi
  say "cabal sdist $p"
  cabal sdist "$p" --output-directory="$SDIST" >/dev/null
done
ls -1 "$SDIST"

# ---- 5. build and test from the tarballs ------------------------------------
say "unpacking tarballs"
UNPACK="$WORK/unpacked"; mkdir -p "$UNPACK"
for p in "${PACKAGES[@]}"; do
  tar -xzf "$SDIST/$p-$VERSION.tar.gz" -C "$UNPACK"
done
{
  echo "packages:"
  for p in "${PACKAGES[@]}"; do echo "  $UNPACK/$p-$VERSION"; done
  # carry the repo's solver tweaks (allow-newer), not its package list
  if [ -f cabal.project.local ]; then grep -v '^ignore-project' cabal.project.local || true; fi
} > "$UNPACK/cabal.project"
# Build outside the working tree so nothing in it can leak in; the package store is shared.
BUILD="$WORK/dist"
say "build from tarballs"
cabal build all --project-file="$UNPACK/cabal.project" --builddir="$BUILD"
if [ "$SKIP_TESTS" = 1 ]; then
  say "tests skipped (--skip-tests)"
else
  # The "containers and VMs (serialized)" group boots real guests on a shared host: opt-in.
  pattern=()
  if [ "$HEAVY" != 1 ]; then pattern=(--test-option=-p --test-option='!/containers and VMs/'); fi
  say "test from tarballs${pattern:+ (container/VM tier excluded; --heavy-tests includes it)}"
  cabal test all --project-file="$UNPACK/cabal.project" --builddir="$BUILD" --test-show-details=direct "${pattern[@]}"
fi

# ---- 6. upload --------------------------------------------------------------
what="candidates"; [ "$PUBLISH" = 1 ] && what="PUBLISHED releases (irreversible)"
if [ "$DRY" = 1 ]; then
  say "dry run: stopping before the upload"
  if [ "$CHECK_FAILED" = 1 ]; then echo "NOTE: cabal check failed above; a real run would have stopped there."; fi
  echo "would upload as $what, in this order:"
  for p in "${PACKAGES[@]}"; do echo "  $p-$VERSION.tar.gz"; done
  if [ "$PUBLISH" = 1 ]; then echo "would then run: git tag -a v$VERSION $COMMIT (never pushed)"; fi
  exit 0
fi

if [ -n "$TOKEN_FILE" ]; then
  [ -r "$TOKEN_FILE" ] || die "cannot read $TOKEN_FILE"
  HACKAGE_TOKEN="$(tr -d '[:space:]' < "$TOKEN_FILE")"
fi
# With no token, cabal uses its own configured credentials (its config file or prompt).
if [ -n "${HACKAGE_TOKEN:-}" ]; then creds="the API token"; else creds="cabal's configured credentials"; fi

if [ "$YES" != 1 ]; then
  printf 'Upload %s %s as %s, using %s? [y/N] ' "${PACKAGES[*]}" "$VERSION" "$what" "$creds"
  read -r ans; [ "$ans" = y ] || die "aborted"
fi

for p in "${PACKAGES[@]}"; do
  say "upload $p $VERSION ($what)"
  flags=()
  if [ -n "${HACKAGE_TOKEN:-}" ]; then flags+=(--token="$HACKAGE_TOKEN"); fi
  if [ "$PUBLISH" = 1 ]; then flags+=(--publish); fi
  cabal upload ${flags[@]+"${flags[@]}"} "$SDIST/$p-$VERSION.tar.gz"
done

if [ "$PUBLISH" = 1 ]; then
  git tag -a "v$VERSION" -m "salmon $VERSION" "$COMMIT"
  say "tagged v$VERSION locally; push it yourself: git push <remote> v$VERSION"
else
  say "candidates uploaded; check them on Hackage, then re-run with --publish"
fi
