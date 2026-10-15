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
  --packages A,B     release only these packages (default: those whose version is not yet published
                     on Hackage, asked of hackage.haskell.org; with --docs-only, all four)
  --set-bounds       rewrite each internal dependency to ^>= that package's own version, then exit
  --no-docs          do not build or upload Haddock documentation
  --docs-only        build and upload only the Haddocks, for the version already in the .cabal
                     files: no package upload, no tag; publishes the docs unless --dry-run
  --skip-tests       build the unpacked tarballs but do not run their test suites
  --heavy-tests      also run the container/VM tier (qemu, podman; needs root-ish prerequisites, shared host)
  --keep             keep the work directory
  --yes              do not ask before uploading
  -h, --help
USAGE
}

DRY=0 PUBLISH=0 TOKEN_FILE="" ONLY="" SET_BOUNDS=0 NO_DOCS=0 DOCS_ONLY=0 SKIP_TESTS=0 HEAVY=0 KEEP=0 YES=0
while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run) DRY=1 ;;
    --publish) PUBLISH=1 ;;
    --token-file) TOKEN_FILE="${2:?--token-file needs a file}"; shift ;;
    --packages) ONLY="${2:?--packages needs a comma-separated list}"; shift ;;
    --set-bounds) SET_BOUNDS=1 ;;
    --no-docs) NO_DOCS=1 ;;
    --docs-only) DOCS_ONLY=1 ;;
    --skip-tests) SKIP_TESTS=1 ;;
    --heavy-tests) HEAVY=1 ;;
    --keep) KEEP=1 ;;
    --yes) YES=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

[ "$NO_DOCS$DOCS_ONLY" != 11 ] || { echo "--no-docs and --docs-only are incompatible" >&2; exit 2; }
[ "$DOCS_ONLY$PUBLISH" != 11 ] || { echo "--docs-only already publishes the docs; drop --publish" >&2; exit 2; }

cd "$(git rev-parse --show-toplevel)"
say() { printf '\n== %s\n' "$*"; }
die() { printf 'release-hackage: %s\n' "$*" >&2; exit 1; }

version_of() { awk '/^version:/ {print $2; exit}' "$1/$1.cabal"; }

# ---- 1. versions ------------------------------------------------------------
# Packages may sit at different versions: one left unchanged since the last release keeps the
# version already on Hackage and is not uploaded again. Every package is still sdisted and built
# from its tarball below, since the others build against it.
declare -A VER
for p in "${PACKAGES[@]}"; do
  VER[$p]="$(version_of "$p")"
  [ -n "${VER[$p]}" ] || die "cannot read version of $p"
done

# ---- 2. internal bounds -----------------------------------------------------
# An internal dependency line is "    , salmon-core" (bare) or "    , salmon-ops ^>=X".
# Each must read "^>=" followed by that dependency's own version.
INTERNAL='^[[:space:]]*,?[[:space:]]*salmon-(core|ops-recipes|ops)([[:space:]]|,|$)'
if [ "$SET_BOUNDS" = 1 ]; then
  for p in "${PACKAGES[@]}"; do
    for dep in salmon-core salmon-ops salmon-ops-recipes; do
      sed -E -i "s/^([[:space:]]*,?[[:space:]]*$dep)([[:space:]].*)?\$/\\1 ^>=${VER[$dep]}/" "$p/$p.cabal"
    done
  done
  echo "internal bounds set to each dependency's own version; review and commit the change, then re-run."
  exit 0
fi
bad=0
for p in "${PACKAGES[@]}"; do
  f="$p/$p.cabal"
  while IFS= read -r line; do
    dep="$(printf '%s\n' "$line" | sed -E 's/^[[:space:]]*,?[[:space:]]*(salmon-[a-z-]+).*/\1/')"
    case "$line" in
      *"$dep ^>=${VER[$dep]}") ;;
      *) printf '  %s: %s (want ^>=%s)\n' "$f" "$line" "${VER[$dep]}" >&2; bad=1 ;;
    esac
  done < <(grep -E "$INTERNAL" "$f" || true)
done
[ "$bad" = 0 ] || die "internal dependencies above lack ^>= their own version (run with --set-bounds, review, commit)"

# ---- 2b. which packages are released ----------------------------------------
# Default: those whose version is not yet published on Hackage (a candidate does not count, and
# re-uploading one replaces it). --docs-only defaults to all of them. --packages overrides both.
on_hackage() {   # prints 200 (published), 404 (not), or something else (could not tell)
  curl -s -o /dev/null -w '%{http_code}' "https://hackage.haskell.org/package/$1-$2" || true
}
RELEASE=()
if [ -n "$ONLY" ]; then
  IFS=, read -r -a wanted <<< "$ONLY"
  for w in "${wanted[@]}"; do
    [ -n "${VER[$w]:-}" ] || die "--packages: unknown package $w"
  done
  for p in "${PACKAGES[@]}"; do   # keep dependency order
    for w in "${wanted[@]}"; do [ "$w" = "$p" ] && RELEASE+=("$p"); done
  done
elif [ "$DOCS_ONLY" = 1 ]; then
  RELEASE=("${PACKAGES[@]}")
else
  for p in "${PACKAGES[@]}"; do
    code="$(on_hackage "$p" "${VER[$p]}")"
    case "$code" in
      200) echo "  $p-${VER[$p]} is already published on Hackage: not released again" ;;
      404) RELEASE+=("$p") ;;
      *) die "cannot tell whether $p-${VER[$p]} is on Hackage (HTTP '$code'); name the packages with --packages" ;;
    esac
  done
fi
[ "${#RELEASE[@]}" -gt 0 ] || die "nothing to release: every package's version is already on Hackage (bump a version, or --packages)"
# The released packages share one version: it names the tag. (--docs-only uploads the docs of
# each package at its own version and tags nothing, so it does not need one.)
VERSION="${VER[${RELEASE[0]}]}"
if [ "$DOCS_ONLY" != 1 ]; then
  for p in "${RELEASE[@]}"; do
    [ "${VER[$p]}" = "$VERSION" ] || die "version mismatch among released packages: $p is ${VER[$p]}, ${RELEASE[0]} is $VERSION"
  done
fi
say "releasing$(for p in "${RELEASE[@]}"; do printf ' %s-%s' "$p" "${VER[$p]}"; done)"
is_released() { case " ${RELEASE[*]} " in *" $1 "*) return 0 ;; *) return 1 ;; esac; }
for p in "${PACKAGES[@]}"; do
  is_released "$p" || echo "  $p stays at ${VER[$p]} (built from its tarball, not uploaded)"
done

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
  if [ "$DOCS_ONLY" != 1 ] && is_released "$p"; then
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
  fi
  say "cabal sdist $p"
  cabal sdist "$p" --output-directory="$SDIST" >/dev/null
done
ls -1 "$SDIST"

# ---- 5. build and test from the tarballs ------------------------------------
say "unpacking tarballs"
UNPACK="$WORK/unpacked"; mkdir -p "$UNPACK"
for p in "${PACKAGES[@]}"; do
  tar -xzf "$SDIST/$p-${VER[$p]}.tar.gz" -C "$UNPACK"
done
{
  echo "packages:"
  for p in "${PACKAGES[@]}"; do echo "  $UNPACK/$p-${VER[$p]}"; done
  # carry the repo's solver tweaks (allow-newer), not its package list
  if [ -f cabal.project.local ]; then grep -v '^ignore-project' cabal.project.local || true; fi
} > "$UNPACK/cabal.project"
# Build outside the working tree so nothing in it can leak in; the package store is shared.
BUILD="$WORK/dist"
say "build from tarballs"
if [ "$DOCS_ONLY" = 1 ]; then
  say "build and tests skipped (--docs-only)"
else
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
fi

# ---- 5b. haddock tarballs ---------------------------------------------------
# Built from the unpacked tarballs, so the docs match what is released. Hackage builds docs
# itself for published packages, but late or not at all; uploading them is the manual path.
DOCS="$WORK/docs"; mkdir -p "$DOCS"
DOCS_FAILED=0
if [ "$NO_DOCS" = 1 ]; then
  say "docs skipped (--no-docs)"
else
  for p in "${RELEASE[@]}"; do
    say "cabal haddock --haddock-for-hackage $p"
    out="$WORK/haddock-$p.log"
    ok=0
    if cabal haddock --haddock-for-hackage --enable-documentation \
         --project-file="$UNPACK/cabal.project" --builddir="$BUILD" "$p" > "$out" 2>&1; then
      ok=1
    fi
    cat "$out" >&2
    if [ "$ok" = 1 ]; then
      # take the path cabal printed; fall back to searching the build directory
      # cabal prints the path on the line after "Documentation tarball created:", and also for
      # dependencies it documents on the way, so pick the line that names this package's tarball.
      tb="$(grep -E "/$p-${VER[$p]}-docs\\.tar\\.gz\$" "$out" | tail -n1 || true)"
      if [ -z "$tb" ] || [ ! -f "$tb" ]; then
        tb="$(find "$BUILD" -name "$p-${VER[$p]}-docs.tar.gz" -print -quit)"
      fi
      if [ -n "$tb" ] && [ -f "$tb" ]; then
        cp "$tb" "$DOCS/$p-${VER[$p]}-docs.tar.gz"
        continue
      fi
      echo "release-hackage: cannot find the docs tarball for $p" >&2
    fi
    if [ "$DRY" = 1 ]; then
      echo "release-hackage: WOULD BLOCK A REAL RELEASE: haddock failed for $p" >&2
      DOCS_FAILED=1
    else
      die "haddock failed for $p"
    fi
  done
  ls -1 "$DOCS"
fi

# ---- 6. upload --------------------------------------------------------------
DOCS_PUBLISH="$PUBLISH"; [ "$DOCS_ONLY" = 1 ] && DOCS_PUBLISH=1
what="candidates"; [ "$PUBLISH" = 1 ] && what="PUBLISHED releases (irreversible)"
if [ "$DRY" = 1 ]; then
  say "dry run: stopping before the upload"
  if [ "$CHECK_FAILED" = 1 ]; then echo "NOTE: cabal check failed above; a real run would have stopped there."; fi
  if [ "$DOCS_FAILED" = 1 ]; then echo "NOTE: haddock failed above; a real run would have stopped there."; fi
  if [ "$DOCS_ONLY" != 1 ]; then
    echo "would upload as $what, in this order:"
    for p in "${RELEASE[@]}"; do echo "  $p-${VER[$p]}.tar.gz"; done
  fi
  if [ "$NO_DOCS" != 1 ]; then
    dwhat="the candidate"; if [ "$PUBLISH" = 1 ] || [ "$DOCS_ONLY" = 1 ]; then dwhat="the PUBLISHED release"; fi
    echo "would upload docs (cabal upload -d) to $dwhat, in this order:"
    for p in "${RELEASE[@]}"; do echo "  $p-${VER[$p]}-docs.tar.gz"; done
  fi
  if [ "$PUBLISH" = 1 ] && [ "$DOCS_ONLY" != 1 ]; then echo "would then run: git tag -a v$VERSION $COMMIT (never pushed)"; fi
  exit 0
fi

if [ -n "$TOKEN_FILE" ]; then
  [ -r "$TOKEN_FILE" ] || die "cannot read $TOKEN_FILE"
  HACKAGE_TOKEN="$(tr -d '[:space:]' < "$TOKEN_FILE")"
fi
# With no token, cabal uses its own configured credentials (its config file or prompt).
if [ -n "${HACKAGE_TOKEN:-}" ]; then creds="the API token"; else creds="cabal's configured credentials"; fi

if [ "$YES" != 1 ]; then
  todo=""
  if [ "$DOCS_ONLY" != 1 ]; then todo="the packages as $what"; fi
  if [ "$NO_DOCS" != 1 ]; then
    d="docs to the candidate"; [ "$DOCS_PUBLISH" = 1 ] && d="docs to the PUBLISHED release"
    todo="${todo:+$todo and }$d"
  fi
  printf 'Upload %s (%s), using %s? [y/N] ' "$(for p in "${RELEASE[@]}"; do printf '%s-%s ' "$p" "${VER[$p]}"; done)" "$todo" "$creds"
  read -r ans; [ "$ans" = y ] || die "aborted"
fi

if [ "$DOCS_ONLY" != 1 ]; then
  for p in "${RELEASE[@]}"; do
    say "upload $p ${VER[$p]} ($what)"
    flags=()
    if [ -n "${HACKAGE_TOKEN:-}" ]; then flags+=(--token="$HACKAGE_TOKEN"); fi
    if [ "$PUBLISH" = 1 ]; then flags+=(--publish); fi
    cabal upload ${flags[@]+"${flags[@]}"} "$SDIST/$p-${VER[$p]}.tar.gz"
  done
fi

if [ "$NO_DOCS" != 1 ]; then
  for p in "${RELEASE[@]}"; do
    say "upload docs $p ${VER[$p]}"
    flags=(-d)
    if [ -n "${HACKAGE_TOKEN:-}" ]; then flags+=(--token="$HACKAGE_TOKEN"); fi
    if [ "$DOCS_PUBLISH" = 1 ]; then flags+=(--publish); fi
    cabal upload "${flags[@]}" "$DOCS/$p-${VER[$p]}-docs.tar.gz"
  done
fi

if [ "$DOCS_ONLY" = 1 ]; then
  say "docs published; nothing else touched, no tag"
elif [ "$PUBLISH" = 1 ]; then
  git tag -a "v$VERSION" -m "salmon $VERSION" "$COMMIT"
  say "tagged v$VERSION locally; push it yourself: git push <remote> v$VERSION"
else
  say "candidates uploaded; check them on Hackage, then re-run with --publish"
fi
