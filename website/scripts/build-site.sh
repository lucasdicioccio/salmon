#!/usr/bin/env bash
# Regenerates the mirrored pages (sync-repo-docs.sh) and produces the site
# into OUTDIR (default docs/, what GitHub Pages serves from master — the
# guides themselves live in resources/). `kitchen-sink produce` writes into
# an output skeleton it does not create, hence the mkdir; .nojekyll keeps
# GitHub Pages from running Jekyll over the output.
set -euo pipefail
cd "$(dirname "$0")/../.."  # repo root
OUT="${1:-docs}"
./website/scripts/sync-repo-docs.sh
mkdir -p "$OUT"/{audios,css,docs,gen,hashtags,images,js,json,raw,text,topics,videos}
kitchen-sink produce --srcDir website/src --outDir "$OUT" > /dev/null
touch "$OUT/.nojekyll"
echo "produced $(ls "$OUT"/*.html | wc -l) pages into $OUT/"
