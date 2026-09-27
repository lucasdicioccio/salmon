#!/usr/bin/env bash
# Generator for the getting-started page's "run tree" example: pipes a real
# `salmon-ops-serve-fixture config | salmon-ops-serve-fixture run tree`
# through, so the tree shown on the page is what the binary actually prints
# today rather than a paste that can drift from it. Invoked by kitchen-sink
# at produce/serve time via getting-started.cmark's =generator:cmd.json
# section; run from the repository root (kitchen-sink runs generators with
# the working directory it was started from).
set -euo pipefail
cd "$(dirname "$0")/../.."  # repo root

BIN="cabal run -v0 salmon-ops-serve-fixture --"

$BIN config --dir /tmp/salmon-getting-started-demo --name web --file index.html --file style.css \
  | $BIN run tree
