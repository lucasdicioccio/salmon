#!/usr/bin/env bash
# Regenerates the mirrored pages of website/src from the repository:
#
#   resources/<name>.md -> website/src/docs-<name>.cmark
#   specs/<name>.md     -> website/src/specs-<name>.cmark
#   README.md's tables  -> website/src/builtins.cmark   (Builtin nodes / Recipes / Binaries)
#   the specs' Status:  -> website/src/specs.cmark      (one line per spec)
#
# so the site never needs hand-copying when a doc changes. Everything this
# writes is mechanical output, not a source of truth — gitignored (see
# .gitignore) and regenerated fresh each run, including the "generated" date.
# build-site.sh runs this and then produces the site.
set -euo pipefail
cd "$(dirname "$0")/../.."  # repo root

OUT=website/src
MIRROR="python3 website/scripts/mirror.py"
GITHUB="https://github.com/lucasdicioccio/salmon/blob/master"
DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

title_of() { sed -n '1s/^# //p' "$1"; }

# the sentence after "# Title": the first non-empty paragraph, joined
summary_of() {
  awk 'NR>1 && NF {print; next} NR>1 && !NF && seen {exit} {if (NR>1 && NF) seen=1}' "$1" \
    | tr '\n' ' ' | sed -e 's/  */ /g' -e 's/ $//' | cut -c1-240
}

# resources/: the guides
for src in resources/*.md; do
  name="$(basename "$src" .md)"
  $MIRROR --kind docs --source "$src" --title "$(title_of "$src")" --topic docs \
    --keywords "guide, documentation" --summary "$(summary_of "$src")" \
    --github "$GITHUB/$src" --date "$DATE" > "$OUT/docs-$name.cmark"
done

# specs/: the design sketches, each headed by a Status: line
for src in specs/*.md; do
  name="$(basename "$src" .md)"
  status="$(grep -m1 -i '^status:' "$src" | sed 's/^[Ss]tatus: *//' | cut -c1-240)"
  $MIRROR --kind specs --source "$src" --title "$(title_of "$src")" --topic specs \
    --keywords "spec, design" --summary "Status: $status" \
    --github "$GITHUB/$src" --date "$DATE" > "$OUT/specs-$name.cmark"
done

# README's three tables, as one page
$MIRROR --kind readme-section --source README.md --title "Builtins, recipes and binaries" \
  --topic reference --keywords "builtins, recipes, binaries, nodes" \
  --summary "Every builtin node module, every recipe and every shipped binary, one line each, as the README lists them." \
  --github "$GITHUB/README.md" --date "$DATE" \
  --section-from "## Builtin nodes" --section-to "## Docs" \
  --intro "# Builtins, recipes and binaries" > "$OUT/builtins.cmark"

# the specs index: title, status, link, one entry per spec
{
  cat <<EOF
=base:build-info.json
{"layout":"article"
,"publicationStatus":"Public"
}

=base:preamble.json
{"author": "Lucas DiCioccio"
,"date": "$DATE"
,"title": "Specs"
}

=base:topic.json
{"topics":["specs"]
,"keywords":["design", "roadmap", "status"]
}

=base:social.json
{"twitter": "lucasdicioccio"
,"linkedin": "lucasdicioccio"
,"github": "lucasdicioccio"
,"mastodon": "https://fosstodon.org/@lucasdicioccio"
}

=base:summary.cmark
The design sketches under specs/, each headed by a Status line saying what of it has shipped.

=base:main-content.cmark

# Specs

The \`specs/\` directory holds design sketches: opinionated documents to react
to, some of which then got built. Each starts with a **Status** line saying
what of it has shipped, which is reproduced here. These pages are generated
from the [salmon repository]($GITHUB/../tree/master/specs) — the repository
is the canonical source, and may be ahead of what is published here.

EOF
  for src in specs/*.md; do
    name="$(basename "$src" .md)"
    status="$(grep -m1 -i '^status:' "$src" | sed 's/^[Ss]tatus: *//')"
    printf -- '- [**%s**](/specs-%s.html) — *Status:* %s\n' "$(title_of "$src")" "$name" "$status"
  done
  cat <<'EOF'

=base:main-css.tramaj-json
{ "format": "css"
, "contents":
  [ ""
  , "@import \"`$ctx.pathPrefix`/css/dev.css\";"
  , "@import \"`$ctx.pathPrefix`/css/colors.css\";"
  , "@import \"`$ctx.pathPrefix`/css/article.css\";"
  , "@import \"`$ctx.pathPrefix`/css/navigation.css\";"
  ]
}
EOF
} > "$OUT/specs.cmark"

echo "synced $(ls $OUT/docs-*.cmark | wc -l) docs, $(ls $OUT/specs-*.cmark | wc -l) specs, builtins.cmark, specs.cmark"
