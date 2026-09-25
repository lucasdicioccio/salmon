# The salmon website

A [Kitchen-Sink](https://kitchensink-tech.github.io/) site, the same shape as
[tramaj's](https://github.com/lucasdicioccio/tramaj/tree/main/website). A
page is a `.cmark` file split into *sections* (content, metadata, CSS);
Kitchen-Sink assembles them into a static site.

- `src/` — the source: `kitchen-sink.json` (site config), the hand-written
  pages (`index.cmark`, `getting-started.cmark`, `docs.cmark`, `llms.txt`),
  the layout pages (`topics`, `hashtags`, `glossary`) and the CSS/JS they
  reference.
- `scripts/` — `sync-repo-docs.sh` regenerates the mirrored pages
  (`docs-*.cmark` from `resources/`, `specs-*.cmark` and `specs.cmark` from
  `specs/`, `builtins.cmark` from the README's tables); `mirror.py` and
  `md_tables_to_html.py` are what it runs. Those outputs are gitignored:
  the repository's markdown is the source of truth, the site is a view of it.
  `build-site.sh` runs the sync and then `kitchen-sink produce` into `docs/`.
- `../docs/` — the produced site, tracked: GitHub Pages serves it from
  `master`, the way tramaj's is served (its guides moved to `resources/` to
  free the name).
- `www/` — the dev server's output directory (gitignored).

## Regenerate and preview

```sh
./website/scripts/sync-repo-docs.sh
kitchen-sink serve --srcDir website/src --outDir website/www --servMode DEV --httpPort 7655
```

Then open http://localhost:7655/. The dev server rebuilds on file changes
under `src/`; re-run the sync script after editing anything under
`resources/`, `specs/` or the README.

To produce what GitHub Pages serves, and commit it:

```sh
./website/scripts/build-site.sh          # -> docs/
git add docs
```

`kitchen-sink.json`'s `basePath` is `/salmon`, so every absolute `/x.html`
link and every CSS import (through `$ctx.pathPrefix`) resolves under
`https://lucasdicioccio.github.io/salmon/`.

## Learn more

- [Features](https://kitchensink-tech.github.io/features.html) — what
  Kitchen-Sink can do.
- [Sections](https://kitchensink-tech.github.io/sections.html) — the
  section format used inside each `.cmark` file.
