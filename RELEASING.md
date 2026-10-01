# Releasing the salmon libraries to Hackage

Four packages are released together, at one shared version, in dependency order:
`salmon-core`, `salmon-ops`, `salmon-ops-recipes`, `salmon-apps`.
(`salmon-ops-recipes-experimental` is not released.) `scripts/release-hackage.sh` does the work.

## Before you run it

1. Bump `version:` in all four `.cabal` files to the same number.
2. If the version changed, run `scripts/release-hackage.sh --set-bounds`. It rewrites every
   internal dependency to `^>=VERSION` (e.g. `salmon-core ^>=0.1.0.0`); review and commit.
3. Commit everything. The script refuses to run if tracked files are modified.
4. Make sure `cabal check` is clean of errors in each package (Hackage rejects a package with
   no `license`, `synopsis`/`description`, or an upper bound on `base`).
5. Credentials: either rely on cabal's own configured credentials (nothing to do if
   `cabal upload` already works for you), or create a Hackage API token (hackage.haskell.org,
   account page) and keep it in the environment as `HACKAGE_TOKEN`, or in a file passed with
   `--token-file FILE`. A token, when given, wins over cabal's credentials. The confirmation
   prompt says which will be used. The script never stores a token and never prints it.

## Run it

```sh
scripts/release-hackage.sh --dry-run     # everything except the upload
scripts/release-hackage.sh               # same, then upload all four as candidates
scripts/release-hackage.sh --publish     # upload as published releases, then tag
scripts/release-hackage.sh --docs-only   # only the Haddocks, for a version already on Hackage
```

Options of note: `--no-docs` skips the Haddock build and upload; `--skip-tests` skips the test suites.

What it does, stopping at the first failure:

1. checks the four versions agree and the internal bounds are `^>=VERSION`;
2. checks the tracked tree is clean;
3. `cabal check` and `cabal sdist` for each package;
4. unpacks the tarballs into a temporary directory and runs `cabal build all` and
   `cabal test all` there, with a generated `cabal.project` listing only the unpacked
   packages (so a file missing from `extra-source-files` fails here and not on Hackage);
   `--skip-tests` skips the tests. The podman-backed tests of `salmon-ops-recipes` are skipped
   loudly when `podman` is absent;
5. builds each package's documentation tarball with `cabal haddock --haddock-for-hackage` inside
   the same unpacked-tarball project (`--no-docs` skips this). A Haddock failure stops the script
   (a dry run reports it and carries on);
6. uploads with `cabal upload` in dependency order, after a `[y/N]` question (`--yes` skips it).
   Without `--publish` these are candidates; with it they are releases, which cannot be undone.
   The docs tarballs are then uploaded with `cabal upload -d` (to the candidate, or with `--publish`
   to the published release). The prompt says what will be uploaded and where;
7. after `--publish` only, creates the annotated tag `vVERSION` **locally**. The script never
   pushes: `git push <remote> vVERSION` is yours to run.

`--dry-run` also tells you, without uploading anything, what a real run would upload and tag.

## Candidates, then publish

1. Run without `--publish`. Look at each candidate page on Hackage.
2. A candidate of a dependant cannot build on Hackage until the package it depends on is
   published (candidates do not resolve against each other), so the build reports of
   `salmon-ops`, `salmon-ops-recipes` and `salmon-apps` may show failures until `salmon-core`
   is out. Judge those by the local build from the tarballs, which the script already ran.
3. Run again with `--publish`. Re-uploading a candidate that already exists replaces it.
4. Push the tag.

## Docs for a release that is already published

Hackage builds docs itself for published packages, but late or not at all. To upload them by hand
for the version in the `.cabal` files (no package upload, no tag):

```sh
scripts/release-hackage.sh --docs-only --dry-run   # builds all four Haddocks, uploads nothing
scripts/release-hackage.sh --docs-only             # same, then publishes the docs
```

`--docs-only` publishes the docs (they replace any existing ones); `--dry-run` is the way to look first.

## Known gaps

- There is no changelog check; write `CHANGELOG.md` entries before running.
