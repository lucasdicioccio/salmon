# Releasing the salmon libraries to Hackage

Four packages, in dependency order: `salmon-core`, `salmon-ops`, `salmon-ops-recipes`,
`salmon-apps`. (`salmon-ops-recipes-experimental` is not released.) `scripts/release-hackage.sh`
does the work.

The packages released together share one version (it names the tag `vVERSION`). A package
unchanged since the last release keeps the version already on Hackage and is not uploaded again
(0.2.0.0 and 0.3.0.0 released `salmon-ops`, `salmon-ops-recipes` and `salmon-apps`; `salmon-core`
stayed at 0.1.0.0). It is still sdisted and built from its tarball, since the others build against it.

## Before you run it

1. Bump `version:` in the `.cabal` files of the packages to release (PVP: a removed or changed
   export is a major bump), all to the same number.
2. Run `scripts/release-hackage.sh --set-bounds`. It rewrites every internal dependency to
   `^>=` that package's own version (e.g. `salmon-core ^>=0.1.0.0`, `salmon-ops ^>=0.2.0.0`);
   review and commit.
3. Check the external bounds (see below) still admit what `cabal build all` resolves, and
   widen any that a dependency's new major version has outgrown once the build passes with it.
4. Commit everything. The script refuses to run if tracked files are modified.
5. Make sure `cabal check` is clean of errors in each package (Hackage rejects a package with
   no `license`, `synopsis`/`description`, or an upper bound on `base`).
6. Credentials: either rely on cabal's own configured credentials (nothing to do if
   `cabal upload` already works for you), or create a Hackage API token (hackage.haskell.org,
   account page) and keep it in the environment as `HACKAGE_TOKEN`, or in a file passed with
   `--token-file FILE`. A token, when given, wins over cabal's credentials. The confirmation
   prompt says which will be used. The script never stores a token and never prints it.

## Run it

```sh
scripts/release-hackage.sh --dry-run     # everything except the upload
scripts/release-hackage.sh               # same, then upload the packages to release as candidates
scripts/release-hackage.sh --publish     # upload as published releases, then tag
scripts/release-hackage.sh --docs-only   # only the Haddocks, for a version already on Hackage
```

Options of note: `--no-docs` skips the Haddock build and upload; `--skip-tests` skips the test suites.

What it does, stopping at the first failure:

1. checks each internal bound is `^>=` the dependency's own version, then picks the packages to
   release: those whose version is not yet published on Hackage (it asks hackage.haskell.org;
   a candidate does not count), or the ones named with `--packages salmon-ops,salmon-apps`.
   `--docs-only` defaults to all four. The released packages must share one version;
2. checks the tracked tree is clean;
3. `cabal check` for each released package and `cabal sdist` for all four;
4. unpacks the tarballs into a temporary directory and runs `cabal build all` and
   `cabal test all` there, with a generated `cabal.project` listing only the unpacked
   packages (so a file missing from `extra-source-files` fails here and not on Hackage);
   `--skip-tests` skips the tests. The podman-backed tests of `salmon-ops-recipes` are skipped
   loudly when `podman` is absent;
5. builds each released package's documentation tarball with `cabal haddock --haddock-for-hackage` inside
   the same unpacked-tarball project (`--no-docs` skips this). A Haddock failure stops the script
   (a dry run reports it and carries on);
6. uploads the released packages with `cabal upload` in dependency order, after a `[y/N]` question (`--yes` skips it).
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
   `salmon-ops-recipes` and `salmon-apps` may show failures until the package they depend on
   is published (`salmon-ops` builds, since `salmon-core` is already out). Judge those by the local build from the tarballs, which the script already ran.
3. Run again with `--publish`. Re-uploading a candidate that already exists replaces it.
4. Push the tag.

## Docs for a release that is already published

Hackage builds docs itself for published packages, but late or not at all. To upload them by hand
for the version in the `.cabal` files (no package upload, no tag):

```sh
scripts/release-hackage.sh --docs-only --dry-run   # builds all four Haddocks, uploads nothing
scripts/release-hackage.sh --docs-only             # same, then publishes the docs
```

`--docs-only` publishes the docs (they replace any existing ones), each package at its own version;
`--packages` narrows it; `--dry-run` is the way to look first.

## External bounds

Every external `build-depends` of the four released packages, in every component (library,
executables, test suites, benchmark), carries a PVP major-version bound: `^>=` the version
`cabal build all` resolved on GHC 9.10.3 when the bound was last set, so the lower bound is
what was built and the upper bound is that version's next major (`aeson ^>=2.3.2.0` is
`>=2.3.2.0 && <2.4`). `base` keeps its explicit range. Internal dependencies are `--set-bounds`'s
business, above. `salmon-ops-recipes-experimental` is not released and has no bounds.

When a dependency's next major is out: build with it (`--allow-newer=PKG`), and if that passes,
widen the bound to an explicit range (`>=OLD && <NEXT`) or move the `^>=` to the new version.
`cabal check` warns of any external dependency left without an upper bound.

These bounds reach Hackage only with a new version (or a Hackage metadata revision); the
versions already published there were uploaded without them.

## Known gaps

- There is no changelog check; write `CHANGELOG.md` entries before running.
- Nothing checks that external bounds still admit the newest Hackage majors; a bound that
  has fallen behind shows only as a solver failure for someone on a newer index.
