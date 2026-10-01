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
5. Create a Hackage API token (hackage.haskell.org, account page) and keep it in the
   environment as `HACKAGE_TOKEN`, or in a file passed with `--token-file FILE`.
   The script never stores it and never prints it.

## Run it

```sh
scripts/release-hackage.sh --dry-run     # everything except the upload
scripts/release-hackage.sh               # same, then upload all four as candidates
scripts/release-hackage.sh --publish     # upload as published releases, then tag
```

What it does, stopping at the first failure:

1. checks the four versions agree and the internal bounds are `^>=VERSION`;
2. checks the tracked tree is clean;
3. `cabal check` and `cabal sdist` for each package;
4. unpacks the tarballs into a temporary directory and runs `cabal build all` and
   `cabal test all` there, with a generated `cabal.project` listing only the unpacked
   packages (so a file missing from `extra-source-files` fails here and not on Hackage);
   `--skip-tests` skips the tests. The podman-backed tests of `salmon-ops-recipes` are skipped
   loudly when `podman` is absent;
5. uploads with `cabal upload` in dependency order, after a `[y/N]` question (`--yes` skips it).
   Without `--publish` these are candidates; with it they are releases, which cannot be undone;
6. after `--publish` only, creates the annotated tag `vVERSION` **locally**. The script never
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

## Known gaps

- Haddocks are not uploaded; Hackage builds them itself for published packages.
- There is no changelog check; write `CHANGELOG.md` entries before running.
