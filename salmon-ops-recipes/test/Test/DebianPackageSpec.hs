{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Debian.Package"'s @check@.

The check exists so that a graph naming packages it already has can run as an
ordinary user (@apt-get install@ wants root even with nothing to do). Its one
subtlety is virtual packages: 'Salmon.Builtin.Nodes.Debian.OS' asks for
@ssh-client@, which @apt-get@ resolves to @openssh-client@ while
@dpkg-query@ answers @not-installed@ for that name forever. Sample lines
below are real @dpkg-query -W@ output.
-}
module Test.DebianPackageSpec (tests) where

import GHC.IO.Exception (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.Debian.Package (interpretAptPolicy, interpretDpkgCatalog)

tests :: TestTree
tests = testGroup "Salmon.Builtin.Nodes.Debian.Package" [catalogTests, policyTests]

{- | 'interpretAptPolicy': whether the apt index has a candidate for every
package, which is what decides if @apt-get update@ runs before an install.
The stanzas are real @LC_ALL=C apt-cache policy@ output (Ubuntu 24.04).
-}
policyTests :: TestTree
policyTests =
    testGroup
        "interpretAptPolicy"
        [ testCase "a candidate for every package needs no refresh" $
            assertEqual "" Success (interpretAptPolicy False ["podman", "rsync"] ExitSuccess policy)
        , testCase "Candidate: (none) is no candidate" $
            -- what a fresh cloud image says of a universe package it has
            -- only heard of through another package's dependencies
            assertEqual
                ""
                (Failure "no installation candidate in the apt index for: pgbouncer")
                (interpretAptPolicy False ["podman", "pgbouncer"] ExitSuccess policy)
        , testCase "a name apt prints no stanza for is no candidate" $
            assertEqual
                ""
                (Failure "no installation candidate in the apt index for: nosuchpkg")
                (interpretAptPolicy False ["rsync", "nosuchpkg"] ExitSuccess policy)
        , testCase "an empty index has no candidate for anything" $
            assertEqual
                ""
                (Failure "no installation candidate in the apt index for: podman, rsync")
                (interpretAptPolicy False ["podman", "rsync"] ExitSuccess "")
        , testCase "an installed version is not a candidate" $
            -- "Installed:" and the version table both carry version strings;
            -- only the Candidate line decides
            assertBool "" (isFailure (interpretAptPolicy False ["orphan"] ExitSuccess policy))
        , testCase "after a recent refresh there is nothing more a refresh can do" $
            -- a virtual name (ssh-client) or a typo: left to the install to report
            assertEqual "" Success (interpretAptPolicy True ["pgbouncer", "nosuchpkg"] ExitSuccess policy)
        , testCase "apt-cache failing outright is 'cannot tell'" $
            assertEqual "" Unknown (interpretAptPolicy False ["rsync"] (ExitFailure 100) "")
        ]
  where
    isFailure (Failure _) = True
    isFailure _ = False

    policy =
        "pgbouncer:\n\
        \  Installed: (none)\n\
        \  Candidate: (none)\n\
        \  Version table:\n\
        \podman:\n\
        \  Installed: 4.9.3+ds1-1ubuntu0.2\n\
        \  Candidate: 4.9.3+ds1-1ubuntu0.2\n\
        \  Version table:\n\
        \ *** 4.9.3+ds1-1ubuntu0.2 500\n\
        \        500 http://archive.ubuntu.com/ubuntu noble-updates/universe amd64 Packages\n\
        \        100 /var/lib/dpkg/status\n\
        \     4.9.3+ds1-1build2 500\n\
        \        500 http://archive.ubuntu.com/ubuntu noble/universe amd64 Packages\n\
        \orphan:\n\
        \  Installed: 1.0-1\n\
        \  Candidate: (none)\n\
        \  Version table:\n\
        \ *** 1.0-1 -1\n\
        \        100 /var/lib/dpkg/status\n\
        \rsync:\n\
        \  Installed: (none)\n\
        \  Candidate: 3.2.7-1ubuntu1.5\n\
        \  Version table:\n\
        \     3.2.7-1ubuntu1.5 500\n\
        \        500 http://archive.ubuntu.com/ubuntu noble-updates/main amd64 Packages\n"

catalogTests :: TestTree
catalogTests =
    testGroup
        "interpretDpkgCatalog"
        [ testCase "an installed package is satisfied" $
            assertEqual "" Success (interpretDpkgCatalog ["rsync"] ExitSuccess catalogue)
        , testCase "a virtual name counts as installed when its provider is" $
            assertEqual
                "apt-get install ssh-client resolves to openssh-client"
                Success
                (interpretDpkgCatalog ["ssh-client"] ExitSuccess catalogue)
        , testCase "a provides entry carrying a version still matches" $
            assertEqual "" Success (interpretDpkgCatalog ["awk"] ExitSuccess catalogue)
        , testCase "a package present but not installed is not satisfied" $
            assertBool "" (isFailure (interpretDpkgCatalog ["podman"] ExitSuccess catalogue))
        , testCase "a package dpkg has never heard of is not satisfied" $
            assertBool "" (isFailure (interpretDpkgCatalog ["nosuchpkg"] ExitSuccess catalogue))
        , testCase "every wanted package must be installed, not just one" $
            assertBool "" (isFailure (interpretDpkgCatalog ["rsync", "podman"] ExitSuccess catalogue))
        , testCase "the reason names what is missing" $
            case interpretDpkgCatalog ["podman", "rsync"] ExitSuccess catalogue of
                Failure msg -> assertEqual "" "not installed: podman" msg
                other -> assertBool ("expected Failure, got " <> show other) False
        , testCase "dpkg-query failing outright is 'cannot tell', not 'missing'" $
            -- It exits non-zero when something else holds the dpkg lock
            -- (unattended-upgrades, typically). Reading that as "missing"
            -- makes the node run apt-get, which fails on the same lock --
            -- seen for real in a full test-suite run.
            assertEqual "" Unknown (interpretDpkgCatalog ["rsync"] (ExitFailure 2) "")
        ]
  where
    isFailure (Failure _) = True
    isFailure _ = False

    -- real `dpkg-query -W -f='${db:Status-Status}|${binary:Package}|${Provides}\n'` lines
    catalogue =
        "installed|rsync|\n\
        \installed|openssh-client|ssh-client\n\
        \installed|original-awk|awk (= 2020-05-22)\n\
        \not-installed|podman|\n\
        \installed|libc6:i386|libc6-i386\n"
