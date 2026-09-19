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
import Salmon.Builtin.Nodes.Debian.Package (interpretDpkgCatalog)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Debian.Package.interpretDpkgCatalog"
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
