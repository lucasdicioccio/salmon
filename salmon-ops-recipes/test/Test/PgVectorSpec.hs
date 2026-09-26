{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.PgVector" and the
'Salmon.Builtin.Nodes.Postgres.extension' node under it: the SQL it renders
(the version floor comes first, the drop has no @CASCADE@), the verdict drawn
from the catalogue, and the shape of the graph (one package however many
databases, the repository only when a caller supplied one).
-}
module Test.PgVectorSpec (tests) where

import Data.Functor.Identity (runIdentity)
import Data.List (nub)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Actions.Query (pathedNodes)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Debian.AptRepository (pgdg, viaRepository)
import Salmon.Builtin.Nodes.Debian.Package (Package (..))
import Salmon.Builtin.Nodes.PgVector
import Salmon.Builtin.Nodes.Postgres
import Salmon.Op.Eval (expand)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)

ext :: PgExtension
ext = PgExtension{extName = "vector", extDatabase = "app", extMinServerVersion = Just 130000, extUpgrade = False}

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.PgVector"
        [ testGroup
            "SQL"
            [ testCase "the version floor is checked before the extension is created" $ do
                let sql = createExtensionSql ext
                    (before, after) = Text.breakOn "CREATE EXTENSION" sql
                assertBool "floor first" ("server_version_num')::int < 130000" `Text.isInfixOf` before)
                assertBool "create present" ("CREATE EXTENSION IF NOT EXISTS \"vector\";" `Text.isPrefixOf` after)
            , testCase "no floor, no check" $
                assertBool "" (not ("server_version_num" `Text.isInfixOf` createExtensionSql ext{extMinServerVersion = Nothing}))
            , testCase "an upgrade is only issued when asked" $ do
                assertBool "" (not ("UPDATE" `Text.isInfixOf` createExtensionSql ext))
                assertBool "" ("ALTER EXTENSION \"vector\" UPDATE;" `Text.isInfixOf` createExtensionSql ext{extUpgrade = True})
            , testCase "the drop has no CASCADE" $
                assertEqual "" "DROP EXTENSION IF EXISTS \"vector\";\n" (dropExtensionSql ext)
            , testCase "a hostile extension name is quoted" $
                assertBool "" ("\"a\"\"b\"" `Text.isInfixOf` createExtensionSql ext{extName = "a\"b"})
            ]
        , testGroup
            "interpretExtensionRow"
            [ testCase "absent is missing" $
                assertEqual "" (Failure "extension vector is not installed in app") (interpretExtensionRow ext "")
            , testCase "present is satisfied" $
                assertEqual "" Success (interpretExtensionRow ext "0.8.0|0.8.6\n")
            , testCase "older than the package is reported only when upgrading" $ do
                assertEqual "" Success (interpretExtensionRow ext "0.7.4|0.8.6")
                assertEqual "" (Failure "extension vector is at 0.7.4, the package has 0.8.6") (interpretExtensionRow ext{extUpgrade = True} "0.7.4|0.8.6")
            , testCase "versions compare numerically, not as text" $
                assertEqual "" Success (interpretExtensionRow ext{extUpgrade = True} "0.10.0|0.9.9")
            , testCase "no package default means nothing to upgrade to" $
                assertEqual "" Success (interpretExtensionRow ext{extUpgrade = True} "0.7.4|")
            ]
        , testGroup
            "graph"
            [ testCase "the package name follows the declared major" $
                assertEqual "" (Package "postgresql-16-pgvector") (pgvectorPackage 16)
            , testCase "the only package is the versioned pgvector one" $
                assertEqual "" [Package "postgresql-16-pgvector"] (nub (packagesOf (node ignoreTrack)))
            , testCase "a supplied source is part of the graph, and ignoreTrack leaves it out" $ do
                let withRepo = node (viaRepository (pgdg "/k/pgdg.asc" "AAAA"))
                assertBool "repo node present" (any ("apt index" `Text.isInfixOf`) (shorthands withRepo))
                assertBool "no repo node" (not (any ("apt index" `Text.isInfixOf`) (shorthands (node ignoreTrack))))
            ]
        ]
  where
    node source =
        pgvector silent silent ignoreTrack source ignoreTrack (PgVector 16 5432 ["app", "other"] False)
    packagesOf :: Op -> [Package]
    packagesOf = concatMap snd . collectDynamics
    shorthands :: Op -> [Text.Text]
    shorthands o = [h | (_, _, h) <- pathedNodes (runIdentity (expand o))]
