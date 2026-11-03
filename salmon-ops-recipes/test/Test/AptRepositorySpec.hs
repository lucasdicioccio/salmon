{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Debian.AptRepository": the
rendering of the sources and preference files, the codename-to-suite
mapping, and the fingerprint pin. Sample @gpg --with-colons@ lines have the
shape real output has (a primary key, a user id, a signing subkey).
-}
module Test.AptRepositorySpec (tests) where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import qualified Data.List.NonEmpty as NEList
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Salmon.Builtin.Extension (Extension (..), Op, opAct)
import Salmon.Builtin.Nodes.Debian.AptRepository
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Eval (expand)

repo :: AptRepository
repo = pgdg "/secrets/pgdg.asc" "B97B 0AFC AA1A 47F0 44F2  44A0 7FCC 7D46 ACCC 4CF8"

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Debian.AptRepository"
        [ testCase "the sources file is a deb822 stanza naming the installed key" $
            assertEqual
                ""
                ( Text.unlines
                    [ "Types: deb"
                    , "URIs: https://apt.postgresql.org/pub/repos/apt"
                    , "Suites: bookworm-pgdg"
                    , "Components: main"
                    , "Signed-By: /etc/apt/keyrings/pgdg.asc"
                    ]
                )
                (renderSources repo (resolveSuite repo.repoSuite "bookworm"))
        , testCase "a codename-suffixed suite is the codename then the suffix" $
            assertEqual "" "bookworm-pgdg" (resolveSuite (CodenameSuffixed "-pgdg") "bookworm")
        , testCase "a fixed suite ignores the codename" $
            assertEqual "" "stable" (resolveSuite (FixedSuite "stable") "bookworm")
        , testCase "the key keeps the extension it was provisioned with" $ do
            assertEqual "" "/etc/apt/keyrings/pgdg.asc" (keyDestination repo)
            assertEqual "" "/etc/apt/keyrings/pgdg.gpg" (keyDestination repo{repoKey = KeyFile "/k/pgdg.gpg" "AAAA"})
            assertEqual "" "/etc/apt/keyrings/pgdg.gpg" (keyDestination repo{repoKey = KeyFile "/k/pgdg" "AAAA"})
        , testCase "a key given as bytes is installed as .asc when armored, .gpg otherwise" $ do
            assertEqual "" "/etc/apt/keyrings/pgdg.asc" (keyDestination repo{repoKey = KeyBytes "\n-----BEGIN PGP PUBLIC KEY BLOCK-----\n..." Nothing})
            assertEqual "" "/etc/apt/keyrings/pgdg.gpg" (keyDestination repo{repoKey = KeyBytes "\x99\x01\x0d" (Just "AAAA")})
            assertEqual "" False (isArmored "")
        , testCase "a key given as bytes is named in the notes by its sha256" $
            assertEqual
                ""
                "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                (keyBytesDigest "")
        , testCase "the codename can sit between a prefix and a suffix" $
            assertEqual "" "google-cloud-ops-agent-noble-2" (resolveSuite (CodenameInfixed "google-cloud-ops-agent-" "-2") "noble")
        , testCase "a key file renders the help and notes it always has" $
            assertEqual
                ""
                [ ("refreshes the apt index for pgdg", ["repository: https://apt.postgresql.org/pub/repos/apt", "key fingerprint pinned: B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8", "only packages: postgresql-*-pgvector"])
                , ("installs the signing key for pgdg", ["pinned fingerprint: B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8"])
                ]
                (keyAndIndex (aptRepository repo))
        , testCase "a key given as bytes shows its sha256 in the notes, and its fingerprint when declared" $ do
            assertEqual
                ""
                [ ("refreshes the apt index for pgdg", ["repository: https://apt.postgresql.org/pub/repos/apt", "key given inline, sha256: " <> keyBytesDigest "KEY", "only packages: postgresql-*-pgvector"])
                , ("installs the signing key for pgdg", ["key given inline, sha256: " <> keyBytesDigest "KEY"])
                ]
                (keyAndIndex (aptRepository repo{repoKey = KeyBytes "KEY" Nothing}))
            assertEqual
                ""
                [ ("refreshes the apt index for pgdg", ["repository: https://apt.postgresql.org/pub/repos/apt", "key fingerprint pinned: AAAA", "only packages: postgresql-*-pgvector"])
                , ("installs the signing key for pgdg", ["pinned fingerprint: AAAA", "key given inline, sha256: " <> keyBytesDigest "KEY"])
                ]
                (keyAndIndex (aptRepository repo{repoKey = KeyBytes "KEY" (Just "aa aa")}))
        , testCase "an aimed apt directory moves every path" $
            assertEqual "" "/t/sources.list.d/pgdg.sources" (sourcesPath repo{repoAptDir = "/t"})
        , testCase "codename comes from os-release, quoted or not" $ do
            assertEqual "" (Just "bookworm") (parseOsReleaseCodename "PRETTY_NAME=\"Debian\"\nVERSION_CODENAME=bookworm\nID=debian\n")
            assertEqual "" (Just "jammy") (parseOsReleaseCodename "VERSION_CODENAME=\"jammy\"\n")
            assertEqual "" Nothing (parseOsReleaseCodename "ID=debian\n")
            assertEqual "" Nothing (parseOsReleaseCodename "VERSION_CODENAME=\n")
        , testCase "the preference file pins the origin low, then the named packages up" $
            assertEqual
                ""
                ( Text.unlines
                    [ "Package: *"
                    , "Pin: origin apt.postgresql.org"
                    , "Pin-Priority: 1"
                    , ""
                    , "Package: postgresql-*-pgvector"
                    , "Pin: origin apt.postgresql.org"
                    , "Pin-Priority: 500"
                    , ""
                    , "Package: postgresql-*-textsearch"
                    , "Pin: origin apt.postgresql.org"
                    , "Pin-Priority: 500"
                    ]
                )
                (renderPreferences repo ("postgresql-*-pgvector" NEList.:| ["postgresql-*-textsearch"]))
        , testCase "the host of a URI, with and without a port or scheme" $ do
            assertEqual "" "apt.postgresql.org" (repositoryHost "https://apt.postgresql.org/pub/repos/apt")
            assertEqual "" "mirror.local:8080" (repositoryHost "http://mirror.local:8080/debian")
        , testCase "index files are named after the URI" $
            assertEqual "" "apt.postgresql.org_pub_repos_apt" (listsPrefix "https://apt.postgresql.org/pub/repos/apt/")
        , testCase "fingerprints compare without spaces or case" $
            assertEqual "" "B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8" (normalizeFingerprint "b97b 0afc aa1a 47f0 44f2  44a0 7fcc 7d46 accc 4cf8")
        , testCase "only a primary key's fingerprint counts, not a subkey's" $
            assertEqual "" ["AAAA"] (primaryFingerprints colons)
        , testCase "two primary keys in one file are both accepted" $
            assertEqual "" ["AAAA", "CCCC"] (primaryFingerprints (colons <> colonsSecond))
        , testCase "no key, no fingerprint" $
            assertEqual "" [] (primaryFingerprints "")
        ]
  where
    -- Help and notes of the index and key nodes, the two the key reaches.
    keyAndIndex :: Op -> [(Text, [Text])]
    keyAndIndex o =
        [ (act.extension.help, act.extension.notes)
        | n <- toList (runIdentity (expand o))
        , Just act <- [opAct n]
        , any (`Text.isPrefixOf` act.extension.help) ["refreshes", "installs"]
        ]
    colons =
        "tru::1:1700000000:0:3:1:5\n\
        \pub:-:4096:1:1111111111111111:1400000000:::-:::scESC:::::::\n\
        \fpr:::::::::AAAA:\n\
        \uid:-::::1400000000::HASH::PostgreSQL Debian Repository:::::::::\n\
        \sub:-:4096:1:2222222222222222:1400000000::::::e::::::\n\
        \fpr:::::::::BBBB:\n"
    colonsSecond =
        "pub:-:4096:1:3333333333333333:1500000000:::-:::scESC:::::::\n\
        \fpr:::::::::CCCC:\n"
