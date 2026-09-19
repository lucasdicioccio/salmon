{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for "SreBox.PostgresTls" and the certificate/ownership builtins
it is built on.

Mostly Layer 0 -- the rendered @openssl@ and @pg_ctlcluster@ command lines,
and the connection string a client is handed. The ownership check is Layer 1,
against a real temp file, because what it asserts ("this file is @0600@") is
not something a fake can be wrong about in the way that matters: Postgres
refuses to start on a key one bit wider, and libpq refuses to connect.
-}
module Test.PostgresTlsSpec (tests) where

import Data.List (isInfixOf, isSubsequenceOf)
import qualified Data.Text as Text
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Nodes.Binary (prepare)
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified SreBox.PostgresTls as Tls

import System.Process.ListLike (CmdSpec (..), CreateProcess, cmdspec)
import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "SreBox.PostgresTls"
        [ testGroup "certificate authority" caTests
        , testGroup "pg_hba" hbaTests
        , testGroup "client connection string" connStringTests
        , testGroup "file ownership" ownershipTests
        ]

processArgs :: CreateProcess -> [String]
processArgs p = case cmdspec p of
    RawCommand _ args -> args
    ShellCommand s -> [s]

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

-------------------------------------------------------------------------------

caTests :: [TestTree]
caTests =
    [ testCase "the CA is created with req -x509, which is what marks it as an issuer" $ do
        -- `openssl x509 -req -signkey` (what selfSign does) produces a
        -- certificate that looks fine and is rejected as a CA.
        let args = processArgs (prepare Certs.openssl (Certs.GenSelfSignedCa "/k/ca.key" "/k/ca.pem" (Certs.Domain "salmon-pg-ca") 3650))
        assertBool (show args) (["req", "-x509", "-new"] `isSubsequenceOf` args)
        assertBool (show args) (["-days", "3650"] `isSubsequenceOf` args)
        assertBool (show args) ("/CN=salmon-pg-ca" `elem` args)
    , testCase "signing names the CA's certificate and key, and creates a serial" $ do
        let args = processArgs (prepare Certs.openssl (Certs.SignCSRWithCa "/k/c.csr" "/k/ca.pem" "/k/ca.key" "/k/c.pem" 397))
        assertBool (show args) (["-CA", "/k/ca.pem"] `isSubsequenceOf` args)
        assertBool (show args) (["-CAkey", "/k/ca.key"] `isSubsequenceOf` args)
        assertBool (show args) ("-CAcreateserial" `elem` args)
        assertBool (show args) (["-days", "397"] `isSubsequenceOf` args)
    , testCase "a client's certificate is requested under the role's own name" $ do
        -- clientcert=verify-full compares the CN against the role, so these
        -- two strings being the same is the whole authentication.
        let cfg =
                Tls.ClientMaterialConfig
                    { Tls.cmc_role = "postgrest_authenticator"
                    , Tls.cmc_authority = authority
                    , Tls.cmc_dir = "/certs"
                    , Tls.cmc_keyType = Certs.RSA4096
                    , Tls.cmc_validityDays = 397
                    }
            (cert, key, ca) = Tls.clientMaterialPaths cfg
        assertEqual "" "/certs/postgrest_authenticator.pem" cert
        assertEqual "" "/certs/postgrest_authenticator.key" key
        assertEqual "" "/certs/ca.pem" ca
    ]
  where
    authority =
        Certs.CertificateAuthority
            { Certs.caKey = Certs.Key Certs.RSA4096 "/ca" "ca.key"
            , Certs.caCertPath = "/ca/ca.pem"
            , Certs.caCommonName = Certs.Domain "salmon-pg-ca"
            , Certs.caValidityDays = 3650
            }

-------------------------------------------------------------------------------

hbaTests :: [TestTree]
hbaTests =
    [ testCase "the line demands TLS and a certificate whose CN is the role" $ do
        let s = hbaScript (Postgres.EnsureHbaLine "main" line)
        assertBool s ("hostssl api_db postgrest_authenticator 0.0.0.0/0 cert clientcert=verify-full" `isInfixOf` s)
    , testCase "the line is appended only if missing, then the cluster is reloaded" $ do
        let s = hbaScript (Postgres.EnsureHbaLine "main" line)
        assertBool s ("grep -qxF" `isInfixOf` s)
        assertBool s ("reload" `isInfixOf` s)
    ]
  where
    line = Text.unwords ["hostssl", "api_db", "postgrest_authenticator", "0.0.0.0/0", "cert", "clientcert=verify-full"]
    hbaScript cmd = case processArgs (prepare Postgres.pgctlRun cmd) of
        (_ : s : _) -> s
        other -> error (show other)

-------------------------------------------------------------------------------

connStringTests :: [TestTree]
connStringTests =
    [ testCase "the connection string is the one libpq wants, with no password in it" $
        assertEqual
            ""
            "host=db.example port=5432 dbname=api_db user=postgrest_authenticator sslmode=verify-ca sslcert=/opt/secrets/cert.pem sslkey=/opt/secrets/key.pem sslrootcert=/opt/secrets/ca.pem"
            ( Tls.clientConnString
                (Postgres.Server "db.example" 5432)
                "api_db"
                "postgrest_authenticator"
                Tls.VerifyCa
                ("/opt/secrets/cert.pem", "/opt/secrets/key.pem", "/opt/secrets/ca.pem")
            )
    , testCase "verify-full is available for a CA that also issues elsewhere" $
        assertEqual "" "verify-full" (Tls.renderSslMode Tls.VerifyFull)
    ]

-------------------------------------------------------------------------------

ownershipTests :: [TestTree]
ownershipTests =
    [ testCase "a key Postgres would refuse is reported, and the mode is named" $
        withTempDir $ \tmp -> do
            let path = tmp </> "server.key"
            writeFile path "not really a key\n"
            setFileMode path 0o644
            result <- FS.checkOwnership (ownership path)
            case result of
                Failure msg -> do
                    assertBool (show msg) ("0o644" `Text.isInfixOf` msg)
                    assertBool (show msg) ("0o600" `Text.isInfixOf` msg)
                other -> assertBool (show other) False
    , testCase "a key at 0600 is satisfying" $
        withTempDir $ \tmp -> do
            let path = tmp </> "server.key"
            writeFile path "not really a key\n"
            setFileMode path 0o600
            assertEqual "" Success =<< FS.checkOwnership (ownership path)
    , testCase "applying the ownership is what makes the check pass" $
        withTempDir $ \tmp -> do
            let path = tmp </> "server.key"
            writeFile path "not really a key\n"
            setFileMode path 0o666
            FS.applyOwnership (ownership path)
            assertEqual "" Success =<< FS.checkOwnership (ownership path)
    , testCase "a missing file is this node failing, not this node's job to fix" $
        withTempDir $ \tmp ->
            assertBool "" . isFailure =<< FS.checkOwnership (ownership (tmp </> "absent.key"))
    ]
  where
    -- user/group left alone: the test process cannot chown to postgres, and
    -- the mode is the half that matters to both Postgres and libpq.
    ownership path =
        FS.FileOwnership
            { FS.ownedPath = path
            , FS.ownedUser = Nothing
            , FS.ownedGroup = Nothing
            , FS.ownedMode = 0o600
            }
